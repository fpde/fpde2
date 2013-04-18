!>
!! @file   2d.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Nov  9 14:07:47 2012
!!
!! @brief update derivative using ghost method to implement boundary
!! conditions
!!
!!
!! The naming convention for areas is denoted below
!!
!! +----+-----+----+
!! | TL |  T  | TR |
!! +----+-----+----+
!! |    |     |    |
!! | L  |  C  |  R |
!! |    |     |    |
!! +----+-----+----+
!! | BL |  B  | BR |
!! +----+-----+----+
!!
!!
!!
!! for the boundary region id's cosult
!! coordinates/cartesian/cartesian2d.f90
!!
!! L = 1
!! R = 2
!! T = 3
!! B = 4
!!
!! The strategy is as follows:
!! 1) fill in the L and R areas using boundary
!! conditions on L and R, see fill_f_LR()
!! 2) fill in the TL, T, TR areas using boundary conditions extended
!! to L and R, then do the same thing for BL, B, BR, see fill_f_TB()
!! 3) calculate the derivatives which depend on the ghost points, see fill_df()
!! 4) copy the calculated derivatives back to the caller using df
!! argument in update_df()
!!
module class_g2d_methods

  use class_coordinates_c2d
  use class_bbox_user
  use class_boundary_ghost
  use class_mesh2d
  use class_boundary_ghost

  use helper_module

  use class_platonic

  private

  integer, parameter :: &
       c_left   = coordinates_c2d_left,&
       c_right  = coordinates_c2d_right,&
       c_top    = coordinates_c2d_top,&
       c_bottom = coordinates_c2d_bottom

  type, public, extends(platonic) :: g2d_methods
     private
     real, allocatable, dimension(:,:) :: f, x, y, df, pt, pb
   contains
     procedure, private :: allocate_temp
     procedure, private :: fill_xy
     procedure, private :: fill_f
     procedure, private :: fill_df
     procedure, private :: fill_f_LR
     procedure, private :: fill_f_TB
     procedure :: update_df
  end type g2d_methods

contains

  subroutine update_df(self, f, x, y, df, m, b, alpha)
    class(g2d_methods) :: self
    real, intent(in), dimension(:,:) :: x, y, f
    real, intent(out) :: df(:,:)
    class(mesh2d), intent(in) :: m
    class(bbox_user), intent(in) :: b
    integer, intent(in) :: alpha(2)

    integer, allocatable :: gp(:)
    integer :: n(2), np(4), i

    gp = m%get_ghost_points()
    do i = 1, 2
       n(i)  = size(x,i)
    end do

    do i = 1, 4
       np(i) = b%num_param(i)
    end do

    call self%allocate_temp(gp, n, np)
    call self%fill_xy(x,y,gp)
    call self%fill_f(x,y,f,b,gp)
    call self%fill_df(df,gp,m,alpha)

  end subroutine update_df


  subroutine allocate_temp(self, gp, n, np)
    class(g2d_methods) :: self
    integer, intent(in) :: gp(2), n(2), np(4)

    integer :: gpx, gpy, ny, nx

    gpx = gp(1)
    gpy = gp(2)
    nx = n(1)
    ny = n(2)

    call realloc_r2lu(self%f,  [1-gpx,1-gpy], [nx+gpx,ny+gpy])
    call realloc_r2lu(self%x,  [1-gpx,1-gpy], [nx+gpx,ny+gpy])
    call realloc_r2lu(self%y,  [1-gpx,1-gpy], [nx+gpx,ny+gpy])
    call realloc_r2lu(self%df, [1-gpx,1-gpy], [nx+gpx,ny+gpy])

    call realloc_r2lu(self%pb, [1, 1-gpx], [np(c_bottom), nx+gpx])
    call realloc_r2lu(self%pt, [1, 1-gpx], [np(c_top   ), nx+gpx])

  end subroutine allocate_temp


  subroutine fill_xy(self, x, y, gp)
    class(g2d_methods) :: self
    real,    intent(in) :: x(:,:), y(:,:)
    integer, intent(in) :: gp(2)

    integer :: gpx, gpy, nx, ny
    integer :: i

    gpx = gp(1)
    gpy = gp(2)

    nx = size(x,1)
    ny = size(x,2)

    ! assume the arrays are already allocatad

    ! here we just copy (all of) the inner values
    !! @todo for optimized procedure we should copy only the values
    !! close to the borders
    self%x(1:nx,1:ny) = x
    self%y(1:nx,1:ny) = y

    ! side: B+T
    do i = 1, gpy
       ! side: B, the formula is (x,y) := (x,2*y0-y)
       self%x(1:nx, 1-i)  =   x(:, 1+i )
       self%y(1:nx, 1-i)  = 2*y(:, 1   ) - y(:, 1+i)
       ! side: T, the formula is (x,y) := (x,2*y1-y)
       self%x(1:nx, ny+i) =   x(:, ny-i)
       self%y(1:nx, ny+i) = 2*y(:, ny  ) - y(:, ny-i)
    end do

    ! side: L + R + BL + BR + TL + TR
    do i = 1, gpx
       ! sides: L + BL + TL, (x,y) := (2*x0-x,y)
       self%x(1-i,  :) = 2*self%x(1,   :) - self%x(1+i,:)
       self%y(1-i,  :) =   self%y(1+i, :)
       ! sides: R + BR + TR, (x,y) := (2*x1-x,y)
       self%x(nx+i, :) = 2*self%x(nx,  :) - self%x(nx-i,:)
       self%y(nx+i, :) =   self%y(nx-i,:)
    end do

  end subroutine fill_xy


  subroutine fill_f(self, x, y, f, box, gp)
    class(g2d_methods) :: self
    real, intent(in), dimension(:,:) :: x, y, f
    class(bbox_user) :: box
    integer, intent(in) :: gp(2)

    integer :: nx, ny

    nx  = size(x,1)
    ny  = size(x,2)

    self%f(1:nx, 1:ny) = f
    call self%fill_f_LR(x, y, f, box, gp)
    call self%fill_f_TB(x, y, f, box, gp)

  end subroutine fill_f


  !> This subroutine fills in the areas L and R
  !!
  subroutine fill_f_LR(self, x, y, f, box, gp)
    class(g2d_methods) :: self
    real, intent(in), dimension(:,:) :: x, y, f
    class(bbox_user), intent(in) :: box
    integer, intent(in) :: gp(2)

    class(boundary_ghost), pointer :: l, r
    real, pointer, dimension(:,:) :: pl, pr
    integer :: gpx, gpy, nx, ny, i

    l => toghost(box%boundary(c_left))
    r => toghost(box%boundary(c_right))

    pl => l%rawp()
    pr => r%rawp()

    gpx = gp(1)
    gpy = gp(2)
    nx  = size(x,1)
    ny  = size(x,2)

    do i = 1, ny
       ! side: L
       call l%generate_values(&
            fin    = self%f(1:nx,       i),&
            fout   = self%f(0:1-gpx:-1, i),&
            xin    = self%x(1:nx,       i),&
            params = pl(:,              i))

       ! side: R
       call r%generate_values(&
            fin    = self%f(nx:1:-1,       i),&
            fout   = self%f(nx+1:nx+gpx,   i),&
            xin    = self%x(nx:1:-1,       i),&
            params = pr(:,                 i))
    end do

  end subroutine fill_f_LR


  subroutine fill_f_TB(self, x, y, f, box, gp)
    class(g2d_methods) :: self
    real, intent(in), dimension(:,:) :: x, y, f
    class(bbox_user), intent(in) :: box
    integer, intent(in) :: gp(2)

    class(boundary_ghost), pointer :: l, r, t, b
    real, pointer, dimension(:,:) :: pl, pr, pt, pb
    integer :: gpx, gpy, nx, ny, i

    l => toghost(box%boundary(c_left))
    r => toghost(box%boundary(c_right))
    b => toghost(box%boundary(c_bottom))
    t => toghost(box%boundary(c_top))

    pl => l%rawp()
    pr => r%rawp()
    pb => b%rawp()
    pt => t%rawp()

    gpx = gp(1)
    gpy = gp(2)
    nx  = size(x,1)
    ny  = size(x,2)

    ! fill in the parameters
    ! side: B
    self%pb(:,1:nx) = pb
    ! side: T
    self%pt(:,1:nx) = pt

    ! side: BL
    do i = 1, size(pb,1)
       call l%generate_values(&
            fin  = self%pb(i, 1:        ),&
            fout = self%pb(i, 0:1-gpx:-1),&
            xin  = self%x(1:nx,        i),&
            params = pl(1:,           ny))
    end do
    ! side: TL
    do i = 1, size(pt,1)
       call l%generate_values(&
            fin  = self%pt(i, 1:        ),&
            fout = self%pt(i, 0:1-gpx:-1),&
            xin  = self%x(1:nx,        i),&
            params = pl(1:,            1))
    end do
    ! side: BR
    do i = 1, size(pb,1)
       call r%generate_values(&
            fin  = self%pb(i, nx:1:-1    ),&
            fout = self%pb(i, nx+1:nx+gpx),&
            xin  = self%x(nx:1:-1,      i),&
            params = pr(1:,            ny))
    end do
    ! side: TR
    do i = 1, size(pt,1)
       call r%generate_values(&
            fin  = self%pt(i, nx:1:-1    ),&
            fout = self%pt(i, nx+1:nx+gpx),&
            xin  = self%x(nx:1:-1,      i),&
            params = pr(1:,             1))
    end do

    ! fill in the function
    !
    do i = 1-gpx, nx+gpx
       ! areas BL, B, BR
       call b%generate_values(&
            fin  = self%f(i, ny:1:-1),&
            fout = self%f(i, ny+1:ny+gpy),&
            xin  = self%y(i, ny:1:-1),&
            params = self%pb(:,i))
       ! areas TL, T, TR
       call t%generate_values(&
            fin  = self%f(i, 1:),&
            fout = self%f(i, 0:1-gpy:-1),&
            xin  = self%y(i, 1:),&
            params = self%pt(:,i))
    end do

  end subroutine fill_f_TB


  subroutine fill_df(self, df, gp, m, alpha)
    class(g2d_methods) :: self
    real, intent(out) :: df(:,:)
    integer, intent(in) :: gp(2), alpha(2)
    class(mesh2d), intent(in) :: m

    integer :: gpx, gpy, nx, ny

    gpx = gp(1)
    gpy = gp(2)

    nx = size(df, 1)
    ny = size(df, 2)

    ! gp = 2
    ! g g O o o o o o
    !   |---|---|
    !           ^ = 0 + 2*gp = 4
    ! gp = 1
    !   g O o o o o
    !   |-|-|
    !       ^ = 0 + 2*gp = 2

    ! area: T
    call m%diff(&
         f  = self%f (1-gpx:nx+gpx,1-gpy:2*gpy),&
         x  = self%x (1-gpx:nx+gpx,1-gpy:2*gpy),&
         y  = self%y (1-gpx:nx+gpx,1-gpy:2*gpy),&
         df = self%df(1-gpx:nx+gpx,1-gpy:2*gpy), &
         k  = alpha)
    df(1:nx,1:gpy) = self%df(1:nx,1:gpy)

    ! area: B
    call m%diff(&
         f = self%f(1-gpx:nx+gpx,ny-2*gpy+1:ny+gpy),&
         x = self%x(1-gpx:nx+gpx,ny-2*gpy+1:ny+gpy),&
         y = self%y(1-gpx:nx+gpy,ny-2*gpy+1:ny+gpy),&
         df = self%df(1-gpx:nx+gpy,ny-2*gpy+1:ny+gpy), &
         k = alpha)
    df(1:nx,ny-gpy+1:ny) = self%df(1:nx,ny-gpy+1:ny)

    ! area: L + LB + LT
    call m%diff(&
         f = self%f(1-gpx:2*gpx, 1-gpy:ny+gpy),&
         x = self%x(1-gpx:2*gpx, 1-gpy:ny+gpy),&
         y = self%y(1-gpx:2*gpx, 1-gpy:ny+gpy),&
         df = self%df(1-gpx:2*gpx, 1-gpy:ny+gpy),&
         k = alpha)
    df(1:gpx,1:ny) = self%df(1:gpx,1:ny)

    ! area: R + RB + RT
    call m%diff(&
         f = self%f(nx-2*gpx+1:nx+gpx, 1-gpy:ny+gpy),&
         x = self%x(nx-2*gpx+1:nx+gpx, 1-gpy:ny+gpy),&
         y = self%y(nx-2*gpx+1:nx+gpx, 1-gpy:ny+gpy),&
         df = self%df(nx-2*gpx+1:nx+gpx, 1-gpy:ny+gpy),&
         k = alpha)
    df(nx-gpx+1:nx,1:ny) = self%df(nx-gpx+1:nx,1:ny)

  end subroutine fill_df

end module class_g2d_methods
