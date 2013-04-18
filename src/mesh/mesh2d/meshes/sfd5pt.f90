!>
!! @file   sfd5pt.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Wed Nov  7 14:59:18 2012
!!
!! @brief
!!
!!
!!
module class_mesh2d_sfd3pt
  use class_mesh2d
  use class_mesh1d_sfd3pt

  private

  type, public, extends(mesh2d) :: mesh2d_sfd5pt
     private
     type(mesh1d_sfd3pt), pointer :: m1d => null()
   contains
     procedure :: diff
  end type mesh2d_sfd5pt

  interface mesh2d_sfd5pt
     module procedure sfd5pt_new
  end interface mesh2d_sfd5pt

contains

  function sfd5pt_new() result(r)
    type(mesh2d_sfd5pt), pointer :: r

    allocate(r)
    allocate(r%m1d)
    call r%m1d%init()
    call r%set_ghost_points([2,2])

  end function sfd5pt_new


  subroutine diff(self, f, x, y, df, k)
    class(mesh2d_sfd5pt), target :: self
    real, intent(in) :: x(:,:), y(:,:), f(:,:)
    real, intent(out) :: df(:,:)
    integer, intent(in) :: k(2)

    integer :: nx, ny, i
    real :: hx, hy

    nx = size(x,1)
    ny = size(x,2)

    ! hx = x(2,1) - x(1,1)
    ! hy = y(1,2) - y(1,1)

    if( k(2) == 0 ) then
       do i = 1, ny
          call self%m1d%diff(f(:,i),x(:,i),df(:,i),k(1))
       end do
       return
    end if

    if( k(1) == 0 ) then
       do i = 1, nx
          call self%m1d%diff(f(i,:),y(i,:),df(i,:),k(2))
       end do
       return
    end if

    call self%loge("diff(): Incompatible rank")

    ! if( all(k(:,kk) == [1,0]) ) then
    !    df( 2:nx-1, :, kk ) =&
    !         (f(3:nx, :) - f(1:nx-2, :)) / hx / 2.
    !    df( 1,  :, kk ) = (f(2, :)-f(1,   :)) / hx
    !    df( nx, :, kk ) = (f(nx,:)-f(nx-1,:)) / hx
    !    cycle
    ! end if

    ! if( all(k(:,kk) == [0,1]) ) then
    !    df( :, 2:ny-1, kk ) =&
    !         (f(:, 3:ny) - f(:, 1:ny-2)) / hy / 2.
    !    df( :,  1, kk ) = (f(:, 2)-f(:,   1)) / hx
    !    df( :, ny, kk ) = (f(:,nx)-f(:,nx-1)) / hx
    !    cycle
    ! end if

  end subroutine diff

end module class_mesh2d_sfd3pt
