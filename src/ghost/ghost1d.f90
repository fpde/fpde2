module class_ghost1d

  use constants_module
  use class_platonic
  use class_icicles_wrap
  use class_boundary
  use class_boundary_box
  use class_mesh1d
  use class_icicles
  use logger_module

  private

  type, public, extends(platonic) :: ghost1d
     private
     real, allocatable :: x(:), f(:), df(:,:)
     real, allocatable :: f_left(:), f_right(:)
     type(icicles) :: ic_l, ic_r
   contains
     procedure :: update_derivatives
  end type ghost1d


contains


  subroutine update_derivatives(self, icw, fname, alpha2, m, error)
    class(ghost1d), target :: self
    type(icicles_wrap) :: icw
    class(mesh1d) :: m
    character(len=*), intent(in), target :: alpha2(:,:), fname
    integer, optional, intent(out) :: error

    character(len=:), allocatable, save :: spatial(:), xname, dfname
    integer, allocatable, save :: ndx(:)
    integer :: nx, nd, i, gp, err
    real, pointer :: x(:), f(:), df(:,:), df1(:)
    class(boundary_box), pointer :: bbox
    class(boundary), pointer :: b_left, b_right

    ! get name of spatial variable
    spatial = icw%get_names(icw_spatial)
    ! at this point we assume that spatial is of size 1, it is safe
    ! because update_derivatives() already checked that.
    xname   = spatial(1)

    ! translate string representation of alpha2 to integer
    ! representation
    ndx = count(alpha2 == xname, 1)
    nd  = size(alpha2,2)
    ! get the number of grid points
    nx  = icw%get_nx(1)
    gp  = m%get_ghost_points(1)

    call icw%get( name = fname, box = bbox, error = err )
    if( err == FPDE_STATUS_ERROR .or. .not. associated(bbox) ) then
       call self%log(FPDE_LOG_ERROR, "Unable to extract boundary&
            & box.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if


    ! get boundary conditions
    call bbox%get( 1, left = b_left, right = b_right, error = err )
    if( err /= FPDE_STATUS_OK ) then
       call self%log(FPDE_LOG_ERROR,&
            "Unable to extract boundary conditions from boundary_box")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! allocate df
    if( .not. allocated(self%df) ) allocate(self%df(0,0))
    if( size(self%df,1) < nx+2*gp .or. size(self%df,2) < nd ) then
       deallocate(self%df)
       allocate(self%df(nx+2*gp,nd))
    end if

    ! allocate x
    if( .not. allocated(self%x) ) allocate(self%x(0))
    if( size(self%x) < nx+2*gp ) then
       deallocate(self%x)
       allocate(self%x(nx+2*gp))
    end if

    ! allocate f
    if( .not. allocated(self%f) ) allocate(self%f(0))
    if( size(self%f) < nx+2*gp ) then
       deallocate(self%f)
       allocate(self%f(nx+2*gp))
    end if

    ! allocate f_left
    if( .not. allocated(self%f_left) ) allocate(self%f_left(0))
    if( size(self%f_left) < gp ) then
       deallocate(self%f_left)
       allocate(self%f_left(gp))
    end if

    ! allocate f_right
    if( .not. allocated(self%f_right) ) allocate(self%f_right(0))
    if( size(self%f_right) < gp ) then
       deallocate(self%f_right)
       allocate(self%f_right(gp))
    end if

    call fill_in_temporary_data(self, icw, fname, xname, m, b_left,&
         b_right, error = err)

    if( err /= FPDE_STATUS_OK ) then
       call self%log(FPDE_LOG_ERROR, "Unable to fill the temporary data.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! assign new short names for temporary variables
    x  => self%x
    f  => self%f
    df => self%df

    ! actual differentiation
    call m%diff(f,x,df,ndx)

    ! copy values back to icicles
    do i = 1, nd
       call icw%get(icw%derivative_name(fname,alpha2(:,i)), vec = df1)
       df1(1:nx) = df(gp+1:nx+gp,i)
    end do

  end subroutine update_derivatives


  subroutine fill_in_temporary_data(self, icw, fname, xname, m,&
       b_left, b_right, error)
    type(ghost1d), target :: self
    type(icicles_wrap) :: icw
    character(len=*) :: fname, xname
    class(boundary) :: b_left, b_right
    class(mesh) :: m
    integer, optional, intent(out) :: error

    type(icicles), pointer :: ic_l, ic_r
    class(boundary_box), pointer :: bbox
    real, pointer :: x(:), f(:), v_temp(:),&
         f_(:,:), f_left_(:,:), f_right_(:,:), x_(:,:)
    ! real, allocatable, save, target :: f_left(:), f_right(:)
    character(len=:), allocatable :: par_l(:), par_r(:),&
         icw_par_l(:), icw_par_r(:)
    integer :: i, gp, err1, err2, nx

    if(present(error)) error = FPDE_STATUS_OK

    gp = m%get_ghost_points(1)
    nx = icw%get_nx(1)
    ic_l => self%ic_l
    ic_r => self%ic_r

    ! cook up the fake icicles
    call fake_icicles(icw, ic_l, b_left,  fname, xname, icw_dir_left,  err1)
    call fake_icicles(icw, ic_r, b_right, fname, xname, icw_dir_right, err2)

    if(err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK) then
       call self%log(FPDE_LOG_ERROR,&
            "Creation of iciles was unsuccessful.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if


    ! get the values of f and x
    call icw%get( name = fname, vec = f, error = err1)
    call icw%get( name = xname, vec = x, error = err2)

    if(err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK) then
       call self%log(FPDE_LOG_ERROR,&
            "Could not get the pointer to ["//trim(fname)//"] or ["&
            //trim(xname)//"].")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! initialize x ghost points
    ! x_left  = x(1)  - (x( gp+1 :2     :-1 ) - x(1))
    ! x_right = x(nx) - (x( nx-1 :nx-gp :-1 ) - x(nx))
    self%x(1:gp) = x(1)  - (x( gp+1 :2     :-1 ) - x(1))
    self%x(gp+1:gp+nx) = x
    self%x(gp+nx+1:2*gp+nx) = x(nx) - (x( nx-1 :nx-gp :-1 ) - x(nx))

    ! self%x(1:)  = [x_left, x, x_right]

    ! self%f_left  = spread(0.,1,gp)
    ! self%f_right = spread(0.,1,gp)

    ! create temporary pointers for boundary_conditions
    f_left_ ( 1:gp, 1:1) => self%f_left (1:gp)
    f_right_( 1:gp, 1:1) => self%f_right(1:gp)
    f_      ( 1:nx, 1:1) => f(1:nx)
    x_      ( 1:nx, 1:1) => x(1:nx)


    ! initialize f ghost points using parameters from fake icicles
    call b_left%generate_values(&
         ic = ic_l,&
         fin = f_,&
         fout = f_left_(gp:1:-1,:),&
         xin = x_,&
         error = err1)
    call b_right%generate_values(&
         ic = ic_r,&
         fin = f_(nx:1:-1,:),&
         fout = f_right_,&
         xin = x_(nx:1:-1,:),&
         error = err2)

    if(err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK) then
       call self%log(FPDE_LOG_ERROR,&
            "Ghost points could not be filled in.")
    end if

    ! self%f(1:) = [f_left, f, f_right]
    self%f(1:gp)  = self%f_left
    self%f(gp+1:gp+nx) = f
    self%f(gp+nx+1:2*gp+nx) = self%f_right

  end subroutine fill_in_temporary_data


  subroutine fake_icicles(icw, fake_ic, b, fname, xname, dir, error)
    class(icicles_wrap), intent(in) :: icw
    type(icicles), intent(out) :: fake_ic
    class(boundary), intent(in) :: b
    character(len=*), intent(in) :: fname, xname, dir
    integer, intent(out), optional :: error

    integer :: err1, err2, i
    character(len=:), allocatable :: icw_par(:), par(:)
    real, pointer :: v_temp(:)

    if(present(error)) error = FPDE_STATUS_OK

    par = b%get_param_names()
    icw_par = b%get_icw_param_names(fname, xname, dir)

    call fake_ic%init(error = err1)
    if( err1 /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    fake_ic%name = "fake_ic"
    call fake_ic%clear()

    do i = 1, size(par)
       call icw%get&
            ( name = icw_par(i), vec = v_temp, error = err1 )
       ! we know that for dimension = 1 shape = [1], so length = 1
       call fake_ic%add&
            ( name = par(i), length = 1, ptr = v_temp )
       if( err1 /= FPDE_STATUS_OK ) then
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if
    end do

  end subroutine fake_icicles

end module class_ghost1d
