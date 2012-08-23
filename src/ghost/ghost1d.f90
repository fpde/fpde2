module class_ghost1d

  use constants_module
  use class_platonic
  use class_icicles_wrap
  use class_iced_boundary
  use class_boundary_box
  use class_mesh1d
  use class_icicles
  use logger_module

  private

  type, public, extends(platonic) :: ghost1d
     private
     real, allocatable :: x(:), f(:), df(:,:)
     real, allocatable :: f_left(:), f_right(:)
     integer :: nx = 0, nd = 0, gp = 0
     logical :: alloced = .false.
   contains
     procedure :: update_derivatives
     procedure, private :: alloc
  end type ghost1d


contains

  subroutine alloc(self, nx, nd, gp)
    class(ghost1d) :: self
    integer, intent(in) :: nx, nd, gp

    if( .not. self%alloced ) then
       allocate(self%df(0,0))
       allocate(self%x(0))
       allocate(self%f(0))
       allocate(self%f_left(0))
       allocate(self%f_right(0))
       self%alloced = .true.
    end if

    ! realloc if needed
    if( self%nx < nx .or. self%nd < nd .or. self%gp < gp ) then
       deallocate(self%df)
       allocate(self%df(nx+2*gp,nd))
       deallocate(self%x)
       allocate(self%x(nx+2*gp))
       deallocate(self%f)
       allocate(self%f(nx+2*gp))
       deallocate(self%f_left)
       allocate(self%f_left(gp))
       deallocate(self%f_right)
       allocate(self%f_right(gp))
       self%nx = nx
       self%nd = nd
       self%gp = gp
    end if

  end subroutine alloc


  subroutine update_derivatives(self, icw, fname, alpha2, m, error)
    class(ghost1d), target :: self
    type(icicles_wrap) :: icw
    class(mesh1d) :: m
    character(len=*), intent(in), target :: alpha2(:,:), fname
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: xname
    integer, allocatable, save :: ndx(:)
    integer :: nx, nd, i, gp, err
    real, pointer :: x(:), f(:), df(:,:), df1(:)
    type(boundary_box), pointer :: bbox
    type(iced_boundary), pointer :: b_left, b_right

    ! get name of spatial variable
    ! spatial = icw%get_names(icw_spatial)
    ! at this point we assume that spatial is of size 1, it is safe
    ! because update_derivatives() already checked that.
    xname   = icw%get_spatial(1)

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
    call bbox%get( 1, 1, b_left)
    call bbox%get( 1, 2, b_right)
    if( err /= FPDE_STATUS_OK ) then
       call self%log(FPDE_LOG_ERROR,&
            "Unable to extract boundary conditions from boundary_box")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! reallocate scratch memory if needed
    call self%alloc(nx, nd, gp)

    ! fill the temporary data using boundary conditions
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
    type(iced_boundary) :: b_left, b_right
    class(mesh) :: m
    integer, optional, intent(out) :: error

    real, pointer :: x(:), f(:),&
         f_(:,:), f_left_(:,:), f_right_(:,:), x_(:,:)
    integer :: gp, err1, err2, nx

    if(present(error)) error = FPDE_STATUS_OK

    gp = m%get_ghost_points(1)
    nx = icw%get_nx(1)

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
    self%x(1:gp) = x(1)  - (x( gp+1 :2     :-1 ) - x(1))
    self%x(gp+1:gp+nx) = x
    self%x(gp+nx+1:2*gp+nx) = x(nx) - (x( nx-1 :nx-gp :-1 ) - x(nx))

    ! create temporary pointers for boundary_conditions
    f_left_ ( 1:gp, 1:1) => self%f_left (1:gp)
    f_right_( 1:gp, 1:1) => self%f_right(1:gp)
    f_      ( 1:nx, 1:1) => f(1:nx)
    x_      ( 1:nx, 1:1) => x(1:nx)

    ! initialize f ghost points using parameters from fake icicles
    call b_left%generate_values(&
         fin = f_,&
         fout = f_left_(gp:1:-1,:),&
         xin = x_,&
         error = err1)

    call b_right%generate_values(&
         fin = f_(nx:1:-1,:),&
         fout = f_right_,&
         xin = x_(nx:1:-1,:),&
         error = err2)

    if(err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK) then
       call self%log(FPDE_LOG_ERROR,&
            "Ghost points could not be filled.")
    end if

    self%f(1:gp)  = self%f_left
    self%f(gp+1:gp+nx) = f
    self%f(gp+nx+1:2*gp+nx) = self%f_right

  end subroutine fill_in_temporary_data


end module class_ghost1d
