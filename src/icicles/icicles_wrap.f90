!>
!! @file   icicles_registry.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sun Jul 29 12:16:10 2012
!!
!! @brief Wrap to icicles structure.
!!
!! The extended icicles allow for boolean type options, shape array
!! and boundary conditions. As for now only adding (as appending) of
!! entries is supported (no entry can be romoved). Also only simplest
!! queries can be made (i.e. ask for functions with given option).
!!
!! @todo regexp queries
!! @todo array of options queries
!!
module class_icicles_wrap

  use constants_module
  use class_icicles
  use class_platonic
  use helper_module
  use class_boundary_box
  use logger_module

  private

  !>
  !! Structure holding additional data describing a "function" or
  !! "entry". It exapnds the data contained in icicles.
  !!
  type :: param
     !> somehow the len=: does not work, hence len=NAME_LEN
     ! character(len=:), allocatable :: name
     character(len=NAME_LEN) :: name
     !> options is a list of strings, e.g. ["spatial", "dependent"]
     character(len=:), allocatable :: options(:)
     !> boundary_box contains all information about boundray
     !! conditions
     type(boundary_box), pointer :: box => null()
     !> e.g. shape = [nx,ny,nz] etc. for scalar it should be [1]
     integer, allocatable :: shape(:)
  end type param


  !>
  !! icicles_wrap is an extension to icicles. It contains information
  !! about mesh size (nx) and about functions (params)
  !!
  type, public, extends(platonic) :: icicles_wrap
     private
     type(icicles), pointer :: ic => null()
     type(param), allocatable :: params(:) !array of parameters
     integer, allocatable:: nx(:)
     !! @todo possibly add cached names of spatial variables?
     ! character(len=:), allocatable :: spatial(:)
     logical :: after_init = .false.
     character(len=:), allocatable :: temporal
     character(len=:), allocatable :: spatial(:)
   contains
     ! procedure :: from_lua
     procedure :: init
     procedure :: set_nx
     procedure :: set_pointers
     procedure :: total_length
     procedure :: get_dim
     procedure, private :: get_all_nx
     procedure, private :: get_some_nx
     generic :: get_nx => get_all_nx, get_some_nx
     procedure :: add
     procedure :: add_spatial
     procedure :: get_spatial
     procedure :: get_temporal
     procedure :: get_names
     procedure :: get
     procedure, private :: add_boundary_data
     procedure, nopass :: derivative_name
  end type icicles_wrap

contains


  subroutine init(p, error)
    class(icicles_wrap), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK
    p%name = "icicles_wrap"

    ! allocate icicles
    allocate(p%ic)
    call p%ic%init()

    ! allocate parameters
    allocate(p%params(0))

    ! check if nx was set
    if( .not. allocated(p%nx) ) then
       call p%log(FPDE_LOG_ERROR, &
            "nx not set, use set_nx([nx1,nx2,...]) before init()")
       if(present(error)) error = FPDE_STATUS_OK
    end if

    p%after_init = .true.
  end subroutine init


  subroutine set_nx(self, nx)
    class(icicles_wrap) :: self
    integer :: nx(:)

    if( .not. self%after_init ) then
       self%nx = nx
    else
       call self%log(FPDE_LOG_WARNING,&
            "Setting nx is forbidden after calling init().")
    end if

  end subroutine set_nx


  subroutine set_pointers(self, vec, names, error)
    class(icicles_wrap), target :: self
    real, target, intent(in) :: vec(:)
    character(len=*), intent(in), optional :: names(:)
    integer, optional, intent(out) :: error

    integer :: err

    if( self%after_init ) then
       if( present(names) ) then
          call self%ic%set_pointers(vec, names, error = err)
       else
          call self%ic%set_pointers(vec, error = err)
       end if

       if(present(error)) error = err
       return
    else
       call self%log(FPDE_LOG_WARNING,&
            "Trying to call set_pointers on uninitialized icicles_wrap&
            &. Call init() first.")
       if(present(error)) error = FPDE_STATUS_ERROR
    end if

  end subroutine set_pointers


  function total_length(self, names)
    class(icicles_wrap), target :: self
    integer :: total_length
    character(len=*), intent(in), optional :: names(:)

    if(present(names)) then
       total_length = self%ic%total_length(names)
    else
       total_length = self%ic%total_length()
    end if

  end function total_length


  !> Returns base shape of data in icicles
  !!
  !! @return [nx1,nx2,...]
  !!
  function get_all_nx(self)
    class(icicles_wrap) :: self
    integer, allocatable :: get_all_nx(:)
    get_all_nx = self%nx
  end function get_all_nx

  !>
  !! @return dimension of the system
  !!
  function get_dim(self) result(d)
    class(icicles_wrap) :: self
    integer :: d
    d = size(self%nx)
  end function get_dim


  !> Returns a shape of data in a direction given by var
  !!
  !! @return nx(var)
  !!
  function get_some_nx(self, var)
    class(icicles_wrap) :: self
    integer, intent(in) :: var
    integer :: get_some_nx
    get_some_nx = self%nx(var)
  end function get_some_nx


  !> returns names matching option
  !!
  !! @todo regexp options
  !!
  !! @param option
  !!
  function get_names(self, option)
    class(icicles_wrap), intent(in) :: self
    character(len=*), intent(in), optional :: option
    character(len=:), allocatable :: get_names(:)

    if(present(option)) then
       get_names = pack(self%params%name, optionq(self%params,option))
    else
       get_names = self%params%name
    end if

  end function get_names


  function get_temporal(self, error) result(r)
    class(icicles_wrap), intent(in) :: self
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: r

    if(present(error)) error = FPDE_STATUS_OK

    if(allocated(self%temporal)) then
       r = self%temporal
    else
       if(present(error)) error = FPDE_STATUS_ERROR
       r = ""
    end if

  end function get_temporal


  function get_spatial(self, var, error) result(r)
    class(icicles_wrap), intent(in) :: self
    integer :: var
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: r


    if(present(error)) error = FPDE_STATUS_ERROR
    r = ""

    if(allocated(self%spatial)) then
       if(.not. var > size(self%spatial)) then
          if(present(error)) error = FPDE_STATUS_OK
          r = self%spatial(var)
       end if
    end if

  end function get_spatial


  !> Implemented for convenient use, it calls add() for each element
  !! of spatial with options = icw_spatial
  !!
  !! @param spatial
  !! @param error
  !!
  !! @todo 10 add flag add_spatial_ok to icw, and make it the only way
  !! to set up the spatial variables. Also, optimize against calls of
  !! get(spatial_variable).
  !!
  subroutine add_spatial(self, spatial, error)
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: spatial(:)
    integer, intent(out), optional :: error

    integer :: i, err

    if( .not. self%after_init) then
       err = FPDE_STATUS_ERROR
       call self%log(FPDE_STATUS_ERROR,&
            "Calling add_spatial() before init().")
       return
    else if( size(spatial) /= size(self%get_nx()) ) then
       err = FPDE_STATUS_ERROR
       call self%log(FPDE_STATUS_ERROR,&
            "Calling add_spatial() with spatial incompatible with nx(:).")
       return
    else
       do i = 1, size(spatial)
          call self%add(&
               name = spatial(i),&
               options = [icw_spatial],&
               error = err)
       end do
    end if

    !! @bug, err can be changed several times in a do loop above
    if(present(error)) error = err
    self%spatial = spatial

  end subroutine add_spatial


  !> Function used to add entries to icicles_wrap.
  !!
  !! add() lets the user to add entries to icicles_wrap. Each entry
  !! can be later on accessed with get(). All arguments besides name
  !! are optional.
  !!
  !! @param name name of the entry to be added
  !! @param options array of options of the entry
  !! @param shape shape of the entry
  !! @param box boundary condtitions of the entry
  !! @param derivatives possible derivatives of the entry
  !! @param error
  !!
  recursive subroutine add(self, name, options, shape,&
       box, derivatives, refs, error)
    use class_boundary
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional ::  options(:),&
         derivatives(:,:)
    integer, intent(in), optional :: shape(:)
    class(boundary_box), intent(in), target, optional :: box
    integer, intent(out), optional :: error
    type(icicles_referencer), optional, intent(in) :: refs(:)

    ! local variables
    type(param) :: p
    ! temporary variables needed to circumvent the bug
    type(param), allocatable :: pp(:)
    integer :: i, err
    ! boundary variables
    character(len=:), allocatable :: spatial(:)

    if(present(error)) error = FPDE_STATUS_OK

    if(any(self%params%name==name)) then
       if(present(error)) error = FPDE_STATUS_ERROR
       call self%log(FPDE_LOG_ERROR,&
            "add: Trying to add a duplicate entry ["&
            //trim(name)//"], ignoring.")
       return
    end if

    ! initialize entry
    p%name = name

    if( present(options) ) then
       p%options = options
       if(any(options==icw_temporal)) then
          self%temporal = name
       end if
    else
       ! by default allocate zero-sized options
       allocate(character(len=0)::p%options(0))
    end if

    if( present(shape) ) then
       p%shape = shape
    else
       ! default shape
       p%shape = self%get_nx()
    end if

    if( present(box) ) then
       ! @todo enclose in add_boundary_box(...)
       spatial = self%get_names(icw_spatial)
       if( size(spatial) < size(self%get_nx()) ) then
          call self%log(FPDE_LOG_ERROR,&
               "Unable to add function with boundary conditions, no sp&
               &atial variables were specified.")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       else
          ! make a deep copy of boundary box
          allocate(p%box, source = box)
          call self%add_boundary_data(p)
       end if
    end if

    if(present(derivatives))then
       ! @todo enclose in add_derivatives(...)
       do i = 1, size(derivatives,2)
          call self%add(&
               name    = self%derivative_name(name, derivatives(:,i)), &
               options = [p%options,icw_derivative],                   &
               shape   = p%shape,                                      &
               error   = err)
          if( err/= FPDE_STATUS_OK ) then
             if(present(error)) error = err
             return
          end if
       end do
    end if

    ! add entry to registry

    ! @bug the following should work in gfortran on after ifort gets
    ! it fixed, see
    ! http://software.intel.com/en-us/forums/showthread.php?t=107075

    ! self%params = [self%params, p]

    ! temporary fix is to use explicit reallocation:
    pp = [self%params, p]
    call move_alloc(pp, self%params)

    ! add entry to icicles
    if(present(refs)) then
       call self%ic%add(name, product(p%shape), refs = refs)
    else
       call self%ic%add(name, product(p%shape))
    end if

  end subroutine add

  subroutine add_boundary_data(self, p, error)
    class(icicles_wrap), target :: self
    type(param), intent(in) :: p
    integer, optional, intent(out) :: error

    integer :: i, j, err
    integer, pointer :: nx(:)
    character(len=:), allocatable :: names(:), spatial(:)
    type(icicles_referencer), allocatable :: refs(:)

    if(present(error)) error = FPDE_STATUS_OK

    spatial = self%get_names(icw_spatial)

    nx => self%nx

    do j = 1, self%get_dim()

       call p%box%generate_ic_data(&
            var = j,&
            fname = p%name,&
            spatial = spatial,&
            names = names,&
            refs = refs,&
            error = err)

       do i = 1, size(names)
          call self%add(&
               name = names(i),&
               options = [p%options,icw_boundary],&
               shape = [nx(:j-1),1,nx(j+1:)],&
               error = err, &
               refs = refs)

          if( err /= FPDE_STATUS_OK ) then
             if(present(error)) error = err
             call self%log(FPDE_STATUS_ERROR,&
                  "Unable to add boundary condition for function ["&
                  //trim(p%name)//"].")
             return
          end if

       end do

    end do


  end subroutine add_boundary_data


  !> Helper function to used find matching options
  !!
  !! @param o iciles_wrap entry of type(param)
  !! @param el option we are looking for
  !!
  !! @return .true. if o has option el present
  !!
  elemental logical function optionq(o, el)
    type(param), intent(in) :: o
    character(len=*), intent(in) :: el
    optionq = any(o%options == el)
  end function optionq


  subroutine get(self, name, vec, scal, options, box, shape, error)
    class(icicles_wrap), intent(in), target :: self
    character(len=*), intent(in) :: name
    character(len=:), intent(out), optional, allocatable :: options(:)
    type(boundary_box), intent(out), pointer, optional :: box
    integer, intent(out), allocatable, optional :: shape(:)
    real, intent(out), optional, pointer :: vec(:), scal
    integer, intent(out), optional :: error

    ! local variables
    real, pointer :: v(:), s
    type(param), pointer :: p
    integer :: err, i

    if(present(error)) error = FPDE_STATUS_OK

    call self%ic%get(name, v, s, err)

    do i = 1, size(self%params)
       if(self%params(i)%name == name) then
          p => self%params(i)
          exit
       end if
    end do

    if( .not. ( err == FPDE_STATUS_OK .and. &
       associated(p)) ) then
       call self%log(FPDE_LOG_ERROR,&
            "No entry named ["//trim(name)//"]")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( present(scal     ))  scal    => s
    if( present(vec      ))  vec     => v
    if( present(options  ))  options =  p%options
    if( present(box      ))  box     => p%box
    if( present(shape    ))  shape   =  p%shape

  end subroutine get

  function derivative_name(fname, derivative) result(r)
    character(len=*) :: fname, derivative(:)

    character(len=:), allocatable :: r

    r = icw_derivative_name// "(" // &
         trim(fname) // "," // &
         trim(join(derivative,",")) // ")"
  end function derivative_name


end module class_icicles_wrap
