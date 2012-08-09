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
     class(boundary_box), pointer :: boundary => null()
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
   contains
     ! procedure :: from_lua
     procedure :: init
     procedure :: set_nx
     procedure :: get_nx
     procedure :: add
     procedure :: get_names
     procedure :: get
  end type icicles_wrap

contains


  subroutine init(p, error)
    class(icicles_wrap), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "icicles_wrap"
    allocate(p%ic)
    p%nx=[1]
  end subroutine init


  subroutine set_nx(self, nx)
    class(icicles_wrap) :: self
    integer :: nx(:)
    self%nx = nx
  end subroutine set_nx


  function get_nx(self)
    class(icicles_wrap) :: self
    integer, allocatable :: get_nx(:)
    get_nx = self%nx
  end function get_nx


  function get_names(self, option)
    class(icicles_wrap), intent(in) :: self
    character(len=*), intent(in) :: option
    character(len=:), allocatable :: get_names(:)

    get_names = pack(self%params%name, optionq(self%params,option))
  end function get_names


  subroutine add(self, name, options, shape, boundary, error)
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional ::  options(:)
    integer, intent(in), optional :: shape(:)
    class(boundary_box), intent(in), target, optional :: boundary
    integer, intent(out), optional :: error

    ! local variables
    type(param) :: p
    ! temporary variables needed to circumvent the bug
    type(param), allocatable :: pp(:)

    if(present(error)) error = FPDE_STATUS_OK

    if(any(self%params%name==name)) then
       if(present(error)) error = FPDE_STATUS_ERROR
       call self%log(FPDE_LOG_ERROR,&
            "add: Could not add a duplicate entry ["//trim(name)//"]")
       return
    end if

    ! initialize entry
    p%name = name

    if(present(boundary)) then
       p%boundary => boundary
    end if

    if(present(options)) then
       p%options = options
    else
       allocate(character(len=0)::p%options(0))
    end if

    if(present(shape)) then
       p%shape = shape
    else
       p%shape = [1]
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
    call self%ic%add(name, product(p%shape))

  end subroutine add


  elemental logical function optionq(o, el)
    type(param), intent(in) :: o
    character(len=*), intent(in) :: el
    optionq = any(o%options == el)
  end function optionq


  subroutine get(self, name, vec, scal, options, boundary, shape, error)
    class(icicles_wrap), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=:), intent(out), optional, allocatable :: options(:)
    class(boundary_box), intent(out), pointer, optional :: boundary
    integer, intent(out), allocatable, optional :: shape(:)
    real, intent(out), optional, pointer :: vec(:), scal
    integer, intent(out), optional :: error

    ! local variables
    real, pointer :: v(:), s
    type(param), pointer :: p
    ! type(param) :: p
    integer :: err

    if(present(error)) error = FPDE_STATUS_OK

    call self%ic%get(name, v, s, err)
    p => first_match(self%params, name )

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
    if( present(boundary )) boundary => p%boundary
    if( present(shape    ))    shape =  p%shape

  end subroutine get


  function first_match(nvs, name)
    type(param), intent(in), target :: nvs(:)
    type(param), pointer :: first_match
    character(len=*) :: name

    integer :: i

    nullify(first_match)
    do i = 1, size(nvs)
       if( nvs(i)%name == name ) first_match => nvs(i)
    end do

  end function first_match


end module class_icicles_wrap
