!>
!! @file   function_registry.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sun Apr  8 13:11:21 2012
!!
!! @brief
!!
!!
!!
!! @todo implement function_registry using lists
module class_func_registry

  use logger_module
  use constants_module
  use class_platonic
  use class_function
  use flu_module
  use flu_get_module
  use helper_module

  private

  !> regenerates the pointers of all named_vectors
  !! in icicles based on the icicles_registry
  type, public, extends(platonic) :: func_registry
     !> register table
     type(func) :: func(MAX_FUNC)
     !> number of registers already added
     integer :: n_func = 0
     !> @todo, this is additional data not required for some mesh
     !! arrangements, it is written by the solver
     integer, pointer :: nx(:) => null()
   contains
     ! procedure :: add
     ! procedure :: create_icicles
     ! procedure :: set_pointers
     ! procedure :: info
     ! procedure :: sort
     procedure :: set_nx
     procedure :: init
     procedure :: get
     procedure :: insert
     procedure :: delete
     procedure :: from_lua
     procedure, private :: d_create
  end type func_registry

contains

  subroutine set_nx(p, nx)
    class(func_registry) :: p
    integer, intent(in) :: nx(:)

    if( associated(p%nx) ) deallocate(p%nx)

    allocate(p%nx(size(nx)))

    p%nx = nx

  end subroutine set_nx


  subroutine from_lua(p, l, error)
    class(func_registry) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    class(func), pointer :: f => null()
    integer :: i, n, dummy
    integer :: err = FPDE_STATUS_OK, err2
    character(len=NAME_LEN) :: msg

    allocate(f)

    if( lua_type(l,-1) == C_LUA_TTABLE ) then
       n = lua_rawlen(l,-1)

       do i = 1, n
          call lua_pushinteger(l,i)
          call lua_gettable(l,-2)
          call f%from_lua(l, err2)
          call p%insert(f)
          call lua_pop(l,1)
          if(err2 /= FPDE_STATUS_OK) err = err2
       end do
    end if

    deallocate(f)

    if(present(error)) error = err

  end subroutine from_lua


  subroutine get(p, name, vec, scal, f, index, alpha, error)
    class(func_registry), target :: p
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: alpha(:)
    real, pointer, optional, intent(out) :: vec(:), scal
    integer, optional, intent(out) :: index
    type(func), pointer, optional, intent(out) :: f
    integer, optional, intent(out) :: error

    integer :: i
    type(func), pointer :: f_temp => null()
    character(len=:), pointer :: names(:) => null()

    names => p%func(1:p%n_func)%name

    if(present(error)) error = FPDE_STATUS_OK

    i = findloc_first(names,name)
    if( present(alpha) ) then
       i = findloc_first( names, f%d_name(alpha) )
    end if

    if( i > p%n_func) then
       if(present(error)) error = FPDE_STATUS_ERROR
       call p%log(FPDE_LOG_ERROR,&
            "There is no function named ["//name//"] in the registry")
       return
    end if

    f_temp => p%func(i)

    if( present( f   ) ) f   => f_temp
    if( present( vec ) ) vec => f_temp%val
    if( present( scal) ) scal=> f_temp%val(i)
    if( present( index)) index= i
  end subroutine get


  subroutine insert(p, f, name, index, error)
    class(func_registry), target :: p
    character(len=*), optional, intent(in) :: name
    integer, optional, intent(in) :: index
    type(func), intent(in) :: f
    integer, optional, intent(out) :: error

    integer :: i, idx, err

    if(present(error)) error = FPDE_STATUS_ERROR

    if( any(p%func%name == f%name) ) then
       call p%log(FPDE_LOG_INFO,&
            "Request to add a duplicate entry ["//trim(f%name)//"]")
       return
    end if

    if(present(name)) then
       call p%get(name, index = idx, error = err)
       if( err /= FPDE_STATUS_OK ) return

       idx = idx + 1

    else if(present(index)) then
       idx = index + 1

    else
       idx = p%n_func + 1

    end if

    do i = p%n_func+1, idx+1, -1
       p%func(i) = p%func(i-1)
    end do

    p%n_func = p%n_func + 1
    p%func(idx) = f

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine insert


  subroutine delete(p, name, index, error)
    class(func_registry), target :: p
    character(len=*), optional, intent(in) :: name
    integer, optional, intent(in) :: index
    integer, optional, intent(out) :: error

    integer :: i, idx, err

    if(present(error)) error = FPDE_STATUS_ERROR

    if(present(name)) then
       call p%get(name, index = idx, error = err)
       if( err /= FPDE_STATUS_OK ) return
       idx = idx

    else if(present(index)) then
       idx = index

    else
       return

    end if

    do i = idx, p%n_func
       p%func(i) = p%func(i+1)
    end do

    p%n_func = p%n_func - 1

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine delete

  subroutine d_create(p, idx, error)
    class(func_registry), target :: p
    integer, intent(in) :: idx
    integer, optional, intent(out) :: error

    type(func), pointer :: f
    type(func) :: d_f
    integer :: err, i
    character(len=:), pointer :: alpha(:)

    if(present(error)) error = FPDE_STATUS_OK

    f => p%func(idx)

    d_f%spatial = .false.
    d_f%evolved = .false.

    do i = 1, size(f%derivatives,2)
       alpha => f%derivatives(:,i)
       d_f%name = f%d_name(alpha)
       call p%insert(d_f)
    end do

  end subroutine d_create

  subroutine init(p, error)
    class(func_registry) :: p
    integer, optional, intent(out) :: error

    integer :: n_func, err, err2, i

    err = FPDE_STATUS_OK

    if( p%name == "" ) p%name = "function_registry"

    n_func = p%n_func

    do i = 1, n_func
       call p%d_create(i,error = err2)
       if( err2 /= FPDE_STATUS_OK ) err = err2
    end do

    if(present(error)) error = err

  end subroutine init

end module class_func_registry
