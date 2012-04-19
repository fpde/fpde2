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
   contains
     ! procedure :: add
     ! procedure :: create_icicles
     ! procedure :: set_pointers
     ! procedure :: info
     ! procedure :: sort
     procedure :: get
     procedure :: insert
     procedure :: delete
     procedure :: from_lua
  end type func_registry

contains

  subroutine from_lua(p, l, error)
    class(func_registry) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    class(func), pointer :: f
    integer :: i, n, dummy
    integer :: err = FPDE_STATUS_OK, err2
    character(len=NAME_LEN) :: msg

    if( lua_type(l,-1) == C_LUA_TTABLE ) then
       n = lua_rawlen(l,-1)
       p%n_func = n

       do i = 1, n
          call lua_pushinteger(l,i)
          call lua_gettable(l,-2)
          call p%func(i)%from_lua(l, err2)
          call lua_pop(l,1)
          if(err2 /= FPDE_STATUS_OK) err = err2
       end do
    end if

    if(present(error)) error = err

  end subroutine from_lua


  subroutine get(p, name, vec, scal, f, index, error)
    class(func_registry), target :: p
    character(len=*), intent(in) :: name
    real, pointer, optional, intent(out) :: vec(:), scal
    integer, optional, intent(out) :: index
    type(func), pointer, optional, intent(out) :: f
    integer, optional, intent(out) :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_OK

    i = findloc_first(p%func(1:p%n_func)%name,name)
    if( i > p%n_func) then
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( present( f   ) ) f   => p%func(i)
    if( present( vec ) ) vec => p%func(i)%val
    if( present( scal) ) scal=> p%func(i)%val(i)
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


end module class_func_registry
