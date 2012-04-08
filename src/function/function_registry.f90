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

  !> maximal number of registry entries
  integer, parameter, private :: MAX_FUNC = 1000

  private

  !> regenerates the pointers of all named_vectors
  !! in icicles based on the icicles_registry
  type, public, extends(platonic) :: func_registry
     !> register table
     ! type(func) :: entries(MAX_ENTRIES)
     type(func) :: func(MAX_FUNC)
     !> number of registers already added
     integer :: n_func = 0
   contains
     ! procedure :: add
     ! procedure :: create_icicles
     ! procedure :: set_pointers
     ! procedure :: info
     procedure :: from_lua
  end type func_registry

contains

  subroutine from_lua(p, l, error)
    class(func_registry) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    class(func), pointer :: f
    integer :: i = 0, dummy
    integer :: err

    call p%log(FPDE_LOG_DEBUG, "calling func_registry%from_lua")

    if(present(error)) error = FPDE_STATUS_ERROR

    call lua_pushnil(l)

    do while( lua_next(l,-2) /= 0 )
       if( lua_type(l,-1) == C_LUA_TTABLE ) then
          i = i+1
          call p%func(i)%from_lua(l, error = err)
       end if
       call lua_pop(l,1)
    end do

    p%n_func = i

    call lua_pop(l,1)

  end subroutine from_lua

end module class_func_registry
