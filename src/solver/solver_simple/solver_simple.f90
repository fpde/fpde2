module class_solver_simple

  use constants_module
  use logger_module
  use class_solver
  use icicles_module
  use icicles_registry_module

  private

  character(len=NAME_LEN), parameter :: TAG_NX = "nx"

  type, public, extends(solver) :: solver_simple
     integer :: nx = 0
   contains
     procedure :: info
     procedure :: from_lua
  end type solver_simple

contains

  subroutine from_lua(p, l, error)
    use flu_module
    use flu_get_module

    class(solver_simple) :: p
    type(flu) :: l
    integer, intent(out), optional :: error

    integer :: err

    if(present(error)) error = FPDE_STATUS_ERROR

    call p%solver%from_lua( l, error = err)

    if(err /= FPDE_STATUS_OK) then
       return
    end if

    call flu_get_atomic(l, val = p%nx, key = TAG_NX)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine from_lua

  subroutine info(s)
    class(solver_simple) :: s

    call s%solver%info()

    print *, "Calling solver_simple%info():"
    print *, "s%nx=", s%nx

  end subroutine info


end module class_solver_simple
