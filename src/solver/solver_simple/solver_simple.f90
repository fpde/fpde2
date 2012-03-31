module class_solver_simple

  use constants_module
  use logger_module
  use class_solver
  use icicles_module
  use icicles_registry_module

  private

  type, public, extends(solver) :: solver_simple
     integer :: nx = 0
   contains
     procedure :: init
     procedure :: free
     procedure :: info
     procedure :: from_lua => solver_from_lua
  end type solver_simple

contains

  subroutine init(s, error)
    class(solver_simple) :: s
    integer, optional, intent(out) :: error

    integer :: err

    if(present(error)) error = FPDE_STATUS_ERROR

    if(s%name == "") then
       s%name = "solver_simple"
    end if

    call s%solver%init(err)

    if( err /= FPDE_STATUS_OK ) then
       return
    end if

    print *, "Initializing solver_simple"

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine init

  subroutine free(s)
    class(solver_simple) :: s

  end subroutine free

  subroutine solver_from_lua(s, l, error)
    use flu_module
    use flu_get_module

    class(solver_simple) :: s
    type(flu) :: l
    integer, intent(out), optional :: error

    integer :: err

    if(present(error)) error = FPDE_STATUS_ERROR

    call s%solver%from_lua( l, error = err)

    if(err /= FPDE_STATUS_OK) then
       return
    end if

    call flu_get_scalar_integer(l, s%nx, "nx")

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine solver_from_lua

  subroutine info(s)
    class(solver_simple) :: s

    call s%solver%info()

    print *, "Calling solver_simple%info():"
    print *, "s%nx=", s%nx

  end subroutine info


end module class_solver_simple
