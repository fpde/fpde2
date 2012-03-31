module class_solver

  use constants_module
  use logger_module
  use icicles_module
  use icicles_registry_module

  private

  character(len=NAME_LEN) :: TAG_NAME = "name"

  type, public, extends(named) :: solver
     type(icicles), pointer :: ic => null()
     type(icicles_registry), pointer :: icreg => null()
   contains
     procedure :: init
     procedure :: free
     ! procedure :: get => get_vector, get_scalar
     ! procedure :: set => set_vector, set_scalar
     procedure :: info
     procedure :: from_lua => solver_from_lua
  end type solver

contains

  subroutine init(s, error)
    class(solver) :: s
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK

    if(s%name == "") then
       s%name = "solver"
    end if

    print *, "Initializing solver"

  end subroutine init

  subroutine free(s)
    class(solver) :: s

  end subroutine free

  subroutine solver_from_lua(s, l, error)
    use flu_module
    use flu_get_module

    class(solver) :: s
    type(flu) :: l
    integer, intent(out), optional :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    call flu_get_scalar_character(l, s%name, TAG_NAME)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine solver_from_lua

  subroutine info(s)
    class(solver) :: s

    print *, "Calling solver%info():"
    print *, "solver%name=", trim(s%name)

  end subroutine info


end module class_solver
