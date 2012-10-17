module class_generic_function_dummy

  use class_generic_function
  use class_icicles_user_
  use constants_module

  private

  type, public, extends(generic_function) :: generic_function_dummy
   contains
     procedure :: call
  end type generic_function_dummy

contains

  subroutine call(this, solver, error)
    class(generic_function_dummy) :: this
    class(icicles_user), target :: solver
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine call

end module class_generic_function_dummy
