module class_action

  use class_platonic
  use class_icicles_user
  use constants_module

  private

  type, public, abstract, extends(platonic) :: action
     contains
       procedure :: start
       procedure :: stop
       procedure(execute_i), deferred :: execute
  end type action

  abstract interface
     subroutine execute_i(ac, ic, error)
       import action
       import icicles_user
       class(action) :: ac
       class(icicles_user) :: ic
       integer, optional, intent(out) :: error
     end subroutine execute_i
  end interface

contains

  subroutine start(ac, ic, error)
    class(action) :: ac
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine start

  subroutine stop(ac, ic, error)
    class(action) :: ac
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine stop

end module class_action
