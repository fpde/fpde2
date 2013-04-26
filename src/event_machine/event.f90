module class_event

  use class_platonic
  use class_icicles_user
  use constants_module

  private

  type, public, abstract, extends(platonic) :: event
     contains
       procedure :: start
       procedure :: stop
       procedure(test_i), deferred :: test
  end type event

  interface
     function test_i(self, ic, error) result(r)
       import event, icicles_user
       class(event), intent(inout) :: self
       class(icicles_user) :: ic
       integer, optional, intent(out) :: error
       logical :: r
     end function test_i
  end interface

contains

  subroutine start(self, ic, error)
    class(event), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine start

  subroutine stop(self, ic, error)
    class(event), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine stop

end module class_event
