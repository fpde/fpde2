module class_event

  use class_ea_common
  use class_icicles_user
  use constants_module

  private

  type, public, abstract, extends(ea_common) :: event
     contains
       procedure :: start
       procedure :: stop
       procedure(test_i), deferred :: test
       procedure, non_overridable :: try_test
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

  function try_test(self, ic, error) result(r)
    class(event), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    logical :: r

    integer :: err

    r = .false.
    err = FPDE_STATUS_ERROR

    select case(self%state_get())
    case(STATE_START)
       r = self%test(ic, err)
       if( err /= FPDE_STATUS_OK ) then
          call self%loge("Error trying to test for event.")
          call self%state_set(STATE_ERROR)
       end if
    case default
       ! ignore other states
    end select

    if(present(error)) error = err

  end function try_test


end module class_event
