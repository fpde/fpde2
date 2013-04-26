module class_action

  use class_ea_common
  use class_icicles_user
  use constants_module

  private

  type, public, abstract, extends(ea_common) :: action
     contains
       procedure :: start
       procedure :: stop
       procedure(execute_i), deferred :: execute
       procedure, non_overridable :: try_execute
  end type action

  abstract interface
     subroutine execute_i(self, ic, error)
       import action
       import icicles_user
       class(action) :: self
       class(icicles_user) :: ic
       integer, optional, intent(out) :: error
     end subroutine execute_i
  end interface

contains

  subroutine start(self, ic, error)
    class(action) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine start

  subroutine stop(self, ic, error)
    class(action) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine stop

  subroutine try_execute(self, ic, error)
    class(action) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    integer :: err
    err = FPDE_STATUS_ERROR

    select case(self%state_get())
    case(STATE_START)

       call self%execute(ic, err)
       if( err /= FPDE_STATUS_OK ) then
          call self%loge("Error trying to execute action.")
          call self%state_set(STATE_ERROR)
       end if

    case default
       ! ignore other states
    end select

    if(present(error)) error = err

  end subroutine try_execute

end module class_action
