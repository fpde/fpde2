module class_ea_common

  use class_platonic
  use class_icicles_user
  use constants_module

  private

  integer, parameter, public :: &
       STATE_START=1, &
       STATE_STOP =2, &
       STATE_ERROR=3

  integer, parameter :: valid_states(3)=[1,2,3]

  type, public, abstract, extends(platonic) :: ea_common
     private
     integer :: state = STATE_STOP
   contains
     procedure(start_stop_i), deferred :: start
     procedure(start_stop_i), deferred :: stop
     procedure, non_overridable :: try_start
     procedure, non_overridable :: try_stop

     ! generic procedure won't work here according to ifort warning #7953
     ! generic :: state => state_set, state_get
     procedure, non_overridable :: state_set
     procedure, non_overridable :: state_get
  end type ea_common

  abstract interface
     subroutine start_stop_i(self, ic, error)
       import icicles_user, ea_common
       class(ea_common), intent(inout) :: self
       class(icicles_user) :: ic
       integer, optional, intent(out) :: error
     end subroutine start_stop_i
  end interface

contains

  function state_get(self) result(r)
    class(ea_common), intent(inout) :: self
    integer :: r
    r = self%state
  end function state_get

  subroutine state_set(self, st)
    class(ea_common), intent(inout) :: self
    integer, intent(in) :: st

    if( any(valid_states==st) ) then
       self%state = st
    else
       call self%loge("state_set(): Invalid state.")
    end if
  end subroutine state_set


  subroutine try_start(self, ic, error)
    class(ea_common), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    integer :: err
    err = FPDE_STATUS_ERROR

    select case(self%state_get())
    case(STATE_STOP)

       call self%start(ic, err)
       if( err == FPDE_STATUS_OK ) then
          call self%state_set(STATE_START)
       else
          call self%loge("Error trying to start event/action.")
          call self%state_set(STATE_ERROR)
       end if

    case default
       ! ignore other states
    end select

    if(present(error)) error = err

  end subroutine try_start


  subroutine try_stop(self, ic, error)
    class(ea_common), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    integer :: err
    err = FPDE_STATUS_ERROR

    select case(self%state_get())
    case(STATE_START)

       call self%stop(ic, err)
       if( err == FPDE_STATUS_OK ) then
          call self%state_set(STATE_STOP)
       else
          call self%loge("Error trying to stop event/action.")
          call self%state_set(STATE_ERROR)
       end if

    case default
       ! ignore other states
    end select

    if(present(error)) error = err

  end subroutine try_stop



end module class_ea_common
