module class_event_machine

  use class_platonic
  use class_event
  use class_action
  use class_icicles_user
  use constants_module

  private

  type :: action_p
     class(action), pointer :: val
  end type action_p

  type :: event_p
     class(event), pointer :: val
     type(action_p), allocatable :: actions(:)
  end type event_p

  type, public, extends(platonic) :: event_machine
     type(event_p), allocatable :: events(:)
   contains
     procedure :: add
     procedure :: execute
  end type event_machine

  interface event_machine
     module procedure :: em_new
  end interface event_machine

contains

  function em_new() result(r)
    type(event_machine), pointer :: r
    allocate(r)
    allocate(r%events(0))
  end function em_new

  subroutine add(em, ev, ac)
    class(event_machine) :: em
    class(event), target :: ev
    class(action), target :: ac

    ! variables used to keep temporary arrays, used because of ifort
    ! bug
    type(action_p), allocatable :: acs_(:)
    type(event_p), allocatable :: evs_(:)
    integer :: i

    ! if event is already in the table we only add an action to it
    associate(evs => em%events)
      do i = 1, size(evs)
         if( associated(evs(i)%val, ev) ) then
            ! ifort bug prevents me from doing the one-line automatic
            ! reallocation
            ! evs(i)%actions = [evs(i)%actions, action_p(val=ac)]
            acs_           = evs(i)%actions
            evs(i)%actions = [acs_, action_p(val=ac)]
            return
         end if
      end do
    end associate

    ! if there is no such event we have to add it to the table
    ! em%events = [em%events, event_p(val=ev, actions=[action_p(val=ac)])]
    ! ifort bug prevents me from doing the one-line automatic
    ! reallocation
    evs_ = em%events
    em%events = [evs_, event_p(val=ev, actions=[action_p(val=ac)])]

  end subroutine add

  !> Tries to test the events and execute actions assigned to them. If
  !! any of the events or actions produces an error, the function
  !! returns immediately after the error skipping remaining events and
  !! actions.
  !!
  !! @param ic icicles passed to tests and actions
  !! @param error
  !!
  subroutine execute(em, ic, error)
    class(event_machine) :: em
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    integer :: i, j, err
    logical :: test

    if(present(error)) error = FPDE_STATUS_OK
    err = FPDE_STATUS_OK

    associate(evs => em%events)
      events_loop: do i = 1, size(evs)
         test = evs(i)%val%test(ic, err)

         if( err /= FPDE_STATUS_OK ) then
            exit events_loop
         end if

         if( test ) then
            associate(acs => evs(i)%actions)
              actions_loop: do j = 1, size(acs)
                 call acs(j)%val%execute(ic, err)
                 if( err /= FPDE_STATUS_OK ) exit events_loop
              end do actions_loop
            end associate
         end if
      end do events_loop

    end associate

    if( err /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
       call em%loge("Error occured while executing one of the events or actions,&
            & skipping remaining tests and actions.")
       return
    end if

  end subroutine execute

end module class_event_machine
