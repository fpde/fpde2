module class_event_not

  use class_event
  use class_event_logical_base
  use class_icicles_user
  use constants_module

  private

  type, public, extends(event_logical_base) :: event_not
   contains
     procedure :: test
  end type event_not

  interface event_not
     module procedure :: ev_not
  end interface event_not

contains


  function ev_not(op1) result(r)
    class(event), target, intent(in) :: op1
    type(event_not), pointer :: r
    allocate(r)
    r%op1 => op1
  end function ev_not


  function test(ev, ic, error) result(r)
    class(event_not) :: ev
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    logical :: r
    integer :: err1

    if(present(error)) error = FPDE_STATUS_OK

    r = .not. ev%op1%test(ic, err1)

    if( err1 /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
    end if

  end function test


end module class_event_not
