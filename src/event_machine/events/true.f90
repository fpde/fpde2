module class_event_true

  use class_event
  use constants_module
  use class_icicles_user

  private

  type, public, extends(event) :: event_true
     private
     logical :: val = .true.
   contains
     procedure :: test
  end type event_true

  interface event_true
     module procedure et_new
  end interface event_true

contains

  function et_new(val) result(r)
    logical, optional, intent(in) :: val
    type(event_true), pointer :: r

    allocate(r)
    if(present(val)) r%val = val
  end function et_new


  function test(ev, ic, error) result(r)
    class(event_true), intent(inout) :: ev
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    logical :: r
    if(present(error)) error = FPDE_STATUS_OK
    r = ev%val
  end function test

end module class_event_true
