module class_event_or

  use class_event
  use class_event_logical_base
  use class_icicles_user
  use constants_module

  private

  type, public, extends(event_logical_base) :: event_or
   contains
     procedure :: test
  end type event_or

  interface event_or
     module procedure :: ev_or
  end interface event_or

contains

  function ev_or(op1, op2) result(r)
    class(event), target, intent(in) :: op1, op2
    type(event_or), pointer :: r
    allocate(r)
    r%op1 => op1
    r%op2 => op2
  end function ev_or


  function test(self, ic, error) result(r)
    class(event_or) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    logical :: r
    integer :: err1, err2

    if(present(error)) error = FPDE_STATUS_OK

    r = self%op1%test(ic, err1) .or. self%op2%test(ic, err2)

    if( err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
    end if

  end function test

end module class_event_or
