module class_event_logical

  use class_event
  use class_event_and
  use class_event_or
  use class_event_not

  private

  public :: operator(.and.), operator(.or.), operator(.not.)

  interface operator(.and.)
     module procedure ev_and
  end interface operator(.and.)

  interface operator(.or.)
     module procedure ev_or
  end interface operator(.or.)

  interface operator(.not.)
     module procedure ev_not
  end interface operator(.not.)

contains

  function ev_and(op1, op2) result(r)
    class(event), intent(in), target :: op1, op2
    class(event), pointer :: r
    r => event_and(op1,op2)
  end function ev_and

  function ev_or(op1, op2) result(r)
    class(event), intent(in), target :: op1, op2
    class(event), pointer :: r
    r => event_or(op1,op2)
  end function ev_or

  function ev_not(op1) result(r)
    class(event), intent(in), target :: op1
    class(event), pointer :: r
    r => event_not(op1)
  end function ev_not

end module class_event_logical
