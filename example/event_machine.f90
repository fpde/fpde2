program event_machine_test

  ! those two modules are not used explicitely, but ifort is so buggy
  ! that it won't compile without them
  use class_event
  use class_action


  use class_event_machine
  use class_event_true
  use class_event_logical
  use class_action_hello
  use class_icicles
  use class_icicles_implementation
  use class_generic_function_dummy

  type(event_machine), pointer :: em
  class(icicles), pointer :: ic
  class(event), pointer :: evt, evf

  ic => icicles_implementation(init = generic_function_dummy())

  em  => event_machine()
  evt => event_true(val = .true. )
  evf => event_true(val = .false.)

  call em%add(evf .or.  evt, action_hello(text = "T or F"))
  call em%add(evt .and. evt, action_hello(text = "T and T"))
  call em%add(.not. evf, action_hello(text = "not F"))
  call em%start(ic)
  call em%execute(ic)
  call em%stop(ic)

end program event_machine_test
