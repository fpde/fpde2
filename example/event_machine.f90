program event_machine_test

  use events
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
