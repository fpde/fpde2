program explicit

   use class_ode_stepper_rkv65
   use ieee_arithmetic

   type(ode_stepper_rkv65) :: s

   s % dim = 2

   call s%init()

   call s%consistency_test()

   call s%reset()
   call s%free()

end program explicit
