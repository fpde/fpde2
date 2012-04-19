program simple

   use class_ode_stepper_explicit

   type(ode_stepper_explicit) :: s

   call s%init()
!   call s%apply()
   call s%reset()
   call s%free()

end program simple
