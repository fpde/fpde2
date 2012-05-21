program rk_explicit_abstract

   use logger_module
   use constants_module
   use class_ode_stepper_rk_explicit_abstract

   type(ode_stepper_rk_explicit_abstract) :: stepper
   integer :: err

   call set_log_level(FPDE_LOG_DEBUG)

   !> should cause an error and return FPDE_STATUS_ERROR
   call stepper%log(FPDE_LOG_INFO, "Fault test 1")
   call stepper%init(err)
   if ( err .eq. FPDE_STATUS_ERROR ) then
      call stepper%log(FPDE_LOG_DEBUG, "Test passed")
   else
      call stepper%log(FPDE_LOG_DEBUG, "Test failed")
   end if

   !> should cause an error and return FPDE_STATUS_ERROR
   call stepper%log(FPDE_LOG_INFO, "Fault test 2")
   stepper%stages=4
   stepper%dim=-3
   call stepper%init(err)
   if ( err .eq. FPDE_STATUS_ERROR ) then
      call stepper%log(FPDE_LOG_DEBUG, "Test passed")
   else
      call stepper%log(FPDE_LOG_DEBUG, "Test failed")
   end if

   !> shouldn't cause an error and return FPDE_STATUS_OK
   call stepper%log(FPDE_LOG_INFO, "Correct test 1")
   stepper%stages=4
   stepper%dim=4
   call stepper%init(err)
   if ( err .eq. FPDE_STATUS_OK ) then
      call stepper%log(FPDE_LOG_DEBUG, "Test passed")
   else
      call stepper%log(FPDE_LOG_DEBUG, "Test failed")
   end if

   !> Testing memory free

   !> shouldn't cause an error and return FPDE_STATUS_OK
   call stepper%log(FPDE_LOG_INFO, "Correct test 2")
   call stepper%free(err)
   if ( err .eq. FPDE_STATUS_OK ) then
      call stepper%log(FPDE_LOG_DEBUG, "Test passed")
   else
      call stepper%log(FPDE_LOG_DEBUG, "Test failed")
   end if

   !> shouldn't cause an error and return FPDE_STATUS_OK
   call stepper%log(FPDE_LOG_INFO, "Correct test 3")
   call stepper%free(err)
   if ( err .eq. FPDE_STATUS_OK ) then
      call stepper%log(FPDE_LOG_DEBUG, "Test passed")
   else
      call stepper%log(FPDE_LOG_DEBUG, "Test failed")
   end if

   ! call stepper%bt%test()
   !> @todo causes an error, replace class(butcher_tableu), pointer
   !! with type(butcher_tableu), add the method test to
   !! ode_stepper_rk_explicit_abstract?

end program rk_explicit_abstract
