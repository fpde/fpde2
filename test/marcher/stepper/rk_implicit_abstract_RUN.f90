program rk_implicit_abstract

   use logger_module
   use constants_module
   use class_ode_stepper_rk_implicit_abstract

   type(ode_stepper_rk_implicit_abstract) :: stepper
   integer :: err
   real, target :: r(2,2)
   real, pointer :: p(:)

   r(1,:) = [ 1., 2. ]
   r(2,:) = [ 3., 4. ]

   p => r(1,:)
   print *, p


   call set_log_level(FPDE_LOG_DEBUG)

   !> should cause an error and return FPDE_STATUS_ERROR
   call stepper%log(FPDE_LOG_INFO, "Fault test 1")
   call stepper%init(err)
   if ( err .eq. FPDE_STATUS_ERROR ) then
      call stepper%log(FPDE_LOG_DEBUG, "Test passed")
   else
      call stepper%log(FPDE_LOG_DEBUG, "Test failed")
   end if

end program rk_implicit_abstract
