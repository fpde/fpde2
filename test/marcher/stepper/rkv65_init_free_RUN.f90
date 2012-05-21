!>
!! @file   rkv65_RUN.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu May 10 16:22:48 2012
!!
!! @brief  Memory allocation/free test for the explicit
!! Runge-Kutta stepper. Sequence of init/free/reset method
!! calls, with Butcher tableu consistency test.
!!
program rkv65_RUN

   use logger_module
   use constants_module
   use class_ode_stepper_rkv65

   type(ode_stepper_rkv65) :: s
   integer :: err

   call set_log_level(FPDE_LOG_DEBUG)

   s % dim = 100

   call s%log(FPDE_LOG_INFO, "call s%init")
   call s%init(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "init test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "init test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%bt%test")
   call s%bt%test(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "Butcher tableu test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "Butcher tableu test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%reset")
   call s%reset(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "reset test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "reset test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%free")
   call s%free(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "free test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "free test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%reset")
   call s%reset(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "reset test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "reset test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%init")
   call s%init(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "init test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "init test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%free")
   call s%free(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "free test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "free test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%free")
   call s%free(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "free test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "free test passed")
   end if

end program rkv65_RUN
