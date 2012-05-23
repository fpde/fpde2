program radauIIA_apply_RUN

   use logger_module
   use constants_module
   use class_odeiv_van_der_Pol
   use class_ode_stepper_radauIIA

   type(odeiv_vdp) :: vdp
   type(ode_stepper_radauIIA) :: s
   integer :: err, i
   real :: time, tstep
   real, pointer :: ysol(:)

   call set_log_level(FPDE_LOG_ERROR)

   !> Initialize ode sys to get it's dimension
   call vdp%init()

   s % dim = vdp % sys % dim

   allocate(ysol(s%dim))

   !> Copy initial values from ode sys
   ysol = vdp % y0

   call s%log(FPDE_LOG_INFO, "call s%init")
   call s%init(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "init test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "init test passed")
   end if

   call s%log(FPDE_LOG_INFO, "call s%apply")

   time = vdp%t(0)
   tstep = 0.1e-6

   ! call s%apply(sys = vdp%sys, y = ysol, t = time, h = tstep, error=err)

   ! i=0
   ! do while (i<10)
   !    call s%apply(sys = vdp%sys, y = ysol, t = time, h = tstep, error=err)
   !    time = time + tstep
   !    i = i + 1
   !    print '(3(es))', time, ysol(1:2)
   ! end do

   do while (time < vdp%t(1))
      call s%apply(sys = vdp%sys, y = ysol, t = time, h = tstep, error=err)
      time = time + tstep
      print 11, time, ysol(1:2)
   end do

11 FORMAT (2000(es22.14))

   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "free apply failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "free apply passed")
   end if


   call s%log(FPDE_LOG_INFO, "call s%free")
   call s%free(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "free test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "free test passed")
   end if

   call vdp%free()

   deallocate(ysol)

end program radauIIA_apply_RUN
