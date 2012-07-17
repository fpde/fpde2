program radauIIA_marcher_RUN

   use logger_module
   use constants_module
   use class_odeiv_harm
   use class_odeiv_van_der_Pol

   use class_ode_stepper_rkv65
   use class_ode_stepper_radauIIA

   use class_ode_marcher_simple

   type(odeiv_vdp) :: odeiv
   ! type(odeiv_harm) :: odeiv

   type(ode_stepper_radauIIA), target :: s
   ! type(ode_stepper_rkv65), target :: s

   type(ode_marcher_simple) :: m
   integer :: err, i
   real :: time, tstep
   real, pointer :: ysol(:)

   call set_log_level(FPDE_LOG_ERROR)

   !> Initialize ode sys to get it's dimension
   call odeiv%init()

   s % dim = odeiv % sys % dim

   allocate(ysol(s%dim))

   !> Copy initial values from ode sys
   ysol = odeiv % y0

   call s%log(FPDE_LOG_INFO, "call s%init")
   call s%init(err)
   if ( err .ne. FPDE_STATUS_OK ) then
      call s%log(FPDE_LOG_ERROR, "s%init test failed")
      stop
   else
      call s%log(FPDE_LOG_INFO, "s%init test passed")
   end if

   !> initializing marcher
   call m%log(FPDE_LOG_INFO, "call m%init")
   m % dim = s % dim
   m % s => s

   call m%init(err)

   if ( err .ne. FPDE_STATUS_OK ) then
      call m%log(FPDE_LOG_ERROR, "m%init test failed")
      stop
   else
      call m%log(FPDE_LOG_INFO, "m%init test passed")
   end if

   !> use the apply method
   call m%log(FPDE_LOG_INFO, "call m%apply")
   time = odeiv%t(0)
   tstep = 1.0e-6

   ! do i=1,200
   !    call s%apply(sys=odeiv%sys, y=ysol, t=time, h=tstep, error=err)
   !    time = time + tstep
   !    print '(200(es22.14))', i*1., time, ysol(1:2), tstep
   !    if ( err /= FPDE_STATUS_OK ) then
   !       print *, "ERROR: ", err
   !       stop
   !    end if
   ! end do

   i=1
   odeiv%t(1) = 2.0
   do while ( time < odeiv%t(1) )
      call m%apply(sys=odeiv%sys, y=ysol, t=time, t1=odeiv%t(1), h=tstep, error=err)
      print '(200(es22.14))', i*1., time, ysol(1:2), tstep, 1.0*s%k_last
      i = i + 1
      if ( err /= FPDE_STATUS_OK ) then
         print *, "ERROR: ", err
         stop
      end if
   end do



   if ( err .ne. FPDE_STATUS_OK ) then
      call m%log(FPDE_LOG_ERROR, "m%apply test failed")
      stop
   else
      call m%log(FPDE_LOG_INFO, "m%apply test passed")
   end if

   stop



   call s%log(FPDE_LOG_INFO, "call s%apply")

   time = odeiv%t(0)
   tstep = 0.1e-6

   ! call s%apply(sys = odeiv%sys, y = ysol, t = time, h = tstep, error=err)

   ! i=0
   ! do while (i<10)
   !    call s%apply(sys = odeiv%sys, y = ysol, t = time, h = tstep, error=err)
   !    time = time + tstep
   !    i = i + 1
   !    print '(3(es))', time, ysol(1:2)
   ! end do

   do while (time < odeiv%t(1))
      call s%apply(sys = odeiv%sys, y = ysol, t = time, h = tstep, error=err)
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

   call odeiv%free()

   deallocate(ysol)

end program radauIIA_marcher_RUN
