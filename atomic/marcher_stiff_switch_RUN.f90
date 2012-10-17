!>
!! @file   marcher_stiff_switch_RUN.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sat Aug  4 17:38:02 2012
!!
!! @brief  Atomic test for stiffness switching marcher.
!!
!!
!!
program marcher_stiff_switch_RUN

   use logger_module
   use constants_module

   use class_ode_stepper_rkv65
   use class_ode_stepper_radauIIA
   use class_ode_marcher_stiff_switch
   use class_odeiv_van_der_Pol

   !> explicit and implicit steppers
   type(ode_stepper_radauIIA), target :: radau
   type(ode_stepper_rkv65), target :: verner

   !> marcher
   type(ode_marcher_stiff_switch) :: m

   !> test problem
   type(odeiv_vdp) :: odeiv

   !> additional parameters
   integer :: err, i
   real :: time, tstep
   real, pointer :: ysol(:)

   call set_log_level(FPDE_LOG_ERROR)

   !> Initialize ode sys to get it's dimension
   call odeiv%init()

   !> Initializing marcher
   m % dim = odeiv % sys % dim
   m % stepper_explicit => verner
   m % stepper_implicit => radau
   call m%init(err)

   if ( err .ne. FPDE_STATUS_OK ) then
      call m%log(FPDE_LOG_ERROR, "m%init test failed")
      stop
   else
      call m%log(FPDE_LOG_INFO, "m%init test passed")
   end if

   !> marcher settings
   m%detect_stiff = .true.
   m%detect_nonstiff = .true.

   m%c%eps_abs = 1.0e-7
   m%c%eps_rel = 1.0e-7

   !> Copy initial values from ode sys
   allocate(ysol(m%dim))
   ysol = odeiv % y0
   odeiv % mu = 1.0/sqrt(0.003)

   !> use the apply method

   call m%log(FPDE_LOG_INFO, "call m%apply")

   time = odeiv%t(0)
   tstep = 1.0e-7
   odeiv%t(1) = 2.5
   i=1

   do while ( time < odeiv%t(1) )
      call m%apply(sys=odeiv%sys, y=ysol, t=time, t1=odeiv%t(1), h=tstep, error=err)

      ! print '(6(es22.14),I3,I3,es22.14)', i*1., time, ysol(1:2), &
      !      tstep, 1.0*s%k_last, s%jac_recompute, m%failed_steps, s%last_yerr_norm

      print '(I7,3(es22.14),A,A)', i, time, ysol(1:2), ' ', trim(m%s%name)

      i = i + 1
      if ( err /= FPDE_STATUS_OK ) then
         call m%log(FPDE_LOG_ERROR, "m%apply failed")
         stop
      end if
   end do

   ! @todo free memory

end program marcher_stiff_switch_RUN
