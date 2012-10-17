!!!>
!! @file   stepper_adams_RUN.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Aug  9 15:49:32 2012
!!
!! @brief  Atomic test of Adams stepper.
!!
!!
!!
program stepper_adams_RUN

   use logger_module
   use constants_module

   use class_ode_stepper_adams
   use class_ode_marcher_simple
   use class_odeiv_harm

   !> explicit and implicit steppers
   type(ode_stepper_adams), target :: adams

   !> marcher
   type(ode_marcher_simple) :: m

   !> test problem
   type(odeiv_harm) :: odeiv

   !> additional parameters
   integer :: err, i
   real :: time, tstep
   real, pointer :: ysol(:)

   call set_log_level(FPDE_LOG_ERROR)

   !> Initialize ode sys to get it's dimension
   call odeiv%init()

   !> Initializing stepper
   adams % dim = odeiv % sys % dim

   !> Initializing marcher
   m % dim = odeiv % sys % dim
   m % s => adams
   call m%init(err)

   if ( err .ne. FPDE_STATUS_OK ) then
      call m%log(FPDE_LOG_ERROR, "m%init test failed")
      stop
   else
      call m%log(FPDE_LOG_INFO, "m%init test passed")
   end if

   m%c%eps_abs = 1.0e-10
   m%c%eps_rel = 1.0e-10

   !> Copy initial values from ode sys
   allocate(ysol(m%dim))
   ysol = [1.0,0.0]

   !> use the apply method

   call m%log(FPDE_LOG_INFO, "call m%apply")

   time = odeiv%t(0)
   tstep = 0.1
   odeiv%t(1) = 10.0
   i=1

   do while ( time <= odeiv%t(1) )
      call adams%apply(sys=odeiv%sys, y=ysol, t=time, h=tstep, error=err)
      print '(I7,3(es22.14),A,A)', i, time, ysol(1:2), ' ', trim(m%s%name)
      time = time + tstep
      i = i + 1
   end do

   ! do while ( time < odeiv%t(1) )
   !    call m%apply(sys=odeiv%sys, y=ysol, t=time, t1=odeiv%t(1), h=tstep, error=err)

   !    ! print '(6(es22.14),I3,I3,es22.14)', i*1., time, ysol(1:2), &
   !    !      tstep, 1.0*s%k_last, s%jac_recompute, m%failed_steps, s%last_yerr_norm

   !    print '(I7,3(es22.14),A,A)', i, time, ysol(1:2), ' ', trim(m%s%name)

   !    i = i + 1
   !    if ( err /= FPDE_STATUS_OK ) then
   !       call m%log(FPDE_LOG_ERROR, "m%apply failed")
   !       stop
   !    end if
   ! end do

   ! @todo free memory

end program stepper_adams_RUN
