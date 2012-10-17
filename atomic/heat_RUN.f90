!>
!! @file   heat_RUN.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sat Aug  4 17:38:02 2012
!!
!! @brief  Atomic test for stiffness switching marcher.
!!
!!
!!
program heat_RUN

   use logger_module
   use constants_module

   use class_ode_stepper_rkv65
   use class_ode_stepper_radauIIA
   use class_ode_marcher_stiff_switch
   use class_odeiv_heat

   !> explicit and implicit steppers
   type(ode_stepper_radauIIA), target :: radau
   type(ode_stepper_rkv65), target :: verner

   !> marcher
   type(ode_marcher_stiff_switch) :: m

   !> test problem
   type(odeiv_heat) :: odeiv

   !> additional parameters
   integer :: err, i, nx, ny
   real :: time, tstep
   real, pointer :: ysol(:), usol(:,:)

   call set_log_level(FPDE_LOG_ERROR)

   nx = 12
   ny = 12

   odeiv % nx = nx
   odeiv % ny = ny

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

   m%c%eps_abs = 1.0e-14
   m%c%eps_rel = 1.0e-10

   !> Copy initial values from ode sys
   allocate(ysol(m%dim))
   ysol = odeiv % y0

   !> use the apply method

   call m%log(FPDE_LOG_INFO, "call m%apply")

   time = odeiv%t(0)
   tstep = 1.0e-7
   odeiv%t(1) = 1000.5
   i=1

   usol(1:nx,1:ny) => ysol

   do while ( time < odeiv%t(1) )
      ! do i=1,ny
      !    print '(*(E10.2))', usol(:,i)
      ! end do
      ! print *, ' '
      ! print *, ' '

      call m%apply(sys=odeiv%sys, y=ysol, t=time, t1=odeiv%t(1), h=tstep, error=err)

      print '(I7,3(es22.14),A,A)', i, time, usol((nx+1)/2,(ny+1)/2), tstep, ' ', trim(m%s%name)

      i = i + 1
      if ( err /= FPDE_STATUS_OK ) then
         call m%log(FPDE_LOG_ERROR, "m%apply failed")
         stop
      end if
   end do




   ! @todo free memory

end program heat_RUN
