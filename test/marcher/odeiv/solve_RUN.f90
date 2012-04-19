!>
!! @file   solve_RUN.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 19 12:59:47 2012
!!
!! @brief  ODE stepper simple odeiv solve test.
!!
!!
!!
program solve_odeiv

   use logger_module
   use constants_module
   use class_odeiv_generic
   use class_odeiv_factory
   use class_ode_stepper
   use class_ode_stepper_factory
   use class_ode_marcher

   class(odeiv_generic), pointer :: odeiv
   class(ode_stepper), pointer :: s
   type(ode_marcher) :: m
   integer :: i
   real, pointer :: y(:)
   real :: h

   odeiv => odeiv_new( 'HARM' )
   call odeiv%init()

   call odeiv%info()

   s => ode_stepper_new( 'rkv65' )
   s % dim = odeiv % sys % dim
   call s%init()
   m % dim = odeiv % sys % dim
   call m%init()

   allocate( y(odeiv % sys % dim) )

   h = odeiv % h
   y = odeiv % y0

   print '(3(A,TR22))', 't', 'y(1)', 'y(2)'
   write(*,'(100(A))',advance='yes')  ('-', i=1,22*3)

   do while ( odeiv % t(0) .lt. odeiv % t(1) )

      call m%apply( s = s, sys = odeiv % sys, t = odeiv % t(0), t1 = odeiv % t(1), &
           &        h = h, y = y )

      if ( m % status /= FPDE_STATUS_OK ) then
         call m%log(FPDE_LOG_ERROR, "marcher % status /= FPDE_STATUS_OK")
      end if

      write(*,'(3(ES22.14))') odeiv % t(0), y(1), y(2)

   end do

   call odeiv%free()
   call s%free()
   call m%free()
   deallocate( y )

end program solve_odeiv
