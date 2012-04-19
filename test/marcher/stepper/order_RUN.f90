!>
!! @file   order_RUN.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 19 12:58:35 2012
!!
!! @brief  ODE stepper order test.
!!
!!
!!
program stepper_order_run

   use logger_module
   use constants_module
   use class_odeiv_generic
   use class_odeiv_factory
   use class_ode_stepper
   use class_ode_stepper_factory
   use class_ode_marcher
   use ieee_arithmetic

   class(ode_stepper), pointer :: s
   class(odeiv_generic), pointer :: o
   integer :: i, j
   real :: p
   character(len=FPDE_NAME_LEN) :: s_name(1:2) = [ "rkv65", "rkv65" ]
   character(len=FPDE_NAME_LEN) :: o_name(1:2) = [ "AREN", "HARM" ]

   print '("#",TR2,3(A,TR7))', "ODEIV","order (computed)","order (expected)"

   do i=1,size(s_name)

      ! selecting stepper
      s => ode_stepper_new( trim( s_name(i) ) )
      print '("#",TR2,2(A))', "stepper: ", trim(s_name(i))

      ! loop over selected ODEIV problems
      do j=1,size(o_name)

         ! selecting and initializing ODEIV problem
         o => odeiv_new( trim( o_name(j) ) )
         call o%init()

         ! initializing stepper
         s % dim = o % sys % dim
         call s%init()

         ! calling stepper order test

         call test_stepper_order( s=s, o=o, tmax=o%t(1), hmin=o%h, p=p )

         print *, trim(o % name), p, s % method_order

         ! freeing stepper
         call s%free()

      end do

      call s%free()

   end do

contains

   subroutine test_stepper_order( s, o, tmax, hmin, p )
      class(ode_stepper) :: s
      class(odeiv_generic) :: o
      real, intent(in) :: tmax, hmin
      real, intent(out) :: p
      ! local variables
      type(ode_marcher) :: marcher
      integer :: i, j, nsteps
      real, pointer :: yh1(:), yh2(:), yh4(:)
      real :: t1, t2, t4, h1, h2, h4, num, den
      real, allocatable :: ptable(:)

      h1 = hmin
      h2 = hmin*2.0
      h4 = hmin*4.0

      ! number of time steps with time step h4
      nsteps = nint(tmax/h4)

      ! initializing marcher
      marcher % dim = s % dim
      call marcher%init()

      ! allocating workspace vectors
      allocate( yh1(s % dim) )
      allocate( yh2(s % dim) )
      allocate( yh4(s % dim) )
      allocate( ptable(nsteps) )

      ! copy initial data of ODEIV
      yh1 = o % y0
      yh2 = o % y0
      yh4 = o % y0

      t1 = o % t(0)
      t2 = o % t(0)
      t4 = o % t(0)

      do i=1,nsteps

         ! performing four integration steps with step h1
         do j=1,4

            call marcher%apply(s    = s,       &
                 &             sys  = o % sys, &
                 &             t    = t1,      &
                 &             t1   = tmax,    &
                 &             h    = h1,      &
                 &             y    = yh1 )

            if ( marcher % status /= FPDE_STATUS_OK ) then
               call marcher%log(FPDE_LOG_ERROR, "marcher % status /= FPDE_STATUS_OK")
            end if

         end do

         ! performing two integration steps with step h2
         do j=1,2

            call marcher%apply(s    = s,       &
                 &             sys  = o % sys, &
                 &             t    = t2,      &
                 &             t1   = tmax,    &
                 &             h    = h2,      &
                 &             y    = yh2 )

            if ( marcher % status /= FPDE_STATUS_OK ) then
               call marcher%log(FPDE_LOG_ERROR, "marcher % status /= FPDE_STATUS_OK")
            end if

         end do

         ! performing one integration steps with step h4
!         do j=1,1

            call marcher%apply(s    = s,       &
                 &             sys  = o % sys, &
                 &             t    = t4,      &
                 &             t1   = tmax,    &
                 &             h    = h4,      &
                 &             y    = yh4 )

            if ( marcher % status /= FPDE_STATUS_OK ) then
               call marcher%log(FPDE_LOG_ERROR, "marcher % status /= FPDE_STATUS_OK")
            end if

!         end do

         num = norm2(yh4-yh1)
         den = norm2(yh2-yh1)

         ptable(i) = log10(num/den)/log10(2.0)
         ! print *, i, yh2(1:2)
         ! print *, i, yh2(3:4)

         if ( .not. ieee_is_finite(ptable(i)) ) ptable(i) = 0.0

         ! print *, "testing stepper: ", trim(s%name), " by solving ODEIV problem: ", trim(o%name)
      end do

      p = norm2( ptable )/sqrt(real(nsteps))

      call marcher%free()
      deallocate(yh1)
      deallocate(yh2)
      deallocate(yh4)

   end subroutine test_stepper_order

end program stepper_order_run
