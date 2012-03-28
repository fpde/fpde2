!>
!! @file   ode_system_OUT.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Fri Mar 23 22:26:26 2012
!!
!! @brief  ODE system object test.
!!
!! Parameters in the user-defined function must be of class type.
!!
module dho_module

   use constants_module

   type :: dho_params
      real :: m, b, k, f, o
   end type dho_params

contains

   subroutine dho_rhs(t, y, dydt, p, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(dho_params) :: p
      integer, optional :: status

      ! dy_1/dt
      dydt(1) = y(2)
      ! dy_2/dt
      dydt(2) = (-p%b*y(2)-p%k*y(1)+p%f*cos(p%o*t))/p%m

      if ( present( status ) ) then
         status = FPDE_STATUS_OK
      end if

   end subroutine dho_rhs

   subroutine dho_jac(t, y, dfdy, dfdt, p, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(dho_params) :: p
      integer, optional :: status

      ! df_1/dy_1
      dfdy(1,1) = 0.0
      ! df_1/dy_2
      dfdy(1,2) = 1.0
      ! df_2/dy_1
      dfdy(2,1) = -p%k/p%m
      ! df_2/dy_2
      dfdy(2,1) = -p%b/p%m

      ! df_1/dt
      dfdt(1) = 0.0
      ! df_2/dt
      dfdt(2) = -p%f/p%m*p%o*sin(p%o*t)

      if ( present( status ) ) then
         status = FPDE_STATUS_OK
      end if

   end subroutine dho_jac

end module dho_module


program ode_system_OUT

   use class_ode_system
   use dho_module

   type(ode_system) :: ode
   type(dho_params) :: dho
   real :: t0 = 0.1, y0(2) = [ 0.1, -0.2 ]
   real, pointer :: y(:), dydt(:)
   logical :: test

   allocate(y(2))
   allocate(dydt(2))

   y = y0

   dho%m=1.0
   dho%b=2.0
   dho%k=0.1
   dho%f=0.2
   dho%o=1.3

   ode%name = "Damped harmonic oscillator"

   print *, trim(ode%name)

   call ode%init(fun=dho_rhs,dim=2, params=dho)
   call ode%fun(t=t0, y=y, dydt=dydt, params=dho)

   ! check whether the y0 stays unchanged
   if ( norm2(y-y0) .eq. 0.0 ) then
      test = .true.
   else
      test = .false.
   end if

   print *, "y0 stays intact: ", test

   ! check if dydt is computed correctly (with right parameters)
   y0(1) = y(2)
   y0(2) = (-dho%b*y(2)-dho%k*y(1)+dho%f*cos(dho%o*t0))/dho%m

   if ( norm2(y0-dydt) .eq. 0.0 ) then
      test = .true.
   else
      test = .false.
   end if

   print *, "dydt is correct: ", test

   deallocate(y)
   deallocate(dydt)

end program ode_system_OUT
