!>
!! @file   e5.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sun Sep  9 17:40:08 2012
!!
!! @brief  The chemical reaction problem E5
!!
!!
!!
module class_odeiv_e5

   use ieee_arithmetic

   use constants_module
   use logger_module
   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_e5
      real :: a = 77.27, b = 8.375e-6, c = 0.161
   contains
      procedure :: init
   end type odeiv_e5

contains

   subroutine init(this)
      class(odeiv_e5) :: this
      integer, parameter :: n=3

      this % name = 'E5'
      this % description = "the E5nator model"

      allocate( this % y0(n), this % y1(n) )

      !> initial conditions
      this % y0 = [1.0,2.0,3.0]
      this % h = 1.0e-4
      this % t = [ 0.0, 360. ]

      call this%sys%init(fun = e5_rhs, jac = e5_jac, dim = n, params=this)

   end subroutine init

   subroutine e5_rhs(t, y, dydt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(odeiv_e5) :: params
      integer, optional :: status
      !> local variables
      real :: a, b, c

      a = params%a
      b = params%b
      c = params%c

      dydt = [ &
           a*(y(1)*(1 - b*y(1) - y(2)) + y(2)), &
           (-((1 + y(1))*y(2)) + y(3))/a, &
           c*(y(1) - y(3)) &
           ]

      if( present(status) ) then
         if( any(ieee_is_nan(dydt)) .or. (.not. all(ieee_is_finite(dydt))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine e5_rhs

   subroutine e5_jac(t, y, dfdy, dfdt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(odeiv_e5) :: params
      integer, optional :: status
      !> local variables
      real :: a, b, c

      a = params%a
      b = params%b
      c = params%c

      dfdt = 0.0

      ! df_i/dy_j = dfdy(i,j)
      dfdy(1,:) = [a*(1 - 2*b*Y(1) - Y(2)),a*(1 - Y(1)),0.]
      dfdy(2,:) = [-(Y(2)/a),-((1 + Y(1))/a),1/a]
      dfdy(3,:) = [c,0.,-c]

      if( present(status) ) then
         if( any(ieee_is_nan(dfdy)) .or. (.not. all(ieee_is_finite(dfdy))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine e5_jac

end module class_odeiv_e5
