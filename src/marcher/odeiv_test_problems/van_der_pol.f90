!>
!! @file   van_der_pol.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Tue Sep  4 11:06:35 2012
!!
!! @brief
!!
!! @todo - [] write doc
!!       - [] clean the code
!!
module class_odeiv_van_der_Pol

   use ieee_arithmetic

   use constants_module
   use logger_module
   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_vdp
      real :: mu = 1000
   contains
      procedure :: init
   end type odeiv_vdp

contains

   subroutine init(this)
      class(odeiv_vdp) :: this
      integer, parameter :: n=2

      this % name = 'VDPOL'
      this % description = "van der Pol's equation"

      allocate( this % y0(n) )
      allocate( this % y1(n) )

      this % y0 = [ 2.0, -0.6 ]
      this % h = 1.0e-4
      this % t = [ 0.0, 2.0 ]

      call this%sys%init(fun = vdpol_rhs, jac = vdpol_jac, dim = n, params=this)

   end subroutine init

   subroutine vdpol_rhs(t, y, dydt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(odeiv_vdp) :: params
      integer, optional :: status
      !> local variables
      real :: mu
      mu = params % mu

      dydt = [ &
           y(2), &
           mu**2*( (1.0-y(1)**2)*y(2) - y(1) ) &
           ]

      if( present(status) ) then
         status = FPDE_STATUS_OK
      end if
   end subroutine vdpol_rhs

   subroutine vdpol_jac(t, y, dfdy, dfdt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(odeiv_vdp) :: params
      integer, optional :: status
      !> local variables
      real :: mu
      mu = params % mu

      dfdt = 0.0

      dfdy(1,:) = [ 0.0, 1.0 ]
      dfdy(2,:) = [ -mu**2*( 1.0 + 2.0*y(1)*y(2) ), mu**2*( 1.0 - y(1)**2 ) ]

      if( present(status) ) then
         if( any(ieee_is_nan(dfdy)) .or. (.not. all(ieee_is_finite(dfdy))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine vdpol_jac

end module class_odeiv_van_der_Pol
