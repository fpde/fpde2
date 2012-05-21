module class_odeiv_van_der_Pol

   use constants_module
   use logger_module
   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_vdp
   contains
      procedure :: init
   end type odeiv_vdp

contains

   subroutine init(this)
      class(odeiv_vdp) :: this
      integer, parameter :: n=2

      this % name = 'VDPOL'
      this % description = "van der Pol's equation with parameter mu=500"

      allocate( this % y0(n) )
      allocate( this % y1(n) )

      this % y0 = [ 2.0, 0.0 ]
      this % h = 1.0e-4
      this % t = [ 0.0, 2.0 ]

      call this%sys%init(fun = vdpol_rhs, jac = vdpol_jac, dim = n)

   end subroutine init

   subroutine vdpol_rhs(t, y, dydt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status
      !> fixed equation parameters
      real, parameter :: mu = 50.0

      dydt = [ y(2) , mu**2*( (1.0-y(1)**2)*y(2) - y(1) ) ]

      if( present(status) ) then
         status = FPDE_STATUS_OK
      end if
   end subroutine vdpol_rhs

   subroutine vdpol_jac(t, y, dfdy, dfdt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(*) :: params
      integer, optional :: status
      !> fixed equation parameters
      real, parameter :: mu = 50.0

      dfdt = 0.0

      dfdy(1,:) = [ 0.0, -mu**2*( 1.0 + 2.0*y(1)*y(2) ) ]
      dfdy(2,:) = [ 1.0, mu**2*( 1.0 - y(1)**2 ) ]

      if( present(status) ) then
         status = FPDE_STATUS_OK
      end if
   end subroutine vdpol_jac

end module class_odeiv_van_der_Pol
