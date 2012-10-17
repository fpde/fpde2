!>
!! @file   rober.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sun Aug  5 17:09:36 2012
!!
!! @brief  The reaction of Robertson (1966) the most prominent
!! example ode of the 'stiff' literature.
!!
!!
module class_odeiv_rober

   use ieee_arithmetic

   use constants_module
   use logger_module
   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_rober
   contains
      procedure :: init
   end type odeiv_rober

contains

   subroutine init(this)
      class(odeiv_rober) :: this
      integer, parameter :: n=3

      this % name = 'ROBER'
      this % description = "the reaction of Robertson"

      allocate( this % y0(n), this % y1(n) )

      this % y0 = [ 1.0, 0.0, 0.0 ]
      this % h = 1.0e-4
      this % t = [ 0.0, 40.0 ]

      call this%sys%init(fun = rober_rhs, jac = rober_jac, dim = n, params=this)

   end subroutine init

   subroutine rober_rhs(t, y, dydt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(odeiv_rober) :: params
      integer, optional :: status

      dydt = [ -0.04*y(1) + 1.0e4*y(2)*y(3), &
           &    0.04*y(1) - 1.0e4*y(2)*y(3) - 3.0e7*y(2)*y(2), &
           &    3.0e7*y(2)*y(2) ]

      if( present(status) ) then
         if( any(ieee_is_nan(dydt)) .or. (.not. all(ieee_is_finite(dydt))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine rober_rhs

   subroutine rober_jac(t, y, dfdy, dfdt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(odeiv_rober) :: params
      integer, optional :: status

      dfdt = 0.0

      dfdy(1,:) = [ -0.04, 1.0e4*y(3), 1.0e4*y(2) ]
      dfdy(2,:) = [ 0.04, -1.0e4*y(3) - 6.0e7*y(2), -1.0e4*y(2) ]
      dfdy(3,:) = [ 0.0, 6.0e7*y(2), 0.0 ]

      if( present(status) ) then
         if( any(ieee_is_nan(dfdy)) .or. (.not. all(ieee_is_finite(dfdy))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine rober_jac

end module class_odeiv_rober
