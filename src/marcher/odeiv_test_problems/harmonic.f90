!>
!! @file   harmonic.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 19 07:42:24 2012
!!
!! @brief  ODE Initial Value test problem: HARM.
!!
!! Harmonic oscillator.
!!
module class_odeiv_harm

   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_harm
   contains

      procedure :: init

   end type odeiv_harm

contains

   subroutine init( odeiv )
      class(odeiv_harm) :: odeiv
      ! local variables
      integer, parameter :: n = 2 !< dimension of the problem

      ! ODEIV problem name
      odeiv % name = 'HARM'
      ! description
      odeiv % description = 'Harmonic oscillator'

      ! initial values
      allocate ( odeiv % y0(n) )
      allocate ( odeiv % y1(n) )

      odeiv % y0(1) = 0.0
      odeiv % y0(2) = 1.0

      odeiv % h = 1.0e-1

      odeiv % t = [0.0, 20.0]

      ! ODE system initialization
      call odeiv % sys % init( fun = harm_rhs, dim = n )

   end subroutine init

   subroutine harm_rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status

      dydt(1)  = y(2)
      dydt(2)  = -y(1)

   end subroutine harm_rhs

end module class_odeiv_harm
