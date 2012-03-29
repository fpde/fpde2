!>
!! @file   aren.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Mar 29 19:52:41 2012
!!
!! @brief  ODE Initial Value test problem: AREN.
!!
!! Arenstorf orbits - the rescticted three body problem.
!!
module class_odeiv_aren

   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_aren
   contains

      procedure :: init

   end type odeiv_aren

contains

   subroutine init( odeiv )
      class(odeiv_aren) :: odeiv
      ! local variables
      integer, parameter :: n = 4 !< dimension of the problem

      ! ODEIV problem name
      odeiv % name = 'AREN'
      ! description
      odeiv % description = 'Arenstorf orbits - the rescticted three body problem'

      ! initial values
      allocate ( odeiv % y0(n) )
      allocate ( odeiv % y1(n) )

      odeiv % y0(1) = 0.994
      odeiv % y0(2) = 0.0
      odeiv % y0(3) = 0.0
      odeiv % y0(4) = -2.00158510637908252240537862224

      odeiv % t = [0.0, 17.0652165601579625588917206249]

      ! ODE system initialization
      call odeiv % sys % init( fun = aren_rhs, dim = n )

   end subroutine init

   subroutine aren_rhs( t, y, dydt, params, status )
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(*) :: params
      integer, optional :: status

      ! fixed equation parameters
      ! masses
      real, parameter :: mu = 0.012277471, mu1 = 1.0 - mu
      real :: D1, D2

      D1 = ( (y(1)+mu)**2 + y(2)**2 )**(3.0/2.0)
      D2 = ( (y(1)-mu1)**2 + y(2)**2 )**(3.0/2.0)

      dydt(1)  = y(3)
      dydt(2)  = y(4)
      dydt(3)  = y(1) + 2.0*y(4) - mu1*(y(1)+mu)/D1 - mu*(y(1)-mu1)/D2
      dydt(4)  = y(2) - 2.0*y(3) - mu1*y(2)/D1 - mu*y(2)/D2
   end subroutine aren_rhs

end module class_odeiv_aren
