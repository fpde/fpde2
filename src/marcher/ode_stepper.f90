!>
!! @file   ode_stepper.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Fri Mar 23 19:25:11 2012
!!
!! @brief  ODE stepper class.
!!
!! Generic stepper class. @todo write doc for module methods
!!
module class_ode_stepper

   use logger_module
   use class_ode_system

   private

   type, public, extends(named) :: ode_stepper

      !> Dimension of ODE system to solve (workspace size)
      integer :: dim = 0
      !> @todo check when the method can get benefit of y' at input
      logical :: can_use_dydt_in = .false.
      !> @todo check when the method gives the exact y' at output
      logical :: gives_exact_dydt_out = .false.
      !> Flag indicating error estimation by stepper
      logical :: gives_estimated_yerr = .false.
      !> Method order
      integer :: method_order = 0

   contains

      !> Allocates workspace memory for instance of class. Assigns stepper
      !! name, method order and stepper specific flags.
      procedure :: init
      !> Applies the stepping function to the system of equations defined
      !! by sys, using the step-size h to advance the system from time t
      !! and state y to time t+h.
      procedure :: apply
      !> Resets the stepper object (cleans workspace but not frees).
      !! It should be used whenever the next stepper use will not be
      !! a continuation of a previous step.
      procedure :: reset
      !> Frees all the memory associated with the stepping function.
      procedure :: free

   end type ode_stepper

contains

   subroutine init( s )
      class(ode_stepper), intent(inout) :: s
   end subroutine init

   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper), intent(inout) :: s
      integer, intent(in) :: dim
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(inout) :: y(:), yerr(:)
      real, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, pointer, contiguous, intent(inout) :: dydt_out(:)
      class(ode_system) :: sys
      integer, optional :: status
   end subroutine apply

   subroutine reset( s )
      class(ode_stepper), intent(inout) :: s
   end subroutine reset

   subroutine free( s )
      class(ode_stepper), intent(inout) :: s
   end subroutine free

end module class_ode_stepper
