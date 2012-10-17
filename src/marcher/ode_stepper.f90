!>
!! @file   ode_stepper.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Fri Mar 23 19:25:11 2012
!!
!! @brief  ODE stepper class.
!!
!! @todo
!! - [ ] write doc for module methods
!! - [ ] check which steppers can benefit y' at input and which at output
!!
module class_ode_stepper

   use constants_module
   use class_platonic
   use class_ode_system
   use class_ode_step_control

   private

   !> Generic stepper object
   !!
   type, public, abstract, extends(platonic) :: ode_stepper
      !> Dimension of ODE system to solve (implies the amount of workspace)
      integer :: dim = 0

      !??
      !> @todo check when the method can get benefit of y' at input
      logical :: can_use_dydt_in = .false.
      !> @todo check when the method gives the exact y' at output
      logical :: gives_exact_dydt_out = .false.
      !> Flag indicating error estimation by stepper
      logical :: gives_estimated_yerr = .false.
      !> Method order
      integer :: method_order = 0

      !> Linear stability boundary
      real :: lsb
      !> Jacobian matrix pointer
      real, pointer, contiguous  :: J(:,:) !> n x n

   contains

      !> Applies the stepping function to the system of equations, defined
      !! by sys, using the step-size h and advances the system from time t
      !! and state y(t) to time t+h and state y(t+h).
      procedure(apply), deferred :: apply
      !> Resets the stepper object (cleans workspace but not frees).
      !! It should be used whenever the next stepper use will not be
      !! a continuation of a previous step.
      procedure(reset), deferred :: reset

      procedure(refine_step), deferred :: refine_step

      procedure :: stiff_test

   end type ode_stepper


   interface

      subroutine refine_step( this, sys, t, y0, y1, yerr, dydt_in, dydt_out, c, hold, hnew, accept, error )
         import :: ode_stepper, ode_system, ode_step_control
         class(ode_stepper), intent(inout) :: this
         class(ode_system), intent(inout) :: sys
         real, intent(in) :: t
         real, pointer, contiguous, intent(in) :: y0(:), y1(:)
         real, pointer, contiguous, intent(inout) :: yerr(:)
         real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
         real, optional, pointer, contiguous, intent(in) :: dydt_out(:)
         class(ode_step_control), intent(inout) :: c
         real, intent(in) :: hold
         real, intent(out) :: hnew
         logical, intent(out) :: accept
         integer, optional, intent(out) :: error
      end subroutine refine_step

      subroutine apply( this, sys, y, t, h, yerr, dydt_in, dydt_out, error )
         import :: ode_stepper, ode_system
         class(ode_stepper), intent(inout) :: this
         class(ode_system), intent(inout) :: sys
         real, intent(in) :: t, h
         real, pointer, contiguous, intent(inout) :: y(:)
         real, optional, pointer, contiguous, intent(inout) :: yerr(:)
         real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
         real, optional, pointer, contiguous, intent(inout) :: dydt_out(:)
         integer, optional, intent(out) :: error
      end subroutine apply

      subroutine reset( this, error )
         import :: ode_stepper
         class(ode_stepper), intent(inout) :: this
         integer, optional, intent(out) :: error
      end subroutine reset

   end interface

contains

   subroutine stiff_test( this, sys, y, t, h, lambda, error )
      class(ode_stepper), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, pointer, intent(in) :: y(:)
      real, intent(in) :: t, h
      real, intent(out) :: lambda
      integer, optional, intent(out) :: error

      lambda = 0.0

      if(present(error)) error = FPDE_STATUS_OK

   end subroutine stiff_test

end module class_ode_stepper
