!>
!! @file   ode_marcher.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 26 12:54:20 2012
!!
!! @brief  ODE marcher class.
!!
!! Generic marcher class.
!!
module class_ode_marcher

   use class_platonic
   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control

   private

   !> Generic marcher object
   !!
   type, public, abstract, extends(platonic) :: ode_marcher
      !> Dimension of ODE system to solve (implies the amount of workspace)
      integer :: dim = 0
      !> Stepper pointer there will be at least one stepper
      class(ode_stepper), pointer :: s
      !> Step control structure
      type(ode_step_control) :: c

   contains

      !> Applay integration step procedure
      procedure(apply), deferred :: apply
      !> Reset marcher workspace
      procedure(reset), deferred :: reset

   end type ode_marcher

   interface

      subroutine apply( this, sys, y, t, t1, h, error )
         import :: ode_marcher, ode_system
         class(ode_marcher), intent(inout) :: this
         class(ode_system) :: sys
         real, intent(inout) :: t
         real, intent(in) :: t1
         real, optional, intent(inout) :: h
         real, pointer, intent(inout) :: y(:)
         integer, optional, intent(out) :: error
      end subroutine apply

      subroutine reset( this, error )
         import :: ode_marcher
         class(ode_marcher), intent(inout) :: this
         integer, optional, intent(out) :: error
      end subroutine reset

   end interface

end module class_ode_marcher
