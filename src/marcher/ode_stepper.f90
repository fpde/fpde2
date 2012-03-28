!>
!! @file   ode_stepper.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Fri Mar 23 19:25:11 2012
!!
!! @brief  ODE stepper class.
!!
!!
!!
module class_ode_stepper

   use logger_module
   use class_ode_system

   private

   type, public, extends(named) :: ode_stepper
      integer :: dim
      logical :: can_use_dydt_in
      logical :: gives_exact_dydt_out
      logical :: gives_estimated_yerr
      integer :: method_order
   contains
      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: free
   end type ode_stepper

contains

   subroutine init( s, dim )
      class(ode_stepper), intent(inout) :: s
      integer :: dim
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
