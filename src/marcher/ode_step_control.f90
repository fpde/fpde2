!>
!! @file   ode_step_control.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Mar 29 16:49:34 2012
!!
!! @brief  ODE step control class.
!!
!! Generic step control class.
!!
!! @todo
!! - [ ] translate comments
!! - [ ] write doc
!! - [ ] what about other step control algorithms?
!! - [ ] rewrite error_fnc to operate on vectors not on vector components
!!
module class_ode_step_control

   use constants_module
   use logger_module

   private

   type, public, extends(named) :: ode_step_control

      !> Absolute error parameter.
      real :: eps_abs = 1.0e-4
      !> Relative error parmater.
      real :: eps_rel = 1.0e-4
      !> Error scaling factors for the system state y
      real :: a_y = 1.0
      !> Error scaling factors for the system state y'
      real :: a_dydt = 1.0
      !> Safety factor
      real :: sfactor=0.9

   contains
      procedure :: error_fnc
   end type ode_step_control

contains

   subroutine error_fnc( c, i, yi, dydti, h, err )
      class(ode_step_control), intent(inout) :: c
      integer, intent(in) :: i
      real, intent(in) :: yi
      real, intent(in) :: dydti
      real, intent(in) :: h
      real, intent(inout) :: err

      err = c % eps_abs + &
           c % eps_rel * ( c % a_y * abs(yi) + c % a_dydt * abs( h * dydti ) )

   end subroutine error_fnc

end module class_ode_step_control
