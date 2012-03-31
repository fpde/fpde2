!>
!! @file   standard.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sat Mar 31 11:05:28 2012
!!
!! @brief  ODE step control standard object.
!!
!! It's the most gerneral step control object. It's error function is:
!!
!!    eps_abs + eps_rel * ( a_y * abs(yi) + a_dydt * abs( h * dydti ) )
!!
!! where user defined params are: eps_abs, eps_rel, a_y and a_dydt.
!! Symbol h stands for the current integration step size.
!!
module class_ode_step_control_standard

   use class_ode_stepper
   use class_ode_step_control

   private

   type, public, extends( ode_step_control ) :: ode_step_control_standard
   contains
      procedure :: init
      procedure :: error_fnc
   end type ode_step_control_standard

contains

   subroutine init ( c, eps_abs, eps_rel, a_y, a_dydt, scale_abs, dim )
      class(ode_step_control_standard), intent(inout) :: c
      real, intent(in) :: eps_abs, eps_rel
      real, intent(in), optional :: a_y, a_dydt
      real, intent(in), optional :: scale_abs
      integer, intent(in), optional :: dim

      ! @todo przed zapisaniem przekazywanych jako argumenty funkcji
      ! wartosci do struktury step controlera nalezy sprawdzic
      ! czy sa one wieksze od: zera i najmniejszej reprezentowalnej
      ! liczby typu real? i wyjsc z programu? czy zapisac domyslne
      ! wartosci w strukturze c??
      c % eps_abs = eps_abs
      c % eps_rel = eps_rel
      c % a_y = a_y
      c % a_dydt = a_dydt
      c % name = "standard"

      c % status = 0 !@todo co zrobic ze statusem step controlera
      ! zastanawiam sie czy jest on potrzebny, narazie domyslnie
      ! ustawiam go na stan ktory oznacza wg umowy ze krok pozostal
      ! nie zmieniony
   end subroutine init

   subroutine error_fnc( c, i, yi, dydti, h, err )
      class(ode_step_control_standard), intent(inout) :: c
      integer, intent(in) :: i
      real, intent(in) :: yi
      real, intent(in) :: dydti
      real, intent(in) :: h
      real, intent(inout) :: err

      err = c % eps_abs + &
           c % eps_rel * ( c % a_y * abs(yi) + c % a_dydt * abs( h * dydti ) )

   end subroutine error_fnc

end module class_ode_step_control_standard
