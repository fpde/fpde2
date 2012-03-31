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
   use class_ode_stepper


   integer, public, parameter :: &
        ODE_STEP_DECREASED = -1,&
        ODE_STEP_INCREASED = +1,&
        ODE_STEP_NOCHANGED = 0

   private

   type, public, extends(named) :: ode_step_control

      !> Absolute error parameter.
      real :: eps_abs = 0.0
      !> Relative error parmater.
      real :: eps_rel = 0.0
      !> Error scaling factors for the system state y
      real :: a_y = 0.0
      !> Error scaling factors for the system state y'
      real :: a_dydt = 0.0

   contains

      !>
      procedure :: init
      !>
      procedure :: apply
      !>
      procedure :: error_fnc
      !>
      procedure :: free

   end type ode_step_control


contains

   subroutine init ( c, eps_abs, eps_rel, a_y, a_dydt, scale_abs, dim )
      class(ode_step_control), intent(inout) :: c
      real, intent(in) :: eps_abs, eps_rel
      real, intent(in), optional :: a_y, a_dydt
      real, intent(in), optional :: scale_abs
      integer, intent(in), optional :: dim
   end subroutine init

   subroutine apply( c, s, y, yerr, dydt, h )
      class(ode_step_control), intent(inout) :: c
      class(ode_stepper), intent(in) :: s
      real, intent(in) :: y(:)
      real, intent(in) :: yerr(:)
      real, intent(in) :: dydt(:)
      real, intent(inout) :: h

      integer :: i, dim, order
      real, parameter :: min_real=epsilon(1.0) !< smallest positive number added to 1.0 /= 1.0
      real, parameter :: sfactor=0.9 !< safety factor
      real :: h_old, rmax, d0, r

      dim = s % dim
      order = s % method_order

      h_old = h
      rmax = min_real

      do i=1,dim
         ! Wywolujemy funkcje estymujaca blad, zwracana wartosc
         ! jest zapisywana do zmiennej d0
         call c % error_fnc( i, y(i), dydt(i), h_old, d0 )
         r = abs(yerr(i))/abs(d0)
         rmax = max(r,rmax)
      end do

      if ( rmax > 1.1 ) then
         ! Zmniejszamy krok, nie wiecej niz czynnik 5, lecz sfactor
         ! wicej niz sugeruje skalowanie
         r = sfactor/rmax**(1.0/order) ! @bug ????
         if ( r < 0.2 ) then
            r = 0.2
         end if
         h = r * h_old;
         c % status = ODE_STEP_DECREASED ! status ujemny oznacza ze krok zostal zmniejszony
         return

      else if ( rmax < 0.5 ) then
         ! Zwiekszamy krok, nie wiecej niz czynnik 5
         r = sfactor/rmax**(1.0/(order+1)) ! @bug ?????
         if ( r > 5.0 ) then
            r = 5.0
         else if ( r < 1.0 ) then ! sprawdzamy czy sfactor nie spowodowal zmniejszenia kroku
            r = 1.0
         end if
         h = r * h_old;
         c % status = ODE_STEP_INCREASED ! status dodatni oznacza ze krok zostal zwiekszony
         return

      else
         ! Krok pozostaje bez zmian
         c % status = ODE_STEP_NOCHANGED ! status rowny zero oznacza ze krok nie zostal zmieniony
      end if

   end subroutine apply

   subroutine error_fnc( c, i, yi, dydti, h, err )
      class(ode_step_control), intent(inout) :: c
      integer, intent(in) :: i
      real, intent(in) :: yi
      real, intent(in) :: dydti
      real, intent(in) :: h
      real, intent(inout) :: err
   end subroutine error_fnc

   subroutine free( c )
      class(ode_step_control), intent(inout) :: c
   end subroutine free

end module class_ode_step_control
