!>
!! @file   simple.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 26 19:22:10 2012
!!
!! @brief  Simple ODE marcher class.
!!
!! @todo
!! - [ ] change the name of time interval parameters passed to
!! marcher%apply method
!! - [ ] step_control status c % status == -1
!! - [ ] check the validity of goto 100 usage
!! - [ ] real, pointer, contiguous :: y0(:) => null() causes
!! compilation error: "If dummy argument is declared CONTIGUOUS,
!! actual argument must be contiguous as well"
!!
module class_ode_marcher_simple

   use constants_module
   use logger_module
   use flu_get_module
   use class_ode_marcher
   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control

   private

   type, public, extends(ode_marcher) ::  ode_marcher_simple

      ! @todo move this part to the step control object
      !> Number of performed steps.
      integer :: count = 0
      !> Number of steps rejected by step controller.
      integer :: failed_steps = 0
      !> Previous integration step size.
      real :: last_step = 0

      !! @defgroup workvec Workspace vectors
      !> @{
      !> Storage of y copy at each apply call
      real, pointer, contiguous :: y0(:)
      !> Estimated error vector
      real, pointer, contiguous :: yerr(:)
      !> Vector of y' derivative at input
      real, pointer, contiguous :: dydt_in(:)
      !> Vector of y' derivative at output
      real, pointer, contiguous :: dydt_out(:)
      !> @}

   contains

      !> Marcher initialization procedure, includes memory allocation
      !! for all of the wokspace vectors.
      procedure :: init
      !>
      procedure :: apply
      !>
      procedure :: reset
      !>
      procedure :: free

      procedure :: from_lua

   end type ode_marcher_simple

contains

   subroutine init( p, error )
      class(ode_marcher_simple), intent(inout) :: p
      integer, optional, intent(out) :: error

      ! ! m % count = 0
      ! ! m % failed_steps = 0
      ! ! m % last_step = 0.0

      if ( p % dim .le. 0 ) then
         call p%log(FPDE_LOG_ERROR, "Dimension passed in init cannot be <= 0")
         if (present(error)) error = FPDE_STATUS_ERROR
      end if

      ! !! allocate marcher workspace vectors
      ! allocate( m%y0( m%dim ) )
      ! allocate( m%yerr( m%dim ) )
      ! allocate( m%dydt_in( m%dim ) )
      ! allocate( m%dydt_out( m%dim ) )

      if (present(error)) error = FPDE_STATUS_OK

   end subroutine init

   subroutine from_lua(p, l, error)
      class(ode_marcher_simple) :: p
      type(flu) :: l
      integer, optional, intent(out) :: error
      integer :: err

      if(present(error)) error = FPDE_STATUS_OK

      call flu_get_atomic(l, key="name", char = p%name, error=err)
      if ( err /= FPDE_STATUS_OK ) then
         if ( present(error) ) error = err
      end if

      call flu_get_atomic(l, index=-1, key="dim", val = p%dim, error=err)
      if ( err /= FPDE_STATUS_OK ) then
         if ( present(error) ) error = err
      end if

      call flu_get(l, index=-1, key="step_control", error=err)

      if ( err == FPDE_STATUS_OK ) then

         call flu_get_atomic(l, key="a_y", val = p%dim, error=err)
         if ( err /= FPDE_STATUS_OK ) then
            if ( present(error) ) error = err
         end if

      end if


      call lua_pop(l,1)


   end subroutine from_lua


   subroutine apply( m, sys, y, t, t1, h, error )
      class(ode_marcher_simple), intent(inout) :: m
      class(ode_system) :: sys
      real, intent(inout) :: t
      real, intent(in) :: t1
      real, optional, intent(inout) :: h
      real, pointer, intent(inout) :: y(:)
      integer, optional, intent(out) :: error

      if(present(error)) error = FPDE_STATUS_OK
   end subroutine apply


!    subroutine apply( m, s, c, sys, t, t1, h, y )
!       class(ode_marcher), intent(inout) :: m
!       class(ode_stepper), intent(inout) :: s
!       class(ode_step_control), optional :: c
!       class(ode_system) :: sys
!       real, intent(inout) :: t
!       real, intent(in) :: t1
!       real, intent(inout) :: h
!       real, pointer, intent(inout) :: y(:)
!       ! local variables
!       logical :: final_step
!       integer :: step_status
!       real :: h0, t0, dt, h_old, t_curr, t_next

!       ! h0 zmienna na ktorej operujemy, ewentualna zmiane kroku
!       ! czyli zmiennej h dokonujemy na koncu subrutyny
!       h0=h
!       t0=t
!       dt=t1-t0

!       ! Sprawdzanie poprawnosci wymiarow, kierunek calkowania,
!       ! calkowania ze zmiennym krokiem ... @todo

!       ! Sprawdzamy zgodnosc wymiarow marchera oraz steppera
!       if ( m % dim /= s % dim ) then
!          m % status = FPDE_STATUS_ERROR
!          call m%log(FPDE_LOG_ERROR, "Dimensions of stepper and marcher differ")
!          return
!       end if

!       ! Sprawdzamy zgodnosc kierunku calkowania
!       if ( (dt<0.0 .and. h0>0.0) .or. (dt>0.0 .and. h0<0.0) ) then
!          m % status = FPDE_STATUS_ERROR
!          call m%log(FPDE_LOG_ERROR, "Marching direction must match time interval direction")
!          return
!       end if

!       ! Jezeli calkujemy ze zmiennym krokiem czyli stepper
!       ! wylicza blad kroku oraz zostala podana metoda kontrolujaca
!       ! krok to wykonujemy kopie wejsciowego wektora y do struktury
!       ! matchera m % y0
!       if ( s % gives_estimated_yerr .and. present( c ) ) then
!          m % y0 = y
!       end if

!       ! Wyliczamy pochodne jezeli metoda moze z nich skorzystac
!       if ( s % can_use_dydt_in ) then
!          call sys % fun( t, y, m % dydt_in, sys % params, sys % status )
!          if ( sys % status /= FPDE_STATUS_OK ) then
!             m % status = sys % status
!             return
!          end if
!       end if

!       ! Wykonujemy probny krok

!       ! Sprawdzenie czy krok jest ostatnim krokiem
!       ! (w przypadku calkowania do przodu i do tylu)
! 100   if ( ( dt>=0.0 .and. h0>dt ).or.( dt<0.0 .and. h0<dt ) ) then
!          h0=dt
!          final_step=.true.
!       else
!          final_step=.false.
!       end if

!       ! Uruchamiamy stepper z uzyciem dydt_in
!       if ( s % can_use_dydt_in ) then
!          ! Kopiujemy wektor y na wypadek wystapienia bledu
!          m % y0 = y
!          call s % apply( s % dim, t0, h0, y, m % yerr, m % dydt_in, m % dydt_out, sys, s % status )
!       else
!          ! lub bez uzycia dydt_in
!          call s % apply( s % dim, t0, h0, y, m % yerr, null(), m % dydt_out, sys, s % status )
!       end if

!       ! Sprawdzamy czy stepper wykonal sie poprawnie
!       if ( s % status /= FPDE_STATUS_OK ) then
!          ! jezeli wystapil blad przekazujemy taki sam
!          ! status bledu do statusu marchera aby mozna go
!          ! bylo z zewnatrz odczytac
!          m % status = s % status
!          h = h0 ! zwracamy krok przy jakim pojawil sie blad
!          t = t0 ! przywracamy wartosc t podana na wejsciu
!          return
!       end if

!       ! Jezeli stepper nie spowodowal zadnych bledow zwiekszamy
!       ! licznik m % count i zapisujemy krok w m % last_step
!       m % count = m % count + 1
!       m % last_step = h0

!       ! Zapisujemy aktualny czas
!       if ( final_step ) then
!          t = t1
!       else
!          t = t0 + h0
!       end if

!       ! Ponizej kod odpowiadajacy za calkowanie ze zmiennym krokiem

!       ! Jezeli metoda na to pozwala oraz zostal podany step control
!       ! uzywamy metody z adaptywnym krokiem
!       if ( s % gives_estimated_yerr .and. present( c ) ) then
!          ! present( c ) zwraca .true. jesli zostal podany step control
!          h_old = h0 ! zapamietujemy wielkosc kroku
!          call c % apply ( s, y, m % yerr, m % dydt_out, h0 )
!          ! po wykonaniu apply step control ustawia swoj status
!          ! czyli zmienna c % status w zaleznosci czy krok ma
!          ! zostac zmieniony badz nie. Przyjeta konwencja:
!          ! c % status = 1   zostal zwiekszony ODE_STEP_INCREASED
!          ! c % status =-1   zostal zmniejszony ODE_STEP_DECREASED
!          ! c % status = 0   nie zostal zmieniony ODE_STEP_NOCHANGED

!          if ( c % status == ODE_STEP_DECREASED ) then
!             ! Sprawdzamy poprawnosc sugerowanego kroku:
!             ! czy h0 zostalo 'naprawde' zmniejszone
!             ! oraz czy sugerowane h0 zmieni czas t conajmniej
!             ! o jedna ULP

!             ! @todo double coerce?
!             t_curr = t
!             t_next = t+h0

!             if ( abs(h0) < abs(h_old) .and. t_next /= t_curr ) then
!                ! Krok zostal zmniejszony, anulujemy wykonany krok
!                ! i probujemy znow z nowym krokiem h0
!                y = m % y0
!                m % failed_steps = m % failed_steps + 1
!                !$omp barrier
!                go to 100
!             else
!                ! W przeciwnym wypadku trzymamy aktualny krok
!                h0 = h_old
!             end if
!          end if
!       end if

!       ! Zapisujemy sugerowana wielkosc dla nastepnego
!       ! kroku czasowego
!       h = h0

!    end subroutine apply

   subroutine reset( m, error )
      class(ode_marcher_simple), intent(inout) :: m
      integer, optional, intent(out) :: error

      m % count = 0
      m % failed_steps = 0
      m % last_step = 0.0

      m % y0 = 0.0
      m % yerr = 0.0
      m % dydt_in = 0.0
      m % dydt_out = 0.0

      if (present(error)) error = FPDE_STATUS_OK

   end subroutine reset

   subroutine free( p, error )
      class(ode_marcher_simple), intent(inout) :: p
      integer, optional, intent(out) :: error

      ! if ( associated( m % y0 ) ) then
      !    deallocate( m % y0 )
      !    m % y0 => null()
      ! end if

      ! if ( associated( m % yerr ) ) then
      !    deallocate( m % yerr )
      !    m % yerr => null()
      ! end if

      ! if ( associated( m % dydt_in ) ) then
      !    deallocate( m % dydt_in )
      !    m % dydt_in => null()
      ! end if

      ! if ( associated( m % dydt_out ) ) then
      !    deallocate( m % dydt_out )
      !    m % dydt_out => null()
      ! end if

      if (present(error)) error = FPDE_STATUS_OK

   end subroutine free

end module class_ode_marcher_simple
