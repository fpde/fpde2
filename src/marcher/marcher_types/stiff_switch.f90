!>
!! @file   stiff_switch.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Tue Jul 31 15:14:58 2012
!!
!! @brief
!!
!!
!!
module class_ode_marcher_stiff_switch

   use constants_module
   use logger_module
   use flu_get_module
   use class_ode_marcher
   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control

   private

   type, public, extends(ode_marcher) ::  ode_marcher_stiff_switch

      ! @todo move this part to the step control object
      !> Number of performed step.
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

      class(ode_stepper), pointer :: stepper_explicit
      class(ode_stepper), pointer :: stepper_implicit

      logical :: detect_nonstiff = .true.
      logical :: detect_stiff = .true.

      !> Linear stability boundary safety factor - determines
      !! severity of the stiffness and nonstiff test
      real :: lsb_sfactor = 0.9

      logical :: is_stiff = .false.
      logical, pointer :: stiff_journal(:)
      integer :: stiff_journal_len = 10

   contains

      procedure :: stiff_test
      procedure :: nonstiff_test

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

   end type ode_marcher_stiff_switch

contains

   subroutine stiff_test(this, sys, y, t, h, error)
      class(ode_marcher_stiff_switch), intent(inout) :: this
      class(ode_system) :: sys
      real, pointer, intent(in) :: y(:)
      real, intent(in) :: t, h
      integer, optional, intent(out) :: error
      !> local variables
      integer :: err
      real :: lambda
      logical :: is_stiff

      err = FPDE_STATUS_OK

      call this%stepper_explicit%stiff_test(sys,y,t,h,lambda,err)

      if( abs( h*lambda )/this%stepper_explicit%lsb <= this%lsb_sfactor ) then
         is_stiff = .true.
      else
         is_stiff = .false.
      end if

      !> save stiffness status into the journal
      this%stiff_journal = cshift(this%stiff_journal,-1)
      this%stiff_journal(1) = is_stiff

      !> check if stiffness occured is_stiff_no times in succession
      this%is_stiff = all(this%stiff_journal)

      if(present(error)) error = err

   end subroutine stiff_test


   subroutine nonstiff_test(this, sys, y, t, h, error)
      class(ode_marcher_stiff_switch), intent(inout) :: this
      class(ode_system) :: sys
      real, pointer, intent(in) :: y(:)
      real, intent(in) :: t, h
      integer, optional, intent(out) :: error
      !> local variables
      integer :: err
      real :: lambda
      logical :: is_non_stiff

      lambda = norm2(this%stepper_implicit%J)/sqrt(real(this%dim*this%dim))

      if( abs( h*lambda )/this%stepper_explicit%lsb >= this%lsb_sfactor ) then
         is_non_stiff = .true.
      else
         is_non_stiff = .false.
      end if

      !> save stiffness status into the journal
      this%stiff_journal = cshift(this%stiff_journal,-1)
      this%stiff_journal(1) = is_non_stiff

      !> .not. since we are are testing for nonstiffness so
      !! all(this%stiff_journal) = .true. indicates problem is
      !! non stiff for this%stiff_journal_len consecutive steps
      this%is_stiff = .not. all(this%stiff_journal)

      err = FPDE_STATUS_OK

      if(present(error)) error = err

   end subroutine nonstiff_test

   subroutine init( p, error )
      class(ode_marcher_stiff_switch), intent(inout) :: p
      integer, optional, intent(out) :: error
      !> local variables
      integer :: n, err

      err = FPDE_STATUS_OK

      p % name = "marcher_stiff_switch"
      n = p % dim

      p % count = 0
      p % failed_steps = 0
      p % last_step = 0.0

      if ( n .le. 0 ) then
         call p%log(FPDE_LOG_ERROR, "Dimension passed in init cannot be <= 0")
         err = FPDE_STATUS_ERROR
      end if

      if ( .not. associated(p % stepper_explicit) ) then
         !> @todo initialize some default stepper
         call p%log(FPDE_LOG_ERROR, "explicit stepper not set")
         err = FPDE_STATUS_ERROR
      else
         p%stepper_explicit%dim = p%dim
         call p%stepper_explicit%init()
         !@todo check if stepper is initialized, how to do this?
         call p%log(FPDE_LOG_DEBUG, &
              "explicit stepper '"//trim(p%stepper_explicit%name)//"' is set")
      end if

      if ( .not. associated(p % stepper_implicit) ) then
         !> @todo initialize some default stepper
         call p%log(FPDE_LOG_ERROR, "implicit stepper not set")
         err = FPDE_STATUS_ERROR
      else
         p%stepper_implicit%dim = p%dim
         call p%stepper_implicit%init()
         !@todo check if stepper is initialized, how to do this?
         call p%log(FPDE_LOG_DEBUG, &
              "implicit stepper '"//trim(p%stepper_implicit%name)//"' is set")
      end if

      if ( err == FPDE_STATUS_OK ) then
         !> allocate marcher workspace vectors
         allocate( p%y0(n), p%yerr(n), p%dydt_in(n), p%dydt_out(n) )
         !> @todo check memory allocation status, if memory cannot be
         !! allocated call p%free() and return error

         allocate( p%stiff_journal(p%stiff_journal_len) ) !> stiffness test history

         !> Initially use explicit stepper
         p%s => p%stepper_explicit

      end if

      if (present(error)) error = err

   end subroutine init

   subroutine from_lua(p, l, error)
      class(ode_marcher_stiff_switch) :: p
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


   subroutine apply( this, sys, y, t, t1, h, error )
      class(ode_marcher_stiff_switch), intent(inout) :: this
      class(ode_system) :: sys
      real, intent(inout) :: t
      real, intent(in) :: t1
      real, optional, intent(inout) :: h
      real, pointer, intent(inout) :: y(:)
      integer, optional, intent(out) :: error
      !> local variables
      logical :: final_step, step_stat
      integer :: err, step_status
      real :: h0, t0, dt, h_old, t_curr, t_next

      err = FPDE_STATUS_OK

      ! h0 zmienna na ktorej operujemy, ewentualna zmiane kroku
      ! czyli zmiennej h dokonujemy na koncu subrutyny
      h0=h
      t0=t
      dt=t1-t0

      ! Sprawdzanie poprawnosci wymiarow, kierunek calkowania,
      ! calkowania ze zmiennym krokiem ... @todo

      ! Sprawdzamy zgodnosc wymiarow marchera oraz steppera
      if ( this % dim /= this % s % dim ) then
         this % status = FPDE_STATUS_ERROR
         call this%log(FPDE_LOG_ERROR, "Dimensions of stepper and marcher differ")
         err = FPDE_STATUS_ERROR
         return
      end if

      ! Sprawdzamy zgodnosc kierunku calkowania
      if ( (dt<0.0 .and. h0>0.0) .or. (dt>0.0 .and. h0<0.0) ) then
         this % status = FPDE_STATUS_ERROR
         call this%log(FPDE_LOG_ERROR, &
              "Marching direction must match time interval direction")
         err = FPDE_STATUS_ERROR
         return
      end if


      !> Check for nonstiffness if needed and if implicit stepper is in use
      if( this%detect_nonstiff .and. associated(this%s,this%stepper_implicit) ) then
         !> Perform nonstiff test (take benefits of implicit stepper, use
         !! computed Jacobian, etc.)
         call this%nonstiff_test(sys=sys, y=y, t=t, h=h, error=err)
         if ( err /= FPDE_STATUS_OK ) then
            call this%log(FPDE_LOG_ERROR, "nonstiff test failed")
            return
         end if
         !> Switch steppers if nonstiff problem detected
         if( .not. this%is_stiff ) then
            this%s => this%stepper_explicit
            !> reset stiffness journal
            this%stiff_journal = .false.
            call this%log(FPDE_LOG_DEBUG, "NONSTIFFNESS DETECTED")
         end if
      end if

      ! Jezeli calkujemy ze zmiennym krokiem czyli stepper
      ! wylicza blad kroku oraz zostala podana metoda kontrolujaca
      ! krok to wykonujemy kopie wejsciowego wektora y do struktury
      ! matchera m % y0 ! ! .and. present( c ) ! @todo
      if ( this % s % gives_estimated_yerr  ) then
         this % y0 = y
      end if

      ! Wyliczamy pochodne jezeli metoda moze z nich skorzystac
      if ( this % s % can_use_dydt_in ) then
         call sys%fun( t, y, this % dydt_in, sys % params, sys % status )
         if ( sys % status /= FPDE_STATUS_OK ) then
            this % status = sys % status
            return
         end if
      end if

      ! Wykonujemy probny krok

      ! Sprawdzenie czy krok jest ostatnim krokiem
      ! (w przypadku calkowania do przodu i do tylu)
100   if ( ( dt>=0.0 .and. h0>dt ).or.( dt<0.0 .and. h0<dt ) ) then
         h0=dt
         final_step=.true.
      else
         final_step=.false.
      end if

      ! Uruchamiamy stepper z uzyciem dydt_in
      if ( this % s % can_use_dydt_in ) then
         ! Kopiujemy wektor y na wypadek wystapienia bledu
         this % y0 = y
         call this%s%apply( sys=sys, y=y, t=t0, h=h0, yerr=this%yerr, &
              &             dydt_in=this%dydt_in, dydt_out=this%dydt_out, error=err )
      else
         ! lub bez uzycia dydt_in !@todo if method cannot benefit dydt_in can it give dydt_out?
         call this%s%apply( sys=sys, y=y, t=t0, h=h0, yerr=this%yerr, &
              &             dydt_out=this%dydt_out, error=err )
      end if

      ! Sprawdzamy czy stepper wykonal sie poprawnie
      if ( err /= FPDE_STATUS_OK ) then
         ! jezeli wystapil blad przekazujemy taki sam
         ! status bledu do statusu marchera aby mozna go
         ! bylo z zewnatrz odczytac
         this % status = this % s % status
         h = h0 ! zwracamy krok przy jakim pojawil sie blad
         t = t0 ! przywracamy wartosc t podana na wejsciu
         return
      end if

      ! Jezeli stepper nie spowodowal zadnych bledow zwiekszamy
      ! licznik m % count i zapisujemy krok w m % last_step
      this % count = this % count + 1
      this % last_step = h0

      ! Zapisujemy aktualny czas
      if ( final_step ) then
         t = t1
      else
         t = t0 + h0
      end if

      ! Ponizej kod odpowiadajacy za calkowanie ze zmiennym krokiem

      ! Jezeli metoda na to pozwala oraz zostal podany step control
      ! uzywamy metody z adaptywnym krokiem
      if ( this % s % gives_estimated_yerr ) then
         h_old = h0 ! zapamietujemy wielkosc kroku

         ! @todo check whether the stepper can provide dydt_out, dydt_in
         ! and redirect it to the refine_step procedure, since some
         ! of them may use these values

         !> this is totally new part
         call this%s%refine_step(sys=sys, t=t0, y0=this % y0, y1=y, yerr=this%yerr, &
              &                  dydt_out = this % dydt_out, &
              &                  c=this%c, hold=h_old, hnew=h0, accept=step_stat, &
              &                  error=err)

         if ( err /= FPDE_STATUS_OK ) then
            h = h0 ! zwracamy krok przy jakim pojawil sie blad
            t = t0 ! przywracamy wartosc t podana na wejsciu
            call this%log(FPDE_LOG_ERROR, "integration step refining failed")
            return
         end if


         if ( step_stat ) then
            !> accept step this step and try next with h0
         else
            !> step rejected
            ! Krok zostal zmniejszony, anulujemy wykonany krok
            ! i probujemy znow z nowym krokiem h0
            y = this % y0
            this % failed_steps = this % failed_steps + 1
            go to 100
         end if

      end if

      ! Zapisujemy sugerowana wielkosc dla nastepnego
      ! kroku czasowego
      h = h0

      !> Check for stiffness if needed and if explicit stepper is in use
      if( this%detect_stiff .and. associated(this%s,this%stepper_explicit) ) then
         !> Perform stiff test (take benefits of explicit stepper)
         call this%stiff_test(sys=sys, y=y, t=t, h=h_old, error=err) !> since
         !! the stiff test can call the sys%fun it is important to use the old
         !! step size (h_old) used to compute all off the k_i in the internal
         !! RK method steps
         if ( err /= FPDE_STATUS_OK ) then
            call this%log(FPDE_LOG_ERROR, "stiff test failed")
            return
         end if
         !> Switch steppers if stiffness detected
         if(this%is_stiff) then
            this%s => this%stepper_implicit
            !> reset stiffness journal
            this%stiff_journal = .false.
            call this%log(FPDE_LOG_DEBUG, "STIFFNESS DETECTED")
         end if
      end if

      if(present(error)) error = err

   end subroutine apply


   subroutine reset( this, error )
      class(ode_marcher_stiff_switch), intent(inout) :: this
      integer, optional, intent(out) :: error

      ! m % count = 0
      ! m % failed_steps = 0
      ! m % last_step = 0.0

      ! m % y0 = 0.0
      ! m % yerr = 0.0
      ! m % dydt_in = 0.0
      ! m % dydt_out = 0.0

      !> reset stiffness journal
      this%stiff_journal = .false.

      if (present(error)) error = FPDE_STATUS_OK

   end subroutine reset

   subroutine free( p, error )
      class(ode_marcher_stiff_switch), intent(inout) :: p
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

end module class_ode_marcher_stiff_switch
