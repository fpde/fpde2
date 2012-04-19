!>
!! @file   ode_stepper_explicit.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr  5 02:05:42 2012
!!
!! @brief  ODE stepper - generic explicit (embedded) Runge-Kutta.
!!
!! @todo
!! - [ ] consistency_test() method should return FPDE_STATUS
!!
module class_ode_stepper_explicit

   use logger_module
   use constants_module
   use class_ode_system
   use class_ode_stepper

   private

   type, public, extends(ode_stepper) :: ode_stepper_explicit

      !> Number of internal stages
      integer :: stages = 0

      !> Workspace vectors
      real, pointer, contiguous :: k(:,:)
      real, pointer, contiguous :: y0(:)
      real, pointer, contiguous :: ytmp(:)

      !! @defgroup Butcher Butcher tableu
      !> @{
      !> Nodes coefficients
      real, pointer :: c(:) => null()
      !> The Runge–Kutta matrix
      real, pointer :: a(:,:) => null()
      !> Weights coefficients
      real, pointer :: b(:) => null()
      !> Error coefficients
      real, pointer :: ec(:) => null()
      !> @}

   contains

      procedure :: init

      procedure :: apply

      procedure :: reset

      procedure :: free

      procedure, non_overridable :: consistency_test

   end type ode_stepper_explicit

contains

   subroutine init( s )
      class(ode_stepper_explicit), intent(inout) :: s
      ! local variables
      integer :: n, m
      n = s % stages

      if ( n .gt. 0 ) then
         if ( .not. associated( s % c ) ) allocate( s % c(n) )
         if ( .not. associated( s % a ) ) allocate( s % a(n,n) )
         if ( .not. associated( s % b ) ) allocate( s % b(n) )
         if ( .not. associated( s % ec ) ) allocate( s % ec(n) )
      else
         call s%log(FPDE_LOG_ERROR, "initializing stepper rk explicit with stages < 0")
      end if

      m = s % dim
      if ( m .gt. 0 ) then
         if ( .not. associated( s % k ) ) allocate( s % k(n,m) )
         if ( .not. associated( s % y0 ) ) allocate( s % y0(m) )
         if ( .not. associated( s % ytmp ) ) allocate( s % ytmp(m) )
      else
         call s%log(FPDE_LOG_ERROR, "initializing stepper rk explicit with dim < 0")
      end if

   end subroutine init


   subroutine apply( s, dim, t, h, y, yerr, dydt_in, dydt_out, sys, status )
      class(ode_stepper_explicit), intent(inout) :: s
      integer, intent(in) :: dim
      real, intent(in)  :: t, h
      real, pointer, intent(inout) :: y(:), yerr(:)
      real, pointer, intent(in)  :: dydt_in(:)
      real, pointer, intent(inout) :: dydt_out(:)
      class(ode_system)  :: sys
      integer, optional :: status
      ! local variables
      integer :: i, j
      real, pointer :: k_ptr(:)

      ! Wykonujemy kopie wektora y na wypadek wystapiena bledow
      ! zwracanych przez funkcje sys % fun (prawej strony rownan).
      ! W przypadku ich wystapienia nalezy przywrocic oryginalna
      ! zawartosc wektora y poprzez: y = s % y0, oraz zwrocic
      ! status.
      s % y0 = y

      ! krok k1

      ! pochodne na wejsciu
      ! sprawdzamy czy metoda moze wykorzystac podane
      ! na wejsciu pochodne
      if ( s % can_use_dydt_in .and. associated(dydt_in) ) then
         ! jesli tak to zapisujemy je do s%k(1,:)
         s % k(1,:) = dydt_in
      else
         ! w przeciwnym wypadku musimy je wyliczyc
         k_ptr => s % k(1,:)
         call sys % fun( t, s % y0, k_ptr, sys % params, sys % status )
         if ( sys % status /= FPDE_STATUS_OK ) then
            s % status = sys % status
            return
         end if
      end if

      do i=2, s % stages


         s % ytmp = y
         do j=1, i-1

            s % ytmp = s % ytmp + & ! @bug ? a(j,i) or a(i,j)
                 h * s % a(i,j) * s % k(j,:) ! a(j,i) ze wzgledu na sposob w jaki Fortran
            ! przechowywuje w pamieci macierze dwuwymiarowe
         end do

         k_ptr => s % k(i,:)

         call sys % fun( t + (s % c(i))*h, s % ytmp, k_ptr, sys % params, sys % status )
         if ( sys % status /= FPDE_STATUS_OK ) then
            s % status = sys % status
            return
         end if

      end do

      ! suma koncowa
      s % ytmp = 0.0
      do i=1, s % stages
         s % ytmp = s%ytmp + s%b(i) * s%k(i,:)
      end do
      y = y + h * s % ytmp

      ! pochodne na wyjsciu
      if ( s % gives_exact_dydt_out .and. associated(dydt_out) ) then
         ! wyliczamy pochodne
         call sys % fun( t+h, y, dydt_out, sys % params, sys % status )
         if ( sys % status /= FPDE_STATUS_OK ) then
            s % status = sys % status

            ! poniewaz wektor y zostal juz nadpisany
            ! musimy go odzyskac z kopi zrobionej na
            ! poczaktu subrutyny
            y = s % y0
            return
         end if
      end if

      ! estymowany blad - roznica pomiedzy p-tym a pb-tym rzedem
      yerr = 0.0
      do i=1, s % stages
         yerr = yerr + h * s % ec(i) * s % k(i,:)
      end do

      ! pomyslnie zakonczono subrutyne
      s % status = FPDE_STATUS_OK

      k_ptr => null()

   end subroutine apply


   subroutine reset( s )
      class(ode_stepper_explicit), intent(inout) :: s

      if ( associated( s % k ) ) s % k = 0.0
      if ( associated( s % y0 ) ) s % y0 = 0.0
      if ( associated( s % ytmp ) ) s % ytmp = 0.0
      ! @todo restet the stepper status?

   end subroutine reset

   subroutine free( s )
      class(ode_stepper_explicit), intent(inout) :: s

      if ( associated( s % k ) ) then
         deallocate( s % k )
         s % k => null()
      end if

      if ( associated( s % y0 ) ) then
         deallocate( s % y0 )
         s % y0 => null()
      end if

      if ( associated( s % ytmp ) ) then
         deallocate( s % ytmp )
         s % ytmp => null()
      end if

      if ( associated( s % c ) ) then
         deallocate( s % c )
         s % c => null()
      end if

      if ( associated( s % a ) ) then
         deallocate( s % a )
         s % a => null()
      end if

      if ( associated( s % b ) ) then
         deallocate( s % b )
         s % b => null()
      end if

      if ( associated( s % ec ) ) then
         deallocate( s % ec )
         s % ec => null()
      end if

   end subroutine free

   subroutine consistency_test( s )
      class(ode_stepper_explicit), intent(inout) :: s
      ! local variables
      integer :: i
      real :: t, r

      t = norm2(sum( s % a, dim=2, mask=s % a .ne. 0.0  ) - s % c )/s % stages
      r = abs(sum( s % b ) - 1.0)

      print *, t .lt. epsilon(1.0)
      print *, r .lt. epsilon(1.0)

   end subroutine consistency_test

end module class_ode_stepper_explicit
