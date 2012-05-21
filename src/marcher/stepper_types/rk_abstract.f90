!>
!! @file   rk_abstract.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Wed May  9 23:44:11 2012
!!
!! @brief  Abstract Runge-Kutta stepper class
!!
!!
!!
! module class_ode_stepper_rk_abstract

!    use logger_module
!    use constants_module
!    use class_ode_system
!    use class_ode_stepper

!    private

!    type, public, abstract, extends(ode_stepper) :: ode_stepper_rk_abstract

!       !> Number of internal stages
!       integer :: stages = 0

!       !> Workspace vectors
!       real, pointer, contiguous :: k(:,:)
!       real, pointer, contiguous :: y0(:)
!       real, pointer, contiguous :: ytmp(:)

!       !! @defgroup Butcher Butcher tableu
!       !> @{
!       !> Nodes coefficients
!       real, pointer :: c(:) => null()
!       !> The Runge–Kutta matrix
!       real, pointer :: a(:,:) => null()
!       !> Weights coefficients
!       real, pointer :: b(:) => null()
!       !> Error coefficients
!       real, pointer :: ec(:) => null()
!       !> @}

!    contains

!       procedure :: init
!       procedure :: apply
!       procedure :: reset
!       procedure :: free

!       procedure, non_overridable :: self_test

!    end type ode_stepper_explicit

! contains

!    subroutine init( s )
!       class(ode_stepper_explicit), intent(inout) :: s
!       ! local variables
!       integer :: n, m
!       n = s % stages

!       if ( n .gt. 0 ) then
!          if ( .not. associated( s % c ) ) allocate( s % c(n) )
!          if ( .not. associated( s % a ) ) allocate( s % a(n,n) )
!          if ( .not. associated( s % b ) ) allocate( s % b(n) )
!          if ( .not. associated( s % ec ) ) allocate( s % ec(n) )
!       else
!          call s%log(FPDE_LOG_ERROR, "initializing stepper rk explicit with stages < 0")
!       end if

!       m = s % dim
!       if ( m .gt. 0 ) then
!          if ( .not. associated( s % k ) ) allocate( s % k(n,m) )
!          if ( .not. associated( s % y0 ) ) allocate( s % y0(m) )
!          if ( .not. associated( s % ytmp ) ) allocate( s % ytmp(m) )
!       else
!          call s%log(FPDE_LOG_ERROR, "initializing stepper rk explicit with dim < 0")
!       end if

!    end subroutine init



!    subroutine reset( s )
!       class(ode_stepper_explicit), intent(inout) :: s

!       if ( associated( s % k ) ) s % k = 0.0
!       if ( associated( s % y0 ) ) s % y0 = 0.0
!       if ( associated( s % ytmp ) ) s % ytmp = 0.0
!       ! @todo restet the stepper status?

!    end subroutine reset

!    subroutine free( s )
!       class(ode_stepper_explicit), intent(inout) :: s

!       if ( associated( s % k ) ) then
!          deallocate( s % k )
!          s % k => null()
!       end if

!       if ( associated( s % y0 ) ) then
!          deallocate( s % y0 )
!          s % y0 => null()
!       end if

!       if ( associated( s % ytmp ) ) then
!          deallocate( s % ytmp )
!          s % ytmp => null()
!       end if

!       if ( associated( s % c ) ) then
!          deallocate( s % c )
!          s % c => null()
!       end if

!       if ( associated( s % a ) ) then
!          deallocate( s % a )
!          s % a => null()
!       end if

!       if ( associated( s % b ) ) then
!          deallocate( s % b )
!          s % b => null()
!       end if

!       if ( associated( s % ec ) ) then
!          deallocate( s % ec )
!          s % ec => null()
!       end if

!    end subroutine free

!    ! subroutine consistency_test( s )
!    !    class(ode_stepper_explicit), intent(inout) :: s
!    !    ! local variables
!    !    integer :: i
!    !    real :: t, r

!    !    t = norm2(sum( s % a, dim=2, mask=s % a .ne. 0.0  ) - s % c )/s % stages
!    !    r = abs(sum( s % b ) - 1.0)

!    !    print *, t .lt. epsilon(1.0)
!    !    print *, r .lt. epsilon(1.0)

!    ! end subroutine consistency_test

! end module class_ode_stepper_rk_abstract
