module class_ode_stepper_rk_implicit_abstract

   use logger_module
   use constants_module
   use class_butcher_tableu
   use class_ode_system
   use class_ode_stepper

   use kronecker_product
   use mkl95_lapack, only : getrf, getrs

   private

   type, public, extends(ode_stepper) :: ode_stepper_rk_implicit_abstract

      !> Number of internal stages
      integer :: stages = 0
      !> Butcher tableu storage
      class(butcher_tableu), pointer :: bt => null()
      real, pointer, contiguous :: d(:) !> d = b*A^{-1}
      !> Workspace vectors
      real, pointer, contiguous  :: dfdt(:) !> n
      real, pointer, contiguous  :: J(:,:) !> n x n
      real, pointer, contiguous :: AI(:,:) !> n*s x n*s
      real, pointer, contiguous :: IhAJ(:,:) !> n*s x n*s !> @todo reduce storage
      real, pointer, contiguous :: Z(:), FZ(:), DZ(:) !> n*s
      integer, pointer, contiguous :: pivot(:)

      real, pointer, contiguous :: ytmp(:) !> n

      integer :: kmax = 8
      integer :: k_last
      real :: tol = 1.0e-4
      real :: kappa = 5.0e-2
      logical :: jac_recompute = .true.

      real :: eta_last = 0.0
      ! real :: dz_norm_last = 0.0
      ! real :: theta_last = 0.0

   contains

      procedure :: init
      procedure :: apply
      procedure :: iterate_newton
      procedure :: reset
      procedure :: free

   end type ode_stepper_rk_implicit_abstract

contains

   subroutine init(p,error)
      class(ode_stepper_rk_implicit_abstract) :: p
      integer, optional, intent(out) :: error
      integer :: err, s, n

      err = FPDE_STATUS_OK

      s = p % stages
      n = p % dim

      if ( s .gt. 0 ) then
         !> allocating Butcher tableu
         if ( .not. associated( p % bt ) ) then

            allocate( p % bt, p % d(s) )
            p % bt % s = s
            call p%bt%init(err)

            if ( err .ne. FPDE_STATUS_OK ) then
               call p%log(FPDE_LOG_ERROR, "initializing Butcher tableu: failure")

               deallocate( p % bt, p % d )
               nullify( p % bt, p % d )

            end if
         end if

      else
         !> log the error
         call p%log(FPDE_LOG_ERROR, "initializing stepper: number of stages <= 0")
         err = FPDE_STATUS_ERROR

      end if

      if ( n .gt. 0 ) then
         !> allocating workspace vectors
         if ( .not. associated( p % dfdt ) ) allocate( p % dfdt(n) )
         if ( .not. associated( p % J ) ) allocate( p % J(n,n) )
         if ( .not. associated( p % AI ) ) allocate( p % AI(n*s,n*s) )
         if ( .not. associated( p % IhAJ ) ) allocate( p % IhAJ(n*s,n*s) )
         if ( .not. associated( p % Z ) ) allocate( p % Z(n*s) )
         if ( .not. associated( p % FZ ) ) allocate( p % FZ(n*s) )
         if ( .not. associated( p % DZ ) ) allocate( p % DZ(n*s) )
         if ( .not. associated( p % pivot ) ) allocate( p % pivot(n*s) )
         if ( .not. associated( p % ytmp ) ) allocate( p % ytmp(n*s) )
      else
         !> log the error
         call p%log(FPDE_LOG_ERROR, "initializing stepper: dim <= 0")
         err = FPDE_STATUS_ERROR

      end if

      if ( present( error ) ) then
         error = err
      end if

   end subroutine init

   subroutine apply(this, sys, y, t, h, yerr, dydt_in, dydt_out, error)
      class(ode_stepper_rk_implicit_abstract), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(inout) :: y(:)
      real, optional, pointer, contiguous, intent(inout) :: yerr(:)
      real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, optional, pointer, contiguous, intent(inout) :: dydt_out(:)
      integer, optional, intent(out) :: error
      !> local variables
      integer :: i, j, n, s, err
      real, pointer :: Z(:)

      err = FPDE_STATUS_OK

      n = this % dim
      s = this % stages

      !> Compute Jacobian matrix
      if ( associated(sys%jac) ) then
         call this%log(FPDE_LOG_DEBUG, "call sys%jac")
         call sys%jac(t, y, this % J, this % dfdt, &
              &       sys % params, sys % status)
         if ( sys % status /= FPDE_STATUS_OK ) then
            ! this % status = sys % status !@todo
            err = sys % status
            call this%log(FPDE_LOG_ERROR, "ode_system call failed")
            return ! @todo
         end if
         call this%log(FPDE_LOG_DEBUG, "call sys%jac passed")

      else
         !> @todo implement algorith which will compute jacobian matrix
         !! via finite difference approximation with a selected order
         call this%log(FPDE_LOG_DEBUG, "calculating Jacobian matrix via FDA")
         call this%log(FPDE_LOG_ERROR, "functionality not implemented yet")

      end if

      !> Construct I-h*AxJ matrix (which is n*s by n*s) and store in this%IhAJ
      call this%log(FPDE_LOG_DEBUG, "constructing I-h*AxJ matrix: start")
      this % IhAJ = 0.0
      do i=1,n*s
         this % IhAJ(i,i) = 1.0
      end do
      call kronecker(A=this%bt%A, B=this%J, C=this%IhAJ, alpha=1.0, beta=-h)
      call this%log(FPDE_LOG_DEBUG, "constructing I-h*AxJ matrix: end")

      !> Perform LU factorization of the main matrix of linear system
      call getrf(a=this%IhAJ, ipiv=this%pivot, info=err)
      if ( err == 0 ) then
         call this%log(FPDE_LOG_DEBUG, "LU factorization successful")
      else if ( err < 0 ) then !> the i-th parameter had an illegal value
         call this%log(FPDE_LOG_ERROR, "LU factorization failed, illegal entry")
      else !> the U_{ii} is 0, division by 0 will occur if this factorized matrix will by used
         call this%log(FPDE_LOG_ERROR, "LU factorization failed, singular U")
      end if

      !> Generate starting values for the Newton iteration
      !> @todo
      call this%log(FPDE_LOG_DEBUG, "generate init values for Netwton iteration")
      this % Z = 0.0

      call this%log(FPDE_LOG_DEBUG, "Newton solver: start")
      call this%iterate_newton(sys, y, t, h, err)
      call this%log(FPDE_LOG_DEBUG, "Newton solver: end")

      if ( err /= FPDE_STATUS_OK ) then
         !> @todo
         call this%log(FPDE_LOG_ERROR, "Newton solver failed")
      else
         !> Newton iteration converged, compute the final sum
         this % ytmp = 0.0
         do i=1,s
            Z  => this % Z((i-1)*n+1:i*n)
            this % ytmp = this % ytmp + this % d(i) * Z
         end do
         y = y + this % ytmp
      end if

      if ( present( error ) ) then
         error = err
      end if

   end subroutine apply

   subroutine iterate_newton(this, sys, y0, t, h, error)
      class(ode_stepper_rk_implicit_abstract), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(in) :: y0(:)
      integer, optional, intent(out) :: error
      integer :: err, i, k, kmax, s, n
      real :: eta, theta, dz_norm_last, dz_norm, kappatol, divtest
      real, parameter :: min_real=epsilon(1.0)
      real, pointer :: FZk(:), Zk(:)
      character(len=3) :: ck

      err = FPDE_STATUS_OK

      n = this % dim
      s = this % stages
      kmax = this % kmax
      kappatol = this % kappa*this % tol

      do k=0,kmax
         !> Compute F(Z^k) and store in this%FZ(:)
         write(ck,'(g0)') k
         call this%log(FPDE_LOG_DEBUG, "Newton iteration loop, k: "//trim(ck))

         do i=1,s
            FZk => this % FZ((i-1)*n+1:i*n)
            Zk => this % Z((i-1)*n+1:i*n)
            this % ytmp = y0 + Zk
            call sys%fun(t + h * this % bt % c(i), this % ytmp, Fzk, &
                 &       sys % params, sys % status)
            if ( sys % status /= FPDE_STATUS_OK ) then
               ! this % status = sys % status !@todo
               err = sys % status
               call this%log(FPDE_LOG_ERROR, "ode_system call failed")
               return ! @todo
            end if
         end do
         call this%log(FPDE_LOG_DEBUG, "Newton iteration loop, end")

         !> Generate rhs for Newton iteration
         this % FZ = - this % Z + h*matmul( this % AI, this % FZ )

         !> Solve LU system
         call getrs( a=this % IhAJ, ipiv=this % pivot, b=this % FZ, info=err )
         if ( err /= 0 ) then !> (info = -i) the i-th parameter had an illegal value
            call this%log(FPDE_LOG_ERROR, "LU solver failed, illegal entry")
            err = FPDE_STATUS_ERROR
            return ! @todo
         else
            call this%log(FPDE_LOG_DEBUG, "LU solver success")
         end if

         !> Compute Z^{k+1}
         this % Z = this % Z + this % FZ

         !> Check the convergence of Newton's method
         dz_norm = norm2(this % FZ)

         !> @todo use proper norm, compatible with local error estimation
         if ( k == 0 ) then
            dz_norm_last = dz_norm
            eta = ( max(this % eta_last, min_real) )**0.8  !> eta_0
         else !> compute \Theta_k and \eta_k
            theta = dz_norm/dz_norm_last
            eta = theta/(1.0-theta)
            !> save values for next iteration step
            dz_norm_last = dz_norm
         end if

         !> Divergence criterion chcecked for k>0
         if ( k > 0 ) then
            divtest = theta**(kmax-k)/(1.0-theta)*dz_norm - kappatol
            if ( theta >= 1.0 .or. divtest > 0.0 ) then
               !> the Newton's diverged how to get this info to the marcher?
               call this%log(FPDE_LOG_DEBUG, "Newton iteration diverged")
               print *, k, divtest, theta
               stop !> leave the k loop ! @todo
            end if
         end if

         !> if no divergence detected, save value of eta for the next step
         this % eta_last = eta

         !> Convergence criterion
         if ( k > 0 ) then
            if ( eta*dz_norm <= kappatol ) then
               !> accept current Z as a solution to the Newton's procedure
               call this%log(FPDE_LOG_DEBUG, "Newton iteration converged")
               exit !> leave the k loop ! @todo
            end if
         end if

      end do ! k loop

      !> save number of iterations
      this % k_last = k

      if ( err == FPDE_STATUS_OK ) then

         !> Check Jacobian recomputation condition
         if ( k == 0 .or. theta <= 1.0e-3 ) then !@todo make this constant a parameter of method
            !> do not recompute Jacobian in the last step
            this % jac_recompute = .false.
         else
            this % jac_recompute = .true.
         end if

      end if

      if ( present( error ) ) then
         error = err
      end if

   end subroutine iterate_newton

   subroutine reset(this,error)
      class(ode_stepper_rk_implicit_abstract) :: this
      integer, optional, intent(out) :: error

      ! if ( associated( this % k ) ) this % k = 0.0
      ! if ( associated( this % y0 ) ) this % y0 = 0.0
      ! if ( associated( this % ytmp ) ) this % ytmp = 0.0

      if ( present(error) ) then
         error = FPDE_STATUS_OK
      end if
   end subroutine reset

   subroutine free(p,error)
      class(ode_stepper_rk_implicit_abstract) :: p
      integer, optional, intent(out) :: error

      !@todo assign p%dim = 0 ?? to prevent further usage of
      !! this object wihtout defining new dimension ?

      ! if ( associated( p % bt ) ) then
      !    call p%bt%free()
      !    deallocate( p % bt )
      !    p % bt => null()
      ! end if

      ! if ( associated( p % k ) ) then
      !    deallocate( p % k )
      !    p % k => null()
      ! end if

      ! if ( associated( p % y0 ) ) then
      !    deallocate( p % y0 )
      !    p % y0 => null()
      ! end if

      ! if ( associated( p % ytmp ) ) then
      !    deallocate( p % ytmp )
      !    p % ytmp => null()
      ! end if

      if ( present(error) ) then
         error = FPDE_STATUS_OK
      end if

   end subroutine free

end module class_ode_stepper_rk_implicit_abstract
