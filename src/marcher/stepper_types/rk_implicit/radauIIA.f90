module class_ode_stepper_radauIIA

   use logger_module
   use constants_module
   use class_butcher_tableu
   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control
   use class_ode_stepper_rk_implicit_abstract

   use kronecker_product
   use mkl95_lapack, only : getrf, getrs, getri

   private

   real, parameter :: gamma = 3.637834252744495732208418513577776
   real, parameter :: alpha = 2.681082873627752133895790743211112
   real, parameter :: beta = 3.050430199247410569426377624787568

   type, public, extends(ode_stepper_rk_implicit_abstract) :: ode_stepper_radauIIA

      real, pointer :: TT(:,:), TTI(:,:)

      real, pointer :: TT_cr_ID(:,:), TTI_cr_ID(:,:) !> 3*n x 3*n
      real, pointer :: ASmall(:,:) !> n x n
      real, pointer :: ALarge(:,:), mu(:,:) !> 2n x 2n
      real, pointer :: W(:) !> 3*n
      integer :: newton_status = FPDE_STATUS_ERROR
      logical :: first_step = .true.
      logical :: last_step_rejected = .false.
      ! real :: atol = 1.0e-4, rtol = 1.0e-4

      !> predictive controler storage
      real :: last_yerr_norm, last_step_size

   contains

      procedure :: init
      procedure :: apply
      procedure :: iterate_newton
      procedure :: refine_step
      procedure :: interpolate_newton_init
   end type ode_stepper_radauIIA

contains

   subroutine refine_step(this, sys, t, y0, y1, yerr, dydt_in, dydt_out, c, hold, hnew, accept, error)
      class(ode_stepper_radauIIA), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y0(:), y1(:)
      real, pointer, contiguous, intent(inout) :: yerr(:)
      real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, optional, pointer, contiguous, intent(in) :: dydt_out(:)
      class(ode_step_control), intent(inout) :: c
      real, intent(in) :: hold
      real, intent(out) :: hnew
      logical, intent(out) :: accept
      integer, optional, intent(out) :: error
      !> local variables
      integer :: i, err, s, n
      real, pointer :: Z(:,:), e(:)
      real :: yerr_norm, atol, rtol, sc, sfactor, hnew_tmp
      real, parameter :: c1 = 1.0, c2 = 1.2
      real, parameter :: d1=0.2, d2=8.0

      err = FPDE_STATUS_OK

      !> If Newton iteration converged refine step size,
      !! if not halve step step size and restart computations
      if ( this % newton_status == FPDE_STATUS_OK ) then

         n = this % dim
         s = this % stages

         Z(1:n,1:s) => this % Z
         e => this % bt % ec

         if ( present(dydt_in) ) then
            !> use it in order to avoid function call
            yerr = dydt_in
         else
            !> compute derivatives needed for error estimation,
            !! use yerr as storage
            call sys%fun(t, y0, yerr, sys % params, sys % status)

            if ( sys % status /= FPDE_STATUS_OK ) then
               ! this % status = sys % status !@todo
               err = sys % status
               call this%log(FPDE_LOG_ERROR, "ode_system call failed")
               return ! @todo
            end if
         end if

         yerr = yerr*hold/gamma + e(1)*Z(:,1) + e(2)*Z(:,2) + e(3)*Z(:,3)

         !> compute \f$ A_{small}^{-1} = ( -J + \gamma/h I )^{-1} \f$
         call getri( a=this%Asmall, ipiv=this%pivot(1:n), info=err )
         if ( err == 0 ) then !> the execution is successful
            call this%log(FPDE_LOG_DEBUG, "LU inverse success")
         else if ( err < 0 ) then !> the i-th parameter had an illegal value
            call this%log(FPDE_LOG_ERROR, "LU inverse failed, illegal entry")
            err = FPDE_STATUS_ERROR
         else !> the i-th diagonal element of the factor U is zero, U is singular,
            !! and the inversion could not be completed
            call this%log(FPDE_LOG_ERROR, "LU inverse failed, U is singular")
            err = FPDE_STATUS_ERROR
         end if

         yerr = gamma/hold*matmul(this%Asmall, yerr)

         atol = c % eps_abs
         rtol = c % eps_rel

!------------------------------------------------------------------------------
         if ( this%first_step .or. this%last_step_rejected ) then
            !> compute another error estimator, this error estimator
            !! uses previously computed vector yerr

            this % ytmp = y0 + yerr
            ! print *, "yerr: ", yerr
            ! print *, "y0: ", y0
            ! print *, "ytmp: ", this % ytmp
            ! stop

            call sys%fun(t, this % ytmp, yerr, sys % params, sys % status)

            if ( sys % status /= FPDE_STATUS_OK ) then
               ! this % status = sys % status !@todo
               err = sys % status
               call this%log(FPDE_LOG_ERROR, "ode_system call failed")
               return ! @todo
            end if

            yerr = yerr*hold/gamma + e(1)*Z(:,1) + e(2)*Z(:,2) + e(3)*Z(:,3)
            yerr = gamma/hold*matmul(this%Asmall, yerr)

         end if
!------------------------------------------------------------------------------

         !> Compute the scaled y error
         do i=1,n
            sc = atol + max( abs(y0(i)), abs(y1(i)) )*rtol
            this % ytmp(i) = yerr(i)/sc
            ! @todo check for 0 division
         end do
         yerr_norm = norm2(this % ytmp)/sqrt(real(n))


         !> Set the step status for marcher (accept) and for
         !! stepper (this%last_step_rejected)
         !! (this will avoid one function call when computing
         !! error estimation \f$ \tilde{yerr} \f$ if this step
         !! is accepted
         if ( yerr_norm < 1 ) then
            accept = .true.
            this%last_step_rejected = .false.
            !> Save error norm and previous step size for next use
            this%last_yerr_norm = yerr_norm
            this%last_step_size = hold

         else !> yerr_norm >= 1
            accept = .false.
            this%last_step_rejected = .true.
            print *, "# ", t, yerr_norm                                  !> @ERROR ??
         end if

         !> Determine safety factor
         sfactor = 0.9*( 2.0*this%kmax + 1)/( 2.0*this%kmax + (this%k_last+1) )

         !> If this is a first integration step then use standard step size
         !! controller and initialize predictive one
         if ( this%first_step ) then
            hnew = sfactor * hold * yerr_norm**(-1.0/4.0)
            if( accept ) this%first_step = .false.
         else
            hnew = sfactor * hold * hold/(this%last_step_size) &
                 * (this%last_yerr_norm)**(1.0/4.0) * (yerr_norm)**(-1.0/2.0)
            hnew = min(hnew, sfactor * hold * yerr_norm**(-1.0/4.0))
         end if

         !> Restrict hte new step size
         !! d1 <= hnew/hold <= d2
         if ( hold*d1 > hnew ) hnew = d1*hold
         if ( hnew > hold*d2 ) hnew = d2*hold

         ! print *, "err: ", yerr_norm
         ! print *, "sfactor: ", sfactor
         ! print *, "factor: ", sfactor*yerr_norm**(-1.0/4.0)
         ! print *, "hold: ", hold
         ! print *, "hnew: ", hnew
         ! print *, "accept: ", accept
         ! stop

         if ( .not. this % jac_recompute ) then
            !> !@todo do not perform LU factorization, there
            !! should be more workspace to store Asmall^{-1}
            !! to take full advantage of this opportunity
            if ( c1*abs(hold) <= abs(hnew) .and. abs(hnew) <= c2*abs(hold) ) then
               hnew = hold
            end if
         end if

      else !> Newton interation failed (this % newton_status != FPDE_STATUS_OK)
         !! restart computations with smaller step size
         hnew = hold/2.0
         accept = .false.
         this%last_step_rejected = .true.
         call this%log(FPDE_LOG_DEBUG, "STEP HALVED")
      end if

      if ( present( error ) ) then
         error = err
      end if

   end subroutine refine_step

   subroutine init(p,error)
      class(ode_stepper_radauIIA), intent(inout) :: p
      integer, optional, intent(out) :: error
      integer :: err

      integer :: n
      real, pointer :: idm(:,:), lambda(:,:)

      err = FPDE_STATUS_OK

      n = p % dim

      !> setting constant parameters
      p % can_use_dydt_in = .true. !@todo
      p % gives_exact_dydt_out = .true. !@todo
      p % gives_estimated_yerr = .true. !@todo
      p % method_order = 5
      p % name = "radauIIA"
      p % stages = 3
      p % status = FPDE_STATUS_OK ! @todo

      p % lsb = 3.3

      call p%ode_stepper_rk_implicit_abstract%init(err)

      !> filling Butcher tableu
      if ( err .eq. FPDE_STATUS_OK ) then

         p % bt % c = [ (4 - Sqrt(6.))/10., (4 + Sqrt(6.))/10., 1. ]

         p % bt % a(1,:) = [ (88-7*Sqrt(6.))/360., &
              &              (296-169*Sqrt(6.))/1800., &
              &              (-2+3*Sqrt(6.))/225. ]

         p % bt % a(2,:) = [ (296+169*Sqrt(6.))/1800., &
              &              (88+7*Sqrt(6.))/360., &
              &              (-2-3*Sqrt(6.))/225. ]

         p % bt % a(3,:) = [ (16-Sqrt(6.))/36., &
              &              (16+Sqrt(6.))/36., &
              &              1./9. ]

         p % bt % b = [ (16-Sqrt(6.))/36., (16+Sqrt(6.))/36., 1./9. ]

         p % d = [ 0.0, 0.0, 1.0 ]

         p % bt % ec = &
              [ -13.-7*sqrt(6.), -13.+7*sqrt(6.), -1. ]/gamma/3.0

         allocate( p % ASmall(n,n), p % ALarge(2*n,2*n) )

         allocate(p % TT(3,3), p % TTI(3,3), &
              &  p  % TT_cr_ID(3*n, 3*n), p % TTI_cr_ID(3*n, 3*n), &
              &  p % mu( 2*n, 2*n ), p % W(3*n) )

         p % TT(1,:) = [ 9.1232394870892942792E-02, &
              &          -0.14125529502095420843E0, &
              &          -3.0029194105147424492E-02 ]

         p % TT(2,:) = [ 0.24171793270710701896E0, &
              0.20412935229379993199E0, &
              0.38294211275726193779E0 ]

         p % TT(3,:) = [ 0.96604818261509293619E0, &
              1.0, &
              0.0 ]

         p % TTI(1,:) = [ 4.3255798900631553510E0, &
              0.33919925181580986954E0, &
              0.54177053993587487119E0 ]

         p % TTI(2,:) = [ -4.1787185915519047273E0, &
              -0.32768282076106238708E0, &
              0.47662355450055045196E0 ]

         p % TTI(3,:) = [ -0.50287263494578687595E0, &
              2.5719269498556054292E0, &
              -0.59603920482822492497E0 ]

         idm => identity_matrix(n)

         allocate( lambda(2,2) )

         lambda = reshape( [alpha, beta, -beta, alpha], shape(lambda) )

         call kronecker( A = lambda, &
              &          B = idm, &
              &          C = p % mu )

         ! @todo remove this part
         call kronecker( A = p % bt % A, &
              &          B = idm, &
              &          C = p % AI )

         call kronecker( A = p % TT, &
              &          B = IDM, &
              &          C = p % TT_cr_ID )

         call kronecker( A = p % TTI, &
              &          B = IDM, &
              &          C = p % TTI_cr_ID )

         deallocate(idm, lambda)

      end if

      if ( present(error) ) then
         error = err
      end if

   end subroutine init

   subroutine interpolate_newton_init(this, w, z, q)
      class(ode_stepper_radauIIA), intent(inout) :: this
      real, intent(in) :: w
      real, intent(in) :: z(:,:)
      real, intent(out) :: q(:)

      q(:) = w*(5.531972647421809 + w*(21.112754694671032 + 15.580782047249224*w))*z(:,1) &
           + w*(-7.531972647421808 + (-16.446088028004365 - 8.914115380582556*w)*w)*z(:,2) &
           + (1. + w*(5. + w*(7.333333333333333 + 3.3333333333333335*w)))*z(:,3)

   end subroutine interpolate_newton_init



   subroutine apply(this, sys, y, t, h, yerr, dydt_in, dydt_out, error)
      class(ode_stepper_radauIIA), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(inout) :: y(:)
      real, optional, pointer, contiguous, intent(inout) :: yerr(:)
      real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, optional, pointer, contiguous, intent(inout) :: dydt_out(:)
      integer, optional, intent(out) :: error
      !> local variables
      integer :: i, j, n, s, err
      real :: argw
      real, pointer :: Z(:), Z_ns(:,:), W_ns(:,:)

      err = FPDE_STATUS_OK

      n = this % dim
      s = this % stages

      !> Compute Jacobian matrix
      if ( this % jac_recompute ) then
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
      end if

      !> Construct Asmall and Alarge matrices
      call this%log(FPDE_LOG_DEBUG, "constructing Asmall matrix: start")
      !> @todo do not recompute Asmall if jacobian is not not recomputed
      this % Asmall = - this % J
      ! this % Asmall = - transpose( this % J )
      do i=1,n
         this % Asmall(i,i) = this % Asmall(i,i) + gamma/h
      end do
!       print *, ""
!       print *, "J: "
!       do i=1,n
!          print 11, ( this % J(i,j), j=1,n )
!       end do
!       print *, ""
!       print *, ""
!       print *, "Asmall: "
!       do i=1,n
!          print 11, ( this % Asmall(i,j), j=1,n )
!       end do
!       print *, ""
! 11    FORMAT (2000(f12.5))
!      stop
      call this%log(FPDE_LOG_DEBUG, "constructing Asmall matrix: end")


      call this%log(FPDE_LOG_DEBUG, "constructing Alarge matrix: start")
      this % Alarge = 0.0

      this % Alarge(1:n,1:n) = - ( this % J )
      this % Alarge(n+1:2*n,n+1:2*n) = - ( this % J )

      ! this % Alarge(1:n,1:n) = - transpose( this % J )
      ! this % Alarge(n+1:2*n,n+1:2*n) = - transpose( this % J )

      this % Alarge = this % Alarge + this % mu/h

      ! this % Alarge = transpose(this % Alarge)
!       print *, "Alarge: "
!       do i=1,n*2
!          print 11, ( this % Alarge(i,j), j=1,n*2 )
!       end do
!       print *, ""
! stop
      call this%log(FPDE_LOG_DEBUG, "constructing Alarge matrix: end")

      !> Perform LU factorization of the Asmall matrix
      call getrf(a=this%Asmall, ipiv=this%pivot(1:n), info=err)
      if ( err == 0 ) then
         call this%log(FPDE_LOG_DEBUG, "LU factorization of Asmall successful")
      else if ( err < 0 ) then !> the i-th parameter had an illegal value
         call this%log(FPDE_LOG_ERROR, "LU factorization of Asmall failed, illegal entry")
      else !> the U_{ii} is 0, division by 0 will occur if this factorized matrix will by used
         call this%log(FPDE_LOG_ERROR, "LU factorization  of Asmall failed, singular U")
      end if

      !> Perform LU factorization of the Alarge matrix
      call getrf(a=this%Alarge, ipiv=this%pivot(n+1:3*n), info=err)
      if ( err == 0 ) then
         call this%log(FPDE_LOG_DEBUG, "LU factorization of Alarge successful")
      else if ( err < 0 ) then !> the i-th parameter had an illegal value
         call this%log(FPDE_LOG_ERROR, "LU factorization of Alarge failed, illegal entry")
      else !> the U_{ii} is 0, division by 0 will occur if this factorized matrix will by used
         call this%log(FPDE_LOG_ERROR, "LU factorization  of Alarge failed, singular U")
      end if

      !> Generate starting values for Newton iteration
      call this%log(FPDE_LOG_DEBUG, "generate init values for Netwton iteration")
      if( this%newton_status /= FPDE_STATUS_OK .or. this%first_step ) then
         this%Z = 0.0
      else !> use interpolation (use this%W as temporary storage)
         Z_ns(1:n,1:s) => this%Z
         W_ns(1:n,1:s) => this%W

         do i=1,3
            argw = (h/this%last_step_size)*this%bt%c(i) !> w*c_i
            call this%interpolate_newton_init( w=argw, z=Z_ns, q=W_ns(:,i) )
         end do
         !> z^0_i = q(1+w*c_i)-d_3*z_3
         do i=1,3
            Z_ns(:,i) = W_ns(:,i) - Z_ns(:,3)
         end do
      end if

      call this%log(FPDE_LOG_DEBUG, "Newton solver: start")
      call this%iterate_newton(sys, y, t, h, err)
      if ( this%newton_status /= FPDE_STATUS_OK ) then
         !> leave the function
         call this%log(FPDE_LOG_DEBUG, "NEWTON DIVERGED")
         return
      end if

      call this%log(FPDE_LOG_DEBUG, "Newton solver: end")

      if ( err /= FPDE_STATUS_OK ) then
         !> @todo detection of Newton convergence
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
      class(ode_stepper_radauIIA), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(in) :: y0(:)
      integer, optional, intent(out) :: error
      !> local variables
      integer :: err, i, k, kmax, s, n
      real :: eta, theta, dz_norm_last, dz_norm, kappatol, divtest
      real, parameter :: min_real=epsilon(1.0)
      real, pointer :: Z(:), W(:), F(:)
      real, pointer :: F_ns(:,:), Z_ns(:,:), W_ns(:,:)
      real, pointer :: Z_n(:), F_n(:)
      character(len=3) :: ck

      err = FPDE_STATUS_OK

      n = this % dim
      s = this % stages
      kmax = this % kmax
      kappatol = this % kappa*this % tol

      Z => this % Z
      W => this % W
      F => this % FZ

      Z_ns(1:n,1:s) => Z
      W_ns(1:n,1:s) => W
      F_ns(1:n,1:s) => F

      !> Starting values for the Newton iteration
      !! \f$ W^{k} = (T^{-1} \otimes I) Z^{k} \f$
      W = matmul(this % TTI_cr_ID, Z)

      this % newton_status = FPDE_STATUS_ERROR

      do k=0,kmax-1
         ! print *, "przed"
         ! print *, F
         !> Compute F(Z^k) and store it in this%FZ(:)
         write(ck,'(g0)') k
         call this%log(FPDE_LOG_DEBUG, "Newton iteration loop, k: "//trim(ck))

         call this%log(FPDE_LOG_DEBUG, "Newton iteration loop s, start")
         do i=1,s

            ! Z_n => Z_ns(:,s)
            ! F_n(1:n) => F_ns(1:n,s) !@bug this doesnt work !

            Z_n => Z((i-1)*n+1:i*n)
            F_n => F((i-1)*n+1:i*n)

            this % ytmp = y0 + Z_n

            call sys%fun(t + h * this % bt % c(i), this % ytmp, F_n, &
                 &       sys % params, sys % status)
            ! print *, F_n

            if ( sys % status /= FPDE_STATUS_OK ) then
               ! this % status = sys % status !@todo
               err = sys % status
               call this%log(FPDE_LOG_ERROR, "ode_system call failed")
               return ! @todo
            end if
         end do
         ! print *, "po"
         ! print *, F
         ! stop
         call this%log(FPDE_LOG_DEBUG, "Newton iteration loop s, end")

         !> Generate rhs for Newton iteration
         !! \f$ (T^{-1} \otimes I) F(Z^{k}) \f$
         F = matmul( this % TTI_cr_ID, F )
         !! \f$ -h^{-1} ( \Lambda \otimes I )*W  \f$
         F_ns(:,1) = F_ns(:,1) - gamma/h * W_ns(:,1)
         F_ns(:,2) = F_ns(:,2) - ( alpha*W_ns(:,2) - beta*W_ns(:,3) )/h
         F_ns(:,3) = F_ns(:,3) - ( beta*W_ns(:,2) + alpha*W_ns(:,3) )/h


         !> Solve LU small system
         call getrs( a=this%Asmall, ipiv=this%pivot(1:n), b=F(1:n), info=err )
         if ( err /= 0 ) then !> (info = -i) the i-th parameter had an illegal value
            call this%log(FPDE_LOG_ERROR, "LU solver failed, illegal entry")
            err = FPDE_STATUS_ERROR
            return ! @todo
         else
            call this%log(FPDE_LOG_DEBUG, "LU solver success")
         end if

         !> Solve LU large system
         call getrs( a=this%Alarge, ipiv=this%pivot(n+1:3*n), b=F(n+1:3*n), info=err )
         if ( err /= 0 ) then !> (info = -i) the i-th parameter had an illegal value
            call this%log(FPDE_LOG_ERROR, "LU solver failed, illegal entry")
            err = FPDE_STATUS_ERROR
            return ! @todo
         else
            call this%log(FPDE_LOG_DEBUG, "LU solver success")
         end if

         !> Compute W^{k+1}
         W = W + F !> dW is stored in F

         !> Compute Z^{k+1} from W^{k+1}
         Z = matmul(this % TT_cr_ID, W)

         !> Check the convergence of Newton's method
         ! dz_norm=0.0
         ! do i=1,s*n
         !    dz_norm = dz_norm + (F(i)/(this%atol + abs(F(i))*this%rtol))**2
         ! end do
         ! dz_norm = sqrt( dz_norm/real(s*n) )
         dz_norm = norm2(F)/sqrt(real(s*n)) !> dW is stored in F
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
               ! print *, "# k: ", k, ", theta: ", theta, ", divtest: ", divtest

               print '("# k: ",I2," theta: ",ES10.3," divtest: ",ES10.3)', &
                    k, theta, divtest

               !> set newton status flag
               this % newton_status = FPDE_STATUS_ERROR
               return !> leave function ! @todo
            end if
         end if

         !> Convergence criterion
         if ( k >= 0 ) then                                                !@todo >= ? >
            if ( eta*dz_norm <= kappatol ) then
               !> accept current Z as a solution to the Newton's procedure
               call this%log(FPDE_LOG_DEBUG, "Newton iteration converged")
               this % newton_status = FPDE_STATUS_OK
               exit !> leave the k loop ! @todo
            end if
         end if

      end do ! k loop

      !> save number of iterations
      this % k_last = k

      !> if no divergence detected, save value of eta
      !! for the next integration step
      this % eta_last = eta

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

end module class_ode_stepper_radauIIA
