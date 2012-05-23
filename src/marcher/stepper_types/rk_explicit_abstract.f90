module class_ode_stepper_rk_explicit_abstract

   use logger_module
   use constants_module
   use class_butcher_tableu
   use class_ode_system
   use class_ode_stepper

   private

   type, public, extends(ode_stepper) :: ode_stepper_rk_explicit_abstract

      !> Number of internal stages
      integer :: stages = 0
      !> Butcher tableu storage
      class(butcher_tableu), pointer :: bt => null()
      !> Workspace vectors
      real, pointer, contiguous :: k(:,:), y0(:), ytmp(:)

   contains

      procedure :: init
      procedure :: apply
      procedure :: refine_step
      procedure :: reset
      procedure :: free

   end type ode_stepper_rk_explicit_abstract

contains

   subroutine refine_step( this, sys, t, y0, y1, yerr, dydt_in, dydt_out, hold, hnew, accept, error )
      class(ode_stepper_rk_explicit_abstract), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y0(:), y1(:)
      real, pointer, contiguous, intent(inout) :: yerr(:)
      real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, optional, pointer, contiguous, intent(in) :: dydt_out(:)
      real, intent(in) :: hold
      real, intent(out) :: hnew
      logical, intent(out) :: accept
      integer, optional, intent(out) :: error

      hnew = hold
      accept = .true.

      if ( present( error ) ) then
         error = FPDE_STATUS_OK
      end if

   end subroutine refine_step

   subroutine init(p,error)
      class(ode_stepper_rk_explicit_abstract) :: p
      integer, optional, intent(out) :: error
      integer :: err, s, n

      err = FPDE_STATUS_OK

      s = p % stages
      n = p % dim

      if ( s .gt. 0 ) then
         !> allocating Butcher tableu
         if ( .not. associated( p % bt ) ) then
            allocate( p % bt )
            p % bt % s = s
            call p%bt%init(err)

            if ( err .ne. FPDE_STATUS_OK ) then
               call p%log(FPDE_LOG_ERROR, "initializing Butcher tableu: failure")
               deallocate( p % bt )
               p % bt => null()
            end if
         end if

      else
         !> log the error
         call p%log(FPDE_LOG_ERROR, "initializing stepper: number of stages <= 0")
         err = FPDE_STATUS_ERROR

      end if

      if ( n .gt. 0 ) then
         !> allocating workspace vectors
         if ( .not. associated( p % k ) ) allocate( p % k(s,n) )
         if ( .not. associated( p % y0 ) ) allocate( p % y0(n) )
         if ( .not. associated( p % ytmp ) ) allocate( p % ytmp(n) )

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
      class(ode_stepper_rk_explicit_abstract), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(inout) :: y(:)
      real, optional, pointer, contiguous, intent(inout) :: yerr(:)
      real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, optional, pointer, contiguous, intent(inout) :: dydt_out(:)
      integer, optional, intent(out) :: error
      !> local variables
      integer :: i, j, err
      real, pointer :: k_ptr(:)

      err =  FPDE_STATUS_OK

      !> Backup y vector in case of error.
      this % y0 = y

      !> Performing k1 step. Use derivatives at input, if passed
      !! and if the method can benefit from them.
      if ( this % can_use_dydt_in .and. associated(dydt_in) ) then
         !> save derivatives
         this % k(1,:) = dydt_in
      else
         !> compute derivatives
         k_ptr => this % k(1,:)
         call sys%fun(t, this % y0, k_ptr, sys % params, sys % status)
         if ( sys % status /= FPDE_STATUS_OK ) then
            ! this % status = sys % status !@todo
            err = sys % status
            call this%log(FPDE_LOG_ERROR, "ode_system call failed")
            return ! @todo
         end if
      end if

      !> loop over the stepper internal stages
      do i=2, this % stages
         !> copy the initial value
         this % ytmp = y
         !> loop over the nonzero elements of the a matrix in
         !! Butcher tableu for this RK method
         do j=1, i-1
            this % ytmp = this % ytmp + h * this % bt % a(i,j) * this % k(j,:)
         end do
         !> where to save the result of the function call
         k_ptr => this % k(i,:)
         !> compute derivatives
         call sys%fun(t + h * this % bt % c(i), this % ytmp, k_ptr, &
              &       sys % params, sys % status)
         if ( sys % status /= FPDE_STATUS_OK ) then
            ! this % status = sys % status !@todo
            err = sys % status
            call this%log(FPDE_LOG_ERROR, "ode_system call failed")
            return ! @todo
         end if
      end do

      !> calculating the final sum
      this % ytmp = 0.0
      do i=1, this % stages
         this % ytmp = this % ytmp + this % bt % b(i) * this % k(i,:)
      end do

      y = y + h * this % ytmp

      !> Calculate defivatives at output if a stroage vector is passed
      !! and the method can benefit from them.
      if ( this % gives_exact_dydt_out .and. associated(dydt_out) ) then
         !> compute derivatives
         call sys%fun(t + h, y, dydt_out, sys % params, sys % status )
         if ( sys % status /= FPDE_STATUS_OK ) then
            err = sys % status
            ! this % status = sys % status !@todo
            call this%log(FPDE_LOG_ERROR, "ode_system call failed")

            !> recovering initial state vector
            !! @todo check whether recovering the initial state
            !! should be done after each failure of internal steps?
            y = this % y0
            return
         end if
      end if

      !> compute the estimated error
      yerr = 0.0
      do i=1, this % stages
         yerr = yerr + h * this % bt % ec(i) * this % k(i,:)
      end do

      !> nullify the auxiliary pointer
      k_ptr => null()

      if ( present(error) ) then
         error = err
      end if

   end subroutine apply

   subroutine reset(this,error)
      class(ode_stepper_rk_explicit_abstract) :: this
      integer, optional, intent(out) :: error

      if ( associated( this % k ) ) this % k = 0.0
      if ( associated( this % y0 ) ) this % y0 = 0.0
      if ( associated( this % ytmp ) ) this % ytmp = 0.0

      if ( present(error) ) then
         error = FPDE_STATUS_OK
      end if
   end subroutine reset

   subroutine free(p,error)
      class(ode_stepper_rk_explicit_abstract) :: p
      integer, optional, intent(out) :: error

      !@todo assign p%dim = 0 ?? to prevent further usage of
      !! this object wihtout defining new dimension ?

      if ( associated( p % bt ) ) then
         call p%bt%free()
         deallocate( p % bt )
         p % bt => null()
      end if

      if ( associated( p % k ) ) then
         deallocate( p % k )
         p % k => null()
      end if

      if ( associated( p % y0 ) ) then
         deallocate( p % y0 )
         p % y0 => null()
      end if

      if ( associated( p % ytmp ) ) then
         deallocate( p % ytmp )
         p % ytmp => null()
      end if

      if ( present(error) ) then
         error = FPDE_STATUS_OK
      end if

   end subroutine free

end module class_ode_stepper_rk_explicit_abstract
