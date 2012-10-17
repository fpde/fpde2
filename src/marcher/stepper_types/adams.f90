!>
!! @file   adams.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Aug  9 14:37:48 2012
!!
!! @brief  Implementation of the Adams method using
!! the vodef90 code.
!!
!!
!!
module class_ode_stepper_adams

   use constants_module
   use class_platonic
   use class_ode_system
   use class_ode_stepper
   use class_ode_step_control

   use dvode_f90_m

   private

   class(ode_system), pointer :: local_sys
   procedure(fun_interface), pointer :: local_fun
   class(*), pointer :: local_params


   type, public, extends(ode_stepper) :: ode_stepper_adams
      type(vode_opts) :: vode_options
      integer :: istate = 1
      integer :: istats(31)
      real :: rstats(22)
   contains

      procedure :: init
      procedure :: apply
      procedure :: reset
      procedure :: refine_step

   end type ode_stepper_adams

contains

   subroutine init(p,error)
      class(ode_stepper_adams), intent(inout) :: p
      integer, optional, intent(out) :: error
      !> local variables
      integer :: err

      err = FPDE_STATUS_OK

      if ( present(error) ) error = err

   end subroutine init

   subroutine f(neq, t, y, ydot)
      integer :: neq
      real :: t
      real, target :: y(*), ydot(*)
      !> local variables
      real, pointer :: yp(:), ydotp(:)


      yp(1:neq) => y(1:neq)
      ydotp(1:neq) => ydot(1:neq)

      ! print *, t, yp

      call local_fun(t,yp,ydotp,local_params)

   end subroutine f

   subroutine apply(this, sys, y, t, h, yerr, dydt_in, dydt_out, error )
      class(ode_stepper_adams), intent(inout) :: this
      class(ode_system), intent(inout) :: sys
      real, intent(in) :: t, h
      real, pointer, contiguous, intent(inout) :: y(:)
      real, optional, pointer, contiguous, intent(inout) :: yerr(:)
      real, optional, pointer, contiguous, intent(in)  :: dydt_in(:)
      real, optional, pointer, contiguous, intent(inout) :: dydt_out(:)
      integer, optional, intent(out) :: error
      !> local variables
      real :: rtol = 1e-6, atol = 1e-6
      real :: tin, tout
      integer, save :: j=1
      integer :: itask

      ! local_sys => sys
      local_fun => sys%fun
      local_params => sys%params


      !> itask=2 - take one setp only and return
      tin = t
      tout = t+h
      itask = 1

      !> Initialize vode to use Adams methods suitable for the nonstiff
      !! problems.
      this%vode_options = set_opts(relerr=rtol,abserr=atol,tcrit=17.0)

      if( this%istate == 1 ) then
         call dvode_f90(f,this%dim,y,tin,tout,itask,this%istate,this%vode_options)
         this % istate = 1
      else
         call get_stats(this%rstats,this%istats)
         tin = 0.0
         tout = this%rstats(13) + h
         print *, tin, tout
         call dvode_f90(f,this%dim,y,tin,tout,itask,this%istate,this%vode_options)
         ! stop
      end if

      ! call dvode_f90(f,this%dim,y,tin,tout,2,this%istate,this%vode_options)
      ! this % istate = 2


      ! ! HU, HCUR, TCUR
      ! print *, this%rstats(11), this%rstats(12), this%rstats(13)
      ! ! nst, nfe, nqu
      ! print *, this%istats(11), this%istats(12), this%istats(14)

      if( present(error) ) error = FPDE_STATUS_OK

   end subroutine apply

   subroutine reset( this, error)
      class(ode_stepper_adams), intent(inout) :: this
      integer, optional, intent(out) :: error

      if( present(error) ) error = FPDE_STATUS_OK
   end subroutine reset

   subroutine refine_step( this, sys, t, y0, y1, yerr, dydt_in, dydt_out, c, hold, hnew, accept, error )
      class(ode_stepper_adams), intent(inout) :: this
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

      hnew = hold
      accept = .true.

      if( present(error) ) error = FPDE_STATUS_OK

   end subroutine refine_step

end module class_ode_stepper_adams




