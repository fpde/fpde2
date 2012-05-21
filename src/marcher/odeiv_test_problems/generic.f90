!>
!! @file   generic.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Mar 29 19:53:53 2012
!!
!! @brief  ODE Initial Value test problem generic object.
!!
!! @todo
!! - [ ] add analytical solution function
!!
module class_odeiv_generic

   use constants_module
   use logger_module
   use class_ode_system

   private

   type, public, extends(named) :: odeiv_generic

      !> Structure to gather function, jacobian, dimension and parameters
      type(ode_system) :: sys
      !> Description of a ODEIV problem
      character(len=FPDE_MSG_LEN) :: description = ""
      !> Initial data at time t(0)
      real, pointer, contiguous :: y0(:)
      !> Analytic or high precision solution at time t(1)
      real, pointer, contiguous :: y1(:)
      !> Integration time interval
      real :: t(0:1)=[0.0,0.0]
      !> Time step size
      real :: h = 0.0
      !> Flag to indicate if ODEIV problem has an analycial solution
      logical :: analytic_solution = .false.

   contains

      !> Initialization procedure.
      procedure :: init
      !> Info procedure.
      procedure :: info
      !> Memory free procedure.
      procedure :: free

   end type odeiv_generic

contains

   subroutine init(this)
      class(odeiv_generic) :: this
   end subroutine init

   subroutine info(this)
      class(odeiv_generic) :: this
      integer :: j

      print *, 'ODE initial value problem: ', trim(this % name)
      print *, 'description:               ', trim(this % description)
      print *, 'dim:                       ', this % sys % dim
      print *, 'integration inverval'
      print *, '[t(0),t(1)] =              ', this % t
      print *, 'integration step size      ', this % h
      print *, 'initial data: y0 =         '
      do j=1,this%sys%dim
         print *, this % y0(j)
      end do
      print *, 'final data: y1 =           '
      do j=1,this%sys%dim
         print *, this % y1(j)
      end do
      print *, 'has analytical solution:   ', this % analytic_solution

   end subroutine info

   subroutine free(this)
      class(odeiv_generic) :: this

      if ( associated( this % y0 ) ) then
         deallocate( this % y0 )
         this % y0 => null()
      end if

      if ( associated( this % y1 ) ) then
         deallocate( this % y1 )
         this % y1 => null()
      end if

   end subroutine free

end module class_odeiv_generic
