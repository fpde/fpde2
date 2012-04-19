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
      real, pointer, contiguous :: y0(:) => null()
      !> Analytic or high precision solution at time t(1)
      real, pointer, contiguous :: y1(:) => null()
      !> Integration time interval
      real :: t(0:1)=[0.0,0.0]
      !> Time step size
      real :: h = 0.0
      !> Flag to indicate if ODEIV problem has an analycial solution
      logical :: analytical_solution = .false.

   contains

      !> Initialization procedure.
      procedure :: init
      !> Info procedure.
      procedure :: info
      !> Memory free procedure.
      procedure :: free

   end type odeiv_generic

contains

   subroutine init( odeiv )
      class(odeiv_generic) :: odeiv
   end subroutine init

   subroutine info( odeiv )
      class(odeiv_generic) :: odeiv
      integer :: j

      print *, 'ODE initial value problem: ', trim(odeiv % name)
      print *, 'description:               ', trim(odeiv % description)
      print *, 'dim:                       ', odeiv % sys % dim
      print *, 'integration inverval'
      print *, '[t(0),t(1)] =              ', odeiv % t
      print *, 'integration step size      ', odeiv % h
      print *, 'initial data: y0 =         '
      do j=1,odeiv%sys%dim
         print *, odeiv % y0(j)
      end do
      print *, 'final data: y1 =           '
      do j=1,odeiv%sys%dim
         print *, odeiv % y1(j)
      end do
      print *, 'has analytical solution:   ', odeiv % analytical_solution

   end subroutine info

   subroutine free( odeiv )
      class(odeiv_generic) :: odeiv

      if ( associated( odeiv % y0 ) ) then
         deallocate( odeiv % y0 )
         odeiv % y0 => null()
      end if

      if ( associated( odeiv % y1 ) ) then
         deallocate( odeiv % y1 )
         odeiv % y1 => null()
      end if

   end subroutine free

end module class_odeiv_generic
