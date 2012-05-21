module class_ode_stepper_rkv65

   use constants_module
   use class_ode_stepper_rk_explicit_abstract

   private

   type, public, extends(ode_stepper_rk_explicit_abstract) :: ode_stepper_rkv65

   contains

      procedure :: init

   end type ode_stepper_rkv65

contains

   subroutine init(p,error)
      class(ode_stepper_rkv65), intent(inout) :: p
      integer, optional, intent(out) :: error
      integer :: err

      err = FPDE_STATUS_OK

      !> setting constant parameters
      p % can_use_dydt_in = .true.
      p % gives_exact_dydt_out = .true.
      p % gives_estimated_yerr = .true.
      p % method_order = 6
      p % name = "rkv65"
      p % stages = 8
      p % status = FPDE_STATUS_OK ! @todo

      call p%ode_stepper_rk_explicit_abstract%init(err)

      !> filling Butcher tableu
      if ( err .eq. FPDE_STATUS_OK ) then

         p % bt % c = [ 0., 1./6., 4./15., 2./3., 5./6., 1., 1./15., 1. ]

         p % bt % a(1,:) = [ 0.,0.,0.,0.,0.,0.,0.,0. ]
         p % bt % a(2,:) = [ 1./6.,0.,0.,0.,0.,0.,0.,0. ]
         p % bt % a(3,:) = [ 4./75.,16./75.,0.,0.,0.,0.,0.,0. ]
         p % bt % a(4,:) = [ 5./6.,-(8./3.),5./2.,0.,0.,0.,0.,0. ]
         p % bt % a(5,:) = [ -(165./64.),55./6.,-(425./64.),85./96.,0.,0.,0.,0. ]
         p % bt % a(6,:) = [ 12./5.,-8.,4015./612.,-(11./36.),88./255.,0.,0.,0. ]
         p % bt % a(7,:) = [ -(8263./15000.),124./75.,-(643./680.),-(81./250.),2484./10625.,0.,0.,0. ]
         p % bt % a(8,:) = [ 3501./1720., -300./43., 297275./52632., -319./2322., 24068./84065., 0., 3850./26703., 0. ]


         p % bt % b = [  3./40., 0., 875./2244., 23./72., 264./1955., 0., 125./11592., 43./616. ]

         p % bt % ec = [ 1./160., 0., 125./17952., -(1./144.), 12./1955., 3./44., -(125./11592.), -(43./616.) ]
      end if

      if ( present(error) ) then
         error = err
      end if

   end subroutine init

end module class_ode_stepper_rkv65
