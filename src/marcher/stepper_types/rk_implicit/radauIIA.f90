module class_ode_stepper_radauIIA

   use constants_module
   use class_ode_stepper_rk_implicit_abstract
   use kronecker_product

   private

   type, public, extends(ode_stepper_rk_implicit_abstract) :: ode_stepper_radauIIA

      real, pointer :: Tmat(:,:)

   contains

      procedure :: init

   end type ode_stepper_radauIIA

contains

   subroutine init(p,error)
      class(ode_stepper_radauIIA), intent(inout) :: p
      integer, optional, intent(out) :: error
      integer :: err

      integer :: n
      real, pointer :: idm(:,:)

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

      call p%ode_stepper_rk_implicit_abstract%init(err)

      !> filling Butcher tableu
      if ( err .eq. FPDE_STATUS_OK ) then

         p % bt % c = [ (4 - Sqrt(6.))/10., (4 + Sqrt(6.))/10., 1. ]

         p % bt % a(1,:) = [ (88 - 7*Sqrt(6.))/360., (296 - 169*Sqrt(6.))/1800., (-2 + 3*Sqrt(6.))/225. ]
         p % bt % a(2,:) = [ (296 + 169*Sqrt(6.))/1800., (88 + 7*Sqrt(6.))/360., (-2 - 3*Sqrt(6.))/225. ]
         p % bt % a(3,:) = [ (16 - Sqrt(6.))/36., (16 + Sqrt(6.))/36., 1./9. ]

         p % bt % b = [ (16 - Sqrt(6.))/36., (16 + Sqrt(6.))/36., 1./9. ]

         p % d = [ 0.0, 0.0, 1.0 ]

         allocate(p % Tmat(3,3))

         ! p % bt % ec  !@todo

         idm => identity_matrix(n)

         call kronecker(A=p % bt % A, B=idm, C= p % AI)

         deallocate(idm)

      end if

      if ( present(error) ) then
         error = err
      end if

   end subroutine init

end module class_ode_stepper_radauIIA
