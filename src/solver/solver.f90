! module class_solver

!   use constants_module
!   use logger_module
!   use icicles_module
!   use icicles_registry_module
!   use class_platonic
!   use platonic_from_lua_module

!   private

!   type, public, extends(platonic) :: solver
!      type(icicles), pointer :: ic => null()
!      type(icicles_registry), pointer :: icreg => null()
!    contains
!      ! procedure :: get => get_vector, get_scalar
!      ! procedure :: set => set_vector, set_scalar
!      procedure :: info
!      procedure :: from_lua
!   end type solver

! contains

!   subroutine from_lua(p, l, error)
!     use flu_module
!     use flu_get_module

!     class(solver) :: p
!     type(flu) :: l
!     integer, intent(out), optional :: error

!     integer :: err

!     call p%platonic%from_lua(l,error = err)

!     if(present(error)) error = err

!   end subroutine from_lua

!   subroutine info(s)
!     class(solver) :: s

!     print *, "Calling solver%info():"
!     print *, "solver%name=", trim(s%name)

!   end subroutine info


! end module class_solver
