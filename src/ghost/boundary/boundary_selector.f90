module ghost_boundary_selector

  use constants_module
  use class_boundary_ghost
  use class_boundary_ghost_dirichlet
  use class_boundary_ghost_neumann

  private

  character(len=*), parameter :: nprefix = "Boundary(ghost) "
  character(len=*), parameter :: tprefix = "ghost_"

  type(boundary_ghost_dirichlet), target, save :: dirichlet
  type(boundary_ghost_neumann),   target, save :: neumann

  public :: ghost_boundary_select

contains

  function ghost_boundary_select(id, error) result(s)
    character(len=*), intent(in) :: id
    integer, optional, intent(out) :: error

    class(boundary_ghost), pointer :: s

    if(present(error)) error = FPDE_STATUS_OK

    select case(trim(id))
    case( "dirichlet" )
       s => dirichlet
    case( "neumann" )
       s => neumann
    case default
       if(present(error)) error = FPDE_STATUS_ERROR
       nullify( s )
       return
    end select

  end function ghost_boundary_select

end module ghost_boundary_selector
