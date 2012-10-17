module ghost_boundary_factory

  use constants_module
  use class_boundary
  use class_boundary_ghost_dirichlet
  use class_boundary_ghost_neumann

  private

  character(len=*), parameter :: nprefix = "Boundary(ghost) "
  character(len=*), parameter :: tprefix = "ghost_"

  public :: ghost_boundary_new

contains

  function ghost_boundary_new(id, error) result(s)
    class(boundary), pointer :: s
    integer, optional, intent(out) :: error
    character(len=*) :: id

    if(present(error)) error = FPDE_STATUS_OK

    select case(trim(id))
    case( "dirichlet" )
       allocate( boundary_ghost_dirichlet :: s )
       s%name = nprefix // id
       s%type = tprefix // id
    case( "neumann" )
       allocate( boundary_ghost_neumann :: s )
       s%name = nprefix // id
       s%type = tprefix // id
    case default
       if(present(error)) error = FPDE_STATUS_ERROR
       nullify( s )
    end select

  end function ghost_boundary_new

end module ghost_boundary_factory
