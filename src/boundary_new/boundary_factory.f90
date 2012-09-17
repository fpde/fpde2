module boundary_factory

  use constants_module
  use class_boundary
  use class_boundary_dirichlet
  use class_boundary_neumann

  private

  character(len=*), parameter :: prefix = "Boundary "

  public :: boundary_new

contains

  function boundary_new(id, error) result(s)
    class(boundary), pointer :: s
    integer, optional, intent(out) :: error
    character(len=*) :: id

    if(present(error)) error = FPDE_STATUS_OK

    select case(trim(id))
    case( "dirichlet" )
       allocate( boundary_dirichlet :: s )
       s%name = prefix // id
       s%type = id
    case( "neumann" )
       allocate( boundary_neumann :: s )
       s%name = prefix // id
       s%type = id
    case default
       if(present(error)) error = FPDE_STATUS_ERROR
       nullify( s )
    end select

  end function boundary_new

end module boundary_factory
