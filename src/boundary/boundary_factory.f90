module boundary_factory

  use constants_module
  use class_boundary
  use class_solver_simple
  use class_boundary_dirichlet
  use class_boundary_periodic

contains

  function boundary_new(id, error) result(s)
    class(boundary), pointer :: s
    integer, optional, intent(out) :: error
    character(len=*) :: id

    if(present(error)) error = FPDE_STATUS_OK

    select case(trim(id))
    case( "boundary_dirichlet" )
       allocate( boundary_dirichlet :: s )
    case( "boundary_periodic" )
       allocate( boundary_periodic :: s )
    case default
       if(present(error)) error = FPDE_STATUS_ERROR
       nullify( s )
    end select

  end function boundary_new

end module boundary_factory
