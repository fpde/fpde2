module solver_factory

  use constants_module
  use class_solver
  use class_solver_simple

contains

  function solver_new(id, error) result(s)
    class(solver), pointer :: s
    integer, optional, intent(out) :: error
    character(len=*) :: id

    if(present(error)) error = FPDE_STATUS_OK

    select case(trim(id))
    case( "solver" )
       allocate( solver :: s )
    case( "solver_simple" )
       allocate( solver_simple :: s )
    case default
       if(present(error)) error = FPDE_STATUS_ERROR
       nullify( s )
    end select

  end function solver_new

end module solver_factory
