module class_boundary_periodic

  use constants_module
  use class_boundary

  private

  type, public, extends(boundary) :: boundary_periodic
   contains
     procedure :: generate_values
  end type boundary_periodic

contains

  !>
  !! \$ f_0 = f_n \$, \$ f_{-1} = f_{n-1} \$, etc.
  !!
  subroutine generate_values(this, from, to, error)
    class(boundary_periodic) :: this
    integer, intent(out), optional :: error
    real, intent(in) :: from(:)
    real, intent(out) :: to(0:) ! align the table to start from 0

    integer :: n, m

    if(present(error)) error = FPDE_STATUS_OK

    n = ubound(to,1)
    m = ubound(from,1)
    to(0:n) = from(m:1:-1)

  end subroutine generate_values

end module class_boundary_periodic
