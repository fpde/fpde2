module class_boundary_dirichlet

  use constants_module
  use class_boundary

  private

  type, public, extends(boundary) :: boundary_dirichlet
   contains
     procedure :: generate_values
  end type boundary_dirichlet

contains

  subroutine generate_values(p, from, to, error)
    class(boundary_dirichlet) :: p
    integer, intent(out), optional :: error
    real, intent(in) :: from(:)
    real, intent(out) :: to(:)

    if(present(error)) error = FPDE_STATUS_OK

    if(size(from) == 0) return

    to = -from
    to(1) = 0.0

  end subroutine generate_values

end module class_boundary_dirichlet


