module class_boundary_ghost_dirichlet

  use class_boundary_ghost
  use logger_module
  use constants_module

  private

  type, public, extends(boundary_ghost) :: boundary_ghost_dirichlet
   contains
     procedure :: generate_values
     procedure :: p_names
  end type boundary_ghost_dirichlet

contains


  function p_names(self)
    class(boundary_ghost_dirichlet) :: self
    character(len=:), allocatable :: p_names(:)
    p_names = ["a"]
  end function p_names


  subroutine generate_values(self, fin, fout, xin, params, error)
    class(boundary_ghost_dirichlet) :: self
    integer, intent(out), optional :: error
    real, intent(in) :: fin(:,:), xin(:,:), params(:,:)
    real, intent(out) :: fout(:,:)

    integer :: i, err

    if(present(error)) error = FPDE_STATUS_OK

    do i = 1, size(fout,2)
       fout(1:,i) = 2*params(1:,i)-fin(2:,i)
    end do

  end subroutine generate_values

end module class_boundary_ghost_dirichlet
