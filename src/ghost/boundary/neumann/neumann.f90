module class_boundary_ghost_neumann

  use class_boundary_ghost
  use logger_module
  use constants_module

  private

  type, public, extends(boundary_ghost) :: boundary_ghost_neumann
   contains
     procedure :: generate_values
     procedure :: p_names
  end type boundary_ghost_neumann

contains


  function p_names(self)
    class(boundary_ghost_neumann) :: self
    character(len=:), allocatable :: p_names(:)
    p_names = ["a"]
  end function p_names


  subroutine generate_values(self, fin, fout, xin, params, error)
    class(boundary_ghost_neumann) :: self
    integer, intent(out), optional :: error
    real, intent(in) :: fin(:,:), xin(:,:), params(:,:)
    real, intent(out) :: fout(:,:)

    integer :: n, i, err, nrows

    if(present(error)) error = FPDE_STATUS_OK

    nrows = size(fout,2)

    do i = 1, nrows
       fout(:,i) = fin(2:,i)+2*(xin(1,i)-xin(2:,i))*params(i,1)
    end do

  end subroutine generate_values

end module class_boundary_ghost_neumann
