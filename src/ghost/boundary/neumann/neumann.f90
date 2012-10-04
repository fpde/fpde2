module class_boundary_ghost_neumann

  use class_boundary_ghost
  use logger_module
  use constants_module

  private

  character(len=*), parameter :: a = "z"

  type, public, extends(boundary_ghost) :: boundary_ghost_neumann
   contains
     ! procedure :: init
     procedure :: generate_values
  end type boundary_ghost_neumann

contains

  ! subroutine init(p, error)
  !   class(boundary_neumann), target :: p
  !   integer, optional, intent(out) :: error
  !   if(present(error)) error = FPDE_STATUS_OK
  !   p%name = "Boundary_neumann"
  !   call p%set_param_names([a])
  ! end subroutine init


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
