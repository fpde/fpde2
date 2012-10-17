module class_boundary_ghost_dirichlet

  use class_boundary_ghost
  use logger_module
  use constants_module

  private

  character(len=*), parameter :: a = "a"

  type, public, extends(boundary_ghost) :: boundary_ghost_dirichlet
   contains
     ! procedure :: init
     procedure :: generate_values
  end type boundary_ghost_dirichlet

contains

  ! subroutine init(p, error)
  !   class(boundary_dirichlet), target :: p
  !   integer, optional, intent(out) :: error
  !   if(present(error)) error = FPDE_STATUS_OK
  !   p%name = "Boundary_dirichlet"
  !   call p%set_param_names([a])
  ! end subroutine init


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
