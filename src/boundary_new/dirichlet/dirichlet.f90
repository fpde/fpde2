module class_boundary_dirichlet

  use class_boundary
  use class_passive_icicles
  use logger_module
  use constants_module

  private

  character(len=*), parameter :: a = "a"

  type, public, extends(boundary) :: boundary_dirichlet
   contains
     procedure :: init
     procedure :: generate_values
  end type boundary_dirichlet

contains

  subroutine init(p, error)
    class(boundary_dirichlet), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Boundary_dirichlet"
    call p%set_param_names([a])
  end subroutine init


  subroutine generate_values(self, ic, fin, fout, xin, error)
    class(boundary_dirichlet) :: self
    class(passive_icicles), intent(in) :: ic
    integer, intent(out), optional :: error
    real, intent(in) :: fin(:,:), xin(:,:)
    real, intent(out) :: fout(:,:)

    real, pointer :: av(:), v(:)
    integer :: i, err

    if(present(error)) error = FPDE_STATUS_OK

    call ic%get(a,vec=v,error=err)

    if( err /= FPDE_STATUS_OK ) then
       if(present(error)) error = err
       call self%log(FPDE_LOG_ERROR,&
            "Parameter ["//trim(a)//"] not found in icicles")
       return
    end if

    av => v(1:size(fout,2))

    do i = 1, size(fout,2)
       fout(:,i) = 2*av(i)-fin(2:,i)
    end do

  end subroutine generate_values

end module class_boundary_dirichlet
