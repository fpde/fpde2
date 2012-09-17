module class_boundary_neumann

  use class_boundary
  use class_passive_icicles
  use logger_module
  use constants_module

  private

  character(len=*), parameter :: a = "z"

  type, public, extends(boundary) :: boundary_neumann
   contains
     procedure :: init
     procedure :: generate_values
  end type boundary_neumann

contains

  subroutine init(p, error)
    class(boundary_neumann), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Boundary_neumann"
    call p%set_param_names([a])
  end subroutine init


  subroutine generate_values(self, ic, fin, fout, xin, error)
    class(boundary_neumann) :: self
    class(passive_icicles), intent(in) :: ic
    integer, intent(out), optional :: error
    real, intent(in) :: fin(:,:), xin(:,:)
    real, intent(out) :: fout(:,:)

    real, pointer :: v(:)
    integer :: n, i, err, nrows

    if(present(error)) error = FPDE_STATUS_OK

    call ic%get(a,vec=v,error=err)

    if( err /= FPDE_STATUS_OK ) then
       if(present(error)) error = err
       call self%log(FPDE_LOG_ERROR,&
            "Parameter ["//trim(a)//"] not found in icicles")
       return
    end if

    nrows = size(fout,2)

    do i = 1, nrows
       fout(:,i) = fin(2:,i)+2*(xin(1,i)-xin(2:,i))*v(i)
    end do

  end subroutine generate_values

end module class_boundary_neumann
