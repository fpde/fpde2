module class_boundary

  use class_platonic
  use class_icicles
  use constants_module

  private

  type, public, abstract, extends(platonic) :: boundary
     private
     character(len=:), allocatable :: param_names(:)
   contains
     procedure :: init
     procedure(gen_val), deferred :: generate_values
     procedure, non_overridable :: get_param_names
     procedure, non_overridable :: set_param_names
  end type boundary

  interface
     subroutine gen_val(self, ic, fin, fout, xin, error)
       import boundary, icicles
       class(boundary) :: self
       type(icicles), intent(in) :: ic
       integer, intent(out), optional :: error
       real, intent(in) :: fin(:,:), xin(:,:)
       real, intent(out) :: fout(:,:)
     end subroutine gen_val
  end interface

contains

  subroutine init(p, error)
    class(boundary), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Boundary"
  end subroutine init

  function get_param_names(self)
    class(boundary) :: self
    ! character(len=:), allocatable :: get_param_names(:)
    character(len=:), allocatable :: get_param_names(:)

    get_param_names = self%param_names
  end function get_param_names

  subroutine set_param_names(self, names)
    class(boundary) :: self
    character(len=*) :: names(:)

    self%param_names = names
  end subroutine set_param_names


end module class_boundary
