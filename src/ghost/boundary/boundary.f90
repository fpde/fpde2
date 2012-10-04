module class_boundary_ghost

  use class_boundary
  use constants_module

  private

  type, public, abstract, extends(boundary) :: boundary_ghost
     private
     character(len=:), allocatable :: param_names(:)
   contains
     ! procedure :: init
     procedure(gen_val), deferred :: generate_values
     procedure, non_overridable :: get_param_names
     procedure, non_overridable :: set_param_names
  end type boundary_ghost

  interface
     subroutine gen_val(self, fin, fout, xin, params, error)
       import boundary_ghost
       class(boundary_ghost) :: self
       real, intent(in) :: params(:,:)
       real, intent(in) :: fin(:,:), xin(:,:)
       real, intent(out) :: fout(:,:)
       integer, intent(out), optional :: error
     end subroutine gen_val
  end interface

contains

  function get_param_names(self)
    class(boundary_ghost) :: self
    ! character(len=:), allocatable :: get_param_names(:)
    character(len=:), allocatable :: get_param_names(:)

    get_param_names = self%param_names
  end function get_param_names

  subroutine set_param_names(self, names)
    class(boundary_ghost) :: self
    character(len=*) :: names(:)

    self%param_names = names
  end subroutine set_param_names


end module class_boundary_ghost
