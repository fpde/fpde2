module class_boundary_ghost

  use class_platonic
  use constants_module

  private

  type, public, abstract, extends(platonic) :: boundary_ghost
     private
   contains
     procedure(gen_val), deferred :: generate_values
     procedure :: p_names
  end type boundary_ghost

  interface
     subroutine gen_val(self, fin, fout, xin, params, error)
       import boundary_ghost
       class(boundary_ghost) :: self
       real, intent(in) :: params(:)
       real, intent(in) :: fin(:), xin(:)
       real, intent(out) :: fout(:)
       integer, intent(out), optional :: error
     end subroutine gen_val
  end interface

contains

  function p_names(self)
    class(boundary_ghost) :: self
    character(len=:), allocatable :: p_names(:)
    allocate( character(len=0) :: p_names(0) )
  end function p_names

end module class_boundary_ghost
