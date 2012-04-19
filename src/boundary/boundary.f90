module class_boundary

  use constants_module
  use class_platonic

  private

  type, abstract, public, extends(platonic) :: boundary
   contains
     procedure(gen_val), deferred :: generate_values
  end type boundary

  interface
     subroutine gen_val(p, from, to, error)
       import boundary
       class(boundary) :: p
       integer, intent(out), optional :: error
       real, intent(in) :: from(:)
       real, intent(out) :: to(:)
     end subroutine gen_val
  end interface

end module class_boundary
