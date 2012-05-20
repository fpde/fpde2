module class_boundary

  use constants_module
  use class_platonic

  private

  type, abstract, public, extends(platonic) :: boundary
   contains
     procedure(gen_val), deferred :: generate_values
  end type boundary

  interface
     !> Assuming the function values are stored in \$ f_n \$ with
     !! \$ n=1,2,...,n \$ fills in the points to the _left_ of \$ f_{1} \$
     !! i.e. \$ f_0, f_{-1}, f_{-2} \$.
     !!
     !! @param from[in] table containing values of f_{1}, f_{ 2}, f_{ 3}, ...
     !! @param to[out] table containing values of  f_{0}, f_{-1}, f_{-2}, ...
     !! @param error
     !!
     subroutine gen_val(this, from, to, error)
       import boundary
       class(boundary) :: this
       integer, intent(out), optional :: error
       real, intent(in) :: from(:)
       real, intent(out) :: to(:)
     end subroutine gen_val
  end interface

end module class_boundary
