module class_mesh2d

  use class_mesh

  private

  type, public, abstract, extends(mesh) :: mesh2d
   contains
     procedure(diff_global2d_i), deferred :: diff
     procedure, non_overridable :: get_dim
  end type mesh2d

  abstract interface
     subroutine diff_global2d_i(self, f, x, y, df, k)
       import mesh2d
       class(mesh2d), target :: self
       real, intent(in) :: x(:,:), y(:,:), f(:,:)
       real, intent(out) :: df(:,:)
       integer, intent(in) :: k(2)
     end subroutine diff_global2d_i
  end interface

contains

  function get_dim( self ) result(dim)
    class(mesh2d), intent(in) :: self
    integer :: dim
    dim = 2
  end function get_dim

end module class_mesh2d
