module class_mesh1d

  use class_mesh

  private

  type, public, abstract, extends(mesh) :: mesh1d

   contains
     procedure(diff_global1d_interface), deferred :: diff
     procedure, non_overridable :: get_dim
  end type mesh1d

  interface
     subroutine diff_global1d_interface( self, f, x, df, k )
       import mesh1d
       class(mesh1d), target, intent(inout) :: self
       integer, intent(in) :: k
       real, intent(in)  :: f(:), x(:)
       real, intent(out) :: df(:)
     end subroutine diff_global1d_interface
  end interface

contains

  function get_dim( self ) result(dim)
    class(mesh1d), intent(in) :: self
    integer :: dim
    dim = 1
  end function get_dim

end module class_mesh1d
