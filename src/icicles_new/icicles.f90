module class_icicles_

  use class_icicles_user_
  use class_named_vector_
  use class_named_vector_user_

  private

  type, public, abstract :: crit
   contains
     procedure(test_i), deferred :: test
  end type crit

  type, public, abstract, extends(icicles_user) :: icicles
     private
   contains
     procedure(add_i),     deferred :: add
     procedure(i_point_i), deferred :: point
     procedure(length_i),  deferred :: length
     procedure(initialize_i), deferred :: initialize
  end type icicles


  abstract interface

     subroutine add_i(self, nv)
       import icicles, named_vector
       class(icicles) :: self
       class(named_vector), target, intent(in) :: nv
     end subroutine add_i

     function test_i(self, nv)
       import crit, named_vector, named_vector_user
       class(crit) :: self
       class(named_vector_user), intent(in) :: nv
       logical :: test_i
     end function test_i

     subroutine i_point_i(self, v, cr)
       import icicles, crit
       class(icicles), intent(in) :: self
       real, target, intent(in) :: v(:)
       class(crit), optional :: cr
     end subroutine i_point_i

     function length_i(self, cr)
       import icicles, crit
       class(icicles), intent(in) :: self
       class(crit), optional :: cr
       integer :: length_i
     end function length_i

     subroutine initialize_i(self)
       import icicles, crit
       class(icicles), intent(in) :: self
     end subroutine initialize_i

  end interface

end module class_icicles_
