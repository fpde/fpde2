module class_icicles

  use class_icicles_user
  use class_named_vector_user

  private

  type, public, abstract :: crit
   contains
     procedure(test_i), deferred :: test
  end type crit

  type, public, abstract, extends(icicles_user) :: icicles
     private
   contains
     procedure(add_i),     deferred :: add
     procedure(point_i), deferred :: point
     procedure(length_i),  deferred :: length
     procedure(initialize_i), deferred :: initialize
  end type icicles


  abstract interface

     !> Adds a vectors to the icicles, it is possible to add the same
     !! vector several times.
     !!
     !! @param nv vector to be added
     !!
     subroutine add_i(self, nv)
       import icicles, named_vector_user
       class(icicles) :: self
       class(named_vector_user), target, intent(in) :: nv
     end subroutine add_i

     !> ???
     function test_i(self, nv)
       import crit, named_vector_user
       class(crit) :: self
       class(named_vector_user), intent(in) :: nv
       logical :: test_i
     end function test_i

     !> Function used to chagne the target of pointers in all (or
     !! some, see cr) vectors. The order in which vectors are pointed
     !! to v is not defined, although it has to be consistent through
     !! the calls (e.g. if called several times in a row the resulting
     !! order cannot vary).
     !!
     !! @param v  vectors used as a target, must be of the
     !! appropriate size
     !!
     !! @param cr optional criterion, if not present all vectors will
     !! be used
     !!
     subroutine point_i(self, v, cr)
       import icicles, crit
       class(icicles), intent(in) :: self
       real, target, intent(in) :: v(:)
       class(crit), optional :: cr
     end subroutine point_i

     !> Function used to determine the minimal length of the vector
     !! needed by point(). cr has the same meaning as in point()
     !!
     !! @param cr optional criterion, if not present all vectors will
     !! be used
     !!
     !! @return length of the vectors full
     !!
     function length_i(self, cr)
       import icicles, crit
       class(icicles), intent(in) :: self
       class(crit), optional :: cr
       integer :: length_i
     end function length_i

     !> initializer for icicles
     subroutine initialize_i(self)
       import icicles, crit
       class(icicles), intent(in) :: self
     end subroutine initialize_i

  end interface

end module class_icicles
