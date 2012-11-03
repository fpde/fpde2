module class_named_vector_

  use class_named_vector_user_
  use class_icicles_user_

  private

  type, public, abstract, extends(named_vector_user) :: named_vector
     private
   contains
     procedure(point_i), deferred :: point
  end type named_vector


  abstract interface

     subroutine point_i(self, v)
       import named_vector
       class(named_vector) :: self
       real, target, intent(in) :: v(:)
     end subroutine point_i

  end interface


end module class_named_vector_
