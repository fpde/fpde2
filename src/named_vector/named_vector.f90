module class_named_vector_

  use class_named_vector_user_
  use class_icicles_user_

  private

  type, public, abstract, extends(named_vector_user) :: named_vector
     private
   contains
     procedure(point_i), deferred :: point
     procedure(initialize_i), deferred :: initialize
  end type named_vector


  abstract interface

     subroutine point_i(self, v)
       import named_vector
       class(named_vector) :: self
       real, target, intent(in) :: v(:)
     end subroutine point_i


     subroutine initialize_i(self, ic)
       import named_vector, icicles_user
       class(named_vector) :: self
       class(icicles_user) :: ic
     end subroutine initialize_i


  end interface


end module class_named_vector_
