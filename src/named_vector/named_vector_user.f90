module class_named_vector_user_

  use class_platonic

  private

  type, public, abstract, extends(platonic) :: named_vector_user
     private
   contains
     procedure(vec_i), deferred :: vec
     procedure(scal_i), deferred :: scal
     procedure(shape_i), deferred :: shape
     procedure(length_i), deferred :: length
  end type named_vector_user


  abstract interface

     function vec_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       real, pointer :: vec_i(:)
     end function vec_i

     function scal_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       real, pointer :: scal_i
     end function scal_i

     function shape_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       integer, allocatable :: shape_i(:)
     end function shape_i

     function length_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       integer :: length_i
     end function length_i

  end interface

end module class_named_vector_user_
