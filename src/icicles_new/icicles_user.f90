module class_icicles_user_

  use class_named_vector_user_
  use class_platonic

  private

  type, public, abstract, extends(platonic) :: icicles_user
   contains
     procedure(get_i), deferred :: get
  end type icicles_user


  abstract interface

     function get_i(self, name)
       import icicles_user, named_vector_user
       class(icicles_user), intent(in) :: self
       character(len=*), intent(in) :: name
       class(named_vector_user), pointer :: get_i
     end function get_i

  end interface


end module class_icicles_user_
