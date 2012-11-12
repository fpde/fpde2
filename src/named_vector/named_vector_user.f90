module class_named_vector_user_

  use class_platonic

  private

  type, public, abstract, extends(platonic) :: named_vector_user
     private
   contains
     procedure(vec_i), deferred :: vec
     procedure(scal_i), deferred :: scal
     procedure(length_i), deferred :: length
     generic :: assignment(=) => ass_real

     procedure, private :: ass_real
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

     function length_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       integer :: length_i
     end function length_i

  end interface

  public :: nvtor

contains

  subroutine ass_real(self, v)
    class(named_vector_user), intent(inout) :: self
    real, intent(in) :: v(:)

    real, pointer :: my_v(:)

    my_v => self%vec()

    if( .not. associated(my_v) ) then
       call self%loge("assignment(=): Trying to assign to a null pointer")
       return
    end if

    my_v = v
  end subroutine ass_real


  function nvtor(nvu) result(r)
    class(named_vector_user), intent(in) :: nvu
    real, pointer :: r(:)
    r => nvu%vec()
  end function nvtor

end module class_named_vector_user_
