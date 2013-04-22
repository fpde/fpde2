module class_named_vector_user

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

     !> returns a pointer to the value of the vector, i.e.
     !! the following code should return true
     !!
     !! real, target :: v(:)
     !! call nv%point(v)
     !! associated(nv%val(),v)
     !!
     !! @return pointer to the value of the named_vector
     !!
     function vec_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       real, pointer :: vec_i(:)
     end function vec_i

     !> Similar to vec, but returns a scalar pointer to the first element
     !! of the result of vec()
     !!
     !! @return scalar pointer to the first element of the result of
     !! self%vec()
     !!
     function scal_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       real, pointer :: scal_i
     end function scal_i

     !> Returns the length of vec()
     !!
     !! @return length(self%vec())
     !!
     function length_i(self)
       import named_vector_user
       class(named_vector_user), intent(in) :: self
       integer :: length_i
     end function length_i

  end interface

  public :: nvtor

contains

  !> implementation of the assignment to self%vec(), equivalent to
  !! self%vec()=v
  !!
  !! @param v serves as a right hand side of the assignment
  !!
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


  !> Conversion from named_vector to real(:), equivalent to vec()
  !!
  !! The following is forbidden in Fortran 2008
  !! g = f%dx(alpha(:,1))%val()
  !! to avoid storing the result of f%dx(alpha(:,1)) we can write
  !! g = nvtor(f%dx(alpha(:,1))).
  !!
  !! I don't remember now why not to overload the assignment operator
  !! for nemed_vector
  !!
  !! @todo Probably should be removed, its purpose is too obscure and
  !! its presence is not justified enough.
  !!
  !! @param nvu named_vector to convert
  !!
  !! @return nvu%val()
  !!
  function nvtor(nvu) result(r)
    class(named_vector_user), intent(in) :: nvu
    real, pointer :: r(:)
    r => nvu%vec()
  end function nvtor

end module class_named_vector_user
