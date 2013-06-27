module class_icicles_user

  use class_named_vector_user
  use class_platonic

  private

  type, public, abstract, extends(platonic) :: icicles_user
   contains
     procedure(get_i), deferred :: get
     procedure :: getvec
  end type icicles_user


  abstract interface

     function get_i(self, name)
       import icicles_user, named_vector_user
       class(icicles_user), intent(in) :: self
       character(len=*), intent(in) :: name
       class(named_vector_user), pointer :: get_i
     end function get_i

  end interface

contains

  !> Similar to get() but gets the pointer to vector representation of
  !! an entry
  !! @param name entry name
  !!
  !! @return pointer to a vector contained in a named_vector
  function getvec(self, name) result(r)
    class(icicles_user), intent(in) :: self
    character(len=*), intent(in) :: name

    real, pointer :: r(:)

    class(named_vector_user), pointer :: nv

    r => null()
    nv => self%get(name)
    if( associated(nv) ) r => nv%vec()

  end function getvec


end module class_icicles_user
