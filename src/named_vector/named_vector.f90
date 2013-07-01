module class_named_vector

  use class_platonic

  private

  type, public, extends(platonic) :: named_vector
     private
     real, pointer :: val(:) => null()
     integer :: length_ = 0
   contains
     procedure :: vec
     procedure :: scal
     procedure :: length
     procedure :: length_set
     procedure :: point
  end type named_vector


  interface named_vector
     module procedure :: nvi_constructor
  end interface named_vector

  public :: nvi_constructor

contains

  function nvi_constructor(name, length) result(r)
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: length

    type(named_vector), pointer :: r

    allocate(r)

    r%name = trim(name)

    if(present(length)) then
       r%length_ = length
    end if

  end function nvi_constructor


  !> returns a pointer to the value of the vector, i.e.
  !! the following code should return true
  !!
  !! real, target :: v(:)
  !! call nv%point(v)
  !! associated(nv%val(),v)
  !!
  !! @return pointer to the value of the named_vector
  !!
  function vec(self)
    class(named_vector), intent(in) :: self
    real, pointer :: vec(:)
    vec => self%val

    if( .not. associated(vec) ) then
       call self%loge("vec(): Trying to extract a null vector")
    end if

  end function vec

  !> Similar to vec, but returns a scalar pointer to the first element
  !! of the result of vec()
  !!
  !! @return scalar pointer to the first element of the result of
  !! self%vec()
  !!
  function scal(self)
    class(named_vector), intent(in) :: self
    real, pointer :: scal
    scal => self%val(1)
  end function scal

  !> Returns the length of vec()
  !!
  !! @return length(self%vec())
  !!
  function length(self)
    class(named_vector), intent(in) :: self
    integer :: length
    length = self%length_
  end function length

  !> Changes the length of the named_vector
  !! @param length
  !!
  subroutine length_set(self, length)
    class(named_vector) :: self
    integer, intent(in) :: length
    self%length_ = length
  end subroutine length_set

  !> The only difference from named_vector_user is the presence of
  !! point(). This method accepts a 1d array on input and uses it
  !! to store all the values associated with vector.
  !!
  !! @param v target array.
  !!
  subroutine point(self, v)
    class(named_vector) :: self
    real, target, intent(in) :: v(:)

    integer :: len

    len = self%length()

    if( size(v) < len ) then
       call self%loge("point(): Cannot change pointer,&
            & target too short")
       return
    end if

    self%val => v(1:len)

  end subroutine point

end module class_named_vector
