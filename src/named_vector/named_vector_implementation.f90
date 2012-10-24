module class_named_vector_implementation_

  use class_named_vector_
  use class_generic_function
  use class_icicles_user_

  private

  type, public, extends(named_vector) :: named_vector_implementation
     private
     real, pointer :: val(:) => null()
     integer :: length_ = 0
     class(generic_function), pointer :: initial_ => null()
   contains
     procedure :: vec
     procedure :: scal
     procedure :: length
     procedure :: point
     procedure :: initialize
  end type named_vector_implementation


  interface named_vector_implementation
     module procedure :: nvi_constructor
  end interface named_vector_implementation


contains

  function nvi_constructor(name, length, initial) result(r)
    character(len=*), intent(in) :: name
    integer, intent(in), optional :: length
    class(generic_function), intent(in), target, optional :: initial

    type(named_vector_implementation), pointer :: r

    allocate(r)

    r%name = trim(name)

    if( present(length) ) then
       r%length_ = length
    else
       r%length_ = 1
    end if

    if( present(initial) ) then
       r%initial_ => initial
    end if

  end function nvi_constructor


  function vec(self)
    class(named_vector_implementation), intent(in) :: self
    real, pointer :: vec(:)
    vec => self%val
  end function vec

  function scal(self)
    class(named_vector_implementation), intent(in) :: self
    real, pointer :: scal
    scal => self%val(1)
  end function scal

  function length(self)
    class(named_vector_implementation), intent(in) :: self
    integer :: length
    length = self%length_
  end function length

  subroutine point(self, v)
    class(named_vector_implementation) :: self
    real, target, intent(in) :: v(:)

    integer :: len

    len = self%length()
    if( size(v) < len ) return
    self%val => v(1:len)

  end subroutine point


  subroutine initialize(self, ic)
    class(named_vector_implementation) :: self
    class(icicles_user) :: ic

    if( associated(self%initial_) ) then
       call self%initial_%call(ic)
    end if
  end subroutine initialize



end module class_named_vector_implementation_
