module class_named_vector_implementation_

  use class_named_vector_

  private

  type, public, extends(named_vector) :: named_vector_implementation
     private
     real, pointer :: val(:) => null()
     integer, allocatable :: shape_(:)
   contains
     procedure :: vec
     procedure :: scal
     procedure :: length
     procedure :: shape
     procedure :: point
  end type named_vector_implementation

  interface named_vector_implementation
     module procedure :: nvi_constructor
  end interface named_vector_implementation


contains

  function nvi_constructor(name, shape) result(r)
    character(len=*), intent(in) :: name
    integer, intent(in) :: shape(:)

    type(named_vector_implementation), pointer :: r

    allocate(r)

    r%name = trim(name)
    r%shape_ = shape
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

  function shape(self)
    class(named_vector_implementation), intent(in) :: self
    integer, allocatable :: shape(:)
    shape = self%shape_
  end function shape

  function length(self)
    class(named_vector_implementation), intent(in) :: self
    integer :: length
    length = product(self%shape_)
  end function length

  subroutine point(self, v)
    class(named_vector_implementation) :: self
    real, target, intent(in) :: v(:)

    integer :: len

    len = self%length()
    if( size(v) < len ) return
    self%val => v(1:len)

  end subroutine point

end module class_named_vector_implementation_
