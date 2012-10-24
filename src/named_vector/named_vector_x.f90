module class_named_vector_x

  use class_named_vector_implementation_
  use class_generic_function

  private

  type, public, extends(named_vector_implementation) :: named_vector_x
     private
  end type named_vector_x

  interface named_vector_x
     module procedure :: nvx_constructor
  end interface named_vector_x


contains

  function nvx_constructor(name, length, initial) result(r)
    character(len=*), intent(in) :: name
    integer, intent(in) :: length
    class(generic_function), intent(in), target :: initial

    type(named_vector_x), pointer :: r

    allocate(r)

    r%named_vector_implementation&
         = named_vector_implementation( &
         name = name,&
         initial = initial,&
         length = length)

  end function nvx_constructor

end module class_named_vector_x
