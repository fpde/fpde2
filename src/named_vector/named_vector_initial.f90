module class_named_vector_initial

  use class_named_vector_
  use class_icicles_user_
  use class_named_vector_implementation_
  use class_generic_function

  private

  type, public, extends(named_vector_implementation) :: named_vector_initial
     private
     class(generic_function), pointer :: fun => null()
   contains
     procedure :: initialize
  end type named_vector_initial

  interface named_vector_initial
     module procedure :: nvi_constructor
  end interface named_vector_initial


contains

  function nvi_constructor(name, shape, initial) result(r)
    character(len=*), intent(in) :: name
    integer, intent(in) :: shape(:)
    class(generic_function), intent(in), target :: initial
    type(named_vector_initial) :: r

    class(named_vector_implementation), allocatable :: ri

    r%named_vector_implementation &
         = named_vector_implementation( &
         name  = name, &
         shape = shape )

    r%fun => initial

  end function nvi_constructor


  subroutine initialize(self, ic)
    class(named_vector_initial) :: self
    class(icicles_user) :: ic
    if( associated(self%fun) ) then
       call self%fun%call(ic)
    end if
  end subroutine initialize

end module class_named_vector_initial
