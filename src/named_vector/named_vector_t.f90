module class_named_vector_t

  use class_named_vector

  private

  type, public, extends(named_vector) :: named_vector_t
     private
     real :: tmax_ = 1.0
   contains
     procedure :: tmax
  end type named_vector_t


  interface named_vector_t
     module procedure :: nvt_constructor
  end interface named_vector_t


contains

  function nvt_constructor(name, tmax) result(r)
    character(len=*), intent(in) :: name
    real, intent(in) :: tmax

    type(named_vector_t), pointer :: r

    allocate(r)

    r%named_vector = named_vector(&
         & name = name,&
         & length = 1)

    r%tmax_ = tmax
  end function nvt_constructor


  function tmax(self)
    class(named_vector_t) :: self
    real :: tmax
    tmax = self%tmax_
  end function tmax

end module class_named_vector_t
