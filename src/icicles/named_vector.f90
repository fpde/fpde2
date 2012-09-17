module class_named_vector

  private

  type, public :: named_vector
     real, pointer :: val(:) => null()
     character(len=:), allocatable :: name
  end type named_vector

end module class_named_vector
