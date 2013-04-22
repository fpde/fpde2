module class_coordinates_c1d

  use class_regions
  use class_coordinates_c
  use class_named_vector
  use class_named_vector_user

  private

  type, public, extends(coordinates_c) :: coordinates_c1d
     private
     integer :: nx_ = 0
     type(regions) :: bregion_
     class(named_vector_user), pointer :: x_ => null()
   contains
     procedure :: nx
     procedure :: bregion
     procedure :: var
     procedure :: dim
     procedure :: length
  end type coordinates_c1d

  interface coordinates_c1d
     module procedure :: c1d_new
  end interface coordinates_c1d

contains

  function c1d_new(x) result(r)
    class(named_vector_user), target :: x
    type(coordinates_c1d), pointer :: r

    allocate(r)

    r%nx_ =  x%length()
    r%x_  => x

    call r%bregion_%new(id = 1, idx = [1])
    call r%bregion_%new(id = 2, idx = [r%nx_])

  end function c1d_new


  function nx(self)
    class(coordinates_c1d) :: self
    integer :: nx
    nx = self%nx_
  end function nx

  function dim(self)
    class(coordinates_c1d) :: self
    integer :: dim
    dim = 1
  end function dim

  function length(self)
    class(coordinates_c1d) :: self
    integer :: length
    length = self%nx()
  end function length

  function var(self, n)
    class(coordinates_c1d) :: self
    integer, intent(in) :: n
    class(named_vector_user), pointer :: var

    select case(n)
    case(1)
       var => self%x_
    case default
       var => null()
    end select

  end function var

  function bregion(self, id)
    class(coordinates_c1d) :: self
    integer, intent(in) :: id

    integer, pointer :: bregion(:)

    bregion => self%bregion_%region(id)
  end function bregion

end module class_coordinates_c1d
