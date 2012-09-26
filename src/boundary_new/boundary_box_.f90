module class_boundary_box_

  use class_platonic
  use class_boundary
  use boundary_factory


  private


  type :: b_ptr
     integer :: alpha(2) = [0,0]
     class(boundary), pointer :: val
  end type b_ptr


  type, public, extends(platonic) :: boundary_box
     private
     type(b_ptr), allocatable :: entries(:)
     class(boundary), pointer :: default => null()
   contains

     procedure, private :: add_single
     procedure, private :: add_default
     generic :: add => add_single, add_default

     procedure :: get

  end type boundary_box


contains


  function get(self, x, side)
    class(boundary_box) :: self
    integer, intent(in) :: x, side

    class(boundary), pointer :: get

    integer :: i

    get => self%default

    associate( e => self%entries )
      do i = 1, size(e)
         if( all(e(i)%alpha == [x,side]) ) then
            get => e(i)%val
            return
         end if
      end do
    end associate

  end function get


  subroutine add_default(self, b)
    class(boundary_box) :: self
    class(boundary), intent(in), target :: b

    self%default => b
  end subroutine add_default


  subroutine add_single(self, x, side, b)
    class(boundary_box) :: self
    integer, intent(in) :: x, side
    class(boundary), intent(in), target :: b

    type(b_ptr), allocatable :: temp(:)

    temp = [self%entries, b_ptr(alpha = [x,side],val = b)]
    self%entries = temp
  end subroutine add_single


end module class_boundary_box_
