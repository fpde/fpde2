module class_ghost_boundary_box_implementation

  use class_ghost_boundary_box

  use class_boundary
  use class_named_vector_
  use class_named_vector_user_
  use boundary_factory

!   private


!   type :: p_ptr
!      integer, allocatable :: alpha(:)
!      class(named_vector), pointer :: val => null()
!   end type p_ptr


!   type :: b_ptr
!      integer :: alpha(2) = [0,0]
!      class(boundary), pointer :: val
!      type(p_ptr), allocatable :: params(:)
!   end type b_ptr


!   type, public, extends(ghost_boundary_box) :: ghost_boundary_box_implementation
!      private
!      type(b_ptr), allocatable :: entries(:)
!      class(boundary), pointer :: default => null()
!    contains
!      procedure :: get_boundary
!      procedure :: add
!      procedure :: add_default
!   end type ghost_boundary_box_implementation

!   interface ghost_boundary_box_implementation
!      module procedure :: gbbi_constructor
!   end interface ghost_boundary_box_implementation


! contains


!   function get_boundary(self, x, side)
!     class(boundary_box_implementation) :: self
!     integer, intent(in) :: x, side

!     class(boundary), pointer :: get

!     integer :: i

!     get => self%default

!     associate( e => self%entries )
!       do i = 1, size(e)
!          if( all(e(i)%alpha == [x,side]) ) then
!             get => e(i)%val
!             return
!          end if
!       end do
!     end associate

!   end function get


!   function get_parameter(self, x, side, param)
!     class(boundary_box_implementation), intent(in), target :: self
!     integer, intent(in) :: x, side, param

!     class(named_vector_user), pointer :: get_parameter

!     integer :: i

!     get_parameter => null()

!     associate( e => self%entries )
!       do i = 1, size(e)
!          if( all(e(i)%alpha == [x,side]) ) then
!             if( param > size(e(i)%params) ) return
!             get_parameter => e(i)%params(param)%val
!          end if
!       end do
!     end associate

!   end function get_parameter


!   subroutine add_default(self, b)
!     class(boundary_box_implementation) :: self
!     class(boundary), intent(in), target :: b

!     self%default => b
!   end subroutine add_default


!   subroutine add_single(self, x, side, b)
!     class(boundary_box_implementation) :: self
!     integer, intent(in) :: x, side
!     class(boundary), intent(in), target :: b

!     type(b_ptr), allocatable :: temp(:)

!     temp = [self%entries, b_ptr(alpha = [x,side],val = b)]
!     self%entries = temp
!   end subroutine add_single



end module class_ghost_boundary_box_implementation
