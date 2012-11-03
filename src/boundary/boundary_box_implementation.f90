module class_bbox_implementation

  use class_bbox
  use class_boundary
  use class_named_vector_user_

  private

  type :: bptr
     class(boundary), pointer :: val => null()
  end type bptr


  type, public, extends(bbox) :: bbox_implementation
     private
     type(bptr), allocatable :: boundary_(:)
   contains
     procedure :: boundary => get_boundary
     procedure :: param
     procedure :: num_param
   contains

  end type bbox_implementation


  abstract interface
     function bfac_i(id, length, error)
       import boundary
       character(len=*), intent(in) :: id
       integer, intent(in) :: length
       integer, intent(out), optional :: error
       class(boundary), pointer :: bfac_i
     end function bfac_i
  end interface


  interface bbox_implementation
     module procedure :: bboxi_new
  end interface bbox_implementation


contains

  function bboxi_new(btypes, factory, lengths) result(r)
    character(len=*), intent(in) :: btypes(:)
    integer, intent(in) :: lengths(:)
    procedure(bfac_i) :: factory

    class(bbox_implementation), pointer :: r

    integer :: i

    if( size(btypes) /= size(lengths) ) return

    allocate(r)

    allocate(r%boundary_(size(btypes)))

    do i = 1, size(btypes)
       r%boundary_(i)%val => factory(btypes(i), lengths(i))
    end do

  end function bboxi_new


  function get_boundary(self, id)
    class(bbox_implementation), target :: self
    integer, intent(in) :: id

    class(boundary), pointer :: get_boundary

    get_boundary => null()

    if(.not. allocated(self%boundary_)) return
    if( id < 1 .or. id > size(self%boundary_)) return

    get_boundary => self%boundary_(id)%val

  end function get_boundary


  function param(self, id, n)
    class(bbox_implementation), intent(in) :: self
    integer, intent(in) :: id, n

    class(named_vector_user), pointer :: param

    class(boundary), pointer :: b

    param => null()
    b => self%boundary(id)
    if( .not. associated(b) ) return
    param => b%p(n)
  end function param


  function num_param(self, id)
    class(bbox_implementation), intent(in) :: self
    integer, intent(in) :: id

    integer :: num_param

    class(boundary), pointer :: b

    num_param = 0

    b =>  self%boundary(id)
    if( .not. associated(b) ) return

    num_param = b%np()

  end function num_param

end module class_bbox_implementation
