module class_bbox_implementation

  use class_bbox

  use class_boundary
  use class_named_vector_
  use class_named_vector_implementation_
  use class_named_vector_user_

  use class_generic_function

  use names_module

  private

  ! @todo move to named_vector_user and rename to
  ! named_vector_user_ptr?
  type :: p_ptr
     class(named_vector), pointer :: val => null()
  end type p_ptr


  type :: b_ptr
     class(boundary), pointer :: val => null()
     type(p_ptr), allocatable :: params(:)
  end type b_ptr


  type, public, extends(bbox) :: bbox_implementation
     private
     type(b_ptr), allocatable :: entries(:,:)
   contains
     procedure :: boundary => get_boundary
     procedure :: param => get_parameter
     procedure :: num_param => get_num_parameter
  end type bbox_implementation


  interface bbox_implementation
     module procedure :: gbbi_constructor
  end interface bbox_implementation


  abstract interface
     function bfac_i(id, error)
       import boundary
       character(len=*), intent(in) :: id
       integer, intent(out), optional :: error
       class(boundary), pointer :: bfac_i
     end function bfac_i
  end interface


contains


  function gbbi_constructor(shape, update, default, btypes, factory) result(r)
    integer, intent(in) :: shape(:)
    class(generic_function), intent(in), target :: update
    character(len=*), intent(in) :: default
    type(btype), intent(in), optional :: btypes(:)
    procedure(bfac_i), intent(in), pointer :: factory

    type(bbox_implementation), pointer :: r

    integer :: dim, i, x, side, np, k, length
    character(len=:), allocatable :: pname

    allocate(r)

    dim = size(shape)

    allocate(r%entries(2,dim))

    if( present(btypes) ) then
       do i = 1, size(btypes)
          r%entries(btypes(i)%side,btypes(i)%x)%val &
               => factory(btypes(i)%type)
       end do
    end if

    do x = 1, dim
       length = product([shape(1:x-1),1,shape(x+1:)])
       do side = 1, 2
          associate(e => r%entries(side,x))
            if( .not. associated(e%val) ) e%val => factory(default)
            np = 0 ! @todo np = b%params()
            allocate(e%params(np))
            do k = 1, np
               pname = "p" ! @todo pname = "b%param_name(k)"
               pname = boundary_param_name(x,side,k,pname)
               e%params(k)%val => named_vector_implementation(&
                    length = length,&
                    name = pname)
            end do
          end associate
       end do
    end do

  end function gbbi_constructor


  function get_boundary(self, x, side)
    class(bbox_implementation) :: self
    integer, intent(in) :: x, side

    class(boundary), pointer :: get_boundary

    get_boundary => self%entries(side,x)%val

  end function get_boundary


  function get_parameter(self, x, side, num)
    class(bbox_implementation), intent(in), target :: self
    integer, intent(in) :: x, side, num

    class(named_vector_user), pointer :: get_parameter

    get_parameter => self%entries(side, x)%params(num)%val

  end function get_parameter


  function get_num_parameter(self, x, side) result(r)
    class(bbox_implementation), intent(in), target :: self
    integer, intent(in) :: x, side

    integer :: r

    r = size(self%entries(side,x)%params)

  end function get_num_parameter


end module class_bbox_implementation
