module class_boundary

  use class_platonic
  use class_named_vector_user_
  use class_named_vector_implementation_

  private

  type :: p_ptr
     class(named_vector_user), pointer :: val => null()
  end type p_ptr

  type, abstract, public, extends(platonic) :: boundary
     type(p_ptr), allocatable :: p_(:)
   contains
     procedure :: p
     procedure :: np
     procedure(p_names_i), deferred :: p_names
     procedure :: allocate_params
  end type boundary

  abstract interface
     function p_names_i(self)
       import boundary
       class(boundary) :: self
       character(len=:), allocatable :: p_names_i(:)
     end function p_names_i
  end interface

contains

  subroutine allocate_params(self, length)
    use class_generic_function_dummy
    class(boundary) :: self
    integer :: length

    character(len=:), allocatable :: names(:)
    integer :: i
    type(named_vector_implementation), pointer :: nv
    real, pointer :: v(:)

    allocate(self%p_(size(names)))

    do i = 1, size(names)
       !> @bug nvi_constructor is used explicitely due to the bug in
       !! ifort, this is only a temporery workaround
       ! nv => named_vector_implementation(&
       !      name = names(i),&
       !      length = length)
       nv => nvi_constructor(&
            name = names(i),&
            length = length)
       self%p_(i)%val => nv

       ! allocate the memory for the parameters
       allocate(v(length))
       call nv%point(v)
    end do

  end subroutine allocate_params


  function p(self, id)
    class(boundary) :: self
    integer, intent(in) :: id

    class(named_vector_user), pointer :: p
    p => null()

    if( .not. allocated(self%p_) ) return
    if( id > size(self%p_) .or. id < 1 ) return

    p => self%p_(id)%val
  end function p


  function np(self)
    class(boundary) :: self
    integer :: np

    if( .not.  allocated(self%p_)) then
       np = 0
    else
       np = size(self%p_)
    end if

  end function np

end module class_boundary
