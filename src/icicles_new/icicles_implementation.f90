module class_icicles_implementation

  use class_icicles_
  use class_icicles_user_
  use class_named_vector_
  use class_named_vector_user_
  use class_generic_function

  private

  type :: nv_pointer
     class(named_vector), pointer :: val
  end type nv_pointer

  type, public, extends(icicles) :: icicles_implementation
     private
     class(generic_function), pointer :: init_
     type(nv_pointer), allocatable :: vectors(:)
   contains
     procedure :: add
     procedure :: point
     procedure :: length
     procedure :: get
     procedure :: initialize
  end type icicles_implementation


  interface icicles_implementation
     module procedure :: ici_new
  end interface icicles_implementation

contains

  function ici_new(init) result(r)
    class(generic_function), target :: init
    class(icicles_implementation), pointer :: r

    allocate(r)
    r%init_ => init
  end function ici_new


  function get(self, name)
    class(icicles_implementation), intent(in) :: self
    character(len=*), intent(in) :: name
    class(named_vector_user), pointer :: get

    integer :: i

    get => null()
    associate(sv => self%vectors)
      do i = 1, size(sv)
         if( name == sv(i)%val%name ) then
            get => sv(i)%val
            return
         end if
      end do
    end associate

  end function get


  subroutine add(self, nv)
    class(icicles_implementation) :: self
    class(named_vector_user), target, intent(in) :: nv

    type(nv_pointer), allocatable :: nv_temp(:)

    class(named_vector), pointer :: nv_
    type(nv_pointer) :: nvp

    if( .not. allocated(self%vectors) ) allocate(self%vectors(0))

    select type(nv)
    class is(named_vector)
       nv_ => nv
    class default
       return
    end select

    nv_temp = [ self%vectors, nv_pointer(val = nv_) ]
    self%vectors = nv_temp

  end subroutine add


  subroutine point(self, v, cr)
    class(icicles_implementation), intent(in) :: self
    real, target, intent(in) :: v(:)
    class(crit), optional :: cr

    integer :: pos, len, i

    pos = 1
    associate( sv => self%vectors )
      do i = 1, size(sv)

         if( present(cr) ) then
            if( .not. cr%test(sv(i)%val) ) then
               cycle
            end if
         end if

         len = sv(i)%val%length()
         if( size(v) < pos + len - 1 ) return

         call sv(i)%val%point( v(pos : pos + len - 1) )
         pos = pos + len

      end do
    end associate

  end subroutine point


  function length(self, cr)
    class(icicles_implementation), intent(in) :: self
    class(crit), optional :: cr
    integer :: length

    integer :: i

    length = 0
    associate( sv => self%vectors )
      do i = 1, size(sv)

         if( present(cr) ) then
            if( .not. cr%test(sv(i)%val) ) then
               cycle
            end if
         end if

         length = length + sv(i)%val%length()

      end do
    end associate

  end function length


  subroutine initialize(self)
    class(icicles_implementation), intent(in) :: self

    call self%init_%call(self)
  end subroutine initialize


end module class_icicles_implementation
