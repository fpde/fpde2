module class_icicles_implementation

  use class_icicles
  use class_icicles_user
  use class_coordinates
  use class_named_vector
  use class_named_vector_f
  use class_generic_function
  use class_derivator

  private

  type :: nv_pointer
     class(named_vector), pointer :: val
  end type nv_pointer


  type, public, extends(icicles) :: icicles_implementation
     private
     class(generic_function), pointer :: init_ => null()
     type(nv_pointer), allocatable :: vectors(:)
     class(coordinates), pointer :: coords => null()
     class(derivator), pointer :: der => null()
   contains
     procedure :: add => add_nv
     procedure :: d
     procedure :: point
     procedure :: length
     procedure :: get
     procedure :: coordinates => get_coordinates
     procedure :: initialize
  end type icicles_implementation


  interface icicles_implementation
     module procedure :: ici_new
  end interface icicles_implementation

contains

  function ici_new(init, der, coords) result(r)
    class(generic_function), target :: init
    class(icicles_implementation), pointer :: r
    class(derivator), target :: der
    class(coordinates), target :: coords

    allocate(r)

    r%init_  => init
    r%der    => der
    r%coords => coords
    allocate(r%vectors(0))

  end function ici_new


  function get(self, name)
    class(icicles_implementation), intent(in) :: self
    character(len=*), intent(in) :: name
    class(named_vector), pointer :: get

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


  subroutine add_nv(self, nv)
    class(icicles_implementation) :: self
    class(named_vector), target, intent(in) :: nv

    type(nv_pointer), allocatable :: nv_temp(:)

    nv_temp = [ self%vectors, nv_pointer(val = nv) ]
    self%vectors = nv_temp

  end subroutine add_nv


  function get_coordinates(self) result(r)
    class(icicles_implementation) :: self

    class(coordinates), pointer :: r

    r => self%coords

  end function get_coordinates


  function d(self, fname, alpha, vars)
    class(icicles_implementation), intent(in) :: self
    character(len=*), intent(in) :: fname, vars(:)
    integer, intent(in), target :: alpha(:)
    real, pointer :: d(:)

    class(named_vector), pointer :: nv
    type(vecptr), allocatable, save :: xptr(:)

    d => null()

    nv => self%get(fname)

    xptr = [(vecptr(val=self%getvec(vars(i))),i=1,size(vars))]

    select type(nv)
    class is(named_vector_f)
       call self%der%dx(nv, xptr, alpha, self%coords)
       d => nv%dx(alpha)
    class default
       call self%loge("d(): no function "//fname//" found.")
    end select

  end function d


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


  !> @todo why doesn't it do allocate(self%vectors(0))?
  subroutine initialize(self)
    class(icicles_implementation), intent(in) :: self

    call self%init_%call(self)
  end subroutine initialize


end module class_icicles_implementation
