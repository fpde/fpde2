module class_derivator_g2d

  use class_g2d_methods

  use class_derivator

  use class_coordinates
  use class_coordinates_c2d

  use class_bbox
  use class_bbox_implementation

  use ghost_boundary_factory

  use class_named_vector_f
  use class_named_vector_user_

  use class_mesh2d

  private

  type, public, extends(derivator) :: derivator_g2d
     private
     class(coordinates_c2d), pointer :: c_ => null()
     class(mesh2d), pointer :: m_ => null()
     type(g2d_methods) :: g2d_m
   contains
     procedure :: coordinates => get_coordinates
     procedure :: bbox => get_bbox
     procedure :: dx
     procedure :: initialize_x
  end type derivator_g2d

  interface derivator_g2d
     module procedure g2d_new
  end interface derivator_g2d


contains

  function g2d_new(x, y, n, m) result(r)
    class(named_vector_user), intent(in), target :: x, y
    integer, intent(in) :: n(2)
    class(mesh2d), intent(in), target :: m

    type(derivator_g2d), pointer :: r

    class(coordinates_c2d), pointer :: c2d

    allocate(r)
    r%name = "derivator_g2d"

    r%c_ => coordinates_c2d(x, y, n)

    if( .not. associated(r%c_) ) then
       call r%loge("derivator_g2d(): Unable to initialize coordinates,&
            & aborting")
       deallocate(r)
       return
    end if

    r%m_ => m

  end function g2d_new


  function get_coordinates(self) result(r)
    class(derivator_g2d) :: self
    class(coordinates), pointer :: r
    r => self%c_
  end function get_coordinates


  function get_bbox(self, btypes) result(r)
    class(derivator_g2d) :: self
    character(len=*), intent(in) :: btypes(:)
    class(bbox), pointer :: r

    integer :: id, nreg, lengths(4)
    integer, pointer :: reg(:)

    nreg = 4
    r => null()

    if( size(btypes) /= nreg ) then
       call self%loge("bbox(): btypes expected to be of size 4, aborting")
       return
    end if

    do id = 1, nreg
       reg => self%c_%bregion(id)
       lengths(id) = size(reg)
    end do

    r => bbox_implementation(btypes, ghost_boundary_new, lengths)

  end function get_bbox


  subroutine dx(self, f, alpha)
    class(derivator_g2d) :: self
    class(named_vector_f) :: f
    integer, intent(in) :: alpha(:,:)

    real, pointer, dimension(:,:) :: x2d, y2d, f2d, df2d
    integer :: i

    x2d => self%c_%vec2d(self%c_%var(1))
    y2d => self%c_%vec2d(self%c_%var(2))
    f2d => self%c_%vec2d(f)

    do i = 1, size(alpha,2)
       df2d => self%c_%vec2d(f%dx(alpha(:,i)))

       ! calculate the derivatives disregarding boundary conditions
       call self%m_%diff(f2d, x2d, y2d, df2d, alpha(:,i))

       ! use the ghost point to overwrite some of the derivatives we
       ! just calculated
       call self%g2d_m%update_df(&
            f     = f2d,&
            x     = x2d,&
            y     = y2d,&
            df    = df2d,&
            m     = self%m_,&
            b     = f%bbox(),&
            alpha = alpha(:,i))

    end do

  end subroutine dx


  !> Fills the vectors x and y with particular variables based on the
  !! mesh size returned by nx()
  !!
  subroutine initialize_x(self)
    class(derivator_g2d) :: self

    integer :: i, j, n(2)
    class(named_vector_user), pointer :: x, y

    !! @todo initialize should use mesh to initialize spatial variables
    n =  self%c_%nx()
    x => self%c_%var(1)
    y => self%c_%var(2)

    x = [((   (i-1.)/(n(1)-1.), i=1, n(1) ), j=1, n(2))]
    y = [(( (n(2)-j)/(n(2)-1.), i=1, n(1) ), j=1, n(2))]
  end subroutine initialize_x

end module class_derivator_g2d
