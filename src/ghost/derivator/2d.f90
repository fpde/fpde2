module class_derivator_g2d

  use class_g2d_methods

  use class_derivator

  use class_coordinates
  use class_coordinates_c2d

  use ghost_boundary_factory

  use class_named_vector_f
  use class_named_vector

  use class_mesh2d

  private

  type, public, extends(derivator) :: derivator_g2d
     private
     class(mesh2d), pointer :: m_ => null()
     type(g2d_methods) :: g2d_m
   contains
     procedure :: dx
  end type derivator_g2d

  interface derivator_g2d
     module procedure g2d_new
  end interface derivator_g2d


contains

  function g2d_new(m) result(r)
    class(mesh2d), intent(in), target :: m

    type(derivator_g2d), pointer :: r

    allocate(r)
    r%name = "derivator_g2d"
    r%m_ => m

  end function g2d_new


  subroutine dx(self, f, alpha)
    class(derivator_g2d) :: self
    class(named_vector_f), target :: f
    integer, intent(in) :: alpha(:,:)

    real, pointer, dimension(:,:) :: x2d, y2d, f2d, df2d
    integer :: i

    class(coordinates), pointer :: c
    class(coordinates_c2d), pointer :: c2d

    c => f%coordinates()
    select type(c)
       class is(coordinates_c2d)
       c2d => c
    class default
       call self%loge("dx(): Incompatible coordinates")
       return
    end select

    x2d => c2d%vec2d(c2d%var(1))
    y2d => c2d%vec2d(c2d%var(2))
    f2d => c2d%vec2d(f)

    do i = 1, size(alpha,2)
       df2d => c2d%vec2d(f%dx(alpha(:,i)))

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
            b     = f%bdata,&
            alpha = alpha(:,i))

    end do

  end subroutine dx


  !> Fills the vectors x and y with particular variables based on the
  !! mesh size returned by nx()
  !!
  subroutine initialize_x(self)
    class(derivator_g2d) :: self

    integer :: i, j, n(2)
    class(named_vector), pointer :: x, y

    ! x = [((   (i-1.)/(n(1)-1.), i=1, n(1) ), j=1, n(2))]
    ! y = [(( (n(2)-j)/(n(2)-1.), i=1, n(1) ), j=1, n(2))]
  end subroutine initialize_x

end module class_derivator_g2d
