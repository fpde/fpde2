module class_derivator_g1d

  use constants_module

  use class_derivator

  use ghost_boundary_selector

  use class_coordinates
  use class_coordinates_c1d

  use class_named_vector
  use class_named_vector_f

  use class_mesh1d
  use mesh1d_factory

  use class_derivator_g1d_methods

  private

  type, public, extends(derivator) :: derivator_g1d
     private
     class(mesh1d), pointer :: m_ => null()
     type(g1d_methods) :: g1d_m
   contains
     procedure :: dx
  end type derivator_g1d


  interface derivator_g1d
     module procedure :: g1d_new
  end interface derivator_g1d

  public :: g1d_new


contains

  function g1d_new(mesh) result(r)
    character(len=*), intent(in) :: mesh

    type(derivator_g1d), pointer :: r

    integer :: err

    allocate(r)

    r%name = "derivator_g1d"
    r%m_ => mesh1d_new(mesh, err)

    if( err /= FPDE_STATUS_OK ) then
       call r%loge("derivator_g1d(): Unable to create mesh, wrong id?")
       !! @bug due to an ifort 13.0.1 bug the result "r" is recognized
       !! as "associated" despite explicit deallocation
       !! below. gfortran interprets the result in a proper way.
       deallocate(r)
       return
    end if

    call r%m_%init()

  end function g1d_new


  ! actual algorithms goes in here
  subroutine dx(self, f, alpha)
    use class_boundary_ghost
    class(derivator_g1d) :: self
    class(named_vector_f), target :: f
    integer, intent(in) :: alpha(:,:)

    class(named_vector), pointer :: xu
    real, pointer :: dfv(:,:), fv(:), xv(:), dfdxv(:)
    integer :: i, length, left, right

    class(coordinates), pointer :: c
    class(coordinates_c1d), pointer :: c1d
    class(boundary_ghost), pointer :: bl, br

    ! define the ids of the left and right boundary
    left = 1; right = 2;

    ! abort if the rank of the derivative is higher than one
    if( size(alpha,1) > 1 ) then
       return
    end if

    c => f%coordinates()
    select type(c)
       class is(coordinates_c1d)
          c1d => c
       class default
          call self%loge("dx(): Incompatible coordinates")
       return
    end select

    xu => c1d%var(1)
    xv => xu%vec()
    fv => f%vec()

    ! get the pointers to appropriate algorithms for dealing with
    ! boundary conditions
    bl => ghost_boundary_select(f%bdata(left )%btype)
    br => ghost_boundary_select(f%bdata(right)%btype)

    ! extract length
    length = f%length()

    ! compute all the derivatives
    !! @todo remove the possibility of calculating several derivatives
    !! at once?
    do i = 1, size(alpha,2)
       dfdxv => f%dx(alpha(:,i))

       call self%m_%diff(fv, xv, dfdxv, alpha(1,i))

       call self%g1d_m%dx(&
            fv,&
            xv,&
            dfdxv,&
            alpha(1,i),&
            self%m_,&
            bl,&
            f%bdata(left)%params)

       call self%g1d_m%dx(&
            fv(length:1:-1),&
            xv(length:1:-1),&
            dfdxv(length:1:-1),&
            alpha(1,i),&
            self%m_,&
            br,&
            f%bdata(right)%params)
    end do

  end subroutine dx

end module class_derivator_g1d
