module class_derivator_g1d

  use constants_module

  use class_derivator

  use class_bbox
  use class_bbox_implementation

  use ghost_boundary_factory

  use class_boundary

  use class_coordinates
  use class_coordinates_c1d

  use class_named_vector_user
  use class_named_vector_f

  use class_mesh1d
  use mesh1d_factory

  use class_derivator_g1d_methods

  private

  type, public, extends(derivator) :: derivator_g1d
     private
     class(coordinates_c1d), pointer :: c_ => null()
     class(mesh1d), pointer :: m_ => null()
     type(g1d_methods) :: g1d_m
   contains
     procedure :: coordinates => get_c
     procedure :: dx
     procedure :: bbox => get_bbox
     procedure :: initialize_x
  end type derivator_g1d


  interface derivator_g1d
     module procedure :: g1d_new
  end interface derivator_g1d

  public :: g1d_new


contains

  function g1d_new(var, mesh) result(r)
    class(named_vector_user), intent(in), target :: var
    character(len=*), intent(in) :: mesh

    type(derivator_g1d), pointer :: r

    integer :: nx, err

    nx = var%length()

    allocate(r)

    r%name = "derivator_g1d"
    r%c_ => coordinates_c1d(var)
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


  function get_c(self) result(r)
    class(derivator_g1d) :: self
    class(coordinates), pointer :: r
    r => self%c_
  end function get_c


  function get_bbox(self, btypes) result(r)
    class(derivator_g1d) :: self
    character(len=*), intent(in) :: btypes(:)

    class(bbox), pointer :: r

    integer :: lengths(2)

    r => null()

    if( size(btypes) /= 2 ) return

    lengths = [1,1]

    r => bbox_implementation(btypes, ghost_boundary_new, lengths)

  end function get_bbox


  ! actual algorithms goes in here
  subroutine dx(self, f, alpha)
    use class_bbox_user
    use class_boundary_ghost
    class(derivator_g1d) :: self
    class(named_vector_f) :: f
    integer, intent(in) :: alpha(:,:)

    class(named_vector_user), pointer :: xu, dxu
    real, pointer :: dfv(:,:), fv(:), xv(:), dfdxv(:)
    real, pointer :: plv(:,:), prv(:,:)
    integer :: i, len
    class(boundary), pointer :: bl, br
    class(bbox_user), pointer :: bb

    if( size(alpha,1) > 1 ) then
       return
    end if

    xu => self%c_%var(1)
    xv => xu%vec()
    fv => f%vec()

    ! extract boundaries
    bb => f%bbox()
    bl => bb%boundary(1)
    br => bb%boundary(2)

    ! extract boundary parameters
    plv => bl%rawp()
    prv => br%rawp()

    ! extract length
    len = f%length()

    do i = 1, size(alpha,2)
       ! this is equivalent to:
       ! dfv => f%dx(alpha(:,i))%vec()
       dxu => f%dx(alpha(:,i))
       dfdxv => dxu%vec()

       call self%m_%diff(fv, xv, dfdxv, alpha(1,i))

       select type(bl)
       class is(boundary_ghost)
          call self%g1d_m%dx(&
               fv,&
               xv,&
               dfdxv,&
               alpha(1,i),&
               self%m_,&
               bl,&
               plv)
       end select

       select type(br)
       class is(boundary_ghost)
          call self%g1d_m%dx(&
               fv(len:1:-1),&
               xv(len:1:-1),&
               dfdxv(len:1:-1),&
               alpha(1,i),&
               self%m_,&
               br,&
               prv)
       end select
    end do

  end subroutine dx


  subroutine initialize_x(self)
    class(derivator_g1d) :: self

    integer :: i, nx
    class(named_vector_user), pointer :: x

    nx =  self%c_%nx()
    x  => self%c_%var(1)

    !! @todo initialize should use mesh to initialize spatial variables

    x = [((i-1.)/(nx-1.),i=1, nx)]
  end subroutine initialize_x


end module class_derivator_g1d
