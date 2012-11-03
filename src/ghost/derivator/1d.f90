module class_derivator_g1d

  use class_derivator

  use class_bbox
  use class_bbox_implementation

  use ghost_boundary_factory

  use class_boundary

  use class_coordinates
  use class_coordinates_c1d

  use class_named_vector_user_
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
  end type derivator_g1d


  interface derivator_g1d
     module procedure :: g1d_new
  end interface derivator_g1d


contains

  function g1d_new(var, mesh) result(r)
    class(named_vector_user), intent(in), target :: var
    character(len=*), intent(in) :: mesh

    type(derivator_g1d), pointer :: r

    integer :: nx

    nx = var%length()

    allocate(r)

    r%c_ => coordinates_c1d(var)
    r%m_ => mesh1d_new(mesh)
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

    integer, allocatable :: lengths(:)
    integer :: nregs

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
    real, pointer :: dfv(:,:), fv(:), xv(:), dfdxv(:), dfdxv_(:,:)
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

       dfdxv_(1:len,1:1) => dfdxv
       call self%m_%diff(fv, xv, dfdxv_, alpha(:,i))

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


end module class_derivator_g1d
