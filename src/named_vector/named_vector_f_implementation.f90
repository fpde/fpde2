module class_named_vector_f_implementation_ghost

  use class_named_vector_
  use class_named_vector_user_
  use class_named_vector_implementation_
  use class_named_vector_f

  use class_ghost_boundary_box_user

  use class_generic_function

  use class_icicles_user_

  private


  type :: d_ptr
     integer, allocatable :: alpha(:)
     class(named_vector), pointer :: val => null()
  end type d_ptr


  type, public, extends(named_vector_f) :: named_vector_f_implementation
     private
     class(ghost_boundary_box_user), pointer :: box => null()
     type(d_ptr), pointer :: dx_(:) => null()
     class(named_vector), pointer :: dt_ => null()
   contains
     procedure :: dx
     procedure :: dt => der_t
     procedure :: boundary_param
     procedure :: num_boundary_param
     procedure :: update_boundary_param
  end type named_vector_f_implementation


  interface named_vector_f_implementation
     module procedure :: nvf_constructor
  end interface named_vector_f_implementation


contains

  function nvf_constructor(name, shape, initial, dx, boundary, dt) result(r)
    character(len=*), intent(in)                :: name
    integer, intent(in)                         :: shape(:)
    class(generic_function), intent(in), target :: initial
    integer, intent(in), optional               :: dx(:,:)
    logical, intent(in), optional               :: dt
    class(ghost_boundary_box_user), target, optional       :: boundary

    type(named_vector_f_implementation), pointer :: r

    integer :: i
    ! real, pointer :: mem(:)

    allocate(r)

    r%named_vector_implementation&
         = named_vector_implementation(&
         name = name, &
         shape = shape,&
         initial = initial )

    if( present(boundary) ) then
       r%box => boundary
    end if

    if( present(dx) ) then
       allocate(r%dx_(size(dx,2)))
       associate(d => r%dx_)
         do i = 1, size(dx,2)
            d(i)%val => named_vector_implementation(&
                 name    = "d",&
                 shape   = shape)
            d(i)%alpha = dx(:,i)

            ! allocate(mem(d(i)%val%length()))
            ! call d(i)%val%point(mem)
         end do
       end associate
    end if

    if( present(dt) ) then
       r%dt_ => named_vector_implementation(&
            name = "dt",&
            shape = shape)
       ! allocate(mem(r%dt_%length()))
       ! call r%dt_%point(mem)
    end if

  end function nvf_constructor


  function dx(self, alpha)
    use class_named_vector_user_

    class(named_vector_f_implementation) :: self
    integer, intent(in) :: alpha(:)

    class(named_vector_user), pointer :: dx

    integer :: i

    dx => null()

    associate( d => self%dx_ )
      do i = 1, size(d)
         if( all(d(i)%alpha == alpha) ) then
            dx => d(i)%val
            return
         end if
      end do
    end associate

  end function dx


  function der_t(self)
    class(named_vector_f_implementation) :: self

    class(named_vector_user), pointer :: der_t

    der_t => self%dt_
  end function der_t


  function boundary(self)
    class(named_vector_f_implementation), target :: self

    class(ghost_boundary_box_user), pointer :: boundary

    boundary => self%box

  end function boundary


  function alpha(self)
    class(named_vector_f_implementation) :: self
    integer, allocatable :: alpha(:,:)

    associate( d => self%dx_)

      ! its just an elaborate way of writing
      ! alpha = d%alpha
      alpha = reshape(&
           [ (d(i)%alpha, i=1,size(d)) ],&
           [size(self%shape()),size(d)])

    end associate

  end function alpha


  function boundary_param(self, var, side, param)
    class(named_vector_f_implementation) :: self
    integer, intent(in) :: var, side, param

    class(named_vector_user), pointer :: boundary_param

    boundary_param => null()
  end function boundary_param


  function num_boundary_param(self, var, side)
    class(named_vector_f_implementation) :: self
    integer, intent(in) :: var, side

    integer :: num_boundary_param

    num_boundary_param = 0
  end function num_boundary_param


  subroutine update_boundary_param(self, ic)
    class(named_vector_f_implementation) :: self
    class(icicles_user), target :: ic
  end subroutine update_boundary_param


end module class_named_vector_f_implementation_ghost
