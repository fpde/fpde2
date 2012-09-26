module class_named_vector_f

  use class_named_vector_user_
  use class_named_vector_initial
  use class_named_vector_implementation_
  use class_boundary_box_
  use class_generic_function

  private


  type :: d_ptr
     integer, allocatable :: alpha(:)
     class(named_vector_user), pointer :: val => null()
  end type d_ptr


  type, public, extends(named_vector_initial) :: named_vector_f
     private
     type(boundary_box), pointer :: box => null()
     type(d_ptr), pointer :: derivatives(:) => null()
   contains
     procedure :: boundary
     procedure :: derivative
  end type named_vector_f


  interface named_vector_f
     module procedure :: nvf_constructor
  end interface named_vector_f


contains

  function nvf_constructor(name, shape, initial, derivatives, boundary) result(r)
    character(len=*), intent(in) :: name
    integer, intent(in) :: shape(:)
    class(generic_function), intent(in), target :: initial
    integer, intent(in), optional :: derivatives(:,:)
    class(boundary_box), target, optional :: boundary

    type(named_vector_f), pointer :: r

    integer :: i

    allocate(r)

    r%named_vector_initial&
         = named_vector_initial(&
         name = name, &
         shape = shape,&
         initial = initial )

    if( present(boundary) ) then
       r%box => boundary
    end if

    if( present(derivatives) ) then

       allocate(r%derivatives(size(derivatives,2)))

       associate(d => r%derivatives)
         do i = 1, size(derivatives,2)

            d(i)%val => named_vector_implementation("d",shape)
            d(i)%alpha = derivatives(:,i)

         end do
       end associate

    end if


  end function nvf_constructor


  function derivative(self, alpha)
    class(named_vector_f) :: self
    integer, intent(in) :: alpha(:)

    class(named_vector_user), pointer :: derivative

    integer :: i

    derivative => null()

    associate( d => self%derivatives )
      do i = 1, size(d)
         if( all(d(i)%alpha == alpha) ) then
            derivative => d(i)%val
            return
         end if
      end do
    end associate

  end function derivative


  function boundary(self, var, dir, param)
    class(named_vector_f) :: self
    integer, intent(in) :: var, dir, param

    class(named_vector_user), pointer :: boundary

    boundary => null()

  end function boundary


end module class_named_vector_f
