module init
  use class_named_vector_user
  use class_icicles_user
  use class_named_vector_f

contains

  subroutine initialization_f(ic)
    class(icicles_user) :: ic

    class(named_vector_f), pointer :: f
    class(named_vector_user), pointer :: x, y
    real, pointer :: g(:)

    integer :: n(2), alpha(2)

    ! alpha is an array of multiindices defined in such way that
    ! alpha(:,i) is the i-th multiindex. Each single multiindex is of
    ! the form [n,m] and describes the derivative $f^(n,m)(x,y)$
    !
    ! at this point we are interested in calculating the f^(1,0)
    alpha = [1,0]

    ! extract f from icicles and convert it to named_vector_f
    f => nvtof(ic%get("f"))
    ! extract g from icicles
    g => ic%getvec("g")
    ! extract both variables from the function (ech function can be
    ! defined on its own set of coordinates/variables)
    x => f%var(1)
    y => f%var(2)

    ! assign a value to f using the assignment operation overloaded
    ! for all named_vectors
    f = x%vec()

    ! calculate the derivatives for f. It is, in general, possible to
    ! calculate derivatives described by many multiindexes, hence
    ! alpha is of dimension(:,:)
    call f%dx_update(reshape(alpha, [2,1]), ic)

    ! assign the result to g. Unfortunately fortran does not allow to
    ! call a method on a result of function, hence instead of
    ! consistent
    ! f%dx(alpha(:,1))%vec()
    ! we are forced to the use of nvtor() which is defined to have
    ! the same result.
    g = f%dx(alpha)

  end subroutine initialization_f


  subroutine boundary_update_f(ic)
    class(icicles_user) :: ic

    class(named_vector_f), pointer :: f
    class(named_vector_user), pointer   :: x

    ! extract f from icicles and convert it to named_vector_f
    f => nvtof(ic%get("f"))

    ! extract the first variable from coordinates on which f is
    ! defined
    x => f%var(1)

    !      3         1 = neumann
    !    +---+       2 = dirichlet
    !  1 |   | 2     3 = neumann
    !    +---+       4 = dirichlet
    !      4
    !
    ! The above is the list of boundary conditions defined for f when
    ! it was constructed using
    ! named_vector_f_implementation(..., btypes = [...], ...)
    !
    ! each boundary condition has one parameter which we set to x.
    ! x%vec() is used to refer to the real array which x envelops
    !
    call f%bbox_param_set(1,1,x%vec())
    call f%bbox_param_set(2,1,x%vec())
    call f%bbox_param_set(3,1,x%vec())
    call f%bbox_param_set(4,1,x%vec())

  end subroutine boundary_update_f


end module init

program derivator_test

  use helper_module

  use init

  use class_derivator
  use class_derivator_g2d

  use class_mesh2d_sfd3pt

  use class_icicles
  use class_icicles_implementation

  use class_named_vector_user
  use class_named_vector_implementation
  use class_named_vector_f_implementation_ghost

  use class_generic_function_fortran

  class(derivator), pointer :: der
  class(icicles), pointer :: ic
  class(named_vector_user), pointer :: x, y, g, f
  real, pointer :: u(:)
  integer :: n(2), len

  ! size of the mesh
  n        = [5,6]
  ! temporary variable, needed due to ifort bug
  len      = product(n)

  ! create named_vectors to be used as spatial variables
  x => named_vector_implementation("x",len)
  y => named_vector_implementation("y",len)

  ! create 2d derivator using ghost points
  der => derivator_g2d( x, y, n, m = mesh2d_sfd5pt() )

  ! create function to be differentiated
  !
  ! btypes are the specific boundary conditions
  !
  ! bbox_update is a generic_function called by f%update_dx() before calculating
  ! the derivative
  f => named_vector_f_implementation(&
       name = "f",&
       d = der,&
       btypes = [character(len=10)::"neumann", "dirichlet", "neumann", "dirichlet"],&
       bbox_update = generic_function_fortran(boundary_update_f))

  ! create a named_vector to store the result of differentiation
  g => named_vector_implementation("g",len)

  ! create icicles
  ! init is the generic_function used to set the initial values of
  ! x,y,f and g
  ic => icicles_implementation(&
       init = generic_function_fortran(initialization_f) )

  ! add all the named_vectors
  call ic%add(f)
  call ic%add(g)
  call ic%add(x)
  call ic%add(y)

  ! allocate all the space needed for icicles.
  ! ic%length() is the total length of all the named_vectors we added
  allocate( u(ic%length()) )
  ! point all the named_vectors inside icicles to the array u
  call ic%point(u)

  ! initialize values of spatial variables added to der, x and y
  call der%initialize_x()
  call ic%initialize()

  ! print the states of all named_vectors
  call print_vec1d_as_2d(x%vec(),n)
  call print_vec1d_as_2d(y%vec(),n)
  call print_vec1d_as_2d(f%vec(),n)
  call print_vec1d_as_2d(g%vec(),n)


end program derivator_test
