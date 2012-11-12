module init
  use class_named_vector_user_
  use class_icicles_user_
  use class_named_vector_f
  use class_coordinates

contains

  subroutine init_f(ic)
    class(icicles_user) :: ic

    class(named_vector_f), pointer :: f
    class(named_vector_user), pointer :: x, g

    integer :: alpha(1,1), nx

    alpha(:,1) = [2]

    f  => nvtof(ic%get("f"))
    x  => f%var(1)
    g  => ic%get("g")

    nx = x%length()
    x = [( (i-1.)/(nx-1.), i=1, nx )]

    f = 0.5*x%vec()**2

    call f%dx_update(alpha, ic)

    g = nvtor(f%dx([2]))

  end subroutine init_f


  subroutine fb_update_f(ic)
    class(icicles_user) :: ic

    class(named_vector_f), pointer :: f
    class(named_vector_user), pointer   :: x

    f => nvtof(ic%get("f"))
    x => f%var(1)

    call f%bbox_param_set(1,1,x%vec())
    call f%bbox_param_set(2,1,x%vec())

  end subroutine fb_update_f


end module init

program derivator_test

  use init

  use class_derivator
  use class_derivator_g1d

  use class_icicles_
  use class_icicles_implementation

  use class_named_vector_user_
  use class_named_vector_implementation_
  use class_named_vector_f_implementation_ghost

  use class_generic_function
  use class_generic_function_fortran

  class(derivator), pointer :: der
  class(generic_function), pointer :: t0, fb_update
  class(icicles), pointer :: ic
  class(named_vector_user), pointer :: x, f, g

  real, pointer :: u(:)

  integer :: nx, i

  nx      = 11

  t0        => generic_function_fortran(init_f)
  fb_update => generic_function_fortran(fb_update_f)

  x => named_vector_implementation("x",nx)

  der => derivator_g1d( x, "sfd3pt" )

  f => named_vector_f_implementation(&
       name = "f",&
       d = der,&
       btypes = ["neumann", "neumann"],&
       bbox_update = fb_update)

  g => named_vector_f_implementation(&
       name = "g",&
       d = der)

  ic => icicles_implementation( init = t0 )

  call ic%add(f)
  call ic%add(g)
  call ic%add(x)

  allocate(u(ic%length()))

  call ic%point(u)

  call ic%initialize()

  g => ic%get("g")

  print *, g%vec()

end program derivator_test
