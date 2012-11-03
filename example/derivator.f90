module init
  use class_named_vector_user_
  use class_icicles_user_
  use class_named_vector_f
  use class_coordinates

contains

  subroutine init_f(ic)
    class(icicles_user) :: ic

    class(named_vector_f), pointer :: f
    class(named_vector_user), pointer :: xu, g
    class(coordinates), pointer :: co
    real, pointer :: x(:)

    integer :: alpha(1,1), nx
    alpha = 1

    print *, "init_f()"

    f  => nvtof(ic%get("f"))
    co => f%c()
    nx = co%length()

    xu => co%var(1)
    xu = [((i-1.)/(nx-1),i=1,nx)]

    g => ic%get("g")

    x => xu%vec()
    f = x

    call f%dx_update(alpha, ic)

    g = nvtor(f%dx([1]))

  end subroutine init_f

  subroutine fb_update_f(ic)
    class(icicles_user) :: ic

    class(named_vector_f), pointer :: ff
    class(named_vector_user), pointer   :: xu, aleftu
    class(coordinates), pointer :: co
    real, pointer :: x(:), aleft(:)
    integer, pointer :: left(:), right(:)

    print *, "fb_update_f()"

    ff  => nvtof(ic%get("f"))

    co  => ff%c()

    xu  => co%var(1)
    x   => xu%vec()

    left  => co%bregion(1)
    right => co%bregion(2)

    aleftu => ff%bbox_param(1,1)
    aleftu  = x(left)

    aleftu => ff%bbox_param(2,1)
    aleftu  = x(right)

  end subroutine fb_update_f


end module init

program derivator_test

  use init

  use class_derivator
  use class_derivator_g1d

  use class_icicles_
  use class_icicles_implementation

  use class_named_vector_
  use class_named_vector_user_
  use class_named_vector_implementation_
  use class_named_vector_f
  use class_named_vector_f_implementation_ghost

  use class_coordinates

  use class_generic_function
  use class_generic_function_fortran


  class(derivator), pointer :: der
  class(coordinates), pointer :: co
  class(generic_function), pointer :: t0, fb_update
  class(icicles), pointer :: ic
  class(named_vector_user), pointer :: x, f, g, df, x_


  real, pointer :: xx(:)
  real, pointer :: u(:)

  integer :: dx(1,1), nx

  dx(1,1) = 1
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

  ic => icicles_implementation(init=t0)

  call ic%add(f)
  call ic%add(g)
  call ic%add(x)

  allocate(u(ic%length()))
  allocate(u(100))

  u = 1.
  call ic%point(u)
  call ic%initialize()

  g => ic%get("g")

  ! dfu => f%dx(dx(:,1))

  print *, g%vec()
  ! print *, nvtor(f)
  ! ! print *, nvtor(dfu)
  ! print *, nvtor(g)

end program derivator_test
