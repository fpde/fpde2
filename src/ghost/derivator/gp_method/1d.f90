module class_derivator_g1d_methods

  use class_boundary_ghost
  use class_mesh1d

  private

  type, public :: g1d_methods
   contains
     procedure :: dx => calculate_dx
  end type g1d_methods

contains

  subroutine calculate_dx(self, f, x, dfdx, k, mesh, b, p)
    class(g1d_methods) :: self
    real, intent(in) :: f(:), x(:), p(:,:)
    real, intent(out) :: dfdx(:)
    integer :: k
    class(mesh1d) :: mesh
    class(boundary_ghost), intent(in) :: b

    integer :: gp

    real, allocatable :: f_(:,:), x_(:,:), dfdx_(:,:)

    gp = mesh%get_ghost_points(1)

    ! e.g. gp=2:
    ! g - ghost point
    ! O - physical boundary
    ! o - inner points
    ! g g O o o o o o o o ...
    ! * * * * *
    !   * * * * *
    !           ^
    ! this is the last point affected by differentiating the ghost
    ! points. Total number of points needed is:
    ! gp + (2*gp+1) -1 = 3*gp
    ! the "-1" comes from one overlapping 'g' and '*' in the last row

    allocate(f_(3*gp,1), x_(3*gp,1), dfdx_(3*gp,1))

    f_(gp+1:,1) = f(1:)
    x_(gp+1:,1) = x(1:)

    x_(gp:1:-1,1)    = 2*x(1) - x(2:gp+1)

    call b%generate_values(&
         fin  = f_(gp+1:2*gp+1, :),&
         fout = f_(gp:1:-1,      :),&
         xin  = x_(gp+1:2*gp+1, :),&
         params = p)

    ! actual differentiation
    call mesh%diff( f_(:,1), x_(:,1), dfdx_, [k])

    ! update the result
    dfdx(1:2*gp) = dfdx_(gp+1:3*gp,1)

  end subroutine calculate_dx

end module class_derivator_g1d_methods
