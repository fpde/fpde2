!>
!! @file   sfd3pt.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat May 12 19:54:09 2012
!!
!! @brief An example mesh implementing a symmetric finite difference
!! method of order 2
!!
!!
!!
module class_mesh1d_sfd3pt
  use constants_module
  use logger_module
  use class_mesh1d

  private

  type, public, extends( mesh1d ) :: mesh1d_sfd3pt
   contains
     ! overloaded procedures go here (if needed)
     procedure :: init
     procedure :: diff
     ! procedure :: diff_point
  end type mesh1d_sfd3pt

contains

  subroutine init( self, error )
    class(mesh1d_sfd3pt) :: self
    integer, optional, intent(out) :: error

    integer :: dx(1,2)

    if(present(error)) error = FPDE_STATUS_OK

    self % name = "sfd3pt"
    call self % set_ghost_points([2])

    dx(1,1) = 1
    dx(1,2) = 2
    call self % set_calculable_derivatives(dx)

  end subroutine init


  subroutine diff( self, f, x, df, k )
    class(mesh1d_sfd3pt), target, intent(inout) :: self
    real, intent(in) :: f(:), x(:)
    real, intent(out) :: df(:)
    integer, intent(in) :: k

    integer :: j, n
    integer, allocatable :: ders(:,:)
    real :: h, h2

    ders = self%get_calculable_derivatives()

    n = size(f)
    h = (x(2) - x(1))
    h2 = h**2

    select case(k)
    case(1)
       df(2:n-1) = (f(3:n)-f(1:n-2))/h/2.
       df(1    ) = (f(2)-f(1  ))/(x(2)-x(1))
       df(n    ) = (f(n)-f(n-1))/(x(n)-x(n-1))

    case(2)
       df(2:n-1) = (f(3:n)-2*f(2:n-1)+f(1:n-2))/h2
       df(1    ) = (2*f(1)-5*f(2)+4*f(3)-f(4))/h2
       df(n    ) = (2*f(n)-5*f(n-1)+4*f(n-2)-f(n-3))/h2
    case default
       call self%loge("diff(): Too large rank of derivative to calculate with this mesh")
    end select

  end subroutine diff


  function diff_point( self, f, x, k, i ) result(d)
    class(mesh1d_sfd3pt), target, intent(inout) :: self
    integer, intent(in) :: i,k
    real, intent(in) :: f(:), x(:)
    real :: d

    integer :: j, n
    real :: h

  end function diff_point


end module class_mesh1d_sfd3pt
