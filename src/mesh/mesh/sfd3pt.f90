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
module class_mesh_sfd3pt
  use constants_module
  use logger_module
  use class_mesh

  private

  type, public, extends( mesh ) :: mesh_sfd3pt
   contains
     ! overloaded procedures go here (if needed)
     procedure :: init
     procedure :: diff_global
     procedure :: diff_point
  end type mesh_sfd3pt

contains

  subroutine init( p, error )
    class(mesh_sfd3pt), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK

    p % name = "sfd3pt"
    p % ghost_points = 1
    p % max_derivative = 1

  end subroutine init

  subroutine diff_global( m, f, x, df, k )
    class(mesh_sfd3pt), target, intent(inout) :: m
    real, intent(in) :: f(:), x(:)
    real, intent(out) :: df(:)
    integer, intent(in) :: k

    integer :: j, n
    real :: h

    if( k > m%max_derivative ) then
       call m%log(FPDE_LOG_ERROR,&
            "Too large rank of derivative to calculate with this mesh")
       return
    end if

    n = size(f)
    h = x(2) - x(1)             ! we assume x is a uniform grid

    if( k == 1 ) then
       forall( j = 2 : n - 1 )
          df(j)=(f(j+1)-f(j-1))/h/2.
       end forall
       df(1) = (f(2)-f(1  ))/h
       df(n) = (f(n)-f(n-1))/h
    end if

  end subroutine diff_global

  function diff_point( m, f, x, k, i ) result(d)
    class(mesh_sfd3pt), target, intent(inout) :: m
    integer, intent(in) :: i,k
    real, intent(in) :: f(:), x(:)
    real :: d

    integer :: j, n
    real :: h

    if( k > m%max_derivative ) then
       call m%log(FPDE_LOG_ERROR,&
            "Too large rank of derivative to calculate with this mesh")
       return
    end if

    n = size(f)
    h = x(2) - x(1)             ! we assume x is a uniform grid

    if( k == 1 ) then
       if( 1 < i .and. i < n ) d = (f(i+1)-f(i-1))/h/2.
       if( i == 1 )            d = (f(2  )-f(1  ))/h
       if( i == n )            d = (f(n  )-f(n-1))/h
    end if

  end function diff_point


end module class_mesh_sfd3pt
