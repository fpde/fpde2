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
  use class_mesh1d

  private

  type, public, extends( mesh1d ) :: mesh_sfd3pt
   contains
     ! overloaded procedures go here (if needed)
     procedure :: init
     procedure :: diff
     ! procedure :: diff_point
  end type mesh_sfd3pt

contains

  subroutine init( p, error )
    class(mesh_sfd3pt), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK

    p % name = "sfd3pt"
    call p % set_ghost_points([1])
    call p % set_calculable_derivatives(reshape([1],[1,1]))

  end subroutine init

  subroutine diff( self, f, x, df, k )
    class(mesh_sfd3pt), target, intent(inout) :: self
    real, intent(in) :: f(:), x(:)
    real, intent(out) :: df(:,:)
    integer, intent(in) :: k(:)

    integer :: j, n, kk
    integer, allocatable :: ders(:,:)

    real :: h

    ders = self%get_calculable_derivatives()

    if( any( k > ders(:,1) ) ) then
       call self%log(FPDE_LOG_ERROR,&
            "Too large rank of derivative to calculate with this mesh")
       return
    end if

    n = size(f)
    h = x(2) - x(1)             ! we assume x is a uniform grid

    do kk = 1, size(k)
       if( kk == 1 ) then
          forall( j = 2 : n - 1 )
             ! df(j,kk)=(f(j+1)-f(j-1))/h/2.
             df(j,kk)=(f(j+1)-2*f(j)+f(j-1))/h/h
          end forall
          df(1,kk) = (f(2)-f(1  ))/h
          df(n,kk) = (f(n)-f(n-1))/h
       end if
    end do


  end subroutine diff

  function diff_point( self, f, x, k, i ) result(d)
    class(mesh_sfd3pt), target, intent(inout) :: self
    integer, intent(in) :: i,k
    real, intent(in) :: f(:), x(:)
    real :: d

    integer :: j, n
    real :: h

  end function diff_point


end module class_mesh_sfd3pt
