!>
!! @file   heat.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sun Aug  5 17:09:36 2012
!!
!! @brief  Heat equation on unit square.
!!
!! @todo - [] write doc
!!
module class_odeiv_heat

   use ieee_arithmetic

   use constants_module
   use logger_module
   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_heat
      integer :: nx = 10
      integer :: ny = 10
      real, pointer :: x(:), y(:)
   contains
      procedure :: init
   end type odeiv_heat

contains

   subroutine init(this)
      class(odeiv_heat) :: this
      integer :: i,j
      integer :: nx, ny
      real, parameter :: pi = acos(-1.0)
      real, pointer :: x(:,:), y(:,:), y0(:,:)

      this % name = 'HEAT'
      this % description = "heat equation on unit square"

      nx = this % nx
      ny = this % ny

      allocate( this % y0(nx*ny), this % y1(nx*ny), &
           this % x(nx*ny), this % y(nx*ny) )

      x(1:nx,1:ny) => this % x
      y(1:nx,1:ny) => this % y
      y0(1:nx,1:ny) => this % y0

      y0 = 0.0

      y0(1,:) = 1.0

      do i=1,nx
         do j=1,ny
            x(i,j) = real(i-1)/(nx-1)
            y(i,j) = real(j-1)/(ny-1)
         end do
      end do

      y0(2:nx-1,2:ny-1) = sin(2*pi*x(2:nx-1,2:ny-1))*sin(pi*y(2:nx-1,2:ny-1))

      this % h = 1.0e-4
      this % t = [ 0.0, 40.0 ]

      call this%sys%init(fun = heat_rhs, jac = heat_jac, dim = nx*ny, params=this)

   end subroutine init

   subroutine heat_rhs(t, y, dydt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(odeiv_heat) :: params
      integer, optional :: status

      integer :: i, j, nx, ny
      real :: hx2, hy2
      real, pointer :: u(:,:), du(:,:), xx(:,:), yy(:,:)
      real, parameter :: pi = acos(-1.0)

      nx = params % nx
      ny = params % ny

      hx2 = 1.0/real(nx-1)/real(nx-1)
      hy2 = 1.0/real(ny-1)/real(ny-1)

      xx(1:nx,1:ny) => params % x
      yy(1:nx,1:ny) => params % y

      u(1:nx,1:ny) => y
      du(1:nx,1:ny) => dydt

      du = 0.0

      do i=2,nx-1
         do j=2,ny-1

            du(i,j) = (u(i-1,j)-2*u(i,j)+u(i+1,j))/hx2 + (u(i,j-1)-2*u(i,j)+u(i,j+1))/hy2 &
                 + sin(0.1*t)**4*sin(2*pi*xx(i,j))*sin(pi*yy(i,j))

         end do
      end do

      if( present(status) ) then
         if( any(ieee_is_nan(dydt)) .or. (.not. all(ieee_is_finite(dydt))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine heat_rhs

   subroutine heat_jac(t, y, dfdy, dfdt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(odeiv_heat) :: params
      integer, optional :: status
      !> local varables
      integer :: i, j, nx, ny
      real :: hx2, hy2
      real, pointer :: u(:,:), du(:,:)
      real, pointer :: dfdu(:,:,:,:)

      nx = params % nx
      ny = params % ny

      hx2 = 1.0/real(nx-1)/real(nx-1)
      hy2 = 1.0/real(ny-1)/real(ny-1)

      u(1:nx,1:ny) => y
      dfdu(1:nx,1:ny,1:nx,1:ny) => dfdy

      !dfdt = cos(t)*sin(pi*x(2:nx-1,2:ny-1))*sin(pi*y(2:nx-1,2:ny-1))
      dfdu = 0.0

      do i=2,nx-1
         do j=2,ny-1

            dfdu(i,j, i-1:i+1:2, j) = 1./hx2

            dfdu(i,j, i,         j-1:j+1:2) = 1./hy2

            dfdu(i,j,i,j) = -2./hx2 -2./hy2

         end do
      end do

      if( present(status) ) then
         if( any(ieee_is_nan(dfdy)) .or. (.not. all(ieee_is_finite(dfdy))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine heat_jac

end module class_odeiv_heat
