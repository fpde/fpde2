!>
!! @file   hires.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Tue Sep  4 11:45:27 2012
!!
!! @brief Chemical reaction system.
!!
!! Chemical reaction system involving eight reactants proposed
!! by Scheafer (1975).
!!
module class_odeiv_hires

   use ieee_arithmetic

   use constants_module
   use logger_module
   use class_odeiv_generic

   private

   type, public, extends(odeiv_generic) :: odeiv_hires
   contains
      procedure :: init
   end type odeiv_hires

contains

   subroutine init(this)
      class(odeiv_hires) :: this
      integer, parameter :: n=8

      this % name = 'HIRES'
      this % description = "chemical reaction system"

      allocate( this % y0(n), this % y1(n) )

      !> initial conditions
      this % y0(1) = 1.0
      this % y0(2:7) = 0.0
      this % y0(8) = 0.0057

      this % h = 1.0e-4
      this % t = [ 0.0, 321.8122 ]

      call this%sys%init(fun = hires_rhs, jac = hires_jac, dim = n, params=this)

   end subroutine init

   subroutine hires_rhs(t, y, dydt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dydt(:)
      class(odeiv_hires) :: params
      integer, optional :: status

      dydt = [ &
           0.0007 - (171*y(1))/100. + (43*y(2))/100. + (208*y(3))/25., &
           (171*y(1))/100. - (35*y(2))/4., &
           (-1003*y(3))/100. + (43*y(4))/100. + (7*y(5))/200., &
           (208*y(2))/25. + (171*y(3))/100. - (28*y(4))/25., &
           (-349*y(5))/200. + (43*y(6))/100. + (43*y(7))/100., &
           (69*y(4))/100. + (171*y(5))/100. - (43*y(6))/100. + (69*y(7))/100. - 280*y(6)*y(8), &
           (-181*y(7))/100. + 280*y(6)*y(8), &
           (181*y(7))/100. - 280*y(6)*y(8) &
           ]

      if( present(status) ) then
         if( any(ieee_is_nan(dydt)) .or. (.not. all(ieee_is_finite(dydt))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine hires_rhs

   subroutine hires_jac(t, y, dfdy, dfdt, params, status)
      real, intent(in) :: t
      real, pointer, contiguous, intent(in) :: y(:)
      real, pointer, contiguous, intent(out) :: dfdy(:,:)
      real, pointer, contiguous, intent(out) :: dfdt(:)
      class(odeiv_hires) :: params
      integer, optional :: status

      dfdt = 0.0

      ! df_i/dy_j = dfdy(i,j)
      dfdy(1,:) = [-1.71,0.43,8.32,0.,0.,0.,0.,0.]
      dfdy(2,:) = [1.71,-8.75,0.,0.,0.,0.,0.,0.]
      dfdy(3,:) = [0.,0.,-10.03,0.43,0.035,0.,0.,0.]
      dfdy(4,:) = [0.,8.32,1.71,-1.12,0.,0.,0.,0.]
      dfdy(5,:) = [0.,0.,0.,0.,-1.745,0.43,0.43,0.]
      dfdy(6,:) = [0.,0.,0.,0.69,1.71,-0.43 - 280*Y(8),0.69,-280*Y(6)]
      dfdy(7,:) = [0.,0.,0.,0.,0.,280*Y(8),-1.81,280*Y(6)]
      dfdy(8,:) = [0.,0.,0.,0.,0.,-280*Y(8),1.81,-280*Y(6)]

      if( present(status) ) then
         if( any(ieee_is_nan(dfdy)) .or. (.not. all(ieee_is_finite(dfdy))) ) then
            status = FPDE_STATUS_ERROR
         else
            status = FPDE_STATUS_OK
         end if
      end if

   end subroutine hires_jac

end module class_odeiv_hires
