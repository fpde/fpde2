!>
!! @file   factory.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 12 18:49:52 2012
!!
!! @brief  ODE stepper factory
!!
!! Avaiable types:
!! - rkv65
!!
module class_ode_stepper_factory

   use class_ode_stepper
   use class_ode_stepper_rkv65

contains

   function ode_stepper_new(id) result(c)
      class(ode_stepper), pointer :: c
      character(len=*) :: id

      select case( trim(id) )

      case( "rkv65" )
         allocate( ode_stepper_rkv65 :: c )

      case default
         nullify( c )
      end select

   end function ode_stepper_new


end module class_ode_stepper_factory
