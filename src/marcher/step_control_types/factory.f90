!>
!! @file   factory.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sat Mar 31 11:15:50 2012
!!
!! @brief  ODE step control factory.
!!
!! Available types of step control objects:
!! - standard: four parameter step control object
!! - scaled:
!! - standard_y: standard one with parameters @f$ a_y=1.0, a_dydt=0.0 @f$
!! - standard_dydt: standard one with parameters @f$ a_y=0.0, a_dydt=1.0 @f$
!!
!! @todo write docs
!!
module class_ode_step_control_factory

   use class_ode_step_control
   use class_ode_step_control_standard
   use class_ode_step_control_scaled
   use class_ode_step_control_standard_y
   use class_ode_step_control_standard_dydt

contains

   function ode_step_control_new(id) result(c)
      class(ode_step_control), pointer :: c
      character(len=*) :: id

      select case( trim(id) )

      case( "standard" )
         allocate( ode_step_control_standard :: c )

      case( "scaled" )
         allocate( ode_step_control_scaled :: c )

      case( "standard_y" )
         allocate( ode_step_control_standard_y :: c )

      case( "standard_dydt" )
         allocate( ode_step_control_standard_dydt :: c )

      case default
         ! print *, "invalid id" ! @todo should log error, how? it's not named object
         nullify( c )
      end select

   end function ode_step_control_new

end module class_ode_step_control_factory
