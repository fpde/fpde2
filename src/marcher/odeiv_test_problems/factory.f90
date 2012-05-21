!>
!! @file   factory.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Thu Apr 12 18:28:02 2012
!!
!! @brief  ODE Initial Value test problems factory.
!!
!! Avaiable types of ODEIV:
!! - AREN:
!! - HARM:
!! - VDPOL:
!!
module class_odeiv_factory

   use class_odeiv_generic
   use class_odeiv_aren
   use class_odeiv_harm
   use class_odeiv_van_der_Pol

contains

   function odeiv_new(id) result(c)
      class(odeiv_generic), pointer :: c
      character(len=*) :: id

      select case( trim(id) )

      case ( "AREN" )
         allocate( odeiv_aren :: c )

      case ( "HARM" )
         allocate( odeiv_harm :: c )

      case ( "VDPOL" )
         allocate( odeiv_vdp :: c )

      case default
         nullify( c )
      end select

   end function odeiv_new

end module class_odeiv_factory
