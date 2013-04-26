!> @file   generic_functions.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Apr 26 14:23:42 2013
!!
!! @brief  Header file for generic functions
!!
module generic_functions

  use class_generic_function
  use class_generic_function_dummy
  use class_generic_function_fortran
  use class_generic_function_from_object

  public :: generic_function

  public ::&
       &generic_function_dummy,&
       &generic_function_fortran,&
       &generic_function_from_object

end module generic_functions
