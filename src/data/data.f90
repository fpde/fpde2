!>
!! @file   data.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Mar  3 21:11:35 2012
!!
!! @brief  contains the primary data structure: the icicles
!!
!!
!!

module icicles_module

  use constants_module
  use logger_module
  !> @todo: later on it could be useful to define a bind(c) compatible
  ! wrapper to icicles
  ! use iso_c_binding

  private

  type, public :: named_vector
     real, pointer :: val(:)
     character(len=NAME_LEN) :: name
  end type named_vector


  type, public :: named_scalar
     real, pointer :: val
     character(len=NAME_LEN) :: name
  end type named_scalar


  type, public :: icicles
     real, pointer, contiguous :: data(:)!> the data is stored here,
                                         !everything else points to
                                         !icicles%data
     real, pointer :: evolved(:)!> vector pointing to what will be
                                !evolved using marcher
     type(named_vector), pointer :: vectors(:)
     type(named_scalar), pointer :: scalars(:)
  end type icicles


contains



end module icicles_module
