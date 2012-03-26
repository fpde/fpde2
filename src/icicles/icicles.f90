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
  !! wrapper to icicles
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
     type(named_vector), pointer :: vectors(:)
     type(named_scalar), pointer :: scalars(:)
   contains
     procedure, private :: get_vector
     procedure, private :: get_scalar
     generic :: get => get_vector, get_scalar
  end type icicles

contains

  !> returns error=FPDE_STATUS_ERROR if vector was not found
  function get_vector(ic, name, v) result(error)
    class(icicles) :: ic
    type(named_vector), pointer, intent(out) :: v
    character(len=*) :: name
    integer :: error
    integer :: i

    do i = 1, size(ic%vectors)
       if(trim(ic%vectors(i)%name)==trim(name)) then
          v=>ic%vectors(i)
          error = FPDE_STATUS_OK
          return
       end if
    end do

    error = FPDE_STATUS_ERROR

  end function get_vector

  !> returns FPDE_STATUS_ERROR if scalar was not found
  function get_scalar(ic, name, s) result(error)
    class(icicles) :: ic
    type(named_scalar), pointer, intent(out) :: s
    character(len=*) :: name
    integer :: error
    integer :: i

    do i = 1, size(ic%scalars)
       if(trim(ic%scalars(i)%name)==trim(name)) then
          s=>ic%scalars(i)
          error = FPDE_STATUS_OK
          return
       end if
    end do

    error = FPDE_STATUS_ERROR

  end function get_scalar

end module icicles_module
