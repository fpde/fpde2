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


  type, public :: icicles
     real, pointer, contiguous :: data(:)!> the data is stored here,
                                         !everything else points to
                                         !icicles%data
     type(named_vector), pointer :: vectors(:)
   contains
     procedure, private :: get_vector
     procedure, private :: get_named_vector
     procedure, private :: get_scalar
     generic :: get => get_vector, get_scalar, get_named_vector
  end type icicles

contains

  !> returns error=FPDE_STATUS_ERROR if vector was not found
  subroutine get_vector(ic, name, v, error)
    class(icicles) :: ic
    real, pointer, intent(out) :: v(:)
    character(len=*) :: name
    integer, optional, intent(out) :: error

    type(named_vector), pointer :: nv
    integer :: i, err

    if(present(error)) error = FPDE_STATUS_ERROR

    call ic%get_named_vector(name, nv, error = err)

    if ( err /= FPDE_STATUS_OK ) then
       return
    end if

    v => nv%val

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine get_vector

  !> returns FPDE_STATUS_ERROR if scalar was not found
  subroutine get_scalar(ic, name, s, error)
    class(icicles) :: ic
    real, pointer, intent(out) :: s
    character(len=*), intent(in) :: name
    integer, optional, intent(out) :: error

    integer :: i, err
    real, pointer :: v(:) => null()

    if(present(error)) error = FPDE_STATUS_ERROR

    call ic%get_vector(name, v, error = err)

    if ( err /= FPDE_STATUS_OK ) then
       return
    else if ( size(v) > 1 ) then
       return
    end if

    s => v(1)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine get_scalar


  !> returns error=FPDE_STATUS_ERROR if vector was not found
  subroutine get_named_vector(ic, name, v, error)
    class(icicles) :: ic
    type(named_vector), pointer, intent(out) :: v
    character(len=*) :: name
    integer, optional, intent(out) :: error
    integer :: i

    if(present(error)) error = FPDE_STATUS_ERROR

    do i = 1, size(ic%vectors)
       if(trim(ic%vectors(i)%name)==trim(name)) then

          v => ic%vectors(i)

          if(present(error)) error = FPDE_STATUS_OK
          return
       end if
    end do

  end subroutine get_named_vector


end module icicles_module
