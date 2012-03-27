!>
!! @file   iciles_register_add_scalar_RUN.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Tue Mar 27 15:29:02 2012
!!
!! @brief  test icicles_register%add in scalar version
!!

program icicles_register_add_scalar

  use constants_module
  use icicles_register_module

  type(icicles_register) :: reg

  character :: name = "scalar"
  integer :: err

  call reg%add(name, error = err)

  if( err /= FPDE_STATUS_OK ) then
     stop 1
  end if

  if( .not. reg%n_entries == 1 ) then
     stop 1
  end if

  if( .not. trim(reg%entries(1)%name) == name ) then
     stop 2
  end if

  if( .not. reg%entries(1)%len == 1 ) then
     stop 3
  end if

  ! try to add a duplicate entry
  call reg%add(name, error = err)

  if( err /= FPDE_STATUS_ERROR ) then
     stop 5
  end if


end program icicles_register_add_scalar
