!>
!! @file   iciles_register_add_vector_RUN.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Tue Mar 27 15:29:02 2012
!!
!! @brief  test icicles_register%add in vector version
!!

program icicles_register_add_vector

  use constants_module
  use icicles_register_module

  type(icicles_register) :: reg

  character :: name = "vector"
  integer :: l = 10, err

  call reg%add(name, l, error = err)

  if( err /= FPDE_STATUS_OK ) then
     stop 1
  end if

  if( .not. reg%n_entries == 1 ) then
     stop 2
  end if

  if( .not. trim(reg%entries(1)%name) == name ) then
     stop 3
  end if

  if( .not. reg%entries(1)%len == l ) then
     stop 4
  end if

  ! try to add a duplicate entry
  call reg%add(name, l, error = err)

  if( err /= FPDE_STATUS_ERROR ) then
     stop 5
  end if


end program icicles_register_add_vector
