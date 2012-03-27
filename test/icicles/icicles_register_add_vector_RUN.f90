!>
!! @file   iciles_register_add_mixed_RUN.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Tue Mar 27 15:36:05 2012
!!
!! @brief adds mixed entries to icicles_register and checks results
!!

program icicles_register_add_vector

  use icicles_register_module

  type(icicles_register) :: reg

  integer, parameter :: n = 4
  character(len=5) :: name(n) = ["name1","name2","name3","name4"]
  integer :: ln(n) = [3,2,5,4]
  integer :: i

  do i = 1, n
     call reg%add(name(i),ln(i))
  end do

  if( .not. reg%n_entries == n ) then
     stop 1
  end if

  do i = 1, n
     if( .not. trim(reg%entries(i)%name) == name(i) ) then
        stop 2
     end if
  end do

  do i = 1, n
     if( .not. reg%entries(i)%len == ln(i) ) then
        stop 3
     end if
  end do

end program icicles_register_add_vector
