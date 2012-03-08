program icicles_prog
  use icicles_module
  use tentacle_module

  integer :: i
  character(len=10) :: str

  type(named_vector), pointer :: v
  type(magical_tentacle) :: t
  type(icicles), pointer :: ic

  do i = 1, 9
     write(str,'(i1)') i
     str=trim(adjustl(str))
     call t%add(str,i)
  end do

  call t%create_icicles(ic)
  ! ic%data = 0
  ! print *, ic%data
  ic%data = [(i,i=1,55)]

  do i = 1, 9
     write(str,'(i1)') i
     str=trim(adjustl(str))

     if( ic%get(str,v) == 0 ) then
        v%val = i
     end if
  end do

  print *, ic%data


end program icicles_prog
