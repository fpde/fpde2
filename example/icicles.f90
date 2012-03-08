!>
!! @file   icicles.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Thu Mar  8 21:40:28 2012
!!
!! @brief  example of icicles
!!
!! @todo try to do some caching of vectors and scalars positions in
!! order to use them with t%set_pointers, now they are looked up using
!! icicles%get.
!!
!! @todo replace [1,n] with sequence 1,n in t%set_pointers?
!!

program icicles_prog
  use icicles_module
  use tentacle_module

  integer :: i,j
  integer ::  n = 40
  character(len=10) :: str

  type(named_vector), pointer :: v
  type(named_scalar), pointer :: s
  type(magical_tentacle) :: t
  type(icicles), pointer :: ic

  real, target :: x(5)


  ! create n registry entries, each with len=i and name=i
  do i = 1, n
     write(str,'(i4)') i
     str=trim(adjustl(str))
     call t%add(str,i)
  end do

  ! create icicles from registry
  call t%create_icicles(ic)
  ! initialize data inside icicles
  ic%data = [(n*(n+1)/2-i+1,i=1,n*(n+1)/2)]
  print *, ic%data

  do i = 1, n
     write(str,'(i4)') i
     str=trim(adjustl(str))
     ! obtain vector 'i' from icicles and set it to i
     if( ic%get(str,v) == 0 ) then
        v%val = i
     end if
  end do

  ! set value of the only scalar to n
  if( ic%get("1",s) == 0 ) then
     s%val=1
  else
     print *, "scalar not found"
  end if

  do i = 1, n
     do j = 1, i
        print *, "testing data[",i,"]", ic%data(i*(i-1)/2+j) == i
     end do
  end do

  ! lets try to point some vectors from icicles to x
  x = -1
  call t%set_pointers(ic, [2,3], x)

  ! all of the following tests should print "T"
  if( ic%get("2",v) == 0) then
     print *, v%val == -1
  else
     stop
  end if

  if( ic%get("3",v) == 0 ) then
     print *, v%val == -1
  else
     stop
  end if

  ! now lets change the bounary values
  x(1) = -2
  x(5) = -2

  ! all of the following tests should print "T"
  if( ic%get("2",v) == 0) then
     print *, v%val(1) == -2
  else
     stop
  end if

  if( ic%get("3",v) == 0 ) then
     print *, v%val(3) == -2
  else
     stop
  end if

  ! meanwhile, data shouldn't be changed
  do i = 1, n
     do j = 1, i
        print *, "testing data[",i,"]", ic%data(i*(i-1)/2+j) == i
     end do
  end do

  ! reset pointers
  call t%set_pointers(ic,[1,n],ic%data)

  ! lets check if the pointers are pointing back to data now
  if( ic%get("2",v) == 0) then
     print *, v%val == 2
  else
     stop
  end if

  if( ic%get("3",v) == 0 ) then
     print *, v%val == 3
  else
     stop
  end if



end program icicles_prog
