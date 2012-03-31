program icicles_registry_set_pointers

  use constants_module
  use icicles_module
  use icicles_registry_module

  type(icicles_registry) :: reg
  type(icicles), pointer :: ic => null()

  integer, parameter :: n = 2+3+1+1 !total length of entries
  real, target :: x(n)
  real, target :: x_short(1)
  integer :: rng1(2) = [1,2]    !left half
  integer :: rng2(2) = [2,3]    !right half
  integer :: rng3(2) = [3,4]    !middle

  integer :: err

  ! add vectors and scalars in a mixed way
  call reg%add("v1",2)
  call reg%add("s1")
  call reg%add("v2",3)
  call reg%add("s2")

  ! manually create icicles
  allocate(ic)
  allocate(ic%vectors(4))

  ic%vectors(1)%name = "v1"
  ic%vectors(1)%val => null()

  ic%vectors(2)%name = "s1"
  ic%vectors(2)%val => null()

  ic%vectors(3)%name = "v2"
  ic%vectors(3)%val => null()

  ic%vectors(4)%name = "s2"
  ic%vectors(4)%val => null()


  ! test set pointers
  call reg%set_pointers(ic, x, rng1, error = err)

  if( err /= FPDE_STATUS_OK ) then
     stop 1
  end if

  x = 1
  x(1) = -1
  x(2+1) = -1

  ! test left end
  if( .not. associated(ic%vectors(1)%val) .or. &
       .not. associated(ic%vectors(2)%val) ) then
     stop 2
  else if (&
       ic%vectors(1)%val(1) /= -1  .or.&
       ic%vectors(1)%val(2) /=  1  .or.&
       ic%vectors(2)%val(1) /= -1 ) then
     stop 3
  end if

  ! reset pointers
  ic%vectors(1)%val => null()
  ic%vectors(2)%val => null()
  ic%vectors(3)%val => null()
  ic%vectors(4)%val => null()

  ! test middle
  call reg%set_pointers(ic, x, rng2, error = err)
  if( err /= FPDE_STATUS_OK ) then
     stop 4
  end if

  x = 1
  x(1) = -1
  x(1+3) = -1

  if( &
       .not. associated(ic%vectors(2)%val) .or. &
       .not. associated(ic%vectors(3)%val)) then
     stop 6
  else if( &
       ic%vectors(2)%val(1) /= -1 .or.&
       any(ic%vectors(3)%val(1:2) /= 1) .or.&
       ic%vectors(3)%val(3) /= -1 ) then
     stop 7
  end if

  ! reset pointers
  ic%vectors(1)%val => null()
  ic%vectors(2)%val => null()
  ic%vectors(3)%val => null()
  ic%vectors(4)%val => null()

  ! test right end
  call reg%set_pointers(ic, x, rng3, error = err)
  if( err /= FPDE_STATUS_OK ) then
     stop 8
  end if
  x = 1
  x(1) = -1
  x(3+1) = -1

  if( .not. associated(ic%vectors(3)%val) .or. &
       .not. associated(ic%vectors(4)%val) ) then
     stop 9
  else if( ic%vectors(4)%val(1) /= -1 .or.&
       ic%vectors(3)%val(1) /= -1 .or. &
       any(ic%vectors(3)%val(2:3) /= 1)) then
     stop 10
  end if

  ! test vector too short error
  call reg%set_pointers(ic, x_short, rng1, error = err)
  if( err /= FPDE_STATUS_ERROR ) then
     stop 11
  end if

  deallocate(ic%vectors)
  deallocate(ic)

end program icicles_registry_set_pointers
