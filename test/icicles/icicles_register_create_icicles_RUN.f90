program icicles_registry_create_icicles
  use constants_module
  use icicles_module
  use icicles_registry_module

  type(icicles_registry) :: reg
  type(icicles), pointer :: ic => null()

  integer, parameter :: n_v = 4
  character(len=6) :: name_v(n_v) = ["vname1","vname2","vname3","vname4"]
  integer :: ln_v(n_v) = [3,1,5,4]
  integer :: i, err

  type(named_vector), pointer :: v => null()

  ! add vectors
  do i = 1, n_v
     call reg%add(name_v(i),ln_v(i))
  end do

  ! create icicles
  call reg%create_icicles(ic, error = err)

  ! check error status
  if ( err /= FPDE_STATUS_OK ) then
     stop 2
  end if

  ! check size of allocated ic%data
  if ( .not. associated(ic%data) ) then
     stop 3
  else if ( size(ic%data) /= sum(ln_v) ) then
     stop 4
  end if

  ! check number of allocated vectors and scalars
  if ( size(ic%vectors) /= n_v ) then
     stop 6
  end if

  ! check if vectors were added to icicles
  do i = 1, n_v
     call ic%get(name_v(i), v, error = err)
     if ( err /= FPDE_STATUS_OK ) then
        stop 7
     ! implicit check of ic%get_vector
     else if ( v%name /= name_v(i) ) then
        stop 8
     ! implicit check of set_pointers
     else if ( size(v%val) /= ln_v(i) ) then
        stop 9
     end if
  end do


end program icicles_registry_create_icicles
