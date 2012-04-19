program icicles_registry_default_pointer_alignment

  use constants_module
  use icicles_module
  use icicles_registry_module

  type(icicles_registry) :: reg
  type(icicles), pointer :: ic

  integer, parameter :: n = 3   ! maximal n = 9
  character(len=5) :: name(n)
  ! mixed lengths
  integer :: ln(n)

  integer :: i, s_err, v_err, err, m
  character(len=10) :: str
  real, pointer, contiguous :: x(:)
  real, pointer :: v(:)
  real, pointer :: s


  ln = [(i,i=1,n)]
  do i = 1, n
     ln(i) = i                  ! sizes of entries
     write(str,'(i3)') i
     name(i) = trim(adjustl(str))
  end do
  m = sum(ln)

  ! add several vectors and scalars
  do i = 1, n
     call reg%add(name(i),ln(i), error = err)
     if( err /= FPDE_STATUS_OK ) then
        stop 1
     end if
  end do

  call reg%create_icicles(ic)

  ! one way test, data is fixed, check if vectors and scalars read it
  ! correctly
  ic%data = [(i,i=1,m)]

  do i = 1, n

     ! cath a vector or scalar with given name
     call ic%get(name(i), v, error = v_err)
     call ic%get(name(i), s, error = s_err)

     if( v_err == FPDE_STATUS_OK ) then
        if( any( v /= ic%data( sum(ln(1:i-1))+1 : sum(ln(1:i)) ))) then
           stop 1
        end if
     else if( s_err == FPDE_STATUS_OK ) then
        if( s /= sum( ln(1:i-1)) + 1 ) then
           stop 2
        end if
     else
        stop 3
     end if

  end do


  ! other way around, update vectors and check if ic%data is updated as well
  do i = 1, n
     call ic%get(name(i), v, error = v_err)
     call ic%get(name(i), s, error = s_err)

     if( v_err == FPDE_STATUS_OK ) then
        v = -1
     else if( s_err == FPDE_STATUS_OK ) then
        s = -1
     else
        stop 4
     end if
  end do

  if( any( ic%data /= -1 ) ) then
     stop 5
  end if

end program icicles_registry_default_pointer_alignment
