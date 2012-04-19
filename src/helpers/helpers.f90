module helper_module

  public

  !> findloc_() tries to emulate the behavior of Fortran 2008 finfloc
  !! function, whic is not yet supported by ifort
  interface findloc_
     module procedure findloc_character
     module procedure findloc_integer
     module procedure findloc_logical
  end interface findloc_

  interface findloc_first
     module procedure findloc_first_character
  end interface findloc_first

contains

  function findloc_character ( array, val ) result(r)
    character(len=*), intent(in) :: array(:), val
    integer, allocatable :: r(:)

    r = findloc_logical(array == val, .true.)

  end function findloc_character

  function findloc_integer ( array, val ) result(r)
    integer, intent(in) :: array(:), val
    integer, allocatable :: r(:)

    r = findloc_logical(array == val, .true.)

  end function findloc_integer

  function findloc_logical ( array, val ) result(r)
    logical, intent(in) :: array(:), val

    integer :: i,j,n
    integer, allocatable :: r(:)

    j = 1
    n = count(array .eqv. val)
    allocate(r(n))

    do i = 1, size(array)
       if( array(i) .eqv. val ) then
          r(j) = i
          j = j + 1
       end if
    end do

  end function findloc_logical

  function findloc_first_character ( array, val ) result(i)
    character(len=*), intent(in) :: array(:), val
    integer :: i

    do i = 1, size(array)
       if( array(i) == val ) return
    end do

    ! produce out of bound value in case of error
    i = i + 1

  end function findloc_first_character

end module helper_module
