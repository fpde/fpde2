module helper_module

  use constants_module

  public

  !> findloc_() tries to emulate the behavior of Fortran 2008 finfloc
  !! function, whic is not yet supported by ifort
  interface findloc_
     module procedure findloc_character
     module procedure findloc_integer
     module procedure findloc_logical
  end interface findloc_

  interface findloc_first
     module procedure findloc_first_logical
     module procedure findloc_first_character
  end interface findloc_first

contains

  function sys( cmd )

#ifdef __INTEL_COMPILER
  use ifport
#endif

    character(len=*), intent(in) :: cmd
    logical :: sys

    integer :: estat

    sys = .false.

#ifdef __GFORTRAN__
    call execute_command_line(cmd, exitstat = estat)
    sys = (estat == 0)
#endif

#ifdef __INTEL_COMPILER
    sys = systemqq(cmd)
#endif

  end function sys


  subroutine mkdir( dirname, error )
    character(len=*), intent(in) :: dirname
    integer, intent(out), optional :: error

    character(len=:), allocatable :: cmd

    if( present(error) ) error = FPDE_STATUS_ERROR

    cmd = "mkdir -p " // dirname

    if( .not. sys(cmd) ) return

    if( present(error) ) error = FPDE_STATUS_OK
  end subroutine mkdir


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

    do i = lbound(array,1), ubound(array,1)
       if( array(i) .eqv. val ) then
          r(j) = i
          j = j + 1
       end if
    end do

  end function findloc_logical


  function findloc_first_logical ( array, val ) result(i)
    logical, intent(in) :: array(:)
    logical, optional, intent(in) :: val
    integer :: i

    logical :: v = .true.

    if(present(val)) v = val

    do i = lbound(array,1), ubound(array,1)
       if( array(i) .eqv. v ) return
    end do

    ! produce out of bound value in case when the searched value is
    ! not found
    i = size(array) + 1

  end function findloc_first_logical


  !> @todo [0] implement as a call to findloc_first_logical
  pure function findloc_first_character ( array, val ) result(i)
    character(len=*), intent(in) :: array(:), val
    integer :: i

    do i = 1, size(array)
       if( array(i) == val ) return
    end do

    ! produce out of bound value in case when the searched value is
    ! not found
    i = size(array) + 1

  end function findloc_first_character


  pure function join(chars,separator) result(r)
    character(len=*), intent(in) :: chars(:), separator
    character(len=:), allocatable :: r

    integer :: i, n

    n = size(chars)
    if( n == 0 ) then
       r = ""
       return
    else
       r = trim(chars(1))
       do i = 2, n
          if( chars(i) == "" ) return
          r = trim(r) // trim(separator) // trim(chars(i))
       end do
    end if

  end function join


  pure function itoa(i)
    integer, intent(in) :: i

    character(len=:), allocatable :: itoa

    character(len=100) :: temp
    write(temp, '(g0)') i
    itoa = trim(adjustl(temp))
  end function itoa


  !> Resize a 2d array so that its indices cover
  !! (lo1:up1,lo2:up2). The input array is reallocated only if
  !! needeed.
  !!
  !! @param a array to be resized
  !! @param lo1
  !! @param lo2
  !! @param up1
  !! @param up2
  !!
  !! @return
  subroutine realloc_r2lu(a, lo1, lo2, up1, up2)
    real, allocatable :: a(:,:)
    integer, intent(in) :: lo1, lo2, up1, up2

    integer :: lo1_, lo2_, up1_, up2_, i

    if(.not. allocated(a)) then
       allocate(a(lo1:up1,lo2:up2))
    else if( lo1 < lbound(a,1) .or. lo2 < lbound(a,2) .or.&
         &   up1 > ubound(a,1) .or. up2 > ubound(a,2) ) then
       lo1_ = min(lbound(a,1),lo1)
       lo2_ = min(lbound(a,2),lo2)
       up1_ = max(ubound(a,1),up1)
       up2_ = max(ubound(a,2),up2)
       deallocate(a)
       allocate(a(lo1_:up2_,lo2_:up2_))
    end if

  end subroutine realloc_r2lu


  subroutine print_vec2d(a)
    real, intent(in) :: a(:,:)

    integer :: i

    do i = lbound(a,2), ubound(a,2)
       print '(*(f6.3,"  "))', a(:,i)
    end do
    print *, ""

  end subroutine print_vec2d


  !! @todo create a print method for coordinates
  subroutine print_vec1d_as_2d(a, n)
    real, intent(in), target :: a(:)
    integer, intent(in) :: n(2)

    integer :: i
    real, pointer :: b(:,:)
    b(1:n(1), 1:n(2)) => a

    call print_vec2d(b)

  end subroutine print_vec1d_as_2d

end module helper_module
