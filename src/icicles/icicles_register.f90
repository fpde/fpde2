module icicles_register_module

  use constants_module
  use icicles_module
  use logger_module

  !> maximal number of registry entries
  integer, parameter, private :: MAX_ENTRIES = 1000

  private

  type :: icicles_register_entry
     !> length of register, defaults to 1
     integer :: len = 0
     !> name of register, defaults to an empty string
     character(len=NAME_LEN) :: name = ""
  end type icicles_register_entry

  !> regenerates the pointers of all named_vectors and named_scalars
  !! in icicles based on the icicles_register
  type, public, extends(named) :: icicles_register
     !> register table
     type(icicles_register_entry) :: entries(MAX_ENTRIES)
     !> number of registers already added
     integer :: n_entries = 0
   contains
     procedure :: add
     procedure :: create_icicles
     procedure :: set_pointers
     procedure :: info
  end type icicles_register

contains

  !> Adds a single entry to icicles_register.
  !!
  !! @param ic icicles_register
  !! @param name name of entry
  !! @param len length of entry, len=1 corresponds to scalars,
  !!         while len>1 corresponds to vectors
  !!
  subroutine add(reg, name, len, error)
    class(icicles_register) :: reg
    character(len=*) :: name
    integer, optional :: len
    integer, optional, intent(out) :: error

    integer :: n
    type(icicles_register_entry) :: entr

    n = reg%n_entries
    if( n == MAX_ENTRIES ) then
       call reg%log(FPDE_LOG_ERROR, "icicles_register is full, change MAX_ENTRIES")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( any(reg%entries(1:n)%name == name ) ) then
       call reg%log(FPDE_LOG_ERROR,&
            "Adding a duplicate entry to icicles_register")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if


    entr%name = trim(name)

    ! if len is not present it is set to 1
    if(present(len)) then
       entr%len=len
    else
       entr%len=1
    end if

    reg%entries(n+1) = entr
    reg%n_entries = n+1

    call reg%log(FPDE_LOG_DEBUG, "Registered vector [" // trim(name) // "]")
    if(present(error)) error = FPDE_STATUS_OK

  end subroutine add


  !> @brief initializes pointers inside icicles' vectors and scalars
  !! to point at the content of @vec
  !!
  !! @param reg initialized icicles
  !!
  !! @param reg_range optional array of indexes of
  !!         icicles_register%entries entries deisgnated to point to
  !!         new location.  If @reg_range is not present all registry
  !!         entries will be used
  !!
  !! @param vec allocated 1d vector
  !!
  !! You can also use set_pointers with custom made icicles_register
  !! as long as names, lengths and order of common entries are the
  !! same. You can create registers with evolved data, time
  !! derivatives etc. and use set_pointers with icicles and various
  !! registers to set appropriate pointers accordingly.
  !!
  subroutine set_pointers(reg, ic, vec, entr_range, error)
    class(icicles_register), target :: reg
    type(icicles), pointer :: ic
    type(icicles_register_entry), pointer :: entr
    integer, intent(in), optional, target :: entr_range(:)
    real, target, intent(in) :: vec(:)
    integer, optional, intent(out) :: error

    integer, pointer :: rng(:)
    ! temporary static array used if entr_range is not present
    integer, target :: temp(reg%n_entries)
    type(named_vector), pointer :: v
    type(named_scalar), pointer :: s
    integer :: len, i, j, l, v_err, s_err

    if( present(entr_range) ) then
       ! use entr_range if present
       rng => entr_range
    else
       ! else use temp initialized to contain all integers from 1 to
       ! reg%n_entries
       temp = [ (i, i=1, reg%n_entries) ]
       rng => temp
    end if

    ! this points to the nearest not assigned space in vec
    len = 1

    do i = 1, size(rng)
       j = rng(i)

       if( j > reg%n_entries .or. j < 1 ) then
          call reg%log(FPDE_LOG_ERROR, &
               "Attempt to use set_pointers with negative index")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if

       entr => reg%entries(j)
       l = entr%len

       ! if we exceed the size of vec return error, and leave already
       ! associated vectors
       if( len+l-1 > size(vec) ) then
          call reg%log(FPDE_LOG_ERROR, &
               "Attempt to run set_pointers with array size too small")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if

       call ic%get( entr%name, v, error = v_err )
       call ic%get( entr%name, s, error = s_err )

       ! vector case
       if( v_err == FPDE_STATUS_OK ) then
          v%val(1:l) => vec(len:len + l-1)
          len = len + l

       ! scalar case
       else if( s_err == FPDE_STATUS_OK ) then
          s%val => vec(len)
          len = len + 1

       else
          call reg%log(FPDE_LOG_ERROR, &
               "Icicles does not have a scalar/vector&
               & with name ["//trim(entr%name)//"]")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if

    end do

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine set_pointers


  !> Creates icicles from icicles_register, the pointers in
  !! icicles%vectors and icicles%scalars are initialized according to
  !! the order of entries in icicles_register
  !!
  !! @param ics null pointer to the icicles to be created
  !!
  subroutine create_icicles(reg, ics, error)
    class(icicles_register), target :: reg
    type(icicles), pointer, intent(out) :: ics
    integer, optional, intent(out) :: error

    integer :: n_entries,i,l
    integer, pointer :: len(:)
    integer :: v_len = 0, s_len = 0
    class(icicles_register_entry), pointer :: entr

    ! we do not want to toy with already associated icicles
    if( associated(ics) ) then
       call reg%log(FPDE_LOG_WARNING,&
            "Attempt to pass an associated icicles poionter to&
            & create_icicles, icicles will not be recreated")
       if( present(error) ) error = FPDE_STATUS_ERROR
       return
    end if

    n_entries = reg%n_entries
    len => reg%entries(1:n_entries)%len

    ! count entries of len=1
    s_len=count(len==1)
    ! total of lengths of entries with len >1
    v_len=count(len>1)

    allocate(ics)
    allocate(ics%data(sum(len)))
    allocate(ics%vectors(v_len))
    allocate(ics%scalars(s_len))

    ! reset local variables to serve different purpose
    s_len = 1
    v_len = 1

    do i = 1, n_entries
       entr => reg%entries(i)
       if( entr%len == 1) then
          ics%scalars(s_len)%name = trim(entr%name)
          s_len = s_len+1
       else
          ics%vectors(v_len)%name = trim(entr%name)
          v_len = v_len+1
       end if
    end do

    ! set pointers in icicles to their default locations in ics%data
    call reg%set_pointers(ics, ics%data)

  end subroutine create_icicles


  subroutine info(reg)
    class(icicles_register) :: reg

    integer :: i

    do i = 1, reg%n_entries
       print *, i, trim(reg%entries(i)%name), reg%entries(i)%len
    end do

  end subroutine info


end module icicles_register_module
