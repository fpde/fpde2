module icicles_creator_module

  use constants_module
  use icicles_module
  use logger_module

  integer, parameter, private :: MAX_REG = 1000

  type :: register
     integer :: len = 1
     character(len=NAME_LEN) :: name = ""
     logical :: evolved = .false.
  end type register


  type, public, extends(named) :: icicles_creator
     type(register) :: reg(MAX_REG)
     integer :: n_reg = 0
   contains
     procedure :: add
     procedure :: create_icicles
  end type icicles_creator


contains

  subroutine add(ic, name, evolved, len)
    class(icicles_creator) :: ic
    character(len=*) :: name
    logical, optional :: evolved
    integer, optional :: len

    integer :: n
    type(register) :: reg

    n = ic%n_reg

    if(present(evolved)) then
       reg%evolved=evolved
    end if

    if(present(len)) then
       reg%len=len
    end if

    ic%reg(n+1) = reg
    ic%n_reg = n+1

    call ic%log(FPDE_LOG_DEBUG, "Registered vector [" // trim(name) // "]")

  end subroutine add

  subroutine create_icicles(ic, ics)
    class(icicles_creator), target :: ic
    class(icicles), pointer, intent(out) :: ics

    integer :: n_reg,i,l
    integer :: len = 0, ev_len = 0, v_len = 0, s_len = 0
    class(register), pointer :: reg
    n_reg = ic%n_reg

    ! @todo reduce the loop using sum()
    do i = 1, n_reg
       reg => ic%reg(i)
       len = len + reg%len
       if(reg%evolved) then
          ev_len=ev_len+reg%len
       end if
       if(reg%len == 1) then
          s_len = s_len + 1
       else
          v_len = v_len + 1
       end if
    end do

    allocate(ics)
    allocate(ics%data(len))
    allocate(ics%vectors(v_len))
    allocate(ics%scalars(s_len))
    ics%evolved(1:ev_len) => ics%data(len-ev_len:len)

    ! those now point to the next not initialized element of vectors
    ! and scalars respectively
    v_len = 1
    s_len = 1
    ! this points to the nearest not assigned space in ics%evolved
    ev_len = 1
    ! this points to the nearest not assigned space in ics%data
    len = 1

    do i = 1, n_reg
       reg => ic%reg(i)

       if(reg%evolved .and. reg%len == 1) then
          ! evolved scalar
          ics%scalars(s_len)%val => ics%evolved(ev_len)
          ics%scalars(s_len)%name = reg%name
          ev_len=ev_len+1       !scalar, one field forward
          s_len=s_len+1
          call ic%log(FPDE_LOG_DEBUG,&
               "Added evolved scalar ["//trim(reg%name)//"]")

       else if(reg%evolved) then
          ! evolved vector
          l = reg%len
          ics%vectors(v_len)%val(1:l) => ics%evolved(ev_len:ev_len+l)
          ics%vectors(v_len)%name = reg%name
          len=len+l             !vector, l fields forward
          v_len=v_len+1
          call ic%log(FPDE_LOG_DEBUG,&
               "Added evolved vector ["//trim(reg%name)//"]")

       else if(reg%len == 1) then
          ! scalar
          ics%scalars(s_len)%val => ics%data(len)
          ics%scalars(s_len)%name = reg%name
          len=len+1             !scalar, one field forward
          s_len=s_len+1
          call ic%log(FPDE_LOG_DEBUG,&
               "Added evolved scalar ["//trim(reg%name)//"]")
       else
          ! vector
       end if
    end do



  end subroutine create_icicles


end module icicles_creator_module
