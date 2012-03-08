module tentacle_module

  use constants_module
  use icicles_module
  use logger_module

  integer, parameter, private :: MAX_REG = 1000

  private

  type :: tentacle_register
     integer :: len = 1
     character(len=NAME_LEN) :: name = ""
  end type tentacle_register

  !> regenerates the pointers of all named_vectors and named_scalars
  !> in icicles based on the tentacle_register
  type, public, extends(named) :: magical_tentacle
     !> register table
     type(tentacle_register) :: reg(MAX_REG)
     !> number of registers already added
     integer :: n_reg = 0
   contains
     procedure :: add
     procedure :: create_icicles
     procedure :: set_pointers
  end type magical_tentacle

contains

  subroutine add(ic, name, len)
    class(magical_tentacle) :: ic
    character(len=*) :: name
    integer, optional :: len

    integer :: n
    type(tentacle_register) :: reg

    n = ic%n_reg
    if( n == MAX_REG ) then
       call ic%log(FPDE_LOG_ERROR, "Magical register is full")
       return
    end if

    reg%name = trim(name)

    ! default len is set in tentacle_register
    if(present(len)) then
       reg%len=len
    end if

    ic%reg(n+1) = reg
    ic%n_reg = n+1

    call ic%log(FPDE_LOG_DEBUG, "Registered vector [" // trim(name) // "]")

  end subroutine add

  subroutine set_pointers(ten, ic, reg_range, vec)
    class(magical_tentacle), target :: ten
    type(icicles), pointer :: ic
    type(tentacle_register), pointer :: reg
    integer, intent(in) :: reg_range(2)
    real, target, intent(in) :: vec(:)

    type(named_vector), pointer :: v
    type(named_scalar), pointer :: s
    integer :: len, i

    ! this points to the nearest not assigned space in ics%data
    len = 1

    do i = reg_range(1), reg_range(2)
       reg => ten%reg(i)

       if( len+reg%len-1 > size(vec) ) then
          ! @todo log error, too small vec
          return

       else if( ic%get(reg%name,v) == 0 ) then
          v%val(1:reg%len) => vec(len:reg%len+len-1)
          len = len + reg%len

       else if( ic%get(reg%name,s) == 0) then
          s%val => vec(len)
          len = len + 1
       end if

    end do
  end subroutine set_pointers


  subroutine create_icicles(ten, ics)
    class(magical_tentacle), target :: ten
    type(icicles), pointer, intent(out) :: ics

    integer :: n_reg,i,l
    integer :: len = 0, v_len = 0, s_len = 0
    class(tentacle_register), pointer :: reg
    n_reg = ten%n_reg

    ! @todo reduce the loop using sum()
    do i = 1, n_reg
       reg => ten%reg(i)
       len = len + reg%len
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

    s_len = 1
    v_len = 1

    do i = 1, n_reg
       reg => ten%reg(i)
       if( reg%len == 1) then
          ics%scalars(s_len)%name = trim(reg%name)
          s_len = s_len+1
       else
          ics%vectors(v_len)%name = trim(reg%name)
          v_len = v_len+1
       end if
    end do

    call ten%set_pointers(ics, [1,n_reg], ics%data)

  end subroutine create_icicles

end module tentacle_module
