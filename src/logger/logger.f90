module logger_module

  use constants_module

  private

  integer, public, parameter :: _NAME_LEN = 100
  integer, public, parameter :: _PATH_LEN = 1000
  integer, public, parameter :: _STDOUT = 5


  type, public :: named
     integer :: logfile_unit = 0 !if 0, log will write to logger%unit
     integer :: status = 0       !0 means OK!
     character(len=_NAME_LEN) :: name = "" !empty name should produce a warning
   contains
     procedure :: init
     procedure :: free
     procedure, non_overridable :: log
  end type named


  type, private :: logger_singleton
     integer :: log_level = 0
     character(len=_PATH_LEN) :: path = "log/"
   contains
     procedure :: try_write
  end type logger_singleton


  type(logger_singleton), private, parameter :: logger = logger_singleton(log_level=1, path=".")

  public :: get_new_logfile_unit


contains

  subroutine log(n, lvl, msg)
    class(named), pointer :: n
    integer :: lvl, status
    character(len=*) :: msg
    character(len=_MSG_LEN) :: text
    character(len=10) :: lvl_text

    if( logger%log_level < lvl ) return

    select case(lvl)
    case (1)
       lvl_text = "ERROR"
    case (2)
       lvl_text = "WARNING"
    case (3)
       lvl_text = "INFO"
    case (4)
       lvl_text = "DEBUG"
    case default
       lvl_text = "UNDEFINED"
    end select

    write(text,*) trim(lvl_text) // ": " // msg

    call logger%try_write( n%logfile_unit, text, status )

    if( status /= _STATUS_OK ) then
       n%logfile_unit = _STDOUT
    end if

  end subroutine log


  subroutine try_write(l, unit, text, status)
    type(logger_singleton) :: l
    integer :: unit
    integer :: iostat, opened, status
    character(len=*) :: text
    character(len=_PATH_LEN) :: name

    inquire(unit = unit, opened = opened, named = name )

    if( opened /=0 ) then
       print *, "ERROR: Logger was unable to write to a file named ", name
       print *, text
       status = _STATUS_ERROR
    else
       write( unit, text, iostat=iostat )
       if( iostat /= 0  ) then
          print *, "ERROR: Logger was unable to write to a file, iostat=", iostat
          print *, text
          status = _STATUS_ERROR
       end if
    end if

  end subroutine try_write


  subroutine get_new_logfile_unit(unit, filename)
    integer :: unit
    character(len=PATH_LEN), optional :: filename
    character(len=19) :: timestamp
    integer :: iostat


    if(.not. present(filename)) then
       get_timestamp(timestamp)
       write(filename, *) logger%path // timestamp // ".log"
    end if

    open(newunit = unit,&
         file    = filename,  &
         form    = 'formatted', &
         action  = 'write', &
         ! access = 'direct', &
         recl    = 10000, &
         iostat = iostat, &
         status  = 'replace')

    if( iostat /= 0  ) then
       print *, "ERROR: in logger: get_new_logfile_unit ioostat =", iostat, ". Fallback to STDOUT"
       unit = STDOUT
       return
    end if

    if( unit == 0) then
       print *, "ERROR: in logger: get_new_logfile_unit unit =", unit, ". Fallback to STDOUT"
       unit = STDOUT
       return
    end if

  end subroutine get_new_logfile_unit


  subroutine get_timestamp(timestamp)
    character(len=19) :: timestamp
    character(len=8) :: date
    character(len=10) :: time

    call date_and_time(date=date, time=time)

    write(timestamp, *) trim(date), "-", trim(time)

  end subroutine get_timestamp


end module logger_module
