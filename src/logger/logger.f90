module logger_module

  use constants_module

  private

  integer, public, parameter :: FPDE_PATH_LEN = 1000
  integer, public, parameter :: FPDE_MSG_LEN  = 1000
  ! Fortran and Unix file descriptors are equivalent
  integer, public, parameter :: FPDE_STDOUT   = 6
  integer, public, parameter :: FPDE_STDIN    = 5
  integer, public, parameter :: FPDE_STDERR   = 0


  !< fpde logger log levels idendtifiers
  integer, public, parameter ::&
       FPDE_LOG_ERROR   = 1,&
       FPDE_LOG_WARNING = 2,&
       FPDE_LOG_INFO    = 3,&
       FPDE_LOG_DEBUG   = 4


  type, public :: named
     integer :: logfile_unit = FPDE_STDOUT !if 0, log will write to logger%unit
     integer :: status = 0       !0 means OK!
     character(len=NAME_LEN) :: name = "" !empty name should produce a warning
   contains
     procedure, non_overridable :: log
  end type named


  type, private :: logger_singleton
     integer :: log_level = 1
     integer :: msg_id = 1
     character(len=FPDE_PATH_LEN) :: path = "log/" !@todo what if this dir do not exists?
   contains
     procedure :: try_write
  end type logger_singleton


  type(logger_singleton), public, save :: logger


  public :: get_new_logfile_unit, set_log_level


contains


   subroutine set_log_level(lvl)
      integer :: lvl
      logger%log_level = lvl
   end subroutine set_log_level


   subroutine log(n, lvl, msg)
      class(named) :: n
      integer :: lvl, status=FPDE_STATUS_OK
      character(len=*) :: msg
      character(len=FPDE_MSG_LEN) :: text
      character(len=10) :: lvl_text
      character(len=19) :: timestamp

      ! @todo this if block is strange, since one might
      ! to have logs in stderr
      if ( n%logfile_unit == 0 ) then
         call get_new_logfile_unit( n%logfile_unit )
      end if

      if( logger%log_level < lvl ) return

      select case(lvl)
      case (FPDE_LOG_ERROR)
         lvl_text = "E"
      case (FPDE_LOG_WARNING)
         lvl_text = "W"
      case (FPDE_LOG_INFO)
         lvl_text = "I"
      case (FPDE_LOG_DEBUG)
         lvl_text = "D"
      case default
         lvl_text = "?"
      end select

      call get_timestamp(timestamp)

      write(text,'("[",i3,"] ",A15,"[",A1,"] ",A)')&
           logger%msg_id,&
           n%name,&
           lvl_text,&
           msg

      logger%msg_id = logger%msg_id + 1

      call logger%try_write( n%logfile_unit, trim(text), status )

      if( status /= FPDE_STATUS_OK ) then
         n%logfile_unit = FPDE_STDOUT
      end if

   end subroutine log


   subroutine try_write(l, unit, text, status)
      class(logger_singleton) :: l
      integer :: unit
      integer :: iostat, status
      logical :: opened
      character(len=*) :: text
      character(len=FPDE_PATH_LEN) :: name

      inquire(unit = unit, opened = opened, name = name )

      if( .not. opened ) then
         print *, "ERROR: try_write was unable to write to a file named ", trim(name)
         print *, text
         status = FPDE_STATUS_ERROR
      else
         write( unit, *, iostat=iostat ) trim(text)
         if( iostat /= 0  ) then
            print *, "ERROR: try_write was unable to write to a file iostat=", iostat
            print *, text
            status = FPDE_STATUS_ERROR
         end if
      end if

   end subroutine try_write


   subroutine get_new_logfile_unit(unit, fn)
      integer :: unit
      character(len=FPDE_PATH_LEN) :: filename
      character(len=*), optional :: fn
      character(len=19) :: timestamp
      integer :: iostat


      if(.not. present(fn)) then
         call get_timestamp(timestamp)
         write(filename, '(A)') trim(logger%path) // timestamp // ".log"
      else
         write(filename, '(A)') fn
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
         unit = FPDE_STDOUT
         return
      end if

      if( unit == 0) then
         print *, "ERROR: in logger: get_new_logfile_unit unit =", unit, ". Fallback to STDOUT"
         unit = FPDE_STDOUT
         return
      end if

   end subroutine get_new_logfile_unit


   subroutine get_timestamp(timestamp)
      character(len=19) :: timestamp
      character(len=8) :: date
      character(len=10) :: time

      call date_and_time(date=date, time=time)

      write(timestamp, '(3(A))') date, "-", time

   end subroutine get_timestamp


end module logger_module
