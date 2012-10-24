module logger_module

  use helper_module
  use iso_fortran_env
  use constants_module

  private

  integer, public, parameter :: FPDE_MSG_LEN  = 1000
  ! io units are taken from iso_fortran_env
  integer, public, parameter :: FPDE_STDOUT   = output_unit
  integer, public, parameter :: FPDE_STDIN    = input_unit
  integer, public, parameter :: FPDE_STDERR   = error_unit


  !< fpde logger log levels idendtifiers
  integer, public, parameter ::&
       FPDE_LOG_ERROR   = 1,&
       FPDE_LOG_WARNING = 2,&
       FPDE_LOG_INFO    = 3,&
       FPDE_LOG_DEBUG   = 4,&
       FODE_LOG_STOP    = 5

  character(len=1), parameter :: &
       lvl_stamp_array(5) = ["E", "W", "I", "D", "S"]

  type, public :: named
     integer, private :: logfile_unit = FPDE_STDOUT
     logical, private :: logging = .true.
     integer :: status = 0       !0 means OK!
     character(len=:), allocatable :: name !empty name should produce a warning
   contains
     procedure, non_overridable :: log
     procedure, non_overridable :: loge
     procedure, non_overridable :: logw
     procedure, non_overridable :: logi
     procedure, non_overridable :: logd

     !! @todo get_name() is intended to replace variable name as a
     !! function name() if name is not to be changed after creation
     !! of type(named)
     procedure, non_overridable :: get_name

     procedure, non_overridable :: log_newfile
     procedure, non_overridable :: log_clear
  end type named


  type :: logger_singleton
     integer :: log_level = FPDE_LOG_DEBUG
     integer :: msg_id = 1
     character(len=:), allocatable :: path !@todo what if this dir do not exists?
     ! character(len=FPDE_PATH_LEN) :: path = "log/" !@todo what if
     ! this dir do not exists?
  end type logger_singleton


  type(logger_singleton), save :: logger

  public :: set_log_level, log_newdir

contains

  function lvl_stamp(lvl)
    integer, intent(in) :: lvl
    character(len=1) :: lvl_stamp

    if( lvl <= size(lvl_stamp_array) .and. lvl >= 1) then
       lvl_stamp = lvl_stamp_array(lvl)
    else
       lvl_stamp = "?"
    end if
  end function lvl_stamp


  function get_name(self)
    class(named) :: self
    character(len=:), allocatable :: get_name

    if( allocated(self%name) ) then
       get_name = self%name
    else
       get_name = "unnamed"
    end if
  end function get_name


  subroutine set_log_level(lvl)
    integer :: lvl
    ! if lvl < 1 than log_level = 1,
    ! if lvl > size(lvl_stamp) then log_level = size(lvl_stamp)
    ! otherwise log_level = lvl
    logger%log_level = min( max(lvl, 1), size(lvl_stamp_array) )
  end subroutine set_log_level


  subroutine log_newdir(dirname, error)
    character(len=*), intent(in) :: dirname
    integer, intent(out), optional :: error

    integer :: err

    if(present(error)) error = FPDE_STATUS_ERROR

    call mkdir(dirname, error = err)
    if( err /= FPDE_STATUS_OK ) return

    if(present(error)) error = FPDE_STATUS_OK
    logger%path = dirname
  end subroutine log_newdir


  subroutine loge(self,msg)
    class(named) :: self
    character(len=*), intent(in) :: msg
    call self%log(FPDE_LOG_ERROR,msg)
  end subroutine loge


  subroutine logw(self,msg)
    class(named) :: self
    character(len=*), intent(in) :: msg
    call self%log(FPDE_LOG_WARNING,msg)
  end subroutine logw


  subroutine logi(self,msg)
    class(named) :: self
    character(len=*), intent(in) :: msg
    call self%log(FPDE_LOG_INFO,msg)
  end subroutine logi


  subroutine logd(self,msg)
    class(named) :: self
    character(len=*), intent(in) :: msg
    call self%log(FPDE_LOG_DEBUG,msg)
  end subroutine logd


  subroutine log(self, lvl, msg)
    class(named) :: self
    character(len=*), intent(in) :: msg
    integer, intent(in) :: lvl

    integer :: err
    character(len=FPDE_MSG_LEN) :: text

    ! non-writable unit, return immediately
    if( .not. self%logging ) return

    ! if lvl is higher than maximum, ignore this call and return
    if( logger%log_level < lvl ) return

    write( text, '("[",i6,"] (",A1,")",A15,": ",A)' )&
         logger%msg_id,&
         lvl_stamp(lvl),&
         self%get_name(),&
         msg

    ! count all calls to log which fit the logging level
    logger%msg_id = logger%msg_id + 1

    ! try to write to a unit
    call try_write( self%logfile_unit, text, error = err)
    if( err /= FPDE_STATUS_OK ) then
       call try_write(FPDE_STDERR, "Unable to use unit "//itoa(self%logfile_unit))
       self%logging = .false.
    end if

  end subroutine log


  subroutine try_write(unit, text, error)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: text
    integer, intent(out), optional :: error

    logical :: o
    integer :: ios
    character(len=7) :: w

    if( present(error) ) error = FPDE_STATUS_ERROR

    ! determine if the unit is writable
    inquire( unit, write = w, opened = o )
    if( .not. o .or. w /= 'YES' ) return

    if( unit == FPDE_STDERR ) then
       write (unit, *, iostat = ios) trim("-- Error -- "//text)
    else
       write (unit, *, iostat = ios) trim(text)
    end if

    if( ios /= 0 ) return

    if( present(error) ) error = FPDE_STATUS_OK

  end subroutine try_write


  subroutine log_clear(self)
    class(named) :: self

    logical :: opened

    inquire( unit = self%logfile_unit, opened = opened )
    if( opened ) then
       open( unit = self%logfile_unit, status = 'replace' )
    end if

  end subroutine log_clear


  subroutine log_newfile(self, filename, error)
    class(named) :: self
    character(len=*), optional :: filename
    integer, optional, intent(out) :: error

    character(len=:), allocatable :: fn
    character(len=7) :: w
    integer :: iostat, u
    logical :: opened

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( .not. present(filename) ) then
       fn = "default" // ".log"
    else
       fn = filename
    end if

    ! prepend the global log path (if it exists)
    if( allocated(logger%path) ) then
       fn = logger%path // fn
    end if

    ! check if the file with such name is already open and writable
    inquire(file = fn, number = u, write = w, opened = opened)

    if( opened .and. w == "YES") then
       ! if file is writable and open, assign its unit to self
       self%logfile_unit = u
       if(present(error)) error = FPDE_STATUS_OK

    else
       ! otherwise, try to open a file
       open(newunit = self%logfile_unit,&
            file = fn,&
            action = 'write',&
            recl = 10000,&
            iostat = iostat,&
            status = 'replace')

       ! and check for errors after opening it
       if( iostat /= 0  ) then
          call try_write(FPDE_STDERR, "Unable to open file ["//fn//"]")
          self%logging = .false.
          return
       end if
    end if

    ! if a unit is working resume logging
    self%logging = .true.

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine log_newfile


end module logger_module
