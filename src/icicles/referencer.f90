module class_icicles_referencer

  use constants_module
  use logger_module
  use class_platonic
  use class_named_vector

  private

  type, public, extends(platonic) :: icicles_referencer
     private
     type(named_vector), pointer :: nv => null()
     real, pointer :: last(:) => null()
   contains
     procedure :: init
     procedure :: set_nv
     procedure :: set_to
     procedure :: restore
     procedure :: get_val
     procedure :: get_name
  end type icicles_referencer

contains

  !!!!!!!!!!!!!!!!!!!! icicles referencer starts !!!

  subroutine init(p, error)
    class(icicles_referencer), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Icicles referencer"
  end subroutine init


  subroutine set_nv(self, nv)
    class(icicles_referencer) :: self
    class(named_vector), intent(in), target :: nv
    self%nv => nv
  end subroutine set_nv


  !>
  !! Backdoor to setting pointers of icicles entries.
  !!
  !! Can possibly change the length of the icicles entry to which it
  !! is connected.
  !!
  !! @param self
  !! @param dest
  !! @param error
  !!
  recursive subroutine set_to(self, dest, error)
    class(icicles_referencer) :: self
    real, target :: dest(:)
    integer, optional, intent(out) :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_OK

    if( .not. associated(self%nv) ) then
       call self%log(FPDE_LOG_WARNING,&
            "set_to(): Referencer is empty.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! !! @todo remove this error?
    ! if( .not. allocated(self%nv%name) ) then
    !    call self%log(FPDE_LOG_WARNING,&
    !         "set_to(): Referenced vector has no name, probably broken.")
    !    if(present(error)) error = FPDE_STATUS_ERROR
    !    return
    ! end if

    self%last    => self%nv%val
    self%nv%val  => dest

  end subroutine set_to


  !! @todo log error
  subroutine restore(self)
    class(icicles_referencer) :: self

    integer :: i

    if( .not. associated(self%nv) ) then
       return
    end if

    self%nv%val => self%last

  end subroutine restore


  function get_val(self) result(vec)
    class(icicles_referencer) :: self
    real, pointer :: vec(:)

    !! @todo log error
    if( .not. associated(self%nv) ) then
       return
    end if

    vec => self%nv%val
  end function get_val


  function get_name(self, error) result(name)
    class(icicles_referencer) :: self
    character(len=:), allocatable :: name
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    name = ""

    if( .not. associated(self%nv) ) then
       call self%log(FPDE_LOG_ERROR,&
            "get_name(): Reference not associated.")
       return
    end if

    if( .not. allocated(self%nv%name) ) then
       call self%log(FPDE_LOG_ERROR,&
            "get_name(): Referenced vector has no name.")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    name = self%nv%name

  end function get_name

!!!!!!!!!!!!!!!!!!!! icicles referencer ends !!!!

end module class_icicles_referencer
