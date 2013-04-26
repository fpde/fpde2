module class_event_logical_base

  use class_event
  use class_icicles_user
  use constants_module

  private

  type, public, abstract, extends(event) :: event_logical_base
     class(event), pointer :: op1 => null()
     class(event), pointer :: op2 => null()
   contains
     procedure :: start
     procedure :: stop
     ! procedure :: test
  end type event_logical_base

contains

  subroutine start(self, ic, error)
    class(event_logical_base), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    integer :: err1, err2

    err1 = FPDE_STATUS_OK
    err2 = FPDE_STATUS_OK
    if(present(error)) error = FPDE_STATUS_OK

    if(associated(self%op1)) call self%op1%start(ic, err1)
    if(associated(self%op2)) call self%op2%start(ic, err2)

    if( err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
    end if

  end subroutine start

  subroutine stop(self, ic, error)
    class(event_logical_base), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error

    integer :: err1, err2

    err1 = FPDE_STATUS_OK
    err2 = FPDE_STATUS_OK
    if(present(error)) error = FPDE_STATUS_OK

    if(associated(self%op1)) call self%op1%stop(ic, err1)
    if(associated(self%op2)) call self%op2%stop(ic, err2)

    if( err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
    end if

  end subroutine stop

end module class_event_logical_base
