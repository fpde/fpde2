module class_event_true

  use class_event
  use constants_module
  use class_icicles_user

  private

  type, public, extends(event) :: event_true
     private
     logical :: val = .true.
   contains
     procedure :: test
  end type event_true

  interface event_true
     module procedure et_new
     module procedure et_new_lua
  end interface event_true

contains

  function et_new(val) result(r)
    logical, optional, intent(in) :: val
    type(event_true), pointer :: r

    allocate(r)

    r%name = "Event: T/F"
    if(present(val)) r%val = val

    if(r%val) then
       call r%logd("Construction complete, [val]=True")
    else
       call r%logd("Construction complete, [val]=False")
    end if

  end function et_new


  function et_new_lua(l, index, error) result(r)
    use flu_module
    use flu_get_module

    type(flu) :: l
    integer, intent(in) :: index
    integer, optional, intent(out) :: error
    type(event_true), pointer :: r

    integer :: err
    logical :: val

    if(present(error)) error = FPDE_STATUS_OK

    val = .true.

    call lua_getfield(l,index,"val")     ! val
    call flu_get(l, -1, val, err)        ! val
    call lua_pop(l,1)                    !

    if( err == FPDE_STATUS_OK ) then
       r => et_new(val)
    else
       if(present(error)) error = FPDE_STATUS_ERROR
       r => et_new()
       call r%loge("Error reading [val] from Lua")
    end if

    call r%logd("Read input from Lua!")

  end function et_new_lua


  function test(self, ic, error) result(r)
    class(event_true), intent(inout) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    logical :: r
    if(present(error)) error = FPDE_STATUS_OK
    r = self%val
  end function test

end module class_event_true
