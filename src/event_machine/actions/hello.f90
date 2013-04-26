module class_action_hello

  use class_action
  use class_icicles_user
  use constants_module

  private

  type, public, extends(action) :: action_hello
     private
     character(len=:), allocatable :: text
     contains
       procedure :: start
       procedure :: execute
       procedure :: stop
  end type action_hello

  interface action_hello
     module procedure :: ah_new
     module procedure :: ah_new_lua
  end interface action_hello

contains

  function ah_new(text) result(r)
    character(len=*), optional, intent(in) :: text
    type(action_hello), pointer :: r

    allocate(r)

    r%name = "Action: Hello world!"

    if(present(text)) then
       r%text = text
    else
       r%text = "Hello!"
    end if

    call r%logd("Construction complete, [text]="//r%text)

  end function ah_new


  function ah_new_lua(l, index, error) result(r)
    use flu_module
    use flu_get_module

    type(flu) :: l
    integer, intent(in) :: index
    integer, optional, intent(out) :: error
    type(action_hello), pointer :: r

    character(len=100) :: text
    integer :: err

    if(present(error)) error = FPDE_STATUS_OK

    call lua_getfield(l, index, "text") !text
    call flu_get(l,-1,text)             !text
    call lua_pop(l,1)                   !

    r => ah_new(text)

    call r%logd("Read input from Lua!")

  end function ah_new_lua


  subroutine execute(self, ic, error)
    class(action_hello) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    print *, self%text
  end subroutine execute

  subroutine start(self, ic, error)
    class(action_hello) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    print *,   "Start: ", self%text
  end subroutine start

  subroutine stop(self, ic, error)
    class(action_hello) :: self
    class(icicles_user) :: ic
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    print *,   "Stop: ", self%text
  end subroutine stop

end module class_action_hello
