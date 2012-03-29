module flu_get_module

  use flu_module
  use constants_module

contains

  subroutine flu_get_scalar_integer( l, i, key, index, error )
    type(flu) :: l
    character(len=*) :: key
    integer, intent(out) :: i
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index

    integer :: idx

    if(present(error)) error = FPDE_STATUS_ERROR

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    if( idx == 0 ) then
       call lua_getglobal( l, key )
    else
       call lua_getfield( l, idx, key )
    end if

    if( lua_type(l, -1) == C_LUA_TNUMBER) then
       i = lua_tointeger(l, -1)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, should be number ["//trim(key)//"]")
    end if

    call lua_pop(l,1)

  end subroutine flu_get_scalar_integer


  subroutine flu_get_scalar_character( l, str, key, index, error )
    type(flu) :: l
    character(len=*), intent(out) :: str
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index

    integer :: idx

    if(present(error)) error = FPDE_STATUS_ERROR

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    if( idx == 0 ) then
       call lua_getglobal( l, key )
    else
       call lua_getfield( l, idx, key )
    end if

    if( lua_type(l, -1) == C_LUA_TSTRING) then
       call lua_tostring(l, -1, str)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, should be string ["//trim(key)//"]")
    end if

    call lua_pop(l,1)

  end subroutine flu_get_scalar_character


end module flu_get_module
