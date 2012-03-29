!>
!! @file   flu_get.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Thu Mar 29 19:12:30 2012
!!
!! @brief Higher level functions used to obtain Lua gloabal variables
!! and elements from Lua tables.
!!
module flu_get_module

  use flu_module
  use constants_module

contains

  !> Combines lua_getglobal and lua_getfield
  !!
  !! @param key name of field or global variable
  !!
  !! @param index if 0 then lua_getglobal(l,key) is called, otherwise
  !! it's lua_getfield(l,index,key)
  !!
  !! @param error FPDE_STATUS_ERROR if field was nil, FPDE_STATUS_OK otherwise
  subroutine flu_get(l, index, key, error)
    type(flu) :: l
    integer, intent(in) :: index
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( index == 0 ) then
       call lua_getglobal( l, key )
    else
       call lua_getfield( l, index, key )
    end if

    if( lua_isnil(l, -1) ) then
       call l%log(FPDE_LOG_WARNING,&
            "No field named ["//trim(key)//"]")
    else
       if(present(error)) error = FPDE_STATUS_OK
    end if

  end subroutine flu_get


  !> Tries to read an integer scalar variable from Lua state l
  !!
  !! @param l flu object
  !! @param[out] i returned integer value
  !! @param[in] key
  !! @param[in] index|-1 if present and = 0, then key is treated as a
  !! name of global variable
  !!
  !! @param[out] error if variable corresponding to key is not
  !!
  subroutine flu_get_scalar_integer( l, i, key, index, error )
    type(flu) :: l
    character(len=*) :: key
    integer, intent(out) :: i
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index

    integer :: idx, err

    if(present(error)) error = FPDE_STATUS_ERROR

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    call flu_get(l, idx, key, error = err)

    if( err == FPDE_STATUS_OK ) then
       if( lua_type(l, -1) == C_LUA_TNUMBER) then
          i = lua_tointeger(l, -1)
          if(present(error)) error = FPDE_STATUS_OK
       else
          call l%log(FPDE_LOG_ERROR,&
               "Invalid type, should be number ["//trim(key)//"]")
       end if
    end if

    call lua_pop(l,1)

  end subroutine flu_get_scalar_integer


  !> analogous to flu_get_scalar_integer()
  !! @param[out] str returned integer value
  subroutine flu_get_scalar_character( l, str, key, index, error )
    type(flu) :: l
    character(len=*), intent(out) :: str
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index

    integer :: idx, err

    if(present(error)) error = FPDE_STATUS_ERROR

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    call flu_get(l, idx, key, error = err)

    if( err == FPDE_STATUS_OK ) then
       if( lua_type(l, -1) == C_LUA_TSTRING) then
          call lua_tostring(l, -1, str)
          if(present(error)) error = FPDE_STATUS_OK
       else
          call l%log(FPDE_LOG_ERROR,&
               "Invalid type, should be string ["//trim(key)//"]")
       end if
    end if

    call lua_pop(l,1)

  end subroutine flu_get_scalar_character


end module flu_get_module
