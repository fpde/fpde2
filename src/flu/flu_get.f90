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
  subroutine flu_get_scalar_integer( l, i, key, index, default, error )
    type(flu) :: l
    integer, intent(out) :: i
    character(len=*), optional :: key
    integer, optional, intent(in) :: index
    integer, optional, intent(in) :: default
    integer, optional, intent(out) :: error

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
          call lua_pop(l,1)
          return
       else
          call l%log(FPDE_LOG_ERROR,&
               "Invalid type, should be number ["//trim(key)//"]")
       end if
    end if

    call lua_pop(l,1)

    if(present(default)) i = default

  end subroutine flu_get_scalar_integer


  !> analogous to flu_get_scalar_integer()
  !! @param[out] str returned integer value
  subroutine flu_get_scalar_character( l, str, key, index, default, error )
    type(flu) :: l
    character(len=*), intent(out) :: str
    character(len=*), optional, intent(in) :: default
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
          call lua_pop(l,1)
          return
       else
          call l%log(FPDE_LOG_ERROR,&
               "Invalid type, should be string ["//trim(key)//"]")
       end if
    end if

    call lua_pop(l,1)

    if(present(default)) str = default

  end subroutine flu_get_scalar_character

  !> Tries to fill in a fortran table of type real from Lua. All
  !! parameters are analogous to flu_get_scalar_integer.
  subroutine flu_get_table_real( l, table, key, index, default, error )
    type(flu) :: l
    character(len=*) :: key
    real, intent(out) :: table(:)
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index
    real, optional, intent(in) :: default

    integer :: idx, i, err, n

    ! default to error
    if(present(error)) error = FPDE_STATUS_ERROR
    err = FPDE_STATUS_OK

    ! assign a default value
    if(present(default)) table = default

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    call flu_get(l, idx, key, error = err)

    ! check if flu_get succeeded, if not report no error, flu_get
    ! should report it on its own this.
    if( err /= FPDE_STATUS_OK ) then !ignore the rest of the function
                                     !code

    ! check if the type of result of flu_get is a lua table
    else if( lua_type(l, -1) /= C_LUA_TTABLE ) then
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type of ["//trim(key)//"], table expected")

    ! check if fortran table is large enough
    else if( lua_len(l,-1) > size(table) ) then
       call l%log(FPDE_LOG_ERROR,&
            "Lua table ["//trim(key)//"] is larger than fortran table")
       ! pop the length off the stack
       call lua_pop(l,-1)

    ! if no errors, then...
    else
       ! if we got here, the last result on the stack comes from
       ! lua_len, so write it to n and pop it
       n = lua_tointeger(l,-1)
       call lua_pop(l,1)

       ! copy lua table to fortran table value-by-value
       do i = 1, n

          ! get i-th element from lua table
          call lua_pushinteger(l,i)
          call lua_gettable(l,-2)

          ! check if the i-th element of lua table is a number
          if( lua_type(l, -1) == C_LUA_TNUMBER ) then
             ! assign a value from lua table to i-th element of
             ! fortran table
             table(i) = lua_tointeger(l, -1)
          else
             ! switch the error flag
             err = FPDE_STATUS_ERROR
             ! report an error
             call l%log(FPDE_LOG_ERROR,&
                  "Invalid type of element of &
                  &["//trim(key)//"], number expected")
          end if

          ! pop the i-th element from the lua stack
          call lua_pop(l,1)

       end do

    end if

    if(present(error)) error = err

    ! pop the result of flu_get
    call lua_pop(l,1)

  end subroutine flu_get_table_real


  subroutine flu_get_table_integer( l, table, key, index, default, error )
    type(flu) :: l
    character(len=*) :: key
    integer, intent(out) :: table(:)
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index
    integer, optional, intent(in) :: default

    real :: temp(size(table)), def
    integer :: idx, err

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    ! call the get_table_real version
    if(present(default)) then
       call flu_get_table_real( l,&
            temp, key, idx, default = real(default), error = err )
    else
       call flu_get_table_real( l,&
            temp, key, idx, error = err )
    end if

    ! cast the result to int
    table = int(temp)

    if(present(error)) error = err

  end subroutine flu_get_table_integer

end module flu_get_module
