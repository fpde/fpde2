!>
!! @file   flu_get.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Thu Mar 29 19:12:30 2012
!!
!! @brief Higher level functions used to obtain Lua gloabal variables
!! and elements from Lua tables.
!!
!! The implementation is quite tricky, and uses Fortran 2008
!! features. Some of them are not yet supported by ifort or the support
!! is buggy.
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

    else if( lua_type(l,index) == C_LUA_TTABLE ) then
       call lua_getfield( l, index, key )

    else
       call l%log(FPDE_LOG_INFO,&
            "There is no table at the stack index passed to flu_get")
       ! push nil to the stack
       call lua_pushnil( l )
       return

    end if

    ! if getglobal or getfield was called, check if the value is not
    ! nil, if it is the case, pop it, otherwise set error to OK
    if( lua_isnil(l, -1) ) then
       call l%log(FPDE_LOG_INFO,&
            "No global name or field ["//trim(key)//"] exists")
    else
       if(present(error)) error = FPDE_STATUS_OK
    end if

  end subroutine flu_get


  subroutine flu_get_atomic(l,  index, key, char, val, val1d, val2d, error)
    type(flu) :: l
    integer, optional, intent(in) :: index
    character(len=*), optional, intent(in) :: key
    integer, optional, intent(out) :: error

    class(*), intent(out), optional :: val
    class(*), intent(out), optional :: val1d(:)
    class(*), intent(out), optional :: val2d(:,:)
    character(len=*), intent(out), optional :: char
    integer :: err, idx
    logical :: pushed = .false.

    err = FPDE_STATUS_ERROR

    ! check if optional parameters are passed in right combination
    if( .not. (present(val)&
         .neqv. present(val1d)&
         .neqv. present(val2d)&
         .neqv. present(char)) ) then
       call l%log(FPDE_LOG_ERROR,&
            "Exactly one of char, val, val1d and val2d arguments is permited")
       if(present(error)) error = err
       return
    end if

    ! assign default value of index to idx
    if( present(index) ) then
       idx = index
    else
       idx = -1
    end if

    ! default to ok
    err = FPDE_STATUS_OK

    ! if key is present, then look for a global variable or a
    ! component of a table, and push it to the top of the stack
    if( present(key) ) then
       call flu_get(l,idx,key,err)
       idx = -1
    end if

    if(err == FPDE_STATUS_OK) then ! error from flu_get or ok

       !> @todo due to the bug in ifort we have to distinguish between
       ! characaters and noncharacters, 'char' argument should be deleted
       ! later when the bug is fixed
       if(present(char)) then
          call flu_get_scalar_character(l,idx,char,err)

       else if(present(val)) then
          call flu_get_scalar(l,idx,val,err)

       else if(present(val1d)) then
          call flu_get_array1d(l,idx,val1d,err)

       else if(present(val2d)) then
          call flu_get_array2d(l,idx,val2d,err)

       end if

    end if

    if(present(key)) call lua_pop(l,1)

    if(present(error)) error = err

  end subroutine flu_get_atomic


  subroutine flu_get_scalar_integer( l, index, i, error )
    type(flu) :: l
    integer, intent(out) :: i
    integer, intent(in) :: index
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TNUMBER) then
       i = lua_tointeger(l, index)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, number expected")
    end if

  end subroutine flu_get_scalar_integer


  subroutine flu_get_scalar_real( l, index, r, error )
    type(flu) :: l
    real, intent(out) :: r
    integer, intent(in) :: index
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TNUMBER) then
       r = lua_tonumber(l, index)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, number expected")
    end if

  end subroutine flu_get_scalar_real


  subroutine flu_get_scalar_logical( l, index, r, error )
    type(flu) :: l
    logical, intent(out) :: r
    integer, intent(in) :: index
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TBOOLEAN ) then
       r = lua_toboolean(l, index)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, number expected")
    end if

  end subroutine flu_get_scalar_logical


  subroutine flu_get_scalar_character( l, index,  str, error )
    type(flu) :: l
    integer, intent(in) :: index
    character(len=*), intent(out) :: str
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TSTRING) then
       call lua_tostring(l, index, str)
       if(present(error)) error = FPDE_STATUS_OK
       return
    else
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, should be string")
    end if

  end subroutine flu_get_scalar_character


  subroutine flu_get_scalar(l,index,val,error)
    type(flu) :: l
    integer, intent(in) :: index
    class(*), intent(out) :: val
    integer, optional, intent(out) :: error

    integer :: err

    ! assign a value from lua table to i-th element of
    ! fortran table
    select type(val)

       ! integer case
    type is(integer)
       call flu_get_scalar_integer(l, index, val, err)

       ! real case
    type is(real)
       call flu_get_scalar_real(l, index, val, err)

       ! logical case
    type is(logical)
       call flu_get_scalar_logical(l, index, val, err)

       ! character case
    type is(character(len=*))
       call flu_get_scalar_character(l, index, val, err)

    class default
       err = FPDE_STATUS_ERROR
       call l%log(FPDE_LOG_ERROR,&
            "Urecognized type passed to flu_get_scalar")

    end select

    if(present(error)) error = err

  end subroutine flu_get_scalar

  !> Tries to fill in a fortran table of type real from Lua. All
  !! parameters are analogous to flu_get_scalar_integer.
  subroutine flu_get_array1d( l, index, table, error )
    type(flu) :: l
    integer, intent(in) :: index
    class(*), intent(out) :: table(:)
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: tp = ""
    integer :: i, err, err2, n, idx

    ! its safer to operate on absolute index
    idx = lua_absindex(l,index)

    ! default to error
    err = FPDE_STATUS_ERROR

    if( lua_type(l, idx) /= C_LUA_TTABLE ) then
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, table expected")

       ! if no errors are present, then...
    else
       n = lua_rawlen(l, idx)

       ! default to ok
       err = FPDE_STATUS_OK

       ! check if fortran table is large enough
       if( n > size(table) ) then
          err = FPDE_STATUS_ERROR
          call l%log(FPDE_LOG_WARNING,&
               "Lua table is larger than fortran table")
          n = size(table)
       end if

       ! copy lua table to fortran table value-by-value, here we
       ! assume that the first index is 1 (as in Fortran)
       do i = 1, n

          ! get i-th element from lua table
          call lua_pushinteger(l,i)
          call lua_gettable(l,idx)

          call flu_get_scalar(l,-1,table(i),err2)

          call lua_pop(l,1)

          ! for every element we check if it was read correctly, if
          ! not, we set appropriate error flag
          if( err2 /= FPDE_STATUS_OK ) then
             err = FPDE_STATUS_ERROR
          end if

       end do

    end if

    if(present(error)) error = err

  end subroutine flu_get_array1d

  !> Tries to fill in a fortran table of type real from Lua. All
  !! parameters are analogous to flu_get_scalar_integer.
  subroutine flu_get_array2d( l, index, table, error )
    type(flu) :: l
    integer, intent(in) :: index
    class(*), intent(out) :: table(:,:)
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: tp = ""
    integer :: i, err, n, e, idx

    ! default to error
    err = FPDE_STATUS_ERROR

    ! its safer to operate on absolute index
    idx = lua_absindex(l,index)

    if( lua_type(l, idx) /= C_LUA_TTABLE ) then
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, table expected")

    else ! if no errors are present, then...
       ! if we got here, the last result on the stack comes from
       ! lua_len, so write it to n and pop it
       n = lua_rawlen(l, idx)

       ! default to ok
       err = FPDE_STATUS_OK

       ! check if fortran table is large enough
       if( n > size(table,2) ) then
          ! default to ok
          err = FPDE_STATUS_ERROR

          call l%log(FPDE_LOG_WARNING,&
               "Lua table is larger than fortran table")
          n = size(table,2)
       end if

       ! copy lua table to fortran table value-by-value, here we
       ! assume that the first index is 1 (as in Fortran)
       do i = 1, n

          ! get i-th element from lua table
          call lua_pushinteger(l,i)
          call lua_gettable(l,idx)

          ! check if the i-th element of lua table is a number
          if( lua_type(l, -1) /= C_LUA_TTABLE ) then
             write(tp,'(g0)') i
             call l%log(FPDE_LOG_ERROR,&
               "Invalid type of an element of lua table: &
               &[table] expected at position ["//trim(tp)//"]")
             err = FPDE_STATUS_ERROR
          else
             call flu_get_array1d(l,-1,table(:,i),e)
             if(e /= FPDE_STATUS_OK) err = e
          end if

          call lua_pop(l,1)

       end do

    end if

    if(present(error)) error = err

  end subroutine flu_get_array2d

end module flu_get_module
