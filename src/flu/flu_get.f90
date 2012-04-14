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

  interface flu_get_scalar
     module procedure flu_get_scalar_integer
     module procedure flu_get_scalar_character
     module procedure flu_get_scalar_real
     module procedure flu_get_scalar_logical
  end interface flu_get_scalar

contains


  function flu_get_len(l, index)
    type(flu) :: l
    integer, intent(in) :: index
    integer :: flu_get_len

    if( lua_type(l,-1) /= C_LUA_TTABLE ) then
       flu_get_len = 0
    else
       call lua_len(l,index)
       flu_get_len = lua_tointeger(l,-1)
       call lua_pop(l,1)
    end if

  end function flu_get_len


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
       if( lua_type(l,index) == C_LUA_TTABLE ) then
          call lua_getfield( l, index, key )
       else
          call l%log(FPDE_LOG_INFO,&
               "Invalid index passed to flu_get")
          return
       end if
    end if

    if( lua_isnil(l, -1) ) then
       call l%log(FPDE_LOG_INFO,&
            "No global name or field ["//trim(key)//"] exists")
       call lua_pop(l,-1)
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
    else if( .not.( present(key) .or. present(index) ) ) then
       call l%log(FPDE_LOG_ERROR,&
            "At least one out of key and index arguments is permited")
       if(present(error)) error = err
       return
    end if

    ! assign default value of index to idx
    if( present(index) ) then
       idx = index
    else
       idx = -1
    end if

    ! if key is present, then look for a global variable or a
    ! component of a table, and push it to the top of the stack
    !! @todo call lua_get instead
    if( present(key) ) then
       if( idx == 0 ) then
          call lua_getglobal( l, key )
       else
          if( lua_type(l,idx) == C_LUA_TTABLE ) then
             call lua_getfield( l, idx, key )
          else
             call l%log(FPDE_LOG_ERROR,&
                  "No table found at the index passed to flu_get_atomic,&
                  & lookup key was ["//trim(key)//"]")
             if(present(error)) error = err
             return
          end if
       end if
       ! idx is an index of datum pushed by lua_getglobal or
       ! lua_getfield
       idx = -1
       if( lua_type(l,-1) == C_LUA_TNIL ) then
          call l%log(FPDE_LOG_INFO,&
               "No global name or field ["//trim(key)//"] exists")
          if(present(error)) error = err
          call lua_pop(l,1)
          return
       end if
    end if

    if(present(char)) then
       call flu_get_scalar(l,idx,char,err)

    else if(present(val)) then

       select type(val)
       type is(integer)
          call flu_get_scalar(l,idx,val,err)
       ! temporarly replaced by "char"
       ! type is(character(len=*))
       !    call flu_get_scalar_character(l,idx,val,err)
       type is(real)
          call flu_get_scalar(l,idx,val,err)
       type is(logical)
          call flu_get_scalar(l,idx,val,err)
       class default
          call l%log(FPDE_LOG_ERROR,"Unrecognized type")
       end select

    else if(present(val1d)) then

       select type(val1d)
       type is(integer)
          call flu_get_array1d(l,idx,val1d,err)
       type is(character(len=*))
          call flu_get_array1d(l,idx,val1d,err)
       type is(real)
          call flu_get_array1d(l,idx,val1d,err)
       type is(logical)
          call flu_get_array1d(l,idx,val1d,err)
       class default
          call l%log(FPDE_LOG_ERROR,"Unrecognized type")
       end select

    else if(present(val2d)) then

       select type(val2d)
       type is(integer)
          call flu_get_array2d(l,idx,val2d,err)
       type is(character(len=*))
          call flu_get_array2d(l,idx,val2d,err)
       type is(real)
          call flu_get_array2d(l,idx,val2d,err)
       type is(logical)
          call flu_get_array2d(l,idx,val2d,err)
       class default
          call l%log(FPDE_LOG_ERROR,"Unrecognized type")
       end select

    end if

    ! if we pushed something on top of the stack we have to pop it
    if(present(key)) then
       call lua_pop(l,1)
    end if

    if(present(error)) error = err

    if(present(key) .and. err /= FPDE_STATUS_OK) then
       call l%log(FPDE_LOG_ERROR,&
            "Error(s) occured while reading ["//trim(key)//"]")
    end if

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
       r = lua_tointeger(l, index)
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

  !> Tries to fill in a fortran table of type real from Lua. All
  !! parameters are analogous to flu_get_scalar_integer.
  subroutine flu_get_array1d( l, index, table, error )
    type(flu) :: l
    integer, intent(in) :: index
    class(*), intent(out) :: table(:)
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: tp = ""
    integer :: i, err, n, idx

    ! its safer to operate on absolute index
    idx = lua_absindex(l,index)

    ! default to error
    err = FPDE_STATUS_ERROR

    if( lua_type(l, idx) /= C_LUA_TTABLE ) then
       call l%log(FPDE_LOG_ERROR,&
            "Invalid type, table expected")

       ! if no errors are present, then...
    else
       ! if we got here, the last result on the stack comes from
       ! lua_len, so write it to n and pop it
       n = flu_get_len(l, idx)

       ! default to ok
       err = FPDE_STATUS_OK

       ! check if fortran table is large enough
       if( n > size(table) ) then
          ! default to ok
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

          ! assign a value from lua table to i-th element of
          ! fortran table
          select type(table)

             ! integer case
          type is(integer)
             call flu_get_scalar(l,-1,table(i),err)
             ! ! check if the i-th element of lua table is a number
             ! if( lua_type(l, -1) == C_LUA_TNUMBER ) then
             !    table(i) = lua_tointeger(l, -1)
             ! else
             !    ! switch the error flag
             !    err = FPDE_STATUS_ERROR
             !    ! tp = "integer"
             !    exit
             ! end if

             ! real case
          type is(real)
             call flu_get_scalar(l,-1,table(i),err)
             ! if( lua_type(l, -1) == C_LUA_TNUMBER ) then
             !    table(i) = lua_tonumber(l,-1)
             ! else
             !    err = FPDE_STATUS_ERROR
             !    ! tp = "number"
             ! end if

             ! character case
          type is(character(len=*))
             call flu_get_scalar(l,-1,table(i),err)

             ! if( lua_type(l, -1) == C_LUA_TSTRING ) then
             !    call lua_tostring(l,-1,table(i))
             ! else
             !    err = FPDE_STATUS_ERROR
             !    ! tp = "string"
             !    exit
             ! end if
          end select

          call lua_pop(l,1)

       end do

       ! if(err == FPDE_STATUS_ERROR) then
       !    ! report an error
       !    call l%log(FPDE_LOG_ERROR,&
       !         "Invalid type of an element of lua table&
       !         &, ["//trim(tp)//"] expected")
       ! end if

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
       n = flu_get_len(l, idx)

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

  ! subroutine flu_get_table_integer( l, table, key, index, default, error )
  !   type(flu) :: l
  !   character(len=*) :: key
  !   integer, intent(out) :: table(:)
  !   integer, optional, intent(out) :: error
  !   integer, optional, intent(in) :: index
  !   integer, optional, intent(in) :: default

  !   real :: temp(size(table)), def
  !   integer :: idx, err

  !   if(present(index)) then
  !      idx = index
  !   else
  !      idx = -1
  !   end if

  !   ! call the get_table_real version
  !   if(present(default)) then
  !      call flu_get_table_real( l,&
  !           temp, key, idx, default = real(default), error = err )
  !   else
  !      call flu_get_table_real( l,&
  !           temp, key, idx, error = err )
  !   end if

  !   ! cast the result to int
  !   table = int(temp)

  !   if(present(error)) error = err

  ! end subroutine flu_get_table_integer

end module flu_get_module
