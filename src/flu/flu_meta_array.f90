module flu_meta_array_module
  use flu_meta_module
  use flu_primitives_module
  use iso_c_binding

  private

  character(c_char) :: FLU_ARRAY_METATABLE = "flu_array_metatable"

  public lua_register_userdata_as_array

contains

  subroutine lua_register_userdata_as_array(l,a,name,cptr)
    use iso_c_binding
    type(c_ptr) :: l
    ! @bug: at this moment it will not work with quad, user has to be
    ! warned
    real(c_double), target :: a(:)
    type(c_ptr) :: cptr
    type(lua_userdata_ptr), pointer :: ud
    character(c_char) :: name(*)

    ! possible @bug, size of lua_userdata_ptr might be larger than 32
    ! bytes?
    print *, transfer(l,1)
    ! cptr = lua_newuserdata(l,LUA_USERDATA_PTR_SIZE)
    ! point ud to newly created userdata
    call c_f_pointer(cptr,ud)
    ! use pointer inside lua_userdata_ptr as a pointer to the table
    ud%ptr = c_loc(a)

    call lua_add_to_metatable(l, FLU_ARRAY_METATABLE,"__index",index)
    call lua_add_to_metatable(l, FLU_ARRAY_METATABLE,"__newindex",newindex)
    call lua_add_to_metatable(l, FLU_ARRAY_METATABLE,"__len",len)
    call lua_getfield(l, LUA_REGISTRYINDEX, FLU_ARRAY_METATABLE)
    call lua_setmetatable(l,-2)
    call lua_setglobal(l,name)

  end subroutine lua_register_userdata_as_array


  function newindex(l) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    type(c_ptr), value :: l
    integer(c_int) :: newindex, i
    type(c_ptr) :: cptr
    type(lua_userdata_ptr), pointer :: ud
    real(c_double), pointer :: x(:)
    real(c_double) :: val

    ! cptr = luaL_checkudata(l,1,"m1"//c_null_char)
    cptr = lua_topointer(l,1)
    i = luaL_checkinteger(l,2)
    val = luaL_checknumber(l,3)

    call c_f_pointer(cptr,ud)
    ! call c_f_pointer(ud%ptr,x,[s]) !@todo size check
    ! x(i)=val

    ! return the number of results
    newindex = 0
  end function newindex

  function index(l) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    type(c_ptr), value :: l
    integer(c_int) :: index
    integer(c_int) :: i
    type(c_ptr) :: cptr
    type(lua_userdata_ptr), pointer :: ud
    real(c_double), pointer :: x(:)

    ! cptr = luaL_checkudata(l,1,"m1"//c_null_char)
    cptr = lua_topointer(l,1)
    ! cptr = lua_getuservalue(l,1)
    i = luaL_checkinteger(l,2)

    call c_f_pointer(cptr,ud)
    ! call c_f_pointer(ud%ptr,x,[s]) !@todo size
    ! call lua_pushnumber(l,x(i))

    ! return the number of results
    index = 1
  end function index

  function len(l) bind(c)
    use iso_c_binding, only: c_int, c_ptr
    type(c_ptr), value :: l
    integer(c_int) :: len
    type(c_ptr) :: cptr
    type(lua_userdata_ptr), pointer :: ud
    real(c_double), pointer :: x(:)

    cptr = luaL_checkudata(l,1,"m1"//c_null_char)

    call c_f_pointer(cptr,ud)
    ! call c_f_pointer(ud%ptr,x,[s]) !@todo size
    ! call lua_pushinteger(l,size(x))

    ! return the number of results
    len = 1
  end function len


end module flu_meta_array_module
