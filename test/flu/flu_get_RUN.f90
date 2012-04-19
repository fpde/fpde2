program flu_get_test

  use flu_get_module
  use flu_module

  type(flu) :: l
  logical :: dump
  integer :: err
  integer :: stack_size

  l = luaL_newstate()

  dump = luaL_dostring(l, "a = {b = 10}")
  if( .not. dump ) then
     stop 111
  end if

  ! access existing global value
  stack_size = lua_gettop(l)
  call flu_get(l,0,"a",err)
  if( stack_size + 1/= lua_gettop(l) ) then
     stop 1
  else if( err /= FPDE_STATUS_OK ) then
     stop 2
  end if

  ! access existing key in a table
  call flu_get(l,-1,"b",err)
  if( stack_size + 2 /= lua_gettop(l) ) then
     stop 3
  else if( err /= FPDE_STATUS_OK ) then
     stop 4
  else if( lua_type(l,-1) /= C_LUA_TNUMBER ) then
     stop 5
  else if( lua_tointeger(l,-1) /= 10 ) then
     stop 6
  end if

  ! try to get a key-value out of integer
  call flu_get(l,-1,"zzz",err)
  if( stack_size + 3 /= lua_gettop(l) ) then
     stop 7
  else if( err == FPDE_STATUS_OK ) then
     stop 8
     ! nil should be pushed
  else if( .not. lua_isnil(l,-1) ) then
     stop 9
  end if

  ! pull back the stack
  call lua_pop(l,2)

  ! try to access nonexistant key from the table "a"
  call flu_get(l,-1,"zzz",err)
  if( stack_size + 2 /= lua_gettop(l) ) then
     stop 10
  else if( err == FPDE_STATUS_OK ) then
     stop 11
     ! nil should be pushed
  else if( .not. lua_isnil(l,-1) ) then
     stop 12
  end if

  ! clear the stack
  call lua_pop(l,2)

  ! try to access a nonexistant global value
  call flu_get(l,-1,"zzz",err)
  if( stack_size + 1 /= lua_gettop(l) ) then
     stop 13
  else if( err == FPDE_STATUS_OK ) then
     stop 14
     ! nil should be pushed
  else if( .not. lua_isnil(l,-1) ) then
     stop 15
  end if

end program flu_get_test
