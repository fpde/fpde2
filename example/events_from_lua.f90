program events_from_lua

  use flu_get_module
  use events
  use flu_module

  type(flu) :: l
  class(action), pointer :: ac
  class(event), pointer :: ev
  integer :: n, idx
  real, allocatable :: tab(:)

  call set_log_level(FPDE_LOG_DEBUG)
  l = luaL_newstate()
  call luaL_openlibs(l)
  if( luaL_dofile(l, "../example/events.lua") ) then
  end if

  call lua_getglobal(l,"ac1")
  ac => action_hello(l, -1)

  call lua_getglobal(l,"ev1")
  ev => event_true(l, -1)

  call lua_getglobal(l,"tab")

  idx = lua_absindex(l,-1)
  n = lua_rawlen(l,idx)
  allocate(tab(n))
  do i = 1, n
     call lua_pushinteger(l,i)
     call lua_gettable(l,idx)
     call flu_get_scalar(l,-1,tab(i))
  end do
  call lua_pop(l,n)

  print *, tab

  call lua_pop(l,3)

  call lua_close(l)

end program events_from_lua
