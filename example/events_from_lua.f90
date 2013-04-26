program events_from_lua

  use flu_get_module
  use events
  use flu_module

  type(flu) :: l
  class(action), pointer :: ac
  class(event), pointer :: ev
  integer :: n, i, idx
  real, allocatable :: tab(:), tab2d(:,:)

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
  call flu_get(l,-1,tab)

  print *, tab

  call lua_getglobal(l,"tab2d")
  call flu_get(l,-1,tab2d)

  do i = 1, size(tab2d,2)
     print *, tab2d(:,i)
  end do

  call lua_pop(l,4)

  call lua_close(l)

end program events_from_lua
