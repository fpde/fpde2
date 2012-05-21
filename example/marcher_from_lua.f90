program marcher_from_lua

   use flu_module
   use class_ode_marcher_simple

   type(flu) :: l
   type(ode_marcher_simple) :: m
   integer :: err

   call set_log_level(FPDE_LOG_DEBUG)

   l = luaL_newstate()
   if( luaL_dofile(l,"../example/marcher.lua") ) then
   end if

   call lua_getglobal(l,"m")
   call m%from_lua(l, error = err)

   print *, trim(m%name)
   print *, m%dim

   call m%init()

   print *, m%dim

end program marcher_from_lua
