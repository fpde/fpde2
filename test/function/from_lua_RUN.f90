program function_from_lua_test

  use class_function
  use constants_module
  use flu_module
  use logger_module

  type(func) :: f, g
  type(flu) :: l
  logical :: dump
  integer :: err

  call set_log_level(FPDE_LOG_ERROR)

  l = luaL_newstate()
  dump = luaL_dostring(l,&
       'bdries = {default = {type = "boundary_dirichlet"}}')
  if( .not. dump )                      stop 1

  dump = luaL_dostring(l,&
       'f = {name = "f", derivatives = {{"x","y"},{"z"}}, boundary = bdries}')
  if( .not. dump )                      stop 1

  dump = luaL_dostring(l,&
       'g = {name = "g"}')
  if( .not. dump )                      stop 1

  call lua_getglobal(l,"f")
  call f%from_lua(l, error = err)
  if( err /= FPDE_STATUS_OK )           stop 2
  if( f%name /= "f" )                   stop 3
  if( .not. associated(f%derivatives) ) stop 4
  if( size(f%derivatives, 2) /= 2 )     stop 5
  if( f%derivatives(1,1) /=  "x" )      stop 6
  if( f%derivatives(2,1) /=  "y" )      stop 6
  if( f%derivatives(1,2) /=  "z" )      stop 6
  if( f%boundary%default%type /= "boundary_dirichlet" ) stop 11

  call lua_getglobal(l,"g")
  call g%from_lua(l, error = err)
  if( err /= FPDE_STATUS_OK )           stop 7
  ! derivatives should be allocated anyway
  if( .not. associated(g%derivatives) ) stop 8
  if( size(g%derivatives, 2) /= 0 )     stop 9
  if( g%name /= "g" )                   stop 10

end program function_from_lua_test
