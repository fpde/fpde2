program function_registry_test

  use logger_module
  use class_func_registry
  use constants_module
  use flu_module

  type(flu) :: l
  type(func_registry) :: r
  logical :: dump
  integer :: err

  r%name = "reg"

  call set_log_level(FPDE_LOG_DEBUG)

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

  dump = luaL_dostring(l,&
       'reg = {f,g}')
  if( .not. dump )                      stop 1

  call lua_getglobal(l,"reg")
  call r%from_lua(l, err)

  !> @bug this test makes excessive use of the internal structure of
  !! func_registry, which may change in future implementations.
  if( err /= FPDE_STATUS_OK ) stop 2
  if( r%n_func /= 2 ) stop 3
  if( r%func(1)%name /= "f" ) stop 4
  if( r%func(2)%name /= "g" ) stop 4

end program function_registry_test
