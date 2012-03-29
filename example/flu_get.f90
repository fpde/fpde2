program flu_get_program
  use logger_module
  use flu_module
  use flu_get_module
  use solver_flu_module
  use class_solver
  use class_solver_simple

  type(flu) :: l
  class(solver), pointer :: s

  integer :: err

  call set_log_level(FPDE_LOG_DEBUG)

  l = luaL_newstate()

  if( luaL_dofile(l,"../example/flu_get.lua") ) then
  end if

  call flu_get_solver(l, s, "s", index = 0, error = err)

  call s%info()

  call lua_close(l)

end program flu_get_program
