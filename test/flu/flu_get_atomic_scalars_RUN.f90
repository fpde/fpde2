program flu_get_atomic_scalars_test

  use flu_get_module
  use constants_module
  use flu_module

  type(flu) :: l
  logical :: dump
  integer :: err
  integer :: stack_size

  ! all of the types supported by flu_get_scalar
  logical :: lg, lg2
  integer :: in, in2
  real    :: re, re2
  character(len = 10) :: st, st2

  ! example of an unsupported type
  complex :: cp

  call set_log_level(FPDE_LOG_DEBUG)

  l = luaL_newstate()

  lg = .true.
  lg2 = .false.
  dump = luaL_dostring(l, "lg=true")

  in = 1
  in2 = 2
  ! 'in' is a keyword in Lua, so we replace it with 'i'
  dump = luaL_dostring(l, "i=1")

  re = 1.23
  re2 = 2.34
  dump = luaL_dostring(l, "re=1.23")

  st = "abc"
  st2 = "def"
  dump = luaL_dostring(l, 'st="abc"')

  ! construct a table
  dump = luaL_dostring(l, 'tab={lg=lg, re=re, i=i, st=st}')

  stack_size = lua_gettop(l)

  ! test global
  call flu_get_atomic(l, key = "lg", index = 0, val = lg2, error = err)
  if( err /= FPDE_STATUS_OK ) then
     stop 1
  else if( lg2 .neqv. lg ) then
     stop 2
  else if( lua_gettop(l) /= stack_size ) then
     stop 3
  end if

  call flu_get_atomic(l, key = "i", index = 0, val = in2, error = err)
  if( err/= FPDE_STATUS_OK ) then
     stop 4
  else if( in2 /= in ) then
     stop 5
  else if( lua_gettop(l) /= stack_size ) then
     stop 6
  end if

  call flu_get_atomic(l, key = "re", index = 0, val = re2, error = err)
  if( err/= FPDE_STATUS_OK ) then
     stop 7
  else if( re2 /= re ) then
     stop 8
  else if( lua_gettop(l) /= stack_size ) then
     stop 9
  end if

  call flu_get_atomic(l, key = "st", index = 0, val = st2, error = err)
  if( err/= FPDE_STATUS_OK ) then
     stop 10
  else if( st2 /= st ) then
     stop 11
  else if( lua_gettop(l) /= stack_size ) then
     stop 12
  end if

  ! test table key-value extraction
  call lua_getglobal(l, "tab")
  stack_size = lua_gettop(l)

  ! with explicit index = -1
  call flu_get_atomic(l, key = "lg", index = -1, val = lg2, error = err)
  if( err /= FPDE_STATUS_OK ) then
     stop 13
  else if( lg2 .neqv. lg ) then
     stop 14
  else if( lua_gettop(l) /= stack_size ) then
     stop 15
  end if

  ! with assumed index = -1
  call flu_get_atomic(l, key = "lg", val = lg2, error = err)
  if( err /= FPDE_STATUS_OK ) then
     stop 16
  else if( lg2 .neqv. lg ) then
     stop 17
  else if( lua_gettop(l) /= stack_size ) then
     stop 18
  end if



end program flu_get_atomic_scalars_test
