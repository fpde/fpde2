program flu_get_scalar_test

  use flu_get_module
  use constants_module
  use flu_module

  type(flu) :: l
  logical :: dump
  integer :: err
  integer :: stack_size

  ! all of the types supported by flu_get_scalar
  logical :: lg = .true., lg2 = .false.
  integer :: in = 1, in2 = 2
  real    :: re = 1.23, re2 = 2.34
  character(len = 10) :: st = "abc", st2 = "def"

  ! example of an unsupported type
  complex :: cp

  l = luaL_newstate()

  stack_size = lua_gettop(l)

  call lua_pushboolean(l, lg)
  call lua_pushinteger(l, in)
  call lua_pushnumber (l, re)
  call lua_pushstring (l, st)

  if(stack_size + 4 /= lua_gettop(l)) then
     stop 1
  end if

  ! try to use unsupported type
  call flu_get_scalar(l,stack_size + 1, cp, err)
  ! result should be an error
  if( err == FPDE_STATUS_OK ) then
     stop 2
     ! it should never change the size of the stack
  else if( stack_size + 4 /= lua_gettop(l) ) then
     stop 3
  end if


  ! try logical
  call flu_get_scalar(l,stack_size + 1, lg2, err)
  ! result should be an error
  if( err /= FPDE_STATUS_OK ) then
     stop 4
     ! value should be the one we pushed
  else if( lg2 .neqv. lg ) then
     stop 5
     ! it should never change the size of the stack
  else if( stack_size + 4 /= lua_gettop(l) ) then
     stop 6
  end if

  ! try integer
  call flu_get_scalar(l,stack_size + 2, in2, err)
  ! result should be an error
  if( err /= FPDE_STATUS_OK ) then
     stop 7
     ! value should be the one we pushed
  else if( in2 /= in ) then
     stop 8
     ! it should never change the size of the stack
  else if( stack_size + 4 /= lua_gettop(l) ) then
     stop 9
  end if

  ! try real
  call flu_get_scalar(l,stack_size + 3, re2, err)
  ! result should be an error
  if( err /= FPDE_STATUS_OK ) then
     stop 10
     ! value should be the one we pushed
  else if( re2 /= re ) then
     stop 11
     ! it should never change the size of the stack
  else if( stack_size + 4 /= lua_gettop(l) ) then
     stop 12
  end if

  ! try string
  call flu_get_scalar(l,stack_size + 4, st2, err)
  ! result should be an error
  if( err /= FPDE_STATUS_OK ) then
     stop 13
     ! value should be the one we pushed
  else if( st2 /= st ) then
     stop 14
     ! it should never change the size of the stack
  else if( stack_size + 4 /= lua_gettop(l) ) then
     stop 15
  end if


end program flu_get_scalar_test
