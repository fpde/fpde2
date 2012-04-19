program flu_get_scalar_real_test

  use flu_get_module
  use constants_module
  use flu_module

  type(flu) :: l
  logical :: dump
  integer :: err
  real :: i_in = 1.11, i_out = 0.0, i_out2 = 0.0
  integer :: stack_size

  l = luaL_newstate()

  stack_size = lua_gettop(l)

  call lua_pushnumber(l,i_in)
  call lua_pushstring(l,"abc")
  call lua_pushnil(l)
  call lua_pushinteger(l,11)

  if(stack_size + 4 /= lua_gettop(l)) then
     stop 1
  end if

  ! get real
  call flu_get_scalar_real(l,stack_size + 1, i_out, err)
  if( err /= FPDE_STATUS_OK ) then
     stop 2
  else if( i_in /= i_out ) then
     stop 3
  else if( lua_gettop(l) /= stack_size + 4 ) then
     stop 4
  end if

  ! get some non-numerical value
  i_out2 = i_out
  call flu_get_scalar_real(l,stack_size + 2, i_out2, err)
  if( err == FPDE_STATUS_OK ) then
     stop 5
  else if( i_out /= i_out2 ) then
     stop 6
  else if( lua_gettop(l) /= stack_size + 4 ) then
     stop 7
  end if

  ! try to access a nil value
  i_out2 = i_out
  call flu_get_scalar_real(l,stack_size + 3, i_out2, err)
  if( err == FPDE_STATUS_OK ) then
     stop 8
  else if( i_out /= i_out2 ) then
     stop 9
  else if( lua_gettop(l) /= stack_size + 4 ) then
     stop 10
  end if

  ! try to access an integer value
  call flu_get_scalar_real(l,stack_size + 4, i_out, err)
  if( err /= FPDE_STATUS_OK ) then
     stop 11
     ! value of i_out is unspecified in lua manual
  else if( i_out /= 11 ) then
     stop 12
  else if( lua_gettop(l) /= stack_size + 4 ) then
     stop 13
  end if


end program flu_get_scalar_real_test
