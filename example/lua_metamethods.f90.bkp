program metamethods

  use iso_c_binding
  use flu_module
  use c_lua_module
  use flu_meta_module
  use flu_meta_array_module

  type(flu) :: l
  integer, parameter :: s = 10000000
  real, target :: a(s)
  type(lua_userdata_ptr), pointer :: ud
  type(c_funptr) :: fun
  type(c_ptr) :: cptr
  integer(4) :: i
  logical :: loaded
  real :: x = 0.0

  ! a=[(s-i, i=1,s)]
  do i = 1, s
     x = x+sin(real(i))
  end do

  l = luaL_newstate()
  call luaL_openlibs(l)

  i = 100
  print *, c_associated(l%lstate)
  call lua_pushinteger(l,1)
  ! cptr = c_lua_newuserdata(l%lstate,i)
  ! cptr = lua_newuserdata(l,100) !@todo, proper size
  call lua_register_userdata_as_array(l,a,"a",cptr)

  ! call c_f_pointer(cptr,ud)
  ! ud%ptr = c_loc(a)

  ! call lua_setglobal(l%lstate,"a")

  ! fun = c_funloc(index)
  ! print *, transfer(fun,i)

  ! ! call lua_add_to_metatable(l%lstate,"m1","__index",c_funloc(index))
  ! call lua_add_to_metatable(l%lstate,"m1","__index",index)
  ! ! call lua_add_to_metatable(l%lstate,"m1","__newindex",c_funloc(newindex))
  ! ! call lua_add_to_metatable(l%lstate,"m1","__len",c_funloc(len))

  ! call lua_getglobal(l%lstate,"a")
  ! call lua_getfield(l%lstate,LUA_REGISTRYINDEX,"m1")
  ! call lua_setmetatable(l%lstate,-2)

  print *, "Fortran: a(2)=",a(2)
  ! call l%load_file("../example/meta.lua")
  loaded = luaL_dofile(l,"../example/meta.lua")
  print *, "Fortran: a(2)=",a(2)

  call lua_close(l)

! contains

  ! function newindex(l)
  !   use iso_c_binding, only: c_int, c_ptr
  !   type(c_ptr), value :: l
  !   integer(c_int) :: newindex, i
  !   type(c_ptr) :: cptr
  !   type(lua_userdata_ptr), pointer :: ud
  !   real(c_double), pointer :: x(:)
  !   real(c_double) :: val

  !   ! cptr = luaL_checkudata(l,1,"m1"//c_null_char)
  !   cptr = lua_topointer(l,1)
  !   i = luaL_checkinteger(l,2)
  !   val = luaL_checknumber(l,3)

  !   call c_f_pointer(cptr,ud)
  !   call c_f_pointer(ud%ptr,x,[s]) !@todo size check
  !   x(i)=val

  !   ! return the number of results
  !   newindex = 0
  ! end function newindex

  ! function index(l)
  !   use iso_c_binding, only: c_int, c_ptr
  !   type(c_ptr), value :: l
  !   integer(c_int) :: index, i
  !   type(c_ptr) :: cptr
  !   type(lua_userdata_ptr), pointer :: ud
  !   real(c_double), pointer :: x(:)

  !   ! cptr = luaL_checkudata(l,1,"m1"//c_null_char)
  !   cptr = lua_topointer(l,1)
  !   ! cptr = lua_getuservalue(l,1)
  !   i = luaL_checkinteger(l,2)

  !   call c_f_pointer(cptr,ud)
  !   call c_f_pointer(ud%ptr,x,[s]) !@todo size
  !   call lua_pushnumber(l,x(i))

  !   ! return the number of results
  !   index = 1
  ! end function index

  ! function len(l)
  !   use iso_c_binding, only: c_int, c_ptr
  !   type(c_ptr), value :: l
  !   integer(c_int) :: len
  !   type(c_ptr) :: cptr
  !   type(lua_userdata_ptr), pointer :: ud
  !   real(c_double), pointer :: x(:)

  !   cptr = luaL_checkudata(l,1,"m1"//c_null_char)

  !   call c_f_pointer(cptr,ud)
  !   call c_f_pointer(ud%ptr,x,[s]) !@todo size
  !   call lua_pushinteger(l,size(x))

  !   ! return the number of results
  !   len = 1
  ! end function len



end program metamethods
