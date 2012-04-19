module c_lua_module
  use iso_c_binding !, only: c_ptr, c_null_ptr, c_null_char, c_char, c_int


    interface

     function c_luaL_newstate() bind(C,name="luaL_newstate")
       use iso_c_binding, only: c_ptr, c_funptr
       type(c_ptr) :: c_luaL_newstate
     end function c_luaL_newstate

     subroutine c_lua_close(lstate) bind(C,name="lua_close")
       use iso_c_binding, only: c_ptr
       type(c_ptr), value :: lstate
     end subroutine c_lua_close

     function c_luaL_loadstring(lstate, str) bind(C,name="luaL_loadstring")
       use iso_c_binding, only: c_ptr, c_char, c_int
       integer(c_int) :: c_luaL_loadstring
       type(c_ptr), value :: lstate
       character(kind=c_char) :: str(*)
     end function c_luaL_loadstring

     function c_lua_pcallk(lstate, nargs, nresults, errfunc, ctx, k) bind(C,name="lua_pcallk")
       use iso_c_binding, only: c_ptr, c_int
       integer(c_int) :: c_lua_pcallk
       type(c_ptr), value :: lstate, k
       integer(c_int), value :: nargs, nresults, errfunc, ctx
     end function c_lua_pcallk

     subroutine c_lua_getfield(lstate,index,name) bind(C,name="lua_getfield")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       character(kind=c_char) :: name(*)
     end subroutine c_lua_getfield

     subroutine c_lua_setfield(lstate,index,name) bind(C,name="lua_setfield")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       character(kind=c_char) :: name(*)
     end subroutine c_lua_setfield

     subroutine c_lua_getglobal(lstate,name) bind(C,name="lua_getglobal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine c_lua_getglobal

     subroutine c_lua_getlocal(lstate,name) bind(C,name="lua_getlocal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine c_lua_getlocal

     subroutine c_lua_setglobal(lstate,name) bind(C,name="lua_setglobal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine c_lua_setglobal

     subroutine c_lua_setlocal(lstate,name) bind(C,name="lua_setlocal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine c_lua_setlocal

     function c_lua_tolstring(lstate, index, len) bind(C,name="lua_tolstring")
       use iso_c_binding, only: c_ptr, c_char, c_int, c_size_t
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_size_t) :: len
       type(c_ptr) :: c_lua_tolstring
     end function c_lua_tolstring

     subroutine c_lua_settop(lstate,stackIdx) bind(C,name="lua_settop")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: stackIdx
     end subroutine c_lua_settop

     function c_lua_isstring(lstate, index) bind(C,name="lua_isstring")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: c_lua_isstring
     end function c_lua_isstring

     function c_lua_type(lstate, index) bind(C,name="lua_type")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: c_lua_type
     end function c_lua_type

     subroutine c_lua_len(lstate, index) bind(C,name="lua_len")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine c_lua_len


     function c_luaL_loadfilex(lstate, filename, mode) bind(C,name="luaL_loadfilex")
       use iso_c_binding, only: c_ptr, c_char, c_int
       integer(c_int) :: c_luaL_loadfilex
       type(c_ptr), value :: lstate
       ! type(c_ptr), value :: mode
       character(kind=c_char) :: filename(*), mode(*)
     end function c_luaL_loadfilex

     function c_luaL_loadfilex_ptr(lstate, filename, mode) bind(C,name="luaL_loadfilex")
       use iso_c_binding, only: c_ptr, c_char, c_int
       integer(c_int) :: c_luaL_loadfilex_ptr
       type(c_ptr), value :: lstate
       type(c_ptr), value :: mode
       character(kind=c_char) :: filename(*)
     end function c_luaL_loadfilex_ptr

     subroutine c_lua_gettable(lstate, index) bind(C,name="lua_gettable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine c_lua_gettable

     subroutine c_lua_settable(lstate, index) bind(C,name="lua_settable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine c_lua_settable

     function c_lua_absindex(lstate, index) bind(C,name="lua_absindex")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: c_lua_absindex
     end function c_lua_absindex

     function c_lua_topointer(lstate, index) bind(C,name="lua_topointer")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       type(c_ptr) :: c_lua_topointer
       integer(c_int), value :: index
     end function c_lua_topointer

     subroutine c_lua_rawget(lstate, index) bind(C,name="lua_rawget")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine c_lua_rawget

     function c_lua_rawlen(lstate, index) bind(C,name="lua_rawlen")
       use iso_c_binding, only: c_ptr, c_int, c_size_t
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_size_t) :: c_lua_rawlen
     end function c_lua_rawlen

     subroutine c_lua_pushinteger(lstate, int) bind(C,name="lua_pushinteger")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: int
     end subroutine c_lua_pushinteger

     subroutine c_lua_pushboolean(lstate, int) bind(C,name="lua_pushboolean")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: int
     end subroutine c_lua_pushboolean

     subroutine c_lua_pushnil(lstate) bind(C,name="lua_pushnil")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
     end subroutine c_lua_pushnil

     subroutine c_lua_pushnumber(lstate, val) bind(C,name="lua_pushnumber")
       use iso_c_binding, only: c_ptr, c_double
       type(c_ptr), value :: lstate
       real, value :: val
     end subroutine c_lua_pushnumber

     function c_lua_tointegerx(lstate, idx, isnum) bind(C,name="lua_tointegerx")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate, isnum
       integer(c_int), value :: idx
       integer(c_int) :: c_lua_tointegerx
     end function c_lua_tointegerx


     function c_lua_toboolean(lstate, idx) bind(C,name="lua_toboolean")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: idx
       integer(c_int) :: c_lua_toboolean
     end function c_lua_toboolean


     !> @todo use two variants, one for quad precision and one for
     !! double precision.
     function c_lua_tonumberx(lstate, idx, isnum) bind(C,name="lua_tonumberx")
       use iso_c_binding, only: c_ptr, c_int, c_double
       type(c_ptr), value :: lstate, isnum
       integer(c_int), value :: idx
       real(c_double) :: c_lua_tonumberx
     end function c_lua_tonumberx

     subroutine c_lua_pushstring(lstate, s) bind(C,name="lua_pushstring")
       use iso_c_binding, only: c_ptr, c_char
       type(c_ptr), value :: lstate
       character(kind=c_char) :: s(*)
     end subroutine c_lua_pushstring

     function c_lua_gettop(lstate) bind(C,name="lua_gettop")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int) :: c_lua_gettop
     end function c_lua_gettop

     subroutine c_luaL_openlibs(lstate) bind(C,name="luaL_openlibs")
       use iso_c_binding, only: c_ptr
       type(c_ptr), value :: lstate
     end subroutine c_luaL_openlibs

     subroutine c_lua_pushcclosure(lstate, ptr, n) bind(C,name="lua_pushcclosure")
       use iso_c_binding, only: c_ptr, c_int, c_funptr
       type(c_ptr), value :: lstate
       type(c_funptr), value :: ptr
       integer(c_int), value :: n
     end subroutine c_lua_pushcclosure

     subroutine c_lua_pushlightuserdata(lstate, ptr) bind(C,name="lua_pushlightuserdata")
       use iso_c_binding, only: c_ptr
       type(c_ptr), value :: lstate
       type(c_ptr), value :: ptr
     end subroutine c_lua_pushlightuserdata

     function c_lua_newuserdata(lstate, size) bind(C,name="lua_newuserdata")
       use iso_c_binding, only: c_ptr, c_size_t
       type(c_ptr), value :: lstate
       integer(c_size_t), value :: size
       type(c_ptr) :: c_lua_newuserdata
     end function c_lua_newuserdata

     subroutine c_lua_setmetatable(lstate, index) bind(C,name="lua_setmetatable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine c_lua_setmetatable

     subroutine c_lua_getmetatable(lstate, index) bind(C,name="lua_getmetatable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine c_lua_getmetatable

     function c_luaL_newmetatable(lstate, name) bind(C,name="luaL_newmetatable")
       use iso_c_binding, only: c_ptr, c_int, c_char
       type(c_ptr), value :: lstate
       character(c_char) :: name(*)
       integer(c_int) :: c_luaL_newmetatable
     end function c_luaL_newmetatable

     function c_luaL_checknumber(lstate, index) bind(C,name="luaL_checknumber")
       use iso_c_binding, only: c_ptr, c_int, c_double
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       real(c_double) :: c_luaL_checknumber
     end function c_luaL_checknumber

     function c_luaL_checkinteger(lstate, index) bind(C,name="luaL_checkinteger")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: c_luaL_checkinteger
     end function c_luaL_checkinteger

     function c_luaL_checkudata(lstate, index, name) bind(C,name="luaL_checkudata")
       use iso_c_binding, only: c_ptr, c_int, c_char
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       type(c_ptr) :: c_luaL_checkudata
       character(c_char) :: name(*)
     end function c_luaL_checkudata

     function c_lua_next(lstate, index) bind(C,name="lua_next")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: c_lua_next
     end function c_lua_next

  end interface

end module c_lua_module
