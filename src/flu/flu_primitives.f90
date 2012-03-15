module flu_primitives_module
  use iso_c_binding !, only: c_ptr, c_null_ptr, c_null_char, c_char, c_int

  !> lua constants
  integer(c_int), parameter :: &
       LUA_MULTRET = -1,&
       LUAI_MAXSTACK = 1000000,&
       LUAI_FIRSTPSEUDOINDEX = -LUAI_MAXSTACK - 1000,&
       LUA_REGISTRYINDEX = LUAI_FIRSTPSEUDOINDEX

  !> lua types
  integer(c_int), parameter ::&
       LUA_TNONE          =(-1),&
       LUA_TNIL           =0,&
       LUA_TBOOLEAN       =1,&
       LUA_TLIGHTUSERDATA =2,&
       LUA_TNUMBER        =3,&
       LUA_TSTRING        =4,&
       LUA_TTABLE         =5,&
       LUA_TFUNCTION      =6,&
       LUA_TUSERDATA      =7,&
       LUA_TTHREAD        =8

  !> a type used as a handle for userdata. it stores a pointer to the
  !> real data
  type :: lua_userdata_ptr
     type(c_ptr) :: ptr
  end type lua_userdata_ptr

  !> assumed size of userdata, @todo check if 32 bytes is enough
  integer(c_int), parameter :: LUA_USERDATA_PTR_SIZE = 100

  abstract interface
     !> abstract interface to the type of lua_CFunction
     integer(c_int) function lua_cfunction(l) bind(c)
       use iso_c_binding, only: c_int, c_ptr
       type(c_ptr), value :: l
     end function lua_cfunction

  end interface

  public :: lua_cfunction


    interface

     function luaL_newstate() bind(C,name="luaL_newstate")
       use iso_c_binding, only: c_ptr, c_funptr
       type(c_ptr) :: luaL_newstate
     end function luaL_newstate

     subroutine lua_close(lstate) bind(C,name="lua_close")
       use iso_c_binding, only: c_ptr
       type(c_ptr), value :: lstate
     end subroutine lua_close

     function luaL_loadstring(lstate, str) bind(C,name="luaL_loadstring")
       use iso_c_binding, only: c_ptr, c_char, c_int
       integer(c_int) :: luaL_loadstring
       type(c_ptr), value :: lstate
       character(kind=c_char) :: str(*)
     end function luaL_loadstring

     function lua_pcallk(lstate, nargs, nresults, errfunc, ctx, k) bind(C,name="lua_pcallk")
       use iso_c_binding, only: c_ptr, c_int
       integer(c_int) :: lua_pcallk
       type(c_ptr), value :: lstate, k
       integer(c_int), value :: nargs, nresults, errfunc, ctx
     end function lua_pcallk

     subroutine lua_getfield(lstate,index,name) bind(C,name="lua_getfield")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       character(kind=c_char) :: name(*)
     end subroutine lua_getfield

     subroutine lua_setfield(lstate,index,name) bind(C,name="lua_setfield")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       character(kind=c_char) :: name(*)
     end subroutine lua_setfield

     subroutine lua_getglobal(lstate,name) bind(C,name="lua_getglobal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine lua_getglobal

     subroutine lua_getlocal(lstate,name) bind(C,name="lua_getlocal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine lua_getlocal

     subroutine lua_setglobal(lstate,name) bind(C,name="lua_setglobal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine lua_setglobal

     subroutine lua_setlocal(lstate,name) bind(C,name="lua_setlocal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine lua_setlocal

     function lua_tolstring(lstate, index, len) bind(C,name="lua_tolstring")
       use iso_c_binding, only: c_ptr, c_char, c_int, c_size_t
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_size_t) :: len
       type(c_ptr) :: lua_tolstring
     end function lua_tolstring

     subroutine lua_settop(lstate,stackIdx) bind(C,name="lua_settop")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: stackIdx
     end subroutine lua_settop

     function lua_isstring(lstate, index) bind(C,name="lua_isstring")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: lua_isstring
     end function lua_isstring

     function lua_type(lstate, index) bind(C,name="lua_type")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: lua_type
     end function lua_type

     function lua_len(lstate, index) bind(C,name="lua_len")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: lua_len
     end function lua_len


     function luaL_loadfilex(lstate, filename, mode) bind(C,name="luaL_loadfilex")
       use iso_c_binding, only: c_ptr, c_char, c_int
       integer(c_int) :: luaL_loadfilex
       type(c_ptr), value :: lstate
       ! type(c_ptr), value :: mode
       character(kind=c_char) :: filename(*), mode(*)
     end function luaL_loadfilex

     function luaL_loadfilex_ptr(lstate, filename, mode) bind(C,name="luaL_loadfilex")
       use iso_c_binding, only: c_ptr, c_char, c_int
       integer(c_int) :: luaL_loadfilex_ptr
       type(c_ptr), value :: lstate
       type(c_ptr), value :: mode
       character(kind=c_char) :: filename(*)
     end function luaL_loadfilex_ptr

     subroutine lua_gettable(lstate, index) bind(C,name="lua_gettable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine lua_gettable

     function lua_topointer(lstate, index) bind(C,name="lua_topointer")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       type(c_ptr) :: lua_topointer
       integer(c_int), value :: index
     end function lua_topointer

     subroutine lua_settable(lstate, index) bind(C,name="lua_settable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine lua_settable

     subroutine lua_rawget(lstate, index) bind(C,name="lua_rawget")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine lua_rawget

     subroutine lua_pushinteger(lstate, int) bind(C,name="lua_pushinteger")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: int
     end subroutine lua_pushinteger

     subroutine lua_pushnumber(lstate, val) bind(C,name="lua_pushnumber")
       use iso_c_binding, only: c_ptr, c_double
       type(c_ptr), value :: lstate
       real(c_double), value :: val
     end subroutine lua_pushnumber

     function lua_tointegerx(lstate, idx, isnum) bind(C,name="lua_tointegerx")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate, isnum
       integer(c_int), value :: idx
       integer(c_int) :: lua_tointegerx
     end function lua_tointegerx


     subroutine lua_pushstring(lstate, s) bind(C,name="lua_pushstring")
       use iso_c_binding, only: c_ptr, c_char
       type(c_ptr), value :: lstate
       character(kind=c_char) :: s(*)
     end subroutine lua_pushstring

     function lua_gettop(lstate) bind(C,name="lua_gettop")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int) :: lua_gettop
     end function lua_gettop

     subroutine luaL_openlibs(lstate) bind(C,name="luaL_openlibs")
       use iso_c_binding, only: c_ptr
       type(c_ptr), value :: lstate
     end subroutine luaL_openlibs

     subroutine lua_pushcclosure(lstate, ptr, n) bind(C,name="lua_pushcclosure")
       use iso_c_binding, only: c_ptr, c_int, c_funptr
       type(c_ptr), value :: lstate
       type(c_funptr), value :: ptr
       integer(c_int), value :: n
     end subroutine lua_pushcclosure

     subroutine lua_pushlightuserdata(lstate, ptr) bind(C,name="lua_pushlightuserdata")
       use iso_c_binding, only: c_ptr
       type(c_ptr), value :: lstate
       type(c_ptr), value :: ptr
     end subroutine lua_pushlightuserdata

     function lua_newuserdata(lstate, size) bind(C,name="lua_newuserdata")
       use iso_c_binding, only: c_ptr, c_size_t
       type(c_ptr), value :: lstate
       integer(c_size_t) :: size
       type(c_ptr) :: lua_newuserdata
     end function lua_newuserdata

     subroutine lua_setmetatable(lstate, index) bind(C,name="lua_setmetatable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine lua_setmetatable

     subroutine lua_getmetatable(lstate, index) bind(C,name="lua_getmetatable")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
     end subroutine lua_getmetatable

     function luaL_newmetatable(lstate, name) bind(C,name="luaL_newmetatable")
       use iso_c_binding, only: c_ptr, c_int, c_char
       type(c_ptr), value :: lstate
       character(c_char) :: name(*)
       integer(c_int) :: luaL_newmetatable
     end function luaL_newmetatable

     function luaL_checknumber(lstate, index) bind(C,name="luaL_checknumber")
       use iso_c_binding, only: c_ptr, c_int, c_double
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       real(c_double) :: luaL_checknumber
     end function luaL_checknumber

     function luaL_checkinteger(lstate, index) bind(C,name="luaL_checkinteger")
       use iso_c_binding, only: c_ptr, c_int
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       integer(c_int) :: luaL_checkinteger
     end function luaL_checkinteger

     function luaL_checkudata(lstate, index, name) bind(C,name="luaL_checkudata")
       use iso_c_binding, only: c_ptr, c_int, c_char
       type(c_ptr), value :: lstate
       integer(c_int), value :: index
       type(c_ptr) :: luaL_checkudata
       character(c_char) :: name(*)
     end function luaL_checkudata


  end interface

contains

  function lua_pcall(L,n,r,f)
    type(c_ptr) :: L
    integer(c_int) :: lua_pcall
    integer(c_int) :: n, r, f

    lua_pcall = lua_pcallk(L, n, r, f, 0, c_null_ptr)
  end function lua_pcall

  function luaL_loadfile(lstate, filename)
    use iso_c_binding, only: c_ptr, c_char, c_int
    integer(c_int) :: luaL_loadfile
    type(c_ptr) :: lstate
    character(len=*) :: filename

    luaL_loadfile = luaL_loadfilex_ptr( lstate, trim(filename)//c_null_char, c_null_ptr )

    if( luaL_loadfile /= 0 ) then
       return
    end if

  end function luaL_loadfile

  ! returns >0 in case of error
  function luaL_dofile(lstate, filename)
    integer(c_int) :: luaL_dofile
    type(c_ptr) :: lstate
    character(len=*) :: filename

    luaL_dofile=luaL_loadfile(lstate,trim(filename))
    if( luaL_dofile /=0 ) then
       return
    end if

    luaL_dofile=lua_pcall(lstate,0,LUA_MULTRET,0)
    if( luaL_dofile /= 0 ) then
       return
    end if

  end function luaL_dofile

  subroutine lua_tostring(lstate, index, str)
    type(c_ptr) :: lstate
    character(len=*), intent(out) :: str
    integer(c_int) :: index, len
    type(c_ptr) :: ptr
    ! character(c_char), pointer :: c_str(:)

    ptr = lua_tolstring(lstate,index,len)

    ! call c_f_pointer(ptr, c_str, [len])
    call c_ptr_to_str(ptr, str, len)

  end subroutine lua_tostring

  ! converts c_ptr (assuming it points to c_char) to character(len=*)
  subroutine c_ptr_to_str(ptr, str, len)
    character(len=*), intent(out) :: str
    type(c_ptr) :: ptr
    integer(c_int) :: len, i
    character(c_char), pointer :: c_str(:)

    call c_f_pointer(ptr, c_str, [len])

    write (str,*) (c_str(i), i=1,len)
    str=trim(adjustl(str))

  end subroutine c_ptr_to_str

  subroutine lua_pop(lstate, n) bind(C,name="lua_pop")
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), value :: lstate
    integer(c_int), value :: n
    call lua_settop(lstate,-n-1)
  end subroutine lua_pop


end module flu_primitives_module
