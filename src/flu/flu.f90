!>
!! @file   flu.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Tue Feb 21 21:49:43 2012
!!
!! @brief
!!
!! @todo : separate low-level operations to flu_primitives
!! @todo : pop the stack after get_
!!
!!
!!
module flu_module
  use iso_c_binding !, only: c_ptr, c_null_ptr, c_null_char, c_char, c_int
  use logger_module

  private

  integer(c_int), parameter :: LUA_MULTRET = -1

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


  type, public, extends(named) :: flu
     type(c_ptr), private :: lstate = c_null_ptr
   contains
     procedure :: init
     procedure :: free
     procedure :: push_by_key
     procedure :: get_recursively
     procedure :: load_file
     procedure :: array_to_key
     procedure :: get_len
     procedure, private :: get_character
     procedure, private :: get_real
     procedure, private :: get_real_array
     generic :: get => get_real, get_character, get_real_array
  end type flu

  ! interface get
  !    module procedure get_real, get_character
  ! end interface get


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

     subroutine lua_getglobal(lstate,name) bind(C,name="lua_getglobal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine lua_getglobal

     subroutine lua_setglobal(lstate,name) bind(C,name="lua_setglobal")
       use iso_c_binding, only: c_ptr, c_char, c_int
       type(c_ptr), value :: lstate
       character(kind=c_char) :: name(*)
     end subroutine lua_setglobal

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

     subroutine lua_pushstring(lstate, s) bind(C,name="lua_pushstring")
       use iso_c_binding, only: c_ptr, c_char
       type(c_ptr), value :: lstate
       character(kind=c_char) :: s(*)
     end subroutine lua_pushstring

  end interface

contains

!!!!!!!!! Some low-level lua calls

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
    character(len=100) :: out=""
    ! character(kind=c_char), pointer :: null_char

    ! create a NULL pointer of type char*
    ! call c_f_pointer(c_null_ptr, null_char)

    luaL_loadfile = luaL_loadfilex_ptr( lstate, trim(filename)//c_null_char, c_null_ptr )

    if( luaL_loadfile /= 0 ) then
       call lua_tostring( lstate,-1,out )
       ! @todo log error
       print*, trim(out)
    end if

  end function luaL_loadfile

  ! returns >0 in case of error
  function luaL_dofile(lstate, filename)
    integer(c_int) :: luaL_dofile
    type(c_ptr) :: lstate
    character(len=*) :: filename

    character(len=100) :: out

    luaL_dofile=luaL_loadfile(lstate,trim(filename))
    if( luaL_dofile /=0 ) then
       return
    end if

    luaL_dofile=lua_pcall(lstate,0,LUA_MULTRET,0)
    if( luaL_dofile /= 0 ) then
       call lua_tostring( lstate,-1,out )
       print*, trim(out)
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



!!!!!!!! Some high level lua calls

  recursive function get_recursively(f, desc) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: desc(:)
    type(c_ptr) :: L
    integer :: r
    r = 0

    L = f%lstate

    call lua_pushstring(L,trim(desc(1))//c_null_char)
    ! because we assume table is already on the top of the stack
    ! index is -2
    call lua_gettable(L,-2)
    if( size(desc) > 1 ) then
       if( lua_type(L,-1) == LUA_TTABLE ) then
          r = get_recursively(f, desc(2:))
       else if( lua_type(L,-1) == LUA_TNIL ) then
          r = 1
          call f%log( FPDE_LOG_ERROR, desc(1) // " is nil" )
          return
       else
          r = 1
          call f%log( FPDE_LOG_ERROR, desc(1) // " is not a table" )
          return
       end if
    end if
  end function get_recursively



  function push_by_key(f,id) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: id(:)
    character(len=100) :: str   !@bug, remove declaration later
    integer :: t, r
    type(c_ptr) :: L

    r = 0
    L = f%lstate

    ! bring the root of the table to the stack
    call lua_getglobal(L,trim(id(1))//c_null_char)
    t = lua_type(L,-1)
    ! call lua_tostring(L,-1,str)

    if( size(id) > 1 ) then
       if( t == LUA_TTABLE ) then
          r = get_recursively(f,id(2:))
       else
          r = 1
          call f%log( FPDE_LOG_ERROR, &
               id(1) // " is not a table" )
          return
       end if
    end if

    ! one way or another we should end up with a string or table
    if( (t /= LUA_TSTRING ) &
         .and. (t /= LUA_TTABLE)&
         .and. (t /= LUA_TNUMBER) ) then
       r = 1
       call f%log(FPDE_LOG_ERROR, &
            "type of " // trim(f%array_to_key(id)) // " is not supported")
       return
    end if

  end function push_by_key


  ! initializes lua vm
  subroutine init(f)
    class(flu) :: f
    f%lstate = luaL_newstate()
    f%name = "flu"

    if( c_associated( f%lstate ) ) then
       !@todo load libraries
       call f%log(FPDE_LOG_INFO, "Lua VM started")
    else
       call f%log(FPDE_LOG_ERROR, "unable to run Lua VM")
    end if

  end subroutine init

  ! stops lua vm
  subroutine free(f)
    class(flu) :: f
    call lua_close(f%lstate)
  end subroutine free


  ! loads and executes a file in lua vm
  subroutine load_file(f,filename)
    class(flu) :: f
    character(len=*), intent(in) :: filename
    ! @bug: len=100
    character(len=100) :: str
    integer :: status

    status = luaL_dofile(f%lstate, filename)

    if( status /= 0 ) then
       ! @todo log error
       return
    end if

  end subroutine load_file


  function get_character(f, id, out) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: id(:)
    character(len=*), intent(out) :: out
    ! @bug: fixed length string
    integer :: r

    ! get the variable to the top of the stack
    if( f%push_by_key(id) /= 0 ) then
       r = 1
       return
    else if( lua_type(f%lstate,-1) == LUA_TSTRING ) then
       call lua_tostring(f%lstate,-1,out)
       r = 0
       return
    else
       r = 1
       call f%log(FPDE_LOG_ERROR,&
            f%array_to_key(id)//" is not a string")
       return
    end if

  end function get_character


  function get_real(f, id, out) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: id(:)
    real, intent(out) :: out
    ! @bug: fixed length string
    character(len=100) :: str
    integer :: r

    ! get the variable to the top of the stack
    if( f%push_by_key(id) /= 0 ) then
       r = 1
       return
    else if( lua_type(f%lstate,-1) == LUA_TNUMBER ) then
       call lua_tostring(f%lstate,-1,str)
       read(str, *) out
       r = 0
       return
    else
       r = 1
       call f%log(FPDE_LOG_ERROR,&
            f%array_to_key(id)//" is not a number")
       return
    end if
  end function get_real

  ! writes length of the table designated by id to out
  function get_len(f, id, out) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: id(:)
    integer, intent(out) :: out
    ! @bug: fixed length string
    character(len=100) :: str
    integer :: r, i = 0, n = 0
    type(c_ptr) :: L

    L = f%lstate
    ! get the variable to the top of the stack
    if( f%push_by_key(id) /= 0 ) then
       r = 1
       return
    else if( lua_type(L,-1) == LUA_TTABLE ) then
       r = 0
       ! determine the length of the array
       out = lua_len(L, -1)
    else
       r = 1
       call f%log(FPDE_LOG_WARNING,&
            f%array_to_key(id)//" is not a table")
       return
    end if
  end function get_len


  function get_real_array(f, id, out) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: id(:)
    real, pointer, intent(out) :: out(:)
    ! @bug: fixed length string
    character(len=100) :: str
    integer :: r, i = 0, n = 0
    type(c_ptr) :: L

    L = f%lstate
    ! get the variable to the top of the stack
    if( f%push_by_key(id) /= 0 ) then
       r = 1
       return
    else if( lua_type(L,-1) == LUA_TTABLE ) then

       ! determine the length of the array
       n = lua_len(L, -1)
       ! pop the length
       call lua_pop(L,1)

       ! read all the numbers
       do i = 1, n
          ! call lua_pushstring(L,trim(itoa(i))//c_null_char)
          call lua_pushinteger(L,i)
          call lua_gettable(L,-2)
          if(lua_type(L,-1) == LUA_TNUMBER ) then
             call lua_tostring(L,-1,str)
             read(str, *) out(i)
             ! pop the result, so the table is back at index -1
             call lua_pop(L,1)
          else
             ! error, one of the elements is not a number
             r = 1
             call f%log(FPDE_LOG_ERROR,&
                  trim(f%array_to_key(id))//"["//trim(itoa(i))//"] is not a number")
             return
          end if
       end do
       r = 0
       return
    else
       r = 1
       call f%log(FPDE_LOG_ERROR,&
            f%array_to_key(id)//" is not a number")
       return
    end if
  end function get_real_array


  ! turns array of the form ["a", "b", "c"] to "a.b.c"
  function array_to_key(f,key) result(r)
    class(flu) :: f
    character(len=*) :: key(:)
    character(len=size(key)*(len(key)+len("."))) :: r

    r = join(key, ".")
  end function array_to_key

  function join(key, space) result(r)
    character(len=*) :: key(:)
    character(len=*) :: space
    character(len=size(key)*(len(key)+len(space))) :: r
    integer :: i

    r = key(1)

    do i = 2, size(key)
       r = trim(r)//"."//key(i)
    end do

  end function join

  function itoa(i) result(r)
    integer, intent(in) :: i
    ! possibly @bug, constant length character on output
    character(len=100) :: r

    write(r,'(i99)') i
    r=trim(adjustl(r))

  end function itoa


end module flu_module
