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
  use flu_primitives_module
  use flu_func_module
  use flu_meta_module
  use logger_module

  private


  !> this class takes care of the lua VM
  type, public, extends(named) :: flu
     type(c_ptr) :: lstate = c_null_ptr
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

contains

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


  !> pushes the value of the key f to the top of the stack
  function push_by_key(f,id) result(r)
    class(flu) :: f
    character(len=*), intent(in) :: id(:)
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
       call luaL_openlibs( f%lstate )
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
    character(len=100) :: out
    integer :: status

    status = luaL_dofile(f%lstate, filename)

    if( status /= 0 ) then
       call lua_tostring( f%lstate,-1,out )
       call f%log(FPDE_LOG_ERROR, trim(out))
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

  ! @todo extract to some utils library
  function join(key, space) result(r)
    character(len=*) :: key(:)
    character(len=*) :: space
    character(len=size(key)*(len(key)+len(space))) :: r
    integer :: i

    r = key(1)

    do i = 2, size(key)
       ! if( len(trim(key(1))) == 0) continue
       r = trim(r)//space//key(i)
    end do

  end function join

  ! @todo extract to some utils library
  function itoa(i) result(r)
    integer, intent(in) :: i
    ! possibly @bug, constant length character on output
    character(len=100) :: r

    write(r,'(i99)') i
    r=trim(adjustl(r))

  end function itoa


end module flu_module
