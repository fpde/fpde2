!>
!! @file   flu_get.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Thu Mar 29 19:12:30 2012
!!
!! @brief Higher level functions used to obtain Lua gloabal variables
!! and elements from Lua tables.
!!
!! The implementation is quite tricky, and uses Fortran 2008
!! features. Some of them are not yet supported by ifort or the support
!! is buggy.
!!
module flu_get_module

  use flu_module
  use constants_module

  private

  ! @todo to be removed
  public :: flu_get_atomic

  public :: flu_get

  interface flu_get
     module procedure :: flu_get_scalar_integer
     module procedure :: flu_get_scalar_real
     module procedure :: flu_get_scalar_character
     module procedure :: flu_get_scalar_logical
     module procedure :: flu_get_array1d_real
     module procedure :: flu_get_array2d_real
  end interface flu_get

contains

  !> Combines lua_getglobal and lua_getfield
  !!
  !! @param key name of field or global variable
  !!
  !! @param index if 0 then lua_getglobal(l,key) is called, otherwise
  !! it's lua_getfield(l,index,key)
  !!
  !! @param error FPDE_STATUS_ERROR if field was nil, FPDE_STATUS_OK otherwise
  subroutine flu_get_combined(l, index, key, error)
    type(flu) :: l
    integer, intent(in) :: index
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( index == 0 ) then
       call lua_getglobal( l, key )

    else if( lua_type(l,index) == C_LUA_TTABLE ) then
       call lua_getfield( l, index, key )

    else
       call l%log(FPDE_LOG_INFO,&
            "There is no table at the stack index passed to flu_get")
       ! push nil to the stack
       call lua_pushnil( l )
       return

    end if

    ! if getglobal or getfield was called, check if the value is not
    ! nil, if it is the case, pop it, otherwise set error to OK
    if( lua_isnil(l, -1) ) then
       call l%log(FPDE_LOG_INFO,&
            "No global name or field ["//trim(key)//"] exists")
    else
       if(present(error)) error = FPDE_STATUS_OK
    end if

  end subroutine flu_get_combined


  subroutine flu_get_atomic(l,  index, key, char, val, val1d, val2d, error)
    type(flu) :: l
    integer, optional, intent(in) :: index
    character(len=*), optional, intent(in) :: key
    integer, optional, intent(out) :: error

    class(*), optional :: val
    class(*), optional :: val1d(:)
    class(*), optional :: val2d(:,:)
    character(len=*), optional :: char
    integer :: err, idx
    logical :: pushed = .false.

    call l%loge("Warning! flu_get_atomic() doesn't work anymore! Use flu_get_raw() or flu_get_by_key() instead!")
    if( present(error) ) error = FPDE_STATUS_ERROR

  end subroutine flu_get_atomic


  !! @todo warn if the field contains non-integer number
  subroutine flu_get_scalar_integer( l, index, i, error )
    type(flu) :: l
    integer, intent(out) :: i
    integer, intent(in) :: index
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TNUMBER) then
       i = lua_tointeger(l, index)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%loge("Invalid type, number expected")
    end if

  end subroutine flu_get_scalar_integer


  subroutine flu_get_scalar_real( l, index, r, error )
    type(flu) :: l
    real, intent(out) :: r
    integer, intent(in) :: index
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TNUMBER) then
       r = lua_tonumber(l, index)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%loge("Invalid type, number expected")
    end if

  end subroutine flu_get_scalar_real


  subroutine flu_get_scalar_logical( l, index, r, error )
    type(flu) :: l
    logical, intent(out) :: r
    integer, intent(in) :: index
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TBOOLEAN ) then
       r = lua_toboolean(l, index)
       if(present(error)) error = FPDE_STATUS_OK
    else
       call l%loge("Invalid type, boolean expected")
    end if

  end subroutine flu_get_scalar_logical


  subroutine flu_get_scalar_character( l, index, str, error )
    type(flu) :: l
    integer, intent(in) :: index
    character(len=*), intent(out) :: str
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( lua_type(l, index) == C_LUA_TSTRING) then
       call lua_tostring(l, index, str)
       if(present(error)) error = FPDE_STATUS_OK
       return
    else
       call l%loge("Invalid type, string expected")
    end if

  end subroutine flu_get_scalar_character


  subroutine flu_get_array1d_real( l, index, table, error )
    type(flu) :: l
    integer, intent(in) :: index
    real, allocatable, intent(out) :: table(:)
    integer, optional, intent(out) :: error

    integer :: idx, n, i
    logical :: isnumber = .true.
    character(100) :: str
    real :: ti

    idx = lua_absindex(l,index)

    ! default to error
    if( present(error) ) error = FPDE_STATUS_ERROR

    if( lua_type(l, idx) /= C_LUA_TTABLE ) then
       call l%loge("Invalid type, table expected")
       return
    end if

    n = lua_rawlen(l,idx)

    ! clear the table
    table = [real::]

    do i = 1, n
       call lua_pushinteger(l,i) ! i items on the stack
       call lua_gettable(l,idx)

       ! if any of the entries is a non-number return an error
       if( lua_type(l, -1) /= C_LUA_TNUMBER ) then
          write(str,*) i
          call l%loge("Expected number at position &
               &["//trim(adjustl(str))//"] in table.")
          ! undo the pushes
          call lua_pop(l,i)
          return
       end if

       ! if its a number grow the table
       call flu_get_scalar_real(l,-1,ti)
       table = [table, ti]

    end do

    call lua_pop(l,n)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine flu_get_array1d_real


  subroutine flu_get_array2d_real(l, index, table, error)
    type(flu) :: l
    integer, intent(in) :: index
    real, allocatable, intent(out) :: table(:,:)
    integer, optional, intent(out) :: error

    integer :: idx, n, m, i, err
    logical :: isnumber = .true.
    character(100) :: str
    real, allocatable :: ti(:), table_(:)

    idx = lua_absindex(l,index)

    ! default to error
    if( present(error) ) error = FPDE_STATUS_ERROR

    if( lua_type(l, idx) /= C_LUA_TTABLE ) then
       call l%loge("Invalid type, table expected")
       return
    end if

    n = lua_rawlen(l,idx)

    ! clear the table
    table  = reshape([real::],[0,0])
    table_ = [real::]

    do i = 1, n
       call lua_pushinteger(l,i) ! i items on the stack
       call lua_gettable(l,idx)

       ! if any of the entries is a non-number return an error
       if( lua_type(l, -1) /= C_LUA_TTABLE ) then
          write(str,*) i
          call l%loge("Expected table at position &
               &["//trim(adjustl(str))//"].")
          ! undo the pushes
          call lua_pop(l,i)
          return
       end if

       ! try to fetch the subvector ti
       call flu_get_array1d_real(l,-1,ti,err)

       if( err /= FPDE_STATUS_OK ) then
          write(str,*) i
          call l%loge("Unable to fetch row &
               &["//trim(adjustl(str))//"].")
          ! undo the pushes
          call lua_pop(l,i)
       end if

       ! set the size of ti from the first row
       if( i == 1 ) m  = size(ti)

       if( m /= size(ti) ) then
          write(str,*) i
          call l%loge("Length of row ["//trim(adjustl(str))&
               &//"] doesn't match the previous rows.")
          ! undo the pushes
          call lua_pop(l,i)
       end if

       table_=[table_,ti]
       table = reshape(table_,[i,m])

    end do

    call lua_pop(l,n)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine flu_get_array2d_real



end module flu_get_module
