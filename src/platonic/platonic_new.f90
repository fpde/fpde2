!>
!! @file   platonic_new.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Wed Apr  4 16:07:57 2012
!!
!! @brief
!!
!!
!!
module platonic_from_lua_module

  use flu_module
  use flu_get_module
  use class_platonic
  use constants_module
  ! use platonic_new_module

  interface
     !> Interface to platonic_new in platonic_new_module
     function platonic_new_module_mp_platonic_new(id, error) result(p)
       import platonic
       character(len=*), intent(in) :: id
       integer, optional, intent(out) :: error
       class(platonic), pointer :: p
     end function platonic_new_module_mp_platonic_new
  end interface

contains

  !> Tries to read type and name of an extended plationc object from
  !! lua state, allocates it and sets its type and name to appropriate
  !! values. After that, it calls from_lua of this object to capture
  !! the remaining, extension dependent, attributes.
  !!
  !! @param l lua state
  !! @param p pointer to platonic object to be created
  !! @param key is either, a global name or a key in a table, see index
  !!
  !! @param index|-1 if present and 0 then key is considered global
  !! variable, otherwise key is considered a key in a table contained
  !! at the given index
  !!
  !! @param error
  !!
  subroutine platonic_from_lua(l, p, key, index, error)
    type(flu) :: l
    class(platonic), pointer :: p
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index

    character(len=NAME_LEN) :: platonic_type
    ! class(*), pointer :: ptr
    integer :: err, idx

    if(present(error)) error = FPDE_STATUS_ERROR

    if(present(index)) then
       idx = index
    else
       idx = -1
    end if

    call flu_get(l, idx, key, error = err)

    if( err == FPDE_STATUS_OK ) then
       if( lua_type(l, -1) == C_LUA_TTABLE) then
          call flu_get_atomic(l, &
               val = platonic_type, key = TAG_TYPE, error = err)
          if ( err /= FPDE_STATUS_OK ) then
             return
          else
             p => platonic_new_module_mp_platonic_new(&
                  platonic_type, error = err)
             if( err /= FPDE_STATUS_OK ) then
                call p%log(FPDE_LOG_ERROR, &
                     "Type not recognized: ["//trim(platonic_type)//"]")
                return
             else
                p % type = platonic_type
                p % name = ""
                call flu_get_atomic(l,&
                     val = p%name, key = TAG_NAME)
             end if
          end if
       end if

       call p%from_lua(l, error = err)

       if( err == FPDE_STATUS_OK ) then
          call p%init( error = err )
          if ( err == FPDE_STATUS_OK ) then
             if(present(error)) error = FPDE_STATUS_OK
          end if
       end if

    end if

    call lua_pop(l,1)

  end subroutine platonic_from_lua

end module platonic_from_lua_module
