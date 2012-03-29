module solver_flu_module
  use constants_module
  use flu_module
  use flu_get_module
  use class_solver
  use solver_factory

  private

  character(len=NAME_LEN), parameter :: TAG_TYPE = "type"

  public :: flu_get_solver

contains


  subroutine flu_get_solver(l, s, key, index, error)
    type(flu) :: l
    class(solver), pointer :: s
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: error
    integer, optional, intent(in) :: index

    character(len=NAME_LEN) :: solver_type

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
          call flu_get_scalar_character(l, solver_type, TAG_TYPE, error = err)
          if ( err /= FPDE_STATUS_OK ) then
             return
          else
             s => solver_new(solver_type)
             if( .not. associated(s) ) then
                return
             end if
          end if
       end if

       call s%from_lua(l, error = err)

       if( err == FPDE_STATUS_OK ) then
          call s%init( error = err )
          if ( err == FPDE_STATUS_OK ) then
             if(present(error)) error = FPDE_STATUS_OK
          end if
       end if

    end if

    call lua_pop(l,1)

  end subroutine flu_get_solver


end module solver_flu_module
