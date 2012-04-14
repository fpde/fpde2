module boundary_lr_module

  use constants_module
  use class_boundary
  use class_platonic
  use platonic_from_lua_module
  use flu_module
  use flu_get_module

  private

  character(len=NAME_LEN), parameter :: &
       TAG_BDRY_DEFAULT = "default",&
       TAG_BDRY_SPECIFIC = "specific"

  type :: entry
     character(len = NAME_LEN) :: varname = ""
     class(boundary), pointer :: left => null()
     class(boundary), pointer :: right => null()
  end type entry


  type, public, extends(platonic) :: boundary_lr
     type(entry), pointer :: entries(:) => null()
     class(boundary), pointer :: default => null()
   contains
     procedure :: from_lua
  end type boundary_lr

contains

  subroutine get_boundary(p, varname, left, right, error)
    class(boundary_lr) :: p
    character(len=*), intent(in) :: varname
    class(boundary), pointer, intent(out), optional :: left, right
    integer, intent(out), optional :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_ERROR

    if(associated(p%entries)) then
       ! @todo use 'findloc' from fortran 2008 when it is implemented in
       ! ifort
       do i = 1, size(p%entries)
          associate(blr => p%entries(i))
            if( blr%varname == varname ) then
               if(present(left)) left => blr%left
               if(present(right)) right => blr%right
               if(present(error)) error = FPDE_STATUS_OK
               return
            else if( blr%varname == "") then
               return
            end if
          end associate
       end do
    end if

    if(.not. associated(p%default) ) then
       return
    else
       left => p%default
       right => p%default
       if(present(error)) error = FPDE_STATUS_OK
    end if

  end subroutine get_boundary

  subroutine from_lua(p, l, error)
    class(boundary_lr) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    integer :: err
    class(platonic), pointer :: ptr

    ! determine if there is a field named default
    call flu_get(l,-1,TAG_BDRY_DEFAULT,err)
    if( err == FPDE_STATUS_OK ) then
       call lua_pop(l,1)
       call platonic_from_lua(l, &
            ptr,&
            key = "default", &
            index = -1,&
            error = err)
       if( err == FPDE_STATUS_OK ) then
          select type(ptr)
             class is(boundary)
             p%default => ptr
          end select
       else
          call p%log(FPDE_LOG_ERROR,&
               "Unrecognized boundary conditions type")
       end if
    end if

    if(present(error)) error = err

    ! determine if there is a field named default
    call flu_get(l,-1,TAG_BDRY_SPECIFIC,err)
    if( err == FPDE_STATUS_OK ) then
       call lua_pop(l,1)
       call p%log(FPDE_LOG_WARNING,&
            "Specific boundary conditions&
            & are not yet implemented and are ignored")
    end if

    end subroutine from_lua

end module boundary_lr_module
