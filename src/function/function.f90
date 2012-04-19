module class_function

  use platonic_from_lua_module
  use constants_module
  use class_platonic
  use boundary_lr_module
  use class_boundary
  use flu_get_module
  use flu_module

  private

  character(len=NAME_LEN), parameter, public :: &
       TAG_FUNC_EVOLVED = "evolved",&
       TAG_FUNC_DERIVATIVE = "derivatives",&
       TAG_FUNC_SCALAR = "scalar",&
       TAG_FUNC_SPATIAL = "spatial",&
       TAG_FUNC_TEMPORAL = "temporal",&
       TAG_FUNC_BOUNDARY = "boundary"

  integer, parameter :: &
       MAX_PARAMETERS = 10

  type, public, extends(platonic) :: func
     logical :: scalar = .false.
     logical :: evolved = .true.
     logical :: temporal = .false.
     logical :: spatial = .false.
     type(boundary_lr) :: bdry
     ! class(boundary), pointer :: bdry_default => null()
     real, pointer :: val(:) => null()
     character(len=NAME_LEN), pointer :: derivatives(:,:) => null()
     ! integer, pointer :: derivatives(:,:) => null()
   contains
     procedure :: from_lua
  end type func

contains

  subroutine from_lua(p, l, error)
    class(func) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    integer :: err = FPDE_STATUS_ERROR
    character(len=100) :: name
    class(platonic), pointer :: ptr

    ! get name
    call flu_get_atomic( l,&
         char = p%name,&
         key = TAG_NAME,&
         error = err )

    ! calculate the length of derivatives vector and allocate it
    call lua_getfield(l,-1,TAG_FUNC_DERIVATIVE)
    allocate(p%derivatives(MAX_RK,flu_get_len(l,-1)))
    call lua_pop(l,1)

    ! get derivatives vector
    call flu_get_atomic( l,&
         val2d = p%derivatives,&
         key = TAG_FUNC_DERIVATIVE,&
         error = err )

    call flu_get_atomic( l,&
         val = p%scalar,&
         key = TAG_FUNC_SCALAR,&
         error = err )

    call flu_get_atomic( l,&
         val = p%spatial,&
         key = TAG_FUNC_SPATIAL,&
         error = err )

    call flu_get_atomic( l,&
         val = p%temporal,&
         key = TAG_FUNC_TEMPORAL,&
         error = err )

    if(p%temporal) then
       p%spatial = .false.
       p%scalar = .true.
       p%evolved = .false.
    end if

    if(p%spatial) then
       p%temporal = .false.
       p%scalar = .false.
       p%evolved = .false.
    end if

    if( .not. p%scalar ) then
       call flu_get(l,-1,TAG_FUNC_BOUNDARY, err)

       if( err == FPDE_STATUS_OK) then
          call p%bdry%from_lua(l,err)
          call lua_pop(l,-1)
       end if

       if (err /= FPDE_STATUS_OK) then
          call p%log(FPDE_LOG_ERROR,&
               "Bounadry conditions could not be determined")
       end if

    end if

    if(present(error)) error = err

  end subroutine from_lua

end module class_function
