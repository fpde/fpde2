module class_function

  use platonic_from_lua_module
  use constants_module
  use class_platonic
  use boundary_box_module
  use class_boundary
  use flu_get_module
  use flu_module
  use helper_module

  private

  character(len=NAME_LEN), parameter, public :: &
       TAG_FUNC_EVOLVED = "evolved",&
       TAG_FUNC_DERIVATIVE = "derivatives",&
       TAG_FUNC_SCALAR = "scalar",&
       TAG_FUNC_SPATIAL = "spatial",&
       TAG_FUNC_TEMPORAL = "temporal",&
       TAG_FUNC_BOUNDARY = "boundary"

  type, public, extends(platonic) :: func
     !>
     !! treat this function as a scalar?
     !!
     logical :: scalar = .false.
     !>
     !! treat this function as it is to be evolved?
     !!
     logical :: evolved = .true.
     !>
     !! treat this funtcion as a time-like independent variable?
     !!
     logical :: temporal = .false.
     !>
     !! treat this function as a space-like indepenedent variable?
     !!
     logical :: spatial = .false.
     !>
     !!  boundary conditions for this function
     !!
     type(boundary_box) :: boundary
     !>
     !! pointer to discretized values of this function.
     !! It should match the values of spatial functions, so that
     !! f%val(n) corresponds to f(x%val(n),y%val(n),z%val(n), ...)
     !!
     real, pointer :: val(:) => null()
     !>
     !! table of derivatives, the convention is such that
     !! derivatives(:,n) is a multindex describing the derivative, so
     !! to describe \$\partial_{x,y}\$ we would use derivatives(:,n)
     !! == ["x","y","","", ... ,""]
     !!
     character(len=NAME_LEN), pointer :: derivatives(:,:) => null()
   contains
     procedure :: from_lua
     procedure :: info
     procedure :: d_name
  end type func

contains

  subroutine from_lua(p, l, error)
    class(func) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    integer :: err = FPDE_STATUS_OK, err2, der_num = 0
    character(len=100) :: name
    class(platonic), pointer :: ptr

    ! get name
    call flu_get_atomic( l,&
         char = p%name,&
         key = TAG_NAME,&
         error = err2 )

    if(err2 /= FPDE_STATUS_OK) then
       err = err2
       call p%log( FPDE_LOG_ERROR,&
            "Unnamed function")
    end if

    ! calculate the length of derivatives vector and allocate it
    call flu_get( l,&
         index = -1,&
         key   = TAG_FUNC_DERIVATIVE,&
         error = err2 )

    if( err2 == FPDE_STATUS_OK ) then
       if( lua_type(l,-1) == C_LUA_TTABLE ) then
          allocate(p%derivatives(MAX_RK,lua_rawlen(l,-1)))
          p%derivatives = ""
          ! get derivatives vector
          call flu_get_atomic( l,&
               val2d = p%derivatives,&
               error = err2 )
          ! check the err2
       else
          err = FPDE_STATUS_ERROR
          call p%log( FPDE_LOG_ERROR,&
               "Error reading derivatives")
       end if
    else
       ! if no derivatives were defined, allocate zero length arr
       allocate(p%derivatives(MAX_RK, 0))
    end if

    call lua_pop(l,1)


    call flu_get_atomic( l,&
         val = p%scalar,&
         key = TAG_FUNC_SCALAR)

    call flu_get_atomic( l,&
         val = p%spatial,&
         key = TAG_FUNC_SPATIAL)

    call flu_get_atomic( l,&
         val = p%temporal,&
         key = TAG_FUNC_TEMPORAL)

    ! to be moved to a lua script
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

    if( .not. p%scalar .and. size(p%derivatives) > 0 ) then
       call flu_get(l,-1,TAG_FUNC_BOUNDARY, err2)
       if(err2 /= FPDE_STATUS_OK) err = err2

       if( err == FPDE_STATUS_OK) then
          call p%boundary%from_lua(l,err)
       end if

       if (err /= FPDE_STATUS_OK) then
          call p%log(FPDE_LOG_ERROR,&
               "Bounadry conditions could not be determined")
       end if

       call lua_pop(l,1)
    end if

    if(present(error)) error = err

  end subroutine from_lua

  subroutine info(p)
    class(func) :: p

    print '("name: ",g0)', p%name
    ! print '("derivatives =",*(a1,:," "))', p%derivatives
    print '("scalar: ",g0)', p%scalar
    print '("evolved: ",g0)', p%evolved
    print '("temporal: ",g0)', p%temporal
    print '("spatial: ",g0)', p%spatial
  end subroutine info

  function d_name(p,alpha) result(r)
    class(func) :: p
    character(len=*) :: alpha(:)
    character(len=NAME_LEN) :: r

    r = "D(" // trim(p%name) // "," // trim(join(alpha,",")) // ")"

  end function d_name

end module class_function
