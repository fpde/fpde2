!>
!! @file   platonic_factory.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Wed Apr  4 16:04:09 2012
!!
!! @brief This module is a dummy module, it is not intended to be used
!! by any other module or program directly, but contains a function
!! plationc_new used by platonic_from_lua_module. platonic_new can be
!! used only by declaring an interface like in
!! platonic_from_lua_module, which is a hack to avoid fortran's flaw
!! (feature?) prohibiting circular dependencies.
!!
!! @todo Later on, this should be fixed in some other manner.
!!
module platonic_new_module

contains

  !> This is a factory of platonic objects. If the object type is
  !! recognized, an apropriate factory is called to produce this
  !! object. Otherwise, type(platonic) is produces
  !!
  !! @param id type of object to create (e.g. "solver_simple")
  !! @param error
  !!
  !! @return pointer to an allocated object
  !!
  function platonic_new(id, error) result(p)

    use constants_module
    use class_platonic
    use solver_factory
    use boundary_factory

    character(len=*), intent(in) :: id
    integer, optional, intent(out) :: error

    integer :: pos = 0
    ! major type (or class), explained below
    character(len=NAME_LEN) :: major = ""
    class(platonic), pointer :: p

    if(present(error)) error = FPDE_STATUS_OK

    if(associated(p)) then
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! select a substring before first '_', so with id="solver_abc" or
    ! id="solver" major shall be major="solver"
    pos = scan(id,"_")
    if( pos == 0 ) then
       major = id
    else
       major = id(1 : pos-1)
    end if

    ! this selects the factory based on type name
    select case(trim(major))

       ! call solver_new for solver types
    case( "solver" )
       p => solver_new(id)

    case( "boundary" )
       p => boundary_new(id)

       ! default behavior of factory is to nullify, but here we make an
       ! exception and allocate platonic type instead. It will be used to
       ! log an error.
    case default
       nullify(p)
    end select

    if( .not. associated(p) ) then
       if(present(error)) error = FPDE_STATUS_ERROR
       allocate(p)
    end if

  end function platonic_new

end module platonic_new_module
