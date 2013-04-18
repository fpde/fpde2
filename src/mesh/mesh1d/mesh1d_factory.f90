module mesh1d_factory

  use constants_module
  use class_mesh1d
  use class_mesh1d_sfd3pt

  private

  character(len=*), parameter :: tprefix = "mesh1d_"
  character(len=*), parameter :: nprefix = "mesh1d_"

  public :: mesh1d_new

contains

  function mesh1d_new(id, error) result(s)
    class(mesh1d), pointer :: s
    integer, optional, intent(out) :: error
    character(len=*) :: id

    if(present(error)) error = FPDE_STATUS_OK

    select case(trim(id))
    case( "sfd3pt" )
       allocate( mesh1d_sfd3pt :: s )
    case default
       if(present(error)) error = FPDE_STATUS_ERROR
       nullify( s )
       return
    end select

    if( .not. associated(s) ) then
       s%name = nprefix // id
       s%type = tprefix // id
    end if

  end function mesh1d_new

end module mesh1d_factory
