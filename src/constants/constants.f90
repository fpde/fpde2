module constants_module

  public

  integer, parameter ::&
       FPDE_STATUS_OK = 0,&
       FPDE_STATUS_ERROR = 1

  integer, parameter ::&
       NAME_LEN = 100

  integer, parameter ::&
       MAX_DIM = 10

  !> Lua tags
  character(len=NAME_LEN), parameter ::&
       TAG_TYPE = "type",&
       TAG_NAME = "name"

  !> maximal number of functions
  integer, parameter :: MAX_FUNC = 20

  !> maximal number of functions
  integer, parameter :: MAX_RK = 5


  character(len=*), parameter, public :: &
       icw_spatial    = "spa",&
       icw_dependent  = "dep",&
       icw_temporal   = "tem",&
       icw_derivative = "der",&
       icw_boundary   = "bdr"

  character(len=*), parameter, public :: &
       icw_derivative_name = "D",&
       icw_boundary_name   = "B"

  character(len=*), parameter, public :: &
       icw_dir_left  = "L",&
       icw_dir_right = "R"



end module constants_module
