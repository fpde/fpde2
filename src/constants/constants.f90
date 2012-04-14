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



end module constants_module
