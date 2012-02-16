! todo: openmp compatibility

module data_module

  use logger_module

  private


  integer, parameter, private :: MAX_REG = 1000


  type, private :: init_register
     integer :: length = 0      ! length of vector to add, 0 means scalar
     integer :: number = 1      ! number of copies to add, additional
                                ! copies are named using the
                                ! convention name_1, name_2, etc.
     logical :: evolved = .true.
     logical :: vector = .true.  ! if .false. then its a scalar
     character(len=FPDE_NAME_LEN) :: name = "" ! not named by default
  end type init_register


  type, public :: named_vector
     real, pointer :: value(:) => null()
     character(len=FPDE_NAME_LEN) :: name = "" ! not named by default
  end type named_vector


  type, public :: named_scalar
     real, pointer :: value => null()
     character(len=FPDE_NAME_LEN) :: name = "" ! not named by default
  end type named_scalar


  type, public, extends(named) :: data
     real, pointer, contiguous :: data_table(:) => null()
     real, pointer :: evolved(:) => null()
     type(init_register) :: init_table(MAX_REG)
     type(named_vector), pointer :: vec(:) => null()
     type(named_scalar), pointer :: scal(:) => null()
   contains
     ! procedure :: add_vector
     ! procedure :: add_scalar
     ! procedure :: allocate
  end type data


contains


  subroutine add_vector()

  end subroutine add_vector


  subroutine add_scalar()

  end subroutine add_scalar


  subroutine allocate()

  end subroutine allocate


end module data_module

