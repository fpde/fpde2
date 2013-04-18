!>
!! @file   platonic.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Wed Apr  4 16:12:58 2012
!!
!! @brief  Contains a type definition of a plationic object.
!!
module class_platonic

  use constants_module
  use logger_module
  use flu_module
  use flu_get_module

  private

  !> platonic object is a basic object intended to be used with
  !! fpde. It is suited to be read off from Lua using from_lua()
  !! function, and then initialized using init function.
  !!
  !! Means of creating platonic object from scratch are provided by
  !! platonic_from_lua_module, see its documentation for further
  !! instructions.
  !!
  type, public, extends(named) :: platonic
     !> type of platonic object, usually set and used by
     !! platonic_from_lua()
     character(len=NAME_LEN) :: type = "platonic"
   contains
     !> Initializes an object assuming all the necessery internal
     !! variables have been defined (init takes no arguments apart
     !! from optional error status)
     procedure :: init
     !> Tries to deallocate object
     procedure :: free
     !> Loads necessary internal variables from Lua
     procedure :: from_lua
  end type platonic

contains

  subroutine init(self, error)
    class(platonic) :: self
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine init

  subroutine free(self, error)
    class(platonic) :: self
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
  end subroutine free

  subroutine from_lua(self, l, error)
    class(platonic) :: self
    type(flu) :: l
    integer, optional, intent(out) :: error

    ! get name
    call flu_get_atomic( l,&
         char = self%name,&
         key = TAG_NAME)

    if(present(error)) error = FPDE_STATUS_OK
  end subroutine from_lua

end module class_platonic
