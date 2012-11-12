module class_boundary_ghost

  use class_boundary
  use constants_module

  private

  type, public, abstract, extends(boundary) :: boundary_ghost
     private
   contains
     procedure(gen_val), deferred :: generate_values
     procedure :: p_names
  end type boundary_ghost

  interface
     subroutine gen_val(self, fin, fout, xin, params, error)
       import boundary_ghost
       class(boundary_ghost) :: self
       real, intent(in) :: params(:)
       real, intent(in) :: fin(:), xin(:)
       real, intent(out) :: fout(:)
       integer, intent(out), optional :: error
     end subroutine gen_val
  end interface

  public :: toghost

contains

  function p_names(self)
    class(boundary_ghost) :: self
    character(len=:), allocatable :: p_names(:)
    allocate( character(len=0) :: p_names(0) )
  end function p_names


  function toghost(b) result(r)
    class(boundary), target :: b

    class(boundary_ghost), pointer :: r

    select type(b)
    class is(boundary_ghost)
       r => b
    class default
       call b%loge("toghost(): Unable to convert to boundary_ghost")
    end select

  end function toghost


end module class_boundary_ghost
