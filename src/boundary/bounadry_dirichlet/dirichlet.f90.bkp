module class_boundary_dirichlet

  use constants_module
  use class_boundary
  use flu_get_module
  use flu_module

  private

  character(len=NAME_LEN), parameter :: TAG_A = "a"

  type, public, extends(boundary) :: boundary_dirichlet
     private
     !> diriclet boundary condition with u(0) = a
     real :: a = 0.0
   contains
     procedure :: generate_values
     procedure :: from_lua
  end type boundary_dirichlet

contains

  !>
  !! \$ f_0 = f_2 \$, \$ f_{-1} = f_3 \$, etc.
  !!
  subroutine generate_values(this, from, to, error)
    class(boundary_dirichlet) :: this
    integer, intent(out), optional :: error
    real, intent(in) :: from(:)
    real, intent(out) :: to(0:) ! align the table to start from 0

    if(present(error)) error = FPDE_STATUS_OK

    to(0:) = this%a-from(2:)

  end subroutine generate_values

  subroutine from_lua(p, l, error)
    class(boundary_dirichlet) :: p
    type(flu)   :: l
    integer, optional, intent(out) :: error

    call flu_get_atomic(l,&
         val = p%a,&
         key = TAG_A)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine from_lua

end module class_boundary_dirichlet
