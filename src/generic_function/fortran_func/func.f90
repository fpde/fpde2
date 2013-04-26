module class_generic_function_fortran

  use class_generic_function
  use class_icicles_user
  use constants_module

  private

  type, public, extends(generic_function) :: generic_function_fortran
     private
     procedure(fptr_i), pointer, nopass :: call_ => null()
   contains
     procedure :: call
  end type generic_function_fortran


  abstract interface
     subroutine fptr_i(icu)
       import icicles_user
       class(icicles_user), target :: icu
     end subroutine fptr_i
  end interface


  interface generic_function_fortran
     module procedure :: gff_new
  end interface generic_function_fortran

contains

  function gff_new(fptr) result(r)
    procedure(fptr_i) :: fptr

    class(generic_function_fortran), pointer :: r

    allocate(r)
    r%call_ => fptr

  end function gff_new


  subroutine call(this, solver, error)
    class(generic_function_fortran) :: this
    class(icicles_user), target :: solver
    integer, optional, intent(out) :: error

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( .not. associated(this%call_) ) return

    call this%call_(solver)

    if( present(error) ) error = FPDE_STATUS_OK

  end subroutine call


end module class_generic_function_fortran
