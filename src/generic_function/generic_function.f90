!>
!! @file   generic_function.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Jul 20 13:54:45 2012
!!
!! @brief
!!
!!
!!
module class_generic_function

  use class_platonic

  private

  type, public, abstract, extends(platonic) :: generic_function
   contains
     procedure(call_interface), deferred :: call
  end type generic_function

  interface
     subroutine call_interface(this, solver, error)
       import generic_function
       class(generic_function) :: this
       class(*) :: solver
       integer, optional, intent(out) :: error
     end subroutine call_interface
  end interface

end module class_generic_function
