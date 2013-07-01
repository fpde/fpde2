module class_derivator

  use class_platonic
  use class_coordinates
  use class_named_vector_f

  private

  type, public, abstract, extends(platonic) :: derivator
   contains
     procedure(dx_i), deferred :: dx
  end type derivator


  abstract interface

     function coordinates_i(self)
       import derivator, coordinates
       class(derivator) :: self
       class(coordinates), pointer :: coordinates_i
     end function coordinates_i

     subroutine dx_i(self, f, alpha)
       import derivator, named_vector_f
       class(derivator) :: self
       class(named_vector_f), target :: f
       integer, intent(in) :: alpha(:,:)
     end subroutine dx_i

     subroutine initialize_x_i(self)
       import derivator
       class(derivator) :: self
     end subroutine initialize_x_i

  end interface

end module class_derivator
