module class_derivator

  use class_platonic
  use class_coordinates
  use class_bbox
  use class_named_vector_f

  private

  type, public, abstract, extends(platonic) :: derivator
   contains
     procedure(coordinates_i), deferred :: coordinates
     procedure(bbox_i), deferred :: bbox
     procedure(dx_i), deferred :: dx
     procedure(initialize_x_i), deferred :: initialize_x
  end type derivator


  abstract interface

     function coordinates_i(self)
       import derivator, coordinates
       class(derivator) :: self
       class(coordinates), pointer :: coordinates_i
     end function coordinates_i

     function bbox_i(self, btypes)
       import derivator, bbox
       class(derivator) :: self
       character(len=*), intent(in) :: btypes(:)
       class(bbox), pointer :: bbox_i
     end function bbox_i

     subroutine dx_i(self, f, alpha)
       import derivator, named_vector_f
       class(derivator) :: self
       class(named_vector_f) :: f
       integer, intent(in) :: alpha(:,:)
     end subroutine dx_i

     subroutine initialize_x_i(self)
       import derivator
       class(derivator) :: self
     end subroutine initialize_x_i

  end interface

end module class_derivator
