module class_coordinates

  use class_regions
  use class_platonic
  use class_named_vector

  private

  type, public, abstract, extends(platonic) :: coordinates
     private
   contains
     procedure(b_i), deferred :: bregion
     procedure(v_i), deferred :: var
     procedure(d_i), deferred :: dim
     procedure(l_i), deferred :: length
  end type coordinates


  abstract interface
     function b_i(self, id)
       import coordinates
       class(coordinates) :: self
       integer, intent(in) :: id
       integer, pointer :: b_i(:)
     end function b_i

     function r_i(self, id)
       import coordinates
       class(coordinates) :: self
       integer, intent(in) :: id
       integer, pointer :: r_i(:)
     end function r_i

     function v_i(self, n)
       import coordinates, named_vector
       class(coordinates) :: self
       integer, intent(in) :: n
       class(named_vector), pointer :: v_i
     end function v_i

     function d_i(self)
       import coordinates
       class(coordinates) :: self
       integer :: d_i
     end function d_i

     function l_i(self)
       import coordinates
       class(coordinates) :: self
       integer :: l_i
     end function l_i

  end interface

end module class_coordinates
