module class_ghost_boundary_box_user

  use class_named_vector_user_
  use class_boundary
  use class_platonic

  private

  type, public, abstract, extends(platonic) :: ghost_boundary_box_user
   contains
     procedure(boundary_i), deferred  :: boundary
     procedure(param_i), deferred     :: param
     procedure(num_param_i), deferred :: num_param
  end type ghost_boundary_box_user

  interface
     function boundary_i(self, x, side)
       import ghost_boundary_box_user, boundary
       class(ghost_boundary_box_user), intent(in) :: self
       integer, intent(in) :: x, side
       class(boundary), pointer :: boundary_i
     end function boundary_i

     function param_i(self, x, side, num)
       import ghost_boundary_box_user, named_vector_user
       class(ghost_boundary_box_user), intent(in), target :: self
       integer, intent(in) :: x, side, num

       class(named_vector_user), pointer :: param_i
     end function param_i

     function num_param_i(self, x, side)
       import ghost_boundary_box_user
       class(ghost_boundary_box_user), intent(in), target :: self
       integer, intent(in) :: x, side

       integer :: num_param_i
     end function num_param_i

  end interface

end module class_ghost_boundary_box_user
