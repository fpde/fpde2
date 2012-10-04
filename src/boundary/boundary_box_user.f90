module class_bbox_user

  use class_platonic
  use class_boundary
  use class_named_vector_user_

  private

  type, public, abstract, extends(platonic) :: bbox_user
   contains
     procedure(boundary_i), deferred  :: boundary
     procedure(param_i), deferred     :: param
     procedure(num_param_i), deferred :: num_param
  end type bbox_user

  interface
     function boundary_i(self, x, side)
       import bbox_user, boundary
       class(bbox_user), intent(in) :: self
       integer, intent(in) :: x, side
       class(boundary), pointer :: boundary_i
     end function boundary_i

     function param_i(self, x, side, num)
       import bbox_user, named_vector_user
       class(bbox_user), intent(in), target :: self
       integer, intent(in) :: x, side, num

       class(named_vector_user), pointer :: param_i
     end function param_i

     function num_param_i(self, x, side)
       import bbox_user
       class(bbox_user), intent(in), target :: self
       integer, intent(in) :: x, side

       integer :: num_param_i
     end function num_param_i

  end interface

end module class_bbox_user
