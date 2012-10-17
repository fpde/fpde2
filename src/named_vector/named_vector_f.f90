module class_named_vector_f

  use class_icicles_user_
  use class_bbox_user
  use class_named_vector_user_
  use class_named_vector_implementation_

  private

  type, public, abstract, extends(named_vector_implementation)&
       :: named_vector_f_user
     private
   contains
     procedure(dx_i), deferred :: dx
     procedure(dt_i), deferred :: dt
     procedure(b_i), deferred  :: b
  end type named_vector_f_user


  type, public, abstract, extends(named_vector_f_user) :: named_vector_f
     private
   contains
     ! procedure(num_boundary_param_i), deferred     :: num_boundary_param
     ! procedure(update_boundary_param_i ), deferred :: update_boundary_param
     procedure(bbox_i), deferred                   :: bbox
  end type named_vector_f


  interface

     function b_i(self, var, side, param)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: var, side, param

       class(named_vector_user), pointer :: b_i
     end function b_i


     function num_boundary_param_i(self, var, side)
       import named_vector_f, named_vector_user
       class(named_vector_f) :: self
       integer, intent(in) :: var, side

       integer :: num_boundary_param_i
     end function num_boundary_param_i


     function dx_i(self, alpha)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: alpha(:)

       class(named_vector_user), pointer :: dx_i
     end function dx_i


     function dt_i(self)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self

       class(named_vector_user), pointer :: dt_i
     end function dt_i


     subroutine update_boundary_param_i(self, ic)
       import named_vector_f, icicles_user
       class(named_vector_f) :: self
       class(icicles_user), target :: ic
     end subroutine update_boundary_param_i


     function bbox_i(self)
       import named_vector_f, bbox_user
       class(named_vector_f), intent(in), target :: self
       class(bbox_user), pointer :: bbox_i
     end function bbox_i

  end interface

end module class_named_vector_f
