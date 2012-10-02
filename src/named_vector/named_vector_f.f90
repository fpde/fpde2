module class_named_vector_f

  use class_icicles_user_
  use class_named_vector_user_
  use class_named_vector_implementation_

  private

  type, public, abstract, extends(named_vector_implementation) :: named_vector_f
     private
   contains
     procedure(dx_i), deferred                     :: dx
     ! procedure(available_dx_i), deferred           :: available_dx
     procedure(dt_i), deferred                     :: dt
     procedure(boundary_param_i), deferred         :: boundary_param
     procedure(num_boundary_param_i), deferred     :: num_boundary_param
     procedure(update_boundary_param_i ), deferred :: update_boundary_param
  end type named_vector_f

  interface

     function boundary_param_i(self, var, side, param)
       import named_vector_f, named_vector_user
       class(named_vector_f) :: self
       integer, intent(in) :: var, side, param

       class(named_vector_user), pointer :: boundary_param_i
     end function boundary_param_i


     function num_boundary_param_i(self, var, side)
       import named_vector_f, named_vector_user
       class(named_vector_f) :: self
       integer, intent(in) :: var, side

       integer :: num_boundary_param_i
     end function num_boundary_param_i


     function dx_i(self, alpha)
       import named_vector_f, named_vector_user
       class(named_vector_f) :: self
       integer, intent(in) :: alpha(:)

       class(named_vector_user), pointer :: dx_i
     end function dx_i


     function dt_i(self)
       import named_vector_f, named_vector_user
       class(named_vector_f) :: self

       class(named_vector_user), pointer :: dt_i
     end function dt_i


     subroutine update_boundary_param_i(self, ic)
       import named_vector_f, icicles_user
       class(named_vector_f) :: self
       class(icicles_user), target :: ic
     end subroutine update_boundary_param_i

  end interface

end module class_named_vector_f
