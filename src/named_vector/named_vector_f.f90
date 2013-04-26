module class_named_vector_f

  use class_coordinates
  use class_icicles_user
  use class_bbox_user
  use class_named_vector_user
  use class_named_vector_implementation

  private

  type, public, abstract, extends(named_vector_implementation)&
       :: named_vector_f_user
     private
   contains
     procedure(dx_i), deferred :: dx
     procedure(dt_i), deferred :: dt
     procedure(dx_update_i), deferred :: dx_update
     procedure(c_i), deferred  :: c
     procedure(bbox_nparam_i), deferred  :: bbox_nparam
     procedure(bbox_param_i), deferred   :: bbox_param

     ! the following functions combine some commonly used methods
     ! together for easier usage
     procedure :: bbox_param_set
     procedure :: var
  end type named_vector_f_user


  type, public, abstract, extends(named_vector_f_user) :: named_vector_f
     private
   contains
     ! procedure(bbox_update_i ), deferred :: bbox_update
     procedure(bbox_i ), deferred :: bbox
  end type named_vector_f


  interface

     function b_i(self, id, param)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: id, param

       class(named_vector_user), pointer :: b_i
     end function b_i


     function dx_i(self, alpha)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: alpha(:)

       class(named_vector_user), pointer :: dx_i
     end function dx_i


     subroutine dx_update_i(self, alpha, ic)
       import named_vector_f_user, icicles_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: alpha(:,:)
       class(icicles_user), intent(in) :: ic
     end subroutine dx_update_i


     function dt_i(self)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self

       class(named_vector_user), pointer :: dt_i
     end function dt_i


     function c_i(self)
       import named_vector_f_user, coordinates
       class(named_vector_f_user), intent(in), target :: self
       class(coordinates), pointer :: c_i
     end function c_i


     function bbox_param_i(self, id, param) result(r)
       import named_vector_f_user, named_vector_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: id, param
       class(named_vector_user), pointer :: r
     end function bbox_param_i


     function bbox_nparam_i(self, id) result(r)
       import named_vector_f_user
       class(named_vector_f_user) :: self
       integer, intent(in) :: id
       integer :: r
     end function bbox_nparam_i


     subroutine bbox_update_i(self, ic)
       import named_vector_f, icicles_user
       class(named_vector_f) :: self
       class(icicles_user), target :: ic
     end subroutine bbox_update_i


     function bbox_i(self)
       import named_vector_f, bbox_user
       class(named_vector_f) :: self
       class(bbox_user), pointer :: bbox_i
     end function bbox_i

  end interface

  public :: nvtof

contains

  subroutine bbox_param_set(self, id, param, v)
    class(named_vector_f_user) :: self
    integer, intent(in) :: id, param
    real, intent(in) :: v(:)

    class(named_vector_user), pointer :: b
    class(coordinates), pointer :: co

    b => self%bbox_param(id,param)
    co => self%c()
    b = v(co%bregion(id))

  end subroutine bbox_param_set


  function var(self, n) result(r)
    class(named_vector_f_user) :: self
    integer, intent(in) :: n

    class(named_vector_user), pointer :: r

    class(coordinates), pointer :: co

    r => null()

    co => self%c()

    if( .not. associated(co) ) return

    r => co%var(n)
  end function var


  function nvtof(nv) result(r)
    class(named_vector_user), target, intent(in) :: nv
    class(named_vector_f), pointer :: r

    r => null()

    select type(nv)
    class is(named_vector_f)
       r => nv
    class default
       call nv%loge("Conversion to named_vector_f failed due to incomp&
            &atible type")
    end select
  end function nvtof

end module class_named_vector_f
