module class_icicles_wrap_

  use class_icicles_
  use class_named_vector_
  use class_icicles_implementation

  private

  type, public, extends(icicles_implementation) :: icicles_wrap
     private
   contains
     procedure :: add
  end type icicles_wrap

contains

  subroutine add(self, nv)
    class(icicles_wrap) :: self
    class(named_vector), target, intent(in) :: nv

    ! select type(nv)
    ! class is(function_vector)
    ! class is(x_vector)
    ! class is(t_vector)
    ! end select

  end subroutine add

end module class_icicles_wrap_
