module class_passive_icicles

  use class_platonic

  private

!> Limited access version of icicles
  type, public, abstract, extends(platonic) :: passive_icicles
   contains
     procedure(get_i), deferred :: get
  end type passive_icicles


  abstract interface
     subroutine get_i(self, name, vec, scal, error)
       import passive_icicles
       class(passive_icicles), target :: self
       character(len=*), intent(in) :: name
       real, pointer, optional :: scal, vec(:)
       integer, intent(out), optional :: error
     end subroutine get_i
  end interface

end module class_passive_icicles
