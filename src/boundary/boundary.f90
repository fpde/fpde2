module class_boundary

  use class_platonic

  private

  type, public, abstract, extends(platonic) :: boundary
  end type boundary

end module class_boundary
