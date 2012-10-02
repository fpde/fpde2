module class_ghost_boundary_box

  use class_ghost_boundary_box_user
  use class_boundary

  private

  type, public, abstract, extends(ghost_boundary_box_user) :: ghost_boundary_box
  end type ghost_boundary_box

end module class_ghost_boundary_box
