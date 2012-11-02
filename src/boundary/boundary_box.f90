module class_bbox

  use class_bbox_user

  private

  type, public, abstract, extends(bbox_user) :: bbox
  end type bbox


end module class_bbox
