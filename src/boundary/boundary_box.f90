module class_bbox

  use class_bbox_user

  private

  type, public :: btype
     integer :: x, side
     character(len=:), allocatable :: type
  end type btype


  type, public, abstract, extends(bbox_user) :: bbox
   contains
  end type bbox


end module class_bbox
