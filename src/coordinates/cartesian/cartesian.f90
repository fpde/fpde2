module class_coordinates_c

  use class_coordinates

  private

  type, public, abstract, extends(coordinates) :: coordinates_c
     private
   contains
  end type coordinates_c

end module class_coordinates_c
