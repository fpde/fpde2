!>
!! @file   ghost.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Mar 31 21:36:52 2012
!!
!! @brief This module implements boundary conditions using ghost
!! points, and acts as an interface between solver (or any higher
!! level caller) and a set of meshes.
!!
!! It is suited for multi-dimensional differentiation
!!
module class_ghost

  use constants_module
  use logger_module
  use class_mesh
  use icicles_module, only: named_vector

  private

  !> named pointer to mesh.
  type :: names_mesh
     !> Names of spatial variables to which this mesh is assigned
     character(len=NAME_LEN) :: names(MAX_DIM) = ""
     !> Pointer to a mesh
     class(mesh), pointer :: val => null()
  end type names_mesh

  type, extends(named) :: ghost
     type(names_mesh), pointer :: meshes(:) => null()
     real, pointer :: temp_array_in(:) => null()
     real, pointer :: temp_array_out(:) => null()
   contains
     ! procedure :: init
     procedure :: derivative
  end type ghost

contains

  !> Calculates partial derivatives d(:) of function(s) given in
  !! fun(:) with respect to spatial variables given in x(:). To
  !! calculate derivatives calls to an array of meshes corresponding to
  !!
  !! @param g ghost
  !! @param fun function to be differentiated
  !! @param x array of all of spatial variables
  !! @param d array of derivatives
  !!
  subroutine derivative(g, fun, x, d, error)
    class(ghost) :: g
    type(named_vector), intent(in) :: fun, x(:), d(:)
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    ! 1) check if d(:) corresponds to fun (by comparing their names)
    !
    ! 2) convert d(:) to array of characters of the form
    ! "00", "01", "11" etc.
    !
    ! 3) split the strings from the array into segments, determined by
    ! meshes%mesh%dimension, e.g. "120213" -> ["12"],["0"],["213"]
    !
    ! 4) for each segment
    !
    !    a) check if any such segment has already been calculated and
    !    written into d(:)%val, if yes read it to a temp_array_in(:)
    !    (by copying and adding ghost points) instead of calculating
    !    it again
    !
    !    b) use mesh to caclulate derivatives and write the result to
    !    temp_array_out(:)
    !
    !    c) copy the contents of temp_array_out(:) to d(:)%val
    !
    !    d) proceede to the next segment
    !
    ! 5) repeat for each element of d(:)

  end subroutine derivative

end module class_ghost
