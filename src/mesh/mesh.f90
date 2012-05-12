!>
!! @file   mesh.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Nov 18 12:57:09 2011
!!
!! @brief File contains a skeleton of the mesh class used to store a
!! discrete representation of a set of functions defined on grid
!! points.
!!
module class_mesh

  use class_platonic
  use constants_module
  use logger_module

  private

  type, public, abstract, extends(platonic) :: mesh
     !> max_derivative holds a maximal rank of derivative that can be
     !! calculated using this mesh
     integer                   :: max_derivative = 0
     integer                   :: ghost_points = 0
   contains
     procedure(diff_global_interface), deferred :: diff_global
     ! procedure(diff_point_interface ), deferred :: diff_point
  end type mesh

  interface
     !> Should calculate
     !! \f$D_k f(x_i)\f$ for \f$k\f$ given as argument and save it to
     !! df
     !!
     !! @param m
     !! @param f function do differentiate
     !! @param df derivative is going to be saved here
     !! @param k rank of derivative to calculate
     !!
     subroutine diff_global_interface( m, f, x, df, k )
       import mesh
       class(mesh), target, intent(inout) :: m
       integer, intent(in) :: k
       real, intent(in)  :: f(:), x(:)
       real, intent(out) :: df(:)
     end subroutine diff_global_interface

     !> Same as diff_global, but calculates only the derivative at point i
     !!
     !! @return value of the k-th derivative at point x_i
     !!
     function diff_point_interface( m, f, x, k, i ) result(d)
       import mesh
       class(mesh), target, intent(inout) :: m
       integer, intent(in) :: i,k
       real, intent(in) :: f(:), x(:)
       real :: d
     end function diff_point_interface

  end interface

end module class_mesh
