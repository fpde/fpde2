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
     private
     !> max_derivative holds a maximal rank of derivative that can be
     !! calculated using this mesh
     !! @todo make it allocatable
     integer, allocatable      :: calculable_derivatives(:,:)
     !> (i,1) corresponds to left side of coordinate #i,
     !! (i,2) corresponds to the right side of coordinate #i
     integer, allocatable      :: ghost_points(:)
   contains
     procedure, private :: get_all_ghost_points
     procedure, private :: get_some_ghost_points
     generic :: get_ghost_points => &
          get_all_ghost_points,&
          get_some_ghost_points
     procedure :: get_calculable_derivatives
     procedure, non_overridable :: set_ghost_points
     procedure, non_overridable :: set_calculable_derivatives
     procedure(diff_global_interface), deferred :: diff_global
     procedure(get_dim_interface), deferred :: get_dim
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
     subroutine diff_global_interface( self, f, x, df, k )
       import mesh
       class(mesh), target, intent(inout) :: self
       integer, intent(in) :: k
       real, intent(in)  :: f(:), x(:)
       real, intent(out), target :: df(:)
     end subroutine diff_global_interface

     !> Same as diff_global, but calculates only the derivative at point i
     !!
     !! @return value of the k-th derivative at point x_i
     !!
     function diff_point_interface( self, f, x, k, i ) result(d)
       import mesh
       class(mesh), target, intent(inout) :: self
       integer, intent(in) :: i,k
       real, intent(in) :: f(:), x(:)
       real :: d
     end function diff_point_interface

     function get_dim_interface( self ) result(dim)
       import mesh
       class(mesh), intent(in) :: self
       integer :: dim
     end function get_dim_interface

  end interface

contains


  function get_all_ghost_points(self) result(r)
    class(mesh) :: self

    integer, allocatable :: r(:)

    r = self%ghost_points
  end function get_all_ghost_points

  function get_some_ghost_points(self, var) result(r)
    class(mesh) :: self
    integer, intent(in) :: var

    integer :: r

    r = self%ghost_points(var)
  end function get_some_ghost_points


  function get_calculable_derivatives(self) result(r)
    class(mesh) :: self

    integer, allocatable :: r(:,:)

    r = self%calculable_derivatives
  end function get_calculable_derivatives


  subroutine set_ghost_points(self, gp, error)
    class(mesh) :: self
    integer, intent(in) :: gp(:)
    integer, intent(out), optional :: error

    if(present(error)) error = FPDE_STATUS_OK

    if(.not. allocated(self%ghost_points)) then
       self%ghost_points = gp
    else
       call self%log(FPDE_LOG_ERROR,&
            "Trying to change ghost_points which are already set.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

  end subroutine set_ghost_points


  subroutine set_calculable_derivatives(self, cd, error)
    class(mesh) :: self
    integer, intent(in) :: cd(:,:)
    integer, intent(out), optional :: error

    if(present(error)) error = FPDE_STATUS_OK

    if(.not. allocated(self%calculable_derivatives)) then
       self%calculable_derivatives = cd
    else
       call self%log(FPDE_LOG_ERROR,&
            "Trying to change max_derivative which are already set.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

  end subroutine set_calculable_derivatives


end module class_mesh
