module class_ghost

  use class_platonic
  use class_icicles_wrap
  use class_mesh
  use class_mesh1d
  use constants_module
  use logger_module
  use class_ghost1d

  private

  type, public, extends(platonic) :: ghost
     private
     type(ghost1d) :: g1
   contains
     private
     procedure, public :: update_derivatives
  end type ghost



contains

  subroutine update_derivatives(self, icw, fname, alpha2, m, error)
    class(ghost), target :: self
    type(icicles_wrap) :: icw
    class(mesh), intent(in) :: m
    character(len=*), intent(in), target :: alpha2(:,:), fname
    integer, optional, intent(out) :: error

    integer :: err
    integer, allocatable :: nx(:)
    character(len=:), allocatable :: temporal(:)

    if(present(error)) error = FPDE_STATUS_ERROR

    nx = icw%get_nx()

    temporal = icw%get_names(icw_temporal)

    ! check for any temporal derivatives
    if( size(temporal) > 0 ) then
       if( any(alpha2 == temporal(1)) ) then
          if(present(error)) error = FPDE_STATUS_ERROR
          call self%log(FPDE_LOG_ERROR, &
               "Temporal derivatives cannot be updated (due to unknown bo&
               &undary conditions)")
          return
       end if
    end if

    ! check if the mesh is compatible with icicles
    if( size(nx) /= m%get_dim() ) then
       if(present(error)) error = FPDE_STATUS_ERROR
       call self%log(FPDE_LOG_ERROR, &
            "Mesh and data have incompatible dimensions.")
       return
    end if

    select type(m)
    class is(mesh1d)
       call self%g1%update_derivatives(icw,fname,alpha2,m,err)
       if(present(error)) error = err
    ! class is(mesh2d)

    ! class is(mesh3d)

    class default
       if(present(error)) error = FPDE_STATUS_ERROR
       call self%log(FPDE_LOG_ERROR, &
            "Unsupported mesh class, supported mesh classes are: mesh1&
            &d, mesh2d and mesh3d.")
       return
    end select

  end subroutine update_derivatives



end module class_ghost
