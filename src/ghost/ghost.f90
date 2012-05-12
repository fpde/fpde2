!>
!! @file   ghost.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Mar 31 21:36:52 2012
!!
!! @brief This module implements boundary conditions using ghost
!! points, and acts as an interface between solver (or any higher
!! level caller) and a set of meshes.
!!
!! It will be (is) suited for multi-dimensional differentiation
!!
!! @todo As for now, class ghost is implemented with only one mesh in
!! mind, but later on it should be reimplement to support multiple
!! meshes
!!
module class_ghost

  use constants_module
  use logger_module
  use class_platonic
  use class_mesh
  use class_func_registry
  use class_function
  use class_boundary
  use helper_module

  private

  !> named pointer to mesh.
  type :: names_mesh
     !> Names of spatial variables to which this mesh is assigned
     character(len=NAME_LEN) :: names(MAX_DIM) = ""
     !> Pointer to a mesh
     class(mesh), pointer :: m => null()
  end type names_mesh

  type, public, extends(platonic) :: ghost
     type(func_registry), pointer :: reg => null()
     type(names_mesh), private :: meshes(MAX_DIM)
     real, pointer, private :: temp_array_in(:) => null()
     real, pointer, private :: temp_array_out(:) => null()
     real, pointer, private :: temp_array_xgp(:) => null()
     integer, private :: max_ghost_points = 0
     integer, private :: dim = 0
   contains
     ! procedure :: init
     ! procedure :: update_derivatives
     ! procedure :: calculate_derivative_single
  end type ghost

contains

  subroutine set_reg(g,reg)
    class(ghost) :: g
    type(func_registry), target, intent(in) :: reg
    g%reg => reg
  end subroutine set_reg


  subroutine init(g,error)
    class(ghost), target :: g
    integer, optional, intent(out) :: error

    integer, pointer :: mgp

    if(present(error)) error = FPDE_STATUS_OK

    g%name = "ghost"

    deallocate(g%temp_array_in)
    deallocate(g%temp_array_out)

    mgp => g%max_ghost_points
    mgp = g%meshes(1)%m%ghost_points

    allocate( g%temp_array_in (g%reg%nx(1)+2*mgp) )
    allocate( g%temp_array_out(g%reg%nx(1)+2*mgp) )
    allocate( g%temp_array_xgp(g%reg%nx(1)+2*mgp) )

  end subroutine init

  subroutine add_mesh(g, m, names)
    class(ghost), target :: g
    class(mesh), target, intent(in) :: m
    character(len=*) :: names(:)

    g%meshes(1)%m => m
    g%meshes(1)%names = names

  end subroutine add_mesh

  !> Function used to update the values of the derivatives designated
  !! by a combination of alpha and name
  !!
  !! @param g
  !! @param reg
  !! @param name
  !! @param alpha
  !! @param error
  !!
  !! @return
  !!
  subroutine update_derivatives(g, reg, name, alpha, error)
    class(ghost), target :: g
    integer, optional, intent(out) :: error
    type(func_registry) :: reg
    character(len=*), intent(in), optional, target :: alpha(:,:), name

    character(len=:), pointer :: alpha_first(:), alpha_rest(:), a1
    class(mesh), pointer :: m
    type(func), pointer :: fn
    class(boundary), pointer :: bl, br
    real, pointer :: f(:), x(:), dfdx(:)
    real, pointer :: in(:), out(:), xgp(:)
    real, pointer :: in_normal(:), out_normal(:), xgp_normal(:)
    integer :: n_der, n_alpha, n_ghost_points, i, j, k, err, mesh_max_der
    integer :: n_f, last, max_gp, mesh_gp

    ! alpha[1]=["x","x",""], alpha[2]=["x","",""], alpha[3]=["x","t","x"] etc.
    n_der = size(alpha,2)       !size of alpha(1,:)
    n_alpha = size(alpha,1)     !size of alpha(:,1)

    ! get the function we are going to differentiate
    call g%reg%get(name = name, f = fn, error = err )
    f => fn%val
    n_f = n_f

    ! set in/out tables
    max_gp = g%max_ghost_points
    in (1-max_gp : n_f+max_gp) => g%temp_array_in
    out(1-max_gp : n_f+max_gp) => g%temp_array_out
    xgp(1-max_gp : n_f+max_gp) => g%temp_array_xgp

    ! fill in the out table
    out(1:n_f) = f

    if( err /= FPDE_STATUS_OK ) then
       if(present(error)) error = FPDE_STATUS_ERROR
       !> @todo log error
       return
    end if

    !> @todo no optimization yet, just traverse all alpha and
    !! calculate all derivatives
    do i = 1, n_alpha
       ! n_ghost_points = m%ghost_points       !number of ghost points
       ! !needed by this mesh
       ! mesh_max_der = m%rank

       call g%reg%get(name = name, vec = dfdx, error = err, alpha = alpha(:,i) )

       if( err /= FPDE_STATUS_OK ) then
          if(present(error)) error = FPDE_STATUS_ERROR
          !> @todo log error
          return
       end if

       ! alpha_first should be null from the start
       alpha_first => null()
       ! alpha_rest points to the nonempty part of alpha
       last        =  findloc_first( alpha(:,i), "" )
       last        =  min(last,n_der)
       alpha_rest  => alpha(1:last,i)

       !> @todo extract to a separate private function?
       do
          ! continue untile end of table is reached
          if( size(alpha_rest) <= 0 ) exit
          a1 = alpha_rest(1)

          ! select an appropriate mesh
          m => null()
          do k = 1, size(g%meshes)
             if( any( g%meshes(k)%names == a1 ) ) then
                m => g%meshes(k)%m
             end if
          end do

          if( .not. associated(m) ) then
             if(present(error)) error = FPDE_STATUS_ERROR
             call g%log(FPDE_LOG_ERROR,&
                  "No mesh is associated with variable: ["//trim(a1)//"]")
             return
          end if

          !> @todo [5] extract the following to a function

          ! get the spatial variable
          call g%reg%get(name = name, vec = x, error = err)
          ! copy the spatial variable to xgp
          xgp(1:n_f) = x
          ! set the ghost points of xgp
          xgp(1   : : -1) = xgp(1   :     )
          xgp(n_f :     ) = xgp(n_f : : -1)

          ! get the rank of the mesh
          mesh_max_der = m%max_derivative
          ! get the number of ghost points required by the mesh
          mesh_gp = m%ghost_points

          ! slect apropriate boundary conditions
          !> @todo [0] implement get_boundary as a method on func
          call fn%boundary%get_boundary(a1, bl, br, err)

          ! select the apropriate piece of alpha_rest
          last = min(&
               mesh_max_der,&
               findloc_first( alpha_rest,  a1) - 1,&
               size(alpha_rest))
          alpha_first => alpha_rest(1:last)

          !
          ! at this point we have a mesh (m), number of ghost points
          ! that are required by it (mesh_gp), boundary conditions (b)
          ! and a rank of derivative we should calculate
          ! (alpha_first), so it is time to prepare the proper tables
          ! for differentiation
          !

          ! copy output to input
          in = out

          ! set the values at the ghost points
          call bl%generate_values(in,in(   1 : 1-mesh_gp   : -1))
          call br%generate_values(in,in( n_f : n_f+mesh_gp     ))

          ! setup the normalized versions of in, out and xgp.
          ! Normalized version starts its index from 1 and is used in
          ! the call to m%diff_global()
          in_normal (1:n_f+2*mesh_gp) => in ( 1-mesh_gp : n_f+mesh_gp )
          xgp_normal(1:n_f+2*mesh_gp) => out( 1-mesh_gp : n_f+mesh_gp )
          out_normal(1:n_f+2*mesh_gp) => xgp( 1-mesh_gp : n_f+mesh_gp )

          ! pretty naive 1d implementation
          call m%diff_global(   &
               in_normal ,      &
               xgp_normal,      &
               out_normal,      &
               size(alpha_rest) & !rank of the derivative to calculate
               )

          ! alpha_rest for the next iteration
          alpha_rest => alpha_rest(last+1:)

       end do

       dfdx = out(1:n_f)

    end do

  end subroutine update_derivatives

  ! subroutine calculate_derivative_single(g, in, out, error)
  !   class(ghost) :: g
  !   integer, optional, intent(out) :: error
  !   real, intent(in) :: in(:)
  !   real, intent(out) :: out(:)
  !   integer :: err

  !   if(present(error)) error = FPDE_STATUS_ERROR

  !   ! let d to point to the place D(f,alpha) should be stored
  !   call g%reg%get(f%name, alpha = alpha, vec = d, error = err)

  !   if( err /= FPDE_STATUS_OK ) return



  ! end subroutine calculate_derivative_single




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

  !> Returns the number of layers of ghost points required to
  !! calculate a derivative given by multiindex alpha
  !!
  !! @param alpha multiindex of a derivative
  !!
  !! @param rk rank of a differentiation procedure (maximal rank of
  !! derivative the given mesh is capable of calculating at once)
  !!
  !! @param varname name of a variable corresponding to the given mesh
  !!
  !! @return number of layers
  !!
  function get_num_layers(alpha, rk, varname) result(r)
    character(len=*), intent(in) :: alpha(:)
    integer, intent(in) :: rk
    character(len=*), intent(in) :: varname
    integer :: r

    integer :: i, j = 0

    r = 0
    do i = 1, size(alpha)
       if( alpha(i) == varname ) then
          j = j + 1
       else
          r = r + ceiling(real(j)/real(rk))
          j = 0
       end if
    end do

  end function get_num_layers

end module class_ghost
