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
     type(func_registry), pointer, private :: reg => null()
     type(names_mesh), private :: meshes(MAX_DIM)
     real, pointer, private :: in(:) => null()
     real, pointer, private :: out(:) => null()
     real, pointer, private :: x(:) => null()
     integer, private :: max_ghost_points = 0
     integer, private :: dim = 0
   contains
     procedure :: init
     procedure :: update_derivatives
     !> @todo [0] make it private
     procedure :: add_mesh
     procedure :: set_reg
     procedure :: calculate_nth_derivative
  end type ghost

contains

  subroutine set_reg(g,reg)
    class(ghost) :: g
    type(func_registry), target, intent(in) :: reg
    g%reg => reg
  end subroutine set_reg


  subroutine init(p,error)
    class(ghost), target :: p
    integer, optional, intent(out) :: error

    integer, pointer :: mgp

    if(present(error)) error = FPDE_STATUS_OK

    p%name = "ghost"

    if(associated(p%in))  deallocate(p%in)
    if(associated(p%out)) deallocate(p%out)
    if(associated(p%x))   deallocate(p%x)

    mgp => p%max_ghost_points
    mgp =  p%meshes(1)%m%ghost_points*(MAX_RK/p%meshes(1)%m%max_derivative + 1)

    allocate( p%in (1 - mgp : p%reg%nx(1) + mgp) )
    allocate( p%out(1 - mgp : p%reg%nx(1) + mgp) )
    allocate( p%x  (1 - mgp : p%reg%nx(1) + mgp) )

  end subroutine init

  subroutine add_mesh(g, m, names)
    class(ghost), target :: g
    class(mesh), target, intent(in) :: m
    character(len=*) :: names(:)

    g%meshes(1)%m => m
    g%meshes(1)%names = names

  end subroutine add_mesh


  !> Function used to update the values of the derivatives designated
  !! by a combination of alpha and name.
  !!
  !! @bug [1] Here we assume that all the components of alpha are the
  !! same spatial variables, this is a big simplification. In future
  !! we shall allow mixed derivatives as well. See @todo below.
  !!
  !! @todo [1] Propositions of how to implement the mixed
  !! spatio-temporal derivatives:
  !!
  !! 1) If one needs to include a mixed derivative as well, one should
  !! add the equation for its evolution. i.e. if one needs u_xtx, then
  !! let v=u_x and add v to rhs. Then
  !! u_xtx = (u_x)_tx = v_tx = (v_t)_x = (rhs[v])_x
  !! The same should be done for derivatives of type u_ttt: v=u_t, w =
  !! v_t etc.
  !!
  !! 2) The other, simpler way would be to always commute temporal
  !! derivative to the beginning and use the rhs to fill it
  !! in. i.e. use
  !! u_xtx = u_txx = (u_t)_xx = (rhs[u])_xx
  !!
  !! 3) because 1) should be transparent to ghost, and ghost has no
  !! way to calculate temporal derivative accurately enough, 2) should
  !! be the default behavior.
  !!
  subroutine update_derivatives(this, reg, name, alpha2, error)
    class(ghost), target :: this
    integer, optional, intent(out) :: error
    type(func_registry) :: reg
    character(len=*), intent(in), optional, target :: alpha2(:,:), name

    character(len=len(alpha2)) ::&
         alpha(size(alpha2,2)), t_name, x_name
    character(len=NAME_LEN), allocatable :: x_names(:)
    integer :: n_alpha2, n_x, n_t = 0, n_blank, i
    integer :: err
    type(func), pointer :: x, fun
    real, pointer :: df(:)
    class(mesh), pointer :: m

    ! alpha[1]=["x","x",""], alpha[2]=["x","t",""], etc.
    n_alpha2 = size(alpha2,2)       ! size of alpha(1,:)

    t_name = ""
    ! get the name of a temporal variable
    call this%reg%get_temporal(name = t_name, error = err)
    ! get the first spatial variable
    call this%reg%get_spatial(names = x_names, error = err)
    x_name = x_names(1)
    call this%reg%get(name = x_name, f = x, error = err)


    ! for each derivative multiindex do
    do i = 1, n_alpha2
       alpha = alpha2(:,i)

       ! count the spatial and temporal derivatives
       if( t_name /= "" ) then
          n_t = count(alpha == t_name)
       end if
       n_x = count(alpha == x_name)
       n_blank = count(alpha == "")

       ! complain if there are any other derivatives, not counted by n_t and n_x
       if (n_t + n_x + n_blank /= size(alpha) )then
          call this%log(FPDE_STATUS_ERROR,&
               "Malformed derivative")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if

       select case(n_t)
       case(0) ! no temporal derivatives
          call this%reg%get( name = name, f = fun, error = err )
       case(1) ! one temporal derivative, get f_t
          call this%reg%get( name = name, f = fun, error = err, alpha = [t_name] )
       case default ! too many temporal derivatives
          call this%log(FPDE_STATUS_ERROR,&
               "Too many temporal derivatives")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end select

       ! get the right mesh, as we allow only one spatial variable it
       ! will be the first mesh in g%meshes, if any
       !> @todo [2] check null
       m => this%meshes(1)%m

       ! get the storage for the derivative
       call this%reg%get( name = name, vec = df, error = err, alpha = alpha )
       ! no storage error, nowhere to put the result
       if(err /= FPDE_STATUS_OK) then
          call this%log(FPDE_STATUS_ERROR,&
               "No storage for the result of derivation")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if

       ! fun now has to be differentiated n_x times using a mesh m. We
       ! need fun instead of fun%val because fun holds the boundary
       ! conditions, x holds its name which is needed to extract the
       ! boundary conditions.
       call this%calculate_nth_derivative(n_x, fun, x, df, m)

    end do

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine update_derivatives


  !> Calculates n_der-th derivative of fun with respect to spatial
  !! variable funx using mesh m. Result is stored in df.
  !!
  !! @param n_der integer
  !! @param fun type(func)
  !! @param funx type(func)
  !! @param df real(:)
  !! @param m class(mesh)
  !!
  subroutine calculate_nth_derivative(this, n_der, fun, funx, df, m)
    class(ghost) :: this
    integer, intent(in) :: n_der
    type(func), intent(in), pointer :: fun, funx
    real, intent(out) :: df(:)
    class(mesh), intent(in), pointer :: m

    integer :: n_x, n_ghost, n_mesh_der, n_calls, j, n_max_ghost, err, d, lbnd
    real, pointer :: x(:)
    class(boundary), pointer :: bl, br

    ! get the values of the points
    x => funx%val
    n_x = size(x)

    ! get the mesh data
    n_ghost    = m%ghost_points
    n_mesh_der = m%max_derivative

    ! how many ghost points do we actually need
    n_calls = n_der/n_mesh_der
    if( mod(n_der,n_mesh_der) /= 0 ) then
       n_calls = n_calls + 1
    end if
    n_max_ghost = n_calls * n_ghost

    ! get boundary conditions
    call fun%boundary%get_boundary(funx%name, bl, br, error = err)

    ! copy the interior values
    this%x  (1:n_x) = x(1:n_x)
    this%out(1:n_x) = fun%val(1:n_x)

    ! fill in the ghost points:
    lbnd = lbound(this%x,1)
    this%x(0     : lbnd : -1) = 2*x( 1 ) - x(2     :     )
    this%x(n_x+1 :          ) = 2*x(n_x) - x(n_x-1 : : -1)
    call bl%generate_values(&
         this%out(1:n_x), this%out(   0   : lbnd : -1 ))
    call br%generate_values(&
         this%out(n_x:1:-1), this%out( n_x+1 :           ))

    ! do the differentiation n_calls-times
    j = n_der ! number of differentiations left to perform
    do
       d = min(j, n_mesh_der)
       if( d < 1 ) exit ! differentiation is done
       this%in = this%out ! copy output to input
       call m%diff_global(&
            this%in ( 1-n_max_ghost: n_x + n_max_ghost ),&
            this%x  ( 1-n_max_ghost: n_x + n_max_ghost ),&
            this%out( 1-n_max_ghost: n_x + n_max_ghost ),&
            d )
       j = j - d
    end do

    ! copy the results to df
    df = this%out(1:n_x)

  end subroutine calculate_nth_derivative


    ! character(len=:), pointer :: alpha_first(:), alpha_rest(:), a1
    ! class(mesh), pointer :: m
    ! type(func), pointer :: fn
    ! class(boundary), pointer :: bl, br
    ! real, pointer :: f(:), x(:), dfdx(:)
    ! real, pointer :: in_normal(:), out_normal(:), x_normal(:)
    ! integer :: n_der, n_alpha, n_ghost_points, i, j, k, err, mesh_max_der
    ! integer :: n_f, last, max_gp, mesh_gp

    ! ! alpha[1]=["x","x",""], alpha[2]=["x","",""], etc.
    ! n_der = size(alpha,2)       !size of alpha(1,:)
    ! n_alpha = size(alpha,1)     !size of alpha(:,1)

    ! ! get the function we are going to differentiate
    ! call g%reg%get(name = name, f = fn, error = err )
    ! f => fn%val
    ! n_f = n_f

    ! ! slect apropriate boundary conditions
    ! !> @todo [0] implement get_boundary as a method on func
    ! !> @todo [0] catch error
    ! call fn%boundary%get_boundary(a1, bl, br, error = err)
    ! ! set the values at the ghost points
    ! call bl%generate_values(g%in,g%in(   1 : 1   - max_gp : -1))
    ! call br%generate_values(g%in,g%in( n_f : n_f + max_gp     ))

    ! ! set in/out tables
    ! max_gp = g%max_ghost_points

    ! ! fill in the out table
    ! g%out(1:n_f) = f

    ! if( err /= FPDE_STATUS_OK ) then
    !    if(present(error)) error = FPDE_STATUS_ERROR
    !    !> @todo log error
    !    return
    ! end if

    ! !> @todo no optimization yet, just traverse all alpha and
    ! !! calculate all derivatives
    ! do i = 1, n_alpha
    !    ! n_ghost_points = m%ghost_points       !number of ghost points
    !    ! !needed by this mesh
    !    ! mesh_max_der = m%rank

    !    call g%reg%get(name = name, vec = dfdx, error = err, alpha = alpha(:,i) )

    !    if( err /= FPDE_STATUS_OK ) then
    !       if(present(error)) error = FPDE_STATUS_ERROR
    !       !> @todo log error
    !       return
    !    end if

    !    ! alpha_first should be null from the start
    !    alpha_first => null()
    !    ! alpha_rest points to the nonempty part of alpha
    !    last        =  findloc_first( alpha(:,i), "" )
    !    last        =  min(last,n_der)
    !    alpha_rest  => alpha(1:last,i)

    !    !> @todo extract to a separate private function?
    !    do
    !       ! continue until end of table is reached
    !       if( size(alpha_rest) <= 0 ) exit
    !       a1 = alpha_rest(1)

    !       ! select an appropriate mesh
    !       m => null()
    !       do k = 1, size(g%meshes)
    !          if( any( g%meshes(k)%names == a1 ) ) then
    !             m => g%meshes(k)%m
    !          end if
    !       end do

    !       if( .not. associated(m) ) then
    !          if(present(error)) error = FPDE_STATUS_ERROR
    !          call g%log(FPDE_LOG_ERROR,&
    !               "No mesh is associated with variable: ["//trim(a1)//"]")
    !          return
    !       end if

    !       !> @todo [5] extract the following to a function

    !       ! get the spatial variable
    !       call g%reg%get(name = name, vec = x, error = err)
    !       ! copy the spatial variable to x
    !       g%x(1:n_f) = x
    !       ! set the ghost points of x
    !       g%x(1   : : -1) = x(1   :     )
    !       g%x(n_f :     ) = x(n_f : : -1)

    !       ! get the max derivative the mesh can calculate in one call
    !       mesh_max_der = m%max_derivative
    !       ! get the number of ghost points required by the mesh
    !       mesh_gp = m%ghost_points

    !       !
    !       ! at this point we have a mesh (m), number of ghost points
    !       ! that are required by it (mesh_gp), boundary conditions (b)
    !       ! and a rank of derivative we should calculate
    !       ! (alpha_first), so it is time to prepare the proper tables
    !       ! for differentiation
    !       !

    !       ! copy output to input
    !       g%in = g%out

    !       ! setup the normalized versions of in, out and x.
    !       ! Normalized version starts its index from 1 and is used in
    !       ! the call to m%diff_global()
    !       in_normal (1:n_f+2*mesh_gp) => g%in ( 1-mesh_gp : n_f+mesh_gp )
    !       x_normal  (1:n_f+2*mesh_gp) => g%out( 1-mesh_gp : n_f+mesh_gp )
    !       out_normal(1:n_f+2*mesh_gp) => g%x  ( 1-mesh_gp : n_f+mesh_gp )

    !       ! select the apropriate piece of alpha_rest
    !       last = min(&
    !            mesh_max_der,&
    !            findloc_first( alpha_rest,  a1) - 1,&
    !            size(alpha_rest))
    !       alpha_first => alpha_rest(1:last)

    !       !> @bug [10] the whole g%in/out/x should not be passed, it
    !       !! should be only a part depending on the mesh_gp.
    !       !! It is especially important for spectral mesh.
    !       ! pretty naive 1d implementation
    !       call m%diff_global(   &
    !            g%in,      &
    !            g%x,      &
    !            g%out,      &
    !            last & !rank of the derivative to calculate
    !            )

    !       ! alpha_rest for the next iteration
    !       alpha_rest => alpha_rest(last+1:)

    !    end do

    !    dfdx = g%out(1:n_f)

    ! end do

  ! end subroutine update_derivatives


  ! subroutine fill_in_temporary_arrays_1d(this, alpha_rest, )

  ! end subroutine fill_in_temporary_arrays_1d


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
  !! @return number of calls to diff_global
  !!
  function max_calls_to_diff_global(alpha, rk, varname) result(r)
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

  end function max_calls_to_diff_global

end module class_ghost
