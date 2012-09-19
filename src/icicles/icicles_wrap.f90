!>
!! @file   icicles_registry.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sun Jul 29 12:16:10 2012
!!
!! @brief Wrap to icicles structure.
!!
!! The extended icicles allow for boolean type options, shape array
!! and boundary conditions. As for now only adding (as appending) of
!! entries is supported (no entry can be romoved). Also only simplest
!! queries can be made (i.e. ask for functions with given option).
!!
!! @todo regexp queries
!!
module class_icicles_wrap

  use constants_module
  use class_icicles
  use class_iced_boundary
  use class_platonic
  use class_named_vector
  use class_icicles_referencer
  use helper_module
  use class_boundary_box
  use logger_module

  private

  !>
  !! Structure holding additional data describing a "function" or
  !! "entry". It exapnds the data contained in icicles.
  !!
  type, extends(named_vector) :: extended_vector
     !> options is a list of strings, e.g. ["spatial", "dependent"]
     character(len=:), allocatable :: options(:)
     character(len=:), allocatable :: derivatives(:,:)
     !> boundary_box contains all information about boundray
     !! conditions
     !! @todo remove pointer attribute
     type(boundary_box), pointer :: box => null()
     logical, allocatable :: shape_mask(:)
     type(icicles_referencer), allocatable :: refs(:)
   end type extended_vector

  ! extended vector constructor
  interface extended_vector
     module procedure :: extended_vector_constructor
  end interface extended_vector


  !>
  !! icicles_wrap is an extension to icicles. It contains information
  !! about mesh size (nx) and about functions (vectors)
  !!
  type, public, extends(platonic) :: icicles_wrap
     private
     type(extended_vector), allocatable :: vectors(:)
     integer, allocatable :: nx(:)
     character(len=:), allocatable :: t
     character(len=:), allocatable :: x(:)
     !! @todo use references and change interface of get_t and get_x
     !! to also return a pointer to the location of t and x respectively
     ! type(icicles_referencer), allocatable :: t
     ! type(icicles_referencer), allocatable :: x(:)
   contains
     procedure :: init

     procedure :: set_nx
     procedure :: get_nx

     procedure :: get_dim

     procedure :: set_pointers
     procedure :: total_length

     procedure :: add
     procedure :: get

     procedure :: get_names

     procedure :: set_x
     procedure :: get_x

     procedure :: set_t
     procedure :: get_t

     procedure :: derivative_name => internal_derivative_name

     procedure, private :: internal_add_vector
     procedure, private :: internal_add_derivatives
     procedure, private :: internal_add_boundary_parameters

     procedure, private :: internal_derivative_name
     procedure, private :: internal_boundary_name
  end type icicles_wrap


contains

  subroutine set_nx(self, nx, error)
    class(icicles_wrap) :: self
    integer, intent(in) :: nx(:)
    integer, intent(out), optional :: error

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( allocated(self%nx) ) then
       call self%log(FPDE_LOG_ERROR,&
            "set_nx(): nx already set, ignoring")
       return
    end if

    if( any(nx <= 0) ) then
       call self%log(FPDE_LOG_ERROR,&
            "set_nx(): nx > 0 required.")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    self%nx = nx

  end subroutine set_nx


  function get_nx(self, i)
    class(icicles_wrap) :: self
    integer, intent(in) :: i
    integer :: get_nx
    get_nx = merge( 0, self%nx(i), i > size(self%nx) )
  end function get_nx


  function get_dim(self)
    class(icicles_wrap) :: self
    integer :: get_dim
    get_dim = size(self%nx)
  end function get_dim


  function get_t(self, error)
    class(icicles_wrap) :: self
    integer, optional, intent(out) :: error

    character(len=:), allocatable :: get_t

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( allocated(self%t) ) then
       if( present(error) ) error = FPDE_STATUS_OK
       get_t = self%t
    else
       get_t = ""
    end if

  end function get_t


  function get_x(self, i, error)
    class(icicles_wrap) :: self
    integer, intent(in) :: i
    integer, optional, intent(out) :: error

    character(len=:), allocatable :: get_x

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( .not. allocated(self%x)) then
       get_x = ""
    else if( i > size(self%x)) then
       get_x = ""
    else
       if( present(error) ) error = FPDE_STATUS_OK
       get_x = self%x(i)
    end if

  end function get_x


  function get_names(self, option)
    class(icicles_wrap) :: self
    character(len=*), intent(in), optional :: option

    character(len=:), allocatable :: get_names(:)

    integer :: maxlen, i

    allocate( character(len=0) :: get_names(0) )

    do i = 1, size(self%vectors)
       associate(v => self%vectors(i))

         if( present(option) ) then
            if( all(v%options /= option)) then
               cycle
            end if
         end if

         maxlen = max(len(get_names), len(v%name))
         get_names = [ character(len=maxlen) :: get_names, v%name ]

       end associate
    end do

  end function get_names


  subroutine init(p, error)
    class(icicles_wrap), target :: p
    integer, optional, intent(out) :: error

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( .not. allocated(p%nx) ) then
       call p%log(FPDE_LOG_ERROR,&
            "init(): nx not set, call set_nx() first.")
       return
    end if

    if( present(error) ) error = FPDE_STATUS_OK

    allocate(p%vectors(0))

  end subroutine init


  function total_length(self, names, error) result(L)
    class(icicles_wrap), target :: self
    character(len=*), intent(in), optional :: names(:)
    integer, intent(out), optional :: error

    integer :: L

    class(extended_vector), pointer :: p
    integer :: i

    if( present(error) ) error = FPDE_STATUS_OK

    L = 0
    do i = 1, size(self%vectors)
       p => self%vectors(i)
       if( present(names) ) then
          if( all(names /= p%name) ) cycle
       end if

       L = L + product( self%nx, self%vectors(i)%shape_mask)
    end do

  end function total_length


  subroutine set_pointers(self, vec, names, error)
    class(icicles_wrap), target :: self
    real, target, intent(in) :: vec(:)
    character(len=*), intent(in), optional :: names(:)
    integer, intent(out), optional :: error

    integer :: k, i, j, inc

    if(present(error)) error = FPDE_STATUS_ERROR

    ! k is a current position in vec(:), each vector from icicles_wrap
    ! will be pointing to a position in vec deisgnated by k
    k = 1
    do i = 1, size(self%vectors)
       associate(p => self%vectors(i) )

         if( present(names) ) then
            if( all(names /= p%name) ) cycle
         end if

         inc = product( self%nx, mask = p%shape_mask )

         ! return here will result in error = FPDE_STATUS_ERROR
         if( k + inc - 1 > size(vec)) then
            call self%log(FPDE_STATUS_ERROR,&
                 "set_pointers(): vec too small.")
            return
         end if

         p%val => vec(k:k+inc-1)
         do j = 1, size(p%refs)
            call p%refs(j)%set_to( vec(k:k+inc-1) )
         end do

         k = k + inc
       end associate
    end do

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine set_pointers


  subroutine set_x( self, names, error )
    class(icicles_wrap), target :: self
    character(len=*), intent(in) :: names(:)
    integer, optional, intent(out) :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_ERROR

    if( size(names) /= self%get_dim()) then
       call self%log(FPDE_LOG_ERROR,&
            "size(names) /= dim")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    do i = 1, size(names)
       call self%add( names(i), options = [icw_spatial] )
    end do

    self%x = names

  end subroutine set_x


  subroutine set_t( self, name )
    class(icicles_wrap), target :: self
    character(len=*), intent(in) :: name

    call self%add( name, options = [icw_temporal] )

    self%t = name

  end subroutine set_t


  subroutine get( self, name, vec, scal, options, box, derivatives, error )
    class(icicles_wrap), target :: self
    character(len=*), intent(in) :: name
    real, pointer, intent(out), optional :: vec(:), scal
    character(len=*), intent(out), optional :: options(:)
    character(len=*), intent(out), optional :: derivatives(:,:)
    !! @bug if final method is added to boundary_box it will result in
    !! segfaults in a call to icicles_wrap%get() due to a bug in ifort
    !! 13.0.0 20120731, for more details see
    !! http://software.intel.com/en-us/forums/topic/327918
    !!
    !! a temporary workaround is to remove intent(out) or change it
    !! to intent(inout)
    type(boundary_box), pointer, intent(out), optional :: box
    integer, optional, intent(out) :: error

    class(extended_vector), pointer :: p
    integer :: i

    if( present(error) ) error = FPDE_STATUS_ERROR

    p => null()
    do i = 1, size(self%vectors)
       associate( v => self%vectors(i) )
         if( v%name == name ) then
            p => v
            exit
         end if
       end associate
    end do

    if( .not. associated(p) ) then
       call self%log(FPDE_LOG_ERROR,&
            "Unable to find vector ["//trim(name)//"]")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    if( associated(p%val)) then
       if( present(vec) ) vec                 => p%val
       if( present(scal) ) scal               => p%val(1)
    end if

    if( allocated(p%options)) then
       if( present(options) )         options = p%options
    end if

    if( present(box) )                    box => p%box

    if( allocated(p%options) ) then
       if( present(derivatives) ) derivatives = p%derivatives
    end if

  end subroutine get


  !> Just a constructor for extended_vector
  !!
  !! @param name
  !! @param dim
  !!
  !! @return
  !!
  function extended_vector_constructor(name, dim) result(vec)
    character(len=*), intent(in) :: name
    integer, intent(in) :: dim

    type(extended_vector) :: vec

    vec%name        = name
    vec%options     = reshape([character(len=0)::],[0])
    vec%derivatives = reshape([character(len=0)::],[0,0])
    allocate(vec%shape_mask(dim))
    vec%shape_mask  = .true.
    allocate(vec%refs(0))

  end function extended_vector_constructor


  subroutine add( self, name, options, box, derivatives, error )
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: options(:), derivatives(:,:)
    ! type(boundary_box), intent(in), optional :: box
    type(boundary_box), intent(in), optional :: box
    integer, optional, intent(out) :: error

    type(extended_vector) :: vec

    if( present(error) ) error = FPDE_STATUS_ERROR

    vec = extended_vector(name = name, dim = self%get_dim())

    if( present(options) ) then
       vec%options = options
    end if

    if( present(derivatives) ) then
       vec%derivatives = derivatives
       call self%internal_add_derivatives( name, derivatives )
    end if

    if( present(box) ) then
       ! after this line box can be safely deallocated outside the
       ! add()
       ! at this point, vec contains a pointer to a copy of box
       allocate(vec%box)
       ! to copy all the internal data the overloaded assignment has
       ! to be used
       vec%box = box
       call self%internal_add_boundary_parameters( name, vec%box )
    end if

    call self%internal_add_vector( vec )

    if( present(error) ) error = FPDE_STATUS_OK

  end subroutine add


  subroutine internal_add_vector( self, vector )
    class(icicles_wrap) :: self
    type(extended_vector), intent(in) :: vector

    type(extended_vector), allocatable :: vectors_temp(:)

    vectors_temp = [self%vectors,vector]
    self%vectors = vectors_temp

  end subroutine internal_add_vector


  subroutine internal_add_derivatives( self, fname, derivatives )
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: fname, derivatives(:,:)

    character(len=:), allocatable :: dname
    integer :: i

    associate(                 &
         x     => self%x,      &
         d     => derivatives )

      do i = 1, size(d,2)
         dname = self%internal_derivative_name( fname, d(:,i) )
         call self%add( dname, options = [ icw_derivative ] )
      end do

    end associate

  end subroutine internal_add_derivatives


  subroutine internal_add_boundary_parameters(self, fname, box)
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: fname
    type(boundary_box), intent(in), target :: box

    integer :: i, side, var, k, err, dim
    type(extended_vector) :: vec
    type(iced_boundary), pointer :: ib

    dim = self%get_dim()

    allocate(character(len=0) :: vec%derivatives(0,0))
    allocate(vec%shape_mask(dim))

    do side = 1,2
       do var = 1, dim
          ib => box%get(dir = var, side = side, error = err)

          if( err /= FPDE_STATUS_OK ) then
             call self%log(FPDE_LOG_ERROR,&
                  "internal_add_boundary_parameters(): Unable to retri&
                  &eve boundary from boundary_box.")
             return
          end if

          do k = 1, ib%get_param_num()

             vec%refs = [ ib%get_param_ref(k) ]

             vec%name = self%internal_boundary_name(&
                  fname = fname,&
                  xname = self%get_x(var),&
                  pname = vec%refs(1)%get_name(),&
                  side  = side )

             vec%options = [ icw_boundary ]

             vec%shape_mask = .true.
             vec%shape_mask(var) = .false.

             call self%internal_add_vector(vec)

          end do
          call ib%lock_refs()
       end do
    end do

  end subroutine internal_add_boundary_parameters


  function internal_derivative_name(self, fname, d) result(dname)
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: fname, d(:)

    character(len=:), allocatable :: dname

    dname = icw_derivative_name// "(" // &
         trim(fname) // "," // &
         trim(join(d,",")) // ")"

  end function internal_derivative_name


  function internal_boundary_name(self, fname, xname, pname, side) result(name)
    class(icicles_wrap) :: self
    character(len=*), intent(in) :: fname, xname, pname
    integer, intent(in) :: side

    character(len=:), allocatable :: name

    name = icw_boundary_name // "(" //&
         trim(fname) // "," //&
         trim(xname) // "," //&
         merge("L", "R", side == 1) // "," //&
         trim(pname) // ")"

  end function internal_boundary_name


end module class_icicles_wrap
