!>
!! @file   named_vector_f.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Nov  2 16:44:18 2012
!!
!! @brief
!!
module class_named_vector_f

  use class_named_vector

  use class_coordinates

  private


  !> this structure represents a boundary conditions for this function
  !! @todo shouldn't this structure be moved to icicles?
  type, public :: boundary_data
     !> type of the boundary condition (e.g. "dirichlet")
     character(len=:), allocatable  :: btype
     !> values of the parameters shape(params)=[length,params_number]
     real, allocatable :: params(:,:)
  end type boundary_data


  type :: d_ptr
     integer, allocatable :: alpha(:)
     !! @todo store the derivatives in a straight-forward 2d array
     !! instead of in an array of named_vectors?
     class(named_vector), pointer :: val => null()
  end type d_ptr


  type, public, extends(named_vector) :: named_vector_f
     private
     ! bdata is made public intentionally, its compontents
     ! (params(:,:)) will be reallocated by icicles
     type(boundary_data), allocatable, public :: bdata(:)
     type(d_ptr), allocatable     :: dx_(:)
     class(named_vector), pointer :: dt_    => null()
     class(coordinates), pointer  :: coord  => null()
   contains
     procedure :: coordinates => get_coordinates
     procedure :: dx
     procedure :: dt
  end type named_vector_f


  interface named_vector_f
     module procedure :: nvf_constructor
  end interface named_vector_f

contains

  function nvf_constructor&
       (name, coord, btype) result(r)
    character(len=*), intent(in)                :: name
    class(coordinates), intent(in), target      :: coord
    character(len=*), intent(in)                :: btype(:)

    type(named_vector_f), pointer :: r

    integer :: i

    allocate(r)

    r%name  =  name
    r%coord => coord


    allocate(r%bdata(size(btype)))

    ! we do not allocate the params array here but in the
    ! icicles.  It's because we don't know yet how many parameters are
    ! required by each boundary type.

    ! set the boundary types
    do i = 1, size(btype)
       r%bdata(i)%btype = btype(i)
    end do


    ! initialize parent
    r%named_vector = named_vector(&
         name = name,&
         length = coord%length())

  end function nvf_constructor



  !> Given a generalized index alpha it returns a pointer to a vector
  !! where spatial derivatives are stored.  If the requested vector
  !! was not already allocated it allocates it.  The length of vectors
  !! representing spatial derivatives does not count as the length of
  !! their owner.
  !!
  !! @param alpha
  !!
  !! @return pointer to a location where results of spatial derivation
  !! are stored
  function dx(self, alpha)
    class(named_vector_f) :: self
    integer, intent(in) :: alpha(:)

    real, pointer :: dx(:)

    type(d_ptr), allocatable :: temp_ptr(:)
    real, pointer :: dxv(:)
    class(named_vector), pointer :: dxnv
    integer :: i, length

    dx => null()

    if( allocated(self%dx_) ) then
       associate( d => self%dx_ )
         do i = 1, size(d)
            if( all(d(i)%alpha == alpha) ) then
               dx => d(i)%val%vec()
               return
            end if
         end do
       end associate
    else
       allocate(self%dx_(0))
    end if

    if( .not. associated(dx) ) then
       ! generate new vector
       length = self%length()
       dxnv => named_vector(&
            name = "d",&
            length = length)

       ! allocate space
       allocate(dxv(self%length()))
       call dxnv%point(dxv)

       dx => dxnv%vec()

       ! add the vector to the table
       temp_ptr = [self%dx_, d_ptr(alpha = alpha, val = dxnv)]
       self%dx_ = temp_ptr
    end if

  end function dx


  !! @todo allocate dt_ on the fly?
  function dt(self)
    class(named_vector_f) :: self

    real, pointer :: dt(:)

    if(.not. associated(self%dt_)) then
       self%dt_ => named_vector("dt", length = self%length())
    end if

    dt => self%dt_%vec()
  end function dt


  function get_coordinates(self)
    class(named_vector_f), intent(in), target :: self

    class(coordinates), pointer :: get_coordinates
    get_coordinates => self%coord
  end function get_coordinates


end module class_named_vector_f
