!>
!! @file   named_vector_f_implementation.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Nov  2 16:44:18 2012
!!
!! @brief
!!
!! @todo allocate dx and dt on the fly and remove arguments "dx" and
!! "dt" from the constructor
!!
!!
!!
module class_named_vector_f_implementation_ghost

  use class_named_vector_
  use class_named_vector_user_
  use class_named_vector_implementation_
  use class_named_vector_f

  use class_generic_function

  use class_bbox

  use class_icicles_user_

  use class_derivator
  use class_coordinates

  private


  type :: d_ptr
     integer, allocatable :: alpha(:)
     class(named_vector), pointer :: val => null()
  end type d_ptr


  type, public, extends(named_vector_f) :: named_vector_f_implementation
     private
     class(bbox), pointer         :: bbox_  => null()
     class(generic_function), pointer :: bbox_update_ => null()
     type(d_ptr), allocatable     :: dx_(:)
     class(named_vector), pointer :: dt_    => null()
     class(derivator), pointer    :: d_     => null()
   contains
     procedure :: c => get_coordinates
     procedure :: dx
     procedure :: dx_update
     procedure :: dt => der_t
     procedure :: bbox_param
     procedure :: bbox_nparam
     procedure, private :: bbox_update
     procedure :: bbox => get_bbox
  end type named_vector_f_implementation


  interface named_vector_f_implementation
     module procedure :: nvf_constructor
  end interface named_vector_f_implementation

  public :: nvf_constructor

contains

  function nvf_constructor&
       (name, d, btypes, bbox_update) result(r)
    character(len=*), intent(in)                :: name
    class(derivator), intent(in), target        :: d
    character(len=*), intent(in), optional      :: btypes(:)
    class(generic_function), intent(in), target, optional :: bbox_update

    type(named_vector_f_implementation), pointer :: r
    class(coordinates), pointer :: c

    integer :: i, length

    allocate(r)

    ! assign derivator
    r%d_ => d

    ! generate boundary box
    if( present(btypes) ) then
       r%bbox_ => d%bbox(btypes)
    end if

    ! save the bbox_update function
    if( present(bbox_update) ) then
       r%bbox_update_ => bbox_update
    end if

    !! @todo check if bbox_update .and. btypes are both present

    ! get length from coordinates, equivalent to:
    ! length = d%coordinates()%length()
    c => d%coordinates()
    length = c%length()

    ! initialize parent
    r%named_vector_implementation&
         = named_vector_implementation(&
         name = name,&
         length = length)

  end function nvf_constructor


  !! @todo allocate results of dx() on the fly
  function dx(self, alpha)
    use class_named_vector_user_

    class(named_vector_f_implementation) :: self
    integer, intent(in) :: alpha(:)

    class(named_vector_user), pointer :: dx

    type(d_ptr), allocatable :: temp_ptr(:)
    real, pointer :: dxv(:)
    class(named_vector), pointer :: dxnv
    integer :: i, length

    dx => null()

    if( allocated(self%dx_) ) then
       associate( d => self%dx_ )
         do i = 1, size(d)
            if( all(d(i)%alpha == alpha) ) then
               dx => d(i)%val
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
       dxnv => named_vector_implementation(&
            name = "d",&
            length = length)
       dx => dxnv

       ! allocate space
       allocate(dxv(self%length()))
       call dxnv%point(dxv)

       ! add the vector to the table
       temp_ptr = [self%dx_, d_ptr(alpha = alpha, val = dxnv)]
       self%dx_ = temp_ptr
    end if

  end function dx


  subroutine dx_update(self, alpha, ic)
    class(named_vector_f_implementation) :: self
    integer, intent(in) :: alpha(:,:)
    class(icicles_user), intent(in) :: ic

    call self%bbox_update(ic)
    call self%d_%dx(self,alpha)
  end subroutine dx_update


  !! @todo allocate dt_ on the fly?
  function der_t(self)
    class(named_vector_f_implementation) :: self

    class(named_vector_user), pointer :: der_t
    real, pointer :: dt

    ! if(.not. associated(self%dt_)) then
    !    self%dt_ => named_vector("dt", length = self%length())
    ! end if
    der_t => self%dt_
  end function der_t


  function get_coordinates(self)
    class(named_vector_f_implementation), intent(in), target :: self

    class(coordinates), pointer :: get_coordinates
    get_coordinates => self%d_%coordinates()
  end function get_coordinates


  function bbox_param(self, id, param) result(r)
    class(named_vector_f_implementation) :: self
    integer, intent(in) :: id, param

    class(named_vector_user), pointer :: r

    r => self%bbox_%param(id, param)
  end function bbox_param


  function bbox_nparam(self, id) result(r)
    class(named_vector_f_implementation) :: self
    integer, intent(in) :: id
    integer :: r
    r = self%bbox_%num_param(id)
  end function bbox_nparam


  subroutine bbox_update(self, ic)
    class(named_vector_f_implementation) :: self
    class(icicles_user), target :: ic
    call self%bbox_update_%call(ic)
  end subroutine bbox_update


  function get_bbox(self) result(r)
    use class_bbox_user
    class(named_vector_f_implementation) :: self
    class(bbox_user), pointer :: r
    r => self%bbox_
  end function get_bbox


end module class_named_vector_f_implementation_ghost
