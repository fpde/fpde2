!>
!! @file   cartesian2d.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Nov  9 15:19:08 2012
!!
!! @brief
!!
!!
!! Identification of the boundary regions is as follows:
!!
!!     3
!!   +---+
!! 1 |   | 2
!!   +---+
!!     4
!!
!! method bregion() returns the arrays of one dimensional indices.
!! Below is the relation of one dimensional indices to two dimensional
!! idicies:
!!
!! Id       1d idx             2d idx
!! 1        k = 1  + nx*(i-1)  (1, i )
!! 2        k = ny + nx*(i-1)  (nx,i )
!! 3        k = i              (i, 1 )
!! 4        k = (ny-1)*nx + i  (i, ny)
!!
!! where i = 1,2,3,...,n
!!       n = nx for Id = 3,4
!!           ny for Id = 1,2
!!
!! For each id length(bregion(id)) = n.
!!
!! The convention is to remain constant through the revisions of this
!! code.
!!
module class_coordinates_c2d

  use class_regions
  use class_coordinates_c
  use class_named_vector_
  use class_named_vector_user_
  use logger_module

  private

  integer, parameter, public :: &
       coordinates_c2d_left   = 1,&
       coordinates_c2d_right  = 2,&
       coordinates_c2d_top    = 3,&
       coordinates_c2d_bottom = 4

  type, public, extends(coordinates_c) :: coordinates_c2d
     private
     type(regions) :: bregion_
     integer :: nx_ = 0, ny_ = 0
     class(named_vector_user), pointer :: x_ => null(), y_ => null()
   contains
     procedure :: nx
     procedure :: bregion
     procedure :: var
     procedure :: dim
     procedure :: length
     procedure :: vec2d
  end type coordinates_c2d

  interface coordinates_c2d
     module procedure c2d_new
  end interface coordinates_c2d


contains

  function nx(self) result(r)
    class(coordinates_c2d), intent(in) :: self
    integer :: r(2)
    r(1) = self%nx_
    r(2) = self%ny_
  end function nx


  function c2d_new(x, y, n) result(r)
    class(named_vector_user), target, intent(in) :: x, y
    integer, intent(in) :: n(2)

    type(coordinates_c2d), pointer :: r

    integer :: nx, ny

    allocate(r)
    r%name = "coordinates_c2d"

    if( x%length() /= product(n) ) then
       call r%loge("coordinates_c2d(): Incompatible size of x and n")
       deallocate(r)
       return
    end if

    if( y%length() /= product(n) ) then
       call r%loge("coordinates_c2d(): Incompatible size of y and n")
       deallocate(r)
       return
    end if

    if( any(n<3) ) then
       call r%loge("coordinates_c2d(): one of elements of n&
            & is too small (n<3)")
       deallocate(r)
       return
    end if

    nx = n(1)
    ny = n(2)

    r%x_ => x
    r%y_ => y
    r%nx_ = nx
    r%ny_ = ny

    call r%bregion_%new(id = 1, idx = [(1+(i-1)*nx, i=1,ny)])
    call r%bregion_%new(id = 2, idx = [(i*nx,       i=1,ny)])
    call r%bregion_%new(id = 3, idx = [(i,          i=1,nx)])
    call r%bregion_%new(id = 4, idx = [(i+nx*(ny-1),i=1,nx)])

  end function c2d_new


  function bregion(self, id)
    class(coordinates_c2d) :: self
    integer, intent(in) :: id

    integer, pointer :: bregion(:)

    bregion => self%bregion_%region(id)
  end function bregion


  function var(self, n)
    class(coordinates_c2d) :: self
    integer, intent(in) :: n

    class(named_vector_user), pointer :: var

    select case(n)
    case(1)
       var => self%x_
    case(2)
       var => self%y_
    case default
       call self%loge("var(): Wrong argument, only n=1 or n=2 are permited.")
       nullify(var)
    end select

  end function var


  function dim(self)
    class(coordinates_c2d) :: self
    integer :: dim
    dim = 2
  end function dim


  function length(self)
    class(coordinates_c2d) :: self
    integer :: length
    length = self%nx_ * self%ny_
  end function length


  function vec2d(self, v)
    class(coordinates_c2d) :: self
    class(named_vector_user), target :: v

    real, pointer :: vec2d(:,:), vec1d(:)

    if( self%length() /= v%length() ) then
       call self%loge("vec2d(): Incompatible length of the input vector")
       return
    end if

    vec1d => v%vec()
    vec2d(1:self%nx_,1:self%ny_) => vec1d

  end function vec2d


end module class_coordinates_c2d
