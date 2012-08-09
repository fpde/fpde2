!>
!! @file   data.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Mar  3 21:11:35 2012
!!
!! @brief  contains the primary data structure: the icicles
!!
!!
!!
module class_icicles

  use constants_module
  use class_platonic
  use helper_module
  use logger_module
  !> @todo: later on it could be useful to define a bind(c) compatible
  !! wrapper to icicles
  ! use iso_c_binding

  private

  type :: named_vector
     real, pointer :: val(:) => null()
     character(len=NAME_LEN) :: name = ""
     integer :: length = 0
  end type named_vector


  type, public, extends(platonic) :: icicles
     private
     type(named_vector), allocatable :: vectors(:)
   contains
     procedure :: init
     procedure :: add
     procedure :: get
     procedure :: set_pointers
     procedure :: total_length
  end type icicles

contains


  subroutine init(p, error)
    class(icicles), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Icicles"
  end subroutine init


  subroutine add(this, name, length)
    class(icicles) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: length

    type(named_vector) :: v

    v % name = name
    v % length = length
    this%vectors = [ this%vectors, v ]

  end subroutine add


  subroutine get(this, name, vec, scal, error)
    class(icicles), target :: this
    character(len=*), intent(in) :: name
    real, pointer, optional :: scal, vec(:)
    ! type(named_vector), pointer, optional :: named
    integer, intent(out), optional :: error

    ! integer :: i
    type(named_vector), pointer :: v

    if(present(error)) error = FPDE_STATUS_OK

    ! nullify pointers, just in case
    ! if( present( named ) ) nullify(named)
    if( present( vec   ) ) nullify(vec)
    if( present( scal  ) ) nullify(scal)

    v => first_match(this%vectors, name)

    if( .not. associated(v) ) then
       if(present(error)) error = FPDE_STATUS_ERROR
       call this%log(FPDE_STATUS_ERROR, "No entry named ["//trim(name)//"]")
       return
    end if

    ! if( present( named ) ) named => v
    if( present( vec   ) ) vec   => v%val
    if( present( scal  ) .and. size(v%val) > 0 ) scal  => v%val(1)

  end subroutine get


  subroutine set_pointers(self, names, vec, error)
    class(icicles), target :: self
    character(len=*), intent(in) :: names(:)
    real, target, intent(in) :: vec(:)
    integer, optional, intent(out) :: error

    integer :: i, n, j
    class(named_vector), pointer :: nv
    n = size(vec)
    j = 1

    if( present(error) ) error = FPDE_STATUS_OK
    do i = 1, size(self%vectors)
       nv => self%vectors(i)
       if( any(names == nv%name) ) then
          if( j+nv%length-1 <= n ) then
             nv%val => vec(j:j+nv%length-1)
             j = j+nv%length
          else
             call self%log(FPDE_LOG_ERROR,&
                  "set_pointers: Target vector too small.")
             if( present(error) ) error = FPDE_STATUS_OK
             return
          end if
       end if
    end do

  end subroutine set_pointers


  function total_length(self, names)
    class(icicles), target :: self
    integer :: total_length
    character(len=*), intent(in), optional :: names(:)

    integer :: i
    class(named_vector), pointer :: nv

    if( .not. present(names) ) then
       total_length = sum(self%vectors%length)
    else
       total_length = 0
       do i = 1, size(self%vectors)
          nv => self%vectors(i)
          if( any(names == nv%name) ) then
             total_length = total_length + nv%length
          end if
       end do
    end if

  end function total_length


  function first_match(nvs, name)
    type(named_vector), intent(in), target :: nvs(:)
    type(named_vector), pointer :: first_match
    character(len=*) :: name

    integer :: i

    nullify(first_match)
    do i = 1, size(nvs)
       if( nvs(i)%name == name ) first_match => nvs(i)
    end do

  end function first_match


end module class_icicles
