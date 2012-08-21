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

  integer, parameter :: buff = 1

  type :: named_vector
     real, pointer :: val(:) => null()
     ! character(len=NAME_LEN) :: name = ""
     character(len=:), allocatable :: name
     integer :: length = 0
  end type named_vector


  type, public, extends(platonic) :: icicles
     private
     type(named_vector), allocatable :: vectors(:)
     integer :: nv = 0
   contains
     procedure :: init
     procedure :: add
     procedure :: clear
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
    allocate(p%vectors(0))
  end subroutine init


  subroutine add(this, name, length, ptr)
    class(icicles) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: length
    real, target, optional, intent(in) :: ptr(:)

    integer :: nv
    type(named_vector) :: v
    type(named_vector), allocatable :: temp_vectors(:)

    v % name = trim(name)
    v % length = length
    if( present(ptr) ) then
       if( size(ptr) >= length ) then
          v % val => ptr
       end if
    end if
    ! ! @todo possible ifort bug with realloc_lhs
    temp_vectors = this%vectors
    this%vectors = [ temp_vectors, v ]

    ! alternative implementation
    ! nv = this%nv
    ! this%nv = nv + 1
    ! if( nv + 1 > size(this%vectors) ) then
    !    ! realloc
    !    temp_vectors = this%vectors
    !    deallocate(this%vectors)
    !    allocate(this%vectors(nv+buff))
    !    this%vectors(1:nv) = temp_vectors
    ! end if
    ! this%vectors(nv+1) = v

  end subroutine add


  subroutine clear(this)
    class(icicles), target :: this
    deallocate(this%vectors)
    allocate(this%vectors(0))
    ! this%nv = 0
  end subroutine clear


  subroutine get(this, name, vec, scal, error)
    class(icicles), target :: this
    character(len=*), intent(in) :: name
    real, pointer, optional :: scal, vec(:)
    ! type(named_vector), pointer, optional :: named
    integer, intent(out), optional :: error

    type(named_vector), pointer :: v
    integer :: nv

    if(present(error)) error = FPDE_STATUS_OK

    ! nullify pointers, just in case
    ! if( present( named ) ) nullify(named)
    if( present( vec   ) ) nullify(vec)
    if( present( scal  ) ) nullify(scal)

    nv = size(this%vectors)
    ! alternative implementation
    ! nv = this%nv
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


  subroutine set_pointers(self, vec, names, error)
    class(icicles), target :: self
    real, target, intent(in) :: vec(:)
    character(len=*), intent(in), optional :: names(:)
    integer, optional, intent(out) :: error

    integer :: i, j, tl, nv
    class(named_vector), pointer :: n_v
    j = 1

    if( present(error) ) error = FPDE_STATUS_OK


    if(present(names)) then
       tl = self%total_length(names)
    else
       tl = self%total_length()
    end if

    ! check size of vec
    if( tl > size(vec) ) then
       call self%log(FPDE_LOG_ERROR,&
            "set_pointers: Target vector too small.")
       if( present(error) ) error = FPDE_STATUS_OK
       return
    end if

    nv = size(self%vectors)
    ! alternative implementation
    ! nv = self%nv

    do i = 1, nv

       n_v => self%vectors(i)

       if( present(names) ) then
          if( all(names /= n_v%name) ) then
             continue
          end if
       end if

       n_v%val => vec(j:j+n_v%length-1)
       j = j+n_v%length

    end do


  end subroutine set_pointers


  function total_length(self, names)
    class(icicles), target :: self
    integer :: total_length
    character(len=*), intent(in), optional :: names(:)

    integer :: i, nv
    class(named_vector), pointer :: n_v

    nv = size(self%vectors)
    ! alternative implementation
    ! nv = self%nv

    if( .not. present(names) ) then
       total_length = sum(self%vectors(1:nv)%length)
    else
       total_length = 0
       do i = 1, nv
          n_v => self%vectors(i)
          if( any(names == n_v%name) ) then
             total_length = total_length + n_v%length
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
