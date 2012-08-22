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

  type, public, extends(platonic) :: icicles_referencer
     private
     type(named_vector), pointer :: nv => null()
   contains
     procedure :: set_to
     procedure :: init => init_ir
  end type icicles_referencer


  type :: named_vector
     real, pointer :: val(:) => null()
     ! character(len=NAME_LEN) :: name = ""
     character(len=:), allocatable :: name
     integer :: length = 0
     type(icicles_referencer), allocatable :: refs(:)
  end type named_vector


  type, public, extends(platonic) :: icicles
     private
     type(named_vector), allocatable :: vectors(:)
   contains
     procedure :: init => init_ic
     procedure :: add
     procedure :: get
     procedure :: set_pointers
     procedure :: total_length
  end type icicles

contains

  subroutine init_ir(p, error)
    class(icicles_referencer), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Icicles referencer"
  end subroutine init_ir


  subroutine set_to(self, dest, error)
    class(icicles_referencer) :: self
    real, target :: dest(:)
    integer, optional, intent(out) :: error

    integer :: length

    if(present(error)) error = FPDE_STATUS_OK

    if( .not. associated(self%nv) ) then
       call self%log(FPDE_LOG_WARNING,&
            "Referencer is empty")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    length = self%nv%length

    if(size(dest) < length) then
       call self%log(FPDE_LOG_ERROR,&
            "Unable to change reference, target too short")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    self%nv%val(1:length) => dest(1:length)
  end subroutine set_to


  subroutine init_ic(p, error)
    class(icicles), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Icicles"
    allocate(p%vectors(0))
  end subroutine init_ic


  subroutine add(self, name, length, this_ref, refs)
    class(icicles), target :: self
    character(len=*), intent(in) :: name
    integer, intent(in) :: length
    type(icicles_referencer), optional, intent(out) :: this_ref
    type(icicles_referencer), optional, intent(in) :: refs(:)

    integer :: n
    type(named_vector) :: v
    type(named_vector), allocatable :: temp_vectors(:)

    v % name = trim(name)
    v % length = length
    ! ! @todo possible ifort bug with realloc_lhs
    temp_vectors = self%vectors
    self%vectors = [ temp_vectors, v ]

    n = size(self%vectors)

    ! generate reference
    if( present(this_ref) ) then
       this_ref%nv => self%vectors(n)
    end if

    ! add references
    if( present(refs) ) then
       self%vectors(n)%refs = refs
    end if


  end subroutine add


  subroutine get(self, name, vec, scal, error)
    class(icicles), target :: self
    character(len=*), intent(in) :: name
    real, pointer, optional :: scal, vec(:)
    ! type(named_vector), pointer, optional :: named
    integer, intent(out), optional :: error

    type(named_vector), pointer :: v => null()
    integer :: nv, i

    if(present(error)) error = FPDE_STATUS_OK

    ! nullify pointers, just in case
    ! if( present( vec   ) ) nullify(vec)
    ! if( present( scal  ) ) nullify(scal)

    nv = size(self%vectors)
    do i = 1, nv
       if( self%vectors(i)%name == name) then
          v => self%vectors(i)
          exit
       end if
    end do

    if( associated(v) ) then
       if( associated(v%val) ) then
          if( present( vec   ) ) vec   => v%val
          if( present( scal  ) ) scal  => v%val(1)
       end if
    else
       if(present(error)) error = FPDE_STATUS_ERROR
       call self%log(FPDE_STATUS_ERROR, "No entry named ["&
            //trim(name)//"]")
       return
    end if

  end subroutine get


  subroutine set_pointers(self, vec, names, error)
    class(icicles), target :: self
    real, target, intent(in) :: vec(:)
    character(len=*), intent(in), optional :: names(:)
    integer, optional, intent(out) :: error

    integer :: i, j, k, tl, nv
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

    do i = 1, nv

       n_v => self%vectors(i)

       if( present(names) ) then
          if( all(names /= n_v%name) ) then
             continue
          end if
       end if

       n_v%val => vec(j:j+n_v%length-1)
       if(allocated(n_v%refs)) then
          do k = 1, size(n_v%refs)
             call n_v%refs(k)%set_to(vec(j:j+n_v%length-1))
          end do
       end if
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


end module class_icicles
