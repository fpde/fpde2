!>
!! @file   data.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Mar  3 21:11:35 2012
!!
!! @brief  contains the primary data structure: the icicles
!!
!! @todo icicles referencer should be a submodule to icicles.
!!
module class_icicles

  use constants_module
  use class_platonic
  use class_named_vector
  use class_icicles_referencer
  use logger_module
  !> @todo: later on it could be useful to define a bind(c) compatible
  !! wrapper to icicles
  ! use iso_c_binding

  private

  !> Limited access version of icicles
  type, public, abstract, extends(platonic) :: passive_icicles
   contains
     procedure(get_i), deferred :: get
  end type passive_icicles


  abstract interface
     subroutine get_i(self, name, vec, scal, error)
       import passive_icicles
       class(passive_icicles), target :: self
       character(len=*), intent(in) :: name
       real, pointer, optional :: scal, vec(:)
       integer, intent(out), optional :: error
     end subroutine get_i
  end interface


  !>
  !! Full version of icicles
  !!
  type, public, extends(passive_icicles) :: icicles
     private
     type(named_vector), allocatable :: vectors(:)
     logical :: locked = .false.
   contains
     procedure :: init => init_ic
     procedure :: add
     procedure :: get
     procedure :: get_reference
     procedure :: set_pointers
     procedure :: total_length
     procedure, private :: internal_get
  end type icicles


contains


  subroutine init_ic(p, error)
    class(icicles), target :: p
    integer, optional, intent(out) :: error
    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Icicles"
    allocate(p%vectors(0))
  end subroutine init_ic


  subroutine add(self, name, error)
    class(icicles), target :: self
    character(len=*), intent(in) :: name
    integer, optional, intent(out) :: error

    integer :: n
    type(named_vector) :: v
    type(named_vector), allocatable :: temp_vectors(:)

    if( present(error) ) error = FPDE_STATUS_OK

    if( self%locked ) then
       call self%log(FPDE_LOG_ERROR,&
            "Cannot add entry after calling start().")
       if( present(error) ) error = FPDE_STATUS_ERROR
       return
    end if

    v % name = trim(name)
    ! @todo possible ifort bug with realloc_lhs
    temp_vectors = self%vectors
    self%vectors = [ temp_vectors, v ]

    n = size(self%vectors)

  end subroutine add


  subroutine get(self, name, vec, scal, error)
    class(icicles), target :: self
    character(len=*), intent(in) :: name
    real, pointer, optional :: scal, vec(:)
    integer, intent(out), optional :: error

    class(named_vector), pointer :: v => null()

    if(present(error)) error = FPDE_STATUS_ERROR

    v => self%internal_get(name)
    if( .not. associated(v) ) then
       call self%log(FPDE_LOG_WARNING,&
            "get(): Unable to return result: ["//trim(name)//"].")
       return
    end if

    if( .not. associated(v%val) ) then
       call self%log(FPDE_LOG_WARNING,&
            "get(): Entry found, but null: ["//trim(name)//"].")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    if( present( vec   ) ) vec   => v%val
    if( present( scal  ) ) scal  => v%val(1)

  end subroutine get


  function get_reference(self, name, error) result(ref)
    class(icicles) :: self
    character(len=*), intent(in) :: name
    integer, intent(out), optional :: error

    type(icicles_referencer) :: ref

    class(named_vector), pointer :: v => null()

    if( present(error) ) error = FPDE_STATUS_ERROR

    ! lock icicles, add() will refuse to add anything to icicles from
    ! now on.
    self%locked = .true.

    v => self%internal_get(name)

    if( .not. associated(v) ) then
       call self%log(FPDE_LOG_ERROR,&
            "get_reference(): Unable to create reference")
       return
    end if

    if( present(error) ) error = FPDE_STATUS_OK

    call ref%set_nv(v)
    call ref%init()

  end function get_reference


  function internal_get(self, name) result(v)
    class(icicles), target :: self
    character(len=*), intent(in) :: name

    class(named_vector), pointer :: v

    integer :: i

    v => null()

    do i = 1, size(self%vectors)
       if( self%vectors(i)%name == name) then
          v => self%vectors(i)
          return
       end if
    end do

    call self%log(FPDE_LOG_WARNING, "No entry named ["&
         //trim(name)//"]")

  end function internal_get

!!!!!
!!!!! Following functions are possibly not needed and duplicate the
!!!!! functionallity of referencers
!!!!!


  !> Sets the pointers of icicles entries to a new target.
  !!
  !! If names is present, only names mathcing one of names will be set
  !! to the new target. Additionally, if lengths is present the
  !! lengths of pointers is due to change to the new values given by
  !! lengths.
  !!
  !! @param vec target vector
  !! @param names list of names, defaults to all names if not present
  !! @param lengths list of lengths of vectors, name is nonoptional
  !! if lengths is present
  !! @param error
  !!
  subroutine set_pointers(self, vec, names, lengths, error)
    class(icicles), target :: self
    real, target, intent(in) :: vec(:)
    character(len=*), intent(in), optional :: names(:)
    integer, intent(in), optional :: lengths(:)
    integer, optional, intent(out) :: error

    integer :: i, j, k, inc, tl
    class(named_vector), pointer :: n_v
    logical :: name_matched

    if( present(error) ) error = FPDE_STATUS_OK

    ! check for errors
    if( present(lengths) .and. .not. present(lengths)) then
       call self%log(FPDE_LOG_ERROR,&
            "set_pointers: names is required when lengths is present.")
       if( present(error) ) error = FPDE_STATUS_ERROR
       return
    end if

    if( present(names) .and. present(lengths) ) then
       if( size(names) /= size(lengths) ) then
          call self%log(FPDE_LOG_ERROR,&
               "set_pointers: names and lengths do not have the same size.")
          if( present(error) ) error = FPDE_STATUS_ERROR
          return
       end if
    end if

    if( present(lengths) ) then
       tl = sum(lengths)
    else
       if(present(names)) then
          tl = self%total_length(names)
       else
          tl = self%total_length()
       end if
    end if

    if( tl > size(vec) ) then
       call self%log(FPDE_LOG_ERROR,&
            "set_pointers: Target vector (possibly) too small.")
       if( present(error) ) error = FPDE_STATUS_OK
       return
    end if

    ! actual algorithm for setting pointers
    j = 1
    traverse: do i = 1, size(self%vectors)

       n_v => self%vectors(i)

       if( present(names) ) then

          ! determine the k such that names(j) == self%vectors(i)%name
          name_matched = .false.
          find_name: do k = 1, size(names)
             if(names(k) == n_v%name) then
                name_matched = .true.
                exit find_name
             end if
          end do find_name

          ! continue the outer do loop if no name matches
          if( .not. name_matched ) cycle traverse
       end if

       ! if name was matched then use the length from lengths(:),
       ! otherwise use the old length or zero
       if( present(names) .and. present(lengths) ) then
          ! length(k) corresponds to names(k)
          inc = lengths(k)
       else if( associated(n_v%val) ) then
          inc = size(n_v%val)
       else
          inc = 0
       end if

       if( inc > 0 ) then
          n_v%val => vec(j:j+inc-1)
       else
          nullify(n_v%val)
       end if

       j = j + inc

    end do traverse

  end subroutine set_pointers


  function total_length(self, names, error)
    class(icicles), target :: self
    integer :: total_length
    character(len=*), intent(in), optional :: names(:)
    integer, optional, intent(out) :: error

    integer :: i
    class(named_vector), pointer :: n_v

    if( present(error) ) error = FPDE_STATUS_OK

    total_length = 0

    do i = 1, size(self%vectors)
       n_v => self%vectors(i)

       ! do not count if names are present and name of a vector does
       ! not appear on the list
       if( present(names) ) then
          if( all(names /= n_v%name)) cycle
       end if

       ! if this vector is not associated skip this vector in the
       ! counting
       if( .not. associated(n_v%val) ) cycle

       ! otherwise, increase total_length
       total_length = total_length + size(n_v%val)

    end do

  end function total_length

end module class_icicles
