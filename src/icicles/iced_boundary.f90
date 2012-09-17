module class_iced_boundary

  use class_platonic
  use class_icicles
  use class_boundary
  use class_icicles_referencer
  use constants_module
  use logger_module
  use boundary_factory

  private

  type, public, extends(platonic) :: iced_boundary
     private
     class(boundary), pointer :: b => null()
     type(icicles), allocatable :: ic
     type(icicles_referencer), allocatable :: refs(:)
     logical :: refs_locked = .false.
     integer :: np = 0
   contains
     procedure :: init
     procedure :: set_boundary
     procedure :: lock_refs
     procedure :: get_param_num
     procedure :: get_type
     procedure :: set_to
     procedure :: restore
     procedure :: get_param_val
     procedure :: get_param_ref
     procedure :: generate_values
     procedure :: update_refs
     procedure :: print
  end type iced_boundary

contains

  subroutine print(self)
    class(iced_boundary), target :: self

    real, pointer :: v(:)
    character(len=:), allocatable :: name
    integer :: err, i

    do i = 1, size(self%refs)
       name = self%refs(i)%get_name()
       call self%ic%get(name = name, vec = v, error = err)
       if( err /= FPDE_STATUS_OK ) then
          print *, name, " NULL"
       else
          print *, name, v
       end if
    end do

  end subroutine print


  subroutine init(p, error)
    class(iced_boundary), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Iced boundary"
  end subroutine init


  function get_type(self) result(t)
    class(iced_boundary) :: self
    character(len=:), allocatable :: t
    t = self%b%type
  end function get_type


  subroutine set_boundary(self, id, error)
    class(iced_boundary) :: self
    ! class(boundary), target :: b
    character(len=*), intent(in) :: id
    integer, optional, intent(out) :: error

    character(len=:), allocatable :: params(:)
    integer :: err, i, np

    if( present(error) ) error = FPDE_STATUS_ERROR

    ! produce new instance of boundary conditions
    self%b => boundary_new(id, err)

    if( err /= FPDE_STATUS_OK ) then
       call self%log(FPDE_LOG_ERROR,&
            "set_boundary(): Error producing boundary.")
       return
    end if

    call self%b%init(err)

    if( err /= FPDE_STATUS_OK ) then
       call self%log(FPDE_LOG_ERROR,&
            "set_boundary(): Could not initialize boundary.")
       return
    end if

    ! no errors from here
    if( present(error) ) error = FPDE_STATUS_OK

    ! initialize icicles using parameters from boundary condition
    params = self%b%get_param_names()

    ! reallocate icicles if needed
    if(allocated(self%ic)) deallocate(self%ic)
    allocate(self%ic)
    call self%ic%init()


    np = size(params)
    do i = 1, np
       call self%ic%add( params(i) )
    end do

    call self%update_refs(error = err)

    if( err /= FPDE_STATUS_OK ) then
       if( present(error) ) error = FPDE_STATUS_ERROR
       call self%log(FPDE_LOG_ERROR,&
            "set_boundary(): references could not be updated.")
       return
    end if


  end subroutine set_boundary


  subroutine lock_refs(self)
    class(iced_boundary) :: self
    self%refs_locked = .true.
  end subroutine lock_refs


  subroutine update_refs(self, error)
    class(iced_boundary) :: self
    integer, intent(out), optional :: error

    character(len=:), allocatable :: params(:)
    integer :: i, np, err

    if(present(error)) error = FPDE_STATUS_ERROR

    params = self%b%get_param_names()
    np = size(params)

    ! fill in the references
    if(allocated(self%refs)) deallocate(self%refs)
    allocate(self%refs(np))

    do i = 1, np
       self%refs(i) = self%ic%get_reference( params(i), error = err )
       if( err /= FPDE_STATUS_OK ) then
          call self%log(FPDE_LOG_ERROR,&
               "update_refs(): unable to get reference for ["&
               //params(i)//"]")
          return
       end if
    end do

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine update_refs


  function get_param_num(self) result(i)
    class(iced_boundary) :: self
    integer :: i
    i = size(self%refs)
  end function get_param_num


  subroutine set_to(self, dest)
    class(iced_boundary) :: self
    real, target, intent(in) :: dest(:,:)

    integer :: i, np

    np = self%get_param_num()

    if( size(dest,2) /= np ) then
       call self%log(FPDE_LOG_WARNING,&
            "set_to(): size of dest doesn't match with&
            & the number of parameters.")
       return
    end if

    do i = 1, np
       call self%refs(i)%set_to(dest(:,i))
    end do
  end subroutine set_to


  !> @bug if set_to wasn't called for every i restore will not work
  !! properly
  subroutine restore(self)
    class(iced_boundary) :: self

    integer :: i

    do i = 1, size(self%refs)
       call self%refs(i)%restore()
    end do
  end subroutine restore


  function get_param_val(self, i) result(vec)
    class(iced_boundary) :: self
    integer, intent(in) :: i
    real, pointer :: vec(:)
    vec => self%refs(i)%get_val()
  end function get_param_val


  function get_param_ref(self, i, error) result(ref)
    class(iced_boundary) :: self
    integer, intent(in) :: i
    type(icicles_referencer) :: ref
    integer, optional, intent(out) :: error

    if( present(error) ) error = FPDE_STATUS_ERROR

    if( self%refs_locked ) then
       call self%log(FPDE_LOG_ERROR,&
            "get_param_ref(): obtaining references has been locked.")
       return
    end if

    if( i > size(self%refs) .or. i < 1 ) then
       call self%log(FPDE_LOG_ERROR,&
            "get_param_ref(): Incorrect reference number.")
       return
    end if

    if( present(error) ) error = FPDE_STATUS_OK

    ref = self%refs(i)

  end function get_param_ref


  subroutine generate_values(self, fin, fout, xin, error)
    class(iced_boundary) :: self
    real, intent(in) :: fin(:,:), xin(:,:)
    ! real, intent(in), optional, target :: params(:)
    real, intent(out) :: fout(:,:)
    integer, intent(out), optional :: error

    integer :: err, np

    np = size(self%refs)

    if( present(error) ) error = FPDE_STATUS_OK

    call self%b%generate_values(self%ic,fin,fout,xin,err)

    if( err /= FPDE_STATUS_OK ) then
       if(present(error)) error = err
       call self%log(FPDE_LOG_ERROR,&
            "generate_values(): Unable to generate values.")
    end if

  end subroutine generate_values

end module class_iced_boundary
