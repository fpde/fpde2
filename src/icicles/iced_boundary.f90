module class_iced_boundary
  use class_platonic
  use class_icicles
  use class_boundary
  use constants_module
  use logger_module

  private

  !! @todo move it out to the separate file
  type, public, extends(platonic) :: iced_boundary
     private
     class(boundary), pointer :: b => null()
     type(icicles), allocatable :: ic
   contains
     procedure :: init
     procedure :: set_boundary
     procedure :: generate_references
     procedure :: generate_names
     procedure :: generate_values
  end type iced_boundary

contains

  subroutine init(p, error)
    class(iced_boundary), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK
    p%name = "Iced boundary"
  end subroutine init


  subroutine set_boundary(self, b)
    class(iced_boundary) :: self
    class(boundary), target :: b
    self%b => b
  end subroutine set_boundary


  subroutine generate_values(self, fin, fout, xin, error)
    class(iced_boundary) :: self
    real, intent(in) :: fin(:,:), xin(:,:)
    real, intent(out) :: fout(:,:)
    integer, intent(out), optional :: error

    integer :: err

    if( .not. associated(self%b) ) then
       call self%log(FPDE_LOG_ERROR,&
            "Boundary is not associated.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if


    if( .not. allocated(self%ic) ) then
       call self%log(FPDE_LOG_ERROR,&
            "Icicles are not associated.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    call self%b%generate_values(&
         self%ic,fin,fout,xin,err)

    if(present(error)) error = err
  end subroutine generate_values


  subroutine generate_references(self, length, refs)
    class(iced_boundary) :: self
    integer, intent(in) :: length
    type(icicles_referencer), allocatable, intent(out) :: refs(:)

    integer :: i, n
    character(len=:), allocatable :: params(:)
    type(icicles_referencer) :: r

    if( .not. allocated(self%ic) ) then
       allocate(self%ic)
       call self%ic%init()
    else
       ! @todo error
       return
    end if

    ! do not change params definition without altering
    ! get_icw_param_names, both tables have to be ordered in the
    ! exactly same way
    params = self%b%get_param_names()

    n = size(params)

    ! it is crutial for the refs to be ordered in exact same way as params
    if( allocated(refs) ) deallocate(refs)
    allocate(refs(n))

    do i = 1, n
       call refs(i)%init()
       call self%ic%add(&
            name = params(i),&
            length = length,&
            this_ref = refs(i))
    end do

  end subroutine generate_references


  function generate_names(self, fname, var, dir) result(r)
    class(iced_boundary) :: self
    character(len=*), intent(in) :: fname, var, dir

    character(len=:), allocatable :: r(:)

    character(len=:), allocatable :: params(:)
    integer :: i, l

    params = self%b%get_param_names()

    !   params + fname + var
    l =  len_trim(icw_boundary_name)&
         + len_trim(fname)&
         + len_trim(var)&
         + len_trim(dir)&
         + maxval(len_trim(params))&
         + 5                    !commas + brackets

    allocate(character(len=l) :: r(size(params)))

    do i = 1, size(r)
       r(i) = icw_boundary_name//"("//&
            trim(fname)//","//&
            trim(var)//","//&
            trim(dir)//","//&
            trim(params(i))//")"
    end do

  end function generate_names


end module class_iced_boundary
