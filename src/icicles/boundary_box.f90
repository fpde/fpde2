module class_boundary_box

  use class_platonic
  use class_icicles
  use class_boundary
  use class_iced_boundary
  use constants_module
  use logger_module

  private

  type, public, extends(platonic) :: boundary_box
     private
     type(iced_boundary), allocatable :: entries(:,:)
     class(boundary), pointer :: default => null()
     integer, allocatable :: nx(:)
     logical :: after_init = .false.
     logical :: after_generate = .false.
   contains
     !> to be called before init()
     procedure :: set_nx
     !> init()
     procedure :: init
     !> after init() only
     procedure :: add_default
     procedure :: add
     procedure :: get
     procedure :: generate_ic_data
  end type boundary_box


contains

  subroutine set_nx(self, nx)
    class(boundary_box) :: self
    integer :: nx(:)

    if( .not. self%after_init) then
       self%nx = nx
    else
       call self%log(FPDE_LOG_WARNING,&
            "Trying to set nx after calling init(), set_nx() will be&
            & ignored.")
       return
    end if
  end subroutine set_nx


  subroutine init(p, error)
    class(boundary_box), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK

    if( .not. p%after_init) then
       if( .not. allocated(p%nx) ) then
          call p%log(FPDE_LOG_ERROR,&
            "Trying to call init() before set_nx().")
          if(present(error)) error = FPDE_STATUS_ERROR
          return
       end if
       allocate(p%entries(size(p%nx),2))
       p%name = "boundary_box"
       p%after_init = .true.
    else
       call p%log(FPDE_LOG_WARNING,&
            "Trying to call init() for a second time, this call to &
            &init() will be ignored.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

  end subroutine init


  subroutine add_default(self, b, error)
    class(boundary_box) :: self
    class(boundary), target, intent(in) :: b
    integer, intent(out), optional :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_OK

    if( .not. self%after_init ) then
       call self%log(FPDE_LOG_ERROR,&
            "add_default() called before init")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    else
       do i = 1, size(self%entries,1)
          call self%entries(i,1)%init()
          call self%entries(i,1)%set_boundary(b)
          call self%entries(i,2)%init()
          call self%entries(i,2)%set_boundary(b)
       end do
    end if

  end subroutine add_default


  subroutine add(self, var, lr, b, error)
    class(boundary_box) :: self
    integer, intent(in) :: var, lr
    class(boundary), target, intent(in) :: b
    integer, intent(out), optional :: error

    if(present(error)) error = FPDE_STATUS_OK

    if( .not. self%after_init) then
       call self%log(FPDE_LOG_ERROR,&
            "add() called before init().")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( var > size(self%entries,1) ) then
       call self%log(FPDE_LOG_ERROR,&
            "add() called with var > dim.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( lr > 2 .or. lr < 1 ) then
       call self%log(FPDE_LOG_ERROR,&
            "add() called with lr different than 1 or 2.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    call self%entries(var,lr)%set_boundary(b)

  end subroutine add


  subroutine get(self, var, lr, ib, error)
    class(boundary_box), target :: self
    integer, intent(in) :: var, lr
    type(iced_boundary), pointer, intent(out) :: ib
    integer, intent(out), optional :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_OK

    if( .not. self%after_init) then
       call self%log(FPDE_LOG_ERROR,&
            "get() called before init().")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( var > size(self%entries,1) ) then
       call self%log(FPDE_LOG_ERROR,&
            "get() called with var > dim.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    if( lr > 2 .or. lr < 1 ) then
       call self%log(FPDE_LOG_ERROR,&
            "get() called with lr different than 1 or 2.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ib => self%entries(var,lr)

  end subroutine get


  subroutine generate_ic_data(self, var, fname, spatial, names, refs, error)
    class(boundary_box), target :: self
    integer, intent(in) :: var
    character(len=*), intent(in) :: fname, spatial(:)
    character(len=:), allocatable, intent(out) :: names(:)
    type(icicles_referencer), allocatable, intent(out) :: refs(:)
    integer, intent(out), optional :: error

    integer :: j, k, maxlen
    type(iced_boundary), pointer :: ib
    integer, pointer :: nx(:)
    character(len=:), allocatable :: ns(:)
    type(icicles_referencer), allocatable :: rs(:), rs_temp(:)


    if(present(error)) error = FPDE_STATUS_OK

    if( .not. self%after_init) then
       call self%log(FPDE_LOG_ERROR,&
            "Calling generate_ic_data() before init().")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    ! this function should be called only once
    if( self%after_generate ) then
       call self%log(FPDE_LOG_ERROR,&
            "Calling generate_ic_data() for a second time.")
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if


    if( allocated(names) ) deallocate(names)
    if( allocated(refs) )  deallocate(refs)

    allocate(character(len=0)::names(0))
    allocate(refs(0))

    nx => self%nx

    do j = 1, 2
       ib => self%entries(var,j)
       call ib%generate_references(&
            length = product([nx(:var-1),1,nx(var+1:)]),&
            refs = rs)

       ns = ib%generate_names(&
            fname = fname,&
            var = spatial(var),&
            dir = merge(icw_dir_left,icw_dir_right,j==1))

       do k = 1, size(ns)
          ns(k) = ns(k)
       end do

       maxlen=max(len(ns),len(names))
       names = [character(len=maxlen) :: names,ns]
       rs_temp = refs
       refs = [rs_temp,rs]
    end do

    self%after_generate = .true.

  end subroutine generate_ic_data

end module class_boundary_box
