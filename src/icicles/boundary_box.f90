module class_boundary_box

  use class_platonic
  use class_boundary
  use class_iced_boundary
  use constants_module
  use logger_module

  private


  !! @todo add a passive_boundary_box with limited privilages
  type, public, extends(platonic) :: boundary_box
     private
     type(iced_boundary), allocatable :: entries(:,:)
     integer :: dim = 0
     logical :: after_init = .false.
   contains
     !> to be called before init()
     procedure :: set_dim
     !> init()
     procedure :: init
     !> after init() only
     procedure :: get_dim
     procedure :: set
     procedure :: get
     procedure :: print

     procedure, private :: boundary_box_assignment
     generic :: assignment(=) => boundary_box_assignment
  end type boundary_box

  ! ! constructor
  ! interface assignment(=)
  !    module procedure :: boundary_box_assignment
  ! end interface

contains


  subroutine print(self)
    class(boundary_box), intent(in) :: self

    integer :: i, side

    if( .not. self%after_init ) return

    do i = 1, self%dim
       do side = 1, 2
          associate(ib => self%entries(i,side))
            print *, trim(ib%get_type())
            call ib%print()
            print *, ""
          end associate
       end do
    end do
    print *, ""

  end subroutine print


  subroutine boundary_box_assignment(self, box)
    class(boundary_box), intent(out) :: self
    type(boundary_box), intent(in) :: box

    integer :: i, side

    ! print *, "====== Assignment", box%dim

    self%dim = box%dim

    if( .not. box%after_init ) return

    self%after_init = .true.
    self%entries = box%entries

    do i = 1, self%dim
       do side = 1, 2
          associate(ib => self%entries(i,side))
            call ib%update_refs()
          end associate
       end do
    end do

  end subroutine boundary_box_assignment


  subroutine set_dim(self, dim, error)
    class(boundary_box) :: self
    integer :: dim
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_ERROR

    if( self%after_init ) then
       call self%log(FPDE_LOG_WARNING,&
            "set_dim(): called after init(), ignoring.")
       return
    end if

    if( dim <= 0 ) then
       call self%log(FPDE_LOG_ERROR,&
               "set_dim(): dim > 0 required.")
       return
    end if

    ! if we are here, all went good and we continue
    if(present(error)) error = FPDE_STATUS_OK

    self%dim = dim

  end subroutine set_dim


  function get_dim(self) result(dim)
    class(boundary_box) :: self
    integer :: dim
    dim = self%dim
  end function get_dim


  subroutine init(p, error)
    class(boundary_box), target :: p
    integer, optional, intent(out) :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_ERROR

    if( p%after_init ) then
       call p%log(FPDE_LOG_WARNING,&
            "init(): called for a second time, ignoring.")
       return
    end if

    if( p%dim == 0 ) then
       call p%log(FPDE_LOG_ERROR,&
            "init(): called before set_dim(), call set_dim() first.")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    allocate(p%entries(p%dim,2))

    do i = 1, p%dim
       call p%entries(i,1)%init()
       call p%entries(i,2)%init()
    end do

    p%name = "boundary_box"
    p%after_init = .true.

  end subroutine init


  subroutine set(self, id, dir, side, error)
    class(boundary_box) :: self
    integer, intent(in), optional :: dir, side
    character(len=*), intent(in) :: id
    integer, intent(out), optional :: error

    integer :: err, i, err1, err2

    if(present(error)) error = FPDE_STATUS_ERROR

    if( .not. self%after_init) then
       call self%log(FPDE_LOG_ERROR,&
            "set() called before init().")
       return
    end if

    if( present(dir) ) then
       if( dir > size(self%entries,1) ) then
          call self%log(FPDE_LOG_ERROR,&
               "set() called with dir > dim.")
          return
       end if
    end if

    if( present(side) ) then
       if( side > 2 .or. side < 1 ) then
          call self%log(FPDE_LOG_ERROR,&
               "set() called with side different than 1 or 2.")
          return
       end if
    end if

    err = FPDE_STATUS_OK

    ! the actual setting of the boundary
    if( present(dir) .and. present(side) ) then
       call self%entries(dir,side)%set_boundary(id, err)

    ! dir is present but side not
    else if( present(dir) ) then
       call self%entries(dir,1)%set_boundary(id, err)
       call self%entries(dir,2)%set_boundary(id, err)

    ! here dir and side are not present
    else if( .not. present(side) ) then
       do i = 1, self%dim
          call self%entries(i,1)%set_boundary(id, err1)
          call self%entries(i,2)%set_boundary(id, err2)
          if( err1 /= FPDE_STATUS_OK .or. err2 /= FPDE_STATUS_OK ) then
             err = FPDE_STATUS_ERROR
             exit
          end if
       end do

    ! side is present but dir is not
    else
       call self%log(FPDE_LOG_ERROR,&
            "set(): Use of [side] without [dir] is forbidden.")
       return

    end if

    ! err is set in every of the above cases or the function returns
    ! in the last case, so checking err here makes sense.
    if( err /= FPDE_STATUS_OK ) then
       call self%log(FPDE_LOG_ERROR,&
            "set(): Unable to create boundary conditions.")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine set


  function get(self, dir, side, error) result(ib)
    class(boundary_box), target :: self
    integer, intent(in) :: dir, side
    integer, intent(out), optional :: error

    type(iced_boundary), pointer :: ib

    if(present(error)) error = FPDE_STATUS_ERROR
    ib => null()

    if( .not. self%after_init) then
       call self%log(FPDE_LOG_ERROR,&
            "get() called before init().")
       return
    end if

    if( dir > size(self%entries,1) ) then
       call self%log(FPDE_LOG_ERROR,&
            "get() called with dir > dim.")
       return
    end if

    if( side > 2 .or. side < 1 ) then
       call self%log(FPDE_LOG_ERROR,&
            "get() called with side different than 1 or 2.")
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    ib => self%entries(dir,side)

  end function get


end module class_boundary_box
