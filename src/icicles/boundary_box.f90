module class_boundary_box

  use class_platonic
  use class_boundary
  use constants_module
  use logger_module

  private

  type :: entry
     class(boundary), pointer :: left => null()
     class(boundary), pointer :: right => null()
  end type entry

  type, public, extends(platonic) :: boundary_box
     private
     type(entry), allocatable :: entries(:)
     class(boundary), pointer :: default => null()
     integer, allocatable :: nx(:)
   contains
     procedure :: init
     procedure :: add
     procedure :: get
     procedure :: get_icw_param_names
  end type boundary_box

contains

  subroutine init(p, error)
    class(boundary_box), target :: p
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK

    p%name = "boundary_box"

    allocate(p%entries(0))

  end subroutine init


  subroutine add(self, var, left, right, default, error)
    class(boundary_box) :: self
    integer, intent(in), optional :: var
    class(boundary), target, intent(in), optional :: left, right, default
    integer, intent(out), optional :: error

    ! @todo entries is used as a temporary variables untill
    ! ifort bug with reallocating a=[a,1] is fixed or gfortran is used
    type(entry), allocatable :: entries(:)
    type(entry) :: e
    integer :: ne
    ! variable used to hold a new array

    ne = size(self%entries)

    if( present(error) ) error = FPDE_STATUS_OK

    if( present(var) ) then
       ! realloc if needed
       if( var > ne ) then
          entries = [self%entries, spread(e,1,var-ne)]
          call move_alloc(entries, self%entries)
       end if

       ! set the appropriate pointers
       if( present(left)  ) self%entries(var)%left => left
       if( present(right) ) self%entries(var)%right => right

    else
       if( present(default) ) then
          self%default => default
       end if
    end if

  end subroutine add


  subroutine get(self, var, left, right, error)
    class(boundary_box) :: self
    integer, intent(in) :: var
    class(boundary), pointer, intent(out), optional :: left, right
    integer, intent(out), optional :: error

    integer :: i

    if(present(error)) error = FPDE_STATUS_OK

    associate( e => self%entries, d => self%default )

      if( var > size(e) ) then
         ! default boundary conditions
         if(present(left )) left  => d
         if(present(right)) right => d
      else
         if(present(left)) then
            if(associated(e(var)%left)) then
               left => e(var)%left
            else
               left => d
            end if
         end if

         if(present(right)) then
            if(associated(e(var)%right)) then
               right => e(var)%right
            else
               right => d
            end if
         end if
      end if
    end associate

    ! at this point left and right should be associated
    if( present(left) .and. .not. associated(left) .or.&
         present(right) .and. .not. associated(right)) then
       ! otherwise, print error
       call self%log(FPDE_LOG_ERROR,&
            "Boundary condition not found.")

       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

  end subroutine get


  function get_icw_param_names(self, fname, spatial) result(r)
    class(boundary_box) :: self
    character(len=*) :: fname, spatial(:)

    character(len=:), allocatable :: r(:)

    character(len=:), allocatable :: name_left(:), name_right(:)
    integer :: i, maxlen
    class(boundary), pointer :: left, right

    allocate( character(len=0) :: r(0) )

    do i = 1, size(spatial)
       call self%get(i,left = left, right = right)

       name_left  = left %get_icw_param_names(fname, spatial(i),icw_dir_left )
       name_right = right%get_icw_param_names(fname, spatial(i),icw_dir_right)

       maxlen=max(len(r),len(name_left),len(name_right))
       r = [character(len=maxlen) :: r,name_left,name_right]
    end do

  end function get_icw_param_names


  subroutine update_icicles(self, nx)
    class(boundary_box) :: self
    integer, intent(in) :: nx(:)

    ! extend entries
  end subroutine update_icicles

end module class_boundary_box
