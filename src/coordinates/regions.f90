module class_regions

  use class_platonic

  private

  type :: region
     private
     integer, pointer :: idx(:) => null()
     integer :: id = 0
  end type region


  type, public, extends(platonic) :: regions
     private
     type(region), allocatable :: regs(:)
   contains
     procedure :: new => new_region
     procedure :: region => get_region
  end type regions

contains

  subroutine new_region(self, id, idx)
    class(regions) :: self
    integer, intent(in) :: id, idx(:)

    integer, pointer :: new_idx(:)

    ! copy the idx table
    allocate(new_idx(size(idx)))
    new_idx = idx

    if( .not. allocated(self%regs)) allocate(self%regs(0))
    self%regs = [self%regs, region(idx = new_idx, id = id)]
  end subroutine new_region


  function get_region(self, i)
    class(regions) :: self
    integer, intent(in) :: i

    integer, pointer :: get_region(:)

    integer :: k

    get_region => null()

    do k = 1, size(self%regs)
       if( self%regs(i)%id == i ) then
          get_region => self%regs(i)%idx
       end if
    end do

  end function get_region


end module class_regions
