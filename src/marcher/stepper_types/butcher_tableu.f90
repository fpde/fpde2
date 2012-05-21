module class_butcher_tableu

   use constants_module

   private

   type, public :: butcher_tableu

      integer :: s = 0
      real, pointer :: c(:) => null()
      real, pointer :: a(:,:) => null()
      real, pointer :: b(:) => null()
      real, pointer :: ec(:) => null()

   contains

      procedure :: init
      procedure :: test
      procedure :: free

   end type butcher_tableu

contains

   subroutine init(this,error)
      class(butcher_tableu) :: this
      integer, optional, intent(out) :: error
      integer :: s

      s = this % s

      if ( s .gt. 0 ) then
         if ( .not. associated( this % c ) ) allocate( this % c(s) )
         if ( .not. associated( this % a ) ) allocate( this % a(s,s) )
         if ( .not. associated( this % b ) ) allocate( this % b(s) )
         if ( .not. associated( this % ec ) ) allocate( this % ec(s) )
      else
         if ( present(error) ) then
            error = FPDE_STATUS_ERROR
         end if

      end if
   end subroutine init

   subroutine test(this,error)
      class(butcher_tableu) :: this
      integer, optional, intent(out) :: error
      integer :: i
      real :: t, r

      t = norm2(sum(this % a,dim=2,mask=this % a .ne. 0.0)-this % c)/this % s
      r = abs(sum( this % b ) - 1.0)

      if ( present(error) ) then
         if ( (t .lt. epsilon(1.0)) .and. (r .lt. epsilon(1.0)) ) then
            error = FPDE_STATUS_OK
         else
            error = FPDE_STATUS_ERROR
         end if
      end if

   end subroutine test

   subroutine free(this,error)
      class(butcher_tableu) :: this
      integer, optional, intent(out) :: error

      if ( associated( this % c ) ) then
         deallocate( this % c )
         this % c => null()
      end if

      if ( associated( this % a ) ) then
         deallocate( this % a )
         this % a => null()
      end if

      if ( associated( this % b ) ) then
         deallocate( this % b )
         this % b => null()
      end if

      if ( associated( this % ec ) ) then
         deallocate( this % ec )
         this % ec => null()
      end if

      this % s = 0

      if( present( error ) ) then
         error = FPDE_STATUS_OK
      end if

   end subroutine free

end module class_butcher_tableu
