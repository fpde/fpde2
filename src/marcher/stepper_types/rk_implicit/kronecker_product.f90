!>
!! @file   kronecker_product.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sat May 12 17:47:24 2012
!!
!! @brief  Performs operation consisting of kronecker product of
!! A and B matrices modified via two scalars
!!
!! C = alpha * C + beta *( A kron B )
!!
!! where alpha and beta are real numbers. Result is returned
!! in matrix C.
!!
!! @todo
!!  - [0] rewrite docs
!!  - [1] check whether the dimension of the array C can hold
!! the output of this operation.
!!
module kronecker_product

   public :: kronecker, identity_matrix

contains

   function identity_matrix(n,alpha) result(id)
      integer, intent(in) :: n
      real, optional, intent(in) :: alpha
      real, pointer :: id(:,:)
      integer :: i

      if ( n > 0 ) then
         allocate(id(n,n))
      end if

      id = 0.0

      if ( present(alpha) ) then
         do i=1,n
            id(i,i) = alpha
         end do
      else
         do i=1,n
            id(i,i) = 1.0
         end do
      end if

   end function identity_matrix

   subroutine kronecker(A, B, C, alpha, beta)
      real, pointer, intent(in) :: A(:,:), B(:,:)
      real, pointer, intent(inout) :: C(:,:)
      real, optional, intent(in) :: alpha, beta
      integer :: ma, na, mb, nb
      integer :: i, j, i1, j1
      real :: al, be

      ma = size(A,1)
      na = size(A,2)

      mb = size(B,1)
      nb = size(B,2)

      if ( present(alpha) .and. present(beta) ) then
         al = alpha
         be = beta
      else if ( present(alpha) ) then
         al = alpha
         be = 1.0
      else if ( present(beta) ) then
         al = 0.0
         be = beta
      else
         al = 0.0
         be = 1.0
      end if

      do j=1,na
         do i=1,ma
            i1 = (i-1)*mb + 1
            j1 = (j-1)*nb + 1
            C(i1:i1+mb-1,j1:j1+nb-1) = &
                 al*C(i1:i1+mb-1,j1:j1+nb-1) + be*A(i,j)*B
         end do
      end do

   end subroutine kronecker

end module kronecker_product
