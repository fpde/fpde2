!>
!! @file   kronecker_prod_OUT.f90
!! @author Maciej Maliborski <maciej.maliborski@gmail.com>
!! @date   Sat May 12 18:07:27 2012
!!
!! @brief Testing kroncker poroduct of A and B matrices.
!!
!!
program kronecker_product_test

   use kronecker_product

   integer i, j
   integer, parameter :: ma=4, na=8
   integer, parameter :: mb=2, nb=3
   integer, parameter :: mc = ma*mb, nc=na*nb
   real, pointer :: A(:,:), B(:,:), C(:,:)

   allocate(A(ma,na), B(mb,nb), C(mc,nc))

   do j = 1, na
      do i = 1, ma
         A(i,j) = i*j
      end do
   end do

   do j = 1, nb
      do i = 1, mb
         B(i,j) = i + j
      end do
   end do

   call kronecker(A, B, C, 0.0, 1.0)

   print *, 'A:'
   do i = 1, ma
      print 11, (A(i,j), j = 1, na)
   end do
   print *, ''

   print *, 'B:'
   do i = 1, mb
      print 11, (B(i,j), j = 1, nb)
   end do
   print *, ''

   print *, 'C:'
   do i = 1, mc
      print 11, (C(i,j), j = 1, nc)
   end do

11 FORMAT (2000(f9.2))

   deallocate(A, B, C)

end program kronecker_product_test
