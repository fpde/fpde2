program lusolve_test

   ! use lapack95
   use mkl95_lapack, only : getrf, getrs

   integer, parameter :: n=3
   integer :: i, j, status
   integer, pointer :: ipiv(:)
   real, pointer :: A(:,:), B(:)

   allocate(A(n,n),B(n),ipiv(n))

   A(1,:) = [ 0., 5., 5. ]
   A(2,:) = [ 2., 9., 0. ]
   A(3,:) = [ 6., 8., 8. ]

   ! A(1,:) = [ 0., 2., 6. ]
   ! A(2,:) = [ 5., 9., 8. ]
   ! A(3,:) = [ 5., 0., 8. ]

   ipiv = 0
   B(:) = [ 0., 2., 6. ]

   print *, 'A:'
   do i=1,n
      print 11, (A(i,j), j=1,n)
   end do

   !> LU factorize matrix A
   call getrf(a = A, ipiv = ipiv, info = status)
   if ( status .eq. 0 ) then
      print *, "LU factorization success"
   else if ( status < 0 ) then
      print *, "LU factorization failed, the ", &
           status, "-th parameter had an illegal value"
   else
      print *, "LU factorization failed, the ", &
           "U_{", status, status, "} is 0"
   end if

   ! print *, 'A'
   ! do i=1,n
   !    print 11, (A(i,j), j=1,n)
   ! end do

   !> solve LU system
   call getrs(a=A, ipiv=ipiv, b=B, info = status)
   if ( status .ne. 0 ) then
      print *, "LU solver failed, the ", &
           status, "-th parameter had an illegal value"
   else
      print *, "LU solver success"
   end if

   print *, 'X:'
   do i=1,n
      print 11, B(i)
   end do

   print *, 'ipiv:'
   print *, ipiv

   ! call getri( A, ipiv )
   ! print *, 'A^{-1}'
   ! do i=1,n
   !    print 11, (A(i,j), j=1,n)
   ! end do

11 FORMAT (2000(es22.15))

   deallocate(A,B,ipiv)

end program lusolve_test
