      subroutine invmt
     .  (n          ,a          ,ainv       ,fail       )
!***********************************************************************
! Computes the inverse of a matrix by LU factorization: 
!               -1
!     ainv = [a]
!
! (M. Estrada, 2015)
!-----------------------------------------------------------------------
!     
!     n     : (in)  matrix size (number of columns and rows)
!     a     : (in)  matrix (nxn)
!     ainv  : (out) inverse matrix (nxn)
!     fail  : (out) fail flag (true if inverse process fails)
!     
!***********************************************************************
!     
!     Variable declaration
!     --------------------
      implicit none
!      
!     Arguments
      integer n
      double precision a(n,n), ainv(n,n)
      logical fail
!      
!     Local variables
      integer lda
      integer lwork
      integer info
      integer ipiv(n)
      double precision work(n)
!     
!-----------------------------------------------------------------------
!     
!     Set control fail flag and other variables
      fail = .false.
      lda = n
      lwork = n
!      
!     Factor the matrix
      call dgetrf ( n, n, a, lda, ipiv, info )
!      
      if ( info .ne. 0 ) then
        fail = .true.
        return
      end if
!      
!     Compute the inverse of the matrix
      ainv = a
      call dgetri ( n, ainv, lda, ipiv, work, lwork, info )
 
      if ( info .ne. 0 ) then
        fail = .true.
        return
      end if
!      
      return
      end
