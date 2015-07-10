      subroutine faclu
     .  (n          ,a          ,lu         ,info       )
!***********************************************************************
! Computes the LU factorization of matrix A: 
! 
!     lu = L and U elements of A factorization. Ones of L matrix are 
!     not stored
!
! (M. Estrada, 2015)
!-----------------------------------------------------------------------
!     
!     n     : (in)  matrix size (number of columns and rows)
!     a     : (in)  matrix (nxn)
!     lu    : (out) LU factors matrix (nxn), ones of L are not stored
!     info  : (out) fail flag 
!                   0 if no errors
!                   1 if factorization is not possible
!                   2 if matrix a is singular
!     
!***********************************************************************
!     
!     Variable declaration
!     --------------------
      implicit none
!      
!     Arguments
      integer n, info
      double precision a(n,n), lu(n,n)
!      
!     Local variables
      integer i, j, k
!      
!     External functions

!     
!-----------------------------------------------------------------------
!     
!     Set control fail flag and other variables
      info = 0
!      
!     Select lu_11
      lu(1,1) = a(1,1)
      if (lu(1,1) .eq. 0) then
!       Impossible factorization
        info = 1
        return
      end if
!      
!     Compute row 1 and column 1
      do j = 2, n
        lu(1,j) = a(1,j)
        lu(j,1) = a(j,1)/lu(1,1)
      end do
!      
!     Loop to compute from second to n-1 rows and columns
      do i = 2, n-1
!        
!       Compute lu_ii
        lu(i,i) = a(i,i)
        do k = 1, i-1
          lu(i,i) = lu(i,i) - lu(i,k)*lu(k,i)
        end do
        if (lu(i,i) .eq. 0.d0) then
!         Impossible factorization
          info = 1
          return
        end if
!        
!       Compute ith row and column
        do j = i+1, n
          lu(i,j) = a(i,j)
          lu(j,i) = a(j,i)/lu(i,i)
          do k = 1, i-1
            lu(i,j) = lu(i,j) - lu(i,k)*lu(k,j)
            lu(j,i) = lu(j,i) - lu(j,k)*lu(k,i)/lu(i,i)
          end do
        end do
!        
      end do
!      
!     Compute lu_nn
      lu(n,n) = a(n,n)
      do k = 1, n-1
        lu(n,n) = lu(n,n) - lu(n,k)*lu(k,n)
      end do
      if (lu(n,n) .eq. 0.d0) then
!       Factorization is possible, but matrix a is singular
        info = 2
      end if
!      
      return
      end
