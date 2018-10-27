      double precision function tranmt
     &  (mat        ,nrow       ,ncol       )
!***********************************************************************
! Computes the transpose of a matrix: 
!     tranmt = mat^t
!
! (M. Estrada, 2015)
!***********************************************************************
      implicit none
      double precision
     &   mat
      dimension
     &   mat(nrow,ncol)   ,tranmt(ncol,nrow)
      integer
     &   nrow       ,ncol       ,irow       ,icol
!
      do irow = 1, nrow
        do icol = 1, ncol
          tranmt(icol,irow) = mat(irow,icol)
        end do
      end do
!
      return
      end