      subroutine invmt2
     &  (a          ,ainv       )
!***********************************************************************
! Invert a real 2x2 matrix
!***********************************************************************
      implicit none
! Arguments declaration
      double precision  a(2,2)     ,ainv(2,2)
! Local variables declaration
      double precision  adet
!
! Compute determinant
      adet = a(1,1)*a(2,2)-a(1,2)*a(2,1)
      if (adet.eq.0.d0) call errprt('EE0011')
!
! Compute inverse
      ainv(1,1) = a(2,2)/adet
      ainv(2,1) = -a(2,1)/adet
      ainv(1,2) = -a(1,2)/adet
      ainv(2,2) = a(1,1)/adet
! 
      return
      end