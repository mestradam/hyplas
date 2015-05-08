      SUBROUTINE RVSCAL
     1(   V          ,N          ,SCAL       )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(N)
C***********************************************************************
C MULTIPLIES THE DOUBLE PRECISION VECTOR 'V', OF DIMENSION 'N',
C BY THE SCALAR 'SCAL'
C***********************************************************************
      DO 10 I=1,N
        V(I)=SCAL*V(I)
   10 CONTINUE
      RETURN
      END
