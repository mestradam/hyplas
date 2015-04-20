      SUBROUTINE RVSUB
     1(   U          ,V          ,W          ,N          )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    U(N)               ,V(N)               ,W(N)
C***********************************************************************
C SUBTRACTS THE VECTOR 'W' FROM THE VECTOR 'V' AND STORE THE RESULT
C IN 'U'. U ,V AND W ARE DOUBLE PRECISION VECTORS OF DIMENSION N.
C***********************************************************************
      DO 10 I=1,N
        U(I)=V(I)-W(I)
   10 CONTINUE
      RETURN
      END
