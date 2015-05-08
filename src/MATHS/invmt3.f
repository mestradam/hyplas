      SUBROUTINE INVMT3
     1(   S          ,SINV       ,DETS       )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    S(3,3)             ,SINV(3,3)
      DATA
     1   R0   ,R1   /
     2   0.0D0,1.0D0/
C***********************************************************************
C INVERT A REAL 3x3 MATRIX
C***********************************************************************
      DETS=S(1,1)*S(2,2)*S(3,3)+S(1,2)*S(2,3)*S(3,1)+
     1     S(1,3)*S(2,1)*S(3,2)-S(1,2)*S(2,1)*S(3,3)-
     2     S(1,1)*S(2,3)*S(3,2)-S(1,3)*S(2,2)*S(3,1)
      IF(DETS.EQ.R0)CALL ERRPRT('EE0011')
C
      DETSIN=R1/DETS
      SINV(1,1)=+DETSIN*(S(2,2)*S(3,3)-S(2,3)*S(3,2))
      SINV(2,1)=-DETSIN*(S(2,1)*S(3,3)-S(2,3)*S(3,1))
      SINV(3,1)=+DETSIN*(S(2,1)*S(3,2)-S(2,2)*S(3,1))
      SINV(1,2)=-DETSIN*(S(1,2)*S(3,3)-S(1,3)*S(3,2))
      SINV(2,2)=+DETSIN*(S(1,1)*S(3,3)-S(1,3)*S(3,1))
      SINV(3,2)=-DETSIN*(S(1,1)*S(3,2)-S(1,2)*S(3,1))
      SINV(1,3)=+DETSIN*(S(1,2)*S(2,3)-S(1,3)*S(2,2))
      SINV(2,3)=-DETSIN*(S(1,1)*S(2,3)-S(1,3)*S(2,1))
      SINV(3,3)=+DETSIN*(S(1,1)*S(2,2)-S(1,2)*S(2,1))
C
      RETURN
      END
