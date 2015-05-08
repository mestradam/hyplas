      SUBROUTINE RTV
     1(   MODE       ,MROWR      ,NCOLR      ,NROWR      ,P          ,
     2    R          ,V          ,SCAL       )  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    P(NCOLR)           ,R(MROWR,NCOLR)     ,V(NROWR)
      DATA  R0   /0.0D0/
C***********************************************************************
C PERFORMS THE PRODUCT
C                                  T
C                     P := SCAL * R  V          (IF MODE=1)
C OR
C                                      T
C                     P := P + SCAL * R  V      (OTHERWISE)
C
C WHERE 'R' IS A REAL RECTANGULAR MATRIX, 'V' A REAL VECTOR AND
C 'SCAL' A SCALAR.
C***********************************************************************
      IF(MODE.EQ.1)CALL RVZERO(P,NCOLR)
      DO 30 I=1,NCOLR
        DO 20 J=1,NROWR
          IF(R(J,I).NE.R0)THEN
            P(I)=P(I)+SCAL*R(J,I)*V(J)
          ENDIF
   20   CONTINUE
   30 CONTINUE
C
      RETURN
      END
