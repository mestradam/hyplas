      SUBROUTINE RTSR
     1(   AUXM       ,MODE       ,MROWQ      ,MROWR      ,NCOLR      ,
     2    NROWR      ,Q          ,R          ,S          ,SCAL       ,
     3    UNSYM      )  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UNSYM
      DIMENSION
     1    AUXM(NCOLR,NROWR)  ,Q(MROWQ,MROWQ)     ,R(MROWR,NCOLR)     ,
     2    S(MROWR,MROWR)
      DATA  R0   /0.0D0/
C***********************************************************************
C PERFORMS THE MATRIX PRODUCTS
C                                 T
C                    Q := SCAL * R  S R        (IF MODE=1)
C OR
C                                     T
C                    Q := Q + SCAL * R  S R    (OTHERWISE)
C
C WHERE 'R' IS A REAL RECTANGULAR MATRIX, 'S' A REAL SQUARE MATRIX
C AND 'SCAL' A SCALAR.
C***********************************************************************
      CALL RVZERO(AUXM,NCOLR*NROWR)
      DO 30 I=1,NCOLR
        DO 20 K=1,NROWR
          IF(R(K,I).NE.R0)THEN
            DO 10 J=1,NROWR
              AUXM(I,J)=AUXM(I,J)+SCAL*R(K,I)*S(K,J)
   10       CONTINUE
          ENDIF
   20   CONTINUE
   30 CONTINUE
C
      IF(MODE.EQ.1)THEN
        DO 50 I=1,NCOLR
          DO 40 J=1,NCOLR
            Q(I,J)=R0
   40     CONTINUE
   50   CONTINUE
      ENDIF
C
      IF(UNSYM)THEN
C Construct the whole matrix Q at once
        DO 80 J=1,NCOLR
          DO 70 K=1,NROWR
            IF(R(K,J).NE.R0)THEN
              DO 60 I=1,NCOLR
                Q(I,J)=Q(I,J)+AUXM(I,K)*R(K,J)
   60         CONTINUE
            ENDIF
   70     CONTINUE
   80   CONTINUE
      ELSE
C Construct the lower triangle of Q first
        DO 110 J=1,NCOLR
          DO 100 K=1,NROWR
            IF(R(K,J).NE.R0)THEN
              DO 90 I=J,NCOLR
                Q(I,J)=Q(I,J)+AUXM(I,K)*R(K,J)
   90         CONTINUE
            ENDIF
  100     CONTINUE
  110   CONTINUE
C and then assemble the upper triangle
        DO 130 I=1,NCOLR
          DO 120 J=I+1,NCOLR
            Q(I,J)=Q(J,I)
  120     CONTINUE
  130   CONTINUE
      ENDIF
C
      RETURN
      END
