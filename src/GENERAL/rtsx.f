      SUBROUTINE RTSX
     1(   AUXM       ,MODE       ,MROWQ      ,MROWR      ,NCOLR      ,
     2    NROWR      ,Q          ,R          ,S          ,X          ,
     3    SCAL       )  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    AUXM(NCOLR,NROWR)  ,Q(MROWQ,MROWQ)     ,R(MROWR,NCOLR)     ,
     2    S(MROWR,MROWR)     ,X(MROWR,NCOLR)
      DATA  R0   /0.0D0/
C***********************************************************************
C PERFORMS THE MATRIX PRODUCTS
C                                  T
C                     Q := SCAL * R  S X        (IF MODE=1)
C OR
C                                      T
C                     Q := Q + SCAL * R  S X    (OTHERWISE)
C
C WHERE 'R' AND 'X' ARE REAL RECTANGULAR MATRICES OF IDENTICAL
C DIMENSIONS, 'S' A REAL SQUARE MATRIX AND 'SCAL' A SCALAR.
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
C Construct the matrix Q
      DO 80 J=1,NCOLR
        DO 70 K=1,NROWR
          IF(X(K,J).NE.R0)THEN
            DO 60 I=1,NCOLR
              Q(I,J)=Q(I,J)+AUXM(I,K)*X(K,J)
   60       CONTINUE
          ENDIF
   70   CONTINUE
   80 CONTINUE
C
      RETURN
      END
