      SUBROUTINE LEFTCG
     1(   BN         ,BNP1       ,FINCR      ,NTYPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    BN(*)              ,BNP1(*)            ,FINCR(3,3)
      DIMENSION
     1    AUXM(2,2)          ,BNMTX(2,2)         ,BNP1M(2,2)
C***********************************************************************
C COMPUTES THE LEFT CAUCHY-GREEN STRAIN TENSOR ACCORDING TO THE
C FORMULA:
C
C                                             T
C                    B        :=   F     B   F
C                     n+1           incr  n   incr
C
C REFERENCE: Box 13.1. The formula used here is equivalent to that of
C                      item (i) of Box 13.1.
C***********************************************************************
C Convert previously converged left Cauchy-Green strain tensor from
C vector form to matrix form
      BNMTX(1,1)=BN(1)
      BNMTX(2,1)=BN(3)
      BNMTX(1,2)=BN(3)
      BNMTX(2,2)=BN(2)
C
C In-plane components of the left Cauchy-Green tensor
C
      CALL RVZERO(AUXM,4)
      DO 30 I=1,2
        DO 20 J=1,2
          DO 10 K=1,2
            AUXM(I,J)=AUXM(I,J)+FINCR(I,K)*BNMTX(K,J)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      CALL RVZERO(BNP1M,4)
      DO 60 I=1,2
        DO 50 J=1,2
          DO 40 K=1,2
            BNP1M(I,J)=BNP1M(I,J)+AUXM(I,K)*FINCR(J,K)
   40     CONTINUE
   50   CONTINUE
   60 CONTINUE
C
C Store B    in vector form
C        n+1 
C
      BNP1(1)=BNP1M(1,1)
      BNP1(2)=BNP1M(2,2)
      BNP1(3)=BNP1M(1,2)
C out-of-plane component
      IF(NTYPE.EQ.2)THEN
        BNP1(4)=BN(4)
      ELSEIF(NTYPE.EQ.3)THEN
        BNP1(4)=BN(4)*FINCR(3,3)*FINCR(3,3)
      ENDIF
C
      RETURN
      END
