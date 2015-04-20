      SUBROUTINE BETRIA
     1(   BETRL      ,EEN        ,FINCR      ,NTYPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    BETRL(*)           ,EEN(*)             ,FINCR(3,3)
      DIMENSION
     1    AUXM(2,2)          ,BEN(4)              ,BENMTX(2,2)        ,
     2    BETRLM(2,2)
C***********************************************************************
C COMPUTES THE "ELASTIC TRIAL" LEFT CAUCHY-GREEN STRAIN TENSOR FOR
C HYPERELASTIC-BASED LARGE STRAIN ELASTO-PLASTIC MODELS ACCORDING TO
C THE FORMULA:
C
C                     e trial             e   T
C                    B        :=   F     B   F
C                     n+1           incr  n   incr
C         e
C WHERE  B   IS OBTAINED AS:
C         n
C                     e              e
C                    B   :=  exp[ 2 E  ]
C                     n              n
C        e
C WITH  E   DENOTING THE ELASTIC LOGARITHMIC STRAIN AT  t .
C        n                                               n
C
C REFERENCE: Box 14.3, item (ii)
C***********************************************************************
      BEN(1)=EEN(1)
      BEN(2)=EEN(2)
      BEN(3)=EEN(3)
      BEN(4)=EEN(4)
C Convert engineering elastic logarithmic strain components into the
C corresponding elastic left Cauchy-Green strain tensor components
      CALL SETBE(BEN,NTYPE)
C Convert left Cauchy-Green strain tensor from vector array to matrix
C form
      BENMTX(1,1)=BEN(1)
      BENMTX(2,1)=BEN(3)
      BENMTX(1,2)=BEN(3)
      BENMTX(2,2)=BEN(2)
C
C In-plane components of the elastic trial left Cauchy-Green tensor
C
      CALL RVZERO(AUXM,4)
      DO 30 I=1,2
        DO 20 J=1,2
          DO 10 K=1,2
            AUXM(I,J)=AUXM(I,J)+FINCR(I,K)*BENMTX(K,J)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      CALL RVZERO(BETRLM,4)
      DO 60 I=1,2
        DO 50 J=1,2
          DO 40 K=1,2
            BETRLM(I,J)=BETRLM(I,J)+AUXM(I,K)*FINCR(J,K)
   40     CONTINUE
   50   CONTINUE
   60 CONTINUE
C
C        e trial
C Store B        in array form
C        n+1 
C
      BETRL(1)=BETRLM(1,1)
      BETRL(2)=BETRLM(2,2)
      BETRL(3)=BETRLM(1,2)
C out-of-plane component
      IF(NTYPE.EQ.2)THEN
        BETRL(4)=BEN(4)
      ELSEIF(NTYPE.EQ.3)THEN
        BETRL(4)=BEN(4)*FINCR(3,3)**2
      ENDIF
C
      RETURN
      END
