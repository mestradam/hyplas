      SUBROUTINE LISTRA
     1(   BMATX      ,ELDISP     ,MDOFN      ,MBDIM      ,NDOFN      ,
     2    NNODE      ,NTYPE      ,STRAN      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    BMATX(MBDIM,*)     ,ELDISP(MDOFN,*)    ,STRAN(*)
C***********************************************************************
C COMPUTES THE SYMMETRIC GRADIENT (LINEAR STRAIN MEASURE) ASSOCIATED
C WITH THE ELEMENT DISPLACEMENT 'ELDISP' IN 2-D: PLANE STRAIN, PLANE
C STRESS AND AXISYMMETRIC PROBLEMS
C
C REFERENCE: Expression (4.53)
C***********************************************************************
      IF(NTYPE.EQ.1)THEN
        NSTRE=3
        NBDIM=3
      ELSEIF(NTYPE.EQ.2)THEN
        NSTRE=4
        NBDIM=3
      ELSEIF(NTYPE.EQ.3)THEN
        NSTRE=4
        NBDIM=4
      ELSE
        CALL ERRPRT('EI0023')
      ENDIF
C
      CALL RVZERO(STRAN,NSTRE)
      DO 30 ISTRE=1,NBDIM
        IEVAB=0
        DO 20 INODE=1,NNODE
          DO 10 IDOFN=1,NDOFN
            IEVAB=IEVAB+1
            STRAN(ISTRE)=STRAN(ISTRE)+
     1                   BMATX(ISTRE,IEVAB)*ELDISP(IDOFN,INODE)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      RETURN
      END
