      SUBROUTINE DEFGRA
     1(   ELDISP     ,F          ,GMATX      ,MDOFN      ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   MCOMP=5    )
      DIMENSION
     1    ELDISP(MDOFN,*)    ,F(3,3)             ,GMATX(MGDIM,*)
      DIMENSION
     1    FVEC(MCOMP)
      DATA  R0   ,R1    / 0.0D0,1.0D0 /
C***********************************************************************
C COMPUTES THE DEFORMATION GRADIENT TENSOR ASSOCIATED WITH THE ELEMENT
C DISPLACEMENT 'ELDISP'
C***********************************************************************
C Set total number of deformation gradient components
      IF(NTYPE.EQ.1.OR.NTYPE.EQ.2)THEN
        NCOMP=4
      ELSEIF(NTYPE.EQ.3)THEN
        NCOMP=5
      ELSE
        CALL ERRPRT('EI0021')
      ENDIF
C Evaluate the deformation gradient stored in vector form
      CALL RVZERO(FVEC,NCOMP)
      DO 30 ICOMP=1,NCOMP
        IEVAB=0
        DO 20 INODE=1,NNODE
          DO 10 IDOFN=1,NDOFN
            IEVAB=IEVAB+1
            FVEC(ICOMP)=FVEC(ICOMP)+
     1                  GMATX(ICOMP,IEVAB)*ELDISP(IDOFN,INODE)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C Store the deformation gradient in matrix form
      F(1,1)=FVEC(1)+R1
      F(2,1)=FVEC(2)
      F(3,1)=R0
      F(1,2)=FVEC(3)
      F(2,2)=FVEC(4)+R1
      F(3,2)=R0
      F(1,3)=R0
      F(2,3)=R0
      IF(NTYPE.EQ.1)THEN
        F(3,3)=R0
      ELSEIF(NTYPE.EQ.2)THEN
        F(3,3)=R1
      ELSEIF(NTYPE.EQ.3)THEN
        F(3,3)=FVEC(5)+R1
      ENDIF
C
      RETURN
      END
