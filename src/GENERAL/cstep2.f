      SUBROUTINE CSTEP2
     1(   AMATX      ,BETRL      ,DMATX      ,STRES      ,DETF       ,
     2    NTYPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   MADIM=5    ,MSTRE=4    )
      EXTERNAL  DDLGD2 ,DLGD2
      LOGICAL   OUTOFP
      DIMENSION
     1    AMATX(MADIM,MADIM)        ,BETRL(MSTRE)              ,
     2    DMATX(MSTRE,MSTRE)        ,STRES(MSTRE)
      DIMENSION
     1    AUXMTX(MADIM,MADIM)       ,BMTX(MADIM,MADIM)         ,
     2    DLGAUX(MSTRE,MSTRE)       ,DLGMTX(MADIM,MADIM)       ,
     3    DMATX2(MADIM,MADIM)       ,IG(MADIM)
      DATA
     1    R1   ,R2    /
     2    1.0D0,2.0D0 /
      DATA
     1    IG(1),IG(2),IG(3),IG(4),IG(5)  /
     2    1    ,3    ,3    ,2    ,4      /
C***********************************************************************
C COMPUTE THE CONSISTENT SPATIAL TANGENT MODULUS 'a' FOR LARGE STRAIN
C HYPERELASTIC-BASED ELASTOPLASTIC MATERIAL MODELS
C
C REFERENCE: Section 14.5
C***********************************************************************
      IF(NTYPE.EQ.3)THEN
        OUTOFP=.TRUE.
        NADIM=5
      ELSEIF(NTYPE.EQ.1.OR.NTYPE.EQ.2)THEN
        OUTOFP=.FALSE.
        NADIM=4
      ELSE
        CALL ERRPRT('EI0020')
      ENDIF
C                           e trial   e trial
C Compute the derivative  dE       /dB
      CALL DISO2
     1(   DLGAUX     ,DDLGD2     ,DLGD2      ,OUTOFP     ,BETRL      )
      FACTOR=R1/DETF
      DO 20 I=1,MSTRE
        DO 10 J=1,MSTRE
          DLGAUX(I,J)=FACTOR*DLGAUX(I,J)
   10   CONTINUE
   20 CONTINUE
C                                      e trial   e trial
C Rearrange components of DMATX and  dE       /dB         into the
C ordering (11,21,12,22,33), compatible with the discrete gradient G.
      CALL RVZERO(DMATX2,MADIM*MADIM)
      CALL RVZERO(DLGMTX,MADIM*MADIM)
      DO 40 INEW=1,NADIM
        DO 30 JNEW=1,NADIM
          IOLD=IG(INEW)
          JOLD=IG(JNEW)
          DLGMTX(INEW,JNEW)=DLGAUX(IOLD,JOLD)
          DMATX2(INEW,JNEW)=DMATX(IOLD,JOLD)
   30   CONTINUE
   40 CONTINUE
C Compute remaining needed matrix [DELTA_ik BETRL_jl+DELTA_jl BETRL_il]
      CALL RVZERO(BMTX,MADIM*MADIM)
      BMTX(1,1)=R2*BETRL(1)
      BMTX(1,3)=R2*BETRL(3)
      BMTX(2,1)=BETRL(3)
      BMTX(2,2)=BETRL(1)
      BMTX(2,3)=BETRL(2)
      BMTX(2,4)=BETRL(3)
      BMTX(3,1)=BETRL(3)
      BMTX(3,2)=BETRL(1)
      BMTX(3,3)=BETRL(2)
      BMTX(3,4)=BETRL(3)
      BMTX(4,2)=R2*BETRL(3)
      BMTX(4,4)=R2*BETRL(2)
      IF(OUTOFP)BMTX(5,5)=R2*BETRL(4)
C Assemble the spatial tangent modulus a
C --------------------------------------
C compute the product  D:L:B
      CALL RVZERO(AUXMTX,MADIM*MADIM)
      DO 70 I=1,NADIM
        DO 60 J=1,NADIM
          DO 50 K=1,NADIM
            AUXMTX(I,J)=AUXMTX(I,J)+DMATX2(I,K)*DLGMTX(K,J)
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
      CALL RVZERO(AMATX,MADIM*MADIM)
      DO 100 I=1,NADIM
        DO 90 J=1,NADIM
          DO 80 K=1,NADIM
            AMATX(I,J)=AMATX(I,J)+AUXMTX(I,K)*BMTX(K,J)
   80     CONTINUE
   90   CONTINUE
  100 CONTINUE
C subtract  [SIGMA_il DELTA_jk]
      AMATX(1,1)=AMATX(1,1)-STRES(1)
      AMATX(1,3)=AMATX(1,3)-STRES(3)
      AMATX(2,1)=AMATX(2,1)-STRES(3)
      AMATX(2,3)=AMATX(2,3)-STRES(2)
      AMATX(3,2)=AMATX(3,2)-STRES(1)
      AMATX(3,4)=AMATX(3,4)-STRES(3)
      AMATX(4,2)=AMATX(4,2)-STRES(3)
      AMATX(4,4)=AMATX(4,4)-STRES(2)
      IF(OUTOFP)AMATX(5,5)=AMATX(5,5)-STRES(4)
      RETURN
      END
