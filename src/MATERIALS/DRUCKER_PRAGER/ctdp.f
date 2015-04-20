      SUBROUTINE CTDP
     1(   DGAM       ,DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,
     2    NTYPE      ,RPROPS     ,RSTAVA     ,STRAT      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=7  ,MSTRE=4)
      LOGICAL APEX, EPFLAG, LALGVA(3)
      DIMENSION
     1    DMATX(MSTRE,MSTRE),IPROPS(*)           ,RPROPS(*)          ,
     2    RSTAVA(MSTRE+1)   ,STRAT(MSTRE)
      DIMENSION
     1    EETD(MSTRE)        ,FOID(MSTRE,MSTRE)  ,SOID(MSTRE)        ,
     2    UNIDEV(MSTRE)
      DATA
     1    FOID(1,1),FOID(1,2),FOID(1,3),FOID(1,4)/
     2    1.0D0    ,0.0D0    ,0.0D0    ,0.0D0    /
     3    FOID(2,1),FOID(2,2),FOID(2,3),FOID(2,4)/
     4    0.0D0    ,1.0D0    ,0.0D0    ,0.0D0    /
     5    FOID(3,1),FOID(3,2),FOID(3,3),FOID(3,4)/
     6    0.0D0    ,0.0D0    ,0.5D0    ,0.0D0    /
     7    FOID(4,1),FOID(4,2),FOID(4,3),FOID(4,4)/
     8    0.0D0    ,0.0D0    ,0.0D0    ,1.0D0    /
      DATA
     1    SOID(1)  ,SOID(2)  ,SOID(3)  ,SOID(4)  /
     2    1.0D0    ,1.0D0    ,0.0D0    ,1.0D0    /
      DATA
     1    R0   ,R1   ,RP5  ,R2   ,R3   /
     2    0.0D0,1.0D0,0.5D0,2.0D0,3.0D0/
C***********************************************************************
C COMPUTATION OF CONSISTENT TANGENT MODULUS FOR DRUCKER-PRAGER TYPE
C ELASTO-PLASTIC MATERIAL WITH ASSOCIATIVE/NON-ASSOCIATIVE FLOW RULE AND
C PIECE-WISE LINEAR ISOTROPIC HARDENING
C
C REFERENCE: Section 8.3.5
C***********************************************************************
      IF(NTYPE.EQ.2)THEN
        NSTRE=3
      ELSEIF(NTYPE.EQ.3)THEN
        NSTRE=4
      ELSE
        CALL ERRPRT('EI0017')
      ENDIF
C Retrieve accumulated plastic strain, DGAMA and APEX algorithm flag
      EPBAR=RSTAVA(MSTRE+1)
      DGAMA=DGAM
      APEX=LALGVA(3)
C Set some material properties
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      ETA=RPROPS(4)
      XI=RPROPS(5)
      ETABAR=RPROPS(6)
      NHARD=IPROPS(3)
C and some constants
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R1D3=R1/R3
      ROOT2=SQRT(R2)
C
      IF(EPFLAG)THEN
C Compute elastoplastic consistent tangent
C ========================================
C Hardening slope
        HSLOPE=DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
        IF(APEX)THEN
C Elastoplastic tangent consistent with apex return
C -------------------------------------------------
          ALPHA=XI/ETABAR
          BETA=XI/ETA
          AFACT=BULK*(R1-BULK/(BULK+ALPHA*BETA*HSLOPE))
          DO 20 I=1,NSTRE
            DO 10 J=1,NSTRE
              DMATX(I,J)=AFACT*SOID(I)*SOID(J)
   10       CONTINUE       
   20     CONTINUE
        ELSE
C Elastoplastic tangent consistent with smooth cone wall return
C -------------------------------------------------------------
C Elastic trial deviatoric (physical) strain
          EEVD3=(STRAT(1)+STRAT(2)+STRAT(4))*R1D3
          EETD(1)=STRAT(1)-EEVD3
          EETD(2)=STRAT(2)-EEVD3
          EETD(3)=STRAT(3)*RP5
          EETD(4)=STRAT(4)-EEVD3
          ETDNOR=SQRT(EETD(1)*EETD(1)+EETD(2)*EETD(2)+
     1             R2*EETD(3)*EETD(3)+EETD(4)*EETD(4))
C Unit deviatoric flow vector
          IF(ETDNOR.NE.R0)THEN
            EDNINV=R1/ETDNOR
          ELSE
            EDNINV=R0
          ENDIF
          DO 30 I=1,NSTRE
            UNIDEV(I)=EETD(I)*EDNINV
   30     CONTINUE
C Assemble tangent
          AUX=R1/(GMODU+BULK*ETA*ETABAR+XI*XI*HSLOPE)
          AFACT=R2G*(R1-DGAMA/(ROOT2*ETDNOR))
          AFACD3=AFACT*R1D3
          BFACT=R2G*(DGAMA/(ROOT2*ETDNOR)-GMODU*AUX)
          CFACT=-ROOT2*GMODU*BULK*AUX
          DFACT=BULK*(R1-BULK*ETA*ETABAR*AUX)
          DO 50 I=1,NSTRE
            DO 40 J=1,NSTRE
              DMATX(I,J)=AFACT*FOID(I,J)+BFACT*UNIDEV(I)*UNIDEV(J)+
     1                   CFACT*(ETA*UNIDEV(I)*SOID(J)+
     2                           ETABAR*SOID(I)*UNIDEV(J))+
     3                   (DFACT-AFACD3)*SOID(I)*SOID(J)
   40       CONTINUE       
   50     CONTINUE
        ENDIF
      ELSE
C Compute elasticity matrix
C =========================
        FACTOR=BULK-R2G*R1D3
        DO 70 I=1,NSTRE
          DO 60 J=I,NSTRE
            DMATX(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   60     CONTINUE       
   70   CONTINUE
        DO 90 J=1,NSTRE-1
          DO 80 I=J+1,NSTRE
            DMATX(I,J)=DMATX(J,I)
   80     CONTINUE
   90   CONTINUE
      ENDIF
      RETURN
      END
