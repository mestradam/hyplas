      SUBROUTINE CTMC
     1(   DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRAT      ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=7  ,MDIM=3,  MSTRE=4)
C Arguments
      LOGICAL EPFLAG  ,LALGVA(5)
      DIMENSION
     1    DMATX(MSTRE,MSTRE) ,IPROPS(*)          ,RPROPS(*)          ,
     2    RSTAVA(MSTRE+1)    ,STRAT(MSTRE)       ,STRES(MSTRE)
C Local arrays and variables
      LOGICAL APEX ,EDGE ,OUTOFP ,RIGHT ,REPEAT
      DIMENSION
     1    DPSTRS(MDIM,MDIM)  ,EIGPRJ(MSTRE,2)    ,FOID(MSTRE,MSTRE)  ,
     2    PSTRS(MDIM)        ,PSTRA(MDIM)        ,SOID(MSTRE)        ,
     3    STRAC(MSTRE)
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
     1    RP5  ,R1   ,R2   ,R3   ,R4   / 
     2    0.5D0,1.0D0,2.0D0,3.0D0,4.0D0/
C***********************************************************************
C COMPUTATION OF CONSISTENT TANGENT MODULUS FOR MOHR-COULOMB TYPE
C ELASTO-PLASTIC MATERIAL WITH ASSOCIATIVE/NON-ASSOCIATIVE FLOW RULE AND
C PIECE-WISE LINEAR ISOTROPIC HARDENING.
C PLANE STRAIN AND AXISYMMETRIC IMPLEMENTATIONS.
C
C REFERENCE: Section 8.2.5
C***********************************************************************
C Stops program if neither plane strain nor axisymmetric state
      IF(NTYPE.NE.2.AND.NTYPE.NE.3)CALL ERRPRT('EI0026')
C Current accumulated plastic strain
      EPBAR=RSTAVA(MSTRE+1)
C Set material properties
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      SINPHI=RPROPS(4)
      COSPHI=RPROPS(5)
      SINPSI=RPROPS(6)
      NHARD=IPROPS(3)
C Set needed algorithmic variables
      EDGE=LALGVA(3)
      RIGHT=LALGVA(4)
      APEX=LALGVA(5)
C Set some constants
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R4G=R4*GMODU
      R2BULK=R2*BULK
      R2CPHI=R2*COSPHI
      R4C2PH=R2CPHI*R2CPHI
      R1D3=R1/R3
      R2D3=R2*R1D3
      R2GD3=R2G*R1D3
      R4GD3=R4G*R1D3
      IF(EPFLAG)THEN
C Compute elastoplastic consistent tangent
C ----------------------------------------
C Spectral decomposition of the elastic trial strain
        STRAC(1)=STRAT(1)
        STRAC(2)=STRAT(2)
        STRAC(3)=STRAT(3)*RP5
        CALL SPDEC2(EIGPRJ,PSTRA,REPEAT,STRAC)
        PSTRA(3)=STRAT(4)
C and current total stress
        PSTRS(1)=STRES(1)*EIGPRJ(1,1)+STRES(2)*EIGPRJ(2,1)+
     1                             R2*STRES(3)*EIGPRJ(3,1)
        PSTRS(2)=STRES(1)*EIGPRJ(1,2)+STRES(2)*EIGPRJ(2,2)+
     1                             R2*STRES(3)*EIGPRJ(3,2)
        PSTRS(3)=STRES(4)
C Identify directions of maximum and minimum principal trial stresses
        II=1
        JJ=1
        PSTMAX=PSTRA(II)
        PSTMIN=PSTRA(JJ)
        DO 10 I=2,3
          IF(PSTRA(I).GE.PSTMAX)THEN
            II=I
            PSTMAX=PSTRA(II)
          ENDIF
          IF(PSTRA(I).LT.PSTMIN)THEN
            JJ=I
            PSTMIN=PSTRA(JJ)
          ENDIF
   10   CONTINUE
        IF(II.NE.1.AND.JJ.NE.1)MM=1
        IF(II.NE.2.AND.JJ.NE.2)MM=2
        IF(II.NE.3.AND.JJ.NE.3)MM=3
        IF(EDGE)THEN
C Tangent consistent with 2-vector return to edge
          SPHSPS=SINPHI*SINPSI
          CONSTA=R4G*(R1+R1D3*SPHSPS)+R4*BULK*SPHSPS
          IF(RIGHT)THEN
            CONSTB=R2G*(R1+SINPHI+SINPSI-R1D3*SPHSPS)+R4*BULK*SPHSPS
          ELSE
            CONSTB=R2G*(R1-SINPHI-SINPSI-R1D3*SPHSPS)+R4*BULK*SPHSPS
          ENDIF
          FACTA=R4C2PH*DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
          DRVAA=-CONSTA-FACTA
          DRVAB=-CONSTB-FACTA
          DRVBA=-CONSTB-FACTA
          DRVBB=-CONSTA-FACTA
          AUX1=R2G*(R1+R1D3*SINPSI)+R2BULK*SINPSI
          AUX2=(R4GD3-R2BULK)*SINPSI
          AUX3=R2G*(R1-R1D3*SINPSI)-R2BULK*SINPSI
          R1DDET=R1/(DRVAA*DRVBB-DRVAB*DRVBA)
          IF(RIGHT)THEN
C ...returned to right edge
            DPSTRS(II,II)=BULK+R4GD3+AUX1*(-DRVAB+DRVBB+DRVAA-DRVBA)*
     1                    (R2G+(R2BULK+R2GD3)*SINPHI)*R1DDET
            DPSTRS(II,MM)=BULK-R2GD3+AUX1*(R2G*(DRVAB-DRVAA)+
     1                    ((-DRVAB+DRVBB+DRVAA-DRVBA)*(R2BULK+R2GD3)+
     2                    (DRVBA-DRVBB)*R2G)*SINPHI)*R1DDET
            DPSTRS(II,JJ)=BULK-R2GD3+AUX1*(R2G*(DRVBA-DRVBB)+
     1                    ((-DRVAB+DRVBB+DRVAA-DRVBA)*(R2BULK+R2GD3)+
     2                    (DRVAB-DRVAA)*R2G)*SINPHI)*R1DDET
            DPSTRS(MM,II)=BULK-R2GD3+(AUX2*(DRVAB-DRVBB)+AUX3*(DRVBA-
     1                    DRVAA))*(R2G+(R2BULK+R2GD3)*SINPHI)*R1DDET
            DPSTRS(MM,MM)=BULK+R4GD3+(AUX2*((R2BULK*(DRVAB-DRVBB)+
     1                    (DRVAB*R2GD3+DRVBB*R4GD3))*SINPHI-DRVAB*R2G)+
     2                    AUX3*(DRVAA*R2G+(R2BULK*(DRVBA-DRVAA)-
     3                    (DRVAA*R2GD3+DRVBA*R4GD3))*SINPHI))*R1DDET
            DPSTRS(MM,JJ)=BULK-R2GD3+(AUX2*((R2BULK*(DRVAB-DRVBB)-
     1                    (DRVBB*R2GD3+DRVAB*R4GD3))*SINPHI+DRVBB*R2G)+
     2                    AUX3*((R2BULK*(DRVBA-DRVAA)+(DRVAA*R4GD3+
     3                    DRVBA*R2GD3))*SINPHI-DRVBA*R2G))*R1DDET
            DPSTRS(JJ,II)=BULK-R2GD3+((AUX2*(DRVBA-DRVAA)+AUX3*(DRVAB-
     1                    DRVBB))*((R2BULK+R2GD3)*SINPHI+R2G))*R1DDET
            DPSTRS(JJ,MM)=BULK-R2GD3+(AUX2*(((R2BULK*(DRVBA-DRVAA)-
     1                    (DRVBA*R4GD3+DRVAA*R2GD3))*SINPHI)+DRVAA*R2G)+
     2                    AUX3*(((R2BULK*(DRVAB-DRVBB)+(DRVAB*R2GD3+
     3                    DRVBB*R4GD3))*SINPHI)-DRVAB*R2G))*R1DDET
            DPSTRS(JJ,JJ)=BULK+R4GD3+(AUX2*(((R2BULK*(DRVBA-DRVAA)+
     1                    (DRVAA*R4GD3+DRVBA*R2GD3))*SINPHI)-DRVBA*R2G)+
     2                    AUX3*(((R2BULK*(DRVAB-DRVBB)-(DRVAB*R4GD3+
     3                    DRVBB*R2GD3))*SINPHI)+DRVBB*R2G))*R1DDET
          ELSE
C ...returned to left edge
            DPSTRS(II,II)=BULK+R4GD3+(AUX1*(((R2BULK*(DRVBB-DRVAB)+
     1                    (DRVAB*R4GD3+DRVBB*R2GD3))*SINPHI)+DRVBB*R2G)+
     2                    AUX2*(((R2BULK*(DRVBA-DRVAA)+(DRVAA*R4GD3+
     3                    DRVBA*R2GD3))*SINPHI)+DRVBA*R2G))*R1DDET
            DPSTRS(II,MM)=BULK-R2GD3+(AUX1*(((R2BULK*(DRVBB-DRVAB)-
     1                    (DRVAB*R2GD3+DRVBB*R4GD3))*SINPHI)-DRVAB*R2G)+
     2                    AUX2*(((R2BULK*(DRVBA-DRVAA)-(DRVAA*R2GD3+
     3                    DRVBA*R4GD3))*SINPHI)-DRVAA*R2G))*R1DDET
            DPSTRS(II,JJ)=BULK-R2GD3+((AUX1*(DRVBB-DRVAB)+AUX2*(DRVBA-
     1                    DRVAA))*(((R2BULK+R2GD3)*SINPHI)-R2G))*R1DDET
            DPSTRS(MM,II)=BULK-R2GD3+(AUX1*(((R2BULK*(DRVAA-DRVBA)-
     1                    (DRVAA*R4GD3+DRVBA*R2GD3))*SINPHI)-DRVBA*R2G)+
     2                    AUX2*(((R2BULK*(DRVAB-DRVBB)-(DRVAB*R4GD3+
     3                    DRVBB*R2GD3))*SINPHI)-DRVBB*R2G))*R1DDET
            DPSTRS(MM,MM)=BULK+R4GD3+(AUX1*(((R2BULK*(DRVAA-DRVBA)+
     1                    (DRVAA*R2GD3+DRVBA*R4GD3))*SINPHI)+DRVAA*R2G)+
     2                    AUX2*(((R2BULK*(DRVAB-DRVBB)+(DRVAB*R2GD3+
     3                    DRVBB*R4GD3))*SINPHI)+DRVAB*R2G))*R1DDET
            DPSTRS(MM,JJ)=BULK-R2GD3+((AUX1*(DRVAA-DRVBA)+AUX2*(DRVAB-
     1                    DRVBB))*(((R2BULK+R2GD3)*SINPHI)-R2G))*R1DDET
            DPSTRS(JJ,II)=BULK-R2GD3+(AUX3*(((R2BULK*(DRVAB-DRVBB-DRVAA+
     1                    DRVBA)+(DRVAA-DRVAB)*R4GD3+(DRVBA-DRVBB)*
     2                    R2GD3)*SINPHI)+(DRVBA-DRVBB)*R2G))*R1DDET
            DPSTRS(JJ,MM)=BULK-R2GD3+(AUX3*(((R2BULK*(DRVAB-DRVBB-DRVAA+
     1                    DRVBA)+(DRVAB-DRVAA)*R2GD3+(DRVBB-DRVBA)*
     2                    R4GD3)*SINPHI)+(DRVAB-DRVAA)*R2G))*R1DDET
            DPSTRS(JJ,JJ)=BULK+R4GD3+(AUX3*(DRVAB-DRVBB-DRVAA+DRVBA)*
     1                    (((R2BULK+R2GD3)*SINPHI)-R2G))*R1DDET
          ENDIF
        ELSEIF(APEX)THEN
C Tangent consistent with multi-vector return to apex
          COTPHI=COSPHI/SINPHI
          DSIDEJ=BULK*(R1-(BULK/(BULK+
     1           DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))*COTPHI*COSPHI/
     2           SINPSI)))
          DPSTRS(II,II)=DSIDEJ
          DPSTRS(II,MM)=DSIDEJ
          DPSTRS(II,JJ)=DSIDEJ
          DPSTRS(MM,II)=DSIDEJ
          DPSTRS(MM,MM)=DSIDEJ
          DPSTRS(MM,JJ)=DSIDEJ
          DPSTRS(JJ,II)=DSIDEJ
          DPSTRS(JJ,MM)=DSIDEJ
          DPSTRS(JJ,JJ)=DSIDEJ
        ELSE
C Tangent consistent with 1-vector return to main active plane
          SPHSPS=SINPHI*SINPSI
          CONSTA=R4G*(R1+R1D3*SPHSPS)+R4*BULK*SPHSPS
          DENOM=-CONSTA-R4C2PH*DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
          B1=(R2G*(R1+R1D3*SINPSI)+R2BULK*SINPSI)/DENOM
          B2=(R4G*R1D3-R2BULK)*SINPSI/DENOM
          B3=(R2G*(R1-R1D3*SINPSI)-R2BULK*SINPSI)/DENOM
          DPSTRS(II,II)=R2G*(R2D3+B1*(R1+R1D3*SINPHI))+
     1                  BULK*(R1+R2*B1*SINPHI)
          DPSTRS(II,MM)=R1D3*(R3*BULK-R2G)*(R1+R2*B1*SINPHI)
          DPSTRS(II,JJ)=R2G*(-R1D3-B1*(R1-R1D3*SINPHI))+
     1                  BULK*(R1+R2*B1*SINPHI)
          DPSTRS(MM,II)=R2G*(-R1D3-B2*(R1+R1D3*SINPHI))+
     1                  BULK*(R1-R2*B2*SINPHI)
          DPSTRS(MM,MM)=R4G*R1D3*(R1+B2*SINPHI)+BULK*(R1-R2*B2*SINPHI)
          DPSTRS(MM,JJ)=R2G*(-R1D3+B2*(R1-R1D3*SINPHI))+
     1                  BULK*(R1-R2*B2*SINPHI)
          DPSTRS(JJ,II)=R2G*(-R1D3-B3*(R1+R1D3*SINPHI))+
     1                  BULK*(R1-R2*B3*SINPHI)
          DPSTRS(JJ,MM)=R1D3*(R3*BULK-R2G)*(R1-R2*B3*SINPHI)
          DPSTRS(JJ,JJ)=R2G*(R2D3+B3*(R1-R1D3*SINPHI))+
     1                  BULK*(R1-R2*B3*SINPHI)
        ENDIF
C
        IF(NTYPE.EQ.2)THEN
          OUTOFP=.FALSE.
        ELSEIF(NTYPE.EQ.3)THEN
          OUTOFP=.TRUE.
        ENDIF
        CALL DGISO2
     1(   DPSTRS     ,DMATX      ,EIGPRJ     ,PSTRA      ,PSTRS      ,
     2    OUTOFP     ,REPEAT     )
      ELSE
C Compute elasticity matrix
C -------------------------
        IF(NTYPE.EQ.2)THEN
          NSTRE=3
        ELSEIF(NTYPE.EQ.3)THEN
          NSTRE=4
        ENDIF
        FACTOR=BULK-R2G*R1D3
        DO 50 I=1,NSTRE
          DO 40 J=I,NSTRE
            DMATX(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   40     CONTINUE
   50   CONTINUE
        DO 70 J=1,NSTRE-1
          DO 60 I=J+1,NSTRE
            DMATX(I,J)=DMATX(J,I)
   60     CONTINUE
   70   CONTINUE
      ENDIF
      RETURN
      END
