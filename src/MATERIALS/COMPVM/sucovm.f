      SUBROUTINE SUcoVM
     1(   DGAMA      ,IPROPS     ,LALGVA     ,NTYPE      ,RPROPS     ,
     2    RSTAVA     ,STRAT      ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=4  ,MSTRE=4)
      LOGICAL IFPLAS, LALGVA(2), SUFAIL
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)          ,RSTAVA(MSTRE+1)    ,
     2    STRAT(MSTRE)       ,STRES(MSTRE)
      DIMENSION
     1    EET(MSTRE)
      DATA
     1    R0   ,RP5  ,R1   ,R2   ,R3   ,TOL   / 
     2    0.0D0,0.5D0,1.0D0,2.0D0,3.0D0,1.D-06/
      DATA MXITER / 50 /
C***********************************************************************
C STATE UPDATE PROCEDURE FOR THE VON MISES ELASTO-PLASTIC MATERIAL MODEL
C WITH NON-LINEAR (PIECEWISE LINEAR) ISOTROPIC HARDENING:
C IMPLICIT ELASTIC PREDICTOR/RETURN MAPPING ALGORITHM (BOXES 7.3-4).
C PLANE STRAIN AND AXISYMMETRIC IMPLEMENTATIONS.
C
C REFERENCE: Section 7.3.5
C***********************************************************************
C Stop program if neither plane strain nor axisymmetric state
      IF(NTYPE.NE.2.AND.NTYPE.NE.3)CALL ERRPRT('EI0013')
C Initialise some algorithmic and internal variables
      DGAMA=R0
      IFPLAS=.FALSE.
      SUFAIL=.FALSE.
      EPBARN=RSTAVA(MSTRE+1)
C Set some material properties
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      NHARD=IPROPS(3)
C Shear and bulk moduli and other necessary constants
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R3G=R3*GMODU
C Elastic predictor: Compute elastic trial state
C ----------------------------------------------
C Volumetric strain and pressure stress
      EEV=STRAT(1)+STRAT(2)+STRAT(4)
      P=BULK*EEV
C Elastic trial deviatoric strain
      EEVD3=EEV/R3
      EET(1)=STRAT(1)-EEVD3
      EET(2)=STRAT(2)-EEVD3
      EET(4)=STRAT(4)-EEVD3
C Convert engineering shear component into physical component
      EET(3)=STRAT(3)/R2
C Compute trial effective stress and uniaxial yield stress
      VARJ2T=R2G*R2G*(EET(3)*EET(3)+RP5*(EET(1)*EET(1)+
     1                     EET(2)*EET(2)+EET(4)*EET(4)))
      QTRIAL=SQRT(R3*VARJ2T)
      SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))
C Check for plastic admissibility
C -------------------------------
      PHI=QTRIAL-SIGMAY
      IF(PHI/SIGMAY.GT.TOL)THEN
C Plastic step: Apply return mapping - use Newton-Raphson algorithm
C               to solve the return mapping equation (Box 7.4)
C -------------------------------------------------------------------
        IFPLAS=.TRUE.
        EPBAR=EPBARN
        DO 10 NRITER=1,MXITER
C Compute residual derivative
          DENOM=-R3G-DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
C Compute Newton-Raphson increment and update variable DGAMA
          DDGAMA=-PHI/DENOM
          DGAMA=DGAMA+DDGAMA
C Compute new residual
          EPBAR=EPBAR+DDGAMA
          SIGMAY=PLFUN(EPBAR,NHARD,RPROPS(IPHARD))
          PHI=QTRIAL-R3G*DGAMA-SIGMAY
C Check convergence
          RESNOR=ABS(PHI/SIGMAY)
          IF(RESNOR.LE.TOL)THEN
C update accumulated plastic strain
            RSTAVA(MSTRE+1)=EPBAR
C update stress components
            FACTOR=R2G*(R1-R3G*DGAMA/QTRIAL)
            STRES(1)=FACTOR*EET(1)+P
            STRES(2)=FACTOR*EET(2)+P
            STRES(3)=FACTOR*EET(3)
            STRES(4)=FACTOR*EET(4)+P
C compute converged elastic (engineering) strain components
            FACTOR=FACTOR/R2G
            RSTAVA(1)=FACTOR*EET(1)+EEVD3
            RSTAVA(2)=FACTOR*EET(2)+EEVD3
            RSTAVA(3)=FACTOR*EET(3)*R2
            RSTAVA(4)=FACTOR*EET(4)+EEVD3
            GOTO 999
          ENDIF
   10   CONTINUE
C reset failure flag and print warning message if the algorithm fails
        SUFAIL=.TRUE.
        CALL ERRPRT('WE0004')
      ELSE
C Elastic step: Update stress using linear elastic law
C ----------------------------------------------------
        STRES(1)=R2G*EET(1)+P
        STRES(2)=R2G*EET(2)+P
        STRES(3)=R2G*EET(3)
        STRES(4)=R2G*EET(4)+P
C elastic engineering strain
        RSTAVA(1)=STRAT(1)
        RSTAVA(2)=STRAT(2)
        RSTAVA(3)=STRAT(3)
        RSTAVA(4)=STRAT(4)
      ENDIF
  999 CONTINUE
C Update some algorithmic variables before exit
      LALGVA(1)=IFPLAS
      LALGVA(2)=SUFAIL
      RETURN
      END
