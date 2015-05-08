      SUBROUTINE SUDPPN
     1(   RALGVA     ,IPROPS     ,LALGVA     ,NTYPE      ,RPROPS     ,
     2    RSTAVA     ,STRAT      ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=7  ,MSTRE=4)
C Arguments
      LOGICAL LALGVA(3)
      DIMENSION
     1    RALGVA(3)          ,IPROPS(*)          ,RPROPS(*)          ,
     2    RSTAVA(MSTRE+1)    ,STRAT(MSTRE)       ,STRES(MSTRE)
C Local arrays and variables
      LOGICAL EPFLAG ,IFPLAS ,SUFAIL
      DIMENSION
     1    DMATX(MSTRE,MSTRE) ,RSTAUX(MSTRE+1)
      DATA
     1    R0    ,TOL   / 
     2    0.0D0 ,1.D-08/
      DATA MXITER / 20 /
C***********************************************************************
C STATE UPDATE PROCEDURE FOR THE DRUCKER-PRAGER ELASTO-PLASTIC MODEL
C WITH NON-LINEAR (PIECEWISE LINEAR) ISOTROPIC HARDENING IN PLANE
C STRESS. NESTED ITERATION APPROACH.
C
C REFERENCE: Section 9.2.2
C***********************************************************************
C Stop program if not plane stress
      IF(NTYPE.NE.1)CALL ERRPRT('EI0038')
C Initialise the state update failure flag
      SUFAIL=.FALSE.
C Set some material properties
      NHARD=IPROPS(3)
C
C Begin Newton-Raphson iteration loop for plane stress enforcement
C ----------------------------------------------------------------
C
C Set initial guess for elastic trial thickness strain. Use previously
C converged elastic thickness strain.
      E33TRL=RSTAVA(4)
C Start N-R loop
      DO 20 ITER=1,MXITER
C Set state variables to values at beginning of increment
        DO 10 I=1,MSTRE+1
          RSTAUX(I)=RSTAVA(I)
   10   CONTINUE
C Use axisymmetric integration algorithm to compute stresses, etc.
        STRAT(4)=E33TRL
        CALL SUDP
     1(   RALGVA     ,IPROPS     ,LALGVA     ,3          ,RPROPS     ,
     2    RSTAUX     ,STRAT      ,STRES      )
        SUFAIL=LALGVA(2)
        IF(SUFAIL)THEN
C... emergency exit in case of failure of the state apdate procedure
          GOTO 999
        ENDIF
        IFPLAS=LALGVA(1)
C Check plane stress convergence
        EPBAR=RSTAVA(MSTRE+1)
        COHE=PLFUN(EPBAR,NHARD,RPROPS(IPHARD))
        RES=ABS(STRES(4))
C...use normalised out-of-plane stress
        IF(COHE.NE.R0)RES=RES/ABS(COHE)
        IF(RES.LE.TOL)THEN
C...and break N-R loop in case of convergence
          GOTO 30
        ENDIF
C Compute axisymmetric consistent tangent components
        EPFLAG=IFPLAS
        CALL CTDP
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,
     2    3          ,RPROPS     ,RSTAUX     ,STRAT      )
C Apply Newton-Raphson correction to normal elastic trial strain
        D22=DMATX(4,4)
        E33TRL=E33TRL-STRES(4)/D22
   20 CONTINUE
C Emergency exit in case of failure of the plane stress enforcement loop
      SUFAIL=.TRUE.
      LALGVA(2)=SUFAIL
      CALL ERRPRT('WE0016')
      GOTO 999
   30 CONTINUE
C Set state variables to current updated values
      DO 40 I=1,MSTRE+1
        RSTAVA(I)=RSTAUX(I)
   40 CONTINUE
C Store the converged elastic trial thickness strain in the array of
C real algorithmic variables
      RALGVA(3)=E33TRL
C
  999 CONTINUE
      RETURN
      END
