      SUBROUTINE SUELPRU
     1(   NTYPE      ,RPROPS     ,RSTAVA     ,STRAN      ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MSTRE=4)
      DIMENSION
     1    RPROPS(*)          ,RSTAVA(MSTRE)      ,STRAN(*)           ,
     2    STRES(*)    
      DIMENSION
     1    EED(MSTRE)
      DATA
     1    RP5  ,R2   ,R3   ,R4   / 
     2    0.5D0,2.0D0,3.0D0,4.0D0/
C***********************************************************************
C STATE UPDATE PROCEDURE FOR LINEAR ELASTIC MATERIAL MODEL
C
C REFERENCE: Expression (4.43)
C***********************************************************************
C
C Set shear and bulk modulus
C
      GMODU=RPROPS(2)
      BULK=RPROPS(3)
C
C Decompose strain into deviatoric and volumetric components
C ----------------------------------------------------------
C
      R2G=R2*GMODU
      IF(NTYPE.EQ.1)THEN
C for plane stress
        R4G=R4*GMODU
        R4GD3=R4G/R3
        FACTOR=R2G/(BULK+R4GD3)
        EEV=(STRAN(1)+STRAN(2))*FACTOR
        EEVD3=EEV/R3
        EED(1)=STRAN(1)-EEVD3
        EED(2)=STRAN(2)-EEVD3
      ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
C for plane strain and axisymmetric cases
        EEV=STRAN(1)+STRAN(2)+STRAN(4)
        EEVD3=EEV/R3
        EED(1)=STRAN(1)-EEVD3
        EED(2)=STRAN(2)-EEVD3
        EED(4)=STRAN(4)-EEVD3
      ELSE
        CALL ERRPRT('EI0018')
      ENDIF
C Convert engineering shear component into physical component
      EED(3)=STRAN(3)*RP5
C
C Update stress using linear elastic law
C ---------------------------------------
C
C hydrostatic stress
      P=BULK*EEV
C stress tensor components
      STRES(1)=R2G*EED(1)+P
      STRES(2)=R2G*EED(2)+P
      STRES(3)=R2G*EED(3)
      IF(NTYPE.EQ.2.OR.NTYPE.EQ.3)STRES(4)=R2G*EED(4)+P
C
C Store elastic engineering strain in RSTAVA
C ------------------------------------------
C
      RSTAVA(1)=STRAN(1)
      RSTAVA(2)=STRAN(2)
      RSTAVA(3)=STRAN(3)
      IF(NTYPE.EQ.1)THEN
        R3BULK=R3*BULK
        RSTAVA(4)=-(STRAN(1)+STRAN(2))*(R3BULK-R2G)/(R3BULK+R4G)
      ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
        RSTAVA(4)=STRAN(4)
      ENDIF
C
      RETURN
      END
