      SUBROUTINE SUCOPL
     1(   DGAMA      ,IPROPS     ,LALGVA     ,NTYPE      ,RPROPS     ,
     2    RSTAVA     ,STRAT      ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( IPHARD=4  ,MSTRE=4  ,NSTRE=3 )
      LOGICAL IFPLAS, LALGVA(2), SUFAIL
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)          ,RSTAVA(MSTRE+1)    ,
     2    STRAT(MSTRE)       ,STRES(MSTRE)
      DIMENSION
     1    EET(MSTRE)         ,STREST(NSTRE)      ,STPRI(NSTRE) , 
     2  AMAT(NSTRE,NSTRE)  ,SIGGO(NSTRE)     ,PRINGO(NSTRE) ,
     3  CE(NSTRE,NSTRE)    ,VN(NSTRE,1)          ,CN(NSTRE,1) 
      DIMENSION
     1    FOID(MSTRE,MSTRE)  ,SOID(MSTRE)
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
     1    R0   ,RP5  ,R1   ,R2   ,R3   ,R4   ,R6   ,TOL   / 
     2    0.0D0,0.5D0,1.0D0,2.0D0,3.0D0,4.0D0,6.0D0,1.D-03/
      DATA MXITER / 150 /


C1072  FORMAT(/////15X,'ENTRANDO A SUCOPL')
C      WRITE(*,1072)
C LER: CON RETIRN MAPING USANDO MÉTODO SECANTE O DE MÜLLER PARA CALCULAR DGAMA
C***********************************************************************
C STATE UPDATE PROCEDURE FOR THE VON MISES ELASTO-PLASTIC MODEL WITH
C NON-LINEAR (PIECEWISE LINEAR) ISOTROPIC HARDENING IN PLANE STRESS:
C IMPLICIT PLANE STRESS-PROJECTED ELASTIC PREDICTOR/RETURN MAPPING
C ALGORITHM (BOXES 9.4-5).
C
C REFERENCE: Section 9.4.3
C            Boxes 9.4-5
C***********************************************************************
C Stop program if not plane stress
      IF(NTYPE.NE.1)CALL ERRPRT('EI0031')
C Initialise some algorithmic and internal variables
      DGAMA=0.0

      DGAMA0=RPROPS(8)
      DGAMA1=RPROPS(9)
C DGAMA2 SOLO PARA METODO DE MÜLLER
      DGAMA2=1.0E-10
      IFPLAS=.FALSE.
      SUFAIL=.FALSE.
C...set previously (equilibrium) converged accumulated plastic strain
      EPBARN=RSTAVA(MSTRE+1)
C Set some material properties
      EC=RPROPS(2)
      POISS=RPROPS(3)
      FPC=RPROPS(4)
      EPSO=RPROPS(5)
      RSIG=RPROPS(6)
      REPS=RPROPS(7)
      NHARD=IPROPS(3)
      YOUNG=EC

C Shear and bulk moduli and other necessary constants
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R4G=R4*GMODU
      R1D3=R1/R3
      R1D6=R1/R6
      R2D3=R2*R1D3
      SQR2D3=SQRT(2.0/3.0)
      R4GD3=R4G*R1D3
C Elastic predictor: Compute elastic trial state
C ----------------------------------------------
C Volumetric strain
      FACTOR=R2G/(BULK+R4GD3)
      EEV=(STRAT(1)+STRAT(2))*FACTOR
C Elastic trial deviatoric strain
      EEVD3=EEV/R3
      EET(1)=STRAT(1)-EEVD3
      EET(2)=STRAT(2)-EEVD3
C Convert engineering shear component into physical component
      EET(3)=STRAT(3)*RP5
C Elastic trial stress components
      PT=BULK*EEV
      STREST(1)=R2G*EET(1)+PT
      STREST(2)=R2G*EET(2)+PT
      STREST(3)=R2G*EET(3)
C Calcula el tensor de esfuerzos principales con una rutina de PEFiCA anidada en este
C mismo archivo.
      CALL TRPRIN(   STREST  ,STPRI      )
C STRES--->VECTOR DE ESFUERZOS
C STPRI--->VECTOR DE ESFUERZOS PRNCIPALES
C  
C Selector para determinar el estado del tensor de esfuerzos
C usando el tensor de esfuerzos en una base principal.
C 1 : TENSION     -   TENSION
C 2 : TENSION     -   COMPRESION
C 3 : COMPRESION  -   COMPRESION
C
C CALCULA DE LA FUNCION DE FLUENCIA DE PRUEBA (CON SIGMA TRIAL Y SIN DGAMA)
C 
      IF(STPRI(1).EQ.0.0)THEN
        S2DS1=0
      ELSE
        S2DS1=STPRI(2)/STPRI(1)
      ENDIF
      IF(STPRI(2).EQ.0.0)THEN
        S1DS2=0
      ELSE
        S1DS2=STPRI(1)/STPRI(2)
      ENDIF
C ESFUERZO UNIAXIAL EQUIVALENTE PARA CUADRANTES C-C Y T-C
C
!  SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))
      CALL FUNSY (EPBARN,FPC,EC,EPSO,RSIG,REPS,STPRI,SIGMAY)
C
C ESFUERZO CORTANTE OCTAEDRICO
      TAOOC=SQRT(2.0)/3.0
      TAOOC=TAOOC*SQRT(STPRI(1)**2.0-STPRI(1)*STPRI(2)+STPRI(2)**2.0)
C ESFUERZO MEDIO
      SIGMM=R1/R3*(STPRI(1)+STPRI(2))
C AA=f´t / f'c = 0.09 (kupfer)
      AA=0.09
      IF(STPRI(2).GT.R0)THEN
C TENSION - TENSION
      ICASO=1
C 1078 FORMAT(/////15X,'TENSION-TENSION')
C      WRITE(*,1078)
      C1=1.0-0.4019*(S2DS1)+0.008913*(S2DS1)**2.0
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI=C1*(Z1+Z2)-SIGMAY
      ELSE
        IF(STPRI(1).GT.R0)THEN
C TENSION - COMPRESION
C 1079 FORMAT(/////15X,'TENSION-COMPRESION')
C      WRITE(*,1079)
      BB=-0.103
      IF(S1DS2.LT.BB) THEN
        ICASO=2
        C2=1-0.02886*S2DS1
        C2=C2-0.006657*(S2DS1)**2
        C2=C2-0.0002443*(S2DS1)**3
      ELSE
        ICASO=3
        C2=1+6.339*(S1DS2)
        C2=C2+68.82*(S1DS2)**2
        C2=C2+183.8*(S1DS2)**3
      ENDIF
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI=C2*(Z1+Z2)-SIGMAY
        ELSE
C COMPRESION - COMPRESION
      ICASO=4
C 1080 FORMAT(/////15X,'COMPRESION- COMPRESION')
C      WRITE(*,1080)
        C3=1+0.05848*S1DS2-0.05848*(S1DS2)**2.0
        BETA=1.16
        Z1=3.0/SQRT(2.0)*(2.0*BETA-1.0)/BETA*TAOOC
        Z2=3.0*(BETA-1.0)/BETA*SIGMM
        PHI=C3*(Z1+Z2)-SIGMAY
        ENDIF
      ENDIF
C
C
C
C Compute yield function value at trial state
C      A1=(STREST(1)+STREST(2))*(STREST(1)+STREST(2))
C      A2=(STREST(2)-STREST(1))*(STREST(2)-STREST(1))
C      A3=STREST(3)*STREST(3)
C      XI=R1D6*A1+RP5*A2+R2*A3
C
C      SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))
C...yield function
C      PHI=RP5*XI-R1D3*SIGMAY*SIGMAY
C
C Check for plastic admissibility
C -------------------------------
        IF(PHI/SIGMAY.GT.TOL)THEN
C 1076 FORMAT(/////15X,'PASO PLASTICO')
C      WRITE(*,1076)
C Plastic step: Apply return mapping - use Newton-Raphson algorithm
C               to solve the plane stress-projected return mapping
C               equation for the plastic multiplier (Box 9.5)
C -----------------------------------------------------------------
        IFPLAS=.TRUE.
C AHORA SE DEBE EVALUAR LA FUNCION DE FLUENCIA CON SIGMA GORRO EN PRINCIPALES (PRINGO)
C Y APLICANDO EL METODO DE SECANTE CALCULAR EL NUEVO D GAMA
        EPBAR=EPBARN
        B1=R1
        B2=R1
        FMODU=YOUNG/(R3*(R1-POISS))








C **************** METER ESTO EN SUBRRITINA DE CÁLCULO DE C:N ***************


C Se requiere el tensor elastico:
        R4GD3=R4*GMODU/R3
        FACTOR=(BULK-R2G/R3)*(R2G/(BULK+R4GD3))
        DO 26 I=1,NSTRE
          DO 25 J=I,NSTRE
            CE(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   25     CONTINUE
   26   CONTINUE
C lower triangle
        DO 28 J=1,NSTRE-1
          DO 27 I=J+1,NSTRE
            CE(I,J)=CE(J,I)
   27     CONTINUE
   28   CONTINUE

C CÁLCULO DEL VECTOR N "VN" EN TÉRMINOS DEL TENSOR DE ESFUERZOS EN LA BASE GENERAL 

      S1=STPRI(1)
      S2=STPRI(2)
      F1=STPRI(1)**2-STPRI(1)*STPRI(2)+STPRI(2)**2
      VAR1=1-0.4019*S2DS1+0.008913*S2DS1**2
      VAR2=(1+AA)*0.5/SQRT(F1)*(2*S1-S2)+(1-AA)
      VAR3=0.4019*S2/(S1**2)-2*0.008913*S2/S1*S2/(S1**2)
      VAR4=(1+AA)*SQRT(F1)+(1-AA)*(S1+S2)
      VAR5=(1+AA)*0.5/SQRT(F1)*(2*S2-S1)+(1-AA)
      VAR6=-0.4019/(S1)+2*0.008913*S2/S1*(1/S1)
      VAR7=1-0.02886*S2/S1-0.006657*(S2/S1)**2-0.0002443*(S2/S1)**3
      VAR8A=0.02886*S2/(S1**2)+2*0.006657*S2/S1*S2/(S1**2)
      VAR8B=3.0*0.0002443*(S2/S1)**2*S2/(S1**2)
      VAR8=VAR8A+VAR8B
      VAR9A=-0.02886/(S1)-2*0.006657*S2/(S1**2)
      VAR9B=-3.0*0.0002443*S2/(S1**2)
      VAR9=VAR9A+VAR9B
      VAR10=1+6.339*S1/S2+68.83*(S1/S2)**2+183.8*(S1/S2)**3
      VAR11A=6.339/(S2)+2*68.83*S1/(S2**2)
      VAR11B=3.0*183.8*(S1/S2)**2*1/(S2)
      VAR11=VAR11A+VAR11B
      VAR12A=-6.339*S1/(S2**2)-2*68.83*S1/S2*S1/(S2**2)
      VAR12B=-3.0*183.8*(S1/S2)**2*S1/(S2**2)
      VAR12=VAR12A+VAR12B
      VAR13=1+0.05848*S1/S2-0.05848*(S1/S2)**2
      VAR14=(2*BETA-1)*0.5/SQRT(F1)*(2*S1-S2)+(BETA-1)
      VAR15=0.05848*1/S2-2.0*0.05848*S1/(S2**2)
      VAR16=(2*BETA-1)*SQRT(F1)+(BETA-1)*(S1+S2)
      VAR17=(2*BETA-1)*0.5/SQRT(F1)*(2*S2-S1)+(BETA-1)
      VAR18=-0.05848*S1/(S2**2)+2.0*0.05848*S1**2/(S2**3)
      IF(STPRI(2).GE.0.0)THEN
C TENSION - TENSION
      DFDS1=1/(2*AA)*(VAR1*VAR2+VAR3*VAR4)
      DFDS2=1/(2*AA)*(VAR1*VAR5+VAR6*VAR4)
      ELSE
        IF(STPRI(1).GT.0.0)THEN
C TENSION - COMPRESION
      IF(S1DS2.LT.-0.103) THEN
        DFDS1=1/(2*AA)*(VAR7*VAR2+VAR8*VAR4)
        DFDS2=1/(2*AA)*(VAR7*VAR5+VAR9*VAR4)
      ELSE
        DFDS1=1/(2*AA)*(VAR10*VAR2+VAR11*VAR4)
        DFDS2=1/(2*AA)*(VAR10*VAR5+VAR12*VAR4)
      ENDIF
        ELSE
C COMPRESION - COMPRESION
      DFDS1=(1/BETA)*(VAR13*VAR14+VAR15*VAR16)
      DFDS2=(1/BETA)*(VAR13*VAR17+VAR18*VAR16)
        ENDIF
      ENDIF
      SXX=STREST(1)
      SYY=STREST(2)
      SXY=STREST(3)
      LI1=((SXX-SYY)**2+4*SXY**2)**(-0.5)
      LI2=SXX-SYY
      DS1DSXX=0.5+0.5*LI1*LI2
      DS2DSXX=0.5-0.5*LI1*LI2
      DS1DSYY=0.5-0.5*LI1*LI2
      DS2DSYY=0.5+0.5*LI1*LI2
      DS1DSXY=2.0*LI1*SXY
      DS2DSXY=-2.0*LI1*SXY

      DFDSXX=DFDS1*DS1DSXX+DFDS2*DS2DSXX
      DFDSYY=DFDS1*DS1DSYY+DFDS2*DS2DSYY
      DFDSXY=DFDS1*DS1DSXY+DFDS2*DS2DSXY

C NORMALIZA EL VECTOR N EN LA BASE GENERAL

      TAMV=SQRT(DFDSXX**2+DFDSYY**2+DFDSXY**2)

      VN(1,1)=DFDSXX/TAMV
      VN(2,1)=DFDSYY/TAMV
      VN(3,1)=DFDSXY/TAMV

C SE CALCULA PRODUCTO C:N EN LA BASE GENERAL

      CALL MV(   CE  ,VN  ,CN   )

C************************ FINALIZA SUBRUTINA CALCULO DE C:N *************








C INICIA CICLO PARA CALCULAR DGAMA USANDO METODO DE LA TANGENTE

        DO 10 NRITER=1,MXITER


C      Compute residual derivative
C      HSLOPE=DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
C      DXI=-A1*FMODU/(R3*B1*B1*B1)-R2G*(A2+R4*A3)/(B2*B2*B2)
C      HBAR=R2*SIGMAY*HSLOPE*SQR2D3*(SQRTXI+DGAMA*DXI/(R2*SQRTXI))
C      DPHI=RP5*DXI-R1D3*HBAR
C Compute Newton-Raphson increment and update equation variable DGAMA
C      DGAMA=DGAMA-PHI/DPHI



C EVALUACION DE LA FUNCION DE FLUENCIA EN TERMINOS DE DGAMA, SEGUN CADA CASO T-T T-C O C-C
C CALCULA DE LA FUNCION DE FLUENCIA GORRO, USANDO ESFUERZOS PRINGO, PRIMERO PARA DGAMA0 (PHI0) Y LUEGO PARA DGAMA1 (PHI1)


C PHI GORRO CON DGAMA0
C______________________________




      SIGGO(1)=STREST(1)-DGAMA0*CN(1,1)  
      SIGGO(2)=STREST(2)-DGAMA0*CN(2,1)
      SIGGO(3)=STREST(3)-DGAMA0*CN(3,1)

C AHORA SIGMA GORRO SE PASA A PRINCIPALES Y SE OBTIENE (PRINGO)
  
      CALL TRPRIN(   SIGGO  ,PRINGO      )

C 
      IF(PRINGO(1).EQ.0.0)THEN
        S2DS1=0
      ELSE
        S2DS1=PRINGO(2)/PRINGO(1)
      ENDIF
      IF(PRINGO(2).EQ.0.0)THEN
        S1DS2=0
      ELSE
        S1DS2=PRINGO(1)/PRINGO(2)
      ENDIF
C ESFUERZO UNIAXIAL EQUIVALENTE PARA CUADRANTES C-C Y T-C

C EPBAR CON DGAMA0

        A1=(SIGGO(1)+SIGGO(2))*(SIGGO(1)+SIGGO(2))
        A2=(SIGGO(2)-SIGGO(1))*(SIGGO(2)-SIGGO(1))
        A3=SIGGO(3)*SIGGO(3)
          B1=R1+FMODU*DGAMA0
          B2=R1+R2G*DGAMA0
          XI=R1D6*A1/(B1*B1)+(RP5*A2+R2*A3)/(B2*B2)
          SQRTXI=SQRT(XI)
          EPBAR0=EPBARN+DGAMA0*SQR2D3*SQRTXI


C SIGMAY CON DGAMA0
C  SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))
      CALL FUNSY (EPBAR0,FPC,EC,EPSO,RSIG,REPS,PRINGO,SIGMAY0)
C
C ESFUERZO CORTANTE OCTAEDRICO
      TAOOC=SQRT(2.0)/3.0
      RADI=PRINGO(1)**2.0-PRINGO(1)*PRINGO(2)+PRINGO(2)**2.0
      TAOOC=TAOOC*SQRT(RADI)
C ESFUERZO MEDIO
      SIGMM=R1/R3*(PRINGO(1)+PRINGO(2))

      IF(PRINGO(2).GT.R0)THEN
C TENSION - TENSION
      ICASO=1
C      WRITE(*,1078)
      C1=1.0-0.4019*(S2DS1)+0.008913*(S2DS1)**2.0
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI0=C1*(Z1+Z2)-SIGMAY0
      ELSE
        IF(PRINGO(1).GT.R0)THEN
C TENSION - COMPRESION

C      WRITE(*,1079)
      BB=-0.103
      IF(S1DS2.LT.BB) THEN
        ICASO=2
        C2=1-0.02886*S2DS1
        C2=C2-0.006657*(S2DS1)**2
        C2=C2-0.0002443*(S2DS1)**3
      ELSE
        ICASO=3
        C2=1+6.339*(S1DS2)
        C2=C2+68.82*(S1DS2)**2
        C2=C2+183.8*(S1DS2)**3
      ENDIF
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI0=C2*(Z1+Z2)-SIGMAY0
        ELSE
C COMPRESION - COMPRESION
      ICASO=4

C      WRITE(*,1080)
        C3=1+0.05848*S1DS2-0.05848*(S1DS2)**2.0
        BETA=1.16
        Z1=3.0/SQRT(2.0)*(2.0*BETA-1.0)/BETA*TAOOC
        Z2=3.0*(BETA-1.0)/BETA*SIGMM
        PHI0=C3*(Z1+Z2)-SIGMAY0
        ENDIF
      ENDIF



C PHI GORRO CON DGAMA1
C______________________________


      SIGGO(1)=STREST(1)-DGAMA1*CN(1,1)  
      SIGGO(2)=STREST(2)-DGAMA1*CN(2,1)
      SIGGO(3)=STREST(3)-DGAMA1*CN(3,1)


C AHORA SIGMA GORRO SE PASA A PRINCIPALES Y SE OBTIENE (PRINGO)
  
      CALL TRPRIN(   SIGGO  ,PRINGO      )

C 
      IF(PRINGO(1).EQ.0.0)THEN
        S2DS1=0
      ELSE
        S2DS1=PRINGO(2)/PRINGO(1)
      ENDIF
      IF(PRINGO(2).EQ.0.0)THEN
        S1DS2=0
      ELSE
        S1DS2=PRINGO(1)/PRINGO(2)
      ENDIF

C EPBAR CON DGAMA1

        A1=(SIGGO(1)+SIGGO(2))*(SIGGO(1)+SIGGO(2))
        A2=(SIGGO(2)-SIGGO(1))*(SIGGO(2)-SIGGO(1))
        A3=SIGGO(3)*SIGGO(3)
          B1=R1+FMODU*DGAMA1
          B2=R1+R2G*DGAMA1
          XI=R1D6*A1/(B1*B1)+(RP5*A2+R2*A3)/(B2*B2)
          SQRTXI=SQRT(XI)
          EPBAR1=EPBARN+DGAMA1*SQR2D3*SQRTXI


C SIGMAY CON DGAMA1
C ESFUERZO UNIAXIAL EQUIVALENTE PARA CUADRANTES C-C Y T-C
C
!  SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))
      CALL FUNSY (EPBAR1,FPC,EC,EPSO,RSIG,REPS,PRINGO,SIGMAY1)
C
C ESFUERZO CORTANTE OCTAEDRICO
      TAOOC=SQRT(2.0)/3.0
      RADI=PRINGO(1)**2.0-PRINGO(1)*PRINGO(2)+PRINGO(2)**2.0
      TAOOC=TAOOC*SQRT(RADI)
C ESFUERZO MEDIO
      SIGMM=R1/R3*(PRINGO(1)+PRINGO(2))

      IF(PRINGO(2).GT.R0)THEN
C TENSION - TENSION
      ICASO=1

C      WRITE(*,1078)
      C1=1.0-0.4019*(S2DS1)+0.008913*(S2DS1)**2.0
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI1=C1*(Z1+Z2)-SIGMAY1
      ELSE
        IF(PRINGO(1).GT.R0)THEN
C TENSION - COMPRESION

C      WRITE(*,1079)
      BB=-0.103
      IF(S1DS2.LT.BB) THEN
        ICASO=2
        C2=1-0.02886*S2DS1
        C2=C2-0.006657*(S2DS1)**2
        C2=C2-0.0002443*(S2DS1)**3
      ELSE
        ICASO=3
        C2=1+6.339*(S1DS2)
        C2=C2+68.82*(S1DS2)**2
        C2=C2+183.8*(S1DS2)**3
      ENDIF
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI1=C2*(Z1+Z2)-SIGMAY1
        ELSE
C COMPRESION - COMPRESION
      ICASO=4

C      WRITE(*,1080)
        C3=1+0.05848*S1DS2-0.05848*(S1DS2)**2.0
        BETA=1.16
        Z1=3.0/SQRT(2.0)*(2.0*BETA-1.0)/BETA*TAOOC
        Z2=3.0*(BETA-1.0)/BETA*SIGMM
        PHI1=C3*(Z1+Z2)-SIGMAY1
        ENDIF
      ENDIF


C USANDO METODO DE LA SECANTE SE ACTUALIZA EL VALOR PARA DGAMA      
      
      
      DGAMA=DGAMA1-PHI1*(DGAMA1-DGAMA0)/(PHI1-PHI0)
      DGAMA0=DGAMA1
      DGAMA1=DGAMA






C  DGAMA=(DGAMA+DGAMA1-PHI1*(DGAMA1-DGAMA0)/(PHI1-PHI0))/2
!  DGAMA=DGAMA1-PHI1*(DGAMA1-DGAMA0)/(PHI1-PHI0)
!  IF(DGAMA.LT.0.0)THEN
!    DGAMA=(DGAMA0+DGAMA1)/2
!  END IF
!  DGAMA0=DGAMA1
!  DGAMA1=DGAMA




C 1084 FORMAT(34X,'DGAMA = ',G15.6)
C      WRITE(*,1084) DGAMA
C SIGUE SI SE USA METODO DE MÜLLER

C PHI GORRO CON DGAMA2
C______________________________

    
      SIGGO(1)=STREST(1)-DGAMA2*CN(1,1)  
      SIGGO(2)=STREST(2)-DGAMA2*CN(2,1)
      SIGGO(3)=STREST(3)-DGAMA2*CN(3,1)

C AHORA SIGMA GORRO SE PASA A PRINCIPALES Y SE OBTIENE (PRINGO)
  
      CALL TRPRIN(   SIGGO  ,PRINGO      )

C 
      IF(PRINGO(1).EQ.0.0)THEN
        S2DS1=0
      ELSE
        S2DS1=PRINGO(2)/PRINGO(1)
      ENDIF
      IF(PRINGO(2).EQ.0.0)THEN
        S1DS2=0
      ELSE
        S1DS2=PRINGO(1)/PRINGO(2)
      ENDIF

C EPBAR CON DGAMA2

        A1=(SIGGO(1)+SIGGO(2))*(SIGGO(1)+SIGGO(2))
        A2=(SIGGO(2)-SIGGO(1))*(SIGGO(2)-SIGGO(1))
        A3=SIGGO(3)*SIGGO(3)
          B1=R1+FMODU*DGAMA2
          B2=R1+R2G*DGAMA2
          XI=R1D6*A1/(B1*B1)+(RP5*A2+R2*A3)/(B2*B2)
          SQRTXI=SQRT(XI)
          EPBAR2=EPBARN+DGAMA2*SQR2D3*SQRTXI


C SIGMAY CON DGAMA2
C ESFUERZO UNIAXIAL EQUIVALENTE PARA CUADRANTES C-C Y T-C
C
!  SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))
      CALL FUNSY (EPBAR2,FPC,EC,EPSO,RSIG,REPS,PRINGO,SIGMAY2)
C
C ESFUERZO CORTANTE OCTAEDRICO
      TAOOC=SQRT(2.0)/3.0
      RADI=PRINGO(1)**2.0-PRINGO(1)*PRINGO(2)+PRINGO(2)**2.0
      TAOOC=TAOOC*SQRT(RADI)
C ESFUERZO MEDIO
      SIGMM=R1/R3*(PRINGO(1)+PRINGO(2))

      IF(PRINGO(2).GT.R0)THEN
C TENSION - TENSION
      ICASO=1
C      WRITE(*,1078)
      C1=1.0-0.4019*(S2DS1)+0.008913*(S2DS1)**2.0
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI2=C1*(Z1+Z2)-SIGMAY2
      ELSE
        IF(PRINGO(1).GT.R0)THEN
C TENSION - COMPRESION

C      WRITE(*,1079)
      BB=-0.103
      IF(S1DS2.LT.BB) THEN
        ICASO=2
        C2=1-0.02886*S2DS1
        C2=C2-0.006657*(S2DS1)**2
        C2=C2-0.0002443*(S2DS1)**3
      ELSE
        ICASO=3
        C2=1+6.339*(S1DS2)
        C2=C2+68.82*(S1DS2)**2
        C2=C2+183.8*(S1DS2)**3
      ENDIF
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI2=C2*(Z1+Z2)-SIGMAY2
        ELSE
C COMPRESION - COMPRESION
      ICASO=4

C      WRITE(*,1080)
        C3=1+0.05848*S1DS2-0.05848*(S1DS2)**2.0
        BETA=1.16
        Z1=3.0/SQRT(2.0)*(2.0*BETA-1.0)/BETA*TAOOC
        Z2=3.0*(BETA-1.0)/BETA*SIGMM
        PHI2=C3*(Z1+Z2)-SIGMAY2
        ENDIF
      ENDIF

C METODO DE MÜLLER PARA CALCULAR DGAMA

c    AAA=(DGAMA1-DGAMA2)*(PHI0-PHI2)
c    AAA=AAA-(DGAMA0-DGAMA2)*(PHI1-PHI2)
c    AAA=AAA/((DGAMA0-DGAMA2)*(DGAMA1-DGAMA2)*(DGAMA0-DGAMA1))
    
c    BBB=(DGAMA0-DGAMA2)**2*(PHI1-PHI2)
c    BBB=BBB-(DGAMA1-DGAMA2)**2*(PHI0-PHI2)
c    BBB=BBB/((DGAMA0-DGAMA2)*(DGAMA1-DGAMA2)*(DGAMA0-DGAMA1))
    
c    CCC=PHI2
  
c    SB=BBB/ABS(BBB)
c    DETTE=BBB**2-4.0*AAA*CCC

c  IF(DETTE.LE.0.0)THEN
c 1082 FORMAT(/////15X,'¡¡¡DETERM. NEG!!!')
c      WRITE(*,1082)
c  ENDIF
c CALCULO DE DGAMA CON MÜLLER

c    DGAMA=DGAMA2-2.0*CCC/(BBB+SB*SQRT(DETTE))    
c    DGAMA0=DGAMA1
c    DGAMA1=DGAMA2
c    DGAMA2=DGAMA


C EPBAR CON DGAMA


C PHI GORRO CON DGAMA ¿Y EVALUADA EN STPRI O PRINGO?
C ES CON PRINGO, PUES EN LA BOX 9.5 ESTA EN TERMINOS DE DGAMA
C______________________________

    
      SIGGO(1)=STREST(1)-DGAMA*CN(1,1)  
      SIGGO(2)=STREST(2)-DGAMA*CN(2,1)
      SIGGO(3)=STREST(3)-DGAMA*CN(3,1)



        A1=(SIGGO(1)+SIGGO(2))*(SIGGO(1)+SIGGO(2))
        A2=(SIGGO(2)-SIGGO(1))*(SIGGO(2)-SIGGO(1))
        A3=SIGGO(3)*SIGGO(3)
          B1=R1+FMODU*DGAMA
          B2=R1+R2G*DGAMA
          XI=R1D6*A1/(B1*B1)+(RP5*A2+R2*A3)/(B2*B2)
          SQRTXI=SQRT(XI)
          EPBAR=EPBARN+DGAMA*SQR2D3*SQRTXI


C ACTUALIZAR FUNCIÓN DE FLUENCIA PARA NUEVO VALOR DE DGAMA




C AHORA SIGMA GORRO SE PASA A PRINCIPALES Y SE OBTIENE (PRINGO)
  
      CALL TRPRIN(   SIGGO  ,PRINGO      )
C 
      IF(PRINGO(1).EQ.0.0)THEN
        S2DS1=0
      ELSE
        S2DS1=PRINGO(2)/PRINGO(1)
      ENDIF
      IF(PRINGO(2).EQ.0.0)THEN
        S1DS2=0
      ELSE
        S1DS2=PRINGO(1)/PRINGO(2)
      ENDIF
C ESFUERZO UNIAXIAL EQUIVALENTE PARA CUADRANTES C-C Y T-C
C
!  SIGMAY=PLFUN(EPBAR,NHARD,RPROPS(IPHARD))

        
      
      CALL FUNSY (EPBAR,FPC,EC,EPSO,RSIG,REPS,PRINGO,SIGMAY)
      

          
!  CALL FUNSY (EPBARN,FPC,EC,EPSO,RSIG,REPS,PRINGO,SIGMAY)
C
C ESFUERZO CORTANTE OCTAEDRICO
      TAOOC=SQRT(2.0)/3.0
      RADI=PRINGO(1)**2.0-PRINGO(1)*PRINGO(2)+PRINGO(2)**2.0
      TAOOC=TAOOC*SQRT(RADI)
C ESFUERZO MEDIO
      SIGMM=R1/R3*(PRINGO(1)+PRINGO(2))

      IF(PRINGO(2).GT.R0)THEN
C TENSION - TENSION
      ICASO=1

C      WRITE(*,1078)
      C1=1.0-0.4019*(S2DS1)+0.008913*(S2DS1)**2.0
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI=C1*(Z1+Z2)-SIGMAY
      ELSE
        IF(PRINGO(1).GT.R0)THEN
C TENSION - COMPRESION

C      WRITE(*,1079)
      BB=-0.103
      IF(S1DS2.LT.BB) THEN
        ICASO=2
        C2=1-0.02886*S2DS1
        C2=C2-0.006657*(S2DS1)**2
        C2=C2-0.0002443*(S2DS1)**3
      ELSE
        ICASO=3
        C2=1+6.339*(S1DS2)
        C2=C2+68.82*(S1DS2)**2
        C2=C2+183.8*(S1DS2)**3
      ENDIF
      Z1=3.0/(2.0*SQRT(2.0))*(1.0+AA)/AA*TAOOC
      Z2=3.0/2.0*(1.0-AA)/AA*SIGMM
      PHI=C2*(Z1+Z2)-SIGMAY
        ELSE
C COMPRESION - COMPRESION
      ICASO=4

C      WRITE(*,1080)
        C3=1+0.05848*S1DS2-0.05848*(S1DS2)**2.0
        BETA=1.16
        Z1=3.0/SQRT(2.0)*(2.0*BETA-1.0)/BETA*TAOOC
        Z2=3.0*(BETA-1.0)/BETA*SIGMM
        PHI=C3*(Z1+Z2)-SIGMAY
        ENDIF
      ENDIF


C Check for convergence
      RESNOR=ABS(PHI/SIGMAY)
        IF(RESNOR.LE.TOL)THEN

C 1081 FORMAT(/////15X,'****CONVERGE DEGAMA****')
C      WRITE(*,1081)

C update accumulated plastic strain
        RSTAVA(MSTRE+1)=EPBAR
C update stress components:   sigma := A sigma^trial
C        ASTAR1=R3*(R1-POISS)/(R3*(R1-POISS)+YOUNG*DGAMA)
C        ASTAR2=R1/(R1+R2G*DGAMA)
C        A11=RP5*(ASTAR1+ASTAR2)
C        A22=A11
C        A12=RP5*(ASTAR1-ASTAR2)
C        A21=A12
C        A33=ASTAR2
C        STRES(1)=A11*STREST(1)+A12*STREST(2)
C        STRES(2)=A21*STREST(1)+A22*STREST(2)
C        STRES(3)=A33*STREST(3)

        STRES(1)=STREST(1)-DGAMA*CN(1,1)  
        STRES(2)=STREST(2)-DGAMA*CN(2,1)
        STRES(3)=STREST(3)-DGAMA*CN(3,1)

C compute corresponding elastic (engineering) strain components
        FACTG=R1/R2G
        P=R1D3*(STRES(1)+STRES(2))
        EEV=P/BULK
        EEVD3=R1D3*EEV
        RSTAVA(1)=FACTG*(R2D3*STRES(1)-R1D3*STRES(2))+EEVD3
        RSTAVA(2)=FACTG*(R2D3*STRES(2)-R1D3*STRES(1))+EEVD3
        RSTAVA(3)=FACTG*STRES(3)*R2
        RSTAVA(4)=-POISS/(R1-POISS)*(RSTAVA(1)+RSTAVA(2))
        GOTO 999
        ENDIF
   10    CONTINUE
C reset failure flag and print warning message if N-R algorithm fails
        SUFAIL=.TRUE.
        CALL ERRPRT('WE0013')
        ELSE
C Elastic step: Update stress using linear elastic law
C 1077 FORMAT(/////15X,'PASO ELASTICO')
C      WRITE(*,1077)

C ----------------------------------------------------
      STRES(1)=STREST(1)
      STRES(2)=STREST(2)
      STRES(3)=STREST(3)

C elastic engineering strain
      RSTAVA(1)=STRAT(1)
      RSTAVA(2)=STRAT(2)
      RSTAVA(3)=STRAT(3)
      RSTAVA(4)=-POISS/(R1-POISS)*(STRAT(1)+STRAT(2))
        ENDIF
  999    CONTINUE
C Update some algorithmic variables before exit
      LALGVA(1)=IFPLAS
      LALGVA(2)=SUFAIL
C1073  FORMAT(/////15X,'SALIENDO DE SUCOPL')
C      WRITE(*,1073)  
      RETURN
      END
C
C





C CALCULO DE SIGMAY
C SE CALCULA CON LA CURVA UNIAXIAL EQUIVALENTE DEL MODELO

      SUBROUTINE FUNSY (EBAR,FPC,EC,EPSO,RSIG,REPS,SIGMA,SIGMAY)
C Definir variables
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    SIGMA (*)
      IF(SIGMA(1).EQ.0.0)THEN
        S2DS1=0
      ELSE
        S2DS1=SIGMA(2)/SIGMA(1)
      ENDIF
      IF(SIGMA(2).EQ.0.0)THEN
        S1DS2=0
      ELSE
        S1DS2=SIGMA(1)/SIGMA(2)
      ENDIF
      F1=FPC/(EC*EPSO)
      F2=1.0-F1
  
      IF(SIGMA(2).GT.0.0)THEN
C TENSION - TENSION
      SIGMAY=FPC
      ICASO=1
      ELSE
      ICASO=2
        IF(SIGMA(1).GT.0.0)THEN
C TENSION - COMPRESION
      BB=-0.103
      IF(S1DS2.LT.BB) THEN
        F3=0.001231*S2DS1+0.001469*S2DS1**2+0.0000134*S2DS1**3
      ELSE
        F3=1.0+13.96*S1DS2+59.21*S1DS2**2+69.24*S1DS2**3
      ENDIF

        ELSE
C COMPRESION - COMPRESION
        F3=1.0+1.782*S1DS2+0.5936*S1DS2**2
        ENDIF
      ENDIF
      Q=F1+F2*F3
      EPSAS=Q*EPSO
      EO=FPC/EPSAS
      RE=EC/EO
      R=RE*(RSIG-1.0)/((REPS-1.0)**2)-1.0/REPS
  
C CONTROL EN CASO DE QUE ALGUNA ITERACIÓN DE 
C COMO RESULTADO UN DGAMA NEGATIVO  
  
      IF(EBAR.LT.0.0)THEN
        EPSILON=EPSAS
      ELSE
        EPSILON=EBAR+EPSAS
      ENDIF

      FAC1=EC*EPSILON
      FAC2=1.0+(R+RE-2.0)*EPSILON/EPSAS
      FAC3=(2.0*R-1.0)*(EPSILON/EPSAS)**2-R*(EPSILON/EPSAS)**3
        IF(ICASO.EQ.2)THEN
      SIGMAY=FAC1/(FAC2-FAC3)
        ENDIF
C  SIGMAY=21000

      END
