      SUBROUTINE CTCOPL
     1(   DGAMA      ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=4  ,MSTRE=4   ,NSTRE=3)
      LOGICAL EPFLAG
C Array arguments
      DIMENSION
     1    DMATX(MSTRE,MSTRE),IPROPS(*)           ,RPROPS(*)          ,
     2    RSTAVA(MSTRE+1)   ,STRES(MSTRE)
C Local arrays
      DIMENSION
     1    FOID(MSTRE,MSTRE)  ,SOID(MSTRE)        ,VECN(3)
      DIMENSION
     1    DG(NSTRE,1)     ,DF(1,NSTRE)  ,CE(NSTRE,NSTRE) ,
     2    CP(NSTRE,NSTRE) ,STPRI(NSTRE)
      DIMENSION
     1    Z1(NSTRE,1)  ,Z2(NSTRE,NSTRE)  , Z3(NSTRE,NSTRE),
     2    Z4(1,NSTRE)  ,Z5(1,1)
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

C1071  FORMAT(/////15X,'ENTRANDO A CTCOPL')
C      WRITE(*,1071)

C***********************************************************************
C COMPUTATION OF THE CONSISTENT TANGENT MODULUS FOR VON MISES TYPE
C ELASTO-PLASTIC MATERIAL WITH PIECE-WISE LINEAR ISOTROPIC HARDENING.
C PLANE STRESS IMPLEMENTATION ONLY.
C
C REFERENCE: Section 9.4.5
C***********************************************************************
C Stops program if neither not plane stress
      IF(NTYPE.NE.1)CALL ERRPRT('EI0032')
C Current accumulated plastic strain
      EPBAR=RSTAVA(MSTRE+1)
C Set material properties
      EC=RPROPS(2)
      POISS=RPROPS(3)
      FPC=RPROPS(4)
      EPSO=RPROPS(5)
      RSIG=RPROPS(6)
      REPS=RPROPS(7)
      NHARD=IPROPS(3)

      CALL TRPRIN(   STRES ,STPRI      )
      CALL FUNSY (EPBAR,FPC,EC,EPSO,RSIG,REPS,STPRI,SIGMAY)
C  CALL ETAN (EPBAR,FPC,EC,EPSO,RSIG,REPS,STPRI,ET)

      YOUNG=EC
C Shear and bulk moduli
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R1D3=R1/R3
      R2D3=R2*R1D3
C
      IF(EPFLAG)THEN
C CALCULA TENSOR CONSTITUTIVO TANGENTE
C ===========================================================
      CALL ETAN (EPBAR,FPC,EC,EPSO,RSIG,REPS,STPRI,ET)
C Se requiere el tensor elastico:
        R4GD3=R4*GMODU/R3
        FACTOR=(BULK-R2G/R3)*(R2G/(BULK+R4GD3))
        DO 20 I=1,NSTRE
          DO 10 J=I,NSTRE
            CE(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   10     CONTINUE
   20   CONTINUE
C lower triangle
        DO 40 J=1,NSTRE-1
          DO 30 I=J+1,NSTRE
            CE(I,J)=CE(J,I)
   30     CONTINUE
   40   CONTINUE
C


C STRES--->VECTOR DE ESFUERZOS
C STPRI--->VECTOR DE ESFUERZOS PRNCIPALES

C OJO, NO SE SABE SI LAS VARIABLES QUE REQUIERE SIGNAY SON GLOBALES**********
C  SIGMAY=PLFUN(EPBARN,NHARD,RPROPS(IPHARD))


      F1=STPRI(1)**2-STPRI(1)*STPRI(2)+STPRI(2)**2
      DGDS1=(1.0/2.0)/(F1**(0.5))*(2*STPRI(1)-STPRI(2))
      DGDS2=(1.0/2.0)/(F1**(0.5))*(2*STPRI(2)-STPRI(1))
C
C Selector para determinar el estado del tensor de esfuerzos
C usando el tensor de esfuerzos en una base principal.
C 1 : TENSION     -   TENSION
C 2 : TENSION     -   COMPRESION
C 3 : COMPRESION  -   COMPRESION
C
C CALCULO DE LAS FUNCIONES DE FLUENCIA DEPENDIENDO DEL ESTADO DE ESFUERZOS
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
C
C ESFUERZO CORTANTE OCTAEDRICO
      TAOOC=SQRT(2.0)/3.0
      TAOOC=TAOOC*SQRT(STPRI(1)**2.0-STPRI(1)*STPRI(2)+STPRI(2)**2.0)
C ESFUERZO MEDIO
      SIGMM=R1/R3*(STPRI(1)+STPRI(2))
      AA=0.09
      BETA=1.16
      S1=STPRI(1)
      S2=STPRI(2)
C
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
C
      IF(STPRI(2).GE.0.0)THEN
C TENSION - TENSION
C

      DFDS1=1/(2*AA)*(VAR1*VAR2+VAR3*VAR4)
      DFDS2=1/(2*AA)*(VAR1*VAR5+VAR6*VAR4)
C
      ELSE
C
        IF(STPRI(1).GT.0.0)THEN
C TENSION - COMPRESION
      IF(S1DS2.LT.-0.103) THEN
C
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

      DG(1,1)=DGDS1
      DG(2,1)=DGDS2
      DG(3,1)=0.0

      DF(1,1)=DFDS1
      DF(1,2)=DFDS2
      DF(1,3)=0.0

      G=3.0*TAOOC/SQRT(2.0)
      GDSY=G/SIGMAY

      CALL MV(CE,DG,Z1)
      CALL VV1(Z1,DF,Z2)
      CALL MM(Z2,CE,Z3)
      CALL VM(DF,CE,Z4)
      CALL VV2(Z4,DG,Z5)

C VARIABLE DE ENDURECIMIENTO / ABLANDAMIENTO

      HSLOPE=EC*ET/(EC-ET)
C  HSLOPE=ET
C  HSLOPE=10E6
C     48000000


      Z6=HSLOPE*GDSY+Z5(1,1)
C CÁLCULO DE TENSOR CONSTITUTIVO PLÁSTICO
      CP(1,1)=Z3(1,1)/Z6
      CP(1,2)=Z3(1,2)/Z6
      CP(1,3)=Z3(1,3)/Z6
      CP(2,1)=Z3(2,1)/Z6
      CP(2,2)=Z3(2,2)/Z6
      CP(2,3)=Z3(2,3)/Z6
      CP(3,1)=Z3(3,1)/Z6
      CP(3,2)=Z3(3,2)/Z6
      CP(3,3)=Z3(3,3)/Z6
C CÁLCULO DE TENSOR CONSTITUTIVO TANGENTE
      DMATX(1,1)=CE(1,1)-CP(1,1)
      DMATX(1,2)=CE(1,2)-CP(1,2)
      DMATX(1,3)=CE(1,3)-CP(1,3)
      DMATX(2,1)=CE(2,1)-CP(2,1)
      DMATX(2,2)=CE(2,2)-CP(2,2)
      DMATX(2,3)=CE(2,3)-CP(2,3)
      DMATX(3,1)=CE(3,1)-CP(3,1)
      DMATX(3,2)=CE(3,2)-CP(3,2)
      DMATX(3,3)=CE(3,3)-CP(3,3)

      ELSE
C Compute plane stress elasticity matrix
C ===========================================================
        R4GD3=R4*GMODU/R3
        FACTOR=(BULK-R2G/R3)*(R2G/(BULK+R4GD3))
        DO 60 I=1,NSTRE
          DO 50 J=I,NSTRE
            DMATX(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   50     CONTINUE
   60   CONTINUE
C lower triangle
        DO 80 J=1,NSTRE-1
          DO 70 I=J+1,NSTRE
            DMATX(I,J)=DMATX(J,I)
   70     CONTINUE
   80   CONTINUE
      ENDIF
C1075 FORMAT(/////15X,'HSLOPE',G15.6)
C     WRITE(*,1075)CE(1,1)
C  WRITE(*,1075)CP(1,1)
C  WRITE(*,1075)DMATX(1,1)
C  WRITE(*,1075)HSLOPE
      RETURN
      END
C
C
C
C
C
C
C CALCULA LA MULTIPLICACION DE DOS MATRICES 3X3
C
C    C = A x B
C
      SUBROUTINE MM
     1(   A  ,  B  ,  C    )
C Definir variables
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER H
      DIMENSION
     1    A(3,3),B(3,3),C(3,3)
      DO 110 I=1,3
        DO 100 J=1,3
      SUMA=0.0
      DO 90 H=1,3
        SUMA=SUMA+A(I,H)*B(H,J)
   90      CONTINUE
      C(I,J)=SUMA
  100    CONTINUE
  110  CONTINUE
      RETURN
      END


C CALCULA LA MULTIPLICACION DE UNA MATRIZ 3X3 Y UN VECTOR 3X1
C
C    C = A x B
C
      SUBROUTINE MV
     1(   A  ,  B  ,  C     )
C Definir variables
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER H
      DIMENSION
     1    A(3,3),B(3,1),C(3,1)
      DO 110 I=1,3
        DO 100 J=1,1
      SUMA=0.0
      DO 90 H=1,3
        SUMA=SUMA+A(I,H)*B(H,J)
   90      CONTINUE
      C(I,J)=SUMA
  100    CONTINUE
  110  CONTINUE
      RETURN
      END
C
C CALCULA LA MULTIPLICACION UN VECTOR 3X1 CON UN VECTOR 1X3
C
C    C = A x B
C
      SUBROUTINE VV1
     1(   A  ,  B  ,  C     )
C Definir variables
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER H
      DIMENSION
     1    A(3,1),B(1,3),C(3,3)
      DO 110 I=1,3
        DO 100 J=1,3
      SUMA=0.0
      DO 90 H=1,1
        SUMA=SUMA+A(I,H)*B(H,J)
   90      CONTINUE
      C(I,J)=SUMA
  100    CONTINUE
  110  CONTINUE
      RETURN
      END
C
C CALCULA LA MULTIPLICACION UN VECTOR 1X3 CON UN VECTOR DE 3X1
C
C    C = A x B
C
      SUBROUTINE VV2
     1(   A  ,  B  ,  C    )
C Definir variables
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER H
      DIMENSION
     1    A(1,3),B(3,1),C(1,1)
      DO 110 I=1,1
        DO 100 J=1,1
      SUMA=0.0
      DO 90 H=1,3
        SUMA=SUMA+A(I,H)*B(H,J)
   90      CONTINUE
      C(I,J)=SUMA
  100    CONTINUE
  110  CONTINUE
      RETURN
      END



C
C CALCULA LA MULTIPLICACION UN VECTOR 1X3 CON UNA MATRIZ DE 3X3
C
C    C = A x B
C
      SUBROUTINE VM
     1(   A  ,  B  ,  C     )
C Definir variables
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER H
      DIMENSION
     1    A(1,3),B(3,3),C(1,3)
      DO 110 I=1,1
        DO 100 J=1,3
      SUMA=0.0
      DO 90 H=1,3
        SUMA=SUMA+A(I,H)*B(H,J)
   90      CONTINUE
      C(I,J)=SUMA
  100    CONTINUE
  110  CONTINUE
      RETURN
      END







C CALCULO DEL MÓDULO TANGENTE UNIAXIAL EQUIVALENTE (ET)

      SUBROUTINE ETAN (EBAR,FPC,EC,EPSO,RSIG,REPS,SIGMA,ET)
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
      HSLOPE=1.0E-6
      ELSE
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
      Q=1.0
      EPSAS=Q*EPSO
      EO=FPC/EPSAS
      RE=EC/EO
      R=RE*(RSIG-1.0)/((REPS-1.0)**2)-1.0/REPS

      EPSILON=EPSAS+EBAR

      W1=1.0+(2.0*R-1.0)*(EPSILON/EPSAS)**2
      W2=2.0*R*(EPSILON/EPSAS)**3
      FAC1=EC*(W1-W2)
      FAC2=1.0+(R+RE-2.0)*EPSILON/EPSAS
      FAC3=(2.0*R-1.0)*(EPSILON/EPSAS)**2-R*(EPSILON/EPSAS)**3
      ET=FAC1/((FAC2-FAC3)**2)

C 1083 FORMAT(34X,'EPS = ',G15.6)
C      WRITE(*,1083) EPSILON
      END
