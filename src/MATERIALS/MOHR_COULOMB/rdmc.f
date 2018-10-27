      SUBROUTINE RDMC
     1(   IPROPS     ,MIPROP     ,MLALGV     ,MRALGV     ,MRPROP     ,
     2    MRSTAV     ,RPROPS     ,UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UNSYM
      PARAMETER( IPHARD=7  ,NIPROP=3  ,NLALGV=5  ,NRALGV=2  ,NRSTAV=5 )
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)
      DATA R0   ,R1   ,R90   ,R180   / 
     1     0.0D0,1.0D0,90.0D0,180.0D0/
C***********************************************************************
C READ AND ECHO MATERIAL PROPERTIES FOR MOHR-COULOMB TYPE
C ELASTO-PLASTIC MATERIAL WITH ASSOCIATIVE/NON-ASSOCIATIVE FLOW
C RULE AND NON-LINEAR ISOTROPIC HARDENING
C
C REFERENCE: Section 8.2
C***********************************************************************
 1000 FORMAT(' Elasto-plastic with MOHR-COULOMB yield criterion'/)
 1100 FORMAT(
     1' Mass density ...................................... =',G15.6/
     2' Young''s modulus ................................... =',G15.6/
     3' Poisson''s ratio ................................... =',G15.6/
     4' Friction angle  (degrees) ......................... =',G15.6/
     5' Dilatancy angle (degrees) ......................... =',G15.6)
 1200 FORMAT(/
     1'  Friction and dilatancy angles coincide ->',
     2 ' ASSOCIATIVE flow')
 1300 FORMAT(/
     1'  Friction and dilatancy angles are distinct ->',
     2 ' NON-ASSOCIATIVE flow')
 1400 FORMAT(/
     1' Number of points on hardening curve ............... =',I3//
     2'           Epstn            cohesion'/)
 1500 FORMAT(2(5X,G15.6))
C
C Read and echo some of the real properties
      WRITE(16,1000)
      READ(15,*)DENSE
      READ(15,*)YOUNG,POISS,PHI,PSI
      WRITE(16,1100)DENSE,YOUNG,POISS,PHI,PSI
C Check validity if some material properties
      IF(YOUNG.LT.R0)CALL ERRPRT('ED0136')
      IF(PHI.LT.R0.OR.PHI.GE.R90)CALL ERRPRT('ED0137')
C Check and echo if flow rule is associative or non-associative
      IF(PHI.EQ.PSI)THEN
        WRITE(16,1200)
      ELSE
        WRITE(16,1300)
      ENDIF
C Set material constants array
      RADEG=ACOS(-R1)/R180
      PHIRAD=PHI*RADEG
      SINPHI=SIN(PHIRAD)
      COSPHI=COS(PHIRAD)
      PSIRAD=PSI*RADEG
      SINPSI=SIN(PSIRAD)
C Hardening curve
      READ(15,*)NHARD
      WRITE(16,1400)NHARD
      IF(NHARD.LT.2)CALL ERRPRT('ED0138')
C Check dimension of IPROPS
      IF(MIPROP.LT.NIPROP)CALL ERRPRT('ED0187')
      IPROPS(3)=NHARD
C Check dimension of RPROPS
      NRPROP=IPHARD+NHARD*2-1
      IF(NRPROP.GT.MRPROP)CALL ERRPRT('ED0183')
C Store real properties in RPROPS
      RPROPS(1)=DENSE
      RPROPS(2)=YOUNG
      RPROPS(3)=POISS
      RPROPS(4)=SINPHI
      RPROPS(5)=COSPHI
      RPROPS(6)=SINPSI
      DO 10 IHARD=1,NHARD
        READ(15,*)RPROPS(IPHARD+IHARD*2-2),
     1            RPROPS(IPHARD+IHARD*2-1)
        WRITE(16,1500)RPROPS(IPHARD+IHARD*2-2),
     1                RPROPS(IPHARD+IHARD*2-1)
   10 CONTINUE
C
C Check dimension of RSTAVA, LALGVA and RALGVA
C
      IF(NRSTAV.GT.MRSTAV)CALL ERRPRT('ED0184')
      IF(NLALGV.GT.MLALGV)CALL ERRPRT('ED0185')
      IF(NRALGV.GT.MRALGV)CALL ERRPRT('ED0186')
C
C Set unsymmetric tangent stiffness flag for non-associative flow
      IF(PHI.NE.PSI)THEN
        UNSYM=.TRUE.
      ELSE
        UNSYM=.FALSE.
      ENDIF 
C
      RETURN
      END
