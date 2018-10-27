      SUBROUTINE RDTR
     1(   IPROPS     ,MIPROP     ,MLALGV     ,MRALGV     ,MRPROP     ,
     2    MRSTAV     ,NLARGE     ,NTYPE      ,RPROPS     ,UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UNSYM
      PARAMETER( IPHARD=4  ,NIPROP=3  ,NLALGV=4  ,NRALGV=2  ,NRSTAV=5 )
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)
      DATA R0 / 0.0D0 /
C***********************************************************************
C READ AND ECHO MATERIAL PROPERTIES FOR TRESCA TYPE ELASTO-PLASTIC
C MATERIAL WITH NON-LINEAR ISOTROPIC HARDENING
C
C REFERENCE: Section 8.1
C***********************************************************************
 1000 FORMAT(' Elasto-plastic with TRESCA yield criterion'/)
 1100 FORMAT(
     1' Mass density ...................................... =',G15.6/
     2' Young''s modulus ................................... =',G15.6/
     3' Poisson''s ratio ................................... =',G15.6)
 1200 FORMAT(/
     1' Number of points on hardening curve ............... =',I3//
     2'           Epstn        uniaxial yield stress '/)
 1300 FORMAT(2(5X,G15.6))
C
C Model not yet implemented for large strains with plane stress
      IF(NLARGE.EQ.1.AND.NTYPE.EQ.1)CALL ERRPRT('ED0198')
C Set unsymmetric tangent stiffness flag
      UNSYM=.FALSE.
C Read and echo some of the real properties
      WRITE(16,1000)
      READ(15,*)DENSE
      READ(15,*)YOUNG,POISS
      WRITE(16,1100)DENSE,YOUNG,POISS
      IF(YOUNG.LT.R0)CALL ERRPRT('ED0107')
C Hardening curve
      READ(15,*)NHARD
      WRITE(16,1200)NHARD
      IF(NHARD.LT.2) CALL ERRPRT('ED0108')
C Check dimensions of IPROPS
      IF(MIPROP.LT.NIPROP)CALL ERRPRT('ED0109')
      IPROPS(3)=NHARD
C Check dimensions of RPROPS
      NRPROP=IPHARD+NHARD*2-1
      IF(NRPROP.GT.MRPROP)CALL ERRPRT('ED0110')
C Store real properties in RPROPS
      RPROPS(1)=DENSE
      RPROPS(2)=YOUNG
      RPROPS(3)=POISS
C
C Read and set hardening curve
C
      DO 10 IHARD=1,NHARD
        READ(15,*)RPROPS(IPHARD+IHARD*2-2),
     1            RPROPS(IPHARD+IHARD*2-1)
        WRITE(16,1300)RPROPS(IPHARD+IHARD*2-2),
     1                RPROPS(IPHARD+IHARD*2-1)
   10 CONTINUE
C
C Check dimension of RSTAVA, LALGVA and RALGVA
C
      IF(NRSTAV.GT.MRSTAV)CALL ERRPRT('ED0111')
      IF(NLALGV.GT.MLALGV)CALL ERRPRT('ED0112')
      IF(NRALGV.GT.MRALGV)CALL ERRPRT('ED0113')
C
      RETURN
      END
