      SUBROUTINE RDVM
     1(   IPROPS     ,MIPROP     ,MLALGV     ,MRPROP     ,MRSTAV     ,
     2    RPROPS     ,UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UNSYM
      PARAMETER( IPHARD=4  ,NLALGV=2  ,NRSTAV=5 )
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)
      DATA R0   /0.0D0/
C***********************************************************************
C READ AND ECHO MATERIAL PROPERTIES FOR VON MISES TYPE ELASTO-PLASTIC
C MATERIAL WITH NON-LINEAR (PIECEWISE LINEAR) ISOTROPIC HARDENING
C
C REFERENCE: Section 7.3.5
C***********************************************************************
 1000 FORMAT(' Elasto-plastic with VON MISES yield criterion'/)
 1100 FORMAT(
     1' Mass density ...................................... =',G15.6/
     2' Young''s modulus ................................... =',G15.6/
     3' Poisson''s ratio ................................... =',G15.6)
 1200 FORMAT(/
     1' Number of points on hardening curve ............... =',I3//
     2'           Epstn        uniaxial yield stress '/)
 1300 FORMAT(2(5X,G15.6))
C
C Set unsymmetric tangent stiffness flag
      UNSYM=.FALSE.
C
C Read and echo some of the real properties
      WRITE(16,1000)
      READ(15,*)DENSE
      READ(15,*)YOUNG,POISS
      WRITE(16,1100)DENSE,YOUNG,POISS
      IF(YOUNG.LE.R0)CALL ERRPRT('ED0100')
C number of points on hardening curve
      READ(15,*)NHARD
      WRITE(16,1200)NHARD
      IF(NHARD.LT.2) CALL ERRPRT('ED0101')
C check dimensions of IPROPS
      IF(MIPROP.LT.3)CALL ERRPRT('ED0102')
      IPROPS(3)=NHARD
C check dimensions of RPROPS
      NRPROP=IPHARD+NHARD*2-1
      IF(NRPROP.GT.MRPROP)CALL ERRPRT('ED0103')
C
      RPROPS(1)=DENSE
      RPROPS(2)=YOUNG
      RPROPS(3)=POISS
C Read and set hardening curve
      DO 10 IHARD=1,NHARD
        READ(15,*)RPROPS(IPHARD+IHARD*2-2),
     1            RPROPS(IPHARD+IHARD*2-1)
        WRITE(16,1300)RPROPS(IPHARD+IHARD*2-2),
     1                RPROPS(IPHARD+IHARD*2-1)
   10 CONTINUE
C Check dimension of RSTAVA and LALGVA
      IF(NRSTAV.GT.MRSTAV)CALL ERRPRT('ED0104')
      IF(NLALGV.GT.MLALGV)CALL ERRPRT('ED0105')
C
      RETURN
      END
