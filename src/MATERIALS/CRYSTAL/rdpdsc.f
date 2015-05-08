      SUBROUTINE RDPDSC
     1(   IPROPS     ,MIPROP     ,MLALGV     ,MRALGV     ,MRPROP     ,
     2    MRSTAV     ,NLARGE     ,NTYPE      ,RPROPS     ,UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   IPHARD=6   ,NIPROP=3   ,NLALGV=6   ,NRALGV=4   ,NRSTAV=5   )
C Arguments
      LOGICAL UNSYM
      DIMENSION
     1    IPROPS(MIPROP)     ,RPROPS(MRPROP)
C Local data
      DATA R1   ,R180   /
     1     1.0D0,180.0D0/
C***********************************************************************
C READ AND ECHO MATERIAL PROPERTIES FOR THE LARGE STRAIN PLANAR
C DOUBLE-SLIP SINGLE CRYSTAL ELASTO-PLASTIC MODEL WITH NON-LINEAR
C (PIECEWISE LINEAR) ISOTROPIC TAYLOR HARDENING
C***********************************************************************
 1000 FORMAT(' Large strain planar double-slip SINGLE CRYSTAL'/)
 1100 FORMAT(
     1' Mass density ...................................... =',G15.6/
     2' Shear modulus ..................................... =',G15.6/
     3' Bulk modulus ...................................... =',G15.6/
     4' Initial orientation of FIRST SLIP SYSTEM relative'          /
     5' to X-axis (degrees, counterclockwise-positive) .... =',G15.6/
     6' Initial orientation of SECOND SLIP SYSTEM relative'         /
     7' to first syst. (degrees, counterclockwise-positive) =',G15.6)
 1200 FORMAT(/
     1' Number of points on Taylor hardening curve ........ =',I3//
     2'       Accum. slip    Resolved Schmid yield stress '/)
 1300 FORMAT(2(5X,G15.6))
C
C Set unsymmetric tangent stiffness flag
      UNSYM=.TRUE.
C Check that stress state type and large strain flags are compatible
C with the present model
      IF(NTYPE.NE.2)CALL ERRPRT('ED0154')
      IF(NLARGE.NE.1)CALL ERRPRT('ED0155')
C
C Read and echo some of the real properties
      WRITE(16,1000)
      READ(15,*)DENSE
      READ(15,*)GMODU,BULK,THETA,BETA
      WRITE(16,1100)DENSE,GMODU,BULK,THETA,BETA
C number of points on hardening curve
      READ(15,*)NHARD
      WRITE(16,1200)NHARD
      IF(NHARD.LT.2) CALL ERRPRT('ED0148')
C check dimensions of IPROPS
      IF(NIPROP.GT.MIPROP)CALL ERRPRT('ED0149')
      IPROPS(3)=NHARD
C check dimensions of RPROPS
      NRPROP=IPHARD+NHARD*2-1
      IF(NRPROP.GT.MRPROP)CALL ERRPRT('ED0150')
C convert angles into radians
      RADEG=ACOS(-R1)/R180
      THETA=RADEG*THETA
      BETA=RADEG*BETA
      RPROPS(1)=DENSE
      RPROPS(2)=GMODU
      RPROPS(3)=BULK
      RPROPS(4)=THETA
      RPROPS(5)=BETA
C Read and set hardening curve
      DO 10 IHARD=1,NHARD
        READ(15,*)RPROPS(IPHARD+IHARD*2-2),
     1            RPROPS(IPHARD+IHARD*2-1)
        WRITE(16,1300)RPROPS(IPHARD+IHARD*2-2),
     1                RPROPS(IPHARD+IHARD*2-1)
   10 CONTINUE
C Check dimension of RSTAVA, RALGVA and LALGVA
      IF(NRSTAV.GT.MRSTAV)CALL ERRPRT('ED0151')
      IF(NRALGV.GT.MRALGV)CALL ERRPRT('ED0152')
      IF(NLALGV.GT.MLALGV)CALL ERRPRT('ED0153')
C
      RETURN
      END
