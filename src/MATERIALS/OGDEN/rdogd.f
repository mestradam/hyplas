      SUBROUTINE RDOGD
     1(   IPROPS     ,MIPROP     ,MRPROP     ,MRSTAV     ,RPROPS     ,
     2    UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UNSYM
      PARAMETER( IPOGDC=2  ,NIPROP=3  ,NRSTAV=4 )
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)
C***********************************************************************
C READS AND ECHOES MATERIAL PROPERTIES FOR OGDEN TYPE HYPERELASTIC
C MATERIAL MODEL
C
C REFERENCE: Section 13.2.2
C***********************************************************************
 1000 FORMAT(' Ogden type hyperelastic material'/)
 1010 FORMAT(
     1' Mass density ...................................... =',G15.6)
 1020 FORMAT(/
     1' Number of terms in Ogden''s strain-energy function.. =',I3//
     2'            mu                  alpha'/)
 1030 FORMAT(2(5X,G15.6))
 1040 FORMAT(/
     1' Bulk modulus ...................................... =',G15.6/)
C
C Set unsymmetric tangent stiffness flag
      UNSYM=.FALSE.
C Read and echo some of the real properties
      WRITE(16,1000)
      READ(15,*)DENSE
      WRITE(16,1010)DENSE
C Ogden's constants
      READ(15,*)NOGTRM
      WRITE(16,1020)NOGTRM
      IF(NOGTRM.LT.1)CALL ERRPRT('ED0067')
C Check dimension of IPROPS
      IF(MIPROP.LT.NIPROP)CALL ERRPRT('ED0195')
      IPROPS(3)=NOGTRM
C Check dimension of RPROPS
      NRPROP=IPOGDC+NOGTRM*2
      IF(NRPROP.GT.MRPROP)CALL ERRPRT('ED0196')
C Store real properties in RPROPS
      RPROPS(1)=DENSE
      DO 10 I=1,NOGTRM
        READ(15,*)RPROPS(IPOGDC+I*2-2),RPROPS(IPOGDC+I*2-1)
        WRITE(16,1030)RPROPS(IPOGDC+I*2-2),RPROPS(IPOGDC+I*2-1)
   10 CONTINUE
C Bulk modulus
      READ(15,*)BULK
      RPROPS(IPOGDC+NOGTRM*2)=BULK
      WRITE(16,1040)BULK
C Check dimension of RSTAVA
      IF(NRSTAV.GT.MRSTAV)CALL ERRPRT('ED0197')
C
      RETURN
      END
