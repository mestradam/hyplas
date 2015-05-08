      SUBROUTINE RDEL
     1(   MRPROP     ,MRSTAV     ,RPROPS     ,UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UNSYM
      PARAMETER( NRSTAV=4 )
      DIMENSION
     1    RPROPS(*)
      DATA R0   ,RP5  ,R1   ,R2   ,R3   /
     1     0.0D0,0.5D0,1.0D0,2.0D0,3.0D0/
C***********************************************************************
C READ AND ECHO MATERIAL PROPERTIES FOR LINEAR ELASTIC MATERIAL MODEL
C***********************************************************************
 1000 FORMAT(' LINEAR ELASTIC material (HENCKY material in large',
     1       ' strains)'/)
 1010 FORMAT(
     1' Mass density ...................................... =',G15.6/
     2' Young''s modulus ................................... =',G15.6/
     3' Poisson''s ratio ................................... =',G15.6)
C
C Set unsymmetric tangent stiffness flag
C
      UNSYM=.FALSE.
C
C Read and echo material properties
C
      WRITE(16,1000)
      READ(15,*)DENSE
      READ(15,*)YOUNG,POISS
      WRITE(16,1010)DENSE,YOUNG,POISS
      IF(YOUNG.LT.R0)CALL ERRPRT('ED0077')
      IF(POISS.LE.-R1.AND.POISS.GE.RP5)CALL ERRPRT('ED0078')
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
C
C Set vector of real material properties
C
      NRPROP=4
      IF(NRPROP.GT.MRPROP)CALL ERRPRT('ED0181')
      RPROPS(1)=DENSE
      RPROPS(2)=GMODU
      RPROPS(3)=BULK
      RPROPS(4)=YOUNG
C
C Check dimensioning of RSTAVA
      IF(NRSTAV.GT.MRSTAV)CALL ERRPRT('ED0182')
C
      RETURN
      END
