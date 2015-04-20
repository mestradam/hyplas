      SUBROUTINE LOGSTR
     1(   B          ,E          ,NTYPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL  DLGD2
      LOGICAL   OUTOFP
      DIMENSION
     1    B(*)               ,E(*)
      DATA  R2   /2.0D0/
C***********************************************************************
C COMPUTES THE LOGARITHMIC STRAIN TENSOR:
C
C                       E :=  1/2 ln[ B ]
C
C REFERENCE: Box 14.3, item (ii)
C***********************************************************************
      IF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
        OUTOFP=.TRUE.
      ELSEIF(NTYPE.EQ.1)THEN
        OUTOFP=.FALSE.
      ELSE
        CALL ERRPRT('EI0022')
      ENDIF
C
C Use isotropic tensor function to compute the logarithmic (physical)
C strain components
C
      CALL ISO2
     1(   DLGD2      ,OUTOFP     ,B          ,E          )
C
C Convert physical components into engineering strain components 
C
      E(3)=R2*E(3)
C
      RETURN
      END
