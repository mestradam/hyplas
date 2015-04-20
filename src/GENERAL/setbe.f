      SUBROUTINE SETBE
     1(   BE         ,NTYPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL  EXP2X
      LOGICAL   OUTOFP
      DIMENSION BE(*)
      DATA  RP5  /0.5D0/
C***********************************************************************
C COMPUTES THE ELASTIC CAUCHY-GREEN TENSOR AS A FUNCTION OF
C THE ELASTIC LOGARITHMIC STRAIN TENSOR:
C
C                      Be   :=  exp[ 2 Ee  ]
C
C REFERENCE: Box 14.3, item (ii)
C***********************************************************************
      IF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
        OUTOFP=.TRUE.
      ELSEIF(NTYPE.EQ.1)THEN
        OUTOFP=.TRUE.
      ELSE
        CALL ERRPRT('EI0024')
      ENDIF
C Convert engineering elastic strain components into physical components
      BE(3)=RP5*BE(3)
C Use isotropic tensor function to compute elastic Cauchy-Green tensor
      CALL ISO2
     1(   EXP2X      ,OUTOFP     ,BE         ,BE         )
C
      RETURN
      END
