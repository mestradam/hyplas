      SUBROUTINE SFQ4
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    DERIV(MDIME,*)     ,SHAPE(*)
      DATA RP25 ,RP5  ,R1   /0.25D0,0.5D0,1.0D0/
C***********************************************************************
C COMPUTES SHAPE FUNCTIONS AND SHAPE FUNCTION DERIVATIVES FOR
C ELEMENT 'QUAD_4':
C                         4         3
C                          o-------o
C                          |       |     STANDARD ISOPARAMETRIC
C                          |       |     BI-LINEAR 4-NODE QUADRILATERAL 
C                          |       |
C                          o-------o
C                         1         2
C
C REFERENCE: Expression (4.42)
C***********************************************************************
      IF(IBOUND.EQ.0)THEN
C Shape functions and derivatives on element DOMAIN
C -------------------------------------------------
        S=EXISP
        T=ETASP
        ST=S*T
C Shape functions
        SHAPE(1)=(R1-T-S+ST)*RP25
        SHAPE(2)=(R1-T+S-ST)*RP25
        SHAPE(3)=(R1+T+S+ST)*RP25
        SHAPE(4)=(R1+T-S-ST)*RP25
C Shape function derivatives
        DERIV(1,1)=(-R1+T)*RP25
        DERIV(1,2)=(+R1-T)*RP25
        DERIV(1,3)=(+R1+T)*RP25
        DERIV(1,4)=(-R1-T)*RP25
        DERIV(2,1)=(-R1+S)*RP25
        DERIV(2,2)=(-R1-S)*RP25
        DERIV(2,3)=(+R1+S)*RP25
        DERIV(2,4)=(+R1-S)*RP25
      ELSE
C Shape function and derivatives on element BOUNDARY (1-D)
C --------------------------------------------------------
        S=EXISP
C Shape functions
        SHAPE(1)=(-S+R1)*RP5
        SHAPE(2)=(+S+R1)*RP5
C Shape functions derivatives
        DERIV(1,1)=-RP5
        DERIV(1,2)=RP5
C
      ENDIF
C
      RETURN
      END
