      SUBROUTINE SFT3
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    DERIV(MDIME,*)     ,SHAPE(*)
      DATA R0   ,RP5  ,R1   /0.0D0,0.5D0,1.0D0/
C***********************************************************************
C COMPUTES SHAPE FUNCTIONS AND SHAPE FUNCTION DERIVATIVES FOR
C ELEMENT 'TRI_3':
C                        3 o
C                          |\
C                          | \       
C                          |  \     
C                          |   \         STANDARD LINEAR
C                          |    \        3 NODE TRIANGLE
C                          |     \  
C                          o------o
C                         1       2
C
C REFERENCE: Expression (4.38)
C***********************************************************************
      IF(IBOUND.EQ.0)THEN
C Shape functions and derivatives on element DOMAIN
C -------------------------------------------------
        S=EXISP
        T=ETASP
C Shape functions
        SHAPE(1)=R1-S-T
        SHAPE(2)=S
        SHAPE(3)=T
C Shape function derivatives
        DERIV(1,1)=-R1
        DERIV(1,2)=+R1
        DERIV(1,3)=+R0
        DERIV(2,1)=-R1
        DERIV(2,2)=+R0
        DERIV(2,3)=+R1
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
