      SUBROUTINE SFQ4SD
     &( DERIV       ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     &  SHAPE       )
C***********************************************************************
C Computes shape functions and shape function derivatives for
C element 'QUAD_4':
C                         4         3
C                          O-------O
C                          |       |     standard isoparametric
C                          |       |     bi-linear 4-node quadrilateral 
C                          |       |
C                          O-------O
C                         1         2
C
C REFERENCE: Expression (4.42)
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     &  DERIV(MDIME,*)     ,SHAPE(*)
C
C
C Compute shape functions and derivatives for domain or boundary
      IF(IBOUND.EQ.0)THEN
C Shape functions and derivatives on element domain
C -------------------------------------------------
        S=EXISP
        T=ETASP
        ST=S*T
C Shape functions
        SHAPE(1)=(1.0D0-T-S+ST)*0.25D0
        SHAPE(2)=(1.0D0-T+S-ST)*0.25D0
        SHAPE(3)=(1.0D0+T+S+ST)*0.25D0
        SHAPE(4)=(1.0D0+T-S-ST)*0.25D0
C Shape function derivatives
        DERIV(1,1)=(-1.0D0+T)*0.25D0
        DERIV(1,2)=(+1.0D0-T)*0.25D0
        DERIV(1,3)=(+1.0D0+T)*0.25D0
        DERIV(1,4)=(-1.0D0-T)*0.25D0
        DERIV(2,1)=(-1.0D0+S)*0.25D0
        DERIV(2,2)=(-1.0D0-S)*0.25D0
        DERIV(2,3)=(+1.0D0+S)*0.25D0
        DERIV(2,4)=(+1.0D0-S)*0.25D0
      ELSE
C Shape function and derivatives on element boundary (1-D)
C --------------------------------------------------------
        S=EXISP
C Shape functions
        SHAPE(1)=(-S+1.0D0)*0.5D0
        SHAPE(2)=(+S+1.0D0)*0.5D0
C Shape functions derivatives
        DERIV(1,1)=-0.5D0
        DERIV(1,2)=0.5D0
C
      ENDIF
C
      RETURN
      END
