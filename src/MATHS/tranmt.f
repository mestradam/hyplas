      DOUBLE PRECISION FUNCTION TRANMT
     &( MAT         ,DIM1       ,DIM2       )
C***********************************************************************
C Computes the transpose of a matrix: 
C     TRANMT = MAT^T
C
C (M. Estrada, 2015)
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION
     &  MAT
      DIMENSION
     &  MAT(DIM1,DIM2)    ,TRANMT(DIM2,DIM1)
      INTEGER
     &  DIM1        ,DIM2       ,IDIM1      ,IDIM2
C
      DO 10, IDIM1 = 1, DIM1
        DO 20, IDIM2 = 1, DIM2
          TRANMT(IDIM2,IDIM1) = MAT(IDIM1,IDIM2)
  20    CONTINUE
  10  CONTINUE
C
      RETURN
      END