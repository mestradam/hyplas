      DOUBLE PRECISION FUNCTION MULTMT
     &( MAT1        ,MAT2       ,DIM1       ,DIM2       ,DIM3       )
C***********************************************************************
C Computes the product of two matrices: 
C     MULTMT = MAT1 * MAT2
C
C (M. Estrada, 2015)
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION
     &  MAT1        ,MAT2       ,MAT3
      DIMENSION
     &  MAT1(DIM1,DIM2)   ,MAT2(DIM2,DIM3)    ,MULTMT(DIM1,DIM3)
      INTEGER
     &  DIM1        ,DIM2       ,DIM3         ,IDIM1      ,IDIM2    ,
     &  IDIM3
C
      DO 10, IDIM1 = 1, DIM1
        DO 30, IDIM3 = 1, DIM3
          MULTMT(IDIM1,IDIM3) = 0.D0
          DO 20, IDIM2 = 1, DIM2
            MULTMT(IDIM1,IDIM3) = MULTMT(IDIM1,IDIM3) + 
     &                            MAT1(IDIM1,IDIM2) * MAT2(IDIM2,IDIM3)
  20      CONTINUE
  30    CONTINUE
  10  CONTINUE
C
      RETURN
      END