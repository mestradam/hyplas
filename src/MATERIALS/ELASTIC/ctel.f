      SUBROUTINE CTEL
     1(   DMATX      ,NTYPE      ,RPROPS     )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MSTRE=4)
      DIMENSION
     1    DMATX(MSTRE,MSTRE),RPROPS(*)
      DIMENSION
     1    FOID(MSTRE,MSTRE)  ,SOID(MSTRE)
      DATA
     1    FOID(1,1),FOID(1,2),FOID(1,3),FOID(1,4)/
     2    1.0D0    ,0.0D0    ,0.0D0    ,0.0D0    /
     3    FOID(2,1),FOID(2,2),FOID(2,3),FOID(2,4)/
     4    0.0D0    ,1.0D0    ,0.0D0    ,0.0D0    /
     5    FOID(3,1),FOID(3,2),FOID(3,3),FOID(3,4)/
     6    0.0D0    ,0.0D0    ,0.5D0    ,0.0D0    /
     7    FOID(4,1),FOID(4,2),FOID(4,3),FOID(4,4)/
     8    0.0D0    ,0.0D0    ,0.0D0    ,1.0D0    /
      DATA
     1    SOID(1)  ,SOID(2)  ,SOID(3)  ,SOID(4)  /
     2    1.0D0    ,1.0D0    ,0.0D0    ,1.0D0    /
      DATA
     1    R1   ,R2   ,R3   ,R4   /
     2    1.0D0,2.0D0,3.0D0,4.0D0/
C***********************************************************************
C COMPUTATION OF THE TANGENT MODULUS (ELASTICITY MATRIX) FOR THE LINEAR
C ELASTIC MATERIAL MODEL
C
C REFERENCE: Expression (4.44)
C***********************************************************************
C
C Set shear and bulk modulus
C --------------------------
C
      GMODU=RPROPS(2)
      BULK=RPROPS(3)
C
      R1D3=R1/R3
      R2G=R2*GMODU
      FACTOR=BULK-R2G*R1D3
C
C Compute elasticity matrix
C -------------------------
C
      IF(NTYPE.EQ.1)THEN
C plane stress
        NSTRE=3
        R4GD3=R4*GMODU*R1D3
        FACTOR=(BULK-R2G*R1D3)*(R2G/(BULK+R4GD3))
      ELSEIF(NTYPE.EQ.2)THEN
C plane strain
        NSTRE=3
        FACTOR=BULK-R2G*R1D3
      ELSEIF(NTYPE.EQ.3)THEN
C axisymmetric
        NSTRE=4
        FACTOR=BULK-R2G*R1D3
      ELSE
C stops program if other stress state
        CALL ERRPRT('EI0019')
      ENDIF
C
C Assemble matrix
C
      DO 20 I=1,NSTRE
        DO 10 J=I,NSTRE
          DMATX(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   10   CONTINUE
   20 CONTINUE
C lower triangle
      DO 40 J=1,NSTRE-1
        DO 30 I=J+1,NSTRE
          DMATX(I,J)=DMATX(J,I)
   30   CONTINUE
   40 CONTINUE
C
      RETURN
      END
