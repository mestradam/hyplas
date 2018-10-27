      SUBROUTINE ISO2
     1(   FUNC       ,OUTOFP     ,X          ,Y          )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   MCOMP=4    ,NDIM=2     )
      LOGICAL OUTOFP ,REPEAT
      DIMENSION
     1    X(*)                      ,Y(*)
      DIMENSION
     1    EIGPRJ(MCOMP,NDIM)        ,EIGX(NDIM)                ,
     1    EIGY(NDIM)
C***********************************************************************
C COMPUTE THE TENSOR Y (STORED IN VECTOR FORM) AS AN ISOTROPIC
C FUNCTION OF THE TYPE:
C
C                     Y(X) = sum{ y(x_i) E_i }
C
C WHERE Y AND X ARE SYMMETRIC TENSORS, x_i AND E_i ARE, RESPECTIVELY
C THE EIGENVALUES AND EIGENPROJECTIONS OF X, AND y(.) IS A SCALAR
C FUNCTION. THIS ROUTINE IS RESTRICTED TO 2-D TENSORS WITH ONE
C POSSIBLE (TRANSVERSAL) OUT-OF-PLANE COMPONENT.
C
C REFERENCE: Section A.5
C***********************************************************************
C Performs the spectral decomposition of X
      CALL SPDEC2
     1(   EIGPRJ     ,EIGX       ,REPEAT     ,X          )
C Computes the in-plane eigenvalues of Y
      DO 10 IDIR=1,2
        EIGY(IDIR)=FUNC(EIGX(IDIR))
   10 CONTINUE
C Assembles in-plane component of Y (in vector form)
      CALL RVZERO(Y,3)
      DO 30 ICOMP=1,3
        DO 20 IDIR=1,2
          Y(ICOMP)=Y(ICOMP)+EIGY(IDIR)*EIGPRJ(ICOMP,IDIR)
   20   CONTINUE
   30 CONTINUE
C Out-of-plane component required
      IF(OUTOFP)Y(4)=FUNC(X(4))
C
      RETURN
      END
