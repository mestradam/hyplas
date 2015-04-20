      DOUBLE PRECISION FUNCTION EXP2X(X)
C
      DOUBLE PRECISION X
      DOUBLE PRECISION R2
      DATA   R2  /2.0D0/
C***********************************************************************
C SCALAR FUNCTION THAT RELATES EIGENVALUES OF THE CAUCHY-GREEN
C TENSOR TO THE PRINCIPAL LOGARITHMIC STRECTHES
C
C REFERENCE: Section 3.1.7
C***********************************************************************
      EXP2X=DEXP(R2*X)
C
      RETURN
      END
