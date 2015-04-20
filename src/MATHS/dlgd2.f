      DOUBLE PRECISION FUNCTION DLGD2(X)
C
      DOUBLE PRECISION X
      DOUBLE PRECISION RP5
      DATA   RP5 /0.5D0/
C***********************************************************************
C SCALAR FUNCTION THAT RELATES PRINCIPAL LOGARITHMIC STRECTHES AND
C EIGENVALUES OF THE CAUCHY-GREEN TENSOR
C
C REFERENCE: Section 3.1.7
C***********************************************************************
      DLGD2=RP5*DLOG(X)
C
      RETURN
      END
