      DOUBLE PRECISION FUNCTION DDLGD2(X)
C
      DOUBLE PRECISION X
      DOUBLE PRECISION RP5
      DATA   RP5 /0.5D0/
C***********************************************************************
C DERIVATIVE OF THE SCALAR FUNCTION 'DLGD2' THAT RELATES PRINCIPAL
C LOGARITHMIC STRECTHES AND EIGENVALUES OF THE CAUCHY-GREEN TENSOR
C***********************************************************************
      DDLGD2=RP5/X
C
      RETURN
      END
