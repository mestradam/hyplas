      SUBROUTINE ORCOMELA( NOUTF , NTYPE , STRES )
      IMPLICIT NONE
      INTEGER NOUTF, NTYPE, MSTRE
      REAL*8 P, EFFST, STRES
      PARAMETER(MSTRE=4)
      DIMENSION  STRES(*)
C------------------
C Output results for linear elastic composite material model
C (M. Estrada 2010)
C------------------
C
 1000 FORMAT(' S-eff = ',G12.4,' Press.= ',G12.4)
      IF(NTYPE.EQ.1)THEN
        P=(STRES(1)+STRES(2))/3.D0
        EFFST=SQRT(3.D0/2.D0*((STRES(1)-P)**2+(STRES(2)-P)**2+2.D0
     &       *STRES(3)**2+P**2))
      ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
        P=(STRES(1)+STRES(2)+STRES(4))/3.D0
        EFFST=SQRT(3.D0/2.D0*((STRES(1)-P)**2+(STRES(2)-P)**2+2.D0
     &       *STRES(3)**2+(STRES(4)-P)**2))
      ENDIF
C     Write values to output file
      WRITE(NOUTF,1000)EFFST,P
      RETURN
      END
C
