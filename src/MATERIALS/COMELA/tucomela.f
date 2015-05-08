      SUBROUTINE TUCOMELA( DETF , RSTAVA , THICK , MODE )
      IMPLICIT NONE
      INTEGER MODE, MSTRE
      REAL*8 DETF, THICK, STRTC3, E11, E22, E33, RSTAVA
      PARAMETER(MSTRE=4)
      DIMENSION RSTAVA(MSTRE)
C----------------------------------------------------------------------
C Thickness update for the hencky elastic model under large strains
C and plane stress
C
C Reference: Section 13.3.2 of the book
C----------------------------------------------------------------------
C
C Compute determinant of total deformation gradient (including
C out-of-plane contribution).
C ...start by retrieving the diagonal components of the logarithmic
C    strain tensor
      E11=RSTAVA(1)
      E22=RSTAVA(2)
      E33=RSTAVA(4)
C ...then compute determinant of total deformation gradient
      DETF=EXP(E11+E22+E33)
      IF(MODE.EQ.1)THEN
C Compute thickness stretch
        STRTC3=EXP(E33)
C Update thickness
        THICK=THICK*STRTC3
      ENDIF
C
      RETURN
      END
