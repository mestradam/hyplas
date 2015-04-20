      SUBROUTINE TUEL
     1(   DETF       ,RSTAVA     ,THICK      ,MODE    )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( MSTRE=4 )
      DIMENSION
     1    RSTAVA(MSTRE)
C***********************************************************************
C THICKNESS UPDATE FOR THE HENCKY ELASTIC MODEL MODEL UNDER LARGE
C STRAINS AND PLANE STRESS
C
C REFERENCE: Section 13.3.2
C***********************************************************************
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
