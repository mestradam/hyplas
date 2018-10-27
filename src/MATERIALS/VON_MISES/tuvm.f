      SUBROUTINE TUVM
     1(   DETF       ,RSTAVA     ,THICK      ,MODE    )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( MSTRE=4 )
      DIMENSION
     1    RSTAVA(MSTRE+1)
C***********************************************************************
C THICKNESS UPDATE FOR THE VON MISES ELASTO-PLASTIC MODEL UNDER LARGE
C STRAINS AND PLANE STRESS
C
C REFERENCE: Expressions (14.113-115)
C***********************************************************************
C Compute determinant of total deformation gradient (including
C out-of-plane contribution). Note that, for this model, the determinant
C of the total and elastic deformation gradient coincide due to plastic
C incompressibility.
C... start by retrieving the diagonal components of the elastic
C    logarithmic strain tensor
      EE11=RSTAVA(1)
      EE22=RSTAVA(2)
      EE33=RSTAVA(4)
C... then compute determinant of total deformation gradient
      DETFT=EXP(EE11+EE22+EE33)
      IF(MODE.EQ.1)THEN
C Compute thickness stretch
        STRTC3=DETFT/DETF
C Update thickness
        THICK=THICK*STRTC3
      ENDIF
C return total deformation gradient determinant in DETF
      DETF=DETFT
C
      RETURN
      END
