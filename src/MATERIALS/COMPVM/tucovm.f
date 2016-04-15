      SUBROUTINE TUCOVM
     1(   DETF       ,RSTAVA     ,THICK      ,MODE    )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( MSTRE=4 )
      DIMENSION
     1    RSTAVA(13)
!***********************************************************************
! Thickness update
! for composite material (plane strain and axisymmetric only).
!   
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   mode (in)   : Task to perform
!                   0   Initialises the relevant data.
!                   1   Assigns current values of the state variables
!                       to converged solution (when the current
!                       iteration satisfies the convergence criterion).
!                   2   Assigns the last converged solution to current
!                       state variables values (when a new iteration is
!                       required by the iterative process).
!                   3   Assigns the last converged solution to current
!                       state variables values (when increment cutting
!                       is required).
!   ntype (in)  : Analysis type
!                   1   plane stress
!                   2   plane strain
!                   3   axisymmetric
!   lalgvc (inout): Current logical algorithmic variables
!   lalgvl (inout): Last logical algorithmic variables
!   ralgvc (inout): Current real algorithmic variables
!   rstavc (inout): Current real state variables
!   rstavl (inout): Last logical state variables
!   stresc (inout): Current stress
!   stresl (inout): Last stress
!***********************************************************************
!     
! Compute determinant of total deformation gradient (including
! out-of-plane contribution). Note that, for this model, the determinant
! of the total and elastic deformation gradient coincide due to plastic
! incompressibility.
!... start by retrieving the diagonal components of the elastic
!    logarithmic strain tensor
      EE11=RSTAVA(1)
      EE22=RSTAVA(2)
      EE33=RSTAVA(4)
!... then compute determinant of total deformation gradient
      DETFT=EXP(EE11+EE22+EE33)
      IF(MODE.EQ.1)THEN
! Compute thickness stretch
        STRTC3=DETFT/DETF
! Update thickness
        THICK=THICK*STRTC3
      ENDIF
! return total deformation gradient determinant in DETF
      DETF=DETFT
!
      RETURN
      END
