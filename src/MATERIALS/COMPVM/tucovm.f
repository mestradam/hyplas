      subroutine tucovm
     .(   detf       ,rstava     ,thick      ,mode    )
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
! Variable declaration
      implicit double precision (a-h,o-z)
      parameter( mstre=4 )
!      
! ... arguments
      dimension
     .    rstava(14)
!     
! Compute determinant of total deformation gradient (including
! out-of-plane contribution). Note that, for this model, the determinant
! of the total and elastic deformation gradient coincide due to plastic
! incompressibility.
!... start by retrieving the diagonal components of the elastic
!    logarithmic strain tensor
      ee11 = rstava(1)
      ee22 = rstava(2)
      ee33 = rstava(4)
!... then compute determinant of total deformation gradient
      detft = exp(ee11+ee22+ee33)
      if(mode.eq.1)then
! Compute thickness stretch
        strtc3 = detft/detf
! Update thickness
        thick = thick*strtc3
      endif
! return total deformation gradient determinant in DETF
      detf = detft
!
      return
      end
