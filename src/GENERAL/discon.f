      subroutine discon
!***********************************************************************
! Some computations on the discontinuity
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!***********************************************************************
!
!     Variable declaration
!     --------------------
      implicit double precision (a-h, o-z)
!
!     Database
      include '../MAXDIM.INC'
      include '../ELEMENTS.INC'
      include '../MATERIAL.INC'
      include '../GLBDBASE.INC'
      include '../LOCALIZA.INC'
!   
!-----------------------------------------------------------------------
!     
!     Begin loop over all elements
!     ============================
      do ielem = 1, nelem
!
!       Call element interface for discontinuity computations
!       -----------------------------------------------------
        call eleidi
     .  (ielem      )
!     
      end do
!      
      return
      end