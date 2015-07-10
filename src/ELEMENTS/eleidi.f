      subroutine eleidi
     .  (ielem      )
!***********************************************************************
! Element interface for the computation of some discontinuity values
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
!     Recover element and material type group identification numbers
!     --------------------------------------------------------------
      igrup = igrpid(ielem)
      ielidn = ieltid(igrup)
      nnode = ielprp(3,ielidn)
!      
!     Identify element class
      ielcls = ielprp(2,ielidn)
!      
!     Call discontinuity routine according to element class
!     -----------------------------------------------------
!      
      if (ielcls .eq. wsdisc) then
!       2-D isoparametric weak and strong discontinuity elements
!       (M Estrada 2014)
        call diwsd2
     .  (ielem      ,mdime      ,melem      ,mpoin      ,mtotv      ,
     .   dincr      ,lnods    ,coord(1,1,1)     ,ielprp(1,ielidn)   ,
     .   relprp(1,ielidn)     ,restv(1,ielem)   ,iestv(1,ielem)     )
      end if
!      
      return
      end
