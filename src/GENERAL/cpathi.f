      subroutine cpathi
!***********************************************************************
! Interface for the computation of the crack path
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
!     Local variables
      dimension
     .   smtvar(npoin)  ,cartd(nelem,mtotg,ndime,mnode)
!        
!     Call crack path routine according to crack parth method
!     -------------------------------------------------------
!      
      if (cpmeth.eq.dsmooth) then
!        
!       Double smooth procedure
        call cpdsmo
     .  (nelem      ,npoin      ,ndime      ,mtotv      ,mrstav     ,
     .   ielprp     ,relprp     ,iestv      ,restv      ,lnods      ,
     .   igrpid     ,ieltid     ,coord      ,tdisp      ,dincr      ,
     .   thkgp(:,:,1)           ,rstava(:,:,:,2)        ,smtvar     ,
     .   cartd      )
!     
!       Compute gradient of phi
        call graphii
     .  (nelem      ,npoin      ,ndime      ,
     .   iestv      ,restv      ,lnods      ,smtvar     ,cartd      ,
     .   igrpid     ,ieltid     )
      end if
!      
      return
      end
