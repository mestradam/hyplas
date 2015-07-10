      subroutine graphii
     .  (nelem      ,npoin      ,ndime      ,
     .   iestv      ,restv      ,lnods      ,smtvar     ,cartd      ,
     .   igrpid     ,ieltid     )
!***********************************************************************
! Computes the gradient of phi, which determines which sides are 
! crossed by the crack
!
! (M Estrada 2015)
! REFERENCE: Diaz, 2012: Equation 5.23
!            commatfail 'smoth_var.f90'
!***********************************************************************
!     
!     Variable declaration
!     --------------------
!        
      implicit double precision (a-h, o-z)
!     
!     Database
      include '../MAXDIM.INC'
      include '../ELEMENTS.INC'
      include '../LOCALIZA.INC'
!     
!     Arguments
      dimension
     .   iestv(miestv,nelem)        ,restv(mrestv,nelem)            ,
     .   ielprp(mieprp,mgrup)       ,lnods(nelem,mevab)             ,
     .   smtvar(npoin)              ,cartd(nelem,mtotg,ndime,mnode) ,
     .   igrpid(nelem)              ,ieltid(mgrup)
!     
!     Local variables
      dimension
     .   smtpos(mside)  ,lside(mside)   ,phi1(mside)  ,phi2(mside)  ,
     .   graphi1(ndime) ,graphi2(ndime) ,cartdt(mnode,ndime)        ,
     .   vn(ndime)      ,phi(mside)     ,graphi(ndime)
      integer
     .   einj       ,etrack     ,side(mside,2)  ,poin1    ,poin2    
!     
!-----------------------------------------------------------------------
!     
!     Loop over all elements
      do ielem = 1, nelem
!        
!       Recover element and material type group identification numbers
        igrup = igrpid(ielem)
        ielidn = ieltid(igrup)
        nnode = ielprp(3,ielidn)
        ielcls = ielprp(2,ielidn)
        ieltyp = ielprp(1,ielidn)
        ngausp = ielprp(4,ielidn)
!        
!       Control for different element types
        if (ielcls .ne. wsdisc) return
!        
!       Get injection stage
        einj = iestv(3,ielem)
!        
!       Only compute grad(phi)_i before injection, after that it does 
!       not change
        if (einj .lt. 2) then
!          
!         Get vector normal to discontinuity
          vn(1) = restv(12,ielem)
          vn(2) = restv(13,ielem)
!          
!         Set sides of the element
          if (ieltyp .eq. q4wsd) then
            nside = 4
            side(1,1) = 1; side(1,2) = 2
            side(2,1) = 2; side(2,2) = 3
            side(3,1) = 3; side(3,2) = 4
            side(4,1) = 4; side(4,2) = 1
          end if
!          
!         Number of crossed sides
          ncside = 0
!          
!         Check if sides are crossed by the discontinuity
!         -----------------------------------------------
          do iside = 1, nside
!            
!           Global point of the side start and end points
            ipoin1 = iabs(lnods(ielem,side(iside,1)))
            ipoin2 = iabs(lnods(ielem,side(iside,2)))
!            
!           Verify if smoothed variable changes sign
            sign1 = dsign(1.d0,smtvar(poin1))
            sign2 = dsign(1.d0,smtvar(poin2))
            smtpos(iside) = dabs(smtvar(poin1))
            if (sign1 .ne. sign2) then
              ncside = ncside + 1
              lside(ncside) = iside
            end if
          end do
!          
!         Set phi function values if two sides are crossed
!         ------------------------------------------------
          if (ncside .eq. 2) then
            etrack = 1
            if (lside(1) .eq. 1 .and. lside(2) .eq. 2) then
              phi1(1) = 0 ; phi2(1) = 1
              phi1(2) = 1 ; phi2(2) = 0
              phi1(3) = 0 ; phi2(3) = 1
              phi1(4) = 0 ; phi2(4) = 1
            else if (lside(1) .eq. 2 .and. lside(2) .eq. 3) then
              phi1(1) = 0 ; phi2(1) = 1
              phi1(2) = 0 ; phi2(2) = 1
              phi1(3) = 1 ; phi2(3) = 0
              phi1(4) = 0 ; phi2(4) = 1
            else if (lside(1) .eq. 3 .and. lside(2) .eq. 4) then
              phi1(1) = 0 ; phi2(1) = 1
              phi1(2) = 0 ; phi2(2) = 1
              phi1(3) = 0 ; phi2(3) = 1
              phi1(4) = 1 ; phi2(4) = 0
            else if (lside(1) .eq. 1 .and. lside(2) .eq. 4) then
              phi1(1) = 1 ; phi2(1) = 0
              phi1(2) = 0 ; phi2(2) = 1
              phi1(3) = 0 ; phi2(3) = 1
              phi1(4) = 0 ; phi2(4) = 1
            else if (lside(1) .eq. 1 .and. lside(2) .eq. 3) then
              phi1(1) = 0 ; phi2(1) = 1
              phi1(2) = 1 ; phi2(2) = 0
              phi1(3) = 1 ; phi2(3) = 0
              phi1(4) = 0 ; phi2(4) = 1
            else if (lside(1) .eq. 2 .and. lside(2) .eq. 4) then
              phi1(1) = 0 ; phi2(1) = 1
              phi1(2) = 0 ; phi2(2) = 1
              phi1(3) = 1 ; phi2(3) = 0
              phi1(4) = 1 ; phi2(4) = 0
            end if
!            
!           Gradient of phi
            igausp = ngausp
            graphi1=multmt(cartd(ielem,igausp,:,:),phi1,ndime,nnode,1)
            graphi2=multmt(cartd(ielem,igausp,:,:),phi2,ndime,nnode,1)
!            
!           Dot product to choose function
            elengi = 0.d0
            do idime = 1, ndime
              elengi = elengi + graphi(idime)*vn(idime)
            end do
            if (elengi .gt. 0) then
!              do iside = 1, nside
!                phi(iside) = phi1(iside)
!              end do
              do idime = 1, ndime
                graphi(idime) = graphi1(idime)
              end do
            else
!              do iside = 1, nside
!                phi(iside) = phi2(iside)
!              end do
              do idime = 1, ndime
                graphi(idime) = graphi2(idime)
              end do
            end if
!            
!           Save gradient of phi into 'restv' variable
            restv(20,ielem) = graphi(1)
            restv(21,ielem) = graphi(2)
!            
!           End conditional of crossed sides: ncside == 2
          end if
!          
!         Save 'etrack' flag to 'iestv' variable
          iestv(2,ielem) = etrack
!          
!         End conditional of injection state: einj < 2
        end if
!        
!       End loop over all elements
      end do
!      
      return
      end