      subroutine diwsd2
     .  (ielem      ,mdime      ,melem      ,mpoin      ,mtotv      ,
     .   dincr      ,lnods      ,coord1     ,ielprp     ,relprp     ,
     .   restv      ,iestv      )
!***********************************************************************
! Computation of some discontinuity values: 
!     - Selection on vector n, normal to discontinuity
!     - Computation of element length
!     - Computation of grad(u_x), and grad(u_y)
!     - Computation of discontinuity bandwidth
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
      include '../../ELEMENTS.INC'
      include '../../MATERIAL.INC'
      include '../../LOCALIZA.INC'
!
!     Arguments
      dimension
     .   coord1(mdime,mpoin),dincr(mtotv)   ,ielprp(*)  ,relprp(*)  ,
     .   lnods(melem,mevab) ,restv(mrestv)  ,iestv(miestv)
      parameter
     .  (ndime = 2  ,ndofn = 2  )
!
!     Local variables and arrays
      dimension
     .  deldis(mdofn,mnode) ,deriv(ndime,mnode) ,elcod(ndime,mnode) ,
     .  shape(mnode)        ,cartddis(ndime,mnode)
      integer
     .  ebif, einj
!
!-----------------------------------------------------------------------
!     
!     Set and read variables from arrays
!     ==================================
!      
!     Num. of nodes per element
      nnode = ielprp(3)
      ngausp = ielprp(4)
!      
!     Identify element type
      ieltyp= ielprp(1)
!      
!     Injection state
      einj = iestv(3)
!      
!     Bifurcation state
      ebif = iestv(1)
!
!     Set element arrays of current nodal coordinates, total and 
!     incremental displacements
      do inode = 1, nnode
        lnode = iabs(lnods(ielem,inode))
        nposn = (lnode-1)*ndofn
        do idofn = 1, ndofn
          nposn = nposn+1
          elcod(idofn,inode) = coord1(idofn,lnode)
          deldis(idofn,inode) = dincr(nposn)
        end do
      end do
!      
      ippos = 1
      ipwei = ngausp*ndime+1
      igausp = ngausp-1
!        
!     Set Gauss points positions and weights
      exisp = relprp(ippos-1+igausp*2-1)
      etasp = relprp(ippos-1+igausp*2)
      weigp = relprp(ipwei-1+igausp)
!        
!     Evaluate shape functions and their derivatives (use current
!     configuration for large strains)
      call shpfun
     .  (deriv      ,etasp      ,exisp      ,0          ,ieltyp     ,
     .   ndime      ,shape      )
      call jacob2
     .  (cartddis   ,deriv      ,detjac     ,elcod      ,ielem      ,
     .   ndime      ,ndime      ,nnode      )
!      
!     Some computations on the discontinuity if second injection is on
!     ----------------------------------------------------------------
      if (einj.lt.2) then
!     
!       Select vector normal to discontinuity
        call nselec
     .  (restv      ,ndime      )
!
!       Compute element length 'eleng'
        call celeng
     .  (nnode      ,ndime      ,ndofn      ,elcod      ,cartddis   ,
     .   restv      )
!     
!       Compute grad(u_x) and grad(u_y) for next step normal selection
        call grauxy
     .  (nnode      ,ndime      ,deldis     ,cartddis   ,restv      )
      end if
!      
!     Band width computation
!     ----------------------
      eleng = restv(1)
      elk = eleng
      if (injtyp.eq.symsdinj .or. injtyp.eq.sdinj) then
        qinj = restv(5)
        qsda = restv(6)
        qq = restv(7)
!      
!       Band width for strong discontinuities
        ekinf = parak*eleng
!      
!       Assign band width depending on injection stage
        if (ebif.eq.2) then
          elk = ekinf - (ekinf-eleng) * dexp(5.d0*(qq-qinj)/(qq-qsda))
        else if (ebif.ge.3) then
          elk = ekinf
        end if
      end if
!      
!     Save element band width in element real state variables array
      restv(2) = elk
!      
      return
      end
