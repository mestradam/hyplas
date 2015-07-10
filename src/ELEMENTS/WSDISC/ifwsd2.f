      subroutine ifwsd2
     .  (ielem      ,inccut     ,mdime      ,melem      ,mpoin      ,
     .   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype      ,
     .   coord1     ,dincr      ,eload      ,ielprp     ,iprops     ,
     .   lalgva     ,lnods      ,ralgva     ,relprp     ,rprops     ,
     .   rstava     ,strsg      ,thkgp      ,tdisp      ,restv      ,
     .   iestv      )
!***********************************************************************
! Compute internal force vector of all elements of class 'wsdisc'
! (Isoparametric elements enriched with weak or strong discontinuity) in
! 2D: plane stress
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   ielem   : element number (ID)
!   inccut  : increment cut flag
!   mdime   : max. element dimensions
!   melem   : max. number of elements
!   mpoin   : max. number of gauss points
!   mstre   : max. number of stress components
!   mtotv   : 
!   naxis
!   nlarge
!   ntype
!   coord1
!   dincr
!   eload
!   ielprp
!   iprops
!   lelgva
!   lnods
!   ralgva
!   relprp
!   rprops
!   rstava
!   strsg
!   thkgp
!   tdisp
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
      logical  
     .   inccut     ,lalgva
      dimension
     .   coord1(mdime,mpoin),dincr(mtotv)   ,eload(mevab)           ,
     .   ielprp(*)          ,iprops(*)      ,lalgva(mlalgv,mtotg)   ,
     .   lnods(melem,mevab) ,ralgva(mralgv,mtotg)                   ,
     .   relprp(*)          ,rprops(*)      ,rstava(mrstav,mtotg)   ,
     .   strsg(mstre,mtotg) ,thkgp(mtotg)   ,tdisp(mtotv)           ,
     .   restv(mrestv)      ,iestv(miestv)
      parameter
     .  (mgdim = 5  ,mbdim = 4  ,ndime = 2  ,ndofn = 2  )
!
!     Local variables and arrays
      logical sufail
      parameter
     .  (reg = 0    ,dis = 1    ,ela = 2    )
      dimension
     .  bmatx(mbdim,mevab)  ,cartd(ndime,mnode) ,deldis(mdofn,mnode),
     .  deriv(ndime,mnode)  ,eincr(mbdim)       ,elcod(ndime,mnode) ,
     .  fincin(3,3)         ,fincr(3,3)         ,finv(3,3)          ,
     .  gmatx(mgdim,mevab)  ,gpcod(ndime)       ,shape(mnode)       ,
     .  teldis(mdofn,mnode) ,regstr(mstre)      ,disstr(mstre)      ,
     .  elastr(mstre)       ,tilstr(mstre)      ,graphi(mstre,ndofn),
     .  vecn(mstre,ndofn)   ,eljump(ndofn)      ,residb(ndofn)      ,
     .  cartddis(ndime,mnode)
      integer
     .   etrack     ,ebif       ,eloadt
!
!     Set and read variables from arrays
!     ==================================
!
!     Set dimension of B matrix and strain increment
      if (ntype.eq.1) then
        nstre = 3
        nbdim = 3
      else if(ntype.eq.2) then
        nstre = 4
        nbdim = 3
      else if(ntype.eq.3) then
        twopi = 8.d0*atan(1.d0)
        nstre = 4
        nbdim = 4
      else
        call errprt('EI0062')
      end if
      
!     Identify element type
      ieltyp= ielprp(1)
!      
!     Num. of nodes per element
      nnode = ielprp(3)
!      
!     Num. of Gauss Points per element
      ngausp= ielprp(4)
!      
!     Num. of element degrees of freedom: ndofel or nevab
      nevab = ielprp(5)
!      
!     Element length 
      eleng = restv(1)
      if (eleng.eq.0.d0) eleng = 1.d0
!      
!     Strong discontinuities band width (paramenter k)
      elk = restv(2)
      if (elk.le.0.d0) elk = eleng
      elkinv = 1.d0 / elk
! 
      etrack = iestv(2)
      ebif = iestv(1)
      eload = iestv(4)
!      
!     Jump vector
      eljump(1) = restv(22)
      eljump(2) = restv(23)
!      
!     Gradient of phi
      graphi(1,1) = restv(20)
      graphi(1,2) = 0.d0
      graphi(2,1) = 0.d0
      graphi(2,2) = restv(21)
      graphi(3,1) = restv(21)
      graphi(3,2) = restv(20)
!      
!     Vector normal to discontinuity
      vecn(1,1) = restv(12)
      vecn(1,2) = 0.d0
      vecn(2,1) = 0.d0
      vecn(2,2) = restv(13)
      vecn(3,1) = restv(13)
      vecn(3,1) = restv(12)
!      
!     Update injection state
      einj = 0
      if (ebif.ge.1 .and. eloadt.eq.1) einj = 1
      if (gamloc.eq.1 .and. ebif.ge.2 .and. etrack.eq.1) einj = 2
      iestv(3) = einj
!      
!     Set stabilization term (tau), and injection type term (xi)
!     REFERENCE: Box 5.10 (Diaz, 2012)
      tau = taumxd
      xi = 0.d0
      if (einj.eq.1) then
        tau = tauloc
        xi = 0.d0
      else if (einj.eq.2) then
        tau = 0.d0
        xi = 1.d0
      end if
!
!     Set element arrays of current nodal coordinates, total and 
!     incremental displacements
      do inode = 1, nnode
        lnode = iabs(lnods(ielem,inode))
        nposn = (lnode-1)*ndofn
        do idofn = 1, ndofn
          nposn = nposn+1
          if (nlarge.eq.1) then
            elcod(idofn,inode) = coord1(idofn,lnode)
            teldis(idofn,inode) = -tdisp(nposn)
            deldis(idofn,inode) = -dincr(nposn)
          else
            elcod(idofn,inode) = coord1(idofn,lnode)
            deldis(idofn,inode) = dincr(nposn)
          end if
        end do
      end do
!
!     Initialize element force vector
      call rvzero(eload,nevab)
!
!=======================================================================
!          Begin loop over all Gauss points to compute stress          |
!=======================================================================
! This loop runs backwards from ngausp to 1. The purpose of this 
! behavior is to compute first the stresses at the sampling points to 
! use them in the integration process
!
      ippos = 1
      ipwei = ngausp*ndime+1
      do igausp = ngausp, 1, -1
!        
!       Set Gauss points positions and weights
        exisp = relprp(ippos-1+igausp*2-1)
        etasp = relprp(ippos-1+igausp*2)
        weigp = relprp(ipwei-1+igausp)
!        
!       set gauss point type: regular=0, reduced integration=1, elastic=2
        if (igausp.eq.ngausp) gptype = ela
        if (igausp.eq.ngausp-1) gptype = dis
        if (igausp.lt.ngausp-1) gptype = reg
!        
!       Evaluate shape functions and their derivatives (use current
!       configuration for large strains)
        call shpfun
     .  (deriv      ,etasp      ,exisp      ,0          ,ieltyp     ,
     .   ndime      ,shape      )
        call jacob2
     .  (cartd      ,deriv      ,detjac     ,elcod      ,ielem      ,
     .   ndime      ,ndime      ,nnode      )
!     
!       Save gauss point dis cartd for eleng calculation later
        if (gptype.eq.dis) then
          do idime = 1, ndime
            do inode = 1, nnode
              cartddis(idime,inode) = cartd(idime,inode)
            end do 
          end do
        end if
!        
!       Cut increment if current jacobian is not positive definite
        if (detjac.le.0.d0) then
          call errprt('WE0027')
          inccut=.true.
          goto 999
        end if
!
!       Compute basic kinematic variables needed for state update
!       =========================================================
!
        if (nlarge.eq.1) then
!          
!         Large strains: compute incremental deformation gradient
!         -------------------------------------------------------
!         Note: Large strains is not yet implemented. The use of large
!               strains will produce an error and stop the program
!          
          call errprt('ED0200')
        else
!          
!         Small strains: compute incremental infinitesimal strain: EINCR
!         --------------------------------------------------------------
!          
!         Evaluate symmetric gradient operator B
          call getbmx
     .  (bmatx      ,gpcod      ,cartd      ,ndime      ,nbdim      ,
     .   naxis      ,nnode      ,ntype      ,shape      )
!     
!         Compute the incremental infinitesimal strain
          call sdstra
     .  (bmatx      ,deldis     ,mdofn      ,nbdim      ,ndofn      ,
     .   nnode      ,ntype      ,gptype     ,xi         ,graphi     ,
     .   eincr      ,eljump     ,vecn       ,elkinv     )
!
        end if
!
!       Call material interface routine for state update calls: update
!       stress and other state variables
!       ==============================================================
!
        call matisu
     .  (detf       ,nlarge     ,ntype      ,sufail     ,
     .   thkgp(igausp)          ,eincr      ,fincr      ,iprops     ,
     .   lalgva(1,igausp)       ,ralgva(1,igausp)       ,rprops     ,
     .   rstava(1,igausp)       ,strsg(1,igausp)        )
!     
        if (sufail) then
!         State updating failed for current gauss point: break loop over
!         gauss points and exit with increment cutting flag activated
          inccut=.true.
          goto 999
        end if
!
!       Save stresses from reduced integration, elastic, and regular
!       gauss points for more clarity on 'tilstr' stress calculation 
!       later
!       REFERENCE: Eq. 5.100 (Dias, 2012)
!       ----------------------------------------------------------------
!
        if (gptype .eq. reg) then
!         Regular gauss points
          do istre = 1, nstre
            regstr(istre) = strsg(istre,igausp)
          end do
        else if (gptype .eq. dis) then
!         Reduced integration gauss point
          do istre = 1, nstre
            disstr(istre) = strsg(istre,igausp)
          end do
        else if (gptype .eq. ela) then
!         Elastic gauss point
          do istre = 1, nstre
            elastr(istre) = strsg(istre,igausp)
          end do
        end if
!
!       Add current regular gauss point contribution to the element
!       internal force vector
!       ===========================================================
!
!       Evaluate elemental volume
        dvolu=detjac*weigp
        if (ntype.eq.1) then
!       ... plane stress analysis
          dvolu=dvolu*thkgp(igausp)
        else if (ntype.eq.3) then
!       ... axisymmetric analysis
          dvolu=dvolu*twopi*gpcod(naxis)
        end if
        if (ngausp.eq.ela) then
!       ... delta_volume for elastic gp
          dvela = dvolu
        end if
        de = dvela/eleng
!
!       Elemental internal force computation (only on regular gauss
!       points)
        if (gptype .eq. reg) then
!
!         Compute 'tilstr' stress for integration
!         REFERENCE: Eq. 5.99 (Dias, 2012)
          do istre = 1, nstre
            tilstr(istre) = 
     .  (1.d0-xi)*(tau*regstr(istre) + (1.d0-tau)*disstr(istre)) + 
     .  xi*((1.d0-gamwsd)*disstr(istre) + gamwsd*elastr(istre))
!     
!           save 'tilstr' in real state variables array
            rstava(istre,igausp) = tilstr(istre)
          end do
!                                                 t
!         Add current regular gauss point  dvolu*B [sigma]
          call rtv
     .  (0          ,nbdim      ,nevab      ,nbdim      ,eload
     .  ,bmatx      ,tilstr     ,dvolu      )
!
        end if
!
      end do
!=======================================================================
!                 End of loop over gauss points                        |
!=======================================================================
!                                   t                 t
!     Compute residual: residb = [n] [disstr]*de - [n] [elastr]*de
!     (REFERENCE: Diaz, 2012: Appendix C)
!     ------------------------------------------------------------
      if (injtyp.eq.symsdinj) then
!                        t                      t
!       residb = -xi ([n] [disstr]*de - [graphi] [elastr]*dvela)
        scalar = -de*xi
        call rtv
     .  (1          ,mstre      ,ndofn      ,nstre      ,residb     ,
     .   vecn       ,disstr     ,scalar     )
        scalar = dvela*xi
        call rtv
     .  (0          ,mstre      ,ndofn      ,nstre      ,residb     ,
     .   graphi     ,elastr     ,scalar     )
      else
!                        t                 t
!       residb = -xi ([n] [disstr]*de - [n] [elastr]*de)
        scalar = -de*xi
        call rtv
     .  (1          ,mstre      ,ndofn      ,nstre      ,residb     ,
     .   vecn       ,disstr     ,scalar     )
        scalar = de*xi
        call rtv
     .  (0          ,mstre      ,ndofn      ,nstre      ,residb     ,
     .   vecn       ,elastr     ,scalar     )
      end if
      
!
!     Save residual 'residb' in 'restv' variable
      restv(31) = residb(1)
      restv(32) = residb(2)
!
  999 continue
      return
      end
