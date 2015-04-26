      subroutine ifsd2
     &  (ielem      ,inccut     ,mdime      ,melem      ,mpoin      ,
     &   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype      ,
     &   coord1     ,dincr      ,eload      ,ielprp     ,iprops     ,
     &   lalgva     ,lnods      ,ralgva     ,relprp     ,rprops     ,
     &   rstava     ,strsg      ,thkgp      ,tdisp      )
c***********************************************************************
c Compute internal force vector of all elements of class 'strdis' 
c (Isoparametric elements enriched with weak or strong discontinuity) in
c 2D: plane stress
c
c (M. Estrada, 2014)
c-----------------------------------------------------------------------
c Subroutine arguments:
c
c   ielem   : element number (ID)
c   inccut  : increment cut flag
c   mdime   : max. element dimensions
c   melem   : max. number of elements
c   mpoin   : max. number of gauss points
c   mstre   : max. number of stress components
c   mtotv   : 
c   naxis
c   nlarge
c   ntype
c   coord1
c   dincr
c   eload
c   ielprp
c   iprops
c   lelgva
c   lnods
c   ralgva
c   relprp
c   rprops
c   rstava
c   strsg
c   thkgp
c   tdisp
c***********************************************************************
c
c Variable declaration
c --------------------
      implicit double precision (a-h, o-z)
c
c Database
      include '../../ELEMENTS.INC'
      include '../../MATERIAL.INC'
      include '../../LOCALIZA.INC'
c
c Arguments
      logical  
     &   inccut     ,lalgva
      dimension
     &   coord1(mdime,mpoin),dincr(mtotv)   ,eload(mevab)           ,
     &   ielprp(*)          ,iprops(*)      ,lalgva(mlalgv,mtotg)   ,
     &   lnods(melem,mevab) ,ralgva(mralgv,mtotg)                   ,
     &   relprp(*)          ,rprops(*)      ,rstava(mrstav,mtotg)   ,
     &   strsg(mstre,mtotg) ,thkgp(mtotg)   ,tdisp(mtotv)
      parameter
     &  (mgdim = 5  ,mbdim = 4  ,ndime = 2  ,ndofn = 2  )
c
c Local variables and arrays
      logical sufail
      dimension
     &   bmatx(mbdim,mevab) ,cartd(ndime,mnode) ,deldis(mdofn,mnode),
     &   deriv(ndime,mnode) ,eincr(mbdim)       ,elcod(ndime,mnode) ,
     &   fincin(3,3)        ,fincr(3,3)         ,finv(3,3)          ,
     &   gmatx(mgdim,mevab) ,gpcod(ndime)       ,shape(mnode)       ,
     &   teldis(mdofn,mnode),regstr(mstre)      ,disstr(mstre)      ,
     &   elastr(mstre)      ,tilstr(mstre)      ,graphi(3,2)        ,
     &   vecn(3,2)
      integer
     &   etrack     ,ebif       ,eloadt
      parameter
     &  (reg = 0    ,dis = 1    ,ela = 2    )
c
c Set and read variables from arrays
c ==================================
c
c Set dimension of B matrix and strain increment
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
c Identify element type
      ieltyp= ielprp(1)
c Num. of nodes per element
      nnode = ielprp(3)
c Num. of Gauss Points per element
      ngausp= ielprp(4)
c Num. of element degrees of freedom
      nevab = ielprp(5)
c Element length 
      eleng = relprp(21)
      if (eleng.eq.0.d0) eleng = 1.d0
c Strong discontinuities band width (paramenter k)
      elk = relprp(22)
      if (elk.le.0.d0) elk = eleng
      elkinv = 1.d0 / elk
c 
      etrack = ielprp(18)
      ebif = ielprp(17)
      eload = ielprp(20)
c Gradient of phi
      graphi(1,1) = relprp(39)
      graphi(1,2) = 0.d0
      graphi(2,1) = 0.d0
      graphi(2,2) = relprp(40)
      graphi(3,1) = relprp(40)
      graphi(3,2) = relprp(39)
c Vector normal to discontinuity
      vecn(1,1) = relprp(31)
      vecn(1,2) = 0.d0
      vecn(2,1) = 0.d0
      vecn(2,2) = relprp(32)
      vecn(3,1) = relprp(32)
      vecn(3,1) = relprp(31)
c Update injection state
      einj = 0
      if (ebif.ge.1 .and. eloadt.eq.1) einj = 1
      if (gamloc.eq.1 .and. ebif.ge.2 .and. etrack.eq.1) einj = 2
      ielprp(19) = einj
c Set stabilization term (tau), and injection type term (xi)
c REFERENCE: Box 5.10 (Diaz, 2012)
      tau = taumxd
      xi = 0.d0
      if (einj.eq.1) then
        tau = tauloc
        xi = 0.d0
      else if (einj.eq.2) then
        tau = 0.d0
        xi = 1.d0
      end if
c
c Set element arrays of current nodal coordinates, total and incremental
c displacements
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
c
c Initialize element force vector
      call rvzero(eload,nevab)
c
c=======================================================================
c          Begin loop over all Gauss points to compute stress          |
c=======================================================================
c This loop runs backwards from ngausp to 1. The purpose of this 
c behavior is to compute first the stresses at the sampling points to 
c use them in the integration process
c
      ippos = 1
      ipwei = ngausp*ndime+1
      do igausp = ngausp, 1, -1
c Set Gauss points positions and weights
        exisp = relprp(ippos-1+igausp*2-1)
        etasp = relprp(ippos-1+igausp*2)
        weigp = relprp(ipwei-1+igausp)
c set gauss point type: regular=0, reduced integration=1, elastic=2
        if (igausp.eq.ngausp) gptype = ela
        if (igausp.eq.ngausp-1) gptype = dis
        if (igausp.lt.ngausp-1) gptype = reg
c Evaluate shape functions and their derivatives (use current
c configuration for large strains)
        call shpfun
     &  (deriv      ,etasp      ,exisp      ,0          ,ieltyp     ,
     &   ndime      ,shape      )
        call jacob2
     &  (cartd      ,deriv      ,detjac     ,elcod      ,ielem      ,
     &   ndime      ,ndime      ,nnode      )
        if (detjac.le.0.d0) then
c ... cut increment if current jacobian is not positive definite
          call errprt('WE0027')
          inccut=.true.
          goto 999
        end if
c        if(ntype.eq.3)call getgco
c     &( gpcod       ,elcod      ,ndime      ,ndime      ,nnode      ,
c     &  shape       )
c Evaluate symmetric gradient operator B
        call getbmx
     &  (bmatx      ,gpcod      ,cartd      ,ndime      ,nbdim      ,
     &   naxis      ,nnode      ,ntype      ,shape      )
c
c Compute basic kinematic variables needed for state update
c =========================================================
c
        if (nlarge.eq.1) then
c Large strains: compute incremental deformation gradient
c -------------------------------------------------------
c Note: Large strains is not yet implemented. The use of large strains 
c       will produce an error and stop the program
          call errprt('ED0200')
        else
c Small strains: compute incremental infinitesimal strain: EINCR
c --------------------------------------------------------------
          call sdstra
     &  (bmatx      ,deldis     ,mdofn      ,nbdim      ,ndofn      ,
     &   nnode      ,ntype      ,gptype     ,xi         ,graphi     ,
     &   eincr      )
c
        end if
c
c Call material interface routine for state update calls: update stress
c and other state variables
c =====================================================================
c
        call matisu
     &  (detf       ,nlarge     ,ntype      ,sufail     ,
     &   thkgp(igausp)          ,eincr      ,fincr      ,iprops     ,
     &   lalgva(1,igausp)       ,ralgva(1,igausp)       ,rprops     ,
     &   rstava(1,igausp)       ,strsg(1,igausp)        )
        if (sufail) then
c State updating failed for current gauss point: break loop over gauss
c points and exit with increment cutting flag activated
          inccut=.true.
          goto 999
        end if
c
c Save stresses from reduced integration, elastic, and regular gauss
c points for more clarity on 'tilstr' stress calculation later
c REFERENCE: Eq. 5.100 (Dias, 2012)
c ------------------------------------------------------------------
c
        if (gptype .eq. reg) then
c ... regular gauss points
          do istre = 1, nstre
            regstr(istre) = strsg(istre,igausp)
          end do
        else if (gptype .eq. dis) then
c ... reduced integration gauss point
          do istre = 1, nstre
            disstr(istre) = strsg(istre,igausp)
          end do
        else if (gptype .eq. ela) then
c ... elastic gauss point
          do istre = 1, nstre
            elastr(istre) = strsg(istre,igausp)
          end do
        end if
c
c Add current regular gauss point contribution to the element internal
c force vector
c ====================================================================
c
c Evaluate elemental volume
        dvolu=detjac*weigp
        if (ntype.eq.1) then
c ... plane stress analysis
          dvolu=dvolu*thkgp(igausp)
        else if (ntype.eq.3) then
c ... axisymmetric analysis
          dvolu=dvolu*twopi*gpcod(naxis)
        end if
c
c Elemental internal force computation only on regular gauss points
        if (gptype .eq. reg) then
c
c Compute 'tilstr' stress for integration
c REFERENCE: Eq. 5.99 (Dias, 2012)
          do istre = 1, nstre
            tilstr(istre) = 
     &      (1.d0-xi)*(tau*regstr(istre) + (1.d0-tau)*disstr(istre)) + 
     &      xi*((1.d0-gamwsd)*disstr(istre) + gamwsd*elastr(istre))
          end do
c                           t
c Add current regular gauss point  B [sigma]
          call rtv
     &  (0          ,nbdim      ,nevab      ,nbdim      ,eload
     &  ,bmatx      ,tilstr     ,dvolu      )
c
        end if
c
      end do
c=======================================================================
c                 End of loop over gauss points                        |
c=======================================================================
c
c
  999 continue
      return
      end
