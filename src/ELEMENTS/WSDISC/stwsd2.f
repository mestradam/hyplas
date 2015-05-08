      subroutine stwsd2
     &  (ielem      ,kunld      ,mdime      ,melem      ,mpoin      ,
     &   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype      ,
     &   unsym      ,coord1     ,dincr      ,estif      ,ielprp     ,
     &   iprops     ,lalgva     ,lnods      ,ralgva     ,relprp     ,
     &   rprops     ,rstava     ,rstav2     ,strsg      ,thkgp      ,
     &   tdisp      )
!***********************************************************************
! Evaluates the element tangent stiffness matrix for elements of class
! 'wsdisc' (Isoparametric elements enriched with weak or strong
! discontinuity) in 2D: plane stress
!
! (M. Estrada, 2015)
!
! REFERENCE: Diaz, 2012: Apendix C
!***********************************************************************
!
! Variable declaration
! --------------------
      implicit double precision (a-h,o-z)
!
! Database
      include '../../ELEMENTS.INC'
      include '../../MATERIAL.INC'
      include '../../LOCALIZA.INC'
!
! Arguments
      logical  
     &   lalgva(mlalgv,mtotg)   ,unsym
      integer
     &   ielprp(mieprp)     ,iprops(miprop)     ,lnods(melem,mevab) ,
     &   ielem      ,kunld      ,mdime      ,melem      ,mpoin      ,
     &   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype
      double precision
     &   coord1(mdime,mpoin)  ,dincr(mtotv)     ,estif(mevab,mevab) ,
     &   ralgva(mralgv,mtotg) ,relprp(mreprp)   ,rprops(mrprop)     ,
     &   rstava(mrstav,mtotg) ,rstav2(mrstav,mtotg)                 ,
     &   strsg(mstre,mtotg)   ,thkgp(mtotg)     ,tdisp(mtotv)
!
! Local arrays and variables
      integer
     &   reg        ,dis        ,ela        ,mbdim      ,ndime      ,
     &   ndofn      ,einj
      parameter
     &  (reg = 0    ,dis = 1    ,ela = 2    ,mbdim = 4  ,ndime = 2  ,
     &   ndofn = 2  )
      double precision
     &   auxm(mevab,mbdim)  ,amatx(mbdim,mbdim) ,bmatx(mbdim,mevab) ,
     &   cartd(ndime,mnode) ,deldis(mdofn,mnode),deriv(ndime,mnode) ,
     &   dmatx(mbdim,mbdim) ,eincr(mbdim)       ,elcod(ndime,mnode) ,
     &   fincin(3,3)        ,fincr(3,3)         ,finv(3,3)          ,
     &   gpcod(ndime)       ,shape(mnode)       ,
     &   teldis(2,mnode)    ,graphi(3,ndofn)    ,vecn(3,2)          ,
     &   kub(mevab,ndofn)   ,kbu(ndofn,mevab)   ,kbb(ndofn,ndofn)   ,
     &   kbbinv(ndofn,ndofn),kubt(mevab,mevab)  ,
     &   de         ,dvolu      ,detf       ,detjac     ,eleng      ,
     &   eljump     ,elk        ,elkinv     ,etasp      ,exisp      ,
     &   scalar
!
! Set and read variables from arrays
! ==================================
!
! Set dimension of B matrix and strain increment
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
! Identify element type
      ieltyp= ielprp(1)
! Num. of nodes per element
      nnode = ielprp(3)
! Num. of Gauss Points per element
      ngausp= ielprp(4)
! Num. of element degrees of freedom
      nevab = ielprp(5)
! Element length
      eleng = relprp(21)
      if (eleng.eq.0.d0) eleng = 1.d0
! Strong discontinuities band width (paramenter k)
      elk = relprp(22)
      if (elk.le.0.d0) elk = eleng
      elkinv = 1.d0 / elk
! Injection type
      einj = ielprp(19)
! Gradient of phi
      graphi(1,1) = relprp(39)
      graphi(1,2) = 0.d0
      graphi(2,1) = 0.d0
      graphi(2,2) = relprp(40)
      graphi(3,1) = relprp(40)
      graphi(3,2) = relprp(39)
! Vector normal to discontinuity
      vecn(1,1) = relprp(31)
      vecn(1,2) = 0.d0
      vecn(2,1) = 0.d0
      vecn(2,2) = relprp(32)
      vecn(3,1) = relprp(32)
      vecn(3,1) = relprp(31)
!
! Set stabilization term (tau), and injection type term (xi)
! REFERENCE: Box 5.10 (Diaz, 2012)
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
! Set element nodal coordinates, total and incremental displacements
! vectors
      do inode =1,nnode
        lnode=iabs(lnods(ielem,inode))
        nposn=(lnode-1)*ndofn
        do idofn=1,ndofn
          nposn=nposn+1
          elcod(idofn,inode)=coord1(idofn,lnode)
          if(nlarge.eq.1)then
            teldis(idofn,inode)=-tdisp(nposn)
            deldis(idofn,inode)=-dincr(nposn)
          else
            deldis(idofn,inode)=dincr(nposn)
          end if
        end do
      end do
!
! Initialize the element stiffness matrixes
      do ievab = 1,nevab
        do jevab = 1,nevab
          estif(ievab,jevab) = 0.d0
        end do
        do idofn = 1,ndofn
          kub(ievab,idofn) = 0.d0
          kbu(idofn,ievab) = 0.d0
          do jdofn = 1,ndofn
            kbb(idofn,jdofn) = 0.d0
            kbbinv(idofn,jdofn) = 0.d0
          end do
        end do
      end do
!
!=======================================================================
!                    Begin loop over gauss points                      |
!=======================================================================
      ippos=1
      ipwei=ngausp*ndime+1
      do igausp = ngausp, 1, -1
! Set Gauss points positions and weights
        exisp=relprp(ippos-1+igausp*2-1)
        etasp=relprp(ippos-1+igausp*2)
        weigp=relprp(ipwei-1+igausp)
! set gauss point type: regular=0, reduced integration=1, elastic=2
        if (igausp.eq.ngausp) gptype = ela
        if (igausp.eq.ngausp-1) gptype = dis
        if (igausp.lt.ngausp-1) gptype = reg
! Evaluate the shape functions and derivatives
        call shpfun
     &  (deriv      ,etasp      ,exisp      ,0          ,ieltyp     ,
     &   ndime      ,shape      )
        call jacob2
     &  (cartd      ,deriv      ,detjac     ,elcod      ,ielem      ,
     &   ndime      ,ndime      ,nnode      )
        if(detjac.le.0.d0)then
!... stops program if element jacobian is not positive definite
          call errprt('EE0012')
        end if
!
        if(nlarge.eq.1)then
! Large strains: compute incremental deformation gradient
! -------------------------------------------------------
! Note: Large strains is not yet implemented. The use of large strains 
!       will produce an error and stop the program
          call errprt('ED0200')
        else
! Small strains: compute incremental infinitesimal strain
! -------------------------------------------------------
! compute the symmetric gradient operator b
          call getbmx
     &  (bmatx      ,gpcod      ,cartd      ,ndime      ,4          ,
     &   naxis      ,nnode      ,ntype      ,shape      )
! and the incremental infinitesimal strain
          call sdstra
     &  (bmatx      ,deldis     ,ndofn      ,4          ,ndofn      ,
     &   nnode      ,ntype      ,gptype     ,xi         ,graphi     ,
     &   eincr      ,eljump     ,vecn       ,elkinv     )
        end if
!
! Call material interface routine for consistent tangent computation
! calls: Compute either the standard consistent tangent matrix DMATX
! (small strains) or the spatial tangent modulus AMATX (large strains)
! ====================================================================
!
        call matict
     &  (detf       ,kunld      ,mbdim      ,mgdim      ,
     &   nlarge     ,ntype      ,
     &   amatx      ,dmatx      ,eincr      ,fincr      ,iprops     ,
     &   lalgva(1,igausp)       ,ralgva(1,igausp)       ,rprops     ,
     &   rstava(1,igausp)       ,rstav2(1,igausp)       ,
     &   strsg(1,igausp)        )
!
! Add current gauss point contribution to element stiffness
! =========================================================
!
! Compute elemental volume
        dvolu=detjac*weigp
        if(ntype.eq.1)then
          dvolu=dvolu*thkgp(igausp)
        else if(ntype.eq.3)then
          dvolu=dvolu*twopi*gpcod(naxis)
        end if
        if (ngausp.eq.ela) then
          de = dvolu/eleng
        end if
!
! Assemble the element stiffness matrixes
! ---------------------------------------
!
        if (gptype .eq. reg) then
! Regular gauss points: 
!                  t
! k = (1-xi)*tau [B C B] dV
          scalar = (1.d0-xi)*tau*dvolu
          call rtsr
     &  (auxm       ,0          ,mevab      ,4          ,nevab      ,
     &   nbdim      ,estif      ,bmatx      ,dmatx      ,scalar     ,
     &   unsym      )
!
        else if (gptype .eq. dis) then
! Reduced integration gauss point
!                    t                          t
! k = (1-xi)(1-tau)[B C B]dV + xi(1-gamma_wsd)[B C B]dV
          scalar = ((1.d0-xi)*(1.d0-tau) + xi*(1.d0-gamwsd))*dvolu
          call rtsr
     &  (auxm       ,0          ,mevab      ,4          ,nevab      ,
     &   nbdim      ,estif      ,bmatx      ,dmatx      ,scalar     ,
     &   unsym      )
          if (einj.eq.2) then
!                        t                                     t
! kub = -(1-gamma_wsd) [B C grad(phi)] dV + (1-gamma_wsd)1/k [B C n]dV
            scalar = -(1.d0-gamwsd)*dvolu
            call rtsx
     &  (auxm       ,0          ,mevab      ,4          ,nevab      ,
     &   nbdim      ,kub        ,bmatx      ,dmatx      ,graphi     ,
     &   scalar     )
            scalar = (1.d0-gamwsd)*elkinv*dvolu
            call rtsx
     &  (auxm       ,0          ,mevab      ,4          ,nevab      ,
     &   nbdim      ,kub        ,bmatx      ,dmatx      ,vecn       ,
     &   scalar     )
!         t
! kbu = [n C B] de
            scalar = de
            call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbu        ,vecn       ,dmatx      ,bmatx      ,
     &   scalar     )
!           t                        t
! kbb = - [n C grad(phi)] de + 1/k [n C n] de
              scalar = -de
              call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbb        ,vecn       ,dmatx      ,graphi     ,
     &   scalar     )
              scalar = elkinv*de
              call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbb        ,vecn       ,dmatx      ,vecn       ,
     &   scalar     )
          end if
        else if (gptype .eq. ela) then
! Elastic gauss point: 
!                    t
! k = xi*gamma_wsd [B C B]dV
          scalar = xi*gamwsd*dvolu
          call rtsr
     &  (auxm       ,0          ,mevab      ,4          ,nevab      ,
     &   nbdim      ,estif      ,bmatx      ,dmatx      ,scalar     ,
     &   unsym      )
          if (einj.eq.2) then
!                    t
! kub = -gamma_wsd [B C grad(phi)] dV
            scalar = -gamwsd*dvolu
            call rtsx
     &  (auxm       ,0          ,mevab      ,4          ,nevab      ,
     &   nbdim      ,kub        ,bmatx      ,dmatx      ,graphi     ,
     &   scalar     )
            if (injtyp.eq.symsdinj) then
! ... symetric strong discontinuity formulation
! ... REFERENCE: Box C.1 (Dias, 2012)
!                   t
! kbu = - [grad(phi) C B] dV
              scalar = -dvolu
              call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbu        ,graphi     ,dmatx      ,bmatx      ,
     &   scalar     )
!                 t
! kbb = [grad(phi) C grad(phi)] dV
              scalar = dvolu
              call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbu        ,graphi     ,dmatx      ,graphi     ,
     &   scalar     )
            else
! ... non-symetric strong/weak discontinuity formulation
! ... REFERENCE: Box C.2 and C.3 (Dias, 2012)
!           t
! kbu = - [n C B] de
              scalar = -de
              call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbu        ,vecn       ,dmatx      ,bmatx      ,
     &   scalar     )
!         t
! kbb = [n C grad(phi)] de
              scalar = de
              call rtsx
     &  (auxm       ,0          ,ndofn      ,3          ,ndofn      ,
     &   3          ,kbb        ,vecn       ,dmatx      ,graphi     ,
     &   scalar     )
            end if
          end if
        end if
      end do
!=======================================================================
!                 End of loop over gauss points                        |
!=======================================================================
!                                           -1
! Last assemble : k = k + (1-xi)tau [kub kbb  kbu]
      if (einj.eq.2) then
        call invmt2(kbb, kbbinv)
        kubt = tranmt(kub, nevab, ndofn)
        scalar = (1.d0-xi)*tau
        call rtsx
     &  (auxm       ,0          ,mevab      ,ndofn      ,nevab      ,
     &   ndofn      ,estif      ,kubt       ,kbbinv     ,kbu        ,
     &   scalar     )
      end if
! 
! Save alternate stifness matrices: kub, kbu, and kbb
! ---------------------------------------------------
! These matrices are stored in relprp array, from position 51 onwards.
! Values are stored in row order (first all row 1, then all row 2, and
! so on)
!
      irelprp = 51
! Matrix kub
      do ievab = 1, nevab
        do idofn = 1, ndofn
          relprp(irelprp) = kub(ievab,idofn)
          irelprp = irelprp + 1
        end do
      end do
!
      irelprp = 51 + nevab*ndofn
! Matrix kbu
      do idofn = 1, ndofn
        do ievab = 1, nevab
          relprp(irelprp) = kbu(idofn,ievab)
          irelprp = irelprp + 1
        end do
      end do
!
      irelprp = 51 + nevab*ndofn + nevab*ndofn
! Matrix kbb
      do idofn = 1, ndofn
        do jdofn = 1, ndofn
          relprp(irelprp) = kbb(idofn,jdofn)
          irelprp = irelprp + 1
        end do
      end do
      return
      end
