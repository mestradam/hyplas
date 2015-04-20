      subroutine stsd2
     &  (ielem      ,kunld      ,mdime      ,melem      ,mpoin      ,
     &   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype      ,
     &   unsym      ,coord1     ,dincr      ,estif      ,ielprp     ,
     &   iprops     ,lalgva     ,lnods      ,ralgva     ,relprp     ,
     &   rprops     ,rstava     ,rstav2     ,strsg      ,thkgp      ,
     &   tdisp      )
c***********************************************************************
c Evaluates the element tangent stiffness matrix for elements of class
c 'strdis' (Isoparametric elements enriched with weak or strong 
c discontinuity) in 2D: plane stress
c
c (M. Estrada, 2015)
c
c REFERENCE: Diaz, 2012: Apendix C
c***********************************************************************
c
c Variable declaration
c --------------------
      implicit double precision (a-h,o-z)
c
c Database
      include '../../ELEMENTS.INC'
      include '../../MATERIAL.INC'
      include '../../LOCALIZA.INC''
c
      parameter( mgdim=5 ,mbdim=4 ,ndime=2 ,ndofn=2 )
c Arguments
      logical  lalgva ,unsym
      dimension
     1    coord1(mdime,mpoin),dincr(mtotv)       ,estif(mevab,mevab) ,
     2    ielprp(*)          ,iprops(*)         ,lalgva(mlalgv,mtotg),
     3    lnods(melem,mevab) ,ralgva(mralgv,mtotg),relprp(*)         ,
     4    rprops(*)        ,rstava(mrstav,mtotg),rstav2(mrstav,mtotg),
     5    strsg(mstre,mtotg) ,thkgp(mtotg)       ,tdisp(mtotv)
c Local arrays and variables
      dimension
     1    auxm(mevab,mgdim)  ,amatx(mgdim,mgdim) ,bmatx(4,mevab)     ,
     2    cartd(ndime,mnode) ,deldis(2,mnode)    ,deriv(ndime,mnode) ,
     3    dmatx(mbdim,mbdim) ,eincr(mbdim)       ,elcod(ndime,mnode) ,
     4    fincin(3,3)        ,fincr(3,3)         ,finv(3,3)          ,
     5    gmatx(mgdim,mevab) ,gpcod(ndime)       ,shape(mnode)       ,
     6    teldis(2,mnode)
      data r0   ,r1   ,r8   /
     1     0.0d0,1.0d0,8.0d0/
      if(ntype.eq.3)twopi=r8*atan(r1)
c Identify element type
      ieltyp=ielprp(1)
c Recover element information
      nnode =ielprp(3)
      ngausp=ielprp(4)
      nevab =ielprp(5)
c
c Set element nodal coordinates, total and incremental displacements
c vectors
c
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
c
c Initialize the element stiffness matrix
c
      do ievab=1,nevab
        do jevab=1,nevab
          estif(ievab,jevab)=r0
        end do
      end do
c
c=======================================================================
c                 Begin loop over gauss points                         |
c=======================================================================
      ippos=1
      ipwei=ngausp*ndime+1
      do igausp=1,ngausp
        exisp=relprp(ippos-1+igausp*2-1)
        etasp=relprp(ippos-1+igausp*2)
        weigp=relprp(ipwei-1+igausp)
c Evaluate the shape functions and derivatives
        call shpfun
     1(   deriv      ,etasp      ,exisp      ,0          ,ieltyp     ,
     2    ndime      ,shape      )
        call jacob2
     1(   cartd      ,deriv      ,detjac     ,elcod      ,ielem      ,
     2    ndime      ,ndime      ,nnode      )
        if(detjac.le.r0)then
c... stops program if element jacobian is not positive definite
          call errprt('EE0003')
        end if
        if(ntype.eq.3)call getgco
     1(   gpcod      ,elcod      ,ndime      ,ndime      ,nnode      ,
     2    shape      )
c
        if(nlarge.eq.1)then
c Large strains: compute incremental deformation gradient
c -------------------------------------------------------
c gradient operator g in the current configuration
          call getgmx
     1(   gpcod      ,cartd      ,gmatx      ,ndime      ,mgdim      ,
     2    naxis      ,nnode      ,ntype      ,shape      )
c inverse of incremental deformation gradient
          call defgra
     1(   deldis     ,fincin     ,gmatx      ,2          ,mgdim      ,
     2    ndofn      ,ntype      ,nnode      )
c stops program if deformation gradient determinant is non-positive
          if(ntype.eq.3.and.fincin(3,3).le.r0)call errprt('EE0004')
c incremental deformation gradient
          call invf2
     1(   fincin     ,fincr      ,ntype      )
c... and the determinant of the total deformation gradient
          call defgra
     1(   teldis     ,finv       ,gmatx      ,2          ,mgdim      ,
     2    ndofn      ,ntype      ,nnode      )
          detfin=finv(1,1)*finv(2,2)-finv(1,2)*finv(2,1)
          if(ntype.eq.3)then
c stops program if deformation gradient is not positive definite
            if(finv(3,3).le.r0)call errprt('EE0004')
            detfin=detfin*finv(3,3)
          end if
          detf=r1/detfin
        else
c Small strains: compute incremental infinitesimal strain
c -------------------------------------------------------
c compute the symmetric gradient operator b
          call getbmx
     1(   bmatx      ,gpcod      ,cartd      ,ndime      ,4          ,
     2    naxis      ,nnode      ,ntype      ,shape      )
c and the incremental infinitesimal strain
          call listra
     1(   bmatx      ,deldis     ,2          ,4          ,ndofn      ,
     2    nnode      ,ntype      ,eincr      )
        end if
c
c Call material interface routine for consistent tangent computation
c calls: Compute either the standard consistent tangent matrix DMATX
c (small strains) or the spatial tangent modulus AMATX (large strains)
c ====================================================================
c
        call matict
     1(   detf       ,kunld      ,mbdim      ,mgdim      ,
     2    nlarge     ,ntype      ,
     3    amatx      ,dmatx      ,eincr      ,fincr      ,iprops     ,
     4    lalgva(1,igausp)       ,ralgva(1,igausp)       ,rprops     ,
     5    rstava(1,igausp)       ,rstav2(1,igausp)       ,
     5    strsg(1,igausp)        )
c
c Add current gauss point contribution to element stiffness
c =========================================================
c
c Compute elemental volume
        dvolu=detjac*weigp
        if(ntype.eq.1)then
          dvolu=dvolu*thkgp(igausp)
        else if(ntype.eq.3)then
          dvolu=dvolu*twopi*gpcod(naxis)
        end if
        if(nlarge.eq.1)then
c                                                         t
c Large strains:  assemble the element stiffness as  k:= g [a] g
c --------------------------------------------------------------
          if(ntype.eq.3)then
            ngdim=5
          else
            ngdim=4
          end if
          call rtsr
     1(   auxm       ,0          ,mevab      ,mgdim      ,nevab      ,
     2    ngdim      ,estif      ,gmatx      ,amatx      ,dvolu      ,
     3    unsym      )
        else
c                                                        t
c Small strains: assemble the element stiffness as  k:= b d b
c -----------------------------------------------------------
          if(ntype.eq.1.or.ntype.eq.2)then
            nbdim=3
          else if(ntype.eq.3)then
            nbdim=4
          end if
          call rtsr
     1(   auxm       ,0          ,mevab      ,4          ,nevab      ,
     2    nbdim      ,estif      ,bmatx      ,dmatx      ,dvolu      ,
     3    unsym      )
        end if
c
      end do
c=======================================================================
c                 End of loop over gauss points                        |
c=======================================================================
      return
      end
