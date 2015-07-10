      subroutine cpdsmo
     .  (nelem      ,npoin      ,ndime      ,mtotv      ,mrstav     ,
     .   ielprp     ,relprp     ,iestv      ,restv      ,lnods      ,
     .   igrpid     ,ieltid     ,coord      ,tdisp      ,dincr      ,
     .   thkgp      ,rstava     ,smtvar     ,cartd      )
!***********************************************************************
! Double smooth procedure for the computation of the crack path 
!
! (M Estrada 2015)
! REFERENCE: Diaz, 2012: Section 5.3, Box 5.9
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
     .   ielprp(mieprp,mgrup)       ,relprp(mreprp,mgrup)     ,
     .   iestv(miestv,nelem)        ,restv(mrestv,nelem)      ,
     .   lnods(nelem,mevab)         ,igrpid(nelem)            ,
     .   ieltid(mgrup)              ,tdisp(mtotv)             ,
     .   dincr(mtotv)               ,thkgp(mtotg,nelem)       ,
     .   rstava(mrstav,mtotg,nelem) ,smtvar(npoin)            ,
     .   cartd(nelem,mtotg,ndime,mnode)
!     
!     Local variables
      dimension
     .   dgmass(npoin)      ,elcod(ndime,mnode) ,teldis(mdofn,mnode),
     .   deldis(mdofn,mnode),shape(mtotg,mnode) ,dvolu(nelem,mtotg) ,
     .   smtvar1(npoin)     ,vn(ndime)          ,deriv(ndime)
      parameter
     .   twopi = 8.d0*atan(1.d0)
!     
!-----------------------------------------------------------------------
!     
!     Compute shape functions and element volume
!     ==========================================
!     
      do ielem = 1, nelem
!        
!       Recover element and material type group identification numbers
        igrup = igrpid(ielem)
        ielidn = ieltid(igrup)
        nnode = ielprp(3,ielidn)
        ielcls = ielprp(2,ielidn)
        ngausp = ielprp(4,ielidn)
!        
!       Control for different element types
        if (ielcls .ne. wsdisc) return
!
!       Set element arrays of current nodal coordinates, total and 
!       incremental displacements
        do inode = 1, nnode
          ipoin = iabs(lnods(ielem,inode))
          nposn = (lnode-1)*ndofn
          do idofn = 1, ndofn
            nposn = nposn+1
            if (nlarge.eq.1) then
              elcod(idofn,inode) = coord(idofn,ipoin)
              teldis(idofn,inode) = -tdisp(nposn)
              deldis(idofn,inode) = -dincr(nposn)
            else
              elcod(idofn,inode) = coord(idofn,lnode)
              deldis(idofn,inode) = dincr(nposn)
            end if
          end do
        end do
!        
!       Compute shape functions and derivatives
        ippos = 1
        ipwei = ngausp*ndime+1
!      
        do igausp = 1, ngausp
!        
!         Set Gauss points positions and weights
          exisp = relprp(ippos-1+igausp*2-1,igrup)
          etasp = relprp(ippos-1+igausp*2,igrup)
          weigp = relprp(ipwei-1+igausp,igrup)
!        
!         Evaluate shape functions and their derivatives
          call shpfun
     .  (deriv      ,etasp      ,exisp      ,0          ,ieltyp     ,
     .   ndime      ,shape(igausp,:)        )
          call jacob2
     .  (cartd(ielem,igausp,:,:),deriv      ,detjac     ,elcod      ,
     .   ielem      ,ndime      ,ndime      ,nnode      )
!
!         Evaluate elemental volume
          dvolu(ielem,igausp)=detjac*weigp
          if (ntype.eq.1) then
!         ... plane stress analysis
            dvolu(ielem,igausp)=dvolu(ielem,igausp)*thkgp(igausp,ielem)
          end if
        end do
!      
!       Compute diagonal global mass matrix
!       -----------------------------------
        do inode = 1, nnode
          ipoin = iabs(lnods(ielem,inode))
          do jnode = 1, nnode
            dgmass(ipoin) = 0.d0
            do igausp = 1, ngausp-2
              dgmass(ipoin) = dgmass(ipoin) + shape(igausp,inode) * 
     .                        shape(igausp,jnode) * dvolu(ielem,igausp)
            end do
          end do
        end do
!        
!     End loop over elements
      end do
!      
!-----------------------------------------------------------------------
!      
!     Perform first smoothing
!     =======================
!     
      do ipoin = 1, npoin
        smtvar1(ipoin) = 0.d0
      end do
!      
!     Loop over all elements
      do ielem = 1, nelem
!        
!       Get variable to smooth
        igausp = ngausp-1
        gn = rstava(13,igausp,ielem)
        restv(19,ielem) = gn
        eleng = restv(1,ielem)
        elk = restv(2,ielem)
!        
!       (variable to smoth)
        alpha = gn * elk/eleng
!        
!       Smooth internal variable 'alpha'
        call smooth
     .  (nnode      ,ngausp     ,npoin      ,shape      ,dvolu(ielem,:),
     .   lnods(ielem,:)         ,dgmass     ,smtvar1    ,alpha     )
!        
!       End loop over elements for the first smoothing
      end do
!      
!-----------------------------------------------------------------------
!      
!     Perform second smoothing
!     ========================
!     
      do ipoin = 1, npoin
        smtvar(ipoin) = 0.d0
      end do
!      
!     Loop over all elements
      do ielem = 1, nelem
!        
!       Get vector normal to discontinuity
        vn(1) = restv(12,ielem)
        vn(2) = restv(13,ielem)
!        
!       Compute the normal derivative to smooth
        norder = 0.d0
        igausp = ngausp
        do idime = 1, ndime
          deriv(idime) = 0.d0
          do inode = 1, nnode
            ipoin = iabs(lnods(ielem,inode))
!            
!           Derivative: [deriv] = [grad(N)] [smtvar1]
            deriv(idime) = deriv(idime) + 
     .  cartd(ielem,igausp,idime,inode) * smtvar1(ipoin)
          end do
!          
!         Normal derivative: [deriv] [n]
!         (variable to smooth)
          derivn = derivn + deriv(idime) * vn(idime)
        end do
!        
!       Smooth normal derivative 'derivn'
        call smooth
     .  (nnode      ,ngausp     ,npoin      ,shape      ,dvolu(ielem,:),
     .   lnods(ielem,:)         ,dgmass     ,smtvar     ,derivn     )
!        
!       End loop over elements for second smoothing
      end do
!     
      return
      end
      
!      
!=======================================================================
!=======================================================================
!      
      subroutine smooth
     .  (nnode      ,ngausp     ,npoin      ,shape      ,dvolu      ,
     .   lnods      ,dgmass     ,smtvar     ,variab     )
!***********************************************************************
! Smooth procedure
!     
!     Solves the equation:
!                   -1              -1               T
!     smtvar = [M_L]  Assembly( [Me]  [Me_L] int( [N] variab ) )
!
! (M Estrada 2015)
! REFERENCE: Diaz, 2012: Section 5.3
!            
!***********************************************************************
!     
!     Variable declaration
!     --------------------
!        
      implicit double precision (a-h, o-z)
!     
!     Arguments
      dimension
     .   shape(ngausp,nnode),dvolu(ngausp)      ,dgmass(npoin)      ,
     .   smtvar(npoin)      ,lnods(nnode)
!     
!     Local variables
      dimension
     .   emass(nnode,nnode) ,eminv(nnode,nnode) ,edgmass(nnode)     ,
     .   esmtvar(nnode)
      logical failinv
!     
!-----------------------------------------------------------------------
!      
!      
!     Compute elemental mass matrix
!     -----------------------------
      do inode = 1, nnode
        edgmass(inode) = 0.d0
        do jnode = 1, nnode
          emass(inode,jnode) = 0.d0
          do igausp = 1, ngausp
            emass(inode,jnode) = emass(inode,jnode) + 
     .  shape(igausp,inode) * shape(igausp,jnode) * dvolu(igausp)
          end do
          edgmass(inode) = edgmass(inode) + emass(inode,jnode)
        end do
      end do
!     
      call invmt(nnode,emass,eminv,failinv)
      if (failinv) call errprt('EE0013')
        
!        
!     Compute local smoothing variable a element nodes
!     ------------------------------------------------
!                    T
!     [esmtvar] = [N] variab dV
      do inode = 1, nnode
        esmtvar(inode) = 0.d0
        do igausp = 1, ngausp
          esmtvar(inode) = esmtvar(inode) +
     .  shape(igausp,inode) * variab * dvolu(igausp)
        end do
      end do
!                               -1
!     [esmtvar] = [M_lump] [M_e]  [esmtvar]
      esmtvar = multmt(eminv, esmtvar, nnode, nnode, 1)
      do inode = 1, nnode
        esmtvar(inode) = edgmass(inode) * esmtvar(inode)
      end do
!       
!     Assemble element smooth variable to mesh nodes
!                     -1
!     [smtvar] = [M_L]  Assemble( [esmtvar] )
      do inode = 1, nnode
        ipoin = iabs(lnods(inode))
        if (dgmass(ipoin) .ne. 0.d0) then
          smtvar(ipoin) = smtvar(ipoin)+esmtvar(inode)/dgmass(ipoin)
        end if
      end do
!     
      return
      end