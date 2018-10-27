      subroutine ctcovm
     .(   ralgva     ,dmatx      ,lalgva     ,iprops     ,ntype      ,
     .    rprops     ,rstava     ,stres      ,strat      )
!***********************************************************************
! Computation of the consistent tangent modulus 
! for composite material (plane strain and axisymmetric only).
!   
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   ralgva  () :
!   dmatx   () :
!   epflag  () :
!   iprops  () :
!   ntype   () :
!   rprops  () :
!   rstava  () :
!   stresk  () :
!***********************************************************************
!     
! Variable declaration
      implicit double precision (a-h,o-z)
      parameter(iphard=21  ,mstre=4)
!      
! ... arguments
      logical 
     .    lalgva(3)
      integer
     .    iprops(4)  ,ntype
      real(kind=8)
     .    ralgva(*)  ,dmatx(4,4) ,rprops(*)  ,rstava(14) ,stres(4)   ,
     .    strat(4)
!     
! ... local variables
      logical
     .    epflag     ,damflag
      integer
     .    nstre      ,nhard
      real(kind=8)
     .    devprj(4,4),foid(4,4)  ,s(4)       ,soid(4)    ,vecn(3)
      real(kind=8)
     .    young      ,poiss      ,youngf     ,poissf     ,sigmauf    ,
     .    alphaf     ,betaf      ,volfrf     ,anglef     ,ee(4)      ,
     .    eef        ,mx         ,my         ,damage     ,dmatxf(3,3),
     .    rpropst(30),epbar      ,dgama      ,gmodu      ,bulk
      data
     1    foid(1,1),foid(1,2),foid(1,3),foid(1,4)/
     2    1.0d0    ,0.0d0    ,0.0d0    ,0.0d0    /
     3    foid(2,1),foid(2,2),foid(2,3),foid(2,4)/
     4    0.0d0    ,1.0d0    ,0.0d0    ,0.0d0    /
     5    foid(3,1),foid(3,2),foid(3,3),foid(3,4)/
     6    0.0d0    ,0.0d0    ,0.5d0    ,0.0d0    /
     7    foid(4,1),foid(4,2),foid(4,3),foid(4,4)/
     8    0.0d0    ,0.0d0    ,0.0d0    ,1.0d0    /
      data
     1    soid(1)  ,soid(2)  ,soid(3)  ,soid(4)  /
     2    1.0d0    ,1.0d0    ,0.0d0    ,1.0d0    /
      data
     1    r1   ,r2   ,r3   ,r6   /
     2    1.0d0,2.0d0,3.0d0,6.0d0/
!     
! Stops program if neither plane strain nor axisymmetric state
      if(ntype.ne.2.and.ntype.ne.3)call errprt('EI0030')
!
! ======================================================================
! Start tangent modulus computation for the LIGNIN MATRIX (Von Mises ps)
! ======================================================================
      
! Current accumulated plastic strain
      epbar  = rstava(9)
      epflag = lalgva(1)
      dgama  = rstava(11)
! Set material properties
      young=rprops(2)
      poiss=rprops(3)
      nhard=iprops(3)
! Shear and bulk moduli
      gmodu=young/(r2*(r1+poiss))
      bulk=young/(r3*(r1-r2*poiss))
      r2g=r2*gmodu
      r1d3=r1/r3
! Set deviatoric projection tensor
      if(ntype.eq.2)then
        nstre=3
      elseif(ntype.eq.3)then
        nstre=4
      endif
      do i=1,nstre
        do j=1,nstre
          devprj(i,j)=foid(i,j)-soid(i)*soid(j)*r1d3
        enddo
      enddo
      if(epflag)then
! Compute elastoplastic consistent tangent
! ----------------------------------------
        r3g=r3*gmodu
        roo3d2=sqrt(r3/r2)
! Hydrostatic pressure
        p=(stres(1)+stres(2)+stres(4))*r1d3
! Deviatoric stress components
        s(1)=stres(1)-p
        s(2)=stres(2)-p
        s(3)=stres(3)
        s(4)=stres(4)-p
! Recover last elastic trial von Mises effective stress
        snorm=sqrt(s(1)*s(1)+s(2)*s(2)+r2*s(3)*s(3)+s(4)*s(4))
        q=roo3d2*snorm
        qtrial=q+r3g*dgama
! Assemble elastoplastic tangent (upper triangle only)
        afact=r2g*(r1-r3g*dgama/qtrial)
        bfact=r6*gmodu*gmodu*(dgama/qtrial-
     .        r1/(r3g+dplfun(epbar,nhard,rprops(iphard))))/
     .        (snorm*snorm)
        do i=1,nstre
          do j=i,nstre
            dmatx(i,j)=afact*devprj(i,j)+bfact*s(i)*s(j)+
     .                 bulk*soid(i)*soid(j)
          enddo       
        enddo
      else
! Compute elasticity matrix (upper triangle only)
! -----------------------------------------------
        do i=1,nstre
          do j=i,nstre
            dmatx(i,j)=r2g*devprj(i,j)+bulk*soid(i)*soid(j)
          enddo      
        enddo
      endif
! Assemble lower triangle
! -----------------------
      do j=1,nstre-1
        do i=j+1,nstre
          dmatx(i,j)=dmatx(j,i)
        enddo
      enddo
!
! =================================================================
! Start tangent modulus computation for the FIBERS (Damage Weibull)
! =================================================================
!
! Set some material properties
      youngf = rprops(12)
      poissf = rprops(13)
      alphaf = rprops(15)
      betaf  = rprops(16)
      anglef = rprops(18)
      mx = dcos(anglef)
      my = dsin(anglef)
      volfrf = rprops(17)
      volfrm = 1.d0-volfr
      if (volfrf.le.tol) goto 888
!      
! Initialize algorithmic and internal variables
      damflag = lalgva(3)
! ... set previously accumulated damage variable
      damage = rstava(10)
!
!       ******************************* borrar
      damflag = .false.
!       **************************** borrar
      if (.not.damflag) then
! Compute elastic modulus matrix
! ------------------------------
          dmatxf(1,1) = youngf * mx**4
          dmatxf(1,2) = youngf * mx**2 * my**2
          dmatxf(1,3) = youngf * mx**3 * my
          dmatxf(2,1) = youngf * my**2 * mx**2
          dmatxf(2,2) = youngf * my**4
          dmatxf(2,3) = youngf * my**3 * mx
          dmatxf(3,1) = youngf * mx**3 * my
          dmatxf(3,2) = youngf * my**3 * mx
          dmatxf(3,3) = youngf * mx**2 * my**2
      else
! Compute tangent modulus matrix
! ------------------------------
!
! Elastic strain
          ee(1) = strat(1)
          ee(2) = strat(2)
!
! Convert engineering shear component into physical component
          ee(3) = (strat(3))/2.d0
!
! Strain component parallel to fibers direction "anglef"
          eef = ee(1)*mx**2+ee(2)*my**2 + 2.d0*ee(3)*mx*my
!
! Effective stress on fibers
          streff = youngf*eef
!
! Compute tangent axial modulus
          factor = (1.d0-damage)*(1.d0-alpha*(streff/beta)**alpha)
          youngft = youngf*factor
!
! Transform axial modulus to global coordinate system
          dmatxf(1,1) = youngft * mx**4
          dmatxf(1,2) = youngft * mx**2 * my**2
          dmatxf(1,3) = youngft * mx**3 * my
          dmatxf(2,1) = youngft * my**2 * mx**2
          dmatxf(2,2) = youngft * my**4
          dmatxf(2,3) = youngft * my**3 * mx
          dmatxf(3,1) = youngft * mx**3 * my
          dmatxf(3,2) = youngft * my**3 * mx
          dmatxf(3,3) = youngft * mx**2 * my**2
      endif
!
! =============================================
! Compute COMPOSITE tangent constitutive matrix
! =============================================
!
! Homogenization by mixture law
      do i=1, nstre
          do j=1, nstre
              dmatx(i,j) = volfrm*dmatx(i,j) + volfrf*dmatxf(i,j)
          enddo
      enddo
!      
  888 continue
      return
      end
