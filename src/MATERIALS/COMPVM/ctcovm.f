      subroutine ctcovm
     1(   dgama      ,dmatx      ,epflag     ,iprops     ,ntype     ,
     2    rprops     ,rstava     ,stres      )
      implicit double precision (a-h,o-z)
      parameter (iphard=20, mstre=4)
c output arguments
      double precision dmatx(mstre,mstre)
c input arguments
      integer ntype
      double precision dgama
      dimension iprops(*), rprops(*), strat(mstre), rstava(mstre+1), 
     &  stres(mstre)
      logical epflag
c local variables
      parameter (nstre=6)
      double precision volfr, a1, a2, a3, l1, l2, l3, m1, m2, m3, n1,
     &  n2, n3, densef, youngf, poissf, densem, youngm, poissm
      dimension c(nstre,nstre), adilu(nstre,nstre), amori(nstre,nstre), 
     &  sesh(nstre,nstre), calgm(nstre,nstre), calgf(nstre,nstre), 
     &  celm(nstre,nstre)
      dimension rstavam(mstre+1), rstavaf(mstre+1), sigma(nstre), 
     &  s(nstre)
      dimension idev(nstre,nstre), ixi(nstre,nstre)
c
c-----------------------------------------------------------------------
c computation of the consistent tangent modulus for a composite material
c with elastic fibers and von mises elasto-plastic matrix with 
c piece-wise non-linear isotropic hardening. plane stress only
c
c (m. estrada, 2011)
c-----------------------------------------------------------------------
c
c stops program if neither not plane stress
c      if(ntype.ne.1)call errprt('ei0032')
c
c set some material properties
      volfr=rprops(1)
      a1=rprops(2)
      a2=rprops(3)
      a3=rprops(4)
      l1=rprops(5)
      m1=rprops(6)
      n1=rprops(7)
      l2=rprops(8)
      m2=rprops(9)
      n2=rprops(10)
      l3=rprops(11)
      m3=rprops(12)
      n3=rprops(13)
c.. fibers properties
      densef=rprops(14)
      youngf=rprops(15)
      poissf=rprops(16)
c.. matrix properties
      densem=rprops(17)
      youngm=rprops(18)
      poissm=rprops(19)
      nhard=iprops(3)
c deviatoric projection tensor in a 6x6 matrix
      do i=1,6
        do j=1,6
          idev(i,j) = 0.d0
          ixi(i,j) = 0.d0
        enddo
      enddo
      do i=1,3
        do j=1,3
          ixi(i,j) = 1.d0
        enddo
      enddo
      idev(1,1) = 2.d0/3.d0
      idev(2,2) = idev(1,1)
      idev(3,3) = idev(1,1)
      idev(4,4) = 1.d0/2.d0
      idev(5,5) = idev(4,4)
      idev(6,6) = idev(4,4)
      idev(1,2) = -1.d0/3.d0
      idev(1,3) = idev(1,2)
      idev(2,3) = idev(1,2)
      idev(2,1) = idev(1,2)
      idev(3,1) = idev(1,2)
      idev(3,2) = idev(1,2)
c map stres into 6x1 array
      sigma(1) = stres(1)
      sigma(2) = stres(2)
      sigma(3) = stres(4)
      sigma(4) = 0.d0
      sigma(5) = 0.d0
      sigma(6) = stres(3)
c fibers moduli
      call celast(calgf, youngf, poissf)
c matrix elastic moduli
      call celast(celm, youngm, poissm)
c matrix elato-plastic tangent moduli
c.. hydrostatic pressure
      p = (sigma(1)+sigma(2)+sigma(3))/3.d0
c.. deviatoric stress
      do i=1,3
        s(i) = sigma(i)-p
        s(i+3) = sigma(i+3)
      enddo
c.. trial effective von mises stress
      snorm = sqrt(s(1)*s(1)+s(2)*s(2)+s(3)*s(3)+2.d0*(s(4)*s(4)+
     &  s(5)*s(5)+s(6)*s(6)))
      q = sqrt(3.d0/2.d0)*snorm
      qt = q+3.d0*gmodum*dgama
c.. hardening slope
      epbar = rstava(mstre+1)
      h = dplfun(epbar,nhard,rprops(iphard))
c.. assemble tangent elasto-plastic moduli
      afact = 2.d0*gmodum*(1.d0-dgama*3.d0*gmodum/qt)
      bfact = 6.d0*gmodum*gmodum*(dgama/qt-1.d0/(3.d0*gmodum+h))/snorm
      do i=1,nstre
        do j=1,nstre
          calgm(i,j) = afact*idev(i,j)+bfact*s(i)*s(j)+bulkm*ixi(i,j)
        enddo
      enddo
c extract isotropic part of calgm (only poisson is needed)
      bulkt = bulkm
      gmodut = gmodum-3.d0/5.d0*gmodum*gmodum*(1.d0/h+4.d0*dgama/qt)
      poisst = (3.d0*bulkt-2.d0*gmodut)/(2.d0*(3*bulkt+gmodut))
c compute eshelby tensor s
      call seshmat(sesh, poisst, a1, a2, a3)
c dilute strain concentration tensor
      call tenad(adilu, calgm, celf, sesh)
c mori-tanaka strain concentration tensor
      call tenam(amori, adilu, volfr)
c compute macro-moduli (tangent)      
      call homog(c, calgm, celf, volfr, amori)
c
C Compute elasticity matrix in local coordinates
C
      IF(NTYPE.EQ.1)THEN
C Plane stress
      dmatx(1,1) = 
     &     c(1,1) - c(1,3) * (c(3,5) * c(4,4) * c(5,1) + c(5,5) * c(3,4)
     & * c(4,1) - c(4,5) * c(3,4) * c(5,1) - c(3,5) * c(5,4) * c(4,1) + 
     &c(5,4) * c(4,5) * c(3,1) - c(4,4) * c(5,5) * c(3,1)) / (c(5,5) * c
     &(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,
     &3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) 
     &* c(3,4) * c(5,3)) - c(1,4) * (-c(3,5) * c(5,1) * c(4,3) + c(3,5) 
     &* c(5,3) * c(4,1) - c(5,3) * c(4,5) * c(3,1) - c(5,5) * c(3,3) * c
     &(4,1) + c(5,1) * c(4,5) * c(3,3) + c(5,5) * c(3,1) * c(4,3)) / (c(
     &5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4
     &) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) -
     & c(4,5) * c(3,4) * c(5,3)) + c(1,5) * (c(3,3) * c(4,4) * c(5,1) + 
     &c(3,1) * c(5,4) * c(4,3) + c(3,4) * c(5,3) * c(4,1) - c(3,4) * c(5
     &,1) * c(4,3) - c(3,1) * c(4,4) * c(5,3) - c(3,3) * c(5,4) * c(4,1)
     &) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) 
     &* c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c
     &(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(1,2) =
     &     c(1,2) - c(1,3) * (-c(4,4) * c(5,5) * c(3,2) + c(3,5) * c(4,4
     &) * c(5,2) + c(5,4) * c(4,5) * c(3,2) - c(4,5) * c(3,4) * c(5,2) +
     & c(5,5) * c(3,4) * c(4,2) - c(3,5) * c(5,4) * c(4,2)) / (c(5,5) * 
     &c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4
     &,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5)
     & * c(3,4) * c(5,3)) - c(1,4) * (c(5,5) * c(3,2) * c(4,3) - c(5,5) 
     &* c(3,3) * c(4,2) + c(5,2) * c(4,5) * c(3,3) - c(5,3) * c(4,5) * c
     &(3,2) + c(3,5) * c(5,3) * c(4,2) - c(3,5) * c(5,2) * c(4,3)) / (c(
     &5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4
     &) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) -
     & c(4,5) * c(3,4) * c(5,3)) + c(1,5) * (c(3,4) * c(5,3) * c(4,2) + 
     &c(3,3) * c(4,4) * c(5,2) - c(3,3) * c(5,4) * c(4,2) - c(3,2) * c(4
     &,4) * c(5,3) + c(3,2) * c(5,4) * c(4,3) - c(3,4) * c(5,2) * c(4,3)
     &) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) 
     &* c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c
     &(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(1,3) =
     &     c(1,6) - c(1,3) * (-c(4,5) * c(3,4) * c(5,6) - c(3,5) * c(5,4
     &) * c(4,6) + c(5,4) * c(4,5) * c(3,6) - c(4,4) * c(5,5) * c(3,6) +
     & c(3,5) * c(4,4) * c(5,6) + c(5,5) * c(3,4) * c(4,6)) / (c(5,5) * 
     &c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4
     &,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5)
     & * c(3,4) * c(5,3)) - c(1,4) * (-c(5,3) * c(4,5) * c(3,6) - c(3,5)
     & * c(5,6) * c(4,3) + c(3,5) * c(5,3) * c(4,6) + c(5,6) * c(4,5) * 
     &c(3,3) + c(5,5) * c(3,6) * c(4,3) - c(5,5) * c(3,3) * c(4,6)) / (c
     &(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,
     &4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) 
     &- c(4,5) * c(3,4) * c(5,3)) + c(1,4) * (-c(3,3) * c(5,4) * c(4,6) 
     &+ c(3,3) * c(4,4) * c(5,6) - c(3,6) * c(4,4) * c(5,3) + c(3,6) * c
     &(5,4) * c(4,3) + c(3,4) * c(5,3) * c(4,6) - c(3,4) * c(5,6) * c(4,
     &3)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5
     &) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) *
     & c(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(2,1) = 
     &     c(2,1) - c(2,3) * (c(3,5) * c(4,4) * c(5,1) + c(5,5) * c(3,4)
     & * c(4,1) - c(4,5) * c(3,4) * c(5,1) - c(3,5) * c(5,4) * c(4,1) + 
     &c(5,4) * c(4,5) * c(3,1) - c(4,4) * c(5,5) * c(3,1)) / (c(5,5) * c
     &(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,
     &3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) 
     &* c(3,4) * c(5,3)) - c(2,4) * (-c(3,5) * c(5,1) * c(4,3) + c(3,5) 
     &* c(5,3) * c(4,1) - c(5,3) * c(4,5) * c(3,1) - c(5,5) * c(3,3) * c
     &(4,1) + c(5,1) * c(4,5) * c(3,3) + c(5,5) * c(3,1) * c(4,3)) / (c(
     &5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4
     &) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) -
     & c(4,5) * c(3,4) * c(5,3)) + c(2,5) * (c(3,3) * c(4,4) * c(5,1) + 
     &c(3,1) * c(5,4) * c(4,3) + c(3,4) * c(5,3) * c(4,1) - c(3,4) * c(5
     &,1) * c(4,3) - c(3,1) * c(4,4) * c(5,3) - c(3,3) * c(5,4) * c(4,1)
     &) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) 
     &* c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c
     &(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(2,2) = 
     &     c(2,2) - c(2,3) * (-c(4,4) * c(5,5) * c(3,2) + c(3,5) * c(4,4
     &) * c(5,2) + c(5,4) * c(4,5) * c(3,2) - c(4,5) * c(3,4) * c(5,2) +
     & c(5,5) * c(3,4) * c(4,2) - c(3,5) * c(5,4) * c(4,2)) / (c(5,5) * 
     &c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4
     &,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5)
     & * c(3,4) * c(5,3)) - c(2,4) * (c(5,5) * c(3,2) * c(4,3) - c(5,5) 
     &* c(3,3) * c(4,2) + c(5,2) * c(4,5) * c(3,3) - c(5,3) * c(4,5) * c
     &(3,2) + c(3,5) * c(5,3) * c(4,2) - c(3,5) * c(5,2) * c(4,3)) / (c(
     &5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4
     &) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) -
     & c(4,5) * c(3,4) * c(5,3)) + c(2,5) * (c(3,4) * c(5,3) * c(4,2) + 
     &c(3,3) * c(4,4) * c(5,2) - c(3,3) * c(5,4) * c(4,2) - c(3,2) * c(4
     &,4) * c(5,3) + c(3,2) * c(5,4) * c(4,3) - c(3,4) * c(5,2) * c(4,3)
     &) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) 
     &* c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c
     &(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(2,3) = 
     &     c(2,6) - c(2,3) * (-c(4,5) * c(3,4) * c(5,6) - c(3,5) * c(5,4
     &) * c(4,6) + c(5,4) * c(4,5) * c(3,6) - c(4,4) * c(5,5) * c(3,6) +
     & c(3,5) * c(4,4) * c(5,6) + c(5,5) * c(3,4) * c(4,6)) / (c(5,5) * 
     &c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4
     &,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5)
     & * c(3,4) * c(5,3)) - c(2,4) * (-c(5,3) * c(4,5) * c(3,6) - c(3,5)
     & * c(5,6) * c(4,3) + c(3,5) * c(5,3) * c(4,6) + c(5,6) * c(4,5) * 
     &c(3,3) + c(5,5) * c(3,6) * c(4,3) - c(5,5) * c(3,3) * c(4,6)) / (c
     &(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,
     &4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) 
     &- c(4,5) * c(3,4) * c(5,3)) + c(2,4) * (-c(3,3) * c(5,4) * c(4,6) 
     &+ c(3,3) * c(4,4) * c(5,6) - c(3,6) * c(4,4) * c(5,3) + c(3,6) * c
     &(5,4) * c(4,3) + c(3,4) * c(5,3) * c(4,6) - c(3,4) * c(5,6) * c(4,
     &3)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5
     &) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) *
     & c(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(3,1) = 
     &     c(6,1) - c(6,3) * (c(3,5) * c(4,4) * c(5,1) + c(5,5) * c(3,4)
     & * c(4,1) - c(4,5) * c(3,4) * c(5,1) - c(3,5) * c(5,4) * c(4,1) + 
     &c(5,4) * c(4,5) * c(3,1) - c(4,4) * c(5,5) * c(3,1)) / (c(5,5) * c
     &(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,
     &3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) 
     &* c(3,4) * c(5,3)) - c(6,4) * (-c(3,5) * c(5,1) * c(4,3) + c(3,5) 
     &* c(5,3) * c(4,1) - c(5,3) * c(4,5) * c(3,1) - c(5,5) * c(3,3) * c
     &(4,1) + c(5,1) * c(4,5) * c(3,3) + c(5,5) * c(3,1) * c(4,3)) / (c(
     &5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4
     &) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) -
     & c(4,5) * c(3,4) * c(5,3)) + c(6,5) * (c(3,3) * c(4,4) * c(5,1) + 
     &c(3,1) * c(5,4) * c(4,3) + c(3,4) * c(5,3) * c(4,1) - c(3,4) * c(5
     &,1) * c(4,3) - c(3,1) * c(4,4) * c(5,3) - c(3,3) * c(5,4) * c(4,1)
     &) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) 
     &* c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c
     &(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(3,2) = 
     &     c(6,2) - c(6,3) * (-c(4,4) * c(5,5) * c(3,2) + c(3,5) * c(4,4
     &) * c(5,2) + c(5,4) * c(4,5) * c(3,2) - c(4,5) * c(3,4) * c(5,2) +
     & c(5,5) * c(3,4) * c(4,2) - c(3,5) * c(5,4) * c(4,2)) / (c(5,5) * 
     &c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4
     &,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5)
     & * c(3,4) * c(5,3)) - c(6,4) * (c(5,5) * c(3,2) * c(4,3) - c(5,5) 
     &* c(3,3) * c(4,2) + c(5,2) * c(4,5) * c(3,3) - c(5,3) * c(4,5) * c
     &(3,2) + c(3,5) * c(5,3) * c(4,2) - c(3,5) * c(5,2) * c(4,3)) / (c(
     &5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4
     &) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) -
     & c(4,5) * c(3,4) * c(5,3)) + c(6,5) * (c(3,4) * c(5,3) * c(4,2) + 
     &c(3,3) * c(4,4) * c(5,2) - c(3,3) * c(5,4) * c(4,2) - c(3,2) * c(4
     &,4) * c(5,3) + c(3,2) * c(5,4) * c(4,3) - c(3,4) * c(5,2) * c(4,3)
     &) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) 
     &* c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c
     &(3,3) - c(4,5) * c(3,4) * c(5,3))
      dmatx(3,3) = 
     &     c(6,6) - c(6,3) * (-c(4,5) * c(3,4) * c(5,6) - c(3,5) * c(5,4
     &) * c(4,6) + c(5,4) * c(4,5) * c(3,6) - c(4,4) * c(5,5) * c(3,6) +
     & c(3,5) * c(4,4) * c(5,6) + c(5,5) * c(3,4) * c(4,6)) / (c(5,5) * 
     &c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4
     &,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5)
     & * c(3,4) * c(5,3)) - c(6,4) * (-c(5,3) * c(4,5) * c(3,6) - c(3,5)
     & * c(5,6) * c(4,3) + c(3,5) * c(5,3) * c(4,6) + c(5,6) * c(4,5) * 
     &c(3,3) + c(5,5) * c(3,6) * c(4,3) - c(5,5) * c(3,3) * c(4,6)) / (c
     &(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,
     &4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) 
     &- c(4,5) * c(3,4) * c(5,3)) + c(6,4) * (-c(3,3) * c(5,4) * c(4,6) 
     &+ c(3,3) * c(4,4) * c(5,6) - c(3,6) * c(4,4) * c(5,3) + c(3,6) * c
     &(5,4) * c(4,3) + c(3,4) * c(5,3) * c(4,6) - c(3,4) * c(5,6) * c(4,
     &3)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5
     &) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) *
     & c(3,3) - c(4,5) * c(3,4) * c(5,3))
      ELSEIF (NTYPE.EQ.2) THEN
C Plane strain
      dmatx(1,1) = c(1,1)
      dmatx(1,2) = c(1,2)
      dmatx(1,3) = c(1,6)
      dmatx(2,1) = c(2,1)
      dmatx(2,2) = c(2,2)
      dmatx(2,3) = c(2,6)
      dmatx(3,1) = c(6,1)
      dmatx(3,2) = c(6,2)
      dmatx(3,3) = c(6,6)
      ELSEIF (NTYPE.EQ.3) THEN
C Axisymetric
      dmatx(1,1) = c(1,1)
      dmatx(1,2) = c(1,2)
      dmatx(1,3) = c(1,6)
      dmatx(1,4) = c(1,3)
      dmatx(2,1) = c(2,1)
      dmatx(2,2) = c(2,2)
      dmatx(2,3) = c(2,6)
      dmatx(2,4) = c(2,3)
      dmatx(3,1) = c(6,1)
      dmatx(3,2) = c(6,2)
      dmatx(3,3) = c(6,6)
      dmatx(3,4) = c(6,3)
      dmatx(4,1) = c(4,1)
      dmatx(4,2) = c(4,2)
      dmatx(4,3) = c(4,6)
      dmatx(4,4) = c(4,3)
      ELSE
        CALL ERRPRT('EI0019')
      ENDIF
c
      return
      end
      