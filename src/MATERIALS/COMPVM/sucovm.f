      subroutine sucovm
     &(   dgama      ,iprops     ,lalgva     ,ntype      ,rprops      ,
     &    rstava     ,strat      ,stres      )
      implicit double precision (a-h , o-z)
      parameter (iphard=20, mstre=4)
c arguments and variables
      logical ifplas, sufail, lalgva(2)
      dimension stres(mstre), rstava(mstre+1),  strat(mstre), 
     &  iprops(*), rprops(*)
      data tol, mxiter, alpha / 1.d-06, 50, 0.5d0 /
c local variables
      parameter (nstre=6)
      logical epflag
      double precision volfr, a1, a2, a3, l1, l2, l3, m1, m2, m3, n1,
     &  n2, n3, densef, youngf, poissf, densem, youngm, poissm
      dimension stratc(nstre), stratf(nstre), stratm(nstre), r(nstre),
     &  eetm(nstre), st(nstre)
      dimension adilu(nstre,nstre), amori(nstre,nstre), 
     &  sesh(nstre,nstre), c(nstre,nstre)
      dimension celf(nstre,nstre),calgm(nstre,nstre),celfn(nstre,nstre),
     &  calgmn(nstre,nstre)
      dimension idev(nstre,nstre), ixi(nstre,nstre)
c
c-----------------------------------------------------------------------
c state update procedure for a composite material with elastic fibers
c and von mises elasto-plastic matrix with piece-wise non-linear 
c isotropic hardening.
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
c
c update process (ref: 7.2 - Doghri 2003)
c initialize average strain in inclusion
      do i=1,nstre
        stratf(i) = stratc(i)
      enddo
      do niter1=1,mxiter
c 1. compute fibers moduli
        call celast(celf, youngf, poissf)
c 2. compute average strain in matrix
        do i=1,nstre
          stratm(i) = (strat(i)-volfr*stratf(i))/(1.d0-volfr)
        enddo
c 3. compute matrix algorithmic moduli
c.. initialize algorithmic variables
        dgama=0.d0
        ifplas = .false.
        sufail = .false.
        epbarn = rstava(mstre+1)
c.. elastic predictor
c.... volumetric strain
        eevm = stratm(1)+stratm(2)+stratm(3)
c.... hydrostatic pressure
        p = bulkm*eevm
c.... elastic trial deviatoric strain
        do i=1,3
          st(i) = stratm(i)-eevm/3.d0
          st(i+3) = stratm(i+3)/2.d0
        enddo
c.... trial effective stress
        stnorm = sqrt(st(1)**2+st(2)**2+st(3)**2+2.d0*(st(4)**2+
     &    st(5)**2+st(6)**2))
        qt = sqrt(3.d0/2.d0)*stnorm
c.... uniaxial yield stress
        sigmay = plfun(epbarn,nhard,rprops(iphard))
c.... check for plastic admissibility
        phi = qt-sigmay
        if (phi/sigmay.gt.tol) then
c.... plastic step
c.... ------------
          ifplas = .true.
          epbar = epbarn
          do niter2=1,mxiter
c...... hardening slope
            h = dplfun(epbar,nhard,rprops(iphard))
c...... compute residual derivative
            d = -3.d0*gmodum-h
c...... dgama increment
            ddgama = -phi/d
            dgama = dgama+ddgama
c...... new residual
            epbar = epbar+ddgama
            sigmay = plfun(epbar,nhard,rprops(iphard))
            phi = qt-3.d0*gmodum-sigmay
c...... check convergence
            if (abs(phi/sigmay).le.tol) exit
          enddo
        else
c.... elastic step
c.... ------------
          epbar = epbarn
        endif
c.. hardening slope
        h = dplfun(epbar,nhard,rprops(iphard))
c.. assemble tangent elasto-plastic moduli
        afact = 2.d0*gmodum*(1.d0-dgama*3.d0*gmodum/qt)
        bfact = 6.d0*gmodum*gmodum*(dgama/qt-1.d0/(3.d0*gmodum+h))/snorm
        do i=1,nstre
          do j=1,nstre
            calgm(i,j)=afact*idev(i,j)+bfact*st(i)*st(j)+bulkm*ixi(i,j)
          enddo
        enddo
c 4. extract isotropic part of calgm (only poisson is needed)
        bulkt = bulkm
        gmodut = gmodum-3.d0/5.d0*gmodum*gmodum*(1.d0/h+4.d0*dgama/qt)
        poisst = (3.d0*bulkt-2.d0*gmodut)/(2.d0*(3*bulkt+gmodut))
c 5. compute eshelby tensor s
        call seshmat(sesh, poisst, a1, a2, a3)
c 6. values of inclusion and matrix moduli at n+1
        if (niter1.eq.1) then
          do i=1,nstre
            do j=1,nstre
              calgmn(i,j) = calgm(i,j)
              celfn(i,j) = celf(i,j)
            enddo
          enddo
        else
          do i=1,nstre
            do j=1,nstre
              calgmn(i,j)= (1.d0-alpha)*calgmn(i,j)+alpha*calgm(i,j)
              celfn(i,j)= (1.d0-alpha)*celfn(i,j)+alpha*celf(i,j)
            enddo
          enddo
        endif
c 7. strain concentration tensor
        call tenad(adilu, calgmn, celfn, sesh)
        call tenam(amori, adilu, volfr)
c 8. check compatibility of strains in inclusion
        do i=1,nstre
          r(i) = 0.d0
        enddo
        do i=1,nstre
          do j=1,nstre
            r(i) = r(i)+amori(i,j)*stratc(j)
          enddo
          r(i) = r(i)-stratf(i)
        enddo
        rnorm = sqrt(r(1)**2+r(2)**2+r(3)**2+2.d0*(r(4)**2+r(5)**2+
     &    r(6)**2))
        stratfnorm = sqrt(stratf(1)**2+stratf(2)**2+stratf(3)**2+2.d0*
     &    (stratf(4)**2+stratf(5)**2+stratf(6)**2))
c 9. exit if convergence
        if (ABS(rnorm/stratfnorm).le.tol) exit
c 10. increase average strain in inclusion for new iteration
        do i=1,nstre
          stratf(i) = stratf(i)+r(i)
        enddo
      enddo
c compute macro-moduli (tangent) after convergence   
      call homog(c, celmn, celfn, volfr, amori)
C
C Update stress values from constitutive matrix and strain
      if (ntype.eq.1) then
        stres(1) = stres(1)+
     &     (c(1,2) - c(1,3) * (-c(4,4) * c(5,5) * c(3,2) + c(3,5) * c(4,
     &4) * c(5,2) + c(5,4) * c(4,5) * c(3,2) - c(4,5) * c(3,4) * c(5,2) 
     &+ c(5,5) * c(3,4) * c(4,2) - c(3,5) * c(5,4) * c(4,2)) / (c(5,5) *
     & c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(
     &4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5
     &) * c(3,4) * c(5,3)) - c(1,4) * (c(5,5) * c(3,2) * c(4,3) - c(5,5)
     & * c(3,3) * c(4,2) + c(5,2) * c(4,5) * c(3,3) - c(5,3) * c(4,5) * 
     &c(3,2) + c(3,5) * c(5,3) * c(4,2) - c(3,5) * c(5,2) * c(4,3)) / (c
     &(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,
     &4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) 
     &- c(4,5) * c(3,4) * c(5,3)) + c(1,5) * (c(3,4) * c(5,3) * c(4,2) +
     & c(3,3) * c(4,4) * c(5,2) - c(3,3) * c(5,4) * c(4,2) - c(3,2) * c(
     &4,4) * c(5,3) + c(3,2) * c(5,4) * c(4,3) - c(3,4) * c(5,2) * c(4,3
     &)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5)
     & * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * 
     &c(3,3) - c(4,5) * c(3,4) * c(5,3))) * (strat(2)-rstava(2))
     &+ (-c(1,3) * (-c(4,
     &5) * c(3,4) * c(5,6) - c(3,5) * c(5,4) * c(4,6) + c(5,4) * c(4,5) 
     &* c(3,6) - c(4,4) * c(5,5) * c(3,6) + c(3,5) * c(4,4) * c(5,6) + c
     &(5,5) * c(3,4) * c(4,6)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(
     &5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3
     &) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) - c(1,4) 
     &* (-c(5,3) * c(4,5) * c(3,6) - c(3,5) * c(5,6) * c(4,3) + c(3,5) *
     & c(5,3) * c(4,6) + c(5,6) * c(4,5) * c(3,3) + c(5,5) * c(3,6) * c(
     &4,3) - c(5,5) * c(3,3) * c(4,6)) / (c(5,5) * c(3,4) * c(4,3) - c(4
     &,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4)
     & * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) +
     & c(1,5) * (-c(3,3) * c(5,4) * c(4,6) + c(3,3) * c(4,4) * c(5,6) - 
     &c(3,6) * c(4,4) * c(5,3) + c(3,6) * c(5,4) * c(4,3) + c(3,4) * c(5
     &,3) * c(4,6) - c(3,4) * c(5,6) * c(4,3)) / (c(5,5) * c(3,4) * c(4,
     &3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) 
     &* c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c
     &(5,3)) + c(1,6)) * (strat(3)-rstava(3))
     &+ (c(1,1) - c(1,3) * (c(3,5) * c(4,4) 
     &* c(5,1) + c(5,5) * c(3,4) * c(4,1) - c(4,5) * c(3,4) * c(5,1) - c
     &(3,5) * c(5,4) * c(4,1) + c(5,4) * c(4,5) * c(3,1) - c(4,4) * c(5,
     &5) * c(3,1)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3
     &) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) *
     & c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) - c(1,4) * (-c(3,5) *
     & c(5,1) * c(4,3) + c(3,5) * c(5,3) * c(4,1) - c(5,3) * c(4,5) * c(
     &3,1) - c(5,5) * c(3,3) * c(4,1) + c(5,1) * c(4,5) * c(3,3) + c(5,5
     &) * c(3,1) * c(4,3)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5)
     & * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + 
     &c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) + c(1,5) * (c
     &(3,3) * c(4,4) * c(5,1) + c(3,1) * c(5,4) * c(4,3) + c(3,4) * c(5,
     &3) * c(4,1) - c(3,4) * c(5,1) * c(4,3) - c(3,1) * c(4,4) * c(5,3) 
     &- c(3,3) * c(5,4) * c(4,1)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) *
     & c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(
     &5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3))) * 
     &(strat(1)-rstava(1))
        stres(2) = stres(2)+
     &     (c(2,2) - c(2,3) * (-c(4,4) * c(5,5) * c(3,2) + c(3,5) * c(4,
     &4) * c(5,2) + c(5,4) * c(4,5) * c(3,2) - c(4,5) * c(3,4) * c(5,2) 
     &+ c(5,5) * c(3,4) * c(4,2) - c(3,5) * c(5,4) * c(4,2)) / (c(5,5) *
     & c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(
     &4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5
     &) * c(3,4) * c(5,3)) - c(2,4) * (c(5,5) * c(3,2) * c(4,3) - c(5,5)
     & * c(3,3) * c(4,2) + c(5,2) * c(4,5) * c(3,3) - c(5,3) * c(4,5) * 
     &c(3,2) + c(3,5) * c(5,3) * c(4,2) - c(3,5) * c(5,2) * c(4,3)) / (c
     &(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,
     &4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) 
     &- c(4,5) * c(3,4) * c(5,3)) + c(2,5) * (c(3,4) * c(5,3) * c(4,2) +
     & c(3,3) * c(4,4) * c(5,2) - c(3,3) * c(5,4) * c(4,2) - c(3,2) * c(
     &4,4) * c(5,3) + c(3,2) * c(5,4) * c(4,3) - c(3,4) * c(5,2) * c(4,3
     &)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5)
     & * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * 
     &c(3,3) - c(4,5) * c(3,4) * c(5,3))) * (strat(2)-rstava(2))
     & + (-c(2,3) * (-c(4,
     &5) * c(3,4) * c(5,6) - c(3,5) * c(5,4) * c(4,6) + c(5,4) * c(4,5) 
     &* c(3,6) - c(4,4) * c(5,5) * c(3,6) + c(3,5) * c(4,4) * c(5,6) + c
     &(5,5) * c(3,4) * c(4,6)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(
     &5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3
     &) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) - c(2,4) 
     &* (-c(5,3) * c(4,5) * c(3,6) - c(3,5) * c(5,6) * c(4,3) + c(3,5) *
     & c(5,3) * c(4,6) + c(5,6) * c(4,5) * c(3,3) + c(5,5) * c(3,6) * c(
     &4,3) - c(5,5) * c(3,3) * c(4,6)) / (c(5,5) * c(3,4) * c(4,3) - c(4
     &,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4)
     & * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) +
     & c(2,5) * (-c(3,3) * c(5,4) * c(4,6) + c(3,3) * c(4,4) * c(5,6) - 
     &c(3,6) * c(4,4) * c(5,3) + c(3,6) * c(5,4) * c(4,3) + c(3,4) * c(5
     &,3) * c(4,6) - c(3,4) * c(5,6) * c(4,3)) / (c(5,5) * c(3,4) * c(4,
     &3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) 
     &* c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c
     &(5,3)) + c(2,6)) * (strat(3)-rstava(3))
     & + (c(2,1) - c(2,3) * (c(3,5) * c(4,4) 
     &* c(5,1) + c(5,5) * c(3,4) * c(4,1) - c(4,5) * c(3,4) * c(5,1) - c
     &(3,5) * c(5,4) * c(4,1) + c(5,4) * c(4,5) * c(3,1) - c(4,4) * c(5,
     &5) * c(3,1)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3
     &) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) *
     & c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) - c(2,4) * (-c(3,5) *
     & c(5,1) * c(4,3) + c(3,5) * c(5,3) * c(4,1) - c(5,3) * c(4,5) * c(
     &3,1) - c(5,5) * c(3,3) * c(4,1) + c(5,1) * c(4,5) * c(3,3) + c(5,5
     &) * c(3,1) * c(4,3)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5)
     & * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + 
     &c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) + c(2,5) * (c
     &(3,3) * c(4,4) * c(5,1) + c(3,1) * c(5,4) * c(4,3) + c(3,4) * c(5,
     &3) * c(4,1) - c(3,4) * c(5,1) * c(4,3) - c(3,1) * c(4,4) * c(5,3) 
     &- c(3,3) * c(5,4) * c(4,1)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) *
     & c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(
     &5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3))) * 
     &(strat(1)-rstava(1))
        stres(3) = stres(3)+
     &     (c(6,2) - c(6,3) * (-c(4,4) * c(5,5) * c(3,2) + c(3,5) * c(4,
     &4) * c(5,2) + c(5,4) * c(4,5) * c(3,2) - c(4,5) * c(3,4) * c(5,2) 
     &+ c(5,5) * c(3,4) * c(4,2) - c(3,5) * c(5,4) * c(4,2)) / (c(5,5) *
     & c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(
     &4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5
     &) * c(3,4) * c(5,3)) - c(6,4) * (c(5,5) * c(3,2) * c(4,3) - c(5,5)
     & * c(3,3) * c(4,2) + c(5,2) * c(4,5) * c(3,3) - c(5,3) * c(4,5) * 
     &c(3,2) + c(3,5) * c(5,3) * c(4,2) - c(3,5) * c(5,2) * c(4,3)) / (c
     &(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,
     &4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) 
     &- c(4,5) * c(3,4) * c(5,3)) + c(6,5) * (c(3,4) * c(5,3) * c(4,2) +
     & c(3,3) * c(4,4) * c(5,2) - c(3,3) * c(5,4) * c(4,2) - c(3,2) * c(
     &4,4) * c(5,3) + c(3,2) * c(5,4) * c(4,3) - c(3,4) * c(5,2) * c(4,3
     &)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3) - c(3,5)
     & * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) * c(4,5) * 
     &c(3,3) - c(4,5) * c(3,4) * c(5,3))) * (strat(2)-rstava(2))
     & + (-c(6,3) * (-c(4,
     &5) * c(3,4) * c(5,6) - c(3,5) * c(5,4) * c(4,6) + c(5,4) * c(4,5) 
     &* c(3,6) - c(4,4) * c(5,5) * c(3,6) + c(3,5) * c(4,4) * c(5,6) + c
     &(5,5) * c(3,4) * c(4,6)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(
     &5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3
     &) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) - c(6,4) 
     &* (-c(5,3) * c(4,5) * c(3,6) - c(3,5) * c(5,6) * c(4,3) + c(3,5) *
     & c(5,3) * c(4,6) + c(5,6) * c(4,5) * c(3,3) + c(5,5) * c(3,6) * c(
     &4,3) - c(5,5) * c(3,3) * c(4,6)) / (c(5,5) * c(3,4) * c(4,3) - c(4
     &,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4)
     & * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) +
     & c(6,5) * (-c(3,3) * c(5,4) * c(4,6) + c(3,3) * c(4,4) * c(5,6) - 
     &c(3,6) * c(4,4) * c(5,3) + c(3,6) * c(5,4) * c(4,3) + c(3,4) * c(5
     &,3) * c(4,6) - c(3,4) * c(5,6) * c(4,3)) / (c(5,5) * c(3,4) * c(4,
     &3) - c(4,4) * c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) 
     &* c(4,4) * c(5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c
     &(5,3)) + c(6,6)) * (strat(3)-rstava(3))
     & + (c(6,1) - c(6,3) * (c(3,5) * c(4,4) 
     &* c(5,1) + c(5,5) * c(3,4) * c(4,1) - c(4,5) * c(3,4) * c(5,1) - c
     &(3,5) * c(5,4) * c(4,1) + c(5,4) * c(4,5) * c(3,1) - c(4,4) * c(5,
     &5) * c(3,1)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5) * c(3,3
     &) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + c(5,4) *
     & c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) - c(6,4) * (-c(3,5) *
     & c(5,1) * c(4,3) + c(3,5) * c(5,3) * c(4,1) - c(5,3) * c(4,5) * c(
     &3,1) - c(5,5) * c(3,3) * c(4,1) + c(5,1) * c(4,5) * c(3,3) + c(5,5
     &) * c(3,1) * c(4,3)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) * c(5,5)
     & * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(5,3) + 
     &c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3)) + c(6,5) * (c
     &(3,3) * c(4,4) * c(5,1) + c(3,1) * c(5,4) * c(4,3) + c(3,4) * c(5,
     &3) * c(4,1) - c(3,4) * c(5,1) * c(4,3) - c(3,1) * c(4,4) * c(5,3) 
     &- c(3,3) * c(5,4) * c(4,1)) / (c(5,5) * c(3,4) * c(4,3) - c(4,4) *
     & c(5,5) * c(3,3) - c(3,5) * c(5,4) * c(4,3) + c(3,5) * c(4,4) * c(
     &5,3) + c(5,4) * c(4,5) * c(3,3) - c(4,5) * c(3,4) * c(5,3))) * 
     &(strat(1)-rstava(1))
      endif
C
C Update strain values
      rstava(1) = strat(1)
      rstava(2) = strat(2)
      rstava(3) = strat(3)
      if (ntype.eq.1) then
        rstava(4) = -(c(3,1)*strat(1)+c(3,2)*strat(2))/c(3,3)
      endif
      rstava(mstre+1) = epbar
c
      return
      end
      