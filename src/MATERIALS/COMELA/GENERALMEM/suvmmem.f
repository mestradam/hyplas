      subroutine suvmmem
     1(   dgama      ,iprops     ,lalgva     ,ntype      ,rprops     ,
     2    rstava     ,strat      ,stres      )
      implicit double precision (a-h,o-z)
      parameter( iphard=20  ,mstre=4  ,nstre=3 )
      logical ifplas, lalgva(2), sufail
      dimension
     1    iprops(*)          ,rprops(*)          ,rstava(mstre+1)    ,
     2    strat(mstre)       ,stres(mstre)
      dimension
     1    eet(mstre)         ,strest(nstre)
      data
     1    r0   ,rp5  ,r1   ,r2   ,r3   ,r4   ,r6   ,tol   / 
     2    0.0d0,0.5d0,1.0d0,2.0d0,3.0d0,4.0d0,6.0d0,1.d-06/
      data mxiter / 50 /
c***********************************************************************
c state update procedure for the von mises elasto-plastic model with
c non-linear (piecewise linear) isotropic hardening in plane stress:
c implicit plane stress-projected elastic predictor/return mapping
c algorithm (boxes 9.4-5).
c
c reference: section 9.4.3
c            boxes 9.4-5
c***********************************************************************
c stop program if not plane stress
      if(ntype.ne.1)call errprt('ei0031')
c initialise some algorithmic and internal variables
      dgama=r0
      ifplas=.false.
      sufail=.false.
c...set previously (equilibrium) converged accumulated plastic strain
      epbarn=rstava(mstre+1)
c set some material properties
      young=rprops(18)
      poiss=rprops(19)
      nhard=iprops(3)
c shear and bulk moduli and other necessary constants
      gmodu=young/(r2*(r1+poiss))
      bulk=young/(r3*(r1-r2*poiss))
      r2g=r2*gmodu
      r4g=r4*gmodu
      r1d3=r1/r3
      r1d6=r1/r6
      r2d3=r2*r1d3
      sqr2d3=sqrt(r2d3)
      r4gd3=r4g*r1d3
c elastic predictor: compute elastic trial state
c ----------------------------------------------
c volumetric strain
      factor=r2g/(bulk+r4gd3)
      eev=(strat(1)+strat(2))*factor
c elastic trial deviatoric strain
      eevd3=eev/r3
      eet(1)=strat(1)-eevd3
      eet(2)=strat(2)-eevd3
c convert engineering shear component into physical component
      eet(3)=strat(3)*rp5
c elastic trial stress components
      pt=bulk*eev
      strest(1)=r2g*eet(1)+pt
      strest(2)=r2g*eet(2)+pt
      strest(3)=r2g*eet(3)
c compute yield function value at trial state
      a1=(strest(1)+strest(2))*(strest(1)+strest(2))
      a2=(strest(2)-strest(1))*(strest(2)-strest(1))
      a3=strest(3)*strest(3)
      xi=r1d6*a1+rp5*a2+r2*a3
      sigmay=plfun(epbarn,nhard,rprops(iphard))
c...yield function
      phi=rp5*xi-r1d3*sigmay*sigmay
c check for plastic admissibility
c -------------------------------
      if(phi/sigmay.gt.tol)then
c plastic step: apply return mapping - use newton-raphson algorithm
c               to solve the plane stress-projected return mapping
c               equation for the plastic multiplier (box 9.5)
c -----------------------------------------------------------------
        ifplas=.true.
        epbar=epbarn
        sqrtxi=sqrt(xi)
        b1=r1
        b2=r1
        fmodu=young/(r3*(r1-poiss))
        do 10 nriter=1,mxiter
c compute residual derivative
          hslope=dplfun(epbar,nhard,rprops(iphard))
          dxi=-a1*fmodu/(r3*b1*b1*b1)-r2g*(a2+r4*a3)/(b2*b2*b2)
          hbar=r2*sigmay*hslope*sqr2d3*(sqrtxi+dgama*dxi/(r2*sqrtxi))
          dphi=rp5*dxi-r1d3*hbar
c compute newton-raphson increment and update equation variable dgama
          dgama=dgama-phi/dphi
c compute new residual (yield function value)
          b1=r1+fmodu*dgama
          b2=r1+r2g*dgama
          xi=r1d6*a1/(b1*b1)+(rp5*a2+r2*a3)/(b2*b2)
          sqrtxi=sqrt(xi)
          epbar=epbarn+dgama*sqr2d3*sqrtxi
          sigmay=plfun(epbar,nhard,rprops(iphard))
          phi=rp5*xi-r1d3*sigmay*sigmay
c check for convergence
          resnor=abs(phi/sigmay)
          if(resnor.le.tol)then
c update accumulated plastic strain
            rstava(mstre+1)=epbar
c update stress components:   sigma := a sigma^trial
            astar1=r3*(r1-poiss)/(r3*(r1-poiss)+young*dgama)
            astar2=r1/(r1+r2g*dgama)
            a11=rp5*(astar1+astar2)
            a22=a11
            a12=rp5*(astar1-astar2)
            a21=a12
            a33=astar2
            stres(1)=a11*strest(1)+a12*strest(2)
            stres(2)=a21*strest(1)+a22*strest(2)
            stres(3)=a33*strest(3)
c compute corresponding elastic (engineering) strain components
            factg=r1/r2g
            p=r1d3*(stres(1)+stres(2))
            eev=p/bulk
            eevd3=r1d3*eev
            rstava(1)=factg*(r2d3*stres(1)-r1d3*stres(2))+eevd3
            rstava(2)=factg*(r2d3*stres(2)-r1d3*stres(1))+eevd3
            rstava(3)=factg*stres(3)*r2
            rstava(4)=-poiss/(r1-poiss)*(rstava(1)+rstava(2))
            goto 999
          endif
   10   continue
c reset failure flag and print warning message if n-r algorithm fails
        sufail=.true.
        call errprt('we0013')
      else
c elastic step: update stress using linear elastic law
c ----------------------------------------------------
        stres(1)=strest(1)
        stres(2)=strest(2)
        stres(3)=strest(3)
c elastic engineering strain
        rstava(1)=strat(1)
        rstava(2)=strat(2)
        rstava(3)=strat(3)
        rstava(4)=-poiss/(r1-poiss)*(strat(1)+strat(2))
      endif
  999 continue
c update some algorithmic variables before exit
      lalgva(1)=ifplas
      lalgva(2)=sufail
      return
      end
      