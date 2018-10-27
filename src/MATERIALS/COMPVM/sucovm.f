      subroutine sucovm
     .(   ralgva     ,iprops     ,lalgva     ,ntype      ,rprops     ,
     .    rstava     ,strat      ,stres      )
!***********************************************************************
! State update procedure 
! for composite material (plane strain and axisymmetric only).
!   
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   ralgva (in)     : Real algorithmic variables
!   iprops (in)     : Integer material properties
!   lalgva (out)    : Logical algorithmic variables
!   ntype  (in)     : Analysis type
!                       1   plane stress
!                       2   plane strain
!                       3   axisymmetric
!   rprops (in)     : Real material properties
!   rstava (inout)  : Real state variables
!   strat  (in)     : Strain array for composite material
!   stress (out)    : Stress array for composite material
!***********************************************************************
!     
! Variable declaration
      implicit double precision (a-h,o-z)
      parameter(iphard=21  ,mstre=4)
!      
! ... arguments
      logical lalgva(3)
      dimension
     .    ralgva(*)  ,iprops(*)  ,rprops(*)  ,rstava(14) ,strat(4)   ,
     .    stres(4)
!      
! ... local variables
      logical ifplas, sufail ,ifdama
      real(kind=8)
     .    eet(4)     ,stresf(4)
      data
     .    r0   ,rp5  ,r1   ,r2   ,r3   ,tol   / 
     .    0.0d0,0.5d0,1.0d0,2.0d0,3.0d0,1.d-06/
      data mxiter / 50 /
!      
! Stop program if neither plane strain nor axisymmetric state
      if(ntype.ne.2 .and. ntype.ne.3)call errprt('EI0013')
!      
! ===================================================================
! Start state update process for MATRIX 
! (Von Mises plane strain and axisymmetric)
! ===================================================================
!      
! Initialise some algorithmic and internal variables
      dgama  = r0
      ifplas = .false.
      sufail = .false.
      epbarn = rstava(9)
! Set some material properties
      young = rprops(2)
      poiss = rprops(3)
      nhard = iprops(3)
! Shear and bulk moduli and other necessary constants
      gmodu = young/(r2*(r1+poiss))
      bulk  = young/(r3*(r1-r2*poiss))
      r2g = r2*gmodu
      r3g = r3*gmodu
! Elastic predictor: Compute elastic trial state
! ----------------------------------------------
! Volumetric strain and pressure stress
      eev = strat(1)+strat(2)+strat(4)
      p = bulk*eev
! Elastic trial deviatoric strain
      eevd3  = eev/r3
      eet(1) = strat(1)-eevd3
      eet(2) = strat(2)-eevd3
      eet(4) = strat(4)-eevd3
! Convert engineering shear component into physical component
      eet(3) = strat(3)/r2
! Compute trial effective stress and uniaxial yield stress
      varj2t=r2g*r2g*(eet(3)*eet(3)+rp5*(eet(1)*eet(1)+
     .                     eet(2)*eet(2)+eet(4)*eet(4)))
      qtrial=sqrt(r3*varj2t)
      sigmay=plfun(epbarn,nhard,rprops(iphard))
! Check for plastic admissibility
! -------------------------------
      phi=qtrial-sigmay
      if(phi/sigmay.gt.tol)then
! Plastic step: Apply return mapping - use Newton-Raphson algorithm
!               to solve the return mapping equation (Box 7.4)
! -------------------------------------------------------------------
        ifplas=.true.
        epbar=epbarn
        do nriter=1,mxiter
! Compute residual derivative
          denom=-r3g-dplfun(epbar,nhard,rprops(iphard))
! Compute Newton-Raphson increment and update variable DGAMA
          ddgama=-phi/denom
          dgama=dgama+ddgama
! Compute new residual
          epbar=epbar+ddgama
          sigmay=plfun(epbar,nhard,rprops(iphard))
          phi=qtrial-r3g*dgama-sigmay
! Check convergence
          resnor=abs(phi/sigmay)
          if(resnor.le.tol)then
! update accumulated plastic strain
            rstava(9)=epbar
! update stress components
            factor=r2g*(r1-r3g*dgama/qtrial)
            stres(1)=factor*eet(1)+p
            stres(2)=factor*eet(2)+p
            stres(3)=factor*eet(3)
            stres(4)=factor*eet(4)+p
! compute converged elastic (engineering) strain components
            factor=factor/r2g
            rstava(1)=factor*eet(1)+eevd3
            rstava(2)=factor*eet(2)+eevd3
            rstava(3)=factor*eet(3)*r2
            rstava(4)=factor*eet(4)+eevd3
            goto 999
          endif
        enddo
! reset failure flag and print warning message if the algorithm fails
        sufail=.true.
        call errprt('WE0004')
      else
! Elastic step: Update stress using linear elastic law
! ----------------------------------------------------
        stres(1)=r2g*eet(1)+p
        stres(2)=r2g*eet(2)+p
        stres(3)=r2g*eet(3)
        stres(4)=r2g*eet(4)+p
! elastic engineering strain
        rstava(1)=strat(1)
        rstava(2)=strat(2)
        rstava(3)=strat(3)
        rstava(4)=strat(4)
      endif
  999 continue
! Update some algorithmic variables before exit
      lalgva(1)  = ifplas
      lalgva(2)  = sufail
      rstava(11) = dgama
!      
! ===================================================================
! Start state update process for FIBERS 
! (Damage Weibull uniaxial)
! ===================================================================
!
! Set some material properties
      youngf  = rprops(12)
      poissf  = rprops(13)
      sigmauf = rprops(14)
      alphaf  = rprops(15)
      betaf   = rprops(16)
      anglef  = rprops(18)
      mx = dcos(anglef)
      my = dsin(anglef)
      volfrf = rprops(17)
      volfrm = r1 - volfrf
      if (volfrf.le.tol) goto 888
!      
! Initialize algorithmic and internal variables
      ifdama = .false.
      sufail = .false.
! ... set previously accumulated damage variable
      damage = rstava(10)
! ... set previously accumulated threshold yield function
      qmaxf = rstava(12)
!
! Elastic predictor: Compute elastic trial state
! ----------------------------------------------
!
! Elastic trial strain
      eet(1) = strat(1)
      eet(2) = strat(2)
!
! Convert engineering shear component into physical component
      eet(3) = (strat(3))/r2
!
! Elastic trial strain component parallel to fibers direction "anglef"
      eetf = eet(1)*mx*mx+eet(2)*my*my + r2*eet(3)*mx*my
!
! Effective stress on fibers
      streff = youngf*eetf
      goto 111
!
! Compute damage function value at trial state
! ... threshold
      if (sigmauf.gt.qmaxf) then
          qtrial = sigmauf
      else if (qmaxf.gt.dabs(streff)) then
          qtrial = qmaxf
      else
          qtrial = dabs(streff)
      end if
!
! .. yield function: f = bar{sigma} - q
      phi = dabs(streff) - qtrial
!
! Check for plastic admissibility
! -------------------------------
      if (phi.lt.0.d0) then
!
! Elastic step: Update stress using linear elastic law
! ----------------------------------------------------
          stresf(1) = streff*mx*mx
          stresf(2) = streff*my*my
          stresf(3) = streff*mx*my
          stresf(4) = 0.d0
      else
!
! Plastic step: Update damage variable and nominal
! ------------------------------------------------
          ifdama = .true.
!
! Damage variable (Weibull distribution)
          damage = r1-dexp(-(streff/betaf)**alphaf)
          rstava(10) = damage
!
! Update longitudinal stress on fibers
          streff = (r1-damage) * streff
!
! Update stress matrix
          stresf(1) = streff*mx*mx
          stresf(2) = streff*my*my
          stresf(3) = streff*mx*my
          stresf(4) = r0
      end if
!      *********************************************** borrar
  111    continue
      stresf(1) = streff*mx*mx
          stresf(2) = streff*my*my
          stresf(3) = streff*mx*my
          stresf(4) = r0
!     ************************************************* borrar
!
! Update some algorithmic variables before exit
      lalgva(3)  = ifdama
      rstava(14) = qtrial
!
! Compute COMPOSITE stress: sigma = k^m*sigma^m + k^f*sigma^f
! ===========================================================
!
! Homogenization by mixture law
      stres(1) = volfrm*stres(1) + volfrf*stresf(1)
      stres(2) = volfrm*stres(2) + volfrf*stresf(2)
      stres(3) = volfrm*stres(3) + volfrf*stresf(3)
      stres(3) = volfrm*stres(4) + volfrf*stresf(4)
!
  888 continue
      return
      end
