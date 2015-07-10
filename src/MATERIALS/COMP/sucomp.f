      subroutine sucomp
     &( ralgva      ,iprops     ,lalgva     ,ntype      ,rprops     ,
     &  rstava      ,strat      ,stres      )
!***********************************************************************
! State update procedure for the COMPOSITE material model with two
! material components.
!   Fibers: Damage from Weibull analysis
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
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
      implicit double precision (a-h, o-z)
!
! ... arguments
      logical
     &  lalgva(*)
      integer
     &  iprops(*)   ,ntype
      real(kind=8)
     &  ralgva(2)   ,rprops(*)  ,rstava(*)  ,strat(4)   ,stres(4)
!
! ... local variables
      logical
     &  ifplas      ,sufail     ,ifdama
      integer
     &  iphardm     ,mstre      ,nstre      ,mxiter
      real(kind=8)
     &  youngm      ,poissm     ,youngf     ,poissf     ,sigmauf    ,
     &  alphaf      ,betaf      ,volfrf     ,anglef     ,eet(4)     ,
     &  strest(3)   ,tol        ,dgama      ,stresf(3)  ,streff     ,
     &  damage      ,mx         ,my
      parameter(
     &  mstre=4     ,nstre=3    ,tol=1.d-08 ,mxiter=50  )
      DATA
     1    R0   ,RP5  ,R1   ,R2   ,R3   ,R4   ,R6   ,TOL   /
     2    0.0D0,0.5D0,1.0D0,2.0D0,3.0D0,4.0D0,6.0D0,1.D-08/
!
! Stop program if not plane stress
      if (ntype.ne.1) call errprt('EI0031')
!
! Start state update process for MATRIX (Von Mises plane stress)
! ==============================================================
!
! Initialize some algorithmic and internal variables
      dgama = 0.d0
      ifplas = .false.
      sufail = .false.
!...set previously (equilibrium) converged accumulated plastic strain
      epbarn = rstava(9)
! Set some material properties
      young = rprops(1)
      poiss = rprops(2)
      nhard = iprops(3)
      iphard = 21
! Shear and bulk moduli and other necessary constants
      gmodu = young/(2.d0*(1.d0+poiss))
      bulk = young/(3.d0*(1.d0-2.d0*poiss))
      r2g=r2*gmodu
      r4g=r4*gmodu
      r1d3=r1/r3
      r1d6=r1/r6
      r2d3=r2*r1d3
      sqr2d3=sqrt(r2d3)
      r4gd3=r4g*r1d3
! Elastic predictor: Compute elastic trial state
! ----------------------------------------------
! Volumetric strain
      factor=r2g/(bulk+r4gd3)
      eev=(strat(1)+strat(2))*factor
! Elastic trial deviatoric strain
      eevd3=eev/r3
      eet(1)=strat(1)-eevd3
      eet(2)=strat(2)-eevd3
! Convert engineering shear component into physical component
      eet(3)=strat(3)*rp5
! Elastic trial stress components
      pt=bulk*eev
      strest(1)=r2g*eet(1)+pt
      strest(2)=r2g*eet(2)+pt
      strest(3)=r2g*eet(3)
! Compute yield function value at trial state
      a1=(strest(1)+strest(2))*(strest(1)+strest(2))
      a2=(strest(2)-strest(1))*(strest(2)-strest(1))
      a3=strest(3)*strest(3)
      xi=r1d6*a1+rp5*a2+r2*a3
      sigmay=plfun(epbarn,nhard,rprops(iphard))
!...yield function
      phi=rp5*xi-r1d3*sigmay*sigmay
! Check for plastic admissibility
! -------------------------------
      if(phi/sigmay.gt.tol)then
! Plastic step: Apply return mapping - use Newton-Raphson algorithm
!               to solve the plane stress-projected return mapping
!               equation for the plastic multiplier (Box 9.5)
! -----------------------------------------------------------------
        IFPLAS=.TRUE.
        EPBAR=EPBARN
        SQRTXI=SQRT(XI)
        B1=R1
        B2=R1
        FMODU=YOUNG/(R3*(R1-POISS))
        DO NRITER=1,MXITER
! Compute residual derivative
          HSLOPE=DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
          DXI=-A1*FMODU/(R3*B1*B1*B1)-R2G*(A2+R4*A3)/(B2*B2*B2)
          HBAR=R2*SIGMAY*HSLOPE*SQR2D3*(SQRTXI+DGAMA*DXI/(R2*SQRTXI))
          DPHI=RP5*DXI-R1D3*HBAR
! Compute Newton-Raphson increment and update equation variable DGAMA
          DGAMA=DGAMA-PHI/DPHI
! Compute new residual (yield function value)
          B1=R1+FMODU*DGAMA
          B2=R1+R2G*DGAMA
          XI=R1D6*A1/(B1*B1)+(RP5*A2+R2*A3)/(B2*B2)
          SQRTXI=SQRT(XI)
          EPBAR=EPBARN+DGAMA*SQR2D3*SQRTXI
          SIGMAY=PLFUN(EPBAR,NHARD,RPROPS(IPHARD))
          PHI=RP5*XI-R1D3*SIGMAY*SIGMAY
! Check for convergence
          RESNOR=ABS(PHI/SIGMAY)
          IF(RESNOR.LE.TOL)THEN
! update accumulated plastic strain
            RSTAVA(9)=EPBAR
! update stress components:   sigma := A sigma^trial
            ASTAR1=R3*(R1-POISS)/(R3*(R1-POISS)+YOUNG*DGAMA)
            ASTAR2=R1/(R1+R2G*DGAMA)
            A11=RP5*(ASTAR1+ASTAR2)
            A22=A11
            A12=RP5*(ASTAR1-ASTAR2)
            A21=A12
            A33=ASTAR2
            STRES(1)=A11*STREST(1)+A12*STREST(2)
            STRES(2)=A21*STREST(1)+A22*STREST(2)
            STRES(3)=A33*STREST(3)
! compute corresponding elastic (engineering) strain components
            FACTG=R1/R2G
            P=R1D3*(STRES(1)+STRES(2))
            EEV=P/BULK
            EEVD3=R1D3*EEV
            RSTAVA(1)=FACTG*(R2D3*STRES(1)-R1D3*STRES(2))+EEVD3
            RSTAVA(2)=FACTG*(R2D3*STRES(2)-R1D3*STRES(1))+EEVD3
            RSTAVA(3)=FACTG*STRES(3)*R2
            RSTAVA(4)=-POISS/(R1-POISS)*(RSTAVA(1)+RSTAVA(2))
            GOTO 998
          ENDIF
        end do
! reset failure flag and print warning message if N-R algorithm fails
        SUFAIL=.TRUE.
        CALL ERRPRT('WE0013')
      ELSE
! Elastic step: Update stress using linear elastic law
! ----------------------------------------------------
        STRES(1)=STREST(1)
        STRES(2)=STREST(2)
        STRES(3)=STREST(3)
! elastic engineering strain
        RSTAVA(1)=STRAT(1)
        RSTAVA(2)=STRAT(2)
        RSTAVA(3)=STRAT(3)
        RSTAVA(4)=-POISS/(R1-POISS)*(STRAT(1)+STRAT(2))
      ENDIF
  998 CONTINUE
! Update some algorithmic variables before exit
      LALGVA(1)=IFPLAS
      LALGVA(2)=SUFAIL
      ralgva(1) = dgama
!
!
! Start state update process for FIBERS (Damage Weibull uniaxial)
! ===============================================================
!
! Initialize algorithmic and internal variables
      ifdama = .false.
      sufail = .false.
! ... set previously accumulated damage variable
      damage = rstava(10)
! ... set previously accumulated threshold yield function
      qmaxf = ralgva(2)
!
! Set some material properties
      youngf = rprops(11)
      poissf = rprops(12)
      sigmauf = rprops(13)
      alphaf = rprops(14)
      betaf = rprops(15)
      anglef = rprops(17)
      mx = dcos(anglef)
      my = dsin(anglef)
!
! Elastic predictor: Compute elastic trial state
! ----------------------------------------------
!
! Elastic trial strain
      eet(1) = strat(1)
      eet(2) = strat(2)
!
! Convert engineering shear component into physical component
      eet(3) = (strat(3))/2.d0
!
! Elastic trial strain component parallel to fibers direction "anglef"
      eetf = eet(1)*mx*mx+eet(2)*my*my + 2.d0*eet(3)*mx*my
!
! Effective stress on fibers
      streff = youngf*eetf
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
      else
!
! Plastic step: Update damage variable and nominal
! ------------------------------------------------
          ifdama = .true.
!
! Damage variable (Weibull distribution)
          damage = 1.d0-dexp(-(streff/betaf)**alphaf)
          rstava(10) = damage
!
! Update longitudinal stress on fibers
          streff = (1.d0-damage) * streff
!
! Update stress matrix
          stresf(1) = streff*mx*mx
          stresf(2) = streff*my*my
          stresf(3) = streff*mx*my
      end if
!
! Update some algorithmic variables before exit
      lalgva(3) = ifdama
      ralgva(2) = qtrial
!
! Compute COMPOSITE stress: sigma = k^m*sigma^m + k^f*sigma^f
! ===========================================================
!
! Initialize some variables
      volfrf = rprops(16)
      volfrm = 1.d0 - volfrf
!
! Homogenization by mixture law
      stres(1) = volfrm*stres(1) + volfrf*stresf(1)
      stres(2) = volfrm*stres(2) + volfrf*stresf(2)
      stres(3) = volfrm*stres(3) + volfrf*stresf(3)
!
      return
      end
