      subroutine rdcomp
     &( iprops      ,rprops     ,unsym      ,miprop     ,mlalgv     ,
     &  mrprop      ,mrstav     )
c***********************************************************************
c Read and echo material properties for composite material with two
c material components.
c   Fibers: Damage Weibull model
c   Matrix: Elastoplastic Von-Mises J2 model
c
c (M. Estrada, 2014)
c-----------------------------------------------------------------------
c Subroutine arguments:
c
c   iprops  (out) : Integer props besides the first two (mattyp, matcls)
c   rprops  (out) : Real properties array
c   unsym   (out) : Unsymetric solver flag
c   miprop  (in)  : Max num of integer parameters (from MATERIAL.INC)
c   mlalgv  (in)  : Max mun of logical algorithmic variables per GP
c   mrprop  (in)  : Max num of real parameters (from MATERIAL.INC)
c   mrstav  (in)  : Max mun of real state variables per gauss point
c-----------------------------------------------------------------------
c Material properties:
c
c   props(1)    :   matrix young modulus        :   youngm
c   props(2)    :   matrix poisson ratio        :   poissm
c   props(11)   :   fibers young modulus        :   youngf
c   props(12)   :   fibers poisson ratio        :   poissf
c   props(13)   :   fibers ultimate stress      :   sigmauf
c   props(14)   :   fibers weibull shape factor :   alphaf
c   props(15)   :   fibers weibull scale factor :   betaf
c   props(16)   :   fibers volume fraction      :   volfrf
c   props(17)   :   fibers inclination angle    :   anglef
c   props(21)   props(22)   :   hardstrainm     hardstressm
c       .           .               .               .
c       .           .               .               .
c       .           .               .               .
c   props(*)    props(*)    :   hardstrainm     hardstressm
c
c   irops(3)    :   matrix num. hardening curve :   nhardm
c-----------------------------------------------------------------------
c Usage in the input file:
c
c   <num of material> COMPOSITE
c       <youngm>  <poissm>
c       <youngf>  <poissf>  <sigmauf>  <alphaf>  <betaf>
c       <volfrf>  <anglef>
c       <nhardm>
c       <epstrainm> <epstressm>
c       <epstrainm> <epstressm>
c           .           .
c           .           .
c           .           .
c
c Example:
c   1 COMPOSITE
c	5000	0.35
c	50000	0.35    250.0   2.5298  519.16
c	0.40	0.00
c	23
c	0.000	0.45
c	0.001	0.454577
c	0.002	0.459081
c	0.005	0.472155
c	0.01	0.492564
c	0.02	0.528701
c	0.03	0.559411
c	0.04	0.585539
c	0.05	0.607799
c	0.06	0.626794
c	0.07	0.643032
c	0.08	0.656942
c	0.09	0.668887
c	0.10	0.679172
c	0.12	0.695760
c	0.14	0.708326
c	0.16	0.718025
c	0.20	0.731879
c	0.25	0.743463
c	0.30	0.752122
c	0.50	 0.779564
c	1.00	0.84424
c	11.0	2.13664
c	23
c	0.000	0.45
c	0.001	0.454435762
c	0.002	0.458796841
c	0.005	0.471444418
c	0.01	0.491143029
c	0.02	0.525858133
c	0.03	0.555146425
c	0.04	0.5798531
c	0.05	0.600691721
c	0.06	0.618264715
c	0.07	0.633080684
c	0.08	0.645569021
c	0.09	0.656092241
c	0.10	0.664956399
c	0.12	0.678700849
c	0.14	0.688423202
c	0.16	0.695278735
c	0.20	0.703446598
c	0.25	0.707922245
c	0.30	0.709472875
c	0.50	0.70848216
c	1.00	0.702075988
c	11.0	0.572836
c***********************************************************************
c
c Variable declaration
      implicit none
c
c ... arguments
      logical
     &  unsym
      integer
     &  iprops(*)   ,miprop     ,mlalgv     ,mrprop     ,mrstav
      real(kind=8)
     &  rprops(30)
c
c ... local variables
      integer
     &  nhardm      ,iphard     ,nlalgv     ,nrstav     ,ihard      ,
     &  irprop      ,nrprop
      real(kind=8)
     &  volfrf      ,anglef     ,youngm     ,poissm     ,youngf     ,
     &  poissf      ,sigmauf    ,alphaf     ,betaf      ,pi
      parameter(
     &  iphard=21   ,nlalgv=2   ,nrstav=5   ,pi=dacos(-1.d0)        )
c
c Printing formats
 1000 format(' Composite material '/)
 1100 format(
     &' Fibers volume fraction ............................. =',g15.6/
     &' Fibers inclination angle ........................... =',g15.6)
 1200 format(
     &' Matrix young modulus ............................... =',g15.6/
     &' Matrix poisson ratio ............................... =',g15.6)
 1300 format(
     &' Fibers young modulus ............................... =',g15.6/
     &' Fibers poisson ratio ............................... =',g15.6/
     &' Fibers ultimate stress ............................. =',g15.6/
     &' Fibers Weibull shape factor ........................ =',g15.6/
     &' Fibers Weibull scale factor ........................ =',g15.6)
 1400 format(/
     1' Number of points on matrix hardening curve ......... =',i3//
     2'           Epstn        uniaxial yield stress '/)
 1600 format(2(5x,g15.6))
c
c Set unsymmetric tangent stiffness flag
      unsym=.false.
c
c Write heading
      write(16,1000)
c
c Read matrix properties
      read(15,*) youngm, poissm
      write(16,1200) youngm, poissm
      rprops(1) = youngm
      rprops(2) = poissm
c
c Read fibers properties
      read(15,*) youngf, poissf, sigmauf, alphaf, betaf
      write(16,1300) youngf, poissf, sigmauf, alphaf, betaf
      rprops(11) = youngf
      rprops(12) = poissf
      rprops(13) = sigmauf
      rprops(14) = alphaf
      rprops(15) = betaf
c
c Read some of the composite material real properties
      read(15,*) volfrf, anglef
      write(16,1100) volfrf, anglef
      rprops(16) = volfrf
      rprops(17) = anglef * pi/180.d0
c
c Read number of points on matrix hardening curve
      read(15,*) nhardm
      write(16,1400) nhardm
      if(nhardm.lt.2) call errprt('ED0101')
c
c Check dimensions, and set IPROPS array
      if(miprop.lt.3) call errprt('ED0102')
      iprops(3) = nhardm
c
c Check dimensions, and ser RPROPS array
      nrprop = iphard + nhardm*2-1
      if(nrprop.gt.mrprop) call errprt('ED0103')
c
c Read matrix hardening curve
      do ihard = 1, nhardm
          read(15,*)
     &    rprops(iphard+ihard*2-2),rprops(iphard+ihard*2-1)
          write(16,1600)
     &    rprops(iphard+ihard*2-2),rprops(iphard+ihard*2-1)
      end do
c
c Check dimension of RSTAVA and LALGVA
      if(nrstav.gt.mrstav)call errprt('ED0104')
      if(nlalgv.gt.mlalgv)call errprt('ED0105')
c
      return
      end
