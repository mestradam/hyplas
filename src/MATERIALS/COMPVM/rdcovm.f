      subroutine rdcovm
     .(   iprops     ,miprop     ,mlalgv     ,mrprop     ,mrstav     ,
     .    rprops     ,unsym      )
!***********************************************************************
! Read and echo material properties
! for composite material (plane strain and axisymmetric only).
!   
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   iprops  (out) : Integer props besides the first two (mattyp, matcls)
!   rprops  (out) : Real properties array
!   unsym   (out) : Unsymetric solver flag
!   miprop  (in)  : Max num of integer parameters (from MATERIAL.INC)
!   mlalgv  (in)  : Max mun of logical algorithmic variables per GP
!   mrprop  (in)  : Max num of real parameters (from MATERIAL.INC)
!   mrstav  (in)  : Max mun of real state variables per gauss point
!-----------------------------------------------------------------------
! Material properties:
!
!   props(1)    :   matrix mass density         :   densem
!   props(2)    :   matrix young modulus        :   youngm
!   props(3)    :   matrix poisson ratio        :   poissm
!      
!   props(11)   :   fibers mass density         :   densef
!   props(12)   :   fibers young modulus        :   youngf
!   props(13)   :   fibers poisson ratio        :   poissf
!   props(14)   :   fibers ultimate stress      :   sigmauf
!   props(15)   :   fibers weibull shape factor :   alphaf
!   props(16)   :   fibers weibull scale factor :   betaf
!   props(17)   :   fibers volume fraction      :   volfrf
!   props(18)   :   fibers inclination angle    :   anglef
!      
!   props(21)   props(22)   :   hardstrainm     hardstressm
!       .           .               .               .
!       .           .               .               .
!       .           .               .               .
!   props(*)    props(*)    :   hardstrainm     hardstressm
!
!   irops(3)    :   matrix num. hardening curve :   nhardm
!-----------------------------------------------------------------------
! Usage in the input file:
!
!   <num of material> COMPOSITE
!       <densem>
!       <youngm>  <poissm>
!       <densef>
!       <youngf>  <poissf>  <sigmauf>  <alphaf>  <betaf>
!       <volfrf>  <anglef>
!       <nhardm>
!       <epstrainm> <epstressm>
!       <epstrainm> <epstressm>
!           .           .
!           .           .
!           .           .
!
! Example:
!   1 COMPOSITE
!	5000	0.35
!	50000	0.35    250.0   2.5298  519.16
!	0.40	0.00
!	23
!	0.000	0.45
!	0.001	0.454577
!	0.002	0.459081
!	0.005	0.472155
!	0.01	0.492564
!	0.02	0.528701
!	0.03	0.559411
!	0.04	0.585539
!	0.05	0.607799
!	0.06	0.626794
!	0.07	0.643032
!	0.08	0.656942
!	0.09	0.668887
!	0.10	0.679172
!	0.12	0.695760
!	0.14	0.708326
!	0.16	0.718025
!	0.20	0.731879
!	0.25	0.743463
!	0.30	0.752122
!	0.50	 0.779564
!	1.00	0.84424
!	11.0	2.13664
!	23
!	0.000	0.45
!	0.001	0.454435762
!	0.002	0.458796841
!	0.005	0.471444418
!	0.01	0.491143029
!	0.02	0.525858133
!	0.03	0.555146425
!	0.04	0.5798531
!	0.05	0.600691721
!	0.06	0.618264715
!	0.07	0.633080684
!	0.08	0.645569021
!	0.09	0.656092241
!	0.10	0.664956399
!	0.12	0.678700849
!	0.14	0.688423202
!	0.16	0.695278735
!	0.20	0.703446598
!	0.25	0.707922245
!	0.30	0.709472875
!	0.50	0.70848216
!	1.00	0.702075988
!	11.0	0.572836
!***********************************************************************
!     
! Variable declaration
      implicit double precision (a-h,o-z)
      parameter( iphard=21  ,nlalgv=3  ,nrstav=13 )
!      
! ... arguments
      logical unsym
      dimension
     .    iprops(*)          ,rprops(*)
      data r0   /0.0d0/
 1000 FORMAT(' Composite material reinforced with long fibers '/
     .' - Fibers: Damage Weibull model '/
     .' - Matrix: Elasto-plastic VON MISES yield criterion'/)
 1100 FORMAT(
     .' Matrix mass density ............................... =',G15.6/
     .' Matrix young''s modulus ............................ =',G15.6/
     .' Matrix poisson''s ratio ............................ =',G15.6)
 2100 FORMAT(
     .' Fibers mass density ............................... =',G15.6/
     .' Fibers young''s modulus ............................ =',G15.6/
     .' Fibers poisson''s ratio ............................ =',G15.6/
     .' Fibers ultimate stress ............................ =',g15.6/
     .' Fibers Weibull shape factor ....................... =',g15.6/
     .' Fibers Weibull scale factor ....................... =',g15.6)
 1200 format(/
     .' Number of points on hardening curve ............... =',I3//
     .'           Epstn        uniaxial yield stress '/)
 1300 format(2(5x,g15.6))
!
! Set unsymmetric tangent stiffness flag
      unsym=.false.
!
! Write heading
      write(16,1000)
!
! Read and echo matrix properties
      read(15,*)densem
      read(15,*)youngm,poissm
      write(16,1100)densem,youngm,poissm
      if(youngm.le.r0)call errprt('ED0100')
      rprops(1) = densem
      rprops(2) = youngm
      rprops(3) = poissm
      rprops(4) = r0
      rprops(5) = r0
      rprops(6) = r0
      rprops(7) = r0
      rprops(8) = r0
      rprops(9) = r0
      rprops(10)= r0
      
!
! Read fibers properties
      read(15,*) densef
      read(15,*) youngf, poissf, sigmauf, alphaf, betaf
      read(15,*) volfr, anglef
      write(16,2100) densef, youngf, poissf, sigmauf, alphaf, betaf
      if(youngf.le.r0)call errprt('ED0100')
      rprops(11) = densef
      rprops(12) = youngf
      rprops(13) = poissf
      rprops(14) = sigmauf
      rprops(15) = alphaf
      rprops(16) = betaf
      rprops(17) = volfr
      rprops(18) = anglef
      rprops(19) = r0
      rprops(20) = r0
!
! number of points on hardening curve
      read(15,*)nhard
      write(16,1200)nhard
      if(nhard.lt.2) call errprt('ED0101')
! check dimensions of IPROPS
      if(miprop.lt.3)call errprt('ED0102')
      iprops(3)=nhard
! check dimensions of RPROPS
      nrprop=iphard+nhard*2-1
      if(nrprop.gt.mrprop)call errprt('ED0103')
!
      
! Read and set hardening curve
      do ihard=1,nhard
        read(15,*)rprops(iphard+ihard*2-2),
     .            rprops(iphard+ihard*2-1)
        write(16,1300)rprops(iphard+ihard*2-2),
     .                rprops(iphard+ihard*2-1)
      enddo
! Check dimension of RSTAVA and LALGVA
      if(nrstav.gt.mrstav)call errprt('ED0104')
      if(nlalgv.gt.mlalgv)call errprt('ED0105')
!
      return
      end
