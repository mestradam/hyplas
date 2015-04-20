      subroutine rdcomela
     &(   mrprop      ,mrstav     ,rprops     ,unsym      )
      implicit double precision (a-h,o-z)
      integer mrprop, mrstav, nrstav, nrprop
      logical unsym
      parameter(nrstav=4)
      dimension rprops(*)
      double precision
     &    volfr,a1,a2,a3,l1,m1,n1,l2,m2,n2,l3,m3,n3,densef,youngf,
     &    poissf,densem,youngm,poissm
C-----------------------------------------------------------------------
C Read and echo material properties for linear elastic composite
C material model
C (M. Estrada 2010)
C-----------------------------------------------------------------------
 1000 format('Composite Linear Elastic Material (M. Estrada 2010)'/)
 1010 format(
     &' Volumetric factor fiber ......................... =',G14.6/
     &' Elipsoide length on local axis 1 of fiber ....... =',G14.6/
     &' Elipsoide length on local axis 2 of fiber ....... =',G14.6/
     &' Elipsoide length on local axis 3 of fiber ....... =',G14.6/
     &' Component l1 of local vector e1'' ............... =',G14.6/
     &' Component m1 of local vector e1'' ............... =',G14.6/
     &' Component n1 of local vector e1'' ............... =',G14.6/
     &' Component l2 of local vector e2'' ............... =',G14.6/
     &' Component m2 of local vector e2'' ............... =',G14.6/
     &' Component n2 of local vector e2'' ............... =',G14.6/
     &' Component l3 of local vector e3'' ............... =',G14.6/
     &' Component m3 of local vector e3'' ............... =',G14.6/
     &' Component n3 of local vector e3'' ............... =',G14.6/
     &' Mass density of fibers .......................... =',G14.6/
     &' Fiber Young modulus  ............................ =',G14.6/
     &' Fiber Poisson ratio  ............................ =',G14.6/
     &' Mass density of matrix .......................... =',G14.6/
     &' Matrix Young modulus ............................ =',G14.6/
     &' Matrix Poisson ratio ............................ =',G14.6)
C
C     Set unsymmetric tangent stiffness flag
      unsym=.false.
C
C     Read material properties
      write(16,1000)
      read(15,*)volfr
      read(15,*)a1,a2,a3
      read(15,*)l1,m1,n1
      read(15,*)l2,m2,n2
      read(15,*)l3,m3,n3
      read(15,*)densef
      read(15,*)youngf,poissf
      read(15,*)densem
      read(15,*)youngm,poissm
C
C     Write properties
      write(16,1010)volfr,a1,a2,a3,l1,m1,n1,l2,m2,n2,l3,m3,n3,densef,
     &youngf,poissf,densem,youngm,poissf
      if(youngm.lt.0.d0)call errprt('ed0077')
      if(youngf.lt.0.d0)call errprt('ed0077')
      if(poissm.le.-0.d0.and.poissm.ge.0.5d0)call errprt('ed0078')
      if(poissf.le.-0.d0.and.poissf.ge.0.5d0)call errprt('ed0078')
C
C     Set vector of real material properties
      nrprop=19
      if(nrprop.gt.mrprop)call errprt('ed0181')
      rprops(1)=volfr
      rprops(2)=a1
      rprops(3)=a2
      rprops(4)=a3
      rprops(5)=l1
      rprops(6)=m1
      rprops(7)=n1
      rprops(8)=l2
      rprops(9)=m2
      rprops(10)=n2
      rprops(11)=l3
      rprops(12)=m3
      rprops(13)=n3
      rprops(14)=densef
      rprops(15)=youngf
      rprops(16)=poissf
      rprops(17)=densem
      rprops(18)=youngm
      rprops(19)=poissm
C
C     Check dimensioning of RSTAVA
      if(nrstav.gt.mrstav)call errprt('ed0182')
      return
      end
C
