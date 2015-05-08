      subroutine rdcovm
     &(   iprops     ,miprop     ,mlalgv     ,mrprop     ,mrstav     ,
     &    rprops     ,unsym     )
      implicit double precision (a-h,o-z)
      logical unsym
      parameter ( iphard=20 , nlalgv=2 , nrstav=5 )
      dimension iprops(*) , rprops(*)
c local variables
      double precision volfr, a1, a2, a3, l1, l2, l3, m1, m2, m3, n1,
     &  n2, n3, densef, youngf, poissf, densem, youngm, poissm
c------------------
c read and echo material properties for composite material with elastic
c fibers and elasto-plastic von mises matrix with non-linear hardening
c
c (m. estrada, 2011)
c------------------
c
 1000 format('composite material with elastic fibers and'/
     &'elasto-plastic von mises matrix')
 1100 format(
     &' volumetric factor fiber ......................... =',g14.6/
     &' elipsoide length on local axis 1 of fiber ....... =',g14.6/
     &' elipsoide length on local axis 2 of fiber ....... =',g14.6/
     &' elipsoide length on local axis 3 of fiber ....... =',g14.6/
     &' component l1 of local vector e1'' ............... =',g14.6/
     &' component m1 of local vector e1'' ............... =',g14.6/
     &' component n1 of local vector e1'' ............... =',g14.6/
     &' component l2 of local vector e2'' ............... =',g14.6/
     &' component m2 of local vector e2'' ............... =',g14.6/
     &' component n2 of local vector e2'' ............... =',g14.6/
     &' component l3 of local vector e3'' ............... =',g14.6/
     &' component m3 of local vector e3'' ............... =',g14.6/
     &' component n3 of local vector e3'' ............... =',g14.6)
 1101 format(
     &' mass density of fibers .......................... =',g14.6/
     &' fiber young modulus  ............................ =',g14.6/
     &' fiber poisson ratio  ............................ =',g14.6)
 1102 format(
     &' mass density of matrix .......................... =',g14.6/
     &' matrix young modulus ............................ =',g14.6/
     &' matrix poisson ratio ............................ =',g14.6)
 1200 format(/
     &' number of points on hardening curve ............. =',i3//
     &'           epstn        uniaxial yield stress '/)
 1300 format(2(5x,g15.6))
c
c set unsymmetric tangent stiffness flag
      unsym=.false.
c
c read and write material properties
      write(16,1000)
c.. composite properties
      read(15,*)volfr
      read(15,*)a1,a2,a3
      read(15,*)l1,m1,n1
      read(15,*)l2,m2,n2
      read(15,*)l3,m3,n3
      write(16,1100)volfr,a1,a2,a3,l1,m1,n1,l2,m2,n2,l3,m3,n3
c.. fibers properties
      read(15,*)densef
      read(15,*)youngf,poissf
      write(16,1101)densef,youngf,poissf
      if(youngf.lt.0.d0)call errprt('ed0077')
      if(poissf.lt.-0.d0.and.poissf.ge.0.5d0)call errprt('ed0078')
c.. matrix properties
      read(15,*)densem
      read(15,*)youngm,poissm
      write(16,1102)densem,youngm,poissm
      if (youngm.lt.0.d0) call errprt('ed0077')
      if (poissm.lt.-0.d0.and.poissm.ge.0.5d0) call errprt('ed0078')
c.. number of points on hardening curve
      read(15,*)nhard
      write(16,1200)nhard
      if(nhard.lt.2)call errprt('ed0101')
c check dimensions of iprops
      if(miprop.lt.3)call errprt('ed0102')
      iprops(3)=nhard
c check rprops dimensions
      nrprop=iphard+nhard*2-1
      if(nrprop.gt.mrprop)call errprt('ed0103')
c assign variables to rprops array
c.. composite properties
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
c... fibers properties
      rprops(14)=densef
      rprops(15)=youngf
      rprops(16)=poissf
c... matrix properties
      rprops(17)=densem
      rprops(18)=youngm
      rprops(19)=poissm
c... hardening curve points
      do ihard=1,nhard
        read(15,*)rprops(iphard+ihard*2-2),rprops(iphard+ihard*2-1)
        write(16,1300)rprops(iphard+ihard*2-2),rprops(iphard+ihard*2-1)
      enddo
c check dimension of rstava and lalgva
      if (nrstav.gt.mrstav) call errprt('ed0104')
      if (nlalgv.gt.mlalgv) call errprt('ed0105')
c
      return
      end
