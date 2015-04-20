      subroutine ctvmmem
     1(   dgama      ,dmatx      ,epflag     ,iprops     ,ntype      ,
     2    rprops     ,rstava     ,stres      ,xi)
      implicit double precision (a-h,o-z)
      parameter(iphard=20  ,mstre=4)
      logical epflag
c array arguments
      dimension
     1    dmatx(mstre,mstre),iprops(*)           ,rprops(*)          ,
     2    rstava(mstre+1)   ,stres(mstre)
c local arrays
      dimension
     1    foid(mstre,mstre)  ,soid(mstre)        ,vecn(3)
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
     1    rp5  ,r1   ,r2   ,r3   ,r4   /
     2    0.5d0,1.0d0,2.0d0,3.0d0,4.0d0/
c***********************************************************************
c computation of the consistent tangent modulus for von mises type
c elasto-plastic material with piece-wise linear isotropic hardening.
c plane stress implementation only.
c
c reference: section 9.4.5
c***********************************************************************
c stops program if neither not plane stress
      if(ntype.ne.1)call errprt('ei0032')
c current accumulated plastic strain
      epbar=rstava(mstre+1)
c set material properties
      young=rprops(18)
      poiss=rprops(19)
      nhard=iprops(3)
c shear and bulk moduli
      gmodu=young/(r2*(r1+poiss))
      bulk=young/(r3*(r1-r2*poiss))
      r2g=r2*gmodu
      r1d3=r1/r3
      r2d3=r2*r1d3
      if(epflag)then
c compute elastoplastic consistent tangent (box 9.6)
c ==================================================
c item (i):
c ---------
c compute xi
        xi=r2d3*(stres(1)*stres(1)+stres(2)*stres(2)-stres(1)*stres(2))+
     1     r2*stres(3)*stres(3)
c hardening slope
        hslope=dplfun(epbar,nhard,rprops(iphard))
c matrix e components
        estar1=r3*young/(r3*(r1-poiss)+young*dgama)
        estar2=r2g/(r1+r2g*dgama)
        estar3=gmodu/(r1+r2g*dgama)
        e11=rp5*(estar1+estar2)
        e22=e11
        e12=rp5*(estar1-estar2)
        e33=estar3
c components of the matrix product ep
        epsta1=r1d3*estar1
        epsta2=estar2
        epsta3=epsta2
        ep11=rp5*(epsta1+epsta2)
        ep22=ep11
        ep12=rp5*(epsta1-epsta2)
        ep21=ep12
        ep33=epsta3
c vector n
        vecn(1)=ep11*stres(1)+ep12*stres(2)
        vecn(2)=ep21*stres(1)+ep22*stres(2)
        vecn(3)=ep33*stres(3)
c scalar alpha
        denom1=stres(1)*(r2d3*vecn(1)-r1d3*vecn(2))+
     1         stres(2)*(r2d3*vecn(2)-r1d3*vecn(1))+
     2         stres(3)*r2*vecn(3)
        denom2=r2*xi*hslope/(r3-r2*hslope*dgama)
        alpha=r1/(denom1+denom2)
c item (ii): assemble elasto-plastic tangent
c ------------------------------------------
        dmatx(1,1)=e11-alpha*vecn(1)*vecn(1)
        dmatx(1,2)=e12-alpha*vecn(1)*vecn(2)
        dmatx(1,3)=-alpha*vecn(1)*vecn(3)
        dmatx(2,1)=dmatx(1,2)
        dmatx(2,2)=e22-alpha*vecn(2)*vecn(2)
        dmatx(2,3)=-alpha*vecn(2)*vecn(3)
        dmatx(3,1)=dmatx(1,3)
        dmatx(3,2)=dmatx(2,3)
        dmatx(3,3)=e33-alpha*vecn(3)*vecn(3)
      else
c compute plane stress elasticity matrix
c ======================================
        nstre=3
        r4gd3=r4*gmodu/r3
        factor=(bulk-r2g/r3)*(r2g/(bulk+r4gd3))
        do 20 i=1,nstre
          do 10 j=i,nstre
            dmatx(i,j)=r2g*foid(i,j)+factor*soid(i)*soid(j)
   10     continue
   20   continue
c lower triangle
        do 40 j=1,nstre-1
          do 30 i=j+1,nstre
            dmatx(i,j)=dmatx(j,i)
   30     continue
   40   continue
      endif
      return
      end
      