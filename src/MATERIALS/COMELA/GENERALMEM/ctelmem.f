      subroutine ctelmem
     1(   dmatx      ,ntype      ,rprops     )
      implicit double precision (a-h,o-z)
      parameter(mstre=4)
      dimension
     1    dmatx(mstre,mstre),rprops(*)
      dimension
     1    foid(mstre,mstre)  ,soid(mstre)
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
     1    r1   ,r2   ,r3   ,r4   /
     2    1.0d0,2.0d0,3.0d0,4.0d0/
c***********************************************************************
c computation of the tangent modulus (elasticity matrix) for the linear
c elastic material model
c
c reference: expression (4.44)
c***********************************************************************
c
c set shear and bulk modulus
c --------------------------
c
      gmodu=rprops(2)
      bulk=rprops(3)
c
      r1d3=r1/r3
      r2g=r2*gmodu
      factor=bulk-r2g*r1d3
c
c compute elasticity matrix
c -------------------------
c
      if(ntype.eq.1)then
c plane stress
        nstre=3
        r4gd3=r4*gmodu*r1d3
        factor=(bulk-r2g*r1d3)*(r2g/(bulk+r4gd3))
      elseif(ntype.eq.2)then
c plane strain
        nstre=3
        factor=bulk-r2g*r1d3
      elseif(ntype.eq.3)then
c axisymmetric
        nstre=4
        factor=bulk-r2g*r1d3
      else
c stops program if other stress state
        call errprt('ei0019')
      endif
c
c assemble matrix
c
      do 20 i=1,nstre
        do 10 j=i,nstre
          dmatx(i,j)=r2g*foid(i,j)+factor*soid(i)*soid(j)
   10   continue
   20 continue
c lower triangle
      do 40 j=1,nstre-1
        do 30 i=j+1,nstre
          dmatx(i,j)=dmatx(j,i)
   30   continue
   40 continue
c
      return
      end
      