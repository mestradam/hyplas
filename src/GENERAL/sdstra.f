      subroutine sdstra
     &( bmatx       ,eldisp     ,mdofn      ,mbdim      ,ndofn      ,
     &  nnode       ,ntype      ,gptype     ,xi         ,graphi     ,
     &  stran       ,eljump     ,vecn       ,elkinv     )
c***********************************************************************
c Computes the symmetric gradient (strain measure) associated with the
c element displacement 'eldisp' in 2d elements with strong 
c discontinuities: plane stress, plane strain, and axisymmetric case.
c
c (M. Estrada, 2015)
c
c REFERENCE: Box 5.10 (Dias, 2012)
c***********************************************************************
c
c      implicit double precision (a-h,o-z)
c
c Arguments
      integer
     &  mdofn       ,mbdim      ,ndofn      ,nnode      ,ntype      ,
     &  gptype 
      double precision
     &  bmatx(mbdim,*)  ,eldisp(mdofn,*)  ,xi ,graphi(mbdim,ndofn)  ,
     &  stran(*)        ,eljump(ndofn)    ,vecn(mbdim,ndofn)
c
c Local variables
      integer
     &  nstre       ,nbdim      ,istre      ,ievab      ,idofn      ,
     &  inode
      double precision
     &  temp1(mbdim)    ,temp2(mbdim)   ,temp3(mbdim)
c
c set size of arrays
      if (ntype.eq.1) then
        nstre=3
        nbdim=3
      else if (ntype.eq.2) then
        nstre=4
        nbdim=3
      else if (ntype.eq.3) then
        nstre=4
        nbdim=4
      else
        call errprt('EI0063')
      end if
c
c Initialize strains array 'stran'
      call rvzero(stran,nstre)
c
c Strain computation
c ------------------
      if (gptype.eq.0) then
c Regular Gauss points: 
c stran = bmatx*eldisp
        do istre=1,nbdim
          ievab=0
          do inode=1,nnode
            do idofn=1,ndofn
              ievab=ievab+1
              stran(istre) = stran(istre)+
     &                       bmatx(istre,ievab)*eldisp(idofn,inode)
            end do
          end do
        end do
c
      else if (gptype.eq.1) then
c Reduced integration Gauss point: 
c stran = bmatx*eldisp - xi*graphi*eljump + xi*elkinv*vnnmtx*eljump
        do istre=1,nbdim
          temp1(istre) = 0.d0
          ievab=0
          do inode=1,nnode
            do idofn=1,ndofn
              ievab=ievab+1
              temp1(istre) = temp1(istre) +
     &                       bmatx(istre,ievab)*eldisp(idofn,inode)
            end do
          end do
        end do
c
        do istre=1,nbdim
          temp2(istre) = 0.d0
          do idofn=1,ndofn
            temp2(istre) = temp2(istre) +
     &                     xi*graphi(istre,idofn)*eljump(idofn)
          end do
        end do
c
        do istre=1,nbdim
          temp3(istre) = 0.d0
          do idofn=1,ndofn
            temp3(istre) = temp3(istre) +
     &                     xi*elkinv*vecn(istre,idofn)*eljump(idofn)
          end do
        end do
c
        do istre=1, nstre
          stran(istre) = temp1(istre) - temp2(istre) + temp3(istre)
        end do
c
      else if (gptype.eq.2) then
c Elastic Gauss point:
c stran = bmatx*eldisp - xi*graphi*eljump
        do istre=1,nbdim
          temp1(istre) = 0.d0
          ievab=0
          do inode=1,nnode
            do idofn=1,ndofn
              ievab=ievab+1
              temp1(istre) = temp1(istre) +
     &                       bmatx(istre,ievab)*eldisp(idofn,inode)
            end do
          end do
        end do
c
        do istre=1,nbdim
          temp2(istre) = 0.d0
          do idofn=1,ndofn
            temp2(istre) = temp2(istre) +
     &                     xi*graphi(istre,idofn)*eljump(idofn)
          end do
        end do
c
        do istre=1, nstre
          stran(istre) = temp1(istre) - temp2(istre)
        end do
      end if
c
      return
      end
