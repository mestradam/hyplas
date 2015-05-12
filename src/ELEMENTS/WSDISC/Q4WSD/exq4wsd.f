      subroutine exq4wsd
     &(   ngausp     ,exmatx     )
      implicit double precision (a-h,o-z)
      parameter(nnode=4)
      dimension exmatx(nnode,ngausp)
      data r1    /
     &     1.0d0 /
      data
     &    a4               ,b4                 ,c4                 /
     &    1.866025404d0    ,-0.5d0             ,0.133974596d0      /
!***********************************************************************
! Sets coefficients matrix (EXMATX) for extrapolation from gauss points
! to nodes for element type 'w4wsd' (wsdisc enriched bi-linear 4-noded 
! quadrilateral element with 2 additional sampling points
!
! REFERENCE: Section 5.6.1
!            E Hinton & JS Campbel. Local and global Smoothing of
!            discontinuous finite element functions using a least
!            squares method. Int. J. Num. meth. Engng., 8:461-480, 1974.
!            E Hinton & DRJ Owen. An introduction to finite element
!            computations. Pineridge Press, Swansea, 1979.
!***********************************************************************
      if(ngausp.eq.1)then
        exmatx(1,1)=r1
        exmatx(2,1)=r1
        exmatx(3,1)=r1
        exmatx(4,1)=r1
      elseif(ngausp.eq.4)then
        exmatx(1,1)=a4         
        exmatx(1,2)=b4         
        exmatx(1,3)=b4         
        exmatx(1,4)=c4         
        exmatx(2,1)=b4
        exmatx(2,2)=c4
        exmatx(2,3)=a4
        exmatx(2,4)=b4
        exmatx(3,1)=c4
        exmatx(3,2)=b4
        exmatx(3,3)=b4
        exmatx(3,4)=a4
        exmatx(4,1)=b4
        exmatx(4,2)=a4
        exmatx(4,3)=c4
        exmatx(4,4)=b4
      endif
      return
      end
