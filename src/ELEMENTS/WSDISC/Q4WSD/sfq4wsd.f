      subroutine sfq4wsd
     1(   deriv      ,etasp      ,exisp      ,ibound     ,mdime      ,
     2    shape      )
      implicit double precision (a-h,o-z)
      dimension
     1    deriv(mdime,*)     ,shape(*)
      data rp25 ,rp5  ,r1   /0.25d0,0.5d0,1.0d0/
!***********************************************************************
! computes shape functions and shape function derivatives for
! element 'qua_4_wsd':
!                         4         3
!                          o-------o
!                          |       |     standard isoparametric
!                          |       |     bi-linear 4-node quadrilateral 
!                          |       |
!                          o-------o
!                         1         2
!
! reference: expression (4.42)
!***********************************************************************
      if(ibound.eq.0)then
! shape functions and derivatives on element domain
! -------------------------------------------------
        s=exisp
        t=etasp
        st=s*t
! shape functions
        shape(1)=(r1-t-s+st)*rp25
        shape(2)=(r1-t+s-st)*rp25
        shape(3)=(r1+t+s+st)*rp25
        shape(4)=(r1+t-s-st)*rp25
! shape function derivatives
        deriv(1,1)=(-r1+t)*rp25
        deriv(1,2)=(+r1-t)*rp25
        deriv(1,3)=(+r1+t)*rp25
        deriv(1,4)=(-r1-t)*rp25
        deriv(2,1)=(-r1+s)*rp25
        deriv(2,2)=(-r1-s)*rp25
        deriv(2,3)=(+r1+s)*rp25
        deriv(2,4)=(+r1-s)*rp25
      else
! shape function and derivatives on element boundary (1-d)
! --------------------------------------------------------
        s=exisp
! shape functions
        shape(1)=(-s+r1)*rp5
        shape(2)=(+s+r1)*rp5
! shape functions derivatives
        deriv(1,1)=-rp5
        deriv(1,2)=rp5
!
      endif
!
      return
      end
