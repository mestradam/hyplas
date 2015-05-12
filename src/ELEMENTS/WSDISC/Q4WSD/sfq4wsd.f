      subroutine sfq4wsd
     &  (deriv      ,t          ,s          ,ibound     ,mdime      ,
     &   shape      )
!***********************************************************************
! Computes shape functions and shape function derivatives for
! element 'q4wsd':
!                         4         3
!                          O-------O
!                          |       |     isoparametric bi-linear 
!                          |       |     4-node quadrilateral 
!                          |       |     with strong discontinuities
!                          O-------O
!                         1         2
!
! REFERENCE: Expression (4.42), and (Diaz, 2012)
!***********************************************************************
      implicit double precision (a-h,o-z)
      dimension
     &   deriv(mdime,*)   ,shape(*)
!
!
! Compute shape functions and derivatives for domain or boundary
      if(ibound.eq.0)then
! Shape functions and derivatives on element domain
! -------------------------------------------------
        st=s*t
! Shape functions
        shape(1)=(1.0d0-t-s+st)*0.25d0
        shape(2)=(1.0d0-t+s-st)*0.25d0
        shape(3)=(1.0d0+t+s+st)*0.25d0
        shape(4)=(1.0d0+t-s-st)*0.25d0
! Shape function derivatives
        deriv(1,1)=(-1.0d0+t)*0.25d0
        deriv(1,2)=(+1.0d0-t)*0.25d0
        deriv(1,3)=(+1.0d0+t)*0.25d0
        deriv(1,4)=(-1.0d0-t)*0.25d0
        deriv(2,1)=(-1.0d0+s)*0.25d0
        deriv(2,2)=(-1.0d0-s)*0.25d0
        deriv(2,3)=(+1.0d0+s)*0.25d0
        deriv(2,4)=(+1.0d0-s)*0.25d0
      ELSE
! Shape function and derivatives on element boundary (1-D)
! --------------------------------------------------------
        s=exisp
! Shape functions
        shape(1)=(-s+1.0d0)*0.5d0
        shape(2)=(+s+1.0d0)*0.5d0
! shape functions derivatives
        deriv(1,1)=-0.5d0
        deriv(1,2)=0.5d0
!
      endif
!
      return
      end
