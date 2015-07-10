      subroutine nselec
     .  (restv      ,ndime      )
!***********************************************************************
! Selection of the normal obtained by the bifurcation analysis 
!
! (M Estrada 2015)
! REFERENCE: commatfail 'le_calc.f90'
!***********************************************************************
!     
!     Variable declaration
!     --------------------
!        
      implicit double precision (a-h, o-z)
!     
!     Database
      include '../ELEMENTS.INC'
      include '../LOCALIZA.INC'
!     
!     Arguments
      dimension restv(mrestv)
!     
!     Local variables
      dimension
     .   vnbif1(ndime)  ,vnbif2(ndime)  ,vn(ndime)      ,vm(ndime)  ,
     .   gradxy(ndime)  ,grau(ndime)
!     
!-----------------------------------------------------------------------
!     
!     Start computations
!     ------------------
!     
!     Get two vectors calculated by bifurcation analysis
      vnbif1(1) = restv(8)
      vnbif1(2) = restv(9)
      vnbif2(1) = restv(10)
      vnbif2(2) = restv(11)
!      
!     Get gradient of displacement to compare
      grau(1) = restv(17)
      grau(2) = restv(18)
!      
!     Compute dot product of n vectors with gradient(u)
      dot1 = 0.d0
      dot2 = 0.d0
      do idime = 1, ndime
        dot1 = dot1 + vnbif1(idime)*gradxy(idime)
        dot2 = dot2 + vnbif2(idime)*gradxy(idime)
      end do
!      
!     Choose vector perpendicular and parallel to discontinuity
      if (dot1.ge.dot2) then
        vn(1) = vnbif1(1) ; vn(2) = vnbif1(2)
        vm(1) = vnbif2(1) ; vm(2) = vnbif2(2)
      else
        vn(1) = vnbif2(1) ; vn(2) = vnbif2(2)
        vm(1) = vnbif1(1) ; vm(2) = vnbif1(2)
      end if
!      
!     Select direction of chosen vector n depending on reference vector
      dot1 = 0.d0
      do idime = 1, ndime
        dot1 = dot1 + vn(idime)*vecnre(idime)
      end do
      if (dot1.lt.0.d0) then
        vn(1) = -vn(1) ; vn(2) = -vn(2)
        vm(1) = -vm(1) ; vm(2) = -vm(2)
      end if
!      
!     Save selected normal and parallel vectors
      restv(12) = vn(1)
      restv(13) = vn(2)
      restv(14) = vm(1)
      restv(15) = vm(2)
!      
      return
      end