      subroutine celeng
     .  (nnode      ,ndime      ,ndofn      ,elcod      ,cartddis   ,
     .   restv      )
!***********************************************************************
! Calculation of element length 'eleng'
!
! (M Estrada 2015)
! REFERENCE: commatfail 'le_calc.f90'
!            Oliver, 1989
!-----------------------------------------------------------------------
! Subroutina arguments:
!     
!     nnode   : number of nodes (in)
!     ndime   : number of dimensions of the problem
!     ndofn   : number of degrees of freedom per node
!     elcod   : element coordinates (in)
!     cartddis: 
!     restv   : real element state variables (in/out)
!***********************************************************************
!     
!     Variable declaration
!     --------------------
!     
      implicit double precision (a-h, o-z)
!     
!     Database
      include '../ELEMENTS.INC'
!     
!     Arguments
      dimension
     .   elcod(ndime,nnode)   ,restv(mrestv)  ,cartddis(ndime,nnode)
!     
!     Local variables
      dimension
     .   vn(ndime)   ,elcod1(ndime,nnode)
      integer
     .   phi(nnode)
      parameter
     .   pi = 4.d0*datan(1.d0)
!     
!-----------------------------------------------------------------------
!     
!     Start computations
!     ------------------
!     
!     Get vector normal to discontinuity
      vn(1) = restv(12)
      vn(2) = restv(13)
!     
!     Compute angle of normal vector
      if (vn(1).eq.0.d0) then
        theta = pi / 2.d0
      else
        theta = datan(vn(2)/vn(1))
      end if
!      
!     Choose which nodes are on each side of the discontinuity
      xcg = 0.d0
      ycg = 0.d0
      do inode = 1, nnode
        xcg = xcg + elcod(1,inode)/nnode
        ycg = ycg + elcod(2,inode)/nnode
      end do
!      
      do inode = 1, nnode
        elcod1(1,inode) = (elcod(1,inode) - xcg) * dcos(theta) + 
     .                    (elcod(2,inode) - ycg) * dsin(theta)
        elcod1(2,inode) = -(elcod(1,inode) - xcg) * dsin(theta) + 
     .                     (elcod(2,inode) - ycg) * dcos(theta)
        if (dsign(1.d0, elcod1(1,inode)) .gt. 0.d0) then
          phi(inode) = 1
        else
          phi(inode) = 0
        end if
      end do
!      
!     Compute 'eleng'
      eleng = 0.d0
      do inode = 1, nnode
        eleng = eleng + (cartddis(1,inode) * dcos(theta) +
     .                   cartddis(2,inode) * dsin(theta)) * phi(inonde)
      end do
      if (eleng.ne.0.d0) then
        eleng = 1.d0 / eleng
      end if
!      
!     Save element length in real element state variable 'restv'
      restv(1) = eleng
!      
      return
      end     