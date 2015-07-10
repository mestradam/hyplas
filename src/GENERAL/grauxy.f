      subroutine grauxy
     .  (nnode      ,ndime      ,deldis     ,cartddis   ,restv      )
!***********************************************************************
! Computation of gradient(u_x) and gradient(u_y)
!
! (M Estrada 2015)
! REFERENCE: commatfail 'le_calc.f90'
!-----------------------------------------------------------------------
! Subroutina arguments:
!     
!     nnode   : number of nodes (in)
!     ndime   : number of dimensions of the problem
!     deldis  : element displacement increment (in)
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
     .   deldis(ndime,nnode)  ,cartddis(ndime,nnode),restv(mrestv)
!     
!     Local variables
      dimension
     .   gradu(ndime,ndime)   ,gradxy(ndime)        ,graxyn(ndime)  ,
     .   temp2(ndime)
!     
!-----------------------------------------------------------------------
!     
!     Start computations
!     ------------------
!     
!     Compute grad(u), and norm of grad(u), grad(u_x), and grad(u_y)
      do idime = 1, ndime
        do jdime = 1, ndime
          gradu(idime,jdime) = 0.d0
          do inode = 1, nnode
            gradu(idime,jdime) = gradu(idime,jdime) + 
     .                        deldis(idime,inode)*cartddis(jdime,inode)
          end do
        end do
      end do
!      
!     Compute norm of grad_u
      graun = dsqrt(gradu(1,1)**2 + gradu(1,2)**2 + 
     .              gradu(2,1)**2 + gradu(2,2)**2)
!     
!     Compute norm x and norm y of grad_u
      graxyn(1) = dsqrt(gradu(1,1)**2 + gradu(1,2)**2)
      graxyn(2) = dsqrt(gradu(2,1)**2 + gradu(2,2)**2)
!      
!     Compute gradxy(:) based on maximum of graxyn(:)
      if (graxyn(1).ge.1d-8 .or. graxyn(2).ge.1d-8) then
        if (graxyn(1).ge.graxyn(2)) then
          do idime = 1, ndime
            gradxy(idime) = gradu(1,idime)/graxyn(1)
          end do
        else
          do idime = 1, ndime
            gradxy(idime) = gradu(2,idime)/graxyn(2)
          end do
        end if
      else
        do idime = 1, ndime
          gradxy(idime) = 0.d0
        end do
      end if
!      
!     Save gradxy(:) and norm of gradu in restv
      restv(16) = graun
      restv(17) = gradxy(1)
      restv(18) = gradxy(2)
!      
      return
      end     