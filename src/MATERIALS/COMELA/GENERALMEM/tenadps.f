c
      subroutine tenadps(a, cM, cI, s)
c
      implicit none
      integer i, j
      real*8 temp1(3,3), temp2(3,3), temp3(3,3)
      real*8 cm(3,3), ci(3,3), s(3,3), a(3,3)
      
c----------------------------
c Computes the dilute strain concentration tensor "a" for the composite
c with matrix and inclusions constitutive tensor cM and cI.
c M. Estrada
c
c A = inv(I + S*inv(CM)*(CI-CM)) ..... eq 7a (Benveniste, 1987)
c 
c Input:
c       cM = Constitutive tensor of matrix
c       cI = Constitutive tensor of inclusions
c       s = Eshelby tensor for inclusions I and matrix M
c Output:
c       a = Strain concentration tensor
c----------------------------
c
      do i=1,3
        do j=1,3
          temp1(i,j) = ci(i,j) - cm(i,j)
        enddo
      enddo
      call matxinv(temp2,cm,3)
      call matxmul(temp3, temp2, temp1, 3)
      call matxmul(temp1, s, temp3, 3)
      do i=1,3
        temp1(i,i) = 1.d0 + temp1(i,i)
      enddo
      call matxinv(a,temp1,3)
c
      return
      end
      