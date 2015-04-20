c
      subroutine tenad(a, cm, ci, s)
c
      implicit none
      integer i, j
      real*8 temp1(6,6), temp2(6,6), temp3(6,6)
      real*8 cm(6,6), ci(6,6), s(6,6), a(6,6)
      
c----------------------------
c computes the dilute strain concentration tensor "a" for the composite
c with matrix and inclusions constitutive tensor cm and ci.
c m. estrada
c
c a = inv(i + s*inv(cm)*(ci-cm)) ..... eq 7a (benveniste, 1987)
c 
c input:
c       cm = constitutive tensor of matrix
c       ci = constitutive tensor of inclusions
c       s = eshelby tensor for inclusions i and matrix m
c output:
c       a = strain concentration tensor
c----------------------------
c
      do i=1,6
        do j=1,6
          temp1(i,j) = ci(i,j) - cm(i,j)
        enddo
      enddo
      call matxinv(temp2,cm,6)
      call matxmul(temp3, temp2, temp1, 6)
      call matxmul(temp1, s, temp3, 6)
      do i=1,6
        temp1(i,i) = 1.d0 + temp1(i,i)
      enddo
      call matxinv(a,temp1,6)
c
      return
      end
      