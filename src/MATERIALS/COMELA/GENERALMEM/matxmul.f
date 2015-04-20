      subroutine matxmul(ab, a, b, n)
c
      implicit none
      integer n, i, j, k
      real*8 ab(n,n), a(n,n), b(n,n)
c
      do i=1,n
        do j=1,n
          ab(i,j) = 0.d0
          do k=1,n
            ab(i,j) = ab(i,j) + a(i,k)*b(k,j)
          enddo
        enddo
      enddo
c
      return
      end