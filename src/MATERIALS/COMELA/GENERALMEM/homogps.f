c
      subroutine homogps(c, cmat, cinc, volfr, a)
c
      implicit none
c output arguments
      real*8 c(3,3)
c input arguments
      real*8 cmat(3,3), cinc(3,3), volfr, a(3,3)
c local variables
      integer i, j
      real*8 temp1(3,3), temp2(3,3), temp3(3,3), id(3,3)
c----------------------------
c compute constitutive equivalent matrix based on eshelby's model
c (benveniste, 1987)
c
c m. estrada, 2010
c----------------------------
c
c identity matrix
      do i=1,3
        do j=1,3
          id(i,j) = 0.d0
        enddo
        id(i,j) = 1.d0
      enddo
      do i=1,3
        do j=1,3
          temp1(i,j) = volfr*(cinc(i,j)-cmat(i,j))
        enddo
      enddo
      call matxmul(temp2, temp1, a, 3)
      do i=1,3
        do j=1,3
          c(i,j) = cmat(i,j)+temp2(i,j)
        enddo
      enddo
c
      return
      end
      