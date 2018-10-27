c
      subroutine homog(c, cmat, cinc, volfr, a)
c
      implicit none
c output arguments
      real*8 c(6,6)
c input arguments
      real*8 cmat(6,6), cinc(6,6), volfr, a(6,6)
c local variables
      integer i, j
      real*8 temp1(6,6), temp2(6,6), temp3(6,6), id(6,6)
c----------------------------
c compute constitutive equivalent matrix based on eshelby's model
c (benveniste, 1987)
c
c m. estrada, 2010
c----------------------------
c
c identity 6x6 matrix
      do i=1,6
        do j=1,6
          id(i,j) = 0.d0
        enddo
        id(i,j) = 1.d0
      enddo
      do i=1,6
        do j=1,6
          temp1(i,j) = volfr*(cinc(i,j)-cmat(i,j))
        enddo
      enddo
      call matxmul(temp2, temp1, a, 6)
      do i=1,6
        do j=1,6
          c(i,j) = cmat(i,j)+temp2(i,j)
        enddo
      enddo
c
      return
      end
      