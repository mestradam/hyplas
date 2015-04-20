c
      subroutine cisotr(ciso, cm)
c
      implicit none
      integer i, j, k, l, ind(3,3)
      real*8 tempvol, tempdev
      real*8 kd(3,3)
      real*8 cm(6,6), ciso(6,6)
      real*8 cmtens(3,3,3,3), cisotens(3,3,3,3)
      real*8 id(3,3,3,3), idvol(3,3,3,3), iddev(3,3,3,3)
c----------------------------
c extract the isotropic part of contitutive tensor of the matrix.
c m. estrada
c
c second method (doghiri, 2003)
c 
c input:
c       cm = constitutive tensor of matrix
c output:
c       ciso = isotropic part of cm tensor
c----------------------------
c
c write cm in tensor form
      ind(1,1)=1
      ind(2,2)=2
      ind(3,3)=3
      ind(2,3)=4
      ind(3,2)=4
      ind(1,3)=5
      ind(3,1)=5
      ind(1,2)=6
      ind(2,1)=6
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              cmtens(i,j,k,l) = cm(ind(i,j),ind(k,l))
            enddo
          enddo
        enddo
      enddo
c
c identity tensors
      do i=1,3
        do j=1,3
          kd(i,j) = 0.d0
        enddo
        kd(i,j) = 1.d0
      enddo
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              id(i,j,k,l)=(1.d0/2.d0)*(kd(i,k)*kd(j,l)+kd(i,l)*kd(j,k))
            enddo
          enddo
        enddo
      enddo
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              idvol(i,j,k,l) = (1.d0/3.d0)*(kd(i,j)*kd(k,l))
              iddev(i,j,k,l) = id(i,j,k,l)-idvol(i,j,k,l)
            enddo
          enddo
        enddo
      enddo
c
c forth contraction products (doghri, 2003 - eq 37)
      tempvol = 0.d0
      tempdev = 0.d0
      do i=1,3
        do j=1,3
          tempvol = tempvol+(1.d0/3.d0)*cmtens(i,i,j,j)
          tempdev = tempdev+cmtens(i,j,j,i)
        enddo
      enddo
      tempdev = tempdev-tempvol
c
c compute isotropic part of contitutive tensor
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              cisotens(i,j,k,l) = tempvol*idvol(i,j,k,l)+(1.d0/5.d0)
     &                            *tempdev*iddev(i,j,k,l)
            enddo
          enddo
        enddo
      enddo
c
c write ciso in matrix form
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              ciso(ind(i,j),ind(k,l)) = cisotens(i,j,k,l)
            enddo
          enddo
        enddo
      enddo
c
      return
      end
      