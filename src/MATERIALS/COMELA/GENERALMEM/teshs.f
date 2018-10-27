c
      subroutine teshs(s, cm, a1, a2, a3, ngauss)
c
      implicit none
      integer ngauss, i, j, k, l, m, n, p, q, ind(3,3)
      real*8 a1, a2, a3, xp, xq, wp, wq, d, pi
      real*8 z(3)
      real*8 nodo(ngauss), peso(ngauss)
      real*8 matk(3,3), matn(3,3), dk(3,3)
      real*8 perm(3,3,3)
      real*8 stens(3,3,3,3), g(3,3,3,3), cmtens(3,3,3,3)
      real*8 s(6,6), cm(6,6)
c
      parameter(pi = 3.14159265358979323846)
c------------------
c compute eshelby tensor "s" in 6x6 matrix form.
c m. estrada (ref. meraghni2002)
c
c input:
c     cm = constitutive tensor of the matrix
c     a1, a2, a3 = radius to determine the shape of the ellipsoids
c     ngauss = number of gauss points to numerical integration
c output:
c     s = tensor of eshelby in matrix form (6x6)
c------------------
c
c compute gauss nodes and weights
      call glpts(ngauss, nodo, peso)
c initialize some variables
      do i=1,3
        do j=1,3
          dk(i,j) = 0.d0
        enddo
        dk(i,i) = 1.d0
      enddo
      do i=1,3
        do j=1,3
          do k=1,3
            perm(i,j,k) = (1.d0/2.d0)*(i-j)*(j-k)*(k-i)
            do l=1,3
              stens(i,j,k,l) = 0.d0
            enddo
          enddo
        enddo
      enddo
      ind(1,1)=1
      ind(2,2)=2
      ind(3,3)=3
      ind(2,3)=4
      ind(3,2)=4
      ind(1,3)=5
      ind(3,1)=5
      ind(1,2)=6
      ind(2,1)=6
c... write cm in tensor form
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
c gauss-legendre quadrature integration
      do p=1,ngauss
        xp=nodo(p)
        wp=peso(p)
        do q=1,ngauss
          xq=nodo(q)
          wq=peso(q)
c... initialize variables
          z(1) = sqrt(1-xp*xp)*dcos(xq)/a1
          z(2) = sqrt(1-xp*xp)*dsin(xq)/a2
          z(3) = xp/a3
          do i=1,3
            do j=1,3
              matk(i,j) = 0.d0
              matn(i,j) = 0.d0
              do k=1,3
                do l=1,3
                  g(i,j,k,l) = 0.d0
                enddo
              enddo
            enddo
          enddo
c... evaluation of matk
          do i=1,3
            do j=1,3
              do k=1,3
                do l=1,3
                  matk(i,j)=matk(i,j)+cmtens(i,j,k,l)*z(k)*z(l)
                enddo
              enddo
            enddo
          enddo
c... evaluation of matn
          do i=1,3
            do j=1,3
              do k=1,3
                do l=1,3
                  do m=1,3
                    do n=1,3
                      matn(i,j)=matn(i,j)+0.5d0*perm(i,k,l)*perm(j,m,n)
     &                          *matk(k,m)*matk(l,n)
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
c... evaluation of d
          d = 0.d0
          do i=1,3
            do j=1,3
              do k=1,3
                d = d+perm(j,k,i)*matk(j,1)*matk(k,2)*matk(i,3)
              enddo
            enddo
          enddo
c... evaluation of tensor g
          do i=1,3
            do j=1,3
              do k=1,3
                do l=1,3
                  g(i,j,k,l) = z(k)*z(l)*matn(i,j)/d
                enddo
              enddo
            enddo
          enddo
c... evaluation of eshelby tensor stens
          do i=1,3
            do j=1,3
              do k=1,3
                do l=1,3
                  do m=1,3
                    do n=1,3
                      stens(i,j,k,l)=stens(i,j,k,l)+(1.d0/(8.d0*pi))
     &                  *cmtens(m,n,k,l)*(g(i,m,j,n)+g(j,m,i,n))*wp*wq
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
c
c write stens in matrix form
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              s(ind(i,j),ind(k,l)) = stens(i,j,k,l)
            enddo
          enddo
        enddo
      enddo
c
      return
      end
      