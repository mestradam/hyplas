      subroutine norm6(norm, vect)
      implicit real*8 (a-h , o-z)
c output arguments
      real*8 norm
c input arguments
      real*8 vect(6)
c
      norm = sqrt(vect(1)**2+vect(2)**2+vect(3)**2+2.d0*(vect(4)**2+
     &  vect(5)**2+vect(6)**2))
c
      return
      end
      