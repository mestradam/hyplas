c
      subroutine celast(c, young, poiss)
c
      implicit none
      integer i, j, k, l
      real*8 young, poiss, factor
      real*8 kdelta(3,3)
      real*8 c(6,6)
c------------------
c cálculo del tensor constitutivo de un material isótropo en 
c función de las propiedades de ingeniería del material (módulo de 
c elasticidad y relación de poisson)
c
c entrada:
c young = módulo de elasticidad (módulo de young)
c poiss = relación de poisson
c
c salida:
c cisot = tensor constitutivo isótropo (6x6)
c------------------
c
c initiate variables
      do i=1,6
        do j=1,6
          c(i,j) = 0.d0
        enddo
      enddo
      do i=1,3
        do j=1,3
          kdelta(i,j) = 0.d0
        enddo
        kdelta(i,i) = 1.d0
      enddo
      factor = young / ((1.d0 + poiss) * (1.d0 - 2.d0 * poiss))
c
c compute constitutive matrix (6x6)
      c(1,1) = factor * (1.d0 - poiss)
      c(1,2) = factor * poiss
      c(1,3) = factor * poiss
      c(2,1) = factor * poiss
      c(2,2) = factor * (1.d0 - poiss)
      c(2,3) = factor * poiss
      c(3,1) = factor * poiss
      c(3,2) = factor * poiss
      c(3,3) = factor * (1.d0 - poiss)
      c(4,4) = factor * (1.d0 - 2.d0 * poiss)
      c(5,5) = factor * (1.d0 - 2.d0 * poiss)
      c(6,6) = factor * (1.d0 - 2.d0 * poiss)
c
      return
      end
      