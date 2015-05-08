c
      subroutine transf(ccom, l1, l2, l3, m1, m2, m3, n1, n2, n3)
      implicit none
      integer i, j
      real*8 ccom(6,6), tbar(6,6), temp1(6,6), temp2(6,6)
      real*8 l1, l2, l3, m1, m2, m3, n1, n2, n3
c------------------
c la matriz constitutiva ccom es reemplazada por su transpuesta
c en ejes globales, dependiendo de la orientación de las fibras
c
c c = tbar^t * c' * tbar (ref. eq. 1.49, barbero 2007)
c
c entrada:
c li,mi,ni = componentes de vectores unitarios dela base local
c ccom = matriz constitutiva del compuesto (6x6)
c
c salida:
c ccom = la matriz constitutiva del compuesto en el sistema coordenado
c        local es reemplazada por la matriz en coord. globales.
c
c (m. estrada)
c------------------
c
c asignar valores a tbar
      tbar(1,1) = l1**2
      tbar(1,2) = m1**2
      tbar(1,3) = n1**2
      tbar(1,4) = m1*n1
      tbar(1,5) = l1*n1
      tbar(1,6) = l1*m1
      tbar(2,1) = l2**2
      tbar(2,2) = m2**2
      tbar(2,3) = n2**2
      tbar(2,4) = m2*n2
      tbar(2,5) = l2*n2
      tbar(2,6) = l2*m2
      tbar(3,1) = l3**2
      tbar(3,2) = m3**2
      tbar(3,3) = n3**2
      tbar(3,4) = m3*n3
      tbar(3,5) = l3*n3
      tbar(3,6) = l3*m3
      tbar(4,1) = 2.d0*l2*l3
      tbar(4,2) = 2.d0*m2*m3
      tbar(4,3) = 2.d0*n2*n3
      tbar(4,4) = m2*n3+n2*m3
      tbar(4,5) = l2*n3+n2*l3
      tbar(4,6) = l2*m3+m2*l3
      tbar(5,1) = 2.d0*l1*l3
      tbar(5,2) = 2.d0*m1*m3
      tbar(5,3) = 2.d0*n1*n3
      tbar(5,4) = m1*n3+n1*m3
      tbar(5,5) = l1*n3+n1*l3
      tbar(5,6) = l1*m3+m1*l3
      tbar(6,1) = 2.d0*l1*l2
      tbar(6,2) = 2.d0*m1*m2
      tbar(6,3) = 2.d0*n1*n2
      tbar(6,4) = m1*n2+n1*m2
      tbar(6,5) = l1*n2+n1*l2
      tbar(6,6) = l1*m2+m1*l2
c transformación: c = tbar^t * c' * tbar
      do i=1,6
        do j=1,6
          temp1(i,j) = tbar(j,i)
        enddo
      enddo
      call matxmul(temp2, ccom, tbar, 6)
      call matxmul(ccom, temp1, temp2, 6)
      return
      end
      