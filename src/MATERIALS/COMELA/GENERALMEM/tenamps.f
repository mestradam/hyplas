c
      subroutine tenamps(amt, adil, ki)
c
      implicit none
      integer i, j
      real*8 ki, km
      real*8 temp1(3,3), temp2(3,3), temp3(3,3)
      real*8 amt(3,3), adil(3,3)
      
c----------------------------
c computes the mori-tanaka strain concentration tensor "amt" for the 
c composite based on eshelby's dilute strain concentration tensor "adil"
c m. estrada
c
c amt = adil*inv(km*i+ki*adil) ..... eq 9a (benveniste, 1987)
c 
c input:
c       adil = eshelby dilute strain concentration tensor
c       ki = volume fraction of inclusions
c output:
c       amt = mori-tanaka strain concentration tensor
c----------------------------
c
      km = 1.d0-ki
      do i=1,3
        do j=1,3
          temp1(i,j) = ki*adil(i,j)
        enddo
        temp1(i,i) = km + temp1(i,i)
      enddo
      call matxinv(temp2,temp1,3)
      call matxmul(amt, adil, temp2, 3)
c
      return
      end
      