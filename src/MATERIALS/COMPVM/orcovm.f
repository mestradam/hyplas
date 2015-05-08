      subroutine orcovm
     &(   dgama      ,noutf      ,ntype      ,rstava     ,stres      )
      implicit double precision (a-h,o-z)
      parameter(iphard=20, mstre=4)
      dimension rstava(mstre+1), stres(*)
c local variables
      double precision volfr, a1, a2, a3, l1, l2, l3, m1, m2, m3, n1,
     &  n2, n3, densef, youngf, poissf, densem, youngm, poissm
c
c-----------------------------------------------------------------------
c output results (internal and algorithmic variables) for composite
c material with elastic fibers and elasto-plastic von mises matrix
c with non-linear isotropic hardening
c
c (m. estrada, 2011)
c-----------------------------------------------------------------------
c
 1000 format(' s-eff = ',g12.4,' eps.  = ',g12.4,' dgama = ',g12.4)
c
      epbar = rstava(mstre+1)
      if (ntype.eq.1) then
c plane stress
        p = (stres(1)+stres(2))/3.d0
        effst = sqrt(3.d0/2.d0*((stres(1)-p)**2+(stres(2)-p)**2+
     &    2.d0*stres(3)**2+p**2))
      elseif (ntype.eq.2 .or. ntype.eq.3) then
c plane strain and axisymetric
        p = (stres(1)+stres(2)+stres(4))/3.d0
        effst = sqrt(3.d0/2.d0*((stres(1)-p)**2+(stres(2)-p)**2+
     &    2.d0*stres(3)**2+(stres(4)-p)**2))
      endif
c write to output file
      write(noutf, 1000) effst, epbar, dgama
c
      return
      end
      