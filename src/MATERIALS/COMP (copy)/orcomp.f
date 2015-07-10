      subroutine orcomp(
     &  ralgva      ,noutf1     ,noutf2     ,ntype      ,rstava     ,
     &  stres       ,ielem      ,iincs      ,igausp     ,outda      )
c***********************************************************************
c Output results (int and alg variables) for composite material with two
c material components.
c
c (M. Estrada, 2011)
c-----------------------------------------------------------------------
c Subroutine arguments:
c
c   ralgva  (in)    : real algorithmic variables
c   noutf1  (in)    : regular output file
c   noutf2  (in)    : GiD output file
c   ntype   (in)    : Analysis type
c                       1   plane stress
c                       2   plane strain
c                       3   axisymmetric
c   rstava  (in)    : real state variables
c   stres   (in)    : stress array
c   ielem   (in)    : element number
c   iincs   (in)    : increment number
c   igausp  (in)    : Gauss point number
c   outda   (in)    : type of output
c***********************************************************************
c
c Variable declaration
      implicit double precision (a-h,o-z)
      logical unsym
      parameter(iphard=5  ,mstre=4)
      dimension  rstava(6), stres(*)    ,ralgva(2)
      data  r2  ,r3   / 2.0d0   ,3.0d0 /
c
c Printing formats
 1000 FORMAT(
     &' S-eff = ',G12.4,' Eps.  = ',G12.4,' dgama = ',G12.4 /,
     &' damage = ',G12.4,' qmax = ',G12.4)
 1205 FORMAT('Result "INTERNAL-VARIABLES" "LOAD ANALYSIS"',I3,
     &       ' Vector OnGaussPoints "Board gauss internal"',/,
     &       'ComponentNames "S-EFF", "EPS", "DGAMA", "DAMAGE", "Q"',/,
     &       'Values')
 1206 FORMAT('Result "INTERNAL-VARIABLES-MAT" "LOAD ANALYSIS"',I3,
     &       ' Vector OnNodes ',/,
     &       'ComponentNames "S-EFF", "EPS", "DGAMA", "DAMAGE", "Q"',/,
     &       'Values')
 1211 format(i5,2x,5g15.6)
 1212 format(7x,5g15.6)
c
      epbar=rstava(5)
      damage = rstava(6)
      qmax = ralgva(2)
      if(ielem.eq.1.and.igausp.eq.1.and.outda.eq.2)then
        write(noutf2,1205)iincs
      elseif(ielem.eq.1.and.outda.eq.3)then
        write(noutf2,1206)iincs
      endif
c
      if(ntype.eq.1)then
c Plane stress
        p=(stres(1)+stres(2))/r3
        effst=sqrt(r3/r2*((stres(1)-p)**2+(stres(2)-p)**2+
     &        r2*stres(3)**2+p**2))
      elseif(ntype.eq.2.or.ntype.eq.3)then
c Plane strain and axisymmetric
        p=(stres(1)+stres(2)+stres(4))/r3
        effst=sqrt(r3/r2*((stres(1)-p)**2+(stres(2)-p)**2+
     &        r2*stres(3)**2+(stres(4)-p)**2))
      endif
c
c Write to output file and GiD output file
      if(outda.eq.1)then
        write(noutf1,1000)effst,epbar,dgama,damage,qmax
      elseif(outda.eq.2)then
        if(igausp.eq.1)then
          write(noutf2,1211)ielem,effst,epbar,dgama,damage,qmax
        else
          write(noutf2,1212)effst,epbar,dgama,damage,qmax
        endif
      elseif(outda.eq.3)then
        write(noutf2,1211)ielem,effst,epbar,dgama,damage,qmax
      endif
      return
      end
