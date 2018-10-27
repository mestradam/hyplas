      subroutine orcovm
     .(   ralgva     ,noutf1     ,noutf2     ,ntype      ,rstava     ,
     .    stres      ,ielem      ,iincs      ,igausp     ,outda      )
!***********************************************************************
! Output results (int and alg variables) 
! for composite material (plane strain and axisymmetric only).
!   
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   ralgva  (in)    : real algorithmic variables
!   noutf1  (in)    : regular output file
!   noutf2  (in)    : GiD output file
!   ntype   (in)    : Analysis type
!                       1   plane stress
!                       2   plane strain
!                       3   axisymmetric
!   rstava  (in)    : real state variables
!   stres   (in)    : stress array
!   ielem   (in)    : element number
!   iincs   (in)    : increment number
!   igausp  (in)    : Gauss point number
!   outda   (in)    : type of output
!***********************************************************************
!     
! Variable declaration
      implicit double precision (a-h,o-z)
      parameter(iphard=21  ,mstre=4)
!      
! ... arguments
      dimension  
     .    rstava(14) ,stres(*)   ,ralgva(*)
      data   r2   ,r3    / 2.0d0,3.0d0 /
!      
 1000 format(' S-eff = ',G12.4,' Eps.  = ',G12.4,' dgama = ',G12.4,/,
     .       ' damage = ',G12.4,' qmax = ',G12.4)
 1205 format('Result "INTERNAL-VARIABLES" "LOAD ANALYSIS"',I3,
     1       ' Vector OnGaussPoints "Board gauss internal"',/,
     2       'ComponentNames "S-EFF", "EPS", "DGAMA","DAMAGE","Q"',/,
     3       'Values')
 1206 format('Result "INTERNAL-VARIABLES-MAT" "LOAD ANALYSIS"',I3,
     1       ' Vector OnNodes ',/,
     2       'ComponentNames "S-EFF", "EPS", "DGAMA","DAMAGE","Q"',/,
     3       'Values')
 1211 format(i5,2x,5g15.6)
 1212 format(7x,5g15.6)
!
      epbar  = rstava(9)
      damage = rstava(10)
      qmax   = rstava(12)
      if(ielem.eq.1.and.igausp.eq.1.and.outda.eq.2)then
        write(noutf2,1205)iincs
      elseif(ielem.eq.1.and.outda.eq.3)then
        write(noutf2,1206)iincs
      endif
c
      if(ntype.eq.1)then
! Plane stress
        p=(stres(1)+stres(2))/r3
        effst=sqrt(r3/r2*((stres(1)-p)**2+(stres(2)-p)**2+
     1                     r2*stres(3)**2+p**2))
      elseif(ntype.eq.2.or.ntype.eq.3)then
! Plane strain and axisymmetric
        p=(stres(1)+stres(2)+stres(4))/r3
        effst=sqrt(r3/r2*((stres(1)-p)**2+(stres(2)-p)**2+
     1                     r2*stres(3)**2+(stres(4)-p)**2))
      endif
! Write to output file and GiD output file
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
