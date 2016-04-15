      SUBROUTINE ORCOVM
     1(   ralgva  ,NOUTF1   ,NOUTF2  ,NTYPE   ,RSTAVA   ,STRES,
     2    IELEM   ,IINCS    ,IGAUSP  ,OUTDA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=21  ,MSTRE=4)
      DIMENSION  RSTAVA(13), STRES(*)  ,ralgva(*)
      DATA   R2   ,R3    / 2.0D0,3.0D0 /
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
 1000 FORMAT(' S-eff = ',G12.4,' Eps.  = ',G12.4,' dgama = ',G12.4)
 1205 FORMAT('Result "INTERNAL-VARIABLES" "LOAD ANALYSIS"',I3,
     1       ' Vector OnGaussPoints "Board gauss internal"',/,
     2       'ComponentNames "S-EFF", "EPS", "DGAMA"',/,
     3       'Values')
 1206 FORMAT('Result "INTERNAL-VARIABLES-MAT" "LOAD ANALYSIS"',I3,
     1       ' Vector OnNodes ',/,
     2       'ComponentNames "S-EFF", "EPS", "DGAMA"',/,
     3       'Values')
 1211 FORMAT(I5,2X,3G15.6)
 1212 FORMAT(7X,3G15.6)
!
      EPBAR=RSTAVA(9)
      IF(IELEM.EQ.1.AND.IGAUSP.EQ.1.AND.OUTDA.EQ.2)THEN
        WRITE(NOUTF2,1205)IINCS
      ELSEIF(IELEM.EQ.1.AND.OUTDA.EQ.3)THEN
        WRITE(NOUTF2,1206)IINCS
      ENDIF
c
      IF(NTYPE.EQ.1)THEN
! Plane stress
        P=(STRES(1)+STRES(2))/R3
        EFFST=SQRT(R3/R2*((STRES(1)-P)**2+(STRES(2)-P)**2+
     1                     R2*STRES(3)**2+P**2))
      ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
! Plane strain and axisymmetric
        P=(STRES(1)+STRES(2)+STRES(4))/R3
        EFFST=SQRT(R3/R2*((STRES(1)-P)**2+(STRES(2)-P)**2+
     1                     R2*STRES(3)**2+(STRES(4)-P)**2))
      ENDIF
! Write to output file and GiD output file
      IF(OUTDA.EQ.1)THEN
        WRITE(NOUTF1,1000)EFFST,EPBAR,DGAMA
      ELSEIF(OUTDA.EQ.2)THEN
        IF(IGAUSP.EQ.1)THEN
          WRITE(NOUTF2,1211)IELEM,EFFST,EPBAR,DGAMA
        ELSE
          WRITE(NOUTF2,1212)EFFST,EPBAR,DGAMA
        ENDIF
      ELSEIF(OUTDA.EQ.3)THEN
        WRITE(NOUTF2,1211)IELEM,EFFST,EPBAR,DGAMA
      ENDIF
      RETURN
      END
