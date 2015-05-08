      SUBROUTINE SOLQUA
     1(   A          ,B          ,C          ,ONEROO     ,TWOROO     ,
     2    ROOT1      ,ROOT2      )
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL  ONEROO ,TWOROO
      DATA
     1    R0   ,R1   ,R2   ,R4   ,SMALL /
     2    0.D0 ,1.0D0,2.0D0,4.0D0,1.D-12/
C***********************************************************************
C FINDS THE REAL ROOTS OF A QUADRATIC EQUATION:  A X**2 + B X + C = 0.
C
C REFERENCE:
C W.H.Press, S.A.Teukolsky, W.T.Vetterling and B.P.Flannery. Numerical
C recipes in FORTRAN. The art of scientific computing. 2nd Ed.,
C Cambridge Univ. Press, 1992. (Section 5.6)
C***********************************************************************
C Initialises logical flags
      ONEROO=.FALSE.
      TWOROO=.FALSE.
      IF(A.NE.R0)THEN
C The equation is non-linear in fact
C ----------------------------------
        IF(B.NE.R0)THEN
          SIGNB=B/ABS(B)
        ELSE
          SIGNB=R1
        ENDIF
        B2=B*B
        R4AC=R4*A*C
        SQUAR=B2-R4AC
        IF(SQUAR.GT.R0)THEN
C there are two distinct real roots: uses formula which minimises
C round-off errors when the coefficients A and/or C are small
          TWOROO=.TRUE.
          SQUAR=SQRT(SQUAR)
          Q=-(B+SIGNB*SQUAR)/R2
          ROOT1=Q/A
          ROOT2=C/Q
        ELSEIF(SQUAR.EQ.R0.OR.
     1         (SQUAR/DMAX1(B2,ABS(R4AC))+SMALL).GE.R0)THEN
C there is only one root
          ONEROO=.TRUE.
          ROOT1=-B/(R2*A)
        ENDIF
      ELSE
C The equation is linear
C ----------------------
        IF(B.NE.R0)THEN
C and well defined -> (only) one root exists
          ONEROO=.TRUE.
          ROOT1=-C/B
        ENDIF
      ENDIF
      RETURN
      END
