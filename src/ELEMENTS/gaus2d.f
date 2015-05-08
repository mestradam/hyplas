      SUBROUTINE GAUS2D
     1(   DOMAIN     ,NGAUS      ,POSGP      ,WEIGP      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*3 DOMAIN
      DIMENSION
     1    POSGP(2,*)         ,WEIGP(*)
C***********************************************************************
C SET SAMPLING POINTS POSITIONS AND WEIGHTS FOR GAUSSIAN NUMERICAL
C INTEGRATION RULES IN 2-D
C
C REFERENCE: Expression (4.31)
C            OC Zienkiewicz & RL Taylor. The finite element method,
C            Volume 1: The basis. 5th Edn. Butterworth Heinemann, 2000.
C            J Fish & T Belytschko. A first course in finite element
C            analysis. Wiley, Chichester, 2007.
C***********************************************************************
      IF(DOMAIN.EQ.'QUA')THEN
C
C Integration over quadrilateral domain with vertices
C                                         {(1,1),(1,-1),(-1,-1),(-1,1)}
C
        IF(NGAUS.EQ.1)THEN
          POSGP(1,1)=0.0D0
          POSGP(2,1)=0.0D0
          WEIGP(1)=4.0D0
        ELSEIF(NGAUS.EQ.4)THEN
          POSGP(1,1)=-0.577350269189626D0
          POSGP(2,1)=-0.577350269189626D0
          WEIGP(1)=1.0D0
          POSGP(1,2)=-0.577350269189626D0
          POSGP(2,2)=+0.577350269189626D0
          WEIGP(2)=1.0D0
          POSGP(1,3)=+0.577350269189626D0
          POSGP(2,3)=-0.577350269189626D0
          WEIGP(3)=1.0D0
          POSGP(1,4)=+0.577350269189626D0
          POSGP(2,4)=+0.577350269189626D0
          WEIGP(4)=1.0D0
        ELSEIF(NGAUS.EQ.5)THEN
          POSGP(1,1)=-0.774596669241483D0
          POSGP(2,1)=-0.774596669241483D0
          WEIGP(1)=0.555555555555556D0
          POSGP(1,2)=-0.774596669241483D0
          POSGP(2,2)=+0.774596669241483D0
          WEIGP(2)=0.555555555555556D0
          POSGP(1,3)=+0.774596669241483D0
          POSGP(2,3)=-0.774596669241483D0
          WEIGP(3)=0.555555555555556D0
          POSGP(1,4)=+0.774596669241483D0
          POSGP(2,4)=+0.774596669241483D0
          WEIGP(4)=0.555555555555556D0
          POSGP(1,5)=+0.0D0
          POSGP(2,5)=+0.0D0
          WEIGP(5)=1.777777777777778D0
        ELSEIF(NGAUS.EQ.6)THEN
          POSGP(1,1)=-0.577350269189626D0
          POSGP(2,1)=-0.577350269189626D0
          WEIGP(1)=1.0D0
          POSGP(1,2)=-0.577350269189626D0
          POSGP(2,2)=+0.577350269189626D0
          WEIGP(2)=1.0D0
          POSGP(1,3)=+0.577350269189626D0
          POSGP(2,3)=-0.577350269189626D0
          WEIGP(3)=1.0D0
          POSGP(1,4)=+0.577350269189626D0
          POSGP(2,4)=+0.577350269189626D0
          WEIGP(4)=1.0D0
          POSGP(1,5)=+0.0D0
          POSGP(2,5)=+0.0D0
          WEIGP(5)=+0.0D0
          POSGP(1,6)=+0.0D0
          POSGP(2,6)=+0.0D0
          WEIGP(6)=+4.0D0
        ELSEIF(NGAUS.EQ.9)THEN
          POSGP(1,1)=-0.774596669241483D0
          POSGP(2,1)=-0.774596669241483D0
          WEIGP(1)=0.308641975308643D0
          POSGP(1,2)=-0.774596669241483D0
          POSGP(2,2)=+0.0D0
          WEIGP(2)=0.493827160493828D0
          POSGP(1,3)=-0.774596669241483D0
          POSGP(2,3)=+0.774596669241483D0
          WEIGP(3)=0.308641975308643D0
          POSGP(1,4)=+0.0D0
          POSGP(2,4)=-0.774596669241483D0
          WEIGP(4)=0.493827160493828D0
          POSGP(1,5)=+0.0D0
          POSGP(2,5)=+0.0D0
          WEIGP(5)=0.790123456790124D0
          POSGP(1,6)=+0.0D0
          POSGP(2,6)=+0.774596669241483D0
          WEIGP(6)=0.493827160493828D0
          POSGP(1,7)=+0.774596669241483D0
          POSGP(2,7)=-0.774596669241483D0
          WEIGP(7)=0.308641975308643D0
          POSGP(1,8)=+0.774596669241483D0
          POSGP(2,8)=+0.0D0
          WEIGP(8)=0.493827160493828D0
          POSGP(1,9)=+0.774596669241483D0
          POSGP(2,9)=+0.774596669241483D0
          WEIGP(9)=0.308641975308643D0
        ELSE
          CALL ERRPRT('EI0001')
        ENDIF
      ELSEIF(DOMAIN.EQ.'TRI')THEN
C
C Integration over triangular domain with vertices {(0,0),(1,0),(0,1)}
C
        IF(NGAUS.EQ.1)THEN
          POSGP(1,1)=0.333333333333333D0
          POSGP(2,1)=0.333333333333333D0
          WEIGP(1)=0.5D0
        ELSEIF(NGAUS.EQ.3)THEN
          POSGP(1,1)=0.166666666666667D0
          POSGP(2,1)=0.166666666666667D0
          WEIGP(1)=0.166666666666667D0
          POSGP(1,2)=0.666666666666667D0
          POSGP(2,2)=0.166666666666667D0
          WEIGP(2)=0.166666666666667D0
          POSGP(1,3)=0.166666666666667D0
          POSGP(2,3)=0.666666666666667D0
          WEIGP(3)=0.166666666666667D0
        ELSE
          CALL ERRPRT('EI0002')
        ENDIF
      ELSE
        CALL ERRPRT('EI0003')
      ENDIF
C
      RETURN
      END
