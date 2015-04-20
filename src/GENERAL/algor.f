      SUBROUTINE ALGOR(IINCS ,IITER ,KRESL ,KUNLD )
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Hyplas database: Global parameters and common blocks
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C Numerical constants
      DATA R0   /0.0D0/
C***********************************************************************
C SETS EQUATION RESOLUTION INDEX, KRESL, ACCORDING TO SELECTED ITERATIVE
C ALGORITHM FOR SOLUTION OF THE NON-LINEAR EQUILIBRIUM PROBLEM
C
C REFERENCE: Section 5.4.4
C***********************************************************************
C
C Set KRESL
C ---------
      KRESL=2
      IABSN=IABS(NALGO)
C Initial stiffness method
      IF(IABSN.EQ.1.AND.IINCS.EQ.1.AND.IITER.EQ.1) KRESL=1
C Newton-Raphson tangential stiffness method
      IF(IABSN.EQ.2) KRESL=1
C Modified Newton KT1
      IF(IABSN.EQ.3.AND.IITER.EQ.1) KRESL=1
C Modified Newton KT2
      IF(IABSN.EQ.4.AND.IINCS.EQ.1.AND.IITER.EQ.1) KRESL=1
      IF(IABSN.EQ.4.AND.IITER.EQ.1.AND.KUNLD.EQ.1) KRESL=1
      IF(IABSN.EQ.4.AND.IITER.EQ.2) KRESL=1
C Secant Newton - Initial stiffness
      IF(IABSN.EQ.5.AND.IINCS.EQ.1.AND.IITER.EQ.1) KRESL=1
C Secant Newton - KT1
      IF(IABSN.EQ.6.AND.IITER.EQ.1) KRESL=1
C Secant Newton - KT2
      IF(IABSN.EQ.7.AND.IINCS.EQ.1.AND.IITER.EQ.1) KRESL=1
      IF(IABSN.EQ.7.AND.IITER.EQ.1.AND.KUNLD.EQ.1) KRESL=1
      IF(IABSN.EQ.7.AND.IITER.EQ.2) KRESL=1
C
C Zero prescribed displacements if not first iteration
C ----------------------------------------------------
      IF(IITER.GT.1)THEN
        NRHS=1
        IF(NALGO.LT.0)NRHS=2        
        DO 20 ITOTV = 1,NTOTV
          DO 10 IRHS=1,NRHS
            FIXED(ITOTV,IRHS)=R0
   10     CONTINUE
   20   CONTINUE
      ENDIF
C
      RETURN
      END
