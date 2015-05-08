      SUBROUTINE UPCONF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Hyplas database
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C***********************************************************************
C KINEMATIC/GEOMETRIC CONFIGUTATION UPDATE:
C GIVEN THE ITERATIVE DISPLACEMENTS, THIS ROUTINE UPDATES THE GLOBAL
C ARRAYS OF INCREMENTAL AND TOTAL DISPLACEMENTS.
C FOR GEOMETRICALLY NON-LINEAR ANALYSES (LARGE DEFORMATIONS) IT ALSO
C UPDATES THE CURRENT NODAL COORDINATES.
C
C REFERENCE: Figures 5.2-3
C***********************************************************************
C
C Update incremental and total displacements
C ==========================================
C
      DO 10 ITOTV=1,NTOTV
        DINCR(ITOTV)=DINCR(ITOTV)+DITER(ITOTV)
        TDISP(ITOTV)=TDISP(ITOTV)+DITER(ITOTV)
   10 CONTINUE
C
C Update current nodal coordinates for large deformation analyses
C ===============================================================
C
      IF(NLARGE.EQ.1)THEN
        DO 30 IPOIN=1,NPOIN
          NPOSN=(IPOIN-1)*NDOFN
          DO 20 IDOFN=1,NDIME
            NPOSN=NPOSN+1
            COORD(IDOFN,IPOIN,1)=COORD(IDOFN,IPOIN,2)+DINCR(NPOSN) 
   20     CONTINUE
   30   CONTINUE
      ENDIF
C
      RETURN
      END
