      SUBROUTINE SWCOMELA(MODE, NTYPE, RSTAVC, RSTAVL, STRESC, STRESL)
      IMPLICIT NONE
      INTEGER MODE, NTYPE, NSTRE, ISTRE
      REAL*8 RSTAVC, RSTAVL, STRESC, STRESL
      DIMENSION RSTAVC(*), RSTAVL(*), STRESC(*), STRESL(*)
C------------------
C INITIALISE/SWITCH DATA FOR THE ELASTIC MATERIAL MODEL
C
C   MODE=0:   Initialises the relevant data.
C
C   MODE=1:   Assigns current values of the state variables to
C             converged solution (when the current iteration
C             satisfies the convergence criterion).
C
C   MODE=2:   Assigns the last converged solution to current state
C             variables values (when a new iteration is required by
C             the iterative process).
C
C   MODE=3:   Assigns the last converged solution to current state
C             variables values (when increment cutting is required).
C
C (M. ESTRADA - 2010)
C------------------
C
      IF(NTYPE.EQ.1.OR.NTYPE.EQ.2.OR.NTYPE.EQ.3)NSTRE=4
C
      IF(MODE.EQ.0)THEN
C Initialisation mode
        CALL RVZERO(STRESC,NSTRE)
C RSTAVA stores the infinitesimal egineering strain tensor component
C in small strains and the logarithmic eng. strains in large strain
C analysis
        CALL RVZERO(RSTAVC,NSTRE)
      ELSE
C
C Switching mode
        IF(MODE.EQ.1)THEN
          DO ISTRE=1,NSTRE
            STRESL(ISTRE)=STRESC(ISTRE)
            RSTAVL(ISTRE)=RSTAVC(ISTRE)
          ENDDO
        ELSEIF(MODE.EQ.2.OR.MODE.EQ.3)THEN
          DO ISTRE=1,NSTRE
            STRESC(ISTRE)=STRESL(ISTRE)
            RSTAVC(ISTRE)=RSTAVL(ISTRE)
          ENDDO
        ENDIF
      ENDIF
      RETURN
      END
C
