      SUBROUTINE CONVER
     1(   CONVRG     ,DIVERG     ,IITER      ,TOLER     ,TFACT      )
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Hyplas database: Global parameters and common blocks
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C
      LOGICAL CONVRG,DIVERG
      SAVE RATOLD ,REMOLD
      DATA R0   ,R20   ,R100   ,R1000   /
     1     0.0D0,20.0D0,100.0D0,1000.0D0/
C***********************************************************************
C COMPUTE GLOBAL RESIDUAL (OUT-OF-BALANCE) FORCE VECTOR AND ITS RELATIVE
C NORM AND SET EQUILIBRIUM CONVERGENCE FLAG
C
C REFERENCE: Expressions (4.72) and (4.77)
C***********************************************************************
 1000 FORMAT(6X,I3,19X,G14.6,15X,G14.6)
 1010 FORMAT(6X,I3,11X,G14.6,6X,G14.6,6X,G14.6)
C Initialize relevant variables
C -----------------------------
      CONVRG=.FALSE.
      DIVERG=.FALSE.
      RESID=R0
      RETOT=R0
      REMAX=R0
C Evaluate global nodal internal and external forces
C --------------------------------------------------
      CALL RVZERO(STFOR,NTOTV)
      CALL RVZERO(TOFOR,NTOTV)
      DO 30 IELEM=1,NELEM
        IGRUP =IGRPID(IELEM)
        IELIDN=IELTID(IGRUP)
        NNODE =IELPRP(3,IELIDN)
        KEVAB=0
        DO 20 INODE=1,NNODE
          LOCNO=IABS(LNODS(IELEM,INODE))
          DO 10 IDOFN=1,NDOFN
            KEVAB=KEVAB+1
            NPOSI=MASTER((LOCNO-1)*NDOFN+IDOFN)
C current internal force
            STFOR(NPOSI)=STFOR(NPOSI)+ELOAD(KEVAB,IELEM)
C current external force
            TOFOR(NPOSI)=TOFOR(NPOSI)+TFACT*RLOAD(KEVAB,IELEM)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C Loop over nodal points
C ----------------------
      DO 70 IPOIN=1,NPOIN
        ISVAB=(IPOIN-1)*NDOFN
C Search for node in prescribed displacements
        DO 60 IVFIX=1,NVFIX
          IF(NOFIX(IVFIX).EQ.IPOIN.AND.ANGLE(IVFIX).NE.R0)THEN
C Rotate forces to local nodal coordinate system for prescribed
C displacements at an angle (for 2-D only)
            C=COS(ANGLE(IVFIX))
            S=SIN(ANGLE(IVFIX))
            ISVAB=ISVAB+1
            JSVAB=ISVAB+1
            GASHI= C*STFOR(ISVAB)+S*STFOR(JSVAB)
            GASHJ=-S*STFOR(ISVAB)+C*STFOR(JSVAB)
            STFOR(ISVAB)=GASHI
            STFOR(JSVAB)=GASHJ
            GASHI= C*TOFOR(ISVAB)+S*TOFOR(JSVAB)
            GASHJ=-S*TOFOR(ISVAB)+C*TOFOR(JSVAB)
            TOFOR(ISVAB)=GASHI
            TOFOR(JSVAB)=GASHJ
C Evaluate reactions
            KSVAB=ISVAB-1
            DO 40 IDOFN=1,NDOFN
            KSVAB=KSVAB+1
            IF(IFFIX(KSVAB).NE.0)THEN
              TREAC(IVFIX,IDOFN)=STFOR(KSVAB)-TOFOR(KSVAB)
              TOFOR(KSVAB)=TOFOR(KSVAB)+TREAC(IVFIX,IDOFN)
            ELSE
              TREAC(IVFIX,IDOFN)=R0
            ENDIF
   40       CONTINUE
C Rotate forces and reactions back to global system
            GASHI= C*STFOR(ISVAB)-S*STFOR(JSVAB)
            GASHJ= S*STFOR(ISVAB)+C*STFOR(JSVAB)
            STFOR(ISVAB)=GASHI
            STFOR(JSVAB)=GASHJ
            GASHI= C*TOFOR(ISVAB)-S*TOFOR(JSVAB)
            GASHJ= S*TOFOR(ISVAB)+C*TOFOR(JSVAB)
            TOFOR(ISVAB)=GASHI
            TOFOR(JSVAB)=GASHJ
            GASHI= C*TREAC(IVFIX,1)-S*TREAC(IVFIX,2)
            GASHJ= S*TREAC(IVFIX,1)+C*TREAC(IVFIX,2)
            TREAC(IVFIX,1)=GASHI
            TREAC(IVFIX,2)=GASHJ
            GOTO 70
          ELSEIF(NOFIX(IVFIX).EQ.IPOIN)THEN
C Evaluate reactions
            DO 50 IDOFN=1,NDOFN
              ISVAB=ISVAB+1
              IF(IFFIX(ISVAB).NE.0)THEN
                TREAC(IVFIX,IDOFN)=STFOR(ISVAB)-TOFOR(ISVAB)
                TOFOR(ISVAB)=TOFOR(ISVAB)+TREAC(IVFIX,IDOFN)
              ELSE
                TREAC(IVFIX,IDOFN)=R0
              ENDIF
   50       CONTINUE
            GOTO 70
          ENDIF
   60   CONTINUE
   70 CONTINUE
C Evaluate residual and external force norm
C -----------------------------------------
      DO 80 ITOTV=1,NTOTV
        REFOR=TOFOR(ITOTV)-STFOR(ITOTV)
        RESID=RESID+REFOR*REFOR
        RETOT=RETOT+TOFOR(ITOTV)*TOFOR(ITOTV)
C maximum nodal residual
        AGASH=ABS(REFOR)
        IF(AGASH.GT.REMAX)REMAX=AGASH
   80 CONTINUE
C Euclidean norm of residual
      RESID=SQRT(RESID)
C Euclidean norm of external force
      RETOT=SQRT(RETOT)
C compute relative residual norm
      IF(RETOT.EQ.R0)THEN
        RATIO=R0
      ELSE
        RATIO=R100*RESID/RETOT
      ENDIF
      IF(NALGO.GT.0)THEN
        WRITE(16,1000)IITER,RATIO,REMAX
        WRITE(*,1000) IITER,RATIO,REMAX
      ELSE
        WRITE(16,1010)IITER,RATIO,REMAX,TFACT
        WRITE(*,1010) IITER,RATIO,REMAX,TFACT
      ENDIF
C Set convergence/divergence flags
C --------------------------------
      IF(RATIO.LE.TOLER.OR.ABS(REMAX).LE.(TOLER/R1000))CONVRG=.TRUE.
      IF(IITER.NE.1.AND.(RATIO.GT.R20*RATOLD.OR.REMAX.GT.R20*REMOLD))
     1  DIVERG=.TRUE.
      RATOLD=RATIO
      REMOLD=REMAX
C Evaluate element residual forces before exit -> store it in ELOAD
C -----------------------------------------------------------------
      DO 100 IELEM=1,NELEM
        IGRUP=IGRPID(IELEM)
        IELIDN=IELTID(IGRUP)
        NEVAB=IELPRP(5,IELIDN)
C Current element residual (out-of-balance force) = current element
C external load - current element internal force
        DO 90 IEVAB=1,NEVAB
          ELOAD(IEVAB,IELEM)=TFACT*RLOAD(IEVAB,IELEM)-ELOAD(IEVAB,IELEM)
   90   CONTINUE
  100 CONTINUE
C
      RETURN
      END
