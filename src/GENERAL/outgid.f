      SUBROUTINE OUTGID
     1        (TFACT, IINCS, IITER, NOUTP, OUTDA)
          IMPLICIT DOUBLE PRECISION (A - H, O - Z)
C Hyplas database
          INCLUDE '../MAXDIM.INC'
          INCLUDE '../MATERIAL.INC'
          INCLUDE '../ELEMENTS.INC'
          INCLUDE '../GLBDBASE.INC'
C
          DIMENSION NOUTP(5)
C
          DIMENSION
     1        DERIV(MDIME, MNODE), ELCOD(MDIME, MNODE), GPCOD(MDIME),
     2        PSTRS(3), SHAPE(MNODE)
          DATA R0/0.0D0/
C***********************************************************************
C OUTPUTS DISPLACEMENTS, REACTIONS, STRESSES AND OTHER STATE AND
C ALGORITHMIC VARIABLES TO GiD RESULTS FILE
C
C REFERENCE: Linero D.L. (2009), GiD User Guide (2008)
C***********************************************************************
C
C      Format to GiD Postprocess
C	 =========================
C
 1032 FORMAT('Result "DISPLACEMENT" "LOAD ANALYSIS" ', I3,
     1    ' Vector OnNodes', /,
     2    'ComponentNames "X-DISPLACEMENT", "Y-DISPLACEMENT"', /,
     3    'Values')
 1201 FORMAT('Result "STRESS" "LOAD ANALYSIS"', I3,
     1    ' Vector OnGaussPoints "Board gauss internal"', /,
     2    'ComponentNames "S-XX", "S-YY", "S-XY"', /, 'Values')
 1202 FORMAT('Result "STRESS" "LOAD ANALYSIS"', I3,
     1    ' Vector OnGaussPoints "Board gauss internal"', /,
     2    'ComponentNames "S-XX", "S-YY", "S-XY", "S-ZZ"', /, 'Values')
 1203 FORMAT('Result "STRESS" "LOAD ANALYSIS"', I3,
     1    ' Vector OnGaussPoints "Board gauss internal"', /,
     2    'ComponentNames "S-RR", "S-ZZ", "S-RZ", "S-H"', /, 'Values')
 1204 FORMAT('Result "STRESS" "LOAD ANALYSIS"', I3,
     1    ' Vector OnGaussPoints "Board gauss internal"', /,
     2    'ComponentNames "S-XX", "S-RR", "S-XR", "S-H"', /, 'Values')
 1205 FORMAT('Result "PRINCIPAL-STRESS" "LOAD ANALYSIS"', I3,
     1    ' Vector OnGaussPoints "Board gauss internal"', /,
     2    'ComponentNames "S-MAX", "S-MIN", "S-MED"', /, 'Values')
 1206 FORMAT('Result "VARIABLES INTERNAS" "LOAD ANALYSIS"',I3, 'Values')
 1060 FORMAT(I5, 2X, 6G15.6)
 1211 FORMAT(I5, 2X, 4G15.6)
 1212 FORMAT(7X, 4G15.6)
 1215 FORMAT('End values', /)
 1220 FORMAT(2G15.6)
C
C Set output flags
C ================
C
C      GiD postprocess - Output displacements
C      ======================================
C
          WRITE(22, 1032) IINCS
C
          DO 10 IPOIN = 1, NPOIN
              NGASH = IPOIN * NDOFN
              NGISH = NGASH - NDOFN + 1
              WRITE(22, 1060) IPOIN,(TDISP(IGASH),IGASH=NGISH,NGASH)
 10           CONTINUE
          WRITE(22, 1215)

C
C      Stresses and other state and algorithmic variables at gauss points
C      ==================================================================
C
C      GiD postprocess - Current Stresses
C      ==================================
C
          IF (NTYPE .EQ. 1)THEN
              LSTRE = 3
              WRITE(22, 1201) IINCS
          ELSEIF (NTYPE .EQ. 2)THEN
              LSTRE = 4
              WRITE(22, 1202) IINCS
          ELSEIF (NTYPE .EQ. 3)THEN
              LSTRE = 4
              IF (NAXIS .EQ. 1)THEN
                  WRITE(22, 1203) IINCS
              ELSE
                  WRITE(22, 1204) IINCS
              ENDIF
          ENDIF
c ---> este ciclo no se cierra!!!!!!! se debe cerrar ahi....
          DO 120 IELEM = 1, NELEM
              IGRUP = IGRPID(IELEM)
              IELIDN = IELTID(IGRUP)
              IELTYP = IELPRP(1, IELIDN)
              NNODE = IELPRP(3, IELIDN)
              NGAUSP = IELPRP(4, IELIDN)
C
C      Evaluate Gauss point coordinates
C
              DO 91 INODE = 1, NNODE
                  LNODE = IABS(LNODS(IELEM, INODE))
                  DO 90 IDIME = 1, NDIME
                      ELCOD(IDIME, INODE) = COORD(IDIME, LNODE, 1)
 90                   CONTINUE
 91               CONTINUE
              IPPOS = 1
              DO 110 IGAUSP = 1, NGAUSP
                  EXISP = RELPRP(IPPOS - 1 + IGAUSP * 2 - 1, IELIDN)
                  ETASP = RELPRP(IPPOS - 1 + IGAUSP * 2, IELIDN)
                  CALL SHPFUN
     1                (DERIV, ETASP, EXISP, 0, IELTYP,
     2                MDIME, SHAPE)
                  CALL GETGCO
     1                (GPCOD, ELCOD, MDIME, NDIME, NNODE,
     2                SHAPE)
C
C      Output - Current Stresses
C
                  IF (IGAUSP .EQ. 1)THEN
      WRITE(22, 1211) IELEM, (STRSG(I, IGAUSP, IELEM, 1), I = 1, LSTRE)
                  ELSE
      WRITE(22, 1212) (STRSG(I, IGAUSP, IELEM, 1), I = 1, LSTRE)
                  ENDIF
 110              CONTINUE
 120          CONTINUE
          WRITE(22, 1215)
C
C      GiD postprocess - Principal Stresses
C      ====================================
C
          WRITE(22, 1205) IINCS
          DO 140 IELEM = 1, NELEM
              IGRUP = IGRPID(IELEM)
              IELIDN = IELTID(IGRUP)
              IELTYP = IELPRP(1, IELIDN)
              NNODE = IELPRP(3, IELIDN)
              NGAUSP = IELPRP(4, IELIDN)
C
C      Evaluate Gauss point coordinates
C
              DO 111 INODE = 1, NNODE
                  LNODE = IABS(LNODS(IELEM, INODE))
                  DO 112 IDIME = 1, NDIME
                      ELCOD(IDIME, INODE) = COORD(IDIME, LNODE, 1)
 112                  CONTINUE
 111              CONTINUE
              IF (NTYPE .EQ. 1)THEN
                  LSTRE = 3
              ELSEIF (NTYPE .EQ. 2)THEN
                  LSTRE = 4
              ELSEIF (NTYPE .EQ. 3)THEN
                  LSTRE = 4
              ENDIF
              IPPOS = 1
              DO 130 IGAUSP = 1, NGAUSP
                  EXISP = RELPRP(IPPOS - 1 + IGAUSP * 2 - 1, IELIDN)
                  ETASP = RELPRP(IPPOS - 1 + IGAUSP * 2, IELIDN)
                  CALL SHPFUN
     1                (DERIV, ETASP, EXISP, 0, IELTYP,
     2                MDIME, SHAPE)
                  CALL GETGCO
     1                (GPCOD, ELCOD, MDIME, NDIME, NNODE,
     2                SHAPE)
C
                  CALL PRINC2(PSTRS, STRSG(1, IGAUSP, IELEM, 1))
C
C      Output - Principal Stresses
C
                  IF (IGAUSP .EQ. 1)THEN
                      WRITE(22, 1211) IELEM, (PSTRS(I), I = 1, 3)
                  ELSE
                      WRITE(22, 1212) (PSTRS(I), I = 1, 3)
                  ENDIF
 130              CONTINUE
 140          CONTINUE
          WRITE(22, 1215)
C      ENDIF
C segun parece habia error con este ENDIF....., cerraba un ciclo superior....
C
C      GiD postprocess - Internal and State Variables
C      ==============================================
C
          DO 200 IELEM = 1, NELEM
              IGRUP = IGRPID(IELEM)
              IELIDN = IELTID(IGRUP)
              IELTYP = IELPRP(1, IELIDN)
              NNODE = IELPRP(3, IELIDN)
              NGAUSP = IELPRP(4, IELIDN)
C
C      Evaluate Gauss point coordinates
C
              DO 114 INODE = 1, NNODE
                  LNODE = IABS(LNODS(IELEM, INODE))
                  DO 115 IDIME = 1, NDIME
                      ELCOD(IDIME, INODE) = COORD(IDIME, LNODE, 1)
 115                  CONTINUE
 114              CONTINUE
              IF (NTYPE .EQ. 1)THEN
                  LSTRE = 3
              ELSEIF (NTYPE .EQ. 2)THEN
                  LSTRE = 4
              ELSEIF (NTYPE .EQ. 3)THEN
                  LSTRE = 4
              ENDIF
              IPPOS = 1
              DO 190 IGAUSP = 1, NGAUSP
                  EXISP = RELPRP(IPPOS - 1 + IGAUSP * 2 - 1, IELIDN)
                  ETASP = RELPRP(IPPOS - 1 + IGAUSP * 2, IELIDN)
                  CALL SHPFUN
     1                (DERIV, ETASP, EXISP, 0, IELTYP,
     2                MDIME, SHAPE)
                  CALL GETGCO
     1                (GPCOD, ELCOD, MDIME, NDIME, NNODE,
     2                SHAPE)

C
C      Output other (material-specific) state and algorithmic variables
C
                  CALL MATIOR
     1  (NTYPE, IPROPS(1, MATTID(IGRPID(IELEM))),
     2  RALGVA(1, IGAUSP, IELEM, 1), RPROPS(1, MATTID(IGRPID(IELEM))),
     3  RSTAVA(1, IGAUSP, IELEM, 1), STRSG(1, IGAUSP, IELEM, 1), IELEM,
     4  IINCS, IGAUSP, OUTDA)
 190              CONTINUE
 200          CONTINUE
          WRITE(22, 1215)
C
C      GiD postprocess - Stresses and other state and algorithmic variables at nodes
C      =============================================================================
C
          CALL NODGID(IINCS, IELEM, IGAUSP, OUTDA)
          RETURN
      END