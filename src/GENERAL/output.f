      SUBROUTINE OUTPUT
     1(   TFACT      ,IINCS      ,IITER      ,NOUTP,  OUTDA  )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Hyplas database
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C
      DIMENSION NOUTP(5)
C
      DIMENSION
     1    DERIV(MDIME,MNODE) ,ELCOD(MDIME,MNODE) ,GPCOD(MDIME)       ,
     2    PSTRS(3)           ,SHAPE(MNODE)
      DATA R0/0.0D0/
C***********************************************************************
C OUTPUTS DISPLACEMENTS, REACTIONS, STRESSES AND OTHER STATE AND
C ALGORITHMIC VARIABLES TO RESULTS FILE
C
C REFERENCE: Section 5.4.7
C***********************************************************************
 1020 FORMAT(//' Results for load increment ',I3,' Load factor =',G15.6/
     1 1X,59('=')//' Converged solution at iteration number =',2I5/)
 1030 FORMAT(//' Displacement of structure from initial configuration'/,
     1         ' ====================================================')
 1032 FORMAT('Result "DISPLACEMENT" "LOAD ANALYSIS" ',I3,
     1       ' Vector OnNodes',/,
     2       'ComponentNames "X-DISPLACEMENT", "Y-DISPLACEMENT"',/,
     3       'Values')
 1033 FORMAT('End values',/)
 1040 FORMAT(/' Node       R-Disp         Z-Disp')
 1045 FORMAT(/' Node       X-Disp         R-Disp')
 1050 FORMAT(/' Node       X-Disp         Y-Disp')
 1060 FORMAT(I5,2X,6G15.6)
 1070 FORMAT(//' Reactions'/1X,9('='))
 1080 FORMAT(/' Node      R-Force          Z-Force')
 1081 FORMAT(/' Node      X-Force          R-Force')
 1085 FORMAT(/' Node      R-Force          Z-Force          X-Local',
     1'          Y-Local')
 1086 FORMAT(/' Node      X-Force          R-Force          X-Local',
     1'          Y-Local')
 1090 FORMAT(/' Node      X-Force          Y-Force')
 1095 FORMAT(/' Node      X-Force          Y-Force          X-Local',
     1'          Y-Local')
 1100 FORMAT(I5,6(2X,G15.6))
 1105 FORMAT('       ---------------  ---------------'/' Totals',
     1G15.6,2X,G15.6)
 1110 FORMAT(//' Gauss point stresses and and other state variables'/
     1         ' ==================================================')
 1115 FORMAT(//' Element number',I5)
 1150 FORMAT(/' Gauss point ',I2,6X,' X-Coord= ',G11.4,
     1        ' Y-Coord= ',G11.4)
 1152 FORMAT(/' Gauss point ',I2,6X,' R-Coord= ',G11.4,
     1        ' Z-Coord= ',G11.4)
 1153 FORMAT(/' Gauss point ',I2,6X,' X-Coord= ',G11.4,
     1        ' R-Coord= ',G11.4)
 1160 FORMAT(' S-xx  = ',G12.4,' S-yy  = ',G12.4,' S-xy  = ',G12.4)
 1161 FORMAT(' S-xx  = ',G12.4,' S-yy  = ',G12.4,' S-xy  = ',G12.4,
     1       ' S-zz  = ',G12.4)
 1162 FORMAT(' S-rr  = ',G12.4,' S-zz  = ',G12.4,' S-rz  = ',G12.4,
     1       ' S-h   = ',G12.4)
 1163 FORMAT(' S-xx  = ',G12.4,' S-rr  = ',G12.4,' S-xr  = ',G12.4,
     1       ' S-h   = ',G12.4)
 1164 FORMAT(' S-max = ',G12.4,' S-min = ',G12.4,' Angle = ',G12.4)
 1165 FORMAT(' Thick = ',G12.4)
C
C
C Set output flags
C ================
C
      N1=NOUTP(1)
      N2=NOUTP(2)
      N3=NOUTP(3)
      N4=NOUTP(4)
      IF(NALGO.LT.0)THEN
        IF(N1.NE.0)THEN
          IF(MOD(IINCS,N1).EQ.0)THEN
            N1=1
          ELSE
            N1=0
          ENDIF
        ENDIF
        IF(N2.NE.0)THEN
          IF(MOD(IINCS,N2).EQ.0)THEN
            N2=1
          ELSE
            N2=0
          ENDIF
        ENDIF
        IF(N3.NE.0)THEN
          IF(MOD(IINCS,N3).EQ.0)THEN
            N3=1
          ELSE
            N3=0
          ENDIF
        ENDIF
        IF(N4.NE.0)THEN
          IF(MOD(IINCS,N4).EQ.0)THEN
            N4=1
          ELSE
            N4=0
          ENDIF
        ENDIF
      ENDIF
      IF(N1.EQ.0.AND.N2.EQ.0.AND.N3.EQ.0.AND.N4.EQ.0)RETURN
C
C
      WRITE(16,1020)IINCS,TFACT,IITER
C
C Output displacements
C ====================
C
      IF(N1.NE.0)THEN
        WRITE(16,1030)
        IF(NTYPE.EQ.3)THEN
          IF(NAXIS.EQ.1)THEN
            WRITE(16,1040)
          ELSE
            WRITE(16,1045)
          ENDIF
        ELSE
          WRITE(16,1050)
        ENDIF
        DO 10 IPOIN=1,NPOIN
          NGASH=IPOIN*NDOFN
          NGISH=NGASH-NDOFN+1
          WRITE(16,1060)IPOIN,(TDISP(IGASH),IGASH=NGISH,NGASH)
   10   CONTINUE
      ENDIF
C
C Output reactions
C ================
C
      IF(N2.NE.0)THEN
        WRITE(16,1070)
        DO 45 IVFIX=1,NVFIX
          IF(ANGLE(IVFIX).NE.R0)THEN
            IF(NTYPE.EQ.3)THEN
              IF(NAXIS.EQ.1)THEN
                WRITE(16,1085)
              ELSE
                WRITE(16,1086)
              ENDIF
            ELSE
              WRITE(16,1095)
            ENDIF
            GOTO 47
          ENDIF
   45   CONTINUE
        IF(NTYPE.EQ.3)THEN
          IF(NAXIS.EQ.1)THEN
            WRITE(16,1080)
          ELSE
            WRITE(16,1081)
          ENDIF
        ELSE
          WRITE(16,1090)
        ENDIF
   47   CONTINUE
        TRX=R0
        TRY=R0
        DO 70 IPOIN=1,NPOIN
          ISVAB=(IPOIN-1)*NDOFN
          DO 50 IVFIX=1,NVFIX
            IF(NOFIX(IVFIX).EQ.IPOIN)GOTO 60
   50     CONTINUE
          GOTO 70
   60     CONTINUE
          IF(ANGLE(IVFIX).NE.R0)THEN
            C=COS(ANGLE(IVFIX))
            S=SIN(ANGLE(IVFIX))
            GASHI= C*TREAC(IVFIX,1)+S*TREAC(IVFIX,2)
            GASHJ=-S*TREAC(IVFIX,1)+C*TREAC(IVFIX,2)
            IF(IFFIX(ISVAB+1).EQ.0)GASHI=R0
            IF(IFFIX(ISVAB+2).EQ.0)GASHJ=R0
            WRITE(16,1100)IPOIN,(TREAC(IVFIX,IDOFN),IDOFN=1,NDOFN),
     1                    GASHI,GASHJ
          ELSE
            WRITE(16,1100)IPOIN,(TREAC(IVFIX,IDOFN),IDOFN=1,NDOFN)
          ENDIF
          TRX=TRX+TREAC(IVFIX,1)
          TRY=TRY+TREAC(IVFIX,2)
   70   CONTINUE
        WRITE(16,1105)TRX,TRY
      ENDIF
C
C Stresses and other state and algorithmic variables at gauss points
C ==================================================================
C
      IF(N3.NE.0)THEN
        WRITE(16,1110)
        DO 120 IELEM=1,NELEM
          IGRUP=IGRPID(IELEM)
          IELIDN=IELTID(IGRUP)
          IELTYP=IELPRP(1,IELIDN)
          NNODE =IELPRP(3,IELIDN)
          NGAUSP=IELPRP(4,IELIDN)
C
          WRITE(16,1115)IELEM
C Evaluate Gauss point coordinates
          DO 91 INODE=1,NNODE
            LNODE=IABS(LNODS(IELEM,INODE))
            DO 90 IDIME=1,NDIME
              ELCOD(IDIME,INODE)=COORD(IDIME,LNODE,1)
   90       CONTINUE
   91     CONTINUE
          IF(NTYPE.EQ.1)THEN
            LSTRE=3
          ELSEIF(NTYPE.EQ.2)THEN
            LSTRE=4
          ELSEIF(NTYPE.EQ.3)THEN
            LSTRE=4
          ENDIF
          IPPOS=1
          DO 110 IGAUSP=1,NGAUSP
            EXISP=RELPRP(IPPOS-1+IGAUSP*2-1,IELIDN)
            ETASP=RELPRP(IPPOS-1+IGAUSP*2  ,IELIDN)
            CALL SHPFUN
     1(   DERIV      ,ETASP      ,EXISP      ,0          ,IELTYP     ,
     2    MDIME      ,SHAPE      )
            CALL GETGCO
     1(   GPCOD      ,ELCOD      ,MDIME      ,NDIME      ,NNODE      ,
     2    SHAPE      )
C Output gauss points stresses (common to all materials)
C ------------------------------------------------------
            IF(NTYPE.EQ.1)THEN
              WRITE(16,1150)IGAUSP,(GPCOD(I),I=1,NDIME)
              WRITE(16,1160)(STRSG(I,IGAUSP,IELEM,1),I=1,LSTRE)
            ELSEIF(NTYPE.EQ.2)THEN
              WRITE(16,1150)IGAUSP,(GPCOD(I),I=1,NDIME)
              WRITE(16,1161)(STRSG(I,IGAUSP,IELEM,1),I=1,LSTRE)
            ELSEIF(NTYPE.EQ.3)THEN
              IF(NAXIS.EQ.1)THEN
                WRITE(16,1152)IGAUSP,(GPCOD(I),I=1,NDIME)
                WRITE(16,1162)(STRSG(I,IGAUSP,IELEM,1),I=1,LSTRE)
              ELSE
                WRITE(16,1153)IGAUSP,(GPCOD(I),I=1,NDIME)
                WRITE(16,1163)(STRSG(I,IGAUSP,IELEM,1),I=1,LSTRE)
              ENDIF
            ENDIF
C and principal stresses
            CALL PRINC2(PSTRS,STRSG(1,IGAUSP,IELEM,1))
            WRITE(16,1164)(PSTRS(I),I=1,3)
C output current thickness (for large strains in plane stress only)
            IF(NLARGE.EQ.1.AND.NTYPE.EQ.1)THEN
              WRITE(16,1165)THKGP(IGAUSP,IELEM,1)
            ENDIF
C Output other (material-specific) state and algorithmic variables
C ----------------------------------------------------------------
            CALL MATIOR
     1(   NTYPE                    ,IPROPS(1,MATTID(IGRPID(IELEM)))  ,
     2    RALGVA(1,IGAUSP,IELEM,1) ,RPROPS(1,MATTID(IGRPID(IELEM)))  ,
     3    RSTAVA(1,IGAUSP,IELEM,1) ,STRSG(1,IGAUSP,IELEM,1)   ,
     4    IELEM,   IINCS,   IGAUSP,   OUTDA   )
C
  110     CONTINUE
  120   CONTINUE
      ENDIF
C
C Stresses and other state and algorithmic variables at nodes
C ===========================================================
C
      IF(N4.NE.0)THEN
        CALL NODAVE(IINCS  ,IELEM  ,IGAUSP, OUTDA)
      ENDIF
C
      RETURN
      END
