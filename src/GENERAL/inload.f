      SUBROUTINE INLOAD
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Hyplas global database
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C
      LOGICAL FOUND
      CHARACTER*80 INLINE
      DIMENSION
     1    SHAPE(MNODE)       ,DERIV(MDIME,MNODE) ,CARTD(MDIME,MNODE) ,
     2    GPCOD(MDIME)
      DIMENSION
     1    DGASH(MDOFN)       ,ELCOD(MDIME,MNODE) ,NOPRS(MNODE)       ,
     2    PGASH(MDOFN)       ,POINT(MDOFN)       ,PRESS(MNODE,MDOFN)
      DIMENSION
     1    IWBEG(40)          ,IWEND(40)          ,NODCHK(MNODE)      ,
     2    NODAUX(MPOIN)      ,THKN(MNODE)
      DATA R0,R1,R8,R45/0.0D0,1.0D0,8.0D0,45.0D0/
C***********************************************************************
C READS EXTERNAL LOADINGS (BODY FORCE AND SURFACE TRACTIONS) FROM INPUT
C DATA FILE AND ASSEMBLES THE GLOBAL EXTERNAL FORCE VECTOR
C
C REFERENCE: Figure 5.1
C            Section 5.3.3
C***********************************************************************
 1040 FORMAT(///
     1' Loading specification (other than prescribed displacements)'/
     1' ==========================================================='//
     2'    If any of the flags below is set to 1,  then'/
     3'    the corresponding type of loading is applied'/
     4'    to the structure.'//
     5' Point loading flag ...........................=',I3/
     6' Gravity loading flag .........................=',I3/
     7' Distributed edge loading flag ................=',I3)
 1050 FORMAT(//' Point load applied in',I6,' nodes'/
     1         ' ---------------------------------'//
     2         ' Node        X-Component    Y-Component')
 1055 FORMAT(//' Point load applied in',I6,' nodes'/
     1         ' ---------------------------------'//
     2         ' Node        R-Component    Z-Component')
 1070 FORMAT(I5,5X,3G15.6)
 1080 FORMAT(//' Gravity load'/
     1         ' ------------'//
     2         ' Gravity angle (degrees) =',G15.6/
     3         ' Gravity constant .......=',G15.6)
 1110 FORMAT(//' Edge load applied in ',I6,' edges'/
     1         ' ---------------------------------')
 1130 FORMAT(/' Element Number =',I5/' Node      X-Coord.  ',
     1'     Y-Coord.       Norm. Load         Tang. Load')
 1135 FORMAT(/' Element Number =',I5/' Node      R-Coord.  ',
     1'     Z-Coord.       Norm. Load         Tang. Load')
 1140 FORMAT(I5,2X,2G15.6,3X,2G15.6)
C
C
      IF(NTYPE.EQ.3)TWOPI=R8*ATAN(R1)
C
C Initialize load vector of all elements
C ======================================
C
      DO 10 IELEM=1,NELEM
        IGRUP=IGRPID(IELEM)
        IELIDN=IELTID(IGRUP)
        NEVAB=IELPRP(5,IELIDN)
        CALL RVZERO(RLOAD(1,IELEM),NEVAB)
   10 CONTINUE
C
C Read data controlling loading types to be inputted
C ==================================================
C
      IPLOD=0
      IGRAV=0
      IEDGE=0
C
      CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'LOADINGS',
     2    INLINE     ,15         ,NWRD       )
      IF(.NOT.FOUND)CALL ERRPRT('ED0092')
      NLOADS=NWRD-1
      IF(NLOADS.NE.0)THEN
        DO 12 I=1,NLOADS
          IF(INLINE(IWBEG(1+I):IWEND(1+I)).EQ.'POINT')THEN
            IPLOD=1
          ELSEIF(INLINE(IWBEG(1+I):IWEND(1+I)).EQ.'EDGE')THEN 
            IEDGE=1
          ELSEIF(INLINE(IWBEG(1+I):IWEND(1+I)).EQ.'GRAVITY')THEN
            IGRAV=1
          ELSEIF((INLINE(IWBEG(1+I):IWEND(1+I)).EQ.'0').OR.
     1           (INLINE(IWBEG(1+I):IWEND(1+I)).EQ.'NONE'))THEN
            CONTINUE
          ELSE
            CALL ERRPRT('ED0030')
          ENDIF
   12   CONTINUE
      ENDIF
C
      WRITE(16,1040)IPLOD,IGRAV,IEDGE
C
C Read nodal point loads
C ======================
C
      IF(IPLOD.NE.0)THEN
        CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'POINT_LOADS',
     2    INLINE     ,15         ,NWRD       )
        IF(.NOT.FOUND)CALL ERRPRT('ED0093')
        IF(NWRD.EQ.1)CALL ERRPRT('ED0031')
        NPLOAD=INTNUM(INLINE(IWBEG(2):IWEND(2)))
        IF(NTYPE.EQ.1.OR.NTYPE.EQ.2)THEN
          WRITE(16,1050)NPLOAD
        ELSEIF(NTYPE.EQ.3)THEN
          WRITE(16,1055)NPLOAD
        ENDIF
        DO 55 IPLOAD=1,NPLOAD
          READ(15,*)LODPT,(POINT(IDOFN),IDOFN=1,NDOFN)
          WRITE(16,1070)LODPT,(POINT(IDOFN),IDOFN=1,NDOFN)
          IF(LODPT.LE.0.OR.LODPT.GT.NPOIN)CALL ERRPRT('ED0134')
C
C Associate the nodal point loads with an element
C
          DO 35 IELEM=1,NELEM
            IGRUP=IGRPID(IELEM)
            IELIDN=IELTID(IGRUP)
            NNODE=IELPRP(3,IELIDN)
            DO 30 INODE=1,NNODE
              NLOCA=IABS(LNODS(IELEM,INODE))
              IF(LODPT.EQ.NLOCA)GOTO 40
   30       CONTINUE
   35     CONTINUE
   40     CONTINUE
          DO 50 IDOFN=1,NDOFN
            NGASH=(INODE-1)*NDOFN+IDOFN
            RLOAD(NGASH,IELEM)=RLOAD(NGASH,IELEM)+POINT(IDOFN)
   50     CONTINUE
   55   CONTINUE
      ENDIF
C
C Gravity loading
C ===============
C
      IF(IGRAV.NE.0)THEN
C
C Read gravity angle and gravitational constant
C
        CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'GRAVITY_LOAD',
     2    INLINE     ,15         ,NWRD       )
        IF(.NOT.FOUND)CALL ERRPRT('ED0094')
        READ(15,*)THETA,GRAVY
        WRITE(16,1080)THETA,GRAVY
        THETA=THETA*ATAN(R1)/R45
C
C Loop over elements
C
        DO 90 IELEM=1,NELEM
          IGRUP =IGRPID(IELEM)
          IELIDN=IELTID(IGRUP)
          IELTYP=IELPRP(1,IELIDN)
          NNODE =IELPRP(3,IELIDN)
          NGAUSP=IELPRP(4,IELIDN)
C Set up preliminary constants
          MATIDN=MATTID(IGRPID(IELEM))
          DENSE=RPROPS(1,MATIDN)
          IF(DENSE.EQ.R0) GOTO 90
          GXCOM=DENSE*GRAVY*SIN(THETA)
          GYCOM=-DENSE*GRAVY*COS(THETA)
C Compute coordinates of the element nodal points
          DO 65 INODE=1,NNODE
            LNODE=IABS(LNODS(IELEM,INODE))
            DO 60 IDIME=1,NDIME
              ELCOD(IDIME,INODE)=COORD(IDIME,LNODE,1)
   60       CONTINUE
   65     CONTINUE
C
C Loop for numerical integration over element domain
C
          IPPOS=1
          IPWEI=NGAUSP*NDIME+1
          DO 85 IGAUSP=1,NGAUSP
            EXISP=RELPRP(IPPOS-1+IGAUSP*2-1,IELIDN)
            ETASP=RELPRP(IPPOS-1+IGAUSP*2  ,IELIDN)
            WEIGP=RELPRP(IPWEI-1+IGAUSP    ,IELIDN)
C Compute the shape functions at the sampling points and elemental
C volume
            CALL SHPFUN
     1(   DERIV      ,ETASP      ,EXISP      ,0          ,IELTYP     ,
     2    MDIME      ,SHAPE      )
            CALL JACOB2
     1(   CARTD      ,DERIV      ,DETJAC     ,ELCOD      ,IELEM      ,
     2    MDIME      ,NDIME      ,NNODE      )
            CALL GETGCO
     1(   GPCOD      ,ELCOD      ,MDIME      ,NDIME      ,NNODE      ,
     2    SHAPE      )
C
            DVOLU=DETJAC*WEIGP
            IF(NTYPE.EQ.1)THEN
              DVOLU=DVOLU*THKGP(IGAUSP,IELEM,1)
            ELSEIF(NTYPE.EQ.3)THEN
              DVOLU=DVOLU*TWOPI*GPCOD(NAXIS)
            ENDIF
C Calculate equivalent nodal loads and add them to element force vector
            DO 70 INODE=1,NNODE
              NGASH=(INODE-1)*NDOFN+1
              MGASH=(INODE-1)*NDOFN+2
              RLOAD(NGASH,IELEM)=RLOAD(NGASH,IELEM)+
     1                           GXCOM*SHAPE(INODE)*DVOLU
              RLOAD(MGASH,IELEM)=RLOAD(MGASH,IELEM)+
     1                           GYCOM*SHAPE(INODE)*DVOLU
   70       CONTINUE
   85     CONTINUE
   90   CONTINUE
      ENDIF
C
C Distributed edge loads (pressure)
C =================================
C
      IF(IEDGE.NE.0)THEN
        CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'EDGE_LOADS',
     2    INLINE     ,15         ,NWRD       )
        IF(.NOT.FOUND)CALL ERRPRT('ED0095')
        IF(NWRD.EQ.1)CALL ERRPRT('ED0032')
        NLOADE=INTNUM(INLINE(IWBEG(2):IWEND(2)))
        WRITE(16,1110)NLOADE
C
C Loop over loaded edges
C
        DO 160 ILOADE=1,NLOADE
C Read and echo the element number and corresponding global node numbers
C with prescribed pressure
          READ(15,*)IELEM,NNODEG,(NOPRS(INODEG),INODEG=1,NNODEG)
          IF(NTYPE.NE.3)THEN
            WRITE(16,1130)IELEM
          ELSE
            WRITE(16,1135)IELEM
          ENDIF
          IF(IELEM.LE.0.OR.IELEM.GT.NELEM)CALL ERRPRT('ED0019')
          DO 80 INODEG=1,NNODEG
            IPOIN=NOPRS(INODEG)
            IF(IPOIN.LE.0.OR.IPOIN.GT.NPOIN)CALL ERRPRT('ED0020')
   80     CONTINUE 
C Set properties of the current element
          IGRUP=IGRPID(IELEM)
          IELIDN=IELTID(IGRUP)
          IELTYP=IELPRP(1,IELIDN)
          NNODE =IELPRP(3,IELIDN)
          NGAUSP=IELPRP(4,IELIDN)
          NEDGEL=IELPRP(6,IELIDN)
          MNODEG=IELPRP(7,IELIDN)
          NGAUSB=IELPRP(8,IELIDN)
          IPOS=9
C Read and echo pressures
          READ(15,*)((PRESS(INODEG,IDOFN),INODEG=1,NNODEG),
     1                                                    IDOFN=1,NDOFN)
          DO 95 INODEG=1,NNODEG
            IPOIN=NOPRS(INODEG)
            WRITE(16,1140)IPOIN,(COORD(I,IPOIN,1),I=1,NDIME),
     1                                       (PRESS(INODEG,I),I=1,NDIME)
   95     CONTINUE
          IF(NNODEG.GT.MNODEG)CALL ERRPRT('ED0011')
C Check that global node numbers supplied correspond exactly to an edge
C of the current element
          DO 96 INODE=1,NNODE
            NODCHK(INODE)=0
   96     CONTINUE
          DO 98 INODEG=1,NNODEG
            DO 97 INODE=1,NNODE
              IPOIN=IABS(LNODS(IELEM,INODE))
              IF(IPOIN.EQ.NOPRS(INODEG))NODCHK(INODE)=1
   97       CONTINUE
   98     CONTINUE
          CALL CHKNDB
     1(   FOUND    ,NNODE    ,NEDGEL   ,NODCHK   ,IELPRP(IPOS,IELIDN))
          IF(.NOT.FOUND)CALL ERRPRT('ED0012')
C
C Get the global coordinates of the nodes of the loaded edge
          DO 104 INODEG=1,NNODEG
            IPOIN=NOPRS(INODEG)
            DO 100 IDIME=1,NDIME
              ELCOD(IDIME,INODEG)=COORD(IDIME,IPOIN,1)
  100       CONTINUE
C
            DO 102 II=1,NNODEG
              IF(IABS(LNODS(IELEM,NODCHK(II))).EQ.IPOIN)NODAUX(IPOIN)=II
  102       CONTINUE
  104     CONTINUE
C Extrapolate thickness to nodes (for plane stress only)
          IF(NTYPE.EQ.1)THEN
            IPOS=NGAUSP*NDIME+NGAUSP+1
            CALL EXTNOD
     1(   RELPRP(IPOS,IELIDN),
     2    THKGP(1,IELEM,1)   ,THKN     ,1         ,NGAUSP     ,NNODE   )
          ENDIF
C
C Loop for (boundary) numerical integration over loaded edge
C
          DO 150 IGAUSB=1,NGAUSB
C Evaluate the shape functions at the boundary sampling points
            IPPOS=NGAUSP*NDIME+NGAUSP+NGAUSP*NNODE+1
            IPWEI=NGAUSP*NDIME+NGAUSP+NGAUSP*NNODE+NGAUSB+1
            EXISPB=RELPRP(IPPOS-1+IGAUSB,IELIDN)
            WEIGPB=RELPRP(IPWEI-1+IGAUSB,IELIDN)
            CALL SHPFUN
     1(   DERIV      ,DUMMY      ,EXISPB     ,1          ,IELTYP     ,
     2    MDIME      ,SHAPE      )
C Calculate components of the equivalent nodal loads
            DO 114 IDOFN=1,NDOFN
              PGASH(IDOFN)=R0
              DGASH(IDOFN)=R0
              DO 110 INODEG=1,NNODEG
                II=NODAUX(NOPRS(INODEG))
                PGASH(IDOFN)=PGASH(IDOFN)+
     1                       PRESS(INODEG,IDOFN)*SHAPE(II)
                DGASH(IDOFN)=DGASH(IDOFN)+
     1                       ELCOD(IDOFN,INODEG)*DERIV(1,II)
  110         CONTINUE
  114       CONTINUE
            PXCOMP=DGASH(1)*PGASH(2)-DGASH(2)*PGASH(1)
            PYCOMP=DGASH(1)*PGASH(1)+DGASH(2)*PGASH(2)
C
            DVOLU=WEIGPB
            IF(NTYPE.EQ.1)THEN
C interpolate to find thickness at boundary gauss point (plane stress)
              THICK=R0
              DO 115 INODEG=1,NNODEG
                II=NODAUX(NOPRS(INODEG))
                INODE=NODCHK(II)
                THICK=THICK+THKN(INODE)*SHAPE(II)
  115         CONTINUE
              DVOLU=DVOLU*THICK
            ELSEIF(NTYPE.EQ.3)THEN
C interpolate to find radius at boundary gauss point (axisymmetric case)
              RADUS=R0
              DO 117 INODEG=1,NNODEG
                II=NODAUX(NOPRS(INODEG))
                RADUS=RADUS+SHAPE(II)*ELCOD(NAXIS,INODEG)
  117         CONTINUE
              DVOLU=DVOLU*TWOPI*RADUS
            ENDIF
C
C Add the equivalent nodal loads to the element force vector
            DO 130 INODEG=1,NNODEG
              INODE=NODCHK(INODEG)
              NGASH=(INODE-1)*NDOFN+1
              MGASH=(INODE-1)*NDOFN+2
              RLOAD(NGASH,IELEM)=RLOAD(NGASH,IELEM)+
     1                           SHAPE(INODEG)*PXCOMP*DVOLU
              RLOAD(MGASH,IELEM)=RLOAD(MGASH,IELEM)+ 
     1                           SHAPE(INODEG)*PYCOMP*DVOLU
  130       CONTINUE
  150     CONTINUE
  160   CONTINUE
      ENDIF
C
      RETURN
      END
