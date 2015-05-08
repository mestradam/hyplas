      SUBROUTINE INDATA(MXFRON ,UNSYM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Hyplas database: Global parameters and common blocks
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
      include '../LOCALIZA.INC'
C Local array and variables
      LOGICAL CARTES ,CYLIND ,FOUND ,UNSYM ,UNSAUX
      CHARACTER*80 ELTNAM ,MATNAM
      CHARACTER*72 TITLE
      CHARACTER*80 SUBKEY ,INLINE
      DIMENSION
     1    AUXCOR(MDIME)      ,DERIV(MDIME,MNODE) ,ELTHK(MNODE)       ,
     2    IELCHK(MELEM)      ,IETCHK(MGRUP)      ,IGRCHK(MGRUP)      ,
     3    INDCHK(MPOIN)      ,IWBEG(40)          ,IWEND(40)          ,
     4    KSLAV(48)          ,MATCHK(MGRUP)      ,SHAPE(MNODE)       ,
     5    THKNOD(MPOIN)
C Numerical constants
      PARAMETER
     1(   R0=0.0D0   ,R1=1.0D0   ,R45=45.0D0 )
C***********************************************************************
C READS MOST OF THE INPUT DATA
C
C REFERENCE: Section 5.3.2
C***********************************************************************
 1000 FORMAT(/' Title:'/' ======'/)
 1010 FORMAT(A)
 1015 FORMAT(1X,A)
 1020 FORMAT(A80)
 1030 FORMAT(//' Analysis description:'/' ====================='/)
 1040 FORMAT(
     1 ' Analysis type ....................................... =',I5/
     2 '        1 = Plane stress'/
     3 '        2 = Plane strain'/
     4 '        3 = Axisymmetric')
 1050 FORMAT(/
     1 ' Large deformation flag .............................. =',A5)
 1060 FORMAT(/
     1 ' Nonlinear solution algorithm ........................ =',I5/
     2 '        Negative for the arc length method'/
     3 '        1 = Initial stiffness method'/
     4 '        2 = Newton-Raphson tangential stiffness method'/
     5 '        3 = Modified Newton KT1'/
     6 '        4 = Modified Newton KT2'/
     7 '        5 = Secant Newton - Initial stiffness'/
     8 '        6 = Secant Newton - KT1'/
     9 '        7 = Secant Newton - KT2')
 1065 FORMAT(/
     1 ' Arc-length option ................................... =',I5/
     2 '        1 = Follow stiffness determinant sign'/
     3 '        2 = Follow current path')
 1070 FORMAT(//
     1 ' Element connectivities:      Number of elements = ',I5,/
     3 ' ======================='//
     4 ' Elem.  Group             Node numbers'/)
 1080 FORMAT(I4,I8,10X,9I5)
 1082 FORMAT('Elements')
 1083 FORMAT(I4,10X,9I5,I8)
 1084 FORMAT('End elements',/)
 1090 FORMAT(//
     1 ' Nodal point co-ordinates:       Number of nodes = ',I5,/
     2 ' ========================='//
     3 ' Node    X-Coord        Y-Coord'/)
 1100 FORMAT(//
     1 ' Nodal point co-ordinates:       Number of nodes = ',I5,/
     2 ' ========================='//
     3 ' Node    R-Coord        Z-Coord'/)
 1110 FORMAT(I5,3G15.6)
 1112 FORMAT('Coordinates')
 1114 FORMAT('End coordinates',/)
 1120 FORMAT(//
     1 ' Prescribed displacements:    Number of nodes with',
     2 ' prescribed displacement = ',I5/
     3 ' ========================='//
     4 ' Node   Code             Prescribed values           ',
     5 '      Angle'/)
 1130 FORMAT(1X,I4,1X,I6,3X,7G15.6)
 1140 FORMAT(//
     1 ' Element Groups:        Number of element groups = ',I5/
     2 ' ==============='//
     3 ' Group     Element type    Material type'/)
 1150 FORMAT(1X,I5,6X,I5,13X,I5)
 1160 FORMAT(//
     1 ' Element types:          Number of element types = ',I5/
     2 ' ==============')
 1170 FORMAT(/' Element type number  ',I2/,' -------------------')
 1171 FORMAT('MESH dimension 2 ElemType Triangle Nnode 3')
 1172 FORMAT('MESH dimension 2 ElemType Quadrilateral Nnode 4')
 1173 FORMAT('MESH dimension 2 ElemType Quadrilateral Nnode 8')
 1175 FORMAT('GiD Post Results File 1.0',/)
 1176 FORMAT('GaussPoints "Board gauss internal" ',
     1        'ElemType Triangle',/,'Number Of Gauss Points: 1',
     2        /,'Natural Coordinates: Internal',/,'End gausspoints',/)
 1177 FORMAT('GaussPoints "Board gauss internal" ',
     1        'ElemType Quadrilateral',/,'Number Of Gauss Points: 4',
     2        /,'Natural Coordinates: Internal',/,'End gausspoints',/)
 1180 FORMAT(//
     1 ' Material properties:        Number of materials = ',I5/
     2 ' ====================')
 1190 FORMAT(/' Material type number ',I2/,' --------------------')
 1200 FORMAT(
     1 ' Axis of symmetry .................................... =    ',A)
 1210 FORMAT(///' No Master/Slave nodal constraints specified'/
     1          ' ==========================================='/)
 1220 FORMAT(//
     1 ' Master/slave constraint sets:      Number of sets = ',I5/
     2 ' =============================')
 1230 FORMAT(/,' Master node =',I4,' Number of slave nodes =',I3,
     1 ' Fixity condition =',I3/)
 1240 FORMAT(' Slaves =',12I5:/(9X,12I5))
 1250 FORMAT(//
     1 ' Thickness distribution (initial thickness for large strains)'/
     2 ' ============================================================'/)
 1260 FORMAT(' Uniform thickness = ',G15.6/)
 1270 FORMAT(' Elem.   thickness'/)
 1280 FORMAT(' Node    thickness'/)
 1290 FORMAT(I5,1X,G15.6)
C
C Read basic analysis information
C ===============================
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'TITLE',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0080')
      READ(15,1010)TITLE
      WRITE(16,1000)
      WRITE(16,1015)TITLE
C
      WRITE(16,1030)
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'ANALYSIS_TYPE',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0081')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0018')
      NTYPE=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(16,1040)NTYPE
      IF(NTYPE.EQ.1.OR.NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
        NDOFN=2
        NDIME=2
      ELSE
        CALL ERRPRT('ED0009')
      ENDIF
      IF(NTYPE.EQ.3)THEN
        CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'AXIS_OF_SYMMETRY',
     2    INLINE    ,15       ,NWRD     )
        IF(.NOT.FOUND)CALL ERRPRT('ED0082')
        IF(NWRD.EQ.1)CALL ERRPRT('ED0016')
        WRITE(16,1200)INLINE(IWBEG(2):IWEND(2))
        IF(INLINE(IWBEG(2):IWEND(2)).EQ.'Y')THEN
          NAXIS=1
        ELSEIF(INLINE(IWBEG(2):IWEND(2)).EQ.'X')THEN
          NAXIS=2
        ELSE
          CALL ERRPRT('ED0017')
        ENDIF
      ENDIF
C
      CALL FNDKEY
     1(   FOUND   ,IWBEG  ,IWEND  ,'LARGE_STRAIN_FORMULATION',
     2    INLINE  ,15     ,NWRD   )
      IF(.NOT.FOUND)CALL ERRPRT('ED0083')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0022')
      WRITE(16,1050)INLINE(IWBEG(2):IWEND(2))
      IF(INLINE(IWBEG(2):IWEND(2)).EQ.'ON')THEN
        NLARGE=1
      ELSEIF(INLINE(IWBEG(2):IWEND(2)).EQ.'OFF')THEN
        NLARGE=0
      ELSE
        CALL ERRPRT('ED0014')
      ENDIF
C
C Read non-linear equilibrium solution algorithm information
C ==========================================================
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'SOLUTION_ALGORITHM',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0084')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0021')
      NALGO=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(16,1060)NALGO
      IF(IABS(NALGO).NE.1.AND.IABS(NALGO).NE.2.AND.IABS(NALGO).NE.3.AND.
     1   IABS(NALGO).NE.4.AND.IABS(NALGO).NE.5.AND.IABS(NALGO).NE.6.AND.
     2   IABS(NALGO).NE.7)CALL ERRPRT('ED0010')
      NARCL=0
      IF(NALGO.LT.0)THEN
        CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'ARC_LENGTH_PREDICTOR_OPTION',
     2    INLINE    ,15       ,NWRD     )
        IF(NWRD.EQ.1)CALL ERRPRT('ED0140')
        IF(FOUND)THEN
          IF(INLINE(IWBEG(2):IWEND(2)).EQ.'STIFFNESS_SIGN')THEN
            NARCL=1
          ELSEIF(INLINE(IWBEG(2):IWEND(2)).EQ.'SECANT_PATH')THEN
            NARCL=2
          ELSE
            CALL ERRPRT('ED0141')
          ENDIF
          WRITE(16,1065)NARCL
        ELSE
          CALL ERRPRT('ED0139')
        ENDIF
      ENDIF
C
C Read all information concerning element groups
C ==============================================
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'ELEMENT_GROUPS',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0085')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0052')
      NGRUP=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(16,1140)NGRUP
      IF(NGRUP.LT.1)CALL ERRPRT('ED0053')
      IF(NGRUP.GT.MGRUP)CALL ERRPRT('ED0054')
C
C assign element & material type identification numbers to each group
C
      DO 101 IGRUP=1,NGRUP
        IGRCHK(IGRUP)=0
  101 CONTINUE
      DO 102 IGRUP=1,NGRUP
        READ(15,*)IGRP,IELIDN,MATIDN
        WRITE(16,1150)IGRP,IELIDN,MATIDN
        IF(IGRP.GT.NGRUP.OR.IELIDN.GT.NGRUP.OR.MATIDN.GT.NGRUP.OR.
     1     IGRP.LT.1.OR.IELIDN.LT.1.OR.MATIDN.LT.1)CALL ERRPRT('ED0062')
        IF(IGRCHK(IGRP).EQ.1)CALL ERRPRT('ED0063')
        IGRCHK(IGRP)=1
        IELTID(IGRP)=IELIDN
        MATTID(IGRP)=MATIDN
  102 CONTINUE
C
C Read type of element associated with each element type identification
C number and call the appropriate routines to read the element
C properties and set vectors IELPRP and RELPRP of integer and real
C element properties
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'ELEMENT_TYPES',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0086')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0055')
      NELTS=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(16,1160)NELTS
      IF(NELTS.LT.1)CALL ERRPRT('ED0056')
      IF(NELTS.GT.NGRUP)CALL ERRPRT('ED0057')
      DO 84 IELTS=1,NGRUP
        IETCHK(IELTS)=0
   84 CONTINUE
      UNSYM=.FALSE.
      DO 82 IELTS=1,NELTS
        READ(15,1020)SUBKEY
        NSKWRD=NWORD(SUBKEY,IWBEG,IWEND)
        IF(NSKWRD.EQ.0)CALL ERRPRT('ED0058')
        IF(NSKWRD.EQ.1)CALL ERRPRT('ED0059')
        IELIDN=INTNUM(SUBKEY(IWBEG(1):IWEND(1)))
        ELTNAM=SUBKEY(IWBEG(2):IWEND(2))
        WRITE(16,1170)IELIDN
        IF(IELIDN.LE.0.OR.IELIDN.GT.NELTS)CALL ERRPRT('ED0060')
        IF(IETCHK(IELIDN).EQ.1)CALL ERRPRT('ED0061')
        IETCHK(IELIDN)=1
C Set type, class and and read and set other properties
        IF(ELTNAM.EQ.'TRI_3')THEN
          IELTYP=TRI3
          IELCLS=STDARD
          CALL RST3
     1(   IELPRP(1,IELIDN)  ,16  ,RELPRP(1,IELIDN)  ,UNSAUX)
	  WRITE(21,1171)
          WRITE(22,1175)
	  WRITE(22,1176)
        ELSEIF(ELTNAM.EQ.'QUAD_4')THEN
          IELTYP=QUAD4
          IELCLS=STDARD
          CALL RSQ4
     1(   IELPRP(1,IELIDN)  ,15  ,16  ,RELPRP(1,IELIDN)  ,UNSAUX)
	  WRITE(21,1172)
	  WRITE(22,1175)
	  WRITE(22,1177)
        ELSEIF(ELTNAM.EQ.'QUAD_8')THEN
          IELTYP=QUAD8
          IELCLS=STDARD
          CALL RSQ8
     1(   IELPRP(1,IELIDN)  ,15  ,16  ,RELPRP(1,IELIDN)  ,UNSAUX)
	  WRITE(21,1173)
	  WRITE(22,1175)
	  WRITE(22,1177)
        ELSEIF(ELTNAM.EQ.'QUAD_4_FBAR')THEN
          IELTYP=QUA4FB
          IELCLS=FBAR
          IF(NLARGE.NE.1)THEN
            CALL ERRPRT('ED0180')
          ENDIF
          CALL RSQ4FB
     1(   IELPRP(1,IELIDN)  ,15  ,16  ,NTYPE ,RELPRP(1,IELIDN)  ,
     2    UNSAUX  )
        elseif (eltnam.eq.'Q_4_WSD' .or. eltnam.eq.'Q_4WSD' .or.
     &          eltnam.eq.'Q4_WSD'  .or. eltnam.eq.'Q4WSD'  .or.
     &          eltnam.eq.'q_4_wsd' .or. eltnam.eq.'q_4wsd' .or.
     &          eltnam.eq.'q4_wsd'  .or. eltnam.eq.'q4wsd'  .or.
     &          eltnam.eq.'QUA_4_WSD' .or. eltnam.eq.'QUA_4WSD' .or.
     &          eltnam.eq.'QUA4_WSD'  .or. eltnam.eq.'QUA4WSD'  .or.
     &          eltnam.eq.'qua_4_wsd' .or. eltnam.eq.'qua_4wsd' .or.
     &          eltnam.eq.'qua4_wsd'  .or. eltnam.eq.'qua4wsd'  ) then
C Quadrilateral elements with strong discontinuities
C M. Estrada 2014
          ieltyp = q4wsd
          ielcls = wsdisc
          call rsq4wsd
     &( ielprp(1,ielidn)    ,15   ,16   ,relprp(1,ielidn)   ,unsaux )
	  write(21,1172)
	  write(22,1175)
	  write(22,1177)
        else
          call errprt('ED0064')
        endif
        ielprp(1,ielidn) = ieltyp
        ielprp(2,ielidn) = ielcls
        if(unsaux)unsym=.true.
   82 CONTINUE
C Check that the properties associated with all element type
C identification numbers have been read
      DO 83 IGRUP=1,NGRUP
        IELIDN=IELTID(IGRUP)
        IF(IETCHK(IELIDN).NE.1)CALL ERRPRT('ED0065')
   83 CONTINUE
C
C Read type of material associated with each material type
C identification number and call the appropriate routines to read the
C material properties and set vectors IPROPS and RPROPS of integer and
C real material properties
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'MATERIALS',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0087')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0027')
      NMATS=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(16,1180)NMATS
c      IF(NMATS.LE.0.OR.NMATS.GT.NGRUP)CALL ERRPRT('ED0007')
      if(nmats.le.0) call errprt('ED0007')
      DO 99 IMATS=1,NMATS
        MATCHK(IMATS)=0
   99 CONTINUE
      DO 100 IMATS=1,NMATS
        READ(15,1020)SUBKEY
        NSKWRD=NWORD(SUBKEY,IWBEG,IWEND)
        IF(NSKWRD.EQ.0)CALL ERRPRT('ED0028')
        IF(NSKWRD.EQ.1)CALL ERRPRT('ED0029')
        MATIDN=INTNUM(SUBKEY(IWBEG(1):IWEND(1)))
        MATNAM=SUBKEY(IWBEG(2):IWEND(2))
        WRITE(16,1190)MATIDN
        IF(MATIDN.LE.0.OR.MATIDN.GT.NMATS)CALL ERRPRT('ED0044')
        IF(MATCHK(MATIDN).EQ.1)CALL ERRPRT('ED0045')
        MATCHK(MATIDN)=1
C
C Call material interface for reading material-specific data
C
        CALL MATIRD
     1(   MATNAM     ,NLARGE     ,NTYPE      ,UNSAUX     ,
     2    IPROPS(1,MATIDN)       ,RPROPS(1,MATIDN)       )
        IF(UNSAUX)UNSYM=.TRUE.
C
  100 CONTINUE
      DO 103 IGRUP=1,NGRUP
        MATIDN=MATTID(IGRUP)
        IF(MATCHK(MATIDN).NE.1)CALL ERRPRT('ED0066')
  103 CONTINUE
C
C Read elements nodal connectivities and group
C ============================================
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'ELEMENTS',
     2    INLINE    ,15       ,NWRD     )
      IF(.NOT.FOUND)CALL ERRPRT('ED0088')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0023')
      NELEM=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(16,1070)NELEM
      IF(NELEM.LE.0)    CALL ERRPRT('ED0001')
      IF(NELEM.GT.MELEM)CALL ERRPRT('ED0002')
      CALL IVZERO(IELCHK,NELEM)
      MPOSPO=0
      DO 10 IELEM=1,NELEM
C Read element connectivity list
        READ(15,*)NUMEL,IGRPID(NUMEL),(LNODS(NUMEL,INODE),
     1            INODE=1,IELPRP(3,IELTID(IGRPID(NUMEL))))
        NNODE=IELPRP(3,IELTID(IGRPID(NUMEL)))
        WRITE(16,1080)NUMEL,IGRPID(NUMEL),(LNODS(NUMEL,INODE),
     1                INODE=1,NNODE)
        MPOSPO=MPOSPO+NNODE
C Check for repeated element connectivity specification
        IF(IELCHK(NUMEL).EQ.1)CALL ERRPRT('ED0047')
        IELCHK(NUMEL)=1
C Check for invalid element and group numbers
        IF(NUMEL.LE.0.OR.NUMEL.GT.NELEM)CALL ERRPRT('ED0046')
        IF(IGRPID(NUMEL).EQ.0.OR.IGRPID(NUMEL).GT.NGRUP)
     1  CALL ERRPRT('ED0037')
   10 CONTINUE
C
C Read nodal coordinates and thickness
C ====================================
C
      CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'NODE_COORDINATES',
     2    INLINE     ,15         ,NWRD       )
      IF(.NOT.FOUND)CALL ERRPRT('ED0089')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0024')
      IF(NDIME.EQ.2.AND.NWRD.LT.3)CALL ERRPRT('ED0144')
      CYLIND=.FALSE.
      CARTES=.TRUE.
      IF(NDIME.EQ.2)THEN
C accepts data either in polar or cartesian system
        IF(INLINE(IWBEG(3):IWEND(3)).EQ.'CYLINDRICAL')THEN
          CYLIND=.TRUE.
          CARTES=.FALSE.
        ELSEIF(INLINE(IWBEG(3):IWEND(3)).EQ.'CARTESIAN')THEN
          CYLIND=.FALSE.
          CARTES=.TRUE.
        ELSE
          CALL ERRPRT('ED0145')
        ENDIF
      ENDIF
      NPOIN=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      IF(NTYPE.EQ.1.OR.NTYPE.EQ.2)THEN
        WRITE(16,1090)NPOIN
      ELSEIF(NTYPE.EQ.3)THEN
        WRITE(16,1100)NPOIN
      ENDIF
	WRITE(21,1112)
      IF(NPOIN.LE.0)     CALL ERRPRT('ED0003')
      IF(NPOIN.GT.MPOIN) CALL ERRPRT('ED0004')
      IF(NPOIN.GT.MPOSPO)CALL ERRPRT('ED0049')
C
C Set global variable NTOTV (total number of degrees of freedom)
C
      NTOTV=NPOIN*NDOFN
C
C Read coordinates
      CALL IVZERO(INDCHK,NPOIN)
      DO 20 ICOUNT=1,NPOIN
        IF(CYLIND)THEN
C...in polar system
          READ(15,*)IPOIN,(AUXCOR(IDIME),IDIME=1,NDIME),RAD,THET
        ELSEIF(CARTES)THEN
C...in cartesian system
          READ(15,*)IPOIN,(AUXCOR(IDIME),IDIME=1,NDIME)
        ENDIF
        IF(IPOIN.GT.NPOIN.OR.IPOIN.LE.0)CALL ERRPRT('ED0127')
        DO 15 IDIME=1,NDIME
          COORD(IDIME,IPOIN,1)=AUXCOR(IDIME)
   15   CONTINUE
        IF(INDCHK(IPOIN).NE.0)CALL ERRPRT('ED0126')
        INDCHK(IPOIN)=1
        IF(CYLIND)THEN
          IF(RAD.NE.R0)THEN
C Input data in polar coordinates - transform into cartesian coordinates
            THET=THET*ATAN(R1)/R45
            COORD(1,IPOIN,1)=COORD(1,IPOIN,1)+RAD*COS(THET)
            COORD(2,IPOIN,1)=COORD(2,IPOIN,1)+RAD*SIN(THET)
          ENDIF
        ENDIF
C Echo coordinates
        WRITE(16,1110)IPOIN,(COORD(IDIME,IPOIN,1),IDIME=1,NDIME)
C Write element connectivity list in .gid.msh
	  WRITE(21,1110)IPOIN,(COORD(IDIME,IPOIN,1),IDIME=1,NDIME)
   20 CONTINUE
      WRITE(21,1114)
      DO 30 IPOIN=1,NPOIN
C check that the coordinates of all nodes have been defined
        IF(INDCHK(IPOIN).NE.1)CALL ERRPRT('ED0125')
C initialise initial and last converged coordinates sub-arrays
        DO 25 IDIME=1,NDIME
          COORD(IDIME,IPOIN,0)=COORD(IDIME,IPOIN,1)
          COORD(IDIME,IPOIN,2)=COORD(IDIME,IPOIN,1)
   25   CONTINUE
   30 CONTINUE
C
C Read elements nodal connectivities for GiD postprocess
C ======================================================
C
      CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'ELEMENTS',
     2    INLINE    ,15       ,NWRD     )
      NELEM=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      WRITE(21,1082)
      CALL IVZERO(IELCHK,NELEM)
      DO 32 IELEM=1,NELEM
C Read element connectivity list
        READ(15,*)NUMEL,IGRPID(NUMEL),(LNODS(NUMEL,INODE),
     1            INODE=1,IELPRP(3,IELTID(IGRPID(NUMEL))))
        NNODE=IELPRP(3,IELTID(IGRPID(NUMEL)))
C Write element connectivity list in .gid.msh
        IF(ELTNAM.EQ.'QUAD_8')THEN
          WRITE(21,1083)NUMEL,LNODS(NUMEL,1),LNODS(NUMEL,3),
     1                LNODS(NUMEL,5),LNODS(NUMEL,7),LNODS(NUMEL,2),
     2                LNODS(NUMEL,4),LNODS(NUMEL,6),LNODS(NUMEL,8),
     3                IGRPID(NUMEL)
        ELSE
          WRITE(21,1083)NUMEL,(LNODS(NUMEL,INODE),
     1                INODE=1,NNODE),IGRPID(NUMEL)
        ENDIF
   32 CONTINUE
      WRITE(21,1084)
C
C Read initial thickness (for plane stress only)
C ==============================================
C
      IF(NTYPE.EQ.1)THEN
        CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'THICKNESS',
     2    INLINE     ,15         ,NWRD       )
        IF(.NOT.FOUND)CALL ERRPRT('ED0090')
        IF(NWRD.EQ.1)CALL ERRPRT('ED0072')
        WRITE(16,1250)
C
C Uniform initial thickness in the whole mesh
C -------------------------------------------
        IF(INLINE(IWBEG(2):IWEND(2)).EQ.'UNIFORM')THEN
          READ(15,*)THICK
          WRITE(16,1260)THICK
          DO 35 IELEM=1,NELEM
            IGRUP=IGRPID(IELEM)
            IELIDN=IELTID(IGRUP)
            NGAUSP=IELPRP(4,IELIDN)
            DO 34 IGAUSP=1,NGAUSP
              THKGP(IGAUSP,IELEM,0)=THICK
              THKGP(IGAUSP,IELEM,1)=THICK
   34       CONTINUE
   35     CONTINUE
C
C Non-uniform initial thickness (constant within each element)
C ------------------------------------------------------------
        ELSEIF(INLINE(IWBEG(2):IWEND(2)).EQ.'DEFINED_BY_ELEMENT')THEN
          WRITE(16,1270)
          CALL IVZERO(IELCHK,NELEM)
C Read element thicknesses and set corresponding gauss point thicknesses
          IF(NWRD.EQ.2)THEN
            DO 41 I=1,NELEM
              READ(15,*)IELEM,THICK
              WRITE(16,1290)IELEM,THICK
              IF(IELEM.LE.0.OR.IELEM.GT.NELEM)CALL ERRPRT('ED0068')
              IF(IELCHK(IELEM).EQ.1)CALL ERRPRT('ED0069')
              IELCHK(IELEM)=1
              IGRUP=IGRPID(IELEM)
              IELIDN=IELTID(IGRUP)
              NGAUSP=IELPRP(4,IELIDN)
              DO 40 IGAUSP=1,NGAUSP
                THKGP(IGAUSP,IELEM,0)=THICK
                THKGP(IGAUSP,IELEM,1)=THICK
   40         CONTINUE
   41       CONTINUE
          ELSE
            NSET=INTNUM(INLINE(IWBEG(3):IWEND(3)))
            DO 45 ISET=1,NSET
              READ(15,*)IFIRST,ILAST,INC,THICK
              DO 44 IELEM=IFIRST,ILAST,INC
                WRITE(16,1290)IELEM,THICK
                IF(IELEM.LE.0.OR.IELEM.GT.NELEM)CALL ERRPRT('ED0068')
                IF(IELCHK(IELEM).EQ.1)CALL ERRPRT('ED0069')
                IELCHK(IELEM)=1
                IGRUP=IGRPID(IELEM)
                IELIDN=IELTID(IGRUP)
                NGAUSP=IELPRP(4,IELIDN)
                DO 43 IGAUSP=1,NGAUSP
                  THKGP(IGAUSP,IELEM,0)=THICK
                  THKGP(IGAUSP,IELEM,1)=THICK
   43           CONTINUE
   44         CONTINUE
   45       CONTINUE
          ENDIF
C Check that the thickness has been defined for all elements
          DO 46 IELEM=1,NELEM
            IF(IELCHK(IELEM).NE.1)CALL ERRPRT('ED0075')
   46     CONTINUE
C
C Non-uniform initial thickness (continuous across elements)
C ----------------------------------------------------------
        ELSEIF(INLINE(IWBEG(2):IWEND(2)).EQ.'DEFINED_BY_NODE')THEN
          WRITE(16,1280)
          CALL IVZERO(INDCHK,NPOIN)
C Read nodal thicknesses
          IF(NWRD.EQ.2)THEN
            DO 50 I=1,NPOIN
              READ(15,*)IPOIN,THICK
              WRITE(16,1290)IPOIN,THICK
              IF(IPOIN.LE.0.OR.IPOIN.GT.NPOIN)CALL ERRPRT('ED0070')
              IF(INDCHK(IPOIN).EQ.1)CALL ERRPRT('ED0071')
              INDCHK(IPOIN)=1
              THKNOD(IPOIN)=THICK
   50       CONTINUE
          ELSE
            NSET=INTNUM(INLINE(IWBEG(3):IWEND(3)))
            DO 55 ISET=1,NSET
              READ(15,*)IFIRST,ILAST,INC,THICK
              DO 54 IPOIN=IFIRST,ILAST,INC
                WRITE(16,1290)IPOIN,THICK
                IF(IPOIN.LE.0.OR.IPOIN.GT.NPOIN)CALL ERRPRT('ED0070')
                IF(INDCHK(IPOIN).EQ.1)CALL ERRPRT('ED0071')
                INDCHK(IPOIN)=1
                THKNOD(IPOIN)=THICK
   54         CONTINUE
   55       CONTINUE
          ENDIF
C Check that the thickness has been defined for all nodes
          DO 56 IPOIN=1,NPOIN
            IF(INDCHK(IPOIN).NE.1)CALL ERRPRT('ED0076')
   56     CONTINUE
          DO 60 IELEM=1,NELEM
C Set element nodal thicknesses array
            IGRUP=IGRPID(IELEM)
            IELIDN=IELTID(IGRUP)
            IELTYP=IELPRP(1,IELIDN)
            NNODE =IELPRP(3,IELIDN)
            NGAUSP=IELPRP(4,IELIDN)
            DO 58 INODE=1,NNODE
              LNODE=IABS(LNODS(IELEM,INODE))
              ELTHK(INODE)=THKNOD(LNODE)
   58       CONTINUE
C Interpolate nodal thicknesses to gauss points
            IPPOS=1
            DO 59 IGAUSP=1,NGAUSP
              EXISP=RELPRP(IPPOS-1+IGAUSP*2-1,IELIDN)
              ETASP=RELPRP(IPPOS-1+IGAUSP*2  ,IELIDN)
              CALL SHPFUN
     1(   DERIV      ,ETASP      ,EXISP      ,0          ,IELTYP     ,
     2    MDIME      ,SHAPE      )
              THKGP(IGAUSP,IELEM,0)=SCAPRD(ELTHK,SHAPE,NNODE)
              THKGP(IGAUSP,IELEM,1)=SCAPRD(ELTHK,SHAPE,NNODE)
   59       CONTINUE
   60     CONTINUE
        ELSE
          CALL ERRPRT('ED0073')
        ENDIF
      ENDIF
C
C
C Read prescribed displacements
C =============================
C
      CALL FNDKEY
     1(   FOUND  ,IWBEG  ,IWEND  ,'NODES_WITH_PRESCRIBED_DISPLACEMENTS',
     2    INLINE ,15     ,NWRD   )
      IF(.NOT.FOUND)CALL ERRPRT('ED0091')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0025')
      NVFIX=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      IF(NDIME.EQ.2)THEN
        WRITE(16,1120)NVFIX
      ENDIF
      IF(NVFIX.LT.1)    CALL ERRPRT('ED0005')
      IF(NVFIX.GT.MVFIX)CALL ERRPRT('ED0006')
      IF(NVFIX.GT.NPOIN)CALL ERRPRT('ED0048')
      DO 70 ITOTV=1,NTOTV
        IFFIX(ITOTV)=0
        DO 65 IRHS=1,2
          FIXED(ITOTV,IRHS)=R0
   65   CONTINUE
   70 CONTINUE
      DO 91 IVFIX=1,NVFIX
        IF(NDIME.EQ.2)THEN
          READ(15,*)NOFIX(IVFIX),IFPRE,(PRESC(IVFIX,IDOFN),
     1              IDOFN=1,NDOFN),THETA
          WRITE(16,1130)NOFIX(IVFIX),IFPRE,(PRESC(IVFIX,IDOFN),
     1                  IDOFN=1,NDOFN),THETA
          ANGLE(IVFIX)=THETA*ATAN(R1)/R45
        ENDIF
        NLOCA=(NOFIX(IVFIX)-1)*NDOFN
        IFDOF=10**(NDOFN-1)
        DO 90 IDOFN=1,NDOFN
          NGASH=NLOCA+IDOFN
          IF(IFPRE.LT.IFDOF)GOTO 80
          IFFIX(NGASH)=1
          FIXED(NGASH,1)=PRESC(IVFIX,IDOFN)
          FIXED(NGASH,2)=R0
          IFPRE=IFPRE-IFDOF
   80     CONTINUE
          IFDOF=IFDOF/10
   90   CONTINUE
   91 CONTINUE
C Initialize master array
      DO 93 ITOTV=1,NTOTV
        MASTER(ITOTV)=ITOTV
   93 CONTINUE
C
C Master/slave nodal constraints
C ==============================
C
      CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'MASTER_SLAVE_SETS',
     2    INLINE     ,15         ,NWRD       )
      IF(.NOT.FOUND)THEN
        NMAST=0
      ELSE
        IF(NWRD.EQ.1)CALL ERRPRT('ED0026')
        NMAST=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      ENDIF
C Master/slave nodes specified
      IF(NMAST.NE.0)THEN
        WRITE(16,1220)NMAST
        IF(NMAST.LT.0)CALL ERRPRT('ED0036')
C Loop over sets of slave nodes
        DO 97 IMAST=1,NMAST
          READ(15,*)NODEM,ISLAV,ISFIX
          WRITE(16,1230)NODEM,ISLAV,ISFIX
          READ(15,*)(KSLAV(I),I=1,ISLAV)
          WRITE(16,1240)(KSLAV(I),I=1,ISLAV)
C Place these degrees of freedom in the master array
          IDOFN=(NODEM-1)*NDOFN+1
          IDOF1=IDOFN+1
          DO 96 IS=1,ISLAV
            KDOFN=(KSLAV(IS)-1)*NDOFN+1
            IF(NDOFN.EQ.2)THEN
              IF(ISFIX.EQ.10.OR.ISFIX.EQ.11)THEN
                IF(MASTER(KDOFN).NE.KDOFN)CALL ERRPRT('ED0050')
                MASTER(KDOFN)=IDOFN
              ENDIF
              KDOFN=KDOFN+1
              IF(ISFIX.EQ.1.OR.ISFIX.EQ.11)THEN
                IF(MASTER(KDOFN).NE.KDOFN)CALL ERRPRT('ED0051')
                MASTER(KDOFN)=IDOF1
              ENDIF
            ENDIF
   96     CONTINUE
   97   CONTINUE
      ELSE
C No master/slave nodes specified
        WRITE(16,1210)
      ENDIF
C
C Does some further checks on input data
C ======================================
      CALL CHECK2(MXFRON)
C
C Set up nodal valencies for nodal averaging
C ==========================================
C
      DO 110 IPOIN=1,NPOIN
        DO 105 IGRUP=1,NGRUP
          NVALEN(IPOIN,IGRUP)=0
  105   CONTINUE
  110 CONTINUE
C Evaluate nodal valencies
      DO 130 IELEM=1,NELEM
        IGRUP=IGRPID(IELEM)
        IELIDN=IELTID(IGRUP)
        NNODE=IELPRP(3,IELIDN)
        DO 120 INODE=1,NNODE
          LNODE=IABS(LNODS(IELEM,INODE))
          NVALEN(LNODE,IGRUP)=NVALEN(LNODE,IGRUP)+1
  120   CONTINUE
  130 CONTINUE
C
C Read curves of results
C =======================
C
      CALL FNDKEY
     1(   FOUND  ,IWBEG  ,IWEND  ,'CURVES',
     2    INLINE ,15     ,NWRD   )
      IF(.NOT.FOUND)CALL ERRPRT('ED0200')
      IF(NWRD.EQ.1)CALL ERRPRT('ED0201')
      NCURV=INTNUM(INLINE(IWBEG(2):IWEND(2)))
      IF(NCURV.LT.0)  CALL ERRPRT('ED0202')
      IF(NCURV.GT.99) CALL ERRPRT('ED0202')
C
C =======================================
C                HORIZONTAL AXIS OF CURVE
C =======================================
C DATA ON NODES
C AVERAGE FOR HYPLAS
C ---------------------------------------
C LCURV(ICURV,1)  = number of node
C LCURV(ICURV,2)  = number of DOF
C                 = 0: Not-Apply
C                 = 1: X-displacement
C                 = 2: Y-displacement
C                 = 3: X-Force
C                 = 4: Y-Force
C                 = 5: X-Stress
C                 = 6: Y-Stress
C                 = 7: X-Strain
C                 = 8: Y-Strain
C
C =======================================
C DATA ON GAUSS POINTS
C ---------------------------------------
C LCURV(ICURV,1)  = number of Element
C LCURV(ICURV,2)  = number of DOF
C LCURV(ICURV,5)  = number of Gauss Point (H)
C                 = 0: Not-Apply
C                 = 9: X-Stress (GP)
C                 = 10: Y-Stress (GP)
C                 = 11: X-Strain (GP)
C                 = 12: Y-Strain (GP)
C
C =======================================
C                  VERTICAL AXIS OF CURVE
C =======================================
C DATA ON NODES
C AVERAGE FOR HYPLAS
C ---------------------------------------
C LCURV(ICURV,3)  = number of node
C LCURV(ICURV,4)  = number of DOF:
C                 = 1: X-displacement
C                 = 2: Y-displacement
C                 = 3: X-Force
C                 = 4: Y-Force
C                 = 5: X-Stress
C                 = 6: Y-Stress
C                 = 7: X-Strain
C                 = 8: Y-Strain
C
C =======================================
C DATA ON GAUSS POINTS
C ---------------------------------------
C LCURV(ICURV,3)  = number of Element
C LCURV(ICURV,4)  = number of DOF
C LCURV(ICURV,6)  = number of Gauss Point (V)
C                 = 0: Not-Apply
C                 = 9: X-Stress (GP)
C                 = 10: Y-Stress (GP)
C                 = 11: X-Strain (GP)
C                 = 12: Y-Strain (GP)
C
C ========================================
C NOTE: PLEASE FILL-IN ALL THE VALUES OF THE TABLE
C       IN THE CASE THAT THE GP DOES NOT APPLY, FILL
C       THIS CELLS WITH A ZERO VALUE
C ========================================
	DO 140 ICURV=1,NCURV
        IF(NDIME.EQ.2)THEN
          READ(15,*)(LCURV(ICURV,IDATC),IDATC=1,6)
        ENDIF
  140 CONTINUE
c
c Read strong discontinuity parameters set by user
c ================================================
c
      call fndkey
     &( found       ,iwbeg      ,iwend      ,'LOCALIZATION' ,
     &  inline      ,15         ,nwrd       )
      if (found) then
        read(15,*) gamloc, gamwsd
        read(15,*) vecnre(1), vecnre(2)
        read(15,*) tauloc, taumxd
        read(15,*) qinj, qinf
        read(15,*) parak
        read(15,*) injtyp
      end if
c
      RETURN
      END
