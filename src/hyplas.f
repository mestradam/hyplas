C***********************************************************************
C----------------------------------------------------------------------*
C|                                                                    |*
C|                                                                    |*
C|    H Y P L A S    version 2.0                                      |*
C|                                                                    |*
C|                                                                    |*
C|    Program for implicit small and large strain                     |*
C|    finite element analysis of hyperelastic and                     |*
C|    elastoplastic solids.                                           |*
C|                                                                    |*
C|                                                                    |*
C|    Copyright (c) 1996-2008  EA de Souza Neto, D Peric & DRJ Owen   |*
C|                             Civil and Computational Eng. Centre    |*
C|                             School of Engineering                  |*
C|                             Swansea University                     |*
C|                                                                    |*
C|                                                                    |*
C|    This program is a companion to the textbook:                    |*
C|    EA de Souza Neto, D Peric & DRJ Owen. Computational             |*
C|    Methods for Plasticity: Theory and Applications. Wiley,         |*
C|    Chichester, 2008.                                               |*
C|                                                                    |*
C|                                                                    |*
C|    Please send BUG REPORTS to                                      |*
C|                                                                    |*
C|                        hyplas_v2.0@live.co.uk                      |*
C|                                                                    |*
C|    NOTE: Messages sent to the authors' personal email addresses    |*
C|          will NOT be answered.                                     |*
C|                                                                    |*
C----------------------------------------------------------------------*
C***********************************************************************
C----------------------------------------------------------------------*
C                                                                      *
C     COPYRIGHT STATEMENT                                              *
C                                                                      *
C     You may only use this program for your own private purposes.     *
C     You are not allowed, in any circumstances, to distribute this    *
C     program (including its source code, executable and any other     *
C     files related to it, either in their original version or any     *
C     modifications introduced by you, the authors or any other party) *
C     in whole or in part, either freely or otherwise, in any medium,  *
C     without the prior written consent of the copyright holders.      *
C                                                                      *
C     DISCLAIMER                                                       *
C                                                                      *
C     This program (including its source code, executable and any      *
C     other files related to it) is provided "as is" without warranty  *
C     of any kind, either expressed or implied, including, but not     *
C     limited to, any implied warranties of fitness for purpose.       *
C     In particular, THIS PROGRAM IS BY NO MEANS GUARANTEED TO BE FREE *
C     FROM ERRORS.                                                     *
C     The results produced by this program are in no way garanteed to  *
C     be fit for any purpose.                                          *
C     This program (or any modification incorporated to it by you, the *
C     authors or any other party) will be used entirely at your own    *
C     risk.                                                            *
C     Under no circumstances will the authors/copyright holders be     *
C     liable to anyone for damages, including any general, special,    *
C     incidental or consequential damages arising from the use or      *
C     inability to use the program (including, but not limited to,     *
C     loss or corruption of data, failure of the program to operate in *
C     any particular way as well as damages arising from the use of    *
C     any results produced by the program for any purpose).            *
C                                                                      *
C     CONDITIONS OF USE                                                *
C                                                                      *
C     You may only use this program if you fully understand and agree  *
C     with the terms of the above disclaimer. You must not use this    *
C     program if you do not agree with or do not understand (fully or  *
C     in part) these conditions of use.                                *
C                                                                      *
C----------------------------------------------------------------------*
C***********************************************************************
C
C
      PROGRAM HYPLAS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Hyplas database: Global parameters and common blocks
      INCLUDE 'MAXDIM.INC'
      INCLUDE 'MATERIAL.INC'
      INCLUDE 'ELEMENTS.INC'
      INCLUDE 'GLBDBASE.INC'
      include 'LOCALIZA.INC'
C Common block of arrays used only by the frontal solver
      COMMON / FRONTA /
     1    EQRHS(MTOTV,2)     ,EQROW(MFRON,MTOTV) ,EQCOL(MFRON,MTOTV) ,
     2    DECAY(MFRON)       ,GLOAD(MFRON,2)     ,VECRV(MFRON,2)     ,
     3    LOCEL(MEVAB,MELEM) ,NACVA(MFRON,MELEM) ,NAMEV(MTOTV)       ,
     4    NDEST(MEVAB,MELEM) ,NPIVO(MTOTV)       ,NFRON(MELEM)
C Logical control flags for main program
      LOGICAL
     1    CONVRG     ,DIVERG     ,INCCUT     ,RSTRT      ,UNSYM      ,
     2    albrk
C File names
      CHARACTER*256
     1    DATFIL     ,RESFIL     ,RSTINP     ,RSTOUT     ,GIDMSH     ,
     2    GIDRES     ,CURVES
C Increment control arrays for main program
      DIMENSION
     1    DFACTV(MINCS)      ,DFSUB(MSUBIN)      ,MITERV(MINCS)      ,
     2    NOUTP(5)           ,NOUTPV(5,MINCS)    ,TOLERV(MINCS)
C Numerical constants
      PARAMETER
     1(   R0=0.0D0   ,RP5=0.5D0  ,RP7=0.7D0  )
C***********************************************************************
C
 1000 FORMAT(//20X,' H Y P L A S   ANALYSIS  RESULTS'/
     1         20X,'================================='/)
 1010 FORMAT(
     1 7X,' _________________________________________________________ '/
     2 7X,'|                                                         |'/
     3 7X,'| Program compiled with the dimensioning parameters       |'/
     4 7X,'|_________________________________________________________|'/
     5 7X,'|                                                 |       |'/
     6 7X,'| Maximum number of elements              (MELEM) |',I6,' |'/
     7 7X,'| Maximum frontwidth allowed in solution  (MFRON) |',I6,' |'/
     8 7X,'| Maximum number of element groups        (MGRUP) |',I6,' |'/
     9 7X,'| Maximum number of load increments       (MINCS) |',I6,' |'/
     O 7X,'| Maximum number of nodal points          (MPOIN) |',I6,' |'/
     1 7X,'| Size of increment cutting stack array   (MSUBIN)|',I6,' |'/
     2 7X,'| Max. number of nodes with prescr.displ. (MVFIV) |',I6,' |'/
     3 7X,'|_________________________________________________|_______|'/
     4                                                                 )
 1015 FORMAT(/,' Data file name:'/
     1         ' ==============='//,1X,A/)
 1020 FORMAT(////15X,'Reading data...')
 1030 FORMAT(////15X,'H Y P L A S   ANALYSIS starting...'/)
 1070 FORMAT(/////15X,'Program  H Y P L A S  successfully completed.')
 1080 FORMAT(///15X,'Data file name was ------> ',A)
 1090 FORMAT(// 15X,'Results file name is ----> ',A///)
 1092 FORMAT(// 15X,'GiD mesh file name is ---> ',A///)
 1094 FORMAT(// 15X,'GiD results file name  --> ',A///)
 1095 FORMAT(   15X,'Re-start file name is ---> ',A//
     1          15X,'Last increment written -->',I5///)
 1096 FORMAT(// 15X,'Curves results file    --> ',A///)
C
C
C
C Start up. Read data, initialise variables, etc...
C
C REFERENCE: Flowchart of Figure 5.1
C
C *************************************************
C
C Send greeting message to standard output
      CALL GREET
C Read names and open relevant files
      CALL FOPEN( DATFIL, RESFIL, RSTOUT, GIDMSH, GIDRES, CURVES)
C Echo dimensioning parameters defined in file MAXDIM.INC
      WRITE(16,1000)
      WRITE(16,1010)MELEM,MFRON,MGRUP,MINCS,MPOIN,MSUBIN,MVFIX
C Echo data file name
      I=INDEX(DATFIL,' ')-1
      WRITE(16,1015)DATFIL(1:I)
C
C Read relevant data from input data/re-start file
C ------------------------------------------------
C Check if main data is to be read from the input data file or from an
C input re-start file
      CALL RSTCHK(  RSTINP     ,RSTRT      )
      WRITE(*,1020)
C
      IF(RSTRT)THEN
C Re-start mode: Read main data from input re-start file
         CALL RSTART
     1(  DFOLD      ,DLENG      ,DLENGO     ,DLENM      ,DLAMD      ,
     2   IFNEG      ,IINCS      ,MXFRON     ,NOUTP      ,TFACT      ,
     3   TFACTO     ,UNSYM      ,RSTINP     ,RSTOUT     ,0          ,
     4   IDUMMY     )
      ELSE
C Not re-start mode: Read main data from input data file
C ...read most of the problem data
        CALL INDATA(MXFRON ,UNSYM)
C ...read and evaluate the applied external loads
        CALL INLOAD
      ENDIF
C
C For any mode: Read load incrementation data from input data file
      CALL ININCR
     1(   DFACT      ,DLENP      ,FSTOP      ,ITDES      ,MINCS      ,
     2    MITER      ,NALGO      ,NINCS      ,TOLER      ,
     3    DFACTV     ,MITERV     ,NOUTP      ,NOUTPV     ,TOLERV     )
C
C Initialise some variables and arrays if not in re-start mode
C ------------------------------------------------------------
      INCRST=0
      IF(.NOT.RSTRT)THEN
        CALL INITIA
     1(   DLAMD      ,IFNEG      ,KUNLD      ,TFACT      )
      ENDIF
C
C
C
C Start incremental finite element analysis
C *****************************************
C
      write(*,1030)
C
C=======================================================================
C                                                                      |
C                   Start loop over load increments                    |
C                                                                      |
C REFERENCE: Chapter 4 (Boxes 4.1-4) of the companion textbook.        |
C            Section 5.4.                                              |
C            The load incrementation loops carried out here are those  |
C            of the Flowcharts of Figures 5.2-3.                       |
C                                                                      |
C=======================================================================
C
      IF(.NOT.RSTRT)IINCS=0
      albrk = .false.
C
      DO 50 ICOUNT=1,NINCS
C
C Call interface to select solution algorithm
        call algi
     .  (dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     .   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     .   tfacto     ,unsym      ,rstinp     ,rstout     ,dfact      ,
     .   dlenp      ,fstop      ,itdes      ,nincs      ,dfactv     ,
     .   miter      ,miterv     ,noutpv     ,tolerv     ,kunld      ,
     .   icount     ,albrk      ,nalgo      )
C
C Arc-length only: Break loop over increments and stop if maximum
C prescribed load factor has been exceeded
        if(albrk) goto 60
C
   50 continue
C=======================================================================
C                   End loop over load increments                      |
C=======================================================================
C
   60 continue
C
C Exit HYPLAS
C ***********
C
C Close files before exit
      CALL FCLOSE
C Echo file names back to standard output and stop
      WRITE(*,1070)
      I=INDEX(RESFIL,' ')-1
      WRITE(*,1080)DATFIL(1:I)
      WRITE(*,1090)RESFIL(1:I)
      WRITE(*,1092)GIDMSH(1:I+4)
      WRITE(*,1094)GIDRES(1:I+4)
      WRITE(*,1096)CURVES(1:I+2)
      IF(INCRST.NE.0)THEN
        WRITE(*,1095)RSTOUT(1:I),INCRST
      ENDIF
      STOP ' '
      END
