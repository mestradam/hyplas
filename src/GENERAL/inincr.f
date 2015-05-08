      SUBROUTINE ININCR
     1(   DFACT      ,DLENP      ,FSTOP      ,ITDES      ,MINCS      ,
     2    MITER      ,NALGO      ,NINCS      ,TOLER      ,
     3    DFACTV     ,MITERV     ,NOUTP      ,NOUTPV     ,TOLERV     )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION
     1    DFACTV(MINCS)      ,MITERV(MINCS)      ,NOUTP(5)           ,
     2    NOUTPV(5,MINCS)    ,TOLERV(MINCS)
C
      LOGICAL FOUND
      CHARACTER*80 INLINE
      DIMENSION
     1    IWBEG(40), IWEND(40)
      PARAMETER
     1(   R0=0.0D0   )
C***********************************************************************
C READS INPUT DATA FOR LOAD INCREMENTATION
C
C REFERENCE: Figure 5.1
C***********************************************************************
 1000 FORMAT(///' Increment control with fixed load increments selected'
     1         /' ====================================================='
     2        //'       Number of proportional load increments =',I5/)
C
 1010 FORMAT(///' Increment control by the Arc-Length method selected'/
     1          ' ==================================================='//
     2          ' Arc length data'/' ---------------'//
     3' Maximum allowed number of increments ........= ',I5)
 1020 FORMAT(
     1' Initial load increment factor .............. = ',G15.6/
     2' Convergence tolerence ...................... = ',G15.6/
     3' Max. No. of iterations ..................... = ',I5)
 1030 FORMAT(/' Output control parameter for results'/,
     1        '       ( Output frequencies )'/
     2    ' Displacements......................... = ',I3/
     3    ' Reactions............................. = ',I3/
     4    ' State variables at gauss points....... = ',I3/
     5    ' State variables at nodes.............. = ',I3/
     6    ' Output to re-start file............... = ',I3)
 1040 FORMAT(/
     1' Desired number of iterations per increment .. =',I5/
     2' Maximum load factor ......................... =',G15.6/
     3' Maximum arc length parameter ................ =',G15.6)
C
      CALL FNDKEY
     1(   FOUND      ,IWBEG      ,IWEND      ,'INCREMENTS',
     2    INLINE     ,15         ,NWRD       )
      IF(.NOT.FOUND)CALL ERRPRT('ED0079')
C
      IF(NALGO.GT.0)THEN
C Fixed increments option
C -----------------------
        IF(NWRD.EQ.1)CALL ERRPRT('ED0034')
        NINCS=INTNUM(INLINE(IWBEG(2):IWEND(2)))
        WRITE(16,1000)NINCS
        IF(NINCS.LE.0)    CALL ERRPRT('ED0013')
        IF(NINCS.GT.MINCS)CALL ERRPRT('ED0035')
        DO 10 IINCS=1,NINCS
          READ(15,*,ERR=997,END=997)DFACTV(IINCS),TOLERV(IINCS),
     1                             MITERV(IINCS),(NOUTPV(I,IINCS),I=1,5)
          IF(TOLERV(IINCS).LE.R0)CALL ERRPRT('ED0142')
          IF(MITERV(IINCS).LT.1)CALL ERRPRT('ED0146')
   10   CONTINUE
      ELSE
C Arc-length control
C ------------------
        IF(NWRD.EQ.1) CALL ERRPRT('ED0097')
        NINCS=INTNUM(INLINE(IWBEG(2):IWEND(2)))
        WRITE(16,1010)NINCS
        IF(NINCS.LE.0)CALL ERRPRT('ED0098')
C
        READ(15,*,ERR=998,END=998)DFACT,TOLER,MITER,(NOUTP(I),I=1,5),
     1                            ITDES,FSTOP,DLENP
        IF(TOLER.LT.R0)CALL ERRPRT('ED0142')
        IF(ITDES.LT.0)CALL ERRPRT('ED0143')
        WRITE(16,1020)DFACT,TOLER,MITER
        WRITE(16,1030)(NOUTP(I),I=1,5)
        WRITE(16,1040)ITDES,FSTOP,DLENP
      ENDIF
C Send error messages in case of I/O error while reading increment data
      GOTO 999
  997 CALL ERRPRT('ED0178')
      GOTO 999
  998 CALL ERRPRT('ED0179')
  999 CONTINUE
      RETURN
      END
