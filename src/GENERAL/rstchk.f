      SUBROUTINE RSTCHK(  RSTINP     ,RSTRT      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL RSTRT
      CHARACTER*256 RSTINP
C
      LOGICAL AVAIL,FOUND
      CHARACTER*80 INLINE
      DIMENSION IWBEG(40),IWEND(40)
C***********************************************************************
C CHECKS WETHER MAIN DATA IS TO BE READ FROM INPUT RE-START FILE
C AND SET INPUT RE-START FILE NAME IF REQUIRED
C***********************************************************************
 1000 FORMAT(////,
     1' Main input data read from re-start file'/
     2' ======================================='///
     3'            Input re-start file name ------> ',A)
C
C Checks whether the input data file contains the keyword RESTART
C
        CALL FNDKEY
     1(   FOUND     ,IWBEG    ,IWEND    ,'RESTART',
     2    INLINE    ,15       ,NWRD     )
        IF(FOUND)THEN
C sets re-start flag and name of input re-start file
          RSTRT=.TRUE.
          RSTINP=INLINE(IWBEG(2):IWEND(2))//'.rst'
          WRITE(16,1000)INLINE(IWBEG(2):IWEND(2))//'.rst'
C checks existence of the input re-start file
          INQUIRE(FILE=RSTINP,EXIST=AVAIL)
          IF(.NOT.AVAIL)CALL ERRPRT('ED0096')
        ELSE
          RSTRT=.FALSE.
        ENDIF
C
      RETURN
      END
