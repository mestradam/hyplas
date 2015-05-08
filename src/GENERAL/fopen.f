      SUBROUTINE FOPEN
     1(   DATFIL   ,RESFIL   ,RSTOUT   ,GIDMSH, GIDRES, CURVES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER DATFIL*256,RESFIL*256,RSTOUT*256,GIDMSH*256,GIDRES*256
	CHARACTER CURVES*256
      CHARACTER BELL*1
      LOGICAL AVAIL
C***********************************************************************
C READ DATA FILE NAME FROM STANDARD INPUT, SET NAMES FOR AND OPEN DATA
C AND RESULTS FILES AND SET RE-START FILE NAME
C
C REFERENCE: Figure 5.1
C***********************************************************************
C Read data file name from standard input
C ---------------------------------------
      WRITE(*,'(////15X,A,/15X,A)')
     1'Data file name must have extension .dat or .DAT',
     2'and must not contain blank spaces.'
      WRITE(*,'(/15X,A)')'(Type EXIT or QUIT to terminate)'
   10 WRITE(*,'(//15X,A$)')'Input data file name --------> '
      READ(*,'(A)',ERR=10)DATFIL
C Sort out data, result and re-start file names and open data and
C result files
C ---------------------------------------------------------------
      BELL=CHAR(7)
C Find end of data file name
      I=INDEX(DATFIL,' ')-1
      IF(I.EQ.0)THEN
        WRITE(*,'(/15X,A)')
     1          'Data file name must NOT begin with blank space !'
        GOTO 10
      ELSEIF(I.EQ.4.AND.
     1         (DATFIL(1:4).EQ.'EXIT'.OR.DATFIL(1:4).EQ.'exit'
     2      .OR.DATFIL(1:4).EQ.'QUIT'.OR.DATFIL(1:4).EQ.'quit'))THEN
        WRITE(*,'(///15X,A,///)')'Program HYPLAS terminated by user.'
        STOP ' '
      ENDIF
C Check data file name extension: it must be either .dat or .DAT
      IF((I-3.LT.1).OR.(DATFIL(I-3:I).NE.'.dat'.AND.
     1                  DATFIL(I-3:I).NE.'.DAT'))THEN
        WRITE(*,'(/15X,A,/15X,A)')
     1          'Data file name does not have extension .dat or .DAT !',
     2          'Please try again'
        WRITE(*,'(1X,A$)')BELL
        GOTO 10
      ENDIF
C Check existence of data file
      INQUIRE(FILE=DATFIL,EXIST=AVAIL)
      IF(.NOT.AVAIL)THEN
        WRITE(*,'(/A,A,A,A)')
     1'               File "',DATFIL(1:I),'" not found !  ',
     2              ' Please try again'
        WRITE(*,'(1X,A$)')BELL
        GOTO 10
      ENDIF
C give name to results file (with extension .res)
      RESFIL(1:I-3)=DATFIL(1:I-3)
      RESFIL(I-3:I)='.res'
      RESFIL(I+1:256)=DATFIL(I+1:256)
C give name to GiD mesh file (with extension .gid.msh)
      GIDMSH(1:I-3)=DATFIL(1:I-3)
      GIDMSH(I-3:I+4)='.gid.msh'
      GIDMSH(I+5:256)=DATFIL(I+4:256)
C give name to GiD results file (with extension .gid.res)
      GIDRES(1:I-3)=DATFIL(1:I-3)
      GIDRES(I-3:I+4)='.gid.res'
      GIDRES(I+5:256)=DATFIL(I+4:256)
C give name to output re-start file (with extension .rst)
      RSTOUT(1:I-3)=DATFIL(1:I-3)
      RSTOUT(I-3:I)='.rst'
      RSTOUT(I+1:256)=DATFIL(I+1:256)
C give name to output curves file (with extension .cur00)
      CURVES(1:I-3)=DATFIL(1:I-3)
      CURVES(I-3:I+2)='.cur00'
      CURVES(I+3:256)=DATFIL(I+2:256)
C Open data and results file
      OPEN(UNIT=15,FILE=DATFIL,STATUS='OLD')
      OPEN(UNIT=16,FILE=RESFIL,STATUS='UNKNOWN')
	OPEN(UNIT=21,FILE=GIDMSH,STATUS='UNKNOWN')
	OPEN(UNIT=22,FILE=GIDRES,STATUS='UNKNOWN')
	OPEN(UNIT=31,FILE=CURVES,STATUS='UNKNOWN')
C
      RETURN
      END
