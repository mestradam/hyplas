      INTEGER FUNCTION INTNUM(CHRSTR)
      IMPLICIT NONE
      CHARACTER*(*) CHRSTR
      INTEGER I, IASCII, IEND, IPOWER, LEN, LENGTH, NUMBER
C***********************************************************************
C CONVERTS A NUMBER CONTAINED IN A CHARACTER STRING INTO AN INTEGER
C***********************************************************************
 1000 FORMAT(/15X,'ERROR: String of blank characters passed'/
     1        22X,'into integer conversion function INTMUN')
 1100 FORMAT(/15X,'ERROR: Invalid character in string ''',A,''' passed'/
     1        22X,'into integer conversion function INTMUN')
C
      LENGTH=LEN(CHRSTR)
      DO 10 I=LENGTH,1,-1
        IF(CHRSTR(I:I).NE.' ')THEN
          IEND=I
          GOTO 20
        ENDIF
   10 CONTINUE
      WRITE(*,1000)
      WRITE(16,1000)
      CALL PEXIT
   20 CONTINUE
      INTNUM=0
      IPOWER=0
      DO 30 I=IEND,1,-1
        IASCII=ICHAR(CHRSTR(I:I))
        IF(IASCII.GE.48.AND.IASCII.LE.57)THEN
          NUMBER=IASCII-48
          INTNUM=INTNUM+NUMBER*(10**IPOWER)  
          IPOWER=IPOWER+1
        ELSEIF(CHRSTR(I:I).EQ.' ')THEN
          GOTO 40
        ELSEIF(CHRSTR(I:I).EQ.'-'.OR.CHRSTR(I:I).EQ.'+')THEN
          IF(I.NE.IEND)THEN
            IF(CHRSTR(I:I).EQ.'-')INTNUM=-INTNUM
            GOTO 40
          ELSE
            WRITE(*,1100)CHRSTR(1:IEND)
            WRITE(16,1100)CHRSTR(1:IEND)
            CALL PEXIT
          ENDIF
        ELSE
          WRITE(*,1100)CHRSTR(1:IEND)
          WRITE(16,1100)CHRSTR(1:IEND)
          CALL PEXIT
        ENDIF
   30 CONTINUE
   40 CONTINUE
      RETURN
      END
