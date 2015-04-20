      SUBROUTINE FCLOSE
C***********************************************************************
C CLOSES DATA AND RESULTS FILES
C***********************************************************************
      CLOSE(UNIT=15,STATUS='KEEP')
      CLOSE(UNIT=16,STATUS='KEEP')
      CLOSE(UNIT=21,STATUS='KEEP')
	CLOSE(UNIT=22,STATUS='KEEP')
	CLOSE(UNIT=31,STATUS='KEEP')
      RETURN
      END
