      SUBROUTINE PEXIT
C Print message
      WRITE(*,'(///15X,A,///)')'Program HYPLAS aborted.'
      WRITE(16,'(///15X,A,///)')'Program HYPLAS aborted.'
C Close files
      CALL FCLOSE
C and exit program
      STOP ' '
      END
