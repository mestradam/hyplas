      SUBROUTINE SHPFUN
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,IELTYP     ,
     2    MDIME      ,SHAPE      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE '../ELEMENTS.INC'
      DIMENSION
     1    DERIV(MDIME,*)     ,SHAPE(*)
C***********************************************************************
C CALL SPECIFIC ROUTINES FOR COMPUTATION OF SHAPE FUNCTIONS AND
C SHAPE FUNCTION DERIVATIVES FOR EACH TYPE OF ELEMENT
C
C REFERENCE: Section 5.6.3
C***********************************************************************
      IF(IELTYP.EQ.TRI3)THEN
        CALL SFT3
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      ELSEIF(IELTYP.EQ.QUAD4)THEN
        CALL SFQ4
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      ELSEIF(IELTYP.EQ.QUAD8)THEN
        CALL SFQ8
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      ELSEIF(IELTYP.EQ.QUA4FB)THEN
        CALL SFQ4FB
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      ELSEIF(IELTYP.EQ.QUA4SD)THEN
        CALL SFQ4SD
     1(   DERIV      ,ETASP      ,EXISP      ,IBOUND     ,MDIME      ,
     2    SHAPE      )
      ELSE
        CALL ERRPRT('EI0005')
      ENDIF
C
      RETURN
      END
