      SUBROUTINE SUDMEL
     1(   NTYPE      ,RPROPS     ,RSTAVA     ,STRAN      ,STRES      ,
     2    SUFAIL     )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MSTRE=4)
C Arguments
      LOGICAL  SUFAIL
      DIMENSION
     1    RPROPS(*)          ,RSTAVA(MSTRE)      ,STRAN(MSTRE)       ,
     2    STRES(MSTRE)
C Local variables and arrays
      LOGICAL  DUMMY
      DIMENSION
     1    DPSTRA(3,3)        ,DPSTRE(3,3)        ,EED(MSTRE)         ,
     2    EIGPRJ(MSTRE,2)    ,PDMINU(3,3)        ,PDPLUS(3,3)        ,
     4    PSTRA(3)           ,PSTRES(3)          ,RESVEC(3)          ,
     5    SIGMA(MSTRE)       ,STRAIN(MSTRE)      ,VI(3)              ,
     6    VIDMIN(3)          ,VIDPLU(3)
      DATA
     1    VI(1),VI(2),VI(3)/
     2    1.D0 ,1.D0 ,1.D0 /
      DATA
     1    R0   ,R1   ,R2    ,R3    ,TOL   / 
     2    0.D0 ,1.D0 ,2.0D0 ,3.0D0 ,1.D-10/
      DATA
     1    MXITER/    10/
C***********************************************************************
C STATE UPDATE PROCEDURE FOR ISOTROPICALLY DAMAGED ISOTROPIC ELASTIC
C MODEL ACCOUNTING FOR PARTIAL MICROCRACK/VOID CLOSURE EFFECTS
C
C REFERENCE: Box 12.7
C***********************************************************************
      IF(NTYPE.NE.2.AND.NTYPE.NE.3)CALL ERRPRT('EI0057')
C Initialise logical flag
      SUFAIL=.FALSE.
C Set material constants
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      DAMAGE=RPROPS(4)
      HFACT=RPROPS(5)
      GMODU=RPROPS(6)
      BULK=RPROPS(7)
C Transform engineering into physical strains
      STRAIN(1)=STRAN(1)
      STRAIN(2)=STRAN(2)
      STRAIN(3)=STRAN(3)/R2
      STRAIN(4)=STRAN(4)
C Perform spectral decomposition of the strain tensor
      CALL SPDEC2(EIGPRJ,PSTRA,DUMMY,STRAIN)
      PSTRA(3)=STRAN(4)
C
C Newton-Raphson iterations to solve the piece-wise linear elastic
C constitutive equation
C ----------------------------------------------------------------
C
C Set initial guess for principal stresses as the stresses obtained for
C the damaged elastic material without partial crack closure effects
C... compute corresponding stress tensor components first
C     Trace of strain 
      EEV=STRAN(1)+STRAN(2)+STRAN(4)
C     Mead strain 
	EEVD3=EEV/R3
C     Desviator strain
      EED(1)=STRAN(1)-EEVD3
      EED(2)=STRAN(2)-EEVD3
	EED(3)=STRAN(3)
      EED(4)=STRAN(4)-EEVD3
C
      P=(R1-DAMAGE)*BULK*EEV
      R1DR2G=(R1-DAMAGE)*R2*GMODU
C
      SIGMA(1)=R1DR2G*EED(1)+P
      SIGMA(2)=R1DR2G*EED(2)+P
      SIGMA(3)=R1DR2G*EED(3)
      SIGMA(4)=R1DR2G*EED(4)+P
C	¿Desviator or total strain?
C
C... and then the principal stresses (internal product between stress
C    tensor and individual eigenprojection tensors)
      PSTRES(1)=SIGMA(1)*EIGPRJ(1,1)+SIGMA(2)*EIGPRJ(2,1)+
     1          R2*SIGMA(3)*EIGPRJ(3,1)
      PSTRES(2)=SIGMA(1)*EIGPRJ(1,2)+SIGMA(2)*EIGPRJ(2,2)+
     1          R2*SIGMA(3)*EIGPRJ(3,2)
      PSTRES(3)=SIGMA(4)
C Zero relevant arrays
      CALL RVZERO(PDPLUS,9)
      CALL RVZERO(PDMINU,9)
C
C Begin N-R iterations
C
      DO 80 ITER=1,MXITER
C Construct current projection matrices
C... positive and negative principal stress projection matrices
        IF(PSTRES(1).GE.R0)THEN
          PDPLUS(1,1)=R1/(R1-DAMAGE)
          PDMINU(1,1)=R0
        ELSE
          PDPLUS(1,1)=R0
          PDMINU(1,1)=R1/(R1-HFACT*DAMAGE)
        ENDIF
        IF(PSTRES(2).GE.R0)THEN
          PDPLUS(2,2)=R1/(R1-DAMAGE)
          PDMINU(2,2)=R0
        ELSE
          PDPLUS(2,2)=R0
          PDMINU(2,2)=R1/(R1-HFACT*DAMAGE)
        ENDIF
        IF(PSTRES(3).GE.R0)THEN
          PDPLUS(3,3)=R1/(R1-DAMAGE)
          PDMINU(3,3)=R0
        ELSE
          PDPLUS(3,3)=R0
          PDMINU(3,3)=R1/(R1-HFACT*DAMAGE)
        ENDIF
C... positive and negative trace operators
        TRACE=PSTRES(1)+PSTRES(2)+PSTRES(3)
        IF(TRACE.GE.R0)THEN
          VIDPLU(1)=R1/(R1-DAMAGE)
          VIDPLU(2)=R1/(R1-DAMAGE)
          VIDPLU(3)=R1/(R1-DAMAGE)
          VIDMIN(1)=R0
          VIDMIN(2)=R0
          VIDMIN(3)=R0
        ELSE
          VIDPLU(1)=R0
          VIDPLU(2)=R0
          VIDPLU(3)=R0
          VIDMIN(1)=R1/(R1-HFACT*DAMAGE)
          VIDMIN(2)=R1/(R1-HFACT*DAMAGE)
          VIDMIN(3)=R1/(R1-HFACT*DAMAGE)
        ENDIF
C Inverse elasticity operator that transforms principal stresses into
C principal strains (matrix of derivatives of principal strains with
C respect to principal stresses)
        DO 20 I=1,3
          DO 10 J=1,3
            DPSTRA(I,J)=(R1+POISS)/YOUNG*(PDPLUS(I,J)+PDMINU(I,J))-
     1                  POISS/YOUNG*(VI(I)*(VIDPLU(J)+VIDMIN(J)))
   10     CONTINUE
   20   CONTINUE
C Compute residual of constitutive equation   
        DO 40 I=1,3
          RESVEC(I)=R0
          DO 30 J=1,3
            RESVEC(I)=RESVEC(I)-DPSTRA(I,J)*PSTRES(J)
   30     CONTINUE
          RESVEC(I)=PSTRA(I)+RESVEC(I)
   40   CONTINUE
C... residual norm
        RESNOR=SQRT(RESVEC(1)**2+RESVEC(2)**2+RESVEC(3)**2)
        IF(RESNOR.LT.TOL)THEN
C Iterations converged: update stress tensor components, store
C                       engineering strain in RSTAVA, break N-R loop
C                       and exit this routine
          STRES(1)=PSTRES(1)*EIGPRJ(1,1)+PSTRES(2)*EIGPRJ(1,2)
          STRES(2)=PSTRES(1)*EIGPRJ(2,1)+PSTRES(2)*EIGPRJ(2,2)
          STRES(3)=PSTRES(1)*EIGPRJ(3,1)+PSTRES(2)*EIGPRJ(3,2)
          STRES(4)=PSTRES(3)
          RSTAVA(1)=STRAN(1)
          RSTAVA(2)=STRAN(2)
          RSTAVA(3)=STRAN(3)
          RSTAVA(4)=STRAN(4)
          GOTO 999
        ENDIF
C Compute elasticity operator that transforms principal strains into
C principal stresses (matrix of derivatives of principal stresses with
C respect to principal strains). This is the inverse of the residual
C vector derivative
        CALL INVMT3(DPSTRA,DPSTRE,DET)
C Apply N-R correction to principal stresses
        DO 70 I=1,3        
          DO 60 J=1,3        
            PSTRES(I)=PSTRES(I)+DPSTRE(I,J)*RESVEC(J)
   60     CONTINUE 
   70   CONTINUE 
   80 CONTINUE
C N-R loop not converged: Failure of stress update procedure.
C                         Stresses are not updated. Send warning message
      SUFAIL=.TRUE.
      CALL ERRPRT('WE0020')
C
  999 RETURN
      END
