      SUBROUTINE CTDMEL
     1(   DMATX      ,NTYPE      ,RPROPS     ,RSTAVA     ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MSTRE=4)
C Arguments
      DIMENSION
     1    DMATX(MSTRE,MSTRE) ,RPROPS(*)          ,RSTAVA(*)          ,
     2    STRES(MSTRE)
C Local arrays and variables
      LOGICAL
     1    REPEAT             ,OUTOFP
      DIMENSION
     1    DPSTRA(3,3)        ,DPSTRE(3,3)        ,EIGPRJ(MSTRE,2)    ,
     2    PDMINU(3,3)        ,PDPLUS(3,3)        ,PSTRA(3)           ,
     3    PSTRES(3)          ,STRAIN(MSTRE)      ,VI(3)              ,
     4    VIDMIN(3)          ,VIDPLU(3)
      DATA
     1    VI(1),VI(2),VI(3)/
     2    1.D0 ,1.D0 ,1.D0 /
      DATA
     1    R0   ,R1   ,R2   /
     2    0.0D0,1.0D0,2.0D0/
C***********************************************************************
C COMPUTATION OF THE TANGENT MODULUS (ELASTICITY MATRIX) FOR THE
C ISOTROPICALLY DAMAGED ISOTROPIC ELASTIC MATERIAL MODEL WITH
C MICROCRACK/VOID CLOSURE EFFECTS
C
C REFERENCE: Section 12.6.1
C***********************************************************************
      IF(NTYPE.NE.2.AND.NTYPE.NE.3)CALL ERRPRT('EI0056')
C Set material constants
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      DAMAGE=RPROPS(4)
      HFACT=RPROPS(5)
C Retrieve current (physical) strains
      STRAIN(1)=RSTAVA(1)
      STRAIN(2)=RSTAVA(2)
      STRAIN(3)=RSTAVA(3)/R2
      STRAIN(4)=RSTAVA(4)
C Perform spectral decomposition of the strain tensor
      CALL SPDEC2(EIGPRJ,PSTRA,REPEAT,STRAIN)
      PSTRA(3)=STRAIN(4)
C Compute the principal stresses (internal product between stress
C tensor and individual eigenprojection tensors)
      PSTRES(1)=STRES(1)*EIGPRJ(1,1)+STRES(2)*EIGPRJ(2,1)+
     1          R2*STRES(3)*EIGPRJ(3,1)
      PSTRES(2)=STRES(1)*EIGPRJ(1,2)+STRES(2)*EIGPRJ(2,2)+
     1          R2*STRES(3)*EIGPRJ(3,2)
      PSTRES(3)=STRES(4)
C Zero relevant arrays
      CALL RVZERO(PDPLUS,9)
      CALL RVZERO(PDMINU,9)
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
     1                POISS/YOUNG*(VI(I)*(VIDPLU(J)+VIDMIN(J)))
   10   CONTINUE
   20 CONTINUE
C Compute elasticity operator that transforms principal strains into
C principal stresses (matrix of derivatives of principal stresses with
C respect to principal strains)
      CALL INVMT3(DPSTRA,DPSTRE,DET)
C
C Use general routine for isotropic tensor functions of a single tensor
C to compute the tangent operator
C
      IF(NTYPE.EQ.2)THEN
        OUTOFP=.FALSE.
      ELSEIF(NTYPE.EQ.3)THEN
        OUTOFP=.TRUE.
      ENDIF
      CALL DGISO2
     1(   DPSTRE     ,DMATX      ,EIGPRJ     ,PSTRA      ,PSTRES     ,
     2    OUTOFP     ,REPEAT     )
C
      RETURN
      END
