      SUBROUTINE CSTOGD
     1(   AMATX      ,B          ,IPROPS     ,NTYPE      ,RPROPS     ,
     2    STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL OUTOFP ,REPEAT
      PARAMETER
     1(   MADIM=5    ,MSTRE=4    ,NDIM=2     )
      PARAMETER(IPOGDC=2)
      DIMENSION
     1    AMATX(MADIM,MADIM) ,B(MSTRE)           ,IPROPS(*)          ,
     2    RPROPS(*)          ,STRES(MSTRE)
      DIMENSION
     1    DELTA(3,3)         ,DPSTRE(3,3)        ,DTAUDB(MSTRE,MSTRE),
     2    EIGPRJ(MSTRE,NDIM) ,EIGB(NDIM)         ,PSTALP(3)          ,
     3    PSTRES(3)          ,PSTRTC(3)
      DATA
     1    DELTA(1,1)   ,DELTA(1,2)   ,DELTA(1,3)    /
     2    1.0D0        ,0.0D0        ,0.0D0         /
     3    DELTA(2,1)   ,DELTA(2,2)   ,DELTA(2,3)    /
     4    0.0D0        ,1.0D0        ,0.0D0         /
     5    DELTA(3,1)   ,DELTA(3,2)   ,DELTA(3,3)    /
     6    0.0D0        ,0.0D0        ,1.0D0         /
      DATA
     1    R1   ,R2   ,R3   ,R6   /
     2    1.0D0,2.0D0,3.0D0,6.0D0/
C***********************************************************************
C COMPUTATION OF THE CONSISTENT SPATIAL TANGENT MODULUS 'a' FOR
C OGDEN TYPE HYPERELASTIC MATERIAL MODEL.
C PLANE STRESS, PLANE STRAIN AND AXISYMMETRIC IMPLEMENTATIONS.
C
C REFERENCE: Section 13.5.2
C***********************************************************************
C Set Ogden material constants
C ============================
C Number of terms in Ogden's strain-energy function
      NOGTRM=IPROPS(3)
C Bulk modulus (incompressibility penalty parameter)
      BULK=RPROPS(IPOGDC+NOGTRM*2)
C Compute principal stretches
C ===========================
C Perform spectral decomposition of the left Cauchy-Green tensor B 
      CALL SPDEC2
     1(   EIGPRJ     ,EIGB       ,REPEAT     ,B          )
C Compute in-plane principal stretches
      PSTRTC(1)=SQRT(EIGB(1))
      PSTRTC(2)=SQRT(EIGB(2))
C and out-of-plane stretches
      IF(NTYPE.EQ.1)THEN
        PSTRTC(3)=R1/(PSTRTC(1)*PSTRTC(2))
        DETF=R1
      ELSEIF(NTYPE.EQ.2)THEN
        PSTRTC(3)=R1
        DETF=PSTRTC(1)*PSTRTC(2)
      ELSEIF(NTYPE.EQ.3)THEN
        PSTRTC(3)=SQRT(B(4))
        DETF=PSTRTC(1)*PSTRTC(2)*PSTRTC(3)
      ENDIF
C Recover principal Kirchhoff stresses (from the given Cauchy stress)
      PSTRES(1)=(STRES(1)*EIGPRJ(1,1)+STRES(2)*EIGPRJ(2,1)+
     1                             R2*STRES(3)*EIGPRJ(3,1))*DETF
      PSTRES(2)=(STRES(1)*EIGPRJ(1,2)+STRES(2)*EIGPRJ(2,2)+
     1                             R2*STRES(3)*EIGPRJ(3,2))*DETF
      IF(NTYPE.EQ.2.OR.NTYPE.EQ.3)PSTRES(3)=STRES(4)*DETF
C Compute derivatives of principal Kirchhoff stresses
C ===================================================
      CALL RVZERO(DPSTRE,9)
      IF(NTYPE.EQ.1) THEN
C Plane stress: Perfectly incompressibility assumed
C -------------------------------------------------
        NSTRA=2
        DO 10 IP=1,NOGTRM
          ALPHA=RPROPS(IPOGDC+IP*2-1)
          ALPHMU=ALPHA*RPROPS(IPOGDC+IP*2-2)
          PSTALP(1)=PSTRTC(1)**ALPHA
          PSTALP(2)=PSTRTC(2)**ALPHA
          FACTOR=R1/(PSTALP(1)*PSTALP(2))
          DO I=1,NSTRA 
            DO J=1,NSTRA
              DPSTRE(I,J)=DPSTRE(I,J)+ALPHMU/(R2*PSTRTC(J)**2)*
     1                    (PSTALP(J)*DELTA(I,J)+FACTOR)
            END DO
          END DO
   10   CONTINUE
      ELSE IF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
C Plane strain and axisymmetric: Regularised Ogden model
C ------------------------------------------------------
C compute principal Kirchhoff stresses derivatives
        R1D3=R1/R3
        IF(NTYPE.EQ.2)THEN
          NSTRA=2
        ELSEIF(NTYPE.EQ.3)THEN
          NSTRA=3
        ENDIF
        DO 40 IP=1,NOGTRM
          CMU=RPROPS(IPOGDC-1+IP*2-1)
          ALPHA=RPROPS(IPOGDC-1+IP*2)
          PSTALP(1)=PSTRTC(1)**ALPHA
          PSTALP(2)=PSTRTC(2)**ALPHA
          PSTALP(3)=PSTRTC(3)**ALPHA
          ALPHMU=ALPHA*CMU
          FACTOR=R1D3*(PSTALP(1)+PSTALP(2)+PSTALP(3))
          FACVOL=DETF**(-ALPHA*R1D3)
          DO 30 I=1,NSTRA
            DO 20 J=1,NSTRA
              DPSTRE(I,J)=DPSTRE(I,J)+ALPHMU*FACVOL/(R6*PSTRTC(J)**2)*
     1                    (FACTOR-PSTALP(I)-PSTALP(J)+R3*PSTALP(I)*
     2                    DELTA(I,J))
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
        DO 60 I=1,NSTRA
          DO 50 J=1,NSTRA
            DPSTRE(I,J)=DPSTRE(I,J)+BULK/(R2*PSTRTC(J)**2)
   50     CONTINUE
   60   CONTINUE
      ENDIF
C Compute the derivative of the Kirchhoff stress with respect to B
C (use routine for computation of derivative of general isotropic
C tensor functions of one tensor)
C ================================================================
      IF(NTYPE.EQ.3)THEN
        OUTOFP=.TRUE.
        NADIM=5
      ELSE
        OUTOFP=.FALSE.
        NADIM=4
      ENDIF
      CALL DGISO2
     1(   DPSTRE     ,DTAUDB     ,EIGPRJ     ,EIGB       ,PSTRES     ,
     2    OUTOFP     ,REPEAT     )
C Assemble the spatial tangent modulus 'a'
C ========================================
      R2DDET=R2/DETF
C upper triangle and diagonal terms
      AMATX(1,1)=R2DDET*(DTAUDB(1,1)*B(1)+DTAUDB(1,3)*B(3))-STRES(1)
      AMATX(1,2)=R2DDET*(DTAUDB(1,3)*B(1)+DTAUDB(1,2)*B(3))
      AMATX(1,3)=R2DDET*(DTAUDB(1,1)*B(3)+DTAUDB(1,3)*B(2))-STRES(3)
      AMATX(1,4)=R2DDET*(DTAUDB(1,3)*B(3)+DTAUDB(1,2)*B(2))
      AMATX(2,2)=R2DDET*(DTAUDB(3,3)*B(1)+DTAUDB(3,2)*B(3))
      AMATX(2,3)=R2DDET*(DTAUDB(3,1)*B(3)+DTAUDB(3,3)*B(2))-STRES(2)
      AMATX(2,4)=R2DDET*(DTAUDB(3,3)*B(3)+DTAUDB(3,2)*B(2))
      AMATX(3,3)=R2DDET*(DTAUDB(3,1)*B(3)+DTAUDB(3,3)*B(2))
      AMATX(3,4)=R2DDET*(DTAUDB(3,3)*B(3)+DTAUDB(3,2)*B(2))-STRES(3)
      AMATX(4,4)=R2DDET*(DTAUDB(2,3)*B(3)+DTAUDB(2,2)*B(2))-STRES(2)
      IF(NTYPE.EQ.3) THEN
        AMATX(1,5)=R2DDET*DTAUDB(1,4)*B(4)
        AMATX(2,5)=R2DDET*DTAUDB(3,4)*B(4)
        AMATX(3,5)=R2DDET*DTAUDB(3,4)*B(4)
        AMATX(4,5)=R2DDET*DTAUDB(2,4)*B(4)
        AMATX(5,5)=R2DDET*DTAUDB(4,4)*B(4)-STRES(4)
      ENDIF
C lower triangle
      DO 80 J=1,NADIM
        DO 70 I=J+1,NADIM
          AMATX(I,J)=AMATX(J,I)
 70     CONTINUE
 80   CONTINUE
C
      RETURN
      END
