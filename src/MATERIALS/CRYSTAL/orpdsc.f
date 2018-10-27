      SUBROUTINE ORPDSC
     1(   DGAM       ,NOUTF      ,NTYPE      ,RPROPS     ,RSTAVA     ,
     2    STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   IPHARD=6   ,NDIM=2     ,NRSTAV=5   ,NSYST=4    )
C Arguments
      DIMENSION
     1    DGAM(NSYST)        ,RPROPS(*)          ,RSTAVA(NRSTAV)     ,
     2    STRES(*)
C Local arrays and variables
      DIMENSION
     1    AUX1(NDIM,NDIM)    ,DKIRCH(NDIM,NDIM)  ,FE(NDIM,NDIM)      ,
     2    RE(NDIM,NDIM)      ,ROTSTR(NDIM,NDIM)  ,SCHMID(NSYST)      ,
     3    UE(NDIM,NDIM)      ,VECM0(NDIM,NSYST)  ,VECS0(NDIM,NSYST)
      DATA R0   ,R1   ,R3   ,R180   /
     1     0.0D0,1.0D0,3.0D0,180.0D0/
C***********************************************************************
C OUTPUT RESULTS (INTERNAL AND ALGORITHMIC VARIABLES) FOR THE PLANAR
C DOUBLE-SLIP SINGLE CRYSTAL ELASTO-PLASTIC MODEL
C***********************************************************************
 1000 FORMAT(' Lattice rotation (degrees) = ',G12.4/
     1       ' Accumulated plastic slip   = ',G12.4/
     2       ' tau_1 = ',G12.4,' tau_2 = ',G12.4,' tau_3 = ',G12.4,
     3       ' tau_4 = ',G12.4/
     4       ' dlmd1 = ',G12.4,' dlmd2 = ',G12.4,' dlmd3 = ',G12.4,
     5       ' dlmd4 = ',G12.4)
C Stops program if not plane strain
      IF(NTYPE.NE.2)CALL ERRPRT('EI0037')
C Retrieve stored state variables
C -------------------------------
C... current elastic deformation gradient
      FE(1,1)=RSTAVA(1)
      FE(2,1)=RSTAVA(2)
      FE(1,2)=RSTAVA(3)
      FE(2,2)=RSTAVA(4)
C... current Taylor hardening variable (acummulated slip)
      HRVAR=RSTAVA(NRSTAV)
C Compute lattice rotation
C ------------------------
C Perform polar decomposition of the elastic deformation gradient
      CALL PODEC2
     1(   FE         ,RE         ,UE         )
C From the elastic rotation tensor, compute crystal lattice rotation
      SINE=RE(2,1)
      IF(SINE.GT.R1)SINE=R1
      IF(SINE.LT.-R1)SINE=-R1
      COSINE=RE(1,1)
      IF(COSINE.GT.R1)COSINE=R1
      IF(COSINE.LT.-R1)COSINE=-R1
      DEGRAD=R180/ACOS(-R1)
      SANGLE=DEGRAD*ASIN(SINE)
      CANGLE=DEGRAD*ACOS(COSINE)
      IF(SINE.GE.R0)THEN
        CLROT=CANGLE
      ELSEIF(SINE.LT.R0.AND.COSINE.LT.R0)THEN
        CLROT=-CANGLE
      ELSE
        CLROT=SANGLE
      ENDIF
C Evaluate resolved Schmid stresses (these are not stored in memory)
C ------------------------------------------------------------------
C Set up initial slip systems vectors
C... retrieve initial system orientation
      THETA=RPROPS(4)
C... retrieve relative angle between systems
      BETA=RPROPS(5)
C... system 1:
      VECS0(1,1)=COS(THETA)
      VECS0(2,1)=SIN(THETA)
      VECM0(1,1)=-SIN(THETA)
      VECM0(2,1)=COS(THETA)
C... system 2:
      VECS0(1,2)=COS(THETA+BETA)
      VECS0(2,2)=SIN(THETA+BETA)
      VECM0(1,2)=-SIN(THETA+BETA)
      VECM0(2,2)=COS(THETA+BETA)
C... system 3:
      VECS0(1,3)=-VECS0(1,1)
      VECS0(2,3)=-VECS0(2,1)
      VECM0(1,3)=VECM0(1,1)
      VECM0(2,3)=VECM0(2,1)
C... system 4:
      VECS0(1,4)=-VECS0(1,2)
      VECS0(2,4)=-VECS0(2,2)
      VECM0(1,4)=VECM0(1,2)
      VECM0(2,4)=VECM0(2,2)
C compute rotated stress tensor
      DETF=FE(1,1)*FE(2,2)-FE(1,2)*FE(2,1)
C... deviatoric Kirchhoff stress
      PKIRCH=R1/R3*DETF*(STRES(1)+STRES(2)+STRES(4))
      DKIRCH(1,1)=DETF*STRES(1)-PKIRCH
      DKIRCH(2,2)=DETF*STRES(2)-PKIRCH
      DKIRCH(1,2)=DETF*STRES(3)
      DKIRCH(2,1)=DKIRCH(1,2)
C... rotated deviatoric Kirchhoff stress
      CALL RVZERO(AUX1,NDIM*NDIM)
      DO 30 I=1,NDIM
        DO 20 J=1,NDIM
          DO 10 K=1,NDIM
            AUX1(I,J)=AUX1(I,J)+RE(K,I)*DKIRCH(K,J)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      CALL RVZERO(ROTSTR,NDIM*NDIM)
      DO 60 I=1,NDIM
        DO 50 J=1,NDIM
          DO 40 K=1,NDIM
            ROTSTR(I,J)=ROTSTR(I,J)+AUX1(I,K)*RE(K,J)
   40     CONTINUE
   50   CONTINUE
   60 CONTINUE
C Current Schmid resolved stresses
      CALL RVZERO(SCHMID,NSYST)
      DO 90 ISYST=1,NSYST
        DO 80 I=1,NDIM
          DO 70 J=1,NDIM
            SCHMID(ISYST)=SCHMID(ISYST)+
     1                    ROTSTR(I,J)*VECS0(I,ISYST)*VECM0(J,ISYST)
   70     CONTINUE
   80   CONTINUE
   90 CONTINUE
C
C Write results to output file
C ----------------------------
      WRITE(NOUTF,1000)CLROT,HRVAR,SCHMID(1),SCHMID(2),SCHMID(3),
     1                 SCHMID(4),DGAM(1),DGAM(2),DGAM(3),DGAM(4)
C
      RETURN
      END
