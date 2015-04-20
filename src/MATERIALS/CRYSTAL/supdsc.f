      SUBROUTINE SUPDSC
     1(   DGAM       ,FINCR      ,IPROPS     ,LALGVA     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   IPHARD=6   ,IPHVAR=5   ,MTRIAL=5   ,NDIM=2     ,NIPROP=3   ,
     2    NLALGV=6   ,NRALGV=4   ,NRSTAV=5   ,NSTRE=4    ,NSYST=4    )
C Arguments
      LOGICAL
     1    LALGVA(NLALGV)
      DIMENSION
     1    DGAM(NRALGV)       ,FINCR(3,3)         ,IPROPS(NIPROP)     ,
     2    RPROPS(*)          ,RSTAVA(NRSTAV)     ,STRES(NSTRE)
C Local arrays and variables
      LOGICAL
     1    IFPLAS ,NOCONV ,SUFAIL ,S1ACT  ,S2ACT  ,S3ACT  ,S4ACT
      DIMENSION
     1    BEDEV(4)           ,BEISO(3,3)      ,BMATX(NDIM,NDIM,NSYST),
     2    DDGAM(NSYST)       ,DEREXP(3,3,3,3)    ,DS0M0(NDIM,NDIM)   ,
     3    FEISO(2,2)         ,FEN(2,2)           ,FETISO(2,2)        ,
     4    FETRL(2,2)         ,FPILOG(3,3)        ,FPINCI(3,3)        ,
     5    GINV(NSYST,NSYST)  ,GMATX(NSYST,NSYST),IACSET(NSYST,MTRIAL),
     6    IPACT(0:5)         ,NACSYS(MTRIAL)     ,PHI(NSYST)         ,
     7    SCHMID(NSYST)      ,SM0MS0(NDIM,NDIM,NSYST)                ,
     8    S0M0(NDIM,NDIM,NSYST),VECM(NDIM,NSYST) ,VECM0(NDIM,NSYST)  ,
     9    VECS(NDIM,NSYST)   ,VECS0(NDIM,NSYST)
      DATA
     1    IPACT(0) ,IPACT(1) ,IPACT(2) ,IPACT(3) ,IPACT(4) ,IPACT(5) /
     2    4        ,1        ,2        ,3        ,4        ,1        /
      DATA
     1    R0   ,R1   ,R3   ,SMALL ,TOL   / 
     2    0.0D0,1.0D0,3.0D0,1.D-10,1.D-08/
      DATA MXITER / 50 /
C***********************************************************************
C STRESS UPDATE PROCEDURE FOR THE ANISOTROPIC PLANAR DOUBLE-SLIP SINGLE
C CRYSTAL ELASTO-PLASTIC MODEL WITH PIECE-WISE LINEAR TAYLOR ISOTROPIC
C HARDENING:
C MULTI-SURFACE TYPE IMPLICIT ELASTIC PREDICTOR/RETURN MAPPING ALGORITHM
C BASED ON THE EXPONENTIAL MAP APPROXIMATION OF THE PLASTIC FLOW RULE.
C
C REFERENCE: Section 16.6.2
C            Boxes 16.2-3
C***********************************************************************
C Stops program if not plane strain
      IF(NTYPE.NE.2)CALL ERRPRT('EI0036')
C Initialize some algorithmic and internal variables
      CALL RVZERO(DGAM,NSYST)
      IFPLAS=.FALSE.
      SUFAIL=.FALSE.
      S1ACT=.FALSE.
      S2ACT=.FALSE.
      S3ACT=.FALSE.
      S4ACT=.FALSE.
C... hardening internal variable
      HRVARN=RSTAVA(IPHVAR)
      HRVAR=HRVARN
C... elastic deformation gradient
      FEN(1,1)=RSTAVA(1)
      FEN(2,1)=RSTAVA(2)
      FEN(1,2)=RSTAVA(3)
      FEN(2,2)=RSTAVA(4)
C Retrieve material properties
C... neo-Hookean constants
      GMODU=RPROPS(2)
      BULK=RPROPS(3)
C... initial system orientation
      THETA=RPROPS(4)
C... relative angle between systems
      BETA=RPROPS(5)
C... number of sampling points on hardening curve
      NHARD=IPROPS(3)
C Set up initial slip systems vectors
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
C Set some constants
      R1D3=R1/R3
C Compute elastic trial state
C ---------------------------
C Elastic trial deformation gradient
      CALL RVZERO(FETRL,NDIM*NDIM)
      DO 30 I=1,NDIM
        DO 20 J=1,NDIM
          DO 10 K=1,NDIM
            FETRL(I,J)=FETRL(I,J)+FINCR(I,K)*FEN(K,J)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C Perform isochoric/volumetric split of elastic trial def. grad.
      DETFET=FETRL(1,1)*FETRL(2,2)-FETRL(1,2)*FETRL(2,1)
      VOLFAC=DETFET**(-R1D3)
      FETISO(1,1)=VOLFAC*FETRL(1,1)
      FETISO(2,1)=VOLFAC*FETRL(2,1)
      FETISO(1,2)=VOLFAC*FETRL(1,2)
      FETISO(2,2)=VOLFAC*FETRL(2,2)
C Check plastic consistency
C -------------------------
C Compute yield functions values
C... elastic push forward of slip-systems vectors
      CALL RVZERO(VECS,NDIM*NSYST)
      CALL RVZERO(VECM,NDIM*NSYST)
      DO 60 I=1,NDIM
        DO 50 J=1,NDIM
          DO 40 ISYST=1,NSYST
            VECS(I,ISYST)=VECS(I,ISYST)+FETISO(I,J)*VECS0(J,ISYST)
            VECM(I,ISYST)=VECM(I,ISYST)+FETISO(I,J)*VECM0(J,ISYST)
   40     CONTINUE
   50   CONTINUE
   60 CONTINUE
C... current resolved yield stress
      RYIELD=PLFUN(HRVAR,NHARD,RPROPS(IPHARD))
C... elastic trial Schmid resolved shear stresses
      DO 70 ISYST=1,NSYST
        SCHMID(ISYST)=GMODU*SCAPRD(VECS(1,ISYST),VECM(1,ISYST),2)
   70 CONTINUE
C Check consistency
C... compute yield functions values and determine set of active systems
C    at elastic trial state
      IACSYS=0
      DO 80 ISYST=1,NSYST
        PHI(ISYST)=SCHMID(ISYST)-RYIELD
        IF(PHI(ISYST)/RYIELD.GT.TOL)THEN
          IFPLAS=.TRUE.
          IACSYS=IACSYS+1
          IACSET(IACSYS,1)=ISYST
        ENDIF
   80 CONTINUE
      NACSYS(1)=IACSYS
C... define the other possible tentative sets of active systems
      IF(NACSYS(1).EQ.1)THEN
        NTENT=3
        NACSYS(2)=2
        IACSET(1,2)=IACSET(1,1)
        IACSET(2,2)=IPACT(IACSET(1,1)-1)
        NACSYS(3)=2
        IACSET(1,3)=IACSET(1,1)
        IACSET(2,3)=IPACT(IACSET(1,1)+1)
      ELSEIF(NACSYS(1).EQ.2)THEN
        NTENT=5
        NACSYS(2)=1
        IACSET(1,2)=IACSET(1,1)
        NACSYS(3)=1
        IACSET(1,3)=IACSET(2,1)
        IF(IACSET(1,1).EQ.1.AND.IACSET(2,1).EQ.4)THEN
          NACSYS(4)=2
          IACSET(1,4)=1
          IACSET(2,4)=2
          NACSYS(5)=2
          IACSET(1,5)=3
          IACSET(2,5)=4
        ELSE
          NACSYS(4)=2
          IACSET(1,4)=IACSET(1,1)
          IACSET(2,4)=IPACT(IACSET(1,1)-1)
          NACSYS(5)=2
          IACSET(1,5)=IACSET(2,1)
          IACSET(2,5)=IPACT(IACSET(2,1)+1)
        ENDIF
      ENDIF
      IF(IFPLAS)THEN
C Plastic step: Apply return mapping
C ==================================
C Loop over the tentative sets of active systems
C ----------------------------------------------
        DO 420 ITENT=1,NTENT
C re-set elastic push-forward of slip systems vectors
          CALL RVZERO(VECS,NDIM*NSYST)
          CALL RVZERO(VECM,NDIM*NSYST)
          DO 110 I=1,NDIM
            DO 100 J=1,NDIM
              DO 90 ISYST=1,NSYST
                VECS(I,ISYST)=VECS(I,ISYST)+FETISO(I,J)*VECS0(J,ISYST)
                VECM(I,ISYST)=VECM(I,ISYST)+FETISO(I,J)*VECM0(J,ISYST)
   90         CONTINUE
  100       CONTINUE
  110     CONTINUE
C re-set hardening variable
          HRVAR=HRVARN
C re-set yield function values at trial state
          RYIELD=PLFUN(HRVAR,NHARD,RPROPS(IPHARD))
C... elastic trial Schmid resolved shear stresses
          DO 120 ISYST=1,NSYST
            SCHMID(ISYST)=GMODU*SCAPRD(VECS(1,ISYST),VECM(1,ISYST),NDIM)
            PHI(ISYST)=SCHMID(ISYST)-RYIELD
  120     CONTINUE
C Start Newton-Raphson iterations for plastic multipliers
          CALL RVZERO(DGAM,NSYST)
          DO 410 NRITER=1,MXITER
            HSLOPE=DPLFUN(HRVAR,NHARD,RPROPS(IPHARD))
            IF(NRITER.EQ.1)CALL RVZERO(FPILOG,9) 
            CALL DEXPMP(   DEREXP      ,NOCONV     ,FPILOG     )
            CALL RVZERO(BMATX,NDIM*NDIM*NSYST)
            DO 220 II=1,NACSYS(ITENT)
              ISYST=IACSET(II,ITENT)
              DO 140 I=1,NDIM
                DO 130 J=1,NDIM
                  S0M0(I,J,ISYST)=VECS0(I,ISYST)*VECM0(J,ISYST)
                  SM0MS0(I,J,ISYST)=VECS(I,ISYST)*VECM0(J,ISYST)+
     1                              VECM(I,ISYST)*VECS0(J,ISYST)
  130           CONTINUE
  140         CONTINUE
              CALL RVZERO(DS0M0,NDIM*NDIM)
              DO 180 I=1,NDIM
                DO 170 J=1,NDIM
                  DO 160 K=1,NDIM
                    DO 150 L=1,NDIM
                      DS0M0(I,J)=DS0M0(I,J)+
     1                           DEREXP(I,J,K,L)*S0M0(K,L,ISYST)
  150               CONTINUE
  160             CONTINUE
  170           CONTINUE
  180         CONTINUE
              DO 210 I=1,NDIM
                DO 200 J=1,NDIM
                  DO 190 K=1,NDIM
                    BMATX(I,J,ISYST)=BMATX(I,J,ISYST)+
     1                               FETISO(I,K)*DS0M0(K,J)
  190             CONTINUE
  200           CONTINUE
  210         CONTINUE
  220       CONTINUE
            DO 240 II=1,NACSYS(ITENT)
              ISYST=IACSET(II,ITENT)
C Compute exact jacobian of non-linear system of equations
              DO 230 JJ=1,NACSYS(ITENT)
                JSYST=IACSET(JJ,ITENT)

                GMATX(II,JJ)=GMODU*
     1             SCAPRD(SM0MS0(1,1,ISYST),BMATX(1,1,JSYST),NDIM*NDIM)+
     2             HSLOPE

  230         CONTINUE
  240       CONTINUE
C Invert jacobian: Note that for the double slip model only one or two
C systems may be active
            IF(NACSYS(ITENT).EQ.1)THEN
              IF(GMATX(1,1).LT.SMALL)THEN
C... jacobian is singular: Try another active set or exit if
C                          all possible sets have already been tried
                GOTO 420
              ENDIF
              GINV(1,1)=R1/GMATX(1,1)
            ELSEIF(NACSYS(ITENT).EQ.2)THEN
              DETG=GMATX(1,1)*GMATX(2,2)-GMATX(1,2)*GMATX(2,1)
              IF(DETG.LT.SMALL)THEN
C... jacobian is singular: Try another active set or exit if
C                          all possible sets have already been tried
                GOTO 420
              ENDIF
              DETGIN=R1/DETG
              GINV(1,1)=GMATX(2,2)*DETGIN
              GINV(2,2)=GMATX(1,1)*DETGIN
              GINV(1,2)=-GMATX(1,2)*DETGIN
              GINV(2,1)=-GMATX(2,1)*DETGIN
            ENDIF
C Apply Newton-Raphson correction to plastic multipliers
            CALL RVZERO(DDGAM,NSYST)
            DO 260 II=1,NACSYS(ITENT)
              ISYST=IACSET(II,ITENT)
              DO 250 JJ=1,NACSYS(ITENT)
                JSYST=IACSET(JJ,ITENT)
                DDGAM(ISYST)=DDGAM(ISYST)+GINV(II,JJ)*PHI(JSYST)
  250         CONTINUE
              DGAM(ISYST)=DGAM(ISYST)+DDGAM(ISYST)
  260       CONTINUE
C Compute inverse of incremental plastic deformation gradient
C... sum up contributions from each active slip system
            CALL RVZERO(FPILOG,9) 
            DO 290 II=1,NACSYS(ITENT)
              ISYST=IACSET(II,ITENT)
              DO 280 I=1,NDIM
                DO 270 J=1,NDIM
                  FPILOG(I,J)=FPILOG(I,J)-
     1                        DGAM(ISYST)*VECS0(I,ISYST)*VECM0(J,ISYST)
  270           CONTINUE
  280         CONTINUE
  290       CONTINUE
C... use exponential map to update inverse of incremental Fp
            CALL EXPMAP
     1(   FPINCI     ,NOCONV     ,FPILOG     )
            IF(NOCONV)THEN
C... exponential map algorithm failed: Break loop and exit
              SUFAIL=.TRUE.
              CALL ERRPRT('WE0014')
              GOTO 999
            ENDIF
C Update isochoric component of elastic deformation gradient
            CALL RVZERO(FEISO,4)
            DO 320 I=1,NDIM
              DO 310 J=1,NDIM
                DO 300 K=1,NDIM
                  FEISO(I,J)=FEISO(I,J)+FETISO(I,K)*FPINCI(K,J)
  300           CONTINUE
  310         CONTINUE
  320       CONTINUE
C Update hardening internal variable and yield resolved shear stress
            HRVAR=HRVARN
            DO 330 II=1,NACSYS(ITENT)
              ISYST=IACSET(II,ITENT)
              HRVAR=HRVAR+DGAM(ISYST)
  330       CONTINUE
C Compute yield functions values and check for convergence
C... elastic push forward of all slip-systems vectors
            CALL RVZERO(VECS,NDIM*NSYST)
            CALL RVZERO(VECM,NDIM*NSYST)
            DO 360 I=1,NDIM
              DO 350 J=1,NDIM
                DO 340 ISYST=1,NSYST
                  VECS(I,ISYST)=VECS(I,ISYST)+FEISO(I,J)*VECS0(J,ISYST)
                  VECM(I,ISYST)=VECM(I,ISYST)+FEISO(I,J)*VECM0(J,ISYST)
  340           CONTINUE
  350         CONTINUE
  360       CONTINUE
C... update resolved yield stress
            RYIELD=PLFUN(HRVAR,NHARD,RPROPS(IPHARD))
C... Schmid resolved shear stresses for all systems and corresponding
C    yield function values
            DO 370 ISYST=1,NSYST
              SCHMID(ISYST)=GMODU*
     1                      SCAPRD(VECS(1,ISYST),VECM(1,ISYST),NDIM)
              PHI(ISYST)=SCHMID(ISYST)-RYIELD
  370       CONTINUE
C... check for convergence
            RESNOR=R0
            DO 380 II=1,NACSYS(ITENT)
              ISYST=IACSET(II,ITENT)
              RESNOR=RESNOR+ABS(PHI(ISYST))
  380       CONTINUE
            RESNOR=RESNOR/RYIELD
            IF(RESNOR.LE.TOL)THEN
C... N-R loop converged: check validity of current solution
              DO 390 ISYST=1,NSYST
                IF(DGAM(ISYST).LT.R0.OR.
     1                             PHI(ISYST)/RYIELD-TOL.GT.R0)THEN
C... current solution is not valid: Try another active set or exit if
C                                   all possible sets have already been
C                                   tried
                  GOTO 420
                ENDIF
  390         CONTINUE
C... Stress updated converged: Break loop to update necessary variables
C                              and exit
              DO 400 II=1,NACSYS(ITENT)
                ISYST=IACSET(II,ITENT)
                IF(ISYST.EQ.1)S1ACT=.TRUE.
                IF(ISYST.EQ.2)S2ACT=.TRUE.
                IF(ISYST.EQ.3)S3ACT=.TRUE.
                IF(ISYST.EQ.4)S4ACT=.TRUE.
  400         CONTINUE
              GOTO 450
            ENDIF
  410     CONTINUE
  420   CONTINUE
C Stress update procedure failed to converge: Break loop and exit
        SUFAIL=.TRUE.
        CALL ERRPRT('WE0015')
        GOTO 999
      ELSE
C Elastic step: Trial state is the actual one
C ===========================================
        DO 440 I=1,NDIM
          DO 430 J=1,NDIM
            FEISO(I,J)=FETISO(I,J)
  430     CONTINUE
  440   CONTINUE
      ENDIF
C Use neo-Hookean law to update stresses
C ======================================
  450 CONTINUE
C Compute elastic left Cauchy-Green tensor
      CALL RVZERO(BEISO,9)
      DO 480 I=1,NDIM
        DO 470 J=1,NDIM
          DO 460 K=1,NDIM
            BEISO(I,J)=BEISO(I,J)+FEISO(I,K)*FEISO(J,K)
  460     CONTINUE
  470   CONTINUE
  480 CONTINUE
      BEISO(3,3)=VOLFAC*VOLFAC
C Hydrostatic pressure
      P=BULK*LOG(DETFET)
C Deviatoric component of isochoric elastic left Cauchy-Green tensor
      TRACE=BEISO(1,1)+BEISO(2,2)+BEISO(3,3)
      BEDEV(1)=BEISO(1,1)-R1D3*TRACE
      BEDEV(2)=BEISO(2,2)-R1D3*TRACE
      BEDEV(3)=BEISO(1,2)
      BEDEV(4)=BEISO(3,3)-R1D3*TRACE
C Update Cauchy stress components
      DETINV=R1/DETFET
      STRES(1)=(GMODU*BEDEV(1)+P)*DETINV
      STRES(2)=(GMODU*BEDEV(2)+P)*DETINV
      STRES(3)=(GMODU*BEDEV(3))*DETINV
      STRES(4)=(GMODU*BEDEV(4)+P)*DETINV
C Update elastic deformation gradient components
      RSTAVA(1)=FEISO(1,1)/VOLFAC
      RSTAVA(2)=FEISO(2,1)/VOLFAC
      RSTAVA(3)=FEISO(1,2)/VOLFAC
      RSTAVA(4)=FEISO(2,2)/VOLFAC
C Store updated hardening variable
      RSTAVA(5)=HRVAR
  999 CONTINUE
C Update some algorithmic variables before exit
C =============================================
      LALGVA(1)=IFPLAS
      LALGVA(2)=SUFAIL
      IF(.NOT.SUFAIL)THEN
C Update active system flags if state update was successful
        LALGVA(3)=S1ACT
        LALGVA(4)=S2ACT
        LALGVA(5)=S3ACT
        LALGVA(6)=S4ACT
      ENDIF
      RETURN
      END
