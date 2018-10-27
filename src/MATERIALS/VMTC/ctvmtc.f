      SUBROUTINE CTVMTC
     1(   DGAMA      ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(IPHARD=5  ,MSTRE=4)
      LOGICAL EPFLAG
C Array arguments
      DIMENSION
     1    DMATX(MSTRE,MSTRE),IPROPS(*)           ,RPROPS(*)          ,
     2    RSTAVA(MSTRE+1)   ,STRES(MSTRE)
C Local arrays
      DIMENSION
     1    FOID(MSTRE,MSTRE)  ,SOID(MSTRE)        ,VECN(3)            ,
     2    PSMAT(3)           ,AMAT(3)            ,EMATX(3,3)         ,
     3    PSTRS(3)           ,STPRIN(2)
      DATA
     1    FOID(1,1),FOID(1,2),FOID(1,3),FOID(1,4)/
     2    1.0D0    ,0.0D0    ,0.0D0    ,0.0D0    /
     3    FOID(2,1),FOID(2,2),FOID(2,3),FOID(2,4)/
     4    0.0D0    ,1.0D0    ,0.0D0    ,0.0D0    /
     5    FOID(3,1),FOID(3,2),FOID(3,3),FOID(3,4)/
     6    0.0D0    ,0.0D0    ,0.5D0    ,0.0D0    /
     7    FOID(4,1),FOID(4,2),FOID(4,3),FOID(4,4)/
     8    0.0D0    ,0.0D0    ,0.0D0    ,1.0D0    /
      DATA
     1    SOID(1)  ,SOID(2)  ,SOID(3)  ,SOID(4)  /
     2    1.0D0    ,1.0D0    ,0.0D0    ,1.0D0    /
      DATA
     1    R0   ,RP5  ,R1   ,R2   ,R3   ,R4   /
     2    0.0D0,0.5D0,1.0D0,2.0D0,3.0D0,4.0D0/
C***********************************************************************
C COMPUTATION OF THE CONSISTENT TANGENT MODULUS FOR VON MISES TYPE
C ELASTO-PLASTIC MATERIAL WITH PIECE-WISE LINEAR ISOTROPIC HARDENING WITH
C DIFFERENCED THRESHOLD (T/C).
C PLANE STRESS IMPLEMENTATION ONLY.
C
C REFERENCE: LINERO D.L. (2010)
C***********************************************************************
C Stops program if neither not plane stress
      IF(NTYPE.NE.1)CALL ERRPRT('EI0032')
C Current accumulated plastic strain
      EPBAR=RSTAVA(MSTRE+1)
C Set material properties
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      FACTCT=RPROPS(4)
      NHARD=IPROPS(3)
C Shear and bulk moduli
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R1D3=R1/R3
      R2D3=R2*R1D3
C Using the Scheme of damage model of Oliver X. & Cervera M.
C Compute the principal streses for cheking the hardening threshold
      CALL PRINC2(PSTRS,STRES)
      DO 50 NPRINC=1,2
      IF (PSTRS(NPRINC).LE.R0)THEN
        STPRIN(NPRINC)=R0
      ELSE
        STPRIN(NPRINC)=PSTRS(NPRINC)
      ENDIF
   50  CONTINUE
      PFACT=(STPRIN(1)+STPRIN(2))/(ABS(STRES(1))+ABS(STRES(2)))
      PFACTT=PFACT*(R1-(R1/FACTCT))+(R1/FACTCT)
      IF(EPFLAG)THEN
C Compute elastoplastic consistent tangent (Box 9.6)
C ==================================================
C Item (i):
C ---------
C Compute XI
        XI=R2D3*(STRES(1)*STRES(1)+STRES(2)*STRES(2)-STRES(1)*STRES(2))+
     1     R2*STRES(3)*STRES(3)
C Hardening slope
        HSLOPE=DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))
C Matrix E' components (note: there's not the elastical constitutive fourth order tensor)
C Matrix E' components
        ESTAR1=R3*YOUNG/(R3*(R1-POISS)+YOUNG*DGAMA)
        ESTAR2=R2G/(R1+R2G*DGAMA)
        ESTAR3=GMODU/(R1+R2G*DGAMA)
        E11=RP5*(ESTAR1+ESTAR2)
        E22=E11
        E12=RP5*(ESTAR1-ESTAR2)
        E33=ESTAR3
C Components of the matrix product EP
        EPSTA1=R1D3*ESTAR1
        EPSTA2=ESTAR2
        EPSTA3=EPSTA2
        EP11=RP5*(EPSTA1+EPSTA2)
        EP22=EP11
        EP12=RP5*(EPSTA1-EPSTA2)
        EP21=EP12
        EP33=EPSTA3
C Vector n = E*P*sigma(n+1)
        VECN(1)=EP11*STRES(1)+EP12*STRES(2)
        VECN(2)=EP21*STRES(1)+EP22*STRES(2)
        VECN(3)=EP33*STRES(3)
C Scalar beta
        BETA=2*(RP5*PFACTT-R1D3*HSLOPE*DGAMA)
C Scalar alpha'
        DENOM1=BETA*(STRES(1)*(R2D3*VECN(1)-R1D3*VECN(2))+
     1         STRES(2)*(R2D3*VECN(2)-R1D3*VECN(1))+
     2         STRES(3)*R2*VECN(3))
        DENOM2=R2*XI*HSLOPE/(R3)
        ALPHA=R1/(DENOM1+DENOM2)
C Second order tensor A
C ---------------------
C Estimation of Second order tensor d(phi)/d(sigma)
        SIGMAR=((STRES(1)-STRES(2))/R2)**2+(STRES(3))**2
        SIGR=SQRT(SIGMAR)
        SFACT=((STRES(1)-STRES(2))/(R2*SIGR))*
     1        ((STRES(1)+STRES(2))/(R2*SIGR))
        PSMAT(1)=(R1-(R1/FACTCT))*(R1/(R4*SIGR))*(R1-SFACT)
        PSMAT(2)=(R1-(R1/FACTCT))*(R1/(R4*SIGR))*(R1+SFACT)
        PSMAT(3)=-(R1-(R1/FACTCT))*(STRES(3)/(R4*SIGMAR))*
     1           ((STRES(1)+STRES(2))/R2)
C Estimation of fourth order constitutive elastic tensor E
C Plane stress tensor
        FACTOR=(BULK-R2G/R3)*(R1/(BULK+R4GD3))
        EMATX(1,1)=R2G*(R1+FACTOR)
        EMATX(1,2)=R2G*FACTOR
        EMATX(2,1)=EMATX(1,2)
        EMATX(2,2)=EMATX(1,1)
        EMATX(3,3)=R2G*RP5
        EMATX(1,3)=R0
        EMATX(2,3)=R0
        EMATX(3,1)=EMATX(1,3)
        EMATX(3,2)=EMATX(2,3)
C Estimation of second order tensor A=E:d(phi)/d(sigma)
        AMAT(1)=PSMAT(1)*EMATX(1,1)+PSMAT(2)*EMATX(2,1)
        AMAT(2)=PSMAT(1)*EMATX(1,2)+PSMAT(2)*EMATX(2,2)
        AMAT(3)=PSMAT(3)*EMATX(3,3)
C Item (ii): Assemble elasto-plastic tangent
C ------------------------------------------
C    FACTA=(PFACTT*ALPHA*XI+R2*DGAMA)/R2
C       DMATX(1,1)=E11-PFACTT*ALPHA*BETA*VECN(1)*VECN(1)-
C    1             FACTA*VECN(1)*AMAT(1)
C       DMATX(1,2)=E12-PFACTT*ALPHA*BETA*VECN(1)*VECN(2)-
C  1             FACTA*VECN(1)*AMAT(2)
C       DMATX(1,3)=-PFACTT*ALPHA*BETA*VECN(1)*VECN(3)-
C  1             FACTA*VECN(1)*AMAT(3)
C       DMATX(2,1)=E12-PFACTT*ALPHA*BETA*VECN(1)*VECN(2)-
C  1             FACTA*VECN(2)*AMAT(1)
C       DMATX(2,2)=E22-PFACTT*ALPHA*BETA*VECN(2)*VECN(2)-
C  1             FACTA*VECN(2)*AMAT(2)
C       DMATX(2,3)=-PFACTT*ALPHA*BETA*VECN(2)*VECN(3)-
C  1             FACTA*VECN(2)*AMAT(3)
C       DMATX(3,1)=-PFACTT*ALPHA*BETA*VECN(1)*VECN(3)-
C  1             FACTA*VECN(3)*AMAT(1)
C       DMATX(3,2)=-PFACTT*ALPHA*BETA*VECN(2)*VECN(3)-
C  1             FACTA*VECN(3)*AMAT(2)
C       DMATX(3,3)=E33-PFACTT*ALPHA*BETA*VECN(3)*VECN(3)-
C  1             FACTA*VECN(3)*AMAT(3)
C      
        FACTA=(PFACTT*ALPHA*XI+R2*DGAMA)/R2
        DMATX(1,1)=E11-PFACTT*ALPHA*BETA*VECN(1)*VECN(1)
        DMATX(1,2)=E12-PFACTT*ALPHA*BETA*VECN(1)*VECN(2)
        DMATX(1,3)=-PFACTT*ALPHA*BETA*VECN(1)*VECN(3)
        DMATX(2,1)=E12-PFACTT*ALPHA*BETA*VECN(1)*VECN(2)
        DMATX(2,2)=E22-PFACTT*ALPHA*BETA*VECN(2)*VECN(2)
        DMATX(2,3)=-PFACTT*ALPHA*BETA*VECN(2)*VECN(3)
        DMATX(3,1)=-PFACTT*ALPHA*BETA*VECN(1)*VECN(3)
        DMATX(3,2)=-PFACTT*ALPHA*BETA*VECN(2)*VECN(3)
        DMATX(3,3)=E33-PFACTT*ALPHA*BETA*VECN(3)*VECN(3)  
      ELSE
C Compute plane stress elasticity matrix
C ======================================
        NSTRE=3
        R4GD3=R4*GMODU/R3
        FACTOR=(BULK-R2G/R3)*(R2G/(BULK+R4GD3))
        DO 20 I=1,NSTRE
          DO 10 J=I,NSTRE
            DMATX(I,J)=R2G*FOID(I,J)+FACTOR*SOID(I)*SOID(J)
   10     CONTINUE
   20   CONTINUE
C lower triangle
        DO 40 J=1,NSTRE-1
          DO 30 I=J+1,NSTRE
            DMATX(I,J)=DMATX(J,I)
   30     CONTINUE
   40   CONTINUE
      ENDIF
      RETURN
      END
