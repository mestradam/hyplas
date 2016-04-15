      subroutine ctcomp
     &( ralgva      ,dmatx      ,lalgva     ,iprops     ,ntype      ,
     &  rprops      ,rstava     ,stres      ,strat      )
!***********************************************************************
! Computation of the consistent tangent modulus for composite material
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!   ralgva  () :
!   dmatx   () :
!   epflag  () :
!   iprops  () :
!   ntype   () :
!   rprops  () :
!   rstava  () :
!   stresk  () :
!***********************************************************************
!
! Variable declaration
      implicit double precision (a-h, o-z)
!
! ... arguments
      logical
     &  lalgva(*)
      integer
     &  iprops(*)   ,ntype
      real(kind=8)
     &  ralgva(*)   ,rprops(*)  ,rstava(6)  ,stres(4)   ,dmatx(4,4)
!
! ... local variables
      logical
     &  epflag      ,damflag
      integer
     &  nstre
      real(kind=8)
     &  youngm      ,poissm     ,youngf     ,poissf     ,sigmauf    ,
     &  alphaf      ,betaf      ,volfrf     ,anglef     ,ee(4)      ,
     &  eef         ,mx         ,my         ,damage     ,dmatxf(3,3),
     &  rpropst(30)
      real(kind=8)
     &  devprj(4,4),foid(4,4)  ,s(4)        ,vecn(3)    ,soid(4)
      data
     1    foid(1,1),foid(1,2),foid(1,3),foid(1,4)/
     2    1.0d0    ,0.0d0    ,0.0d0    ,0.0d0    /
     3    foid(2,1),foid(2,2),foid(2,3),foid(2,4)/
     4    0.0d0    ,1.0d0    ,0.0d0    ,0.0d0    /
     5    foid(3,1),foid(3,2),foid(3,3),foid(3,4)/
     6    0.0d0    ,0.0d0    ,0.5d0    ,0.0d0    /
     7    foid(4,1),foid(4,2),foid(4,3),foid(4,4)/
     8    0.0d0    ,0.0d0    ,0.0d0    ,1.0d0    /
      data
     1    soid(1)  ,soid(2)  ,soid(3)  ,soid(4)  /
     2    1.0d0    ,1.0d0    ,0.0d0    ,1.0d0    /
      data
     1    rp5  ,r1   ,r2   ,r3   ,r4   ,r6   /
     2    0.5d0,1.0d0,2.0d0,3.0d0,4.0d0,6.0d0/
!
!
! ======================================================================
! Start tangent modulus computation for the LIGNIN MATRIX (Von Mises ps)
! ======================================================================
 100  continue
!
C Stops program if neither plane strain nor axisymmetric state
      if(ntype.ne.2 .and. ntype.ne.3)call ERRPRT('EI0030')
      nstre = 3
      iphard = 21
C Current accumulated plastic strain
      EPBAR=RSTAVA(5)
C Set material properties
      YOUNG=RPROPS(1)
      POISS=RPROPS(2)
      NHARD=IPROPS(3)
      epflag = lalgva(1)
      dgama = ralgva(1)
C Shear and bulk moduli
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R1D3=R1/R3
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
C Matrix E components
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
C Vector n
        VECN(1)=EP11*STRES(1)+EP12*STRES(2)
        VECN(2)=EP21*STRES(1)+EP22*STRES(2)
        VECN(3)=EP33*STRES(3)
C Scalar alpha
        DENOM1=STRES(1)*(R2D3*VECN(1)-R1D3*VECN(2))+
     1         STRES(2)*(R2D3*VECN(2)-R1D3*VECN(1))+
     2         STRES(3)*R2*VECN(3)
        DENOM2=R2*XI*HSLOPE/(R3-R2*HSLOPE*DGAMA)
        ALPHA=R1/(DENOM1+DENOM2)
C Item (ii): Assemble elasto-plastic tangent
C ------------------------------------------
        DMATX(1,1)=E11-ALPHA*VECN(1)*VECN(1)
        DMATX(1,2)=E12-ALPHA*VECN(1)*VECN(2)
        DMATX(1,3)=-ALPHA*VECN(1)*VECN(3)
        DMATX(2,1)=DMATX(1,2)
        DMATX(2,2)=E22-ALPHA*VECN(2)*VECN(2)
        DMATX(2,3)=-ALPHA*VECN(2)*VECN(3)
        DMATX(3,1)=DMATX(1,3)
        DMATX(3,2)=DMATX(2,3)
        DMATX(3,3)=E33-ALPHA*VECN(3)*VECN(3)
      ELSE
C Compute plane stress elasticity matrix
C ======================================
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
      goto 200
!
!
! ===================================================================
! Start tangent modulus computation for the LIGNIN MATRIX (Von Mises)
! ===================================================================
!
 101  continue
C Stops program if neither plane strain nor axisymmetric state
      IF(NTYPE.NE.2.AND.NTYPE.NE.3)CALL ERRPRT('EI0030')
C Current accumulated plastic strain
      EPBAR=RSTAVA(MSTRE+1)
C Set material properties
      YOUNG=RPROPS(2)
      POISS=RPROPS(3)
      NHARD=IPROPS(3)
C Shear and bulk moduli
      GMODU=YOUNG/(R2*(R1+POISS))
      BULK=YOUNG/(R3*(R1-R2*POISS))
      R2G=R2*GMODU
      R1D3=R1/R3
C Set deviatoric projection tensor
      IF(NTYPE.EQ.2)THEN
        NSTRE=3
      ELSEIF(NTYPE.EQ.3)THEN
        NSTRE=4
      ENDIF
      DO 21 I=1,NSTRE
        DO 11 J=1,NSTRE
          DEVPRJ(I,J)=FOID(I,J)-SOID(I)*SOID(J)*R1D3
   11   CONTINUE
   21 CONTINUE
      IF(EPFLAG)THEN
C Compute elastoplastic consistent tangent
C ----------------------------------------
        R3G=R3*GMODU
        ROO3D2=SQRT(R3/R2)
C Hydrostatic pressure
        P=(STRES(1)+STRES(2)+STRES(4))*R1D3
C Deviatoric stress components
        S(1)=STRES(1)-P
        S(2)=STRES(2)-P
        S(3)=STRES(3)
        S(4)=STRES(4)-P
C Recover last elastic trial von Mises effective stress
        SNORM=SQRT(S(1)*S(1)+S(2)*S(2)+R2*S(3)*S(3)+S(4)*S(4))
        Q=ROO3D2*SNORM
        QTRIAL=Q+R3G*DGAMA
C Assemble elastoplastic tangent (upper triangle only)
        AFACT=R2G*(R1-R3G*DGAMA/QTRIAL)
        BFACT=R6*GMODU*GMODU*(DGAMA/QTRIAL-
     1        R1/(R3G+DPLFUN(EPBAR,NHARD,RPROPS(IPHARD))))/
     2        (SNORM*SNORM)
        DO 41 I=1,NSTRE
          DO 31 J=I,NSTRE
            DMATX(I,J)=AFACT*DEVPRJ(I,J)+BFACT*S(I)*S(J)+
     1                 BULK*SOID(I)*SOID(J)
   31     CONTINUE       
   41   CONTINUE
      ELSE
C Compute elasticity matrix (upper triangle only)
C -----------------------------------------------
        DO 61 I=1,NSTRE
          DO 51 J=I,NSTRE
            DMATX(I,J)=R2G*DEVPRJ(I,J)+BULK*SOID(I)*SOID(J)
   51     CONTINUE       
   61   CONTINUE
      ENDIF
C Assemble lower triangle
C -----------------------
      DO 81 J=1,NSTRE-1
        DO 71 I=J+1,NSTRE
          DMATX(I,J)=DMATX(J,I)
   71   CONTINUE
   81 CONTINUE
      goto 200
!
!
! =================================================================
! Start tangent modulus computation for the FIBERS (Damage Weibull)
! =================================================================
 200  continue
!
! Initialize algorithmic and internal variables
      damflag = lalgva(2)
! ... set previously accumulated damage variable
      damage = rstava(6)
!
! Set some material properties
      youngf = rprops(11)
      poissf = rprops(12)
      alphaf = rprops(14)
      betaf = rprops(15)
      anglef = rprops(17)
      mx = dcos(anglef)
      my = dsin(anglef)
!
      if (.not.damflag) then
! Compute elastic modulus matrix
! ------------------------------
          dmatxf(1,1) = youngf * mx**4
          dmatxf(1,2) = youngf * mx**2 * my**2
          dmatxf(1,3) = youngf * mx**3 * my
          dmatxf(2,1) = youngf * my**2 * mx**2
          dmatxf(2,2) = youngf * my**4
          dmatxf(2,3) = youngf * my**3 * mx
          dmatxf(3,1) = youngf * mx**3 * my
          dmatxf(3,2) = youngf * my**3 * mx
          dmatxf(3,3) = youngf * mx**2 * my**2
      else
! Compute tangent modulus matrix
! ------------------------------
!
! Elastic strain
          ee(1) = strat(1)
          ee(2) = strat(2)
!
! Convert engineering shear component into physical component
          ee(3) = (strat(3))/2.d0
!
! Strain component parallel to fibers direction "anglef"
          eef = ee(1)*mx**2+ee(2)*my**2 + 2.d0*ee(3)*mx*my
!
! Effective stress on fibers
          streff = youngf*eef
!
! Compute tangent axial modulus
          factor = (1.d0-damage)*(1.d0-alpha*(streff/beta)**alpha)
          youngft = youngf*factor
!
! Transform axial modulus to global coordinate system
          dmatxf(1,1) = youngft * mx**4
          dmatxf(1,2) = youngft * mx**2 * my**2
          dmatxf(1,3) = youngft * mx**3 * my
          dmatxf(2,1) = youngft * my**2 * mx**2
          dmatxf(2,2) = youngft * my**4
          dmatxf(2,3) = youngft * my**3 * mx
          dmatxf(3,1) = youngft * mx**3 * my
          dmatxf(3,2) = youngft * my**3 * mx
          dmatxf(3,3) = youngft * mx**2 * my**2
      end if
!
! =============================================
! Compute COMPOSITE tangent constitutive matrix
! =============================================
!
! Initialize some variables
      volfrf = rprops(16)
      volfrm = 1.d0-volfr
!
! Homogenization by mixture law
      do i=1, nstre
          do j=1, nstre
              dmatx(i,j) = volfrm*dmatx(i,j) + volfrf*dmatxf(i,j)
          end do
      end do
      RETURN
      END
