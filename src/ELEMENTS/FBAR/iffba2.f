      SUBROUTINE IFFBA2
     1(   IELEM      ,INCCUT     ,MDIME      ,MELEM      ,MPOIN      ,
     2    MSTRE      ,MTOTV      ,NAXIS      ,NTYPE      ,
     3    COORD1     ,DINCR      ,ELOAD      ,IELPRP     ,IPROPS     ,
     4    LALGVA     ,LNODS      ,RALGVA     ,RELPRP     ,RPROPS     ,
     5    RSTAVA     ,STRSG      ,THKGP      ,TDISP      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE '../../ELEMENTS.INC'
      INCLUDE '../../MATERIAL.INC'
C
      PARAMETER( MGDIM=5 ,MBDIM=4 ,NDIME=2 ,NDOFN=2 )
C Arguments
      LOGICAL  INCCUT ,LALGVA
      DIMENSION
     1    COORD1(MDIME,MPOIN),DINCR(MTOTV)       ,ELOAD(MEVAB)       ,
     2    IELPRP(*)          ,IPROPS(*)         ,LALGVA(MLALGV,MTOTG),
     3    LNODS(MELEM,MEVAB),RALGVA(MRALGV,MTOTG),RELPRP(*)          ,
     4    RPROPS(*)          ,RSTAVA(MRSTAV,MTOTG),STRSG(MSTRE,MTOTG),
     5    THKGP(MTOTG)       ,TDISP(MTOTV)
C Local variables and arrays
      LOGICAL  SUFAIL
      DIMENSION
     1    BMATX(MBDIM,MEVAB) ,CARTD(NDIME,MNODE) ,DELDIS(MDOFN,MNODE),
     2    DERIV(NDIME,MNODE) ,EINCR(MBDIM)       ,ELCOD(NDIME,MNODE) ,
     3    FINCIN(3,3)        ,FINCR(3,3)         ,FINV(3,3)          ,
     4    GMATX(MGDIM,MEVAB) ,GPCOD(NDIME)       ,SHAPE(MNODE)       ,
     5    TELDIS(MDOFN,MNODE)
C Local numerical constants
      DATA
     1    R0   ,RP5  ,R1   ,R3   ,R8   /
     2    0.0D0,0.5D0,1.0D0,3.0D0,8.0D0/
C***********************************************************************
C COMPUTE INTERNAL FORCE VECTOR OF ALL ELEMENTS OF CLASS 'FBAR'
C (F-Bar ELEMENTS) IN 2-D: PLANE STRAIN AND AXISYMMETRIC
C
C REFERENCE: Box 15.1
C***********************************************************************
      R1D3=R1/R3
      IF(NTYPE.EQ.2)THEN
        NBDIM=3
      ELSEIF(NTYPE.EQ.3)THEN
        TWOPI=R8*ATAN(R1)
        NBDIM=4
      ELSE
C F-bar implementation valid only for plane strain and axisymmetric
        CALL ERRPRT('EI0033')
      ENDIF
C Identify element type
      IELTYP=IELPRP(1)
C retrieve some element integer properties
      NNODE =IELPRP(3)
      NGAUSP=IELPRP(4)
      NEVAB =IELPRP(5)
C
C Set element arrays of current nodal coordinates, total and incremental
C displacements
      DO 20 INODE =1,NNODE
        LNODE=IABS(LNODS(IELEM,INODE))
        NPOSN=(LNODE-1)*NDOFN
        DO 10 IDOFN=1,NDOFN
          NPOSN=NPOSN+1
          ELCOD(IDOFN,INODE)=COORD1(IDOFN,LNODE)
          TELDIS(IDOFN,INODE)=-TDISP(NPOSN)
          DELDIS(IDOFN,INODE)=-DINCR(NPOSN)
   10   CONTINUE
   20 CONTINUE
C
C Initialize element force vector
C
      CALL RVZERO(ELOAD,NEVAB)
C
C-----------------------------------------------------------------------
C Calculation of the F-bar deformation gradient determinat
C-----------------------------------------------------------------------
C Evaluate inverse of the incremental deformation gradient at the
C centroid of the F-bar element
      NGAUSB=IELPRP(8)
      IPOS  =NGAUSP*NDIME+NGAUSP+NGAUSP*NNODE+2*NGAUSB+1
      EXISC =RELPRP(IPOS)
      ETASC =RELPRP(IPOS+1)
C
      CALL SHPFUN
     1(   DERIV      ,ETASC      ,EXISC      ,0          ,IELTYP     ,
     2    NDIME      ,SHAPE      )
      CALL JACOB2
     1(   CARTD      ,DERIV      ,DETJAC     ,ELCOD      ,IELEM      ,
     2    NDIME      ,NDIME      ,NNODE      )
      IF(DETJAC.LE.R0)THEN
C cut increment if element jacobian is not positive definite
        CALL ERRPRT('WE0021')
        INCCUT=.TRUE.
        GOTO 999
      ENDIF
      IF(NTYPE.EQ.3)CALL GETGCO
     1(   GPCOD      ,ELCOD      ,NDIME      ,NDIME      ,NNODE      ,
     2    SHAPE      )
C          
      CALL GETGMX
     1(   GPCOD      ,CARTD      ,GMATX      ,NDIME      ,MGDIM      ,
     2    NAXIS      ,NNODE      ,NTYPE      ,SHAPE      )
C Determinant of the incremental deformation gradient at the centroid
      CALL DEFGRA
     1(   DELDIS     ,FINCIN     ,GMATX      ,MDOFN      ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
      IF(NTYPE.EQ.2)THEN
        AFACT=RP5
        DET=FINCIN(1,1)*FINCIN(2,2)-FINCIN(1,2)*FINCIN(2,1)
      ELSEIF(NTYPE.EQ.3)THEN
C... cut increment if incr. def. gradient is not positive definite
        IF(FINCIN(3,3).LE.R0)THEN
          CALL ERRPRT('WE0022')
          INCCUT=.TRUE.
          GOTO 999
        ENDIF
        AFACT=R1D3
        DET=(FINCIN(1,1)*FINCIN(2,2)-FINCIN(1,2)*FINCIN(2,1))*
     1      FINCIN(3,3)
      ENDIF
      DET0=R1/DET
C Determinant of the total deformation gradient at the centroid
      CALL DEFGRA
     1(   TELDIS     ,FINV       ,GMATX      ,MDOFN      ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
      IF(NTYPE.EQ.2)THEN
        DET=FINV(1,1)*FINV(2,2)-FINV(1,2)*FINV(2,1)
      ELSEIF(NTYPE.EQ.3)THEN
C... cut increment if deformation gradient is not positive definite
        IF(FINV(3,3).LE.R0)THEN
          CALL ERRPRT('WE0023')
          INCCUT=.TRUE.
          GOTO 999
        ENDIF
        DET=(FINV(1,1)*FINV(2,2)-FINV(1,2)*FINV(2,1))*FINV(3,3)
      ENDIF
      DETF0=R1/DET
C-----------------------------------------------------------------------
C
C=======================================================================
C                 Begin loop over Gauss points                         |
C=======================================================================
C
      IPPOS=1
      IPWEI=NGAUSP*NDIME+1
      DO 40 IGAUSP=1,NGAUSP
C Set Gauss points positions and weights
        EXISP=RELPRP(IPPOS-1+IGAUSP*2-1)
        ETASP=RELPRP(IPPOS-1+IGAUSP*2)
        WEIGP=RELPRP(IPWEI-1+IGAUSP)
C Evaluate shape functions and their derivatives (use current
C configuration for large strains)
        CALL SHPFUN
     1(   DERIV      ,ETASP      ,EXISP      ,0          ,IELTYP     ,
     2    NDIME      ,SHAPE      )
        CALL JACOB2
     1(   CARTD      ,DERIV      ,DETJAC     ,ELCOD      ,IELEM      ,
     2    NDIME      ,NDIME      ,NNODE      )
        IF(DETJAC.LE.R0)THEN
C ...cut increment if current jacobian is not positive definite
          CALL ERRPRT('WE0024')
          INCCUT=.TRUE.
          GOTO 999
        ENDIF
        IF(NTYPE.EQ.3)CALL GETGCO
     1(   GPCOD      ,ELCOD      ,NDIME      ,NDIME      ,NNODE      ,
     2    SHAPE      )
C Evaluate symmetric gradient operator B
        CALL GETBMX
     1(   BMATX      ,GPCOD      ,CARTD      ,NDIME      ,NBDIM      ,
     2    NAXIS      ,NNODE      ,NTYPE      ,SHAPE      )
C
C Compute basic kinematic variables needed for state update
C =========================================================
C
C Large strains: compute incremental deformation gradient
C -------------------------------------------------------
C gradient operator G in current configuration
        CALL GETGMX
     1(   GPCOD      ,CARTD      ,GMATX      ,NDIME      ,MGDIM      ,
     2    NAXIS      ,NNODE      ,NTYPE      ,SHAPE      )
C incremental deformation gradient
        CALL DEFGRA
     1(   DELDIS     ,FINCIN     ,GMATX      ,MDOFN      ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
        IF(NTYPE.EQ.3.AND.FINCIN(3,3).LE.R0)THEN
C ...cut increment if determinant of incr. def. gradient non positive
          CALL ERRPRT('WE0025')
          INCCUT=.TRUE.
          GOTO 999
        ENDIF
        CALL INVF2
     1(   FINCIN     ,FINCR      ,NTYPE      )
C modified incremental deformation gradient for F-bar element
        IF(NTYPE.EQ.2)THEN
          DET=FINCR(1,1)*FINCR(2,2)-FINCR(1,2)*FINCR(2,1)
        ELSEIF(NTYPE.EQ.3)THEN
          DET=(FINCR(1,1)*FINCR(2,2)-FINCR(1,2)*FINCR(2,1))*
     1         FINCR(3,3)
        ENDIF
        FACTOR=(DET0/DET)**AFACT
        FINCR(1,1)=FACTOR*FINCR(1,1)
        FINCR(1,2)=FACTOR*FINCR(1,2)
        FINCR(2,1)=FACTOR*FINCR(2,1)
        FINCR(2,2)=FACTOR*FINCR(2,2)
        IF(NTYPE.EQ.3)FINCR(3,3)=FACTOR*FINCR(3,3)
C... determinant of total deformation gradient
        DETF=DETF0
C
C Call material interface routine for state update calls: Update stress
C and other state variables
C =====================================================================
C
        NLARGE=1
        CALL MATISU
     1(   DETF       ,NLARGE     ,NTYPE      ,SUFAIL     ,
     2    THKGP(IGAUSP)          ,EINCR      ,FINCR      ,IPROPS     ,
     3    LALGVA(1,IGAUSP)       ,RALGVA(1,IGAUSP)       ,RPROPS     ,
     4    RSTAVA(1,IGAUSP)       ,STRSG(1,IGAUSP)        )
        IF(SUFAIL)THEN
C State updating failed for current Gauss point: break loop over Gauss
C points and exit with increment cutting flag activated
          INCCUT=.TRUE.
          GOTO 999
        ENDIF
C
C Add current Gauss point contribution to the element internal force
C vector
C ==================================================================
C
C evaluate elemental volume
        DVOLU=DETJAC*WEIGP
        IF(NTYPE.EQ.3)THEN
          DVOLU=DVOLU*TWOPI*GPCOD(NAXIS)
        ENDIF
C                           T
C add current gauss point  B [sigma]
        CALL RTV
     1(   0          ,NBDIM      ,NEVAB      ,NBDIM      ,ELOAD      ,
     2    BMATX      ,STRSG(1,IGAUSP)        ,DVOLU      )
C
   40 CONTINUE
C
C=======================================================================
C                 End of loop over Gauss points                        |
C=======================================================================
C
  999 CONTINUE
      RETURN
      END
