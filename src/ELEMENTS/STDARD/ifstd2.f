      SUBROUTINE IFSTD2
     1(   IELEM      ,INCCUT     ,MDIME      ,MELEM      ,MPOIN      ,
     2    MSTRE      ,MTOTV      ,NAXIS      ,NLARGE     ,NTYPE      ,
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
     1    R0   ,R1   ,R8   /
     2    0.0D0,1.0D0,8.0D0/
C***********************************************************************
C COMPUTE INTERNAL FORCE VECTOR OF ALL ELEMENTS OF CLASS 'STDARD'
C (STANDARD ISOPARAMETRIC ELEMENTS) IN 2-D: PLANE STRAIN, PLANE STRESS
C AND AXISYMMETRIC
C
C REFERENCE: Section 4.1.2
C            Box 4.2, item (viii)
C***********************************************************************
      IF(NTYPE.EQ.1)THEN
        NBDIM=3
      ELSEIF(NTYPE.EQ.2)THEN
        NBDIM=3
      ELSEIF(NTYPE.EQ.3)THEN
        TWOPI=R8*ATAN(R1)
        NBDIM=4
      ELSE
        CALL ERRPRT('EI0012')
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
          IF(NLARGE.EQ.1)THEN
            ELCOD(IDOFN,INODE)=COORD1(IDOFN,LNODE)
            TELDIS(IDOFN,INODE)=-TDISP(NPOSN)
            DELDIS(IDOFN,INODE)=-DINCR(NPOSN)
          ELSE
            ELCOD(IDOFN,INODE)=COORD1(IDOFN,LNODE)
            DELDIS(IDOFN,INODE)=DINCR(NPOSN)
          ENDIF
   10   CONTINUE
   20 CONTINUE
C
C Initialize element force vector
C
      CALL RVZERO(ELOAD,NEVAB)
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
          CALL ERRPRT('WE0009')
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
        IF(NLARGE.EQ.1)THEN
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
            CALL ERRPRT('WE0010')
            INCCUT=.TRUE.
            GOTO 999
          ENDIF
          CALL INVF2
     1(   FINCIN     ,FINCR      ,NTYPE      )
C... compute determinant of total deformation gradient
          CALL DEFGRA
     1(   TELDIS     ,FINV       ,GMATX      ,MDOFN      ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
          DETFIN=FINV(1,1)*FINV(2,2)-FINV(1,2)*FINV(2,1)
          IF(NTYPE.EQ.3)THEN
            IF(FINV(3,3).LE.R0)THEN
C... cut increment if deformation gradient is not positive definite
              CALL ERRPRT('WE0010')
              INCCUT=.TRUE.
              GOTO 999
            ENDIF
            DETFIN=DETFIN*FINV(3,3)
          ENDIF
          DETF=R1/DETFIN
        ELSE
C Small strains: compute incremental infinitesimal strain
C -------------------------------------------------------
          CALL LISTRA
     1(   BMATX      ,DELDIS     ,MDOFN      ,NBDIM      ,NDOFN      ,
     2    NNODE      ,NTYPE      ,EINCR      )
        ENDIF
C
C Call material interface routine for state update calls: Update stress
C and other state variables
C =====================================================================
C
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
        IF(NTYPE.EQ.1)THEN
          DVOLU=DVOLU*THKGP(IGAUSP)
        ELSEIF(NTYPE.EQ.3)THEN
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
