      SUBROUTINE STSTD2
     1(   IELEM      ,KUNLD      ,MDIME      ,MELEM      ,
     2    MPOIN      ,MSTRE      ,MTOTV      ,NAXIS      ,NLARGE     ,
     3    NTYPE      ,UNSYM      ,
     4    COORD1     ,DINCR      ,ESTIF      ,IELPRP     ,IPROPS     ,
     5    LALGVA     ,LNODS      ,RALGVA     ,RELPRP     ,RPROPS     ,
     6    RSTAVA     ,RSTAV2     ,STRSG      ,THKGP      ,TDISP      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Hyplas database
      INCLUDE '../../MATERIAL.INC'
      INCLUDE '../../ELEMENTS.INC'
C
      PARAMETER( MGDIM=5 ,MBDIM=4 ,NDIME=2 ,NDOFN=2 )
C Arguments
      LOGICAL  LALGVA ,UNSYM
      DIMENSION
     1    COORD1(MDIME,MPOIN),DINCR(MTOTV)       ,ESTIF(MEVAB,MEVAB) ,
     2    IELPRP(*)          ,IPROPS(*)         ,LALGVA(MLALGV,MTOTG),
     3    LNODS(MELEM,MEVAB) ,RALGVA(MRALGV,MTOTG),RELPRP(*)         ,
     4    RPROPS(*)        ,RSTAVA(MRSTAV,MTOTG),RSTAV2(MRSTAV,MTOTG),
     5    STRSG(MSTRE,MTOTG) ,THKGP(MTOTG)       ,TDISP(MTOTV)
C Local arrays and variables
      DIMENSION
     1    AUXM(MEVAB,MGDIM)  ,AMATX(MGDIM,MGDIM) ,BMATX(4,MEVAB)     ,
     2    CARTD(NDIME,MNODE) ,DELDIS(2,MNODE)    ,DERIV(NDIME,MNODE) ,
     3    DMATX(MBDIM,MBDIM) ,EINCR(MBDIM)       ,ELCOD(NDIME,MNODE) ,
     4    FINCIN(3,3)        ,FINCR(3,3)         ,FINV(3,3)          ,
     5    GMATX(MGDIM,MEVAB) ,GPCOD(NDIME)       ,SHAPE(MNODE)       ,
     6    TELDIS(2,MNODE)
      DATA R0   ,R1   ,R8   /
     1     0.0D0,1.0D0,8.0D0/
C***********************************************************************
C EVALUATES THE ELEMENT TANGENT STIFFNESS MATRIX FOR ELEMENTS OF CLASS
C 'STDARD' (STANDARD ISOPARAMETRIC ELEMENTS) IN 2-D (PLANE STRAIN,
C PLANE STRESS AND AXISYMMETRIC PROBLEMS
C
C REFERENCE: Sections 4.2.4, 4.3.4
C            Box 4.2, item (iii)
C            Box 4.3
C***********************************************************************
      IF(NTYPE.EQ.3)TWOPI=R8*ATAN(R1)
C Identify element type
      IELTYP=IELPRP(1)
C Recover element information
      NNODE =IELPRP(3)
      NGAUSP=IELPRP(4)
      NEVAB =IELPRP(5)
C
C Set element nodal coordinates, total and incremental displacements
C vectors
C
      DO 20 INODE =1,NNODE
        LNODE=IABS(LNODS(IELEM,INODE))
        NPOSN=(LNODE-1)*NDOFN
        DO 10 IDOFN=1,NDOFN
          NPOSN=NPOSN+1
          ELCOD(IDOFN,INODE)=COORD1(IDOFN,LNODE)
          IF(NLARGE.EQ.1)THEN
            TELDIS(IDOFN,INODE)=-TDISP(NPOSN)
            DELDIS(IDOFN,INODE)=-DINCR(NPOSN)
          ELSE
            DELDIS(IDOFN,INODE)=DINCR(NPOSN)
          ENDIF
   10   CONTINUE
   20 CONTINUE
C
C Initialize the element stiffness matrix
C
      DO 40 IEVAB=1,NEVAB
        DO 30 JEVAB=1,NEVAB
          ESTIF(IEVAB,JEVAB)=R0
   30   CONTINUE
   40 CONTINUE
C
C=======================================================================
C                 Begin loop over Gauss points                         |
C=======================================================================
      IPPOS=1
      IPWEI=NGAUSP*NDIME+1
      DO 70 IGAUSP=1,NGAUSP
        EXISP=RELPRP(IPPOS-1+IGAUSP*2-1)
        ETASP=RELPRP(IPPOS-1+IGAUSP*2)
        WEIGP=RELPRP(IPWEI-1+IGAUSP)
C Evaluate the shape functions and derivatives
        CALL SHPFUN
     1(   DERIV      ,ETASP      ,EXISP      ,0          ,IELTYP     ,
     2    NDIME      ,SHAPE      )
        CALL JACOB2
     1(   CARTD      ,DERIV      ,DETJAC     ,ELCOD      ,IELEM      ,
     2    NDIME      ,NDIME      ,NNODE      )
        IF(DETJAC.LE.R0)THEN
C... stops program if element jacobian is not positive definite
          CALL ERRPRT('EE0003')
        ENDIF
        IF(NTYPE.EQ.3)CALL GETGCO
     1(   GPCOD      ,ELCOD      ,NDIME      ,NDIME      ,NNODE      ,
     2    SHAPE      )
C
        IF(NLARGE.EQ.1)THEN
C Large strains: compute incremental deformation gradient
C -------------------------------------------------------
C gradient operator G in the current configuration
          CALL GETGMX
     1(   GPCOD      ,CARTD      ,GMATX      ,NDIME      ,MGDIM      ,
     2    NAXIS      ,NNODE      ,NTYPE      ,SHAPE      )
C inverse of incremental deformation gradient
          CALL DEFGRA
     1(   DELDIS     ,FINCIN     ,GMATX      ,2          ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
C stops program if deformation gradient determinant is non-positive
          IF(NTYPE.EQ.3.AND.FINCIN(3,3).LE.R0)CALL ERRPRT('EE0004')
C incremental deformation gradient
          CALL INVF2
     1(   FINCIN     ,FINCR      ,NTYPE      )
C... and the determinant of the total deformation gradient
          CALL DEFGRA
     1(   TELDIS     ,FINV       ,GMATX      ,2          ,MGDIM      ,
     2    NDOFN      ,NTYPE      ,NNODE      )
          DETFIN=FINV(1,1)*FINV(2,2)-FINV(1,2)*FINV(2,1)
          IF(NTYPE.EQ.3)THEN
C stops program if deformation gradient is not positive definite
            IF(FINV(3,3).LE.R0)CALL ERRPRT('EE0004')
            DETFIN=DETFIN*FINV(3,3)
          ENDIF
          DETF=R1/DETFIN
        ELSE
C Small strains: compute incremental infinitesimal strain
C -------------------------------------------------------
C compute the symmetric gradient operator B
          CALL GETBMX
     1(   BMATX      ,GPCOD      ,CARTD      ,NDIME      ,4          ,
     2    NAXIS      ,NNODE      ,NTYPE      ,SHAPE      )
C and the incremental infinitesimal strain
          CALL LISTRA
     1(   BMATX      ,DELDIS     ,2          ,4          ,NDOFN      ,
     2    NNODE      ,NTYPE      ,EINCR      )
        ENDIF
C
C Call material interface routine for consistent tangent computation
C calls: Compute either the standard consistent tangent matrix DMATX
C (small strains) or the spatial tangent modulus AMATX (large strains)
C ====================================================================
C
        CALL MATICT
     1(   DETF       ,KUNLD      ,MBDIM      ,MGDIM      ,
     2    NLARGE     ,NTYPE      ,
     3    AMATX      ,DMATX      ,EINCR      ,FINCR      ,IPROPS     ,
     4    LALGVA(1,IGAUSP)       ,RALGVA(1,IGAUSP)       ,RPROPS     ,
     5    RSTAVA(1,IGAUSP)       ,RSTAV2(1,IGAUSP)       ,
     5    STRSG(1,IGAUSP)        )
C
C Add current Gauss point contribution to element stiffness
C =========================================================
C
C Compute elemental volume
        DVOLU=DETJAC*WEIGP
        IF(NTYPE.EQ.1)THEN
          DVOLU=DVOLU*THKGP(IGAUSP)
        ELSEIF(NTYPE.EQ.3)THEN
          DVOLU=DVOLU*TWOPI*GPCOD(NAXIS)
        ENDIF
        IF(NLARGE.EQ.1)THEN
C                                                         T
C Large strains:  assemble the element stiffness as  K:= G [a] G
C --------------------------------------------------------------
          IF(NTYPE.EQ.3)THEN
            NGDIM=5
          ELSE
            NGDIM=4
          ENDIF
          CALL RTSR
     1(   AUXM       ,0          ,MEVAB      ,MGDIM      ,NEVAB      ,
     2    NGDIM      ,ESTIF      ,GMATX      ,AMATX      ,DVOLU      ,
     3    UNSYM      )
        ELSE
C                                                        T
C Small strains: assemble the element stiffness as  K:= B D B
C -----------------------------------------------------------
          IF(NTYPE.EQ.1.OR.NTYPE.EQ.2)THEN
            NBDIM=3
          ELSEIF(NTYPE.EQ.3)THEN
            NBDIM=4
          ENDIF
          CALL RTSR
     1(   AUXM       ,0          ,MEVAB      ,4          ,NEVAB      ,
     2    NBDIM      ,ESTIF      ,BMATX      ,DMATX      ,DVOLU      ,
     3    UNSYM      )
        ENDIF
C
   70 CONTINUE
C=======================================================================
C                 End of loop over Gauss points                        |
C=======================================================================
      RETURN
      END
