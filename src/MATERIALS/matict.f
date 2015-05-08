      SUBROUTINE MATICT
     1(   DETF       ,KUNLD      ,MBDIM      ,MGDIM      ,
     2    NLARGE     ,NTYPE      ,
     3    AMATX      ,DMATX      ,EINCR      ,FINCR      ,IPROPS     ,
     4    LALGVA     ,RALGVA     ,RPROPS     ,RSTAVA     ,RSTAV2     ,
     5    STRES      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE '../MATERIAL.INC'
C
      PARAMETER( MSTRA=4 )
C Arguments
      LOGICAL  LALGVA
      DIMENSION
     1    AMATX(MGDIM,MGDIM) ,DMATX(MBDIM,MBDIM) ,EINCR(MBDIM)       ,
     2    FINCR(3,3)         ,IPROPS(*)          ,LALGVA(*)          ,
     3    RALGVA(*)          ,RPROPS(*)          ,RSTAVA(*)          ,
     4    RSTAV2(*)          ,STRES(*)
C Local arrays and variables
      LOGICAL EPFLAG ,IFPLAS
      DIMENSION
     1    BETRL(MSTRA)       ,STRAT(MSTRA)       ,STRESK(4)
C***********************************************************************
C MATERIAL INTERFACE FOR CONSISTENT TANGENT COMPUTATION ROUTINE CALLS:
C ACCORDING TO THE MATERIAL TYPE, CALLS MATERIAL-SPECIFIC TANGENT
C COMPUTATION ROUTINE
C
C REFERENCE: Figure 5.5
C            Sections 5.7.4, 5.7.6
C***********************************************************************
C Start by identifying the material type and class
      MATTYP=IPROPS(1)
      MATCLS=IPROPS(2)
C
C Then call material class/type-specific routines
C
      IF(MATCLS.EQ.HYPEPL)THEN
C
C Elastic/elasto-plastic materials with logarithmic finite strain
C extension
C ===============================================================
C
C Retrieve current stress
        DO 50 ISTRE=1,4
          STRESK(ISTRE)=STRES(ISTRE)
   50   CONTINUE
        IF(NLARGE.EQ.1)THEN
C Large strains: compute last elastic trial LOGARITHMIC strain
C ------------------------------------------------------------
C elastic trial left Cauchy-Green tensor
          CALL BETRIA
     1(   BETRL      ,RSTAV2     ,FINCR      ,NTYPE      )
C elastic trial eulerian logarithmic strain
          CALL LOGSTR
     1(   BETRL      ,STRAT      ,NTYPE      )
          DETFT=DETF
          IF(NTYPE.EQ.1)THEN
C compute total deformation gradient (including thickness strain
C contribution) for plane stress, according to material model
            IF(MATTYP.EQ.ELASTC)THEN
C... Elastic (Hencky material in large strain)
              CALL TUEL
     1(   DETFT      ,RSTAVA     ,DUMMY      ,0          )
            ELSEIF(MATTYP.EQ.VMISES)THEN
C... von Mises elasto-plastic
              CALL TUVM
     1(   DETFT      ,RSTAVA     ,DUMMY      ,0          )
            ELSEIF(MATTYP.EQ.ELAPRU)THEN
C... Elastico de prueba
              CALL TUELPRU
     1(   DETFT      ,RSTAVA     ,DUMMY      ,0          )
            ELSE
C... Error: Material type not recognised or not implemented for finite
C    strains under plane stress
              CALL ERRPRT('EI0059')
            ENDIF
          ENDIF
C retrieve current KIRCHHOFF stress in large strains
          CALL RVSCAL(STRESK,4,DETFT)
        ELSE
C Small strains: compute last elastic trial INFINITESIMAL strain
C --------------------------------------------------------------
          DO 60 ISTRE=1,4
            STRAT(ISTRE)=RSTAV2(ISTRE)+EINCR(ISTRE)
   60     CONTINUE
        ENDIF
C Set plastic elasto-plastic tangent flag
C ---------------------------------------
        IF(MATTYP.NE.ELASTC.or.mattyp.ne.elapru)THEN
          IFPLAS=LALGVA(1)
          IF((.NOT.IFPLAS).OR.KUNLD.EQ.1)THEN
            EPFLAG=.FALSE.
          ELSE
            EPFLAG=.TRUE.
          ENDIF
        ENDIF
C Call material type-specific routines
C ------------------------------------
        IF(MATTYP.EQ.ELASTC)THEN
C Elastic
          CALL CTEL
     1(   DMATX      ,NTYPE      ,RPROPS     )
        ELSEIF(MATTYP.EQ.TRESCA)THEN
C Tresca
          IF(NTYPE.EQ.1)THEN
            CALL CTTRPN
     1(   DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,RPROPS     ,
     2    RSTAVA     ,STRAT      ,STRESK     )
          ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
            CALL CTTR
     1(   DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRAT      ,STRESK     )
          ENDIF
        ELSEIF(MATTYP.EQ.VMISES)THEN
C von Mises
          IF(NTYPE.EQ.1)THEN
            CALL CTVMPS
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE     ,
     2    RPROPS     ,RSTAVA     ,STRESK     )
          ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
            CALL CTVM
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE     ,
     2    RPROPS     ,RSTAVA     ,STRESK     )
          ENDIF
        ELSEIF(MATTYP.EQ.VMISTC)THEN
C von Mises with different hardening threshold
          IF(NTYPE.EQ.1)THEN
            CALL CTVMTC
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE     ,
     2    RPROPS     ,RSTAVA     ,STRESK     )
          ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
        CALL ERRPRT('EI0059')
          ENDIF
        ELSEIF(MATTYP.EQ.MOHCOU)THEN
C Mohr-Coulomb
          CALL  CTMC
     1(   DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRAT      ,STRESK     )
        ELSEIF(MATTYP.EQ.DRUPRA)THEN
C Drucker-Prager
          IF(NTYPE.EQ.1)THEN
            CALL  CTDPPN
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,
     2    NTYPE      ,RPROPS     ,RSTAVA     ,STRAT      )
          ELSEIF(NTYPE.EQ.2.OR.NTYPE.EQ.3)THEN
            CALL  CTDP
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,LALGVA     ,
     2    NTYPE      ,RPROPS     ,RSTAVA     ,STRAT      )
          ENDIF
        ELSEIF(MATTYP.EQ.LEMDAM)THEN
C Lemaitre's ductile damage model
          CALL CTDAMA
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,STRESK     )
        ELSEIF(MATTYP.EQ.DAMELA)THEN
C Isotropically damaged isotropic elastic material with crack closure
C effects
          CALL CTDMEL
     1(   DMATX      ,NTYPE      ,RPROPS     ,STRAT      ,STRESK     )
        ELSEIF(MATTYP.EQ.COPLAS)THEN
C Concrete plastic model (L. Rodriguez 2010)
          CALL CTCOPL
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE     ,
     2    RPROPS     ,RSTAVA     ,STRESK     )
        ELSEIF(MATTYP.EQ.CODAMA)THEN
C Concrete damage model (L. Herrera 2010)
           CALL CTCODA
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE     ,
     2    RPROPS     ,RSTAVA     ,STRESK     )
        ELSEIF(MATTYP.EQ.ELAPRU)THEN
C Elastico de prueba
          CALL CTELPRU
     1(   DMATX      ,NTYPE      ,RPROPS     )
        elseif(mattyp.eq.comp)then
c Composite material (M. Estrada 2014)
          call ctcomp
     &( ralgva      ,dmatx      ,lalgva     ,iprops     ,ntype      ,
     &  rprops      ,rstava     ,stresk     ,strat      )
        ELSE
C Error: Material type not recognised
          CALL ERRPRT('EI0044')
        ENDIF
C Perform extra kinematical operations required by materials of this
C class at large strains for computation of the spatial modulus 'a'
C ------------------------------------------------------------------
        IF(NLARGE.EQ.1)THEN
          CALL CSTEP2
     1(   AMATX      ,BETRL      ,DMATX      ,STRES     ,DETFT      ,
     2    NTYPE      )
        ENDIF
      ELSEIF(MATCLS.EQ.SINCRY)THEN
C
C Single crystal anisotropic elasto-plastic models
C ================================================
C
C Set plastic loading/unloading flag
        IFPLAS=LALGVA(1)
        IF((.NOT.IFPLAS).OR.KUNLD.EQ.1)THEN
          EPFLAG=.FALSE.
        ELSE
          EPFLAG=.TRUE.
        ENDIF
C Call material type-specific routines
C ------------------------------------
        IF(MATTYP.EQ.PDSCRY)THEN
C Planar double slip single crystal
          CALL CSTPDS
     1(   AMATX      ,RALGVA     ,EPFLAG     ,FINCR      ,IPROPS     ,
     2    LALGVA     ,NTYPE      ,RPROPS     ,RSTAVA     ,RSTAV2     ,
     3    STRES      )
        ELSE
C... Error: Material type not recognised
          CALL ERRPRT('EI0044')
        ENDIF
      ELSEIF(MATCLS.EQ.HYPER)THEN
C
C Generic isotropic finite hyperelasticity models
C ===============================================
C
C Call material type-specific routines
C ------------------------------------
        IF(MATTYP.EQ.OGDEN)THEN
C Ogden model
          CALL CSTOGD
     1(   AMATX      ,RSTAVA     ,IPROPS     ,NTYPE      ,RPROPS     ,
     2    STRES      )
        ELSE
C Error: Material type not recognised
          CALL ERRPRT('EI0044')
        ENDIF
      ELSEIF(MATCLS.EQ.PLASTC)THEN
C
C Elasto-plastic materials with small strain implementation only
C ==============================================================
C
        IFPLAS=LALGVA(1)
        IF((.NOT.IFPLAS).OR.KUNLD.EQ.1)THEN
          EPFLAG=.FALSE.
        ELSE
          EPFLAG=.TRUE.
        ENDIF
        IF(MATTYP.EQ.VMMIXD)THEN
C von Mises with mixed isotropic/kinematic hardening
          CALL CTVMMX
     1(   RALGVA     ,DMATX      ,EPFLAG     ,IPROPS     ,NTYPE      ,
     2    RPROPS     ,RSTAVA     ,RSTAV2     ,STRES      )
        ELSE
C Error: Material type not recognised
          CALL ERRPRT('EI0044')
        ENDIF
      ELSE
C Error: Material class not recognised
        CALL ERRPRT('EI0043')
      ENDIF
C
      RETURN
      END
