      SUBROUTINE MATIRD
     1(   MATNAM     ,NLARGE     ,NTYPE      ,UNSAUX     ,IPROPS     ,
     2    RPROPS     )
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Hyplas database
      INCLUDE '../MATERIAL.INC'
C Arguments
      LOGICAL      UNSAUX
      CHARACTER*80 MATNAM
      DIMENSION
     1    IPROPS(*)          ,RPROPS(*)
C***********************************************************************
C MATERIAL INTERFACE FOR READING MATERIAL-SPECIFIC INPUT DATA
C
C REFERENCE: Sections 5.7.1, 5.7.6
C***********************************************************************
C According to MATNAM, call the appropriate routine to read the
C the material-specific data from the input file and store it
C -------------------------------------------------------------
      IF(MATNAM.EQ.'ELASTIC')THEN
C Elastic
        MATTYP=ELASTC
        MATCLS=HYPEPL
        CALL RDEL
     1(   MRPROP ,MRSTAV ,RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'TRESCA')THEN
C Tresca elasto-plastic
        MATTYP=TRESCA
        MATCLS=HYPEPL
        CALL RDTR
     1(   IPROPS ,MIPROP ,MLALGV ,MRALGV ,MRPROP ,MRSTAV ,
     2    NLARGE ,NTYPE  ,RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'VON_MISES')THEN
C von Mises elasto-plastic
        MATTYP=VMISES
        MATCLS=HYPEPL
        CALL RDVM
     1(   IPROPS ,MIPROP ,MLALGV ,MRPROP ,MRSTAV ,
     2    RPROPS ,UNSAUX)
      ELSEIF (MATNAM.EQ.'VON_MISES_TRACTION_COMPRESION')THEN
C von Mises elasto-plastic with different threshold traction-compresion
        MATTYP=VMISTC
        MATCLS=HYPEPL
        CALL RDVMTC
     1(   IPROPS ,MIPROP ,MLALGV ,MRPROP ,MRSTAV ,
     2    RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'MOHR_COULOMB')THEN
C Mohr-Coulomb elasto-plastic
        MATTYP=MOHCOU
        MATCLS=HYPEPL
        CALL RDMC
     1(   IPROPS ,MIPROP ,MLALGV ,MRALGV ,MRPROP ,MRSTAV ,
     2    RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'DRUCKER_PRAGER')THEN
C Drucker-Prager elasto-plastic
        MATTYP=DRUPRA
        MATCLS=HYPEPL
        CALL RDDP
     1(   IPROPS  ,MIPROP ,MLALGV ,MRALGV ,MRPROP ,MRSTAV ,
     2    RPROPS  ,UNSAUX)
      ELSEIF(MATNAM.EQ.'LEMAITRE_DAMAGE')THEN
C Lemaitre's ductile damage model
        MATTYP=LEMDAM
        MATCLS=HYPEPL
        CALL RDDAMA
     1(   IPROPS ,MIPROP ,MLALGV ,MRPROP ,MRSTAV ,NTYPE  ,
     2    RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'DAMAGED_ELASTIC')THEN
C Isotropically damaged isotropic elastic material with crack closure
C effects
        MATTYP=DAMELA
        MATCLS=HYPEPL
        CALL RDDMEL
     1(   NTYPE ,MRPROP ,MRSTAV ,RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'OGDEN')THEN
C Ogden hyperelastic
        MATTYP=OGDEN
        MATCLS=HYPER
        CALL RDOGD
     1(   IPROPS ,MIPROP ,MRPROP ,MRSTAV ,RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'PLANAR_DOUBLE_SLIP_SINGLE_CRYSTAL')THEN
C Planar double-slip single crystal elasto-plastic model
        MATTYP=PDSCRY
        MATCLS=SINCRY
        CALL RDPDSC
     1(   IPROPS ,MIPROP ,MLALGV ,MRALGV ,MRPROP , MRSTAV ,
     2    NLARGE ,NTYPE  ,RPROPS ,UNSAUX )
      ELSEIF(MATNAM.EQ.'VON_MISES_MIXED')THEN
C von Mises elasto-plastic with mixed hardening (small strains only)
        MATTYP=VMMIXD
        MATCLS=PLASTC
        CALL RDVMMX
     1(   IPROPS     ,MIPROP     ,MLALGV     ,MRPROP     ,MRSTAV     ,
     2    NLARGE     ,NTYPE      ,RPROPS     ,UNSAUX     )
      ELSEIF (MATNAM.EQ.'CONCRETE_PLASTIC')THEN
C  Concrete plastic model (L. Rodriguez 2010)
        MATTYP=COPLAS
        MATCLS=HYPEPL
        CALL RDCOPL
     1(   IPROPS ,MIPROP ,MLALGV ,MRPROP ,MRSTAV ,
     2    RPROPS ,UNSAUX)
      ELSEIF (MATNAM.EQ.'CONCRETE_DAMAGE')THEN
C  Concrete isotropic damage model (L. Herrera 2010)
        MATTYP=CODAMA
        MATCLS=HYPEPL
        CALL RDCODA
     1(   IPROPS ,MIPROP ,MLALGV ,MRPROP ,MRSTAV ,
     2    RPROPS ,UNSAUX)
      ELSEIF(MATNAM.EQ.'ELASTIC_PRUEBA')THEN
C Elastico de prueba
        MATTYP=ELAPRU
        MATCLS=HYPEPL
        CALL RDELPRU
     1(   MRPROP ,MRSTAV ,RPROPS ,UNSAUX)
      elseif (matnam.eq.'COMPOSITE') then
c Composite material: VonMises + damage (M. Estrada 2014)
        mattyp = compvm
        matcls = hypepl
        call rdcovm
     &( iprops      ,miprop     ,mlalgv      ,mrprop     ,mrstav     ,
     &  rprops      ,unsaux     )
      ELSE
        CALL ERRPRT('ED0015')
      ENDIF
C Store material type and class flags in IPROPS
C ---------------------------------------------
      IPROPS(1)=MATTYP
      IPROPS(2)=MATCLS
C
      RETURN
      END
