      SUBROUTINE MATISW
     1(   MODE       ,NLARGE     ,NTYPE      ,IPROPS     ,LALGVC     ,
     2    LALGVL     ,RALGVC     ,RALGVL     ,RPROPS     ,RSTAVC     ,
     3    RSTAVL     ,STRESC     ,STRESL     )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE '../MATERIAL.INC'
C Arguments
      LOGICAL
     1     LALGVC            ,LALGVL
      DIMENSION
     1    IPROPS(*)          ,LALGVC(*)          ,LALGVL(*)          ,
     2    RALGVC(*)          ,RALGVL(*)          ,RPROPS(*)          ,
     3    RSTAVC(*)          ,RSTAVL(*)          ,STRESC(*)          ,
     4    STRESL(*)
C***********************************************************************
C MATERIAL INTERFACE FOR INITIALISATION/SWITCHING ROUTINE CALLS:
C ACCORDING TO THE MATERIAL TYPE, CALLS MATERIAL-SPECIFIC ROUTINE TO
C INITIALISE/SWITCH GAUSS POINT STATE AND ALGORITHMIC VARIABLES
C
C REFERENCE: Sections 5.7.3, 5.7.6
C***********************************************************************
C First identify material type and class
C --------------------------------------
      MATTYP=IPROPS(1)
C
C Then call material type-specific routines
C -----------------------------------------
      IF(MATTYP.EQ.ELASTC)THEN
C Elastic (Hencky material in large strains)
        CALL SWEL
     1(   MODE       ,NTYPE      ,RSTAVC     ,RSTAVL     ,STRESC     ,
     2    STRESL     )
      ELSEIF(MATTYP.EQ.TRESCA)THEN
C Tresca elasto-plastic
        CALL SWTR
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.VMISES.OR.MATTYP.EQ.VMISTC)THEN
C von Mises elasto-plastic (isotropic and differenced threshold)
        CALL SWVM
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.MOHCOU)THEN
C Mohr-Coulomb elasto-plastic
        CALL SWMC
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.DRUPRA)THEN
C Drucker-Prager elasto-plastic
        CALL SWDP
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.LEMDAM)THEN
C Lemaitre's ductile damage elasto-plastic model
        CALL SWDAMA
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.DAMELA)THEN
C Isotropically damaged isotropic elastic material with crack closure
C effects
        CALL SWDMEL
     1(   MODE       ,NTYPE      ,RSTAVC     ,RSTAVL     ,STRESC     ,
     2    STRESL     )
      ELSEIF(MATTYP.EQ.PDSCRY)THEN
C Planar double-slip single crystal
        CALL SWPDSC
     1(   MODE       ,LALGVC     ,LALGVL     ,RALGVC     ,RSTAVC     ,
     2    RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.OGDEN)THEN
C Ogden hyperelasticity model
        CALL SWOGD
     1(   MODE       ,NTYPE      ,RSTAVC     ,RSTAVL     ,STRESC     ,
     2    STRESL     )
      ELSEIF(MATTYP.EQ.VMMIXD)THEN
C von Mises with mixed isotropic/kinematic hardening (infinitesimal
C only)
        CALL SWVMMX
     1(   MODE       ,NLARGE     ,NTYPE      ,LALGVC     ,LALGVL     ,
     2    RALGVC     ,RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.COPLAS)THEN
C Concrete plastic model (L. Rodriguez 2010)
        CALL SWCOPL
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.CODAMA)THEN
C Concrete damage model (L. Herrera 2010)
        CALL SWCODA
     1(   MODE       ,NTYPE      ,LALGVC     ,LALGVL     ,RALGVC     ,
     2    RSTAVC     ,RSTAVL     ,STRESC     ,STRESL     )
      ELSEIF(MATTYP.EQ.ELAPRU)THEN
C Elastico de prueba ( )
        CALL SWELPRU
     1(   MODE       ,NTYPE      ,RSTAVC     ,RSTAVL     ,STRESC     ,
     2    STRESL     )
      elseif (mattyp.eq.comp) then
c Composite material (M Estrada 2014)
          call swcomp
     &( mode        ,ntype      ,lalgvc     ,lalgvl     ,ralgvc     ,
     &  rstavc      ,rstavl     ,stresc     ,stresl     )
      ELSE
C Error: Material type not recognised
        CALL ERRPRT('EI0046')
      ENDIF
      RETURN
      END
