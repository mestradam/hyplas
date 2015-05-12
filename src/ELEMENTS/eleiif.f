      SUBROUTINE ELEIIF
     1(   IELEM      ,IFFAIL     )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C
      LOGICAL  IFFAIL
C***********************************************************************
C ELEMENT INTERFACE FOR COMPUTATION OF ELEMENT INTERNAL FORCE VECTOR.
C CALL ELEMENT CLASS-SPECIFIC INTERNAL FORCE VECTOR CALCULATION ROUTINES
C
C REFERENCE: Section 5.5.1
C            Figure 5.4
C***********************************************************************
C Initialize internal force calculation failure flag
      IFFAIL=.FALSE.
C Recover element and material type group identification numbers
C --------------------------------------------------------------
      IGRUP =IGRPID(IELEM)
      IELIDN=IELTID(IGRUP)
      MATIDN=MATTID(IGRUP)
C Identify element class
C ----------------------
      IELCLS=IELPRP(2,IELIDN)
C
C Call internal force computation routine according to element class
C ------------------------------------------------------------------
C
      IF(IELCLS.EQ.STDARD)THEN
C Standard 2-D displacement-based isoparametric elements
        CALL IFSTD2
     1(   IELEM      ,IFFAIL     ,MDIME      ,MELEM      ,MPOIN      ,
     2    MSTRE      ,MTOTV      ,NAXIS      ,NLARGE     ,NTYPE      ,
     3    COORD(1,1,1)       ,DINCR              ,
     4    ELOAD(1,IELEM)     ,IELPRP(1,IELIDN)   ,IPROPS(1,MATIDN)   ,
     5    LALGVA(1,1,IELEM,1),LNODS              ,RALGVA(1,1,IELEM,1),
     6    RELPRP(1,IELIDN)   ,RPROPS(1,MATIDN)   ,RSTAVA(1,1,IELEM,1),
     7    STRSG(1,1,IELEM,1) ,THKGP(1,IELEM,1)   ,TDISP              )
c
      ELSEIF(IELCLS.EQ.FBAR)THEN
C 2-D F-bar elements (for large strain formulation only)
        CALL IFFBA2
     1(   IELEM      ,IFFAIL     ,MDIME      ,MELEM      ,MPOIN      ,
     2    MSTRE      ,MTOTV      ,NAXIS      ,NTYPE      ,
     3    COORD(1,1,1)       ,DINCR              ,
     4    ELOAD(1,IELEM)     ,IELPRP(1,IELIDN)   ,IPROPS(1,MATIDN)   ,
     5    LALGVA(1,1,IELEM,1),LNODS              ,RALGVA(1,1,IELEM,1),
     6    RELPRP(1,IELIDN)   ,RPROPS(1,MATIDN)   ,RSTAVA(1,1,IELEM,1),
     7    STRSG(1,1,IELEM,1) ,THKGP(1,IELEM,1)   ,TDISP              )
c
      elseif (ielcls.eq.wsdisc) then
c 2-D isoparametric weak and strong discontinuity elements
c (M Estrada 2014)
        call ifwsd2
     &  (ielem      ,iffail     ,mdime      ,melem      ,mpoin      ,
     &   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype      ,
     &   coord(1,1,1)        ,dincr             ,
     &   eload(1,ielem)      ,ielprp(1,ielidn)  ,iprops(1,matidn)   ,
     &   lalgva(1,1,ielem,1) ,lnods             ,ralgva(1,1,ielem,1),
     &   relprp(1,ielidn)    ,rprops(1,matidn)  ,rstava(1,1,ielem,1),
     &   strsg(1,1,ielem,1)  ,thkgp(1,ielem,1)  ,tdisp              ,
     &   restv(1,ielem)      ,iestv(1,ielem)    )
      ENDIF
      RETURN
      END
