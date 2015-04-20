      SUBROUTINE ELEIST
     1(   ESTIF      ,IELEM      ,KUNLD      ,UNSYM      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c Hyplas global database
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
c
      LOGICAL UNSYM
      DIMENSION
     1    ESTIF(MEVAB,MEVAB)
c***********************************************************************
c ELEMENT INTERFACE ROUTINE FOR COMPUTATION OF ELEMENT TANGENT STIFFNESS
c MATRIX.
c CALL ELEMENT CLASS-SPECIFIC STIFFNESS MATRIX CALCULATION ROUTINES
c
c REFERENCE: Section 5.6.2
c            Figure 5.5
c***********************************************************************
c
c Recover element and material type group identification numbers
c --------------------------------------------------------------
      IGRUP=IGRPID(IELEM)
      IELIDN=IELTID(IGRUP)
      MATIDN=MATTID(IGRUP)
c Identify element class
      IELCLS=IELPRP(2,IELIDN)
c
c Call stiffness computation routine according to the element class
c -----------------------------------------------------------------
      IF(IELCLS.EQ.STDARD)THEN
        CALL STSTD2
     1(   IELEM      ,KUNLD      ,MDIME      ,MELEM      ,
     2    MPOIN      ,MSTRE      ,MTOTV      ,NAXIS      ,NLARGE     ,
     3    NTYPE      ,UNSYM      ,
     4    COORD(1,1,1)       ,DINCR              ,ESTIF              ,
     5    IELPRP(1,IELIDN)   ,IPROPS(1,MATIDN)   ,LALGVA(1,1,IELEM,1),
     6    LNODS              ,RALGVA(1,1,IELEM,1),RELPRP(1,IELIDN)   ,
     7    RPROPS(1,MATIDN)   ,RSTAVA(1,1,IELEM,1),RSTAVA(1,1,IELEM,2),
     8    STRSG(1,1,IELEM,1) ,THKGP(1,IELEM,1)   ,TDISP              )
      ELSEIF(IELCLS.EQ.FBAR)THEN
        CALL STFBA2
     1(   IELEM      ,KUNLD      ,MDIME      ,MELEM      ,
     2    MPOIN      ,MSTRE      ,MTOTV      ,NAXIS      ,
     3    NTYPE      ,UNSYM      ,
     4    COORD(1,1,1)       ,DINCR              ,ESTIF              ,
     5    IELPRP(1,IELIDN)   ,IPROPS(1,MATIDN)   ,LALGVA(1,1,IELEM,1),
     6    LNODS              ,RALGVA(1,1,IELEM,1),RELPRP(1,IELIDN)   ,
     7    RPROPS(1,MATIDN)   ,RSTAVA(1,1,IELEM,1),RSTAVA(1,1,IELEM,2),
     8    STRSG(1,1,IELEM,1) ,TDISP              )
C
      elseif (ielcls.eq.strdis) then
c 2-D isoparametric weak and strong discontinuity elements
c (M Estrada 2014)
          call stsd2
     &  (ielem      ,kunld      ,mdime      ,melem      ,mpoin      ,
     &   mstre      ,mtotv      ,naxis      ,nlarge     ,ntype      ,
     &   unsym      ,coord(1,1,1)           ,dincr      ,estif      ,
     &   ielprp(1,ielidn)   ,iprops(1,matidn)   ,lalgva(1,1,ielem,1),
     &   lnods      ,ralgva(1,1,ielem,1)    ,relprp(1,ielidn)       ,
     &   rprops(1,matidn)   ,rstava(1,1,ielem,1),rstava(1,1,ielem,2),
     &   strsg(1,1,ielem,1) ,thkgp(1,ielem,1)   ,tdisp  )
      ENDIF
C
      RETURN
      END
