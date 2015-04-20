C***********************************************************************
C----------------------------------------------------------------------*
C                                                                      *
C                                                                      *
C                  H Y P L A S   LOCALIZATION DATABASE                 *
C                                                                      *
C                                                                      *
C----------------------------------------------------------------------*
C***********************************************************************
c
c   Localization of strong discontinuity common block
c   This data is read in 'indata' subroutine
c   -------------------------------------------------
c
      common/locali/
     &  gamloc      ,gamwsd     ,vecnre(2)  ,tauloc     ,taumxd     ,
     &  qinj        ,qinf       ,parak      ,injtyp
c
c       - gamloc    : Gamma for localization time
c
c       - gamwsd    : Gamma weak/strong discontinuity
c
c       - vecnre(1) : Vector n of reference x
c
c       - vecnre(2) : Vector n of reference y
c
c       - tauloc    : Tau for localization time
c
c       - taumxd    : Tau for mixed formulation
c
c       - qinj      : Q for first injection time
c
c       - qinf      : Q at infinity
c
c       - parak     : k parameter
c
c       - injtyp    : Injection type
c
