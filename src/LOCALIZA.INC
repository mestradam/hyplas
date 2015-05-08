!***********************************************************************
!----------------------------------------------------------------------*
!                                                                      *
!                                                                      *
!                  H Y P L A S   LOCALIZATION DATABASE                 *
!                                                                      *
!                                                                      *
!----------------------------------------------------------------------*
!***********************************************************************
!
!   Localization of strong discontinuity common block
!   This data is read in 'indata' subroutine
!   -------------------------------------------------
!
      common/locali/
     &  gamloc      ,gamwsd     ,vecnre(2)  ,tauloc     ,taumxd     ,
     &  qinj        ,qinf       ,parak      ,injtyp
!
!       - gamloc    : Gamma for localization time
!
!       - gamwsd    : Gamma weak/strong discontinuity
!
!       - vecnre(1) : Vector n of reference x
!
!       - vecnre(2) : Vector n of reference y
!
!       - tauloc    : Tau for localization time
!
!       - taumxd    : Tau for mixed formulation
!
!       - qinj      : Q for first injection time
!
!       - qinf      : Q at infinity
!
!       - parak     : k parameter
!
!       - injtyp    : Injection type
      integer   noinj,  symsdinj, sdinj, wdinj
      parameter ( noinj     =   1   )
      parameter ( symsdinj  =   2   )
      parameter ( sdinj     =   3   )
      parameter ( wdinj     =   4   )
!
