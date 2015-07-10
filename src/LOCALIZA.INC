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
     .  gamloc      ,gamwsd     ,vecnre(2)  ,tauloc     ,taumxd     ,
     .  qinjec      ,qinf       ,parak      ,injtyp     ,cpmeth
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
!       - qinjec    : Q for first injection time
!
!       - qinf      : Q at infinity
!
!       - parak     : k parameter
!
!       - injtyp    : Injection type
      integer   noinj, wdinj,  symsdinj, sdinj, gamwsd, gamloc, injtyp
      parameter ( noinj     =   1   )
      parameter ( wdinj     =   2   )
      parameter ( symsdinj  =   3   )
      parameter ( sdinj     =   4   )
!
!       - cpmeth    : Crack path method
      integer   cpmeth, dsmooth
      parameter ( dsmooth   =   1   )
!
