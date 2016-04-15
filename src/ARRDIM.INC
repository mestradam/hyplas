!***********************************************************************
!----------------------------------------------------------------------*
!                                                                      *
!                                                                      *
!                  H Y P L A S   ARRAY'S DIMENSIONS                    *
!                                                                      *
!                                                                      *
!----------------------------------------------------------------------*
!***********************************************************************
!
!   Real dimensions for array's size
!   --------------------------------
!
      common/arrdim/
     .   nrprop     ,niprop     ,nrstav     ,nlalgv     ,nralgv     ,
     .   nrestv     ,niestv     ,nielpr     ,nrelpr
!
!       - nrprop    : Number of real material properties
!       - niprop    : Number of integer material properties
!       - nrstav    : Number of real material state variables
!       - nlalgv    : Number of logical material algorithmic variables
!       - nralgv    : Number of real material algorithmic variables
!       - nrestv    : Number of real element state variables
!       - niestv    : Number of integer element state variables
!       - nielpr    : Number of integer element properties
!       - nrelpr    : Number of real element properties
!
      integer
     .   nrprop     ,niprop     ,nrstav     ,nlalgv     ,nralgv     ,
     .   nrestv     ,niestv     ,nielpr     ,nrelpr