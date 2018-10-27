      subroutine swcovm
     .(   mode       ,ntype      ,lalgvc     ,lalgvl     ,ralgvc     ,
     .    rstavc     ,rstavl     ,stresc     ,stresl     )
!***********************************************************************
! Initialise/Switch data 
! for composite material (plane strain and axisymmetric only).
!   
!   Fibers: Damage Weibull model
!   Matrix: Elastoplastic Von-Mises J2 model
!
! (M. Estrada, 2014)
!-----------------------------------------------------------------------
! Subroutine arguments:
!
!***********************************************************************
!
! Variable declaration
      implicit double precision (a-h,o-z)
!      
! ... arguments
      logical
     .    lalgvc             ,lalgvl
      dimension
     .    lalgvc(*)          ,lalgvl(*)          ,ralgvc(*)          ,
     .    rstavc(*)          ,rstavl(*)          ,stresc(*)          ,
     .    stresl(*)
!
! Set number of variables of each kind
      if(ntype.eq.1.or.ntype.eq.2.or.ntype.eq.3)then
        nstre=4
        nrstav=13
      endif
      nralgv=1
      nlalgv=3
!
! Initialization mode (0)
! -----------------------
      if (mode.eq.0) then
        call rvzero(stresc,nstre)
        call rvzero(ralgvc,nralgv)
        do i = 1, nlalgv
          lalgvc(i) = .false.
        enddo
! RSTAVA stores the infinitesimal elastic egineering strain tensor
! (engineering logarithmic strains in large strains) and the effective
! plastic strain
        call rvzero(rstavc,nrstav)
      else
!
! Switching mode (1)
! ------------------
        if (mode.eq.1) then
          do i = 1, nstre
            stresl(i) = stresc(i)
          enddo
          do i = 1, nrstav
            rstavl(i) = rstavc(i)
          enddo
          do i = 1, nlalgv
            lalgvl(i) = lalgvc(i)
          enddo
! Zero plastic multipliers before starting a new increment
          call rvzero(ralgvc,nralgv)
!
! Switching mode (2 or 3)
! -----------------------
        elseif (mode.eq.2 .or. mode.eq.3) then
          do i = 1, nstre
            stresc(i) = stresl(i)
          enddo
          do i = 1, nrstav
            rstavc(i) = rstavl(i)
          enddo
          do i = 1, nlalgv
            lalgvc(i) = lalgvl(i)
          enddo
! Zero plastic multipliers before starting a new increment
          call rvzero(ralgvc,nralgv)
        endif
      endif
!
      return
      end