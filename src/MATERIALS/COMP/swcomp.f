      subroutine swcomp
     &( mode        ,ntype      ,lalgvc     ,lalgvl     ,ralgvc     ,
     &  rstavc      ,rstavl     ,stresc     ,stresl     )
c***********************************************************************
c Initialise/Switch data for the COMPOSITE material model with two
c material components.
c   Fibers: Elastoplastic Von-Mises J2 model
c   Matrix: Elastoplastic Von-Mises J2 model
c
c (M. Estrada, 2014)
c-----------------------------------------------------------------------
c Subroutine arguments:
c
c   mode (in)   : Task to perform
c                   0   Initialises the relevant data.
c                   1   Assigns current values of the state variables
c                       to converged solution (when the current
c                       iteration satisfies the convergence criterion).
c                   2   Assigns the last converged solution to current
c                       state variables values (when a new iteration is
c                       required by the iterative process).
c                   3   Assigns the last converged solution to current
c                       state variables values (when increment cutting
c                       is required).
c   ntype (in)  : Analysis type
c                   1   plane stress
c                   2   plane strain
c                   3   axisymmetric
c   lalgvc (inout): Current logical algorithmic variables
c   lalgvl (inout): Last logical algorithmic variables
c   ralgvc (inout): Current real algorithmic variables
c   rstavc (inout): Current real state variables
c   rstavl (inout): Last logical state variables
c   stresc (inout): Current stress
c   stresl (inout): Last stress
c***********************************************************************
c
c Variable declaration
      implicit none
c
c ... arguments
      integer
     &  mode        ,ntype
      logical
     &  lalgvc(*)   ,lalgvl(*)
      real(kind=8)
     &  ralgvc(*)   ,rstavc(*)  ,rstavl(*)  ,stresc(*)  ,stresl(*)
c
c ... local variables
      integer
     &  nstre       ,nrstav     ,nralgv     ,nlalgv    ,i
c
c Set number of variables of each kind
      if (ntype.eq.1.or.ntype.eq.2.or.ntype.eq.3) then
          nstre = 4
          nrstav = 13
          nralgv = 2
          nlalgv = 3
      end if
c
c Initialization mode (0)
c -----------------------
      if (mode.eq.0) then
          call rvzero(stresc,nstre)
          call rvzero(ralgvc,nralgv)
          do i = 1, nlalgv
              lalgvc(i) = .false.
          end do
c RSTAVA stores the infinitesimal elastic egineering strain tensor
c (engineering logarithmic strains in large strains) and the effective
c plastic strain
          call rvzero(rstavc,nrstav)
      else
c
c Switching mode (1)
c ------------------
          if (mode.eq.1) then
              do i = 1, nstre
                  stresl(i) = stresc(i)
              end do
              do i = 1, nrstav
                  rstavl(i) = rstavc(i)
              end do
              do i = 1, nlalgv
                  lalgvl(i) = lalgvc(i)
              end do
c Zero plastic multipliers before starting a new increment
              call rvzero(ralgvc,nralgv)
c
c Switching mode (2 or 3)
c -----------------------
          elseif (mode.eq.2 .or. mode.eq.3) then
              do i = 1, nstre
                  stresc(i) = stresl(i)
              end do
              do i = 1, nrstav
                  rstavc(i) = rstavl(i)
              end do
              do i = 1, nlalgv
                  lalgvc(i) = lalgvl(i)
              end do
c Zero plastic multipliers before starting a new increment
              call rvzero(ralgvc,nralgv)
          end if
      end if
c
      return
      end