      subroutine swcovm
     &(   mode       ,ntype      ,lalgvc     ,lalgvl     ,ralgvc     ,
     &    rstavc     ,rstavl     ,stresc     ,stresl     )
      implicit double precision (a-h,o-z)
c arguments
      logical lalgvc, lalgvl
      dimension lalgvc(*), lalgvl(*), ralgvc(*), rstavc(*), rstavl(*), 
     &  stresc(*), stresl(*)
c
c-----------------------------------------------------------------------
c initialize/switch data for elasto-plastic composite material with 
c elastic fibers and von mises matrix with isotropic hardening.
c
c (m. estrada, 2011)
c
c    mode=0:   initialises the relevant data.
c
c    mode=1:   assigns current values of the state variables to
c              converged solution (when the current iteration
c              satisfies the convergence criterion).
c
c    mode=2:   assigns the last converged solution to current state
c              variables values (when a new iteration is required by
c              the iterative process).
c
c    mode=3:   assigns the last converged solution to current state
c              variables values (when increment cutting is required).
c-----------------------------------------------------------------------
c
      if (ntype.eq.1.or.ntype.eq.2.or.ntype.eq.3) then
        nstre=4
        nrstav=5
      endif
      nralgv=1
      nlalgv=2
c
      if (mode.eq.0) then
c initialisation mode:
        call rvzero(stresc,nstre)
        call rvzero(ralgvc,nralgv)
        do i=1,nlalgv
          lalgvc(i) =.false.
        enddo
c.. rstava stores the infinitesimal elastic egineering strain tensor
c.. (engineering logarithmic strains in large strains) and the effective
c.. plastic strain
        call rvzero(rstavc, nrstav)
      else
c switching mode: converged solution
        if (mode.eq.1) then
          do i=1,nstre
            stresl(i) = stresc(i)
          enddo
          do i=1,nrstav
            rstavl(i) = rstavc(i)
          enddo
          do i=1,nlalgv
            lalgvl(i) = lalgvc(i)
          enddo
c.. zero plastic multipliers before starting a new increment
          call rvzero(ralgvc, nralgv)
        elseif (mode.eq.2.or.mode.eq.3) then
c switching mode: new iteration or new increment
          do i=1,nstre
            stresc(i) = stresl(i)
          enddo
          do i=1,nrstav
            rstavc(i) = rstavl(i)
          enddo
          do i=1,nlalgv
            lalgvc(i) = lalgvl(i)
          enddo
          if (mode.eq.3) then
c.. zero plastic multipliers before starting a new increment
            call rvzero(ralgvc,nralgv)
          endif
        endif
      endif
c
      return
      end
      