      subroutine algwsd
     .  (dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     .   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     .   tfacto     ,unsym      ,rstinp     ,rstout     ,dfact      ,
     .   dlenp      ,fstop      ,itdes      ,nincs      ,dfactv     ,
     .   miter      ,miterv     ,noutpv     ,tolerv     ,kunld      ,
     .   icount     ,albrk      )
!======================================================================|
!                                                                      |
!              Subroutine to compute one load increment                |
!   with fixed increments of weak/strong discontinuities fomulation    |
!                                                                      |
!                          Implicit scheme                             |
!                                                                      |
! (M Estrada 2015)                                                     |
!                                                                      |
! REFERENCE: Subroutine calcimpstg_2 from program commatfail           |
!            (Diaz, 2012)                                              |
!                                                                      |
! REFERENCE: Chapter 4 (Boxes 4.1-4) of the companion textbook.        |
!            Section 5.4.                                              |
!            The load incrementation loops carried out here are those  |
!            of the Flowcharts of Figures 5.2-3.                       |
!                                                                      |
!======================================================================|
      implicit double precision (a-h, o-z)
! Hyplas database: Global parameters and common blocks
      include '../MAXDIM.INC'
      include '../MATERIAL.INC'
      include '../ELEMENTS.INC'
      include '../GLBDBASE.INC'
      include '../LOCALIZA.INC'
! Common block of arrays used only by the frontal solver
      common / fronta /
     1    eqrhs(mtotv,2)     ,eqrow(mfron,mtotv) ,eqcol(mfron,mtotv) ,
     2    decay(mfron)       ,gload(mfron,2)     ,vecrv(mfron,2)     ,
     3    locel(mevab,melem) ,nacva(mfron,melem) ,namev(mtotv)       ,
     4    ndest(mevab,melem) ,npivo(mtotv)       ,nfron(melem)
! Logical control flags for main program
      logical
     .    convrg     ,diverg     ,inccut     ,unsym       ,
     .    albrk
! File names
      character*256
     1    rstinp     ,rstout
! Increment control arrays for main program
      dimension
     1    dfactv(mincs)      ,dfsub(msubin)      ,miterv(mincs)      ,
     2    noutp(5)           ,noutpv(5,mincs)    ,tolerv(mincs)
! Numerical constants
      parameter
     1(   r0=0.0d0   ,rp5=0.5d0  ,rp7=0.7d0  )
!
! Formats
 1050 FORMAT(////
     1' INCREMENT NUMBER',I5,19X,'TOTAL LOAD FACTOR =',G15.6/
     2' --------------------------------------------------------------',
     3'------------'/
     4 4X,'         ',13X,'relative residual',13X,'maximum residual'/
     5 4X,'iteration',13X,'    norm (%)     ',13X,'     norm       '/
     6' --------------------------------------------------------------',
     7'------------')
 1055 FORMAT(////
     1' INCREMENT NUMBER',I5,19X,'       ARC LENGTH =',G15.6/
     2' --------------------------------------------------------------',
     3'------------'/
     4 4X,'         ',6X,'relative residual',4X,'maximum residual',
     5 5X,'   total'/
     6 4X,'iteration',6X,'    norm (%)     ',4X,'     norm       ',
     7 5X,'load factor'/
     8' --------------------------------------------------------------',
     9'------------')
 1060 FORMAT(
     1' --------------------------------------------------------------',
     2'------------')
 1063 FORMAT(34X,'INCREMENTAL LOAD FACTOR =',G15.6)
 1065 FORMAT(30X,'CONVERGED TOTAL LOAD FACTOR =',G15.6)
 1067 FORMAT(24X,'CONVERGED INCREMENTAL LOAD FACTOR =',G15.6)
 1040 FORMAT(//' Iterations not converged.')
 1100 FORMAT(//' Iterations diverging.')
 1110 FORMAT(/ ' Re-trying with reduced increment size...'/)
 1120 FORMAT(/ ' Re-trying with reduced arc length...'/)
!
!      
      ipsub=1
      if (nalgo.gt.0) then
        dfsub(1)=dfactv(icount)
        toler=tolerv(icount)
        miter=miterv(icount)
        noutp(1)=noutpv(1,icount)
        noutp(2)=noutpv(2,icount)
        noutp(3)=noutpv(3,icount)
        noutp(4)=noutpv(4,icount)
        noutp(5)=noutpv(5,icount)
      end if
!
! Reset converged problem variables
! ---------------------------------
      call switch( 1 )
!
 10   continue
!
! Update increment counter
! ------------------------
      iincs=iincs+1
!
! Increment external load according to user-prescribed incremental 
! proportional load factor
! ----------------------------------------------------------------
      dfact=dfsub(ipsub)
      call increm
     .  (iincs        ,tfact      ,toler      ,miter      ,noutp      ,
     .   dfact        ,dfold      ,kunld      )
!
!______________________________________________________________________
!                                                                      |
!             Start loop over equilibrium iterations                   |
!______________________________________________________________________|
!
      do iiter=1,miter
!
! Select solution alorithm variable KRESL
        call algor(iincs ,iiter ,kresl ,kunld )
!
! Assemble stiffness matrix and solve for iterative displacements
! (tangential solution for the arc-length method) the linearised system
! of discretised equilibrium equations using the frontal algorithm
! ---------------------------------------------------------------------
        call front
     1(   iiter      ,kresl      ,ifneg      ,kunld      ,mxfron     ,
     2    unsym      ,inccut     )
!
        if(inccut)then
! System solution failed due to zero pivot: break equilibrium iteration
! loop and activate increment cutting
          goto 30
        endif
!
! Update incremental and total displacements. Also update nodal
! coordinates for large deformation analyses
! -------------------------------------------------------------
        call upconf
!
! Re-set converged load factors and print out increment information
! -----------------------------------------------------------------
        if(iiter.eq.1)then
          if(iincs.eq.1)then
! Re-set previous converged load factors/arc-length
            if(nalgo.lt.0)dlengo=dleng
            dfacto=dfact
            tfacto=r0
          endif
          if(nalgo.gt.0)then
! Fixed increments option: print current total load factor
            write(*,1050) iincs,tfact
            write(16,1050)iincs,tfact
          else
! Arc-length: print current arc-length
            write(*,1055) iincs,dleng
            write(16,1055)iincs,dleng
          endif
        endif
!
! Re-set relevant problem variables to last converged solution
! ------------------------------------------------------------
        call switch( 2 )
!
! Update problem variables (stress and other state variables) and
! evaluate internal force vectors of all elements
! ---------------------------------------------------------------
        call intfor( inccut )
!
        if(inccut)then
! Internal force calculation failed: break equilibrium iteration loop
! and activate load increment cutting
          goto 30
        end if
!
! Assemble internal and external global force vectors, reactions,
! compute residual and check for convergence
! ---------------------------------------------------------------
        call conver(convrg,diverg,iiter,toler,tfact)
!
        itact=iiter
!
        if(convrg)then
! Iterations have converged: break equilibrium iteration loop and go to
! next load increment
          write(*,1060)
          write(16,1060)
          if(nalgo.gt.0)then
            write(*,1063) dfact
            write(16,1063)dfact
          else
            write(*,1065) tfact
            write(*,1067) dfact
            write(16,1065)tfact
            write(16,1067)dfact
          end if
          write(*,1060)
          write(16,1060)
          goto 40
        else if(diverg)then
! Iterations are diverging: break equilibrium iteration loop and
! activate load increment cutting
          write(16,1100)
          write(*,1100)
          goto 30
        end if
!
      end do
!______________________________________________________________________
!                                                                      |
!                End loop over equilibrium iterations                  |
!______________________________________________________________________|
!
! Newton-Raphson procedure did not converge within the prescribed
! maximum number of iterations CC
! Print corresponding message and proceed to increment cutting
!
      write(16,1040)
      write(*,1040)
!
!
!
 30   continue
!
!
! Activate increment cutting
!
! REFERENCE: Section 5.4.3
! --------------------------
!
      if(nalgo.gt.0)then
! For fixed increments option: split current load increment into two
! equally sized sub-increments
        write(16,1110)
        write(*,1110)
        if(ipsub.eq.msubin)then
! abort program if maximum permissible number of consecutive increment
! cuts has been exceeded (i.e. sub-increment stack array DFSUB is full)
          call errprt('ee0002')
        endif
        dfsub(ipsub)  =dfsub(ipsub)*rp5
        dfsub(ipsub+1)=dfsub(ipsub)
        ipsub=ipsub+1
      else
! For arc-length method: reduce the arc-length
        write(16,1120)
        write(*,1120)
        if(iincs.eq.1)then
          dfact=dfacto*rp7
          dfacto=dfact
        else
          dleng=dlengo*rp7
          dlengo=dleng
        endif
      endif
! Switch relevant variables to last converged values (in load increment
! cutting mode) before re-trying with reduced load increment/arc-length
      tfact=tfacto
      call switch( 3 )
      iincs=iincs-1
      goto 10
!
!
   40 continue
!
! Newton-Raphson iterations converged for the current load increment
! ------------------------------------------------------------------
! Reset some converged parameters
      dlengo=dleng
      tfacto=tfact
      if(nalgo.gt.0)then
! Fixed increments option: update pointer to sub-increments stack array
        ipsub=ipsub-1
      else
! Arc-length method: update arc-length according to the desired number
! of iterations for convergence and the actual number of iterations
! needed for convergence in the previous load step
        call length(dleng ,dlenm ,itact ,itdes )
      endif
!
! Output results if required
! REFERENCE: Section 5.4.7
!
      outda=1
      call output(tfact,iincs,iiter,noutp,outda)
      outda=2
      call outgid(tfact,iincs,iiter,noutp,outda)
!
      if((nalgo.gt.0.and.ipsub.eq.0).or.(nalgo.lt.0))then
        call rstart
     1(  dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     2   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     3   tfacto     ,unsym      ,rstinp     ,rstout     ,1          ,
     4   incrst     )
      elseif(nalgo.gt.0.and.ipsub.ne.0)then
        call switch( 1 )
        goto 10
      endif
      if(nalgo.lt.0.and.fstop.ne.r0.and.tfact.gt.fstop)then
! Arc-length only: Break loop over increments and stop if maximum
! prescribed load factor has been exceeded
      albrk = .true.
      endif
!
!
      return
      end
