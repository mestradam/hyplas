      subroutine algstd
     .  (dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     .   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     .   tfacto     ,unsym      ,rstinp     ,rstout     ,dfact      ,
     .   dlenp      ,fstop      ,itdes      ,nincs      ,dfactv     ,
     .   miter      ,miterv     ,noutpv     ,tolerv     ,kunld      ,
     .   icount     ,albrk      )
C      
      implicit double precision (a-h, o-z)
C Hyplas database: Global parameters and common blocks
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
      include '../LOCALIZA.INC'
C Common block of arrays used only by the frontal solver
      COMMON / FRONTA /
     1    EQRHS(MTOTV,2)     ,EQROW(MFRON,MTOTV) ,EQCOL(MFRON,MTOTV) ,
     2    DECAY(MFRON)       ,GLOAD(MFRON,2)     ,VECRV(MFRON,2)     ,
     3    LOCEL(MEVAB,MELEM) ,NACVA(MFRON,MELEM) ,NAMEV(MTOTV)       ,
     4    NDEST(MEVAB,MELEM) ,NPIVO(MTOTV)       ,NFRON(MELEM)
C Logical control flags for main program
      LOGICAL
     1    CONVRG     ,DIVERG     ,INCCUT     ,UNSYM       ,
     .    albrk
C File names
      CHARACTER*256
     1    RSTINP     ,RSTOUT
C Increment control arrays for main program
      DIMENSION
     1    DFACTV(MINCS)      ,DFSUB(MSUBIN)      ,MITERV(MINCS)      ,
     2    NOUTP(5)           ,NOUTPV(5,MINCS)    ,TOLERV(MINCS)
C Numerical constants
      PARAMETER
     1(   R0=0.0D0   ,RP5=0.5D0  ,RP7=0.7D0  )
C=======================================================================
C                                                                      |
C              Subroutine to compute one load increment                |
C                                                                      |
C REFERENCE: Chapter 4 (Boxes 4.1-4) of the companion textbook.        |
C            Section 5.4.                                              |
C            The load incrementation loops carried out here are those  |
C            of the Flowcharts of Figures 5.2-3.                       |
C                                                                      |
C=======================================================================
C
C Formats
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
C
        IPSUB=1
        IF(NALGO.GT.0)THEN
          DFSUB(1)=DFACTV(ICOUNT)
          TOLER=TOLERV(ICOUNT)
          MITER=MITERV(ICOUNT)
          NOUTP(1)=NOUTPV(1,ICOUNT)
          NOUTP(2)=NOUTPV(2,ICOUNT)
          NOUTP(3)=NOUTPV(3,ICOUNT)
          NOUTP(4)=NOUTPV(4,ICOUNT)
          NOUTP(5)=NOUTPV(5,ICOUNT)
        ENDIF
C
C Reset converged problem variables
C ---------------------------------
        CALL SWITCH( 1 )
C
   10   CONTINUE
C
C Update increment counter
C ------------------------
        IINCS=IINCS+1
C
C For fixed increments option only: Increment external load according
C to user-prescribed incremental proportional load factor
C -------------------------------------------------------------------
        IF(NALGO.GT.0)THEN
          DFACT=DFSUB(IPSUB)
          CALL INCREM
     1(   IINCS      ,TFACT      ,TOLER      ,MITER      ,NOUTP      ,
     2    DFACT      ,DFOLD      ,KUNLD      )
        ENDIF
C
C______________________________________________________________________
C                                                                      |
C             Start loop over equilibrium iterations                   |
C______________________________________________________________________|
C
        DO 20 IITER=1,MITER
C
C Select solution alorithm variable KRESL
          CALL ALGOR(IINCS ,IITER ,KRESL ,KUNLD )
C
          IF(NALGO.LT.0)THEN
C Set up prescribed displacements for tangential solution for the
C arc-length method
            CALL TANGEN
          ENDIF
C
C Assemble stiffness matrix and solve for iterative displacements
C (tangential solution for the arc-length method) the linearised system
C of discretised equilibrium equations using the frontal algorithm
C ---------------------------------------------------------------------
          CALL FRONT
     1(   IITER      ,KRESL      ,IFNEG      ,KUNLD      ,MXFRON     ,
     2    UNSYM      ,INCCUT     )
C
          IF(INCCUT)THEN
C System solution failed due to zero pivot: break equilibrium iteration
C loop and activate increment cutting
            GOTO 30
          ENDIF
C
C For Arc-Length method only: Compute iterative displacement according
C to the arc-length constraint and update the incremental and total
C load factors
C --------------------------------------------------------------------
          IF(NALGO.LT.0)THEN
            CALL ARCLEN
     1(   DFACT      ,DLAMD      ,DLENG      ,DLENM      ,DLENP      ,
     2    IFNEG      ,IINCS      ,IITER      ,INCCUT     ,TFACT      )
C
            IF(INCCUT)THEN
C No real roots for arc-length constraint equation: break equilibrium
C iteration loop and activate increment cutting
              GOTO 30
            ENDIF
          ENDIF
C
C Update incremental and total displacements. Also update nodal
C coordinates for large deformation analyses
C -------------------------------------------------------------
          CALL UPCONF
C
C Re-set converged load factors and print out increment information
C -----------------------------------------------------------------
          IF(IITER.EQ.1)THEN
            IF(IINCS.EQ.1)THEN
C Re-set previous converged load factors/arc-length
              IF(NALGO.LT.0)DLENGO=DLENG
              DFACTO=DFACT
              TFACTO=R0
            ENDIF
            IF(NALGO.GT.0)THEN
C Fixed increments option: print current total load factor
              WRITE(*,1050) IINCS,TFACT
              WRITE(16,1050)IINCS,TFACT
            ELSE
C Arc-length: print current arc-length
              WRITE(*,1055) IINCS,DLENG
              WRITE(16,1055)IINCS,DLENG
            ENDIF
          ENDIF
C
C Re-set relevant problem variables to last converged solution
C ------------------------------------------------------------
          CALL SWITCH( 2 )
C
C Update problem variables (stress and other state variables) and
C evaluate internal force vectors of all elements
C ---------------------------------------------------------------
          CALL INTFOR( INCCUT )
C
          IF(INCCUT)THEN
C Internal force calculation failed: break equilibrium iteration loop
C and activate load increment cutting
            GOTO 30
          ENDIF
C
C Assemble internal and external global force vectors, reactions,
C compute residual and check for convergence
C ---------------------------------------------------------------
          CALL CONVER(CONVRG,DIVERG,IITER,TOLER,TFACT)
C
          ITACT=IITER
C
          IF(CONVRG)THEN
C Iterations have converged: break equilibrium iteration loop and go to
C next load increment
            WRITE(*,1060)
            WRITE(16,1060)
            IF(NALGO.GT.0)THEN
              WRITE(*,1063) DFACT
              WRITE(16,1063)DFACT
            ELSE
              WRITE(*,1065) TFACT
              WRITE(*,1067) DFACT
              WRITE(16,1065)TFACT
              WRITE(16,1067)DFACT
            ENDIF
            WRITE(*,1060)
            WRITE(16,1060)
            GOTO 40
          ELSEIF(DIVERG)THEN
C Iterations are diverging: break equilibrium iteration loop and
C activate load increment cutting
            WRITE(16,1100)
            WRITE(*,1100)
            GOTO 30
          ENDIF
C
   20   CONTINUE
C______________________________________________________________________
C                                                                      |
C                End loop over equilibrium iterations                  |
C______________________________________________________________________|
C
C Newton-Raphson procedure did not converge within the prescribed
C maximum number of iterations !!
C Print corresponding message and proceed to increment cutting
C
        WRITE(16,1040)
        WRITE(*,1040)
C
C
C
   30   CONTINUE
C
C
C Activate increment cutting
C
C REFERENCE: Section 5.4.3
C --------------------------
C
        IF(NALGO.GT.0)THEN
C For fixed increments option: split current load increment into two
C equally sized sub-increments
          WRITE(16,1110)
          WRITE(*,1110)
          IF(IPSUB.EQ.MSUBIN)THEN
C abort program if maximum permissible number of consecutive increment
C cuts has been exceeded (i.e. sub-increment stack array DFSUB is full)
            CALL ERRPRT('EE0002')
          ENDIF
          DFSUB(IPSUB)  =DFSUB(IPSUB)*RP5
          DFSUB(IPSUB+1)=DFSUB(IPSUB)
          IPSUB=IPSUB+1
        ELSE
C For arc-length method: reduce the arc-length
          WRITE(16,1120)
          WRITE(*,1120)
          IF(IINCS.EQ.1)THEN
            DFACT=DFACTO*RP7
            DFACTO=DFACT
          ELSE
            DLENG=DLENGO*RP7
            DLENGO=DLENG
          ENDIF
        ENDIF
C Switch relevant variables to last converged values (in load increment
C cutting mode) before re-trying with reduced load increment/arc-length
        TFACT=TFACTO
        CALL SWITCH( 3 )
        IINCS=IINCS-1
        GOTO 10
C
C
C
   40   CONTINUE
C
C Newton-Raphson iterations converged for the current load increment
C ------------------------------------------------------------------
C Reset some converged parameters
        DLENGO=DLENG
        TFACTO=TFACT
        IF(NALGO.GT.0)THEN
C Fixed increments option: update pointer to sub-increments stack array
          IPSUB=IPSUB-1
        ELSE
C Arc-length method: update arc-length according to the desired number
C of iterations for convergence and the actual number of iterations
C needed for convergence in the previous load step
          CALL LENGTH(DLENG ,DLENM ,ITACT ,ITDES )
        ENDIF
C
C Output results if required
C REFERENCE: Section 5.4.7
C
        OUTDA=1
        CALL OUTPUT(TFACT,IINCS,IITER,NOUTP,OUTDA)
        OUTDA=2
        CALL OUTGID(TFACT,IINCS,IITER,NOUTP,OUTDA)
c
        IF((NALGO.GT.0.AND.IPSUB.EQ.0).OR.(NALGO.LT.0))THEN
          CALL RSTART
     1(  DFOLD      ,DLENG      ,DLENGO     ,DLENM      ,DLAMD      ,
     2   IFNEG      ,IINCS      ,MXFRON     ,NOUTP      ,TFACT      ,
     3   TFACTO     ,UNSYM      ,RSTINP     ,RSTOUT     ,1          ,
     4   INCRST     )
        ELSEIF(NALGO.GT.0.AND.IPSUB.NE.0)THEN
          CALL SWITCH( 1 )
          GOTO 10
        ENDIF
        IF(NALGO.LT.0.AND.FSTOP.NE.R0.AND.TFACT.GT.FSTOP)THEN
C Arc-length only: Break loop over increments and stop if maximum
C prescribed load factor has been exceeded
          albrk = .true.
        ENDIF
C
C
      return
      end