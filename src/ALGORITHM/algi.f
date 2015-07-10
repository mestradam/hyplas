      subroutine algi
     .  (dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     .   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     .   tfacto     ,unsym      ,rstinp     ,rstout     ,dfact      ,
     .   dlenp      ,fstop      ,itdes      ,nincs      ,dfactv     ,
     .   miter      ,miterv     ,noutpv     ,tolerv     ,kunld      ,
     .   icount     ,albrk      ,nalgo      )
!***********************************************************************
! Interface for the incremental solution algorithm of finite element
! procedure.
!
! (M Estrada 2015)
!***********************************************************************
      implicit double precision (a-h,o-z)
!
! Logical control flags for main program
      logical
     .   convrg     ,diverg     ,inccut     ,unsym      ,albrk
! File names
      character*256
     .   rstinp     ,rstout
! Increment control arrays for main program
!      dimension
!     .   dfactv(mincs)      ,dfsub(msubin)      ,miterv(mincs)      ,
!     .   noutp(5)           ,noutpv(5,mincs)    ,tolerv(mincs)
!
! Call routine according to solution_algotirhm chosen
! ---------------------------------------------------
!
      if(nalgo.eq.8)then
! Incremental solution algorithm for weak/strong discontinuities
! (M Estrada 2015)
        call algwsd
     .  (dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     .   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     .   tfacto     ,unsym      ,rstinp     ,rstout     ,dfact      ,
     .   dlenp      ,fstop      ,itdes      ,nincs      ,dfactv     ,
     .   miter      ,miterv     ,noutpv     ,tolerv     ,kunld      ,
     .   icount     ,albrk      )
      else
! Default solution algorithm (this option was the only one that came
! with the original HYPLAS source files)
        call algstd
     .  (dfold      ,dleng      ,dlengo     ,dlenm      ,dlamd      ,
     .   ifneg      ,iincs      ,mxfron     ,noutp      ,tfact      ,
     .   tfacto     ,unsym      ,rstinp     ,rstout     ,dfact      ,
     .   dlenp      ,fstop      ,itdes      ,nincs      ,dfactv     ,
     .   miter      ,miterv     ,noutpv     ,tolerv     ,kunld      ,
     .   icount     ,albrk      )
      end if
!
      return
      end
