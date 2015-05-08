      subroutine rsq4wsd
     1(   ielprp     ,ndatf      ,nresf      ,relprp     ,unsym      )
      implicit double precision (a-h,o-z)
      parameter
     1(   mgausp=6   ,mnodeg=2   ,ndime=2    ,ndofel=12  ,nedgel=4   ,
     2    ngausb=1   ,nnode=4    )
      logical unsym
      dimension
     1    ielprp(*)          ,relprp(*)
      dimension
     1    nordeb(nnode,nedgel),posgp(2,mgausp)   ,posgpb(ngausb)     ,
     2    weigp(mgausp)      ,weigpb(ngausb)
C***********************************************************************
C Read input data and set properties for element type 'QUA_4_SD'
C (standard isoparametric 4-noded bi-linear quadrilateral)
C
C REFERENCE: Figure 4.5
C***********************************************************************
 1000 FORMAT(' QUA_4_SD (standard 4-noded quadrilateral, with strong'/
     &       ' discontinuities embeded)'/
     &       ' Integration rule: ',i2,' gauss points')
C
C Read number of gauss points for domain integration
C --------------------------------------------------
      READ(NDATF,*)NGAUSP
      WRITE(NRESF,1000)NGAUSP
      IF(NGAUSP.NE.1.AND.NGAUSP.NE.4)CALL ERRPRT('ED0199')
C Set element integer properties (stored in vector IELPRP)
C --------------------------------------------------------
C total number of nodes and gauss points for domain integration
      IELPRP(3)=NNODE
      IELPRP(4)=NGAUSP
C number of degrees of freedom of the element
      IELPRP(5)=NDOFEL
C number of edges of the element
      IELPRP(6)=NEDGEL
C maximum number of nodes per edge
      IELPRP(7)=MNODEG
C number of gauss points for boundary integration
      IELPRP(8)=NGAUSB
C node numbering order on boundaries (set correspondance between local
C element node numbers and "edge" node numbering for boundary
C integration)
      NORDEB(1,1)=1
      NORDEB(2,1)=2
      NORDEB(3,1)=0
      NORDEB(4,1)=0
      NORDEB(1,2)=0
      NORDEB(2,2)=1
      NORDEB(3,2)=2
      NORDEB(4,2)=0
      NORDEB(1,3)=0
      NORDEB(2,3)=0
      NORDEB(3,3)=1
      NORDEB(4,3)=2
      NORDEB(1,4)=2
      NORDEB(2,4)=0
      NORDEB(3,4)=0
      NORDEB(4,4)=1
      IPOS=9
      DO 20, IEDGEL=1,NEDGEL
        DO 10, INODE=1,NNODE
          IELPRP(IPOS)=NORDEB(INODE,IEDGEL)
          IPOS=IPOS+1
   10   CONTINUE
   20 CONTINUE
C
C Set element real properties (stored in vector RELPRP)
C -----------------------------------------------------
C gaussian constants for domain integration
      CALL GAUS2D
     1(  'QUA'       ,NGAUSP     ,POSGP      ,WEIGP      )
      IPOS=1
      DO 30 IGAUSP=1,NGAUSP
        RELPRP(IPOS)=POSGP(1,IGAUSP)
        RELPRP(IPOS+1)=POSGP(2,IGAUSP)
        IPOS=IPOS+NDIME
   30 CONTINUE
      IPOS=NGAUSP*NDIME+1
      DO 40 IGAUSP=1,NGAUSP
        RELPRP(IPOS)=WEIGP(IGAUSP)
        IPOS=IPOS+1
   40 CONTINUE
C set matrix of coefficients for extrapolation from gauss points to
C nodes
      IPOS=NGAUSP*NDIME+NGAUSP+1
      CALL EXQ4WSD
     1(   NGAUSP     ,RELPRP(IPOS))
C gaussian constants for boundary integration (intergration over edges)
      CALL GAUS1D
     1(   NGAUSB     ,POSGPB     ,WEIGPB     )
      IPOS=NGAUSP*NDIME+NGAUSP+NGAUSP*NNODE+1
      DO 50 IGAUSB=1,NGAUSB
        RELPRP(IPOS)=POSGPB(IGAUSB)
        IPOS=IPOS+1
   50 CONTINUE
      IPOS=NGAUSP*NDIME+NGAUSP+NGAUSP*NNODE+NGAUSB+1
      DO 60 IGAUSB=1,NGAUSB
        RELPRP(IPOS)=WEIGPB(IGAUSB)
        IPOS=IPOS+1
   60 CONTINUE
C Set unsymmetric solver flag
C ---------------------------
      UNSYM=.FALSE.
C
      RETURN
      END
