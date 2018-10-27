      subroutine rsq4wsd
     &  (ielprp     ,ndatf      ,nresf      ,relprp     ,unsym      )
      implicit double precision (a-h,o-z)
      parameter
     &  (mgausp=6   ,mnodeg=2   ,ndime=2    ,ndofel=8   ,nedgel=4   ,
     &   ngausb=1   ,nnode=4    )
      logical unsym
      dimension
     &   ielprp(*)          ,relprp(*)
      dimension
     &   nordeb(nnode,nedgel) ,posgp(2,mgausp)  ,posgpb(ngausb)     ,
     &   weigp(mgausp)      ,weigpb(ngausb)
!***********************************************************************
! Read input data and set properties for element type 'q4wsd'
! (wsdisc enriched bi-linear 4-noded quadrilateral element with 2
!  additional sampling points in the centroid)
!
! REFERENCE: Figure 4.5
!***********************************************************************
 1000 format(' q4wsd (wsdisc enriched bi-linear 4-noded quadrilateral'/ 
     &       ' element with 2 additional sampling points in the '/
     &       ' centroid)'/
     &       ' Integration rule: ',i2,' gauss points')
!
! Read number of gauss points for domain integration
! --------------------------------------------------
      read(ndatf,*)ngausp
      write(nresf,1000)ngausp
      if(ngausp.ne.3.and.ngausp.ne.6)call errprt('ED0199')
!
! Set element integer properties (stored in vector IELPRP)
! --------------------------------------------------------
! total number of nodes and gauss points for domain integration
      ielprp(3) = nnode
      ielprp(4) = ngausp
! number of degrees of freedom of the element
      ielprp(5) = ndofel
! number of edges of the element
      ielprp(6) = nedgel
! maximum number of nodes per edge
      ielprp(7) = mnodeg
! number of gauss points for boundary integration
      ielprp(8) = ngausb
! node numbering order on boundaries (set correspondance between local
! element node numbers and "edge" node numbering for boundary
! integration)
      nordeb(1,1)=1
      nordeb(2,1)=2
      nordeb(3,1)=0
      nordeb(4,1)=0
      nordeb(1,2)=0
      nordeb(2,2)=1
      nordeb(3,2)=2
      nordeb(4,2)=0
      nordeb(1,3)=0
      nordeb(2,3)=0
      nordeb(3,3)=1
      nordeb(4,3)=2
      nordeb(1,4)=2
      nordeb(2,4)=0
      nordeb(3,4)=0
      nordeb(4,4)=1
      ipos=9
      do iedgel=1,nedgel
        do inode=1,nnode
          ielprp(ipos)=nordeb(inode,iedgel)
          ipos=ipos+1
        end do
      end do
!
! Set element real properties (stored in vector RELPRP)
! -----------------------------------------------------
! gaussian constants for domain integration
      call gaus2d
     &  ('QUA'      ,ngausp     ,posgp      ,weigp      )
      ipos=1
      do igausp=1,ngausp
        relprp(ipos)=posgp(1,igausp)
        relprp(ipos+1)=posgp(2,igausp)
        ipos=ipos+ndime
      end do
      ipos=ngausp*ndime+1
      do igausp=1,ngausp
        relprp(ipos)=weigp(igausp)
        ipos=ipos+1
      end do
! set matrix of coefficients for extrapolation from gauss points to
! nodes
      ipos=ngausp*ndime+ngausp+1
      call exq4wsd
     &  (ngausp     ,relprp(ipos))
! gaussian constants for boundary integration (intergration over edges)
      call gaus1d
     &  (ngausb     ,posgpb     ,weigpb     )
      ipos=ngausp*ndime+ngausp+(ngausp-2)*nnode+1
      do igausb=1,ngausb
        relprp(ipos)=posgpb(igausb)
        ipos=ipos+1
      end do
      ipos=ngausp*ndime+ngausp+(ngausp-2)*nnode+ngausb+1
      do igausb=1,ngausb
        relprp(ipos)=weigpb(igausb)
        ipos=ipos+1
      end do
! Set unsymmetric solver flag
! ---------------------------
      unsym=.true.
!
      return
      end
