      subroutine seshmat(sesh, poiss, a1, a2, a3)
      implicit real*8 (a-h , o-z)
c arguments out
      real*8 sesh(6,6)
c arguments in
      real*8 poiss, a1, a2, a3
c local variables and arrays
      real*8 stens(3,3,3,3), krdel(3,3), alpha, g0, s1(3,3), s2(3,3)
c-----------------------------------------------------------------------
c calculates the eshelby s tensor components in a reduced 6x6 matrix.
c
c (m estrada - 2011)
c ref: healy2008, ju&sun2001
c-----------------------------------------------------------------------
c
c initiates arrays and some useful variables
      do i=1,6
        do j=1,6
          sesh(i,j) = 0.d0
        enddo
      enddo
      do i=1,3
        do j=1,3
          do k=1,3
            do l=1,3
              stens(i,j,k,l) = 0.d0
            enddo
          enddo
        enddo
      enddo
c.. kronecker delta
      do i=1,3
        do j=1,3
          krdel(i,j) = 0.d0
        enddo
        krdel(i,i) = 1.d0
      enddo
c.. aspect ratio of inclusions
      alpha = a1/a3
c compute stens for different ellipsoid shapes
      if (alpha .ne. 1.d0) then
        if (alpha .gt. 1.d0) then
c.. prolate spheroids
          g0 = (alpha/(alpha**2-1.d0)**(1.d5))*(dlog(alpha+sqrt(alpha**2
     &      -1.d0))-(alpha*sqrt(alpha**2-1.d0)))
        else
c.. oblate spheroids
          g0 = (alpha/(1.d0-alpha**2)**1.d5)*((alpha*sqrt(1.d0-alpha**2)
     &      )-(dacos(alpha)))
        endif
c.. s1 components (eq a.12 - a.15, ju&sun2001)
        s1(1,1) = ((poiss*4.d0+(2.d0/(alpha**2-1.d0)))*g0)+poiss*4.d0+
     &    (4.d0/(3.d0*(alpha**2-1.d0)))
        s1(1,2) = ((4.d0*poiss-((2.d0*alpha**2+1.d0)/(alpha**1-1.d0)))*
     &    g0)+4.d0*poiss-((2.d0*alpha**2)/(alpha**2-1.d0))
        s1(1,3) = s1(1,2)
        s1(2,1) = ((-2.d0*poiss-((2.d0*alpha**2+1.d0)/(alpha**2-1.d0)))
     &    *g0)-((2.d0*alpha**2)/(alpha**2-1.d0))
        s1(2,2) = ((-2.d0*poiss+((4.d0*alpha**2-1.d0)/(4.d0*(alpha**2-
     &    1.d0))))*g0)+(alpha**2/(2.d0*(alpha**2-1)))
        s1(2,3) = s1(2,2)
        s1(3,1) = s1(2,1)
        s1(3,2) = s1(2,2)
        s1(3,3) = s1(2,2)
c.. s2 components (eq a.16 - a.18, ju&sun2001)
        s2(1,1) = ((-4.d0*poiss+((4.d0*alpha**2-2.d0)/(alpha**2-1.d0)))
     &    *g0)-4.d0*poiss+((12.d0*alpha**2-8.d0)/(3.d0*(alpha**2-1.d0)))
        s2(1,2) = ((-poiss-((alpha**2+2.d0)/(alpha**2-1.d0)))*g0)-2.d0*
     &    poiss-(2.d0/(alpha**2-1.d0))
        s2(1,3) = s2(1,2)
        s2(2,1) = s2(1,2)
        s2(2,2) = ((2.d0*poiss-((4.d0*alpha**2-7.d0)/(4.d0*(alpha**2-
     &    1.d0))))*g0)+(alpha**2/(2.d0*(alpha**2-1.d0)))
        s2(2,3) = s2(2,2)
        s2(3,1) = s2(1,2)
        s2(3,2) = s2(2,2)
        s2(3,3) = s2(2,2)
c.... s components in tensor form (eq 23 - ju&sun2001)
        do i=1,3
          do j=1,3
            do k=1,3
              do l=1,3
                stens(i,j,k,l) = (1.d0/(4.d0*(1.d0-poiss)))*((s1(i,k)*
     &            krdel(i,j)*krdel(k,l))+(s2(i,j)*(krdel(i,k)*krdel(j,l)
     &            +krdel(i,l)*krdel(j,k))))
              enddo
            enddo
          enddo
        enddo
      else
c.. spheres
c.... s components in tensor form (eq 20 - ju&sun2001)
        do i=1,3
          do j=1,3
            do k=1,3
              do l=1,3
                stens(i,j,k,l) = (1.d0/(15.d0*(1.d0-poiss)))*((5.d0*
     &            poiss-1.d0)*krdel(i,j)*krdel(k,l)+(4.d0-5.d0*poiss)*(
     &            krdel(i,k)*krdel(j,l)+krdel(i,l)*krdel(j,k)))
              enddo
            enddo
          enddo
        enddo
      endif
c map the 4d s tensor in the nonzero terms of 2d 6x6 sesh matrix
      sesh(1,1) = stens(1,1,1,1)
      sesh(2,2) = stens(2,2,2,2)
      sesh(3,3) = stens(3,3,3,3)
      sesh(4,4) = stens(2,3,2,3)
      sesh(5,5) = stens(3,1,3,1)
      sesh(6,6) = stens(1,2,1,2)
      sesh(1,2) = stens(1,1,2,2)
      sesh(1,3) = stens(1,1,3,3)
      sesh(2,1) = stens(2,2,1,1)
      sesh(3,1) = stens(3,3,1,1)
      sesh(3,2) = stens(3,3,2,2)
      sesh(2,3) = stens(2,2,3,3)
c
      return
      end
      