c
        subroutine glpts(n,x,w)
c       =========================================================
c       purpose : compute the zeros of legendre polynomial pn(x)
c                 in the interval [-1,1], and the corresponding
c                 weighting coefficients for gauss-legendre
c                 integration
c       input :   n    --- order of the legendre polynomial
c       output:   x(n) --- zeros of the legendre polynomial
c                 w(n) --- corresponding weighting coefficients
c       =========================================================
        implicit double precision (a-h,o-z)
        dimension x(n),w(n)
        n0=(n+1)/2
        do 45 nr=1,n0
           z=dcos(3.1415926d0*(nr-0.25d0)/n)
10         z0=z
           p=1.0d0
           do 15 i=1,nr-1
15            p=p*(z-x(i))
           f0=1.0d0
           if (nr.eq.n0.and.n.ne.2*int(n/2)) z=0.0d0
           f1=z
           do 20 k=2,n
              pf=(2.0d0-1.0d0/k)*z*f1-(1.0d0-1.0d0/k)*f0
              pd=k*(f1-z*pf)/(1.0d0-z*z)
              f0=f1
20            f1=pf
           if (z.eq.0.0) go to 40
           fd=pf/p
           q=0.0d0
           do 35 i=1,nr
              wp=1.0d0
              do 30 j=1,nr
                 if (j.ne.i) wp=wp*(z-x(j))
30            continue
35            q=q+wp
           gd=(pd-q*fd)/p
           z=z-fd/gd
           if (dabs(z-z0).gt.dabs(z)*1.0d-15) go to 10
40         x(nr)=z
           x(n+1-nr)=-z
           w(nr)=2.0d0/((1.0d0-z*z)*pd*pd)
45         w(n+1-nr)=w(nr)
        return
        end
        