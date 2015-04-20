c subroutine matxinv.f 
c
c source                                                       
c   bevington, pages 302-303.
c
c purpose
c   invert a symmetric matrix and calculate its determinant
c
c usage 
c   call matxinv (array, norder, det)
c
c description of parameters
c   array  - input matrix which is replaced by its inverse
c   norder - degree of matrix (order of determinant)
c   det    - determinant of input matrix
c
c subroutines and function subprograms required 
c   none
c
c comment
c   dimension statement valid for norder up to 10
c
      subroutine matxinv(array,arrayorig,norder)
      double precision array,arrayorig,amax,save
      dimension array(10,10),arrayorig(10,10),ik(10),jk(10)
      det = determ(arrayorig,norder)
        do i=1,10
          do j=1,10
            array(i,j) = arrayorig(i,j)
          enddo
        enddo
c
10      det=1.
11      do 100 k=1,norder
c
c find largest element array(i,j) in rest of matrix
c
      amax=0. 
21      do 30 i=k,norder
      do 30 j=k,norder
23      if (dabs(amax)-dabs(array(i,j))) 24,24,30
24      amax=array(i,j) 
      ik(k)=i 
      jk(k)=j 
30      continue
c
c interchange rows and columns to put amax in array(k,k)
c
31      if (amax) 41,32,41
32      det=0.
      goto 140
41      i=ik(k) 
      if (i-k) 21,51,43
43      do 50 j=1,norder
      save=array(k,j) 
      array(k,j)=array(i,j)
50      array(i,j)=-save
51      j=jk(k) 
      if (j-k) 21,61,53
53      do 60 i=1,norder
      save=array(i,k) 
      array(i,k)=array(i,j)
60      array(i,j)=-save
c
c accumulate elements of inverse matrix 
c
61      do 70 i=1,norder
      if (i-k) 63,70,63
63      array(i,k)=-array(i,k)/amax
70      continue
71      do 80 i=1,norder
      do 80 j=1,norder
      if (i-k) 74,80,74
74      if (j-k) 75,80,75
75      array(i,j)=array(i,j)+array(i,k)*array(k,j)
80      continue
81      do 90 j=1,norder
      if (j-k) 83,90,83
83      array(k,j)=array(k,j)/amax
90      continue
      array(k,k)=1./amax
100      det=det*amax
c
c restore ordering of matrix
c
101      do 130 l=1,norder
      k=norder-l+1
      j=ik(k) 
      if (j-k) 111,111,105
105      do 110 i=1,norder
      save=array(i,k) 
      array(i,k)=-array(i,j)
110      array(i,j)=save 
111      i=jk(k) 
      if (i-k) 130,130,113
113      do 120 j=1,norder
      save=array(k,j) 
      array(k,j)=-array(i,j)
120      array(i,j)=save 
130      continue
140      return
      end
c
c----------------------------
c
c function determ.f
c
c source
c   bevington, page 294.
c
c purpose
c   calculate the determinant of a square matrix
c
c usage 
c   det = determ (array, norder)
c
c description of parameters
c   array  - matrix
c   norder - order of determinant (degree of matrix)
c
c subroutines and function subprograms required 
c   none
c
c comments
c   this subprogram destroys the input matrix array
c   dimension statement valid for norder up to 10
c
      function determ (array,norder)
      double precision array,save
      dimension array(10,10)
c
10      determ=1.
11      do 50 k=1,norder
c
c interchange columns if diagonal element is zero
c
      if (array(k,k)) 41,21,41
21      do 23 j=k,norder
      if (array(k,j)) 31,23,31
23      continue
      determ=0.
      goto 60 
31      do 34 i=k,norder
      save=array(i,j) 
      array(i,j)=array(i,k)
34      array(i,k)=save 
      determ=-determ
c
c subtract row k from lower rows to get diagonal matrix 
c
41      determ=determ*array(k,k)
      if (k-norder) 43,50,50
43      k1=k+1
      do 46 i=k1,norder
      do 46 j=k1,norder
46      array(i,j)=array(i,j)-array(i,k)*array(k,j)/array(k,k)
50      continue
60      return
      end
      