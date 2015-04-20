      SUBROUTINE ARRGO2
     1(   A4TH       ,AMATX      )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER
     1(   NDIM=2     ,NGDIM=4    )
C Arguments
      DIMENSION
     1    A4TH(NDIM,NDIM,NDIM,NDIM)              ,AMATX(NGDIM,NGDIM)
C***********************************************************************
C RE-ARRANGES A FOURTH ORDER TENSOR, STORED AS A 4-INDEX ARRAY, IN
C MATRIX FORM (2-INDEX ARRAY) USING G-MATRIX COMPONENT ORDERING
C (11,21,12,22). FOR 2-D ONLY.
C
C REFERENCE: Section D.2.1
C***********************************************************************
C
      AMATX(1,1)=A4TH(1,1,1,1) 
      AMATX(1,2)=A4TH(1,1,2,1) 
      AMATX(1,3)=A4TH(1,1,1,2)
      AMATX(1,4)=A4TH(1,1,2,2) 
C
      AMATX(2,1)=A4TH(2,1,1,1) 
      AMATX(2,2)=A4TH(2,1,2,1) 
      AMATX(2,3)=A4TH(2,1,1,2) 
      AMATX(2,4)=A4TH(2,1,2,2) 
C
      AMATX(3,1)=A4TH(1,2,1,1) 
      AMATX(3,2)=A4TH(1,2,2,1) 
      AMATX(3,3)=A4TH(1,2,1,2) 
      AMATX(3,4)=A4TH(1,2,2,2) 
C
      AMATX(4,1)=A4TH(2,2,1,1) 
      AMATX(4,2)=A4TH(2,2,2,1) 
      AMATX(4,3)=A4TH(2,2,1,2) 
      AMATX(4,4)=A4TH(2,2,2,2) 
C
      RETURN
      END
