      double precision function multmt
     &  (mat1       ,mat2       ,dim1       ,dim2       ,dim3       )
!***********************************************************************
! Computes the product of two matrices: 
!     multmt = mat1 * mat2
!
! (M. Estrada, 2015)
!***********************************************************************
      implicit none
!
! Variable declaration
      integer
     &   dim1       ,dim2       ,dim3       ,idim1      ,idim2      ,
     &   idim3
      double precision
     &   mat1       ,mat2
      dimension
     &   mat1(dim1,dim2)  ,mat2(dim2,dim3)  ,multmt(dim1,dim3)
!
! Computation
      do idim1 = 1, dim1
        do idim3 = 1, dim3
          multmt(idim1,idim3) = 0.d0
          do idim2 = 1, dim2
            multmt(idim1,idim3) = multmt(idim1,idim3) + 
     &                            mat1(idim1,idim2) * mat2(idim2,idim3)
          end do
        end do
      end do
!
      return
      end