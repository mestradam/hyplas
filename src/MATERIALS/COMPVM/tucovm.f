      subroutine tucovm
     &(   detf       ,rstava     ,thick      ,mode       )
c arguments and variables
      implicit double precision (a-h,o-z)
      parameter (mstre=4)
      dimension rstava(mstre+1)
c
c-----------------------------------------------------------------------
c thickness update for the composite elastic + von mises elasto-plastic 
c material with isotropic hardening
c
c (m. estrada, 2011)
c-----------------------------------------------------------------------
c compute determinant of total deformation gradient (including
c out-of-plane contribution). note that, for this model, the determinant
c of the total and elastic deformation gradient coincide due to plastic
c incompressibility.
c
c... start by retrieving the diagonal components of the elastic
c    logarithmic strain tensor
      ee11=rstava(1)
      ee22=rstava(2)
      ee33=rstava(4)
c... then compute determinant of total deformation gradient
      detft=exp(ee11+ee22+ee33)
      if (mode.eq.1) then
c compute thickness stretch
        strtc3=detft/detf
c update thickness
        thick = thick*strtc3
      endif
c return total deformation gradient determinant in detf
      detf = detft
c
      return
      end
      