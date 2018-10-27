C***********************************************************************
C----------------------------------------------------------------------*
C                                                                      *
C                                                                      *
C                  H Y P L A S   MATERIALS DATABASE                    *
C                                                                      *
C                                                                      *
C----------------------------------------------------------------------*
C***********************************************************************
C
C
C***********************************************************************
C
C 1. Dimensioning parameters associated with available material models
C
C***********************************************************************
C
      integer mrprop, miprop, mrstav, mralgv, mlalgv
      PARAMETER( MRPROP=250 )
      PARAMETER( MIPROP=4   )
      PARAMETER( MRSTAV=20  )
      PARAMETER( MRALGV=10  )
      PARAMETER( MLALGV=15  )
C
C
C   * MRPROP = Maximum number of real material properties that can be
C              stored in array RPROPS for each material.
C
C   * MIPROP = Maximum number of integer material properties that can be
C              stored in array IPROPS for each material.
C
C   * MRSTAV = Maximum permissible number of real state variables that
C              can be stored in array RSTAVA for each gauss point.
C
C   * MRALGV = Maximum permissible number of real algorithmic variables
C              that can be stored in array RALGVA for each gauss point.
C
C   * MLALGV = Maximum permissible number of logical algorithmic
C              variables that can be stored in array LALGVA for each
C              gauss point.
C
C
C
C***********************************************************************
C
C 2. Material classes enumeration
C
C***********************************************************************
C
      INTEGER  HYPEPL ,HYPER ,SINCRY, PLASTC
      PARAMETER( HYPEPL=1 )
      PARAMETER( HYPER =2 )
      PARAMETER( SINCRY=3 )
      PARAMETER( PLASTC=4 )
C
C
C   * HYPEPL = Isotropic hyperelastic based elastic and elasto-plastic
C              material models with logarithmic finite strain extension.
C              In small strains, the kinematic variable required by the
C              state updating procedures for materials of this class is
C              the total (for the elastic material) or the elastic trial
C              infinitesimal strain (for the elasto-plastic materials).
C              The corresponding logarithmic strain measure is required
C              at finite strains.
C
C   * HYPER  = Isotropic hyperelastic material models (for finite
C              strains only).  For materials of this class, the left
C              Cauchy-Green strain tensor is the kinematic variable
C              required by the state updating procedure.
C
C   * SINCRY = Single crystal large strain elasto-plastic model.
C              For materials of this class, the incremental deformation
C              gradient is the kinematic variable required by the state
C              updating procedure.
C
C   * PLASTC = Elasto-plastic models with infinitesimal implementation
C              only (no finite strain extension).
C
C
C***********************************************************************
C
C 3. Material types enumeration
C
C***********************************************************************
C
      INTEGER  ELASTC ,OGDEN  ,TRESCA ,VMISES ,MOHCOU ,DRUPRA ,PDSCRY ,
     1         VMMIXD ,LEMDAM ,DAMELA ,VMISTC ,COPLAS ,CODAMA ,ELAPRU ,
     &         comp
      PARAMETER( ELASTC= 1)
      PARAMETER( OGDEN = 2)
      PARAMETER( TRESCA= 3)
      PARAMETER( VMISES= 4)
      PARAMETER( MOHCOU= 5)
      PARAMETER( DRUPRA= 6)
      PARAMETER( PDSCRY= 7)
      PARAMETER( VMMIXD= 8)
      PARAMETER( LEMDAM= 9)
      PARAMETER( DAMELA=10)
      PARAMETER( VMISTC=11)
      PARAMETER( COPLAS=12)
      PARAMETER( CODAMA=13)
      PARAMETER( ELAPRU=14)
      parameter( compvm=15)
C
C
C   * ELASTC = Standard linear elastic model (generalized Hooke's law).
C              Hencky material in finite strains (logarithimic extension
C              of the linear model).
C
C   * OGDEN  = Ogden's hyperelastic material model (finite strains
C              only).
C
C   * TRESCA = Elasto-plastic material with Tresca yield criterion,
C              associative plastic flow rule and general non-linear
C              isotropic hardening law. Logarithmic finite strain
C              extension.
C
C   * VMISES = Elasto-plastic material with Von-Mises yield criterion,
C              associative plastic flow rule and general piece-wise
C              linear isotropic hardening law. Logarithmic finite strain
C              extension.
C
C   * MOHCOU = Elasto-plastic material with Mohr-Coulomb yield
C              criterion, associative/non-associative plastic flow rules
C              and general piece-wise linear isotropic hardening law.
C              Logarithmic finite strain extension.
C
C   * DRUPRA = Elasto-plastic material with Drucker-Prager yield
C              criterion, associative/non-associative plastic flow rules
C              and general piece-wise linear isotropic hardening law.
C              Logarithmic finite strain extension.
C
C   * PDSCRY = Large strain planar double slip single crystal
C              elasto-plastic model with general piece-wise linear
C              Taylor hardening. This model is compatible only with
C              plane strain.
C
C   * VMMIXD = Von-Mises elasto-plastic model with associative flow
C              rule and non-linear (piece-wise linear) mixed
C              isotropic/kinematic hardening (infinitesimal strains
C              only).
C
C   * LEMDAM = Lemaitre's ductile damage elasto-plastic model
C              with non-linear (piece-wise linear) isotropic hardening.
C              With logarithmic finite strain extension.
C
C   * DAMELA = Lemaitre type isotropically damaged isotropic elastic
C              model with microcrack/void closure effects under
C              compression.
C
C   * VMISTC = Elasto-plastic material with Von-Mises yield criterion,
C              associative plastic flow rule and general differenced piece-wise
C              linear isotropic hardening law (traction-compresion).
C
C   * COPLAS = Elasto-plastic material model of simple concrete in plane stress
C              (L. Rodriguez 2010).
C
C   * CODAMA = Isotropic damage material model of simple concrete in plane stress
C              (L. Herrera 2010).
C
C   * ELAPRU = Modelo elastico de prueba. ( )
C
c   * compvm = Composite material model with the following components
c              Fibers: Damage Weibull model
c              Matrix: Plasticity Von mises
c              (M. Estrada 2014)
