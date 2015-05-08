C***********************************************************************
C----------------------------------------------------------------------*
C                                                                      *
C                                                                      *
C                   H Y P L A S   GLOBAL DATABASE                      *
C                                                                      *
C                                                                      *
C----------------------------------------------------------------------*
C***********************************************************************
C
C
C***********************************************************************
C
C 1. Dimensioning parameters associated with available analyses types
C
C***********************************************************************
C
C
      PARAMETER( MDIME=2 )
      PARAMETER( MSTRE=4 )
      PARAMETER( MTOTV=MDOFN*MPOIN )
C
C   - MDIME = Maximum permissible number of spatial dimensions.
C
C   - MSTRE = Maximum permissible number of stress components.
C
C   - MTOTV = Maximum permissible number of variables (degrees of
C             freedom) in a mesh.
C
C
C
C***********************************************************************
C
C 2. Global data base   ( COMMON blocks )
C
C***********************************************************************
C
C   Internal problem-defining control parameters
C   --------------------------------------------
C
      COMMON/CONTRL/
     1    NDOFN      ,NELEM      ,NGRUP      ,NPOIN      ,NTOTV      ,
     2    NVFIX      ,NTYPE      ,NALGO      ,NARCL      ,NDIME      ,
     3    NLARGE     ,NAXIS      ,NCURV
C
C
C       - NDOFN = Number of degrees of freedom per node.
C
C       - NELEM = Total number of elements in the mesh.
C
C       - NGRUP = Total number of element groups defined.
C
C       - NPOIN = Total number of nodal points in the mesh.
C
C       - NTOTV = Total number of variables (degrees of freedom) in the
C                 mesh.
C
C       - NVFIX = Total number of nodes with prescribed displacements.
C                 Does not include (slave) nodes with kinematical
C                 constraints assigned by means of Master/Slave nodal
C                 constraint specification.
C
C       - NTYPE = Analysis type:
C                     1 - Plane stress analysis;
C                     2 - Plane strain analysis;
C                     3 - Axisymmetric analysis.
C
C       - NALGO = Solution algorithm adopted:
C                     1 - Initial stiffness method;
C                     2 - Newton-Raphson tangent stiffness method;
C                     3 - Modified Newton KT1;
C                     4 - Modified Newton KT2;
C                     5 - Secant Newton - Initial stiffness;
C                     6 - Secant Newton - KT1;
C                     7 - Secant Newton - KT2,
C                 with negative values for combined Arc-length method.
C
C       - NDIME = Number of spatial dimensions.
C
C       - NLARGE= Large strain formulation flag:
C                     0 - small strain analysis,
C                     1 - large strain analysis.
C
C       - NAXIS = Axis of symmetry flag for axisymmetric analysis:
C                     1 - Symmetric about Y;
C                     2 - Symmetric about X.
C
C
C       - NCURV = number of result curves
C
C-----------------------------------------------------------------------
C
      COMMON/CORE  /
     1    FIXED(MTOTV,2)               ,STFOR(MTOTV)                 ,
     2    TOFOR(MTOTV)                 ,ELOAD(MEVAB,MELEM)           ,
     3    ELOADO(MEVAB,MELEM)          ,RLOAD(MEVAB,MELEM)
C
C
C-----------------------------------------------------------------------
C
C   Arrays of material properties
C   -----------------------------
C
      COMMON/MATERL/
     1    RPROPS(MRPROP,MGRUP),IPROPS(MIPROP,MGRUP)
C
C
C       - RPROPS = Array of real material properties. Stores one set of
C                  real material properties for each element group.
C                  Typical real material properties: Young's modulus,
C                  Poisson's ratio, hardening curves, etc.
C
C       - IPROPS = Array of integer material properties. Stores one set
C                  of integer material properties for each element
C                  group.
C                  Typical integer material properties: number of
C                  sampling points in hardening curves, etc.
C
C-----------------------------------------------------------------------
C
C   Arrays defining the mesh and kinematic constraints
C   --------------------------------------------------
C
      COMMON/MESH  /
     1    ANGLE(MVFIX)    ,COORD(MDIME,MPOIN,0:2),PRESC(MVFIX,MDOFN) ,
     2    IELTID(MGRUP)      ,IFFIX(MTOTV)       ,IGRPID(MELEM)      ,
     3    LNODS(MELEM,MEVAB) ,MASTER(MTOTV)      ,MATTID(MGRUP)      ,
     4    NOFIX(MVFIX)       ,NVALEN(MPOIN,MGRUP)
C
C
C       - ANGLE  = Array of fixity angles. For each node with prescribed
C                  displacement, stores the angle (with respect to the
C                  global X-axis) at which the kinematic constraint is
C                  applied.
C
C       - COORD  = Array of nodal coordinates. For each nodal point of
C                  the mesh, stores its initial, current and last
C                  converged (equilibrium) coordinates, respectively,
C                  in positions
C                                   COORD( x , x , 0),
C                                   COORD( x , x , 1) and
C                                   COORD( x , x , 2).
C                  Only initial coordinates are used in infinitesimal
C                  analysis.
C
C       - PRESC  = Array of prescribed displacements. For each node with
C                  with kinematic constraints, stores the prescribed
C                  value for each degree of freedom.
C
C       - IELTID = Array of element type identification numbers. For
C                  each element group, stores the corresponding element
C                  type identification number.
C
C       - IFFIX  = Array of fixed degree of freedom flags. For each
C                  degree of freedom of the structure, stores a flag
C                  (0 or 1) that tells whether that degree of freedom
C                  is kinematically constrained (1) or not (0).
C
C       - IGRPID = Array of element group identification numbers. For
C                  each element of the mesh, stores the identification
C                  number of the group to which it belongs.
C
C       - LNODS  = Array of nodal connectivities.
C
C       - MASTER = Array of master degrees of freedom.
C
C       - MATTID = Array of material type identification numbers. For
C                  each element group, stores the corresponding material
C                  type identification number.
C
C       - NOFIX  = Array of kinematically constrained nodes. Stores the
C                  global node numbers of all nodes with kinematic
C                  constraints.
C
C       - NVALEN = Array of nodal valencies. Stores the number of
C                  elements of each element group that share each node
C                  of the structure (used for nodal averaging for output
C                  purposes only).
C
C-----------------------------------------------------------------------
C
C   Arrays of element properties
C   ----------------------------
C
      COMMON/ELEMEN/
     1    RELPRP(MREPRP,MGRUP),IELPRP(MIEPRP,MGRUP)
C
C
C       - RELPRP = Array of real element properties. Stores one set of
C                  real element properties per element group.
C                  Typical real element properties are, position and
C                  weight of Gauss points, etc.
C
C       - IELPRP = Array of integer element properties. Stores one set
C                  of integer properties per element group.
C                  Typical integer element properties are: number of
C                  nodes, number of Gauss points, number of boundaries,
C                  etc.
C
C-----------------------------------------------------------------------
C
C   Nodal displacements and reactions
C   ---------------------------------
C
      COMMON/RESULT/
     1    DITER(MTOTV)       ,DINCR(MTOTV)       ,DINCRO(MTOTV)      ,
     2    DTANG(MTOTV)       ,TDISP(MTOTV)       ,TDISPO(MTOTV)      ,
     3    TREAC(MVFIX,MDOFN) ,LCURV(MCURV,6)
C
C
C       - DITER  = Global vector of iterative nodal displacements.
C
C       - DINCR  = Global vector of (current) incremental nodal
C                  displacements.
C
C       - DINCRO = Global vector of incremental nodal displacements at
C                  the previous converged (equilibrium) state.
C
C       - DTANG  = Global tangential solution vector (used by Arc-Length
C                  method only).
C
C       - TDISP  = Global vector of (current) total nodal displacements.
C
C       - TDISPO = Global vector of total nodal displacements at the
C                  previous converged (equilibrium) state.
C
C       - TREAC  = Array of nodal reaction forces. Stores the reaction
C                  force for each kinematically constrained degree of
C                  freedom of each kinematically constrained node.
C
C       - LCURV  = Array of result curves of data.
C
C-----------------------------------------------------------------------
C
C   State variables at Gauss points
C   -------------------------------
C
C   Note: These variables usually require appropriate updating in
C         routine SWITCH. The positions
C                          XXXXXX( ... ,1)
C         of the above arrays store current values and
C                          XXXXXX( ... ,2)
C         stores the last converged (equilibrium solution).
C         For THKGP, the positions ( ...,0) store the initial
C         Gauss point thicknesses.
C
      LOGICAL LALGVA
      COMMON/STATE /
     1    RALGVA(MRALGV,MTOTG,MELEM,2) ,RSTAVA(MRSTAV,MTOTG,MELEM,2) ,
     2    STRSG(MSTRE,MTOTG,MELEM,2)   ,THKGP(MTOTG,MELEM,0:2)       ,
     3    LALGVA(MLALGV,MTOTG,MELEM,2)
C
C
C       - RALGVA = Real algorithmic variables at Gauss points
C                  (eg. incremental plastic multipliers for
C                  elasto-plastic materials).
C
C       - RSTAVA = Real state variables at Gauss points (depends on
C                  material type).
C
C       - STRSG  = Stresses (or resultant forces/couples) at Gauss
C                  points.
C
C       - THKGP  = Gauss point thicknesses necessary in plane stress
C                  analysis. The contents of THKGP need updating
C                  in routine SWITCH only in large strain analysis.
C                  It is not updated in small strain analysis.
C
C       - LALGVA = Logical algorithmic variables at Gauss points
C                  (eg. flags for multi-vector return mappings for
C                  material models such as Tresca and Mohr-Coulomb).
C
