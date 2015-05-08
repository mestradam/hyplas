TITLE
Archivo de datos para HYPLAS generado por GiD
*Set var TRIA = nelem(triangle)
*Set var QUAD = nelem(quadrilateral)
*intformat "%i"

ANALYSIS_TYPE *\
*if(strcmp(GenData(Analisys_type),"PLANE_STRESS")==0)
1 (PLANE STRESS)
*elseif(strcmp(GenData(Analisys_type),"PLANE_STRAIN")==0)
2 (PLANE STRAIN)
*elseif(strcmp(GenData(Analisys_type),"AXISYMMETRIC")==0)
3 (AXISYMMETRIC)
*endif

LARGE_STRAIN_FORMULATION *\
*if(strcmp(GenData(Kinematics),"SMALL_STRAIN")==0)
OFF (SMALL STRAIN)
*else
ON (LARGE STRAIN)
*endif

SOLUTION_ALGORITHM *\
*if(strcmp(GenData(Solution_algor),"INITIAL_STIF")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
1 (INITIAL_STIF)
*else
-1 (INITIAL_STIF + ARCLENGTH_MET)
*endif
*elseif(strcmp(GenData(Solution_algor),"NEWTON_TANG_STIF")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
2 (NEWTON_TANG_STIF)
*else
-2 (NEWTON_TANG_STIF + ARCLENGTH_MET)
*endif
*elseif(strcmp(GenData(Solution_algor),"MOD_NEWTON_KT1")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
3 (MOD_NEWTON_KT1)
*else
-3 (MOD_NEWTON_KT1 + ARCLENGTH_MET)
*endif
*elseif(strcmp(GenData(Solution_algor),"MOD_NEWTON_KT2")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
4 (MOD_NEWTON_KT2)
*else
-4 (MOD_NEWTON_KT2 + ARCLENGTH_MET)
*endif
*elseif(strcmp(GenData(Solution_algor),"SEC_NEWTON_INI_STIF")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
5 (SEC_NEWTON_INI_STIF)
*else
-5 (SEC_NEWTON_INI_STIF + ARCLENGTH_MET)
*endif
*elseif(strcmp(GenData(Solution_algor),"SEC_NEWTON_KT1")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
6 (SEC_NEWTON_KT1)
*else
-6 (SEC_NEWTON_KT1 + ARCLENGTH_MET)
*endif
*elseif(strcmp(GenData(Solution_algor),"SEC_NEWTON_KT2")==0)
*if(strcmp(GenData(Arclength_met),"NO")==0)
7 (SEC_NEWTON_KT2)
*else
-7 (SEC_NEWTON_KT2 + ARCLENGTH_MET)
*endif
*endif

ELEMENT_GROUPS *nmats
*loop materials
*intformat "%i"
*matnum() 1 *matnum()
*end materials

ELEMENT_TYPES 1
1 *\
*if(nnode==3)
TRI_3
1 GP
*elseif(nnode==4)
QUAD_4
4 GP
*elseif(nnode==8)
QUAD_8
4 GP
*endif

MATERIALS *nmats
*Set var PENN=0
*loop materials
*intformat "%i"
*realformat "%g"
*if(strcmp(MatProp(MODEL),"ELASTIC")==0)
*matnum() ELASTIC
*MatProp(Density,real)
*MatProp(Young_modulus,real) *MatProp(Poisson's_ratio,real)
*elseif(strcmp(MatProp(MODEL),"VON_MISES")==0)
*matnum() VON_MISES
*MatProp(Density,real)
*MatProp(Young_modulus,real) *MatProp(Poisson's_ratio,real)
2
0.000 *MatProp(yield_stress_ep0,real)
1.000 *MatProp(yield_stress_ep1,real)
*endif
*end materials

*if(strcmp(GenData(Analisys_type),"PLANE_STRESS")==0)
THICKNESS UNIFORM
*GenData(Thickness)
*endif

*realformat "%15.5f"
*intformat "%7i"
ELEMENTS *nelem
*Set elems(triangle)
*add elems(quadrilateral)
*loop elems
    *loopvar *elemsmat *elemsConec
*end elems

NODE_COORDINATES *npoin   CARTESIAN
*loop nodes
  *NodesNum *NodesCoord
*end nodes

*Set Cond Point-Constraints *nodes *CanRepeat
*if(CondNumEntities(int)>0)
NODES_WITH_PRESCRIBED_DISPLACEMENTS *CondNumEntities(int)
*loop nodes *OnlyInCond 
*format "%5i%1i%1i%f%f%f"
*NodesNum *cond(1,int)*cond(3,int)  *cond(2,real) *cond(4,real) 0.0
*end
*endif

*Set Cond Point-Load *nodes
*realformat "%15.5f"
*intformat "%7i"
*if(CondNumEntities>0)
LOADINGS  POINT
POINT_LOADS *CondNumEntities
*loop nodes *OnlyInCond
*format "%5i%f%f"
*NodesNum *cond(1) *cond(2)
*end
*endif

*Set Cond Face-Load    *elems  *CanRepeat
*if(CondNumEntities>0)
LOADINGS  EDGE
EDGE_LOADS *CondNumEntities
*loop elems *OnlyInCond
*if(nnode==8)
*format "%5i%5i%5i"
*elemsnum() 3 *globalnodes
*format "%f%f%f%f%f%f"
*cond(2) *cond(4) *cond(2) *cond(4) *cond(2) *cond(4)
*else
*format "%5i%5i%5i"
*elemsnum() 2 *globalnodes
*format "%f%f%f%f"
*cond(2) *cond(4) *cond(2) *cond(4)
*endif
*end
*endif

*if(strcmp(GenData(Gravity_load),"NONE")!=0)
LOADINGS  GRAVITY
GRAVITY_LOAD
0.0 9.81
*endif

INCREMENTS 1
*format "%10.5f%g%5i%i%i%i%i%i"
*GenData(Incremental_load_factor) *GenData(Solution_tolerance) *\
*GenData(Maximum_number_of_iterations) *\
*GenData(Nodal_displacements) *GenData(Reaction_forces) *\
*GenData(Stress_and_state_var_at_GP) *GenData(Stress_and_state_var_at_nodes) *\
*GenData(Restart_file)

