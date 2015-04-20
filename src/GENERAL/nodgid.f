      SUBROUTINE NODGID(IINCS  ,IELEM  ,IGAUSP, OUTDA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C Hyplas database
      INCLUDE '../MAXDIM.INC'
      INCLUDE '../MATERIAL.INC'
      INCLUDE '../ELEMENTS.INC'
      INCLUDE '../GLBDBASE.INC'
C
      DIMENSION
     1    RALGN(MRALGV,MNODE),RSTAN(MRSTAV,MNODE),STRSN(MSTRE,MNODE) ,
     2    THKN(MNODE)
      DIMENSION
     1    STRSA(MSTRE,MGRUP,MPOIN)     ,PSTRA(3)                     ,
     2    RALGA(MRALGV,MGRUP,MPOIN)    ,RSTAA(MRSTAV,MGRUP,MPOIN)    ,
     3    THKA(MGRUP,MPOIN), DEFVEC(MRSTAV)
      DATA R0   ,R1   /0.0D0,1.0D0/
C***********************************************************************
C PRINTS AVERAGED (SMOOTHED) NODAL STRESSES AND OTHER STATE AND
C ALGORITHMIC VARIABLES. THE SMOOTHED VARIABLE AT EACH NODE IS OBTAINED
C BY EXTRAPOLATING THE VALUE OF THE VARIABLE FROM THE GAUSS POINTS TO
C THE NODE AND THEN AVERAGING THE VALUES OBTAINED FROM ALL ELEMENTS
C SHARING THAT NODE FOR GiD OUTPUT FILE.
C
C REFERENCE: E Hinton & JS Campbel. Local and global smoothing of
C            discontinuous finite element functions using a least
C            squares method. Int. J. Num. Meth. Engng., 8:461-480, 1974.
C            E Hinton & DRJ Owen. An introduction to finite element
C            computations. Pineridge Press, Swansea, 1979.
C***********************************************************************
C
C      Format to GiD Postprocess
C	 =========================
C
 1092 FORMAT('Result "STRESS-MAT-',I1,'" "LOAD ANALYSIS" ',I3,
     1       ' Vector OnNodes',/,
     2       'ComponentNames "S-XX", "S-YY", "S-XY" ',
     3       /,'Values')
 1093 FORMAT('Result "STRESS-MAT-',I1,'" "LOAD ANALYSIS" ',I3,
     1       ' Vector OnNodes',/,
     2       'ComponentNames "S-XX", "S-YY", "S-XY", "S-ZZ" ',
	3       /,'Values')
 1094 FORMAT('Result "STRESS-MAT-',I1,'" "LOAD ANALYSIS" ',I3,
     1       ' Vector OnNodes',/,
     2       'ComponentNames "S-RR", "S-ZZ", "S-XY", "S-RZ" ',
	3       /,'Values')
 1095 FORMAT('Result "STRESS-MAT-',I1,'" "LOAD ANALYSIS" ',I3,
     1       ' Vector OnNodes',/,
     2       'ComponentNames "S-XX", "S-RR", "S-XY", "S-XR" ',
     3       /,'Values')
 1097 FORMAT('Result "PRINCIPAL-STRESS-MAT-',I1,'" "LOAD ANALYSIS" ',I3,
     1       ' Vector OnNodes',/,
     2       'ComponentNames "S-MAX", "S-MIN", "S-MED"',
     3       /,'Values')
 1096 FORMAT(I5,2X,4G15.6)
 1098 FORMAT(I5,2X,3G15.6)
 1099 FORMAT('End values',/)
 1220 FORMAT(2G15.6)
C
C      Loop over element groups
C      ========================
C
      DO 90 IGRUP=1,NGRUP
        DO 10 IPOIN=1,NPOIN
          CALL RVZERO(STRSA(1,IGRUP,IPOIN),MSTRE)
          CALL RVZERO(PSTRA,3)
          CALL RVZERO(RSTAA(1,IGRUP,IPOIN),MRSTAV)
          CALL RVZERO(RALGA(1,IGRUP,IPOIN),MRALGV)
          THKA(IGRUP,IPOIN)=R0
   10   CONTINUE
C
C      Loop over elements
C      ==================
C
        DO 70 IELEM=1,NELEM
          LGRUP=IGRPID(IELEM)
          IF(LGRUP.NE.IGRUP)GOTO 70
          IELIDN=IELTID(IGRUP)
          NNODE =IELPRP(3,IELIDN)
          NGAUSP=IELPRP(4,IELIDN)
C      Extrapolate stresses and other state and algorithmic variables from
C      gauss points to nodes
          IPOS=NGAUSP*NDIME+NGAUSP+1
          CALL EXTNOD
     1(   RELPRP(IPOS,IELIDN),
     2    STRSG(1,1,IELEM,1) ,STRSN    ,MSTRE     ,NGAUSP     ,NNODE   )
          CALL EXTNOD
     1(   RELPRP(IPOS,IELIDN),
     2    RSTAVA(1,1,IELEM,1),RSTAN    ,MRSTAV    ,NGAUSP     ,NNODE   )
          CALL EXTNOD
     1(   RELPRP(IPOS,IELIDN),
     2    RALGVA(1,1,IELEM,1),RALGN    ,MRALGV    ,NGAUSP     ,NNODE   )
C      Thickness (for large strains in plane stress only)
          IF(NLARGE.EQ.1.AND.NTYPE.EQ.1)CALL EXTNOD
     1(   RELPRP(IPOS,IELIDN),
     2    THKGP(1,IELEM,1)   ,THKN     ,1         ,NGAUSP     ,NNODE   )
C      Nodal averaging
          DO 60 INODE=1,NNODE
            IPOIN=IABS(LNODS(IELEM,INODE))
            R1DVAL=R1/DBLE(NVALEN(IPOIN,IGRUP))
            DO 30 ISTRE=1,MSTRE
              STRSA(ISTRE,IGRUP,IPOIN)=STRSA(ISTRE,IGRUP,IPOIN)+
     1                                 STRSN(ISTRE,INODE)*R1DVAL
   30       CONTINUE
            DO 40 INTV=1,MRSTAV
              RSTAA(INTV,IGRUP,IPOIN)=RSTAA(INTV,IGRUP,IPOIN)+
     1                                RSTAN(INTV,INODE)*R1DVAL
   40       CONTINUE
            DO 50 IALGV=1,MRALGV
              RALGA(IALGV,IGRUP,IPOIN)=RALGA(IALGV,IGRUP,IPOIN)+
     1                                 RALGN(IALGV,INODE)*R1DVAL
   50       CONTINUE
            IF(NLARGE.EQ.1.AND.NTYPE.EQ.1)
     1               THKA(IGRUP,IPOIN)=THKA(IGRUP,IPOIN)+
     2                                 THKN(INODE)*R1DVAL
   60     CONTINUE
   70   CONTINUE
C
C      Output average stresses to results GiD file (common to all materials)
C      =================================================================
C
C      GiD postprocess - Output Average Current Stresses
C      =================================================
C
	  DO 80 IPOIN=1,NPOIN
		IF(IPOIN.EQ.1)THEN
			IF(NTYPE.EQ.1)THEN
				LSTRE=3
				WRITE(22,1092)IGRUP,IINCS
			ELSEIF(NTYPE.EQ.2)THEN
				LSTRE=4
				WRITE(22,1093)IGRUP,IINCS
			ELSEIF(NTYPE.EQ.3)THEN
				LSTRE=4
				IF(NAXIS.EQ.1)THEN
					WRITE(22,1094)IGRUP,IINCS
				ELSE
					WRITE(22,1095)IGRUP,IINCS
				ENDIF
			ENDIF
		END IF
		IF(NVALEN(IPOIN,IGRUP).EQ.0)GOTO 80
		WRITE(22,1096)IPOIN,(STRSA(I,IGRUP,IPOIN),I=1,LSTRE)
   80   CONTINUE
        WRITE(22,1099)


C
C      Nodal Plot of values for the Curves - Stresses and Strains
C	 ==========================================================
C
	DO 72 ICURV=1,NCURV
	  LPOIH=LCURV(ICURV,1)
	  LDOFH=LCURV(ICURV,2)
	  LPOIV=LCURV(ICURV,3)
	  LDOFV=LCURV(ICURV,4)
	  LGAUX=LCURV(ICURV,5)
        LGAUY=LCURV(ICURV,6)
C
C	Horizontal Axis data - Curves
C	=============================
C	  
	SELECT CASE(LDOFH)
	CASE(1)
		NGASH=NDOFN*(LPOIH-1)+LDOFH
		HAXIS=TDISP(NGASH)
	CASE(2)
		NGASH=NDOFN*(LPOIH-1)+LDOFH
		HAXIS=TDISP(NGASH)
	CASE(3)
	    DO 74 IVFIX=1,NVFIX
            IF(NOFIX(IVFIX).EQ.LPOIH)THEN
			 LVFIX=IVFIX
	      ENDIF
   74     CONTINUE
	   HAXIS=TREAC(LVFIX,LDOFH-2)
	CASE(4)
	    DO 75 IVFIX=1,NVFIX
            IF(NOFIX(IVFIX).EQ.LPOIH)THEN
			 LVFIX=IVFIX
	      ENDIF
   75     CONTINUE
	   HAXIS=TREAC(LVFIX,LDOFH-2)
	CASE(5)
 		NGASH=NDOFN*(LPOIH-1)+LDOFH
		HAXIS=STRSA(LDOFH-4,IGRUP,LPOIH)
	CASE(6)
		NGASH=NDOFN*(LPOIH-1)+LDOFH
		HAXIS=STRSA(LDOFH-4,IGRUP,LPOIH)
	CASE(7)
		HAXIS=RSTAA(1,IGRUP,LPOIH)
C		DEFVEC=RSTAA(1,IGRUP,LPOIH)
C		HAXIS=DEFVEC(LDOFH-6)
	CASE(8)
		HAXIS=RSTAA(2,IGRUP,LPOIH)
C		DEFVEC=RSTAA(1,IGRUP,LPOIH)
C		HAXIS=DEFVEC(LDOFH-6)
	CASE(9)
		HAXIS=STRSG(1,LGAUX,LPOIH,1)
	CASE(10)
		HAXIS=STRSG(2,LGAUX,LPOIH,1)
	CASE(11)
		HAXIS=RSTAVA(1,LGAUX,LPOIH,1)
	CASE(12)
		HAXIS=RSTAVA(2,LGAUX,LPOIH,1)
	CASE DEFAULT
C	ESCRIBIR UN ERROR PARA EL SELECTOR
	END SELECT
C
C	Vertical Axis data - Curves
C	===========================
C  
	SELECT CASE(LDOFV)
	CASE(1)
		NGASH=NDOFN*(LPOIV-1)+LDOFV
		VAXIS=TDISP(NGASH)
	CASE(2)
		NGASH=NDOFN*(LPOIV-1)+LDOFV
		VAXIS=TDISP(NGASH)
	CASE(3)
	    DO 76 IVFIX=1,NVFIX
            IF(NOFIX(IVFIX).EQ.LPOIV)THEN
			 LVFIX=IVFIX
	      ENDIF
   76     CONTINUE
	   VAXIS=TREAC(LVFIX,LDOFV-2)
	CASE(4)
	    DO 77 IVFIX=1,NVFIX
            IF(NOFIX(IVFIX).EQ.LPOIV)THEN
			 LVFIX=IVFIX
	      ENDIF
   77     CONTINUE
	   VAXIS=TREAC(LVFIX,LDOFV-2)
	CASE(5)
 		NGASH=NDOFN*(LPOIV-1)+LDOFV
		VAXIS=STRSA(LDOFV-4,IGRUP,LPOIV)
	CASE(6)
		NGASH=NDOFN*(LPOIV-1)+LDOFV
		VAXIS=STRSA(LDOFV-4,IGRUP,LPOIV)
	CASE(7)
		VAXIS=RSTAA(1,IGRUP,LPOIV)
C		DEFVEC=RSTAA(1,IGRUP,LPOIV)
C		VAXIS=DEFVEC(LDOFV-6)
	CASE(8)
		VAXIS=RSTAA(2,IGRUP,LPOIV)
C		DEFVEC=RSTAA(1,IGRUP,LPOIV)
C		VAXIS=DEFVEC(LDOFV-6)
	CASE(9)
		VAXIS=STRSG(1,LGAUY,LPOIV,1)
	CASE(10)
		VAXIS=STRSG(2,LGAUY,LPOIV,1)
	CASE(11)
		VAXIS=RSTAVA(1,LGAUY,LPOIV,1)
	CASE(12)
		VAXIS=RSTAVA(2,LGAUY,LPOIV,1)
	CASE DEFAULT
C	ESCRIBIR UN ERROR PARA EL SELECTOR
	END SELECT
	
	WRITE(31,1220)HAXIS,VAXIS
   72 CONTINUE

C
C      GiD postprocess - Output Average Principal Stresses
C      ================================================= 
C
	  DO 100 IPOIN=1,NPOIN
		IF(IPOIN.EQ.1.AND.OUTDA.EQ.2)THEN
		  WRITE(22,1097)IGRUP,IINCS
	    ENDIF
          CALL PRINC2(PSTRA,STRSA(1,IGRUP,IPOIN))
          WRITE(22,1098)IPOIN,(PSTRA(I),I=1,3)
  100   CONTINUE
        WRITE(22,1099)
C
C      GiD postprocess - Average Internal and State Variables
C      ======================================================
C
	OUTDA=3
	  DO 110 IPOIN=1,NPOIN
           CALL MATIOR
     1(   NTYPE      ,IPROPS(1,MATTID(IGRUP))  ,RALGA(1,IGRUP,IPOIN) ,
     2    RPROPS(1,MATTID(IGRUP))              ,RSTAA(1,IGRUP,IPOIN) ,
     3    STRSA(1,IGRUP,IPOIN)   ,IPOIN,   IINCS,   IGAUSP, OUTDA)
  110   CONTINUE
        WRITE(22,1099)
	OUTDA=2	
   90 CONTINUE
      RETURN
      END
