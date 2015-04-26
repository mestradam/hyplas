************************************************************************
*                                                                      *
*                                                                      *
*          THIS IS THE    H Y P L A S   2.0     README FILE            *
*                        -----------------                             *
*                                                                      *
* HYPLAS is a finite element program for implicit small and large      *
* strain analisys of hyperelastic and elasto-plastic two-dimensional   *
* and axisymmetric solids                                              *
*                                                                      *
* HYPLAS v2.0 is the companion software to the textbook:               *
* EA de Souza Neto, D Peric & DRJ Owen. Computational Methods for      *
* Plasticity: Theory and Applications. Wiley, Chichester, 2008.        *
* (www.wiley.com/go/desouzaneto)                                       *
*                                                                      *
* Copyright (c) 1998-2008  EA de Souza Neto, D Peric, D.R.J. Owen      *
*----------------------------------------------------------------------*
* File last updated: 18 October 2008                                   *
*                                                                      *
* This file belongs in the directory ../HYPLAS_v2.0                    *
************************************************************************
*                                                                      *
*                         I M P O R T A N T                            *
*                                                                      *
*    READ SECTIONS 0 TO 3 OF THIS FILE CAREFULLY BEFORE ATTEMPTING     *
*      TO COMPILE AND RUN THE PROGRAM HYPLAS ON YOUR COMPUTER !!033       *
*                                                                      *
*   THE AUTHORS DO NOT GUARANTEE THAT ANY SUGGESTIONS/INSTRUCTIONS     *
*   GIVEN IN THIS README FILE WILL WORK ON ANY PARTICULAR OPERATING    *
*    SYSTEM. IF YOU DECIDE TO FOLLOW ANY SUGGESTIONS/INSTRUCTIONS      *
*            GIVEN HERE YOU MUST DO SO AT YOUR OWN RISK.               *
*                                                                      *
*                                                                      *
* BUG REPORTS: Please send bug reports to                              *
*                                                                      *
*                            hyplas_v2.0@live.co.uk                    *
*                                                                      *
*              Messages sent to the authors' personal email addresses  *
*              will NOT be answered.                                   *
************************************************************************

 This file contains the following sections:

        0. Copyright statement and disclaimer

           0.(a) Copyright statement

           0.(b) Disclaimer

           0.(c) Conditions of use

        1. Introduction

           1.(a) Note on portability

        2. Compiling and running HYPLAS

           2.(a) Memory requirements

           2.(b) Testing a newly compiled executable

        3. The HYPLAS directory tree

        4. Cross-referencing between the source code and the textbook

        5. HYPLAS error messaging

        6. Further remarks on HYPLAS

************************************************************************


 0. COPYRIGHT STATEMENT AND DISCLAIMER
    ==================================

 0.(a) Copyright statement
       -------------------

 You may only use this program for your own private purposes.
 You are not allowed, in any circumstances, to distribute this program
 (including its source code, executable and any other files related to
 it, either in their original version or any modifications introduced by
 you, the authors or any other party) in whole or in part, either freely
 or otherwise, in any medium, without the prior written consent of the
 copyright holders.


 0.(b) Disclaimer
       ----------

 This program (including its source code, executable and any other files
 related to it) is provided "as is" without warranty of any kind, either
 expressed or implied, including, but not limited to, any implied
 warranties of fitness for purpose.
 In particular, THIS PROGRAM IS BY NO MEANS GUARANTEED TO BE FREE FROM
 ERRORS.
 This program (or any modification incorporated to it by you, the
 authors or any other party) will run entirely at your risk.
 The results produced by this program are in no way guaranteed to be fit
 for any purpose.
 Under no circumstances will the authors/copyright holders be liable to
 anyone for damages, including any general, special, incidental or
 consequential damages arising from the use or inability to use the
 program (including, but not limited to, loss or corruption of data,
 failure of the program to operate in any particular way as well as
 damages arising from the use of any results produced by the program
 for any purpose).


 0.(c) Conditions of use
       -----------------

 You may only use this program if you fully understand and agree with
 the terms of the above disclaimer. You must not use this program if you
 do not agree with or do not understand (fully or in part) these
 conditions of use.



 1. INTRODUCTION
    ============

 HYPLAS is a finite element code for small and large strain analysis
 of hyperelastic and elasto-plastic solids. Most procedures implemented
 in HYPLAS are described in detail in its companion textbook:

 EA de Souza Neto, D Peric & DRJ Owen. Computational Methods for
 Plasticity: Theory and Applications. Wiley, Chichester, 2008
 (www.wiley.com/go/desouzaneto).


 1.(a) Note on Portability
       -------------------

 HYPLAS has been written in standard ANSI FORTRAN 77.
 Currently, the only known (and deliberate) exceptions to the FORTRAN 77
 ANSI standard are the instructions:

             INCLUDE '<include_file_name>'

 used in many routines to include the HYPLAS database files (common
 blocks and global variables), and;

             CALL GETENV('HYPLASHOME',HYPLASHOME)

 used in subroutine "ERRPRT" (file ../HYPLAS_v2.0/src/GENERAL/errprt.f).
 This instruction inquires the name of the system environment variable
 HYPLASHOME and writes it on the character string HYPLASHOME.
 This instruction is NOT part of the ANSI FORTRAN 77 standard, but seems
 to work in most currently available FORTRAN 77 compilers.
 



 2. COMPILING AND RUNNING  H Y P L A S
    ==================================

 The HYPLAS source code is stored in directory  ../HYPLAS_v2.0/src/
 (../HYPLAS_v2.0/ being the current directory) and all its
 subdirectories. To generate an executable file, you just need to
 compile the FORTRAN source files:

  ../HYPLAS_v2.0/src/hyplas.f   and    ../HYPLAS_v2.0/src/*/*.f

 together. We recommend that the executable HYPLAS be stored in the
 directory  ../HYPLAS_v2.0/bin  to which the environment variable
 HYPLASHOME should be set (see below how to set a system environmental
 variable).


 WINDOWS (R) systems
 -------------------

 On Microsoft Windows(R) systems, HYPLAS has been successfully compiled
 using Intel Visual Fortran Compiler(R) integrated with Microsoft Visual
 Studio(R). Here you only need to create a project that contains all
 Fortran source files mentioned above as well as the include files

                    ..\HYPLAS_v2.0\src\*.INC

 On a Windows XP system, the system environment variable HYPLASHOME can
 be set as follows:
 	1. Open a File Manager
	2. Right-click on the "My Computer" icon
	3. Select "Properties" on the drop-down menu
	4. A new window named "System Properties" will pop-up. Here
	   select the "Advanced" tab.
	5. On the "Advanced" tab, click the "Environment Variables"
	   button.
	6. A new window titled "Environment Variables" will pop-up. Here
	   click the button "New" in the "System Variables" section of
	   the window.
	7. A new window will pop-up titled "New System Variable". Here
	   you should fill the fields "Variable name" and "Variable
	   Value", respectively, with HYPLASHOME and the path name (in
	   full) of the directory ..\HYPLAS_v2.0\bin.
	8. Press "OK" on the relevant pop-up windows.
	9. The next time the computer is REBOOTED, this variable will be
	   set to the correct path and HYPLAS should be able to find
	   the error messages file ERROR.RUN if required.


 UNIX/LINUX systems
 ------------------

 In a UNIX/LINUX operating system using a C-shell, for instance, the
 HYPLASHOME environment variable should be set with the command:

                      setenv HYPLASHOME <path>

 where <path> here denotes the full path to the directory
 ../HYPLAS_v2.0/bin.

 To compile HYPLAS (from directory ../HYPLAS_v2.0/src) with a FORTRAN 77
 compiler such as g77, you can use the command:

                 g77 -o ../bin/hyplas hyplas.f */*.f

 Note that the executable file "hyplas" will be stored in the directory
 ../HYPLAS_2.0/bin (i.e. the directory set in the HYPLASHOME environment
 variable).

 Alternatively, you may use the Makefile provided (with suitable
 modifications, if needed) to create the HYPLAS executable.



 IMPORTANT: Before generating a HYPLAS executable, read Sections 2.(a)
            and 2.(b) below.


 2.(a) Memory Requirements
       -------------------

 HYPLAS memory requirements depend on the array dimensioning parameters
 set in files:
                ../HYPLAS_v2.0/src/  ELEMENTS.INC
                                     GLBDBASE.INC
                                     MATERIAL.INC
                                     MAXDIM.INC

 Files ELEMENTS.INC, GLBDBASE.INC and MATERIAL.INC contain parameters
 which are associated with the currently implemented finite elements
 and materials.  DO NOT MODIFY THEM !  unless you are absolutely sure of
 what you are doing (only developers coding new elements or new material
 models/analysis types may need to modify them by changing the existing
 dimensioning parameters and/or including new parameters).

 The ONLY dimensioning file that can be safely modified by the average
 user is the file
                               MAXDIM.INC

 This file contains the array dimensioning parameters related to the
 maximum permissible dimension of problems to be analysed by HYPLAS.
 These parameters include the maximum number of nodes, elements, element
 groups, etc. If necessary,

  CHANGE THESE PARAMETERS TO SUIT YOUR PROBLEM SIZE/MEMORY REQUIREMENTS
 
 before compiling HYPLAS.
 


 2.(b) Testing a newly compiled executable
       -----------------------------------

 After you have successfully compiled the HYPLAS source code and created
 an executable file, the next step is to run some tests to verify that
 HYPLAS is working well. To do this, proceed as follows:

 The directory

                ../HYPLAS_v2.0/book_examples/data_files
 
 contains a series of data files named

                             <name>.dat

 of benchmarked examples described in the companion textbook. The
 corresponding (benchmarked) result files are in the directory

                ../HYPLAS_v2.0/book_examples/result_files

 This directory contains a series of result files named

                             <name>.res

 generated with the current version of HYPLAS on a tested platform.
 All these files have been named such that their names start with the
 textbook section number where the corresponding example is described.
 For instance, files 14_9_2_tresca.dat and 14_9_2_tresca.res refer to
 a problem described in section 14.9.2 of the textbook, and so on.


 To check that HYPLAS is working well on your platform, after compiling
 HYPLAS, run the program HYPLAS for the examples of files <name>.dat
 and compare the newly generated results  <name>.res with their
 benchmarked counterparts (of the same filename) in the result_files
 directory. To run an example, execute HYPLAS and use the keyboard to
 enter the name of the corresponding data file in full (including the
 extension .dat). To compare the benchmarked <name>.res files against
 their newly generated you may proceed as follows:

 	1. On MICROSOFT WINDOWS systems - Here we have successfully used
	   the software "ExamDiff" (the task was made particularly easy
	   by selecting "View" and then the "Show Differences Only"
	   option - this refers to version 1.8 of this software).

	2. On UNIX/LINUX systems - Here we use the "diff" command from a
	   shell window (and set the option to ignore blank spaces). A
           shell script may be used to perform this task automatically
           (including running HYPLAS and checking for result file
           differences) for all benchmarked examples provided.

 IMPORTANT:

 THE ONLY ACCEPTABLE DIFFERENCES BETWEEN A THE NEWLY GENERATED RESULT
 FILES AND THEIR BENCHMARKED COUNTERPARTS ARE THE DIMENSIONING
 PARAMETERS (FROM FILE MAXDIM.INC) USED TO COMPILE THE NEW EXECUTABLE
 (THESE PARAMETERS ARE PRINTED RIGHT AT THE BEGINNING OF THE RESULT
 FILES) AND NUMERICAL DIFFERENCES IN RESULTS DUE TO NUMERICAL
 "ROUNDING-OFF" (THESE ARE VERY SMALL DIFFERENCES THAT DEPEND ON THE
 PRECISION OF ARITHMETIC OPERATIONS IN THE PLATFORM USED).

 ALSO NOTE THAT THE EXAMPLES OF THE COMPANION TEXTBOOK DO NOT COVER ALL
 FEATURES OF HYPLAS. HENCE THIS TEST DOES NOT GUARANTEE THAT EVERYTHING
 IS WORKING PROPERLY.



 3. THE  H Y P L A S  DIRECTORY TREE
    ================================


 3.(a) Summary
       -------


    ../  HYPLAS_v2.0/    bin/

                         book_examples/   data_files/
                                          result_files/

                         man/             html/

                         src/             CRYSTAL/
                                          DAMAGE/
                                          DAMAGED_ELASTIC/
                                          DRUCKER_PRAGER/
                                          ELASTIC/
                                          ELEMENTS/
                                          GENERAL/
                                          MATERIALS/
                                          MATHS/
                                          MOHR_COULOMB/
                                          OGDEN/
                                          TRESCA/
                                          VON_MISES/
                                          VON_MISES_MIXED/


 3.(b) Description
       -----------

 The HYPLAS program directory tree is organised as follows:


     ../HYPLAS_v2.0/ (this directory)

          This is the HYPLAS root directory, where the HYPLAS directory
          tree starts.


     ../HYPLAS_v2.0/bin/

          This directory contains the file ERROR.RUN where most HYPLAS
          error/warning messages are.

          IMPORTANT: the environment variable HYPLASHOME should be set
          to this directory. Otherwise, HYPLAS will not find its
          error/warning messages when required.

          We also recommend that the EXECUTABLE of HYPLAS be stored in
          this directory.


     ../HYPLAS_v2.0/book_examples/

          This directory has the following subdirectories:

          ../HYPLAS_v2.0/book_examples/data_files

          ../HYPLAS_v2.0/book_examples/result_files

          Refer to Section 2.(b) above for further details.


     ../HYPLAS_v2.0/man/

          This is the HYPLAS documentation/manuals directory.

          It contains the following files:

          input_man.txt       - A concise input data manual for HYPLAS
	                        in ASCII format;

          hyplas_calltree.txt - Contains a flowgraph (shows the call
                                tree) of HYPLAS in ASCII-format.
                                Note: calls to function subprograms are
				not included in this flowgraph;

          and the subdirectory:

          ../HYPLAS_v2.0/man/html

               This directory contains the hypertext (HTML) format
               Fortran source code and of manual pages of the entire
               HYPLAS program. Manual pages with descriptions of each
               function/subprogram including their argument list are
               linked to their corresponding HTML-format source code.
               This allows the user the navigate through the HYPLAS
               source code using a web browser. To start at the main
	       program, use your web browser to open the file
	       hyplas.html. This facility should be helpful to those
	       trying to understand the flow of program HYPLAS.


     ../HYPLAS_v2.0/src/

          This directory (and its subdirectories) contains the Fortran
          source code of HYPLAS. The files containing the sources are
          named following the standard practice:

                              <procedure_name>.f

          where <procedure_name> is the name of the FORTRAN procedure
          (subroutine, function subprogram, etc.) whose source code
          is in file <procedure_name>.f. The source code of the HYPLAS
          main program is in file

                                  hyplas.f

          and the HYPLAS database (COMMON blocks, array dimensioning
          parameters and other global parameters) is coded in the
          "include files"
                               ELEMENTS.INC
                               GLDBASE.INC
                               MATERIAL.INC
                               MAXDIM.INC

          in this directory. In addition, this directory contains a file
	  named "Makefile" (UNIX-LINUX Release only) which may be used
	  for compiling and linking HYPLAS in UNIX/LINUX systems.
	  
	  The subdirectories of ../HYPLAS_v2.0/src are as follows:

          ../HYPLAS_v2.0/src/CRYSTAL
               Contains the source code of all procedures related to the
               finite strain single crystal plasticity model implemented
               in HYPLAS.

          ../HYPLAS_v2.0/src/DAMAGE
               Source files of the procedures related to the Lemaitre
               ductile damage model implementation.
            
          ../HYPLAS_v2.0/src/DAMAGED_ELASTIC
               Source files of the procedures related to the damaged
               elasticity model with crack closure effect.
            
          ../HYPLAS_v2.0/src/DRUCKER_PRAGER
               Source files of the procedures related to the implemented
               Drucker-Prager plasticity model.
            
          ../HYPLAS_v2.0/src/ELASTIC
               Source files of the procedures related to the linear
               elasticity model (Hencky model under large strains)
               implemented.
            
          ../HYPLAS_v2.0/src/ELEMENTS
               Source files of the element interfaces and
               element-related procedures.
            
          ../HYPLAS_v2.0/src/GENERAL
               Source files of general procedures.
            
          ../HYPLAS_v2.0/src/MATERIALS
               Source files of the material interfaces.

          ../HYPLAS_v2.0/src/MATHS
               Source files of the mathematical procedures.

          ../HYPLAS_v2.0/src/MOHR_COULOMB
               Source files of the procedures related to the implemented
               Mohr-Coulomb plasticity model.

          ../HYPLAS_v2.0/src/OGDEN
               Source files of the procedures related to the implemented
               Ogden hyperelasticity model.

          ../HYPLAS_v2.0/src/TRESCA
               Source files of the procedures related to the implemented
               Tresca plasticity model.

          ../HYPLAS_v2.0/src/VON_MISES
               Source files of the procedures related to the implemented
               von Mises plasticity model with isotropic hardening.

          ../HYPLAS_v2.0/src/VON_MISES_MIXED
               Source files of the procedures related to the implemented
               von Mises plasticity model with mixed
               isotropic/kinematic hardening.




 4. CROSS-REFERENCING BETWEEN THE SOURCE CODE AND THE TEXTBOOK
    ==========================================================

 Many references are made in the textbook to various subprograms of
 HYPLAS. These are usually made when a particular procedure described in
 the text is implemented in the program. The reader should refer to the
 textbook index.

 Also, a substantial number of comment lines have been added to the
 source code of HYPLAS with reference to sections, figures, boxes, etc
 of the textbook related to the part of the code in question. Such
 references are usually displayed after the word "REFERENCE:" (in
 capitals) on commented lines. Searching for this word will take you
 to the line of code where the particular routine has a reference to
 the textbook. NOTE: Occasional references to other textbooks/journal
 papers are also made following the word "REFERENCE:" on commented
 lines.




 5. HYPLAS ERROR MESSAGING
    ======================

 Most error/warning messages issued by HYPLAS are in the ASCII-format
 file ERROR.RUN (kept in the HYPLASHOME directory - ../HYPLAS_v2.0/bin).
 All such error/warning messages have an identification code (e.g.
 ED0015) which is printed both to the standard output (this is usually
 the computer screen) and to the relevant results file. If you wish to
 find where in the source code a particular message is being issued,
 then perform a search for the corresponding message identification code
 in the entire source code of HYPLAS.




 6. FURTHER REMARKS ON HYPLAS
    =========================

 6.(a) Program efficiency

 THIS SECTION IS OF INTEREST ONLY TO THOSE WANTING TO MAKE HYPLAS RUN
 FASTER.

 It is particularly stressed in the textbook that this program has not
 been designed having efficiency in mind (refer to Section 5.1.2 of the
 textbook). Its structure has been designed mainly to illustrate in a
 relatively clear manner the computer implementation of the techniques
 and algorithms described in the text, with a particular view to the
 implementation of solid constitutive models and finite elements.

 For those who are especially interested in the speed of the code, there
 are a few tips that could help in this direction. Unfortunately, these
 involve modifications to the source code which is probably most
 appropriate to readers with a good level of experience in finite
 element programming. To those with this particular interest, we can
 suggest the following:


 (i) The use of faster linear solvers

     This is probably the change that would result in a greater gain in
     efficiency. The Frontal Method adopted in subroutine FRONT (file
     ../HYPLAS_v2.0/src/GENERAL/front.f) has been designed originally to
     save memory (back in the days when computer memory was severely
     limited). There are currently a vast number of methodologies which
     focus on speeding up the linear solution, in addition to reducing
     memory storage requirements (which is a particularly important
     issue in the solution of large scale problems). Some of these are
     extensions/refinements of the original Frontal solver. We remark
     that a number of such procedures (with their respective source
     codes) are available (conditions may apply) from the LAPACK (Linear
     Algebra PACKage - http://www.netlib.org/lapack) repository or from
     the HSL Library (http://www.cse.cse.scitech.ac.uk/nag/hsl). For the
     reader interested in gaining speed, we would recommend the
     replacement of the existing solver of FRONT by a faster one. We
     remark though that this is a substantial programming task.
     
     Another aspect here is the fact that computing times in FRONT are
     directly linked to the frontwidth of the system which, in the
     present version of HYPLAS is fixed and depends, for a given mesh,
     on how the degrees of freedom are numbered (node numbering). The
     incorporation of a frontwidth optimiser (which re-numbers the
     degrees of freedom in order to minimise the frontwidth) in FRONT
     could produce some good savings in computing times. Such savings
     become particularly noticeable in larger problems where the
     original node numbering produces an excessively large frontwidth.

 
 (ii) Material-specific computations
 
     The issues pointed out here affect only the computing times for
     specific material models and are expected to have a much lower
     impact in overall speed than the linear solver issue discussed
     above.

     Some of the material model-specific computations carried out in
     HYPLAS could be made a bit faster. For example, for isotropic
     models whose stress update is carried out in the principal stress
     space (such as the Tresca and Mohr-Coulomb models - see routines
     SUTR and SUMC,  files ../HYPLAS_v2.0/src/TRESCA/sutr.f  and
     ../HYPLAS_v2.0/MOHR_COULOMB/sumc.f, respectively) the spectral
     decomposition of the stress in carried out in the state update
     update routine and then repeated in the corresponding routine for
     computation of the consistent tangent operator (refer to files
     ../HYPLAS_v2.0/src/TRESCA/cttr.f  and
     ../HYPLAS_v2.0/src/MOHR_COULOMB/ctmc.f, respectively, for the
     Tresca and Mohr-Coulomb plasticity models).
     Some savings in computing time can be achieved here by storing
     the stress eigenprojection tensors (these can be stored as state
     variables) during the execution of the state updating and then
     retrieving them later for use in the computation of the consistent
     tangent operator. This change can be incorporated to the code
     relatively easily.

     The computation of the exponential map and is derivative for the
     single crystal plasticity model (routines EXPMAP, file
     ../HYPLAS_v2.0/src/CRYSTAL/expmap.f  and DEXPMP, file
     ../HYPLAS_v2.0/src/CRYSTAL/dexpmp.f) is carried out in three
     dimensions (these routines have been adapted from an earlier
     three-dimensional code). To improve efficiency, these can be
     adapted to work only in two-dimensional problems by removing the
     unnecessary operations related to the third dimension.


 6.(b) Output of nodal averaged values

     The reader should be aware that the way in which nodal averaged
     values of stresses and other variables are calculated in HYPLAS is
     very basic (and rudimentary). This feature of the program is made
     available only to help those interested in producing contour plots,
     etc from results presented in HYPLAS result files and should be
     useful in many circumstances of interest. This facility has in fact
     been used in producing many of the figures presented in the
     textbook.
     But note, for example, that the values of incremental plastic
     multipliers for plasticity models may take (inadmissible) negative
     values when extrapolated from Gauss-point to nodes and averaged.
     We remark that more sophisticated and refined techniques of
     transferring Gauss point values of variables to nodal points and
     obtaining the corresponding smoothed field are available in the
     current literature. These fall outside the scope of the companion
     textbook of HYPLAS.
