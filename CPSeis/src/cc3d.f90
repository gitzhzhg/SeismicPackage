!<CPS_v1 type="PROCESS"/>
!!------------------------------- cc3d.f90 ---------------------------------!!
!!------------------------------- cc3d.f90 ---------------------------------!!
!!------------------------------- cc3d.f90 ---------------------------------!!


!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S             
!
! Name       : CC3D
! Category   : statics
! Written    : 1996-05-25   by: Greg Lazear
! Revised    : 2010-04-12   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : 3D composite correlation statics.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! CC3D is a surface consistent residual statics process that uses a generalized
! composite and correlate algorithm to estimate static shifts.  A detailed
! description of the CC3D algorithm is found in the Algorithm Description for
! Developers section.  A simplified version follows.
!
!       1.  Traces are sorted into super-CMPs, each consisting of COMP_X *
!       COMP_Y actual CMPs.  Loop over steps 2 - 4 for each super-CMP.
!
!       2.  Apply previously derived shifts and composite traces to: super-CMP
!       stacks and partial stacks (common source, common receiver, common CMP
!       and common offset), with number of types of partial stacks determined
!       by TERMS.  The common source stacked trace, common receiver stacked
!       trace, etc., constitute statistical expectations of those individual
!       gathers.
!
!       3.  Correlate each partial stack against the super-CMP stack (with the
!       partial stack removed) and pick the correlations to obtain an initial
!       IMS-like estimate of the static for that term.
!
!       4.  Stack the correlations with other correlations of the same term and
!       sum the IMS-like shifts into an average for each term.
!
!       5.  Pick the stacked correlations to get a FISH-like static estimate,
!       combine this estimate with the IMS-like estimate, weighed according
!       to the WEIGHT parameter, and add the statics estimates of this
!       iteration to those of the previous iterations.
!
!       6.  Iterate steps 2 - 5 NUM_ITER times.
!
!       7.  Print results in the .rpt file, save statics files and optionally
!       write the final stacked correlations to bytefiles.
!
!
! Source and Receiver Locations
!
! Source locations are designated by header words HDR_SX and HDR_SY;
! receiver locations are designated by header words HDR_RX and HDR_RY.
! These header words define a source grid and a receiver grid which determine
! the size of the source and receiver statics files.  Traces that fall outside
! either the source or the receiver grid will be dropped.
!
! The header words SX_INC, SY_INC, RX_INC, and RY_INC specify increments of the
! source and receiver headers for occupied source and receiver locations.  Each
! actual source and receiver location found in trace headers will be binned
! into the nearest location specified by these parameters for purposes of
! calculating statics.
!
! This index saves memory when the grid is only sparsely occupied by sources or
! receivers.
!
!
! CMP Locations and Super-CMPs
!
! CMP locations are designated by header words 7 and 8 which are the stack-bin
! grid coordinates.  Traces are dropped if they fall outside of this grid.
! This grid is extended slightly, if necessary, to make whole super-bins
! of COMP_X and COMP_Y CMPs.  Traces are dropped if the fold in a super-bin
! exceeds FOLD_MAX * COMP_X * COMP_Y traces.
!
! The COMP_X and COMP_Y parameters are in "counting" units, meaning a value
! of seven will composite seven CMPs, independent of their grid increments. Thus
! CMPs with consecutive header 7 values of (120, 120.5, 121, 121.5, 122) would
! be composited when COMP_X=5, even though their range in header word 7 units
! is 2.
!
!
! Disk Allocation
!
! This program is an all-trace process, and uses the CMP grid specification and
! FOLD_MAX to allocate disk space for the traces.  It is important not to make
! FOLD_MAX unnecessarily large, because this could waste a lot of disk space.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Choice of Coordinates
!
! CMP locations are designated by the CMP grid coordinates (header words 7 and
! 8).  Users must choose header words to designate source and receiver
! locations as follows:
!
! Source Coordinate Options     Source Header Words
!
!       Shot Profile Number           9
!       Sequential Ground Position   46
!       Shotpoint and Line Number    29 and 26
!       Grid Coordinates             33 and 34
!
! Receiver Coordinate Options   Receiver Header Words
!
!       Sequential Ground Position   47
!       Shotpoint and Line Number    28 and 27
!       Grid Coordinates             35 and 36
!
! Source and receiver coordinate choices are independent of each other.
!
!
! Coordinate Recommendations
!
! Normally, if sequential ground position header words have been set in the
! input dataset, the ground position is the best choice for source and receiver
! coordinates since static and byte files are smaller and static determination
! may be more acurate.  However, if there is more than one source pop per
! source location, then shot profile number should be used for the source
! coordinate.
!
! If sequential ground position header words have not been set in the input
! dataset and the datset is relatively small then shotpoint or line number
! coordinates may be the next best choice.  Grid coodinates are another
! possible choice with these drawbacks:
!
!       1.  Grid coordinates may not be densely populated making internal
!       calculations slower, disk use higher and static and byte files larger.
!
!       2.  Statics are calculated only at regular grid intersections.  If
!       sources and receivers are not always located exactly on the
!       intersections then applying the statics files will probably produce
!       somewhat inaccurate results.
!
! Normally, grid coordinates are not a good choice for large datasets
! because this usually causes the static files to be extremely large and
! mostly filled with nils.
!
!-------------------------------------------------------------------------------
!                        SUMMARY OF PARAMETERS
!
! Correlation window parameters:   Standard laterally varying time window
!                                  parameters in the LATWIN primitive.
!
! File output parameters:          PATH_SRC, PATH_REC,
! File output parameters:          HDR_SRC, HDR_REC,
!                                  CORR_FILES, NO_DEAD
!
! Printing parameters:             OPT_PRINT
!
! Static solution parameters:      MAX_STATIC, NUM_ITER, WEIGHT, TERMS,
!                                  PWR_CC, TAPER, UPDATE, OFF_WID
!
! Source ground position ranges:   HDR_SX, SX_INIT, SX_INC, SX_LAST, SX_TOT
!                                  HDR_SY, SY_INIT, SY_INC, SY_LAST, SY_TOT
!
! Receiver ground position ranges: HDR_RX, RX_INIT, RX_INC, RX_LAST, RX_TOT
!                                  HDR_RY, RY_INIT, RY_INC, RY_LAST, RY_TOT
!
! CMP parameters:           COMP_X, CMPX_INIT, CMPX_INC, CMPX_LAST, CMPX_TOT
!                           COMP_Y, CMPY_INIT, CMPY_INC, CMPY_LAST, CMPY_TOT
!                           FOLD_MAX
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! This process is an all-trace (loop-splitting) process.
! This process allows traces to be input in gathers or one at a time.
! This process allows traces to be input in any order.
! Traces should have been expanded with XP or MVXP or equivalent.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! If DEAD_END = YES, this process does not output any traces.
! If DEAD_END = NO, this process outputs the same traces it received,
!  one at a time, shifted by the statics solution, in the same order it
!  received them.
! DEAD_END is always forced to YES for parallel processing (more than 1 CPU).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUM_CPUS  number of CPUs in a parallel job        (job data parameter)
! NUMTR     max number of traces input/output       input unused; set to 1.
! GATHERED  whether traces are a legitimate gather  input unused; set to false.
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>
 
 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#       Description                       Action taken
! ----       -----------                       ------------
!  6         offset                            used but not changed
!  7         CMP X grid coordinate             used but not changed
!  8         CMP Y grid coordinate             used but not changed
! HDR_SX     source X ground position          used but not changed
! HDR_SY     source Y ground position          used but not changed
! HDR_RX     source X ground position          used but not changed
! HDR_RY     source Y ground position          used but not changed
! WIN_HDR_X  first coord of window location    used but not changed
! WIN_HDR_Y  second coord of window location   used but not changed
!  2         top mute index                    used (updated if DEAD_END is NO)
! 64         bottom mute index                 used (updated if DEAD_END is NO)
! 43         cumulative residual static             (updated if DEAD_END is NO)
! 25         largest absolute value                     (set if DEAD_END is NO)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 40. 2010-04-12 Stoeckley  Move call latwin_update to before the call to
!                            cc3d_calculate_dependencies so nwins will be set
!                            correctly when update is called only once.
! 39. 2008-12-18 Stoeckley  Modify header word info for cpswrap in SeisSpace.
! 38. 2008-11-11 Stoeckley  Add header word info for cpswrap in SeisSpace.
! 37. 2007-07-12 Stoeckley  Improve layout of header word, bins, and increments
!                            menu.
! 36. 2007-07-03 Stoeckley  Move call latwin_update to after the pc_put calls
!                            so the first tab will be the main tab in SeisSpace;
!                            move header words and their bins and increments to
!                            a separate tab for SeisSpace, and double up some
!                            of them two per line.
!035. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
! 34. 2005-12-22  Stoeckley  Initialize error flag in cc3d_form_stacks to
!                             allow for empty super-cmp.
! 33. 2005-10-24  Stoeckley  Extensively redesigned to make parallel code
!                             more efficient.
! 32. 2005-01-25  Stoeckley  Documentation correction plus make two fields
!                             insensitive when not needed.
! 31. 2004-12-09  Stoeckley  Add logic for parallelization (parallelization
!                             is not yet efficient).
! 30. 2004-10-12  Stoeckley  Change to output NUMTR=1 instead of NUMTR=0 when
!                             DEAD_END=YES to make icps happy.
! 29. 2002-08-29  Stoeckley  Add time column to iterations printout.
! 28. 2001-10-18  Stoeckley  Add file selection boxes and file status messages.
! 27. 2001-05-14  Stoeckley  Change default correlation file extension to trc8.
! 26. 2001-03-21  Stoeckley  Add additional error printouts.
! 25. 2001-02-20  Stoeckley  Change OPT_PRINT default from MAX to MIN; remove
!                             warning message for temporary file > 2GB; change
!                             error message for blank filename to occur less
!                             often.
! 24. 2001-02-13  Stoeckley  Change wrapup flag.
! 23. 2000-10-19  Stoeckley  Add missing context-sensitive help.
! 22. 2000-10-06  Stoeckley  Add statement to use the ADDEXT module (previously
!                             was implicit in PATHCHECK which as been changed);
!                             add missing required documentation sections.
! 21. 2000-08-23  Stoeckley  Minor documentation change regarding parameter
!                             keyword names WIN_HDR_X and WIN_HDR_Y.
! 20. 2000-07-20  Stoeckley  Replace TEMPFILE with TEMPTFILE; change keywords
!                             COMP_INL and COMP_CRL to COMP_X and COMP_Y.
! 19. 2000-06-21  Stoeckley  Replace TEMPIO with TEMPFILE.
! 18. 2000-06-16  Stoeckley  Make some keyword changes to match other statics
!                             processes; make small changes to link to
!                             upgraded STATUTIL primitive.
! 17. 2000-06-05  Stoeckley  Remove the EVERY parameter, and fix TAPER error.
! 16. 2000-05-26  Stoeckley  Add new options for static file header words
!                             (source and receiver shotpoints and line
!                             numbers); change to not allow MUTE window
!                             option.
! 15. 2000-05-22  Stoeckley  Change to use the PERMTFILE primitive instead
!                             of the TRCIO primitive and BYTE process; add
!                             DEAD_END parameter and use the STATSHIFT
!                             primitive.
! 14. 2000-04-25  Stoeckley  Improved CFE screen.
! 13. 2000-04-04  Stoeckley  Converted from old system.
! 12. 1998-12-15  Vunderink  Begin using the f90 compiler.
! 11. 1998-02-05  Vunderink  Fix bug in scratch memory calculation
! 10. 1997-12-02  Stoeckley  Added TERMS=1 (FISH-style) option.
!                             Also made all subroutines implicit none.
!  9. 1997-09-08  Vunderink  Added IBWIN to common block.
!  8. 1997-08-28  Vunderink  Added spatial correlation window and added
!                             keyword OPT_PRINT.
!  7. 1997-01-10  Stoeckley  Remove obsolete call to CC3D_PICK.
!  6. 1996-12-16  Stoeckley  Major revision (see special notes).
!  5. 1996-10-11  Stoeckley  Fixed two bugs in calculation of offset terms.
!  4. 1996-07-19  Lazear     Modified indexing of stack trace array.
!  3. 1996-07-12  Lazear     Added CMP and Residual NMO terms.
!  2. 1996-06-21  Lazear     Modified grid input parameters.
!  1. 1996-06-18  Lazear     Original version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           true      whether this process frees tape drives.
! NDISK           >0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  true      whether this process can be in a parallelized loop.
! PCPS_BOSS_EXEC_MODE      PCPS_BOSS_EXECS
! PCPS_SEND_MODE           PCPS_SEND_ALL
! PCPS_ALT_SEND_MODE       PCPS_SEND_ALL
! PCPS_SEND_EOF_MODE       PCPS_SEND_ALL_EOF
! PCPS_RECEIVE_MODE        PCPS_RECEIVE_GATHER
! PCPS_ALT_RECEIVE_MODE    PCPS_RECEIVE_GATHER
! PCPS_GENERATOR_MODE      PCPS_NO_TRACE_GEN
! PCPS_ALT_GENERATOR_MODE  PCPS_NO_TRACE_GEN
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces (DEAD_END = NO).
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces (DEAD_END = NO).
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
!
!  1. Write all traces to disk sorted into super-CMPs consisting
!     of COMP_X * COMP_Y actual CMPs.
!     (CC3D_TRACE_ENTRY)
!
!  2. Iterate for NUM_ITER times through the following:
!     (CC3D_SOLUTION_ENTRY)
!
!     (a) Loop over all super-CMPs.
!         (CC3D_PERFORM_ITERATION)
!         |
!     (b) |  Read each trace of this super-CMP into memory.
!         |  (CC3D_PROCESS_SUPER_CMP)
!         |
!     (c) |  Apply shifts (derived from previous iterations) for
!         |  source, receiver, CMP within the super-CMP (if TERMS=3 or
!         |  TERMS=4), and offset (if TERMS=4).
!         |  (CC3D_PROCESS_SUPER_CMP)
!         |
!     (d) |  Stack the shifted traces five ways:
!         |      by common sources (if source file was requested),
!         |      by common receivers (if receiver file was requested),
!         |      by common CMPs (if TERMS=1 or TERMS=3 or TERMS=4),
!         |      by common offset bins defined by OFF_WID (if TERMS=4),
!         |      and by super-CMP (all traces in super-CMP).
!         |  (CC3D_PROCESS_SUPER_CMP)
!         |
!     (e) |  Successively remove each of the "partial stack" traces
!         |  from the stack of the super-CMP, and correlate
!         |  the partial stack with this "full stack".  For TERMS=1,
!         |  use only the portion of the "full stack" with CMPs which
!         |  match the CMPs present in the partial stack for that
!         |  ground position.  The "partial stack" traces are common
!         |  source, common receiver, common CMP (if TERMS=3 or TERMS=4),
!         |  and common offset (if TERMS=4).
!         |  (CC3D_FORM_CORRELATIONS)
!         |
!     (f) |  Pick the correlation to get an "IMS" static estimate
!         |  for the corresponding term (source, receiver, CMP, or
!         |  offset), and sum the shift into an average for that term.
!         |  (CC3D_FORM_CORRELATIONS)
!         |
!     (g) |  Stack the correlation with others from the same term.
!         |  (CC3D_FORM_CORRELATIONS)
!         |
!     (h) |  Optionally form least-squares estimates of X and Y CMP dip
!         |  (if TERMS=3 or TERMS=4) and residual moveout coefficient
!         |  (if TERMS=4) for this super-CMP.
!         |  (CC3D_FORM_CORRELATIONS)
!         |
!     (i) End of loop over super-CMPs.
!
!     (j) Pick stacked correlations to get a "FISH" type static
!         estimate, and use the WEIGHT term to average it with the
!         average picks (IMS) to form the estimated residual static.
!         (CC3D_PICK_STACKED_CORRELATIONS)
!
!     (k) Sum the residual statics from this iteration into the
!         total static estimate.
!         (CC3D_UPDATE_STATICS)
!
!     (l) Save static files (and optionally stacked correlations) for
!         source and receiver terms only.
!         (CC3D_SAVE_FILES)
!
!     (m) If the iteration count is less than NUM_ITER, go back to (a).
!
!  3. Optionally print the final static solution.
!     (CC3D_PRINT_STATICS)
!
!  4. Optionally shift and output the input traces.
!     (STATSHIFT PRIMITIVE)
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!              SPECIAL NOTES REGARDING MAJOR REVISION IN 1996
!
!  NEW PARAMETERS:
!
!    (1) The following new static solution parameters were added:
!          WEIGHT, TERMS, PWR_CC, TAPER, UPDATE.
!
!    (2) The following new source ground position parameters were added:
!          HDR_SX, SX_INC, HDR_SY, SY_INIT, SY_LAST, SY_INC.
!
!    (3) The following new parameter was added for disposing files:
!          PATH.
!
!  IMPLICATIONS OF THE NEW PARAMETERS:
!
!    (1) The parameters FILR and FILS no longer need to include a
!        directory path, since the path can optionally (and preferably)
!        be provided using the new PATH parameter.
!
!    (2) The parameters FILR and FILS should no longer include a
!        file extension, since any extension included will be changed
!        to "cc3d" (for static files) or "byt" (for BYTE files).
!
!    (3) If PATH is specified, correlation functions are now output
!        to BYTE files for viewing and editing in CBYT.
!
!    (4) The default values of all new parameters cause the new version
!        of CC3D to work the same as the old version which did not have
!        the new parameters.  One exception is TERMS, whose default
!        value is 3; the old version behaved as if the default value
!        of TERMS were 4.
!
!    (5) The names of all old parameters have been retained unchanged,
!        even though this causes some inconsistency in parameter
!        naming.  These old names have been retained to allow CC3D
!        to be edited from jobs originally built using the old CC3D
!        front end (CFE) which did not include the new parameters,
!        and to allow submission of old CC3D jobs to work with the
!        new CC3D.
!
!  CHANGES IN CODING:
!
!    (1) Several subroutines were added to reduce duplication of code.
!
!    (2) The new CPS primitives PKUTIL, TOOLS, TEMPIO, STRING_UTIL,
!        FILENAME_UTIL, and HEAP_UTIL were created to hold
!        general-purpose subroutines containing code originally
!        hard-wired in CC3D or needed in the revised CC3D.
!
!    (3) The CPS primitive STATUTIL was expanded to hold new
!        general-purpose subroutines containing code originally
!        hard-wired in CC3D or needed in the revised CC3D.
!
!    (4) Loop searches to find the correct composited traces were
!        eliminated by using an array of pointers to heap memory.
!
!    (5) Additional diagnostic and summary printouts were added.
!
!  CHANGES IN MEMORY AND DISK USAGE:
!
!    Prior to this revision:
!
!    (1) Entire trace lengths (for all traces used) were stored on
!           disk using DTROT, packed 2:1.
!    (2) Entire trace lengths (for common-ground-position composited
!           traces) were stored in scratch memory unpacked.
!    (3) Stacked correlations were stored in scratch memory unpacked.
!
!    After this revision:
!
!    (1) Correlation windows (for all traces used) are stored on disk
!           packed 8:1 using Fortran direct-access (random) I/O.
!    (2) Correlation windows (for common-ground-position composited
!           traces) are packed 4:1 in heap memory.
!    (3) Stacked correlations are packed 4:1 in scratch memory.
!
!    These changes have the following consequences:
!
!    (1) The static solution is not significantly affected.
!    (2) Disk space usage is reduced by a factor of about 4
!           (or about  8 for window sizes smaller than about 2 sec)
!           (or about 16 for window sizes smaller than about 1 sec).
!    (3) I/O requests are reduced by a factor of about 10.
!    (4) Central memory usage (the total of scratch, permanent, and
!           heap) is reduced by a factor of nearly 4.
!    (5) Total CPU time is significantly reduced (by as much as a
!           factor of 2 or more in some cases).
!    (6) The wall clock time has been considerably reduced.
!
!  NOTE REGARDING VECTORIZATION, DISK I/O, AND CPU TIME:
!
!    Initially, the packing/unpacking routines were not vectorized,
!    resulting in a modest increase in the total CPU time used.  But
!    when these were modified so they would vectorize, total CPU time
!    was actually reduced significantly!  This is because the extra
!    "user" CPU time required for vectorized packing and unpacking was
!    much more than offset by a several-fold reduction of "system" CPU
!    time associated with disk I/O.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS CC3D Process/NC=80>
!                     Composite Correlation 3-D Statics
!
!          MAX_STATIC=`FFFFF ms    FOLD_MAX=`IIII        WEIGHT=`FFFFF 
!          NUM_ITER~~=`III         OFF_WID =`FFFFFF      TERMS =`C
!          PWR_CC~~~~=`XXXXXX      TAPER~~~=`XXXXXX      UPDATE=`XXXXXX
!
!Select PATH_SRC[PATH_SRC]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [PATH_SRC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!Select PATH_REC[PATH_REC]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [PATH_REC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!          CORR_FILES=`CCCC    NO_DEAD=`CC    OPT_PRINT=`CCC    DEAD_END=`CC
!
! NSTORE=`XXXXXXXXX NSCRATCH=`XXXXXXXXX DISK=`XXXXXXXXX megabytes   NCPUS=`XXXX
!
!<NS Bins and Ranges/NC=80>
!`------------------------------------------------------------------------------
! HDR_SRC=`CCCC
!   HDR_SX=`X   SX_INIT=`FFFFFFF  SX_INC=`FFF   SX_LAST=`FFFFFFF  SX_TOT=`IIIII
!   HDR_SY=`X   SY_INIT=`FFFFFFF  SY_INC=`FFF   SY_LAST=`FFFFFFF  SY_TOT=`IIIII
! HDR_REC=`CCCC
!   HDR_RX=`X   RX_INIT=`FFFFFFF  RX_INC=`FFF   RX_LAST=`FFFFFFF  RX_TOT=`IIIII
!   HDR_RY=`X   RY_INIT=`FFFFFFF  RY_INC=`FFF   RY_LAST=`FFFFFFF  RY_TOT=`IIIII
! [/L]CMPs:
!   COMP_X=`I CMPX_INIT=`FFFFFF CMPX_INC=`FFF CMPX_LAST=`FFFFFF CMPX_TOT=`IIIII
!   COMP_Y=`I CMPY_INIT=`FFFFFF CMPY_INC=`FFF CMPY_LAST=`FFFFFF CMPY_TOT=`IIIII
!`------------------------------------------------------------------------------
!
!<PARMS path_src[/ML=128/XST]>
!<PARMS path_rec[/ML=128/XST]>
!
!<NS Correlation Window Parameters/NC=80>
!<include latwin.f90>
!
!<PARMS Correlation Window Parameters [screen2]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATH_SRC">
!<Tip> Choose PATH_SRC using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATH_REC">
!<Tip> Choose PATH_REC using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_SRC_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_SRC. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_REC_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_REC. </Tip>
!</Help>
!
!
!<Help KEYWORD="NSTORE" TYPE="DISPLAY_ONLY">
!<Tip> Number of word of permanent storage to be used. </Tip>
!</Help>
!
!
!<Help KEYWORD="NSCRATCH" TYPE="DISPLAY_ONLY">
!<Tip> Number of word of temporary storage to be used. </Tip>
!</Help>
!
!
!<Help KEYWORD="DISK" TYPE="DISPLAY_ONLY">
!<Tip> Disk space (in megabytes) to be used. </Tip>
!</Help>
!
!
!<Help KEYWORD="NCPUS" TYPE="DISPLAY_ONLY">
!<Tip> Number of processors in the job. </Tip>
!</Help>
!
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> How much printing to do. </Tip>
! Default =  MIN
! Allowed =  MIN (do a minimum amount of printing).
! Allowed =  MAX (print complete table of the calculated static values).
!</Help>
!
!
!<Help KEYWORD="MAX_STATIC">
!<Tip> Maximum static to solve for (in milliseconds). </Tip>
! Default =  40.0 
! Allowed =  real > 0.
!</Help>
!
!
!<Help KEYWORD="FOLD_MAX">
!<Tip> Maximum fold in a CMP. </Tip>
! Default =  50  
! Allowed =  int > 0.
!
! Used to set up the trace buffer indexing of bins on disk.
! If the fold of an input CMP exceeds FOLD_MAX the excess traces are dropped.
!</Help>
!
!
!<Help KEYWORD="WEIGHT">
!<Tip> Weight for static estimate from stacked correlations. </Tip>
! Default =  0.5  
! Allowed = real from 0.0 through 1.0
!
! WEIGHT = 1.0 uses only picks of stacked correlations (like FISH).
! WEIGHT = 0.0 uses only the average pick of individual correlations (like IMS).
! WEIGHT = 0.5 uses an average of the two types of picks.
!</Help>
!
!
!<Help KEYWORD="NUM_ITER">
!<Tip> Number of solution iterations (passes). </Tip>
! Default =  20 
! Allowed =  int >= 1.
!</Help>
!
!
!<Help KEYWORD="PWR_CC" TYPE="DISPLAY_ONLY">
!<Tip> Power for weighting the residual static shifts. </Tip>
! Default =  1.0   
! Allowed =  real >= 0.0
!
! Residual static shifts are weighted by correlation coefficients raised
! to this power.  This allows picks from poor correlations to be weighted less
! than picks from good correlations.
!
! This parameter is not in the user interface because it has not been
! determined whether it is advisable to adjust it.
!</Help>
!
!
!<Help KEYWORD="TAPER" TYPE="DISPLAY_ONLY">
!<Tip> Taper for correlation functions. </Tip>
! Default =  1.0   
! Allowed =  real between 0.0 (no taper) and 1.0 (full taper).
!
! Correlation functions are tapered down by this fraction at the edges.
! The calculated taper is symmetric and linear.
! The center of the taper always has value 1.0.
! The ends of the taper have value (1.0 - TAPER).
! If TAPER is 0.0, the correlation will not be tapered (all values 1.0).
! If TAPER is 0.5, the correlation will be tapered down to 0.5 at the ends.
! If TAPER is 1.0, the correlation will be tapered down to 0.0 at the ends.
!
! This parameter is not in the user interface because it has not been
! determined whether it is advisable to adjust it.
!</Help>
!
!
!<Help KEYWORD="UPDATE" TYPE="DISPLAY_ONLY">
!<Tip> Static values are updated by this fraction after each pass. </Tip>
! Default =  0.8   
! Allowed =  real between 0.0 and 1.0
!
! This parameter is not in the user interface because it has not been
! determined whether it is advisable to adjust it.
!</Help>
!
!
!<Help KEYWORD="TERMS">
!<Tip> Method to use in static solution. </Tip>
! Default =  3
! Allowed =  1,2,3,4
!
! TERMS = 1 does source and/or receiver solution only.  It uses in the full
!           stack only those traces with same ground position as the partial
!           stack (FISH-style).
!
! TERMS = 2 does source and/or receiver solution only.  It uses in the full
!           stack all the super-CMP traces (original CC3D-style).
!
! TERMS = 3 is the same as TERMS = 2, but also uses CMP term.
!
! TERMS = 4 is the same as TERMS = 2, but also uses CMP term and residual NMO
!           (offset) term.
!
! (TERMS = 1 or 3 are recommended based on very little experience.)
! (Generally TERMS = 1 is expected to work better than TERMS = 2.)
!</Help>
!
!
!<Help KEYWORD="OFF_WID">
!<Tip> Offset bin interval (in distance) for estimating residual NMO. </Tip>
! Default =  1000  
! Allowed =  real > 0.
!
! OFF_WID determines the number of offsets in the least-squares fit of time
! shifts versus offset squared.  OFF_WID is used only if TERMS = 4.
!</Help>
!
!
!<Help KEYWORD="PATH_SRC">
!<Tip> Name for the output source static file. </Tip>
! Default = NONE
! Allowed = char
!
! If PATH_SRC = NONE, then do not calculate or output source statics.
!
! At least one static file (PATH_SRC or PATH_REC) must be specified.
!
! The extension .cc3d will always be appended.
!
! Files are saved after each iteration, overwriting the previous contents of
! the file.  The iteration count will be included in the file history cards.
!</Help>
!
!
!<Help KEYWORD="PATH_REC">
!<Tip> Name for the output receiver static file. </Tip>
! Default = NONE
! Allowed = char
!
! If PATH_REC = NONE, then do not calculate or output receiver statics.
!
! At least one static file (PATH_SRC or PATH_REC) must be specified.
!
! The extension .cc3d will always be appended.
!
! Files are saved after each iteration, overwriting the previous contents of
! the file.  The iteration count will be included in the file history cards.
!</Help>
!
!
!<Help KEYWORD="CORR_FILES">
!<Tip> Type of static correlation files to save. </Tip>
! Default = TRC8
! Allowed = TRC8 (save TRCIO byte files with the extension .trc8)
! Allowed = NONE (do not save correlation files)
!
! The names of the static correlation files will be the same as PATH_SRC and
! PATH_REC except for the extension.
!
! Files are saved after each iteration, overwriting the previous contents
! of the file.
!
! A CBYT option allows the correlation files to be plotted and edited easily.
! This option automatically updates the statics files based on correlation
! edits.
!
! CORR_FILES is always forced to NONE for parallel processing (more than 1 CPU).
!</Help>
!
! tabgroup = Headers, Bins, and Increments
!
!<Help KEYWORD="HDR_SRC">
!<Tip> Header word(s) to use to identify source locations. </Tip>
! Default = SEQU
! Allowed = SEQU  (source sequential ground position - header word  46)
! Allowed = GROUP (original source group number      - header word   9)
! Allowed = LNSP  (source shotpoint and line number  - header words 29 and 26)
! Allowed = GRID  (source grid coordinates           - header words 33 and 34)
!
! SEQU coordinates:
!  Normally, if sequential ground position header words have been set in the
!  input dataset, the sequential ground position is a good choice for source
!  and receiver coordinates in 2D and 3D surveys.
!
! GROUP coordinates:
!  The original source group number should generally be used for 2D and 3D
!  surveys if a source occupies the same location more than once.
!
! LNSP coordinates:
!  Line number and shotpoint header words are generally good choices in 3D
!  surveys.
!
! GRID coordinates:
!  Grid coordinates are less desirable than line number and shotpoint for
!  the following reasons:
!   1. Grid coordinates may not be densely populated, making internal
!       calculations slower, disk use higher, and static and correlation
!       files much larger.
!   2. Statics are calculated only at regular grid intersections.  If
!       sources and receivers are not always located exactly on the
!       intersections then applying the statics files will probably produce
!       somewhat inaccurate results.
!</Help>
!
!
!<Help KEYWORD="HDR_REC">
!<Tip> Header word(s) to use to identify receiver locations. </Tip>
! DIVIDER
! Default = SEQU
! Allowed = SEQU  (receiver sequential ground position - header word  47)
! Allowed = LNSP  (receiver shotpoint and line number  - header words 28 and 27)
! Allowed = GRID  (receiver grid coordinates           - header words 35 and 36)
!
! SEQU coordinates:
!  Normally, if sequential ground position header words have been set in the
!  input dataset, the sequential ground position is a good choice for source
!  and receiver coordinates in 2D and 3D surveys.
!
! LNSP coordinates:
!  Line number and shotpoint header words are generally good choices in 3D
!  surveys.
!
! GRID coordinates:
!  Grid coordinates are less desirable than line number and shotpoint for
!  the following reasons:
!   1. Grid coordinates may not be densely populated, making internal
!       calculations slower, disk use higher, and static and correlation
!       files much larger.
!   2. Statics are calculated only at regular grid intersections.  If
!       sources and receivers are not always located exactly on the
!       intersections then applying the statics files will probably produce
!       somewhat inaccurate results.
!</Help>
!
! tabgroup = Main Tab
!
!<Help KEYWORD="NO_DEAD">
!<Tip> Whether to omit dead traces from the output correlation files. </Tip>
! Default = NO
! Allowed = YES
! Allowed = NO
!
! Setting NO_DEAD = YES may be desirable if the correlation files are very
! large and consist mostly of dead traces.
!</Help>
!
!
!<Help KEYWORD="DEAD_END">
!<Tip> Whether to stop before outputting any traces. </Tip>
! Default = YES 
! Allowed = YES
! Allowed = NO
!
! If this parameter is YES, this will be a dead-end process which does not
! output any traces.
!
! If this parameter is NO, all input traces will be shifted and output, in
! the same order they were received.
!
! Static files and correlation files will always be written, regardless of the
! value of DEAD_END.
!
! Stopping before outputting any traces reduces the required temporary disk
! space by a factor of at least 5, and more commonly by about 10 on average.
! The disk savings are more significant when trace correlation windows are
! small.
!
! DEAD_END is always forced to YES for parallel processing (more than 1 CPU).
!</Help>
!
! tabgroup = Headers, Bins, and Increments
!
!<Help KEYWORD="HDR_SX">
!<Tip> First header word to use for designating source locations. </Tip>
! SPACING
! HEADER
! Default = 46
! Allowed = 46  (source sequential ground position - header word  46)
! Allowed = 9   (original source group number      - header word   9)
! Allowed = 29  (source shotpoint and line number  - header words 29 and 26)
! Allowed = 33  (source grid coordinates           - header words 33 and 34)
!</Help>
!
!
!<Help KEYWORD="HDR_SY">
!<Tip> Second header word to use for designating source locations. </Tip>
! SPACING
! HEADER
! Default = 0
! Allowed = 0   (source sequential ground position - header word  46)
! Allowed = 0   (original source group number      - header word   9)
! Allowed = 26  (source shotpoint and line number  - header words 29 and 26)
! Allowed = 34  (source grid coordinates           - header words 33 and 34)
!</Help>
!
!
!<Help KEYWORD="HDR_RX">
!<Tip> First header word to use for designating receiver locations. </Tip>
! SPACING
! HEADER
! Default = 47
! Allowed = 47  (receiver sequential ground position - header word  47)
! Allowed = 28  (receiver shotpoint and line number  - header words 28 and 27)
! Allowed = 35  (receiver grid coordinates           - header words 35 and 36)
!</Help>
!
!
!<Help KEYWORD="HDR_RY">
!<Tip> Second header word to use for designating receiver locations. </Tip>
! SPACING
! HEADER
! Default = 0
! Allowed = 0   (receiver sequential ground position - header word  47)
! Allowed = 27  (receiver shotpoint and line number  - header words 28 and 27)
! Allowed = 36  (receiver grid coordinates           - header words 35 and 36)
!</Help>
!
!
!<Help KEYWORD="SX_INIT">
!<Tip> Minimum value of HDR_SX. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real (in units of header word HDR_SX).
!</Help>
!
!
!<Help KEYWORD="SX_INC">
!<Tip> Increment between values of HDR_SX. </Tip>
! Default =  1.0
! Allowed = real > 0.0
!
! Increment between values of HDR_SX for occupied source locations.
!</Help>
!
!
!<Help KEYWORD="SX_LAST">
!<Tip> Maximum value of HDR_SX. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real > 0.0
!
! If both SX_LAST and SX_TOT are specified, SX_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="SX_TOT">
!<Tip> Total number of HDR_SX values. </Tip>
! Default =  1
! Allowed = int > 0
!
! If both SX_LAST and SX_TOT are specified, SX_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="SY_INIT">
!<Tip> Minimum value of HDR_SY. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="SY_INC">
!<Tip> Increment between values of HDR_SY. </Tip>
! Default =  1.0
! Allowed = real > 0.0
! Increment between values of HDR_SY for occupied source locations.
!</Help>
!
!
!<Help KEYWORD="SY_LAST">
!<Tip> Maximum value of HDR_SY. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real > 0.0
!
! If both SY_LAST and SY_TOT are specified, SY_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="SY_TOT">
!<Tip> Total number of HDR_SY values. </Tip>
! Default =  1
! Allowed = int > 0
!
! If both SY_LAST and SY_TOT are specified, SY_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="RX_INIT">
!<Tip> Minimum value of HDR_RX. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real (in units of header word HDR_RX).
!</Help>
!
!
!<Help KEYWORD="RX_INC">
!<Tip> Increment between values of HDR_RX. </Tip>
! Default =  1.0
! Allowed = real > 0.0
!
! Increment between values of HDR_RX for occupied receiver locations.
!</Help>
!
!
!<Help KEYWORD="RX_LAST">
!<Tip> Maximum value of HDR_RX. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real > 0.0
!
! If both RX_LAST and RX_TOT are specified, RX_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="RX_TOT">
!<Tip> Total number of HDR_RX values. </Tip>
! Default =  1
! Allowed = int > 0
!
! If both RX_LAST and RX_TOT are specified, RX_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="RY_INIT">
!<Tip> Minimum value of HDR_RY. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="RY_INC">
!<Tip> Increment between values of HDR_RY. </Tip>
! Default =  1.0
! Allowed = real > 0.0
!
! Increment between values of HDR_RY occupied receiver locations.
!</Help>
!
!
!<Help KEYWORD="RY_LAST">
!<Tip> Maximum value of HDR_RY. </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real > 0.0
!
! If both RY_LAST and RY_TOT are specified, RY_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="RY_TOT">
!<Tip> Total number of HDR_RY values. </Tip>
! Default =  1
! Allowed = int > 0
!
! If both RY_LAST and RY_TOT are specified, RY_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="COMP_X">
!<Tip> Number of CMPs to composite in the CMP X grid direction. </Tip>
! DIVIDER
! Default = 5
! Allowed = int > 0
!
! Number of CMPs to composite in the CMP X grid direction to form super-CMP
! base traces.
!</Help>
!
!
!<Help KEYWORD="CMPX_INIT">
!<Tip> Minimum CMP X grid location (header word 7). </Tip>
! ROWGROUP
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="CMPX_INC">
!<Tip> Increment between header word 7 values for CMP locations. </Tip>
! Default =  1.0
! Allowed = real > 0.
!</Help>
!
!
!<Help KEYWORD="CMPX_LAST">
!<Tip> Maximum CMP X grid location (header word 7). </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real
!
! If both CMPX_LAST and CMPX_TOT are specified, CMPX_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="CMPX_TOT">
!<Tip> Total number of CMP X grid locations (header word 7). </Tip>
! Default = 1
! Allowed = int
!
! If both CMPX_LAST and CMPX_TOT are specified, CMPX_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="COMP_Y">
!<Tip> Number of CMPs to composite in the CMP Y grid direction. </Tip>
! SPACING
! Default = 5
! Allowed = int > 0
!
! Number of CMPs to composite in the CMP Y grid direction to form super-CMP
! base traces.
!</Help>
!
!
!<Help KEYWORD="CMPY_INIT">
!<Tip> Minimum CMP Y grid location (header word 8). </Tip>
! ROWGROUP
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="CMPY_INC">
!<Tip> Increment between header word 8 values for CMP locations. </Tip>
! Default =  1.0
! Allowed = real > 0.
!</Help>
!
!
!<Help KEYWORD="CMPY_LAST">
!<Tip> Maximum CMP Y grid location (header word 8). </Tip>
! ROWGROUP
! Default =  1.0
! Allowed = real
!
! If both CMPY_LAST and CMPY_TOT are specified, CMPY_TOT takes precedence.
!</Help>
!
!
!<Help KEYWORD="CMPY_TOT">
!<Tip> Total number of CMP Y grid locations (header word 8). </Tip>
! Default = 1
! Allowed = int
!
! If both CMPY_LAST and CMPY_TOT are specified, CMPY_TOT takes precedence.
!</Help>
!
! rowgroup =        HDR_SX    HDR_SY 
! rowgroup =        SX_INIT   SY_INIT
! rowgroup =        SX_INC    SY_INC
! rowgroup =        SX_LAST   SY_LAST
! rowgroup =        SX_TOT    SY_TOT 
! rowgroup =        HDR_RX    HDR_RY 
! rowgroup =        RX_INIT   RY_INIT
! rowgroup =        RX_INC    RY_INC
! rowgroup =        RX_LAST   RY_LAST
! rowgroup =        RX_TOT    RY_TOT 
! rowgroup =      COMP_X    COMP_Y   
! rowgroup =      CMPX_INIT CMPY_INIT
! rowgroup =      CMPX_INC  CMPY_INC
! rowgroup =      CMPX_LAST CMPY_LAST
! rowgroup =      CMPX_TOT  CMPY_TOT 
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!------------------------ subroutine layout ----------------------------!!
!!------------------------ subroutine layout ----------------------------!!
!!------------------------ subroutine layout ----------------------------!!

!       CC3D
!        |
!        |    CC3D_TRACE_ENTRY
!        |     |
!        |     |   CC3D_WRITE_TRACE_WINDOW
!        |     |
!        |     |------------
!        |
!        |    CC3D_SOLUTION_ENTRY
!        |     |
!        |     |    loop over iterations:
!        |     |    CC3D_PERFORM_ITERATION
!        |     |     |
!        |     |     |    loop over super-cmps:
!        |     |     |    CC3D_PROCESS_SUPER_CMP
!        |     |     |     |
!        |     |     |     |    CC3D_FORM_STACKS
!        |     |     |     |     |
!        |     |     |     |     |    loop over fold in super-cmp:
!        |     |     |     |     |    CC3D_PROCESS_SINGLE_TRACE
!        |     |     |     |     |     |
!        |     |     |     |     |     |    CC3D_READ_TRACE_WINDOW
!        |     |     |     |     |     |    CC3D_SHIFT_TRACE_WINDOW
!        |     |     |     |     |     |    CC3D_ADD_TO_STACKS
!        |     |     |     |     |     |
!        |     |     |     |     |     |------------
!        |     |     |     |     |
!        |     |     |     |     |------------
!        |     |     |     |
!        |     |     |     |    CC3D_FORM_CORRELATIONS
!        |     |     |     |     |
!        |     |     |     |     |    CC3D_FORM_STATIC_CORRELATIONS
!        |     |     |     |     |    CC3D_FORM_STATIC_CORRELATIONS
!        |     |     |     |     |    CC3D_FORM_OFFSET_CORRELATIONS
!        |     |     |     |     |    CC3D_FORM_CMP_CORRELATIONS
!        |     |     |     |     |
!        |     |     |     |     |------------
!        |     |     |     |
!        |     |     |     |------------
!        |     |     |
!        |     |     |    CC3D_MERGE_RESULTS  (from all cpus to boss)
!        |     |     |
!        |     |     |    CC3D_PROCESS_RESULTS  (boss only)
!        |     |     |     |
!        |     |     |     |    CC3D_PICK_STACKED_CORRELATIONS  (boss only)
!        |     |     |     |    CC3D_UPDATE_STATICS             (boss only)
!        |     |     |     |    CC3D_PRINT_ITERATION_RESULTS    (boss only)
!        |     |     |     |    CC3D_SAVE_FILES                 (boss only)
!        |     |     |     |    CC3D_PRINT_STATICS              (boss only)
!        |     |     |     |
!        |     |     |     |------------
!        |     |     |
!        |     |     |------------
!        |     |
!        |     |------------
!        |    
!        |-------------


!!------------------------ programming notes -------------------------------!!
!!------------------------ programming notes -------------------------------!!
!!------------------------ programming notes -------------------------------!!


!     index J is used for looping over trace values.
!                 typically ranging over NWIN or NCORR.
!
!     index I is used for looping over traces.
!                 typically ranging over N or IFOLD(ISUPER).
!
!     index IVEC is used for looping over the solution vector.
!                 typically ranging over NVEC or NVEC2.
!
!     index IGP is used for looping over ground positions.
!                 typically ranging over NSRC or NREC or NSUPER or NGP.
!
!     index ICMP is used for looping over cmps.
!                 typically ranging over NCMP.
!
!     index ISUPER is used for looping over super-cmps.
!                 typically ranging over NSUPER.
!
!     index ICOMP is used for looping over the cmps within a super-cmp.
!                 typically ranging over NCOMP.

!     index ICMP    = 1,NCMP     (number of cmps)
!           IXCMP   = 1,CMPX_TOT
!           IYCMP   = 1,CMPY_TOT
!
!     index ISUPER  = 1,NSUPER   (number of super-cmps)
!           IXSUPER = 1,NXSUPER
!           IYSUPER = 1,NYSUPER
!
!     index ICOMP   = 1,NCOMP    (number of cmps within a super-cmp)
!           ICOMPX  = 1,COMP_X
!           ICOMPY  = 1,COMP_Y


!     NTR        number of traces and headers passed in argument list
!     NWIH       number of words in trace header (64 or more)
!     NDPT       number of values in each trace in argument list
!
!     NCORR      number of values in correlation function
!     NCORR16    number of words needed to store correlation packed to 16 bits
!     NWIN       number of values in trace window on disk
!     NWIN16     number of words needed to store trace window packed to 16 bits
!     NWIN8      number of words needed to store trace window packed to 8 bits
!     NHEAD      number of words in trace header on disk (5)
!     NVEC       number of values in model vector  (NREC + NSRC + 3*NSUPER)
!     NVEC2      number of sources and receivers   (NREC + NSRC)
!     NSUPER     number of super-cmp gathers       (NXSUPER*NYSUPER)
!     NCOMP      number of cmps within a super-cmp (COMP_X*COMP_Y)
!     NCOMP1     number of words needed to store NCOMP packed to 1 bit
!     MAXFOLDS   maximum fold in a super-cmp gather
!     NOFF       maximum number of offsets expected (with OFF_WID bin size)
!
!     NSRC       number of source ground positions
!     NREC       number of receiver ground positions


!  arg   HDI      (:,NTR)           headers passed in argument list
!  arg   TRI      (:,NTR)           traces  passed in argument list
!
!  stor  IFOLD    (NSUPER)          fold of each super-cmp gather
!
!  scr   STAT               (NVEC)   total    statics values
!  scr   RSTAT              (NVEC)   residual statics values
!  scr   WGHTS              (NVEC2)  residual statics weights
!  scr   CCOEF              (NVEC2)  correlation coefficients
!  scr   NTRACES            (NVEC2)  number of traces used
!  scr   NCCOEF             (NVEC2)  number of correlation coefficients stacked
!  scr   EXISTS_IN_SUPER    (NVEC2)  whether gp is in current super-cmp
!  scr   EXISTS_IN_CMP  (NCOMP1,NVEC2)  whether gp is in each cmp of super-cmp
!                                                      (packed to one bit)
!
!  scr   stacked_ground_positions  (NWIN16,  NVEC2)    (packed to 16 bits)
!  scr   stacked_correlations      (NCORR16, NVEC2)    (packed to 16 bits)
!  scr   stacked_cmps              (NWIN,  NCOMP)
!  scr   stacked_offsets           (NWIN,  NOFF)
!
!  scr   TAPR        (NCORR)        taper to apply to correlation function
!  scr   CORR        (NCORR+1)      correlation function
!  scr   is_in_cmp   (NCOMP)        single exists_in_cmp
!  scr   GP_STACK    (NWIN)         single stacked ground position
!  scr   FISH_STACK  (NWIN)         single fish stack
!  scr   SUPER_STACK (NWIN)         single stacked super-cmp gather


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module cc3d_module
      use pc_module
      use named_constants_module
      use permtfile_module
      use mth_module
      use increment_module
      use pkutil_module
      use temptfile_module
      use pathcheck_module
      use pathchoose_module
      use statcc_module
      use statutil_module
      use statio_module
      use latwin_module
      use statshift_module
      use addext_module
      use pcps_module
      use pcpsx_module
      use string_module
      use timer_module
      implicit none
      private
      public :: cc3d_create
      public :: cc3d_initialize
      public :: cc3d_update
      public :: cc3d_delete
      public :: cc3d
      public :: cc3d_wrapup

      character(len=100),public,save :: CC3D_IDENT = &
'$Id: cc3d.f90,v 1.39 2008/12/19 12:51:24 M beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: cc3d_struct              
 
        private
        logical            :: skip_wrapup
        integer            :: ipn
!  global parameters
        integer            :: nwih,ndpt,num_cpus
        real               :: dt,dtms
!  input parameters
        real               :: max_static
        integer            :: fold_max,num_iter,terms
        real               :: weight
        real               :: off_wid,pwr_cc,taper,update
        integer            :: comp_x,comp_y,cmpx_tot,cmpy_tot
        real               :: sx_init,sx_inc,sx_last
        real               :: sy_init,sy_inc,sy_last
        real               :: rx_init,rx_inc,rx_last
        real               :: ry_init,ry_inc,ry_last
        real               :: cmpx_init,cmpx_inc,cmpx_last
        real               :: cmpy_init,cmpy_inc,cmpy_last
        character(len=4)   :: opt_print
        character(len=8)   :: hdr_src,hdr_rec,corr_files
        logical            :: no_dead,dead_end
        character(len=FILENAME_LENGTH) :: path_src,path_rec
!  general parameters
        integer            :: nwin,nwin16,nwin8,ncorr,ncorr16
        integer            :: IDROP,IDROPD,IDROPS,IDROPR,IDROPC,IDROPF,ICOUNT
        integer            :: NSRC,NREC
        integer            :: Ncmp,NCOMP,NCOMP1,ncards
        integer            :: NSUPER,NXSUPER,NYSUPER
        integer            :: maxfolds,maxONDSK,noff,NVEC,NVEC2
        real               :: superxmin,superymin,superxinc,superyinc
        real               :: superxmax,superymax
        logical            :: use_src,use_rec,use_cmp,use_off,fish_style
        integer            :: hdr_sx,hdr_sy,sx_tot,sy_tot
        integer            :: hdr_rx,hdr_ry,rx_tot,ry_tot
        integer            :: timer
        integer            :: isuper_start,isuper_stop
        integer            :: nworkers
        character(len=FILENAME_LENGTH) :: path_byt_src,path_byt_rec
        type(increment_struct) :: increment_xsrco,increment_ysrco
        type(increment_struct) :: increment_xreco,increment_yreco
        type(increment_struct) :: increment_xcmpo,increment_ycmpo
        type(increment_struct) :: increment_offseto
        type(increment_struct) :: increment_xsrc,increment_ysrc
        type(increment_struct) :: increment_xrec,increment_yrec
        type(increment_struct) :: increment_xcmp,increment_ycmp
        type(increment_struct) :: increment_offset
        type(latwin_struct)    ,pointer :: latwin
        type(statshift_struct) ,pointer :: statshift
        type(temptfile_struct) ,pointer :: temptfile
        type(pathchoose_struct),pointer :: dialog_src
        type(pathchoose_struct),pointer :: dialog_rec
!  dependent pointers
        integer                          ,pointer :: ifold(:)
        character(len=PC_DATACARD_LENGTH),pointer :: cards(:)

      end type cc3d_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                  ,save :: lunprint  ! unit number for printing.
      type(cc3d_struct),pointer,save :: object    ! needed for traps.

      integer,parameter,private :: NHEAD     = 5
      real   ,parameter,private :: DELTA     = 0.5
      integer,parameter,private :: NUMINC    = 12
      logical,parameter,private :: NORMALIZE = .true.
      logical,parameter,private :: SUBTRACT  = .true.

      integer,parameter,private ::  OPT_PRINT_NOPT = 2
      integer,parameter,private ::      TERMS_NOPT = 4
      integer,parameter,private ::    HDR_SRC_NOPT = 4
      integer,parameter,private ::    HDR_REC_NOPT = 3
      integer,parameter,private :: CORR_FILES_NOPT = 2

      character(len=4),save,private ::  opt_print_options ( OPT_PRINT_NOPT)  
      integer         ,save,private ::      terms_options (     TERMS_NOPT) 
      character(len=8),save,private ::    hdr_src_options (   HDR_SRC_NOPT)
      character(len=8),save,private ::    hdr_rec_options (   HDR_REC_NOPT)
      character(len=8),save,private :: corr_files_options (CORR_FILES_NOPT)

      data             opt_print_options /'MIN','MAX'/  
      data                 terms_options /1,2,3,4/     
      data               hdr_src_options /'GROUP','SEQU','LNSP','GRID'/
      data               hdr_rec_options /        'SEQU','LNSP','GRID'/
      data            corr_files_options /        'TRC8','NONE'/


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine cc3d_create (obj)

      type(cc3d_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()

      allocate (obj)

      obj%timer = INIL

      nullify (obj%ifold)  
      nullify (obj%cards) 
      nullify (obj%statshift) 
      nullify (obj%temptfile) 
      nullify (obj%latwin) ! jpa
      nullify (obj%dialog_src) ! jpa
      nullify (obj%dialog_rec) ! jpa

      call latwin_create     (obj%latwin, no_mute = .true.)
      call pathchoose_create (obj%dialog_src, 'path_src', 'cc3d')
      call pathchoose_create (obj%dialog_rec, 'path_rec', 'cc3d')
      call cc3d_initialize   (obj)

      end subroutine cc3d_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine cc3d_delete (obj)

      type(cc3d_struct),pointer :: obj       ! arguments

      call cc3d_wrapup (obj)

      if (obj%timer /= INIL) call timer_free (obj%timer)

      if(associated(obj%ifold))    deallocate (obj%ifold)
      if(associated(obj%cards))    deallocate (obj%cards)

      call latwin_delete     (obj%latwin)
      call pathchoose_delete (obj%dialog_src)
      call pathchoose_delete (obj%dialog_rec)

      deallocate(obj)

      end subroutine cc3d_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine cc3d_initialize (obj)

      type(cc3d_struct),intent(inout) :: obj               ! arguments

      obj%num_cpus     = 1        ! in case pc_get_jdata does not contain this.

      obj%max_static   = 40.0
      obj%fold_max     = 50
      obj%weight       = 0.5

      obj%num_iter     = 20
      obj%off_wid      = 1000.0
      obj%terms        = 3

      obj%pwr_cc       = 1.0      ! not in GUI
      obj%taper        = 1.0      ! not in GUI
      obj%update       = 0.8      ! not in GUI

      obj%sx_init      = 1.0
      obj%sx_inc       = 1.0
      obj%sx_last      = 1.0
      obj%sx_tot       = 1

      obj%sy_init      = 1.0
      obj%sy_inc       = 1.0
      obj%sy_last      = 1.0
      obj%sy_tot       = 1

      obj%rx_init      = 1.0
      obj%rx_inc       = 1.0
      obj%rx_last      = 1.0
      obj%rx_tot       = 1

      obj%ry_init      = 1.0
      obj%ry_inc       = 1.0
      obj%ry_last      = 1.0
      obj%ry_tot       = 1

      obj%comp_x       = 5
      obj%cmpx_init    = 1.0
      obj%cmpx_inc     = 1.0
      obj%cmpx_last    = 1.0
      obj%cmpx_tot     = 1

      obj%comp_y       = 1
      obj%cmpy_init    = 1.0
      obj%cmpy_inc     = 1.0
      obj%cmpy_last    = 1.0
      obj%cmpy_tot     = 1

      obj%path_src     = PATHCHECK_EMPTY
      obj%path_rec     = PATHCHECK_EMPTY
      obj%hdr_src      = 'SEQU'
      obj%hdr_rec      = 'SEQU'
      obj%corr_files   = 'TRC8'

      obj%opt_print    = 'MIN'
      obj%no_dead      = .false.
      obj%dead_end     = .true.
      obj%ncards       = 0

      call latwin_initialize (obj%latwin)

      call cc3d_update (obj)

      end subroutine cc3d_initialize


!!----------------------------- update -------------------------------------!!
!!----------------------------- update -------------------------------------!!
!!----------------------------- update -------------------------------------!!


      subroutine cc3d_update (obj)

      type(cc3d_struct),intent(inout),target :: obj                 ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      call cc3d_read_parameters        (obj)
      call cc3d_verify_parameters      (obj)

      call latwin_update (obj%latwin, obj%nwin)     ! new location (nwin needed by calculate_dependencies)

      call cc3d_calculate_dependencies (obj)
      call cc3d_write_parameters       (obj)

!     call latwin_update (obj%latwin, obj%nwin)     ! old location

      call cc3d_print_parameters       (obj)
      call cc3d_fetch_parallel_info    (obj)
      call cc3d_prepare_for_execution  (obj)

      end subroutine cc3d_update


!!------------------------------- traps -----------------------------------!!
!!------------------------------- traps -----------------------------------!!
!!------------------------------- traps -----------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine cc3d (obj,ntr,hd,tr)

      type(cc3d_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments
      integer                         :: i,err                  ! local
      logical                         :: error                  ! local

!  PROCESS INPUT TRACES

      if (ntr >= 1) then
           do i = 1,ntr

                call timer_start      (obj%timer)

                call cc3d_trace_entry (obj,hd(:,i),tr(:,i),  error)

                call timer_stop       (obj%timer)

                if (error) then
                     ntr = FATAL_ERROR
                     call cc3d_wrapup (obj)
                     call pc_print ('CC3D: FINISHED')
                     call pc_print (' ')
                     return
                end if

                if (.not.obj%dead_end) then
                     call statshift_write (obj%statshift,hd(:,i),tr(:,i),err)
                     if (err /= STATSHIFT_OK) then
                          ntr = FATAL_ERROR
                          call cc3d_wrapup (obj)
                          call pc_error ('CC3D: FATAL WRITE ERROR IN STATSHIFT')
                          call pc_print ('CC3D: FINISHED')
                          call pc_print (' ')
                          return
                     end if
                end if

           end do
           ntr = NEED_TRACES
           return
      end if

!  PROCESS NO MORE TRACES

      if (ntr == NO_MORE_TRACES) then

           call cc3d_report_timing  (obj,obj%timer,'TRACE INPUT TIMING')
           call timer_start         (obj%timer)

           call cc3d_solution_entry (obj,error)

           call timer_stop          (obj%timer)
           call cc3d_report_timing  (obj,obj%timer,'STATIC SOLUTION TIMING')

           if (error) then
                ntr = FATAL_ERROR
                call cc3d_wrapup (obj)
                call pc_print ('CC3D: FINISHED')
                call pc_print (' ')
                return
           end if

           if (.not.obj%dead_end) call statshift_rewind (obj%statshift)
           ntr = NEED_TRACES   ! to go into next IF block.

      end if

!  PROCESS NEED TRACES

      if (ntr == NEED_TRACES) then

           if (obj%dead_end) then
                ntr = NO_MORE_TRACES
                call cc3d_wrapup (obj)
                call pc_print ('CC3D: NOTHING PASSED TO NEXT CPS PROCESS')
                call pc_print ('CC3D: FINISHED')
                call pc_print (' ')
                return
           else
                call statshift_read (obj%statshift,hd(:,1),tr(:,1),err)

                if (err == STATSHIFT_ERROR) then
                     ntr = FATAL_ERROR
                     call cc3d_wrapup (obj)
                     call pc_print (' ')
                     call pc_error ('CC3D: FATAL READ ERROR IN STATSHIFT')
                     call pc_print ('CC3D: FINISHED')
                     call pc_print (' ')
                     return
                end if

                if (err == STATSHIFT_EOF) then
                     ntr = NO_MORE_TRACES
                     call cc3d_wrapup (obj)
                     call pc_print (' ')
                     call pc_print ('CC3D: FINISHED OUTPUTTING SHIFTED TRACES')
                     call pc_print ('CC3D: FINISHED')
                     call pc_print (' ')
                     return
                end if
           end if

      end if

!  RETURN ONE TRACE

      ntr = 1

      end subroutine cc3d


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine cc3d_wrapup (obj)

      type(cc3d_struct),intent(inout) :: obj            ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call statshift_close (obj%statshift)
      call temptfile_close (obj%temptfile)

      end subroutine cc3d_wrapup


!!------------------------- cc3d trace entry ------------------------------!!
!!------------------------- cc3d trace entry ------------------------------!!
!!------------------------- cc3d trace entry ------------------------------!!


      subroutine cc3d_TRACE_ENTRY (obj,HDI,TRI,error)

      type(cc3d_struct),intent(inout) :: obj                       ! arguments
      double precision,intent(in) :: HDI(:)                        ! arguments
      real         ,intent(in)    :: TRI(:)                        ! arguments
      logical      ,intent(out)   :: error                         ! arguments
      logical                     :: drop                          ! local
      integer                     :: ioff,IX,IY,ISRC,IREC,icmp     ! local
      integer                     :: ISUPER,ifold,NLIVE            ! local
      real                        :: xcoord_src,ycoord_src         ! local
      real                        :: xcoord_rec,ycoord_rec         ! local
      real                        :: xcoord_cmp,ycoord_cmp         ! local
      real                        :: offset                        ! local
      real                        :: trace(obj%nwin)        ! automatic array

!  INCREMENT COUNTER FOR THIS TRACE

      obj%ICOUNT = obj%ICOUNT + 1
      drop = .false.

!  GET TRACE WINDOW

      call latwin_get_window (obj%latwin,hdi,tri,   trace,nlive=nlive)

      if (NLIVE < obj%ncorr) then
            obj%IDROPD = obj%IDROPD + 1
            drop = .true.
      end if

!  DEFINE THE OFFSET INDEX

      offset = ABS(HDI(6))

      call increment_update (obj%increment_offseto, offset)

      ioff = nint(offset/obj%off_wid) + 1
      obj%noff = max(ioff,obj%noff)

!  DEFINE THE SEQUENTIAL SOURCE INDEX

      xcoord_src = obj%sx_init
      ycoord_src = obj%sy_init
      isrc = 1

      if (obj%use_src) then
           if (obj%hdr_sx > 0) xcoord_src = HDI(obj%hdr_sx)
           if (obj%hdr_sy > 0) ycoord_src = HDI(obj%hdr_sy)

           call increment_update (obj%increment_xsrco, xcoord_src)
           call increment_update (obj%increment_ysrco, ycoord_src)

         call mth_get_indices (xcoord_src,obj%sx_init,obj%sx_inc,obj%sx_tot, &
                               ycoord_src,obj%sy_init,obj%sy_inc,obj%sy_tot, &
                               IX,IY,ISRC)

           if (ISRC == 0) then
                 obj%IDROPS = obj%IDROPS + 1
                 drop = .true.
           end if
      end if

!  DEFINE THE SEQUENTIAL RECEIVER INDEX

      xcoord_rec = obj%rx_init
      ycoord_rec = obj%ry_init
      irec = 1

      if (obj%use_rec) then
           if (obj%hdr_rx > 0) xcoord_rec = HDI(obj%hdr_rx)
           if (obj%hdr_ry > 0) ycoord_rec = HDI(obj%hdr_ry)

           call increment_update (obj%increment_xreco, xcoord_rec)
           call increment_update (obj%increment_yreco, ycoord_rec)

         call mth_get_indices (xcoord_rec,obj%rx_init,obj%rx_inc,obj%rx_tot, &
                               ycoord_rec,obj%ry_init,obj%ry_inc,obj%ry_tot, &
                               IX,IY,IREC)

           if (IREC == 0) then
                 obj%IDROPR = obj%IDROPR + 1
                 drop = .true.
           end if
      end if

!  DEFINE THE SEQUENTIAL CMP INDEX

      xcoord_cmp = HDI(7)
      ycoord_cmp = HDI(8)

      call increment_update (obj%increment_xcmpo, xcoord_cmp)
      call increment_update (obj%increment_ycmpo, ycoord_cmp)

   call mth_get_indices (xcoord_cmp,obj%cmpx_init,obj%cmpx_inc,obj%cmpx_tot, &
                         ycoord_cmp,obj%cmpy_init,obj%cmpy_inc,obj%cmpy_tot, &
                         IX,IY,icmp)

      if (icmp == 0) then
            obj%IDROPC = obj%IDROPC + 1
            drop = .true.
      end if

!  DEFINE THE SEQUENTIAL SUPER-CMP INDEX

   call mth_get_indices (xcoord_cmp,obj%superxmin,obj%superxinc,obj%NXSUPER, &
                         ycoord_cmp,obj%superymin,obj%superyinc,obj%NYSUPER, &
                         IX,IY,ISUPER)

      if (ISUPER == 0) then
            obj%IDROPC = obj%IDROPC + 1
            drop = .true.
      else if (obj%ifold(ISUPER)+1 > obj%maxfolds) then
            obj%IDROPF = obj%IDROPF + 1
            drop = .true.
      end if

!  RETURN IF TRACE IS TO BE DROPPED

      if (drop) then
            obj%IDROP = obj%IDROP + 1
            error = .false.
            return
      end if

      obj%ifold(ISUPER) = obj%ifold(ISUPER) + 1    ! counts fold in super-cmp
      ifold             = obj%ifold(isuper)

!  SAVE INFORMATION ABOUT TRACE BEING USED

      call increment_update (obj%increment_xsrc, xcoord_src)
      call increment_update (obj%increment_ysrc, ycoord_src)
      call increment_update (obj%increment_xrec, xcoord_rec)
      call increment_update (obj%increment_yrec, ycoord_rec)
      call increment_update (obj%increment_xcmp, xcoord_cmp)
      call increment_update (obj%increment_ycmp, ycoord_cmp)
      call increment_update (obj%increment_offset, offset)

!  RETURN IF SUPER-CMP IS NOT WITHIN RANGE FOR THIS CPU

      if (isuper < obj%isuper_start .or. isuper > obj%isuper_stop) then
            error = .false.
            return
      end if

!  SAVE INDICES AND WRITE TRACE TO TEMPORARY TRACE WINDOW FILE

      call cc3d_write_trace_window (obj,isuper,ifold,     &
                                    isrc,irec,ioff,icmp,  &
                                    trace,error)
      if (error) return

      end subroutine cc3d_trace_entry


!!---------------------- cc3d solution entry ------------------------------!!
!!---------------------- cc3d solution entry ------------------------------!!
!!---------------------- cc3d solution entry ------------------------------!!


      subroutine cc3d_solution_entry (obj,error)

      type(cc3d_struct),intent(inout)  :: obj                ! arguments
      logical          ,intent(out)    :: error              ! arguments

      integer :: IORRR,IOSSS,IOHHH,IOCCC,iter                ! local
      real    :: pwr_super_first,pwr_comp_first              ! local
      integer :: timer1,timer2,timer3,timer4,timer5          ! local
      real    :: TAPR    (obj%ncorr)                         ! taper array
      real    :: stat    (obj%NVEC)                          ! statics values

!  PRINT SUMMARIES

      call cc3d_print_summaries (obj)       ! changed

!  COMPUTE THE TAPER ARRAY

      call statutil_taper (obj%taper,   TAPR,obj%ncorr,lunprint)

!  RETURN IF THERE IS NOTHING TO DO

      if (obj%icount == 0) then
           write(lunprint,*) 'CC3D: NO TRACES READ - NO SOLUTION PREPARED'
           error = .false.
           return
      else if (obj%icount - obj%idrop == 0) then
           write(lunprint,*) &
                    'CC3D: ALL TRACES WERE DROPPED - NO SOLUTION PREPARED'
           error = .false.
           return
      end if

!  SET STATIC ARRAY TO NILS

      stat(:) = FNIL

!  DEFINE MEMORY OFFSETS

      IORRR = 0                                 ! offset to receiver values
      IOSSS = obj%NREC                          ! offset to source values
      IOHHH = obj%NREC + obj%NSRC               ! offset to offset values
      IOCCC = obj%NREC + obj%NSRC + obj%NSUPER  ! offset to super-cmp values

!  START TIMINGS

      call timer_alloc (timer1)
      call timer_alloc (timer2)
      call timer_alloc (timer3)
      call timer_alloc (timer4)
      call timer_alloc (timer5)

      call timer_clear (timer1)
      call timer_clear (timer2)
      call timer_clear (timer3)
      call timer_clear (timer4)
      call timer_clear (timer5)

!  DO ITERATIONS

      do iter = 1,obj%num_iter

           call cc3d_perform_iteration                  &
                 (obj,                                  & ! inout  
                  iorrr,iosss,iohhh,ioccc,              & ! in
                  iter,                                 & ! in
                  tapr,                                 & ! in
                  stat,                                 & ! inout    (nvec)
                  pwr_super_first,pwr_comp_first,       & ! out
                  timer1,timer2,timer3,timer4,timer5,   & ! in
                  error)                                  ! out

           if (error) then
                call pc_error ('CC3D: fatal error in cc3d_perform_iteration')
                call pc_print ('CC3D: ITER =',iter,'out of',obj%num_iter)
                return
           end if

      end do

!  REPORT TIMINGS

   call cc3d_report_timing (obj,timer1,'PROCESS SUPER-CMPS (part of solution)')
   call cc3d_report_timing (obj,timer2, 'FORM CORRELATIONS (part of solution)')
   call cc3d_report_timing (obj,timer3,     'MERGE RESULTS (part of solution)')
   call cc3d_report_timing (obj,timer4,    'DERIVE STATICS (part of solution)')
   call cc3d_report_timing (obj,timer5, 'SAVE STATIC FILES (part of solution)')

      call timer_free (timer1)
      call timer_free (timer2)
      call timer_free (timer3)
      call timer_free (timer4)
      call timer_free (timer5)

!  CLOSE TEMPORARY TRACE WINDOW FILE

      write(lunprint,*)' '
      call temptfile_close (obj%temptfile)

!  FINISH UP AND RETURN

      if(associated(obj%ifold))    deallocate (obj%ifold)
      if(associated(obj%cards))    deallocate (obj%cards)

      write(lunprint,*)' '
      write(lunprint,*)'CC3D: FINISHED WITH STATIC SOLUTION'
      write(lunprint,*)' '
      error = .false.

      end subroutine cc3d_solution_entry


!!--------------------------- cc3d save files -----------------------------!!
!!--------------------------- cc3d save files -----------------------------!!
!!--------------------------- cc3d save files -----------------------------!!

        ! Save static files (and optionally stacked correlations)
        ! for both source and receiver terms only.

                            ! boss only.

      subroutine cc3d_save_files          &
             (obj,                        & ! in
              iorrr,iosss,                & ! in
              stat,                       & ! in   (nvec)
              ntraces,                    & ! in   (nvec)
              stacked_correlations,       & ! in   (ncorr16,nvec2)
              iter,                       & ! in
              error)                        ! out

      type(cc3d_struct),intent(in)    :: obj                        ! arguments
      integer          ,intent(in)    :: iorrr,iosss                ! arguments
      real             ,intent(in)    :: stat                 (:)   ! arguments
      integer          ,intent(in)    :: NTRACES              (:)   ! arguments
      integer          ,intent(in)    :: stacked_correlations (:,:) ! arguments
      integer          ,intent(in)    :: iter                       ! arguments
      logical          ,intent(out)   :: error                      ! arguments

      call cc3d_dispose_file                       &
             (obj,                                 & ! in
              obj%path_src,obj%path_byt_src,       & ! in
              'SOURCE',                            & ! in
              obj%hdr_sx,obj%hdr_sy,               & ! in
              obj%sx_init,obj%sy_init,             & ! in
              obj%sx_inc,obj%sy_inc,               & ! in
              obj%sx_tot,obj%sy_tot,               & ! in
              stat                   (IOSSS+1:),   & ! in  (nsrc)
              ntraces                (IOSSS+1:),   & ! in  (nsrc)
              stacked_correlations (:,IOSSS+1:),   & ! in  (ncorr16,nsrc)
              iter,                                & ! in
              error)                                 ! out

      if (error) return

      call cc3d_dispose_file                       &
             (obj,                                 & ! in
              obj%path_rec,obj%path_byt_rec,       & ! in
              'RECEIVER',                          & ! in
              obj%hdr_rx,obj%hdr_ry,               & ! in
              obj%rx_init,obj%ry_init,             & ! in
              obj%rx_inc,obj%ry_inc,               & ! in
              obj%rx_tot,obj%ry_tot,               & ! in
              stat                   (IORRR+1:),   & ! in  (nrec)
              ntraces                (IORRR+1:),   & ! in  (nrec)
              stacked_correlations (:,IORRR+1:),   & ! in  (ncorr16,nrec)
              iter,                                & ! in
              error)                                 ! out

      if (error) return

      end subroutine cc3d_save_files


!!------------------------ cc3d dispose file -----------------------------!!
!!------------------------ cc3d dispose file -----------------------------!!
!!------------------------ cc3d dispose file -----------------------------!!

        ! Save static file (and optionally stacked correlations)
        ! for either source or receiver terms only.

                            ! boss only.

      subroutine cc3d_dispose_file                    &
                    (obj,                             & ! in
                     path,path_byt,                   & ! in
                     adjective,                       & ! in
                     NHX,NHY,X1,Y1,Xinc,Yinc,NX,NY,   & ! in
                     stat,                            & ! in (ngp)
                     ntraces,                         & ! in (ngp)
                     stacked_correlations,            & ! in (ncorr16,ngp)
                     iter,                            & ! in
                     error)                             ! out

      type(cc3d_struct),intent(in)    :: obj                        ! arguments
      character(len=*) ,intent(in)    :: path,path_byt              ! arguments
      character(len=*) ,intent(in)    :: adjective                  ! arguments
      integer          ,intent(in)    :: nhx,nhy,nx,ny              ! arguments
      real             ,intent(in)    :: x1,y1,xinc,yinc            ! arguments
      real             ,intent(in)    :: stat                 (:)   ! arguments
      integer          ,intent(in)    :: NTRACES              (:)   ! arguments
      integer          ,intent(in)    :: stacked_correlations (:,:) ! arguments
      integer          ,intent(in)    :: iter                       ! arguments
      logical          ,intent(out)   :: error                      ! arguments
      real                            :: statms(nx*ny)              ! local

      error = .false.
      if (path == PATHCHECK_EMPTY) return

      statms = stat(1:nx*ny)
      where (statms(:) /= FNIL) statms(:) = statms(:) * obj%dtms

      call cc3d_DISPOSE_stat (obj,path,adjective,             & ! in
                              NHX,NHY,X1,Y1,Xinc,Yinc,NX,NY,  & ! in
                              statms,                         & ! in
                              iter,                           & ! in
                              error)                            ! out
      if (error) return

      call cc3d_DISPOSE_corr (obj,path_byt,adjective,         & ! in
                              NHX,NHY,X1,Y1,Xinc,Yinc,NX,NY,  & ! in
                              statms,                         & ! in
                              NTRACES,stacked_correlations,   & ! in
                              iter,                           & ! in
                              error)                            ! out

      end subroutine cc3d_dispose_file


!!---------------------------- cc3d add16 --------------------------------!!
!!---------------------------- cc3d add16 --------------------------------!!
!!---------------------------- cc3d add16 --------------------------------!!


      subroutine cc3d_add16 (array,narray,   sum,narray16)

      integer,intent(in)    :: narray,narray16          ! arguments
      real   ,intent(in)    :: array(:)                 ! arguments
      integer,intent(inout) :: sum(:)                   ! arguments
      real                  :: scratch(narray)          ! local automatic

      call pkutil_unpack16 (sum,narray16,     scratch,narray)
      call mth_add         (array,narray,     scratch)
      call pkutil_pack16   (scratch,narray,   sum,narray16)

      end subroutine cc3d_add16


!!---------------------------- cc3d add1 ----------------------------------!!
!!---------------------------- cc3d add1 ----------------------------------!!
!!---------------------------- cc3d add1 ----------------------------------!!


      subroutine cc3d_add1 (indx,narray,   sum,narray1)

      integer,intent(in)    :: indx,narray,narray1      ! arguments
      integer,intent(inout) :: sum(:)                   ! arguments
      real                  :: scratch(narray)          ! local automatic

      call pkutil_unpack1 (sum,narray1,   scratch,narray)
      scratch(indx) = 1
      call pkutil_pack1   (scratch,narray,   sum,narray1)

      end subroutine cc3d_add1


!!---------------------- cc3d form static correlations --------------------!!
!!---------------------- cc3d form static correlations --------------------!!
!!---------------------- cc3d form static correlations --------------------!!

         ! Successively remove each of the "partial stack" traces
         ! from the stack of the super-CMP, and correlate
         ! the partial stack with this "full stack".  For TERMS=1,
         ! use only the portion of the "full stack" with CMPs which
         ! match the CMPs present in the partial stack for that
         ! ground position.  The "partial stack" traces are all traces
         ! in the super-CMP which have the same source or receiver
         ! ground position.
         !
         ! Pick the correlation to get an "IMS" static estimate
         ! for the corresponding source or receiver ground position,
         ! and sum the shift into an average for that ground position.
         !
         ! Stack the correlation with others from the same ground position.


      subroutine cc3d_form_static_correlations           &
                 (obj,                                   &   ! in
                  NGP,                                   &   ! in
                  TAPR,                                  &   ! in
                  SUPER_STACK,                           &   ! in
                  stacked_cmps,                          &   ! in (fish)
                  stacked_ground_positions,              &   ! in
                  exists_in_super,                       &   ! in
                  exists_in_cmp,                         &   ! in (fish)
                  rstat,                                 &   ! add to
                  WGHTS,                                 &   ! add to
                  NCCOEF,                                &   ! add to
                  stacked_correlations,                  &   ! add to
                  PWR_COMP)                                  ! add to

      type(cc3d_struct),intent(in) :: obj                           ! arguments
      integer          ,intent(in) :: NGP                           ! arguments
      real             ,intent(in) :: tapr(:)                       ! arguments
      real             ,intent(in) :: SUPER_STACK(:)                ! arguments
      real             ,intent(in) :: stacked_cmps(:,:)             ! arguments
      integer          ,intent(in) :: stacked_ground_positions(:,:) ! arguments
      logical          ,intent(in) :: exists_in_super(:)            ! arguments
      integer          ,intent(in) :: exists_in_cmp(:,:)            ! arguments
      real          ,intent(inout) :: rstat(:)                      ! arguments
      real          ,intent(inout) :: WGHTS(:)                      ! arguments
      integer       ,intent(inout) :: NCCOEF(:)                     ! arguments
      integer       ,intent(inout) :: stacked_correlations(:,:)     ! arguments
      real          ,intent(inout) :: pwr_comp                      ! arguments

      real    :: shft,peak,wt                         ! local
      integer :: igp,icomp                            ! local
      real    :: corr       (obj%ncorr)               ! local automatic
      real    :: is_in_cmp  (obj%ncomp)               ! local automatic
      real    :: GP_STACK   (obj%nwin)                ! local automatic
      real    :: FISH_STACK (obj%nwin)                ! local automatic

      do IGP = 1,NGP
        if (exists_in_super(igp)) then
             call pkutil_unpack16 (stacked_ground_positions(:,igp),  &
                                   obj%nwin16, GP_STACK,obj%nwin)

             if (obj%fish_style) then
                  FISH_STACK(:) = 0.0
                  call pkutil_unpack1 (exists_in_cmp(:,igp),obj%NCOMP1,  &
                                       is_in_cmp,obj%NCOMP)

                  do ICOMP = 1,obj%NCOMP
                       if (is_in_cmp(ICOMP) >= 1) then
                            call mth_add (stacked_cmps(:,ICOMP),  &
                                          obj%nwin, FISH_STACK)
                       end if
                  end do

                  call statutil_corr_enhanced (GP_STACK,FISH_STACK,  &
                               obj%nwin,corr,obj%ncorr,NORMALIZE,SUBTRACT,tapr)
             else
                  call statutil_corr_enhanced (GP_STACK,SUPER_STACK,  &
                               obj%nwin,corr,obj%ncorr,NORMALIZE,SUBTRACT,tapr)
             end if

             call statutil_pick (corr,obj%ncorr,   SHFT,PEAK)
             call mth_power     (GP_STACK,obj%nwin,   PWR_COMP)

             if (PEAK > 0.0) then
                  WT = PEAK**obj%pwr_cc
                  rstat (IGP) = rstat(IGP) + SHFT*WT
                  WGHTS (IGP) = WGHTS(IGP) + WT
                  NCCOEF(IGP) = NCCOEF(IGP) + 1
                  call cc3d_add16 (corr,obj%ncorr,  &
                                   stacked_correlations(:,igp),obj%ncorr16)
             end if
        end if
      end do

      end subroutine cc3d_form_static_correlations


!!--------------------- cc3d pick stacked correlations --------------------!!
!!--------------------- cc3d pick stacked correlations --------------------!!
!!--------------------- cc3d pick stacked correlations --------------------!!

         ! Pick stacked correlations to get a "FISH" type static
         ! estimate, and use the WEIGHT term to average it with the
         ! average picks (IMS) to form the estimated residual static.

                            ! boss only.

      subroutine cc3d_pick_stacked_correlations   &
                     (obj,                        & ! in   
                      stacked_correlations,       & ! in       (ncorr16,nvec2)
                      rstat,                      & ! changed  (nvec)
                      WGHTS,                      & ! in       (nvec2)
                      NCCOEF,                     & ! in       (nvec2)
                      CCOEF,                      & ! out      (nvec2)
                      RMSE,RMSEC)                   ! out

      type(cc3d_struct),intent(in)    :: obj                        ! arguments
      integer          ,intent(in)    :: stacked_correlations (:,:) ! arguments
      real             ,intent(inout) :: rstat                (:)   ! arguments
      real             ,intent(in)    :: WGHTS                (:)   ! arguments
      integer          ,intent(in)    :: nccoef               (:)   ! arguments
      real             ,intent(out)   :: ccoef                (:)   ! arguments
      real             ,intent(out)   :: rmse,rmsec                 ! arguments
      real                            :: ssqec,ssqe,shft,peak       ! local
      real                            :: w1,w2                      ! local
      integer                         :: nsqe,ivec                  ! local
      real                            :: scratch(obj%ncorr)   ! local automatic

      SSQEC = 0.0
      SSQE = 0.0
      NSQE = 0
      w1   = obj%weight
      w2   = 1.0 - obj%weight

      do ivec = 1,obj%nvec2
          if (WGHTS(ivec) > 0.0) then
               rstat(ivec) = rstat(ivec) / WGHTS(ivec)
               SSQE        = SSQE + rstat(ivec)**2
               NSQE        = NSQE + 1

               call pkutil_unpack16 (stacked_correlations(:,ivec),  &
                                     obj%ncorr16, scratch, obj%ncorr)

               call statutil_pick   (scratch,obj%ncorr,   SHFT,PEAK)

               if (PEAK > 0.0) then
                    SSQEC = SSQEC + SHFT**2

!  AVERAGE THE PICK OF STACKED CORRELATION WITH THE AVERAGE OF RAW PICKS

                    rstat(ivec) = w1 * SHFT + w2 * rstat(ivec)
               end if

!  CALCULATE CORRELATION COEFFICIENTS

               if (NCCOEF(ivec) > 0) then
                    CCOEF(ivec) = PEAK / NCCOEF(ivec)
               else
                    CCOEF(ivec) = 0.0
               end if
          end if
      end do

!  FORM THE SQUARED ERROR

      if (NSQE >= 1) then
           RMSE  = SQRT(SSQE  / NSQE)
           RMSEC = SQRT(SSQEC / NSQE)
      else
           RMSE  = 0.0
           RMSEC = 0.0
      end if

      end subroutine cc3d_pick_stacked_correlations


!!------------------------ update statics ---------------------------!!
!!------------------------ update statics ---------------------------!!
!!------------------------ update statics ---------------------------!!

        ! Sum the residual statics from this iteration into the
        ! total static estimate.

                            ! boss only.

      subroutine cc3d_update_statics          &
                        (obj,                 & ! in   
                         rstat,               & ! in       (nvec)
                         nccoef,              & ! in       (nvec2)
                         ccoef,               & ! in       (nvec2)
                         stat,                & ! changed  (nvec)
                         RMSstat,ccaverage)     ! out   

      type(cc3d_struct),intent(in)    :: obj                    ! arguments
      real             ,intent(in)    :: rstat(:)               ! arguments
      integer          ,intent(in)    :: nccoef(:)              ! arguments
      real             ,intent(in)    :: ccoef(:)               ! arguments
      real             ,intent(inout) :: stat(:)                ! arguments
      real             ,intent(out)   :: rmsstat,ccaverage      ! arguments
      integer                         :: ivec,kount             ! local

!  UPDATE TOTAL STATICS WITH RESIDUALS OF PREVIOUS PASS

      do IVEC = 1,obj%NVEC
          stat(IVEC) = stat(IVEC) + obj%update * rstat(IVEC)
      end do

!  GET RMS TOTAL STATIC

      RMSstat = 0.0
      KOUNT   = 0
      do IVEC = 1,obj%NVEC2
           if (stat(IVEC) /= FNIL) then
                RMSstat = RMSstat + stat(IVEC)**2
                KOUNT   = KOUNT + 1
           end if
      end do
      if (KOUNT >= 1) RMSstat = SQRT(RMSstat / KOUNT)

!  GET AVERAGE CORRELATION COEFFICIENT

      CCAVERAGE = 0.0
      KOUNT     = 0
      do IVEC = 1,obj%NVEC2
           if (NCCOEF(IVEC) > 0) then
                CCAVERAGE = CCAVERAGE + ccoef(IVEC)
                KOUNT     = KOUNT + 1
           end if
      end do
      if (KOUNT >= 1) CCAVERAGE = CCAVERAGE / KOUNT

      end subroutine cc3d_update_statics


!!--------------------- print iteration results ----------------------!!
!!--------------------- print iteration results ----------------------!!
!!--------------------- print iteration results ----------------------!!

                            ! boss only.

      subroutine cc3d_print_iteration_results                    &
                              (obj,iter,sum_exists,max_exists,   & ! in
                               RMSE,RMSEC,PWR_SUPER,PWR_COMP,    & ! in
                               PWR_SUPER_FIRST,PWR_COMP_FIRST,   & ! in
                               RMSstat,CCAVERAGE)                  ! in

      type(cc3d_struct),intent(in)    :: obj                    ! arguments
      integer          ,intent(in)    :: iter                   ! arguments
      real             ,intent(in)    :: SUM_exists             ! arguments
      integer          ,intent(in)    :: max_exists             ! arguments
      real             ,intent(in)    :: rmse,rmsec             ! arguments
      real             ,intent(in)    :: pwr_super              ! arguments
      real             ,intent(in)    :: pwr_comp               ! arguments
      real             ,intent(in)    :: pwr_super_first        ! arguments
      real             ,intent(in)    :: pwr_comp_first         ! arguments
      real             ,intent(in)    :: rmsstat                ! arguments
      real             ,intent(in)    :: ccaverage              ! arguments
      character(len=24)               :: timedate               ! local
      integer                         :: iav_exists             ! local

!  PRINT ALLOCATION INFO

      if (iter == 1) then
           IAV_exists = nint(SUM_exists/obj%NSUPER)
           write(lunprint,*)' '
           write(lunprint,*)'CC3D: NUMBER OF COMMON-GROUND-POSITION',    &
                                    ' COMPOSITED TRACE WINDOWS PER SUPER-CMP:'
           write(lunprint,*)'CC3D:    AVERAGE NUMBER = ',IAV_exists,    &
                                 '  (',IAV_exists*obj%nwin16,' words)'
           write(lunprint,*)'CC3D:    MAXIMUM NUMBER = ',max_exists,    &
                                 '  (',max_exists*obj%nwin16,' words)'
      end if

!  PRINT ITERATION SUMMARY HEADER

      if (iter == 1) then
           write(lunprint,*)' '
           write(lunprint,3401)
           write(lunprint,3402)
           write(lunprint,3403)
           write(lunprint,3404)
3401       format ('            time and date         --RMS RESID STATIC-- &
                     &    ----------POWER--------     RMS       AVERAGE')
3402       format ('              iteration           INDIVIDUAL   STACKED &
                     &    SUPER-STACK   COMPOSITE    TOTAL        CORR')
3403       format (' PASS         finished              CORRS       CORRS  &
                     &      TRACES       TRACES     STATIC        COEF')
3404       format ('                                    (ms)        (ms)   &
                     &                               (ms)')
      end if

!  PRINT ITERATION SUMMARY LINE

      call string_time_date (timedate)

      write(lunprint, '(1X,I3,3x,a24,F11.3,F12.3,2X,4F12.3)')          &
                           iter,timedate,RMSE*obj%dtms,RMSEC*obj%dtms, &
                           PWR_SUPER/PWR_SUPER_FIRST,                  &
                           PWR_COMP/PWR_COMP_FIRST,                    &
                           RMSstat*obj%dtms,CCAVERAGE

      end subroutine cc3d_print_iteration_results


!!-------------------- cc3d form offset correlations -----------------------!!
!!-------------------- cc3d form offset correlations -----------------------!!
!!-------------------- cc3d form offset correlations -----------------------!!

         ! Successively remove each of the "partial stack" traces
         ! from the stack of the super-CMP, and correlate the
         ! partial stack with this "full stack".  The "partial stack"
         ! traces are all traces in the super-CMP which fall into
         ! the same offset bin.
         !
         ! Pick the correlation to get an "IMS" static estimate
         ! for the corresponding offset.
         !
         ! Use the picks to form a least-squares estimate of the
         ! residual moveout coefficient for this super-CMP.


      subroutine cc3d_form_offset_correlations             &
                                   (obj,                   &
                                    TAPR,                  &
                                    SUPER_STACK,           &
                                    stacked_offsets,       &
                                    rstat)

      type(cc3d_struct),intent(in)  :: obj                    ! arguments
      real             ,intent(in)  :: tapr            (:)    ! arguments
      real             ,intent(in)  :: SUPER_STACK     (:)    ! arguments
      real             ,intent(in)  :: stacked_offsets (:,:)  ! arguments
      real             ,intent(out) :: rstat                  ! arguments
      real                          :: sumhi4,shft,peak       ! local
      integer                       :: ioff                   ! local
      real                          :: corr(obj%ncorr)        ! local automatic

      rstat  = 0.0
      SUMHI4 = 0.0

      do ioff = 1,obj%noff
             call statutil_corr_enhanced (stacked_offsets(:,ioff),  &
                                          SUPER_STACK,obj%nwin,     &
                                          corr,obj%ncorr,           &
                                          NORMALIZE,SUBTRACT,tapr)

             call statutil_pick          (corr,obj%ncorr,   SHFT,PEAK)

             if (PEAK > 0.0) then
                       rstat  = rstat + SHFT*(ioff**2)
                       SUMHI4 = SUMHI4 + ioff**4
             end if
      end do

      if (SUMHI4 > 0.0) rstat = rstat / SUMHI4

      end subroutine cc3d_form_offset_correlations


!!---------------------- cc3d form cmp correlations ------------------------!!
!!---------------------- cc3d form cmp correlations ------------------------!!
!!---------------------- cc3d form cmp correlations ------------------------!!

         ! Successively remove each of the "partial stack" traces
         ! from the stack of the super-CMP, and correlate the
         ! partial stack with this "full stack".  The "partial stack"
         ! traces are all traces in the super-CMP which fall into
         ! the same CMP.
         !
         ! Pick the correlation to get an "IMS" static estimate
         ! for the corresponding CMP.
         !
         ! Use the picks to form least-squares estimates of the
         ! X and Y CMP dips for this super-CMP.


      subroutine cc3d_form_cmp_correlations         &
                           (obj,                    & ! in
                            TAPR,                   & ! in
                            SUPER_STACK,            & ! in
                            stacked_cmps,           & ! in
                            rstata,rstatb)            ! out

      type(cc3d_struct),intent(in)  :: obj                          ! arguments
      real             ,intent(in)  :: tapr          (:)            ! arguments
      real             ,intent(in)  :: SUPER_STACK   (:)            ! arguments
      real             ,intent(in)  :: stacked_cmps  (:,:)          ! arguments
      real             ,intent(out) :: rstata,rstatb                ! arguments
      real                          :: sumdx2,sumdy2,sumdx,sumdy    ! local
      real                          :: shft,peak                    ! local
      real                          :: delcmpx,delcmpy              ! local
      real                          :: det,beta,gama                ! local
      integer                       :: icomp                        ! local
      real                          :: corr(obj%ncorr)        ! local automatic

      rstata = 0.0
      rstatb = 0.0
      SUMDX2 = 0.0
      SUMDY2 = 0.0
      SUMDX  = 0.0
      SUMDY  = 0.0

      do ICOMP = 1,obj%NCOMP
            call statutil_corr_enhanced (stacked_cmps(:,ICOMP),         &
                                         SUPER_STACK,obj%nwin,          &
                                         corr,obj%ncorr,                &
                                         NORMALIZE,SUBTRACT,tapr)

            call statutil_pick          (corr,obj%ncorr,   SHFT,PEAK)

            if (PEAK > 0.0) then

                 call cc3d_get_delcmpx_delcmpy (obj,icomp,   delcmpx,delcmpy)

                 rstata  = rstata + SHFT * delcmpx
                 rstatb  = rstatb + SHFT * delcmpy
                 SUMDX2  = SUMDX2 + delcmpx**2
                 SUMDY2  = SUMDY2 + delcmpy**2
                 SUMDX   = SUMDX + delcmpx
                 SUMDY   = SUMDY + delcmpy
            end if
      end do

      DET = obj%comp_y*SUMDX2*obj%comp_x*SUMDY2 - (SUMDX*SUMDY)**2

      if (DET /= 0.0) then
            BETA = rstata * obj%comp_x * SUMDY2 - rstatb * SUMDX * SUMDY
            GAMA = rstatb * obj%comp_y * SUMDX2 - rstata * SUMDX * SUMDY
            rstata = BETA/DET
            rstatb = GAMA/DET
      else
            if (SUMDY2 > 0.0) then
                     rstata = 0.0
                     rstatb = rstatb / (obj%comp_x*SUMDY2)
            end if
            if (SUMDX2 > 0.0) then
                     rstata = rstata / (obj%comp_y*SUMDX2)
            end if
      end if

      end subroutine cc3d_form_cmp_correlations


!!----------------------- cc3d dispose stat -------------------------------!!
!!----------------------- cc3d dispose stat -------------------------------!!
!!----------------------- cc3d dispose stat -------------------------------!!

!     dispose statics to static file.
!     adjective = 'RECEIVER' or 'SOURCE'.

                            ! boss only.

      subroutine cc3d_DISPOSE_stat (obj,filename,adjective,         & ! in
                                    NHX,NHY,X1,Y1,Xinc,Yinc,NX,NY,  & ! in
                                    statms,                         & ! in
                                    iter,                           & ! in
                                    error)                            ! out

      type(cc3d_struct),intent(in)  :: obj                          ! arguments
      character(len=*) ,intent(in)  :: FILENAME,adjective           ! arguments
      integer          ,intent(in)  :: nhx,nhy,nx,ny                ! arguments
      real             ,intent(in)  :: x1,y1,xinc,yinc,statms(:)    ! arguments
      integer          ,intent(in)  :: iter                         ! arguments
      logical          ,intent(out) :: error                        ! arguments
      character(len=80)             :: msg                          ! local
      integer                       :: err,luntemp                  ! local
      character(len=80)             :: cards(obj%ncards+1)          ! local

      if (iter == obj%num_iter) then
           luntemp = lunprint
      else
           luntemp = 0
      end if

      if (FILENAME == PATHCHECK_EMPTY) then
        if (luntemp > 0) then
             write(luntemp,*) ' '
             write(luntemp,*) 'CC3D: NO ',adjective,' STATIC FILE OUTPUT'
             write(luntemp,*) ' '
        end if
        error = .false.
        return
      end if

      cards(1:obj%ncards) = obj%cards(1:obj%ncards)

      call string_encode (cards(obj%ncards+1),                  &
                          'CC3D static file saved after',       &
                          iter,'of',obj%num_iter,'iterations')

      call statio_write_file (filename,'RESID',                    &
                              NHX,NHY,0,0,X1,Y1,Xinc,Yinc,NX,NY,   &
                              statms,err,msg,cards,obj%ncards+1,   &
                              'CC3D',luntemp)

      error = (err /= STATIO_OK)
      if (error) call pc_error ('CC3D:',msg)

      end subroutine cc3d_DISPOSE_stat


!!----------------------- cc3d fetch corr -------------------------------!!
!!----------------------- cc3d fetch corr -------------------------------!!
!!----------------------- cc3d fetch corr -------------------------------!!

!     fetch next stacked correlation HD(HDR_NOMINAL_SIZE) and TR(Ncorr).
!     the stacked correlation is first shifted by the negative of the static.
!     IGP = sequential ground position index.

                            ! boss only.

      subroutine cc3d_fetch_corr (obj,IGP,                       &
                                  NHX,NHY,X1,Y1,Xinc,Yinc,NX,    &
                                  statms,NTRACES,                &
                                  stacked_correlations,          &
                                  HD,TR)

      type(cc3d_struct),intent(in)  :: obj                     ! arguments
      integer          ,intent(in)  :: igp,nhx,nhy,nx          ! arguments
      integer          ,intent(in)  :: ntraces                 ! arguments
      real             ,intent(in)  :: x1,y1,xinc,yinc,statms  ! arguments
      integer          ,intent(in)  :: stacked_correlations(:) ! arguments
      double precision ,intent(out) :: HD(:)                   ! arguments
      real             ,intent(out) :: TR(:)                   ! arguments
      integer                       :: ix,iy                   ! local
      real                          :: scratch(obj%Ncorr)      ! local automatic

      HD( :) = 0.0                        ! clears entire array
      HD( 1) = IGP                        ! sequential trace number
      HD( 2) = 1                          ! head mute
      HD( 5) = NTRACES                    ! # traces in correlation
      HD(28) = (obj%Ncorr/2) * obj%dt     ! center of correlation (sec)
      HD(43) = statms                     ! static (ms)
      HD(64) = obj%Ncorr                  ! tail mute
      call mth_split_index (IGP,NX,   IX,IY)
      HD(NHX) = X1 + (IX-1)*Xinc
      HD(NHY) = Y1 + (IY-1)*Yinc

      call pkutil_unpack16 (stacked_correlations,obj%Ncorr16, &
                            scratch,obj%Ncorr)

      call statcc          (-statms/obj%dtms,obj%ncorr,scratch,  TR)

      end subroutine cc3d_fetch_corr


!!----------------------- cc3d dispose corr -------------------------------!!
!!----------------------- cc3d dispose corr -------------------------------!!
!!----------------------- cc3d dispose corr -------------------------------!!

!     dispose correlation functions to byte file.
!     adjective = 'RECEIVER' or 'SOURCE'.

                            ! boss only.

      subroutine cc3d_DISPOSE_corr (obj,filename,adjective,         & ! in
                                    NHX,NHY,X1,Y1,Xinc,Yinc,NX,NY,  & ! in
                                    statms,                         & ! in
                                    NTRACES,stacked_correlations,   & ! in
                                    iter,                           & ! in
                                    error)                            ! out

      type(cc3d_struct),intent(in)   :: obj                         ! arguments
      character(len=*) ,intent(in)   :: FILENAME,adjective          ! arguments
      integer          ,intent(in)   :: nhx,nhy,nx,ny               ! arguments
      real             ,intent(in)   :: x1,y1,xinc,yinc             ! arguments
      real             ,intent(in)   :: statms(:)                   ! arguments
      integer          ,intent(in)   :: NTRACES(:)                  ! arguments
      integer          ,intent(in)   :: stacked_correlations(:,:)   ! arguments
      integer          ,intent(in)   :: iter                        ! arguments
      logical          ,intent(out)  :: error                       ! arguments
      integer                        :: ngp,igp,err,luntemp         ! local
      type(permtfile_struct),pointer :: permtfile                   ! local
      double precision               :: hd(HDR_NOMINAL_SIZE)  ! local automatic
      real                           :: tr(obj%ncorr)         ! local automatic

!---------- get started:

      nullify (permtfile) ! jpa

      if (iter == obj%num_iter) then
           luntemp = lunprint
      else
           luntemp = 0
      end if

      if (FILENAME == PATHCHECK_EMPTY) then
        if (luntemp > 0) then
          write(luntemp,*) ' '
          write(luntemp,*) 'CC3D: NO ',adjective,' STACKED CORRELATIONS OUTPUT'
          write(luntemp,*) ' '
        end if
        error = .false.
        return
      end if

      NGP    = NX*NY

      if (luntemp > 0) then
           write(luntemp,*) ' '
           write(luntemp,*)'CC3D: BEGINNING TO OUTPUT ',NGP, &
                    ' STACKED CORRELATIONS TO ',adjective,' CORRELATION FILE'
      end if

      call permtfile_open_write (permtfile,filename,HDR_NOMINAL_SIZE, &
                                 obj%ncorr,0.0,obj%dt,luntemp,err,obj%ipn,8)

      if (err /= PERMTFILE_OK) then
           write(lunprint,*)'CC3D: ERROR OPENING ',adjective,' CORRELATION FILE'
           error = .true.
           return
      end if

!---------- output the correlations:

      do IGP = 1,NGP
           call cc3d_fetch_corr (obj,IGP,                                 &
                                 NHX,NHY,X1,Y1,Xinc,Yinc,NX,              &
                                 statms(IGP),NTRACES(IGP),                &
                                 stacked_correlations(:,IGP),             &
                                 HD,TR)
           if (obj%no_dead) then
                if (mth_search_unequal(obj%ncorr,tr,1,0.0) == 0) cycle
           end if

           call permtfile_write (permtfile,HD,TR,err)

           if (err /= PERMTFILE_OK) then
                write(lunprint,*)'CC3D: ERROR WRITING CORRELATION ',igp, &
                                  ' TO ',adjective,' CORRELATION FILE'
                call permtfile_close (permtfile)
                error = .true.
                return
           end if
      end do

!---------- finish up and return:

      if (luntemp > 0) then
           write(luntemp,*)'CC3D: ',NGP,' STACKED CORRELATIONS',         &
               ' HAVE BEEN OUTPUT TO ',adjective,' CORRELATION FILE'
           write(luntemp,*)' '
      end if

      call permtfile_close (permtfile)
      error = .false.

      end subroutine cc3d_DISPOSE_corr


!!------------------------ cc3d print summary1 ------------------------------!!
!!------------------------ cc3d print summary1 ------------------------------!!
!!------------------------ cc3d print summary1 ------------------------------!!

                           ! word = REQUESTED.


      subroutine cc3d_print_summary1 (obj,word)

      type(cc3d_struct),intent(in) :: obj                        ! arguments
      character(len=*) ,intent(in) :: word                       ! arguments

 write(lunprint,*)' '
 write(lunprint,*)'CC3D: '//word//' TRACES:'
 write(lunprint,*)'CC3D:              HWD     MINIMUM    MAXIMUM  INCREMENT', &
                                                     '  NUMBER  COMPOSITE'
 write(lunprint,2222)'X-SOURCE'  ,obj%hdr_sx,obj%sx_init  ,obj%sx_last  ,    &
                                             obj%sx_inc   , obj%sx_tot
 write(lunprint,2222)'Y-SOURCE'  ,obj%hdr_sy,obj%sy_init  ,obj%sy_last  ,    &
                                             obj%sy_inc   , obj%sy_tot
 write(lunprint,2222)'X-RECEIVER',obj%hdr_rx,obj%rx_init  ,obj%rx_last  ,    &
                                             obj%rx_inc   , obj%rx_tot
 write(lunprint,2222)'Y-RECEIVER',obj%hdr_ry,obj%ry_init  ,obj%ry_last  ,    &
                                             obj%ry_inc   , obj%ry_tot
 write(lunprint,2222)'X-CMP'     ,   7      ,obj%cmpx_init,obj%cmpx_last,    &
                                             obj%cmpx_inc , obj%cmpx_tot
 write(lunprint,2222)'Y-CMP'     ,   8      ,obj%cmpy_init,obj%cmpy_last,    &
                                             obj%cmpy_inc , obj%cmpy_tot
 write(lunprint,2222)'X-SUPERCMP',   7      ,obj%superxmin,obj%superxmax,    &
                                             obj%superxinc, obj%NXSUPER  ,   &
                                             obj%comp_x
 write(lunprint,2222)'Y-SUPERCMP',   8      ,obj%superymin,obj%superymax,    &
                                             obj%superyinc, obj%NYSUPER  ,   &
                                             obj%comp_y
 write(lunprint,*)' '

2222  FORMAT (' CC3D: ',A12,1x,i3,1x,2F11.2,F9.2,I10,I8)

      end subroutine cc3d_print_summary1


!!------------------------ cc3d print summary2 ------------------------------!!
!!------------------------ cc3d print summary2 ------------------------------!!
!!------------------------ cc3d print summary2 ------------------------------!!

                      ! word = ENCOUNTERED or USED.


      subroutine cc3d_print_summary2 (obj,word,                        &
                                      increment_xsrc, increment_ysrc,  &
                                      increment_xrec, increment_yrec,  &
                                      increment_xcmp, increment_ycmp,  &
                                      increment_offset)

      type(cc3d_struct)     ,intent(in)    :: obj                ! arguments
      character(len=*)      ,intent(in)    :: word               ! arguments
      type(increment_struct),intent(inout) :: increment_xsrc     ! arguments
      type(increment_struct),intent(inout) :: increment_ysrc     ! arguments
      type(increment_struct),intent(inout) :: increment_xrec     ! arguments
      type(increment_struct),intent(inout) :: increment_yrec     ! arguments
      type(increment_struct),intent(inout) :: increment_xcmp     ! arguments
      type(increment_struct),intent(inout) :: increment_ycmp     ! arguments
      type(increment_struct),intent(inout) :: increment_offset   ! arguments
      real :: xsrc_min,xsrc_max,xsrc_inc,ysrc_min,ysrc_max,ysrc_inc  ! local
      real :: xrec_min,xrec_max,xrec_inc,yrec_min,yrec_max,yrec_inc  ! local
      real :: xcmp_min,xcmp_max,xcmp_inc,ycmp_min,ycmp_max,ycmp_inc  ! local
      real :: offset_min,offset_max                                  ! local

      call increment_result (increment_xsrc, xsrc_min, xsrc_max, xsrc_inc)
      call increment_result (increment_ysrc, ysrc_min, ysrc_max, ysrc_inc)
      call increment_result (increment_xrec, xrec_min, xrec_max, xrec_inc)
      call increment_result (increment_yrec, yrec_min, yrec_max, yrec_inc)
      call increment_result (increment_xcmp, xcmp_min, xcmp_max, xcmp_inc)
      call increment_result (increment_ycmp, ycmp_min, ycmp_max, ycmp_inc)
      call increment_result (increment_offset, offset_min, offset_max)

      if (xsrc_max == xsrc_min) xsrc_inc = obj%sx_inc
      if (ysrc_max == ysrc_min) ysrc_inc = obj%sy_inc
      if (xrec_max == xrec_min) xrec_inc = obj%rx_inc
      if (yrec_max == yrec_min) ycmp_inc = obj%ry_inc
      if (xcmp_max == xcmp_min) xcmp_inc = obj%cmpx_inc
      if (ycmp_max == ycmp_min) ysrc_inc = obj%cmpy_inc

 write(lunprint,*)' '
 write(lunprint,*)'CC3D: TRACES '//word//':'
 write(lunprint,*)'CC3D:              HWD     MINIMUM    MAXIMUM  INCREMENT'
 write(lunprint,3333)'X-SOURCE'  ,obj%hdr_sx,  xsrc_min ,  xsrc_max , xsrc_inc
 write(lunprint,3333)'Y-SOURCE'  ,obj%hdr_sy,  ysrc_min ,  ysrc_max , ysrc_inc
 write(lunprint,3333)'X-RECEIVER',obj%hdr_rx,  xrec_min ,  xrec_max , xrec_inc
 write(lunprint,3333)'Y-RECEIVER',obj%hdr_ry,  yrec_min ,  yrec_max , yrec_inc
 write(lunprint,3333)'X-CMP'     ,    7     ,  xcmp_min ,  xcmp_max , xcmp_inc
 write(lunprint,3333)'Y-CMP'     ,    8     ,  ycmp_min ,  ycmp_max , ycmp_inc
 write(lunprint,3333)'OFFSET'    ,    6     , offset_min, offset_max
 write(lunprint,*)' '

3333  FORMAT (' CC3D: ',A12,1x,i3,1x,2F11.2,F9.2)

      end subroutine cc3d_print_summary2


!!--------------------------- parallel range -----------------------------!!
!!--------------------------- parallel range -----------------------------!!
!!--------------------------- parallel range -----------------------------!!


      subroutine cc3d_parallel_range &
                         (word,worker,num_cpus,ngp,igp_start,igp_stop)

      character(len=*)   ,intent(in)  :: word                ! arguments
      integer            ,intent(in)  :: worker,num_cpus,ngp ! arguments
      integer            ,intent(out) :: igp_start,igp_stop  ! arguments
      real                            :: exact               ! local

      if (ngp == 1) then
           igp_start = 1
           igp_stop  = 1
      else if (num_cpus == 1) then
           igp_start = 1
           igp_stop  = ngp
      else
           exact     = float(ngp) / float(num_cpus)
           igp_start = nint(worker * exact) + 1
           igp_stop  = nint((worker + 1) * exact)
           if (worker == num_cpus - 1) igp_stop = ngp
      end if

      end subroutine cc3d_parallel_range


!!------------------------- cc3d form stacks -------------------------------!!
!!------------------------- cc3d form stacks -------------------------------!!
!!------------------------- cc3d form stacks -------------------------------!!

           ! Read each trace of this super-CMP into memory.
           !
           ! Apply shifts (derived from previous iterations) for
           ! source, receiver, CMP within the super-CMP (if TERMS=3 or
           ! TERMS=4), and offset (if TERMS=4).
           !
           ! Stack the shifted traces five ways:
           !     by super-CMP (all traces in super-CMP).
           !     by common sources (if source file was requested),
           !     by common receivers (if receiver file was requested),
           !     by common CMPs (if TERMS=1 or TERMS=3 or TERMS=4),
           !     by common offset bins defined by OFF_WID (if TERMS=4),


      subroutine cc3d_form_stacks                 &
                  (obj,                           & ! changed
                   iorrr,iosss,iohhh,ioccc,       & ! in
                   isuper,                        & ! in
                   stat,                          & ! in        (nvec)
                   ntraces,                       & ! updated   (nvec2)
                   max_exists,                    & ! updated
                   sum_exists,                    & ! updated
                   SUPER_STACK,                   & ! out       (nwin)
                   stacked_cmps,                  & ! out       (nwin,ncomp)
                   stacked_offsets,               & ! out       (nwin,noff)
                   stacked_ground_positions,      & ! out       (nwin16,nvec2)
                   exists_in_super,               & ! out       (nvec2)
                   exists_in_cmp,                 & ! out       (ncomp1,nvec2)
                   error)                           ! out   

!  ARGUMENTS

      type(cc3d_struct),intent(inout) :: obj
      integer          ,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      integer          ,intent(in)    :: isuper
      real             ,intent(in)    :: stat                     (:)         
      integer          ,intent(inout) :: NTRACES                  (:)
      integer          ,intent(inout) :: max_exists
      real             ,intent(inout) :: SUM_exists
      real             ,intent(out)   :: SUPER_STACK              (:)
      real             ,intent(out)   :: stacked_cmps             (:,:)
      real             ,intent(out)   :: stacked_offsets          (:,:) 
      integer          ,intent(out)   :: stacked_ground_positions (:,:)
      logical          ,intent(out)   :: EXISTS_IN_SUPER          (:) 
      integer          ,intent(out)   :: exists_in_cmp            (:,:)
      logical          ,intent(out)   :: error

!  LOCAL VARIABLES

      integer :: ifold,ivec,NUM_exists

!  CLEAR OUTPUT ARRAYS

      SUPER_STACK              = 0.0      ! clear all nwin    
      stacked_cmps             = 0.0      ! clear all nwin * NCOMP
      stacked_offsets          = 0.0      ! clear all nwin * noff
      stacked_ground_positions = 0        ! clear all nwin16 * NVEC2
      exists_in_super          = .false.  ! clear all NVEC2 
      exists_in_cmp            = 0        ! clear all ncomp1 * NVEC2

!  LOOP OVER FOLD

      do IFOLD = 1,obj%ifold(ISUPER)

           call cc3d_process_single_trace         &
                  (obj,                           & ! changed
                   iorrr,iosss,iohhh,ioccc,       & ! in   
                   isuper,ifold,                  & ! in   
                   stat,                          & ! in        (nvec)
                   ntraces,                       & ! updated   (nvec2)
                   SUPER_STACK,                   & ! updated   (nwin)
                   stacked_cmps,                  & ! updated   (nwin,ncomp)
                   stacked_offsets,               & ! updated   (nwin,noff)
                   stacked_ground_positions,      & ! updated   (nwin16,nvec2)
                   exists_in_super,               & ! updated   (nvec2)
                   exists_in_cmp,                 & ! updated   (ncomp1,nvec2)
                   error)                           ! out   

            if (error) return

      end do

!  UPDATE MAX_EXISTS AND SUM_EXISTS

      NUM_exists = 0
      do IVEC = 1,obj%NVEC2
           if (exists_in_super(IVEC)) NUM_exists = NUM_exists + 1
      end do

      max_exists = max(max_exists,NUM_exists)
      SUM_exists = SUM_exists + NUM_exists
      error      = .false.

      end subroutine cc3d_form_stacks


!!--------------------- cc3d form correlations -----------------------------!!
!!--------------------- cc3d form correlations -----------------------------!!
!!--------------------- cc3d form correlations -----------------------------!!

         ! Successively remove each of the "partial stack" traces
         ! from the stack of the super-CMP, and correlate
         ! the partial stack with this "full stack".  For TERMS=1,
         ! use only the portion of the "full stack" with CMPs which
         ! match the CMPs present in the partial stack for that
         ! ground position.  The "partial stack" traces are common
         ! source, common receiver, common CMP (if TERMS=3 or TERMS=4),
         ! and common offset (if TERMS=4).
         !
         ! Pick the correlation to get an "IMS" static estimate
         ! for the corresponding term (source, receiver, CMP, or
         ! offset), and sum the shift into an average for that term.
         !
         ! Stack the correlation with others from the same term.
         !
         ! Optionally form least-squares estimates of X and Y CMP dip
         ! (if TERMS=3 or TERMS=4) and residual moveout coefficient
         ! (if TERMS=4) for this super-CMP.


      subroutine cc3d_form_correlations           &
                  (obj,iorrr,iosss,iohhh,ioccc,   & ! in   
                   isuper,                        & ! in   
                   tapr,                          & ! in       (ncorr)
                   SUPER_STACK,                   & ! in       (nwin)
                   stacked_cmps,                  & ! in       (nwin,ncomp)
                   stacked_offsets,               & ! in       (nwin,noff)
                   stacked_ground_positions,      & ! in       (nwin16,nvec2)
                   exists_in_super,               & ! in       (nvec2)
                   exists_in_cmp,                 & ! in       (ncomp1,nvec2)
                   rstat,                         & ! updated  (nvec)
                   wghts,                         & ! updated  (nvec2)
                   nccoef,                        & ! updated  (nvec2)
                   stacked_correlations,          & ! updated  (ncorr16,nvec2)
                   pwr_super,pwr_comp)              ! updated

      type(cc3d_struct),intent(in)    :: obj
      integer          ,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      integer          ,intent(in)    :: isuper
      real             ,intent(in)    :: tapr                     (:)         
      real             ,intent(in)    :: SUPER_STACK              (:)           
      real             ,intent(in)    :: stacked_cmps             (:,:)
      real             ,intent(in)    :: stacked_offsets          (:,:) 
      integer          ,intent(in)    :: stacked_ground_positions (:,:)
      logical          ,intent(in)    :: EXISTS_IN_SUPER          (:)          
      integer          ,intent(in)    :: exists_in_cmp            (:,:)
      real             ,intent(inout) :: rstat                    (:)
      real             ,intent(inout) :: wghts                    (:)
      integer          ,intent(inout) :: nccoef                   (:)
      integer          ,intent(inout) :: stacked_correlations     (:,:) 
      real             ,intent(inout) :: pwr_super,pwr_comp

!  LOCAL VARIABLES

      integer :: ivec,iveca,ivecb

!  ADD TO STACK POWER OF SUPER-CMPS

      call mth_power (SUPER_STACK,obj%nwin,   PWR_SUPER)

!  LOOP OVER RECEIVERS AND FORM CORRELATIONS WITH THE SUPER-CMP

      if (obj%use_rec) then
      call cc3d_form_static_correlations                      &
                 (obj,                                        & ! in
                  obj%NREC,                                   & ! in
                  TAPR,                                       & ! in
                  SUPER_STACK,                                & ! in
                  stacked_cmps,                               & ! in (fish)
                  stacked_ground_positions (:,IORRR+1:),      & ! in
                  exists_in_super            (IORRR+1:),      & ! in
                  exists_in_cmp            (:,IORRR+1:),      & ! in (fish)
                  rstat                      (IORRR+1:),      & ! add to
                  WGHTS                      (IORRR+1:),      & ! add to
                  NCCOEF                     (IORRR+1:),      & ! add to
                  stacked_correlations     (:,IORRR+1:),      & ! add to
                  pwr_comp)                                     ! add to
      end if

!  LOOP OVER SOURCES AND FORM CORRELATIONS WITH THE SUPER-CMP

      if (obj%use_src) then
      call cc3d_form_static_correlations                      &
                 (obj,                                        & ! in
                  obj%NSRC,                                   & ! in
                  TAPR,                                       & ! in
                  SUPER_STACK,                                & ! in
                  stacked_cmps,                               & ! in (fish)
                  stacked_ground_positions (:,IOSSS+1:),      & ! in
                  exists_in_super            (IOSSS+1:),      & ! in
                  exists_in_cmp            (:,IOSSS+1:),      & ! in (fish)
                  rstat                      (IOSSS+1:),      & ! add to
                  WGHTS                      (IOSSS+1:),      & ! add to
                  NCCOEF                     (IOSSS+1:),      & ! add to
                  stacked_correlations     (:,IOSSS+1:),      & ! add to
                  pwr_comp)                                     ! add to
      end if

!  LOOP OVER OFFSETS AND FORM CORRELATIONS WITH THE SUPER-CMP
!  ALSO FORM LEAST SQUARES ESTIMATES OF THE RESIDUAL NMO

      if (obj%use_off) then
           IVEC = IOHHH + ISUPER
           call cc3d_form_offset_correlations                 &
                      (obj,                                   & ! in
                       TAPR,                                  & ! in
                       SUPER_STACK,                           & ! in
                       stacked_offsets,                       & ! in
                       rstat(IVEC))                             ! out
      end if

!  LOOP OVER CMPS AND FORM CORRELATIONS WITH THE SUPER-CMP
!  ALSO FORM LEAST SQUARES ESTIMATES OF THE RESIDUAL CMP X AND Y DIPS

      if (obj%use_cmp) then
           IVECA = IOCCC + 2*ISUPER - 1
           IVECB = IOCCC + 2*ISUPER
           call cc3d_form_cmp_correlations                    &
                      (obj,                                   & ! in
                       TAPR,                                  & ! in
                       SUPER_STACK,                           & ! in
                       stacked_cmps,                          & ! in
                       rstat(IVECA),rstat(IVECB))               ! out
      end if

      end subroutine cc3d_form_correlations


!!----------------------- cc3d perform iteration --------------------------!!
!!----------------------- cc3d perform iteration --------------------------!!
!!----------------------- cc3d perform iteration --------------------------!!


      subroutine cc3d_perform_iteration                  &
                  (obj,                                  & ! changed
                   iorrr,iosss,iohhh,ioccc,              & ! in
                   iter,                                 & ! in
                   tapr,                                 & ! in       (ncorr)
                   stat,                                 & ! updated  (nvec)
                   pwr_super_first,pwr_comp_first,       & ! out
                   timer1,timer2,timer3,timer4,timer5,   & ! in
                   error)                                  ! out

!  ARGUMENTS

      type(cc3d_struct),intent(inout) :: obj

      integer,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      integer,intent(in)    :: iter
      real   ,intent(in)    :: tapr   (:)       ! taper array
      real   ,intent(inout) :: stat   (:)       ! statics values
      real   ,intent(inout) :: pwr_super_first,pwr_comp_first
      integer,intent(in)    :: timer1,timer2,timer3,timer4,timer5
      logical,intent(out)   :: error

!  LOCAL VARIABLES

      integer :: ISUPER
      integer :: max_exists
      real    :: SUM_exists
      real    :: PWR_SUPER,PWR_COMP

!  AUTOMATIC ARRAYS

      real    :: rstat   (obj%NVEC)           ! residual statics values
      real    :: wghts   (obj%NVEC2)          ! weights for rstat
      integer :: NTRACES (obj%NVEC2)          ! number of traces used
      integer :: NCCOEF  (obj%NVEC2)          ! number of corr coefs stacked

      integer :: stacked_correlations (obj%ncorr16,obj%NVEC2)

!  INITIALIZE VARIABLES

      rstat                = 0.0  ! clear all NVEC       ! residual statics
      WGHTS                = 0.0  ! clear all NVEC2      ! residual weights
      NTRACES              = 0    ! clear all NVEC2      ! number of traces
      NCCOEF               = 0    ! clear all NVEC2      ! number of corr coefs
      stacked_correlations = 0    ! clear all ncorr16*NVEC2

      max_exists           = 0
      SUM_exists           = 0.0
      PWR_SUPER            = 0.0         ! power in all super-stacks
      PWR_COMP             = 0.0         ! power in all composited traces

!  LOOP OVER SUPER-CMPS

      do ISUPER = obj%isuper_start,obj%isuper_stop

           call cc3d_process_super_cmp            &
                  (obj,                           & ! changed
                   iorrr,iosss,iohhh,ioccc,       & ! in   
                   isuper,                        & ! in   
                   tapr,                          & ! in       (ncorr)
                   stat,                          & ! in       (nvec)
                   rstat,                         & ! updated  (nvec)
                   wghts,                         & ! updated  (nvec2)
                   ntraces,                       & ! updated  (nvec2)
                   nccoef,                        & ! updated  (nvec2)
                   stacked_correlations,          & ! updated  (ncorr16,nvec2)
                   max_exists,                    & ! updated
                   sum_exists,                    & ! updated
                   pwr_super,pwr_comp,            & ! updated
                   timer1,timer2,                 & ! in   
                   error)                           ! out   

           if (error) then
                call pc_error ('CC3D: fatal error in cc3d_process_super_cmp')
                call pc_print ('CC3D: ISUPER =',isuper,'out of',obj%nsuper)
                return
           end if

      end do

!  MERGE RESULTS

      call timer_start (timer3)

      call cc3d_merge_results                   &
                (obj,                           & ! in   
                 rstat,                         & ! updated  (nvec)
                 wghts,                         & ! updated  (nvec2)
                 ntraces,                       & ! updated  (nvec2)
                 nccoef,                        & ! updated  (nvec2)
                 stacked_correlations,          & ! updated  (ncorr16,nvec2)
                 max_exists,                    & ! updated
                 sum_exists,                    & ! updated
                 pwr_super,pwr_comp,            & ! updated
                 error)                           ! out   

      call timer_stop (timer3)

      if (error) then
           call pc_error ('CC3D: fatal error in cc3d_merge_results')
           return
      end if

!  PROCESS RESULTS (boss only)

      call cc3d_process_results              &
           (obj,iorrr,iosss,iohhh,ioccc,     & ! in   
            iter,                            & ! in   
            stacked_correlations,            & ! in       (ncorr16,nvec2)
            stat,                            & ! changed  (nvec)
            rstat,                           & ! changed  (nvec)
            wghts,                           & ! in       (nvec2)
            nccoef,                          & ! in       (nvec2)
            ntraces,                         & ! in       (nvec2)
            max_exists,                      & ! in     
            sum_exists,                      & ! in     
            pwr_super_first,pwr_comp_first,  & ! in   
            pwr_super,pwr_comp,              & ! in   
            timer4,timer5,                   & ! in   
            error)                             ! out   

      if (error) then
           call pc_error ('CC3D: fatal error in cc3d_process_results')
           return
      end if

!  BROADCAST STATICS (from boss to workers)

      call pcpsx_broadcast (0, obj%nvec, stat)

      end subroutine cc3d_perform_iteration


!!----------------------- cc3d print statics --------------------------!!
!!----------------------- cc3d print statics --------------------------!!
!!----------------------- cc3d print statics --------------------------!!

            ! Optionally print the final static solution.

                            ! boss only.

      subroutine cc3d_print_statics                 &
                        (obj,                       & ! in   
                         iorrr,iosss,iohhh,ioccc,   & ! in   
                         stat,                      & ! in   (nvec) 
                         rstat,                     & ! in   (nvec) 
                         ccoef,                     & ! in   (nvec2)
                         nccoef,                    & ! in   (nvec2)
                         ntraces)                     ! in   (nvec2)

!  ARGUMENTS

      type(cc3d_struct),intent(in) :: obj

      integer,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      real   ,intent(in)    :: stat   (:)         ! statics values
      real   ,intent(in)    :: rstat  (:)         ! residual statics values
      real   ,intent(in)    :: ccoef  (:)         ! correlation coefficients
      integer,intent(in)    :: NCCOEF (:)         ! number of corr coefs
      integer,intent(in)    :: NTRACES(:)         ! number of traces

!  LOCAL VARIABLES

      integer :: IX,IY,KOUNT,IGP,IVEC,IVECA,IVECB
      real    :: X,Y

!  RETURN IF THERE IS NOTHING TO DO

      if (obj%opt_print /= "MAX") return

!  PRINT SOURCE GROUND POSITION STATICS

      write(lunprint,*)' '
      write(lunprint,*)'***************************************************'
      write(lunprint,*)'CC3D:    SOURCE GROUND POSITION STATICS (ms)       '
      write(lunprint,*)'***************************************************'

      if (obj%use_src) then
         kount = 0
         do IGP = 1,obj%nsrc
             kount = kount + 1
             if (MOD(kount,50) == 1) then
               write(lunprint,*)'            SXGP       SYGP      STATIC     ',&
                                           'INC     CC  #CC  #TR'
             end if
             call mth_split_index (IGP,obj%sx_tot,   IX,IY)
             X = obj%sx_init + (IX-1)*obj%sx_inc
             Y = obj%sy_init + (IY-1)*obj%sy_inc
             IVEC = IOSSS + IGP
             if (stat(IVEC) == FNIL) then
                  write(lunprint, '(1X,I5,2F11.1,4X,A8,F8.1,F7.2,2I5)')   &
                             IGP,X,Y,'     nil'
             else
                  write(lunprint, '(1X,I5,2F11.1,4X,2F8.1,F7.2,2I5)')     &
                             IGP,X,Y,stat(IVEC)*obj%dtms,                 &
                                    rstat(IVEC)*obj%dtms,ccoef(IVEC),     &
                                   NCCOEF(IVEC),NTRACES(IVEC)
             end if
         end do
      else
         write(lunprint,*)'                  NOT CALCULATED'
      end if

!  PRINT RECEIVER GROUND POSITION STATICS

      write(lunprint,*)' '
      write(lunprint,*)'***************************************************'
      write(lunprint,*)'CC3D:    RECEIVER GROUND POSITION STATICS (ms)     '
      write(lunprint,*)'***************************************************'

      if (obj%use_rec) then
         kount = 0
         do IGP = 1,obj%nrec
             kount = kount + 1
             if (MOD(kount,50) == 1) then
               write(lunprint,*)'            RXGP       RYGP      STATIC     ',&
                                              'INC     CC  #CC  #TR'
             end if
             call mth_split_index (IGP,obj%rx_tot,   IX,IY)
             X = obj%rx_init + (IX-1)*obj%rx_inc
             Y = obj%ry_init + (IY-1)*obj%ry_inc
             IVEC = IORRR + IGP
             if (stat(IVEC) == FNIL) then
                  write(lunprint, '(1X,I5,2F11.1,4X,A8,F8.1,F7.2,2I5)')   &
                             IGP,X,Y,'     nil'
             else
                  write(lunprint, '(1X,I5,2F11.1,4X,2F8.1,F7.2,2I5)')     &
                             IGP,X,Y,stat(IVEC)*obj%dtms,                 &
                                    rstat(IVEC)*obj%dtms,ccoef(IVEC),     &
                                   NCCOEF(IVEC),NTRACES(IVEC)
             end if
         end do
      else
         write(lunprint,*)'                  NOT CALCULATED'
      end if

!  PRINT CMP STATIC INCREMENTS

      write(lunprint,*)' '
      write(lunprint,*)'***************************************************'
      write(lunprint,*)'CC3D:          CMP STATIC INCREMENTS               '
      write(lunprint,*)'***************************************************'

      if (obj%use_cmp) then
         kount = 0
         do IGP = 1,obj%NSUPER
          kount = kount + 1
          if (MOD(kount,50) == 1) then
            write(lunprint,*)'SUPER-CMP   #TR     MID XBIN     MID YBIN     ', &
                        'X-INCREMENT   X-RESID    Y-INCREMENT   Y-RESID'
          end if
          call mth_split_index (IGP,obj%NXSUPER,   IX,IY)
          X = obj%superxmin + (IX-1)*obj%superxinc
          Y = obj%superymin + (IY-1)*obj%superyinc
          IVECA = IOCCC + 2*IGP - 1
          IVECB = IOCCC + 2*IGP
          if (stat(IVECA) == FNIL .and.                                       &
              stat(IVECB) == FNIL) then
               write(lunprint, '(2X,I5,I9,2F13.1,3X,A10,10X,5X,A10,F10.2)')   &
                             IGP,obj%ifold(IGP),X,Y,'       nil','       nil'
          else
               write(lunprint, '(2X,I5,I9,2F13.1,3X,2F10.2,5X,2F10.2)')       &
                             IGP,obj%ifold(IGP),X,Y,                          &
                             stat(IVECA)*obj%dtms,rstat(IVECA)*obj%dtms,      &
                             stat(IVECB)*obj%dtms,rstat(IVECB)*obj%dtms
          end if
         end do
      else
         write(lunprint,*)'                  NOT CALCULATED'
      end if

!  PRINT OFFSET STATIC COEFFICIENT

      write(lunprint,*)' '
      write(lunprint,*)'***************************************************'
      write(lunprint,*)'CC3D:        OFFSET STATICS COEFFICIENT            '
      write(lunprint,*)'***************************************************'

      if (obj%use_off) then
         kount = 0
         do IGP = 1,obj%NSUPER
          kount = kount + 1
          if (MOD(kount,50) == 1) then
            write(lunprint,*)'SUPER-CMP   #TR     MID XBIN     MID YBIN     ', &
                                                 '  COEF       INC'
          end if
          call mth_split_index (IGP,obj%NXSUPER,   IX,IY)
          X = obj%superxmin + (IX-1)*obj%superxinc
          Y = obj%superymin + (IY-1)*obj%superyinc
          IVEC = IOHHH + IGP
          if (stat(IVEC) == FNIL) then
               write(lunprint, '(2X,I5,I9,2F13.1,3X,A9,F9.3)')      &
                          IGP,obj%ifold(IGP),X,Y,'      nil'
          else
               write(lunprint, '(2X,I5,I9,2F13.1,3X,2F9.3)')        &
                          IGP,obj%ifold(IGP),X,Y,stat(IVEC),        &
                                                rstat(IVEC)
          end if
         end do
      else
         write(lunprint,*)'                  NOT CALCULATED'
      end if

      end subroutine cc3d_print_statics


!!----------------------- cc3d print summaries -------------------------!!
!!----------------------- cc3d print summaries -------------------------!!
!!----------------------- cc3d print summaries -------------------------!!


      subroutine cc3d_print_summaries (obj)

      type(cc3d_struct),intent(inout) :: obj                    ! arguments
      integer                         :: ISUPER,minTR,maxTR     ! local
      integer                         :: nsuper_relevant        ! local
      integer                         :: ntraces_relevant       ! local
      integer                         :: nsuper_total           ! local
      integer                         :: ntraces_total          ! local

!  GATHER STATISTICS FOR PRINTING SUMMARIES

      minTR = obj%ifold(1)
      maxTR = obj%ifold(1)
      do ISUPER = 2,obj%NSUPER
           minTR = min(obj%ifold(ISUPER),minTR)
           maxTR = max(obj%ifold(ISUPER),maxTR)
      end do

      nsuper_total     = 0
      ntraces_total    = 0
      do ISUPER = 1,obj%NSUPER
           if (obj%ifold(ISUPER) == 0) cycle
           nsuper_total  = nsuper_total  + 1
           ntraces_total = ntraces_total + obj%ifold(isuper)
      end do

      nsuper_relevant  = 0
      ntraces_relevant = 0
      do ISUPER = obj%isuper_start,obj%isuper_stop
           if (obj%ifold(ISUPER) == 0) cycle
           nsuper_relevant  = nsuper_relevant  + 1
           ntraces_relevant = ntraces_relevant + obj%ifold(isuper)
      end do

!  PRINT GENERAL SUMMARIES

 write(lunprint,*)' '
 write(lunprint,*)'********************************************************'
 write(lunprint,*)'              CC3D PROCESSING SUMMARY                   ', &
         '         worker ',pcps_current_worker_num,' of ',obj%nworkers
 write(lunprint,*)'********************************************************'
 write(lunprint,*)'  TOTAL TRACES ENCOUNTERED =    ',obj%ICOUNT
 write(lunprint,*)'  TOTAL TRACES USED        =    ',obj%ICOUNT - obj%IDROP
 write(lunprint,*)'  TOTAL TRACES DROPPED     =    ',obj%IDROP
 write(lunprint,*)'********************************************************'
 write(lunprint,*)'  TRACES WITH INSUFFICIENT DATA WINDOW =   ',obj%IDROPD
 write(lunprint,*)'  TRACES WITH  SOURCE  G.P. OUT OF RANGE = ',obj%IDROPS
 write(lunprint,*)'  TRACES WITH RECEIVER G.P. OUT OF RANGE = ',obj%IDROPR
 write(lunprint,*)'  TRACES WITH   CMP GRID    OUT OF RANGE = ',obj%IDROPC
 write(lunprint,*)'  TRACES DROPPED DUE TO EXCESS FOLD =      ',obj%IDROPF
 write(lunprint,*)'********************************************************'
 write(lunprint,*)'  number of binned offsets = ',obj%noff,                &
                          ' (using OFF_WID = ',obj%off_wid,')'
 write(lunprint,*)'  minimum number of traces in super-CMP = ',minTR
 write(lunprint,*)'  maximum number of traces in super-CMP = ',maxTR
 write(lunprint,*)'  (disk space reserved for ',obj%maxfolds,              &
                          ' traces in super-CMP)'
 write(lunprint,*)'********************************************************'
 write(lunprint,*)'  TOTAL NUMBER OF SUPER-CMPS          = ',nsuper_total
 write(lunprint,*)'  TOTAL NUMBER OF RELEVANT SUPER-CMPS = ',nsuper_relevant
 write(lunprint,*)'  TOTAL TRACES IN ALL SUPER-CMPS      = ',ntraces_total
 write(lunprint,*)'  TOTAL TRACES IN RELEVANT SUPER-CMPS = ',ntraces_relevant
 write(lunprint,*)'  (the "relevant" values are for worker ', &
                 pcps_current_worker_num,' of ',obj%nworkers,')'
 write(lunprint,*)'********************************************************'
 write(lunprint,*)' '

!  PRINT SPECIFIC SUMMARIES

      call cc3d_print_summary1 (obj, 'REQUESTED')                ! in

      call cc3d_print_summary2 (obj, 'ENCOUNTERED',            & ! changed
                    obj%increment_xsrco, obj%increment_ysrco,  & ! changed
                    obj%increment_xreco, obj%increment_yreco,  & ! changed
                    obj%increment_xcmpo, obj%increment_ycmpo,  & ! changed
                    obj%increment_offseto)                       ! changed

      call cc3d_print_summary2 (obj, 'USED',                   & ! changed
                    obj%increment_xsrc, obj%increment_ysrc,    & ! changed
                    obj%increment_xrec, obj%increment_yrec,    & ! changed
                    obj%increment_xcmp, obj%increment_ycmp,    & ! changed
                    obj%increment_offset)                        ! changed

      end subroutine cc3d_print_summaries


!!--------------------- cc3d report timing -------------------------!!
!!--------------------- cc3d report timing -------------------------!!
!!--------------------- cc3d report timing -------------------------!!


      subroutine cc3d_report_timing (obj,timer,title)

      type(cc3d_struct),intent(in) :: obj                    ! arguments
      integer                      :: timer                  ! arguments
      character(len=*) ,intent(in) :: title                  ! arguments

      call pc_print     (' ')
      call pc_print     ('***************************************************')
      call pc_print     ('CC3D',title,':')
      call pc_print     ('Worker',pcps_current_worker_num,'of',obj%nworkers)
      call timer_report (timer, 'CC3D',lunprint)
      call timer_clear  (timer)
      call pc_print     ('***************************************************')
      call pc_print     (' ')

      end subroutine cc3d_report_timing


!!------------------------- shift trace window -----------------------------!!
!!------------------------- shift trace window -----------------------------!!
!!------------------------- shift trace window -----------------------------!!


      subroutine cc3d_shift_trace_window (obj,iosss,iorrr,ioccc,iohhh,   &
                                          delcmpx,delcmpy,               &
                                          isrc,irec,isuper,ioff,         &
                                          stat,trace)

      type(cc3d_struct),intent(in)    :: obj                      ! arguments
      integer          ,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC  ! arguments
      real             ,intent(in)    :: delcmpx,delcmpy          ! arguments
      integer          ,intent(in)    :: isrc,irec,isuper,ioff    ! arguments
      real             ,intent(in)    :: stat   (:)               ! arguments
      real             ,intent(inout) :: trace  (:)               ! arguments
      real                            :: scratch(obj%nwin)        ! local
      real                            :: static                   ! local

      static = 0.0
      if (obj%use_src) then
            static = static + stat(IOSSS+ISRC)
      end if
      if (obj%use_rec) then
            static = static + stat(IORRR+IREC)
      end if
      if (obj%use_cmp) then
            static = static + stat(IOCCC+ISUPER*2-1)*delcmpx +    &
                              stat(IOCCC+ISUPER*2  )*delcmpy
      end if
      if (obj%use_off) then
            static = static + stat(IOHHH+ISUPER)*(ioff**2)
      end if

      scratch(:) = trace(:)
      call statcc (static,obj%nwin,scratch,  trace)

      end subroutine cc3d_shift_trace_window


!!-------------------------- write trace window ---------------------------!!
!!-------------------------- write trace window ---------------------------!!
!!-------------------------- write trace window ---------------------------!!


      subroutine cc3d_write_trace_window (obj,isuper,ifold,     &
                                          isrc,irec,ioff,icmp,  &
                                          trace,error)

      type(cc3d_struct),intent(inout) :: obj                     ! arguments
      integer          ,intent(in)    :: isuper,ifold            ! arguments
      integer          ,intent(in)    :: isrc,irec,ioff,icmp     ! arguments
      real             ,intent(in)    :: trace  (:)              ! arguments
      logical          ,intent(out)   :: error                   ! arguments
      integer                         :: hdsave (NHEAD)          ! local
      integer                         :: trsave (obj%nwin8)      ! local
      integer                         :: irecord,err             ! local

      HDsave(1) = ISRC                 ! source index
      HDsave(2) = IREC                 ! receiver index
      HDsave(3) = ISUPER               ! super-cmp index
      HDsave(4) = ioff                 ! offset index
      HDsave(5) = icmp                 ! cmp index

      IRECORD = obj%maxfolds*(ISUPER-obj%isuper_start) + IFOLD
                                                 ! trace record number

      if (IRECORD < 1) then
             call pc_error ('CC3D: ZERO OR NEGATIVE RECORD NUMBER',IRECORD)
             call pc_print ('CC3D: worker number',pcps_current_worker_num)
             call pc_print ('CC3D: FOLD                  =',ifold)
             call pc_print ('CC3D: trace source index    =',isrc)
             call pc_print ('CC3D: trace receiver index  =',irec)
             call pc_print ('CC3D: trace super-cmp index =',isuper)
             call pc_print ('CC3D: trace offset index    =',ioff)
             call pc_print ('CC3D: trace cmp index       =',icmp)
             error = .true.
             return
      end if
      if (IRECORD > obj%maxONDSK) then
             call pc_error ('CC3D: RECORD NUMBER',IRECORD,'EXCEEDS MAXONDSK')
             call pc_print ('CC3D: worker number',pcps_current_worker_num)
             call pc_print ('CC3D: FOLD                  =',ifold)
             call pc_print ('CC3D: trace source index    =',isrc)
             call pc_print ('CC3D: trace receiver index  =',irec)
             call pc_print ('CC3D: trace super-cmp index =',isuper)
             call pc_print ('CC3D: trace offset index    =',ioff)
             call pc_print ('CC3D: trace cmp index       =',icmp)
             error = .true.
             return
      end if

      call pkutil_pack8    (trace,obj%nwin,   trsave,obj%nwin8)
      call temptfile_write (obj%temptfile, IRECORD, HDsave, trsave, err)
      error = (err /= TEMPTFILE_OK)

      end subroutine cc3d_write_trace_window


!!-------------------------- read trace window ---------------------------!!
!!-------------------------- read trace window ---------------------------!!
!!-------------------------- read trace window ---------------------------!!


      subroutine cc3d_read_trace_window (obj,isuper,ifold,     &
                                         isrc,irec,ioff,icmp,  &
                                         trace,error)

      type(cc3d_struct),intent(inout) :: obj                     ! arguments
      integer          ,intent(in)    :: isuper,ifold            ! arguments
      integer          ,intent(out)   :: isrc,irec,ioff,icmp     ! arguments
      real             ,intent(out)   :: trace  (:)              ! arguments
      logical          ,intent(out)   :: error                   ! arguments
      integer                         :: hdsave (NHEAD)          ! local
      integer                         :: trsave (obj%nwin8)      ! local
      integer                         :: irecord,isuper2,err     ! local

      IRECORD = obj%maxfolds*(ISUPER-obj%isuper_start) + IFOLD
                                                        ! trace record number

      call temptfile_read (obj%temptfile, IRECORD, HDsave, TRsave, err)
      error = (err /= TEMPTFILE_OK)
      if (error) return

      ISRC    = HDsave(1)            ! source index
      IREC    = HDsave(2)            ! receiver index
      ISUPER2 = HDsave(3)            ! super-cmp index
      ioff    = HDsave(4)            ! offset index
      icmp    = HDsave(5)            ! cmp index

      if (ISUPER2 /= ISUPER) then
           call pc_error ('CC3D: BAD SUPER-CMP INDEX')
           call pc_print ('CC3D: worker number',pcps_current_worker_num)
           call pc_print ('CC3D: desired trace super-cmp index =',isuper)
           call pc_print ('CC3D: IFOLD  =',ifold ,'out of',obj%ifold(isuper))
           call pc_print ('CC3D: record number         =',irecord)
           call pc_print ('CC3D: trace source index    =',isrc)
           call pc_print ('CC3D: trace receiver index  =',irec)
           call pc_print ('CC3D: trace super-cmp index =',isuper2)
           call pc_print ('CC3D: trace offset index    =',ioff)
           call pc_print ('CC3D: trace cmp index       =',icmp)
           error = .true.
           return
      end if

      call pkutil_unpack8 (TRsave,obj%nwin8,  trace,obj%nwin)

      end subroutine cc3d_read_trace_window


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      subroutine cc3d_read_parameters (obj)

      type(cc3d_struct),intent(inout) :: obj                  ! arguments

      real                :: sx_last_keep,sy_last_keep        ! local
      real                :: rx_last_keep,ry_last_keep        ! local
      real                :: cmpx_last_keep,cmpy_last_keep    ! local
      integer             :: sx_tot_keep,sy_tot_keep          ! local
      integer             :: rx_tot_keep,ry_tot_keep          ! local
      integer             :: cmpx_tot_keep,cmpy_tot_keep      ! local

      if (pathchoose_update(obj%dialog_src, obj%path_src)) return
      if (pathchoose_update(obj%dialog_rec, obj%path_rec)) return

      obj%ipn = pc_get_ipn()

      call pc_get_jdata  ('num_cpus', obj%num_cpus)

      call pc_get_global ('nwih'  , obj%nwih ) 
      call pc_get_global ('ndpt'  , obj%ndpt ) 
      call pc_get_global ('dt'    , obj%dt   )   

      sx_last_keep   = obj%sx_last
      sy_last_keep   = obj%sy_last
      rx_last_keep   = obj%rx_last
      ry_last_keep   = obj%ry_last
      cmpx_last_keep = obj%cmpx_last
      cmpy_last_keep = obj%cmpy_last

      sx_tot_keep    = obj%sx_tot
      sy_tot_keep    = obj%sy_tot
      rx_tot_keep    = obj%rx_tot
      ry_tot_keep    = obj%ry_tot
      cmpx_tot_keep  = obj%cmpx_tot
      cmpy_tot_keep  = obj%cmpy_tot

      call pc_get  ('max_static'  ,obj%max_static    )
      call pc_get  ('fold_max'    ,obj%fold_max      )
      call pc_get  ('num_iter'    ,obj%num_iter      )
      call pc_get  ('terms'       ,obj%terms         )
      call pc_get  ('comp_x'      ,obj%comp_x        )
      call pc_get  ('comp_y'      ,obj%comp_y        )
      call pc_get  ('opt_print'   ,obj%opt_print     )
      call pc_get  ('no_dead'     ,obj%no_dead       )
      call pc_get  ('dead_end'    ,obj%dead_end      )
      call pc_get  ('weight'      ,obj%weight        )
      call pc_get  ('off_wid'     ,obj%off_wid       )
      call pc_get  ('pwr_cc'      ,obj%pwr_cc        )  ! not in GUI
      call pc_get  ('taper'       ,obj%taper         )  ! not in GUI
      call pc_get  ('update'      ,obj%update        )  ! not in GUI
      call pc_get  ('sx_init'     ,obj%sx_init       )
      call pc_get  ('sx_inc'      ,obj%sx_inc        )
      call pc_get  ('sx_last'     ,obj%sx_last       )
      call pc_get  ('sx_tot'      ,obj%sx_tot        )
      call pc_get  ('rx_init'     ,obj%rx_init       )
      call pc_get  ('rx_inc'      ,obj%rx_inc        )
      call pc_get  ('rx_last'     ,obj%rx_last       )
      call pc_get  ('rx_tot'      ,obj%rx_tot        )
      call pc_get  ('cmpx_init'   ,obj%cmpx_init     )
      call pc_get  ('cmpx_inc'    ,obj%cmpx_inc      )
      call pc_get  ('cmpx_last'   ,obj%cmpx_last     )
      call pc_get  ('cmpx_tot'    ,obj%cmpx_tot      )
      call pc_get  ('sy_init'     ,obj%sy_init       )
      call pc_get  ('sy_inc'      ,obj%sy_inc        )
      call pc_get  ('sy_last'     ,obj%sy_last       )
      call pc_get  ('sy_tot'      ,obj%sy_tot        )
      call pc_get  ('ry_init'     ,obj%ry_init       )
      call pc_get  ('ry_inc'      ,obj%ry_inc        )
      call pc_get  ('ry_last'     ,obj%ry_last       )
      call pc_get  ('ry_tot'      ,obj%ry_tot        )
      call pc_get  ('cmpy_init'   ,obj%cmpy_init     )
      call pc_get  ('cmpy_inc'    ,obj%cmpy_inc      )
      call pc_get  ('cmpy_last'   ,obj%cmpy_last     )
      call pc_get  ('cmpy_tot'    ,obj%cmpy_tot      )
      call pc_get  ('path_src'    ,obj%path_src      )
      call pc_get  ('path_rec'    ,obj%path_rec      )
      call pc_get  ('hdr_src'     ,obj%hdr_src       )
      call pc_get  ('hdr_rec'     ,obj%hdr_rec       )
      call pc_get  ('corr_files'  ,obj%corr_files    )

      call mth_fix_pattern (obj%sx_init, obj%sx_inc, obj%sx_last,  &
                            obj%sx_tot, sx_last_keep, sx_tot_keep)

      call mth_fix_pattern (obj%sy_init, obj%sy_inc, obj%sy_last,  &
                            obj%sy_tot, sy_last_keep, sy_tot_keep)

      call mth_fix_pattern (obj%rx_init, obj%rx_inc, obj%rx_last,  &
                            obj%rx_tot, rx_last_keep, rx_tot_keep)

      call mth_fix_pattern (obj%ry_init, obj%ry_inc, obj%ry_last,  &
                            obj%ry_tot, ry_last_keep, ry_tot_keep)

      call mth_fix_pattern (obj%cmpx_init, obj%cmpx_inc, obj%cmpx_last,  &
                            obj%cmpx_tot, cmpx_last_keep, cmpx_tot_keep)

      call mth_fix_pattern (obj%cmpy_init, obj%cmpy_inc, obj%cmpy_last,  &
                            obj%cmpy_tot, cmpy_last_keep, cmpy_tot_keep)

      end subroutine cc3d_read_parameters


!!------------------------- verify parameters ------------------------------!!
!!------------------------- verify parameters ------------------------------!!
!!------------------------- verify parameters ------------------------------!!


      subroutine cc3d_verify_parameters (obj)

      type(cc3d_struct),intent(inout) :: obj                   ! arguments

      if (obj%corr_files /= 'NONE') obj%corr_files = 'TRC8'

      select case (obj%hdr_src)
           case ('GROUP')
                          obj%hdr_sx  = HDR_ORIGINAL_GROUP       ! 9
                          obj%hdr_sy  = 0
           case ('SEQU')
                          obj%hdr_sx  = HDR_SOURCE_GP            ! 46
                          obj%hdr_sy  = 0
           case ('LNSP')
                          obj%hdr_sx  = HDR_SOURCE_SHOTPOINT     ! 29
                          obj%hdr_sy  = HDR_SOURCE_LINE          ! 26
           case ('GRID')
                          obj%hdr_sx  = HDR_SOURCE_XGRID         ! 33
                          obj%hdr_sy  = HDR_SOURCE_YGRID         ! 34
           case default
                          obj%hdr_src = 'LNSP'
                          obj%hdr_sx  = HDR_SOURCE_SHOTPOINT     ! 29
                          obj%hdr_sy  = HDR_SOURCE_LINE          ! 26
      end select

      select case (obj%hdr_rec)
           case ('SEQU')
                          obj%hdr_rx  = HDR_RECEIVER_GP          ! 47
                          obj%hdr_ry  = 0
           case ('LNSP')
                          obj%hdr_rx  = HDR_RECEIVER_SHOTPOINT   ! 28
                          obj%hdr_ry  = HDR_RECEIVER_LINE        ! 27
           case ('GRID')
                          obj%hdr_rx  = HDR_RECEIVER_XGRID       ! 35
                          obj%hdr_ry  = HDR_RECEIVER_YGRID       ! 36
           case default
                          obj%hdr_rec = 'LNSP'
                          obj%hdr_rx  = HDR_RECEIVER_SHOTPOINT   ! 28
                          obj%hdr_ry  = HDR_RECEIVER_LINE        ! 27
      end select

      call mth_constrain (obj%max_static, 1.0  , 999.0  )
      call mth_constrain (obj%off_wid   , 10.0 , 99999.0)
      call mth_constrain (obj%weight    , 0.0  , 1.0    )
      call mth_constrain (obj%taper     , 0.0  , 1.0    )
      call mth_constrain (obj%update    , 0.1  , 1.0    )
      call mth_constrain (obj%pwr_cc    , 0.0  , 4.0    )
 
      call mth_constrain (obj%terms     , 1, 4    )
      call mth_constrain (obj%num_iter  , 1, 999  )
      call mth_constrain (obj%comp_x    , 1, 9999 )
      call mth_constrain (obj%comp_y    , 1, 9999 )
      call mth_constrain (obj%fold_max  , 1, 99999)
 
      if (obj%opt_print(1:3) == 'MIN' .or. obj%opt_print(1:3) == 'min') then
           obj%opt_print='MIN'
      else
           obj%opt_print='MAX'
      end if

      call pathcheck &
             ('path_src', obj%path_src, '.cc3d', show=PATHCHECK_INFO_OUTPUT)
      call pathcheck &
             ('path_rec', obj%path_rec, '.cc3d', show=PATHCHECK_INFO_OUTPUT)

      if (pc_verify_end()) then
           if (obj%path_src == PATHCHECK_EMPTY .and.  &
               obj%path_rec == PATHCHECK_EMPTY) then
                call pc_error ('PATH_SRC AND PATH_REC CANNOT BOTH BE MISSING')
           else if (obj%path_src == obj%path_rec) then
                call pc_error ('PATH_SRC AND PATH_REC CANNOT BE THE SAME')
           end if
      end if

      if (obj%num_cpus > 1) then
           if (.not. obj%dead_end) then
                call pc_warning &
                      ('DEAD_END set to YES since this is a parallel job.')
                obj%dead_end = .true.
           end if
           if (obj%corr_files /= 'NONE') then
                call pc_warning &
                      ('CORR_FILES set to NONE since this is a parallel job.')
                obj%corr_files = 'NONE'
           end if
      end if

      end subroutine cc3d_verify_parameters


!!----------------------- calculate dependencies ---------------------------!!
!!----------------------- calculate dependencies ---------------------------!!
!!----------------------- calculate dependencies ---------------------------!!

! This routine calculates or initializes all dependent parameters except
! the following:
!    CARDS(:)  NCARDS  TEMPTFILE              (set in prepare section)
!    IFOLD(:)                                 (set in prepare section)
!    INCREMENT_XSRC  INCREMENT_YSRC           (set in prepare section)
!    INCREMENT_XREC  INCREMENT_YREC           (set in prepare section)
!    INCREMENT_XCMP  INCREMENT_YCMP           (set in prepare section)
!    INCREMENT_OFFSET                         (set in prepare section)

! This section also slightly adjusts the following process parameters:
!    SX_LAST  SY_LAST  RX_LAST  RY_LAST  CMPX_LAST  CMPY_LAST  MAX_STATIC
!                                        CMPX_TOT   CMPY_TOT 


      subroutine cc3d_calculate_dependencies (obj)

      type(cc3d_struct),intent(inout) :: obj                   ! arguments

!  DECIDE WHAT TERMS TO CALCULATE AND WHAT FILES TO SAVE

      obj%use_src    = (obj%path_src /= PATHCHECK_EMPTY)
      obj%use_rec    = (obj%path_rec /= PATHCHECK_EMPTY)
      obj%use_cmp    = (obj%terms >= 3)
      obj%use_off    = (obj%terms == 4)
      obj%fish_style = (obj%terms == 1)

!  GET WINDOW AND CORRELATION SIZES

      obj%dtms = 1000.0 * obj%dt

      obj%ncorr     = nint(obj%max_static/obj%dtms)*2 + 1       ! always odd
      obj%ncorr     = max(obj%ncorr,5)
      obj%max_static = (obj%ncorr - 1)*obj%dtms/2.0

      obj%nwin16  = pkutil_npack16 (obj%nwin)  ! for 16-bit packed trace windows
      obj%nwin8   = pkutil_npack8  (obj%nwin)  ! for  8-bit packed trace windows
      obj%ncorr16 = pkutil_npack16 (obj%ncorr) ! for 16-bit packed correlations

!  GET SOURCE AND RECEIVER RANGES

      obj%NSRC   = obj%sx_tot * obj%sy_tot     ! total #sources
      obj%NREC   = obj%rx_tot * obj%ry_tot     ! total #receivers

      if (obj%hdr_sy == 0) obj%nsrc = obj%sx_tot
      if (obj%hdr_ry == 0) obj%nrec = obj%rx_tot

      if (.not.obj%use_src) obj%nsrc = 1
      if (.not.obj%use_rec) obj%nrec = 1

!  GET CMP AND SUPER-CMP RANGES

      obj%NXSUPER  = INT((obj%cmpx_tot-1)/obj%comp_x) + 1        ! super-cmp
      obj%NYSUPER  = INT((obj%cmpy_tot-1)/obj%comp_y) + 1        ! super-cmp
      obj%NSUPER   = obj%NXSUPER*obj%NYSUPER                     ! super-cmp

      obj%cmpx_tot = obj%NXSUPER*obj%comp_x        ! adjust to mult of comp_x
      obj%cmpy_tot = obj%NYSUPER*obj%comp_y        ! adjust to mult of comp_y
      obj%Ncmp     = obj%cmpx_tot * obj%cmpy_tot   ! total #cmps
      obj%NCOMP    = obj%comp_x * obj%comp_y       ! #cmps in super-cmp
      obj%NCOMP1   = pkutil_npack1(obj%ncomp)      ! for packing flags to 1 bit
      obj%maxfolds = obj%fold_max * obj%NCOMP      ! max fold of super-cmp

   obj%cmpx_last = obj%cmpx_init + (obj%cmpx_tot-1)*obj%cmpx_inc  ! adjust exact
   obj%cmpy_last = obj%cmpy_init + (obj%cmpy_tot-1)*obj%cmpy_inc  ! adjust exact

   obj%superxinc = obj%comp_x * obj%cmpx_inc                        ! super-cmp
   obj%superyinc = obj%comp_y * obj%cmpy_inc                        ! super-cmp
   obj%superxmin = obj%cmpx_init + 0.5*(obj%superxinc-obj%cmpx_inc) ! super-cmp
   obj%superymin = obj%cmpy_init + 0.5*(obj%superyinc-obj%cmpy_inc) ! super-cmp
   obj%superxmax = obj%superxmin + (obj%NXSUPER-1)*obj%superxinc    ! super-cmp
   obj%superymax = obj%superymin + (obj%NYSUPER-1)*obj%superyinc    ! super-cmp

      obj%NVEC     = obj%NREC + obj%NSRC + obj%NSUPER + 2*obj%NSUPER
      obj%NVEC2    = obj%NREC + obj%NSRC
      obj%maxONDSK = obj%maxfolds * obj%NSUPER

!  GET CORRELATION FILE NAMES

      obj%path_byt_src = obj%path_src
      obj%path_byt_rec = obj%path_rec

      select case (obj%corr_files)
           case ('TRC8')
              if (obj%use_src) call addext (obj%path_byt_src, 'trc8', .true.)
              if (obj%use_rec) call addext (obj%path_byt_rec, 'trc8', .true.)
           case default
              obj%path_byt_src = PATHCHECK_EMPTY
              obj%path_byt_rec = PATHCHECK_EMPTY
      end select

!  GET NUMBER OF WORKERS (not including the boss)

 !!!  obj%nworkers = pcps_num_workers     ! apparently == 1 when only 1 cpu.
 !!!  obj%nworkers = pcps_num_workers     ! valid at backend only for >1 cpu.
 !!!  obj%nworkers = pcps_num_procs - 1   ! valid at backend only.
      obj%nworkers = obj%num_cpus - 1     ! valid at frontend and backend.

      end subroutine cc3d_calculate_dependencies


!!-------------------------- print parameters ---------------------------!!
!!-------------------------- print parameters ---------------------------!!
!!-------------------------- print parameters ---------------------------!!

! This routine prints information but does not change any member variables.


      subroutine cc3d_print_parameters (obj)

      type(cc3d_struct),intent(in) :: obj                   ! arguments

      call pc_print (' ')
      call pc_print ('CC3D: STATIC AND BYTE FILES TO BE CREATED:')
      if (obj%path_src     /= PATHCHECK_EMPTY) &
                                     call pc_print ('CC3D:',obj%path_src)
      if (obj%path_rec     /= PATHCHECK_EMPTY) &
                                     call pc_print ('CC3D:',obj%path_rec)
      if (obj%path_byt_src /= PATHCHECK_EMPTY) &
                                     call pc_print ('CC3D:',obj%path_byt_src)
      if (obj%path_byt_rec /= PATHCHECK_EMPTY) &
                                     call pc_print ('CC3D:',obj%path_byt_rec)
      call pc_print (' ')

      if (obj%fish_style) then
           call pc_print ('CC3D: FISH-STYLE MATCHED BASE TRACES will be used')
      else
       call pc_print ('CC3D: FISH-STYLE MATCHED BASE TRACES will NOT be used')
      end if

      if (obj%use_src) then
           call pc_print ('CC3D: SOURCE STATIC FILE     will be   calculated')
      else
           call pc_print ('CC3D: SOURCE STATIC FILE   will NOT be calculated')
      end if

      if (obj%use_rec) then
           call pc_print ('CC3D: RECEIVER STATIC FILE   will be   calculated')
      else
           call pc_print ('CC3D: RECEIVER STATIC FILE will NOT be calculated')
      end if

      if (obj%use_cmp) then
           call pc_print ('CC3D: CMP DIP TERM           will be   calculated')
      else
           call pc_print ('CC3D: CMP DIP TERM         will NOT be calculated')
      end if

      if (obj%use_off) then
           call pc_print ('CC3D: RESIDUAL NMO TERM      will be   calculated')
      else
           call pc_print ('CC3D: RESIDUAL NMO TERM    will NOT be calculated')
      end if

      if (obj%path_byt_src /= PATHCHECK_EMPTY) then
           call pc_print ('CC3D: SOURCE BYTE FILE       will be     output')
      else
           call pc_print ('CC3D: SOURCE BYTE FILE     will NOT be   output')
      end if

      if (obj%path_byt_rec /= PATHCHECK_EMPTY) then
           call pc_print ('CC3D: RECEIVER BYTE FILE     will be     output')
      else
           call pc_print ('CC3D: RECEIVER BYTE FILE   will NOT be   output')
      end if

 write(lunprint,*)' '
 write(lunprint,*)'CC3D: MAXIMUM NUMBER OF TRACES TO SAVE TO DISK IS ',  &
                                                                 obj%maxONDSK
 write(lunprint,*)'CC3D: Number of  source  ground positions = ',obj%NSRC
 write(lunprint,*)'CC3D: Number of receiver ground positions = ',obj%NREC
 write(lunprint,*)'CC3D: Number of actual CMPs = ',obj%Ncmp
 write(lunprint,*)'CC3D: Number of  super-CMPs = ',obj%NSUPER
 write(lunprint,*)'CC3D: Maximum fold in actual CMP = ',obj%fold_max
 write(lunprint,*)'CC3D: Maximum fold in  super-CMP = ',obj%maxfolds
 write(lunprint,*)'CC3D: Number of unknowns in the model vector = ',obj%NVEC
 write(lunprint,*)'CC3D: Number of samples in correlation window = ',obj%nwin
 write(lunprint,*)'CC3D: Number of  words  in correlation window = ',  &
                                         obj%nwin16,' (packed to 16 bits)'
 write(lunprint,*)'CC3D: Number of  words  in correlation window = ',  &
                                         obj%nwin8,' (packed to 8 bits)'
 write(lunprint,*)'CC3D: Number of samples in cross-correlations = ',obj%ncorr
 write(lunprint,*)'CC3D: Number of  words  in cross-correlations = ',  &
                                         obj%ncorr16,' (packed to 16 bits)'
 write(lunprint,*)'CC3D: Number of iterations (passes) to perform = ', &
                                                                  obj%num_iter
 write(lunprint,*)' '
 write(lunprint,*)'CC3D: Residual static shifts are weighted by',            &
                        ' Correlation Coefficients raised to power ',obj%pwr_cc
 write(lunprint,*)'CC3D: Correlation functions are tapered down by',         &
                        ' fraction ',obj%taper,' at edges.'
 write(lunprint,*)'CC3D: Static values are updated by fraction ',obj%update, &
                        ' after each pass.'

      end subroutine cc3d_print_parameters


!!-------------------------- write parameters -----------------------------!!
!!-------------------------- write parameters -----------------------------!!
!!-------------------------- write parameters -----------------------------!!

! This section writes parameters but does not change any member variables.
! With the exception that unused ranges are reset to defaults after writing.


      subroutine cc3d_write_parameters (obj)

      type(cc3d_struct),intent(inout) :: obj                   ! arguments

      real               :: adisk                               ! local
      integer            :: noff_est,nstore,nscratch            ! local
      integer            :: nrecords,ndisk                      ! local
      logical            :: show_sx,show_sy,show_rx,show_ry     ! local
      double precision   :: lenw         ! to protect from integer overflow.

 call pc_put_options_field ('opt_print' ,  opt_print_options,  OPT_PRINT_NOPT)
 call pc_put_options_field ('terms'     ,      terms_options,      TERMS_NOPT)
 call pc_put_options_field ('hdr_src'   ,    hdr_src_options,    HDR_SRC_NOPT)
 call pc_put_options_field ('hdr_rec'   ,    hdr_rec_options,    HDR_REC_NOPT)
 call pc_put_options_field ('corr_files', corr_files_options, CORR_FILES_NOPT)

      call pc_put_global  ('numtr'      , 1        )
      call pc_put_global  ('gathered'   ,.false.   )

      call pc_put  ('max_static'  ,obj%max_static     ,6)
      call pc_put  ('fold_max'    ,obj%fold_max         )
      call pc_put  ('num_iter'    ,obj%num_iter         )
      call pc_put  ('terms'       ,obj%terms            )
      call pc_put  ('opt_print'   ,obj%opt_print        )
      call pc_put  ('no_dead'     ,obj%no_dead          )
      call pc_put  ('dead_end'    ,obj%dead_end         )
      call pc_put  ('weight'      ,obj%weight         ,6)
      call pc_put  ('off_wid'     ,obj%off_wid        ,7)
      call pc_put  ('pwr_cc'      ,obj%pwr_cc         ,7)  ! not in GUI
      call pc_put  ('taper'       ,obj%taper          ,7)  ! not in GUI
      call pc_put  ('update'      ,obj%update         ,7)  ! not in GUI
      call pc_put  ('path_src'    ,obj%path_src         )
      call pc_put  ('path_rec'    ,obj%path_rec         )
      call pc_put  ('corr_files'  ,obj%corr_files       )

      call pc_put  ('comp_x'      ,obj%comp_x           )
      call pc_put  ('comp_y'      ,obj%comp_y           )
      call pc_put  ('cmpx_init'   ,obj%cmpx_init      ,7)
      call pc_put  ('cmpy_init'   ,obj%cmpy_init      ,7)
      call pc_put  ('cmpx_inc'    ,obj%cmpx_inc       ,5)
      call pc_put  ('cmpy_inc'    ,obj%cmpy_inc       ,5)
      call pc_put  ('cmpx_last'   ,obj%cmpx_last      ,7)
      call pc_put  ('cmpy_last'   ,obj%cmpy_last      ,7)
      call pc_put  ('cmpx_tot'    ,obj%cmpx_tot         )
      call pc_put  ('cmpy_tot'    ,obj%cmpy_tot         )

      call pc_put          ('hdr_src'     ,obj%hdr_src          )
      call pc_put_gui_only ('hdr_sx'      ,obj%hdr_sx           )
      call pc_put_gui_only ('hdr_sy'      ,obj%hdr_sy           )
      call pc_put          ('sx_init'     ,obj%sx_init        ,8)
      call pc_put          ('sy_init'     ,obj%sy_init        ,8)
      call pc_put          ('sx_inc'      ,obj%sx_inc         ,5)
      call pc_put          ('sy_inc'      ,obj%sy_inc         ,5)
      call pc_put          ('sx_last'     ,obj%sx_last        ,8)
      call pc_put          ('sy_last'     ,obj%sy_last        ,8)
      call pc_put          ('sx_tot'      ,obj%sx_tot           )
      call pc_put          ('sy_tot'      ,obj%sy_tot           )

      call pc_put          ('hdr_rec'     ,obj%hdr_rec          )
      call pc_put_gui_only ('hdr_rx'      ,obj%hdr_rx           )
      call pc_put_gui_only ('hdr_ry'      ,obj%hdr_ry           )
      call pc_put          ('rx_init'     ,obj%rx_init        ,8)
      call pc_put          ('ry_init'     ,obj%ry_init        ,8)
      call pc_put          ('rx_inc'      ,obj%rx_inc         ,5)
      call pc_put          ('ry_inc'      ,obj%ry_inc         ,5)
      call pc_put          ('rx_last'     ,obj%rx_last        ,8)
      call pc_put          ('ry_last'     ,obj%ry_last        ,8)
      call pc_put          ('rx_tot'      ,obj%rx_tot           )
      call pc_put          ('ry_tot'      ,obj%ry_tot           )

      NOFF_EST = nint(15000.0 / obj%off_wid)      ! rough estimate for NOFF
      NSTORE   = obj%NSUPER
      NSCRATCH = 1 + 2 * obj%nvec + 3 * obj%ncorr + obj%ncomp               &
                  + obj%nvec2 * (4 + obj%nwin16 + obj%ncorr16 + obj%ncomp1) &
                  + obj%nwin * (4 + obj%ncomp + noff_est)
      NRECORDS = NHEAD + obj%NWIN8                   ! length of record (words)
      LENW     = DBLE(obj%MAXONDSK) * DBLE(NRECORDS) ! length of file   (words)
      ADISK    = 8.0*LENW/1000000.0                ! length of file (megabytes)
      NDISK    = NINT(ADISK)                       ! length of file (megabytes)

      call pc_put_gui_only ('nstore'      , nstore    ,9  )
      call pc_put_gui_only ('nscratch'    , nscratch  ,9  )
      call pc_put_gui_only ('disk'        , adisk     ,9,2)
      call pc_put_gui_only ('ncpus'       , obj%num_cpus  )

      show_sx = (obj%use_src .and. obj%hdr_sx /= 0)
      show_sy = (obj%use_src .and. obj%hdr_sy /= 0)
      show_rx = (obj%use_rec .and. obj%hdr_rx /= 0)
      show_ry = (obj%use_rec .and. obj%hdr_ry /= 0)

      call pc_put_sensitive_field_flag ('no_dead'   , obj%corr_files /= 'NONE')
      call pc_put_sensitive_field_flag ('corr_files', obj%num_cpus == 1)
      call pc_put_sensitive_field_flag ('dead_end'  , obj%num_cpus == 1)

      call pc_put_sensitive_field_flag ('sx_init', show_sx)
      call pc_put_sensitive_field_flag ('sx_inc ', show_sx)
      call pc_put_sensitive_field_flag ('sx_last', show_sx)
      call pc_put_sensitive_field_flag ('sx_tot ', show_sx)

      call pc_put_sensitive_field_flag ('sy_init', show_sy)
      call pc_put_sensitive_field_flag ('sy_inc ', show_sy)
      call pc_put_sensitive_field_flag ('sy_last', show_sy)
      call pc_put_sensitive_field_flag ('sy_tot ', show_sy)

      call pc_put_sensitive_field_flag ('rx_init', show_rx)
      call pc_put_sensitive_field_flag ('rx_inc ', show_rx)
      call pc_put_sensitive_field_flag ('rx_last', show_rx)
      call pc_put_sensitive_field_flag ('rx_tot ', show_rx)

      call pc_put_sensitive_field_flag ('ry_init', show_ry)
      call pc_put_sensitive_field_flag ('ry_inc ', show_ry)
      call pc_put_sensitive_field_flag ('ry_last', show_ry)
      call pc_put_sensitive_field_flag ('ry_tot ', show_ry)

      call pc_put_control ('need_request'           , .true.)
      call pc_put_control ('need_label'             , .true.)
      call pc_put_control ('nscratch'               , nscratch)
      call pc_put_control ('nstore'                 , nstore)
      call pc_put_control ('ndisk'                  , ndisk)
      call pc_put_control ('iftd'                   , .true.)
      call pc_put_control ('parallel_safe'          , .true.)
      call pc_put_control ('PCPS_BOSS_EXEC_MODE'    , 'PCPS_BOSS_EXECS')
      call pc_put_control ('PCPS_SEND_MODE'         , 'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_SEND_MODE'     , 'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_SEND_EOF_MODE'     , 'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_RECEIVE_MODE'      , 'PCPS_RECEIVE_GATHER')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE'  , 'PCPS_RECEIVE_GATHER')
      call pc_put_control ('PCPS_GENERATOR_MODE'    , 'PCPS_NO_TRACE_GEN')
      call pc_put_control ('PCPS_ALT_GENERATOR_MODE', 'PCPS_NO_TRACE_GEN')

      if (.not.show_sx) then
           obj%sx_init      = 1.0
           obj%sx_inc       = 1.0
           obj%sx_last      = 1.0
           obj%sx_tot       = 1
      end if

      if (.not.show_sy) then
           obj%sy_init      = 1.0
           obj%sy_inc       = 1.0
           obj%sy_last      = 1.0
           obj%sy_tot       = 1
      end if

      if (.not.show_rx) then
           obj%rx_init      = 1.0
           obj%rx_inc       = 1.0
           obj%rx_last      = 1.0
           obj%rx_tot       = 1
      end if

      if (.not.show_ry) then
           obj%ry_init      = 1.0
           obj%ry_inc       = 1.0
           obj%ry_last      = 1.0
           obj%ry_tot       = 1
      end if

      call cc3d_print_summary1 (obj, 'REQUESTED')

      end subroutine cc3d_write_parameters


!!-------------------------- fetch parallel info ---------------------------!!
!!-------------------------- fetch parallel info ---------------------------!!
!!-------------------------- fetch parallel info ---------------------------!!


      subroutine cc3d_fetch_parallel_info (obj)

      type(cc3d_struct),intent(inout) :: obj                    ! arguments

      integer            :: isuper_start_temp,isuper_stop_temp  ! local
      integer            :: worker,kount,number                 ! local
      character(len=4)   :: marker                              ! local

      if (obj%nsuper < obj%num_cpus) then
           call pc_error ("cannot have more CPUs than the number of super-CMPS")
           call pc_print ("the number of CPUs is",obj%num_cpus)
           call pc_print ("the number of super-CMPs is",obj%nsuper)
      end if

      write(lunprint,*) 'CC3D: PARALLEL RANGES:      -------super-cmps--------'
      write(lunprint,*) 'CC3D:                       istart    istop    number'

      kount = 0

      do worker = 0,obj%nworkers         ! worker number 0 is the boss.

           call cc3d_parallel_range ('super-cmps',worker,obj%num_cpus,  &
                                     obj%nsuper,                        &
                                     isuper_start_temp,isuper_stop_temp)

           if (worker == pcps_current_worker_num) then
                marker   = '===>'
                obj%isuper_start = isuper_start_temp
                obj%isuper_stop  = isuper_stop_temp
           else
                marker   = '    '
           end if

           number = isuper_stop_temp - isuper_start_temp + 1
           kount  = kount + number

           write(lunprint,2000) worker,marker,                        &
                                isuper_start_temp, isuper_stop_temp, number

      end do

      write(lunprint,1000) 1,obj%nsuper
      write(lunprint,*)' '

1000  format (' CC3D:',2x,'full range',6x,  2x,3i9)
2000  format (' CC3D:',2x,'worker',i4,2x,a4,2x,3i9)

      if (obj%nsuper >= obj%num_cpus .and. kount /= obj%nsuper) then
           call pc_error ("error dividing up super-CMPs among CPUs")
           call pc_print ("this is a programming error")
      end if

      end subroutine cc3d_fetch_parallel_info


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

! This routine allocates and/or initializes the following dependent variables:
!          CARDS(:)  NCARDS  TEMPTFILE  IFOLD(:)
!          INCREMENT_XSRC  INCREMENT_YSRC
!          INCREMENT_XREC  INCREMENT_YREC
!          INCREMENT_XCMP  INCREMENT_YCMP  INCREMENT_OFFSET


      subroutine cc3d_prepare_for_execution (obj)

      type(cc3d_struct),intent(inout) :: obj                  ! arguments
      integer                         :: err                  ! local

!  DEALLOCATE STORAGE

      if(associated(obj%ifold))    deallocate (obj%ifold)
      if(associated(obj%cards))    deallocate (obj%cards)

!  THE BOSS AND WORKERS ARE EXECUTING HERE.

      if (pc_do_not_process_traces()) return

!  ONLY THE WORKERS ARE EXECUTING HERE IF PCPS_BOSS_DISTRIBUTES.
!  BOSS AND WORKERS ARE EXECUTING HERE IF PCPS_BOSS_EXECS.

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      if (pcps_num_procs /= obj%num_cpus) then
           call pc_error ('wrong number of processes',pcps_num_procs, &
                          '- should be',obj%num_cpus)
      endif

      if (obj%timer /= INIL) call timer_free (obj%timer)

      call timer_alloc (obj%timer)
      call timer_clear (obj%timer)

!  ALLOCATE STORAGE

      call pc_alloc_process_cards (obj%cards,obj%ncards)

      allocate (obj%ifold(obj%NSUPER))       ! fold of each super-cmp
      obj%ifold = 0                          ! clears entire array

!  CREATE TEMPORARY TRACE WINDOW FILE

      call temptfile_open &
                    (obj%temptfile,'cc3d_traces',NHEAD,obj%nwin8,lunprint,err)
      if (err /= TEMPTFILE_OK) call pc_error ('CC3D: TEMPTFILE_OPEN error')

      call increment_init (obj%increment_xsrco, DELTA, NUMINC)
      call increment_init (obj%increment_ysrco, DELTA, NUMINC)
      call increment_init (obj%increment_xreco, DELTA, NUMINC)
      call increment_init (obj%increment_yreco, DELTA, NUMINC)
      call increment_init (obj%increment_xcmpo, DELTA, NUMINC)
      call increment_init (obj%increment_ycmpo, DELTA, NUMINC)
      call increment_init (obj%increment_offseto)

      call increment_init (obj%increment_xsrc, DELTA, NUMINC)
      call increment_init (obj%increment_ysrc, DELTA, NUMINC)
      call increment_init (obj%increment_xrec, DELTA, NUMINC)
      call increment_init (obj%increment_yrec, DELTA, NUMINC)
      call increment_init (obj%increment_xcmp, DELTA, NUMINC)
      call increment_init (obj%increment_ycmp, DELTA, NUMINC)
      call increment_init (obj%increment_offset)

!  CREATE FILE FOR STORING TRACES FOR LATER SHIFTING AND OUTPUT

      if (.not.obj%dead_end) then
            call statshift_open (obj%statshift, 'cc3d_trace_file', &
                                 obj%nwih, obj%ndpt, obj%dt,       &
                                 obj%path_src, obj%path_rec, lunprint, err)
            if (err /= STATSHIFT_OK)  &
                         call pc_error ('CC3D: FATAL OPEN ERROR IN STATSHIFT')
      end if

!  INITIALIZE COUNTERS

      obj%IDROP  = 0     ! total number of traces dropped
      obj%IDROPD = 0     ! drop trace due to insufficient live window size
      obj%IDROPS = 0     ! drop trace due to source out of range
      obj%IDROPR = 0     ! drop trace due to receiver out of range
      obj%IDROPC = 0     ! drop trace due to cmp out of range
      obj%IDROPF = 0     ! drop trace due to excessive fold
      obj%ICOUNT = 0     ! total number of traces received
      obj%noff   = 0

      end subroutine cc3d_prepare_for_execution


!!------------------------- process super cmp ------------------------------!!
!!------------------------- process super cmp ------------------------------!!
!!------------------------- process super cmp ------------------------------!!


      subroutine cc3d_process_super_cmp           &
                  (obj,                           & ! changed
                   iorrr,iosss,iohhh,ioccc,       & ! in   
                   isuper,                        & ! in   
                   tapr,                          & ! in       (ncorr)
                   stat,                          & ! in       (nvec)
                   rstat,                         & ! updated  (nvec)
                   wghts,                         & ! updated  (nvec2)
                   ntraces,                       & ! updated  (nvec2)
                   nccoef,                        & ! updated  (nvec2)
                   stacked_correlations,          & ! updated  (ncorr16,nvec2)
                   max_exists,                    & ! updated
                   sum_exists,                    & ! updated
                   pwr_super,pwr_comp,            & ! updated
                   timer1,timer2,                 & ! in   
                   error)

      type(cc3d_struct),intent(inout) :: obj
      integer          ,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      integer          ,intent(in)    :: isuper
      real             ,intent(in)    :: tapr                     (:)         
      real             ,intent(in)    :: stat                     (:)         
      real             ,intent(inout) :: rstat                    (:)
      real             ,intent(inout) :: wghts                    (:)
      integer          ,intent(inout) :: NTRACES                  (:)
      integer          ,intent(inout) :: nccoef                   (:)
      integer          ,intent(inout) :: stacked_correlations     (:,:) 
      integer          ,intent(inout) :: max_exists
      real             ,intent(inout) :: SUM_exists
      real             ,intent(inout) :: pwr_super,pwr_comp
      integer          ,intent(in)    :: timer1,timer2
      logical          ,intent(out)   :: error

!  AUTOMATIC ARRAYS

      real    :: SUPER_STACK              (obj%nwin)
      real    :: stacked_cmps             (obj%nwin,   obj%NCOMP)
      real    :: stacked_offsets          (obj%nwin,   obj%NOFF)
      integer :: stacked_ground_positions (obj%nwin16, obj%NVEC2)
      logical :: EXISTS_IN_SUPER                      (obj%NVEC2)
      integer :: exists_in_cmp            (obj%NCOMP1, obj%NVEC2)

!  FORM STACKS

      call timer_start (timer1)

      call cc3d_form_stacks                       &
                  (obj,                           & ! changed
                   iorrr,iosss,iohhh,ioccc,       & ! in   
                   isuper,                        & ! in   
                   stat,                          & ! in        (nvec)
                   ntraces,                       & ! updated   (nvec2)
                   max_exists,                    & ! updated
                   sum_exists,                    & ! updated
                   SUPER_STACK,                   & ! out       (nwin)
                   stacked_cmps,                  & ! out       (nwin,ncomp)
                   stacked_offsets,               & ! out       (nwin,noff)
                   stacked_ground_positions,      & ! out       (nwin16,nvec2)
                   exists_in_super,               & ! out       (nvec2)
                   exists_in_cmp,                 & ! out       (ncomp1,nvec2)
                   error)                           ! out   

      call timer_stop (timer1)

      if (error) then
           call pc_error ('CC3D: fatal error in cc3d_form_stacks')
           return
      end if

!  FORM CORRELATIONS

      call timer_start (timer2)

      call cc3d_form_correlations                 &
                  (obj,iorrr,iosss,iohhh,ioccc,   & ! in
                   isuper,                        & ! in
                   tapr,                          & ! in       (ncorr)
                   SUPER_STACK,                   & ! in       (nwin)
                   stacked_cmps,                  & ! in       (nwin,ncomp)
                   stacked_offsets,               & ! in       (nwin,noff)
                   stacked_ground_positions,      & ! in       (nwin16,nvec2)
                   exists_in_super,               & ! in       (nvec2)
                   exists_in_cmp,                 & ! in       (ncomp1,nvec2)
                   rstat,                         & ! updated  (nvec)
                   wghts,                         & ! updated  (nvec2)
                   nccoef,                        & ! updated  (nvec2)
                   stacked_correlations,          & ! updated  (ncorr16,nvec2)
                   pwr_super,pwr_comp)              ! updated

      call timer_stop (timer2)

      end subroutine cc3d_process_super_cmp


!!-------------------------- process single trace --------------------------!!
!!-------------------------- process single trace --------------------------!!
!!-------------------------- process single trace --------------------------!!

           ! Read single trace of this super-CMP into memory.
           !
           ! Apply shifts (derived from previous iterations) for
           ! source, receiver, CMP within the super-CMP (if TERMS=3 or
           ! TERMS=4), and offset (if TERMS=4).
           !
           ! Stack the shifted trace five ways:
           !     by super-CMP (all traces in super-CMP).
           !     by common sources (if source file was requested),
           !     by common receivers (if receiver file was requested),
           !     by common CMPs (if TERMS=1 or TERMS=3 or TERMS=4),
           !     by common offset bins defined by OFF_WID (if TERMS=4),


      subroutine cc3d_process_single_trace        &
                  (obj,                           & ! changed
                   iorrr,iosss,iohhh,ioccc,       & ! in   
                   isuper,ifold,                  & ! in   
                   stat,                          & ! in        (nvec)
                   ntraces,                       & ! updated   (nvec2)
                   SUPER_STACK,                   & ! updated   (nwin)
                   stacked_cmps,                  & ! updated   (nwin,ncomp)
                   stacked_offsets,               & ! updated   (nwin,noff)
                   stacked_ground_positions,      & ! updated   (nwin16,nvec2)
                   exists_in_super,               & ! updated   (nvec2)
                   exists_in_cmp,                 & ! updated   (ncomp1,nvec2)
                   error)                           ! out   

!  ARGUMENTS

      type(cc3d_struct),intent(inout) :: obj
      integer          ,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      integer          ,intent(in)    :: isuper,ifold
      real             ,intent(in)    :: stat                     (:)         
      integer          ,intent(inout) :: NTRACES                  (:)
      real             ,intent(inout) :: SUPER_STACK              (:)
      real             ,intent(inout) :: stacked_cmps             (:,:)
      real             ,intent(inout) :: stacked_offsets          (:,:) 
      integer          ,intent(inout) :: stacked_ground_positions (:,:)
      logical          ,intent(inout) :: EXISTS_IN_SUPER          (:) 
      integer          ,intent(inout) :: exists_in_cmp            (:,:)
      logical          ,intent(out)   :: error

!  LOCAL VARIABLES

      integer :: ISRC,IREC,ioff,icmp
      integer :: ICOMP
      real    :: delcmpx,delcmpy

!  AUTOMATIC ARRAYS

      real :: trace(obj%nwin)

!  READ NEXT SUPER-CMP TRACE FROM TRACE WINDOW FILE INTO MEMORY

      call cc3d_read_trace_window (obj,                  &  ! changed
                                   isuper,ifold,         &  ! in   
                                   isrc,irec,ioff,icmp,  &  ! out   
                                   trace,error)             ! out   
      if (error) return

      NTRACES(IOSSS+ISRC) = NTRACES(IOSSS+ISRC) + 1
      NTRACES(IORRR+IREC) = NTRACES(IORRR+IREC) + 1

      call cc3d_get_delcmpx_delcmpy_icomp &
                      (obj,isuper,icmp,  delcmpx,delcmpy,icomp)

!  APPLY THE CURRENT TRACE STATIC

      call cc3d_shift_trace_window (obj,iosss,iorrr,ioccc,iohhh,   &
                                    delcmpx,delcmpy,               &
                                    isrc,irec,isuper,ioff,         &
                                    stat,trace)

!  ADD TRACE TO STACKED TRACES

      call cc3d_add_to_stacks (obj,iosss,iorrr,           & ! in   
                               icomp,isrc,irec,ioff,      & ! in   
                               trace,                     & ! in   
                               super_stack,               & ! updated
                               stacked_cmps,              & ! updated
                               stacked_offsets,           & ! updated
                               stacked_ground_positions,  & ! updated
                               exists_in_super,           & ! updated
                               exists_in_cmp)               ! updated

      end subroutine cc3d_process_single_trace


!!-------------------------- add to stacks ---------------------------------!!
!!-------------------------- add to stacks ---------------------------------!!
!!-------------------------- add to stacks ---------------------------------!!

           ! Stack the shifted trace five ways:
           !     by super-CMP (all traces in super-CMP).
           !     by common sources (if source file was requested),
           !     by common receivers (if receiver file was requested),
           !     by common CMPs (if TERMS=1 or TERMS=3 or TERMS=4),
           !     by common offset bins defined by OFF_WID (if TERMS=4),


      subroutine cc3d_add_to_stacks (obj,                       & ! in   
                                     iosss,iorrr,               & ! in   
                                     icomp,isrc,irec,ioff,      & ! in   
                                     trace,                     & ! in   
                                     super_stack,               & ! updated
                                     stacked_cmps,              & ! updated
                                     stacked_offsets,           & ! updated
                                     stacked_ground_positions,  & ! updated
                                     exists_in_super,           & ! updated
                                     exists_in_cmp)               ! updated

      type(cc3d_struct),intent(in)    :: obj
      integer          ,intent(in)    :: IORRR,IOSSS
      integer          ,intent(in)    :: icomp,isrc,irec,ioff
      real             ,intent(in)    :: trace                    (:)         
      real             ,intent(inout) :: SUPER_STACK              (:)
      real             ,intent(inout) :: stacked_cmps             (:,:)
      real             ,intent(inout) :: stacked_offsets          (:,:) 
      integer          ,intent(inout) :: stacked_ground_positions (:,:)
      logical          ,intent(inout) :: EXISTS_IN_SUPER          (:) 
      integer          ,intent(inout) :: exists_in_cmp            (:,:)

      call mth_add    (trace,obj%nwin,  SUPER_STACK)

      if (obj%use_cmp .or. obj%fish_style) then
             call mth_add (trace,obj%nwin,  stacked_cmps(:,ICOMP))
      end if

      if (obj%use_off) then
             call mth_add (trace,obj%nwin,  stacked_offsets(:,ioff))
      end if

      if (obj%use_src) then
             exists_in_super(iosss+isrc) = .true.
             call cc3d_add16 (trace, obj%nwin,                         &
                              stacked_ground_positions(:,iosss+isrc),  &
                              obj%nwin16)
      end if

      if (obj%use_rec) then
             exists_in_super(iorrr+irec) = .true.
             call cc3d_add16 (trace, obj%nwin,                         &
                              stacked_ground_positions(:,iorrr+irec),  &
                              obj%nwin16)
      end if

      if (obj%use_src .and. obj%fish_style) then
             call cc3d_add1 (icomp, obj%ncomp,              &
                             exists_in_cmp(:,iosss+isrc),   &
                             obj%ncomp1)
      end if

      if (obj%use_rec .and. obj%fish_style) then
             call cc3d_add1 (icomp, obj%ncomp,              &
                             exists_in_cmp(:,iorrr+irec),   &
                             obj%ncomp1)
      end if

      end subroutine cc3d_add_to_stacks


!!-------------------- get delcmpx delcmpy icomp -----------------------!!
!!-------------------- get delcmpx delcmpy icomp -----------------------!!
!!-------------------- get delcmpx delcmpy icomp -----------------------!!


      subroutine cc3d_get_delcmpx_delcmpy_icomp &
                      (obj,isuper,icmp,  delcmpx,delcmpy,icomp)

      type(cc3d_struct),intent(in)    :: obj                   ! arguments
      integer          ,intent(in)    :: isuper,icmp           ! arguments
      real             ,intent(out)   :: delcmpx,delcmpy       ! arguments
      integer          ,intent(out)   :: icomp                 ! arguments
      integer                         :: ixsuper,iysuper       ! local
      real                            :: excmp,eycmp           ! local
      integer                         :: ixcmp,iycmp           ! local
      integer                         :: icompx,icompy         ! local

      call mth_split_index (ISUPER,obj%NXSUPER,   IXSUPER,IYSUPER)
      excmp = (IXSUPER - 0.5)*obj%comp_x + 0.5
      eycmp = (IYSUPER - 0.5)*obj%comp_y + 0.5

      call mth_split_index (icmp,obj%cmpx_tot,   ixcmp,iycmp)
      delcmpx = ixcmp - excmp
      delcmpy = iycmp - eycmp

      ICOMPX = nint(0.5*(obj%comp_x+1) + delcmpx)
      ICOMPY = nint(0.5*(obj%comp_y+1) + delcmpy)
      ICOMP = ICOMPX + (ICOMPY - 1)*obj%comp_x

      ! IXSUPER and IYSUPER = x and y super-cmp indices.
      ! IXCMP   and IYCMP   = x and y cmp indices.
      ! EXCMP   and EYCMP   = exact x and y cmp indices of center of super-cmp.
      ! ICOMPX  and ICOMPY  = x and y local indices of cmp within super-cmp.
      ! DELCMPX and DELCMPY = x and y local distances of cmp from center of
      !                                           super-cmp.

      ! ISUPER = sequential index of super-cmp.
      ! ICMP   = sequential index of cmp.
      ! ICOMP  = sequential local index of cmp within super-cmp gather.

      end subroutine cc3d_get_delcmpx_delcmpy_icomp


!!------------------------ get delcmpx delcmpy ---------------------------!!
!!------------------------ get delcmpx delcmpy ---------------------------!!
!!------------------------ get delcmpx delcmpy ---------------------------!!


      subroutine cc3d_get_delcmpx_delcmpy (obj,icomp,   delcmpx,delcmpy)

      type(cc3d_struct),intent(in)    :: obj                   ! arguments
      integer          ,intent(in)    :: icomp                 ! arguments
      real             ,intent(out)   :: delcmpx,delcmpy       ! arguments
      integer                         :: icompx,icompy         ! local

      call mth_split_index (ICOMP,obj%comp_x,   ICOMPX,ICOMPY)

      delcmpx = icompx - 0.5 * (obj%comp_x + 1)
      delcmpy = icompy - 0.5 * (obj%comp_y + 1)

      ! ICOMPX  and ICOMPY  = x and y local indices of cmp within super-cmp.
      ! DELCMPX and DELCMPY = x and y local distances of cmp from center of
      !                                           super-cmp.

      ! ICOMP  = sequential local index of cmp within super-cmp gather.

      end subroutine cc3d_get_delcmpx_delcmpy


!!-------------------------- merge results --------------------------------!!
!!-------------------------- merge results --------------------------------!!
!!-------------------------- merge results --------------------------------!!

                    ! Merge from all cpus to boss.

      subroutine cc3d_merge_results             &
                (obj,                           & ! in   
                 rstat,                         & ! updated  (nvec)
                 wghts,                         & ! updated  (nvec2)
                 ntraces,                       & ! updated  (nvec2)
                 nccoef,                        & ! updated  (nvec2)
                 stacked_correlations,          & ! updated  (ncorr16,nvec2)
                 max_exists,                    & ! updated
                 sum_exists,                    & ! updated
                 pwr_super,pwr_comp,            & ! updated
                 error)

      type(cc3d_struct),intent(in) :: obj

      real   ,intent(inout)   :: rstat  (:)       ! residual statics values
      real   ,intent(inout)   :: wghts  (:)       ! correlation coefficients
      integer,intent(inout)   :: NTRACES(:)       ! number of traces
      integer,intent(inout)   :: NCCOEF (:)       ! number of corr coefs
      integer,intent(inout)   :: stacked_correlations (:,:)
      integer,intent(inout)   :: max_exists
      real   ,intent(inout)   :: sum_exists
      real   ,intent(inout)   :: pwr_super,pwr_comp
      logical,intent(out)     :: error

      integer         :: ivec,istat                           ! local
      real   ,pointer :: scratch(:,:)                         ! local

      error = .false.

      if (obj%nworkers == 0) return

      allocate (scratch(obj%ncorr,obj%nvec2),stat=istat)

      if(istat /= 0) then
           call pc_error &
                 ("CC3D: error allocating",obj%nvec * obj%ncorr,"during merge")
           error = .true.
           return
      end if

      do ivec = 1,obj%nvec2
           call pkutil_unpack16 (stacked_correlations(:,ivec),obj%Ncorr16, &
                                              scratch(:,ivec),obj%Ncorr)
      end do

      call pcpsx_sum_reduce (0, obj%nvec  ,rstat      ,rstat)
      call pcpsx_sum_reduce (0, obj%nvec2 ,wghts      ,wghts)
      call pcpsx_sum_reduce (0, obj%nvec2 ,ntraces    ,ntraces)
      call pcpsx_sum_reduce (0, obj%nvec2 ,nccoef     ,nccoef)
      call pcpsx_sum_reduce (0, obj%nvec2 ,scratch    ,scratch)
      call pcpsx_sum_reduce (0            ,sum_exists ,sum_exists)
      call pcpsx_sum_reduce (0            ,pwr_super  ,pwr_super)
      call pcpsx_sum_reduce (0            ,pwr_comp   ,pwr_comp )

      max_exists = pcpsx_max_reduce (0,max_exists)

      if (pcps_current_worker_num > 0) then
           deallocate (scratch)
           return
      end if

      do ivec = 1,obj%nvec2
           call pkutil_pack16              (scratch(:,ivec),obj%Ncorr,    &
                               stacked_correlations(:,ivec),obj%Ncorr16)
      end do

      deallocate (scratch)

      end subroutine cc3d_merge_results


!!--------------------------- process results -----------------------------!!
!!--------------------------- process results -----------------------------!!
!!--------------------------- process results -----------------------------!!

                              ! boss only.

      subroutine cc3d_process_results             &
                (obj,iorrr,iosss,iohhh,ioccc,     & ! in   
                 iter,                            & ! in   
                 stacked_correlations,            & ! in       (ncorr16,nvec2)
                 stat,                            & ! changed  (nvec)
                 rstat,                           & ! changed  (nvec)
                 wghts,                           & ! in       (nvec2)
                 nccoef,                          & ! in       (nvec2)
                 ntraces,                         & ! in       (nvec2)
                 max_exists,                      & ! in
                 sum_exists,                      & ! in
                 pwr_super_first,pwr_comp_first,  & ! changed
                 pwr_super,pwr_comp,              & ! in
                 timer4,timer5,                   & ! in
                 error)                             ! out

      type(cc3d_struct),intent(in) :: obj

      integer,intent(in)    :: IORRR,IOSSS,IOHHH,IOCCC
      integer,intent(in)    :: iter
      integer,intent(in)    :: stacked_correlations(:,:)
      real   ,intent(inout) :: stat   (:)       ! statics values
      real   ,intent(inout) :: rstat  (:)       ! residual statics values
      real   ,intent(in)    :: wghts  (:)       ! correlation coefficients
      integer,intent(in)    :: NCCOEF (:)       ! number of corr coefs
      integer,intent(in)    :: NTRACES(:)       ! number of traces
      integer,intent(in)    :: max_exists
      real   ,intent(in)    :: sum_exists
      real   ,intent(inout) :: pwr_super_first,pwr_comp_first
      real   ,intent(in)    :: pwr_super,pwr_comp
      integer,intent(in)    :: timer4,timer5
      logical,intent(out)   :: error

      real    :: RMSE,RMSEC                     ! local 
      real    :: RMSstat,ccaverage              ! local 
      real    :: CCOEF(obj%NVEC2)               ! correlation coefficients

!  PICK THE STACKED CORRELATIONS AND COMPARE TO AVERAGE OF RAW PICKS (boss only)

      error = .false.

      if (pcps_current_worker_num > 0) return
 
      call timer_start (timer4)

      call cc3d_pick_stacked_correlations      &
                 (obj,                         & ! in   
                  stacked_correlations,        & ! in       (ncorr16,nvec2)
                  rstat,                       & ! changed  (nvec)
                  WGHTS,                       & ! in       (nvec2)
                  NCCOEF,                      & ! in       (nvec2)
                  CCOEF,                       & ! out      (nvec2)
                  RMSE,RMSEC)                    ! out   

!  UPDATE STATICS (boss only)

      call cc3d_update_statics              &
                 (obj,                      & ! in   
                  rstat,                    & ! in       (nvec)
                  nccoef,                   & ! in       (nvec2)
                  ccoef,                    & ! in       (nvec2)
                  stat,                     & ! changed  (nvec)
                  RMSstat,ccaverage)          ! out   

!  PRINT ITERATION RESULTS (boss only)

      if (iter == 1) then
             PWR_SUPER_FIRST = PWR_SUPER
             PWR_COMP_FIRST  = PWR_COMP 
      end if

      call cc3d_print_iteration_results                      &
                         (obj,iter,sum_exists,max_exists,    & ! in
                          RMSE,RMSEC,PWR_SUPER,PWR_COMP,     & ! in
                          PWR_SUPER_FIRST,PWR_COMP_FIRST,    & ! in
                          RMSstat,CCAVERAGE)                   ! in

      call timer_stop (timer4)

!  SAVE STATIC FILES AND STACKED CORRELATIONS (boss only)

      call timer_start (timer5)

      call cc3d_save_files                &
             (obj,                        & ! in
              iorrr,iosss,                & ! in
              stat,                       & ! in   (nvec)
              ntraces,                    & ! in   (nvec)
              stacked_correlations,       & ! in   (ncorr16,nvec2)
              iter,                       & ! in
              error)                        ! out

      call timer_stop (timer5)

!  PRINT FINAL STATICS (boss only)

      if (iter < obj%num_iter) return

      call cc3d_print_statics                &
                 (obj,                       & ! in
                  iorrr,iosss,iohhh,ioccc,   & ! in
                  stat,                      & ! in   (nvec) 
                  rstat,                     & ! in   (nvec) 
                  ccoef,                     & ! in   (nvec2)
                  nccoef,                    & ! in   (nvec2)
                  ntraces)                     ! in   (nvec2)

      end subroutine cc3d_process_results


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cc3d_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

