!<CPS_v1 type="PROCESS"/>
!!------------------------------- fgd.f90 ---------------------------------!!
!!------------------------------- fgd.f90 ---------------------------------!!
!!------------------------------- fgd.f90 ---------------------------------!!


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
! Name       : FGD
! Category   : headers
! Written    : 1990-05-16   by: Tom Stoeckley
! Revised    : 2007-01-03   by: D. Glover
! Maturity   : production
! Purpose    : Apply header information from a .jd file built by CFG, print
!              stacking and ground position charts and dump trace headers.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! FGD attaches CPS headers to traces based on the .jd file built by CFG; it will
! also print stacking and ground position charts and dump trace headers.
!
!
!                               Headers
!
! When attaching headers, FGD sets all header words EXCEPT for the user-defined
! headers 48-55 (which are always left unchanged), and headers 2, 25, and 64
! (which are changed only if the trace is killed in CFG).  The meaning of the
! header words set by FGD is shown in the header word format list below.
!
! Header words 17-19 and 37 may have special meaning that depends on the value
! of FIXED_DIST.
!
! Scratch header words (30-32, 58-62) are reserved for use within CPS processes.
! If you want to retain any of these values permanently, you must use SETWORD
! just after FGD to move them into user-specified header words (48-55 and >64).
!
!
!                              Definitions
!
! SEQUENTIAL GROUND POSITION - This is the number of the LD card in CFG.  If
! FIXED_DIST is positive, this is set differently, as described below.
!
! INLINE DISTANCE - The distance to each flag, measured along the (possibly
! crooked) line from the first flag on the line.  If FIXED_DIST is not zero,
! this inline distance is altered as described below.
!
! NEAREST CMP quantity - The quantity corresponds to a point on the (possibly
! crooked) line closest to the true midpoint, which may not actually fall on
! the line.
!
! CENTER CMP quantity - The quantity corresponds to a point halfway between the
! source and receiver as measured along the (possibly crooked) line.  The
! location of this point is specified by the average of the source and receiver
! inline distances.
!
!
!                   Regularized Coordinate Description
!
! For 2D processing it is customary to use a regularized description of flag
! positions rather than their actual surveyed values.  To use a regularized
! description, set FIXED_DIST to a non-zero value; details are explained below.
!
! For 3D processing the actual surveyed values are normally used rather than a
! regularized description.  To use the actual surveyed values set FIXED_DIST to
! zero.
!
! FIXED_DIST is the regularized (uniform) inline distance between flag
! locations.  If FIXED_DIST is non-zero, an artificial regularized coordinate
! description is imposed on each line as follows:
!
!    (1) The actual inline distance to each flag is replaced by a regularized
!    value, such that the distance from flag to flag is made equal to the
!    absolute value of FIXED_DIST.
!
!    (2) If there is more than one line, the inline coordinates of all flags
!    on all lines are set to values that will match up the shotpoints of all
!    the lines, such that the minimum inline coordinate will be zero.  This
!    will give sensible results only if the shotpoint increment is the same
!    for all lines.
!
!    (3) If FIXED_DIST>0, header word 17 is replaced by the (now fixed)
!    "center" CMP inline distance, and header word 18 is set to 0.
!
!    (4) If FIXED_DIST>0, the "nearest" CMP shotpoint and elevation are
!    replaced by the "center" CMP shotpoint and elevation.
!
!    (5) If FIXED_DIST>0, the sequential ground position is set so that
!    matching shotpoints on different lines will have the same value.  This
!    will work correctly only if the LD card shotpoint increment is the same
!    for all lines, and matches the smallest ground position spacing for the
!    data set.
!
!
!                          Large inline Skids
!
! If a source or receiver has a large inline skid, it is moved automatically to
! a neighboring shotpoint in order to minimize this skid.  This allows the
! trace to fall into the middle of the correct CMP bin when a fixed inline
! distance is requested.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                 
!
!                          Charts for 3D Surveys
!
! Stacking charts and ground position charts find extensive use in 2D
! processing.  However in 3D work they quickly become too complicated to be
! interpreted if many lines are put on one chart.  Therefore for 3D processing
! it is recommended to use one line (or at most two or three) for each stacking
! or ground position chart.
!
!
!                           Ground Position Chart
!
! If a ground position chart is requested, it will be built using header words
! 46 and 47 (sequential ground positions).  If this is a 3-D data set, you will
! get all seismic lines on the same chart, each following the previous, since
! the sequential ground positions change from one line to another.
!
! Additional flexibility in producing ground position charts is available if
! you follow FGD with CHART instead of asking FGD to produce a chart.
!
!
!                              Stacking Chart
!
! If a STACKING CHART is requested, it will be built using header word 7 only,
! with the assumption that bin centers are integer values and bin spacing = 1.
!
! Normally stacking charts are not produced for 3D surveys.
! If you get a stacking chart for a 3-D data set, all of the lines will be
! superimposed on the chart, based on the values of header word 7 (the
! crossline coordinate is not used).  This may be what you want for swath lines
! (2 or 3 closely spaced parallel lines that will be stacked together), but not
! for full 3-D surveys.
!
! Additional flexibility in producing stacking charts is available if you
! follow FGD with CHART instead of asking FGD to produce a chart.
!
!
!                          Using CHART with FGD
!
! If you wish to build your own stacking or ground position charts instead of
! using the charts generated in FGD, you can do one of the following:
!
!     1.  Use OPT_HDR = SETUP in FGD, and follow process FGD with one or more
!         CHART processes (MODE = FGD).
!
!     2.  Use OPT_HDR = SYN in FGD, and follow process FGD with one or more
!         CHART processes (MODE = HEADERS).
!
! Method 2. is more efficient if you are using more than one CHART in a job,
! since trace headers need to be calculated only once.
!
!-------------------------------------------------------------------------------
!           LIST OF ALL HEADER WORDS AND HOW THEY ARE SET BY FGD
!       [items in square brackets are the values when FIXED_DIST > 0]
!
! header word and type  description and value set
! --------------------  -------------------------
!    1      normal      trace sequence number.
!    2      normal      head mute index (UNCHANGED).
!    3      normal      current gather number = header 9.
!    4      normal      trace number within current gather = header 10.
!    5      normal      fold of stack = 1.
!    6      normal      offset (in surveyed coordinates).
!   7,8     normal      CMP grid coordinates (from 17,18 using grid transform).
!    9      normal      original shot profile number.
!   10      normal      trace number within original shot profile.
!  11,12    normal      source surveyed easting and northing coordinates.
!   13      normal      source elevation.
!  14,15    normal      receiver surveyed easting and northing coordinates.
!   16      normal      receiver elevation.
!   17      normal      CMP surveyed easting [or center CMP inline distance].
!   18      normal      CMP surveyed northing [or 0].
!   19      normal      nearest [or center] CMP elevation.
!   20      normal      source hole depth.
!   21      normal      receiver hole depth = 0.
!  22,23    normal      source and receiver component numbers = 0.
!   24      normal      panel number = 0.
!   25      normal      largest absolute value (UNCHANGED, or 0 if killed).
!   26      normal      source line number.
!   27      normal      receiver line number.
!   28      normal      receiver shotpoint.
!   29      normal      source shotpoint.
!   30      scratch     nearest CMP inline distance.
!   31      scratch     center  CMP inline distance.
!   32      scratch     1 (last trace in group) or 2 (last trace) or 0.
!  33,34    normal      source grid coords (from 11,12 using grid transform).
!  35,36    normal      receiver grid coords (from 14,15 using grid transform).
!   37      normal      nearest [or center] CMP shotpoint.
!   38      normal      CMP line number (average of source and receiver).
!   39      normal      pre-NMO datum shift = 0.
!   40      normal      post-NMO datum shift = total datum shift.
! 41,42,43  normal      cumulative datum, refraction, residual static = 0.
!   44      normal      source uphole time.
!   45      normal      receiver uphole time = 0.
!  46,47    normal      source and receiver sequential ground positions.
!  48-55  user defined  (UNCHANGED).
!  56,57    normal      pre-NMO and post-NMO refraction shift = 0.
!  58,59    scratch     CMP surveyed easting and northing coordinates.
!  60,61    scratch     source and receiver inline distance.
!   62      scratch     trace count including skipped and missing traces.
!   63      normal      GVS modifier = 0.
!   64      normal      tail mute index (UNCHANGED).
! past 64 user defined  (UNCHANGED).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a single-trace process.
! Process requires traces to be input in shot profile sort order.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! If OPT_HDR = SETUP:
!   Traces are not processed.
!
! If OPT_HDR = ATTACH:
!   This process does not alter input traces but headers are attached.
!   Trace values are set to zero if the trace is killed by the ZT cards.
!
! If OPT_HDR = SYN:
!   This is a trace supplying process.
!   Traces are output ungathered.
!   Trace amplitude values will be random numbers between -1.0 and 1.0.
!   All traces will have identical amplitude values, except for killed or
!    reversed traces, whose values will be 0 or negated.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NWIH     number of words in trace header    used but not changed
! NDPT     number of words in trace header    used but not changed
! GRID     grid transformation structure      used but not changed
! NUMTR    Number of traces output together   set to   1   when OPT_HDR = SYN.
! GATHERED Whether traces are gathered        set to false when OPT_HDR = SYN.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! If OPT_HDR = SETUP:
!   No header words are changed since traces are not processed.
!
! If OPT_HDR = ATTACH:
!   Header words 1-64 are set according to the above table.
!   This means all header words are reset EXCEPT for the following:
!    (1) Mute header words 2 and 64 are always left unchanged.
!    (2) User defined headers 48-55 are always left unchanged.
!    (3) Headers beyond HDR_NOMINAL_SIZE (64) are always left unchanged.
!   Header 25 is set to 0 if killed by the ZT cards, or unchanged otherwise.
!
! If OPT_HDR = SYN:
!   Header words 1-64 are set according to the above table.
!   In addition, the following header words are set:
!    (1) Mute header words 2 and 64 are set to 1 and NDPT respectively.
!    (2) User defined headers 48-55 are set to zero.
!    (3) Headers beyond HDR_NOMINAL_SIZE (64) are set to zero.
!   Header 25 is set to 0 if killed, or to the LAV of the trace otherwise.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author    Description
!     ----        ------    -----------
! 50. 2007-01-03  D. Glover Added NULLIFY statements for Intel compiler.
! 49. 2006-06-13 Stoeckley  Always read JD file on backend even if the filename
!                            has not changed; needed for SeisSpace.
!048. 2006-01-10  B. Menger   Removed Unused Variables.
! 47. 2001-12-10 Stoeckley  Add additional error printouts; fix longstanding
!                            (but apparently rarely encountered) receiver
!                            pattern card bug in GEOMDATA.
! 46. 2001-10-23 Stoeckley  Omit scanning the JD file in CFE to save time.
! 45. 2001-10-18 Stoeckley  Make VE, DATUM, and FIXED_DIST fields always
!                            sensitive so they can be changed from values on
!                            the JD file; add button and status for pathname.
! 44. 2001-02-13 Stoeckley  Change wrapup flag.
! 43. 2000-11-27 Stoeckley  Add ability to read history cards from JD file.
! 42. 2000-10-13 Stoeckley  Add missing context-sensitive help.
! 41. 2000-10-06 Stoeckley  Add missing required documentation sections.
! 40. 2000-08-07 Stoeckley  Fix bug in fudge factor for the number of CMPs to
!                            pass to CHART (parameter LEN_LAST) to reduce the
!                            possibility of truncating the stacking chart.
! 39. 2000-05-17 Stoeckley  Fix bug where no traces were output for mode SYN.
! 38. 2000-05-11 Stoeckley  Implement printing options for tables.
! 37. 2000-05-02 Stoeckley  Fix bug which did not allow getting charts and
!                            header dumps at setup.
! 36. 2000-04-28 Stoeckley  Fix error in getting SURVEY_UNITS.
! 35. 2000-04-07 Stoeckley  Change ' to ` in GUI definition section.
! 34. 2000-04-06 Stoeckley  Fix problems in GUI definition section.
! 33. 2000-03-22 Stoeckley  Converted from old system.
! 32. 1998-12-08 Goodger    Begin using the f90 compiler.
! 31. 1997-01-09 Stoeckley  Change format for pattern number on RP cards.
! 30. 1996-04-24 Stoeckley  Add subroutine FGDCHECK which calls several
!                           new functions residing on new C-language
!                           file FGD_CROU to read values from optional
!                           cards on JD file, and to abort if values
!                           existing on the file do not match the globals
!                           (grid transform), or VE, REF, and FIXED_DIST.
! 29. 1996-04-04 Stoeckley  Add subroutine FGDOPT to read (and ignore)
!                           optional cards from JD file.
! 28. 1996-01-05 Stoeckley  Modified algorithm in FGDLDF for calculating
!                           skidded coordinates.
! 27. 1995-10-31 Stoeckley  Changed formats for reading RP and ZT cards.
! 26. 1993-06-16 Troutt     Added ENTRY FGDNGRP in FGDPP as a convenience
!                           routine for other processes to call and get
!                           the number of groups defined by the PP cards.
! 25. 1993-05-18 K. Goodger Documentation change on DEV parameter.
! 24. 1993-04-28 Troutt     Changed calculation of LDS and LDR in FGDPP.
!                           This hopefully fixes the problem with missing
!                           traces on an internal stacking chart due to
!                           decrementing receivers on RP cards.  In these
!                           cases, X1 was too small for CHART.
! 23. 1993-01.29 Troutt     Increase field size for STRT and NSKP from 6
!                           to 8 digits so that user can "jump around" in
!                           large 3D survey (Format 3000 in FGDC1 allows
!                           passing these large value to TDMP).  The CFE
!                           was changed for these 2 fields, as well as for
!                           NLAB (no changes needed here for NLAB).
!                           Also increase sequential trace number field in
!                           internal TDMP's from 6 to 7 columns (decreased
!                           header 32 from 3 to 2).
! 22. 1993-01-07 Troutt     Add shotpoint number to error message in entry
!                           FGDINDEX.
! 21. 1992-03-23 Troutt     Implement tail mute header (HW64).  Handle
!                           like head mute (HW2) -- pass the input value
!                           except for synthetic generation or killed
!                           traces.
! 20. 1991-11-11 K. Goodger When two Shot points are found to be coinci-
!                           dent, print out the shot point, line, and X,Y
!                           locations.
! 19. 1991-10-28 K. Goodger Put DEV parameter in common block to be passed
!                           to CHART program. Needed when CHART is run
!                           as an independent process.
! 18. 1991-05-21 K. Goodger Change cray flag check from eq 1 to ne 0 for
!                           change 16.
! 17. 1991-04-16 K. Goodger Add OPS plotter option.
! 16. 1991-02-26 Goodger    Change file name from zero fill to blank fill
!                           for UNICOS.
! 15. 1991-02-25 Goodger    Add parameter DEV to pass to CHART routine.
! 14. 1991-02-01 Stoeckley  Improve method of calculating first CMP
!                           location for stacking chart, for marine data.
! 13. 1991-01-31 Stoeckley  Change method of calculating inline distance
!                           and ground position; add receiver skids to
!                           POST static calculation.
! 12. 1990-11-30 Stoeckley  Modify the method of handling SKIP and DUP
!                           irregularities described on RP cards.
! 11. 1990-11-19 Stoeckley  Fix bug with NLAB parameter; allow HOLD=999
!                           to hold source skid for infinite number of
!                           sources; add MISSING code to ZT cards.
! 10. 1990-11-09 Stoeckley  Further changes regarding excessive inline
!                           skids, and CMP bin centers and bin spacings.
!                           Also changed the definition of inline distance
!                           for 3-D data.
!  9. 1990-11-06 Stoeckley  Add parameter SPACING.  Also adjust source and
!                           receiver shotpoint for excessive inline skid.
!                           Also add subroutine FGDBSMT to change the way
!                           CMP bin centers and bin spacings are
!                           determined for the stacking chart.
!  8. 1990-10-12 Stoeckley  Allow stacking chart to plot negative grid#'s.
!  7. 1990-08-24 Stoeckley  Add calls to PUTG and GETG (needed when
!                           program acts as synthetic trace generator).
!                           Change to always print a few cards even when
!                           not requested.  Add some print messages.
!  6. 1990-08-24 Stoeckley  Change interpolation method for XSD,YSD,ELSD
!                           on LD cards.
!  5. 1990-07-09 Stoeckley  Change to MODE=HEADERS2 for calls to CHART.
!  4. 1990-07-06 Stoeckley  Add OPTION=8, add more documentation, get
!                           around a bug caused by a compiler error,
!                           make some changes to some header words,
!                           and modify the calls to CHART.
!  3. 1990-06-27 Stoeckley  Add flag for last trace in group.
!  2. 1990-06-26 Stoeckley  Add parameter BCHART, remove parameter HEAD,
!                           and change parameter DIST to FIXED_DIST.  Also
!                           modify the way FIXED_DIST works, and put charts
!                           and header dumps into subroutine FGDC1.
!                           Also add fourth ZT card option.  Also fix
!                           bug regarding use of fixed inline distance.
!  1. 1990-05-16 Stoeckley  Initial version.
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false*    whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.       
! NSTORE          >0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false*    whether this process is setup-only.    
!
! * NEED_LABEL is reported as true when OPT_HDR = SYN.
! * SETUP_ONLY is reported as true when OPT_HDR = SETUP.
!
! When OPT_HDR = ATTACH:
!
!    Upon input, NTR must have one of these values:
!      NTR >= 1              means to process the input traces.
!      NTR == NO_MORE_TRACES means no more input traces.      
!
!    Upon output, NTR will have one of these values:
!      NTR >= 1              if this process is outputting traces.
!      NTR == NO_MORE_TRACES if there are no more traces to output.
!      NTR == FATAL_ERROR    if this process has a fatal error.
!
! When OPT_HDR = SYN:
!
!    Upon input, NTR must have this value:
!      NTR == NEED_TRACES    means someone else needs more traces.
!
!    Upon output, NTR will have one of these values:
!      NTR == 1              if this process is outputting traces.
!      NTR == NO_MORE_TRACES if there are no more traces to output.
!      NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!               GET A POINTER TO AN INSTANCE OF FGD
!
!                                           o
!                  CALL FGD_NUM_INSTANCES (num)       
!
!                                           i     o
!                  CALL FGD_FIND_INSTANCE (indx, obj)
!
!
! integer NUM  = Number of instances of FGD in the program or processing job.
! integer INDX = Desired instance of FGD wanted (from 1 through NUM).
!
! type(fgd_struct),pointer OBJ = Pointer to the desired instance of FGD.
!
!
! FGD_NUM_INSTANCES:
!  (1) returns the number of instances of FGD in the program or processing job.
!
! FGD_FIND_INSTANCE:
!  (1) returns a pointer to the desired instance of FGD.
!  (2) calls PC_ERROR and returns a nullified pointer if INDX is out of
!       range or NUM is zero.
!
!
! These routines and the code behind them can be used as templates for
! similar routines in any other process module.  The relevant code in this
! process module is identified as LINKED LIST variables and subroutines.
!
!-------------------------------------------------------------------------------
!               CALL FROM OTHER PROCESSES TO GET TRACE HEADERS
!
! There are two methods for getting trace headers from FGD.  Both of these
! methods call FGD_INITIALIZE_HEADERS (for initialization) followed by
! repeated calls to FGD_NEXT_HEADER (to get the indiviual trace headers).
! The calling routine might be another process module in the job, or any
! other routine in any program.
!
!            +++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Method (1) uses the subroutines which have the arguments OBJ and ITERATOR.
! This method is more general because it can access any instance of FGD and
! the headers can be obtained at any time during setup or trace processing,
! even if other processes are also getting trace headers from the same instance
! of FGD at the same time.
!
! The calling routine must have access to the desired instance of the FGD
! object which it wishes to use to create trace headers.  If the calling
! routine did not create the desired instance of FGD, it can obtain one
! by calling FGD_NUM_INSTANCES and FGD_FIND_INSTANCE.
!
! The calling routine must also declare a GEOMDATA_ITERATOR which is
! defined in the GEOMDATA_MODULE (note that this is a structure, not a
! pointer to a structure).
!
! To use method (1):
!
!               use fgd_module
!               use geomdata_module
!
!               type(geomdata_iterator) :: iterator
!
!                                             i      o
!               CALL FGD_INITIALIZE_HEADERS (obj, iterator)
!
!                                      i      b      o    o    o
!               CALL FGD_NEXT_HEADER (obj, iterator, hd, err, msg)
!
!            +++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Method (2) uses the subroutines which LACK the arguments OBJ and ITERATOR.
! This method is simpler, but it also has the following limitations:  (a) It
! can access only that instance of FGD which is the last instance created by
! the time that the trace headers are requested.  (b) The headers must all be
! obtained at the same time so that no additional instances of FGD become
! created part way through, and no other processes can try to obtain headers
! simultaneously.  Normally, the trace headers should be obtained during
! setup (from the update routine of the calling process) to insure that the
! instance of FGD used is the last instance preceding the calling process
! in the job.
!
! To use method (2):
!
!               use fgd_module
!
!               CALL FGD_INITIALIZE_HEADERS
!
!                                     o    o    o
!               CALL FGD_NEXT_HEADER (hd, err, msg)
!
!            +++++++++++++++++++++++++++++++++++++++++++++++++++
!
! type(fgd_struct)              obj = desired instance of FGD.
! type(geomdata_iterator)  iterator = a trace header iterator.
! double       hd(HDR_NOMINAL_SIZE) = trace header array.
! integer                       err = success or error flag.
! character(len=*)              msg = message describing success or error.
!
!
! ERR = GEOMDATA_OK       if the next trace header array is returned.
! ERR = GEOMDATA_FINISHED if there are no more trace headers.
! ERR = GEOMDATA_ERROR    if an error occurs.
!
!
! FGD_INITIALIZE_HEADERS:
!  (a) initializes an instance of a trace header iterator so that the first
!       call to FGD_NEXT_HEADER will return the first trace header.
!
! FGD_NEXT_HEADER:
!  (a) returns the trace header for the next trace each time it is called.
!  (b) sets user defined headers 48-55 to zero.
!  (c) sets mute headers 2 and 64 to one.
!  (d) sets the LAV header 25 to one of these three values:
!          1 (live trace)
!          0 (trace killed by ZT cards)
!         -1 (trace with polarity reversed by ZT cards)
!  (e) returns the appropriate error flag and message.
!
! When using method (1), the trace header iterator does not modify in any
! way the instance of FGD which is pointed to by OBJ, and does not modify
! the field geometry data provided to it by FGD.  Therefore, there is no
! restriction on when or how many trace header iterators can be operating
! on data from a single instance of FGD at the same time.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
! This process uses three data structures:
!
! type(fgd_struct),pointer :: OBJ
!
!   This is the normal structure analogous to the data structures of
!   other process modules.  This structure owns an instance of each of
!   the data structures GEOMDATA_STRUCT and GEOMDATA_ITERATOR.
!
! type(geomdata_struct),pointer :: GEOMDATA
!
!   This structure contains the field geometry data read from a file,
!   but has no knowledge of FGD.  This data structure resides in the
!   GEOMDATA primitive.
!
! type(geomdata_iterator) :: ITERATOR
!
!   This structure is a trace header iterator which is responsible for
!   actually creating trace headers.  This structure resides in the
!   GEOMDATA primitive.  It has no knowledge of the FGD process.  An
!   instance of this data structure is owned and initialized by FGD for
!   its own use in generating trace headers.  Instances of this data
!   structure can also be owned by other processes, and initialized by
!   FGD, to create trace headers.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS FGD Process/NC=80>
!                 Create Trace Headers from Field Geometry Data
!
! OPT_HDR~~~~=`CCCCCCCC [/L](option for attaching headers to traces)
! TR_SKIP~~~~=`IIIIIIII [/L](number of trace headers to skip)
! VE~~~~~~~~~=`FFFFFFFF [/L](elevation velocity)    [VE_ON_FILE]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! DATUM~~~~~~=`FFFFFFFF [/L](reference elevation)   [DATUM_ON_FILE]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! FIXED_DIST =`FFFFFFFF [/L](fixed inline distance) [FIXED_DIST_ON_FILE]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! Select PATHNAME[PATHNAME]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                  [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! Tables:  OPT_TABLES~~~=`CCCCCCCC [/L](option for printing tables)
!
! Charts:  OPT_CHART~~~~=`CCCCCCCC [/L](types of charts to print)
!          SPACING~~~~~~=`CCCCCCCC [/L](spacing between shot profiles on charts)
!          DEVICE~~~~~~~=`CCCCCCCC [/L](device for printing charts)
!          SURVEY_UNITS =`XXXXXXXX [/L](units for chart distances)
!
! Header dump:  SKIP_INIT =`IIIIIIII    NUM_DUMP =`IIIIII
!               NUM_SKIP~~=`IIIIIIII    TOT_DUMP =`IIIIII
!
!<PARMS PATHNAME[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="SURVEY_UNITS">
!<Tip> Units (feet or meters) used in the surveyed coordinate system. </Tip>
!</Help>
!
!
!<Help KEYWORD="VE_ON_FILE">
!<Tip> Value of VE on the specified field geometry file. </Tip>
!</Help>
!
!
!<Help KEYWORD="DATUM_ON_FILE">
!<Tip> Value of DATUM on the specified field geometry file. </Tip>
!</Help>
!
!
!<Help KEYWORD="FIXED_DIST_ON_FILE">
!<Tip> Value of FIXED_DIST on the specified field geometry file. </Tip>
!</Help>
!
!
!<Help KEYWORD="VE">
!<Tip> Velocity for datum correction. </Tip>
! Default = 8000
! Allowed = real > 0.0
! VE is the velocity used in the datum correction which may be either an
! elevation correction or an up-hole correction.
!</Help>
!
!
!<Help KEYWORD="DATUM">
!<Tip> DATUM elevation (reference elevation for datum correction). </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for the .jd file written by CFG. </Tip>
! Default = from job_data
! Allowed = char
!</Help>
!
!
!<Help KEYWORD="TR_SKIP">
!<Tip> Number of trace headers to skip from beginning of dataset. </Tip>
! Default = 0
! Allowed = int >= 0
! FGD will assume TR_SKIP traces have been deleted from the beginning of the
! dataset and will begin writing headers with the header appropriate for the
! TR_SKIP + 1st trace.
!</Help>
!
!
!<Help KEYWORD="FIXED_DIST">
!<Tip> Uniform inline distance to establish between flag locations. </Tip>
! Default = 0.0
! Allowed = real
! IF FIXED_DIST is non-zero, then the inline distance between flag locations is
! regularized to the absolute value of FIXED_DIST.  Normally FIXED_DIST is set
! to the nominal flag interval for the survey.
!
! FIXED_DIST also acts as a flag for other regularization as detailed in
! the General Description.
!</Help>
!
!
!<Help KEYWORD="OPT_HDR">
!<Tip> Option for attaching headers to traces. </Tip>
! Default = ATTACH
! Allowed = ATTACH  (Attach headers.)
! Allowed = SETUP   (Setup only - don't attach headers.)
! Allowed = SYN     (Generate synthetic traces only.)
!</Help>
!
!
!<Help KEYWORD="OPT_CHART">
!<Tip> Option for printing stacking or ground position chart. </Tip>
! Default = STACK
! Allowed = STACK  (Print stacking chart.)
! Allowed = GP     (Print ground position chart.)
! Allowed = BOTH   (Print stacking and ground position charts.)
! Allowed = NONE   (Print no chart.)
!</Help>
!
!
!<Help KEYWORD="SPACING">
!<Tip> Use single or double spacing between shot profiles on charts. </Tip>
! Default = SINGLE
! Allowed = SINGLE (Less wrap-around, harder to read.)
! Allowed = DOUBLE (More wrap-around, easier to read.)
!</Help>
!
!
!<Help KEYWORD="DEVICE">
!<Tip> Device on which to put the charts (printer or plotter). </Tip>
! Default = PRINTER
! Allowed = PRINTER or OPS or CONP
! Currently only the PRINTER choice is available.
!</Help>
!
!
!<Help KEYWORD="OPT_TABLES">
!<Tip> Option for printing tables. </Tip>
! Default = NONE
! Allowed = NONE   (Print only a few lines from each table.)
! Allowed = LD     (Print LD table.)
! Allowed = RPZ    (Print RP, PP, ZT tables.)
! Allowed = LRPZ   (Print LD, RP, PP, ZT tables.)
!</Help>
!
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of traces to skip initially for the header dump. </Tip>
! Default = 0
! Allowed = int>=0
! First header printed will be for trace number SKIP_INIT + 1.
!</Help>
!
!
!<Help KEYWORD="NUM_DUMP">
!<Tip> Number of trace headers to print at a time for the header dump. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!
!<Help KEYWORD="NUM_SKIP">
!<Tip> Number of trace headers to skip at a time for the header dump. </Tip>
! Default = 0
! Allowed = int>=0
!</Help>
!
!
!<Help KEYWORD="TOT_DUMP">
!<Tip> Total number of trace headers to print. </Tip>
! Default = 100
! Allowed = int>0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module fgd_module
      use pc_module
      use named_constants_module
      use grid_module
      use pathcheck_module
      use pathchoose_module
      use geomio_module
      use geomdata_module
      use chart_module
      use tdmp_module
      use mth_module
      use lav_module
      implicit none
      private
      public :: fgd_create
      public :: fgd_initialize
      public :: fgd_update
      public :: fgd_delete
!<execute_only>
      public :: fgd
      public :: fgd_wrapup
      public :: fgd_initialize_headers
      public :: fgd_next_header
!</execute_only>
      public :: fgd_num_instances
      public :: fgd_find_instance

      character(len=100),public,save :: FGD_IDENT = &
       '$Id: fgd.f90,v 1.50 2007/01/03 14:01:39 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: fgd_struct              
 
        private
        logical                        :: skip_wrapup  ! wrapup flag.

        type(fgd_struct),pointer       :: prev_obj     ! for linked list.
        type(fgd_struct),pointer       :: next_obj     ! for linked list.

        character(len=8)               :: survey_units ! project data
        integer                        :: nwih         ! globals.  
        integer                        :: ndpt         ! globals.  
        type(grid_struct)              :: grid         ! globals.  

        real                           :: ve           ! process parameters
        real                           :: datum        ! process parameters
        character(len=FILENAME_LENGTH) :: pathname     ! process parameters
        integer                        :: tr_skip      ! process parameters
        real                           :: fixed_dist   ! process parameters
        character(len=8)               :: opt_hdr      ! process parameters
        character(len=8)               :: opt_chart    ! process parameters
        character(len=8)               :: spacing      ! process parameters
        character(len=8)               :: device       ! process parameters
        character(len=8)               :: opt_tables   ! process parameters
        integer                        :: skip_init    ! process parameters
        integer                        :: num_dump     ! process parameters
        integer                        :: num_skip     ! process parameters
        integer                        :: tot_dump     ! process parameters

        real                           :: ve_on_file  
        real                           :: datum_on_file
        real                           :: fixed_dist_on_file

        real                   ,pointer :: synthetic(:)
        type(geomdata_struct)  ,pointer :: geomdata
        type(geomdata_iterator)         :: iterator
        type(pathchoose_struct),pointer :: pathchoose
        double precision                :: whoops(HDR_NOMINAL_SIZE)

        type(chart_struct)   ,pointer  :: chart1       ! charts and dumps
        type(chart_struct)   ,pointer  :: chart2       ! charts and dumps
        type(tdmp_struct)    ,pointer  :: tdmp1        ! charts and dumps
        type(tdmp_struct)    ,pointer  :: tdmp2        ! charts and dumps

        logical                        :: want_chart1  ! charts and dumps
        logical                        :: want_chart2  ! charts and dumps
        logical                        :: want_tdmp1   ! charts and dumps
        logical                        :: want_tdmp2   ! charts and dumps


      end type fgd_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface fgd_initialize_headers
           module procedure fgd_initialize_headers1
           module procedure fgd_initialize_headers2
      end interface

      interface fgd_next_header
           module procedure fgd_next_header1
           module procedure fgd_next_header2
      end interface


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                 ,private,save :: num_objs = 0 ! for linked list.
      type(fgd_struct),pointer,private,save :: first_obj    ! for linked list.
      type(fgd_struct),pointer,private,save :: last_obj     ! for linked list.

      type(geomdata_iterator),private,save :: header_iterator

      integer,private,parameter :: NDPT_CHARTS = 2

      type(fgd_struct),pointer,save :: object     ! needed for traps.

      integer,parameter     :: opt_hdr_nopt    = 3
      integer,parameter     :: opt_chart_nopt  = 4
      integer,parameter     :: spacing_nopt    = 2
      integer,parameter     :: device_nopt     = 3
      integer,parameter     :: opt_tables_nopt = 4

      character(len=8),save :: opt_hdr_opt    (opt_hdr_nopt)
      character(len=8),save :: opt_chart_opt  (opt_chart_nopt)
      character(len=8),save :: spacing_opt    (spacing_nopt)
      character(len=8),save :: device_opt     (device_nopt)
      character(len=8),save :: opt_tables_opt (opt_tables_nopt)

      data opt_hdr_opt    /'ATTACH','SETUP','SYN'/
      data opt_chart_opt  /'STACK','GP','BOTH','NONE'/
      data spacing_opt    /'SINGLE','DOUBLE'/
      data device_opt     /'PRINTER','OPS','CONP'/
      data opt_tables_opt /'NONE','LD','RPZ','LRPZ'/


      contains


!!---------------------- linked list routines -----------------------------!!
!!---------------------- linked list routines -----------------------------!!
!!---------------------- linked list routines -----------------------------!!


      subroutine fgd_private_add_to_list (obj)
      implicit none
      type(fgd_struct),pointer :: obj                ! arguments

      nullify(obj%prev_obj)
      nullify(obj%next_obj)

      if (num_objs == 0) then
           first_obj => obj
           last_obj  => obj
      else
           obj%prev_obj      => last_obj
           last_obj%next_obj => obj
           last_obj          => obj
      end if
      num_objs = num_objs + 1
      return
      end subroutine fgd_private_add_to_list



      subroutine fgd_private_remove_from_list (obj)
      implicit none
      type(fgd_struct),pointer :: obj                ! arguments


      if (num_objs == 0) return                 ! this should not happen
      if (associated(obj%prev_obj)) then
           obj%prev_obj%next_obj => obj%next_obj
      else
           first_obj => obj%next_obj
      end if
      if (associated(obj%next_obj)) then
           obj%next_obj%prev_obj => obj%prev_obj
      else
           last_obj => obj%prev_obj
      end if
      num_objs = num_objs - 1
      return
      end subroutine fgd_private_remove_from_list



      subroutine fgd_num_instances (num)
      implicit none
      integer,intent(out) :: num                  ! arguments

      num = num_objs
      return
      end subroutine fgd_num_instances



      subroutine fgd_find_instance (indx, obj)
      implicit none
      integer,intent(in)       :: indx            ! arguments
      type(fgd_struct),pointer :: obj             ! arguments
      integer                  :: i               ! local

      if (indx <= 0 .or. indx > num_objs) then
           nullify (obj)
           return
      end if
      obj => first_obj
      do i = 2,indx
           obj => obj%next_obj
      end do
      return
      end subroutine fgd_find_instance


!!----------------------- initialize headers --------------------------------!!
!!----------------------- initialize headers --------------------------------!!
!!----------------------- initialize headers --------------------------------!!

!<execute_only>

      subroutine fgd_initialize_headers1
      implicit none

      if (num_objs == 0) return
      call fgd_initialize_headers (last_obj, header_iterator)
      return
      end subroutine fgd_initialize_headers1



      subroutine fgd_initialize_headers2 (obj, iterator)
      implicit none
      type(fgd_struct)       ,intent(in)  :: obj              ! arguments
      type(geomdata_iterator),intent(out) :: iterator         ! arguments

      call geomdata_initialize_headers (obj%geomdata, iterator, obj%tr_skip)
      call pc_print &
        ('Calling FGD from another process to generate trace headers:')
      call pc_print &
        (' preparing to generate trace headers starting with trace number', &
        obj%tr_skip + 1)
      return
      end subroutine fgd_initialize_headers2

!</execute_only>

!!------------------------------- next header -------------------------------!!
!!------------------------------- next header -------------------------------!!
!!------------------------------- next header -------------------------------!!

!<execute_only>

      subroutine fgd_next_header1 (hd, err, msg)
      implicit none
      double precision       ,intent(out)   :: hd(:)            ! arguments
      integer                ,intent(out)   :: err              ! arguments
      character(len=*)       ,intent(out)   :: msg              ! arguments

      if (num_objs == 0) then
           err = GEOMDATA_ERROR
           msg = 'Calling FGD when there are no instances of FGD.'
           call pc_error (msg)
           return
      end if
      call fgd_next_header (last_obj, header_iterator, hd, err, msg)
      return
      end subroutine fgd_next_header1



      subroutine fgd_next_header2 (obj, iterator, hd, err, msg)
      implicit none
      type(fgd_struct)       ,intent(in)    :: obj              ! arguments
      type(geomdata_iterator),intent(inout) :: iterator         ! arguments
      double precision       ,intent(out)   :: hd(:)            ! arguments
      integer                ,intent(out)   :: err              ! arguments
      character(len=*)       ,intent(out)   :: msg              ! arguments

      call geomdata_next_header (obj%geomdata, iterator, hd, err, msg)
      if (err == GEOMDATA_ERROR) then
           call pc_print &
             ('Calling FGD from another process to generate trace headers:')
           call pc_error (msg)
      else if (err == GEOMDATA_FINISHED) then
           call pc_print &
             ('Calling FGD from another process to generate trace headers:')
           call pc_print ('finished generating trace headers')
      end if
      return
      end subroutine fgd_next_header2

!</execute_only>

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine fgd_create (obj)
      implicit none
      type(fgd_struct),pointer :: obj       ! arguments

      allocate (obj)

      call fgd_private_add_to_list (obj)

      nullify  (obj%synthetic)
      nullify  (obj%geomdata)
      nullify  (obj%chart1)
      nullify  (obj%chart2)
      nullify  (obj%tdmp1)
      nullify  (obj%tdmp2)
      nullify  (obj%prev_obj) ! jpa
      nullify  (obj%next_obj) ! jpa
      nullify  (obj%pathchoose) ! jpa

      call pathchoose_create (obj%pathchoose, 'pathname', 'jd')
      call geomdata_create   (obj%geomdata)

      call fgd_initialize (obj)
      return
      end subroutine fgd_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine fgd_delete (obj)
      implicit none
      type(fgd_struct),pointer :: obj       ! arguments

!<execute_only>
      call fgd_wrapup (obj)
!</execute_only>

      if (associated(obj%synthetic)) deallocate           (obj%synthetic)
      if (associated(obj%geomdata )) call geomdata_delete (obj%geomdata)
      if (associated(obj%chart1   )) call chart_delete    (obj%chart1  )
      if (associated(obj%chart2   )) call chart_delete    (obj%chart2  )
      if (associated(obj%tdmp1    )) call tdmp_delete     (obj%tdmp1   )
      if (associated(obj%tdmp2    )) call tdmp_delete     (obj%tdmp2   )

      call fgd_private_remove_from_list (obj)

      deallocate(obj)
      return
      end subroutine fgd_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine fgd_initialize (obj)
      implicit none
      type(fgd_struct),intent(inout) :: obj       ! arguments

      call geomdata_clear (obj%geomdata)

      obj%survey_units = "METERS"
      obj%nwih         = -1
      obj%ndpt         = -1
      call grid_initialize (obj%grid)

      obj%ve           = 8000.0 
      obj%datum        = 0.0 
      obj%fixed_dist   = 0.0 
      obj%pathname     = PATHCHECK_EMPTY 
      obj%tr_skip      = 0
      obj%opt_hdr      = 'ATTACH' 
      obj%opt_chart    = 'STACK' 
      obj%spacing      = 'SINGLE' 
      obj%device       = 'PRINTER' 
      obj%opt_tables   = 'NONE' 
      obj%skip_init    = 0
      obj%num_dump     = 1
      obj%num_skip     = 0
      obj%tot_dump     = 100

      obj%ve_on_file         = FNIL   
      obj%datum_on_file      = FNIL  
      obj%fixed_dist_on_file = FNIL  

      call fgd_update (obj)
      return
      end subroutine fgd_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine fgd_update (obj)
      implicit none
      type(fgd_struct),intent(inout),target :: obj               ! arguments
      integer                        :: nscratch,nstore,j,err    ! local
      logical                        :: error                    ! local
      character(len=80)              :: msg                      ! local
      character(len=FILENAME_LENGTH) :: pathname_keep            ! local
      double precision               :: hhh(HDR_NOMINAL_SIZE)    ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      if (pathchoose_update(obj%pathchoose, obj%pathname)) return

      pathname_keep = obj%pathname

      call pc_get_pdata  ('survey_units', obj%survey_units)
      call pc_get_global ('nwih'        , obj%nwih)
      call pc_get_global ('ndpt'        , obj%ndpt)
      call pc_get_global ('grid'        , obj%grid)

      call pc_get ('ve'        , obj%ve        )
      call pc_get ('datum'     , obj%datum     )
      call pc_get ('fixed_dist', obj%fixed_dist)
      call pc_get ('pathname'  , obj%pathname  )
      call pc_get ('tr_skip'   , obj%tr_skip   )
      call pc_get ('opt_hdr'   , obj%opt_hdr   )
      call pc_get ('opt_chart' , obj%opt_chart )
      call pc_get ('spacing'   , obj%spacing   )
      call pc_get ('device'    , obj%device    )
      call pc_get ('opt_tables', obj%opt_tables)
      call pc_get ('skip_init' , obj%skip_init )
      call pc_get ('num_dump'  , obj%num_dump  )
      call pc_get ('num_skip'  , obj%num_skip  )
      call pc_get ('tot_dump'  , obj%tot_dump  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%nwih < HDR_NOMINAL_SIZE) then
           call pc_error  &
                 ('NWIH not found in parameter cache or <',HDR_NOMINAL_SIZE)
           obj%nwih = HDR_NOMINAL_SIZE
      end if

      if (obj%ndpt < 1) then
           call pc_error ('NDPT not found in parameter cache or < 1')
           obj%ndpt = 1
      end if

      if (grid_get_xgrid_width(obj%grid) == 1.0) then
           call pc_error ('GRID TRANSFORM apparently not provided or not set')
      end if

 call fgd_validate ('opt_hdr'   ,obj%opt_hdr   ,opt_hdr_opt   ,opt_hdr_nopt)
 call fgd_validate ('opt_chart' ,obj%opt_chart ,opt_chart_opt ,opt_chart_nopt)
 call fgd_validate ('spacing'   ,obj%spacing   ,spacing_opt   ,spacing_nopt)
 call fgd_validate ('device'    ,obj%device    ,device_opt    ,device_nopt)
 call fgd_validate ('opt_tables',obj%opt_tables,opt_tables_opt,opt_tables_nopt)

      obj%tr_skip   = max(obj%tr_skip  , 0)
      obj%skip_init = max(obj%skip_init, 0)
      obj%num_dump  = max(obj%num_dump , 1)
      obj%num_skip  = max(obj%num_skip , 0)
      obj%tot_dump  = max(obj%tot_dump , 0)

      call pathcheck ('pathname', obj%pathname, 'jd', required=.true., &
                          show=PATHCHECK_INFO_INPUT)

      if (obj%pathname /= pathname_keep .or. &
              pc_get_update_state() == PC_BACKEND) call fgd_read_file (obj)

      if (obj%ve <= 0.0) call pc_error ('VE must be > 0')

      if (obj%opt_hdr == 'SYN') then
           call pc_print ('SYNTHETIC TRACES WILL BE GENERATED')
      else if (obj%opt_hdr == 'ATTACH') then
           call pc_print ('HEADERS WILL BE ATTACHED IN THIS JOB')
      else
           call pc_print ('HEADERS ARE NOT BEING ATTACHED IN THIS JOB')
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


      call fgd_chart1 (obj,nstore,nscratch)

      nstore = nstore + geomdata_get_nstore (obj%geomdata)


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('opt_hdr'   , opt_hdr_opt    ,opt_hdr_nopt)
      call pc_put_options_field ('opt_chart' , opt_chart_opt  ,opt_chart_nopt)
      call pc_put_options_field ('spacing'   , spacing_opt    ,spacing_nopt)
      call pc_put_options_field ('device'    , device_opt     ,device_nopt)
      call pc_put_options_field ('opt_tables', opt_tables_opt ,opt_tables_nopt)

      if (obj%opt_hdr == 'SYN') then
           call pc_put_global ('numtr'   , 1)
           call pc_put_global ('gathered', .false.)
      end if

      call pc_put ('ve'        , obj%ve        , 9)
      call pc_put ('datum'     , obj%datum     , 9)
      call pc_put ('fixed_dist', obj%fixed_dist, 9)
      call pc_put ('pathname'  , obj%pathname     )
      call pc_put ('tr_skip'   , obj%tr_skip      )
      call pc_put ('opt_hdr'   , obj%opt_hdr      )
      call pc_put ('opt_chart' , obj%opt_chart    )
      call pc_put ('spacing'   , obj%spacing      )
      call pc_put ('device'    , obj%device       )
      call pc_put ('opt_tables', obj%opt_tables   )
      call pc_put ('skip_init' , obj%skip_init    )
      call pc_put ('num_dump'  , obj%num_dump     )
      call pc_put ('num_skip'  , obj%num_skip     )
      call pc_put ('tot_dump'  , obj%tot_dump     )

      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore) 
      call pc_put_control ('need_label'  , obj%opt_hdr == 'SYN')
      call pc_put_control ('setup_only'  , obj%opt_hdr == 'SETUP')

      call pc_put_gui_only ('survey_units', obj%survey_units)

      if (obj%ve_on_file /= FNIL .and. &
          obj%ve_on_file /= obj%ve) then
          call pc_put_gui_only ('ve_on_file', &
          'warning: VE on file is '//trim(string_ff2ss(obj%ve_on_file)))
      else
          call pc_put_gui_only ('ve_on_file', ' ')
      end if

      if (obj%datum_on_file /= FNIL .and. &
          obj%datum_on_file /= obj%datum) then
          call pc_put_gui_only ('datum_on_file', &
          'warning: DATUM on file is '          &
                          //trim(string_ff2ss(obj%datum_on_file)))
      else
          call pc_put_gui_only ('datum_on_file', ' ')
      end if

      if (obj%fixed_dist_on_file /= FNIL .and. &
          obj%fixed_dist_on_file /= obj%fixed_dist) then
          call pc_put_gui_only ('fixed_dist_on_file', &
          'warning: FIXED_DIST on file is '          &
                          //trim(string_ff2ss(obj%fixed_dist_on_file)))
      else
          call pc_put_gui_only ('fixed_dist_on_file', ' ')
      end if


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%synthetic)) deallocate (obj%synthetic)

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.    ! to run wrapup code after processing.

      obj%whoops(:) = 0.0

      if (obj%opt_hdr == 'SYN') then
           allocate (obj%synthetic(obj%ndpt))
           do j = 1,obj%ndpt
                obj%synthetic(j) = 2.0 * mth_ranf() - 1.0
           end do
      end if

      call geomdata_initialize_headers (obj%geomdata, obj%iterator, obj%tr_skip)

      if (obj%opt_hdr /= 'SETUP') return
      if (obj%opt_chart == 'NONE' .and. obj%tot_dump == 0) return

!----------we are not attaching headers, but we need a chart or header dump.

      call pc_backend_update (pc_get_lun())
      call pc_backend_execute
      do
           call geomdata_next_header (obj%geomdata, obj%iterator, hhh,err,msg)
           if (err == GEOMDATA_FINISHED) then
                call pc_print ('FINISHED GENERATING HEADERS AT SETUP')
                exit
           end if
           if (err == GEOMDATA_ERROR) then
                call pc_error (msg)
                call pc_print ('ERROR GENERATING HEADERS AT SETUP')
                exit
           end if
           call fgd_chart2 (obj,hhh,error)
           if (error) then
                call pc_error ('ERROR GENERATING CHARTS OR DUMPS AT SETUP')
                exit
           end if
      end do
      call fgd_chart3 (obj)
      call pc_restore

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine fgd_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!-------------------------- fgd validate ---------------------------------!!
!!-------------------------- fgd validate ---------------------------------!!
!!-------------------------- fgd validate ---------------------------------!!


      subroutine fgd_validate (keyword,option,options,noptions)
      implicit none
      character(len=*),intent(in) :: keyword      ! arguments
      character(len=*),intent(in) :: option       ! arguments
      character(len=*),intent(in) :: options(:)   ! arguments
      integer         ,intent(in) :: noptions     ! arguments
      integer                     :: i            ! local

      do i = 1,noptions
           if (option == options(i)) return
      end do
      call pc_error ('illegal value of',keyword)
      return
      end subroutine fgd_validate


!!-------------------------- fgd read file --------------------------------!!
!!-------------------------- fgd read file --------------------------------!!
!!-------------------------- fgd read file --------------------------------!!

! read jd file and allocate and load all data from the file.
! just scans the file (without storing arrays) on the front end.
! errors are fatal on the backend but not on the frontend.
! resets ve and datum and fixed_dist if the file contains different values.
! does NOT reset grid transform if the file contains different values.


      subroutine fgd_read_file (obj)
      implicit none
      type(fgd_struct),intent(inout) :: obj                      ! arguments
      integer                        :: err,i,lun                ! local
      character(len=80)              :: msg                      ! local
      type(grid_struct)              :: grid,nilgrid             ! local
      type(geomio_struct),pointer    :: geomio                   ! local
      character(len=4)               :: chaining                 ! local
      logical                        :: scan,backend,error,gui   ! local
      character(len=80),pointer      :: phist(:)                 ! local
      integer          :: nld,nrp,npp,nzt1,nzt2,nzt3,nzt4        ! local
      integer          :: nlines,ngroups,nhist                   ! local
      real             :: sp,dist,xloc,yloc,elev,depth,tuh       ! ld cards
      real             :: tr,ts,xsd,ysd,elsd                     ! ld cards
      integer          :: line                                   ! ld cards
      integer          :: ipat1,line1,nx,ixinc,ny,iyinc          ! rp cards
      real             :: sp1,xsd1,ysd1,elsd1                    ! rp cards
      character(len=4) :: flag                                   ! rp cards
      real             :: sp2,sp3,xsd2,ysd2,elev2,depth2,tuh2    ! pp cards
      integer          :: line2,line3,ipat2,hold,is,ir,ig        ! pp cards
      character(len=4) :: ccc1,ccc2,ccc3,ccc4                    ! zt cards
      real             :: sss1,sss1a                             ! zt1 cards
      integer          :: lll1                                   ! zt1 cards
      real             :: rrr2,rrr2a                             ! zt2 cards
      integer          :: lll2                                   ! zt2 cards
      integer          :: iggg3,iggg3a,ittt3,ittt3a              ! zt3 cards
      real             :: sss4,sss4a,rrr4,rrr4a                  ! zt4 cards
      integer          :: lll4,lll4a                             ! zt4 cards

!----------get started:

      nullify (geomio) ! jpa

      lun     = pc_get_lun()
      backend = (pc_get_update_state() == PC_BACKEND)
      gui     = (pc_get_update_state() == PC_GUI)
      scan    = (pc_get_update_state() /= PC_BACKEND)
      error   = .false.

      call grid_initialize (nilgrid)
      nullify (phist)

!----------open the file:

      call geomio_open_read (geomio,obj%pathname,err,msg,    &
                             obj%ve_on_file,                 &
                             obj%datum_on_file,              &
                             obj%fixed_dist_on_file,         &
                             grid,chaining,nld,nrp,npp,      &
                             nzt1,nzt2,nzt3,nzt4,phist,nhist)

      if (err /= GEOMIO_OK) go to 999

!----------print information (and check ve,datum,fixed_dist,grid):

      call pc_info (msg)
      call pc_info ('chaining ='           ,chaining)
      call pc_info ('number of LD cards =' ,nld)
      call pc_info ('number of RP cards =' ,nrp)
      call pc_info ('number of PP cards =' ,npp)
      call pc_info ('number of ZT1 cards =',nzt1)
      call pc_info ('number of ZT2 cards =',nzt2)
      call pc_info ('number of ZT3 cards =',nzt3)
      call pc_info ('number of ZT4 cards =',nzt4)

      if (nhist > 0) then
           call pc_print (' ')
           do i = 1,nhist
                call pc_print ('history card:',phist(i))
           end do
           call pc_print (' ')
      end if

      if (nld == 0 .or. nrp == 0 .or. npp == 0) then
           call pc_warning ('JD FILE IS INCOMPLETE:')
           if (nld == 0) call pc_warning ('JD file contains no LD cards')
           if (nrp == 0) call pc_warning ('JD file contains no RP cards')
           if (npp == 0) call pc_warning ('JD file contains no PP cards')
           if (nld == 1) call pc_warning ('JD file contains only 1 LD card')
           if (backend) error = .true.
      end if

      if (obj%ve_on_file == FNIL) then
           call pc_info    ('VE not found on JD file')
      else if (mth_compare(obj%ve_on_file, obj%ve) == 0) then
           call pc_info    ('matching VE found on JD file')
           obj%ve = obj%ve_on_file
      else if (gui) then
           call pc_warning ('warning - mis-matching VE found on JD file')
           call pc_warning ('VE changed from',obj%ve,'to',obj%ve_on_file)
           obj%ve = obj%ve_on_file
      else
           call pc_warning ('warning - mis-matching VE found on JD file')
           call pc_warning ('VE on file =',obj%ve_on_file, &
                                'and in job =', obj%ve)
           call pc_warning ('VE',obj%ve,'will be used')
      end if

      if (obj%datum_on_file == FNIL) then
           call pc_info    ('DATUM not found on JD file')
      else if (mth_compare(obj%datum_on_file, obj%datum) == 0) then
           call pc_info    ('matching DATUM found on JD file')
           obj%datum = obj%datum_on_file
      else if (gui) then
           call pc_warning ('warning - mis-matching DATUM found on JD file')
           call pc_warning ('DATUM changed from',obj%datum,'to', &
                                obj%datum_on_file)
           obj%datum = obj%datum_on_file
      else
           call pc_warning ('warning - mis-matching DATUM found on JD file')
           call pc_warning ('DATUM on file =',obj%datum_on_file, &
                                'and in job =',obj%datum)
           call pc_warning ('DATUM',obj%datum,'will be used')
      end if

      if (obj%fixed_dist_on_file == FNIL) then
           call pc_info    ('FIXED_DIST not found on JD file')
      else if (mth_compare(obj%fixed_dist_on_file, obj%fixed_dist) == 0) then
           call pc_info    ('matching FIXED_DIST found on JD file')
           obj%fixed_dist = obj%fixed_dist_on_file
      else if (gui) then
           call pc_warning &
                         ('warning - mis-matching FIXED_DIST found on JD file')
           call pc_warning ('FIXED_DIST changed from',obj%fixed_dist,'to', &
                                obj%fixed_dist_on_file)
           obj%fixed_dist = obj%fixed_dist_on_file
      else
           call pc_warning &
                         ('warning - mis-matching FIXED_DIST found on JD file')
           call pc_warning ('FIXED_DIST on file =',obj%fixed_dist_on_file, &
                                'and in job =',obj%fixed_dist)
           call pc_warning ('FIXED_DIST',obj%fixed_dist,'will be used')
      end if

      if (grid == nilgrid) then
           call pc_info    ('GRID TRANSFORM not found on JD file')
      else if (grid == obj%grid) then
           call pc_info    ('matching GRID TRANSFORM found on JD file')
      else if (backend) then
        call pc_warning ('error - mis-matching GRID TRANSFORM found on JD file')
        call pc_warning ('xorigin on file ='  ,     grid_get_xorigin(grid), &
                         '  and in job ='     ,     grid_get_xorigin(obj%grid))
        call pc_warning ('yorigin on file ='  ,     grid_get_yorigin(grid), &
                         '  and in job ='     ,     grid_get_yorigin(obj%grid))
        call pc_warning ('xwidth on file ='   , grid_get_xgrid_width(grid), &
                         '  and in job ='     , grid_get_xgrid_width(obj%grid))
        call pc_warning ('ywidth on file ='   , grid_get_ygrid_width(grid), &
                         '  and in job ='     , grid_get_ygrid_width(obj%grid))
        call pc_warning ('angle on file =',  grid_get_rotation_angle(grid), &
                         '  and in job =' ,  grid_get_rotation_angle(obj%grid))
        call pc_warning ('handedness on file =', grid_get_handedness(grid), &
                         '  and in job ='      , grid_get_handedness(obj%grid))
           error = .true.
      else
        call pc_warning &
                   ('warning - mis-matching GRID TRANSFORM found on JD file')
        call pc_warning ('xorigin on file ='  ,     grid_get_xorigin(grid), &
                         '  and in job ='     ,     grid_get_xorigin(obj%grid))
        call pc_warning ('yorigin on file ='  ,     grid_get_yorigin(grid), &
                         '  and in job ='     ,     grid_get_yorigin(obj%grid))
        call pc_warning ('xwidth on file ='   , grid_get_xgrid_width(grid), &
                         '  and in job ='     , grid_get_xgrid_width(obj%grid))
        call pc_warning ('ywidth on file ='   , grid_get_ygrid_width(grid), &
                         '  and in job ='     , grid_get_ygrid_width(obj%grid))
        call pc_warning ('angle on file =',  grid_get_rotation_angle(grid), &
                         '  and in job =' ,  grid_get_rotation_angle(obj%grid))
        call pc_warning ('handedness on file =', grid_get_handedness(grid), &
                         '  and in job ='      , grid_get_handedness(obj%grid))
        call pc_warning ('****************************************************')
        call pc_warning ('batch job will abort if grid transforms do not match')
        call pc_warning ('****************************************************')
      end if

      if (error) then
           call pc_error ('fatal errors with field geometry header information')
      end if

!----------decided not to take the time to scan on front end:

      if (scan) then
           call geomio_close (geomio)
           return
      end if

!----------initialize the data object:

      call geomdata_initialize (obj%geomdata,                              &
                                obj%ve,obj%datum,obj%fixed_dist,obj%grid,  &
                                chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,scan)

!----------read the cards and put them into the data object:

      do i = 1,nld
           call geomio_read_ld_card  (geomio,i,err,msg,   &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_ld_card (obj%geomdata,i,   &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      end do

      do i = 1,nrp
           call geomio_read_rp_card  (geomio,i,err,msg,   &
             ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_rp_card (obj%geomdata,i,   &
             ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      end do

      do i = 1,npp
           call geomio_read_pp_card  (geomio,i,err,msg,   &
            sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_pp_card (obj%geomdata,i,   &
            sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      end do

      do i = 1,nzt1
           call geomio_read_zt1_card  (geomio,i,err,msg,   &
                                       ccc1,sss1,sss1a,lll1)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_zt1_card (obj%geomdata,i,   &
                                       ccc1,sss1,sss1a,lll1)
      end do

      do i = 1,nzt2
           call geomio_read_zt2_card  (geomio,i,err,msg,   &
                                       ccc2,rrr2,rrr2a,lll2)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_zt2_card (obj%geomdata,i,   &
                                       ccc2,rrr2,rrr2a,lll2)
      end do

      do i = 1,nzt3
           call geomio_read_zt3_card  (geomio,i,err,msg,   &
                                       ccc3,iggg3,iggg3a,ittt3,ittt3a)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_zt3_card (obj%geomdata,i,   &
                                       ccc3,iggg3,iggg3a,ittt3,ittt3a)
      end do

      do i = 1,nzt4
           call geomio_read_zt4_card  (geomio,i,err,msg,   &
                                       ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
           if (err /= GEOMIO_OK) go to 999
           call geomdata_set_zt4_card (obj%geomdata,i,   &
                                       ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      end do

!----------validate the data object:

      call geomdata_validate (obj%geomdata,lun,obj%opt_tables,err,msg)
      if (err == GEOMDATA_ERROR) go to 999

      nlines  = geomdata_get_nlines  (obj%geomdata)
      ngroups = geomdata_get_ngroups (obj%geomdata)

      call pc_info      ('number of seismic lines on file =',nlines)
      call pc_info      ('number of shot profiles on file =',ngroups)
      call pc_info      ('field geometry file successfully read.')

      call geomio_close (geomio)
      if (error) call geomdata_clear (obj%geomdata)
      return

!----------an error has occurred:

999   if (backend) then
           call pc_error (msg)
      else
           call pc_warning (msg)
      end if
      call geomio_close   (geomio)
      call geomdata_clear (obj%geomdata)
      return
      end subroutine fgd_read_file


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine fgd (obj,ntr,hd,tr)
      implicit none
      type(fgd_struct),intent(inout) :: obj                    ! arguments
      integer         ,intent(inout) :: ntr                    ! arguments
      double precision,intent(inout) :: hd(:,:)                ! arguments
      real            ,intent(inout) :: tr(:,:)                ! arguments
      double precision               :: hhh(HDR_NOMINAL_SIZE)  ! local
      integer                        :: ntrloop     ,i,j,err ! local
      character(len=80)              :: msg                    ! local
      logical                        :: error                  ! local

!----------we are generating synthetic traces.

      if (obj%opt_hdr == 'SYN') then
           hd(1:obj%nwih,1) = 0.0        ! in case nwih > HDR_NOMINAL_SIZE.
           call geomdata_next_header  &
                        (obj%geomdata, obj%iterator, hd(1:,1), err, msg)
           if (err == GEOMDATA_FINISHED) then
                call pc_print   ('FGD: FINISHED GENERATING SYNTHETIC TRACES')
                call fgd_wrapup (obj)
                ntr = NO_MORE_TRACES
                return
           end if
           if (err == GEOMDATA_ERROR) then
                call pc_error   ('FGD: ERROR GENERATING SYNTHETIC TRACES')
                call pc_error   (msg)
                call fgd_whoops (obj,hd(1:,1))
                call pc_error   ('FGD: ERROR GENERATING SYNTHETIC TRACES')
                call fgd_wrapup (obj)
                ntr = FATAL_ERROR
                return
           end if
           call fgd_chart2 (obj,hd(1:,1),error)
           if (error) then
                call pc_error   ('FGD: ERROR GENERATING CHARTS OR DUMPS')
                call fgd_whoops (obj,hd(1:,1))
                call pc_error   ('FGD: ERROR GENERATING CHARTS OR DUMPS')
                call fgd_wrapup (obj)
                ntr = FATAL_ERROR
                return
           end if
           tr(1:obj%ndpt,1) = hd(25,1)*obj%synthetic(1:obj%ndpt)
           hd(25,1)         = lav(tr(1:,1),obj%ndpt)
           hd(64,1)         = obj%ndpt
           ntr = 1

!----------no more traces to receive headers (obj%opt_hdr == 'ATTACH').

      else if (ntr == NO_MORE_TRACES) then
           call geomdata_next_header  &
                       (obj%geomdata, obj%iterator, hhh, err, msg)
           if (err == GEOMDATA_FINISHED) then
                call pc_print   ('FGD: FINISHED GENERATING HEADERS')
                call pc_print   ('FGD: SAME NUMBER OF TRACES AND HEADERS')
                call fgd_wrapup (obj)
                ntr = NO_MORE_TRACES
                return
           end if
           if (err == GEOMDATA_ERROR) then
                call pc_error   ('FGD: ERROR GENERATING HEADERS')
                call pc_error   (msg)
                call fgd_whoops (obj,hd(1:,1))
                call pc_error   ('FGD: ERROR GENERATING HEADERS')
                call pc_print   ('FGD: RAN OUT OF TRACES BEFORE HEADERS')
                call fgd_wrapup (obj)
                ntr = FATAL_ERROR
                return
           end if
           call pc_print ('FGD: RAN OUT OF TRACES BEFORE HEADERS')
           call fgd_wrapup (obj)
           ntr = NO_MORE_TRACES

!----------we are attaching headers (obj%opt_hdr == 'ATTACH').

      else
           ntrloop = ntr
           do i = 1,ntrloop
                call geomdata_next_header  &
                            (obj%geomdata, obj%iterator, hhh, err, msg)
                if (err == GEOMDATA_FINISHED) then
                     call pc_error   ('FGD: MORE TRACES THAN HEADERS')
                     call fgd_whoops (obj,hd(1:,1))
                     call pc_error   ('FGD: MORE TRACES THAN HEADERS')
                     call fgd_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
                if (err == GEOMDATA_ERROR) then
                     call pc_error   ('FGD: ERROR GENERATING HEADERS')
                     call pc_error   (msg)
                     call fgd_whoops (obj,hd(1:,1))
                     call pc_error   ('FGD: ERROR GENERATING HEADERS')
                     call fgd_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
                call fgd_chart2 (obj,hhh,error)
                if (error) then
                     call pc_error   ('FGD: ERROR GENERATING CHARTS OR DUMPS')
                     call fgd_whoops (obj,hd(1:,1))
                     call pc_error   ('FGD: ERROR GENERATING CHARTS OR DUMPS')
                     call fgd_wrapup (obj)
                     ntr = FATAL_ERROR
                     return
                end if
                if (hhh(25) == 0.0) then
                     tr(1:obj%ndpt,i) = 0.0
                     hd(25,i) = 0.0
                else if (hhh(25) < 0.0) then
                     tr(1:obj%ndpt,i) = -tr(1:obj%ndpt,i)
                end if
                do j = 1,HDR_NOMINAL_SIZE
                     if (j == 2 .or. j == 25 .or. (j >= 48.and.j <= 55)  &
                          .or. j >= 64) cycle
                     hd(j,i) = hhh(j)
                end do
                if (hd(32,1) == -999.0) obj%whoops(:) = hd(1:HDR_NOMINAL_SIZE,1)
           end do
           !!! ntr is unchanged.
           return
      end if
      if (hd(32,1) == -999.0) obj%whoops(:) = hd(1:HDR_NOMINAL_SIZE,1)
      return
      end subroutine fgd

!</execute_only>


!!------------------------------- whoops -----------------------------------!!
!!------------------------------- whoops -----------------------------------!!
!!------------------------------- whoops -----------------------------------!!


!<execute_only>

      subroutine fgd_whoops (obj,hd)
      implicit none
      type(fgd_struct),intent(in) :: obj                 ! arguments
      double precision,intent(in) :: hd(:)               ! arguments
      integer                     :: i,lun               ! local

      lun = pc_get_lun()
      call pc_print ('First column:  Header words for the last good trace.')
      call pc_print ('Second column: Header words currently being built.')
      do i = 1,HDR_NOMINAL_SIZE
           write (lun,1000) i,obj%whoops(i),hd(i),hdr_description(i)
1000       format ('   header word',i3,4x,g15.8,2x,g15.8,2x,a)
      end do
      call pc_print ('Some of the above header words in the second column')
      call pc_print ('may not yet be set, or may contain temporary values.')
      return
      end subroutine fgd_whoops

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine fgd_wrapup (obj)
      implicit none
      type(fgd_struct),intent(inout) :: obj             ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (obj%opt_hdr == 'SETUP') return
      if (obj%opt_chart == 'NONE' .and. obj%tot_dump == 0) return

      if (pc_get_update_state() /= PC_EXECUTE) return

      call pc_print ('WRAPPING UP FGD')
      call fgd_chart3 (obj)
      call pc_print ('FGD IS WRAPPED UP')
      return
      end subroutine fgd_wrapup

!</execute_only>


!!---------------------- other subroutines --------------------------------!!
!!---------------------- other subroutines --------------------------------!!
!!---------------------- other subroutines --------------------------------!!


!<execute_only>


!!---------------------------- fgd chart1 ----------------------------------!!
!!---------------------------- fgd chart1 ----------------------------------!!
!!---------------------------- fgd chart1 ----------------------------------!!

! set up charts and header dumps.


      subroutine fgd_chart1 (obj,nstore,nscratch)
      implicit none
      type(fgd_struct),intent(inout) :: obj                      ! arguments
      integer         ,intent(out)   :: nstore,nscratch          ! arguments
      integer                        :: temporary_nstore         ! local
      integer                        :: temporary_nscratch       ! local
      integer                        :: headers1(64)             ! local
      integer                        :: headers2(64)             ! local
      integer                        :: ngroups,nld              ! local

      data headers1/  &
!!!    1       5        10        15        20        25        30  32
!!!    |       |         |         |         |         |         |   |
       7,5,0,0,0,6,0,0,5,0,7,7,5,7,7,5,7,7,5,0,0,0,0,0,4,0,0,0,0,7,7,0, &
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,7,0,0,0,0,0/
!!!    |       |         |         |         |         |         |   |
!!!   33      37        42        47        52        57        62  64

      data headers2/  &
!!!    1       5        10        15        20        25        30  32
!!!    |       |         |         |         |         |         |   |
       7,0,0,0,0,0,6,6,5,4,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,5,5,6,6,0,0,2, &
       6,6,6,6,6,5,0,6,0,0,0,4,0,5,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!!!    |       |         |         |         |         |         |   |
!!!   33      37        42        47        52        57        62  64

!----------get started:

      nstore   = 0
      nscratch = 0

      obj%want_chart1 = (obj%opt_chart == 'STACK' .or. obj%opt_chart == 'BOTH')
      obj%want_chart2 = (obj%opt_chart == 'GP'    .or. obj%opt_chart == 'BOTH')
      obj%want_tdmp1  = (obj%tot_dump > 0)
      obj%want_tdmp2  = (obj%tot_dump > 0)

      ngroups = geomdata_get_ngroups (obj%geomdata)
      nld     = geomdata_get_nld     (obj%geomdata)

      ngroups = max(ngroups,1)   ! to keep chart from generating fatal error.
      nld     = max(nld    ,1)   ! to keep chart from generating fatal error.

!----------stacking chart:

      if (obj%want_chart1) then

           call pc_clear

           call pc_put_global  ('nwih'     , HDR_NOMINAL_SIZE)
           call pc_put_global  ('ndpt'     , NDPT_CHARTS     )
           call pc_put_process ('spacing'  , obj%spacing     )
           call pc_put_process ('device'   , obj%device      )
           call pc_put_process ('prof_init', 1               )
           call pc_put_process ('prof_last', ngroups         )
           call pc_put_process ('hdr_len'  , 7               )
           call pc_put_process ('len_init' , -999.0          )
           call pc_put_process ('len_inc'  , 1.0             )
           call pc_put_process ('len_last' , 2*nld + 1000.0  )
           call pc_put_process ('hdr_wid'  , 0               )
           call pc_put_process ('wid_init' , 1.0             )
           call pc_put_process ('wid_inc'  , 1.0             )

           if (associated(obj%chart1)) then
                call chart_update (obj%chart1)
           else
                call chart_create (obj%chart1)
           end if

           temporary_nstore   = 0
           temporary_nscratch = 0

           call pc_get_control ("nstore"  , temporary_nstore)
           call pc_get_control ("nscratch", temporary_nscratch)

           nstore   = nstore   + temporary_nstore
           nscratch = nscratch + temporary_nscratch

           call pc_print ('-----PARAMETERS FOR INTERNAL CALL TO CHART',  &
                          '(STACKING CHART)-----')
           call pc_print_process_cards

           call pc_restore

      endif

!----------ground position chart:

      if (obj%want_chart2) then

           call pc_clear

           call pc_put_global  ('nwih'     , HDR_NOMINAL_SIZE)
           call pc_put_global  ('ndpt'     , NDPT_CHARTS     )
           call pc_put_process ('spacing'  , obj%spacing     )
           call pc_put_process ('device'   , obj%device      )
           call pc_put_process ('prof_init', 1               )
           call pc_put_process ('prof_last', ngroups         )
           call pc_put_process ('hdr_len'  , 47              )
           call pc_put_process ('len_init' , 1.0             )
           call pc_put_process ('len_inc'  , 1.0             )
           call pc_put_process ('len_tot'  , nld             )
           call pc_put_process ('hdr_wid'  , 0               )
           call pc_put_process ('wid_init' , 1.0             )
           call pc_put_process ('wid_inc'  , 1.0             )

           if (associated(obj%chart2)) then
                call chart_update (obj%chart2)
           else
                call chart_create (obj%chart2)
           end if

           temporary_nstore   = 0
           temporary_nscratch = 0

           call pc_get_control ("nstore"  , temporary_nstore)
           call pc_get_control ("nscratch", temporary_nscratch)

           nstore   = nstore   + temporary_nstore
           nscratch = nscratch + temporary_nscratch

           call pc_print ('-----PARAMETERS FOR INTERNAL CALL TO CHART',  &
                          '(GROUND POSITION CHART)-----')
           call pc_print_process_cards

           call pc_restore

      endif

!----------first header dump:

      if (obj%want_tdmp1) then

           call pc_clear

           call pc_put_global  ('nwih'     , HDR_NOMINAL_SIZE)
           call pc_put_global  ('ndpt'     , NDPT_CHARTS     )
           call pc_put_process ('skip_init', obj%skip_init   )
           call pc_put_process ('num_do'   , obj%num_dump    )
           call pc_put_process ('num_skip' , obj%num_skip    )
           call pc_put_process ('tot_do'   , obj%tot_dump    )
           call pc_put_process ('opt_hdr'  , 'USER'          )
           call pc_put_process ('headers'  , headers1, HDR_NOMINAL_SIZE)

           if (associated(obj%tdmp1)) then
                call tdmp_update (obj%tdmp1)
           else
                call tdmp_create (obj%tdmp1)
           end if

           temporary_nstore   = 0
           temporary_nscratch = 0

           call pc_get_control ("nstore"  , temporary_nstore)
           call pc_get_control ("nscratch", temporary_nscratch)

           nstore   = nstore   + temporary_nstore
           nscratch = nscratch + temporary_nscratch

           call pc_print  &
                    ('-----PARAMETERS FOR INTERNAL CALL TO FIRST TDMP-----')
           call pc_print_process_cards

           call pc_restore

      endif

!----------second header dump:

      if (obj%want_tdmp2) then

           call pc_clear

           call pc_put_global  ('nwih'     , HDR_NOMINAL_SIZE)
           call pc_put_global  ('ndpt'     , NDPT_CHARTS     )
           call pc_put_process ('skip_init', obj%skip_init   )
           call pc_put_process ('num_do'   , obj%num_dump    )
           call pc_put_process ('num_skip' , obj%num_skip    )
           call pc_put_process ('tot_do'   , obj%tot_dump    )
           call pc_put_process ('opt_hdr'  , 'USER'          )
           call pc_put_process ('headers'  , headers2, HDR_NOMINAL_SIZE)

           if (associated(obj%tdmp2)) then
                call tdmp_update (obj%tdmp2)
           else
                call tdmp_create (obj%tdmp2)
           end if

           temporary_nstore   = 0
           temporary_nscratch = 0

           call pc_get_control ("nstore"  , temporary_nstore)
           call pc_get_control ("nscratch", temporary_nscratch)

           nstore   = nstore   + temporary_nstore
           nscratch = nscratch + temporary_nscratch

           call pc_print  &
                    ('-----PARAMETERS FOR INTERNAL CALL TO SECOND TDMP-----')
           call pc_print_process_cards

!          call pc_clear_gui_cards   ! to throw away tdmp errors.
           call pc_restore

      endif

      return
      end subroutine fgd_chart1


!!-------------------------- fgd chart2 -------------------------------------!!
!!-------------------------- fgd chart2 -------------------------------------!!
!!-------------------------- fgd chart2 -------------------------------------!!

! add one header to the charts and header dumps.


      subroutine fgd_chart2 (obj,hd,error)
      implicit none
      type(fgd_struct),intent(inout) :: obj                     ! arguments
      double precision,intent(inout) :: hd(:)                   ! arguments
      logical         ,intent(out)   :: error                   ! arguments
      double precision               :: hhh(HDR_NOMINAL_SIZE,1) ! local
      real                           :: ttt(NDPT_CHARTS     ,1) ! local
      integer                        :: ntr                     ! local

      error = .false.
      hhh(1:HDR_NOMINAL_SIZE,1) = hd(1:HDR_NOMINAL_SIZE)
      ttt(1:NDPT_CHARTS     ,1) = 1.0

      if (obj%want_chart1) then
           ntr = 1
           call chart (obj%chart1, ntr, hhh, ttt)
           if (ntr == FATAL_ERROR) error = .true.
      end if

      if (obj%want_chart2) then
           ntr = 1
           call chart (obj%chart2, ntr, hhh, ttt)
           if (ntr == FATAL_ERROR) error = .true.
      end if

      if (obj%want_tdmp1) then
           ntr = 1
           call tdmp (obj%tdmp1, ntr, hhh, ttt)
           if (ntr == FATAL_ERROR) error = .true.
      end if

      if (obj%want_tdmp2) then
           ntr = 1
           call tdmp (obj%tdmp2, ntr, hhh, ttt)
           if (ntr == FATAL_ERROR) error = .true.
      end if

      return
      end subroutine fgd_chart2


!!-------------------------- fgd chart3 -------------------------------------!!
!!-------------------------- fgd chart3 -------------------------------------!!
!!-------------------------- fgd chart3 -------------------------------------!!

! generate charts and header dumps.


      subroutine fgd_chart3 (obj)
      implicit none
      type(fgd_struct),intent(inout) :: obj                     ! arguments
      integer                        :: lun                     ! local

      lun = pc_get_lun()
      call pc_print ('FINISHED LOOKING AT HEADERS')

      if (obj%want_chart1) then
           call chart_wrapup (obj%chart1)
      end if

      if (obj%want_chart2) then
           call chart_wrapup (obj%chart2)
      end if

      if (obj%want_tdmp1) then
           write(lun,7001)
           call tdmp_wrapup (obj%tdmp1)
           write(lun,7001)
           write(lun,7000)
      end if

      if (obj%want_tdmp2) then
           write(lun,7002)
           call tdmp_wrapup (obj%tdmp2)
           write(lun,7002)
           write(lun,7000)
      end if

      call pc_print ('FINISHED WITH CHARTS AND HEADER DUMPS')

7000  format &
        (/' (SCRATCH AND UNASSIGNED HEADER WORDS ARE IN small letters ABOVE)'/)
7001  format (' SEQ     MUTE  OFFSET GROUP',                               &
         ' -------SOURCE------   ------RECEIVER----- ',                    &
         '  --------CMP--------  LAV  cmp-inline-dist  --true cmp---'/     &
              ' TR#                       ',                               &
         ' XDIST---YDIST--ELEV   XDIST---YDIST--ELEV ',                    &
         '  XDIST---YDIST--ELEV       nearest--center  xdist---ydist'/     &
         ' 1       2     6      9     11      12      13    14      15',   &
         '      16    17      18      19    25   30      31      58      59')
7002  format (' SEQ     ----CMP----- GROUP  TR#  HD   SOURCE_REC',         &
         '  REC_SOURCE  last ---SOURCE---  --RECEIVER--  ---CMP---  ',     &
         '  POST  TUH  SOURCE-REC'/                                        &
              ' TR#     XGRID--YGRID                  ---LINE---',         &
         '  ----SP----  flag XGRID--YGRID  XGRID--YGRID  SP---LINE  ',     &
         '             ----GP----'/                                        &
         ' 1       7      8      9     10   20   26    27    28     29  ', &
         '   32 33     34     35     36     37     38    40     44   46    47')
      return
      end subroutine fgd_chart3


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

!</execute_only>


      end module fgd_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

