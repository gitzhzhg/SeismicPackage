!<CPS_v1 type="PROCESS"/>
!!----------------------------- flexbin.f90 ---------------------------------!!
!!----------------------------- flexbin.f90 ---------------------------------!!
!!----------------------------- flexbin.f90 ---------------------------------!!


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
! Name       : FLEXBIN
! Category   : miscellaneous
! Written    : 1997-04-15   by: Tom Stoeckley
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Perform flexbinning of pre-stack data.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! FLEXBIN derives a file (called a "flexfile") which contains information
! for flex binning, and uses that file to duplicate or move traces for flex
! binning.  Flex binning is an operation which uses traces from nearby CMP
! bins to fill in holes (deficiencies) in offsets or source-receiver azimuths.
!
! The flexfile is created by using one of the BUILD modes and may be added to
! by using one of the ADD modes.  Traces are duplicated or moved for flex
! binning in the DUPLICATE mode.  TEST, SYNTHETIC and STACK modes are provided
! for diagnostic purposes and to help in adjusting the parameters, principally
! the OFF_DIST and AZMTH_DIST parameters.
!
! FLEXBIN will attempt to duplicate or move traces from neighboring CMP bins
! (subject to the restrictions specified by MAX_DUP, OFF_DIST_INL, and
! OFF_DIST_CRL) to achieve the minimum desired fold in each offset bin.
!
! FLEXBIN will attempt to duplicate or move traces from neighboring CMP bins
! (subject to the restrictions specified by MAX_DUP, AZMTH_DIST_INL, and
! AZMTH_DIST_CRL) to achieve the minimum desired fold in each azimuth bin.
!
! FLEXBIN expects input traces to have values of header words 9 and 10 such
! that those header word values uniquely identify each input trace.
!
!-------------------------------------------------------------------------------
!                            SUMMARY OF MODES
!
!    MODE       flexfile  input traces  output traces          notes
!    ----       --------  ------------  -------------   --------------------
!    BUILD_FGD   created     none           none        uses FGD  setup-only
!    BUILD_HDR   created     yes        same as input
!    ADD_FGD     added to    none           none        uses FGD  setup-only
!    ADD_HDR     added to    yes        same as input
!    DUPLICATE   used        yes       input+duplicate
!    TEST        used        none           none                  setup-only
!    SYNTHETIC   used        none    synthetic+duplicate
!    STACK       used        none    synthetic (stacked)
!
!                      ----------------------------
!
! MODE = BUILD_FGD or BUILD_HDR or ADD_FGD or ADD_HDR:
!     (a) These modes create a new flexfile, or add data to an existing
!          flexfile.
!     (b) These modes need only the MODE and PATHNAME_FLEX parameters.
!     (c) These modes do not take any significant CPU time.
!
! MODE = DUPLICATE or TEST or SYNTHETIC or STACK:
!     (a) These modes use an existing flexfile to derive trace
!          duplication information.
!     (b) These modes use the globals in this job to derive grid
!          coordinates from survey coordinates.
!     (c) These modes need all parameters.
!     (d) These modes might take about 5 minutes of CPU time (on the Cray,
!          but obviously depending on the CPU speed of the computer being
!          used) for each million traces on the flexfile (or double this
!          time when creating synthetic traces with MODE = SYNTHETIC).
!
!                      ----------------------------
!
! MODE = BUILD_FGD:
!     (a) The necessary trace information is obtained from FGD (which
!          uses a JD file) and is saved in a flexfile for future use.
!     (b) The FGD process must reside in the same job, whether or not
!          it is used to attach headers in this job.  If you are not
!          attaching headers in this job, use OPT_HDR = SETUP in FGD.
!     (c) This mode causes this process to run in SETUP ONLY.
!
! MODE = BUILD_HDR:
!     (a) The necessary trace information is obtained from trace headers
!          and is saved in a flexfile for future use.
!     (b) This mode can be used if the JD file is not available.
!
! MODE = ADD_FGD:
!     (a) This mode is the same as BUILD_FGD except information is appended
!          to an existing flexfile.
!     (b) This mode is useful for large 3D surveys which are described
!          by more than one JD file.
!     (c) This mode causes this process to run in SETUP ONLY.
!
! MODE = ADD_HDR:
!     (a) This mode is the same as BUILD_HDR except information is appended
!          to an existing flexfile.
!     (b) This mode is useful for large 3D surveys in situations where
!          the traces are not all available at the same time.
!
!                      ----------------------------
!
! MODE = DUPLICATE:
!     (a) The necessary information is read from the flexfile and used
!          to create duplicates of some of the traces passed to the
!          process.
!     (b) The original traces plus the duplicate traces are passed out.
!     (c) This mode can be used as many times as necessary on different
!          sets of input traces.
!
! MODE = TEST:
!     (a) This mode can be used for testing your parameters to get
!          statistics for helping you to fine-tune your parameters
!          before throwing all your traces at the process.
!     (b) This mode is equivalent to using MODE = DUPLICATE, and passing
!          FLEXBIN a few (or no) arbitrary (possibly synthetic) traces.
!     (c) This mode prints information on CMP fold after flex binning in
!          the report (.rpt) file.
!     (d) This mode causes this process to run in SETUP ONLY.
!
! MODE = SYNTHETIC:
!     (a) This mode works like MODE = DUPLICATE, except that synthetic
!          traces are generated (instead of reading real traces).  This
!          process becomes a trace-supplying process.
!     (b) The output traces are the same as those output by MODE = DUPLICATE,
!          except that most of the trace headers are zero, and the amplitude
!          values are specially coded (see below).
!     (c) This mode is useful for supplying synthetic traces for QC purposes.
!     (d) If a JD is available, this mode is similar to running FGD in
!          its synthetic trace generation mode (OPT_HDR = SYN), followed
!          by this process using MODE = DUPLICATE.  In the FGD case, however,
!          all of the trace headers would be present, and the special
!          trace amplitude coding would not be present.
!
! MODE = STACK:
!     (a) This mode works like MODE = DUPLICATE or MODE = TEST, except
!          that synthetic "stacked" traces are generated.  This process
!          becomes a trace-supplying process.
!     (b) The output traces consist of one trace for each CMP bin in a
!          rectangle determined by the minimum and maximum CMP X and Y
!          bins encountered in the flexfile.  Most of the trace headers
!          are zero, but some contain fold and duplication information
!          (see below).  The amplitude values do not contain any
!          special information.
!     (c) This mode is useful for QC purposes, such as getting time
!          slices of header words to find where traces are duplicated.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
! SEARCH DISTANCE PARAMETERS
!
!     This process will attempt to duplicate or move traces from neighboring
!     CMP bins (subject to the restrictions specified by MAX_DUP, OFF_DIST_INL,
!     and OFF_DIST_CRL) to achieve the minimum desired fold in each offset bin.
!
!     This process will attempt to duplicate or move traces from neighboring
!     CMP bins (subject to restrictions specified by MAX_DUP, AZMTH_DIST_INL,
!     and AZMTH_DIST_CRL) to achieve minimum desired fold in each azimuth bin.
!
!     Traces from outside a CMP bin can contribute to the bin only if they
!     fall within the maximum search distance (OFF_DIST_INL, OFF_DIST_CRL)
!     or (AZMTH_DIST_INL, AZMTH_DIST_CRL).  The maximum search distance is
!     measured from the edge of the bin into adjacent bins, in units of the
!     CMP bin increment (not in units of header words 7 and 8).  A trace can
!     contribute to as many as MAX_DUP bins (or one bin if MAX_DUP is zero),
!     but it can contribute only once to any given bin.
!
!                        Search Distance Examples
!
!     If OFF_DIST_INL and OFF_DIST_CRL are 0.0, no flex binning will occur
!     because this distance does not reach beyond the edge of the CMP bin.
!     If these values are 0.5, this distance reaches to the centers of
!     adjacent CMP bins.  If these values are 1.0, traces can be duplicated
!     for use from anywhere in adjacent CMP bins.  If these values are 1.5,
!     traces can be duplicated for use from adjacent CMP bins, and also from
!     the nearest half of the next CMP bins.  If these values are 2.0,
!     traces can be duplicated for use from adjacent CMP bins, and also from
!     anywhere in the next CMP bins.  OFF_DIST_INL and OFF_DIST_CRL can have
!     different values.
!
!-------------------------------------------------------------------------------
! DUPLICATED TRACE PROPERTIES  (MODE = DUPLICATE and SYNTHETIC)
!
!     When traces are duplicated, the following headers of the
!     duplicated traces are changed to make them look as if they
!     reside in the center of the CMP bin where they are needed:
!                            7, 8, 17, 18.
!     The source and receiver header words (11,12,14,15,33,34,35,36)
!     are modified to make them consistent with the CMP headers, by
!     shifting their values by the same amount that the CMP coordinates
!     were changed.
!
!     For MODE = DUPLICATE, amplitude values in the duplicated traces are not
!     changed.  For MODE = SYNTHETIC, the traces are synthetic, with amplitudes
!     set as described below.
!
!     Only live traces (those with header word 25 not zero) are used
!     for duplication.
!
!     Bins which contain no traces originally (i.e. empty CMP gathers)
!     may receive duplicate traces from other bins.  However, empty
!     bins which are outside of the (irregular) boundaries of the survey
!     will not be filled in.  The boundaries of the survey are determined
!     from the minimum and maximum non-empty CMPs in the inline direction for
!     each inline, and from the minimum and maximum non-empty CMPs in the
!     crossline direction for each crossline.  These boundaries are then
!     straightened in places where the boundaries have "inlets", based on the
!     minimum search distance specified by OFF_DIST_INL, OFF_DIST_CRL,
!     AZMTH_DIST_INL and AZMTH_DIST_CRL.
!
!-------------------------------------------------------------------------------
! SYNTHETIC UNSTACKED TRACE PROPERTIES  (MODE = SYNTHETIC)
!
!     These trace headers are set: 1,2,5-12,14,15,17,18,25,26,27,33-36,
!     64, plus HDR_DUP and HDR_AZMTH if specified.  All other trace headers
!     are zero.
!
!     "Original" traces which are NOT duplicated have non-zero amplitudes
!     only in the bottom 1/2 of the trace.  "Original" traces which ARE
!     duplicated have non-zero amplitudes only in the bottom 3/4 of the trace.
!     "Duplicated" traces have non-zero amplitudes only in the top 1/4 of the
!     trace.  These conventions enable plotting stacks and timeslices of the
!     synthetic traces and easily viewing where duplicate traces were created.
!
!-------------------------------------------------------------------------------
! SYNTHETIC STACKED TRACE PROPERTIES  (MODE = STACK)
!
!     These trace headers are set: 1-5,7,8,17,18,25,58-61,64.
!     All other trace headers are zero.  The headers contain their
!     usual values, with the following exceptions:
!     Header word     5  =  fold after duplicate traces were added.
!     Scratch header 58  =  fold before duplicate traces were added.
!     Scratch header 59  =  number of duplicate traces which were added.
!     Scratch header 60  =  inline CMP index (1 to number of inline bins).
!     Scratch header 61  =  crossline CMP index (1 to number of crossline bins).
!
!-------------------------------------------------------------------------------
! ORDER OF TRACE SELECTION FOR DUPLICATION
!
!     First, traces are searched for which satisfy deficiencies in both
!     offset and azimuth.  This step is skipped if there is no offset array
!     or if there is no azimuth array.
!
!     Second, if there are still deficiencies, traces are searched for which
!     satisfy offset deficiencies by themselves.  This step is skipped if
!     there is no offset array.
!
!     Third, if there are still deficiencies, traces are searched for which
!     satisfy azimuth deficiencies by themselves.  This step is skipped if
!     there is no azimuth array.
!
!     If both offset and azimuth arrays are specified, a trace will not be
!     used for any of the above steps unless it falls within BOTH the
!     offset and azimuth search distances as determined from the
!     offset and azimuth bins in which the trace falls.
!
!     If two or more traces satisfy the same deficiency, the nearest
!     trace is chosen.
!
!-------------------------------------------------------------------------------
! EXAMPLE
!
!     For minimum desired fold of 40:
!
!            OFFSETS   OFF_FOLD    OFF_DIST_INL    OFF_DIST_CRL
!            -------   --------    ------------    ------------
!                0        8            0.5             0.5
!             2000        8            1.0             1.0
!             4000        8            1.5             1.5
!             6000        8            2.0             2.0
!             8000        8            2.5             2.5
!
!            AZMUTHS  AZMTH_FOLD  AZMTH_DIST_INL  AZMTH_DIST_CRL
!            -------  ----------  --------------  --------------
!              -45       10            1.0             0.0
!               45       10            1.0             0.0
!              135       10            1.0             0.0
!              225       10            1.0             0.0
!
!     At the smallest offsets (0 to 2000), we look for traces up to only half
!     a bin outside of the original bin (bin increment distance 0.5 from edge
!     of bin).  At the largest offsets (8000 and more), we look 2.5 bin
!     increments away from the edge of the bin.
!
!     For each azimuth sector, we look at full neighboring bins (up to 1.0 bin
!     increments from the edge of the bin) in the inline direction, but do not
!     look for traces outside of the bin in the crossline direction.  Note that
!     the azimuth sectors in this example are each 90 degrees wide, and are
!     centered at azimuths 0, 90, 180, and 270 degrees respectively.  The first
!     bin contains azimuths from -45 to +45 (centered at azimuth 0 which is
!     north).  The first azimuth could have been specified as 315 instead of
!     -45.  The last azimuth could have been specified as -135 instead of 225.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! MODE = TEST or BUILD_FGD or ADD_FGD:
!  This process does not input or output any traces (is setup only).
!
! MODE = SYNTHETIC or STACK:
!  This process does not input any traces (is a trace-supplying process).
!
! MODE = BUILD_HDR or ADD_HDR or DUPLICATE:
!  Traces can be input in any order, and any number at a time.
!  Input traces do not have to conform to any regular pattern.
!  Input races can be gathered or ungathered.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! MODE = TEST or BUILD_FGD or ADD_FGD:
!  This process does not input or output any traces (is setup only).
!
! MODE = SYNTHETIC:
!  This is a trace-supplying process.
!  The traces are synthetic unstacked traces output as a diagnostic.
!  Traces are output in the order which exists on the flexfile, with
!    duplicate traces following the trace from which they were duplicated,
!    as for MODE = DUPLICATE.
!  Each original trace is output one at a time, followed simultaneously by
!    any duplicates made of that trace.  The number of traces output together
!    can be as much as (MAX_DUP+1)
!  The gather status is set to false.
!
! MODE = STACK:
!  This is a trace-supplying process.
!  The traces are synthetic stacked traces output as a diagnostic.
!  Traces are output one at a time.
!  The gather status is set to false.
!
! MODE = BUILD_HDR or ADD_HDR:
!  Traces are output in the same order as input.
!  The trace values are not altered.
!  The gather status is unchanged.
!
! MODE = DUPLICATE:
!  The input traces plus duplicate traces are output.
!  The input traces are output in their original order.
!  The newly-created traces are output immediately after the trace from
!    which they were created, and therefore they are interspersed among
!    the input traces.  The number of traces output together can be as
!    much as (MAX_DUP+1) times the number of traces input together.
!  The original trace values are not altered.
!  The gather status is set to false.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used and maybe increased
! GATHERED  whether traces are a legitimate gather  maybe changed to false
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! GRID      grid transformation structure           used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Only live traces (header word 25 not 0) are considered for duplication.
!
! HDR_DUP receives trace duplication information (also put in hdr #60).
! HDR_AZMTH receives source-receiver azimuth information (also put in hdr #61).
!
! Input trace header words 9,10,11,12,14,15 are stored in the flexfile.
!
! Header words 7, 8, 17, 18 and 11,12,14,15,33,34,35,36 are changed for
! duplicated traces (to move them to the new CMP center).  But if
! RESET_HEADERS is NO, only header words 7 and 8 are changed.
!
! For MODE=SYNTHETIC, headers 1,2,5-12,14,15,17,18,25,26,27,33-36,64, are set
! plus HDR_DUP and HDR_AZMTH if specified.  All other trace headers are zero.
!
! For MODE=STACK, headers 1-5,7,8,17,18,25,58-61,64 are set.  All other trace
! headers are zero.  The headers contain their usual values, with the following
! exceptions:
!
!     Header word     5  =  fold after duplicate traces were added.
!     Scratch header 58  =  fold before duplicate traces were added.
!     Scratch header 59  =  number of duplicate traces which were added.
!     Scratch header 60  =  inline CMP index (1 to number of inline bins).
!     Scratch header 61  =  crossline CMP index (1 to number of crossline bins).
!
! For MODE = DUPLICATE and SYNTHETIC, header words 1, 60, and 61 (plus
! HDR_DUP and HDR_AZMTH if specified) are reset for the original and
! duplicated traces.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
! 22. 2006-09-11  Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 21. 2006-01-10  B. Menger  Removed Unused Variables.
! 20. 2003-12-09  Goodger    Add use getlun_module.
! 19. 2002-09-17  Stoeckley  Move some traps to end traps to reduce the
!                             number of times some error messages occur.
! 18. 2002-09-03  Stoeckley  Change to use the MTH module for binning.
! 17. 2001-11-05  Stoeckley  Add error messages for memory allocation errors;
!                             convert some automatic arrays to allocatable
!                             arrays to better catch memory errors.
! 16. 2001-10-18  Stoeckley  Allow traces to be moved instead of duplicated
!                             by setting MAX_DUP to zero.
! 15. 2001-08-30  Stoeckley  Add parameter RESET_HEADERS.
! 14. 2001-08-24  Stoeckley  Add file selection box and file status message.
! 13. 2001-03-20  Stoeckley  Make minor changes to incorporate additional
!                             capabilities in the TRIPLESORT primitive.
! 12. 2000-12-08  Stoeckley  Change wrapup flag.
! 11. 2000-06-19  Stoeckley  Added test for keyword present before getting
!                             globals NUMTR and GATHERED to circumvent a
!                             problem with a new capability in PC.
! 10. 2000-06-05  Stoeckley  Converted from old system, changing name from FLEX
!                             to FLEXBIN.
!  9. 1998-01-28  Goodger    Recompile and rename binary flex.o.
!  8. 1998-01-23  Stoeckley  Make better use of Fortran90 modules.
!  7. 1998-01-16  Stoeckley  Convert Cray pointers to Fortran90 pointers.
!  6. 1998-01-15  Stoeckley  Add parameter #traces.
!  5. 1997-12-29  Stoeckley  Two bug fixes in flex_verify_dup_info.
!  4. 1997-11-06  Stoeckley  Added several debug tests and printouts.
!  3. 1997-09-29  Stoeckley  Added SAVE=TEMP option.
!  2. 1997-05-27  Stoeckley  Considerable modifications.
!  1. 1997-04-14  Stoeckley  Initial version.
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
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.       
! NSTORE          >0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK           >0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! NEED_LABEL is true when MODE is SYNTHETIC or STACK (trace supplying process).
! SETUP_ONLY is true when MODE is TEST or BUILD_FGD or ADD_FGD.
!
! Upon input (when SETUP_ONLY is false), NTR must have one of these values:
!  NTR >= 1              means to process the input traces (only when
!                           NEED_LABEL is false).
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces (only when
!                           NEED_LABEL is true).
!
! Upon output (when SETUP_ONLY is false), NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                      PREVIOUS MEMORY REQUIREMENTS
!
! These are memory requirements on the Cray before conversion of this process
! to the new CPS system.  They are now higher because of the use of small data
! structures to replace packed words.
!
!   mode        setup                                      execution
!
!   BUILD_FGD   NTRACES                                    ---     (setup only)
!   BUILD_HDR   0                                          NTRACES (at N=0)
!   ADD_FGD     NTRACES                                    ---     (setup only)
!   ADD_HDR     0                                          NTRACES (at N=0)
!   DUPLICATE   4*NTRACES + 2*NSUB + NDUP + NX*NY + MISC   NDUP
!   TEST        4*NTRACES + 2*NSUB + NDUP + NX*NY + MISC   ---     (setup only)
!   SYNTHETIC   4*NTRACES + 2*NSUB + NDUP + NX*NY + MISC   NDUP
!   STACK       4*NTRACES + 2*NSUB + NDUP + NX*NY + MISC   NX*XY
!
!  ( Goal:     NTRACES +   NSUB + NDUP + NX*NY + MISC    NX*XY )
!  ( probably achievable by reading through the flexfile three times )
!
!  where NTRACES  = number of traces on the flexfile.
!  where NSUB     = number of sub-bins = NX*NY*NO*NA.
!  where NDUP     = number of duplicate traces created.
!  where NX       = number of CMP bins in X direction found on the flexfile.
!  where NY       = number of CMP bins in Y direction found on the flexfile.
!  where MISC     = NCREATED + NSATISFY + 2*NX + 2*NY.
!
!  where NCREATED = sum of all fold values in OFF_FOLD and AZMTH_FOLD arrays.
!  where NSATISFY = sum of all fold values in one super-search bin using
!                     OFF_DIST_INL,OFF_DIST_CRL,AZMTH_DIST_INL,AZMTH_DIST_CRL.
!  where NO       = number of elements in OFFSETS array (minimum 1).
!  where NA       = number of elements in AZIMUTHS  array (minimum 1).
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS FLEXBIN Process/NC=80>
! MODE=`CCCCCCCC     MAX_TR=`IIIIIIII (rough estimate)     RESET_HEADERS=`CCC
!
! Select PATHNAME_FLEX[PATHNAME_FLEX]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                      [PATHNAME_FLEX_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! any bin center      bin increment     HDR_DUP~~=`I (header for dup info)
! INL_INIT=`FFFFFFFF  INL_INC=`FFFFFF   HDR_AZMTH=`I (header for azimuth)
! CRL_INIT=`FFFFFFFF  CRL_INC=`FFFFFF   MAX_DUP~~=`I (max duplicates per trace)
!
!            `------------------------------------------------------
!              OFFSETS   OFF_FOLD  OFF_DIST_INL  OFF_DIST_CRL
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!            `------------------------------------------------------
!
!            `------------------------------------------------------
!              AZIMUTHS  AZMTH_FOLDAZMTH_DIST_INLAZMTH_DIST_CRL
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!              `FFFFFFFFF`FFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF
!            `------------------------------------------------------
!<PARMS PATHNAME_FLEX    [/ML=128/XST]>
!<PARMS OFFSETS_ARRAYSET [/XST/YST]>
!<PARMS AZIMUTHS_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!            ++++++++++++++++++++++++++++++++++++++++++++++++
!                 THE FOLLOWING PARAMETERS ARE NEEDED FOR
!                             FOR ALL MODES
!            ++++++++++++++++++++++++++++++++++++++++++++++++
!
!--------------------------general parameters-----------------------------------
!
!
!<Help KEYWORD="MODE">
!<Tip> General mode of operation of FLEXBIN. </Tip>
! Default = BUILD_FGD
! Allowed = BUILD_FGD   (Create a flex-bin file using FGD.)
! Allowed = BUILD_HDR   (Create a flex-bin file from trace headers.)
! Allowed = ADD_FGD     (Add to a flex-bin file using FGD.)
! Allowed = ADD_HDR     (Add to a flex-bin file from trace headers.)
! Allowed = DUPLICATE   (Use a flex-bin file to duplicate traces.)
! Allowed = TEST        (Use a flex-bin file to test parameters.)
! Allowed = SYNTHETIC   (Use a flex-bin file to synthesize unstacked traces.)
! Allowed = STACK       (Use a flex-bin file to synthesize stacked traces.)
!
! Modes BUILD_FGD, ADD_FGD and TEST are setup-only processes which do not
! require traces.
!</Help>
!
!
!<Help KEYWORD="SELECT_PATHNAME_FLEX">
!<Tip> Choose PATHNAME_FLEX using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_FLEX_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_FLEX. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_FLEX">
!<Tip> Pathname for the flexfile to be used or created. </Tip>
! Default = none
! Allowed = char
!
! This file contains information needed to duplicate traces.
!
! The flexfile is a binary file containing critical header words
! for each trace.  These header words are 9,10,11,12,14,15.
!
! The default extension for this flexfile is ".flex".
!
! In principle, this file could be used in an interactive program for
! deriving duplication information or allowing generalized geometry Q/C.
!
!</Help>
!
!
!            ++++++++++++++++++++++++++++++++++++++++++++++++
!              THE FOLLOWING PARAMETERS ARE NEEDED ONLY FOR
!              MODES DUPLICATE, TEST, SYNTHETIC, and STACK
!            ++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!--------------------------CMP bin parameters-----------------------------------
!
!
!<Help KEYWORD="INL_INIT">
!<Tip> Value of header word 7 for any CMP bin center. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="CRL_INIT">
!<Tip> Value of header word 8 for any CMP bin center. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="INL_INC">
!<Tip> Increment of header word 7 between occupied CMP bin centers. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="CRL_INC">
!<Tip> Increment of header word 8 between occupied CMP bin centers. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!--------------------------miscellaneous parameters-----------------------------
!
!
!<Help KEYWORD="HDR_DUP">
!<Tip> Header word number to receive duplication information. </Tip>
! Default = 0
! Allowed = 0 or 48-55 or >64.
!
! If the user-defined header word HDR_DUP is specified, this header
! word will be set to a value as follows:
!
!       (a) Original traces which were not duplicated: 0.
!       (b) Original traces which were duplicated: #times duplicated (>=1).
!       (c) Duplicated traces: -1.
!
! Scratch header word 60 will always be set to this value.
!
! If HDR_DUP = 0, then record no duplication information.
! Active only if MODE = DUPLICATE or SYNTHETIC.
!</Help>
!
!
!<Help KEYWORD="HDR_AZMTH">
!<Tip> Header word number to receive source-receiver azimuth. </Tip>
! Default = 0
! Allowed = 0 or 48-55 or >64.
!
! If the user-defined header word HDR_AZMTH is specified, this header
! word will be set to the source-to-receiver azimuth (degrees),
! calculated from header words 11, 12, 14, and 15.
!
! Scratch header word 61 will always be set to this value.
!
! Azimuth is calculated from header words 11, 12, 14 and 15 and is expressed as
! compass angle, in degrees, with north being 0 and 360 degrees, east being 90,
! south being 180, and west being 270.  If a trace has an offset of zero, it
! is considered to have an azimuth of 0.
!
! If HDR_AZMTH = 0, then record no azimuth.
! Active only if MODE = DUPLICATE or SYNTHETIC.
!</Help>
!
!
!<Help KEYWORD="MAX_DUP">
!<Tip> Maximum number of times a given trace can be duplicated. </Tip>
! Default = 4
! Allowed = int >= 0
!
! Maximum number of times a given trace can be duplicated to supplement
! neighboring CMP bins deficient in offset or azimuth.
!
! If this parameter is 0, traces will be moved instead of duplicated.
! Only excess traces above the minimum specified fold in a CMP will be moved.
!</Help>
!
!
!<Help KEYWORD="MAX_TR">
!<Tip> Approximate number of traces in this job or on the flexfile. </Tip>
! Default = 1.0E6
! Allowed = int > 0
!
! Approximate number of traces in this job or on the flexfile.
! Used to estimate memory allocation.
!
! The MAX_TR parameter is a rough estimate of the number of
! traces to be read in this job, or the number of traces on the flexfile.
! This information is needed only by the front end, and is used only
! to estimate the amount of dynamic memory to be allocated in the
! processing job.  It need not be accurate since the estimation procedure
! is only very approximate, and uses other information which can only
! be guessed anyway at the front end.  The amount of memory ACTUALLY
! allocated in the processing job is not influenced by this parameter.
! Here is what your estimate should be for different modes:
!
!   MODE = BUILD_FGD: #traces on the JD file.
!   MODE = BUILD_HDR: #traces read in this job.
!   MODE = ADD_FGD:   #traces on JD file  plus #traces already on the flexfile.
!   MODE = ADD_HDR:   #traces in this job plus #traces already on the flexfile.
!   MODE = DUPLICATE: #traces on the flexfile.
!   MODE = TEST:      #traces on the flexfile.
!   MODE = SYNTHETIC: #traces on the flexfile.
!   MODE = STACK:     #traces on the flexfile.
!</Help>
!
!
!<Help KEYWORD="RESET_HEADERS">
!<Tip> Whether to reset most header words of duplicated traces. </Tip>
! Default = YES
! Allowed = YES   Header words 7,8,17,18,11,12,14,15,33,34,35,36 are reset.
! Allowed = NO    Only header words 7 and 8 are reset.
!</Help>
!
!
!-----------------------------offset parameters---------------------------------
!
!
!<Help KEYWORD="OFFSETS">
!<Tip> Array of offsets defining offset bin boundaries. </Tip>
! Default = none
! Allowed = real > 0.0 (linked array)
!
! Array of offsets, in feet or meters, defining offset bin boundaries.  A bin
! is defined as a range of offsets beginning at an OFFSETS entry and less than
! the next OFFSETS entry.  The first bin starts at zero and the last bin is
! everything equal to or greater than the last OFFSETS entry.
!
! Stated another way:
! The minimum offset in the first bin is always zero.
! The minimum offset in any given bin is the stated offset for that bin.
! The maximum offset in any given bin is the minimum offset in the next bin.
! The maximum offset in the last bin is infinite.
!
! Offset bins need not be of uniform width.
!
! You do not need to input anything if offset flex binning is not desired.
!</Help>
!
!
!<Help KEYWORD="OFF_FOLD">
!<Tip> Minimum desired fold in each offset bin. </Tip>
! Default = none
! Allowed = int >= 0 (linked array)
!
! You do not need to input anything if offset flex binning is not desired.
!</Help>
!
!
!<Help KEYWORD="OFF_DIST_INL">
!<Tip> Maximum search distance in inline direction for each offset bin. </Tip>
! Default = none
! Allowed = real >= 0.0 (linked array)
!
! Units of OFF_DIST_INL are increments of header word 7 such that:
!
!    OFF_DIST_INL = 0.5 searches halfway into the adjacent bins.
!    OFF_DIST_INL = 1.0 searches anywhere in the adjacent bins.
!
! You do not need to input anything if offset flex binning is not desired.
!</Help>
!
!
!<Help KEYWORD="OFF_DIST_CRL">
!<Tip> Maximum search distance in crossline direction in each offset bin. </Tip>
! Default = none
! Allowed = real >= 0.0 (linked array)
!
! Units of OFF_DIST_CRL are increments of header word 8 such that:
!
!    OFF_DIST_CRL = 0.5 searches halfway into the adjacent bins.
!    OFF_DIST_CRL = 1.0 searches anywhere in the adjacent bins.
!
! You do not need to input anything if offset flex binning is not desired.
!</Help>
!
!
!---------------------------azimuth parameters----------------------------------
!
!
!<Help KEYWORD="AZIMUTHS">
!<Tip> Array of azimuths defining azimuth bin boundaries. </Tip>
! Default = none
! Allowed = real > 0.0 (linked array)
!
! Array of azimuths, in degrees, defining azimuth bin boundaries.  A bin is
! defined as a range of azimuths beginning at an AZIMUTHS entry and less than
! the next AZIMUTHS entry.  Azimuth bins are cyclic, and the last bin will
! be everything equal to or greater than the last AZIMUTHS entry and less than
! the first AZIMUTHS entry.
!
! Stated another way:
! The minimum azimuth in any given bin is the stated azimuth for that bin.
! The maximum azimuth in any given bin is the minimum azimuth in the next bin.
! The maximum azimuth in the last bin is the minimum azimuth in the first bin.
!
! Azimuth bins need not be of uniform width.
! Azimuth can be expressed in positive or negative degrees.
!
! You do not need to input anything if azimuth flex binning is not desired.
!</Help>
!
!
!<Help KEYWORD="AZMTH_FOLD">
!<Tip> Minimum desired fold in each azimuth bin. </Tip>
! Default = none
! Allowed = int >= 0 (linked array)
!
! You do not need to input anything if azimuth flex binning is not desired.
!</Help>
!
!
!<Help KEYWORD="AZMTH_DIST_INL">
!<Tip> Maximum search distance in inline direction for each azimuth bin. </Tip>
! Default = none
! Allowed = real >= 0.0 (linked array)
!
! Units of AZMTH_DIST_INL are increments of header word 7 such that:
!
!    AZMTH_DIST_INL = 0.5 searches halfway into the adjacent bins.
!    AZMTH_DIST_INL = 1.0 searches anywhere in the adjacent bins.
!
! You do not need to input anything if azimuth flex binning is not desired.
!</Help>
!
!
!<Help KEYWORD="AZMTH_DIST_CRL">
!<Tip> Maximum search distance in crossline direction for azimuth bins. </Tip>
! Default = none
! Allowed = real >= 0.0 (linked array)
!
! Units of AZMTH_DIST_CRL are increments of header word 8 such that:
!
!    AZMTH_DIST_CRL = 0.5 searches halfway into the adjacent bins.
!    AZMTH_DIST_CRL = 1.0 searches anywhere in the adjacent bins.
!
! You do not need to input anything if azimuth flex binning is not desired.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module flexbin_module
      use pc_module
      use named_constants_module
      use grid_module
      use mem_module
      use pathcheck_module
      use pathchoose_module
      use fgd_module
      use geomdata_module
      use flexio_module
      use triplesort_module
      use foursort_module
      use mth_module
      use getlun_module
      implicit none
      private
      public :: flexbin_create
      public :: flexbin_initialize
      public :: flexbin_update
      public :: flexbin_delete
      public :: flexbin
      public :: flexbin_wrapup


      character(len=100),public,save :: FLEXBIN_IDENT = &
       '$Id: flexbin.f90,v 1.22 2006/09/11 13:15:46 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: flexbin_struct              
 
        private
        logical                         :: skip_wrapup        ! wrapup flag.
        integer                         :: nwih,ndpt          ! global params
        type(grid_struct)               :: grid               ! global params
        character(len=16)               :: mode               ! process params
        character(len=FILENAME_LENGTH)  :: pathname_flex      ! process params
        real                            :: inl_init,crl_init  ! process params
        real                            :: inl_inc,crl_inc    ! process params
        integer                         :: hdr_dup,hdr_azmth  ! process params
        integer                         :: max_dup,max_tr     ! process params
        logical                         :: reset_headers      ! process params
        integer                         :: no,na              ! process params
        real                   ,pointer :: offsets       (:)  ! process params
        integer                ,pointer :: off_fold      (:)  ! process params
        real                   ,pointer :: off_dist_inl  (:)  ! process params
        real                   ,pointer :: off_dist_crl  (:)  ! process params
        real                   ,pointer :: azimuths      (:)  ! process params
        integer                ,pointer :: azmth_fold    (:)  ! process params
        real                   ,pointer :: azmth_dist_inl(:)  ! process params
        real                   ,pointer :: azmth_dist_crl(:)  ! process params
        real                            :: xfirst,yfirst      ! dependent
        integer                         :: ntraces            ! dependent
        integer                         :: ninput,noutput     ! dependent
        integer                         :: nduplicated        ! dependent
        integer                         :: ndup,nx,ny         ! dependent
        type(triplesort_ints)  ,pointer :: dup  (:)           ! dependent
        type(fold_struct)      ,pointer :: nfold(:,:)         ! dependent
        type(flexio_struct)    ,pointer :: flexio             ! dependent
        type(pathchoose_struct),pointer :: dialog     

      end type flexbin_struct


!!-------------------------- small structures -----------------------------!!
!!-------------------------- small structures -----------------------------!!
!!-------------------------- small structures -----------------------------!!


      type,private :: ktr_wrapper     ! kpoint(nx,ny,no,na)
         integer,pointer :: ktr(:)
      end type ktr_wrapper


      type,private :: bin_struct ! bin(ntraces)
        private
        real :: xgrid            ! CMP bin X location or exact CMP bin X index.
        real :: ygrid            ! CMP bin Y location or exact CMP bin Y index.
      end type bin_struct


      type,private :: fold_struct   ! nfold(nx,ny)   kfold(nx,ny,no,na)
        private
        integer :: before           ! fold before duplicate traces are added.
        integer :: after            ! fold after  duplicate traces are added.
      end type fold_struct


      type,private :: key_struct   ! key(ntraces)
        private
        integer :: hd9             ! header word  9 (group number).
        integer :: hd10            ! header word 10 (channel number).
        integer :: io              ! offset index.
        integer :: ia              ! azimuth index.
      end type key_struct

        !!! sometimes io = flag as to whether trace has been used for this CMP.
        !!! sometimes ia = number of times trace has been duplicated.


!!!   type :: triplesort_ints   ! dup(ndup)
!!!     private
!!!     integer :: primary        ! header word   9 (group number).
!!!     integer :: secondary      ! header word  10 (channel number).
!!!     integer :: tertiary       ! icmp (CMP number).
!!!   end type triplesort_ints


!!!   type :: foursort_ints   ! satisfy(nsatisfy)
!!!     private
!!!     integer :: one            ! idistance
!!!     integer :: two            ! itrace (trace number).
!!!     integer :: three          ! ix2 (x bin number).
!!!     integer :: four           ! iy2 (y bin number).
!!!   end type foursort_ints


!!!   type :: triplesort_ints   ! temp(ntraces)
!!!     private
!!!     integer :: primary        ! header word   9 (group number).
!!!     integer :: secondary      ! header word  10 (channel number).
!!!     integer :: tertiary       ! itrace (trace number) or icmp (CMP number).
!!!   end type triplesort_ints


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(flexbin_struct),pointer,save :: object      ! needed for traps.

      integer,parameter :: LEVEL = 2

 !!!  logical,parameter :: debug = .true.
      logical,parameter :: debug = .false.

      integer,parameter      :: mode_noptions = 8
      character(len=16),save :: mode_options(mode_noptions)

      data mode_options/'BUILD_FGD','BUILD_HDR',          &
                        'ADD_FGD','ADD_HDR','DUPLICATE',  &
                        'TEST','SYNTHETIC','STACK'/


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine flexbin_create (obj)
      implicit none
      type(flexbin_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%offsets       )
      nullify (obj%off_fold      )
      nullify (obj%off_dist_inl  )
      nullify (obj%off_dist_crl  )
      nullify (obj%azimuths      )
      nullify (obj%azmth_fold    )
      nullify (obj%azmth_dist_inl)
      nullify (obj%azmth_dist_crl)
      nullify (obj%dup           )
      nullify (obj%nfold         )
      nullify (obj%flexio        )

      call mem_alloc (obj%offsets       , 1)
      call mem_alloc (obj%off_fold      , 1)
      call mem_alloc (obj%off_dist_inl  , 1)
      call mem_alloc (obj%off_dist_crl  , 1)
      call mem_alloc (obj%azimuths      , 1)
      call mem_alloc (obj%azmth_fold    , 1)
      call mem_alloc (obj%azmth_dist_inl, 1)
      call mem_alloc (obj%azmth_dist_crl, 1)

      call pathchoose_create  (obj%dialog, 'pathname_flex', 'flex')
      call flexbin_initialize (obj)
      return
      end subroutine flexbin_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine flexbin_delete (obj)
      implicit none
      type(flexbin_struct),pointer :: obj       ! arguments

      call flexbin_wrapup (obj)

      call mem_free (obj%offsets       )
      call mem_free (obj%off_fold      )
      call mem_free (obj%off_dist_inl  )
      call mem_free (obj%off_dist_crl  )
      call mem_free (obj%azimuths      )
      call mem_free (obj%azmth_fold    )
      call mem_free (obj%azmth_dist_inl)
      call mem_free (obj%azmth_dist_crl)

      if (associated(obj%dup  )) deallocate (obj%dup  )
      if (associated(obj%nfold)) deallocate (obj%nfold)

      call pathchoose_delete (obj%dialog)

      deallocate(obj)
      return
      end subroutine flexbin_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine flexbin_initialize (obj)
      implicit none
      type(flexbin_struct),intent(inout) :: obj       ! arguments

      obj%mode          = 'BUILD_FGD'
      obj%pathname_flex = PATHCHECK_EMPTY
      obj%inl_init      = 1.0
      obj%crl_init      = 1.0
      obj%inl_inc       = 1.0
      obj%crl_inc       = 1.0
      obj%hdr_dup       = 0
      obj%hdr_azmth     = 0
      obj%max_dup       = 4
      obj%max_tr        = 1000000
      obj%reset_headers = .true.
      obj%no            = 1
      obj%na            = 1

      obj%offsets       (1) = 0.0 
      obj%off_fold      (1) = 0
      obj%off_dist_inl  (1) = 0.0
      obj%off_dist_crl  (1) = 0.0

      obj%azimuths      (1) = 0.0
      obj%azmth_fold    (1) = 0
      obj%azmth_dist_inl(1) = 0.0
      obj%azmth_dist_crl(1) = 0.0

      call flexbin_update (obj)
      return
      end subroutine flexbin_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine flexbin_update (obj)
      implicit none
      type(flexbin_struct),intent(inout),target :: obj        ! arguments
      integer :: numtr,nstore,nscratch,ndisk        ,i ! local
      integer :: no1,no2,no3,no4                              ! local
      integer :: na1,na2,na3,na4                              ! local
      logical :: setup_only,need_label,gathered,sensitive     ! local
      integer ::       nx,ny,nsub,ndup,ncreated ! local
      integer :: maxfold,ncmp,nsatisfy,misc,show              ! local
      type(grid_struct) :: grid2                              ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update(obj%dialog, obj%pathname_flex)) return

      call pc_register_array_names ("offsets_arrayset", (/  &
                                    "offsets     ",         &
                                    "off_fold    ",         &
                                    "off_dist_inl",         &
                                    "off_dist_crl" /))

      call pc_register_array_names ("azimuths_arrayset", (/  &
                                    "azimuths      ",        &
                                    "azmth_fold    ",        &
                                    "azmth_dist_inl",        &
                                    "azmth_dist_crl" /))

      no1 = obj%no
      no2 = obj%no
      no3 = obj%no
      no4 = obj%no
      na1 = obj%na
      na2 = obj%na
      na3 = obj%na
      na4 = obj%na

      obj%nwih = -1
      obj%ndpt = -1
      numtr    = -1
      gathered = .false.

      call grid_initialize (obj%grid)
      call grid_initialize (grid2)

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('grid'    , obj%grid)
      if (pc_global_keyword_present('numtr')) &
       call pc_get_global ('numtr'   , numtr)
      if (pc_global_keyword_present('gathered')) &
       call pc_get_global ('gathered', gathered)

      call pc_get   ('mode'          , obj%mode         )
      call pc_get   ('pathname_flex' , obj%pathname_flex)
      call pc_get   ('inl_init'      , obj%inl_init     )
      call pc_get   ('crl_init'      , obj%crl_init     )
      call pc_get   ('inl_inc'       , obj%inl_inc      )
      call pc_get   ('crl_inc'       , obj%crl_inc      )
      call pc_get   ('hdr_dup'       , obj%hdr_dup      )
      call pc_get   ('hdr_azmth'     , obj%hdr_azmth    )
      call pc_get   ('max_dup'       , obj%max_dup      )
      call pc_get   ('max_tr'        , obj%max_tr       )
      call pc_get   ('reset_headers' , obj%reset_headers)

      call pc_alloc ('offsets'       , obj%offsets        , no1)
      call pc_alloc ('off_fold'      , obj%off_fold       , no2)
      call pc_alloc ('off_dist_inl'  , obj%off_dist_inl   , no3)
      call pc_alloc ('off_dist_crl'  , obj%off_dist_crl   , no4)
      call pc_alloc ('azimuths'      , obj%azimuths       , na1)
      call pc_alloc ('azmth_fold'    , obj%azmth_fold     , na2)
      call pc_alloc ('azmth_dist_inl', obj%azmth_dist_inl , na3)
      call pc_alloc ('azmth_dist_crl', obj%azmth_dist_crl , na4)

      if (obj%nwih < HDR_NOMINAL_SIZE) then
           obj%nwih = HDR_NOMINAL_SIZE
           call pc_error ('FLEXBIN: NWIH global not set.')
      end if

      if (obj%ndpt <= 0) then
           obj%ndpt = 2
           call pc_error ('FLEXBIN: NDPT global not set.')
      end if

      if (obj%grid == grid2) then
           call pc_error ('FLEXBIN: grid transform global apparently not set.')
      end if

      if (numtr <= 0 .and. obj%mode == 'DUPLICATE') then
           numtr = 1
           call pc_error ('FLEXBIN: NUMTR global not set.')
      end if

      if (no2 /= no1 .or. no3 /= no1 .or. no4 /= no1) then
           call pc_error ('OFFSETS linked arrays have different lengths')
           obj%no = min(no1,no2,no3,no4)
      else
           obj%no = no1
      end if

      if (na2 /= na1 .or. na3 /= na1 .or. na4 /= na1) then
           call pc_error ('AZIMUTHS linked arrays have different lengths')
           obj%na = min(na1,na2,na3,na4)
      else
           obj%na = na1
      end if


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%no == 0) then
           obj%offsets     (1) = 0.0 
           obj%off_fold    (1) = 0
           obj%off_dist_inl(1) = 0.0
           obj%off_dist_crl(1) = 0.0
           obj%no              = 1
      end if

      if (obj%na == 0) then
           obj%azimuths      (1) = 0.0
           obj%azmth_fold    (i) = 0
           obj%azmth_dist_inl(i) = 0.0
           obj%azmth_dist_crl(i) = 0.0
           obj%na                = 1
      end if

      if (obj%mode /= 'BUILD_FGD' .and. obj%mode /= 'BUILD_HDR' .and.  &
          obj%mode /= 'ADD_FGD'   .and. obj%mode /= 'ADD_HDR'   .and.  &
          obj%mode /= 'DUPLICATE' .and. obj%mode /= 'TEST'      .and.  &
          obj%mode /= 'SYNTHETIC' .and. obj%mode /= 'STACK'    ) then
           call pc_error ('FLEXBIN: illegal value of MODE')
      end if

      if (obj%mode == 'BUILD_FGD' .or. obj%mode == 'BUILD_HDR') then
           show = PATHCHECK_INFO_OUTPUT
      else if (obj%mode == 'ADD_FGD' .or. obj%mode == 'ADD_HDR') then
           show = PATHCHECK_INFO_GENERAL
      else
           show = PATHCHECK_INFO_INPUT
      end if

      call pathcheck &
             ('pathname_flex', obj%pathname_flex, 'flex', required = .true., &
                show=show)

      if (obj%max_tr <= 0) obj%max_tr = 1

      do i = 1,obj%no
           if (i == 1) then       
                obj%offsets(i) = 0.0 
           else  
                obj%offsets(i) = max  (obj%offsets     (i), 0.0)
           end if 
           obj%off_fold    (i) = max  (obj%off_fold    (i),   0)
           obj%off_dist_inl(i) = max  (obj%off_dist_inl(i), 0.0)
           obj%off_dist_crl(i) = max  (obj%off_dist_crl(i), 0.0)
      end do

      do i = 1,obj%na
           if (i == 1) then
             if (obj%azimuths(i)> 0.0) obj%azimuths(i) = obj%azimuths(i) - 360.0
             obj%azimuths    (i) = max  (obj%azimuths      (i),-359.0)
             obj%azimuths    (i) = min  (obj%azimuths      (i),   0.0)
           else
             if (obj%azimuths(i)<=0.0) obj%azimuths(i) = obj%azimuths(i) + 360.0
             obj%azimuths    (i) = max  (obj%azimuths      (i),   1.0)
             obj%azimuths    (i) = min  (obj%azimuths      (i), 360.0)
           end if
           obj%azmth_fold    (i) = max  (obj%azmth_fold    (i),     0)
           obj%azmth_dist_inl(i) = max  (obj%azmth_dist_inl(i),   0.0)
           obj%azmth_dist_crl(i) = max  (obj%azmth_dist_crl(i),   0.0)
      end do

      if (pc_verify_end()) then
      if (obj%mode == 'DUPLICATE' .or. obj%mode == 'TEST' .or.  &
          obj%mode == 'SYNTHETIC' .or. obj%mode == 'STACK') then

           if (obj%inl_inc <= 0.0) then
                call pc_warning ('FLEXBIN: INL_INC reset to be > 0.')
                obj%inl_inc = 1.0
           end if

           if (obj%crl_inc <= 0.0) then
                call pc_warning ('FLEXBIN: CRL_INC reset to be > 0.')
                obj%crl_inc = 1.0
           end if

           if (obj%hdr_dup /= 0                .and.  &
               obj%hdr_dup <= HDR_NOMINAL_SIZE .and.  &
                    (obj%hdr_dup < 48 .or. obj%hdr_dup > 55)) then
                call pc_error &
                        ('FLEXBIN: HDR_DUP must be 0 or 48-55 or >64.')
           end if

           if (obj%hdr_azmth /= 0                .and.  &
               obj%hdr_azmth <= HDR_NOMINAL_SIZE .and.  &
                    (obj%hdr_azmth < 48 .or. obj%hdr_azmth > 55)) then
                call pc_error &
                        ('FLEXBIN: HDR_AZMTH must be 0 or 48-55 or >64.')
           end if

           if (obj%hdr_azmth /= 0 .and. obj%hdr_dup == obj%hdr_azmth) then
                call pc_error &
                        ('FLEXBIN: HDR_DUP and HDR_AZMTH cannot be the same.')
           end if

           if (obj%max_dup < 0) then
                call pc_warning ('FLEXBIN: MAX_DUP reset to be >= 0.')
                obj%max_dup = 0
           end if

           if (obj%offsets(obj%no) < 100.0) then
                call pc_error &
                       ('LAST OFFSET IN OFFSETS ARRAY UNREALISTICALLY SMALL')
           end if

           do i = 2,obj%no
                if (obj%offsets(i) < obj%offsets(i-1)) then
                  call pc_error ('OFFSETS ARRAY MUST BE IN ASCENDING ORDER')
                else if (obj%offsets(i) == obj%offsets(i-1)) then
                  call pc_error ('OFFSETS ARRAY MUST NOT HAVE 2 EQUAL VALUES')
                end if
           end do

           do i = 2,obj%na
                if (obj%azimuths(i) < obj%azimuths(i-1)) then
                  call pc_error ('AZIMUTHS ARRAY MUST BE IN ASCENDING ORDER')
                else if (obj%azimuths(i) == obj%azimuths(i-1)) then
                  call pc_error ('AZIMUTHS ARRAY MUST NOT HAVE 2 EQUAL VALUES')
                end if
           end do

      end if
      end if


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------- calculate global and control parameters ----------------!!
!!----------------- calculate global and control parameters ----------------!!
!!----------------- calculate global and control parameters ----------------!!

      nscratch = 0

!!! this calculation for nstore is out of date:
!!! this calculation for nstore is out of date:
!!! this calculation for nstore is out of date:

      if (obj%mode == 'DUPLICATE' .or. obj%mode == 'TEST'.or.  &
          obj%mode == 'SYNTHETIC' .or. obj%mode == 'STACK') then
           nx = 100                          ! unknown.
           ny = 100                          ! unknown.
           nsub = nx * ny * obj%no * obj%na
           ndup = 100000                     ! unknown.
           ncreated = ndup * obj%max_dup     ! or less.
           maxfold = 50                   ! guessed (max fold in one cmp).
           ncmp = 9      ! (could be calculated) max #cmp bins to search over.
           nsatisfy = ncmp * maxfold
           misc = ncreated + nsatisfy + 2*nx + 2*ny
           nstore = 4*obj%max_tr + 2*nsub + ndup + nx*ny + misc
      else
           nstore = obj%max_tr
      end if

      ndisk = nint((6.0 * 4.0 * obj%max_tr) / 1000000.0)
                                                  ! disk usage in megabytes.

      select case (obj%mode)
        case ('DUPLICATE'); gathered = .false.; numtr = (obj%max_dup+1) * numtr
        case ('SYNTHETIC'); gathered = .false.; numtr =  obj%max_dup+1
        case ('STACK'    ); gathered = .false.; numtr =  1
      end select

      need_label = (obj%mode == 'SYNTHETIC' .or. obj%mode == 'STACK')
      setup_only = (obj%mode == 'TEST'      .or. obj%mode == 'BUILD_FGD' .or. &
                    obj%mode == 'ADD_FGD')

      sensitive = (obj%mode == 'DUPLICATE' .or. obj%mode == 'SYNTHETIC' .or.  &
                   obj%mode == 'STACK'     .or. obj%mode == 'TEST')


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('mode', mode_options, mode_noptions)

      call pc_put   ('mode'          , obj%mode          )
      call pc_put   ('pathname_flex' , obj%pathname_flex )
      call pc_put   ('inl_init'      , obj%inl_init      )
      call pc_put   ('crl_init'      , obj%crl_init      )
      call pc_put   ('inl_inc'       , obj%inl_inc       )
      call pc_put   ('crl_inc'       , obj%crl_inc       )
      call pc_put   ('hdr_dup'       , obj%hdr_dup       )
      call pc_put   ('hdr_azmth'     , obj%hdr_azmth     )
      call pc_put   ('max_dup'       , obj%max_dup       )
      call pc_put   ('max_tr'        , obj%max_tr        )
      call pc_put   ('reset_headers' , obj%reset_headers)

      call pc_put   ('offsets'       , obj%offsets       , obj%no)
      call pc_put   ('off_fold'      , obj%off_fold      , obj%no)
      call pc_put   ('off_dist_inl'  , obj%off_dist_inl  , obj%no)
      call pc_put   ('off_dist_crl'  , obj%off_dist_crl  , obj%no)
      call pc_put   ('azimuths'      , obj%azimuths      , obj%na)
      call pc_put   ('azmth_fold'    , obj%azmth_fold    , obj%na)
      call pc_put   ('azmth_dist_inl', obj%azmth_dist_inl, obj%na)
      call pc_put   ('azmth_dist_crl', obj%azmth_dist_crl, obj%na)

      call pc_put_global  ('numtr'       , numtr)
      call pc_put_global  ('gathered'    , gathered)

      call pc_put_control ('need_label'  , need_label)
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)  
      call pc_put_control ('ndisk'       , ndisk)    
      call pc_put_control ('setup_only'  , setup_only)

      call pc_put_minsize_array ('offsets'       , 1)
      call pc_put_minsize_array ('off_fold'      , 1)
      call pc_put_minsize_array ('off_dist_inl'  , 1)
      call pc_put_minsize_array ('off_dist_crl'  , 1)
      call pc_put_minsize_array ('azimuths'      , 1)
      call pc_put_minsize_array ('azmth_fold'    , 1)
      call pc_put_minsize_array ('azmth_dist_inl', 1)
      call pc_put_minsize_array ('azmth_dist_crl', 1)

      call pc_put_sensitive_field_flag ('inl_init'       , sensitive)
      call pc_put_sensitive_field_flag ('crl_init'       , sensitive)
      call pc_put_sensitive_field_flag ('inl_inc'        , sensitive)
      call pc_put_sensitive_field_flag ('crl_inc'        , sensitive)
      call pc_put_sensitive_field_flag ('hdr_dup'        , sensitive)
      call pc_put_sensitive_field_flag ('hdr_azmth'      , sensitive)
      call pc_put_sensitive_field_flag ('max_dup'        , sensitive)

      call pc_put_sensitive_array_flag ('offsets'        , sensitive)
      call pc_put_sensitive_array_flag ('off_fold'       , sensitive)
      call pc_put_sensitive_array_flag ('off_dist_inl'   , sensitive)
      call pc_put_sensitive_array_flag ('off_dist_crl'   , sensitive)
      call pc_put_sensitive_array_flag ('azimuths'       , sensitive)
      call pc_put_sensitive_array_flag ('azmth_fold'     , sensitive)
      call pc_put_sensitive_array_flag ('azmth_dist_inl' , sensitive)
      call pc_put_sensitive_array_flag ('azmth_dist_crl' , sensitive)

      call pc_put_sensitive_arrayset_flag ('off_fold_arrayset'  , sensitive)
      call pc_put_sensitive_arrayset_flag ('azmth_fold_arrayset', sensitive)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%dup  )) deallocate (obj%dup  )
      if (associated(obj%nfold)) deallocate (obj%nfold)

      if (associated(obj%flexio)) call flexio_close_old (obj%flexio)

      obj%ntraces     = 0
      obj%ninput      = 0
      obj%noutput     = 0
      obj%nduplicated = 0
      obj%xfirst      = 0.0
      obj%yfirst      = 0.0
      obj%ndup        = 0
      obj%nx          = 0
      obj%ny          = 0

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      call flexbin_setup (obj%nwih,obj%ndpt,obj%grid,             & ! input
              obj%mode,obj%pathname_flex,obj%max_dup,             & ! input
              obj%inl_init,obj%crl_init,obj%inl_inc,obj%crl_inc,  & ! input
              obj%no,obj%offsets,obj%off_fold,                    & ! input
              obj%off_dist_inl,obj%off_dist_crl,                  & ! input
              obj%na,obj%azimuths,obj%azmth_fold,                 & ! input
              obj%azmth_dist_inl,obj%azmth_dist_crl,              & ! input
              obj%flexio,obj%ntraces,obj%xfirst,obj%yfirst,       & ! output
              obj%ndup,obj%dup,                                   & ! output
              obj%nx,obj%ny,obj%nfold)                              ! output


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine flexbin_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine flexbin (obj,ntr,hd,tr)
      implicit none
      type(flexbin_struct),intent(inout) :: obj                    ! arguments
      integer             ,intent(inout) :: ntr                    ! arguments
      double precision    ,intent(inout) :: hd(:,:)                ! arguments
      real                ,intent(inout) :: tr(:,:)                ! arguments
      logical                            :: error                  ! local

      call flexbin_execute (obj%nwih,obj%ndpt,obj%grid,ntr,hd,tr,         &
             obj%mode,obj%flexio,obj%ntraces,obj%ninput,obj%noutput,      &
             obj%nduplicated,obj%ndup,obj%dup,obj%hdr_dup,obj%hdr_azmth,  &
             obj%xfirst,obj%yfirst,obj%inl_inc,obj%crl_inc,               &
             obj%nx,obj%ny,obj%nfold,obj%max_dup,obj%reset_headers)

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call flexbin_wrapup (obj,error)
           if (error) ntr = FATAL_ERROR
      end if
      return
      end subroutine flexbin


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine flexbin_wrapup (obj,error)
      implicit none
      type(flexbin_struct),intent(inout) :: obj       ! arguments
      logical,optional    ,intent(out)   :: error     ! arguments
      logical                            :: error2    ! local

      if (present(error)) error = .false.
      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.
      if (pc_get_update_state() /= PC_EXECUTE) return

      call flexbin_private_wrapup (obj%mode,obj%flexio,obj%ntraces,         &
                                   obj%ninput,obj%noutput,obj%nduplicated,  &
                                   obj%dup,obj%nfold,obj%max_dup,           &
                                   obj%pathname_flex,error2)
      if (present(error)) error = error2
      return
      end subroutine flexbin_wrapup


!!--------------------------- flexbin setup ---------------------------------!!
!!--------------------------- flexbin setup ---------------------------------!!
!!--------------------------- flexbin setup ---------------------------------!!


      subroutine flexbin_setup (nwih,ndpt,grid,                 & ! input
                         mode,pathname_flex,max_dup,            & ! input
                         inl_init,crl_init,inl_inc,crl_inc,     & ! input
                         no,offsets,off_fold,                   & ! input
                         off_dist_inl,off_dist_crl,             & ! input
                         na,azimuths,azmth_fold,                & ! input
                         azmth_dist_inl,azmth_dist_crl,         & ! input
                         flexio,ntraces,xfirst,yfirst,          & ! output
                         ndup,dup,                              & ! output
                         nx,ny,nfold)                             ! output

      implicit none
      integer            ,intent(in)  :: nwih,ndpt             ! args
      type(grid_struct)  ,intent(in)  :: grid                  ! args
      character(len=*)   ,intent(in)  :: mode,pathname_flex    ! args
      integer            ,intent(in)  :: max_dup,no,na         ! args
      real               ,intent(in)  :: inl_init,crl_init     ! args
      real               ,intent(in)  :: inl_inc,crl_inc       ! args
      real               ,intent(in)  :: offsets       (:)     ! args (no)
      integer            ,intent(in)  :: off_fold      (:)     ! args (no)
      real               ,intent(in)  :: off_dist_inl  (:)     ! args (no)
      real               ,intent(in)  :: off_dist_crl  (:)     ! args (no)
      real               ,intent(in)  :: azimuths      (:)     ! args (na)
      integer            ,intent(in)  :: azmth_fold    (:)     ! args (na)
      real               ,intent(in)  :: azmth_dist_inl(:)     ! args (na)
      real               ,intent(in)  :: azmth_dist_crl(:)     ! args (na)
      integer            ,intent(out) :: ntraces,ndup,nx,ny    ! args
      real               ,intent(out) :: xfirst,yfirst         ! args
      type(flexio_struct)    ,pointer :: flexio                ! args
      type(fold_struct)      ,pointer :: nfold(:,:)            ! args (nx,ny)
      type(triplesort_ints)  ,pointer :: dup(:)                ! args (ndup)
      character(len=80)               :: msg                   ! local
      integer                         ::     ninput,err ! local
      double precision                :: hhh(nwih)             ! local
      logical                         :: error                 ! local

!----------print intentions.

      call pc_print (' ')
      call pc_print ('FLEXBIN: USING MODE =',mode,  &
                                   ', THE FOLLOWING WORK WILL BE PERFORMED:')
      select case (mode)
        case ('BUILD_FGD')
          call pc_print ('FLEXBIN: A new flexfile will be created.')
          call pc_print ('FLEXBIN: Info will be obtained from FGD.')
          call pc_print ('FLEXBIN: This is a setup-only process.')
        case ('BUILD_HDR')
          call pc_print ('FLEXBIN: A new flexfile will be created.')
          call pc_print ('FLEXBIN: Info will be obtained from trace headers.')
        case ('ADD_FGD')
          call pc_print ('FLEXBIN: An existing flexfile will be added to.')
          call pc_print ('FLEXBIN: Info will be obtained from FGD.')
          call pc_print ('FLEXBIN: This is a setup-only process.')
        case ('ADD_HDR')
          call pc_print ('FLEXBIN: An existing flexfile will be added to.')
          call pc_print ('FLEXBIN: Info will be obtained from trace headers.')
        case ('DUPLICATE')
          call pc_print ('FLEXBIN: An existing flexfile will be read.')
          call pc_print ('FLEXBIN: Duplication information will be derived.')
          call pc_print ('FLEXBIN: Traces will be duplicated.')
        case ('TEST')
          call pc_print ('FLEXBIN: An existing flexfile will be read.')
          call pc_print ('FLEXBIN: Duplication information will be derived.')
          call pc_print ('FLEXBIN: This is a setup-only process.')
        case ('SYNTHETIC')
          call pc_print ('FLEXBIN: An existing flexfile will be read.')
          call pc_print ('FLEXBIN: Duplication information will be derived.')
          call pc_print ('FLEXBIN: This is a trace-supplying process.')
          call pc_print ('FLEXBIN: Synthetic traces will be generated.')
        case ('STACK')
          call pc_print ('FLEXBIN: An existing flexfile will be read.')
          call pc_print ('FLEXBIN: Duplication information will be derived.')
          call pc_print ('FLEXBIN: This is a trace-supplying process.')
          call pc_print ('FLEXBIN: Synthetic stacked traces will be generated.')
        case default
          call pc_print ('FLEXBIN: ILLEGAL MODE =',mode)
          go to 999
      end select
      call pc_print (' ')

!----------open flexfile.

      if (mode == 'BUILD_FGD' .or. mode == 'BUILD_HDR') then
           call flexio_open_new (flexio,pathname_flex,pc_get_lun(),err)
           if (err /= FLEXIO_OK) go to 999
           ntraces=0
      else if (mode == 'ADD_FGD' .or. mode == 'ADD_HDR') then
           call flexio_open_append &
                                (flexio,pathname_flex,pc_get_lun(),ntraces,err)
           if (err /= FLEXIO_OK) go to 999
      else if (mode == 'DUPLICATE' .or. mode == 'TEST' .or.   &
               mode == 'SYNTHETIC' .or. mode == 'STACK') then
           call flexio_open_old (flexio,pathname_flex,pc_get_lun(),ntraces,err)
           if (err /= FLEXIO_OK) go to 999
      else
           call pc_print ('FLEXBIN: ILLEGAL MODE =',mode)
           go to 999
      end if

!----------write to flexfile from jd trace headers.

      if (mode == 'BUILD_FGD' .or. mode == 'ADD_FGD') then
           call fgd_initialize_headers
           ninput=0
           do
                call fgd_next_header (hhh,err,msg)
                if (err == GEOMDATA_OK) then
                      if (hhh(25) /= 0.0) then
                           ntraces=ntraces+1
                           ninput=ninput+1
                           call flexio_write_new (flexio,ntraces,  &
                               hhh( 9),hhh(10),hhh(11),hhh(12),    &
                               hhh(14),hhh(15),err)
                           if (err /= FLEXIO_OK) go to 999
                      end if
                else if (err == GEOMDATA_ERROR) then
                      call pc_error ('FLEXBIN:',msg)
                      exit
                else ! if (err == GEOMDATA_FINISHED) then
                      call pc_print ('FLEXBIN:',msg)
                      exit
                end if
           end do
           call pc_print &
                   ('FLEXBIN:',ninput,'LIVE INPUT TRACES RECEIVED FROM FGD')
      end if

!----------read the flexfile.

      if (mode == 'DUPLICATE' .or. mode == 'TEST' .or.   &
          mode == 'SYNTHETIC' .or. mode == 'STACK') then
           call flexbin_get_dup_info (grid,flexio,ntraces,        & ! input
                     max_dup,inl_init,crl_init,inl_inc,crl_inc,   & ! input
                     no,offsets,off_fold,                         & ! input
                     off_dist_inl,off_dist_crl,                   & ! input
                     na,azimuths,azmth_fold,                      & ! input
                     azmth_dist_inl,azmth_dist_crl,               & ! input
                     xfirst,yfirst,                               & ! output
                     ndup,dup,                                    & ! output
                     nx,ny,nfold,error)                             ! output
           if (error) go to 999
      end if

!----------maybe close the flexfile.

      if (mode == 'BUILD_FGD') then
           call flexio_close_new (flexio,ntraces)
           call flexbin_verify_flexfile (pathname_flex,error)
           if (error) go to 999
      else if (mode == 'ADD_FGD') then
           call flexio_close_append (flexio,ntraces,ninput)
           call flexbin_verify_flexfile (pathname_flex,error)
           if (error) go to 999
      else if (mode == 'TEST') then
           deallocate(dup)                                
           deallocate(nfold)                              
           call flexio_close_old (flexio)
      else if (mode == 'DUPLICATE') then
           deallocate(nfold)                              
           call flexio_close_old (flexio)
      else if (mode == 'SYNTHETIC') then
           deallocate(nfold)                              
      else if (mode == 'STACK') then
           deallocate(dup)                                
           call flexio_close_old (flexio)
      end if

!----------finish the setup.

      call pc_print (' ')
      if (mode == 'BUILD_FGD' .or. mode == 'ADD_FGD' .or. mode == 'TEST') then
           call pc_print ('FLEXBIN: THIS PROCESS IS BEING RUN IN SETUP-ONLY')
      end if
      call pc_print ('FLEXBIN: FINISHED WITH SETUP USING MODE =',mode)
      call pc_print (' ')
      return

!----------error returns.

999   call pc_error ('FLEXBIN: FATAL ERROR IN FLEXBIN_SETUP')
      return
      end subroutine flexbin_setup





!!-------------------------- flexbin execute -------------------------------!!
!!-------------------------- flexbin execute -------------------------------!!
!!-------------------------- flexbin execute -------------------------------!!

!!!!!!!!! called when ntr > 0 or ntr = 0,
!!!!!!!!!      or (for trace-supplying mode) ntr undefined.
!!!!!!!!! might set or change ntr, or return ntr = 0.


      subroutine flexbin_execute (nwih,ndpt,grid,ntr,hd,tr,                &
                                  mode,flexio,ntraces,ninput,noutput,      &
                                  nduplicated,ndup,dup,hdr_dup,hdr_azmth,  &
                                  xfirst,yfirst,inl_inc,crl_inc,           &
                                  nx,ny,nfold,max_dup,reset_headers)

      implicit none
      integer                ,intent(in)    :: nwih,ndpt               ! args
      type(grid_struct)      ,intent(in)    :: grid                    ! args
      integer                ,intent(in)    :: hdr_dup,hdr_azmth       ! args
      character(len=*)       ,intent(in)    :: mode                    ! args
      real                   ,intent(in)    :: inl_inc,crl_inc         ! args
      integer                ,intent(inout) :: ntr,ntraces             ! args
      integer                ,intent(inout) :: ninput,noutput          ! args
      integer                ,intent(inout) :: nduplicated,ndup        ! args
      type(flexio_struct)    ,intent(inout) :: flexio                  ! args
      double precision       ,intent(inout) :: hd(:,:)                 ! args
      real                   ,intent(inout) :: tr(:,:),xfirst,yfirst   ! args
      type(triplesort_ints)  ,pointer       :: dup(:)                  ! args
      integer                ,intent(inout) :: nx,ny,max_dup           ! args
      type(fold_struct)      ,pointer       :: nfold(:,:)              ! args
      logical                ,intent(in)    :: reset_headers           ! args
      integer                               :: lasti,i,keep,err        ! local
      integer                               :: duplicated              ! local
      logical                               :: error                   ! local

!--------- preliminaries:

      if (mode == 'STACK') then
           if (ninput == nx*ny) then
                ntr=0
           else
                ntr=1
                ninput=ninput+1
                noutput=noutput+1
                call flexbin_generate3 (nwih,ndpt,grid,ninput,               &
                                        hd(1:,1),tr(1:,1),                   &
                                        xfirst,yfirst,inl_inc,crl_inc,nx,ny, &
                                        nfold)
           end if
           return
      else if (mode == 'SYNTHETIC') then
           if (ninput == ntraces) then
                ntr=0
                return
           else
                ntr=1
                call flexbin_generate1 (nwih,ndpt,grid,flexio,ninput+1,  &
                                        hd(1:,1),tr(1:,1),error)
                if (error) go to 999
           end if
      else if (ntr == 0) then
           return
      end if

!--------- go thru loop:

      lasti=ntr              ! might be incremented by FLEXBIN_DUP.
      do i=1,ntr
        if (hd(25,i) /= 0.0) then
           ninput=ninput+1
           noutput=noutput+1
           if (mode == 'BUILD_HDR' .or. mode == 'ADD_HDR') then
                ntraces=ntraces+1
                call flexio_write_new (flexio,ntraces,          &
                          hd( 9,i),hd(10,i),hd(11,i),hd(12,i),  &
                          hd(14,i),hd(15,i),err)
                if (err /= FLEXIO_OK) go to 999
           else if (mode == 'DUPLICATE' .or. mode == 'SYNTHETIC') then
                keep=lasti
                call flexbin_dup (nwih,ndpt,grid,i,hd,tr,lasti,ndup,dup,       &
                                  xfirst,yfirst,inl_inc,crl_inc,nx,ny,max_dup, &
                                  reset_headers,duplicated,error)
                noutput     = noutput + lasti - keep
                nduplicated = nduplicated + duplicated
                if (error) go to 999
           else
                call pc_print ('FLEXBIN: ILLEGAL MODE =',mode,'IN EXECUTION')
                go to 999
           end if
        end if
      end do
      ntr=lasti

!--------- finish up:

      if (mode == 'SYNTHETIC') then
           call flexbin_generate2 (nwih,ndpt,ntr,hd,tr)
                                             ! uses hd60 set by flexbin_dup.
      end if

      if (mode == 'DUPLICATE' .or. mode == 'SYNTHETIC') then
           do i=1,ntr
                hd(1,i)=noutput+i-ntr
                call flexbin_get_azimuth               &
                      (hd(11,i),hd(12,i),hd(14,i),hd(15,i),  hd(61,i))
                if (hdr_azmth /= 0) hd(hdr_azmth,i)=hd(61,i)
                if (hdr_dup   /= 0) hd(hdr_dup  ,i)=hd(60,i)
                                                 ! hd60 was set by flexbin_dup.
           end do
      end if

      return

!----------error returns.

999   call pc_print &
            ('FLEXBIN: ERROR IN FLEXBIN_EXECUTE AT LIVE INPUT TRACE NUMBER',&
              ninput)
      ntr = FATAL_ERROR
      return
      end subroutine flexbin_execute


!!---------------------- flexbin private wrapup ----------------------------!!
!!---------------------- flexbin private wrapup ----------------------------!!
!!---------------------- flexbin private wrapup ----------------------------!!


      subroutine flexbin_private_wrapup (mode,flexio,ntraces,         &
                                         ninput,noutput,nduplicated,  &
                                         dup,nfold,max_dup,           &
                                         pathname_flex,error)
      implicit none
      character(len=*)   ,intent(in)  :: mode,pathname_flex       ! arguments
      integer            ,intent(in)  :: ntraces,ninput,noutput   ! arguments
      integer            ,intent(in)  :: nduplicated,max_dup      ! arguments
      type(flexio_struct)    ,pointer :: flexio                   ! arguments
      type(fold_struct)      ,pointer :: nfold(:,:)               ! arguments
      type(triplesort_ints)  ,pointer :: dup(:)                   ! arguments
      logical            ,intent(out) :: error                    ! arguments

      if (associated(nfold)) deallocate (nfold)
      if (associated(dup  )) deallocate (dup)

      call pc_print (' ')
      if (mode /= 'STACK') then
           call pc_print ('FLEXBIN:',ninput ,'LIVE INPUT TRACES')
           call pc_print ('FLEXBIN:',noutput,'LIVE OUTPUT TRACES')
      end if

      select case (mode)
        case ('BUILD_HDR')
           call flexio_close_new (flexio,ntraces)
           call flexbin_verify_flexfile (pathname_flex,error)
           if (error) go to 999
        case ('ADD_HDR')
           call flexio_close_append (flexio,ntraces,ninput)
           call flexbin_verify_flexfile (pathname_flex,error)
           if (error) go to 999
        case ('DUPLICATE')
           if (max_dup >= 1) then
             call pc_print ('FLEXBIN:',nduplicated,'DUPLICATE TRACES CREATED')
           else
             call pc_print ('FLEXBIN:',nduplicated,'TRACES MOVED')
           end if
        case ('SYNTHETIC')
           if (max_dup >= 1) then
             call pc_print ('FLEXBIN:',nduplicated,'DUPLICATE TRACES CREATED')
           else
             call pc_print ('FLEXBIN:',nduplicated,'TRACES MOVED')
           end if
           call flexio_close_old (flexio)
        case ('STACK')
           call pc_print ('FLEXBIN:',noutput,'STACKED TRACES CREATED')
        case ('TEST')
           call pc_print ('FLEXBIN: SETUP-ONLY MODE =',mode)
        case ('BUILD_FGD')
           call pc_print ('FLEXBIN: SETUP-ONLY MODE =',mode)
        case ('ADD_FGD')
           call pc_print ('FLEXBIN: SETUP-ONLY MODE =',mode)
        case default
           call pc_print ('FLEXBIN: UNEXPECTED MODE =',mode)
           go to 999
      end select

      if (mode == 'SYNTHETIC' .or. mode == 'STACK') then
           call pc_print ('FLEXBIN: THE TRACES ARE SYNTHETIC')
      end if

      call pc_print ('FLEXBIN: WE ARE FINISHED WITH MODE =',mode)
      call pc_print (' ')
      error = .false.
      return

!----------error returns.

999   call pc_print ('FLEXBIN: ERROR IN FLEXBIN_WRAPUP')
      error = .true.
      return
      end subroutine flexbin_private_wrapup


!!----------------------- flexbin generate1 -------------------------------!!
!!----------------------- flexbin generate1 -------------------------------!!
!!----------------------- flexbin generate1 -------------------------------!!


      subroutine flexbin_generate1 (nwih,ndpt,grid,flexio,itrace,hd,tr,error)

      implicit none
      integer            ,intent(in)    :: nwih,ndpt      ! arguments
      type(grid_struct)  ,intent(in)    :: grid           ! arguments
      integer            ,intent(in)    :: itrace         ! arguments
      type(flexio_struct),intent(inout) :: flexio         ! arguments
      double precision   ,intent(out)   :: hd(:)          ! arguments (nwih)
      real               ,intent(out)   :: tr(:)          ! arguments (ndpt)
      logical            ,intent(out)   :: error          ! arguments
      integer                           ::   err ! local

!--------- set header words.

      hd(1:nwih) = 0.0
      hd( 1) = itrace
      hd( 2) = 1
      hd( 5) = 1
      hd(25) = 1.0
      hd(64) = ndpt
      call flexio_read_old (flexio,itrace,hd(9),hd(10),hd(11),hd(12),  &
                                     hd(14),hd(15),err)
      if (err /= FLEXIO_OK) go to 999

      call flexbin_get_offset   (hd(11),hd(12),hd(14),hd(15), hd( 6))
      call flexbin_get_midpoint (hd(11),hd(12),hd(14),hd(15), hd(17),hd(18))
      call grid_get_grid_coords (grid,  hd(17),hd(18),        hd( 7),hd( 8))
      call grid_get_grid_coords (grid,  hd(11),hd(12),        hd(33),hd(34))
      call grid_get_grid_coords (grid,  hd(14),hd(15),        hd(35),hd(36))

!--------- set trace amplitudes.

      tr(1:ndpt)   =  0.0
      tr(2:ndpt:4) =  1.0
      tr(4:ndpt:4) = -1.0
      error = .false.
      return

!----------error returns.

999   call pc_print ('FLEXBIN: ERROR IN FLEXBIN_GENERATE1')
      error = .true.
      return
      end subroutine flexbin_generate1


!!----------------------- flexbin generate2 -------------------------------!!
!!----------------------- flexbin generate2 -------------------------------!!
!!----------------------- flexbin generate2 -------------------------------!!

!!!!!!!!! clears selected trace amplitudes.
!!!!!!!!! uses the flag in header word 60.
!!!!!!!!!                                    top     amplitudes     bottom
!!!!!!!!!   original non-duplicated trace:   00000000000000111111111111111
!!!!!!!!!       original duplicated trace:   00000001111111111111111111111
!!!!!!!!!             new duplicate trace:   11111110000000000000000000000


      subroutine flexbin_generate2 (nwih,ndpt,ntr,hd,tr)

      implicit none
      integer         ,intent(in)  :: nwih,ndpt,ntr           ! arguments
      double precision,intent(in)  :: hd(:,:)                 ! arguments
      real            ,intent(out) :: tr(:,:)                 ! arguments
      integer                      :: i,istart,nclear         ! local

      if (ndpt < 4) return
      do i=1,ntr
           if (hd(60,i) == 0) then           ! original non-duplicated trace
                istart=1
                nclear=nint(0.50*ndpt)
           else if (hd(60,i) > 0) then       ! original duplicated trace
                istart=1
                nclear=nint(0.25*ndpt)
           else                              ! new duplicate trace
                istart=nint(0.25*ndpt)
                nclear=ndpt-istart+1
           end if
           tr(istart:istart+nclear-1,i) = 0.0
      end do
      return
      end subroutine flexbin_generate2


!!----------------------- flexbin generate3 --------------------------------!!
!!----------------------- flexbin generate3 --------------------------------!!
!!----------------------- flexbin generate3 --------------------------------!!

!!!!!!!!! generates next synthetic stacked trace.


      subroutine flexbin_generate3 (nwih,ndpt,grid,itrace,hd,tr,          &
                                    xfirst,yfirst,inl_inc,crl_inc,nx,ny,  &
                                    nfold)
      implicit none
      integer          ,intent(in)  :: nwih,ndpt                ! arguments
      type(grid_struct),intent(in)  :: grid                     ! arguments
      integer          ,intent(in)  :: itrace,nx,ny             ! arguments
      double precision ,intent(out) :: hd(:)                    ! arguments
      real             ,intent(out) :: tr(:)                    ! arguments
      real             ,intent(in)  :: xfirst,yfirst            ! arguments
      real             ,intent(in)  :: inl_inc,crl_inc          ! arguments
      type(fold_struct),intent(in)  :: nfold(:,:)               ! arguments
      integer :: i,ix,iy,nf1,nf2                                ! local

!--------- get started.

      iy=(itrace+nx-1)/nx
      ix=itrace-(iy-1)*nx
      nf1=nfold(ix,iy)%before
      nf2=nfold(ix,iy)%after

!--------- set header words.

      hd(1:nwih) = 0.0
      hd( 1)=itrace
      hd( 2)=1
      hd( 3)=itrace
      hd( 4)=1
      hd( 5)=nf2                  ! fold after duplicated traces added.
      hd( 7)=mth_bin_center (xfirst, inl_inc, ix)
      hd( 8)=mth_bin_center (yfirst, crl_inc, iy)
      call grid_get_survey_coords (grid,  hd(7),hd(8),   hd(17),hd(18))
      hd(25)=1.0
      hd(58)=nf1                  ! fold before duplicated traces added.
      hd(59)=hd(5)-hd(58)         ! number of duplicated traces added.
      hd(60)=ix                   ! X CMP bin index
      hd(61)=iy                   ! Y CMP bin index
      hd(64)=ndpt

!--------- set trace amplitudes.

      tr(1:ndpt) = 0.0
      do i=2,ndpt,4
           tr(i)=1.0
      end do
      do i=4,ndpt,4
           tr(i)=-1.0
      end do
      return
      end subroutine flexbin_generate3


!!---------------------- temporary file i/o ----------------------------!!
!!---------------------- temporary file i/o ----------------------------!!
!!---------------------- temporary file i/o ----------------------------!!


      subroutine flexbin_open_temporary (lundup,error)
      implicit none
      integer,intent(out) :: lundup                ! arguments
      logical,intent(out) :: error                 ! arguments
      integer             :: istat                 ! local

      call getlun (lundup)
      if (lundup <= 0) then
           call pc_print ('FLEXBIN: GETLUN ERROR FOR TEMPORARY FILE')
           lundup = 0
           error = .true.
           return
      end if

      open (unit=lundup,form='UNFORMATTED',access='SEQUENTIAL',  &
            status='SCRATCH',iostat=istat)
      if (istat /= 0) then
           call pc_print ('FLEXBIN: OPEN ERROR',istat,               &
                                 'FOR TEMPORARY FILE (LUN =',lundup,')')
           lundup = 0
           error = .true.
           return
      end if

      call pc_print ('FLEXBIN: TEMPORARY FILE OPENED (LUN =',lundup,')')
      error = .false.
      return
      end subroutine flexbin_open_temporary




      subroutine flexbin_close_temporary (lundup)
      implicit none
      integer,intent(inout) :: lundup                ! arguments

      if (lundup <= 0) return
      close (unit=lundup,status='DELETE')
      call pc_print ('FLEXBIN: TEMPORARY FILE CLOSED (LUN =',lundup,')')
      lundup = 0
      return
      end subroutine flexbin_close_temporary




      subroutine flexbin_write_temporary (lundup,dup,error)
      implicit none
      integer                ,intent(in)  :: lundup             ! arguments
      type(triplesort_ints)  ,intent(in)  :: dup                ! arguments
      logical                ,intent(out) :: error              ! arguments
      integer                             :: istat              ! local

      write (lundup,iostat=istat) dup
      if (istat /= 0) then
           call pc_print &
                  ('FLEXBIN: WRITE ERROR ON TEMPORARY FILE (LUN =',lundup,')')
           error = .true.
           return
      end if
      error = .false.
      return
      end subroutine flexbin_write_temporary




      subroutine flexbin_read_temporary (lundup,dup,error)
      implicit none
      integer                ,intent(in)  :: lundup             ! arguments
      type(triplesort_ints)  ,intent(out) :: dup                ! arguments
      logical                ,intent(out) :: error              ! arguments
      integer                             :: istat              ! local

      read (lundup,iostat=istat) dup
      if (istat /= 0) then
           call pc_print &
                  ('FLEXBIN: READ ERROR ON TEMPORARY FILE (LUN =',lundup,')')
           error = .true.
           return
      end if
      error = .false.
      return
      end subroutine flexbin_read_temporary


!!--------------------- flexbin get dup info ------------------------------!!
!!--------------------- flexbin get dup info ------------------------------!!
!!--------------------- flexbin get dup info ------------------------------!!

!!!      allocates dup as necessary to hold the cmp number
!!!          for the desired cmp for each of ndup duplicated traces,
!!!          and a matching key for each of ndup duplicated traces.
!!!      the pointee array dup is sorted into increasing order of the
!!!          keys stored in the dup array.
!!!      allocates nfold to contain the cmp fold before and
!!!          after duplicate traces are added.
!!!      temporarily allocates working space, and uses temporary
!!!          files.


      subroutine flexbin_get_dup_info (grid,flexio,ntraces,      & ! input
                     max_dup,inl_init,crl_init,inl_inc,crl_inc,  & ! input
                     no,offsets,off_fold,                        & ! input
                     off_dist_inl,off_dist_crl,                  & ! input
                     na,azimuths,azmth_fold,                     & ! input
                     azmth_dist_inl,azmth_dist_crl,              & ! input
                     xfirst,yfirst,                              & ! output
                     ndup,dup,                                   & ! output
                     nx,ny,nfold,error)                            ! output

      implicit none
      type(grid_struct)  ,intent(in)    :: grid                  ! args
      type(flexio_struct),intent(inout) :: flexio                ! args
      integer            ,intent(in)    :: ntraces,max_dup       ! args
      integer            ,intent(in)    :: no,na                 ! args
      real               ,intent(in)    :: inl_init,crl_init     ! args
      real               ,intent(in)    :: inl_inc,crl_inc       ! args
      real               ,intent(in)    :: offsets       (:)     ! args (no)
      integer            ,intent(in)    :: off_fold      (:)     ! args (no)
      real               ,intent(in)    :: off_dist_inl  (:)     ! args (no)
      real               ,intent(in)    :: off_dist_crl  (:)     ! args (no)
      real               ,intent(in)    :: azimuths      (:)     ! args (na)
      integer            ,intent(in)    :: azmth_fold    (:)     ! args (na)
      real               ,intent(in)    :: azmth_dist_inl(:)     ! args (na)
      real               ,intent(in)    :: azmth_dist_crl(:)     ! args (na)
      real               ,intent(out)   :: xfirst,yfirst         ! args
      integer            ,intent(out)   :: ndup,nx,ny            ! args
      type(fold_struct)      ,pointer   :: nfold(:,:)            ! args
      type(triplesort_ints)  ,pointer   :: dup(:)                ! args
      logical            ,intent(out)   :: error                 ! args
      integer                           ::         kf,ier ! local
      integer                           :: ix,iy,io,ia           ! local
      type(key_struct)   ,allocatable   :: key(:)                ! local
      type(bin_struct)   ,allocatable   :: bin(:)                ! local
      type(ktr_wrapper)  ,allocatable   :: kpoint(:,:,:,:)       ! local
      type(fold_struct)  ,allocatable   :: kfold (:,:,:,:)       ! local

!--------- get started:

      error  = .false.
      xfirst = 0.0
      yfirst = 0.0
      ndup   = 0
      nx     = 0
      ny     = 0
      nullify(dup)                    
      nullify(nfold)                  
      if (ntraces == 0) return

      allocate (key(ntraces),stat=ier) ; if (ier /= 0) go to 901
      allocate (bin(ntraces),stat=ier) ; if (ier /= 0) go to 902

!--------- get keys, bin indices, offset indices, and azimuth indices:

      call pc_print (' ')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print ('+++++++++++ FLEXBIN SUMMARY FROM FLEXFILE +++++++++++++')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print (' ')
      call flexbin_get_data (grid,flexio,ntraces,                 & ! input
                             inl_init,crl_init,inl_inc,crl_inc,   & ! input
                             no,offsets,na,azimuths,              & ! input
                             key,bin,                             & ! output
                             nx,ny,xfirst,yfirst,error)             ! output
      if (error) go to 903

!--------- verify valid key and bin info:

      call flexbin_verify_key_info (key,bin,ntraces,       &
                                    xfirst,inl_inc,nx,     &
                                    yfirst,crl_inc,ny,error)
      if (error) go to 904

!--------- get old fold for each sub-bin:
!--------- get old fold for each cmp bin:

      allocate(kfold(nx,ny,no,na),stat=ier) ; if (ier /= 0) go to 905
      allocate(nfold(nx,ny)      ,stat=ier) ; if (ier /= 0) go to 906

      call flexbin_get_kfold (ntraces,bin,key,nx,ny,no,na,   kfold)
      call flexbin_get_cmp_fold_array (nx,ny,no,na,kfold,1,   nfold)

      call pc_print (' ')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print ('++++ FLEXBIN STATISTICS EXCLUDING DUPLICATE TRACES ++++')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print (' ')

      call flexbin_print_statistics (nx,ny,no,na,kfold,nfold,1,          &
                    offsets ,  off_fold,  off_dist_inl,  off_dist_crl,   &
                    azimuths,azmth_fold,azmth_dist_inl,azmth_dist_crl)

!--------- get list of traces for each sub-bin:

      allocate(kpoint(nx,ny,no,na),stat=ier)
      if (ier /= 0) go to 907

      do ix = 1,nx
      do iy = 1,ny
      do io = 1,no
      do ia = 1,na
           kf = kfold(ix,iy,io,ia)%before
           allocate(kpoint(ix,iy,io,ia)%ktr(kf),stat=ier)
           if (ier /= 0) go to 908
           kpoint(ix,iy,io,ia)%ktr(1:kf) = 0
      end do
      end do
      end do
      end do

      call flexbin_get_ktr (ntraces,bin,key,   &
                            nx,ny,no,na,   kfold,kpoint)

!--------- get new fold for each sub-bin:
!--------- get list of duplicate traces to create:

      call pc_print (' ')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print ('++++++++++++ FLEXBIN DUPLICATION SUMMARY ++++++++++++++')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print (' ')

      call flexbin_do_work (max_dup,                        &  ! input
                   off_fold,  off_dist_inl,  off_dist_crl,  &  ! input
                 azmth_fold,azmth_dist_inl,azmth_dist_crl,  &  ! input
                 ntraces,key,bin,                           &  ! input
                 nx,ny,no,na,kpoint,nfold,                  &  ! input
                 kfold,                                     &  ! update
                 ndup,dup,error)                               ! output

      deallocate (key)                              
      deallocate (bin)

!--------- deallocate list of traces for each sub-bin:

      do ix = 1,nx
      do iy = 1,ny
      do io = 1,no
      do ia = 1,na
           deallocate(kpoint(ix,iy,io,ia)%ktr)               
      end do
      end do
      end do
      end do
      deallocate(kpoint)                              

      if (error) go to 909

!--------- deallocate old fold for each sub-bin:
!--------- deallocate new fold for each sub-bin:
!--------- get new fold for each cmp bin:

      call flexbin_get_cmp_fold_array (nx,ny,no,na,kfold,2,   nfold)
      call flexbin_print_dup_results  (ndup,dup)
      call flexbin_print_cmp_results  (nx,ny,nfold)

      call pc_print (' ')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print ('++++ FLEXBIN STATISTICS INCLUDING DUPLICATE TRACES ++++')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print (' ')

      call flexbin_print_statistics (nx,ny,no,na,kfold,nfold,2,        &
                    offsets ,  off_fold,  off_dist_inl,  off_dist_crl, &
                    azimuths,azmth_fold,azmth_dist_inl,azmth_dist_crl)

      deallocate(kfold)                            

!--------- sort list of duplicate traces:

      call triplesort_sort (dup,ndup)

      call pc_print (' ')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print ('+++++++++++++++ END FLEXBIN STATISTICS ++++++++++++++++')
      call pc_print ('+++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      call pc_print (' ')

!--------- verify valid duplicate trace info and return:

      call flexbin_verify_dup_info (dup,ndup,max_dup,      &
                                    xfirst,inl_inc,nx,     &
                                    yfirst,crl_inc,ny,error)
      if (error) go to 910
      return

!--------- error returns:

901   call pc_print ('FLEXBIN: ERROR ALLOCATING KEY', &
                               ntraces,'IN FLEXBIN_GET_DUP_INFO')
      error = .true.
      return

902   call pc_print ('FLEXBIN: ERROR ALLOCATING BIN', &
                               ntraces,'IN FLEXBIN_GET_DUP_INFO')
      error = .true.
      return

903   call pc_print ('FLEXBIN: READ ERROR IN FLEXBIN_GET_DUP_INFO')
      nx    = 0
      ny    = 0
      error = .true.
      return

904   call pc_print ('FLEXBIN: FATAL ERROR IN FLEXBIN_VERIFY_KEY_INFO')
      nx    = 0
      ny    = 0
      error = .true.
      return

905   call pc_print ('FLEXBIN: ERROR ALLOCATING KFOLD', &
                                    nx*ny*no*na,'IN FLEXBIN_GET_DUP_INFO')
      nx    = 0
      ny    = 0
      error = .true.
      return

906   call pc_print ('FLEXBIN: ERROR ALLOCATING NFOLD', &
                                    nx*ny,'IN FLEXBIN_GET_DUP_INFO')
      nx    = 0
      ny    = 0
      error = .true.
      return

907   call pc_print ('FLEXBIN: ERROR ALLOCATING KPOINT', &
                               nx*ny*no*na,'IN FLEXBIN_GET_KEY_INFO')
      nx    = 0
      ny    = 0
      error = .true.
      return

908   call pc_print ('FLEXBIN: ERROR ALLOCATING KPOINT%KTR', &
                                    kf,'IN FLEXBIN_GET_KEY_INFO')
      nx    = 0
      ny    = 0
      error = .true.
      return

909   call pc_print ('FLEXBIN: WORK ERROR IN FLEXBIN_GET_DUP_INFO')
      if(allocated (kfold)) deallocate (kfold)                            
      if(associated(dup  )) deallocate (dup)                              
      if(associated(nfold)) deallocate (nfold)                            
      ndup  = 0
      nx    = 0
      ny    = 0
      error = .true.
      return

910   call pc_print ('FLEXBIN: VERIFICATION ERROR IN FLEXBIN_GET_DUP_INFO')
      ndup  = 0
      nx    = 0
      ny    = 0
      error = .true.
      return
      end subroutine flexbin_get_dup_info


!!---------------------- flexbin get data --------------------------------!!
!!---------------------- flexbin get data --------------------------------!!
!!---------------------- flexbin get data --------------------------------!!


      subroutine flexbin_get_data (grid,flexio,ntraces,            & ! input
                             inl_init,crl_init,inl_inc,crl_inc,    & ! input
                             no,offsets,na,azimuths,               & ! input
                             key,bin,                              & ! output
                             nx,ny,xfirst,yfirst,error)              ! output
      implicit none
      type(grid_struct)  ,intent(in)    :: grid                 ! args
      integer            ,intent(in)    :: ntraces,no,na        ! args
      real               ,intent(in)    :: inl_init,crl_init    ! args
      real               ,intent(in)    :: inl_inc,crl_inc      ! args
      real               ,intent(in)    :: offsets(:)           ! args (no)
      real               ,intent(in)    :: azimuths(:)          ! args (na)
      type(flexio_struct),intent(inout) :: flexio               ! args
      type(key_struct)   ,intent(out)   :: key(:)               ! args (ntraces)
      type(bin_struct)   ,intent(out)   :: bin(:)               ! args (ntraces)
      integer            ,intent(out)   :: nx,ny                ! args
      real               ,intent(out)   :: xfirst,yfirst        ! args
      logical            ,intent(out)   :: error                ! args
      integer          :: itrace,k,ioff,iaz                     ! local
      double precision :: hd9,hd10,hd11,hd12,hd14,hd15          ! local
      double precision :: azimuth,offset,xmid,ymid,xgrid,ygrid  ! local
      double precision :: xmin9,xmax9,ymin9,ymax9               ! local
      integer          :: igmin9,igmax9,icmin9,icmax9,err       ! local
      integer          :: iomin9,iomax9,iamin9,iamax9           ! local
      integer          :: ixmin9,ixmax9,iymin9,iymax9           ! local
      real             :: xlast,ylast                           ! local

!--------- read through flexfile:
!--------- save keys, bins, offset indices, and azimuth indices:
!--------- get limits.

      igmin9= 999999
      igmax9=-999999
      icmin9= 999999
      icmax9=-999999
      xmin9 = 1.0e30
      xmax9 =-1.0e30
      ymin9 = 1.0e30
      ymax9 =-1.0e30
      iomin9= 999999
      iomax9=-999999
      iamin9= 999999
      iamax9=-999999
      do itrace=1,ntraces
           call flexio_read_old (flexio,itrace,hd9,hd10,hd11,hd12,  &
                                          hd14,hd15,err)
           if (err /= FLEXIO_OK) go to 999

           call flexbin_get_azimuth  (hd11,hd12,hd14,hd15,   azimuth)
           call flexbin_get_offset   (hd11,hd12,hd14,hd15,   offset)
           call flexbin_get_midpoint (hd11,hd12,hd14,hd15,   xmid,ymid)
           call grid_get_grid_coords (grid,  xmid,ymid,      xgrid,ygrid)
           bin(itrace)%xgrid = xgrid
           bin(itrace)%ygrid = ygrid

           ioff=1           ! index between 1 and NOFF (or 1 if NOFF=0)
           iaz =1           ! index between 1 and NAZ  (or 1 if NAZ=0)
           do k=2,no
                if (offset >= offsets(k)) ioff=k
           end do
           do k=2,na
                if (azimuth >= azimuths(k)) iaz=k
           end do
           if (azimuth > azimuths(na)) iaz=1

           key(itrace)%hd9  = nint(hd9)
           key(itrace)%hd10 = nint(hd10)
           key(itrace)%io   = ioff
           key(itrace)%ia   = iaz
           igmin9=min(nint(hd9    ),igmin9)
           igmax9=max(nint(hd9    ),igmax9)
           icmin9=min(nint(hd10   ),icmin9)
           icmax9=max(nint(hd10   ),icmax9)
           xmin9 =min(xgrid        ,xmin9)
           xmax9 =max(xgrid        ,xmax9)
           ymin9 =min(ygrid        ,ymin9)
           ymax9 =max(ygrid        ,ymax9)
           iomin9=min(nint(offset ),iomin9)
           iomax9=max(nint(offset ),iomax9)
           iamin9=min(nint(azimuth),iamin9)
           iamax9=max(nint(azimuth),iamax9)
      end do
      call pc_print ('',ntraces,'TRACES READ FROM FLEXFILE')
      call pc_print (' ')
      call pc_print ('MINIMUM AND MAXIMUM GROUP NUMBERS   =',igmin9,'',igmax9)
      call pc_print ('MINIMUM AND MAXIMUM CHANNEL NUMBERS =',icmin9,'',icmax9)
      call pc_print ('MINIMUM AND MAXIMUM CMP XBINS ='      , xmin9,'', xmax9)
      call pc_print ('MINIMUM AND MAXIMUM CMP YBINS ='      , ymin9,'', ymax9)
      call pc_print ('MINIMUM AND MAXIMUM OFFSETS   ='      ,iomin9,'',iomax9)
      call pc_print ('MINIMUM AND MAXIMUM AZIMUTHS  ='      ,iamin9,'',iamax9)

!--------- convert bin limits to consecutive integers for bin centers:

      ixmin9 = mth_bin_number (inl_init, inl_inc, real(xmin9)) - 1
      ixmax9 = mth_bin_number (inl_init, inl_inc, real(xmax9)) - 1
      iymin9 = mth_bin_number (crl_init, crl_inc, real(ymin9)) - 1
      iymax9 = mth_bin_number (crl_init, crl_inc, real(ymax9)) - 1

!--------- get centers of first and last bins:

      xfirst = mth_bin_center (inl_init, inl_inc, ixmin9 + 1)
      yfirst = mth_bin_center (crl_init, crl_inc, iymin9 + 1)
      xlast  = mth_bin_center (inl_init, inl_inc, ixmax9 + 1)
      ylast  = mth_bin_center (crl_init, crl_inc, iymax9 + 1)
      call pc_print (' ')
      call pc_print ('CENTERS OF FIRST AND LAST CMP XBINS =',xfirst,'',xlast)
      call pc_print ('CENTERS OF FIRST AND LAST CMP YBINS =',yfirst,'',ylast)

!--------- get number of indices:

      nx=ixmax9-ixmin9+1      ! number of X bins
      ny=iymax9-iymin9+1      ! number of Y bins
      call pc_print (' ')
      call pc_print ('NUMBER OF CMP XBINS      =',nx)
      call pc_print ('NUMBER OF CMP YBINS      =',ny)
      call pc_print ('NUMBER OF OFFSET BINS    =',no)
      call pc_print ('NUMBER OF AZIMUTH BINS   =',na)
      call pc_print ('TOTAL NUMBER OF CMP BINS =',nx*ny)
      call pc_print &
             ('TOTAL NUMBER OF CMP/OFFSET/AZIMUTH SUB-BINS =',nx*ny*no*na)

!--------- convert bins to exact (floating point) indices:
!--------- make sure all bin indices are within range (probably not necessary):

      do itrace=1,ntraces
           xgrid = bin(itrace)%xgrid
           ygrid = bin(itrace)%ygrid
           xgrid=(xgrid-inl_init)/inl_inc-ixmin9+1
           ygrid=(ygrid-crl_init)/crl_inc-iymin9+1
           xgrid=min(max(xgrid,0.51d0),nx+0.49d0)
           ygrid=min(max(ygrid,0.51d0),ny+0.49d0)
           bin(itrace)%xgrid = xgrid
           bin(itrace)%ygrid = ygrid
      end do
      error = .false.
      return

!--------- error returns:

999   call pc_print ('FLEXBIN: ERROR IN FLEXBIN_GET_DATA')
      error = .true.
      return
      end subroutine flexbin_get_data


!!--------------------- flexbin get kfold ---------------------------------!!
!!--------------------- flexbin get kfold ---------------------------------!!
!!--------------------- flexbin get kfold ---------------------------------!!

!!!     uses offset and azimuth indices in key.


      subroutine flexbin_get_kfold (ntraces,bin,key,nx,ny,no,na,   kfold)
      implicit none
      integer          ,intent(in)  :: ntraces,nx,ny,no,na ! args
      type(bin_struct) ,intent(in)  :: bin(:)              ! args (ntraces)
      type(key_struct) ,intent(in)  :: key(:)              ! args (ntraces)
      type(fold_struct),intent(out) :: kfold(:,:,:,:)      ! args (nx,ny,no,na)
      integer                       :: itrace,ix,iy,io,ia  ! local
      real                          :: xgrid,ygrid         ! local

      kfold(1:nx,1:ny,1:no,1:na)%before = 0
      kfold(1:nx,1:ny,1:no,1:na)%after  = 0
      do itrace=1,ntraces
           xgrid = bin(itrace)%xgrid
           ygrid = bin(itrace)%ygrid
           ix = nint(xgrid)
           iy = nint(ygrid)
           io = key(itrace)%io
           ia = key(itrace)%ia
           kfold(ix,iy,io,ia)%before = kfold(ix,iy,io,ia)%before + 1
      end do
      return
      end subroutine flexbin_get_kfold


!!----------------------- flexbin get ktr --------------------------------!!
!!----------------------- flexbin get ktr --------------------------------!!
!!----------------------- flexbin get ktr --------------------------------!!

!!!     kfold is obtained again for convenience.
!!!     uses offset and azimuth indices in key.


      subroutine flexbin_get_ktr (ntraces,bin,key,nx,ny,no,na,   kfold,kpoint)
      implicit none
      integer          ,intent(in)  :: ntraces,nx,ny,no,na ! args
      type(bin_struct) ,intent(in)  :: bin(:)              ! args (ntraces)
      type(key_struct) ,intent(in)  :: key(:)              ! args (ntraces)
      type(fold_struct),intent(out) :: kfold (:,:,:,:)     ! args (nx,ny,no,na)
      type(ktr_wrapper),intent(out) :: kpoint(:,:,:,:)     ! args (nx,ny,no,na)
      integer                   :: itrace,ix,iy,io,ia,kf   ! local
      real                      :: xgrid,ygrid             ! local

      kfold(1:nx,1:ny,1:no,1:na)%before = 0
      kfold(1:nx,1:ny,1:no,1:na)%after  = 0
      do itrace=1,ntraces
           xgrid = bin(itrace)%xgrid
           ygrid = bin(itrace)%ygrid
           ix = nint(xgrid)
           iy = nint(ygrid)
           io = key(itrace)%io
           ia = key(itrace)%ia
           kfold(ix,iy,io,ia)%before = kfold(ix,iy,io,ia)%before + 1
           kf = kfold(ix,iy,io,ia)%before
           kpoint(ix,iy,io,ia)%ktr(kf) = itrace        
      end do
      return
      end subroutine flexbin_get_ktr


!!----------------------- get summed fold -----------------------------!!
!!----------------------- get summed fold -----------------------------!!
!!----------------------- get summed fold -----------------------------!!

!!!!!!! flexbin_get_offset_fold      sums kfold over azimuth.
!!!!!!! flexbin_get_azimuth_fold     sums kfold over offset.
!!!!!!! flexbin_get_cmp_fold         sums kfold over offset and azimuth.
!!!!!!! flexbin_get_cmp_fold_array   sums kfold over offset and azimuth.


      subroutine flexbin_get_offset_fold &
                                  (nx,ny,no,na,kfold,nsh,ix,iy,io,  ifold)
      implicit none
      integer          ,intent(in)  :: nx,ny,no,na       ! args
      integer          ,intent(in)  :: ix,iy,io,nsh      ! args
      type(fold_struct),intent(in)  :: kfold(:,:,:,:)    ! args (nx,ny,no,na)
      integer          ,intent(out) :: ifold             ! args
      integer                       :: ia                ! local

      ifold=0
      do ia=1,na
           if (nsh == 1) then
                ifold = ifold + kfold(ix,iy,io,ia)%before
           else
                ifold = ifold + kfold(ix,iy,io,ia)%after
           end if
      end do
      return
      end subroutine flexbin_get_offset_fold



      subroutine flexbin_get_azimuth_fold &
                                  (nx,ny,no,na,kfold,nsh,ix,iy,ia,  ifold)
      implicit none
      integer          ,intent(in)  :: nx,ny,no,na       ! args
      integer          ,intent(in)  :: ix,iy,ia,nsh      ! args
      type(fold_struct),intent(in)  :: kfold(:,:,:,:)    ! args (nx,ny,no,na)
      integer          ,intent(out) :: ifold             ! args
      integer                       :: io                ! local

      ifold=0
      do io=1,no
           if (nsh == 1) then
                ifold = ifold + kfold(ix,iy,io,ia)%before
           else
                ifold = ifold + kfold(ix,iy,io,ia)%after
           end if
      end do
      return
      end subroutine flexbin_get_azimuth_fold



      subroutine flexbin_get_cmp_fold (nx,ny,no,na,kfold,nsh,ix,iy,  ifold)
      implicit none
      integer          ,intent(in)  :: nx,ny,no,na       ! args
      integer          ,intent(in)  :: ix,iy,nsh         ! args
      type(fold_struct),intent(in)  :: kfold(:,:,:,:)    ! args (nx,ny,no,na)
      integer          ,intent(out) :: ifold             ! args
      integer                       :: io,ia ! local

      ifold=0
      do io=1,no
      do ia=1,na
           if (nsh == 1) then
                ifold = ifold + kfold(ix,iy,io,ia)%before
           else
                ifold = ifold + kfold(ix,iy,io,ia)%after
           end if
      end do
      end do
      return
      end subroutine flexbin_get_cmp_fold



      subroutine flexbin_get_cmp_fold_array (nx,ny,no,na,kfold,nsh,  nfold)
      implicit none
      integer          ,intent(in)  :: nx,ny,no,na,nsh    ! args
      type(fold_struct),intent(in)  :: kfold(:,:,:,:)     ! args (nx,ny,no,na)
      type(fold_struct),intent(out) :: nfold(:,:)         ! args (nx,ny)
      integer                       :: ix,iy,ifold        ! local

      do iy=1,ny
      do ix=1,nx
           call flexbin_get_cmp_fold (nx,ny,no,na,kfold,nsh,ix,iy,   ifold)
           if (nsh == 1) then
                nfold(ix,iy)%before = ifold
           else
                nfold(ix,iy)%after  = ifold
           end if
      end do
      end do
      return
      end subroutine flexbin_get_cmp_fold_array


!!--------------------- flexbin print statistics -------------------------!!
!!--------------------- flexbin print statistics -------------------------!!
!!--------------------- flexbin print statistics -------------------------!!


      subroutine flexbin_print_statistics (nx,ny,no,na,kfold,nfold,nsh,      &
                        offsets ,  off_fold,  off_dist_inl,  off_dist_crl,   &
                        azimuths,azmth_fold,azmth_dist_inl,azmth_dist_crl)

      implicit none
      integer          ,intent(in) :: nx,ny,no,na,nsh      ! args
      type(fold_struct),intent(in) :: kfold(:,:,:,:)       ! args (nx,ny,no,na)
      type(fold_struct),intent(in) :: nfold(:,:)           ! args (nx,ny)
      real             ,intent(in) :: offsets       (:)    ! args (no)
      integer          ,intent(in) :: off_fold      (:)    ! args (no)
      real             ,intent(in) :: off_dist_inl  (:)    ! args (no)
      real             ,intent(in) :: off_dist_crl  (:)    ! args (no)
      real             ,intent(in) :: azimuths      (:)    ! args (na)
      integer          ,intent(in) :: azmth_fold    (:)    ! args (na)
      real             ,intent(in) :: azmth_dist_inl(:)    ! args (na)
      real             ,intent(in) :: azmth_dist_crl(:)    ! args (na)
      integer :: minfold2(no,na),maxfold2(no,na)           ! local
      integer :: nsum2   (no,na)                           ! local
      real    :: average2(no,na)                           ! local
      integer :: minfold3(no),maxfold3(no),nsum3(no)       ! local
      integer :: minfold4(na),maxfold4(na),nsum4(na)       ! local
      real    :: average3(no)                              ! local
      real    :: average4(na)                              ! local
      integer :: nbdef3  (no),ntdef3(no)                   ! local
      integer :: nbdef4  (na),ntdef4(na)                   ! local
      integer :: minfold,maxfold,nsum,nfull,ia,io          ! local
      real    :: average,maxval                            ! local
      integer :: ix,iy,number,missing,nempty,nf,lun        ! local

!--------- get and print number of empty bins:

      nempty=0
      do iy=1,ny
      do ix=1,nx
           if (nsh == 1) then
                number = nfold(ix,iy)%before
           else
                number = nfold(ix,iy)%after
           end if
           if (number == 0) nempty = nempty+1
      end do
      end do
      call pc_print ('NUMBER OF EMPTY CMP BINS =',nempty)
      call pc_print &
            ('Empty cmp bins are EXCLUDED in all of the following statistics.')

!--------- initialize variables:

      minfold=999999
      maxfold=0
      average=0.0
      nsum=0
      nfull=0

      do ia=1,na 
      do io=1,no
           minfold2(io,ia)=99999
           maxfold2(io,ia)=0
           average2(io,ia)=0.0
           nsum2   (io,ia)=0
      end do
      end do

      do io=1,no
           minfold3(io)=99999
           maxfold3(io)=0
           average3(io)=0.0
           nsum3   (io)=0
           nbdef3  (io)=0
           ntdef3  (io)=0
      end do

      do ia=1,na
           minfold4(ia)=99999
           maxfold4(ia)=0
           average4(ia)=0.0
           nsum4   (ia)=0
           nbdef4  (ia)=0
           ntdef4  (ia)=0
      end do

!--------- loop over all cmp bins:

      do iy=1,ny
      do ix=1,nx
           if (nsh == 1) then
                nf = nfold(ix,iy)%before
           else
                nf = nfold(ix,iy)%after
           end if
           if (nf > 0) then
                minfold=min(nf,minfold)
                maxfold=max(nf,maxfold)
                nsum   =    nsum +nf
                nfull  =    nfull+1
                do io=1,no
                do ia=1,na
                     if (nsh == 1) then
                          number = kfold(ix,iy,io,ia)%before
                     else
                          number = kfold(ix,iy,io,ia)%after
                     end if
                     minfold2(io,ia)=min(number,minfold2(io,ia))
                     maxfold2(io,ia)=max(number,maxfold2(io,ia))
                     nsum2   (io,ia)=           nsum2   (io,ia)+number
                end do
                end do
                do io=1,no
                     call flexbin_get_offset_fold  &
                              (nx,ny,no,na,kfold,nsh,ix,iy,io,   number)
                     minfold3(io)=min(number,minfold3(io))
                     maxfold3(io)=max(number,maxfold3(io))
                     nsum3   (io)=           nsum3   (io)+number
                     missing=off_fold(io)-number
                     if (missing > 0) then
                          nbdef3(io)=nbdef3(io)+1
                          ntdef3(io)=ntdef3(io)+missing
                     end if
                end do
                do ia=1,na
                     call flexbin_get_azimuth_fold  &
                              (nx,ny,no,na,kfold,nsh,ix,iy,ia,   number)
                     minfold4(ia)=min(number,minfold4(ia))
                     maxfold4(ia)=max(number,maxfold4(ia))
                     nsum4   (ia)=           nsum4   (ia)+number
                     missing=azmth_fold(ia)-number
                     if (missing > 0) then
                          nbdef4(ia)=nbdef4(ia)+1
                          ntdef4(ia)=ntdef4(ia)+missing
                     end if
                end do
           end if
      end do
      end do

!--------- get average folds:

      if (nfull > 0) then
           average=float(nsum)/float(nfull)
           do io=1,no
           do ia=1,na
                average2(io,ia)=float(nsum2(io,ia))/float(nfull)
           end do
           end do
           do io=1,no
                average3(io)=float(nsum3(io))/float(nfull)
           end do
           do ia=1,na
                average4(ia)=float(nsum4(ia))/float(nfull)
           end do
      end if

!--------- print fold statistics:

      lun = pc_get_lun()

      write(lun,*) 'MINIMUM CMP FOLD = ',minfold
      write(lun,*) 'MAXIMUM CMP FOLD = ',maxfold
      write(lun,*) 'AVERAGE CMP FOLD = ',average

      write(lun,*) ' '
      write(lun,*) 'MINIMUM FOLD IN EACH OFFSET/AZIMUTH SUB-BIN:'
      write(lun,1000) (ia,ia=1,na)
      do io=1,no
           write(lun,2000) io,(minfold2(io,ia),ia=1,na)
      end do

      write(lun,*) ' '
      write(lun,*) 'MAXIMUM FOLD IN EACH OFFSET/AZIMUTH SUB-BIN:'
      write(lun,1000) (ia,ia=1,na)
      do io=1,no
           write(lun,2000) io,(maxfold2(io,ia),ia=1,na)
      end do

      write(lun,*) ' '
      write(lun,*) 'AVERAGE FOLD IN EACH OFFSET/AZIMUTH SUB-BIN:'
      write(lun,1000) (ia,ia=1,na)
      do io=1,no
           write(lun,3000) io,(average2(io,ia),ia=1,na)
      end do

!--------- format statements:

1000  format ('  azimuth index -->',10(i5,':'))
2000  format ('  offset index',i3,': ',10i6)
3000  format ('  offset index',i3,': ',10f6.1)
4000  format (5x,'minimum maximum',44x,'-------fold-------   -deficiencies-')
5000  format (i4,2f8.0,i8,f14.2,f15.2,i10,i6,f9.2,i8,i9)

!--------- print offset statistics:

      write(lun,*) ' '
      write(lun,*) 'STATISTICS FOR EACH OFFSET SUB-BIN (all azimuths):'
      write(lun,4000)
      write(lun,*) '    ----OFFSETS----', &
                   '   OFF_FOLD   OFF_DIST_INL   OFF_DIST_CRL', &
                   '   min   max  average   #bins  #traces'
      do io=1,no
              if (io < no) then
                    maxval = offsets(io+1) - 1.0
              else
                    maxval = 99999.0
              end if
              write(lun,5000) io,offsets(io),maxval,off_fold(io),      &
                              off_dist_inl(io),off_dist_crl(io),       &
                              minfold3(io),maxfold3(io),average3(io),  &
                              nbdef3(io),ntdef3(io)
      end do

!--------- print azimuth statistics:

      write(lun,*) ' '
      write(lun,*) 'STATISTICS FOR EACH AZIMUTH SUB-BIN (all offsets):'
      write(lun,4000)
      write(lun,*) '    ----AZIMUTHS---', &
                   ' AZMTH_FOLD AZMTH_DIST_INL AZMTH_DIST_CRL', &
                   '   min   max  average   #bins  #traces'
      do ia=1,na
              if (ia < na) then
                    maxval = azimuths(ia+1) - 1.0
              else
                    maxval = azimuths(1) - 1.0 + 360.0
              end if
              write(lun,5000) ia,azimuths(ia),maxval,azmth_fold(ia),   &
                              azmth_dist_inl(ia),azmth_dist_crl(ia),   &
                              minfold4(ia),maxfold4(ia),average4(ia),  &
                              nbdef4(ia),ntdef4(ia)
      end do
      return
      end subroutine flexbin_print_statistics


!!----------------------- flexbin get boundary ----------------------------!!
!!----------------------- flexbin get boundary ----------------------------!!
!!----------------------- flexbin get boundary ----------------------------!!


      subroutine flexbin_get_boundary (nx,ny,no,na,nfold,            &
                             off_fold,  off_dist_inl,  off_dist_crl, &
                           azmth_fold,azmth_dist_inl,azmth_dist_crl, &
                           ixmina,ixmaxa,iymina,iymaxa)
      implicit none
      integer          ,intent(in)  :: nx,ny,no,na             ! args
      type(fold_struct),intent(in)  :: nfold(:,:)              ! args (nx,ny)
      integer          ,intent(in)  :: off_fold      (:)       ! args (no)
      real             ,intent(in)  :: off_dist_inl  (:)       ! args (no)
      real             ,intent(in)  :: off_dist_crl  (:)       ! args (no)
      integer          ,intent(in)  :: azmth_fold    (:)       ! args (na)
      real             ,intent(in)  :: azmth_dist_inl(:)       ! args (na)
      real             ,intent(in)  :: azmth_dist_crl(:)       ! args (na)
      integer          ,intent(out) :: ixmina(:),ixmaxa(:)     ! args (ny)
      integer          ,intent(out) :: iymina(:),iymaxa(:)     ! args (nx)
      integer :: ix,iy,io,ia,igrowx,igrowy,idistx,idisty,nf    ! local
      real    :: distx,disty                                   ! local

!--------- get actual boundary:

      do iy=1,ny
           ixmina(iy)=nx+1
           ixmaxa(iy)=0
      end do
      do ix=1,nx
           iymina(ix)=ny+1
           iymaxa(ix)=0
      end do
      do iy=1,ny
      do ix=1,nx
           nf = nfold(ix,iy)%before
           if (nf > 0) then
                iymina(ix)=min(iymina(ix),iy)
                iymaxa(ix)=max(iymaxa(ix),iy)
                ixmina(iy)=min(ixmina(iy),ix)
                ixmaxa(iy)=max(ixmaxa(iy),ix)
           end if
      end do
      end do

!--------- get maximum expandable distance:

      igrowx=99999
      igrowy=99999
      do io=1,no
      do ia=1,na
           call flexbin_get_excursion (off_fold,azmth_fold, &
                          off_dist_inl,azmth_dist_inl,io,ia,distx,idistx)
           call flexbin_get_excursion (off_fold,azmth_fold, &
                          off_dist_crl,azmth_dist_crl,io,ia,disty,idisty)
           igrowx=min(igrowx,idistx)
           igrowy=min(igrowy,idisty)
      end do
      end do

!--------- expand dimples in boundary:

      call flexbin_expand_boundary (nx,igrowx,iymina,iymaxa)
      call flexbin_expand_boundary (ny,igrowy,ixmina,ixmaxa)
      return
      end subroutine flexbin_get_boundary


!!--------------------- flexbin expand boundary ---------------------------!!
!!--------------------- flexbin expand boundary ---------------------------!!
!!--------------------- flexbin expand boundary ---------------------------!!


      subroutine flexbin_expand_boundary (ny,igrowy,ixmina,ixmaxa)

      implicit none
      integer,intent(in)    :: ny,igrowy                 ! arguments
      integer,intent(inout) :: ixmina(:),ixmaxa(:)       ! arguments (ny)
      integer :: ixmin1,ixmin2,ixmax1,ixmax2,iy,i        ! local

      do iy=1+igrowy,ny-igrowy
           ixmin1=ixmina(iy)
           ixmin2=ixmina(iy)
           ixmax1=ixmaxa(iy)
           ixmax2=ixmaxa(iy)
           do i=1,igrowy  
                ixmin1=min(ixmina(iy-i),ixmin1)
                ixmin2=min(ixmina(iy+i),ixmin2)
                ixmax1=max(ixmaxa(iy-i),ixmax1)
                ixmax2=max(ixmaxa(iy+i),ixmax2)
           end do
           ixmina(iy)=min(ixmina(iy),max(ixmin1,ixmin2))
           ixmaxa(iy)=max(ixmaxa(iy),min(ixmax1,ixmax2))
      end do
      return
      end subroutine flexbin_expand_boundary


!!------------------------- flexbin do work -------------------------------!!
!!------------------------- flexbin do work -------------------------------!!
!!------------------------- flexbin do work -------------------------------!!

!!!!!!!!! put into kfold the fold after duplicate traces are included.
!!!!!!!!! use duplication criteria to get ndup.
!!!!!!!!! allocate and fill in dup array.

!!!!!!! kfold%before: old fold is input.
!!!!!!! kfold%after : new fold is output.
!!!!!!! key%io:   used for scratch space (flag).
!!!!!!! key%ia:   used for scratch space (numdup).


      subroutine flexbin_do_work (max_dup,                     &  ! input
                      off_fold,  off_dist_inl,  off_dist_crl,  &  ! input
                    azmth_fold,azmth_dist_inl,azmth_dist_crl,  &  ! input
                    ntraces,key,bin,                           &  ! input
                    nx,ny,no,na,kpoint,nfold,                  &  ! input
                    kfold,                                     &  ! update
                    ndup,dup,error)                               ! output

      implicit none
      integer          ,intent(in)    :: max_dup           ! args
      integer          ,intent(in)    :: ntraces           ! args
      integer          ,intent(in)    :: nx,ny,no,na       ! args
      integer          ,intent(in)    :: off_fold      (:) ! args (no)
      real             ,intent(in)    :: off_dist_inl  (:) ! args (no)
      real             ,intent(in)    :: off_dist_crl  (:) ! args (no)
      integer          ,intent(in)    :: azmth_fold    (:) ! args (na)
      real             ,intent(in)    :: azmth_dist_inl(:) ! args (na)
      real             ,intent(in)    :: azmth_dist_crl(:) ! args (na)
      type(key_struct) ,intent(inout) :: key(:)            ! args (ntraces)
      type(bin_struct) ,intent(in)    :: bin(:)            ! args (ntraces)
      type(ktr_wrapper),intent(in)    :: kpoint(:,:,:,:)   ! args (nx,ny,no,na)
      type(fold_struct),intent(in)    :: nfold(:,:)        ! args (nx,ny)
      type(fold_struct),intent(inout) :: kfold(:,:,:,:)    ! args (nx,ny,no,na)
      type(triplesort_ints)  ,pointer :: dup(:)            ! args (ndup)
      integer          ,intent(out)   :: ndup              ! args
      logical          ,intent(out)   :: error             ! args
      integer                         ::      lundup,idup,ier ! local
      type(triplesort_ints)           :: dup2                           ! local
      integer             :: ix,iy,icmp,lfold2,lfold3,need2,need3,need  ! local
      integer             :: io,ia,ncreated,icreated,i,itrace           ! local
      integer             :: nsatisfy,lun,keepfold                      ! local
      integer,allocatable :: created(:)                                 ! local
      integer,allocatable :: ixmina(:),ixmaxa(:)                        ! local
      integer,allocatable :: iymina(:),iymaxa(:)                        ! local

!--------- get started:

      if (debug) then
           lun = pc_get_lun()
           write(lun,*) 'do_work max_dup        = ',max_dup
           write(lun,*) 'do_work off_fold       = ',off_fold
           write(lun,*) 'do_work off_dist_inl   = ',off_dist_inl
           write(lun,*) 'do_work off_dist_crl   = ',off_dist_crl
           write(lun,*) 'do_work azmth_fold     = ',azmth_fold
           write(lun,*) 'do_work azmth_dist_inl = ',azmth_dist_inl
           write(lun,*) 'do_work azmth_dist_crl = ',azmth_dist_crl
           write(lun,*) 'do_work ntraces        = ',ntraces
           write(lun,*) 'do_work key            = ',key
           write(lun,*) 'do_work bin            = ',bin
           write(lun,*) 'do_work nx,ny,no,na    = ',nx,ny,no,na
           write(lun,*) 'do_work kfold          = ',kfold
  !!!!     write(lun,*) 'do_work kpoint         = ',kpoint
      end if

      ndup   = 0
      lundup = 0

      key(:)%io = 0    ! flag.
      key(:)%ia = 0    ! numdup.

!--------- get space for list of duplicate traces which have been created:
!--------- get space for list of traces which can satisfy a deficiency:

      call flexbin_get_ncreated (no,na,off_fold,azmth_fold,   ncreated)
      call flexbin_get_nsatisfy (nx,ny,no,na,nfold,                        &
                                   off_fold,  off_dist_inl,  off_dist_crl, &
                                 azmth_fold,azmth_dist_inl,azmth_dist_crl, &
                                 nsatisfy)
      kfold(:,:,:,:)%after = kfold(:,:,:,:)%before

      allocate(created(ncreated),stat=ier)                                
      if (ier /= 0) go to 981

      call flexbin_open_temporary (lundup,error)
      if (error) go to 999

!--------- get boundary of survey:

      allocate(ixmina(ny),stat=ier) ; if (ier /= 0) go to 983
      allocate(ixmaxa(ny),stat=ier) ; if (ier /= 0) go to 984
      allocate(iymina(nx),stat=ier) ; if (ier /= 0) go to 985
      allocate(iymaxa(nx),stat=ier) ; if (ier /= 0) go to 986

      call flexbin_get_boundary (nx,ny,no,na,nfold,                          &
                                   off_fold,  off_dist_inl,  off_dist_crl,   &
                                 azmth_fold,azmth_dist_inl,azmth_dist_crl,   &
                                 ixmina,ixmaxa,iymina,iymaxa)

!--------- begin loop over cmp bins:

      do iy=1,ny
      do ix=1,nx
      if (ix >= ixmina(iy) .and. ix <= ixmaxa(iy).and.      &
          iy >= iymina(ix) .and. iy <= iymaxa(ix)) then     ! within boundary
      icreated=0

!--------- work on each offset/azimuth sub-bin separately:

      do io=1,no
      do ia=1,na
           call flexbin_get_offset_fold      &
                              (nx,ny,no,na,kfold,2,ix,iy,io,   lfold2)
           call flexbin_get_azimuth_fold     &
                              (nx,ny,no,na,kfold,2,ix,iy,ia,   lfold3)
           need2    = off_fold(io)-lfold2
           need3    = azmth_fold (ia)-lfold3
           need     = min(need2,need3)
           keepfold = max(off_fold(io),azmth_fold(ia))
           call flexbin_search_neighbors (nx,ny,no,na,kpoint,      & ! input
                        off_fold  ,  off_dist_inl,  off_dist_crl,  & ! input
                        azmth_fold,azmth_dist_inl,azmth_dist_crl,  & ! input
                        ntraces,bin,                               & ! input
                        max_dup,ix,iy,io,ia,                       & ! input
                        ncreated,nsatisfy,need,keepfold,           & ! input
                        icreated,created,kfold,key,                & ! updated
                        error)                                       ! output
           if (error) go to 987
      end do   ! IA
      end do   ! IO

!--------- work on each offset sub-bin separately:

      do io=1,no
      do ia=1,na
           call flexbin_get_offset_fold   &
                             (nx,ny,no,na,kfold,2,ix,iy,io,   lfold2)
           need     = off_fold(io)-lfold2
           keepfold = off_fold(io)
           call flexbin_search_neighbors (nx,ny,no,na,kpoint,      & ! input
                        off_fold  ,  off_dist_inl,  off_dist_crl,  & ! input
                        azmth_fold,azmth_dist_inl,azmth_dist_crl,  & ! input
                        ntraces,bin,                               & ! input
                        max_dup,ix,iy,io,ia,                       & ! input
                        ncreated,nsatisfy,need,keepfold,           & ! input
                        icreated,created,kfold,key,                & ! updated
                        error)                                       ! output
           if (error) go to 988
      end do   ! IA
      end do   ! IO

!--------- work on each azimuth sub-bin separately:

      do io=1,no
      do ia=1,na
           call flexbin_get_azimuth_fold   &
                             (nx,ny,no,na,kfold,2,ix,iy,ia,   lfold3)
           need     = azmth_fold(ia)-lfold3
           keepfold = azmth_fold(ia)
           call flexbin_search_neighbors (nx,ny,no,na,kpoint,      & ! input
                        off_fold  ,  off_dist_inl,  off_dist_crl,  & ! input
                        azmth_fold,azmth_dist_inl,azmth_dist_crl,  & ! input
                        ntraces,bin,                               & ! input
                        max_dup,ix,iy,io,ia,                       & ! input
                        ncreated,nsatisfy,need,keepfold,           & ! input
                        icreated,created,kfold,key,                & ! updated
                        error)                                       ! output
           if (error) go to 989
      end do   ! IA
      end do   ! IO

!--------- go thru list of duplicate traces for this cmp bin:

      icmp=ix+(iy-1)*nx
      do i=1,icreated
           itrace=created(i)
           ndup=ndup+1
           dup2%primary   = key(itrace)%hd9
           dup2%secondary = key(itrace)%hd10
           dup2%tertiary  = icmp
           call flexbin_write_temporary (lundup,dup2,error)
           if (error) go to 999
           key(itrace)%io = 0                             ! unset flag.
      end do

!--------- finish loop over cmp bins:

      end if   ! within boundary
      end do   ! IX
      end do   ! IY

!--------- deallocate arrays:

      deallocate(ixmina)                                   
      deallocate(ixmaxa)                                   
      deallocate(iymina)                                   
      deallocate(iymaxa)                                   
      deallocate(created)                                  

!--------- get dup array from temporary file:

      call pc_print ('FLEXBIN:',ndup,'duplicate traces will be created.')

      allocate(dup(ndup),stat=ier)                                  
      if (ier /= 0) go to 991

      rewind lundup
      do idup=1,ndup
           call flexbin_read_temporary (lundup,dup(idup),error)
           if (error) go to 999
      end do
      call flexbin_close_temporary (lundup)
      error = .false.
      return

!--------- error returns:

981   call pc_print ('FLEXBIN: ERROR ALLOCATING CREATED',ncreated)
      go to 999
983   call pc_print ('FLEXBIN: ERROR ALLOCATING IXMINA',ny)
      go to 999
984   call pc_print ('FLEXBIN: ERROR ALLOCATING IXMAXA',ny)
      go to 999
985   call pc_print ('FLEXBIN: ERROR ALLOCATING IYMINA',nx)
      go to 999
986   call pc_print ('FLEXBIN: ERROR ALLOCATING IYMAXA',nx)
      go to 999
987   call pc_print ('FLEXBIN: FATAL ERROR - first search')
      go to 999
988   call pc_print ('FLEXBIN: FATAL ERROR - second search')
      go to 999
989   call pc_print ('FLEXBIN: FATAL ERROR - third search')
      go to 999
991   call pc_print ('FLEXBIN: ERROR ALLOCATING DUP',ndup)

999   call flexbin_close_temporary (lundup)
      call pc_print ('FLEXBIN: FATAL ERROR IN FLEXBIN_DO_WORK')
      error = .true.
      return
      end subroutine flexbin_do_work


!!--------------------- flexbin search neighbors --------------------------!!
!!--------------------- flexbin search neighbors --------------------------!!
!!--------------------- flexbin search neighbors --------------------------!!

!!!!!!! kfold%before: old fold is input.
!!!!!!! kfold%after : new fold is updated.
!!!!!!! key:   component io (used for flag  ) updated.
!!!!!!! key:   component ia (used for numdup) updated.


      subroutine flexbin_search_neighbors (nx,ny,no,na,kpoint,     &  ! input
                        off_fold  ,  off_dist_inl,  off_dist_crl,  &  ! input
                        azmth_fold,azmth_dist_inl,azmth_dist_crl,  &  ! input
                        ntraces,bin,                               &  ! input
                        max_dup,ix,iy,io,ia,                       &  ! input
                        ncreated,nsatisfy,need,keepfold,           &  ! input
                        icreated,created,kfold,key,                &  ! updated
                        error)                                        ! output

      implicit none
      integer          ,intent(in)    :: nx,ny,no,na       ! args
      integer          ,intent(in)    :: max_dup,ntraces   ! args
      integer          ,intent(in)    :: off_fold      (:) ! args (no)
      real             ,intent(in)    :: off_dist_inl  (:) ! args (no)
      real             ,intent(in)    :: off_dist_crl  (:) ! args (no)
      integer          ,intent(in)    :: azmth_fold    (:) ! args (na)
      real             ,intent(in)    :: azmth_dist_inl(:) ! args (na)
      real             ,intent(in)    :: azmth_dist_crl(:) ! args (na)
      type(bin_struct) ,intent(in)    :: bin(:)            ! args (ntraces)
      type(ktr_wrapper),intent(in)    :: kpoint(:,:,:,:)   ! args (nx,ny,no,na)
      integer          ,intent(in)    :: ix,iy,io,ia       ! args
      integer          ,intent(in)    :: ncreated,nsatisfy ! args
      integer          ,intent(in)    :: need              ! args
      integer          ,intent(in)    :: keepfold          ! args
      integer          ,intent(inout) :: icreated          ! args (ncreated)
      integer          ,intent(inout) :: created(:)        ! args (ncreated)
      type(fold_struct),intent(inout) :: kfold(:,:,:,:)    ! args (nx,ny,no,na)
      type(key_struct) ,intent(inout) :: key(:)            ! args (ntraces)
      logical          ,intent(out)   :: error             ! args
      type(foursort_ints),allocatable :: satisfy(:)                 ! local
      integer           :: ix2,iy2,idistance,isatisfy,i,ier         ! local
      integer           :: ixmin,ixmax,iymin,iymax,kfold3,itrace    ! local
      real              :: xmin,xmax,ymin,ymax,xgrid,ygrid          ! local
      real              :: xdiff,ydiff,distance                     ! local
      integer           ::         numdup,iflag,lun,available ! local

!---------- get range of neighboring bins to search through.

      lun = pc_get_lun()

      error = .false.
      if (need <= 0) return
      call flexbin_get_minmax &
                     (off_fold,azmth_fold,off_dist_inl,azmth_dist_inl, &
                      io,ia,ix,nx,xmin,xmax,ixmin,ixmax)
      call flexbin_get_minmax &
                     (off_fold,azmth_fold,off_dist_crl,azmth_dist_crl, &
                      io,ia,iy,ny,ymin,ymax,iymin,iymax)
      if (debug) then
               write(lun,*) 'ix,xmin,xmax,ixmin,ixmax = ',  &
                             ix,xmin,xmax,ixmin,ixmax
               write(lun,*) 'iy,ymin,ymax,iymin,iymax = ',  &
                             iy,ymin,ymax,iymin,iymax
      end if

      allocate (satisfy(nsatisfy),stat=ier)
      if (ier /= 0) go to 993

!---------- go thru neighboring bins and get list of traces
!---------- which can satisfy this deficiency.

      isatisfy=0
      do iy2=iymin,iymax
      do ix2=ixmin,ixmax
      if (iy2 /= iy .or. ix2 /= ix) then     ! only if not middle bin.
           kfold3 = kfold(ix2,iy2,io,ia)%before
           if (debug) then
                write(lun,*) 'iy2,ix2 = ',iy2,ix2
           end if
           if (max_dup == 0) then
                available = 0
                do i=1,kfold3
                     itrace = kpoint(ix2,iy2,io,ia)%ktr(i)                  
                     numdup = key(itrace)%ia
                     if (numdup == 0) available = available + 1
                end do
                if (available <= keepfold) cycle
                                          ! no available traces in this bin.
           end if
           do i=1,kfold3
                itrace=kpoint(ix2,iy2,io,ia)%ktr(i)                  
                numdup = key(itrace)%ia
                iflag  = key(itrace)%io
                xgrid  = bin(itrace)%xgrid
                ygrid  = bin(itrace)%ygrid
                if (debug) then
                     write(lun,*) 'i,itrace = ',i,itrace,          &
                                  ' numdup,iflag,xgrid,ygrid = ',  &
                                    numdup,iflag,xgrid,ygrid
                end if
                if (xgrid  >= xmin .and.  &
                    ygrid  >= ymin .and.  &
                    xgrid  <= xmax .and.  &
                    ygrid  <= ymax .and.  &
                    iflag  == 0    .and.  &
                    numdup <  max(max_dup,1)) then
                      xdiff=xgrid-ix
                      ydiff=ygrid-iy
                      distance=sqrt(xdiff*xdiff+ydiff*ydiff)
                      idistance=nint(100.0*distance)
                      isatisfy=isatisfy+1
                      if (isatisfy > nsatisfy) go to 995
                      satisfy(isatisfy)%one   = idistance
                      satisfy(isatisfy)%two   = itrace
                      satisfy(isatisfy)%three = ix2
                      satisfy(isatisfy)%four  = iy2
                      if (debug) then
                           write(lun,*) &
                              'i,isatisfy,idistance,itrace,distance = ',  &
                               i,isatisfy,idistance,itrace,distance
                      end if
                end if
           end do   ! I
      end if   ! only if not middle bin.
      end do   ! IX2
      end do   ! IY2

!---------- sort the list of traces by distance.

      if (isatisfy == 0) then
           deallocate (satisfy)
           return
      end if
      call foursort_sort (satisfy,isatisfy)

!---------- satisfy the deficiencies by distance.

      isatisfy=min(isatisfy,need)
      if (icreated+isatisfy > ncreated) go to 998
      do i=1,isatisfy
           itrace = satisfy(i)%two
           ix2    = satisfy(i)%three
           iy2    = satisfy(i)%four
           if (debug) then
                call pc_print ('i,itrace =',i,'',itrace)
           end if
           numdup = key(itrace)%ia
           iflag  = key(itrace)%io
           if (iflag  /=       0       ) go to 996
           if (numdup >= max(max_dup,1)) go to 997
           numdup = numdup + 1                          ! increment numdup
           iflag  = 1                                   ! set flag
           key(itrace)%ia = numdup
           key(itrace)%io = iflag
           if (max_dup == 0) then
                kfold(ix2,iy2,io,ia)%after = kfold(ix2,iy2,io,ia)%after - 1
           end if
           icreated=icreated+1
           created(icreated)=itrace
           if (debug) then
                  write (lun,*) 'itrace,icreated,numdup,iflag = ',  &
                                 itrace,icreated,numdup,iflag
           end if
      end do

!---------- update the fold and return:

      deallocate (satisfy)

      kfold(ix,iy,io,ia)%after = kfold(ix,iy,io,ia)%after + isatisfy
      error = .false.
      return

!---------- error returns.

993   call pc_print ('FLEXBIN: ERROR ALLOCATING SATISFY',nsatisfy)
      go to 999
995   call pc_print ('FLEXBIN: PROGRAMMER ERROR - NSATISFY (',  &
                                                  nsatisfy,') TOO SMALL')
      go to 999
996   call pc_print ('FLEXBIN: PROGRAMMER ERROR - FLAG SHOULD NOT BE SET')
      go to 999
997   call pc_print ('FLEXBIN: PROGRAMMER ERROR - NUMDUP SHOULD NOT BE&
                                                  & >= MAX_DUP')
      go to 999
998   call pc_print ('FLEXBIN: PROGRAMMER ERROR - NCREATED (',  &
                                                  ncreated,') TOO SMALL')

999   call pc_print ('FLEXBIN: FATAL ERROR IN FLEXBIN_SEARCH_NEIGHBORS')
      error = .true.
      return
      end subroutine flexbin_search_neighbors


!!------------------------ flexbin get minmax -----------------------------!!
!!------------------------ flexbin get minmax -----------------------------!!
!!------------------------ flexbin get minmax -----------------------------!!

!!!!!!!!! first gets maximum allowed excursion distance.
!!!!!!!!! then returns minimum and maximum bin indices to search.


      subroutine flexbin_get_minmax &
                       (off_fold,azmth_fold,off_dist,azmth_dist, &
                        io,ia,ix,nx,xmin,xmax,ixmin,ixmax)
      implicit none
      integer,intent(in)  :: off_fold  (:)                 ! arguments (no)
      integer,intent(in)  :: azmth_fold(:)                 ! arguments (na)
      real   ,intent(in)  :: off_dist  (:)                 ! arguments (no)
      real   ,intent(in)  :: azmth_dist(:)                 ! arguments (na)
      integer,intent(in)  :: io,ia,ix,nx                   ! arguments
      real   ,intent(out) :: xmin,xmax                     ! arguments
      integer,intent(out) :: ixmin,ixmax                   ! arguments
      real                :: distx                         ! local
      integer             :: idistx                        ! local

      call flexbin_get_excursion  &
               (off_fold,azmth_fold,off_dist,azmth_dist,io,ia,distx,idistx)
      xmin =ix-distx
      xmax =ix+distx
      ixmin=max(ix-idistx,1)
      ixmax=min(ix+idistx,nx)
      return
      end subroutine flexbin_get_minmax


!!------------------------ flexbin get excursion ---------------------------!!
!!------------------------ flexbin get excursion ---------------------------!!
!!------------------------ flexbin get excursion ---------------------------!!

!!!!!!!!! gets maximum allowed excursion distance.
!!!!!!!!! the maximum allowed excursion distance is the smallest
!!!!!!!!!   of those specified for the given offset and given azimuth.
!!!!!!!!! but if the specified required fold is zero, the specified
!!!!!!!!!   distance is irrelevant and ignored.


      subroutine flexbin_get_excursion &
                  (off_fold,azmth_fold,off_dist,azmth_dist,io,ia,distx,idistx)
      implicit none
      integer,intent(in)  :: off_fold  (:)                 ! arguments (no)
      integer,intent(in)  :: azmth_fold(:)                 ! arguments (na)
      real   ,intent(in)  :: off_dist  (:)                 ! arguments (no)
      real   ,intent(in)  :: azmth_dist(:)                 ! arguments (na)
      integer,intent(in)  :: io,ia                         ! arguments
      real   ,intent(out) :: distx                         ! arguments
      integer,intent(out) :: idistx                        ! arguments
      real                :: excursion                     ! local

      if (off_fold(io) > 0 .and. azmth_fold(ia) > 0) then
           excursion = min(off_dist(io),azmth_dist(ia))
      else if (off_fold(io) > 0) then
           excursion = off_dist(io)
      else if (azmth_fold(ia) > 0) then
           excursion = azmth_dist(ia)
      else
           excursion = 0.0
      end if
      distx =0.5+excursion
      idistx=distx+0.49
      return
      end subroutine flexbin_get_excursion


!!------------------------ flexbin get ncreated ---------------------------!!
!!------------------------ flexbin get ncreated ---------------------------!!
!!------------------------ flexbin get ncreated ---------------------------!!

!!!!!! ncreated = sum of all fold values in off_fold and azmth_fold arrays.
!!!!!! ncreated should be more than enough for keeping list of duplicate
!!!!!! traces added to a single cmp gather.


      subroutine flexbin_get_ncreated (no,na,off_fold,azmth_fold, ncreated)

      implicit none
      integer,intent(in)  :: no,na                 ! arguments
      integer,intent(in)  :: off_fold  (:)         ! arguments (no)
      integer,intent(in)  :: azmth_fold(:)         ! arguments (na)
      integer,intent(out) :: ncreated              ! arguments
      integer             :: io,ia                 ! local

      ncreated=0
      do io=1,no
           ncreated=ncreated+off_fold(io)
      end do
      do ia=1,na
           ncreated=ncreated+azmth_fold(ia)
      end do
      call pc_print ('FLEXBIN: making space for temporary partial lists of',  &
                           ncreated,'created traces.')
      call pc_print ('        (sum of fold in OFF_FOLD and AZMTH_FOLD arrays)')
      return
      end subroutine flexbin_get_ncreated


!!------------------------ flexbin get nsatisfy ----------------------------!!
!!------------------------ flexbin get nsatisfy ----------------------------!!
!!------------------------ flexbin get nsatisfy ----------------------------!!

!!!!!! nsatisfy = sum of all fold values in one super-search bin using
!!!!!!               off_dist_inl,off_dist_crl,azmth_dist_inl,azmth_dist_crl.
!!!!!! nsatisfy should be more than enough for keeping list of all
!!!!!! traces in neighboring bins which might satisfy a deficiency.


      subroutine flexbin_get_nsatisfy (nx,ny,no,na,nfold,          &
                           off_fold, off_dist_inl,  off_dist_crl,  &
                         azmth_fold,azmth_dist_inl,azmth_dist_crl,   nsatisfy)

      implicit none
      integer          ,intent(in)  :: nx,ny,no,na          ! arguments
      type(fold_struct),intent(in)  :: nfold(:,:)           ! arguments (nx,ny)
      integer          ,intent(in)  :: off_fold      (:)    ! arguments (no)
      real             ,intent(in)  :: off_dist_inl  (:)    ! arguments (no)
      real             ,intent(in)  :: off_dist_crl  (:)    ! arguments (no)
      integer          ,intent(in)  :: azmth_fold    (:)    ! arguments (na)
      real             ,intent(in)  :: azmth_dist_inl(:)    ! arguments (na)
      real             ,intent(in)  :: azmth_dist_crl(:)    ! arguments (na)
      integer          ,intent(out) :: nsatisfy             ! arguments
      real    :: distx,disty                                ! local
      integer :: idistx,idisty,ix,iy,io,ia,nf               ! local
      integer :: ixmax,iymax,ixrange,iyrange,ncmp,maxfold   ! local

!--------- get maximum number of cmp bins to search over:

      ixmax=0
      iymax=0
      do io=1,no
      do ia=1,na
           call flexbin_get_excursion (off_fold,azmth_fold, &
                          off_dist_inl,azmth_dist_inl,io,ia,distx,idistx)
           call flexbin_get_excursion (off_fold,azmth_fold, &
                          off_dist_crl,azmth_dist_crl,io,ia,disty,idisty)
           ixmax=max(ixmax,idistx)
           iymax=max(iymax,idisty)
      end do
      end do
      ixrange=2*ixmax+1
      iyrange=2*iymax+1
      ncmp=ixrange*iyrange-1

!--------- get maximum fold in any cmp bin:

      maxfold=0
      do ix=1,nx
      do iy=1,ny
           nf = nfold(ix,iy)%before
           maxfold=max(nf,maxfold)
      end do
      end do

!--------- get maximum number of traces in cmp bins to search over:

      nsatisfy=ncmp*maxfold
      call pc_print ('FLEXBIN: making space for temporary partial lists of',  &
                           nsatisfy,'satisfying traces.')
      call pc_print ('        (sum of all fold values in one super-search bin')
      call pc_print ('           using OFF_DIST_INL,OFF_DIST_CRL,')
      call pc_print ('           AZMTH_DIST_INL,AZMTH_DIST_CRL)')
      call pc_print ('        (maximum fold in any CMP bin =',maxfold,')')
      call pc_print ('        (size of super-search bin in X direction =',  &
                                          ixrange,'CMP bins)')
      call pc_print ('        (size of super-search bin in Y direction =',  &
                                          iyrange,'CMP bins)')
      return
      end subroutine flexbin_get_nsatisfy


!!----------------- flexbin print dup results -----------------------------!!
!!----------------- flexbin print dup results -----------------------------!!
!!----------------- flexbin print dup results -----------------------------!!


      subroutine flexbin_print_dup_results (ndup,dup)

      implicit none
      integer                ,intent(in) :: ndup           ! arguments
      type(triplesort_ints)  ,intent(in) :: dup(:)         ! arguments (ndup)
      integer       :: idup,kount,primary,secondary        ! local
      real          :: average                             ! local

      kount     = 0
      average   = 0.0
      primary   = 0
      secondary = 0
      do idup=1,ndup
           if (idup == 1 .or. dup(idup)%primary   /= primary .or.  &
                              dup(idup)%secondary /= secondary) then
               kount = kount + 1
               primary   = dup(idup)%primary
               secondary = dup(idup)%secondary
           end if
      end do
      if (kount > 0) average = float(ndup) / float(kount)
      call pc_print ('FLEXBIN:',kount,'traces will be duplicated')
      call pc_print ('            an average of',average,'times apiece')
      call pc_print ('            to produce',ndup,'duplicated traces.')
      return
      end subroutine flexbin_print_dup_results


!!----------------- flexbin print cmp results -----------------------------!!
!!----------------- flexbin print cmp results -----------------------------!!
!!----------------- flexbin print cmp results -----------------------------!!


      subroutine flexbin_print_cmp_results (nx,ny,nfold)

      implicit none
      integer          ,intent(in) :: nx,ny             ! arguments
      type(fold_struct),intent(in) :: nfold(:,:)        ! arguments (nx,ny)
      integer :: kount,isum,ix,iy,new,nf1,nf2           ! local
      real    :: average                                ! local

      kount   = 0
      isum    = 0
      average = 0.0
      do iy = 1,ny
      do ix = 1,nx
           nf1 = nfold(ix,iy)%before
           nf2 = nfold(ix,iy)%after
           new = nf2 - nf1
           if (new > 0) then
                kount = kount + 1
                isum  = isum + new
           end if
      end do
      end do
      if (kount > 0) average = float(isum) / float(kount)
      call pc_print ('FLEXBIN:',real(kount), &
             'CMPs will be augmented by an average of',average,'traces each.')
      return
      end subroutine flexbin_print_cmp_results


!!----------------------- flexbin get azimuth ------------------------------!!
!!----------------------- flexbin get offset -------------------------------!!
!!----------------------- flexbin get midpoint -----------------------------!!
!!----------------------- flexbin get grid ---------------------------------!!
!!----------------------- flexbin get survey -------------------------------!!


      subroutine flexbin_get_azimuth (hd11,hd12,hd14,hd15,   azimuth)
!     azimuth is measured from the y survey axis toward the x survey axis.
!     the y survey axis is usually considered to point north.
!     the x survey axis is usually considered to point east.
!     the lines commented out with code !GRID would measure the azimuth
!       relative to the grid coordinates rather than the survey coordinates.
!     the azimuth will be within this range: 0 <= azimuth < 360.

      implicit none
      double precision ,intent(in)  :: hd11,hd12,hd14,hd15      ! arguments
      double precision ,intent(out) :: azimuth                  ! arguments
!GRID type(grid_struct),intent(in)  :: grid                     ! arguments
      double precision              :: xdiff,ydiff              ! local
!GRID double precision              :: hd33,hd34,hd35,hd36      ! local

      xdiff = hd14 - hd11
      ydiff = hd15 - hd12
!GRID call grid_get_grid_coords (grid,  hd11,hd12,   hd33,hd34)    ! source
!GRID call grid_get_grid_coords (grid,  hd14,hd15,   hd35,hd36)    ! receiver
!GRID xdiff=hd35-hd33
!GRID ydiff=hd36-hd34
      if (xdiff == 0.0 .and. ydiff == 0.0) then
           azimuth = 0.0
      else
           azimuth = DEGREES_PER_RADIAN * atan2(xdiff,ydiff)
           if (azimuth <    0.0) azimuth = azimuth + 360.0
           if (azimuth >= 360.0) azimuth = azimuth - 360.0
      end if
      return
      end subroutine flexbin_get_azimuth



      subroutine flexbin_get_offset (hd11,hd12,hd14,hd15,   offset)

      implicit none
      double precision,intent(in)  :: hd11,hd12,hd14,hd15      ! arguments
      double precision,intent(out) :: offset                   ! arguments
      double precision             :: xdiff,ydiff              ! local

      xdiff=hd14-hd11
      ydiff=hd15-hd12
      offset=sqrt(xdiff*xdiff+ydiff*ydiff)
      return
      end subroutine flexbin_get_offset



      subroutine flexbin_get_midpoint (hd11,hd12,hd14,hd15,   x,y)

      implicit none
      double precision,intent(in)  :: hd11,hd12,hd14,hd15      ! arguments
      double precision,intent(out) :: x,y                      ! arguments

      x=0.5*(hd11+hd14)                        ! CMP X survey coord
      y=0.5*(hd12+hd15)                        ! CMP Y survey coord
      return
      end subroutine flexbin_get_midpoint


!!------------------------- flexbin dup ------------------------------------!!
!!------------------------- flexbin dup ------------------------------------!!
!!------------------------- flexbin dup ------------------------------------!!

!!!  trace number i is duplicated as often as necessary and added
!!!    to the end of the header and trace arrays.
!!!  lasti is the last trace number existing in the trace array.
!!!  lasti is incremented each time trace number i is duplicated.
!!!  lasti is not changed if traces are moved instead of duplicated.
!!!  the array dup is assumed to be sorted into increasing order
!!!    of the keys stored in the dup array.
!!!  uses dup(ndup) which contains the key for the trace to duplicate,
!!!    and the cmp number for the location of the duplicated trace.
!!!  puts a flag into header word 60.
!!!  max_dup is used only to make sure the trace is not duplicated too often.
!!!  max_dup is zero if the trace is to be moved instead of duplicated.
!!!  duplicated is set to the number of traces actually duplicated or moved.

      subroutine flexbin_dup (nwih,ndpt,grid,i,hd,tr,lasti,ndup,dup,       &
                              xfirst,yfirst,inl_inc,crl_inc,nx,ny,max_dup, &
                              reset_headers,duplicated,error)

      implicit none
      integer                ,intent(in)    :: nwih,ndpt                ! args
      type(grid_struct)      ,intent(in)    :: grid                     ! args
      integer                ,intent(in)    :: i,ndup,nx,ny,max_dup     ! args
      integer                ,intent(inout) :: lasti                    ! args
      double precision       ,intent(inout) :: hd(:,:)                  ! args
      real                   ,intent(inout) :: tr(:,:)                  ! args
      type(triplesort_ints)  ,intent(in)    :: dup(:)                   ! args
      real                   ,intent(in)    :: xfirst,yfirst            ! args
      real                   ,intent(in)    :: inl_inc,crl_inc          ! args
      logical                ,intent(in)    :: reset_headers            ! args
      integer                ,intent(out)   :: duplicated               ! args
      logical                ,intent(out)   :: error                    ! args
      integer                               :: ja,jb,idup,icmp,ix,iy    ! local
      integer                               :: lun,newi,jc,jd,j         ! local
      type(triplesort_ints)                 :: keywant                  ! local

      lun = pc_get_lun()
      duplicated = 0

      keywant%primary   = nint(hd( 9,i))
      keywant%secondary = nint(hd(10,i))
      keywant%tertiary  = 0
      call triplesort_search (ndup,dup,keywant,LEVEL,   ja,jb)
      if (debug) then
           write(lun,*) 'flexbin_dup:  i = ',i,'  keywant = ',keywant, &
                        '  ja,jb = ',ja,jb
      end if

      if (ja == 0) then
           hd(60,i)=0       ! trace will not be duplicated (or moved).
           error = .false.
           return
      end if

      if (max_dup > 0) then
           if (jb-ja+1 > max_dup) go to 998
           hd(60,i)=jb-ja+1      ! number of times trace is being duplicated.
      else
           if (jb > ja) go to 997
      end if

      do idup=ja,jb

           duplicated = duplicated+1
           if (max_dup > 0) then                    ! duplicate the trace.
                lasti = lasti+1
                newi  = lasti
                hd(1:nwih,newi) = hd(1:nwih,i)
                tr(1:ndpt,newi) = tr(1:ndpt,i)
           else                                     ! move the trace.
                newi = i
           end if

           hd(60,newi)=-1        ! flag indicating trace is a duplicate
                                 ! (or has been moved).

!!!!!! reset cmp coordinates:

           icmp       = dup(idup)%tertiary
           iy         = (icmp+nx-1)/nx
           ix         = icmp-(iy-1)*nx
           hd(7,newi) = xfirst + (ix-1)*inl_inc
           hd(8,newi) = yfirst + (iy-1)*crl_inc

           if (reset_headers) then
                call grid_get_survey_coords (grid, hd( 7,newi),hd( 8,newi), &
                                                   hd(17,newi),hd(18,newi))
           end if

!!!!!! reset source and receiver coordinates:

           if (reset_headers) then
                hd(33,newi) = hd(33,i) + hd( 7,newi) - hd( 7,i)
                hd(34,newi) = hd(34,i) + hd( 8,newi) - hd( 8,i)
                hd(35,newi) = hd(35,i) + hd( 7,newi) - hd( 7,i)
                hd(36,newi) = hd(36,i) + hd( 8,newi) - hd( 8,i)
                hd(11,newi) = hd(11,i) + hd(17,newi) - hd(17,i)
                hd(12,newi) = hd(12,i) + hd(18,newi) - hd(18,i)
                hd(14,newi) = hd(14,i) + hd(17,newi) - hd(17,i)
                hd(15,newi) = hd(15,i) + hd(18,newi) - hd(18,i)
           end if
      end do
      error = .false.
      return

!!!!!! an error has occurred:

997   call pc_print ('FLEXBIN: A TRACE HAS BEEN MOVED TOO MANY TIMES')
      call pc_print ('FLEXBIN: THERE WERE ',jb-ja+1,' MOVES')
      call pc_print ('FLEXBIN: THE MAXIMUM ALLOWED NUMBER IS ONE MOVE')
      go to 999
998   call pc_print ('FLEXBIN: A TRACE HAS BEEN DUPLICATED TOO MANY TIMES')
      call pc_print ('FLEXBIN: THERE WERE ',jb-ja+1,' DUPLICATIONS')
      call pc_print ('FLEXBIN: THE MAXIMUM ALLOWED NUMBER IS',max_dup)
      call pc_print ('FLEXBIN: THE ORIGINAL PLUS DUPLICATED TRACES ARE THESE:')
      if (debug) then
           jc=ja+lasti-jb
           jd=jb+lasti-jb
           write(lun,2222)  1,'         ',hd( 1,i),(hd( 1,j),j=jc,jd)
           write(lun,2222)  6,' offset  ',hd( 6,i),(hd( 6,j),j=jc,jd)
           write(lun,2222)  9,'         ',hd( 9,i),(hd( 9,j),j=jc,jd)
           write(lun,2222) 10,'         ',hd(10,i),(hd(10,j),j=jc,jd)
           write(lun,2222) 11,'src Xdist',hd(11,i),(hd(11,j),j=jc,jd)
           write(lun,2222) 12,'src Ydist',hd(12,i),(hd(12,j),j=jc,jd)
           write(lun,2222) 14,'rec Xdist',hd(14,i),(hd(14,j),j=jc,jd)
           write(lun,2222) 15,'rec Ydist',hd(15,i),(hd(15,j),j=jc,jd)
           write(lun,2222) 17,'CMP Xdist',hd(17,i),(hd(17,j),j=jc,jd)
           write(lun,2222) 18,'CMP Ydist',hd(18,i),(hd(18,j),j=jc,jd)
           write(lun,2222) 25,'  LAV    ',hd(25,i),(hd(25,j),j=jc,jd)
           write(lun,2222) 60,'         ',hd(60,i),(hd(60,j),j=jc,jd)
           write(lun,2222) 61,' azimuth ',hd(61,i),(hd(61,j),j=jc,jd)
           write(lun,2222) 33,'src Xgrid',hd(33,i),(hd(33,j),j=jc,jd)
           write(lun,2222) 34,'src Ygrid',hd(34,i),(hd(34,j),j=jc,jd)
           write(lun,2222) 35,'rec Xgrid',hd(35,i),(hd(35,j),j=jc,jd)
           write(lun,2222) 36,'rec Ygrid',hd(36,i),(hd(36,j),j=jc,jd)
           write(lun,2222)  7,'CMP Xgrid',hd( 7,i),(hd( 7,j),j=jc,jd)
           write(lun,2222)  8,'CMP Ygrid',hd( 8,i),(hd( 8,j),j=jc,jd)
2222       format (i4,1x,a10,1x,9f12.2/17x,9f12.2/17x,9f12.2)
      end if
999   call pc_error ('FLEXBIN: FLEXBIN_DUP ERROR FOR DUP TRACE',ja,'OF',ndup)
      error = .true.
      return
      end subroutine flexbin_dup


!!------------------ flexbin verify flexfile -------------------------------!!
!!------------------ flexbin verify flexfile -------------------------------!!
!!------------------ flexbin verify flexfile -------------------------------!!


      subroutine flexbin_verify_flexfile (pathname_flex,error)
      implicit none
      character(len=*),intent(in)   :: pathname_flex                ! arguments
      logical         ,intent(out)  :: error                        ! arguments
      integer                       :: ntraces                      ! local
      type(flexio_struct),pointer   :: flexio                       ! local
      integer                       :: key_errors,itrace,err,ier    ! local

      double precision              :: hd9,hd10,hd11,hd12,hd14,hd15 ! local
      type(triplesort_ints),pointer :: temp(:)                      ! local

!--------- get started:

      nullify (flexio)
      nullify (temp)

!--------- open, read, and close flexfile:

      call pc_print (' ')
      call pc_print ('FLEXBIN: VERIFYING FLEXFILE:')
      call flexio_open_old (flexio,pathname_flex,pc_get_lun(),ntraces,err)
      if (err /= FLEXIO_OK) go to 999

      allocate(temp(ntraces),stat=ier)                                   
      if (ier /= 0) go to 991

      do itrace=1,ntraces
           call flexio_read_old (flexio,itrace,hd9,hd10,hd11,hd12,  &
                                          hd14,hd15,err)
           if (err /= FLEXIO_OK) go to 999

           temp(itrace)%primary   = nint(hd9)
           temp(itrace)%secondary = nint(hd10)
           temp(itrace)%tertiary  = itrace
      end do
      call flexio_close_old (flexio)

!--------- sort and verify information:

      call triplesort_sort (temp,ntraces)
      key_errors   = 0
      do itrace=2,ntraces
           if (triplesort_compare(temp(itrace),temp(itrace-1),LEVEL) == 0) then
                key_errors = key_errors + 1
                if (key_errors < 20) then
                     call flexbin_verify_printer2            &
                              ('flexfile error:  itrace =',  &
                               itrace-1,itrace,temp)
                end if
           end if
      end do
      deallocate(temp)                                    

!--------- print summary:

      call pc_print ('FLEXBIN: NUMBER OF TRACES ON FLEXFILE =',ntraces)
      call pc_print ('FLEXBIN: NUMBER OF REPEATED TRACES ON FLEXFILE =',  &
                                            key_errors)
      if (key_errors > 0) go to 995
      call pc_print ('FLEXBIN: FLEXFILE SUCCESSFULLY VERIFIED')
      error = .false.
      return

!--------- error returns:

991   call pc_print ('FLEXBIN: ERROR ALLOCATING TEMP',ntraces)
      go to 999
995   call pc_print ('FLEXBIN: THERE ARE REPEATED TRACES (with the same', &
                     'values of header words 9 and 10) ON THE FLEXFILE')

999   call pc_print ('FLEXBIN: FATAL ERROR IN FLEXBIN_VERIFY_FLEXFILE')
      call flexio_close_old (flexio)
      if (associated(temp)) deallocate(temp)                                    
      error = .true.
      return
      end subroutine flexbin_verify_flexfile


!!---------------------- flexbin verify printer2 --------------------------!!
!!---------------------- flexbin verify printer2 --------------------------!!
!!---------------------- flexbin verify printer2 --------------------------!!


      subroutine flexbin_verify_printer2 (phrase,idup1,idup2,dup)
      implicit none
      character(len=*)       ,intent(in) :: phrase                ! arguments
      integer                ,intent(in) :: idup1,idup2           ! arguments
      type(triplesort_ints)  ,intent(in) :: dup(:)                ! arguments
      integer                            :: idup,itrace           ! local
      double precision                   :: hd9,hd10              ! local

      do idup=idup1,idup2
           hd9        = dup(idup)%primary
           hd10       = dup(idup)%secondary
           itrace     = dup(idup)%tertiary
           write(pc_get_lun(),1000) phrase,idup,hd9,hd10,itrace
      end do
      call pc_print (' ')
      return
1000  format (1x,a26,'  sorted trace number = ',i8,  &
                     '  headers 9,10 = ',2f11.0,     &
                     '  original trace number = ',i8)
      end subroutine flexbin_verify_printer2


!!------------------ flexbin verify key info ------------------------------!!
!!------------------ flexbin verify key info ------------------------------!!
!!------------------ flexbin verify key info ------------------------------!!


      subroutine flexbin_verify_key_info (key,bin,ntraces,     &
                                          xfirst,inl_inc,nx,   &
                                          yfirst,crl_inc,ny,error)
      implicit none
      integer         ,intent(in)   :: ntraces,nx,ny           ! args
      type(key_struct),intent(in)   :: key(:)                  ! args (ntraces)
      type(bin_struct),intent(in)   :: bin(:)                  ! args (ntraces)
      real            ,intent(in)   :: xfirst,inl_inc          ! args
      real            ,intent(in)   :: yfirst,crl_inc          ! args
      logical         ,intent(out)  :: error                   ! args
      integer             :: key_errors,itrace,ix,iy,icmp,ier  ! local
      real                              :: xgrid,ygrid         ! local
      type(triplesort_ints),allocatable :: temp(:)             ! local

!--------- do the verification:

      call pc_print (' ')
      allocate(temp(ntraces),stat=ier)                                   
      if (ier /= 0) go to 991

      do itrace=1,ntraces
           xgrid = bin(itrace)%xgrid
           ygrid = bin(itrace)%ygrid
           ix=nint(xgrid)
           iy=nint(ygrid)
           icmp=ix+(iy-1)*nx
           temp(itrace)%primary   = key(itrace)%hd9
           temp(itrace)%secondary = key(itrace)%hd10
           temp(itrace)%tertiary  = icmp
      end do
      call triplesort_sort (temp,ntraces)
      key_errors   = 0
      do itrace=2,ntraces
           if (triplesort_compare(temp(itrace),temp(itrace-1),LEVEL) == 0) then
                key_errors = key_errors + 1
                if (key_errors < 20) then
                     call flexbin_verify_printer             &
                              ('flexfile error:  itrace =',  &
                               itrace-1,itrace,temp,         &
                               xfirst,inl_inc,nx,               &
                               yfirst,crl_inc,ny)
                end if
           end if
      end do
      deallocate(temp)                                    
      call pc_print ('FLEXBIN: NUMBER OF TRACES ON FLEXFILE =',ntraces)
      call pc_print ('FLEXBIN: NUMBER OF REPEATED TRACES ON FLEXFILE =',  &
                                            key_errors)
      if (key_errors > 0) go to 992
      error = .false.
      return

!--------- error returns:

991   call pc_print ('FLEXBIN: ERROR ALLOCATING TEMP',ntraces)
      go to 999
992   call pc_print ('FLEXBIN: THERE ARE REPEATED TRACES (with the same', &
                     'values of header words 9 and 10) ON THE FLEXFILE')

999   call pc_print ('FLEXBIN: FATAL ERROR IN FLEXBIN_VERIFY_KEY_INFO')
      error = .true.
      return
      end subroutine flexbin_verify_key_info


!!---------------------- flexbin verify printer ---------------------------!!
!!---------------------- flexbin verify printer ---------------------------!!
!!---------------------- flexbin verify printer ---------------------------!!

! The first dimension of DUP must be at least 3.


      subroutine flexbin_verify_printer (phrase,idup1,idup2,dup,  &
                                         xfirst,inl_inc,nx,       &
                                         yfirst,crl_inc,ny)
      implicit none
      character(len=*)       ,intent(in) :: phrase                  ! args
      integer                ,intent(in) :: idup1,idup2,nx,ny       ! args
      type(triplesort_ints)  ,intent(in) :: dup(:)                  ! args
      real                   ,intent(in) :: xfirst,inl_inc          ! args
      real                   ,intent(in) :: yfirst,crl_inc          ! args
      integer                            :: idup,icmp,iy,ix         ! local
      double precision                   :: hd7,hd8,hd9,hd10        ! local

      do idup=idup1,idup2
           hd9  = dup(idup)%primary
           hd10 = dup(idup)%secondary
           icmp = dup(idup)%tertiary
           iy   = (icmp+nx-1)/nx
           ix   = icmp-(iy-1)*nx
           hd7  = mth_bin_center (xfirst, inl_inc, ix)
           hd8  = mth_bin_center (yfirst, crl_inc, iy)
           write(pc_get_lun(),1000) phrase,idup,hd9,hd10,hd7,hd8
      end do
      call pc_print (' ')
      return
1000  format (1x,a26,i8,'  headers 9,10,7,8 = ',2f13.0,2f13.2)
      end subroutine flexbin_verify_printer


!!------------------ flexbin verify dup info -----------------------------!!
!!------------------ flexbin verify dup info -----------------------------!!
!!------------------ flexbin verify dup info -----------------------------!!


      subroutine flexbin_verify_dup_info (dup,ndup,max_dup,    &
                                          xfirst,inl_inc,nx,   &
                                          yfirst,crl_inc,ny,error)
      implicit none
      integer                ,intent(in)  :: ndup,max_dup,nx,ny      ! args
      type(triplesort_ints)  ,intent(in)  :: dup(:)                  ! args
      real                   ,intent(in)  :: xfirst,inl_inc          ! args
      real                   ,intent(in)  :: yfirst,crl_inc          ! args
      logical                ,intent(out) :: error                   ! args
      integer       :: kount,idup,compare                            ! local
      integer       :: sort_errors,maxdup_errors,maxdup_actual       ! local

      sort_errors   = 0
      maxdup_errors = 0
      maxdup_actual = 1
      kount         = 1
      if (ndup == 0) maxdup_actual = 0
      call pc_print (' ')
      do idup=2,ndup
           compare = triplesort_compare(dup(idup),dup(idup-1),LEVEL)
           if (compare < 0) then
                sort_errors = sort_errors + 1
                if (sort_errors < 20) then
                     call flexbin_verify_printer       &
                              ('sort error:  idup =',  &
                               idup-1,idup,dup,        &
                               xfirst,inl_inc,nx,      &
                               yfirst,crl_inc,ny)
                end if
                kount = 1
           else if (compare == 0) then
                kount = kount + 1
                if (kount > max(max_dup,1)) then
                     maxdup_errors = maxdup_errors + 1
                     if (maxdup_errors < 20) then
                          call flexbin_verify_printer         &
                                   ('maxdup error:  idup =',  &
                                    idup-kount+1,idup,dup,    &
                                    xfirst,inl_inc,nx,        &
                                    yfirst,crl_inc,ny)
                     end if
                end if
                maxdup_actual = max(maxdup_actual,kount)
           else      !  if (dup(idup) > dup(idup-1)) then
                kount = 1
           end if
      end do
      call pc_print ('FLEXBIN: NUMBER OF DUPLICATE TRACES TO MAKE =',ndup)
      call pc_print ('FLEXBIN: NUMBER OF SORT ERRORS =',sort_errors)
      call pc_print ('FLEXBIN: NUMBER OF MAXDUP ERRORS =',maxdup_errors)
      call pc_print &
             ('FLEXBIN: MAXIMUM NUMBER OF TIMES ANY TRACE IS DUPLICATED =',&
                                  maxdup_actual)
      error = (sort_errors > 0 .or. maxdup_errors > 0)
      return
      end subroutine flexbin_verify_dup_info


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module flexbin_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

