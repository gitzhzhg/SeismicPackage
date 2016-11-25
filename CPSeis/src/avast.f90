!<CPS_v1 type="PROCESS"/>
!!------------------------------- avast.f90 ---------------------------------!!
!!------------------------------- avast.f90 ---------------------------------!!
!!------------------------------- avast.f90 ---------------------------------!!

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
! Name       : AVAST
! Category   : stacks
! Written    : 1992-02-03   by: Mike Howard
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Produce various amplitude vs. angle (AVA) stacks.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! DOMAIN determines whether processing is performed in time or depth.
! If DOMAIN = TIME, the input traces must be sampled in time (two way travel).
! Velocities must also be picked in time and may be type VTNM, VTRM or VTIN.
! If DOMAIN = DEPTH, the input traces must be sampled in depth.
! Velocities must be picked in depth and must be type VZIN (interval).
!
! If MODE = ARS, AVAST stacks traces from a CMP gather within the angle ranges
! specified by the arrays ANG_BEG and ANG_END.  The stacked trace will be dead
! for times where the fold in the desired angle range is less than FOLD_MIN.
!
! If MODE = CFOS, AVAST first tries to center the desired number of traces on
! ANG_CFOS.  If the required fold cannot be obtained, the stack is shifted
! left or right of ANG_CFOS in order the obtain the desired fold.  The maximum
! allowable shift is given by SHFT_MAX.  The stacked trace is dead for times
! that would require a shift greater than the SHFT_MAX tolerance.
!
! If MODE = FOLD, AVAST only outputs fold-of-stack traces for use as a
! diagnostic.
!
!
! Angle Calculation
!
! For OPT_AVG = YES, the velocity functions in file PATHNAME_VEL are manipulated
! in order to come up with a single interval velocity function to be used for
! the angle calculation.  If the velocities are of type VTNM or VTRM, they are
! first resampled to the trace sample interval, then averaged, and finally
! converted to interval velocity using Dix's formula.  If they are of type
! VTIN or VZIN, they are resampled to the trace sample interval (honoring the
! constant interval velocity "steps") then averaged.
!
! For OPT_AVG = NO, the angle calculation is done at each velocity location and
! the offset limits for each panel are linearly interpolated for intermediate
! locations.
!
! The intent of the OPT_AVG = NO option is to be able to accommodate a SLOWLY
! varying water depth.  It is up to you to put in functions which have a smooth
! lateral variation. It is NOT recommended that the option be used with raw
! stacking velocites.
!
!
! Input Traces
!
! Traces must be input as NMO corrected CMP gathers and must be GATHERED;
! offsets must be non-negative and in increasing order within each gather.
!
!
! Output Traces
!
! If SORT_TO_PANELS = YES, output is sorted to stack panels; i.e., all CMPs for
! the 1st CFOS angle (or ARS angle range), then all CMPs for the 2nd angle, etc.
! If SORT_TO_PANELS = NO, output is in angle gathers; i.e., all CFOS angles (or
! all ARS angle ranges) for the 1st CMP, then all angles for the 2nd CMP, etc.
! If you want to look at the output both ways, choose SORT_TO_PANELS = NO, and
! then run a sort job (This way, you'll do only one sort instead of two!).
! Choose SORT_TO_PANELS = YES to produce stack panels for migration. You'll
! need the data in angle gathers in order to run gradient-intercept analysis
! (process AVAGRAD) or prestack inversion but, in that case, you might still
! choose SORT_TO_PANELS = YES in order to migrate the data; then sort your
! migrated data to angle gathers. For use by processes such as AVAGRAD or
! prestack inversion, AVAST sets header word 6 to the central angle of each
! output trace (CFOS angle or center of ARS range). If FULLSTACK = YES, full
! stack traces are output as the last stack panel (if SORT_TO_PANELS = YES)
! or the last trace of each angle gather (if SORT_TO_PANELS = NO).
!
!
! CFOS Diagnostic Files
!
! If QC_CFOS = TRAJ, then only a textfile is created, containing angle
! trajectories for all the desired CFOS angles.  This file will have an
! extension of ".avast" and can be displayed using the AVAST tool in CBYT.
!
! If QC_CFOS = BOTH, then the trajectory file is created and a tracefile is also
! created, showing the trace samples included in CMP gathers for each desired
! ANG_CFOS angle.  This tracefile will have a ".trc8" extension and can be
! viewed in CBYT as an underlay plot for the input data.  (The tracefile
! contains as many traces as the input data, so the BOTH option should NOT be
! chosen for production jobs.)
!
! If QC_CFOS = NONE, then no QC files are created.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! Traces must be input as NMO corrected stack gathers.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! This process requires traces to be input in gathers.
!
! Traces must be input as NMO corrected CMP gathers.  Offsets must be
! non-negative, in increasing order within each gather.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
!
! If SORT_TO_PANELS = YES, then:
!
!    This is an all-trace process; i.e., all input traces must be read before
!    any output traces are passed out.
!
!    This process passes out traces one at a time.
!
!    Output traces are in stack panels.
!
!    An optional full stack, if specified, is the last output stack panel.
!
! If SORT_TO_PANELS = NO, then:
!
!    This is a multi-trace process; each input gather gives rise to a
!    corresponding output gather, usually with fewer traces than the input
!    gather due to the stacking which has taken place.
!
!    Output traces are in angle gathers.
!
!    An optional full stack trace, if specified, is the last trace of each
!    output gather.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       Reset (normally reduced)
! GATHERED  whether traces are a legitimate gather  Must be true (& stays true)
! NWIH      number of words in trace header         Used but not changed
! NDPT      number of sample values in trace        Used but not changed
! TSTRT     starting time on trace                  Used but not changed
! DT        trace sample interval                   Used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Renumbered
! 2       Head mute                  Reset
! 3       Current group number       Reset if SORT_TO_PANELS = YES
! 4       Channel number             Reset
! 5       Fold                       Set (probably useful only for full stack)
! 6       Offset                     Set to central angle of stacked trace*
! 24      Panel number               Set
! 64      Tail mute                  Reset
! (user)  HDR_OFF_OUT                Set to near offset of the input gather**
!
! * Header word 6 is set to either the CFOS angle or center of ARS range.
! For the optional full-stack trace, header word 6 is set to 90.
!
! ** The value originally in header word 6 of the first live trace of the
! input gather is placed in user-defined header word HDR_OFF_OUT of each
! output trace derived from that input gather.
!
! All header words other than those listed above are taken from the first live
! trace of the input gather.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!      Date        Author     Description
!      ----        ------     -----------
! 42.  2006-09-11  Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 41.  2006-04-04  Baumel     Documentation change only - pointing out
!                             restrictions when velocity functions are read
!                             from a TRCIO trace file.
! 40.  2005-11-29  Stoeckley  Add ability to read velocity functions from a
!                              trcio trace file.  Except for the addition of
!                              parameter VELTYPE, all code changes are in the
!                              velio primitive and the new velio4 primitive.
! 39.  2002-02-27  CIBurch/   Fix documentation references to byte.  Change
!                  Goodger    keyword PATHNAME_QC_BYTE to PATHNAME_QC_TRC.
! 38.  2002-02-21  Goodger    Fix length problem with file selection boxes. Line
!                             up some parameters for a nicer appearance.  Put
!                             some parameters on a second page to alleviate
!                             crowding.
! 37.  2002-02-14  Selzler    Added DEPTH DOMAIN option and file selection
!                             boxes to gui for user convenience.
!                             Incorporated Bob's optimizations for search.
! 36.  2002-02-08  Baumel     Fix bug that caused out-of-memory references for
!                             some angles with MODE = CFOS. Also fix bug that
!                             incorrectly stacked FOLD_CFOS + 1 traces when
!                             shifting to the right.
! 35.  2001-04-04  Baumel     Fix .avast QC file format for non-integer angles.
! 34.  2001-02-13  Stoeckley  Replace BYTE with PERMTFILE (which uses TRCIO).
! 33.  2001-01-10  Baumel     Add HDR_OFF_OUT parameter.
! 32.  2000-12-07  Baumel     Change wrapped_up flag to skip_wrapup.
! 31.  2000-12-04  Baumel     Add Help for display-only parameters.
! 30.  2000-06-27  Baumel     Allowed GATHERED global to remain true on output
!                             when SORT_TO_PANELS = NO, even when there is only
!                             one trace in the output gather (undid the change
!                             of 2000-05-02).
! 29.  2000-05-24  Baumel     Add logic to avoid isolated, deep, islands of
!                             non-zero values when MODE = ARS (corresponding
!                             logic for MODE = CFOS had already been added in
!                             initial converted version of 2000-04-24).
! 28.  2000-05-02  Baumel     Another correction in setting of GATHERED global.
! 27.  2000-05-01  Baumel     Reset GATHERED global depending on SORT_TO_PANELS.
! 26.  2000-05-01  Baumel     Added SORT_TO_PANELS option.
! 25.  2000-04-27  Baumel     Another tweak of front end behavior.
! 24.  2000-04-26  Baumel     Fix bug involving front end behavior.
! 23.  2000-04-25  Baumel     Fix bug involving identification of dead traces
!                             when QC_CFOS = BOTH and FULLSTACK = YES.
! 22.  2000-04-24  Baumel     Converted from old system. Various enhancements
!                             (e.g., TSTRT doesn't have to be zero, add
!                             FULLSTACK option). Simplified trace flow; now
!                             requires CMP gathers in, angle gathers out.
! 21.  1998-11-13  Vunderink  Begin using the f90 compiler.
! 20.  1998-08-27  Vunderink  Added QCMERGE parameter.
! 19.  1998-08-13  Vunderink  Fix bug associated with the TSMOOTH paramter.
! 18.  1998-07-16  Vunderink  Added TSMOOTH and MAXTOC parameters.
! 17.  1998-07-07  Vunderink  Fix problem with closing Q! byte file when
!                             only on panel and rewrote logic to ignore
!                             dead traces.
! 16.  1998-06-25  Vunderink  Ignore dead traces
! 15.  1998-06-17  Vunderink  Fixed angle table file to contain one table
!                             when AVG=YES
! 14.  1998-06-04  Vunderink  Added dimensions to QC angle table file.
! 13.  1998-05-20  Vunderink  Added constant fold of stack support.
! 12.  1997-03-05  Vunderink  Added MODE parameter.
! 11.  1995-12-11  Goodger    Fix problem with doing only one panel.
!                             The STROT file specified in note 3 containing
!                             the 5 traces was not being written.
! 10.  1993-09-23  Troutt     Fix problem with "N" not valid when JUSTOUT=
!                             .TRUE. (entering AVAST from "below"). Set
!                             N=1 in IF block that contains DOs 201,202.
!  9.  1992-11-10  Troutt     Add AVG parameter to allow for spatially
!                             varying the offset-vs-time tables. (These
!                             changes were avail. for testing 199-23).
!  8.  1992-09-02  Troutt     Remove all references to parameter "INPUT."
!                             This process requires single trace input, and
!                             the front-end now checks for this, as well as
!                             for TSTRT=0.
!                             Fix storage and scratch calc. and doc for
!                             same.
!  7.  1992-08-27  Howard     Check GETV for interval velocity type (VTIN).
!  6.  1992-06-19  Peterson   Add SAVE option for FNAME.
!  5.  1992-05-18  Peterson   Check to not exceed number of angles (nang)
!  4.  1992-02-25  Howard     Change to pass out traces.
!  3.  1992-02-19  Howard     Add tail mute header.
!  2.  1992-02-05  Howard     Add MINFOLD parameter.
!  1.  1992-02-03  Howard     Original version.
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
! NEED_REQUEST   varies*   whether this process ever needs to request traces.
! NEED_LABEL     varies*   whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH       varies    amount of temporary memory needed.
! NSTORE         varies    amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK           0 **     disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! *  NEED_REQUEST and NEED_LABEL are true only if SORT_TO_PANELS = YES.
! ** This process MAY use a lot of disk space (if OPT_AVE = NO and you have
!    a large number of velocity functions, or if SORT_TO_PANELS = YES and you
!    have a large survey) but the amount of disk needed cannot be calculated
!    from the input parameters.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES*   means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES*   if this process needs more traces.
!
! * NTR == NEED_TRACES is possible only if SORT_TO_PANELS = YES.
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
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<gui_def>
!<NS AVAST Process/NC=80>
!                AVA STack Process
! Produce various amplitude vs. angle (AVA) stacks.
!
! DOMAIN=`CCCCC
! MODE=~~`CCC          FULLSTACK=`CC    HDR_OFF_OUT=`IIII
!
! FOLD_CFOS=`IIIIIIII  SHFT_MAX=`IIIIIIII  QC_CFOS=`CCC
!
! SELECT_PATHNAME_QC_CFOS[PATHNAME_QC_CFOS]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!        PATHNAME_QC_TRC= `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!       ANG_CFOS          ANG_BEG     ANG_END
!       `FFFFFFFFFFF      `FFFFFFFFFFF`FFFFFFFFFFF
!       `FFFFFFFFFFF      `FFFFFFFFFFF`FFFFFFFFFFF
!       `FFFFFFFFFFF      `FFFFFFFFFFF`FFFFFFFFFFF
!       `FFFFFFFFFFF      `FFFFFFFFFFF`FFFFFFFFFFF
!
! FOLD_ARS_MIN=`IIIIIIII 
!<PARMS PATHNAME_QC_CFOS[/ML=140/XST]>
!<PARMS PATHNAME_QC_TRC[/ML=140/XST/EN]>
!
!<NS Screen_Two/NC=80>
!
!SELECT_PATHNAME_VEL[PATHNAME_VEL]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                    [PATHNAME_VEL_INFO] `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! VELTYPE=`CCCC <----- type of velocities on PATHNAME_VEL if it is a trace file
!
! OPT_AVE=~~~~~~~`CC                        LEN_SMOOTH=`FFFFFFFFFFFF
!
! FSE=~~~~~~~~~~~`FFFFFFFF    MSCL=`FFFFFFFF     TVFSE=`FFFFFFFF
!
! GATHER SIZE IN=`IIIIIIII             GATHER SIZE OUT=`IIIIIIII
!
! SORT_TO_PANELS=`CC                   DEP_SCALE=~~~~~~`FFFFFFFFF
!<PARMS PATHNAME_VEL[/ML=140/XST]>
!<PARMS PATHNAME_VEL_INFO[/ML=140/XST]>
!<PARMS ANG_CFOS[/XST/YST]>
!<PARMS ANG_BEG_ARRAYSET[/XST/YST]>
!<PARMS GATHERSIZEIN[NUMTR/EN]>
!<PARMS GATHERSIZEOUT[NUMTR_OUT/EN]>
!</gui_def>

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="DOMAIN">
!<Tip> Domain (TIME or DEPTH) of trace samples and velocity picks.</Tip>
! Default = TIME
! Allowed = TIME  Input traces are sampled in time (two way travel).  Velocity
!                 picks (either RMS or interval) are also defined in time.
! Allowed = DEPTH Input traces are sampled in depth, where the starting depth
!                 is DEP_SCALE*TSTRT and depth increment is DEP_SCALE*DT.
!                 Velocity picks (interval required) are defined in depth.
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Type of AVA stack or fold of stack output traces desired. </Tip>
! Default = CFOS
! Allowed = CFOS  (Constant Fold Of Stack traces output.)
! Allowed = ARS   (Angle-Range Stack traces output.)
! Allowed = FOLD  (FOLD of stack traces output.)
! If MODE = CFOS, traces are stacked with a constant fold of stack about a
! given desired center angle.
! If MODE = ARS, traces within a given angle range are stacked.
! If MODE = FOLD, output traces carry fold of stack information only.
!</Help>
!
!<Help KEYWORD="FULLSTACK">
!<Tip> Whether to include a full stack trace for each input gather. </Tip>
! Default = NO
! Allowed = YES/NO
! Setting FULLSTACK = YES is the preferred way to obtain a full stack from
! AVAST since this option bypasses the angle calculations. The full stack
! traces, if included, will be the last stack panel (if SORT_TO_PANELS = YES),
! or the last trace of each output angle gather (if SORT_TO_PANELS = NO).
!</Help>
!
!<Help KEYWORD="HDR_OFF_OUT">
!<Tip> User defined header word to contain (near) offset upon output. </Tip>
! Default = 48
! Allowed = NWIH >= HDR_OFF_OUT >= 1
! Offsets of your INPUT traces are, as usual, assumed to be in header word 6.
! Offsets aren't particularly meaningful after AVAST, as a (depth-dependent)
! range of offsets must be stacked in producing each output angle trace. In
! fact, as angle is the natural replacement for offset after AVAST, header
! word 6 in the output traces is reset to the center of each angle range (or
! to the value 90 for the optional fullstack trace), which will be useful if
! you follow AVAST by AVAGRAD or prestack inversion.
!
! In case you need original offsets (for reapplying a mute based on offset),
! AVAST saves the "near" offset (i.e., the offset of the first live trace in
! the input gather) in user defined header word HDR_OFF_OUT. You may then
! move this value back to header word 6 before reapplying your mute. Please
! understand that every output trace derived from a given input gather will
! have the SAME value in this header word; thus, a near angle stack trace
! will be indicated as having the same "offset" as a far angle stack trace.
!</Help>
!
!<Help KEYWORD="FOLD_CFOS">
!<Tip> Desired fold for the constant fold of stack option. </Tip>
! Default = 11
! Allowed = odd int > 0
!</Help>
!
!<Help KEYWORD="SHFT_MAX">
!<Tip> Allowable shift (in number of traces) from desired center angle. </Tip>
! Default = 0
! Allowed = int >= 0
! SHFT_MAX is the maximum allowable shift (in number of traces) that the center
! of the stack gather can deviate from the desired angle.  In the CFOS mode,
! AVAST will maintain the desired fold of stack centered on the desired angle
! within the SHFT_MAX tolerance.  The stacked trace is dead for times that
! would exceed the SHFT_MAX tolerance.
!
! Active only if MODE = CFOS.
!</Help>
!
!<Help KEYWORD="QC_CFOS">
!<Tip> Option for creation of CFOS QC files. </Tip>
! Default = NONE
! Allowed = NONE
! Allowed = TRAJ
! Allowed = BOTH
! If QC_CFOS = NONE, then no files are created.
!
! If QC_CFOS = TRAJ, then only a textfile is created, containing angle
! trajectories for all the desired CFOS angles.  This file will have an
! ".avast" extension and can be displayed using the AVAST tool in CBYT.
!
! If QC_CFOS = BOTH, then the trajectory file is created and a tracefile is also
! created, showing the trace samples included in CMP gathers for each desired
! ANG_CFOS angle.  This tracefile will have a ".trc8" extension and can be
! viewed in CBYT as an underlay plot for the input data. (The tracefile contains
! as many traces as the input data, so the BOTH option should NOT be chosen for
! production jobs.)
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_QC_CFOS">
!<Tip> Choose PATHNAME_QC_CFOS using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_QC_CFOS">
!<Tip> Pathname for the QC file(s) created for the CFOS mode. </Tip>
! Default = NONE
! Allowed = char
! The supplied pathname is given a ".avast" extension for the trajectory file.
! If QC_CFOS = BOTH, the generated tracefile will use this same pathname, but
! with a ".trc8" extension.
!</Help>
!
!<Help TYPE="DISPLAY_ONLY" KEYWORD="PATHNAME_QC_TRC">
!<Tip> Pathname of QC tracefile produced when QC_CFOS = BOTH. </Tip>
! This is a display-only parameter. When QC_CFOS = BOTH, the tracefile name
! is generated automatically from PATHNAME_QC_CFOS by replacing its "avast"
! extension with "trc8".
!</Help>
!
!<Help KEYWORD="ANG_CFOS">
!<Tip> Array of desired angles for the constant fold of stack mode. </Tip>
! Default =  -
! Allowed = real >= 0.0
!</Help>
!
!<Help KEYWORD="ANG_BEG">
!<Tip> Array of beginning angles for the angle-range stack mode. </Tip>
! Default =  -
! Allowed = real >= 0.0
!</Help>
!
!<Help KEYWORD="ANG_END">
!<Tip> Array of ending angles for the angle-range stack mode. </Tip>
! Default =  -
! Allowed = real > ANG_BEG
!</Help>
!
!<Help KEYWORD="FOLD_ARS_MIN">
!<Tip> Minimum fold for live angle-range stack. </Tip>
! Default =  1
! Allowed = int > 0
! The stacked trace in the ARS mode will be dead for times where the fold in
! the desired angle range is less than FOLD_ARS_MIN.
!</Help>
!
!<Help KEYWORD="PATHNAME_VEL_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_VEL. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_VEL">
!<Tip> Choose PATHNAME_VEL using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_VEL">
!<Tip> Pathname for velocity file to be used in angle calculation. </Tip>
! Default = NONE
! Allowed = char
! This file can be a VELIO velocity file or a modspec file or a TRCIO trace
! file containing velocity functions.  If this file is a TRCIO file, the
! parameter VELTYPE must be set correctly.
!
! Also, if this file is a TRCIO trace file, X and Y locations must be stored
! within this file in header words 7 and 8 (these numbers are hard-wired into
! the velocity primitives that AVAST calls). Velocity traces in this file
! must be sorted with X and Y in ascending order, X increasing most rapidly,
! and must fill a rectangular grid.
!</Help>
!
!<Help KEYWORD="VELTYPE">
!<Tip> Type of velocities on the PATHNAME_VEL file (if is trace file). </Tip>
! Default = VTNM   (RMS velocities versus time)
! Allowed = VTNM   (RMS velocities versus time)
! Allowed = VTRM   (RMS velocities versus time)
! Allowed = VTIN   (interval velocities versus time)
! Allowed = VZIN   (interval velocities versus depth)
! This parameter needs to be specified only if the PATHNAME_VEL file is a
! TRCIO velocity trace file.  Otherwise this parameter is not used.
!</Help>
!
!<Help KEYWORD="OPT_AVE">
!<Tip> Whether to average all velocity functions in the file into one. </Tip>
! Default = YES
! Allowed = YES/NO
! For OPT_AVE = NO, the angle calculation is done at each velocity location,
! and the offset limits for each panel are linearly interpolated for
! intermediate locations.  The intent of the OPT_AVE = NO option is to be able
! to accommodate a SLOWLY varying water depth.  It is up to you to put in
! functions that have a smooth lateral variation. It is NOT recommended that
! this option be used with raw stacking velocites.
!</Help>
!
!<Help KEYWORD="LEN_SMOOTH">
!<Tip> Running average smoothing length for interval velocities. </Tip>
! Default = 0.2
! Allowed = real >= 0.0
! Length of running average smoothing for interval velocity functions.
! If DOMAIN = TIME then the units are in seconds.
! If DOMAIN = DEPTH then LEN_SMOOTH is in depth units (meters or feet).
! If LEN_SMOOTH = 0.0, then no smoothing is done.
!
! Normally some smoothing is required to avoid jagged velocity contours.
! Typical values of LEN_SMOOTH should at least as great as the sampling
! interval of your velocity functions.
!</Help>
!
!<Help KEYWORD="FSE">
!<Tip> Fold of stack exponential method for scaling stacked traces. </Tip>
! Default = 0.0
! Allowed = 0.0-1.0
! The Fold of Stack exponential method scales stacked traces by dividing trace
! samples by the number of live traces stacked together raised to the FSE
! power.  The FSE method is time-independent.  FSE = 0.0 does no scaling.
!</Help>
!
!<Help KEYWORD="MSCL">
!<Tip> Mute Scaling method for scaling stacked traces. </Tip>
! Default = 0.0
! Allowed = 0.0-1.0
! The Mute Scaling method scales stacked traces by multiplying by a complicated
! empirical formula (with no obvious justification) based on maximum fold and
! local time-dependent fold.  Mute scaling is intended to compensate for loss
! of fold due to muting.  MSCL = 0.0 does no scaling.
!
!        Scale(MSCL) = 1.0 + MSCL*(((maximum fold)/(actual fold)) - 1.0)
!</Help>
!
!<Help KEYWORD="TVFSE">
!<Tip> Time Varying Fold of Stack Exponential method. </Tip>
! Default = 1.0
! Allowed = 0.0-1.0
! The Time Varying Fold of Stack Exponential method scales stacked traces by
! dividing trace samples by the time varying number of live samples stacked
! together raised to the TVFSE power.  TVFSE = 0.0 does no scaling.  This is an
! industry-standard method.
!
! If TVFSE /= 0.0, then FSE and MSCL methods are disabled.
!
! For normal AVAST work, TVFSE should be set to 1.0.
!</Help>
!
!<Help TYPE="DISPLAY_ONLY" KEYWORD="NUMTR">
!<Tip> Size of input gathers; i.e., NUMTR global upon input to AVAST. </Tip>
! This is a display-only parameter, illustrating the NUMTR global upon input
! to AVAST. This must be greater than 1, as AVAST requires gathered input.
!</Help>
!
!<Help TYPE="DISPLAY_ONLY" KEYWORD="NUMTR_OUT">
!<Tip> Number of output angles, including fullstack if specified. </Tip>
! This is a display-only parameter, indicating how many angle traces will be
! output from AVAST, and also includes the fullstack trace if FULLSTACK = YES.
! When SORT_TO_PANELS = YES, NUMTR_OUT shows how many stack panels you will
!   get, but the output from AVAST is not gathered in this case.
! When SORT_TO_PANELS = NO, output traces from AVAST *are* gathered, and
!   NUMTR_OUT is actually the NUMTR global upon output from AVAST.
!</Help>
!
!<Help KEYWORD="SORT_TO_PANELS">
!<Tip> Whether to sort output traces to stack panels. </Tip>
! Default = YES
! Allowed = YES/NO
!
! If SORT_TO_PANELS = YES, output is sorted to stack panels; i.e., all CMPs
! for the first CFOS angle (or first ARS angle range), then all CMPs for the
! second angle, etc.  In this case, the "GATHER SIZE OUT" number displayed
! on screen is actually the number of stack panels, and the output traces are
! passed out singly.
!
! If SORT_TO_PANELS = NO, output is in angle gathers; i.e., all CFOS angles
! (or all ARS angle ranges) for the first CMP, then all angles for the
! second CMP, etc.  In this case, the "GATHER SIZE OUT" number displayed
! on screen is actually the number of traces in each output gather.
!
! If you want to look at the output both ways, choose SORT_TO_PANELS = NO,
! and then run a sort job (This way, you'll do only one sort instead of two!).
!
! Choose SORT_TO_PANELS = YES to produce stack panels for migration.
!</Help>
!
!<Help KEYWORD="DEP_SCALE">
!<Tip> Depth interval corresponding to one "second" in traces. </Tip>
! Default = 1000.0
! Allowed = real > 0.0
! This parameter is provided for compatibility with other depth processes
! such as KMIG. However, you really ought to take the default value (1000),
! since any other value is incompatible with Cbyt.
!
! DEP_SCALE is active iff DOMAIN = DEPTH.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------

!
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module avast_module
      use pc_module
      use named_constants_module
      use permtfile_module
      use velio_module
      use pathcheck_module
      use pathchoose_module
      use mutehw_module
      use interp_module
      use intpvelf_module
      use lav_module
      use mem_module
      use sio_module
      use temptfile_module
      use stkscale_module
      use binsort_module
      implicit none
      private
      public :: avast_create
      public :: avast_initialize
      public :: avast_update
      public :: avast_delete
      public :: avast
      public :: avast_wrapup

      character(len=100),public,save :: AVAST_IDENT = &
'$Id: avast.f90,v 1.42 2006/09/11 13:15:43 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: avast_struct
        private

        logical                    :: skip_wrapup      ! wrapup flag.

        character(len=4)           :: mode             ! process parameters.
        character(len=3)           :: fullstack        ! process parameters.
        integer                    :: hdr_off_out      ! process parameters.
        integer                    :: fold_cfos        ! process parameters.
        integer                    :: shft_max         ! process parameters.
        character(len=4)           :: qc_cfos          ! process parameters.
        character(len=FILENAME_LENGTH) :: pathname_qc_cfos
        character(len=FILENAME_LENGTH) :: pathname_qc_trc
        integer                    :: num_ang_cfos     ! process parameters.
        real              ,pointer :: ang_cfos (:)     ! process parameters.
        integer                    :: num_ang_ars      ! process parameters.
        real              ,pointer :: ang_beg (:)      ! process parameters.
        real              ,pointer :: ang_end (:)      ! process parameters.
        integer                    :: fold_ars_min     ! process parameters.
        character(len=FILENAME_LENGTH) :: pathname_vel ! process parameters.
        character(len=3)           :: opt_ave          ! process parameters.
        real                       :: len_smooth       ! process parameters.
        real                       :: fse, mscl, tvfse ! process parameters.
        character(len=3)           :: sort_to_panels   ! process parameters.
        character(len=5)           :: domain           ! process parameters.
        character(len=4)           :: veltype          ! process parameters.
        real                       :: dep_scale        ! process parameters.

        integer                    :: numtr            ! globals.
        integer                    :: nwih, ndpt       ! globals.
        real                       :: tstrt, dt        ! globals.

        type(permtfile_struct),pointer :: byte             ! for bytefile.
        type(velio_struct)    ,pointer :: velio            ! for velocities.
        type(temptfile_struct),pointer :: temptfile_offtab ! offset tables.
        type(binsort_struct)  ,pointer :: binsort          ! for sort_to_panels.
        type(pathchoose_struct),pointer :: pathchoose_cfos ! pathname_qc_cfos.
        type(pathchoose_struct),pointer :: pathchoose_vel  ! pathname_vel.

        integer                    :: lun              ! dependent variables.
        integer                    :: num_ang1         ! dependent variables.
        integer                    :: fold_cfos2       ! dependent variables.
        integer                    :: numtr_out        ! dependent variables.
        integer                    :: ncount_h1        ! dependent variables.
        integer                    :: ndptvel          ! dependent variables.
        integer                    :: nlimits          ! dependent variables.
        integer                    :: sio_avast_lun    ! dependent variables.
        integer                    :: trcfirst         ! dependent variables.
        integer                    :: velfirst         ! dependent variables.
        integer                    :: nlen_smooth      ! dependent variables.
        integer                    :: numvels          ! dependent variables.
        logical                    :: disktables       ! dependent variables.
        integer                    :: ix1mem, ix2mem   ! dependent variables.
        integer                    :: nxmem            ! dependent variables.
        integer                    :: iy1mem, iy2mem   ! dependent variables.
        integer                    :: nymem            ! dependent variables.
        integer                    :: nhx, nhy         ! dependent variables.
        integer                    :: nxbins, nybins   ! dependent variables.
        real                       :: unit_scale       ! dependent variables.
        real              ,pointer :: xbins (:)        ! dependent variables.
        real              ,pointer :: ybins (:)        ! dependent variables.
        real                       :: xlast, ylast     ! dependent variables.
        real              ,pointer :: offlast (:,:,:)  ! dependent variables.
        real              ,pointer :: offbuf (:,:,:,:,:) ! dependent variable.
        real              ,pointer :: h6out (:)        ! dependent variables.
        real              ,pointer :: bytefile(:,:)    ! dependent variables.

      end type avast_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(avast_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine avast_create (obj)

      type(avast_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%ang_cfos)
      nullify (obj%ang_beg)
      nullify (obj%ang_end)
      nullify (obj%byte)
      nullify (obj%velio)
      nullify (obj%temptfile_offtab)
      nullify (obj%binsort)
      nullify (obj%xbins)
      nullify (obj%ybins)
      nullify (obj%offlast)
      nullify (obj%offbuf)
      nullify (obj%h6out)
      nullify (obj%bytefile)
      nullify (obj%pathchoose_cfos)
      nullify (obj%pathchoose_vel)

      call pathchoose_create(obj%pathchoose_cfos, 'pathname_qc_cfos', '.avast')
      call pathchoose_create(obj%pathchoose_vel, 'pathname_vel', '.vel')

      call avast_initialize (obj)

      end subroutine avast_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine avast_delete (obj)

      type(avast_struct),pointer :: obj       ! arguments

      call avast_wrapup (obj)

      call mem_free (obj%ang_cfos)
      call mem_free (obj%ang_beg)
      call mem_free (obj%ang_end)
      call mem_free (obj%xbins)
      call mem_free (obj%ybins)
      call mem_free (obj%offlast)
      call mem_free (obj%bytefile)

      if (associated(obj%offbuf))  deallocate           (obj%offbuf)
      if (associated(obj%byte))    call permtfile_close (obj%byte)
      if (associated(obj%velio))   call velio_close     (obj%velio)
      if (associated(obj%temptfile_offtab))  &
                                   call temptfile_close (obj%temptfile_offtab)
      if (associated(obj%binsort)) call binsort_close   (obj%binsort)
      if (associated(obj%pathchoose_cfos)) &
                                   call pathchoose_delete(obj%pathchoose_cfos)
      if (associated(obj%pathchoose_vel)) &
                                   call pathchoose_delete(obj%pathchoose_vel)
      if (obj%sio_avast_lun >= 1)  call sio_close       (obj%sio_avast_lun)

      deallocate(obj)

      end subroutine avast_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine avast_initialize (obj)

      type(avast_struct),intent(inout) :: obj       ! arguments

      obj%mode             = 'CFOS'
      obj%fullstack        = 'NO'
      obj%hdr_off_out      = 48
      obj%fold_cfos        = 11
      obj%shft_max         = 0
      obj%qc_cfos          = 'NONE'
      obj%pathname_qc_cfos = PATHCHECK_EMPTY
      obj%pathname_qc_trc = PATHCHECK_EMPTY
      obj%num_ang_cfos     = 0
      obj%num_ang_ars      = 0
      obj%fold_ars_min     = 1
      obj%pathname_vel     = PATHCHECK_EMPTY
      obj%opt_ave          = 'YES'
      obj%len_smooth       = 0.2
      obj%fse              = 0.0
      obj%mscl             = 0.0
      obj%tvfse            = 1.0
      obj%sort_to_panels   = 'YES'
      obj%domain           = 'TIME'
      obj%veltype          = 'VTNM'
      obj%dep_scale        = 1000.0

      obj%sio_avast_lun    = 0
      obj%ncount_h1        = 0
      obj%xlast            = -huge(1.0)
      obj%ylast            = -huge(1.0)

      call avast_update (obj)

      end subroutine avast_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine avast_update (obj)

      type(avast_struct),intent(inout),target :: obj            ! arguments

      integer     :: nscratch, nstore, n_beg, n_end             ! local
      integer     :: iang, errflag, maxpicks, i                 ! local
      real        :: atemp                                      ! local
      logical     :: gathered                                   ! local
      logical     :: cfos_sens, ars_sens, fse_sens, tvfse_sens  ! local
      logical     :: fse_sens1                                  ! local
      character(len=4)    :: qc_cfos_local                      ! local
      character(len=128)  :: message                            ! local
      character(len=160)  :: messagebig                         ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.
      obj%lun = pc_get_lun()

      if (pathchoose_update(obj%pathchoose_cfos, obj%pathname_qc_cfos)) return
      if (pathchoose_update(obj%pathchoose_vel, obj%pathname_vel)) return

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("ang_beg_arrayset", (/  &
                                    "ang_beg",              &
                                    "ang_end" /))

!-----Get globals
      call pc_get_global ('NUMTR'   , obj%numtr)
      call pc_get_global ('GATHERED', gathered)
      call pc_get_global ('NWIH'    , obj%nwih)
      call pc_get_global ('NDPT'    , obj%ndpt)
      call pc_get_global ('TSTRT'   , obj%tstrt)
      call pc_get_global ('DT'      , obj%dt)

!-----Check globals
      if (obj%numtr < 2) then
        call pc_error ('This process requires gathered input. Please insert &
                       &GATHER before AVAST.')
      else if (.not. gathered) then
        call pc_error ('This process requires gathered input. Input traces &
                       &are currently grouped but are not in functional &
                       &gathers. Please insert GATHER before AVAST.')
      end if

!-----Get process parameters
      qc_cfos_local = obj%qc_cfos
      call pc_get ('DOMAIN'          , obj%domain)
      call pc_get ('VELTYPE'         , obj%veltype)
      call pc_get ('MODE'            , obj%mode)
      call pc_get ('FULLSTACK'       , obj%fullstack)
      call pc_get ('HDR_OFF_OUT'     , obj%hdr_off_out)
      call pc_get ('FOLD_CFOS'       , obj%fold_cfos)
      call pc_get ('SHFT_MAX'        , obj%shft_max)
      call pc_get ('QC_CFOS'         , obj%qc_cfos)
      call pc_get ('PATHNAME_QC_CFOS', obj%pathname_qc_cfos)
      call pc_alloc ('ANG_CFOS'      , obj%ang_cfos,   obj%num_ang_cfos)
      n_beg = obj%num_ang_ars
      n_end = obj%num_ang_ars
      call pc_alloc ('ANG_BEG'       , obj%ang_beg,    n_beg)
      call pc_alloc ('ANG_END'       , obj%ang_end,    n_end)
      call pc_get ('FOLD_ARS_MIN'    , obj%fold_ars_min)
      call pc_get ('PATHNAME_VEL'    , obj%pathname_vel)
      call pc_get ('OPT_AVE'         , obj%opt_ave)
      call pc_get ('LEN_SMOOTH'      , obj%len_smooth)
      call pc_get ('FSE'             , obj%fse)
      call pc_get ('MSCL'            , obj%mscl)
      call pc_get ('TVFSE'           , obj%tvfse)
      call pc_get ('SORT_TO_PANELS'  , obj%sort_to_panels)
      call pc_get ('DEP_SCALE'       , obj%dep_scale)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (obj%domain(1:1)=='T' .or. obj%domain(1:1)=='t') then
        obj%domain = 'TIME'
      else if (obj%domain(1:1)=='D' .or. obj%domain(1:1)=='d') then
        obj%domain = 'DEPTH'
      else
        call pc_error ('DOMAIN must be TIME or DEPTH.')
        obj%domain = 'TIME'
      end if

      if(obj%domain == 'DEPTH') then
        if(obj%dep_scale .le. 0.0) then
          call pc_error('DEP_SCALE must be greater than 0.0.')
          obj%dep_scale = 1000.0
        else if(obj%dep_scale .ne. 1000.0) then
          call pc_warning('DEP_SCALE is normally 1000.0 (required by CBYT).')
        end if
      end if

      select case (obj%veltype)
           case ('VTNM') ; continue
           case ('VTRM') ; continue
           case ('VTIN') ; continue
           case ('VZIN') ; continue
           case default  ; call pc_error &
                           ('VELTYPE must be VTNM, VTRM, VTIN, or VZIN')
      end select

!!----Unit scale factor for time/depth.
      if(obj%domain == 'TIME') then
        obj%unit_scale = 1.0
      else
        obj%unit_scale = obj%dep_scale
      end if

      cfos_sens  = .false.
      ars_sens   = .false.
      fse_sens   = .false.
      tvfse_sens = .false.

      if (obj%mode(1:1)=='C' .or. obj%mode(1:1)=='c') then
        obj%mode    = 'CFOS'
        cfos_sens   = .true.
        tvfse_sens  = .true.
        obj%nlimits = 1
      else if (obj%mode(1:1)=='A' .or. obj%mode(1:1)=='a') then
        obj%mode    = 'ARS'
        ars_sens    = .true.
        tvfse_sens  = .true.
        obj%nlimits = 2
      else if (obj%mode(1:1)=='F' .or. obj%mode(1:1)=='f') then
        obj%mode    = 'FOLD'
        ars_sens    = .true.
        obj%nlimits = 2
      else
        call pc_error ('MODE must be CFOS, ARS or FOLD.')
        obj%nlimits = 0
      end if

      if (obj%fullstack(1:1)=='Y' .or. obj%fullstack(1:1)=='y') then
        obj%fullstack = 'YES'
      else
        obj%fullstack = 'NO'
      end if

      obj%hdr_off_out = min (max(obj%hdr_off_out, 1), obj%nwih)

      obj%fold_cfos2 = max(obj%fold_cfos,1) / 2
      obj%fold_cfos = 2 * obj%fold_cfos2  +  1
      obj%shft_max  = abs(obj%shft_max)

      if (cfos_sens) then
        if (obj%qc_cfos(1:1)=='T' .or. obj%qc_cfos(1:1)=='t') then
          obj%qc_cfos = 'TRAJ'
        else if (obj%qc_cfos(1:1)=='B' .or. obj%qc_cfos(1:1)=='b') then
          obj%qc_cfos = 'BOTH'
        else
          obj%qc_cfos = 'NONE'
        end if
      else
        obj%qc_cfos = 'NONE'
      end if

      obj%pathname_qc_trc = PATHCHECK_EMPTY
      if (obj%qc_cfos /= 'NONE') then
        call pathcheck ('PATHNAME_QC_CFOS', obj%pathname_qc_cfos, '.avast', &
                        required=.true.)

        if (obj%qc_cfos == 'BOTH') then
          obj%pathname_qc_trc = obj%pathname_qc_cfos
          call pathcheck ('PATHNAME_QC_TRC', obj%pathname_qc_trc, '.trc8', &
                          required=.true.)
          if (qc_cfos_local /= 'BOTH' .and. &
                  pc_get_update_state() == PC_GUI) then
            call pc_warning ('QC_CFOS = BOTH produces a bytefile containing &
                  &as many traces as your prestack data -- NOT INTENDED FOR &
                  &PRODUCTION JOBS!')
          end if
        end if
      else if (pc_get_update_state() /= PC_GUI) then
        obj%pathname_qc_cfos = PATHCHECK_EMPTY
      end if

      obj%num_ang1 = 0

      if (cfos_sens) then
        obj%num_ang1 = obj%num_ang_cfos
        if (pc_verify_array('ANG_CFOS')) then
          do iang = 1, obj%num_ang1
            if (obj%ang_cfos(iang) == FNIL) then
              call pc_error ('Missing element(s) in ANG_CFOS array.')
              call pc_jump_array_element ('ANG_CFOS', iang)
              exit
            else
              obj%ang_cfos(iang) = min (max(obj%ang_cfos(iang),0.0), 90.0)
            end if
          end do
        end if
      else
        if (pc_get_update_state() /= PC_GUI) obj%num_ang_cfos = 0
      end if

      if (ars_sens) then
        if (n_beg /= n_end) then
          call pc_error ('ANG_BEG and ANG_END arrays have different lengths.')
        end if
        obj%num_ang_ars = min (n_beg, n_end)
        obj%num_ang1 = obj%num_ang_ars
        if (pc_verify_arrayset('ANG_BEG_ARRAYSET')) then
          do iang = 1, obj%num_ang1
            if (obj%ang_beg(iang)==FNIL .or. obj%ang_end(iang)==FNIL) then
              call pc_error ('Missing element(s) in ANG_BEG/ANG_END array.')
              call pc_jump_arrayset_row ('ANG_BEG_ARRAYSET', iang)
              exit
            else
              obj%ang_beg(iang) = min (max(obj%ang_beg(iang),0.0), 90.0)
              obj%ang_end(iang) = min (max(obj%ang_end(iang),0.0), 90.0)
            end if
            if (obj%ang_beg(iang) > obj%ang_end(iang)) then
              atemp = obj%ang_beg(iang)
              obj%ang_beg(iang) = obj%ang_end(iang)
              obj%ang_end(iang) = atemp
            else if (obj%ang_beg(iang) == obj%ang_end(iang)) then
              call pc_error ('Interval from ANG_BEG to ANG_END must be &
                             &non-empty.')
              call pc_jump_arrayset_row ('ANG_BEG_ARRAYSET', iang)
              exit
            end if
          end do
        end if
      else
        if (pc_get_update_state() /= PC_GUI) obj%num_ang_ars = 0
      end if

      if (obj%fullstack == 'YES') then
        obj%numtr_out = obj%num_ang1 + 1
      else
        obj%numtr_out = obj%num_ang1
      end if
      if (obj%numtr_out < 1) then
        if (pc_get_update_state() /= PC_GUI) then
          call pc_error('These settings will produce zero (0) output traces!')
        end if
      end if

      obj%fold_ars_min = max(obj%fold_ars_min,1)

      call pathcheck ('PATHNAME_VEL', obj%pathname_vel, 'vel', &
                      required=.true., show = PATHCHECK_INFO_INPUT)

      if (obj%opt_ave(1:1)=='N' .or. obj%opt_ave(1:1)=='n') then
        obj%opt_ave = 'NO'
      else
        obj%opt_ave = 'YES'
      end if

      if (obj%len_smooth > 0.0) then
        obj%nlen_smooth = 2 * nint(0.5*obj%len_smooth / &
          (obj%unit_scale*obj%dt)) + 1
        obj%len_smooth  = (obj%nlen_smooth - 1) * (obj%unit_scale*obj%dt)
      else
        obj%nlen_smooth = 0
        obj%len_smooth  = 0.0
      end if

      call stkscale_check (obj%fse, obj%mscl, obj%tvfse, fse_sens1)
      if (tvfse_sens) fse_sens = fse_sens1

      if (obj%sort_to_panels(1:1)=='N' .or. obj%sort_to_panels(1:1)=='n') then
        obj%sort_to_panels = 'NO'
      else
        obj%sort_to_panels = 'YES'
      end if

!! Determine relationship between trace arrays and velocity/offset arrays,
!! where element 'velfirst' of the velocity array matches element 'trcfirst'
!! of the trace array, and element 1 of the velocity array is always time 0.
      if (obj%tstrt == 0.0) then
        obj%trcfirst = 1
        obj%velfirst = 1
        obj%ndptvel  = obj%ndpt
      else if (obj%tstrt > 0.0) then
        obj%trcfirst = 1
        obj%velfirst = nint(obj%tstrt/obj%dt) + 1
        obj%ndptvel  = obj%ndpt + obj%velfirst - 1
      else ! if (obj%tstrt < 0.0) then
        obj%velfirst = 1
        obj%trcfirst = nint(abs(obj%tstrt)/obj%dt) + 1
        obj%ndptvel  = obj%ndpt - obj%trcfirst + 1
        if (obj%ndptvel <= 0) then
          call pc_error ('No positive times in input traces!')
        end if
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('MODE', (/ 'CFOS', 'ARS ', 'FOLD' /), 3)
      call pc_put_options_field ('FULLSTACK', (/ 'YES', 'NO ' /), 2)
      call pc_put_options_field ('QC_CFOS', (/ 'NONE', 'TRAJ', 'BOTH' /), 3)
      call pc_put_options_field ('OPT_AVE', (/ 'YES', 'NO ' /), 2)
      call pc_put_options_field ('SORT_TO_PANELS', (/ 'YES', 'NO ' /), 2)
      call pc_put_options_field ('DOMAIN', (/ 'TIME ', 'DEPTH' /), 2)
      call pc_put_options_field &
                      ('VELTYPE', (/ 'VTNM', 'VTRM', 'VTIN', 'VZIN' /), 4)

      call pc_put ('DOMAIN'          , obj%domain)
      call pc_put ('VELTYPE'         , obj%veltype)
      call pc_put ('MODE'            , obj%mode)
      call pc_put ('FULLSTACK'       , obj%fullstack)
      call pc_put ('HDR_OFF_OUT'     , obj%hdr_off_out)
      call pc_put ('FOLD_CFOS'       , obj%fold_cfos)
      call pc_put ('SHFT_MAX'        , obj%shft_max)
      call pc_put ('QC_CFOS'         , obj%qc_cfos)
      call pc_put ('PATHNAME_QC_CFOS', obj%pathname_qc_cfos)
      call pc_put ('PATHNAME_QC_TRC', obj%pathname_qc_trc)
      call pc_put ('ANG_CFOS'        , obj%ang_cfos,   obj%num_ang_cfos)
      call pc_put ('ANG_BEG'         , obj%ang_beg,    obj%num_ang_ars)
      call pc_put ('ANG_END'         , obj%ang_end,    obj%num_ang_ars)
      call pc_put ('FOLD_ARS_MIN'    , obj%fold_ars_min)
      call pc_put ('PATHNAME_VEL'    , obj%pathname_vel)
      call pc_put ('OPT_AVE'         , obj%opt_ave)
      call pc_put ('LEN_SMOOTH'      , obj%len_smooth)
      call pc_put ('FSE'             , obj%fse)
      call pc_put ('MSCL'            , obj%mscl)
      call pc_put ('TVFSE'           , obj%tvfse)
      call pc_put_gui_only ('NUMTR'  , obj%numtr)
      call pc_put ('NUMTR_OUT'       , obj%numtr_out)
      call pc_put ('SORT_TO_PANELS'  , obj%sort_to_panels)
      call pc_put ('DEP_SCALE'       , obj%dep_scale)

      if (obj%sort_to_panels == 'YES') then
        call pc_put_global  ('NUMTR'   , 1)
        call pc_put_global  ('GATHERED', .false.)
      else
        call pc_put_global  ('NUMTR'   , obj%numtr_out)
        call pc_put_global  ('GATHERED', .true.)
      end if

      nscratch     = 2 * obj%ndpt * obj%numtr_out  &
                         +  2 * obj%numtr  +  obj%num_ang1
      nstore       = obj%ndpt * obj%num_ang1 * obj%nlimits
      if (obj%opt_ave == 'NO') nstore = nstore * 5
      if (obj%qc_cfos == 'BOTH') nstore = nstore + (obj%ndpt * obj%numtr)
      call pc_put_control ('nscratch' , nscratch)
      call pc_put_control ('nstore'   , nstore)

      call pc_put_control ('need_request', obj%sort_to_panels=='YES')
      call pc_put_control ('need_label'  , obj%sort_to_panels=='YES')

      call pc_put_sensitive_field_flag    ('FOLD_CFOS'       , cfos_sens)
      call pc_put_sensitive_field_flag    ('SHFT_MAX'        , cfos_sens)
      call pc_put_sensitive_field_flag    ('QC_CFOS'         , cfos_sens)
      call pc_put_sensitive_field_flag    ('PATHNAME_QC_CFOS',  &
                                                   obj%qc_cfos /= 'NONE')
      call pc_put_sensitive_array_flag    ('ANG_CFOS'        , cfos_sens)
      call pc_put_sensitive_arrayset_flag ('ANG_BEG_ARRAYSET', ars_sens)
      call pc_put_sensitive_field_flag    ('FOLD_ARS_MIN'    , ars_sens)
      call pc_put_sensitive_field_flag    ('FSE'             , fse_sens)
      call pc_put_sensitive_field_flag    ('MSCL'            , fse_sens)
      call pc_put_sensitive_field_flag    ('TVFSE'           , tvfse_sens)

      if(obj%domain == 'TIME') then
        call pc_put_sensitive_field_flag ('DEP_SCALE', .false.)
      else
        call pc_put_sensitive_field_flag ('DEP_SCALE', .true.)
      end if

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      call mem_free (obj%xbins)
      call mem_free (obj%ybins)
      call mem_free (obj%offlast)
      call mem_free (obj%bytefile)

      if (associated(obj%offbuf)) deallocate           (obj%offbuf)
      if (associated(obj%byte))   call permtfile_close (obj%byte)
      if (associated(obj%velio))  call velio_close     (obj%velio)
      if (associated(obj%temptfile_offtab))  &
                                  call temptfile_close (obj%temptfile_offtab)
      if (obj%sio_avast_lun >= 1) call sio_close       (obj%sio_avast_lun)

      if (pc_do_not_process_traces()) return

!!----Check velocity file:
      call velio_scan_alloc (obj%pathname_vel, obj%numvels, errflag, &
               message, nhx=obj%nhx, nhy=obj%nhy, maxpicks=maxpicks, &
               xbins=obj%xbins, ybins=obj%ybins, nxbins=obj%nxbins,  &
               nybins=obj%nybins)
      if (errflag /= VELIO_OK) then
        call pc_error ('Error scanning velocity file '//obj%pathname_vel)
        call pc_error (message)
        return
      end if
      obj%disktables = .false.
      if (obj%opt_ave == 'NO') then
        if (obj%nxbins==0 .and. obj%nybins==0) then
          call pc_error ("Velocities aren't on a rectangular grid.")
          return
        end if
        if (obj%numvels < 2) then
          call pc_print('Velocity file contains only one velocity function. &
                        &Will handle as if OPT_AVE = YES.')
          obj%opt_ave = 'YES'
        else
          obj%ix1mem = 1
          obj%ix2mem = min (obj%nxbins, 2)
          obj%nxmem  = obj%ix2mem
          obj%iy1mem = 1
          obj%iy2mem = min (obj%nybins, 2)
          obj%nymem  = obj%iy2mem
          allocate (obj%offbuf (obj%ndpt, obj%nxmem, obj%nymem, &
                    obj%num_ang1, obj%nlimits), stat=errflag)
          if (errflag /= 0) then
            call pc_error ('Error allocating offset table memory buffer.')
            return
          end if
          if (obj%nxbins>2 .or. obj%nybins>2) then  ! store tables on disk
            obj%disktables = .true.
            call temptfile_open (obj%temptfile_offtab, 'avastofftab', 0, &
                  obj%ndpt, obj%lun, errflag)
            if (errflag /= TEMPTFILE_OK) then
              call pc_error ('Error opening temp file for offset tables.')
              return
            end if
          end if
        end if
      end if
      allocate (obj%offlast (obj%ndpt, obj%num_ang1, obj%nlimits), &
                stat=errflag)
      if (errflag /= 0) then
        call pc_error ('Error allocating array for current offset table.')
        return
      end if

!!----Open velocity file for reading
      call velio_open_read (obj%velio, obj%pathname_vel, obj%numvels, &
                            errflag, message)
      if (errflag /= VELIO_OK) then
        call pc_error ('Error opening velocity file '//obj%pathname_vel)
        return
      end if

!!----Set up SIO file for output of ".avast" file if requested
      if (obj%qc_cfos /= 'NONE') then
        call sio_open_write (obj%sio_avast_lun, obj%pathname_qc_cfos)
        if (obj%sio_avast_lun < 1) then
          call pc_error ('Error opening QC file '//obj%pathname_qc_cfos)
          return
        end if
        if (obj%opt_ave == 'NO') then
          write(message,797) obj%ndptvel, obj%nxbins, obj%nybins, obj%num_ang1
        else
          write(message,797) obj%ndptvel, 1, 1, obj%num_ang1
        endif
 797    format('### AVAST ANGLE TABLE ',I6,1X,I6,1X,I6,1X,I6)
        call sio_write_card (obj%sio_avast_lun, message)
        write(messagebig,798) obj%ang_cfos(:obj%num_ang1)
 798    format('### angles: ',20(1X,F6.3))
        call avast_strip_zeros (messagebig)
        message = messagebig
        call sio_write_card (obj%sio_avast_lun, message)
        write(message,799) obj%nhx, obj%nhy
 799    format('### offset, x, y headers:   6',1X,I2,1X,I2)
        call sio_write_card (obj%sio_avast_lun, message)
        if(obj%domain == 'TIME') then
          write(message,"('###   OFFSET           X              Y&
            &             TIME          ANGLE')")
        else
          ! assume depth domain
          write(message,"('###   OFFSET           X              Y&
            &             TIME          ANGLE')")
        end if
        call sio_write_card (obj%sio_avast_lun, message)
      end if

!!----Call subroutine to read velocities & create offset tables
      call avast_vels_to_tables (obj, maxpicks)
!!----Close velocity file
      call velio_close (obj%velio)
!!----Close SIO file for ".avast" file if used
      if (obj%sio_avast_lun >= 1) call sio_close (obj%sio_avast_lun)
      if (pc_do_not_process_traces()) return

!!----Open bytefile if needed
      if (obj%qc_cfos == 'BOTH') then
        call mem_alloc (obj%bytefile, obj%ndpt, obj%numtr, status=errflag)
        if (errflag /= 0) return
        call permtfile_open_write (obj%byte, obj%pathname_qc_trc,        &
                                   obj%nwih, obj%ndpt, obj%tstrt, obj%dt, &
                                   obj%lun, errflag, pc_get_ipn(), 8)
        if (errflag /= PERMTFILE_OK) call pc_error ('Error opening bytefile')
      end if

!!----Set h6out array for output values in header word 6
      call mem_alloc (obj%h6out, obj%numtr_out, status=errflag)
      if (errflag /= 0) return
      do i = 1, obj%num_ang1
        if (obj%mode == 'CFOS') then
          obj%h6out(i) = obj%ang_cfos(i)
        else
          obj%h6out(i) = 0.5 * (obj%ang_beg(i) + obj%ang_end(i))
        end if
      end do
      if (obj%fullstack == 'YES') obj%h6out(obj%numtr_out) = 90.
      obj%ncount_h1 = 0

!!----Set scaling parameters for minimal work if MODE = FOLD
      if (obj%mode == 'FOLD') then
        obj%fse   = 0.0
        obj%mscl  = 0.0
        obj%tvfse = 0.0
      end if

!!----Initialize binsort if needed
      if (obj%sort_to_panels == 'YES') then
        call binsort_open (obj%binsort, obj%numtr_out, 'avastsort', &
                           obj%nwih, obj%ndpt, obj%lun, errflag)
        if (errflag /= BINSORT_OK) then
          call pc_error ('Error opening disk file for sorting to panels.')
        end if
      end if

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.      ! needed for the wrapup routine.

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      end subroutine avast_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

      subroutine avast (obj,ntr,hd,tr)

      type(avast_struct),intent(inout) :: obj                   ! arguments
      integer           ,intent(inout) :: ntr                   ! arguments
      double precision  ,intent(inout) :: hd(:,:)               ! arguments
      real              ,intent(inout) :: tr(:,:)               ! arguments

      integer          :: kgood (obj%numtr)                     ! local
      real             :: hdrlav_save (obj%numtr)               ! local
      integer          :: jlast (obj%num_ang1)                  ! local
      real             :: stack (obj%ndpt, obj%numtr_out)       ! local
      real             :: fold  (obj%ndpt, obj%numtr_out)       ! local
      integer          :: irec, iang, ilimit, i1, i2, j1, j2    ! local
      integer          :: ix, iy, errflag, ngood, i, j, k       ! local
      integer          :: kmid, kmid_good, ntrtest, kstrt       ! local
      integer          :: kend, mute1, mute2, nlive, ilive      ! local
      real             :: xnow, ynow, factor, offtestr          ! local
      double precision :: hdtmp(1), offtestd                    ! local
      double precision :: off_out_save                          ! local

      if (ntr == NO_MORE_TRACES) then
        if (obj%sort_to_panels == 'YES') then
          call avast_wrap1 (obj)
          goto 500
        else
          goto 999
        end if
      else if (ntr == NEED_TRACES) then
        if (obj%sort_to_panels == 'YES') goto 500
        call pc_print ('AVAST ERROR: Called with NEED_TRACES but &
                       &SORT_TO_PANELS = NO.')
        ntr = FATAL_ERROR
        goto 999
      end if

      if (obj%opt_ave == 'NO') then  ! AVERAGE OFFSET TABLES IF NECESSARY
        xnow = hd(obj%nhx, 1)
        ynow = hd(obj%nhy, 1)
        if (xnow==obj%xlast .and. ynow==obj%ylast) goto 10
        i1 = obj%ix1mem
        i2 = obj%ix2mem
        j1 = obj%iy1mem
        j2 = obj%iy2mem
        if (obj%disktables) then  ! CHECK LOCATIONS NEEDED IN TABLES
          if (obj%nxbins > 2) then
            call interp_1d_lin (obj%xbins(:obj%nxbins), xnow, i1, i2, factor)
            if (i1 == obj%nxbins) i1 = obj%nxbins - 1
          end if
          if (obj%nybins > 2) then
            call interp_1d_lin (obj%ybins(:obj%nybins), ynow, j1, j2, factor)
            if (j1 == obj%nybins) j1 = obj%nybins - 1
          end if
          if (i1/=obj%ix1mem .or. j1/=obj%iy1mem) then ! MUST READ FROM DISK
            do iy = 1, obj%nymem
              do ix = 1, obj%nxmem
                irec = ((j1+iy-2)*obj%nxbins + i1+ix-2)  &
                                * obj%num_ang1 * obj%nlimits
                do iang = 1, obj%num_ang1
                  do ilimit = 1, obj%nlimits
                    irec = irec + 1
                    call temptfile_read (obj%temptfile_offtab, irec, hdtmp, &
                                      obj%offbuf (:, ix, iy, iang, ilimit), &
                                      errflag)
                    if (errflag /= TEMPTFILE_OK) then
                      call pc_print ('AVAST: Error reading offset tables &
                                     &from temporary disk file.')
                      ntr = FATAL_ERROR
                      goto 999
                    end if
                  end do
                end do
              end do
            end do
            obj%ix1mem = i1
            obj%ix2mem = i2
            obj%iy1mem = j1
            obj%iy2mem = j2
          end if
        end if
        do iang = 1, obj%num_ang1    ! INTERPOLATE FROM TABLES IN MEMORY
          do ilimit = 1, obj%nlimits
            call interp_2d_var_lin_real (obj%xbins(i1:i2),  &
                   obj%ybins(j1:j2), obj%nxmem, obj%nymem,  &
                   obj%offbuf(:,:,:,iang,ilimit), obj%ndpt, &
                   xnow, ynow, obj%offlast(:,iang,ilimit))
          end do
        end do
        obj%xlast = xnow
        obj%ylast = ynow
      end if

 10   continue  !----------------------------WE'RE READY TO START STACKING
      stack = 0.0
      fold  = 0.0
      jlast(:) = obj%ndpt

      if (obj%mode == 'CFOS') then  !----------CONSTANT FOLD OF STACK CASE

        if (obj%qc_cfos == 'BOTH') obj%bytefile = 0.0
        ntrtest = ntr - obj%fold_cfos2 + obj%shft_max

        do j = obj%trcfirst, obj%ndpt      ! BEGIN LOOP OVER TRACE TIMES

          ngood = 0
          do k = 1, ntr
            if ( j >= hd(HDR_TOP_MUTE,k)    .and.  &
                 j <= hd(HDR_BOTTOM_MUTE,k) .and.  &
                 hd(HDR_LAV,k) > 0.0 )  then
              ngood = ngood + 1
              kgood(ngood) = k
            end if
          end do
          if (ngood < obj%fold_cfos) cycle

          do iang = 1, obj%num_ang1             ! BEGIN LOOP OVER ANGLES

            if (j > jlast(iang)) cycle

            offtestd = obj%offlast(j,iang,1)
            if (offtestd < hd(HDR_OFFSET,1)) cycle
            do k = 2, ntr
              if (offtestd <= hd(HDR_OFFSET,k)) exit
            end do
            if (k > ntr) then
              jlast(iang) = j
              cycle
            end if
            if ( (offtestd - hd(HDR_OFFSET,k-1))  >  &
                     (hd(HDR_OFFSET,k) - offtestd) ) then
              kmid = k
            else
              kmid = k - 1
            end if

            if (kmid > ntrtest) then
              jlast(iang) = j
              cycle
            end if

            if (kmid <= kgood(1)) then
              kmid_good = 1
            else
              do k = 2, ngood
                if (kmid <= kgood(k)) exit
              end do
              if (k > ngood) then
                kmid_good = ngood
              else
                if ((kmid-kgood(k-1)) > (kgood(k)-kmid)) then
                  kmid_good = k
                else
                  kmid_good = k - 1
                end if
              end if
            end if

            if (kmid_good < (1 + obj%fold_cfos2)) then
              kstrt = 1
              kend  = obj%fold_cfos
            else if (kmid_good > (ngood - obj%fold_cfos2)) then
              kstrt = ngood - obj%fold_cfos + 1
              kend  = ngood
            else
              kstrt = kmid_good - obj%fold_cfos2
              kend  = kmid_good + obj%fold_cfos2
            end if

            if (abs(kmid - kgood(kstrt+obj%fold_cfos2)) > obj%shft_max) cycle

            do k = kstrt, kend
              stack(j,iang) = stack(j,iang) + tr(j,kgood(k))
            end do
            fold(j,iang) = obj%fold_cfos
            if (obj%qc_cfos == 'BOTH') then
              obj%bytefile(j,kgood(kstrt):kgood(kend))  =  &
                    obj%bytefile(j,kgood(kstrt):kgood(kend))  +  1.0
            end if

          end do                                  ! END LOOP OVER ANGLES
        end do                               ! END LOOP OVER TRACE TIMES

        if (obj%qc_cfos == 'BOTH') then
          hdrlav_save(:ntr) = hd(HDR_LAV,:ntr)
          call lav_set_hdr (hd, obj%bytefile, obj%ndpt, ntr)
          do k = 1,ntr
            call permtfile_write(obj%byte, hd(:,k), obj%bytefile(:,k), errflag)
            if (errflag /= PERMTFILE_OK) then
              call pc_print ('AVAST: Error writing to bytefile')
              ntr = FATAL_ERROR
              goto 999
            end if
          end do
          hd(HDR_LAV,:ntr) = hdrlav_save(:ntr)
        end if

      else  !---------------------------------------ANGLE RANGE STACK CASE

!!------Find where offset trajectories move off far offset
        ntrtest = ntr - obj%fold_ars_min + 1
        if (ntrtest < 1) goto 20
        offtestr = hd(HDR_OFFSET,ntrtest)
        do iang = 1, obj%num_ang1
          do j = obj%trcfirst, obj%ndpt
            if (obj%offlast(j,iang,1) > offtestr) then
              jlast(iang) = j - 1
              exit
            end if
          end do
        end do

        do k = 1, ntr
          if (hd(HDR_LAV,k) <= 0.0) cycle
          offtestr = hd(HDR_OFFSET,k)
          mute1 = max (nint(hd(HDR_TOP_MUTE,k)), obj%trcfirst)
          mute2 = nint(hd(HDR_BOTTOM_MUTE,k))
          do iang = 1, obj%num_ang1
            do j = mute1, min(mute2,jlast(iang))
              if ( offtestr >= obj%offlast(j,iang,1) .and.  &
                   offtestr <= obj%offlast(j,iang,2) )  then
                stack(j,iang) = stack(j,iang) + tr(j,k)
                fold (j,iang) = fold (j,iang) + 1.0
              end if
            end do
          end do
        end do

        do iang = 1, obj%num_ang1
          do j = obj%trcfirst, obj%ndpt
            if (nint(fold(j,iang)) < obj%fold_ars_min) then
              stack(j,iang) = 0.0
              fold (j,iang) = 0.0
            end if
          end do
        end do

      end if

 20   continue
      if (obj%fullstack == 'YES') then  !--INCLUDE FULL STACK IF REQUESTED

        nlive = 0
        do k = 1, ntr
          if (hd(HDR_LAV,k) <= 0.0) cycle
          nlive = nlive + 1
          mute1 = nint(hd(HDR_TOP_MUTE,k))
          mute2 = nint(hd(HDR_BOTTOM_MUTE,k))
          do j = mute1, mute2
            stack(j,obj%numtr_out) = stack(j,obj%numtr_out) + tr(j,k)
            fold (j,obj%numtr_out) = fold (j,obj%numtr_out) + 1.0
          end do
        end do

      end if

!!----Move header of 1st live trace to output locations
      do ilive = 1, ntr
        if (hd(HDR_LAV,ilive) > 0.0) exit
      end do
      if (ilive > ntr) ilive = 1
      do i = 1, obj%numtr_out
        if (i /= ilive) hd(:obj%nwih, i) = hd(:obj%nwih, ilive)
      end do
      off_out_save = hd(HDR_OFFSET, ilive)

!!----Set ntr to size of angle gathers
      ntr = obj%numtr_out

!!----Stack scaling
      do iang = 1, obj%num_ang1
        call stkscale (hd(:,iang), stack(:,iang), fold(:,iang), obj%ndpt, &
                       obj%fse, obj%mscl, obj%tvfse)
      end do
      if (obj%fullstack == 'YES') then
        call stkscale (hd(:,ntr), stack(:,ntr), fold(:,ntr), obj%ndpt, &
                       obj%fse, obj%mscl, obj%tvfse, nlive)
      end if

!!----Move stack or fold to trace array
      if (obj%mode == 'FOLD') then
        tr(:obj%ndpt,:ntr) = fold
      else
        tr(:obj%ndpt,:ntr) = stack
      end if

!!----Set all remaining necessary headers
      do i = 1, ntr
        hd(HDR_SEQUENCE,i)        = obj%ncount_h1 + i
        hd(HDR_CURRENT_CHANNEL,i) = i
        hd(obj%hdr_off_out,i)     = off_out_save
        hd(HDR_OFFSET,i)          = obj%h6out(i)
        hd(HDR_PANEL,i)           = i
      end do
      obj%ncount_h1 = obj%ncount_h1 + ntr
      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      if (obj%sort_to_panels == 'NO') return

 500  call binsort (obj%binsort, ntr, hd, tr)
      if (ntr==NO_MORE_TRACES .or. ntr==FATAL_ERROR) goto 999
      return

 999  call avast_wrapup (obj)

      end subroutine avast

!!------------------------ avast_vels_to_tables ----------------------------!!
!!------------------------ avast_vels_to_tables ----------------------------!!
!!------------------------ avast_vels_to_tables ----------------------------!!
!
! This routine reads the velocity functions and derives offset-time tables.
! Because this routine is called by avast_update, it can use parameter cache.
!
      subroutine avast_vels_to_tables (obj, maxpicks)

      type(avast_struct),intent(inout) :: obj               ! arguments
      integer           ,intent(in)    :: maxpicks          ! arguments

      integer ,parameter :: stride = 50              ! for table printouts
      integer            :: ivel, errflag, j, ix, iy        ! local
      integer            :: iang, ilimit, i, irec           ! local
      real               :: vel_interp (obj%ndptvel)        ! local
      real               :: vel_sum    (obj%ndptvel)        ! local
      real               :: offzero    (obj%ndptvel)        ! local
      double precision   :: hdtmp (1)                       ! local
      character(len=4)   :: veltype, veltype_last           ! local
      character(len=128) :: message                         ! local

      hdtmp(1) = 0.0

      if (obj%opt_ave == 'YES') then  ! derive composite velocity
        vel_sum = 0.0
        do ivel = 1, obj%numvels
          call avast_read_velocity (obj, maxpicks, vel_interp, veltype, &
                                    errflag)
          if (errflag /= 0) goto 100

          if (ivel > 1) then
            if (veltype_last /= veltype) then
              call pc_error ('Velocity type '//veltype//' different &
                             &from type of previous function.')
              goto 100
            end if
          end if
          veltype_last = veltype
          vel_sum = vel_sum + vel_interp
        end do
        vel_sum = vel_sum / obj%numvels
        write (obj%lun, *)
        if (veltype == 'VTIN') then
          write (obj%lun, *) 'Interval velocities in time were input'
        else if (veltype == 'VZIN') then
          write (obj%lun, *) 'Interval velocities in depth were input'
        else
          ! assume velype = VTNM or VTRM 
          write (obj%lun, *) 'Stacking or RMS velocities in time were input'
        end if
        if (obj%domain == 'TIME') then
          write (obj%lun, *) 'Composite input velocity @', &
            stride*obj%dt, ' s'
          write (obj%lun, "(5(F7.2,I6))")  &
               ((j-1)*obj%dt,nint(vel_sum(j)), &
               j=1,obj%ndptvel,stride)
        else
          ! assume depth domain
          write (obj%lun, *) 'Composite input velocity @', &
            stride*(obj%unit_scale*obj%dt), ' depth units'
          write (obj%lun, "(5(F7.0,I6))")  &
               ((j-1)*(obj%unit_scale*obj%dt),nint(vel_sum(j)), &
               j=1,obj%ndptvel,stride)
        end if
        if (veltype == 'VTNM' .or. veltype == 'VTRM') then
          call avast_rms_to_interval_vel (obj%ndptvel, vel_sum, errflag)
          if (errflag /= 0) return
        end if
        call avast_smooth (vel_sum, obj%ndptvel, obj%nlen_smooth, &
                           vel_interp)
        write (obj%lun, *)
        if (obj%domain == 'TIME') then
          write (obj%lun, *) 'Smoothed, composite INTERVAL velocity @', &
                           stride*(obj%unit_scale*obj%dt), ' s'
          write (obj%lun, "(5(F7.2,I6))")  &
               ((j-1)*obj%dt,nint(vel_interp(j)), &
               j=1,obj%ndptvel,stride)
        else
          ! assume depth domain
          write (obj%lun, *) 'Smoothed, composite INTERVAL velocity @', &
                           stride*(obj%unit_scale*obj%dt), ' depth units'
          write (obj%lun, "(5(F7.0,I6))")  &
               ((j-1)*(obj%unit_scale*obj%dt),nint(vel_interp(j)), &
               j=1,obj%ndptvel,stride)
        end if
        obj%nxbins = 1
        obj%nybins = 1
        obj%xlast = 0.0
        obj%ylast = 0.0
      end if

      do iy = 1, obj%nybins
        do ix = 1, obj%nxbins

          if (obj%opt_ave == 'NO') then
            call avast_read_velocity (obj, maxpicks, vel_sum, &
                                      veltype, errflag)
            if (errflag /= 0) goto 100
            write (obj%lun, *)
            write (obj%lun, *) 'Velocity of type ', veltype, ' input at &
                           &location: X =', obj%xlast, ', Y =', obj%ylast
            if (veltype == 'VTNM' .or. veltype == 'VTRM') then
              call avast_rms_to_interval_vel (obj%ndptvel, vel_sum, errflag)
              if (errflag /= 0) return
            end if
            call avast_smooth (vel_sum, obj%ndptvel, obj%nlen_smooth, &
                               vel_interp)
            if (obj%domain == 'TIME') then
              write (obj%lun, *) 'Smoothed INTERVAL velocity @',        &
                               stride*(obj%unit_scale*obj%dt), &
                               's  for location: X =', &
                               obj%xlast, ', Y =', obj%ylast
              write (obj%lun, "(5(F7.2,I6))")  &
                ((j-1)*obj%dt,nint(vel_interp(j)), &
                j=1,obj%ndptvel,stride)
            else
              ! assume depth domain
              write (obj%lun, *) 'Smoothed INTERVAL velocity @',        &
                               stride*(obj%unit_scale*obj%dt), &
                               'depth units for location: X =', &
                               obj%xlast, ', Y =', obj%ylast
              write (obj%lun, "(5(F7.0,I6))")  &
                ((j-1)*(obj%unit_scale*obj%dt),nint(vel_interp(j)), &
                j=1,obj%ndptvel,stride)
            end if
          end if

          ! vel_interp now contains smoothed interval velocities,
          ! sampled at unit_scale*DT.

          if(obj%domain == 'TIME') then
            do iang = 1, obj%num_ang1
              if (obj%mode == 'CFOS') then
                call avast_offset_curve_dt (obj%ang_cfos(iang), obj%ndptvel, &
                           obj%dt, vel_interp, offzero)
                if (obj%qc_cfos /= 'NONE') then
                  do j = 1, obj%ndptvel
                    write (message, 801) min(offzero(j),99999999.), &
                       obj%xlast, obj%ylast, &
                       (j-1)*obj%dt, &
                       obj%ang_cfos(iang)
 801                format (2x,5(f13.3,2x))
                    call sio_write_card (obj%sio_avast_lun, message)
                  end do
                  call sio_write_card (obj%sio_avast_lun, ' ')
                end if
                obj%offlast (obj%trcfirst:, iang, 1) = offzero(obj%velfirst:)
              else
                call avast_offset_curve_dt (obj%ang_beg(iang), obj%ndptvel, &
                           obj%dt, vel_interp, offzero)
                obj%offlast (obj%trcfirst:, iang, 1) = offzero(obj%velfirst:)
                call avast_offset_curve_dt (obj%ang_end(iang), obj%ndptvel, &
                           obj%dt, vel_interp, offzero)
                obj%offlast (obj%trcfirst:, iang, 2) = offzero(obj%velfirst:)
              end if
            end do
          else
            ! assume DOMAIN = DEPTH
            do iang = 1, obj%num_ang1
              if (obj%mode == 'CFOS') then
                call avast_offset_curve_dz (obj%ang_cfos(iang), obj%ndptvel, &
                           (obj%unit_scale*obj%dt), vel_interp, offzero)
                if (obj%qc_cfos /= 'NONE') then
                  do j = 1, obj%ndptvel
                    write (message, 802) min(offzero(j),99999999.), &
                       obj%xlast, obj%ylast, &
                       (j-1)*(obj%unit_scale*obj%dt)*0.001, &
                       obj%ang_cfos(iang)
 802                format (2x,5(f13.3,2x))
                    call sio_write_card (obj%sio_avast_lun, message)
                  end do
                  call sio_write_card (obj%sio_avast_lun, ' ')
                end if
                obj%offlast (obj%trcfirst:, iang, 1) = offzero(obj%velfirst:)
              else
                call avast_offset_curve_dz (obj%ang_beg(iang), obj%ndptvel, &
                           (obj%unit_scale*obj%dt), vel_interp, offzero)
                obj%offlast (obj%trcfirst:, iang, 1) = offzero(obj%velfirst:)
                call avast_offset_curve_dz (obj%ang_end(iang), obj%ndptvel, &
                           (obj%unit_scale*obj%dt), vel_interp, offzero)
                obj%offlast (obj%trcfirst:, iang, 2) = offzero(obj%velfirst:)
              end if
            end do
          end if

          if (obj%trcfirst > 1) then
            obj%offlast (:obj%trcfirst-1, :, :) = 0.0
          end if

          write (obj%lun, *)
          if (obj%opt_ave == 'YES') then
            write (obj%lun, *) ' Offset table for Composite velocity'
          else
           write (obj%lun, *) ' Offset table for velocity at location: X =', &
                              obj%xlast, ', Y =', obj%ylast
          end if
          write (obj%lun, *)
          write (obj%lun, *) '           OFFSET RANGE BY ANGLE'
          write (obj%lun, *)
          if (obj%mode == 'CFOS') then
            if (obj%domain == 'TIME') then
              write (obj%lun, "('  TIME ', 20(5x,i2,5x))") &
                (nint(obj%ang_cfos(i)),i=1,obj%num_ang1)
              write (obj%lun, 863) ('----------', i=1,obj%num_ang1)
              do j = obj%trcfirst, obj%ndpt, stride
                write (obj%lun, '(f6.2, 20(3x,i6,3x))') &
                  obj%tstrt + (j-1)*obj%dt, &
                      (nint(min(obj%offlast(j,i,1),99999.)),i=1,obj%num_ang1)
              end do
            else
              ! assume depth domain
              write (obj%lun, "(' DEPTH ', 20(5x,i2,5x))") &
                (nint(obj%ang_cfos(i)),i=1,obj%num_ang1)
              write (obj%lun, 863) ('----------', i=1,obj%num_ang1)
              do j = obj%trcfirst, obj%ndpt, stride
                write (obj%lun, "(f6.0, 20(3x,i6,3x))") &
                  obj%unit_scale*(obj%tstrt + (j-1)*obj%dt), &
                      (nint(min(obj%offlast(j,i,1),99999.)),i=1,obj%num_ang1)
              end do
            end if
 863        format ('  ---- ', 20(1x,a10,1x))
          else
            if (obj%domain == 'TIME') then
              write (obj%lun, "('  TIME ', 20(3x,a1,i2,'-',i2,a1,3x))") &
                           ('(', nint(obj%ang_beg(i)), &
                           nint(obj%ang_end(i)), ')', i=1,obj%num_ang1)
              write (obj%lun, 963) ('-----------', i=1,obj%num_ang1)
              do j = obj%trcfirst, obj%ndpt, stride
                write (obj%lun, "(f6.2, 20(2x,i5,'-',i5))") &
                  obj%tstrt + (j-1)*obj%dt, &
                     (nint(min(obj%offlast(j,i,1),99999.)),  &
                      nint(min(obj%offlast(j,i,2),99999.)), i=1,obj%num_ang1)
              end do
            else
              ! assume depth domain
              write (obj%lun, "(' DEPTH ', 20(3x,a1,i2,'-',i2,a1,3x))") &
                           ('(', nint(obj%ang_beg(i)), &
                           nint(obj%ang_end(i)), ')', i=1,obj%num_ang1)
              write (obj%lun, 963) ('-----------', i=1,obj%num_ang1)
              do j = obj%trcfirst, obj%ndpt, stride
                write (obj%lun, "(f6.0, 20(2x,i5,'-',i5))") &
                  obj%unit_scale*(obj%tstrt + (j-1)*obj%dt), &
                     (nint(min(obj%offlast(j,i,1),99999.)),  &
                      nint(min(obj%offlast(j,i,2),99999.)), i=1,obj%num_ang1)
              end do
            end if
 963        format ('  ---- ', 20(1x,a11,1x))
          end if

          if (obj%opt_ave == 'NO') then
            if (ix<3 .and. iy<3) then
              obj%offbuf (:, ix, iy, :, :) = obj%offlast
            end if
            if (obj%disktables) then
              irec = ((iy-1)*obj%nxbins + ix-1)  &
                              * obj%num_ang1 * obj%nlimits
              do iang = 1, obj%num_ang1
                do ilimit = 1, obj%nlimits
                  irec = irec + 1
                  call temptfile_write (obj%temptfile_offtab, irec, hdtmp, &
                                       obj%offlast (:, iang, ilimit), errflag)
                  if (errflag /= TEMPTFILE_OK) then
                    call pc_error ('Error writing offset tables to temporary &
                                   &disk file.')
                    return
                  end if
                end do
              end do
            end if
          end if

        end do
      end do

      return

 100  call pc_error ('Problem in velocity at location X =', obj%xlast, &
                     ', Y =', obj%ylast)

      end subroutine avast_vels_to_tables


!!------------------------- avast_read_velocity ----------------------------!!
!!------------------------- avast_read_velocity ----------------------------!!
!!------------------------- avast_read_velocity ----------------------------!!
!
! This routine reads a single velocity function from the velocity file and
! interpolates it to the trace sample rate. Because this routine is called
! only by avast_vels_to_tables, which is itself called only by avast_update,
! we can use the parameter cache.
!
      subroutine avast_read_velocity (obj, maxpicks, vel_interp, &
                                      veltype, errflag)

      type(avast_struct),intent(inout) :: obj               ! arguments
      integer           ,intent(in)    :: maxpicks          ! arguments
      real              ,intent(out)   :: vel_interp(:)     ! arguments
      character(len=4)  ,intent(out)   :: veltype           ! arguments
      integer           ,intent(out)   :: errflag           ! arguments

      integer            :: npicks, i, ipick                ! local
      real               :: ttest                           ! local
      real               :: tpicks(maxpicks)                ! local
      real               :: vpicks(maxpicks)                ! local
      real               :: work(1)                         ! local
      character(len=128) :: message                         ! local

      call velio_read_velfun (obj%velio, obj%xlast, obj%ylast, npicks, &
                     tpicks, vpicks, errflag, message, veltype=veltype)
      if (errflag /= VELIO_OK) then
        call pc_error ('Error reading velocity function')
        return
      end if
      if (veltype == '') veltype = obj%veltype   ! for velocity trace files.
      if (obj%domain == 'TIME' .and. &
          (veltype=='VTNM' .or. veltype=='VTRM')) then
        call intpvelf (npicks, tpicks(:npicks), vpicks(:npicks), work,  &
           4, 0., (obj%unit_scale*obj%dt), obj%ndptvel, &
           vel_interp(:obj%ndptvel), errflag)
        if (errflag /= 0) return
      else if ((obj%domain == 'TIME'  .and. veltype == 'VTIN') .or. &
               (obj%domain == 'DEPTH' .and. veltype == 'VZIN')) then
        ipick = 1
        do i = 1, obj%ndptvel
          ttest = (i-1) * (obj%unit_scale*obj%dt)
          do
            if (ttest<=tpicks(ipick) .or. ipick==npicks) exit
            ipick = ipick + 1
          end do
          vel_interp(i) = vpicks(ipick)
        end do
      else
        errflag = 1
        call pc_error ('Illegal velocity type '//veltype) 
        if(obj%domain == 'TIME') then
          call pc_error ('Velocity type must be VTRM, VTNM, or VTIN.')
        else
          ! assume DOMAIN = DEPTH
          call pc_error ('Velocity type must be VZIN.')
        end if
      end if

      end subroutine avast_read_velocity

!!---------------------- avast_rms_to_interval_vel -------------------------!!
!!---------------------- avast_rms_to_interval_vel -------------------------!!
!!---------------------- avast_rms_to_interval_vel -------------------------!!
!
! This routine converts RMS to Interval velocity (in place within "velocity"
! array). It is assumed that the array begins at time zero. This routine
! calls pc_error to report errors, and also returns errflag = 1 in case of
! error.
!
      subroutine avast_rms_to_interval_vel (ndptvel, velocity, errflag)

      integer , intent(in)    :: ndptvel               ! arguments
      real    , intent(inout) :: velocity(ndptvel)     ! arguments
      integer , intent(out)   :: errflag               ! arguments

      integer     :: j                                 ! local

      if (ndptvel < 2) return

      do j = 1, ndptvel
        velocity(j) = (j-1) * velocity(j)**2
      end do

      velocity(2:ndptvel) = velocity(2:ndptvel) - velocity(1:ndptvel-1)

      if (minval(velocity(2:ndptvel)) <= 0.0) then
        call pc_error ('Error converting RMS to Interval velocity.')
        errflag = 1
        return
      end if

      velocity(2:ndptvel) = sqrt (velocity(2:ndptvel))
      velocity(1) = velocity(2)

      errflag = 0

      end subroutine avast_rms_to_interval_vel

!!----------------------- avast_offset_curve_dt -----------------------------!!
!!----------------------- avast_offset_curve_dt -----------------------------!!
!!----------------------- avast_offset_curve_dt -----------------------------!!
!
! Computes the offset-time trajectory for a given reflection angle.
! Here we assume that times begin at zero (even if TSTRT is non-zero in
! the main process); i.e., the VELINT and OFFSETS arrays both correspond
! to times from 0 to (ndptvel-1)*dt.
!
      subroutine avast_offset_curve_dt (angle, ndptvel, dt, velint, offsets)

      real, parameter :: offset_big_inc = 99999.
!
      real    , intent(in)  :: angle
      integer , intent(in)  :: ndptvel
      real    , intent(in)  :: dt
      real    , intent(in)  :: velint (:)
      real    , intent(out) :: offsets (:)
!
      integer :: i, j
      real    :: snangle, pfactor, v2diff, pinv2
      real    :: vsquare (ndptvel)
!
      offsets(:ndptvel) = 0.
!
      if (angle <= 0.) then
        return
      else if (angle >= 90.) then
        do i = 2, ndptvel
          offsets(i) = (i-1) * offset_big_inc
        end do
        return
      else
        snangle = sin ( real(RADIANS_PER_DEGREE * angle) )
        if (snangle <= 0.) return
        pfactor = 0.5 / snangle
      end if
!
      vsquare(2:ndptvel) = velint(2:ndptvel) ** 2
!
      do i = 2, ndptvel
        if (i < ndptvel) then
          pinv2 = (pfactor * (velint(i)+velint(i+1))) ** 2
        else
          pinv2 = (2. * pfactor * velint(i)) ** 2
        end if
        do j = 2, i
          v2diff = pinv2 - vsquare(j)
          if (v2diff > 0.000001*vsquare(j)) then
            offsets(i) = offsets(i) + dt*vsquare(j)/sqrt(v2diff)
          else
            offsets(i) = offsets(i) + offset_big_inc
          end if
        end do
      end do

      end subroutine avast_offset_curve_dt

!!----------------------- avast_offset_curve_dz -----------------------------!!
!!----------------------- avast_offset_curve_dz -----------------------------!!
!!----------------------- avast_offset_curve_dz -----------------------------!!
!
! Computes the offset-time trajectory for a given reflection angle.
! Here we assume that depths begin at zero (even if TSTRT is non-zero in
! the main process); i.e., the VELINT and OFFSETS arrays both correspond
! to depths from 0 to (ndptvel-1)*dz.
!
      subroutine avast_offset_curve_dz (angle, ndptvel, dz, velint, offsets)

      real, parameter :: offset_big_inc = 99999.
!
      real    , intent(in)  :: angle
      integer , intent(in)  :: ndptvel
      real    , intent(in)  :: dz
      real    , intent(in)  :: velint (:)
      real    , intent(out) :: offsets (:)
!
      integer :: i, j
      real    :: snangle, pfactor, v2diff, pinv2, two_dz
      real    :: vsquare (ndptvel)
!
      offsets(:ndptvel) = 0.
!
      if (angle <= 0.) then
        return
      else if (angle >= 90.) then
        do i = 2, ndptvel
          offsets(i) = (i-1) * offset_big_inc
        end do
        return
      else
        snangle = sin ( real(RADIANS_PER_DEGREE * angle) )
        if (snangle <= 0.) return
        pfactor = 0.5 / snangle
      end if
!
      two_dz = 2.0 * dz
      vsquare(2:ndptvel) = velint(2:ndptvel) ** 2
!
      do i = 2, ndptvel
        if (i < ndptvel) then
          pinv2 = (pfactor * (velint(i)+velint(i+1))) ** 2
        else
          pinv2 = (2. * pfactor * velint(i)) ** 2
        end if
        do j = 2, i
          v2diff = pinv2 - vsquare(j)
          if (v2diff > 0.000001*vsquare(j)) then
            offsets(i) = offsets(i) + two_dz*velint(j)/sqrt(v2diff)
          else
            offsets(i) = offsets(i) + offset_big_inc
          end if
        end do
      end do

      end subroutine avast_offset_curve_dz

!!---------------------------- avast_smooth --------------------------------!!
!!---------------------------- avast_smooth --------------------------------!!
!!---------------------------- avast_smooth --------------------------------!!
!
! Input array XIN is smoothed by applying a running average of length NAVG;
! output in XOUT. The running average is always performed over an ODD number
! of points centered around a desired output point. If NAVG is even, an
! averaging length of NAVG+1 is used. Averaging length is tapered down when
! approaching the beginning and end of the XIN array, to keep the averaging
! operator from running off the ends of that array.
!
      subroutine avast_smooth (xin, n, navg, xout)

      integer , intent(in)  :: n          ! Arguments
      real    , intent(in)  :: xin(n)     ! Arguments
      integer , intent(in)  :: navg       ! Arguments
      real    , intent(out) :: xout(n)    ! Arguments
!
      integer  :: navg2, i, j             ! local variables
      real     :: sfact                   ! local variables
!
      if (navg <= 1) then
        xout = xin
        return
      end if
!
      navg2 = min(navg,n-1)/2
      sfact = 1.0/(2*navg2 + 1)
      xout = 0.
      do j = -navg2, navg2
        do i = 1+abs(j), n-abs(j)
          xout(i) = xout(i) + xin(i+j)
        end do
      end do
      do i = 2, navg2
        xout(i) = xout(i) / (2*i-1)
      end do
      xout(navg2+1:n-navg2) = xout(navg2+1:n-navg2) * sfact
      do i=n-navg2+1,n-1
        xout(i) = xout(i) / (2*(n-i)+1)
      end do

      end subroutine avast_smooth

!!------------------------- avast_strip_zeros ------------------------------!!
!!------------------------- avast_strip_zeros ------------------------------!!
!!------------------------- avast_strip_zeros ------------------------------!!

      subroutine avast_strip_zeros (message)

      character(len=*), intent(inout) :: message               ! arguments
      integer   :: i                                           ! local

      do
        i = index(message, '000 ')
        if (i == 0) exit
        message = message(:i-1) // message(i+3:)
      end do

      do
        i = index(message, '00 ')
        if (i == 0) exit
        message = message(:i-1) // message(i+2:)
      end do

      do
        i = index(message, '0 ')
        if (i == 0) exit
        message = message(:i-1) // message(i+1:)
      end do

      end subroutine avast_strip_zeros

!!--------------------------- private wrapup -------------------------------!!
!!--------------------------- private wrapup -------------------------------!!
!!--------------------------- private wrapup -------------------------------!!

      subroutine avast_wrap1 (obj)

      type(avast_struct),intent(inout) :: obj       ! arguments

      call mem_free (obj%xbins)
      call mem_free (obj%ybins)
      call mem_free (obj%offlast)
      call mem_free (obj%bytefile)

      if (associated(obj%byte))   call permtfile_close (obj%byte)
      if (associated(obj%offbuf)) deallocate           (obj%offbuf)
      if (associated(obj%temptfile_offtab))  &
                                  call temptfile_close (obj%temptfile_offtab)

      end subroutine avast_wrap1

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

      subroutine avast_wrapup (obj)

      type(avast_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call avast_wrap1 (obj)
      if (associated(obj%binsort)) call binsort_close(obj%binsort)

      end subroutine avast_wrapup

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module avast_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
