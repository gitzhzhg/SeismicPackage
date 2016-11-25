!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2001-03-02. />

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
! Name       : TSLC    (Time-SLiCe)
! Category   : sorts
! Written    : 1989-10-20   by: Greg Lazear
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Create time-slice and header-slice traces from a 3D volume.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! TSLC outputs "header-slices" before time-slices.   Header-slices and
! time-slices are each output in the order of entry in the HEADERS array,
! the TIMES array and regular TIME_xxx intervals.
! Header word 30 identifies the output slice (header word number or
! time of slice).
!
! Orientation of the output time-slice plots is determined by the ORIENT
! parameters.  (Users should follow the plotting hints for COLR parameters that
! are printed in .RPT file.)
!
! Traces may be input in any order.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!
! Slice traces may be output either to a file, the trace output stream or both.
!
!     If OPT_OUT = TRACE then slice traces are output in the trace output
!     stream.
!
!     If OPT_OUT = FILE then slice traces are written to a designated file
!     and input traces pass through unchanged.
!
!     If OPT_OUT = BOTH then slice traces are written to a designated file AND
!     are output in the trace output stream.
!
!
! If time slices on a regular time interval are desired, the regular time
! interval parameters may be used.  Times specified by these parameters are
! IN ADDITION to those specified by the TIMES array.
!
! TSLC may change global values for NDPT, DT and/or TSTRT.
! Globals are changed when TSLC is added to the processing flow.
! They may also be changed when certain TSLC parameters are changed
! such as OPT_OUT, X_TOT, Y_TOT, ORIENT_X and ORIENT_Y.
!
! IF GLOBALS ARE CHANGED, THEN SUBSEQUENT TROT AND TTROT DEFAULT VALUES
! FOR TIM_BEG AND TIM_END MAY NEED TO BE ADJUSTED.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is an all-trace (loop-splitting) process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
! This process alters input traces.
!
! This process outputs one trace at a time.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used and set to 64
! NDPT     number of sample values in trace      used and changed, iff OPT_OUT
!                                                is FILE or BOTH.
! TSTRT    starting time on trace                used and possibly changed
! DT       trace sample interval                 used and changed, iff OPT_OUT
!                                                is FILE or BOTH.
! GRID     grid transform                        used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!                                    Changed iff OPT_OUT is TRACE or BOTH.
! *       all headers                zeroed by default
! 1       Sequential Trace Count     Renumbered.
! 2       Head mute                  Reset
! 7       midpoint of shot           set to X inline coordinate
! 25      largest absolute value     recomputed
! 30      scratch header             set to hdr number or time of slice
! 37      midpoint of shot           set to X inline coordinate
!         HDR_X                      Used for slice output
!         HDR_Y                      used for slice output
! 64      Tail mute                  Reset
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!032. 2006-10-16 D. Glover  Added NULLIFY statements for Intel compiler.
!031. 2006-01-10 B. Menger  Removed Unused Variables.
! 30. 2002-09-11 Goodger    Use mth module for binning.
! 29. 2002-08-01 Stoeckley  Use the PATHCHOOSE primitive for the PATH_OUT param.
! 28. 2002-04-15 Selzler    Set scratch header 30 to header number or
!                           time of slice being output.
! 27. 2001-11-16 Selzler    Changed GUI layout per user request.
! 26. 2001-10-18 Selzler    Added HDR_FLAG parameter to select sliced traces,
!                           increased PATH_OUT to 128 character max,
!                           increased max output header count from 10 to 100.
! 25. 2001-09-28 Selzler    Corrected problem with SCALE and warning message.
! 24. 2001-08-17 Selzler    Clarified phrasing for one message.
! 23. 2001-07-12 Selzler    Correct problems with global usage.
! 22. 2001-06-20 Selzler    Correct tr/out_buf sharing bug.
! 21. 2001-05-17 Selzler    Added warning when globals are changed.
! 20. 2001-04-04 Selzler    Fixed bug in FILE output and global setup
! 19. 2001-03-14 Selzler    Added OPT_OUT and regular TIME_xxx intervals.
! 18. 2000-12-11 Selzler    Changed wrapup logic to use skip_wrapup
! 17. 2000-11-29 Selzler    If times entry is nil, squeeze it out of table
! 16. 2000-09-07 Selzler    Changed suggested scale factor for plots.
! 15. 2000-08-15 Selzler    Changed keywords to coordinate based standard.
!                           Added INVERT_PLOT feature.
! 14. 2000-07-07 Selzler    Fixed problems found by CPS Fortran Code Review.
! 13. 2000-05-11 Selzler    Inserted EZGUI layout and fixed GUI support
! 12. 2000-05-10 Selzler    Fixed problems in the GUI support
! 11. 2000-02-02 Selzler    Added support for GUI and general cleanup
! 10. 1999-10-27 Selzler    Added RCS "Id" strings to tag executeable
! 9.  1999-09-21 Selzler    added ORIENT_X and ORIENT_Y functionality.
!                           This required a total rewrite of the code.
! 8.  1999-09-13 Selzler    Conversion to f90
! 7.  1998-12-01 Goodger    Begin using the fortran90 compiler.
! 6.  1998-08-12 Corn       Heretofore, the sample rate coming out of the
!                           process (SROUT) was arbitrarily set to 0.01.
!                           Now it can be set by the SROUT paramenter.
!                           This should give the user more control over
!                           display size and labelling.
!                           Defaults are determined as:
!                           If 1 <= INCY < 100 then SROUT = INCY / 100.
!                           If 100 <= INCY < 10000 then SROUT=INCY/10000.
!                           otherwise SROUT = 0.01.
! 5.  1997-03-11 Corn       The parameters HWX, HWY, INCX, and INCY were
!                           added.  The parameters MINXB, MINYB, MAXXB,
!                           and MAXYB were made to be real instead of
!                           integer.  The # of samples was redefined NYB =
!                           (MAXYB-MINYB+INCY)/INCY and the # of traces
!                           was redefined NXB = (MAXXB-MINXB+INCX)/INCX.
!                           In the past INCY was always 1.0.  Now if INCY
!                           is not 1.0, expect the time scale to reflect
!                           that. Heretofore times entered were treated as
!                           offsets from TSTRT.  Now times are treated as
!                           absolute times.  All times must fall within
!                           the given trace time window.
! 4.  1992-09-18 Peterson   Fix bugs due to splicing because of too small
!                           MEMSIZE. 1. DO 60 loop would store values in
!                           BUF(IADR) even with a bad IADR calculation.
!                           2. Number of traces in a time slice was wrong.
! 3.  1991-01-30 Stoeckley  Change TSTRT calculation to improve Y bin
!                           labelling; change to use headers 7 and 8
!                           for bin values (rather than 17 and 18 and
!                           rotation matrix); add to documentation; and
!                           add printout of new global values.
! 2.  1990-08-16 Lazear     Fix bug in TSTRT calculation and integer the
!                           HEADERS array.
! 1.  1989-10-20 Lazear     Original version converted from CONSEIS
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NEED_TRACES    if more traces are needed before output.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! TSLC trace processing is divided into two major phases.
!
!   Phase 1: read input traces and save only the portion required
!     in a tmp file as a sequence of mini traces (not transposed).
!     The word sequence within one mini traces is input position (X inline
!     and Y crossline bin index), selected headers and selected time samples.
!     If OPT_OUT is TRACE or BOTH, then:
!         During this phase TSLC returns NTR = NEED_TRACES (no output traces).
!     If OPT_OUT is FILE, then:
!         During this phase TSLC returns NTR unchanged from the call.
!     When all input traces have been consumed, phase 2 is started.
!
!   Phase 2: read swaths of mini traces from the tmp file, transposes the
!    data and construct one output trace.
!    If OPT_OUT is TRACE or BOTH, then:
!        Return with NTR = 1, single trace output from TSLC.
!        When all time slices have been returned, NTR = NO_MORE_TRACES.
!    If OPT_OUT is FILE, then:
!        Write the output trace to a file and repeat phase 2 processing.
!        When all time slices have been written, return NTR = NO_MORE_TRACES.
!
!    If HEADERS are selected, they are returned as a psuedo time slice.
!    Next, slices through the trace samples are returned at selected TIMES.
!    Next, slices through the trace samples are returned at regular TIMES.
!
!   Note: double precision headers are converted to single precision real,
!     when assembling the mini trace for the tmp file.
!
!   Caution: the last trace that falls within a particular bin is used,
!     i.e. no attempt is made to select the closest available trace.
!     If no traces fall within a bin, the header and samples are zeroed
!     but the mute index still implies that the trace is live.
!     This is consistant with pre 1999-09-21 versions of the code (SELZLER).
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS TSLC Process/NC=80>
!
! Create slices through headers and or samples from a 3D volume.
!
! OPT_OUT=`CCCCC        HDR_FLAG=`IIIII
!
! Select PATH_OUT[PATH_OUT]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! HDR_X =`II
! X_INIT=`FFFFFFFFFFF  X_INC=`FFFFFFFFFFF  X_LAST=`FFFFFFFFFFF  X_TOT =`IIIIIIIIII
!
! HDR_Y =`II
! Y_INIT=`FFFFFFFFFFF  Y_INC=`FFFFFFFFFFF  Y_LAST=`FFFFFFFFFFF  Y_TOT= `IIIIIIIIII
!
! ORIENT_X=`CCCCC    ORIENT_Y=`CCCCC
!
!              HEADERS                TIMES
!              `IIIIIIII              `FFFFFFFFFFF
!              `IIIIIIII              `FFFFFFFFFFF
!              `IIIIIIII              `FFFFFFFFFFF
!              `IIIIIIII              `FFFFFFFFFFF
!              `IIIIIIII              `FFFFFFFFFFF
!
! TIME_INIT=`FFFFFFFFF  TIME_INC=`FFFFFFFFF  TIME_LAST=`FFFFFFFFF  TIME_TOT=`IIIII
!
! SAMP_INT_OUT=`FFFFFFFFFFF    SCALE=`FFFFFFFFFFF
!<PARMS PATH_OUT[/ML=140/XST]>
!<PARMS info[path_out_info/EN]>
!<PARMS HEADERS[/XST/YST]>
!<PARMS TIMES[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_OUT">
!<Tip> Option whether slice traces are written to a file or passed out. </Tip>
! Default = TRACE
! Allowed = TRACE
! Allowed = FILE
! Allowed = BOTH
! If OPT_OUT = TRACE then slice traces are output in the trace output stream.
!
! If OPT_OUT = FILE then slice traces are written to a designated file
! and input traces pass through unchanged.
!
! If OPT_OUT = BOTH then slice traces are written to a designated file AND are
! output in the trace output stream.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces are sliced.  Otherwise, only traces with
! a non-zero value in header word HDR_FLAG are sliced.
!
!</Help>
!
!<Help KEYWORD="PATH_OUT_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_OUT. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATH_OUT">
!<Tip> Choose PATH_OUT using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_OUT">
!<Tip> Pathname for the output slice file if OPT_OUT = FILE or BOTH. </Tip>
! Default = NONE
! Allowed = char
!</Help>
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word for X coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word for Y coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="X_INIT">
!<Tip> Initial value of X coordinate for time-slice image. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="Y_INIT">
!<Tip> Initial value of Y coordinate for time-slice image. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="X_INC">
!<Tip> Increment for X coordinate for time-slice image. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment for Y coordinate for time-slice image. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="X_LAST">
!<Tip> Last value of X coordinate for time-slice image. </Tip>
! Default = 1.0
! Allowed = real>=X_INIT
!</Help>
!
!<Help KEYWORD="Y_LAST">
!<Tip> Last value of Y coordinate for time-slice image. </Tip>
! Default = 1.0
! Allowed = real>=Y_INIT
!</Help>
!
!<Help KEYWORD="X_TOT">
!<Tip> Total number of X coordinate values for time-slice image. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="Y_TOT">
!<Tip> Total number of Y coordinate values for time-slice image. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="ORIENT_X">
!<Tip> Desired orientation of positive X coordinate axis. </Tip>
! Default = UP
! Allowed = UP
! Allowed = DOWN
! Allowed = RIGHT
! Allowed = LEFT
!</Help>
!
!<Help KEYWORD="ORIENT_Y">
!<Tip> Desired orientation of positive Y coordinate axis. </Tip>
! Default = RIGHT
! Allowed = UP
! Allowed = DOWN
! Allowed = RIGHT
! Allowed = LEFT
! If ORIENT_X = UP or DOWN then ORIENT_Y can only be RIGHT or LEFT.
!
! If ORIENT_X = RIGHT or LEFT then ORIENT_Y can only be UP or DOWN .
!</Help>
!
!-------------------header or time selection parameters-------------------------
!
!<Help KEYWORD="HEADERS">
!<Tip> Array of header words for which "header-slices" are desired. </Tip>
! Default =  -
! Allowed = int array (100 elements max)
! Array of header word numbers (1 - NWIH) for which "header-slices" are desired.
!</Help>
!
!<Help KEYWORD="TIMES">
!<Tip> Array of times for which time-slices are desired. </Tip>
! Default =  -
! Allowed = real array (0 through 100 - TIME_TOT)
! All TIMES must be within the time range of input trace samples.
!</Help>
!
!<Help KEYWORD="TIME_INIT">
!<Tip> Time for first time slice (regular interval), in seconds. </Tip>
! Default = TSTRT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIME_INC">
!<Tip> Increment between regular time slice times, in seconds. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIME_LAST">
!<Tip> Time for last regular time slice, in seconds. </Tip>
! Default = NONE
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIME_TOT">
!<Tip> Total number of regular time slices. </Tip>
! Default = 0
! Allowed = int (0 through 100 - TIMES-element-size)
!</Help>
!
!<Help KEYWORD="SAMP_INT_OUT">
!<Tip> Sample interval of output traces. </Tip>
! Default = absolute value of output sample rate
! Allowed = real>0
!</Help>
!
!<Help KEYWORD="SCALE">
!<Tip> Desired scale for time-slice plots, in bins per vertical inch. </Tip>
! Default = 10.0
! Allowed = real>0
! Value of SCALE is used only for plotting hints that are printed in the .RPT
! file.  The hints include how many "Inches per second" are required to
! achieve the "SCALE" number of bins per vertical inch.
! Note: temporal sample rate here corresponds to X or Y spatial sample rate.
! The number of "Traces per inch" required to maintain the physical aspect
! ratio is also printed.  If these plotting parameters are used, then
! a circle in map view should remain round and not eliptical.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module tslc_module
  use pc_module
  use named_constants_module
  use lav_module
  use trcio_module
  use pathcheck_module
  use pathchoose_module
  use getlun_module
  use pattern_module
  use grid_module
  use mth_module
  implicit none
  private
  public :: tslc_create     ! uses the parameter cache.
  public :: tslc_initialize
  public :: tslc_update     ! uses the parameter cache.
  public :: tslc_delete
  public :: tslc            ! main execution (trace processing) routine.
  public :: tslc_wrapup


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  integer,parameter :: OUT_NDPT_OLD_INIT = -987654321
  integer,parameter :: SWATH_FACTOR = 256 * 1024
  integer,parameter :: SIZE_MEM = 2 * 1024 * 1024
  integer,parameter :: HEADERS_MAX = 100
  integer,parameter :: TIMES_MAX = 100
  integer, parameter :: MAX_LINE = 132 ! Maximum number of characters
                                       ! (columns) in one line

  type,public :: tslc_struct
  private
    logical                    :: skip_wrapup      ! dependent parameter

    character(len=5)           :: opt_out          ! process parameter
    integer                    :: hdr_flag         ! process parameter
    character(len=FILENAME_LENGTH) :: path_out     ! process parameter

    integer                    :: hdr_x            ! process parameter
    integer                    :: hdr_y            ! process parameter
    real                       :: x_init           ! process parameter
    real                       :: y_init           ! process parameter
    real                       :: x_inc            ! process parameter
    real                       :: y_inc            ! process parameter
    real                       :: x_last           ! process parameter
    real                       :: y_last           ! process parameter
    integer                    :: x_tot            ! process parameter
                                                   ! == out_x_cnt
                                                   ! or out_ndpt
    integer                    :: y_tot            ! process parameter
                                                   ! == out_ndpt
                                                   ! or out_x_cnt
    real                       :: samp_int_out     ! process parameter
    real                       :: scale            ! process parameter
    character(len=5)           :: orient_x         ! process parameter
    character(len=5)           :: orient_y         ! process parameter
    integer,dimension(HEADERS_MAX) :: headers      ! process parameter
    real,dimension(TIMES_MAX)  :: times            ! process parameter
    real                       :: time_init        ! process parameter
    real                       :: time_inc         ! process parameter
    real                       :: time_last        ! process parameter
    integer                    :: time_tot         ! process parameter

    integer                    :: nwih             ! global parameter
    integer                    :: ndpt             ! global parameter
    type(grid_struct)          :: grid             ! global parameter
    real                       :: dt               ! global parameter
    real                       :: tstrt            ! global parameter
    logical                    :: invert_plot      ! global parameter

    integer                    :: ndpt_save        ! global parameter
    real                       :: dt_save          ! global parameter
    real                       :: tstrt_save       ! global parameter

    type(trcio_struct),pointer :: trcio_file       ! dependent parameter

    integer                    :: header_cnt       ! dependent parameter
    integer                    :: time_cnt         ! dependent parameter
    integer                    :: time_cnt_tot     ! dependent parameter
    integer                    :: out_ndpt         ! dependent parameter
                                                   ! == y_tot
                                                   ! or x_tot
    integer                    :: out_ndpt_old     ! dependent parameter
    real                       :: out_tstrt        ! dependent parameter
                                                   ! for binning equation
    real                       :: out_tend         ! dependent parameter
                                                   ! for binning equation
    real                       :: out_dt_signed    ! dependent parameter
                                                   ! signed value
                                                   ! for binning equation
                                                   ! == +/- y_inc
                                                   ! or +/- x_inc
    integer                    :: out_x_cnt        ! dependent parameter
                                                   ! == x_tot
                                                   ! or y_tot
    real                       :: out_x_start      ! dependent parameter
                                                   ! for binning equation
    real                       :: out_x_inc        ! dependent parameter
                                                   ! signed value
                                                   ! for binning equation
                                                   ! == +/- x_inc
                                                   ! or +/- y_inc

    integer                    :: out_x_base_x     ! output X, base x input
    integer                    :: out_x_base_y     ! output X, base y input
    integer                    :: out_x_sign_x     ! output x, sign x input
    integer                    :: out_x_sign_y     ! output x, sign y input

    integer                    :: out_t_base_x     ! output t, base x input
    integer                    :: out_t_base_y     ! output t, base y input
    integer                    :: out_t_sign_x     ! output t, sign x input
    integer                    :: out_t_sign_y     ! output t, sign y input

    integer                    :: out_y_cnt        ! dependent parameter
                                                   ! == header_cnt
                                                   !    + time_cnt_tot
    integer                    :: out_x_idx        ! dependent parameter
                                                   ! 1 -> out_x_cnt
    integer                    :: out_y_idx        ! dependent parameter
                                                   ! 1 -> out_y_cnt
    integer                    :: mini_max         ! dependent parameter
                                                   ! maximum number of mini
                                                   ! traces per mini_buf
    integer                    :: mini_idx         ! dependent parameter
                                                   ! current mini trace within
                                                   ! mini_buf
    integer,dimension(TIMES_MAX) :: in_time_idx    ! dependent parameter
                                                   ! tr sample index
                                                   ! corresponding to times.
                                                   ! vector gather indices.
    real,dimension(:,:),pointer :: mini_buf        ! dependent parameter
                                                   ! 1st dim=out_y_cnt
                                                   ! 2nd dim=mini_max
    real,dimension(:,:,:),pointer :: slice_buf     ! dependent parameter
                                                   ! 1st dim=out_ndpt
                                                   ! 2nd dim=out_x_cnt
                                                   ! 3rd dim=slice_buf_max
    real,dimension(:,:),pointer  :: out_buf        ! dependent parameter
                                                   ! 1st dim=out_ndpt
                                                   ! 2nd dim=1
    integer                    :: slice_buf_idx    ! dependent parameter
                                                   ! out Y crossline index of
                                                   ! first slice in buffer,
                                                   ! i.e. slice_buf(:,:,1)
    integer                    :: slice_buf_cnt    ! dependent parameter
                                                   ! Number of slices actually
                                                   ! used within buffer,
                                                   ! i.e. slice_buf(:,:,CNT)
    integer                    :: slice_buf_max    ! dependent parameter
                                                   ! maximum number of slices
                                                   ! in slice_buf.
    integer                    :: tmp_lun          ! dependent parameter
    double precision           :: delx             ! dependent parameter
    double precision           :: dely             ! dependent parameter
    type(pathchoose_struct),pointer :: dialog_out
  end type tslc_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

  type(tslc_struct),pointer,save :: object      ! needed for traps.

  integer :: print_lun = 0                        ! state variable

  character(len=5),dimension(3),parameter :: OPT_OUT_OPTIONS = &
    (/'TRACE','FILE ','BOTH '/)

  character(len=100),public :: tslc_ident = &
    "$Id: tslc.f90,v 1.32 2006/10/17 13:45:48 Glover prod sps $"

  contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

  subroutine tslc_create (obj)
  implicit none
  type(tslc_struct),pointer :: obj       ! arguments

  allocate (obj)

  nullify (obj%trcio_file)
  nullify (obj%mini_buf)
  nullify (obj%slice_buf)
  nullify (obj%out_buf)
  nullify (obj%dialog_out) ! jpa

  call pathchoose_create (obj%dialog_out, 'PATH_OUT', 'trc')

  call tslc_initialize (obj)

  return
  end subroutine tslc_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

  subroutine tslc_delete (obj)
  implicit none
  type(tslc_struct),pointer :: obj       ! arguments

  integer :: ier

  call tslc_wrapup (obj)

  if (associated(obj%mini_buf)) deallocate (obj%mini_buf)
  if (associated(obj%slice_buf)) deallocate (obj%slice_buf)
  if (associated(obj%out_buf)) deallocate (obj%out_buf)
  if(associated(obj%trcio_file)) ier = trcio_close(obj%trcio_file)

  call pathchoose_delete (obj%dialog_out)

  deallocate(obj)

  return
  end subroutine tslc_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

  subroutine tslc_initialize (obj)
  implicit none
  type(tslc_struct),pointer :: obj       ! arguments

  call pc_get_global('ndpt', obj%ndpt)
  call pc_get_global('dt', obj%dt)
  call pc_get_global('tstrt', obj%tstrt)

  obj%opt_out = 'TRACE'
  obj%hdr_flag = 0
  obj%path_out = PATHCHECK_EMPTY
  obj%hdr_x = 8
  obj%hdr_y = 7
  obj%x_init = 1.0
  obj%y_init = 1.0
  obj%x_inc = 1.0
  obj%y_inc = 1.0
  obj%x_last = 1.0
  obj%y_last = 1.0
  obj%x_tot = 1
  obj%y_tot = 1
  obj%samp_int_out = 0.01
  obj%scale = 10.0
  obj%orient_x = 'UP'
  obj%orient_y = 'RIGHT'
  obj%headers = 0
  obj%times = 0.0
  obj%time_init = obj%tstrt
  obj%time_inc = 1.0
  obj%time_last = 1.0
  obj%time_tot = 0

  obj%nwih = 64
  obj%ndpt = 0
  call grid_initialize(obj%grid)
  obj%nwih = 64
  obj%dt = 0.001
  obj%tstrt = 0.0
  obj%invert_plot = .true.

  obj%ndpt_save = 0
  obj%dt_save = 0.0
  obj%tstrt_save = 0.0

  obj%header_cnt = 0
  obj%time_cnt = 0
  obj%time_cnt_tot = 0

  obj%out_ndpt = 0
  obj%out_ndpt_old = OUT_NDPT_OLD_INIT
  obj%out_tstrt = 0.0
  obj%out_dt_signed = 0.0

  obj%out_x_cnt = 0
  obj%out_x_start = 0.0
  obj%out_x_inc = 0.0

  obj%out_x_base_x = 0
  obj%out_x_base_y = 0
  obj%out_x_sign_x = 0
  obj%out_x_sign_y = 0

  obj%out_t_base_x = 0
  obj%out_t_base_y = 0
  obj%out_t_sign_x = 0
  obj%out_t_sign_y = 0

  obj%out_y_cnt = 0
  obj%out_x_idx = 0
  obj%out_y_idx = 0

  obj%mini_max = 0
  obj%mini_idx = 0

  obj%in_time_idx = 0

  obj%slice_buf_idx = 0
  obj%slice_buf_cnt = 0
  obj%slice_buf_max = 0

  obj%tmp_lun = -1
  obj%delx = 0.0
  obj%dely = 0.0

  print_lun = pc_get_lun()

  call tslc_update (obj)

  return
  end subroutine tslc_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

  subroutine tslc_update (obj)
  implicit none
  type(tslc_struct),target :: obj                           ! arguments

  integer :: nstore               ! local
  integer :: nscratch             ! local
  integer :: ndisk                ! local
  integer :: ier                  ! local
  integer :: ier1                 ! local
  integer :: ier2                 ! local
  integer :: ier3                 ! local
  integer :: status               ! local
  integer :: item                 ! local
  integer :: item2               ! local
  character(len=MAX_LINE) :: msg  ! local
  real :: vert_scale_min, horz_scale_max
  real :: vert_scale_nom, horz_scale_nom
  real :: average_scale
  real :: end_time, regular_time
  REAL :: plot_units_per_sec
  real :: plot_traces_per_inch, plot_inches_per_sec   
  integer :: num_global_cards
  character(len=pc_datacard_length),dimension(:),pointer :: global_cards
  integer :: state
  logical :: verify

     nullify (global_cards) ! jpa
     object => obj         ! needed for traps.
     obj%skip_wrapup = .true.

     state = pc_get_update_state()

     if(state == PC_FRONTEND .or. state == PC_BACKEND) then
       verify = .true.
     else
       verify = .false.
     end if

    obj%out_ndpt_old = obj%out_ndpt

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

  if (pathchoose_update(obj%dialog_out,obj%path_out)) return

  call pc_get_global ('nwih', obj%nwih)
  call pc_get_global ('ndpt', obj%ndpt)
  call pc_get_global ('grid', obj%grid)
  call pc_get_global ('dt', obj%dt)
  call pc_get_global ('tstrt', obj%tstrt)

!??? defer support for an input invert_plot value
! if(pc_global_keyword_present('invert_plot')
!   call pc_get_global ('invert_plot', obj%invert_plot)
! end if

  call pc_get('opt_out', obj%opt_out)
  call string_to_upper(object%opt_out)

  call pc_get('hdr_flag', obj%hdr_flag)
  call pc_get('path_out', obj%path_out)
  call pc_get('hdr_x', obj%hdr_x)
  call pc_get('hdr_y', obj%hdr_y)
  call pc_get('x_init', obj%x_init, tslc_x_pattern_trap)
  call pc_get('y_init', obj%y_init, tslc_y_pattern_trap)
  call pc_get('x_inc', obj%x_inc, tslc_x_pattern_trap)
  call pc_get('y_inc', obj%y_inc, tslc_y_pattern_trap)
  call pc_get('x_last', obj%x_last, tslc_x_pattern_trap)
  call pc_get('y_last', obj%y_last, tslc_y_pattern_trap)
  call pc_get('x_tot', obj%x_tot, tslc_x_pattern_trap)
  call pc_get('y_tot', obj%y_tot, tslc_y_pattern_trap)
  call pc_get('samp_int_out', obj%samp_int_out)
  call pc_get('scale', obj%scale)
  call pc_get('orient_x', obj%orient_x, tslc_orient_x_trap)
  call pc_get('orient_y', obj%orient_y, tslc_orient_y_trap)
  call pc_get('headers', obj%headers, obj%header_cnt)
  call pc_get('times', obj%times, obj%time_cnt)
  call pc_get('time_init', obj%time_init)
  call pc_get('time_inc', obj%time_inc)
  call pc_get('time_last', obj%time_last)
  call pc_get('time_tot', obj%time_tot)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

  if(all(OPT_OUT_OPTIONS /= obj%opt_out)) then
    call pc_error( &
      'Invalid OPT_OUT value. Valid values are TRACE, FILE and BOTH')
    obj%opt_out = 'TRACE'
  end if

! if(obj%opt_out == 'FILE' .or. obj%opt_out == 'BOTH') then
!   if(verify .or. pc_verify_scalar('path_out')) then
!     call pathcheck('path_out', obj%path_out, ext='trc', &
!       required=.true., status=status)
!
!     if(status /= PATHCHECK_VALID) then
!       call pc_error("TSLC: invalid, PATH_OUT= ", obj%path_out)
!     end if
!   end if
! end if

  call pathcheck('path_out', obj%path_out, ext='trc', &
                 required=(obj%opt_out == 'FILE' .or. obj%opt_out == 'BOTH'), &
                 show=PATHCHECK_INFO_OUTPUT)

  if(obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih) then
    call pc_error( 'HDR_FLAG < 0 or > NWIH')
    obj%hdr_flag = 0
  end if

  if(obj%hdr_x < 1 .or. obj%hdr_x > obj%nwih) then
    call pc_error('HDR_X < 1 or > NWIH')
    obj%hdr_x = 8
  end if

  if(obj%hdr_y < 1 .or. obj%hdr_y > obj%nwih) then
    call pc_error('HDR_Y < 1 or > NWIH')
    obj%hdr_y = 7
  end if

  if(obj%scale <= 0.0) then
    call pc_error('SCALE must be greater than zero')
    obj%scale = 10.0
  end if

  if(verify .or. pc_verify_array('headers')) then
    do item = 1, obj%header_cnt
      if(obj%headers(item) < 1 .or. obj%headers(item) > obj%nwih) then
        call pc_error('HEADERS, one or more < 1 or > NWIH')
        exit
      end if
    end do
  end if

  if(verify .or. pc_verify_array('times')) then
    1100 continue
    do item = 1, obj%time_cnt
      if(obj%times(item) == FNIL) then
        ! nil (empty) value, squeeze entry out of table
        do item2 = item + 1, obj%time_cnt
          obj%times(item2 - 1) = obj%times(item2)
        end do

        obj%time_cnt = obj%time_cnt - 1
        go to 1100
      end if

      obj%in_time_idx(item) = 1 + nint((obj%times(item) - obj%tstrt) / obj%dt)

      if(obj%in_time_idx(item) < 1 .or. obj%in_time_idx(item) > obj%ndpt) then
        call pc_error('TIMES, one or more values < TSTRT or > end time')
        exit
      end if
    end do
  end if

  end_time = obj%tstrt + (obj%ndpt - 1) * obj%dt

  ! Note: pattern_stop2 is now the preferred way.
  status = pattern_stop2('TSLC:', verify, &
    obj%time_init, obj%time_inc, obj%time_last, obj%time_tot, &
    'TIME_INIT', 'TIME_INC', 'TIME_LAST', 'TIME_TOT', &
    pc_verify_scalar('time_init'), pc_verify_scalar('time_inc'), &
    pc_verify_scalar('time_last'), pc_verify_scalar('time_tot'), &
    start_min=obj%tstrt, inc_min=0.0, total_min=0, &
    stop_max=end_time, total_max=(TIMES_MAX-obj%time_cnt))

  obj%time_cnt_tot = obj%time_cnt + obj%time_tot

  obj%out_y_cnt = obj%header_cnt + obj%time_cnt_tot

  if(verify) then
    IF (obj%out_y_cnt == 0) THEN
      call pc_error('No slices specified by either HEADERS or TIMES')
    ENDIF
  end if

  call grid_get_widths(obj%grid, obj%delx, obj%dely)

  if(obj%delx == 0.0 .or. obj%dely == 0.0) then
    call pc_error('GRID transform implies delta x or y is == 0.0')
  end if

  ! Define mapping between input and output dimensions
  mapping_choice: &
  if(obj%orient_x(1:1) == 'L' .or. obj%orient_x(1:1) == 'R') then
    ! Input X is oriented either left or right (output +/- X)
    ! Input Y is oriented either up or down (output +/- T)
    if(obj%orient_x(1:1) == 'R' .and. obj%x_inc > 0.0) then
      obj%out_x_base_x = 0
      obj%out_x_sign_x = +1
    else
      obj%out_x_base_x = obj%x_tot + 1
      obj%out_x_sign_x = -1
    end if

    if(obj%orient_y(1:1) == 'D' .and. obj%y_inc > 0.0) then
      obj%out_t_base_y = 0
      obj%out_t_sign_y = +1
    else
      obj%out_t_base_y = obj%y_tot + 1
      obj%out_t_sign_y = -1
    end if

    obj%out_x_cnt = obj%x_tot

    if(obj%orient_x(1:1) == 'R') then
      obj%out_x_inc = + abs(obj%x_inc)
      obj%out_x_start = min(obj%x_init, &
        obj%x_init + obj%x_inc * (obj%x_tot - 1))
    else
      obj%out_x_inc = - abs(obj%x_inc)
      obj%out_x_start = max(obj%x_init, &
        obj%x_init + obj%x_inc * (obj%x_tot - 1))
    end if

    obj%out_ndpt = obj%y_tot
    obj%samp_int_out = abs(obj%y_inc)

    obj%out_dt_signed = + obj%samp_int_out
    obj%out_tstrt = min(obj%y_init, &
      obj%y_init + obj%y_inc * (obj%y_tot - 1))

    if(obj%orient_y(1:1) == 'D') then
      obj%invert_plot = .false.
    else
      obj%invert_plot = .true.
    end if

    ! Output T is NOT dependent upon input X
    obj%out_t_base_x = 0
    obj%out_t_sign_x = 0

    ! Output X is NOT dependent upon input Y
    obj%out_x_base_y = 0
    obj%out_x_sign_y = 0
  else mapping_choice
    ! Input X is oriented either up or down (output +/- T)
    ! Input Y is oriented either left or right (output +/- X)
    if(obj%orient_x(1:1) == 'D' .and. obj%x_inc > 0.0) then
      obj%out_t_base_x = 0
      obj%out_t_sign_x = +1
    else
      obj%out_t_base_x = obj%x_tot + 1
      obj%out_t_sign_x = -1
    end if

    if(obj%orient_y(1:1) == 'R' .and. obj%y_inc > 0.0) then
      obj%out_x_base_y = 0
      obj%out_x_sign_y = +1
    else
      obj%out_x_base_y = obj%y_tot + 1
      obj%out_x_sign_y = -1
    end if

    obj%out_ndpt = obj%x_tot
    obj%samp_int_out = abs(obj%x_inc)

    obj%out_dt_signed = + obj%samp_int_out
    obj%out_tstrt = min(obj%x_init, &
      obj%x_init + obj%x_inc * (obj%x_tot - 1))

    if(obj%orient_x(1:1) == 'D') then
      obj%invert_plot = .false.
    else
      obj%invert_plot = .true.
    end if

    obj%out_x_cnt = obj%y_tot

    if(obj%orient_y(1:1) == 'R') then
      obj%out_x_inc = + abs(obj%y_inc)
      obj%out_x_start = min(obj%y_init, &
        obj%y_init + obj%y_inc * (obj%y_tot - 1))
    else
      obj%out_x_inc = - abs(obj%y_inc)
      obj%out_x_start = max(obj%y_init, &
        obj%y_init + obj%y_inc * (obj%y_tot - 1))
    end if

    ! Output T is NOT dependent upon input Y
    obj%out_t_base_y = 0
    obj%out_t_sign_y = 0

    ! Output X is NOT dependent upon input X
    obj%out_x_base_x = 0
    obj%out_x_sign_x = 0
  end if mapping_choice

  obj%out_tend = obj%out_tstrt + obj%out_dt_signed * (obj%out_ndpt - 1)

  ! The logic in the old code indicated that
  ! "The total vertical height of plots should be < 35 inches".

  vert_scale_min = obj%out_ndpt / 35.0
  vert_scale_nom = int(0.9999999 + obj%out_ndpt / 35.0)

  if(obj%orient_x(1:1) == 'L' .or. obj%orient_x(1:1) == 'R') then
    ! normalize based upon nominal 20 traces per inch
    horz_scale_max = 30.0 / abs(obj%dely / obj%delx)
    horz_scale_nom = 20.0 / abs(obj%dely / obj%delx)
  else
    ! normalize based upon nominal 20 traces per inch
    horz_scale_max = 30.0 / abs(obj%delx / obj%dely)
    horz_scale_nom = 20.0 / abs(obj%delx / obj%dely)
  end if

  average_scale = 0.5 * (vert_scale_min + horz_scale_max)

!!!! Temporary debug code
!    select case (state)
!    case (PC_GUI)
!      call pc_warning('pc_update_state == PC_GUI')
!    case (PC_QUICK)
!      call pc_warning('pc_update_state == PC_QUICK')
!    case (PC_FRONTEND)
!      call pc_warning('pc_update_state == PC_FRONTEND')
!    case (PC_BACKEND)
!      call pc_warning('pc_update_state == PC_BACKEND')
!    case (PC_BACKEND_NO_EXEC)
!      call pc_warning('pc_update_state == PC_BACKEND_NO_EXEC')
!    end select

  if(vert_scale_min > horz_scale_max) then
    if(obj%out_ndpt /= obj%out_ndpt_old .or. &
      obj%out_ndpt_old == OUT_NDPT_OLD_INIT) then
      call pc_warning('1:1 aspect ratio for plots not available at any SCALE,')
      call pc_warning('given 35 inch height and 30 traces/inch constraint.')
      call pc_warning('Reduce vertical point count to resolve dilemma.')
    end if
  else if(obj%scale < vert_scale_min) then
    if(vert_scale_nom > horz_scale_nom) then
      obj%scale = average_scale
    else
      obj%scale = vert_scale_nom
    end if

    call pc_warning('SCALE EXCEEDS PLOT HEIGHT OF 35 INCHES - RESET TO ', &
      obj%scale)
  else if(obj%scale > horz_scale_max) then
    if(vert_scale_nom > horz_scale_nom) then
      obj%scale = average_scale
    else
      obj%scale = horz_scale_nom
    end if

    call pc_warning('SCALE EXCEEDS 30 TRACES PER INCH - RESET TO ', &
      obj%scale)
  end if

  plot_inches_per_sec = 1.0 / (obj%samp_int_out * obj%scale)

  ! This preserves the aspect ratio of the raw bins.
  !!! plot_traces_per_inch = obj%scale

  ! This preserves the aspect ratio of the physical input coordinates
  ! A circle in map view should appear round when plotted.
  if(obj%orient_x(1:1) == 'L' .or. obj%orient_x(1:1) == 'R') then
    plot_traces_per_inch = obj%scale * abs(obj%dely / obj%delx)
  else
    plot_traces_per_inch = obj%scale * abs(obj%delx / obj%dely)
  end if

  plot_units_per_sec = 1.0 / obj%samp_int_out

  ! mini_buf (write/read queue) is approximately SWATH_FACTOR words.
  ! Each mini-trace contains the following:
  ! 1) two words for the input X and Y location (scaled to output index)
  ! 2) header_cnt words for the input trace header slices
  ! 3) time_cnt_tot words for the input trace data slices
  obj%mini_max = 1 + SWATH_FACTOR / (obj%out_y_cnt + 2)

  ! slice_buf (transpose queue) is approximately SIZE_MEM words
  ! (or at least one complete slice, which ever is greater)
  ! and never more than the number of output slices.
  obj%slice_buf_max = min(obj%out_y_cnt, &
    max(SIZE_MEM / (obj%x_tot * obj%y_tot), 1))

  nscratch = 0
  nstore = obj%out_y_cnt * obj%mini_max &
    + obj%out_ndpt * obj%out_x_cnt * obj%slice_buf_max

  ndisk = (obj%out_y_cnt + 2) * obj%out_x_cnt * obj%out_ndpt

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

  ! Note: These globals are set here because they are either needed by
  ! trcio_open (OPT_OUT=FILE or BOTH) or
  ! by subsequent processes (OPT_OUT=TRACE).
  ! If OPT_OUT=FILE, these globals will be reset to the value
  ! provided by the previous processing step.
  call pc_put_global ('nwih', 64)
  call pc_put_global ('ndpt', obj%out_ndpt)
  call pc_put_global ('dt', obj%samp_int_out)  ! always positive

  call pc_put_global ('tstrt', obj%out_tstrt)

  call pc_put_global ('invert_plot', obj%invert_plot)  ! plot time axis

  if (.not. pc_do_not_process_traces()) then
    if(obj%opt_out == 'FILE' .or. obj%opt_out == 'BOTH') then
      obj%trcio_file => trcio_open(obj%path_out, 'w')

      if(.not. associated(obj%trcio_file )) then
        call pc_error("trcio_open: error, path_out= " // obj%path_out)
        return
      end if

      obj%trcio_file%num_values = obj%out_ndpt

      obj%trcio_file%tmin = obj%out_tstrt

      obj%trcio_file%dt = obj%samp_int_out
      obj%trcio_file%nwih = obj%nwih
      obj%trcio_file%nbits = 32
      obj%trcio_file%nbits_hd = 64

      ier = trcio_writeheader(obj%trcio_file)

      call pc_alloc_global_cards(global_cards,num_global_cards)

      ier = trcio_write_globals(obj%trcio_file,num_global_cards,global_cards)
    end if
  end if

  if(obj%opt_out == 'FILE') then
    ! Change globals back to the original values for the trace flow
    call pc_put_global ('nwih', obj%nwih)
    call pc_put_global ('ndpt', obj%ndpt)
    call pc_put_global ('dt', obj%dt)

    call pc_put_global ('tstrt', obj%tstrt)

    call pc_remove_global_keyword('invert_plot')

    if(obj%ndpt_save /= obj%ndpt .or. &
      obj%dt_save    /= obj%dt .or. &
      obj%tstrt_save /= obj%tstrt) then

      obj%ndpt_save  = obj%ndpt
      obj%dt_save    = obj%dt
      obj%tstrt_save = obj%tstrt

      call pc_warning('Changing globals for NDPT, DT or TSTRT.')
      call pc_warning('TROT defaults for TIM_BEG and TIM_END ' // &
        'may need adjusting.')
    end if
  else
    if(obj%ndpt_save /= obj%out_ndpt .or. &
      obj%dt_save    /= obj%samp_int_out .or. &
      obj%tstrt_save /= obj%out_tstrt) then

      obj%ndpt_save  = obj%out_ndpt
      obj%dt_save    = obj%samp_int_out
      obj%tstrt_save = obj%out_tstrt

      call pc_warning('Changing globals for NDPT, DT or TSTRT.')
      call pc_warning('TROT defaults for TIM_BEG and TIM_END ' // &
        'may need adjusting.')
    end if
  end if

  call pc_put_options_field('opt_out', OPT_OUT_OPTIONS, 3)
  call pc_put('opt_out', obj%opt_out)
  call pc_put ('hdr_flag', obj%hdr_flag)
  call pc_put('path_out', obj%path_out)
  call pc_put('hdr_x', obj%hdr_x)
  call pc_put('hdr_y', obj%hdr_y)
  call pc_put('x_init', obj%x_init)
  call pc_put('y_init', obj%y_init)
  call pc_put('x_inc', obj%x_inc)
  call pc_put('y_inc', obj%y_inc)
  call pc_put('x_last', obj%x_last)
  call pc_put('y_last', obj%y_last)
  call pc_put('x_tot', obj%x_tot)
  call pc_put('y_tot', obj%y_tot)
  call pc_put('samp_int_out', obj%samp_int_out)
  call pc_put('scale', obj%scale)
  call pc_put_options_field('orient_x', &
    (/ "UP   ", "DOWN ", "RIGHT", "LEFT " /), 4)
  call pc_put('orient_x', obj%orient_x)
  call pc_put_options_field('orient_y', &
    (/ "UP   ", "DOWN ", "RIGHT", "LEFT " /), 4)
  call pc_put('orient_y', obj%orient_y)
  call pc_put('headers', obj%headers, obj%header_cnt)
  call pc_put('times', obj%times, obj%time_cnt)
  call pc_put('time_init', obj%time_init)
  call pc_put('time_inc', obj%time_inc)
  call pc_put('time_last', obj%time_last)
  call pc_put('time_tot', obj%time_tot)

  if(obj%opt_out == 'TRACE') then
    call pc_put_sensitive_field_flag('path_out'       , .false.)
    call pc_put_sensitive_field_flag('path_out_info'  , .false.)
    call pc_put_sensitive_field_flag('select_path_out', .false.)
  else 
    call pc_put_sensitive_field_flag('path_out'       , .true.)
    call pc_put_sensitive_field_flag('path_out_info'  , .true.)
    call pc_put_sensitive_field_flag('select_path_out', .true.)
  end if

  call pc_put_sensitive_field_flag('samp_int_out', .false.)

  call pc_put_control ('nscratch', nscratch)
  call pc_put_control ('nstore'  , nstore)

  if(obj%opt_out == 'TRACE' .or. obj%opt_out == 'BOTH') then
    call pc_put_control ('need_request', .true.)
    call pc_put_control ('need_label'  , .true.)
  end if
  call pc_put_control ('ndisk'       , ndisk)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


  if (pc_do_not_process_traces()) return
  obj%skip_wrapup = .false.

  if(obj%time_tot > 0) then
    do item = 1, obj%time_tot
      regular_time = obj%time_init + (item - 1) * obj%time_inc
      obj%in_time_idx(obj%time_cnt + item) = &
        1 + nint((regular_time - obj%tstrt) / obj%dt)
    end do
  end if

  write(print_lun, 1234) (obj%out_ndpt - 1) * obj%samp_int_out, &
    obj%samp_int_out, obj%out_ndpt, obj%out_tstrt, plot_units_per_sec, &
    plot_inches_per_sec, plot_traces_per_inch, obj%invert_plot

  1234 FORMAT( &
    ' TRACE LENGTH --------------> RESET TO ',G14.7,' "SECONDS" (SPATIAL)'/,&
    ' SAMPLE RATE ---------------> RESET TO ',G14.7,' "SECONDS" (SPATIAL)'/,&
    ' DATA POINTS PER TRACE -----> RESET TO ',I5,' (= X or Y BIN COUNT)'/,&
    ' TRACE START TIME ----------> RESET TO ',G14.7,' "SECONDS" (SPATIAL)'/,&
    ' UNITS/SEC (US) TO USE IN PLOTS = ',G14.7,/,&
    ' INCHES/SEC (IS) TO USE IN PLOTS = ',G14.7,/,&
    ' TRACES/INCH (TI) TO USE IN PLOTS = ',G14.7,/, &
    ' INVERT PLOT (ANNOTATION & SAMPLES) = ',L8)

  write(print_lun, *) &
    'NUMBER OF HEADER AND TIME SLICES TO CREATE = ', obj%out_y_cnt

  write(print_lun, *) &
    'NUMBER OF X-BINS IN EACH TIME SLICE = ', obj%out_x_cnt

  write(print_lun, *) &
    'NUMBER OF Y-BINS IN EACH TIME SLICE = ', obj%out_y_cnt

  write(print_lun, *) &
    'SPACE IN BUFFER FOR ', obj%slice_buf_max, ' SLICES PER PASS'

!? debug support
! write(print_lun, *) &
!   'X-BIN SIZE (FROM GLOBALS) = ', obj%out_x_inc
!
! write(print_lun, *) &
!   'Y-BIN SIZE (FROM GLOBALS) = ', obj%samp_int_out

  allocate(obj%mini_buf(2+obj%out_y_cnt,obj%mini_max),stat=ier1)

  allocate(obj%slice_buf(obj%out_ndpt,obj%out_x_cnt,obj%slice_buf_max), &
    stat=ier2)

  allocate(obj%out_buf(obj%out_ndpt, 1), stat=ier3)

  if(ier1 /= 0 .or. ier2 /= 0 .or. ier3 /= 0) then
    call pc_error('memory allocation failed')
  end if

  call getlun(obj%tmp_lun, ier1)

  if(ier1 == 0) then
    open(unit = obj%tmp_lun, action = 'READWRITE', iostat = status, &
      status = 'SCRATCH', form= 'UNFORMATTED')
    if(status /= 0) then
      write(msg, '("iostat=",I4,", opening tmp slice file")')
      call pc_error(msg)
      obj%tmp_lun = -1
    end if
  else
    call pc_error( &
      'getlun failed, can not open scratch dump file')
  end if


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

  return
  end subroutine tslc_update

!!------------------- ORIENT X trap ------------------------------------!!
!!------------------- ORIENT X trap ------------------------------------!!
!!------------------- ORIENT X trap ------------------------------------!!

  subroutine tslc_orient_x_trap (keyword)        ! scalar trap.
  implicit none
  character(len=*),intent(in) :: keyword           ! arguments

  ! insure that "ORIENT_X" value is spelled correctly and
  ! the "ORIENT_Y" value is complimentary (horizontal / vertical).

  call string_to_upper(object%orient_x)

  if(object%orient_x(1:1) == 'U' .or. object%orient_x(1:1) == 'D') then
    if(object%orient_x(1:1) == 'U') then
      object%orient_x = 'UP'
    else
      object%orient_x = 'DOWN'
    end if

    if(object%orient_y == 'UP' .or. object%orient_y == 'DOWN') then
!??? warning seems too distracting, 2000-03-01 Selzler
!???  call pc_warning('ORIENT_Y must be LEFT or RIGHT')
      object%orient_y = 'RIGHT'
!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!???  call pc_jump_field('orient_y')
    end if
  else if(object%orient_x(1:1) == 'L' .or. object%orient_x(1:1) == 'R') then
    if(object%orient_x(1:1) == 'L') then
      object%orient_x = 'LEFT'
    else
      object%orient_x = 'RIGHT'
    end if

    if(object%orient_y == 'LEFT' .or. object%orient_y == 'RIGHT') then
!??? warning seems too distracting, 2000-03-01 Selzler
!???  call pc_warning('ORIENT_Y must be UP or DOWN')
      object%orient_y = 'DOWN'
!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!???  call pc_jump_field('orient_y')
    end if
  else
    if(object%orient_y == 'UP' .or. object%orient_y == 'DOWN') then
      call pc_error('ORIENT_X must be LEFT or RIGHT')
      object%orient_x = 'RIGHT'
    else
      call pc_error('ORIENT_X must be UP or DOWN')
      object%orient_x = 'DOWN'
    end if

!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!??? call pc_jump_field('orient_x')
  end if

  return
  end subroutine tslc_orient_x_trap

!!------------------- ORIENT Y trap ------------------------------------!!
!!------------------- ORIENT Y trap ------------------------------------!!
!!------------------- ORIENT Y trap ------------------------------------!!

  subroutine tslc_orient_y_trap (keyword)        ! scalar trap.
  implicit none
  character(len=*),intent(in) :: keyword           ! arguments

  ! insure that "ORIENT_Y" value is spelled correctly and
  ! the "ORIENT_X" value is complimentary (horizontal / vertical).

  call string_to_upper(object%orient_y)

  if(object%orient_y(1:1) == 'U' .or. object%orient_y(1:1) == 'D') then
    if(object%orient_y(1:1) == 'U') then
      object%orient_y = 'UP'
    else
      object%orient_y = 'DOWN'
    end if

    if(object%orient_x == 'UP' .or. object%orient_x == 'DOWN') then
!??? warning seems too distracting, 2000-03-01 Selzler
!???  call pc_warning('ORIENT_X must be LEFT or RIGHT')
      object%orient_x = 'RIGHT'
!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!???  call pc_jump_field('orient_x')
    end if
  else if(object%orient_y(1:1) == 'L' .or. object%orient_y(1:1) == 'R') then
    if(object%orient_y(1:1) == 'L') then
      object%orient_y = 'LEFT'
    else
      object%orient_y = 'RIGHT'
    end if

    if(object%orient_x == 'LEFT' .or. object%orient_x == 'RIGHT') then
!??? warning seems too distracting, 2000-03-01 Selzler
!???  call pc_warning('ORIENT_X must be UP or DOWN')
      object%orient_x = 'DOWN'
!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!???  call pc_jump_field('orient_x')
    end if
  else
    if(object%orient_x == 'UP' .or. object%orient_x == 'DOWN') then
      call pc_error('ORIENT_Y must be LEFT or RIGHT')
      object%orient_y = 'RIGHT'
    else
      call pc_error('ORIENT_Y must be UP or DOWN')
      object%orient_y = 'DOWN'
    end if

!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!??? call pc_jump_field('orient_y')
  end if

  return
  end subroutine tslc_orient_y_trap

!!------------------- X PATTERN trap ------------------------------------!!
!!------------------- X PATTERN trap ------------------------------------!!
!!------------------- X PATTERN trap ------------------------------------!!

  subroutine tslc_x_pattern_trap (keyword)       ! scalar trap.
  implicit none
  character(len=*),intent(in) :: keyword           ! arguments

  character(len=MAX_LINE) :: msg                   ! local

  call pattern_init_last(keyword, object%x_init, object%x_last, &
    object%x_inc, object%x_tot, msg, &
    inc_min= tiny(object%x_inc), tot_min = 1)

  if(msg(1:1) /= ' ') then
    call pc_error(msg)
!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!??? call pc_jump_field(keyword)
  end if

  return
  end subroutine tslc_x_pattern_trap

!!------------------- Y PATTERN trap ------------------------------------!!
!!------------------- Y PATTERN trap ------------------------------------!!
!!------------------- Y PATTERN trap ------------------------------------!!

  subroutine tslc_y_pattern_trap (keyword)       ! scalar trap.
  implicit none
  character(len=*),intent(in) :: keyword           ! arguments

  character(len=MAX_LINE) :: msg                   ! local

  call pattern_init_last(keyword, object%y_init, object%y_last, &
    object%y_inc, object%y_tot, msg, &
    inc_min= tiny(object%y_inc), tot_min = 1)

  if(msg(1:1) /= ' ') then
    call pc_error(msg)
!??? "jump" seems to clobber the updated value, 2000-03-01 Selzler
!??? call pc_jump_field(keyword)
  end if

  return
  end subroutine tslc_y_pattern_trap

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


  subroutine tslc (obj,ntr,hd,tr)
  implicit none

  ! DUMMY ARGUMENTS
  type(tslc_struct),intent(inout) :: obj     ! process object
  integer, intent(inout)          :: ntr     ! control / number of traces
  double precision, intent(inout) :: hd(:,:) ! trace headers
  real, intent(inout)             :: tr(:,:) ! trace samples

  INTEGER :: i, j, ier, index_x, index_y, ntr_do, mini_do, mini_stop

  ntr_choice: if(ntr > 0) then
    ! PHASE 1: loop over all input traces and save slice data
    save_input_loop: DO ntr_do = 1, ntr
      IF (obj%hdr_flag /= 0) THEN
        ! slice trace, iff flagged header word is non-zero

        IF(hd(obj%hdr_flag,ntr_do) == 0.0) CYCLE ! not flagged

      ENDIF

      ! compute and window input X inline bin number
      index_x=mth_bin_number(dble(obj%x_init),dble(obj%x_inc),hd(obj%hdr_x,ntr))

!rev30      index_x = 1 + nint((hd(obj%hdr_x,ntr) - obj%x_init) / obj%x_inc)

      if(index_x < 1 .or. index_x > obj%x_tot) cycle  ! excluded

      ! compute and window input Y crossline bin number
      index_y=mth_bin_number(dble(obj%y_init),dble(obj%y_inc),hd(obj%hdr_y,ntr))

!rev30      index_y = 1 + nint((hd(obj%hdr_y,ntr) - obj%y_init) / obj%y_inc)

      if(index_y < 1 .or. index_y > obj%y_tot) cycle  ! excluded

      ! This input trace falls within the slice boundaries.
      ! Convert trace to mini trace within mini_buf (disk write queue).
      obj%mini_idx = obj%mini_idx + 1

      ! MAP BIN NUMBER FOR MINI TRACE
      obj%mini_buf(1, obj%mini_idx) = &
        obj%out_t_base_x + obj%out_t_sign_x * index_x + &
        obj%out_t_base_y + obj%out_t_sign_y * index_y

      obj%mini_buf(2, obj%mini_idx) = &
        obj%out_x_base_x + obj%out_x_sign_x * index_x + &
        obj%out_x_base_y + obj%out_x_sign_y * index_y

      ! SAVE HEADERS FOR MINI TRACE
      IF (obj%header_cnt > 0) THEN
        obj%mini_buf(3:2+obj%header_cnt,obj%mini_idx) = &
          hd(obj%headers(1:obj%header_cnt),ntr_do)
      ENDIF

      ! SAVE TIME SLICE DATA FOR MINI TRACE
      IF (obj%time_cnt_tot > 0) THEN
        obj%mini_buf(3+obj%header_cnt: &
                     2+obj%header_cnt+obj%time_cnt_tot, obj%mini_idx) = &
          tr(obj%in_time_idx(1:obj%time_cnt_tot),ntr_do)
      ENDIF

      if(obj%mini_idx == obj%mini_max) then
        ! mini_buf queue is full, write mini traces to tmp disk file
        write(obj%tmp_lun) obj%mini_max, obj%mini_buf(:,:obj%mini_max)

        obj%mini_idx = 0
      end if
    END DO save_input_loop

    if(obj%opt_out == 'TRACE' .or. obj%opt_out == 'BOTH') then
      ntr = NEED_TRACES
    end if

    RETURN
  else if(ntr == NO_MORE_TRACES) then ! ntr_choice
    ! PHASE 2: read tmp disk, transpose data and output traces
    if(obj%out_y_idx /= 0) then
      call pc_error("tslc: a previous process forgot to set NEED_LABEL")
      call tslc_wrapup(obj)
      ntr = FATAL_ERROR
      return
    end if

    ! PHASE 1 to 2 TRANSITION, end of input, beginning of output
    obj%out_x_idx = obj%out_x_cnt
    obj%out_y_idx = 0
    obj%slice_buf_cnt = 0

    if(obj%mini_idx > 0) then
      ! flush residual mini_buf write queue to tmp disk file
      write(obj%tmp_lun) obj%mini_idx, obj%mini_buf(:,:obj%mini_idx)
    end if

    endfile obj%tmp_lun  ! mark the end of file
  else if(ntr /= NEED_TRACES) then ! ntr_choice
    call tslc_wrapup(obj)
    ntr = FATAL_ERROR
    return
  end if ntr_choice

  ! TOP OF LOOP when slice traces are returned to job flow
  1000 continue

  if(obj%out_x_idx == obj%out_x_cnt) then
    ! increment to next output slice, if any
    obj%out_y_idx = obj%out_y_idx + 1
    obj%out_x_idx = 1
  else
    ! increment to next output trace within same slice
    obj%out_x_idx = obj%out_x_idx + 1
  end if

  if(obj%out_y_idx > obj%out_y_cnt) then
    ! all traces in all slice have now been output
    call tslc_wrapup(obj)

    ntr = NO_MORE_TRACES

    return
  end if

  if(obj%out_y_idx > obj%slice_buf_idx + obj%slice_buf_cnt - 1) then
    ! read tmp disk and (re)initialize slice_buf
    obj%slice_buf_idx = obj%out_y_idx ! first slice within slice_buf

    ! read the entire disk tmp file from the beginning
    rewind obj%tmp_lun

    ! number of slices actually required in slice_buf
    obj%slice_buf_cnt = min(obj%out_y_cnt - obj%slice_buf_idx + 1, &
      obj%slice_buf_max)

    obj%slice_buf(:,:,:obj%slice_buf_cnt) = 0.0 ! zero the slice buffer

    read_and_transpose_loop: do
      ! read mini traces until end-of-file
      read(obj%tmp_lun, iostat=ier) mini_stop, obj%mini_buf

      if(ier < 0) exit read_and_transpose_loop

      ! Transpose mini trace into slice buffer.
      do mini_do = 1, mini_stop
        i = obj%mini_buf(1,mini_do)
        j = obj%mini_buf(2,mini_do)
        obj%slice_buf(i, j, :obj%slice_buf_cnt) = &
          obj%mini_buf(2 + obj%slice_buf_idx: &
                       1 + obj%slice_buf_idx+obj%slice_buf_cnt, mini_do)
      end do
    end do read_and_transpose_loop
  end if

  ! create one output trace and return
  if(obj%invert_plot) then
    obj%out_buf(obj%out_ndpt:1:-1, 1) = obj%slice_buf(:,obj%out_x_idx, &
      obj%out_y_idx - obj%slice_buf_idx + 1)
  else
    obj%out_buf(:obj%out_ndpt, 1) = obj%slice_buf(:,obj%out_x_idx, &
      obj%out_y_idx - obj%slice_buf_idx + 1)
  end if

  hd(:64,1) = 0.0

  hd(1,1) = obj%out_x_idx + (obj%out_y_idx - 1) * obj%out_y_cnt
  hd(2,1) = 1
  hd(7,1) = obj%out_x_start + (obj%out_x_idx - 1) * obj%out_x_inc

  if(obj%out_y_idx <= obj%header_cnt) then
    hd(30,1) = obj%headers(obj%out_y_idx)
  else if(obj%out_y_idx <= obj%header_cnt + obj%time_cnt) then
    hd(30,1) = obj%times(obj%out_y_idx - obj%header_cnt)
  else
    hd(30,1) = obj%time_init + obj%time_inc * &
      nint(obj%out_y_idx - 1.0 - obj%header_cnt - obj%time_cnt)
  end if

  hd(37,1) = hd(7,1)
  hd(64,1) = obj%out_ndpt

  ntr = 1

  call lav_set_hdr(hd, obj%out_buf, obj%out_ndpt, ntr)

  if(obj%opt_out == 'FILE' .or. obj%opt_out == 'BOTH') then
    ier = trcio_write_trace(obj%trcio_file, hd(1:64,1), &
      obj%out_buf(:,1))

    if(ier /= TRCIO_OK) then
      call pc_error("trcio_write_trace: error, path_out= " // obj%path_out)

      ntr = FATAL_ERROR
      return
    end if
  end if

  if(obj%opt_out == 'FILE') then
    ! BOTTOM OF LOOP when slice traces are only written to a file and
    ! none are returned to job flow.
    go to 1000
  end if

  tr(1:obj%out_ndpt,1) = obj%out_buf(:,1)

  return
  end subroutine tslc


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


  subroutine tslc_wrapup (obj)
  implicit none
  type(tslc_struct) :: obj       ! arguments

  if(obj%skip_wrapup) return
  obj%skip_wrapup = .true.

  if(obj%tmp_lun >= 0) close(unit = obj%tmp_lun)

  return
  end subroutine tslc_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

  end module tslc_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
