!<CPS_v1 type="PROCESS"/>

!!------------------------------- scab.f90 ---------------------------------!!
!!------------------------------- scab.f90 ---------------------------------!!
!!------------------------------- scab.f90 ---------------------------------!!

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
! Name       : SCAB    (Surface Consistent Amplitude Balance)
! Category   : amplitude_mod
! Written    : 1998-03-13   by: Donna K. Vunderink & Chuck I. Burch
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : Calculate and apply a surface consistent amplitude balance.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! General Operation of SCAB
!
! 1. Scan input traces individually, calculate window top and bottom for
! each trace and calculate average (or median) of absolute value of trace
! samples in window to form the amplitude measurement for each trace.
! Write trace to disk if PASSES = 1.
!
! 2. After last trace is read, form the surface consistent amplitude for
! each source by calculating the average (or median) of the amplitude
! measurements for each trace associated with that source.
!
! 3. Take running average (running median) of source element amplitudes
!       considered as a one dimensional array.
!
! 4. Repeat steps 2 and 3 for receiver and offset as indicated.
!
! 5. Write a diagnostic file containing the surface consistent amplitude
! for each chosen source, receiver or offset element.  (Same filename
! with .src, .rec or .off extension.)
!
! 6. Calculate a multiplicative correction factor as follows.
!
!  The acceptable range with respect to killing lies between
!                running_average*(1.0 - RANGE_KILL) and
!                running_average*(1.0 + RANGE_KILL).
!
!  Elements whose amplitudes are above or below this range are
!  killed.  Elements whose amplitudes lie within this range are
!  not killed but are subject to possible correction as defined by
!  the RANGE_CORR parameter.
!
!  The acceptable range for correction lies between
!   running_average*(1.0 - RANGE_CORR) and
!   running_average*(1.0 + RANGE_CORR).
!
!  Amplitudes lying within this range are not changed.  Amplitudes
!  below this range are increased to the lower boundary.
!  Amplitudes above this range are reduced to the upper boundary.
!
! 7. Apply amplitude balance to each trace by multiplying each trace
!       sample by the product of source, receiver and offset correction
!       factors appropriate for that trace's headers.
!
!
! Disabling Kill and Correction Operations
!
! If MODE = APPLY (or if PASSES = 1), and RANGE_KILL = 0.0,
! then killing will be disabled.
! If MODE = APPLY (or if PASSES = 1), and RANGE_CORR = 0.0,
! then correction will be disabled.
!
!
! Modes of Operation
!
! If PASSES = 1, then SCAB calculates and applies the correction in one pass.
! If PASSES = 2, then SCAB will calculate the correction in the first pass
! (MODE = CALC) and will either apply the correction (MODE = APPLY) or remove a
! previously applied correction (MODE = REMOVE) in the second pass.
!
! Note: REMOVE can not restore traces that were killed, it can only
! un-do corrections that were previously applied to a trace.
!
! If PASSES = 1, then the PATH_CORR file is created if and only if
! PATH_CORR is specified (non-blank and not "NONE").
! If PASSES = 2 and MODE = CALC, then the PATH_CORR file is created.
! If PASSES = 2 and MODE = APPLY or REMOVE, then the PATH_CORR file is used.
!
!
! Note on Surface Consistency
!
! For the purpose of this process, a surface consistent correction is
! understood to be a trace correction that is identical for all traces
! associated with a given surface consistent element (such as a source).  The
! correction for a given trace is the product of the corrections for each
! surface consistent element with which the trace is associated (that is, a
! trace may have a correction that is the product of the source correction and
! the receiver correction).
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Diagnostic File
!
! SCAB can write a diagnostic file (PATH_DIAG) containing the surface
! consistent amplitude of chosen elements.  You can use the"scab_reformat"
! shell script with the PATH_DIAG file to create files of amplitudes for
! individual surface consistent elements.  These files are especially useful
! for determining the desired values of the RANGE_KILL and RANGE_CORR
! parameters.  These files can be viewed with GnuPlot or other plotting
! application.
!
!
! 2D vs. 3D
!
! SCAB is a 2D process in that the running average smoothing operation asssumes
! that surface consistent amplitudes represent a single line.  It may be
! possible to use SCAB with 3D data by running separate SCAB processes on
! sequential sail lines.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
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
! This process outputs the same traces as it receives (possibly altered if
! PASSES = 1 or PASSES = 2 and MODE = APPLY or REMOVE).
!
! This process outputs traces with same gather status as the input traces.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
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
! 2       Head mute header           Used
! 64      Tail mute header           Used
!         HDR_SRC                    Used
!         HDR_REC                    Used
!         HDR_OFF                    Used
!         HDR_OFF_WIN                Used
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                              REVISION HISTORY
!
!      Date        Author     Description
!      ----        ------     -----------
! 25.  2006-12-04  D. Glover  Added NULLIFY statements for Intel compiler.
! 24.  2006-06-06  Stoeckley  Add call to pc_register_array_names for SeisSpace.
!023.  2006-01-10  B. Menger  Removed Unused Variables.
! 22.  2004-09-07  Menger     Change trcio%nwih to obj%nwih from fixed 64 words.
! 21.  2002-09-11  Goodger    Use mth module for binning.
! 20.  2001-02-15  Selzler    Changed wrapup logic to use skip_wrapup
! 19.  2000-10-06  Selzler    Corrected bug, trace input termination logic.
! 18.  2000-10-04  Selzler    Corrected bug, linked array length initialization.
!                             Corrected bug, tr array sectioning in trcio call.
! 17.  2000-09-07  Selzler    Fixed bugs in path name usage, gui updates,
!                             window parameter validation, memory allocate,
!                             median scanning, and kill values.
! 16.  2000-07-07  Selzler    Fixed problems found by CPS Fortran Code Review.
! 15.  2000-05-24  Selzler    Corrected miscellaneous bugs.
! 14.  2000-05-22  Selzler    Conversion to new CPS
! 13.  1999-06-30  Vunderink  Fixed problem of modifying data in two-pass mode.
! 12.  1999-04-21  Vunderink  Replaced a call to SCAB_SORT with MEDIAN.
!                             One call was missed on 1998-08-26.
! 11.  1999-01-26  Vunderink  Begin using the f90 compiler.
! 10.  1998-08-26  Vunderink  Replaced sort with median primitive, added
!                             support for DIAGFILE=NONE and FILENAME=NONE,
!                             and keeo track of minimum source, receiver,
!                             and offset bins.
!  9.  1998-06-25  Vunderink  Changed format statments to use a G format.
!  8.  1998-06-03  Vunderink  Fixed bug in clearing OFF_ARR variable.
!  7.  1998-05-15  Vunderink  Fixed bug in SCSTAT calculation.
!  5.  1998-05-04  Vunderink  Changed running average-median to a truncated
!                             end type and changed to separate running
!                             average lengths for sources, receivers, and
!                             offsets
!  4.  1998-04-23  Vunderink  Changed running median to ignore zeros.
!  3.  1998-04-01  Vunderink  Fixed problem caused by allocating scratch
!                             when parameter value was zero.  Also, fixed
!                             two-pass apply so that the number of traces
!                             does not have to be exact.
!  2.  1998-03-25  Vunderink  Added KILFTR=-1 to not kill traces
!  1.  1998-03-13  Vunderink  Original version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!
!
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NEED_REQUEST  true/false whether this process ever needs to request traces.
! NEED_LABEL    true/false whether this process needs a label.
!                          True iff passes equal 1, else false.
! NSCRATCH      varies     amount of temporary memory needed.

! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!                        Used iff passes equals 1.
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
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS scab Process/NC=80>
! Surface Consistent Amplitude Balancing
!
! PASSES=`C  MODE=`CCCCC  TR_MAX=`IIIIIIIIIII
! STAT_TR=`CC  STAT_SC=`CC
! PATH_CORR=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! PATH_DIAG=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! Surface Consistent Parameters
! USE_SRC=~~~~`CC           HDR_SRC=`III           SRC_INIT=`FFFFFFFFFFF
!     SRC_INC=`FFFFFFFFFFF  SRC_LAST=`FFFFFFFFFFF  SRC_TOT=`IIIIIIIIIII
! USE_REC=~~~~`CC           HDR_REC=`III           REC_INIT=`FFFFFFFFFFF
!     REC_INC=`FFFFFFFFFFF  REC_LAST=`FFFFFFFFFFF  REC_TOT=`IIIIIIIIIII
! USE_OFF=~~~~`CC           HDR_OFF=`III           OFF_INIT=`FFFFFFFFFFF
!     OFF_INC=`FFFFFFFFFFF  OFF_LAST=`FFFFFFFFFFF  OFF_TOT=`IIIIIIIIIII
!
! Balance Calculation Parameters
! TYPE_RA=`CC  RANGE_KILL=`FFFFFFF  RANGE_CORR=`FFFFFFF
! RAS_SRC=`IIIII  RAS_REC=`IIIII  RAS_OFF=`IIIII
!
! Time Window Parameters
! HDR_OFF_WIN=`III
! OFFSETS     TIM_ADD     WIN_LEN
! `FFFFFFFFFFF`FFFFFFFFF`FFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFF`FFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFF`FFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFF`FFFFFFFFF
!<PARMS PATH_CORR[/ML=128/XST]>
!<PARMS PATH_DIAG[/ML=128/XST]>
!<PARMS OFFSETS_ARRAYSET[/XST/YST]>
!<PARMS TIM_ADD[/ML=20/XST/YST]
!<PARMS WIN_LEN[/ML=20/XST/YST]
!</gui_def>
!
!<HelpSection>
!
!----------------------------General Parameters---------------------------------
!
!<Help KEYWORD="PASSES">
!<Tip> Whether to calculate and apply balance in 1 or 2 passes. </Tip>
! Default = 2
! Allowed = 1, 2
! If PASSES = 1, SCAB calculates and applies the amplitude balance in 1 pass.
! If PASSES = 2, in a given pass, SCAB can either calculate or apply or remove
! a correction.
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Mode of operation of this pass in two pass operation. </Tip>
! Default = CALC
! Allowed = CALC    (Calculate amplitude balance.)
! Allowed = APPLY   (Apply amplitude balance.)
! Allowed = REMOVE  (Remove previously applied amplitude balance.)
!</Help>
!
!<Help KEYWORD="TR_MAX">
!<Tip> Maximum number of input traces to expect. </Tip>
! Default = 10000
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="STAT_TR">
!<Tip> Statistic for measuring amplitude of individual traces. </Tip>
! Default = MED
! Allowed = MED   (Median)
! Allowed = AVE   (Average)
!</Help>
!
!<Help KEYWORD="STAT_SC">
!<Tip> Statistic for calculating surface consistent amplitudes. </Tip>
! Default = MED
! Allowed = MED   (Median)
! Allowed = AVE   (Average)
! STAT_SC is the statistic used to combine individual trace amplitude
! measurements into surface consistent amplitudes.
!</Help>
!
!<Help KEYWORD="PATH_CORR">
!<Tip> Pathname of file containing correction factors for second pass. </Tip>
! Default = -
! Allowed = char
! If PASSES = 1, then a file is created iff PATH_CORR is not NONE.
! If PASSES = 2 and MODE = CALC, then the PATH_CORR file is created.
! If PASSES = 2 and MODE = APPLY or REMOVE, then the PATH_CORR file is used.
!</Help>
!
!<Help KEYWORD="PATH_DIAG">
!<Tip> Pathname of file containing surface consistent amplitudes. </Tip>
! Default = NONE
! Allowed = char
! Pathname of diagnostic file containing surface consistent amplitudes.
! If PATH_DIAG = NONE, then no diagnostic file will be created.
!
! You can use the "scab_reformat" shell script with the PATH_DIAG file to create
! files of amplitudes for individual surface consistent elements.  These files
! can be viewed with GnuPlot or another plotting application.
!</Help>
!
!------------------------Surface Consistent Parameters--------------------------
!
!<Help KEYWORD="USE_SRC">
!<Tip> Use source in surface consistent calculations. </Tip>
! Default = YES
! Allowed = YES/NO
! In marine work surface consistent elements are usually source and offset.  In
! land work surface consistent elements are usually source and receiver.
!</Help>
!
!<Help KEYWORD="HDR_SRC">
!<Tip> Header word labeling sources. </Tip>
! Default = 9
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="SRC_INIT">
!<Tip> Value of HDR_SRC at first source. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="SRC_INC">
!<Tip> Increment between source header word values. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="SRC_LAST">
!<Tip> Value of HDR_SRC at last source. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="SRC_TOT">
!<Tip> Total number of sources. </Tip>
! Default = 1
! Allowed = int
!</Help>
!
!<Help KEYWORD="USE_REC">
!<Tip> Use receiver in surface consistent calculations. </Tip>
! Default = NO
! Allowed = YES/NO
! In marine work surface consistent elements are usually source and offset.  In
! land work surface consistent elements are usually source and receiver.
!</Help>
!
!<Help KEYWORD="HDR_REC">
!<Tip> Header word labeling receivers. </Tip>
! Default = 47
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="REC_INIT">
!<Tip> Value of HDR_REC at first receiver. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="REC_INC">
!<Tip> Increment between receiver header word values. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="REC_LAST">
!<Tip> Value of HDR_REC at last receiver. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="REC_TOT">
!<Tip> Total number of receivers. </Tip>
! Default = 1
! Allowed = int
!</Help>
!
!<Help KEYWORD="USE_OFF">
!<Tip> Use offset in surface consistent calculations. </Tip>
! Default = YES
! Allowed = YES/NO
! In marine work surface consistent elements are usually source and offset.  In
! land work surface consistent elements are usually source and receiver.
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word labeling offset. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Value of HDR_OFF at first offset. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment between offset header word values. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Value of HDR_OFF at last offset. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offsets. </Tip>
! Default = 1
! Allowed = int
!</Help>
!
!------------------------Balance Calculation Parameters-------------------------
!
!<Help KEYWORD="TYPE_RA">
!<Tip> Type of running average smoother to apply. </Tip>
! Default = MED
! Allowed = MED
! Allowed = AVE
!</Help>
!
!<Help KEYWORD="RAS_SRC">
!<Tip> Length of source running average smoother. </Tip>
! Default = 20
! Allowed = int
!</Help>
!
!<Help KEYWORD="RAS_REC">
!<Tip> Length of receiver running average smoother. </Tip>
! Default = 20
! Allowed = int
!</Help>
!
!<Help KEYWORD="RAS_OFF">
!<Tip> Length of offset running average smoother. </Tip>
! Default = 20
! Allowed = int
!</Help>
!
!<Help KEYWORD="RANGE_KILL">
!<Tip> Factor defining acceptable range with respect to killing. </Tip>
! Default = 0.5
! Allowed = real >= 0.0
! The acceptable range with respect to killing lies between
! running_average*(1.0 - RANGE_KILL) and running_average*(1.0 + RANGE_KILL).
!
! Elements whose amplitudes are above or below this range are killed.  Elements
! whose amplitudes lie within this range are not killed but are subject to
! possible correction as defined by the RANGE_CORR parameter.
!
! If RANGE_KILL = 0.0, then the kill operation is disabled and RANGE_KILL is
! not used in the RANGE_CORR value trap.
!</Help>
!
!<Help KEYWORD="RANGE_CORR">
!<Tip> Factor defining acceptable range for correction. </Tip>
! Default = 0.15
! Allowed = RANGE_KILL > RANGE_CORR >= 0.0
! The acceptable range for correction lies between
! running_average*(1.0 - RANGE_CORR) and running_average*(1.0 + RANGE_CORR).
!
! Amplitudes lying within this range are not changed.  Amplitudes below this
! range are increased to the lower boundary.  Amplitudes above this range are
! reduced to the upper boundary.
!
! If RANGE_CORR = 0.0, then the correction operation is disabled.
!</Help>
!
!--------------------------Time Window Parameters-------------------------------
!
!<Help KEYWORD="HDR_OFF_WIN">
!<Tip> Header word designating offset for window specification. </Tip>
! Default = 10
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OFFSETS">
!<Tip> Linked array of offsets used to define time windows. </Tip>
! Default = -
! Allowed = real (linked array 10)
! A linked array of OFFSETS, TIM_ADD and WIN_LEN is used to define time windows.
! Top of time window = mute_time + TIM_ADD.
! Bottom of time window = top of time window + WIN_LEN.
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Top of time window = mute_time + TIM_ADD. </Tip>
! Default = -
! Allowed = real (linked array 10)
! A linked array of OFFSETS, TIM_ADD and WIN_LEN is used to define time windows.
! Top of time window = mute_time + TIM_ADD.
! Bottom of time window = top of time window + WIN_LEN.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Bottom of time window = top of time window + WIN_LEN. </Tip>
! Default = -
! Allowed = real (linked array 10)
! A linked array of OFFSETS, TIM_ADD and WIN_LEN is used to define time windows.
! Top of time window = mute_time + TIM_ADD.
! Bottom of time window = top of time window + WIN_LEN.
!</Help>
!
!</HelpSection>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module scab_module
      use pc_module
      use named_constants_module
      use trcio_module
      use lav_module
      use mem_module
      use median_module
      use mth_module
      use string_module
      use getlun_module
      use tempname_module
      use pattern_module
      use pathcheck_module
      use path_module
      implicit none
      private
      public :: scab_create
      public :: scab_initialize
      public :: scab_update
      public :: scab_delete
!<execute_only>
      public :: scab            ! main execution (trace processing) routine.
      public :: scab_wrapup
!</execute_only>

      character(len=100),public,save :: scab_ident = &
       '$Id: scab.f90,v 1.25 2006/12/04 13:29:56 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer,parameter :: TIM_WIN_MAX = 10

      character(len=1),dimension(2),parameter :: passes_options = &
        (/'1','2'/)
      character(len=6),dimension(3),parameter :: mode_options = &
        (/'CALC  ','APPLY ','REMOVE'/)
      character(len=3),dimension(2),parameter :: stat_tr_options = &
        (/'MED','AVE'/)
      character(len=3),dimension(2),parameter :: stat_sc_options = &
        (/'MED','AVE'/)
      character(len=3),dimension(2),parameter :: type_ra_options = &
        (/'MED','AVE'/)
      integer, parameter :: MAX_LINE = 132 ! Maximum number of characters
                                           ! (columns) in one line

      type,public :: scab_struct
        private
        logical                    :: skip_wrapup       ! wrapup flag.

        integer                    :: passes           ! process parameter
        character(len=6)           :: mode             ! process parameter
        integer                    :: tr_max           ! process parameter
        character(len=3)           :: stat_tr          ! process parameter
        character(len=3)           :: stat_sc          ! process parameter
        character(len=FILENAME_LENGTH) :: path_corr    ! process parameter
        character(len=FILENAME_LENGTH) :: path_diag    ! process parameter

        logical                    :: use_src          ! process parameter
        integer                    :: hdr_src          ! process parameter
        real                       :: src_init         ! process parameter
        real                       :: src_inc          ! process parameter
        real                       :: src_last         ! process parameter
        integer                    :: src_tot          ! process parameter

        logical                    :: use_rec          ! process parameter
        integer                    :: hdr_rec          ! process parameter
        real                       :: rec_init         ! process parameter
        real                       :: rec_inc          ! process parameter
        real                       :: rec_last         ! process parameter
        integer                    :: rec_tot          ! process parameter

        logical                    :: use_off          ! process parameter
        integer                    :: hdr_off          ! process parameter
        real                       :: off_init         ! process parameter
        real                       :: off_inc          ! process parameter
        real                       :: off_last         ! process parameter
        integer                    :: off_tot          ! process parameter

        character(len=3)           :: type_ra          ! process parameter
        integer                    :: ras_src          ! process parameter
        integer                    :: ras_rec          ! process parameter
        integer                    :: ras_off          ! process parameter
        real                       :: range_kill       ! process parameter
        real                       :: range_corr       ! process parameter

        integer                    :: hdr_off_win      ! process parameter
        real,dimension(TIM_WIN_MAX):: offsets          ! process parameter
        real,dimension(TIM_WIN_MAX):: tim_add          ! process parameter
        real,dimension(TIM_WIN_MAX):: win_len          ! process parameter

        integer                    :: nwih             ! global parameter
        integer                    :: ndpt             ! global parameter
        real                       :: dt               ! global parameter
        real                       :: tstrt            ! global parameter

        integer                    :: lun_corr         ! dependent parameter
        integer                    :: lun_diag_src     ! dependent parameter
        integer                    :: lun_diag_rec     ! dependent parameter
        integer                    :: lun_diag_off     ! dependent parameter

        integer                    :: offsets_cnt      ! dependent parameter
        integer                    :: tr_in            ! dependent parameter
        integer                    :: tr_out           ! dependent parameter
        integer                    :: tr_last          ! dependent parameter

        real,dimension(:),pointer  :: tr_amp           ! dependent parameter
        real,dimension(:),pointer  :: tr_src           ! dependent parameter
        real,dimension(:),pointer  :: tr_rec           ! dependent parameter
        real,dimension(:),pointer  :: tr_off           ! dependent parameter

        real,dimension(:),pointer  :: src_arr          ! dependent parameter
        real,dimension(:),pointer  :: rec_arr          ! dependent parameter
        real,dimension(:),pointer  :: off_arr          ! dependent parameter

        integer,dimension(:),pointer  :: indx          ! dependent parameter
        real,dimension(:),pointer  :: tmp_med          ! dependent parameter
        real,dimension(:),pointer  :: run_arr          ! dependent parameter
        real,dimension(:),pointer  :: tmp_arr          ! dependent parameter
        real,dimension(:),pointer  :: wrk              ! dependent parameter

        type(trcio_struct),pointer :: trcio            ! dependent parameter
      end type scab_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(scab_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine scab_create (obj)
      implicit none
      type(scab_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%tr_amp)
      nullify(obj%tr_src)
      nullify(obj%tr_rec)
      nullify(obj%tr_off)

      nullify(obj%src_arr)
      nullify(obj%rec_arr)
      nullify(obj%off_arr)

      nullify(obj%indx)
      nullify(obj%tmp_med)
      nullify(obj%run_arr)
      nullify(obj%tmp_arr)
      nullify(obj%wrk)

      nullify(obj%trcio)

      call scab_initialize (obj)

      return
      end subroutine scab_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine scab_delete (obj)
      implicit none
      type(scab_struct),pointer :: obj       ! arguments

!<execute_only>
      call scab_wrapup (obj)

      call mem_free(obj%tr_amp)
      call mem_free(obj%tr_src)
      call mem_free(obj%tr_rec)
      call mem_free(obj%tr_off)

      call mem_free(obj%src_arr)
      call mem_free(obj%rec_arr)
      call mem_free(obj%off_arr)

      call mem_free(obj%indx)
      call mem_free(obj%tmp_med)
      call mem_free(obj%run_arr)
      call mem_free(obj%tmp_arr)
      call mem_free(obj%wrk)

!</execute_only>

      deallocate(obj)

      return
      end subroutine scab_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine scab_initialize (obj)
      implicit none
      type(scab_struct),intent(inout) :: obj       ! arguments

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      obj%passes = 2
      obj%mode = 'CALC'
      obj%tr_max = 10000
      obj%stat_tr = 'MED'
      obj%stat_sc = 'MED'
      obj%path_corr = PATHCHECK_EMPTY
      obj%path_diag = PATHCHECK_EMPTY

      obj%use_src = .true.
      obj%hdr_src = 9
      obj%src_init = 1.0
      obj%src_inc = 1.0
      obj%src_last = 1.0
      obj%src_tot = 1

      obj%use_rec = .false.
      obj%hdr_rec = 47
      obj%rec_init = 1.0
      obj%rec_inc = 1.0
      obj%rec_last = 1.0
      obj%rec_tot = 1

      obj%use_off = .true.
      obj%hdr_off = 6
      obj%off_init = 1.0
      obj%off_inc = 1.0
      obj%off_last = 1.0
      obj%off_tot = 1

      obj%type_ra = 'MED'
      obj%ras_src = 20
      obj%ras_rec = 20
      obj%ras_off = 20
      obj%range_kill = 0.5
      obj%range_corr = 0.15

      obj%hdr_off_win = 10
      obj%offsets = 0.0
      obj%tim_add = 0.0
      obj%win_len = 0.0

      obj%offsets(1) = 0.0
      obj%tim_add(1) = 0.0
      obj%win_len(1) = obj%tstrt + (obj%ndpt - 1) * obj%dt
      obj%offsets_cnt = 1

      obj%lun_corr = 0
      obj%lun_diag_src = 0
      obj%lun_diag_rec = 0
      obj%lun_diag_off = 0

      obj%tr_in = 0
      obj%tr_out = 0
      obj%tr_last = 0

      call scab_update (obj)
      return
      end subroutine scab_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine scab_update (obj)
      implicit none
      type(scab_struct),intent(inout),target :: obj             ! arguments

      integer :: offsets_cnt2                                   ! local
      integer :: offsets_cnt3                                   ! local
      integer :: state                                          ! local
      integer :: status                                         ! local
      integer :: tr_do                                          ! local
      integer :: win_do                                         ! local
      integer :: idx,i  ,ncrd ! local
      integer :: nscratch                                       ! local
      integer :: max_ras                                        ! local
      real :: time_end                                          ! local
      real :: src_corr                                          ! local
      real :: rec_corr                                          ! local
      real :: off_corr                                          ! local
      real :: dummy_idx                                         ! local
      character(len=MAX_LINE) :: dummy
      character(len=FILENAME_LENGTH) :: tmp_filename
      character(len=FILENAME_LENGTH) :: tmp_path
      character(len=FILENAME_LENGTH) :: tmp_file
      character(len=80),dimension(:),pointer :: card            ! local
      character(len=16) :: path_user, path_node
      logical :: verify                                         ! local

      nullify (card) ! jpa

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("offsets_arrayset", (/  &
                                    "offsets",              &
                                    "tim_add",              &
                                    "win_len" /))

      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      call pc_get('passes', obj%passes)
      call pc_get('mode', obj%mode)
      call string_to_upper(object%mode)
      call pc_get('tr_max', obj%tr_max)
      call pc_get('stat_tr', obj%stat_tr)
      call string_to_upper(object%stat_tr)
      call pc_get('stat_sc', obj%stat_sc)
      call string_to_upper(object%stat_sc)
      call pc_get('path_corr', obj%path_corr)
      call pc_get('path_diag', obj%path_diag)

      call pc_get('use_src', obj%use_src)
      call pc_get('hdr_src', obj%hdr_src)
      call pc_get('src_init', obj%src_init)
      call pc_get('src_inc', obj%src_inc)
      call pc_get('src_last', obj%src_last)
      call pc_get('src_tot', obj%src_tot)

      call pc_get('use_rec', obj%use_rec)
      call pc_get('hdr_rec', obj%hdr_rec)
      call pc_get('rec_init', obj%rec_init)
      call pc_get('rec_inc', obj%rec_inc)
      call pc_get('rec_last', obj%rec_last)
      call pc_get('rec_tot', obj%rec_tot)

      call pc_get('use_off', obj%use_off)
      call pc_get('hdr_off', obj%hdr_off)
      call pc_get('off_init', obj%off_init)
      call pc_get('off_inc', obj%off_inc)
      call pc_get('off_last', obj%off_last)
      call pc_get('off_tot', obj%off_tot)

      call pc_get('type_ra', obj%type_ra)
      call string_to_upper(object%type_ra)
      call pc_get('ras_src', obj%ras_src)
      call pc_get('ras_rec', obj%ras_rec)
      call pc_get('ras_off', obj%ras_off)
      call pc_get('range_kill', obj%range_kill)
      call pc_get('range_corr', obj%range_corr)

      call pc_get('hdr_off_win', obj%hdr_off_win)

      offsets_cnt2 = obj%offsets_cnt
      offsets_cnt3 = obj%offsets_cnt
      call pc_get('offsets', obj%offsets, obj%offsets_cnt)
      call pc_get('tim_add', obj%tim_add, offsets_cnt2)
      call pc_get('win_len', obj%win_len, offsets_cnt3)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%passes == 1) then
        if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
          pc_verify_scalar('path_corr')) then
          call pathcheck('path_corr', obj%path_corr, ext='.scab', &
            required=.false., status=status)

          if(status /= PATHCHECK_VALID .and. &
            status /= PATHCHECK_UNSPECIFIED) then
            call pc_error('invalid PATH_CORR, optional when PASSES = 1')
          else
            ! remove prefix, if any ("user@node:...").
            call path_parse(obj%path_corr, path_user, path_node, &
              tmp_path, tmp_file)

            obj%path_corr = trim(tmp_path) // tmp_file
          end if
        end if
      else if(obj%passes == 2) then
        if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
          pc_verify_scalar('path_corr')) then
          call pathcheck('path_corr', obj%path_corr, ext='.scab', &
            required=.true., status=status)

          if(status /= PATHCHECK_VALID) then
            call pc_error('valid PATH_CORR is required when PASSES = 2')
          else
            ! remove prefix, if any ("user@node:...").
            call path_parse(obj%path_corr, path_user, path_node, &
              tmp_path, tmp_file)

            obj%path_corr = trim(tmp_path) // tmp_file
          end if
        end if
      else
        call pc_error('Invalid PASSES value. Valid values are 1 or 2')
        obj%passes = 2
      end if

      call pathcheck('path_diag', obj%path_diag, &
        required=.false., status=status)

      ! remove prefix, if any ("user@node:...").
      call path_parse(obj%path_diag, path_user, path_node, &
        tmp_path, tmp_file)

      if(status /= PATHCHECK_VALID .and. &
        status /= PATHCHECK_UNSPECIFIED) then
        call pc_error('invalid PATH_DIAG')
      else
        obj%path_diag = trim(tmp_path) // tmp_file
      end if

      if(all(mode_options /= obj%mode)) then
        call pc_error( &
          'Invalid MODE value. Valid values are CALC, APPLY, or REMOVE')
        obj%mode = 'CALC'
      end if

      if(obj%tr_max <= 0) then
        call pc_error('Invalid TR_MAX.  Must be greater than 0')
        obj%tr_max = 10000
      end if

      if(all(stat_tr_options /= obj%stat_tr)) then
        call pc_error( &
          'Invalid STAT_TR value. Valid values are MED or AVE')
        obj%stat_tr = 'MED'
      end if

      if(all(stat_sc_options /= obj%stat_sc)) then
        call pc_error( &
          'Invalid STAT_SC value. Valid values are MED or AVE')
        obj%stat_sc = 'MED'
      end if

      if(obj%use_src) then
        if(obj%hdr_src <= 0 .or. obj%hdr_src > obj%nwih) then
          call pc_error( &
            'Invalid HDR_SRC value.  Must be between 1 and NWIH, inclusive')
          obj%hdr_src = 9
        end if

        status = pattern_stop2('SCAB:', verify, &
          obj%src_init, obj%src_inc, obj%src_last, obj%src_tot, &
          'SRC_INIT', 'SRC_INC', 'SRC_LAST', 'SRC_TOT', &
          pc_verify_scalar('src_init'), pc_verify_scalar('src_inc'), &
          pc_verify_scalar('src_last'), pc_verify_scalar('src_tot'), &
          inc_min=0.0)

        if(obj%ras_src < 1) then
          call pc_error('Invalid RAS_SRC. Value must be greater than 0')
          obj%ras_src = 20
        end if
      end if

      if(obj%use_rec) then
        if(obj%hdr_rec <= 0 .or. obj%hdr_rec > obj%nwih) then
          call pc_error( &
            'Invalid HDR_REC value.  Must be between 1 and NWIH, inclusive')
          obj%hdr_rec = 47
        end if

        status = pattern_stop2('SCAB:', verify, &
          obj%rec_init, obj%rec_inc, obj%rec_last, obj%rec_tot, &
          'REC_INIT', 'REC_INC', 'REC_LAST', 'REC_TOT', &
          pc_verify_scalar('rec_init'), pc_verify_scalar('rec_inc'), &
          pc_verify_scalar('rec_last'), pc_verify_scalar('rec_tot'), &
          inc_min=0.0)

        if(obj%ras_rec < 1) then
          call pc_error('Invalid RAS_REC. Value must be greater than 0')
          obj%ras_rec = 20
        end if
      end if

      if(obj%use_off) then
        if(obj%hdr_off <= 0 .or. obj%hdr_off > obj%nwih) then
          call pc_error( &
            'Invalid HDR_OFF value.  Must be between 1 and NWIH, inclusive')
          obj%hdr_off = 6
        end if

        status = pattern_stop2('SCAB:', verify, &
          obj%off_init, obj%off_inc, obj%off_last, obj%off_tot, &
          'OFF_INIT', 'OFF_INC', 'OFF_LAST', 'OFF_TOT', &
          pc_verify_scalar('off_init'), pc_verify_scalar('off_inc'), &
          pc_verify_scalar('off_last'), pc_verify_scalar('off_tot'), &
          inc_min=0.0)

        if(obj%ras_off < 1) then
          call pc_error('Invalid RAS_OFF. Value must be greater than 0')
          obj%ras_off = 20
        end if
      end if

      if(.not. obj%use_src .and. &
         .not. obj%use_rec .and. &
         .not. obj%use_off) then
        call pc_error('One or more required (USE_SRC, USE_REC, USER_OFF)')
      end if

      if(all(type_ra_options /= obj%type_ra)) then
        call pc_error( &
          'Invalid TYPE_RA value. Valid values are MED or AVE')
        obj%type_ra = 'MED'
      end if

      if(obj%range_kill < 0.0) then
        call pc_error('Invalid RANGE_KILL. Value must not be negative')
        obj%range_kill = 0.5
      else if(obj%range_kill == 0.0) then
        if(obj%range_corr < 0.0) then
          call pc_error('Invalid RANGE_CORR. Value must not be negative')
          obj%range_corr = 0.15
        end if
      else if(obj%range_corr >= obj%range_kill .or. obj%range_corr < 0.0) then
        call pc_error('Invalid RANGE_CORR. Valid iff 0.0 <= Value < RANGE_KILL')
        obj%range_corr = min(0.15, 0.3 * obj%range_kill)
      end if

      if(obj%hdr_off_win <= 0 .or. obj%hdr_off_win > obj%nwih) then
        call pc_error( &
          'Invalid HDR_OFF_WIN value.  Must be between 1 and NWIH, inclusive')
        obj%hdr_off_win = 10
      end if

      if(verify .or. pc_verify_arrayset('offsets_arrayset')) then

        if(obj%offsets_cnt == 0) then
          call pc_error('one or more OFFSETS/TIM_ADD/WIN_LEN required')
        end if

        time_end = obj%tstrt + (obj%ndpt - 1) * obj%dt

        do win_do = 1, obj%offsets_cnt
          if(obj%offsets(win_do) < 0.0 .or. &
            obj%offsets(win_do) == FNIL) then
            call pc_error( &
              'OFFSETS one or more values undefined or less than 0.0')
            exit
          end if

          if(obj%tim_add(win_do) < 0.0 .or. &
            obj%tim_add(win_do) >= time_end .or. &
            obj%tim_add(win_do) == FNIL) then
            call pc_error( &
              'TIM_ADD one or more values undefined, > time end or < 0.0')
            exit
          else
            idx = nint(obj%tim_add(win_do)/obj%dt)

            if(0 /= mth_compare(obj%tim_add(win_do), idx*obj%dt)) then
              call pc_warning('Rounding TIM_ADD(', win_do, &
                ') to nearest DT')
              obj%tim_add(win_do) = idx*obj%dt
            end if
          end if

          if(obj%win_len(win_do) <= 0.0 .or. &
            obj%win_len(win_do) + obj%tim_add(win_do) > time_end .or. &
            obj%win_len(win_do) == FNIL) then
            call pc_error( &
              'WIN_LEN one or more values undefined, > time end or <= 0.0')
            exit
          else
            idx = nint(obj%win_len(win_do)/obj%dt)

            if(0 /= mth_compare(obj%win_len(win_do), idx*obj%dt)) then
              call pc_warning('Rounding WIN_LEN(', win_do, &
                ') to nearest DT')
              obj%win_len(win_do) = idx*obj%dt
            end if
          end if
        end do
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('passes', &
        passes_options, 2)
      call pc_put('passes', obj%passes)

      call pc_put_options_field('mode', &
        mode_options, 3)
      call pc_put('mode', obj%mode)

      call pc_put('tr_max', obj%tr_max)

      call pc_put_options_field('stat_tr', &
        stat_tr_options,2)
      call pc_put('stat_tr', obj%stat_tr)

      call pc_put_options_field('stat_sc', &
        stat_sc_options, 2)
      call pc_put('stat_sc', obj%stat_sc)

      call pc_put('path_corr', obj%path_corr)
      call pc_put('path_diag', obj%path_diag)

      call pc_put_options_field('use_src', (/ "YES", "NO " /), 2)

      call pc_put('use_src', obj%use_src)
      call pc_put('hdr_src', obj%hdr_src)
      call pc_put('src_init', obj%src_init)
      call pc_put('src_inc', obj%src_inc)
      call pc_put('src_last', obj%src_last)
      call pc_put('src_tot', obj%src_tot)

      call pc_put_options_field('use_rec', (/ "YES", "NO " /), 2)

      call pc_put('use_rec', obj%use_rec)
      call pc_put('hdr_rec', obj%hdr_rec)
      call pc_put('rec_init', obj%rec_init)
      call pc_put('rec_inc', obj%rec_inc)
      call pc_put('rec_last', obj%rec_last)
      call pc_put('rec_tot', obj%rec_tot)

      call pc_put_options_field('use_off', (/ "YES", "NO " /), 2)

      call pc_put('use_off', obj%use_off)
      call pc_put('hdr_off', obj%hdr_off)
      call pc_put('off_init', obj%off_init)
      call pc_put('off_inc', obj%off_inc)
      call pc_put('off_last', obj%off_last)
      call pc_put('off_tot', obj%off_tot)

      call pc_put_options_field('type_ra', type_ra_options, 2)
      call pc_put('type_ra', obj%type_ra)

      call pc_put('ras_src', obj%ras_src)
      call pc_put('ras_rec', obj%ras_rec)
      call pc_put('ras_off', obj%ras_off)

      call pc_put('range_kill', obj%range_kill)
      call pc_put('range_corr', obj%range_corr)

      call pc_put('hdr_off_win', obj%hdr_off_win)
      call pc_put('offsets', obj%offsets, obj%offsets_cnt)
      call pc_put('tim_add', obj%tim_add, obj%offsets_cnt)
      call pc_put('win_len', obj%win_len, obj%offsets_cnt)

      if(obj%use_src) then
        call pc_put_sensitive_field_flag ('hdr_src', .true.)
        call pc_put_sensitive_field_flag ('src_init', .true.)
        call pc_put_sensitive_field_flag ('src_inc', .true.)
        call pc_put_sensitive_field_flag ('src_last', .true.)
        call pc_put_sensitive_field_flag ('src_tot', .true.)
      else
        call pc_put_sensitive_field_flag ('hdr_src', .false.)
        call pc_put_sensitive_field_flag ('src_init', .false.)
        call pc_put_sensitive_field_flag ('src_inc', .false.)
        call pc_put_sensitive_field_flag ('src_last', .false.)
        call pc_put_sensitive_field_flag ('src_tot', .false.)
      end if

      if(obj%use_rec) then
        call pc_put_sensitive_field_flag ('hdr_rec', .true.)
        call pc_put_sensitive_field_flag ('rec_init', .true.)
        call pc_put_sensitive_field_flag ('rec_inc', .true.)
        call pc_put_sensitive_field_flag ('rec_last', .true.)
        call pc_put_sensitive_field_flag ('rec_tot', .true.)
      else
        call pc_put_sensitive_field_flag ('hdr_rec', .false.)
        call pc_put_sensitive_field_flag ('rec_init', .false.)
        call pc_put_sensitive_field_flag ('rec_inc', .false.)
        call pc_put_sensitive_field_flag ('rec_last', .false.)
        call pc_put_sensitive_field_flag ('rec_tot', .false.)
      end if

      if(obj%use_off) then
        call pc_put_sensitive_field_flag ('hdr_off', .true.)
        call pc_put_sensitive_field_flag ('off_init', .true.)
        call pc_put_sensitive_field_flag ('off_inc', .true.)
        call pc_put_sensitive_field_flag ('off_last', .true.)
        call pc_put_sensitive_field_flag ('off_tot', .true.)
      else
        call pc_put_sensitive_field_flag ('hdr_off', .false.)
        call pc_put_sensitive_field_flag ('off_init', .false.)
        call pc_put_sensitive_field_flag ('off_inc', .false.)
        call pc_put_sensitive_field_flag ('off_last', .false.)
        call pc_put_sensitive_field_flag ('off_tot', .false.)
      end if

      max_ras = MAX(obj%ras_src,obj%ras_rec,obj%ras_off)

      nscratch = 4 * obj%tr_max + max_ras + &
        obj%src_tot + obj%rec_tot + obj%off_tot

      call pc_put_control('nscratch', nscratch)

      if(obj%passes == 1) then
        call pc_put_control('need_request', .true.)
        call pc_put_control('need_label', .true.)
      else
        call pc_put_control('need_request', .false.)
        call pc_put_control('need_label', .false.)
      end if

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if(obj%passes == 1) then
        ! Create a temporary scratch file for traces
        tmp_filename = tempname("scab")

        if(tmp_filename == ' ') then
          call pc_error( &
            "TEMPNAME can't create (directory write privilege error?)" &
            // " filename =", tmp_filename)
          return
        end if

!???    obj%trcio => trcio_open(tmp_filename, &
!???      'w+', scratch=.true., nwih=obj%nwih, ndpt=obj%ndpt, &
!???      nbits=32, nbitshd=64)
        obj%trcio => trcio_open(tmp_filename, &
          'w+', scratch=.true.)

        if(.not. associated(obj%trcio)) then
          call pc_error('TRCIO_OPEN error opening temp filename=', tmp_filename)
          return
        end if

        obj%trcio%nbits = 32
        obj%trcio%nbits_hd = 64
        obj%trcio%nwih = obj%nwih
        obj%trcio%num_values = obj%ndpt

        status = trcio_writeheader(obj%trcio)

        if(status /= 0) then
          call pc_error('TRCIO_WRITEHEADER error initializing temp filename=', &
            tmp_filename)
          return
        end if
      end if

      call pc_alloc_process_cards(card, ncrd)

      if(obj%path_diag /= PATHCHECK_EMPTY) then
        ! Open diagnostic print file for SRC, REC and OFF.
        if(scab_open_diag(obj, obj%use_src, '.src', obj%lun_diag_src, &
          card, ncrd)) return

        if(scab_open_diag(obj, obj%use_rec, '.rec', obj%lun_diag_rec, &
          card, ncrd)) return

        if(scab_open_diag(obj, obj%use_off, '.off', obj%lun_diag_off, &
          card, ncrd)) return
      END IF

      call mem_alloc(obj%tr_amp, obj%tr_max)
      call mem_alloc(obj%tr_src, obj%tr_max)
      call mem_alloc(obj%tr_rec, obj%tr_max)
      call mem_alloc(obj%tr_off, obj%tr_max)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      obj%tr_amp = 0.0
      obj%tr_src = 0.0
      obj%tr_rec = 0.0
      obj%tr_off = 0.0

      IF (obj%use_src) call mem_alloc(obj%src_arr, obj%src_tot)
      IF (obj%use_rec) call mem_alloc(obj%rec_arr, obj%rec_tot)
      IF (obj%use_off) call mem_alloc(obj%off_arr, obj%off_tot)

      call mem_alloc(obj%indx, obj%tr_max)
      call mem_alloc(obj%tmp_med, obj%tr_max)
      call mem_alloc(obj%run_arr, MAX(obj%src_tot,obj%rec_tot,obj%off_tot))
      call mem_alloc(obj%tmp_arr, MAX(obj%src_tot,obj%rec_tot,obj%off_tot, &
        obj%ndpt))

      ! 2000-08-23 Selzler latent bug fix (+1 added to allocation).
      IF (max_ras > 0) call mem_alloc(obj%wrk, max_ras + 1)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      if(obj%mode == 'CALC' .or. &
        (obj%passes == 1 .and. obj%path_corr /= PATHCHECK_EMPTY)) then
        ! get output logical unit for correction file output
        call getlun(obj%lun_corr, status)

        if (status /= 0) then
          call pc_error('getlun failed, can not open correction file')
          return
        end if

        open(unit = obj%lun_corr, action = 'WRITE', &
          file = obj%path_corr, iostat = status, &
          status = 'REPLACE', recl = MAX_LINE)

        if(status /= 0) then
          call pc_error('iostat=',status,', opening new correction file ' // &
            trim(obj%path_corr))
          return
        end if

        WRITE (obj%lun_corr, '(A)') '# SCAB BALANCE CORRECTIONS'
        WRITE (obj%lun_corr, '(A)') '#'

        DO I = 1, NCRD
          WRITE (obj%lun_corr, '(A,A)') '#', CARD(I)
        END DO

        WRITE (obj%lun_corr, '(A)') '#'
        WRITE (obj%lun_corr, '(A)') &
    '#            SOURCE                         RECEIVER                 &
    &        OFFSET                         PRODUCT'
        WRITE (obj%lun_corr, '(A)') &
    '#    HEADER      CORRECTION          HEADER      CORRECTION        &
    &  HEADER      CORRECTION          TRACE#      CORRECTION'
        WRITE (obj%lun_corr, '(A)') '# END COMMENTS'
      else IF (obj%passes==2 .AND. obj%mode/='CALC') THEN
        ! get input logical unit for correction file input
        call getlun(obj%lun_corr, status)

        if (status /= 0) then
          call pc_error('getlun failed, can not open correction file')
          return
        end if

        open(unit = obj%lun_corr, action = 'READ', &
          file = obj%path_corr, iostat = status, &
          status = 'OLD', recl = MAX_LINE)

        if(status /= 0) then
          call pc_error('iostat=',status,', opening old correction file ' // &
            trim(obj%path_corr))
          return
        end if

        DUMMY = ' '

        DO WHILE(DUMMY(1:14) /= '# END COMMENTS')
          READ (obj%lun_corr, 1002, iostat=status) DUMMY

          if(status /= 0) then
            call pc_error('Read error, correction file=' // obj%path_corr // &
              ', status=', status)
            return
          end if

          call pc_print(dummy)
        END DO

        DO tr_do = 1, obj%tr_max
          READ (obj%lun_corr, 1001, iostat=status) &
            obj%tr_src(tr_do), SRC_CORR, obj%tr_rec(tr_do), &
            REC_CORR, obj%tr_off(tr_do), OFF_CORR, dummy_idx, obj%tr_amp(tr_do)

          if(status < 0) then
            call pc_warning('Premature EOF, correction file=' // obj%path_corr)
            exit
          else if(status /= 0) then
            call pc_error('Read error, correction file=' // obj%path_corr // &
              ', status=', status)
            return
          end if
        END DO

        obj%tr_last = tr_do - 1
      ENDIF


 1001 FORMAT(8(1X,G15.5))
 1002 FORMAT(A)

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine scab_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine scab (obj,ntr,hd,tr)
      implicit none
      type(scab_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer :: ntr_do, status
      integer :: isbin_min, irbin_min, iobin_min
      real :: scale

      input_trace_loop: &
      do ntr_do = 1, ntr
        if(obj%passes == 1) then
          ! save this trace by appending to temporary file
          status = trcio_write_trace(obj%trcio, &
            hd(:obj%nwih,ntr_do), tr(:obj%ndpt,ntr_do))

          if(status /= TRCIO_OK) then
            call pc_error('SCAB: TRCIO_WRITE_TRACE error, status=', status)
            ntr = FATAL_ERROR
            return
          end if
        end if

        obj%tr_in = obj%tr_in + 1

        if(obj%passes == 1 .or. obj%mode == 'CALC') then
          IF (obj%tr_in > obj%tr_max) then
            call pc_error('SCAB: input trace count exceeds TR_MAX')
            ntr = FATAL_ERROR
            return
          end if

          ! calculate statistics for this input trace
          call scab_calculate(obj, ntr_do, hd, tr)
        else
          IF (obj%tr_in > obj%tr_last) then
            call pc_error('SCAB: input trace count exceeds correction file')
            ntr = FATAL_ERROR
            return
          end if
        end if
      end do input_trace_loop

      if(obj%passes == 1 .and. ntr > 0) then
        ntr = NEED_TRACES
        return
      end if

      if(ntr == NO_MORE_TRACES .and. &
        (obj%passes == 1 .or. obj%mode == 'CALC')) then
        ! Form the average or median for each source, receiver, and offset
        obj%tr_last = obj%tr_in

        IF (obj%use_src) THEN
          call scab_source(obj, isbin_min)
        end if

        IF (obj%use_rec) THEN
          call scab_receiver(obj, irbin_min)
        end if

        IF (obj%use_off) THEN
          call scab_offset(obj, iobin_min)
        end if

        ! Calculate trace correction factor (composite of src, rec and off)
        call scab_correction(obj, isbin_min, irbin_min, iobin_min)
      end if

      if(obj%passes == 1) then
        IF (ntr == NO_MORE_TRACES) THEN
          !  Terminate input trace processing (output only now).
          ! Seek back to the beginning of the temporary trace file
          status = trcio_seek_trace(obj%trcio, 1)

          if(status /= TRCIO_OK) then
            call pc_error('SCAB: TRCIO_SEEK error, status=', status)
            ntr = FATAL_ERROR
            return
          end if
        end if

        IF (obj%tr_out < obj%tr_last) THEN
          obj%tr_out = obj%tr_out + 1

          ! read next trace from scratch file
          status = trcio_read_trace(obj%trcio, &
            hd(:obj%nwih,1), tr(:obj%ndpt,1))

          if(status /= TRCIO_OK) then
            call pc_error('SCAB: TRCIO_READ_TRACE error, status=', status)
            ntr = FATAL_ERROR
            return
          end if

          TR(:obj%ndpt,1) = obj%tr_amp(obj%tr_out)*TR(:obj%ndpt,1)

          call lav_set_hdr(hd(:obj%nwih,1), tr(:obj%ndpt,1), obj%ndpt)

          ntr = 1
        else
          ! normal termination, when passes == 1
          call scab_wrapup(obj)

          ntr = NO_MORE_TRACES
        end if

        return
      else if(ntr > 0) then
        ! Note: passes == 2
        do ntr_do = 1, ntr
          obj%tr_out = obj%tr_out + 1

          if(obj%mode == 'CALC') then
            IF (obj%tr_out > obj%tr_max) then
              call pc_error('SCAB: input trace count exceeds TR_MAX')
              ntr = FATAL_ERROR
              return
            end if
          else
            ! Note: mode is APPLY or REMOVE
            IF (obj%tr_out > obj%tr_last) then
              call pc_error('SCAB: too many traces for balance correction file')
              ntr = FATAL_ERROR
              return
            end if

            if(obj%mode == 'APPLY') then
              ! apply correction to each trace
              TR(:obj%ndpt,ntr_do) = obj%tr_amp(obj%tr_out)*TR(:obj%ndpt,ntr_do)
            else if(obj%mode == 'REMOVE') then
              ! remove correction from each trace
              IF (obj%tr_amp(obj%tr_out) /= 0.0) THEN
                SCALE = 1.0/obj%tr_amp(obj%tr_out)
              ELSE
                SCALE = 0.0
              ENDIF
   
              TR(:obj%ndpt,ntr_do) = SCALE*TR(:obj%ndpt,ntr_do)
            end if

            call lav_set_hdr(hd(:obj%nwih,ntr_do), tr(:obj%ndpt,ntr_do), &
              obj%ndpt)
          end if
        end do
      else
        ! normal termination, when passes == 2
        call scab_wrapup(obj)
      end if

      return
      end subroutine scab

!!--------------------------- scab_calculate -------------------------------!!
!!--------------------------- scab_calculate -------------------------------!!
!!--------------------------- scab_calculate -------------------------------!!

      subroutine scab_calculate(obj, ntr_do, hd, tr)
      implicit none
      type(scab_struct),intent(inout) :: obj
      integer          ,intent(in) :: ntr_do
      double precision ,intent(in) :: hd(:,:)
      real             ,intent(in) :: tr(:,:)

      ! calculate statistics for one input trace
      integer :: j, itwin, ibwin, lwin
      real off, twin, bwin, fact, tmp

      obj%tr_src(obj%tr_in) = HD(obj%hdr_src,ntr_do)
      obj%tr_rec(obj%tr_in) = HD(obj%hdr_rec,ntr_do)
      obj%tr_off(obj%tr_in) = HD(obj%hdr_off,ntr_do)
      OFF = HD(obj%hdr_off_win,ntr_do)

      IF (obj%offsets_cnt==1 .OR. OFF<=obj%offsets(1)) THEN
        TWIN = HD(2,ntr_do)*obj%dt + obj%tim_add(1)
        BWIN = TWIN + obj%win_len(1)
      ELSE IF (OFF >= obj%offsets(obj%offsets_cnt)) THEN
        TWIN = HD(2,ntr_do)*obj%dt + obj%tim_add(obj%offsets_cnt)
        BWIN = TWIN + obj%win_len(obj%offsets_cnt)
      ELSE
        DO J = 2, obj%offsets_cnt
          IF (OFF >= obj%offsets(J)) CYCLE
          EXIT
        END DO

        FACT = (OFF - obj%offsets(J-1)) / &
          (obj%offsets(J)-obj%offsets(J-1))
        TWIN = HD(2,ntr_do)*obj%dt + (obj%tim_add(J-1)+FACT* &
          (obj%tim_add(J)-obj%tim_add(J-1)))
        BWIN = TWIN + (obj%win_len(J-1)+FACT* &
          (obj%win_len(J)-obj%win_len(J-1)))
      ENDIF

      ITWIN = MIN(NINT(TWIN/obj%dt) + 1,obj%ndpt)
      IBWIN = MIN(NINT(BWIN/obj%dt) + 1,obj%ndpt)
      LWIN = IBWIN - ITWIN + 1

      IF (obj%stat_tr(1:3) == 'MED') THEN
        DO J = ITWIN, IBWIN
          obj%tmp_arr(J) = ABS(TR(J,ntr_do))
        END DO

        CALL MEDIAN (obj%tmp_arr(ITWIN:IBWIN), LWIN, obj%tr_amp(obj%tr_in))
      ELSE
        TMP = 0.0
        TMP = SUM(TR(ITWIN:IBWIN,ntr_do))
        obj%tr_amp(obj%tr_in) = ABS(TMP/LWIN)
      ENDIF

      return
      end subroutine scab_calculate

!!--------------------------- scab_source -------------------------------!!
!!--------------------------- scab_source -------------------------------!!
!!--------------------------- scab_source -------------------------------!!

      subroutine scab_source(obj, isbin_min)
      implicit none
      type(scab_struct),intent(inout) :: obj
      integer, intent(out) :: isbin_min

      integer :: tr_do, src_do
      integer :: lbin, ncnt, ii, ibin
      real :: hdsav, correct

      CALL SCAB_INDEX (obj%tr_last, obj%tr_src, obj%indx)

      obj%src_arr = 0.0
      obj%tmp_arr = 0.0

      src_run_block: &
      IF (obj%stat_sc(1:3) == 'MED') THEN       !Median
        obj%tmp_med = 0.0
        LBIN = mth_bin_number(obj%src_init,obj%src_inc,obj%tr_src(obj%indx(1)))
!rev21        LBIN = NINT((obj%tr_src(obj%indx(1))-obj%src_init) / &
!rev21          obj%src_inc+1.0)
        ISBIN_MIN = LBIN
        HDSAV = obj%tr_src(obj%indx(1))
        NCNT = 0
        II = 1

        src_median_loop: &
        DO tr_do = 1, obj%tr_last
          ibin=mth_bin_number(obj%src_init,obj%src_inc,&
                              obj%tr_src(obj%indx(tr_do)))
!rev21          IBIN = NINT((obj%tr_src(obj%indx(tr_do))-obj%src_init) / &
!rev21            obj%src_inc+1.0)

          IF (IBIN == LBIN) THEN
            NCNT = NCNT + 1
            obj%tmp_med(NCNT) = obj%tr_amp(obj%indx(tr_do))
          ELSE
            CALL MEDIAN (obj%tmp_med, NCNT, obj%src_arr(II))

            obj%tmp_arr(II) = HDSAV
            ISBIN_MIN = MIN0(LBIN,ISBIN_MIN)
            HDSAV = obj%tr_src(obj%indx(tr_do))
            II = II + 1

            IF (II > obj%src_tot) GO TO 204

            LBIN = IBIN
            NCNT = 1
          ENDIF
        END DO src_median_loop

        CALL MEDIAN (obj%tmp_med, NCNT, obj%src_arr(II))

        obj%tmp_arr(II) = HDSAV
        ISBIN_MIN = MIN0(LBIN,ISBIN_MIN)

  204   CONTINUE

        obj%src_tot = MIN0(II,obj%src_tot)

        CALL SCAB_RUN_MED (obj%ras_src, obj%src_tot, obj%src_arr, &
          obj%run_arr, obj%wrk)
      ELSE   ! src_run_block: Average
        lbin=mth_bin_number(obj%src_init,obj%src_inc,obj%tr_src(obj%indx(1)))
!rev21        LBIN = NINT((obj%tr_src(obj%indx(1))-obj%src_init)&
!rev21                     / obj%src_inc+1.0)
        ISBIN_MIN = LBIN
        HDSAV = obj%tr_src(obj%indx(1))
        NCNT = 0
        II = 1

        src_average_loop: &
        DO tr_do = 1, obj%tr_last
          ibin=mth_bin_number(obj%src_init,obj%src_inc,&
                              obj%tr_src(obj%indx(tr_do)))
!rev21          IBIN = NINT((obj%tr_src(obj%indx(tr_do))-obj%src_init) / &
!rev21            obj%src_inc+1.0)

          IF (IBIN == LBIN) THEN
            obj%src_arr(II) = obj%src_arr(II) + &
              obj%tr_amp(obj%indx(tr_do))
            NCNT = NCNT + 1
          ELSE
            obj%src_arr(II) = obj%src_arr(II)/NCNT
            obj%tmp_arr(II) = HDSAV
            ISBIN_MIN = MIN0(LBIN,ISBIN_MIN)
            HDSAV = obj%tr_src(obj%indx(tr_do))
            II = II + 1

            IF (II > obj%src_tot) GO TO 205

            obj%src_arr(II) = obj%src_arr(II) + &
              obj%tr_amp(obj%indx(tr_do))
            NCNT = 1
            LBIN = IBIN
          ENDIF
        END DO src_average_loop

        obj%src_arr(II) = obj%src_arr(II)/NCNT
        obj%tmp_arr(II) = HDSAV
        ISBIN_MIN = MIN0(LBIN,ISBIN_MIN)

  205   CONTINUE

        obj%src_tot = MIN0(II,obj%src_tot)
        CALL SCAB_RUN_AVE (obj%ras_src, obj%src_tot, obj%src_arr, obj%run_arr)
      ENDIF src_run_block

      ! Calculate source correction factor
      src_correction_loop: &
      DO src_do = 1, obj%src_tot
        IF ((obj%src_arr(src_do) > &
               obj%run_arr(src_do)*(1.0+obj%range_kill) .OR. &
             obj%src_arr(src_do) < &
               obj%run_arr(src_do)*(1.0-obj%range_kill)) .AND. &
          obj%range_kill/=(0.0)) THEN
          CORRECT = 0.0  ! kill it
        ELSE IF (obj%src_arr(src_do) /= 0.0 .and. &
          obj%range_corr/=0.0) THEN
          IF (obj%src_arr(src_do) > &
            obj%run_arr(src_do)*(1.0 + obj%range_corr)) THEN
            CORRECT = obj%run_arr(src_do) * &
              (1.0 + obj%range_corr)/obj%src_arr(src_do)
          ELSE IF (obj%src_arr(src_do) < &
            obj%run_arr(src_do)*(1.0 - obj%range_corr)) THEN
            CORRECT = obj%run_arr(src_do) * &
              (1.0 - obj%range_corr)/obj%src_arr(src_do)
          ELSE
            CORRECT = 1.0
          ENDIF
        ELSE
          CORRECT = 1.0
        ENDIF

        IF (obj%lun_diag_src > 0) THEN
          WRITE (obj%lun_diag_src, 1000) &
            obj%tmp_arr(src_do), obj%src_arr(src_do), &
              obj%run_arr(src_do), CORRECT
        end if

        obj%src_arr(src_do) = CORRECT
      END DO src_correction_loop

      IF (obj%lun_diag_src > 0) THEN
        close(obj%lun_diag_src)
        obj%lun_diag_src = 0
      ENDIF

      return

 1000 format(4(1x,g15.5))
      end subroutine scab_source

!!--------------------------- scab_receiver -------------------------------!!
!!--------------------------- scab_receiver -------------------------------!!
!!--------------------------- scab_receiver -------------------------------!!

      subroutine scab_receiver(obj, irbin_min)
      implicit none
      type(scab_struct),intent(inout) :: obj
      integer, intent(out) :: irbin_min

      integer :: tr_do, rec_do
      integer :: lbin, ncnt, ii, ibin
      real :: hdsav, correct

      CALL SCAB_INDEX (obj%tr_last, obj%tr_rec, obj%indx)
      obj%rec_arr = 0.0
      obj%tmp_arr = 0.0

      rec_run_block: &
      IF (obj%stat_sc(1:3) == 'MED') THEN       !Median
        obj%tmp_med = 0.0

        LBIN=mth_bin_number(obj%rec_init,obj%rec_inc,obj%tr_rec(obj%indx(1)))
!rev21        LBIN = NINT((obj%tr_rec(obj%indx(1))-obj%rec_init) / &
!rev21          obj%rec_inc+1.0)
        IRBIN_MIN = LBIN
        HDSAV = obj%tr_rec(obj%indx(1))
        NCNT = 0
        II = 1

        rec_median_loop: &
        DO tr_do = 1, obj%tr_last
          IBIN=mth_bin_number(obj%rec_init,obj%rec_inc,&
                              obj%tr_rec(obj%indx(tr_do)))
!rev21          IBIN = NINT((obj%tr_rec(obj%indx(tr_do))-obj%rec_init) / &
!rev21            obj%rec_inc+1.0)

          IF (IBIN == LBIN) THEN
            NCNT = NCNT + 1
            obj%tmp_med(NCNT) = obj%tr_amp(obj%indx(tr_do))
          ELSE
            CALL MEDIAN (obj%tmp_med, NCNT, obj%rec_arr(II))

            obj%tmp_arr(II) = HDSAV
            IRBIN_MIN = MIN0(LBIN,IRBIN_MIN)
            HDSAV = obj%tr_rec(obj%indx(tr_do))
            II = II + 1
            IF (II > obj%rec_tot) GO TO 304
            LBIN = IBIN
            NCNT = 1
          ENDIF
        END DO rec_median_loop

        CALL MEDIAN (obj%tmp_med, NCNT, obj%rec_arr(II))

        obj%tmp_arr(II) = HDSAV
        IRBIN_MIN = MIN0(LBIN,IRBIN_MIN)

  304   CONTINUE

        obj%rec_tot = MIN0(II,obj%rec_tot)

        CALL SCAB_RUN_MED (obj%ras_rec, obj%rec_tot, &
          obj%rec_arr, obj%run_arr, obj%wrk)
      ELSE ! rec_run_block, Average
        LBIN=mth_bin_number(obj%rec_init,obj%rec_inc,obj%tr_rec(obj%indx(1)))
!rev21        LBIN = NINT((obj%tr_rec(obj%indx(1))-obj%rec_init) / &
!rev21          obj%rec_inc+1.0)
        IRBIN_MIN = LBIN
        HDSAV = obj%tr_rec(obj%indx(1))
        NCNT = 0
        II = 1

        rec_average_loop: &
        DO tr_do = 1, obj%tr_last
          IBIN=mth_bin_number(obj%rec_init,obj%rec_inc,&
                              obj%tr_rec(obj%indx(tr_do)))
!rev21          IBIN = NINT((obj%tr_rec(obj%indx(tr_do))-obj%rec_init) / &
!rev21            obj%rec_inc+1.0)

          IF (IBIN == LBIN) THEN
            obj%rec_arr(II) = obj%rec_arr(II) + &
              obj%tr_amp(obj%indx(tr_do))
            NCNT = NCNT + 1
          ELSE
            obj%rec_arr(II) = obj%rec_arr(II)/NCNT
            obj%tmp_arr(II) = HDSAV
            IRBIN_MIN = MIN0(LBIN,IRBIN_MIN)
            HDSAV = obj%tr_rec(obj%indx(tr_do))
            II = II + 1

            IF (II > obj%rec_tot) GO TO 305

            obj%rec_arr(II) = obj%rec_arr(II) + &
              obj%tr_amp(obj%indx(tr_do))
            NCNT = 1
            LBIN = IBIN
          ENDIF
        END DO rec_average_loop

        obj%rec_arr(II) = obj%rec_arr(II)/NCNT
        obj%tmp_arr(II) = HDSAV
        IRBIN_MIN = MIN0(LBIN,IRBIN_MIN)

  305   CONTINUE

        obj%rec_tot = MIN0(II,obj%rec_tot)

        CALL SCAB_RUN_AVE (obj%ras_rec, obj%rec_tot, &
          obj%rec_arr, obj%run_arr)
      ENDIF rec_run_block

      ! Calculate receiver correction factor
      rec_correction_loop: &
      DO rec_do = 1, obj%rec_tot
        IF ((obj%rec_arr(rec_do) > &
               obj%run_arr(rec_do)*(1.0+obj%range_kill) .OR. &
             obj%rec_arr(rec_do) < &
               obj%run_arr(rec_do)*(1.0-obj%range_kill)) .AND. &
          obj%range_kill/=(0.0)) THEN
          CORRECT = 0.0  ! kill it
        ELSE IF (obj%rec_arr(rec_do) /= 0.0 .and. &
          obj%range_corr/=0.0) THEN

          IF (obj%rec_arr(rec_do) > &
            obj%run_arr(rec_do)*(1.0 + obj%range_corr)) THEN
            CORRECT = obj%run_arr(rec_do) * &
              (1.0 + obj%range_corr)/obj%rec_arr(rec_do)
          ELSE IF (obj%rec_arr(rec_do) < &
            obj%run_arr(rec_do)*(1.0 - obj%range_corr)) THEN
            CORRECT = obj%run_arr(rec_do) * &
              (1.0 - obj%range_corr)/obj%rec_arr(rec_do)
          ELSE
            CORRECT = 1.0
          ENDIF
        ELSE
          CORRECT = 1.0
        ENDIF

        IF (obj%lun_diag_rec > 0) THEN
          WRITE (obj%lun_diag_rec, 1000) &
            obj%tmp_arr(rec_do), obj%rec_arr(rec_do), &
              obj%run_arr(rec_do), CORRECT
        end if

        obj%rec_arr(rec_do) = CORRECT
      END DO rec_correction_loop

      IF (obj%lun_diag_rec > 0) THEN
        close(obj%lun_diag_rec)
        obj%lun_diag_rec = 0
      ENDIF

      return

 1000 format(4(1x,g15.5))
      end subroutine scab_receiver

!!--------------------------- scab_offset -------------------------------!!
!!--------------------------- scab_offset -------------------------------!!
!!--------------------------- scab_offset -------------------------------!!

      subroutine scab_offset(obj, iobin_min)
      implicit none
      type(scab_struct),intent(inout) :: obj
      integer, intent(out) :: iobin_min

      integer :: tr_do, off_do
      integer :: lbin, ncnt, ii, ibin
      real :: hdsav, correct

      CALL SCAB_INDEX (obj%tr_last, obj%tr_off, obj%indx)
      obj%off_arr = 0.0
      obj%tmp_arr = 0.0

      off_run_block: &
      IF (obj%stat_sc(1:3) == 'MED') THEN       !Median
        obj%tmp_med = 0.0
        LBIN=mth_bin_number(obj%off_init,obj%off_inc,obj%tr_off(obj%indx(1)))
!rev21        LBIN = NINT((obj%tr_off(obj%indx(1))-obj%off_init) / &
!rev21          obj%off_inc+1.0)
        IOBIN_MIN = LBIN
        HDSAV = obj%tr_off(obj%indx(1))
        NCNT = 0
        II = 1

        off_median_loop: &
        DO tr_do = 1, obj%tr_last
          IBIN=mth_bin_number(obj%off_init,obj%off_inc,&
                              obj%tr_off(obj%indx(tr_do)))
!rev21          IBIN = NINT((obj%tr_off(obj%indx(tr_do))-obj%off_init) / &
!rev21            obj%off_inc+1.0)

          IF (IBIN == LBIN) THEN
            NCNT = NCNT + 1
            obj%tmp_med(NCNT) = obj%tr_amp(obj%indx(tr_do))
          ELSE
            CALL MEDIAN (obj%tmp_med, NCNT, obj%off_arr(II))

            obj%tmp_arr(II) = HDSAV
            IOBIN_MIN = MIN0(LBIN,IOBIN_MIN)
            HDSAV = obj%tr_off(obj%indx(tr_do))
            II = II + 1
            IF (II > obj%off_tot) GO TO 404
            LBIN = IBIN
            NCNT = 1
          ENDIF
        END DO off_median_loop

        CALL MEDIAN (obj%tmp_med, NCNT, obj%off_arr(II))

        obj%tmp_arr(II) = HDSAV
        IOBIN_MIN = MIN0(LBIN,IOBIN_MIN)

  404   CONTINUE

        obj%off_tot = MIN0(II,obj%off_tot)

        CALL SCAB_RUN_MED (obj%ras_off, obj%off_tot, &
          obj%off_arr, obj%run_arr, obj%wrk)
      ELSE  ! off_run_block:, Average
        LBIN=mth_bin_number(obj%off_init,obj%off_inc,obj%tr_off(obj%indx(1))) 
!rev21        LBIN = NINT((obj%tr_off(obj%indx(1))-obj%off_init) / &
!rev21          obj%off_inc+1.0)
        IOBIN_MIN = LBIN
        HDSAV = obj%tr_off(obj%indx(1))
        NCNT = 0
        II = 1

        off_average_loop: &
        DO tr_do = 1, obj%tr_last
          IBIN=mth_bin_number(obj%off_init,obj%off_inc,&
                              obj%tr_off(obj%indx(tr_do)))
!rev21          IBIN = NINT((obj%tr_off(obj%indx(tr_do))-obj%off_init) / &
!rev21            obj%off_inc+1.0)

          IF (IBIN == LBIN) THEN
            obj%off_arr(II) = obj%off_arr(II) + &
              obj%tr_amp(obj%indx(tr_do))
            NCNT = NCNT + 1
          ELSE
            obj%off_arr(II) = obj%off_arr(II)/NCNT
            obj%tmp_arr(II) = HDSAV
            IOBIN_MIN = MIN0(LBIN,IOBIN_MIN)
            HDSAV = obj%tr_off(obj%indx(tr_do))
            II = II + 1
            IF (II > obj%off_tot) GO TO 405
            obj%off_arr(II) = obj%off_arr(II) + &
              obj%tr_amp(obj%indx(tr_do))
            NCNT = 1
            LBIN = IBIN
          ENDIF
        END DO off_average_loop

        obj%off_arr(II) = obj%off_arr(II)/NCNT
        obj%tmp_arr(II) = HDSAV
        IOBIN_MIN = MIN0(LBIN,IOBIN_MIN)

  405   CONTINUE

        obj%off_tot = MIN0(II,obj%off_tot)

        CALL SCAB_RUN_AVE (obj%ras_off, obj%off_tot, &
          obj%off_arr, obj%run_arr)
      ENDIF off_run_block

      ! Calculate offset correction factor
      off_correction_loop: &
      DO off_do = 1, obj%off_tot
        IF ((obj%off_arr(off_do) > &
               obj%run_arr(off_do)*(1.0+obj%range_kill) .OR. &
             obj%off_arr(off_do) < &
               obj%run_arr(off_do)*(1.0-obj%range_kill)) .AND. &
          obj%range_kill/=(0.0)) THEN
          CORRECT = 0.0
        ELSE IF (obj%off_arr(off_do) /= 0.0 .and. &
          obj%range_corr/=0.0) THEN

          IF (obj%off_arr(off_do) > &
            obj%run_arr(off_do)*(1.0 + obj%range_corr)) THEN
            CORRECT = obj%run_arr(off_do) * &
              (1.0 + obj%range_corr)/obj%off_arr(off_do)
          ELSE IF (obj%off_arr(off_do) < &
            obj%run_arr(off_do)*(1.0 - obj%range_corr)) THEN
            CORRECT = obj%run_arr(off_do) * &
              (1.0 - obj%range_corr)/obj%off_arr(off_do)
          ELSE
            CORRECT = 1.0
          ENDIF
        ELSE
          CORRECT = 1.0
        ENDIF

        IF (obj%lun_diag_off > 0) THEN
          WRITE (obj%lun_diag_off, 1000) &
            obj%tmp_arr(off_do), obj%off_arr(off_do), &
              obj%run_arr(off_do), CORRECT
        end if

        obj%off_arr(off_do) = CORRECT
      END DO off_correction_loop

      IF (obj%lun_diag_off > 0) THEN
        close(obj%lun_diag_off)
        obj%lun_diag_off = 0
      ENDIF

      return

 1000 format(4(1x,g15.5))
      end subroutine scab_offset

!!--------------------------- scab_correction -------------------------------!!
!!--------------------------- scab_correction -------------------------------!!
!!--------------------------- scab_correction -------------------------------!!

      subroutine scab_correction(obj, isbin_min, irbin_min, iobin_min)
      implicit none
      type(scab_struct),intent(inout) :: obj
      integer, intent(in) :: isbin_min
      integer, intent(in) :: irbin_min
      integer, intent(in) :: iobin_min

      integer :: tr_do, isbin, irbin, iobin
      real :: src_corr, rec_corr, off_corr

      ! Calculate trace correction factor (composite of src, rec and off)
      src_factor_block: &
      IF (obj%use_src) THEN

        src_rec_factor_block: &
        IF (obj%use_rec) THEN

          src_rec_off_loop: &
          DO tr_do = 1, obj%tr_last
            isbin=mth_bin_number(obj%src_init,obj%src_inc,obj%tr_src(tr_do)) &
                                 - ISBIN_MIN + 1
!rev21            ISBIN = NINT((obj%tr_src(tr_do)-obj%src_init) / &
!rev21              obj%src_inc+1.0) - ISBIN_MIN + 1
            irbin=mth_bin_number(obj%rec_init,obj%rec_inc,obj%tr_rec(tr_do)) &
                                 - IRBIN_MIN + 1
!rev21            IRBIN = NINT((obj%tr_rec(tr_do)-obj%rec_init) / &
!rev21              obj%rec_inc+1.0) - IRBIN_MIN + 1
            iobin=mth_bin_number(obj%off_init,obj%off_inc,obj%tr_off(tr_do)) &
                                 - IOBIN_MIN + 1
!rev21            IOBIN = NINT((obj%tr_off(tr_do)-obj%off_init) / &
!rev21              obj%off_inc+1.0) - IOBIN_MIN + 1

            IF (ISBIN>=1 .AND. ISBIN<=obj%src_tot) THEN
              SRC_CORR = obj%src_arr(ISBIN)
            ELSE
              SRC_CORR = 1.0
            ENDIF

            IF (IRBIN>=1 .AND. IRBIN<=obj%rec_tot) THEN
              REC_CORR = obj%rec_arr(IRBIN)
            ELSE
              REC_CORR = 1.0
            ENDIF

            IF (obj%use_off) THEN
              IF (IOBIN>=1 .AND. IOBIN<=obj%off_tot) THEN
                OFF_CORR = obj%off_arr(IOBIN)
              ELSE
                OFF_CORR = 1.0
              ENDIF
            ELSE
              OFF_CORR = 1.0
            ENDIF

            obj%tr_amp(tr_do) = SRC_CORR*REC_CORR*OFF_CORR

            IF (obj%lun_corr > 0) then
              WRITE (obj%lun_corr, 1001) &
                obj%tr_src(tr_do), SRC_CORR, &
                obj%tr_rec(tr_do), REC_CORR, &
                obj%tr_off(tr_do), OFF_CORR, &
                FLOAT(tr_do), obj%tr_amp(tr_do)
            end if
          END DO src_rec_off_loop
        ELSE  ! src_rec_factor_block

          src_off_loop: &
          DO tr_do = 1, obj%tr_last
            isbin=mth_bin_number(obj%src_init,obj%src_inc,obj%tr_src(tr_do)) &
                                 - ISBIN_MIN + 1
!rev21            ISBIN = NINT((obj%tr_src(tr_do)-obj%src_init) / &
!rev21              obj%src_inc+1.0) - ISBIN_MIN + 1
            irbin=mth_bin_number(obj%rec_init,obj%rec_inc,obj%tr_rec(tr_do)) &
                                 - IRBIN_MIN + 1
!rev21            IRBIN = NINT((obj%tr_rec(tr_do)-obj%rec_init) / &
!rev21              obj%rec_inc+1.0) - IRBIN_MIN + 1
            iobin=mth_bin_number(obj%off_init,obj%off_inc,obj%tr_off(tr_do)) &
                                 - IOBIN_MIN + 1
!rev21            IOBIN = NINT((obj%tr_off(tr_do)-obj%off_init) / &
!rev21              obj%off_inc+1.0) - IOBIN_MIN + 1

            IF (ISBIN>=1 .AND. ISBIN<=obj%src_tot) THEN
              SRC_CORR = obj%src_arr(ISBIN)
            ELSE
              SRC_CORR = 1.0
            ENDIF

            REC_CORR = 1.0

            IF (obj%use_off) THEN
              IF (IOBIN>=1 .AND. IOBIN<=obj%off_tot) THEN
                OFF_CORR = obj%off_arr(IOBIN)
              ELSE
                OFF_CORR = 1.0
              ENDIF
            ELSE
              OFF_CORR = 1.0
            ENDIF

            obj%tr_amp(tr_do) = SRC_CORR*REC_CORR*OFF_CORR

            IF (obj%lun_corr > 0) then
              WRITE (obj%lun_corr, 1001) &
                obj%tr_src(tr_do), SRC_CORR, &
                obj%tr_rec(tr_do), REC_CORR, &
                obj%tr_off(tr_do), OFF_CORR, &
                FLOAT(tr_do), obj%tr_amp(tr_do)
            end if
          END DO src_off_loop
        ENDIF src_rec_factor_block
      ELSE  ! src_factor_block

        rec_factor_block: &
        IF (obj%use_rec) THEN

          rec_off_loop: &
          DO tr_do = 1, obj%tr_last
            ISBIN=mth_bin_number(obj%src_init,obj%src_inc,obj%tr_src(tr_do)) &
                                 - ISBIN_MIN + 1
!rev21            ISBIN = NINT((obj%tr_src(tr_do)-obj%src_init) / &
!rev21              obj%src_inc+1.0) - ISBIN_MIN + 1
            IRBIN=mth_bin_number(obj%rec_init,obj%rec_inc,obj%tr_rec(tr_do)) &
                                 - IRBIN_MIN + 1
!rev21            IRBIN = NINT((obj%tr_rec(tr_do)-obj%rec_init) / &
!rev21              obj%rec_inc+1.0) - IRBIN_MIN + 1
            IOBIN=mth_bin_number(obj%off_init,obj%off_inc,obj%tr_off(tr_do)) &
                                 - IOBIN_MIN + 1
!rev21            IOBIN = NINT((obj%tr_off(tr_do)-obj%off_init) / &
!rev21              obj%off_inc+1.0) - IOBIN_MIN + 1
            SRC_CORR = 1.0

            IF (IRBIN>=1 .AND. IRBIN<=obj%rec_tot) THEN
              REC_CORR = obj%rec_arr(IRBIN)
            ELSE
              REC_CORR = 1.0
            ENDIF

            IF (obj%use_off) THEN
              IF (IOBIN>=1 .AND. IOBIN<=obj%off_tot) THEN
                OFF_CORR = obj%off_arr(IOBIN)
              ELSE
                OFF_CORR = 1.0
              ENDIF
            ELSE
              OFF_CORR = 1.0
            ENDIF

            obj%tr_amp(tr_do) = SRC_CORR*REC_CORR*OFF_CORR

            IF (obj%lun_corr > 0) then
              WRITE (obj%lun_corr, 1001) &
                obj%tr_src(tr_do), SRC_CORR, &
                obj%tr_rec(tr_do), REC_CORR, &
                obj%tr_off(tr_do), OFF_CORR, &
                FLOAT(tr_do), obj%tr_amp(tr_do)
            end if
          END DO rec_off_loop
        ELSE  ! rec_factor_block

          off_loop: &
          DO tr_do = 1, obj%tr_last
            ISBIN=mth_bin_number(obj%src_init,obj%src_inc,obj%tr_src(tr_do)) &
                                 - ISBIN_MIN + 1
!rev21            ISBIN = NINT((obj%tr_src(tr_do)-obj%src_init) / &
!rev21              obj%src_inc+1.0) - ISBIN_MIN + 1
            IRBIN=mth_bin_number(obj%rec_init,obj%rec_inc,obj%tr_rec(tr_do)) &
                                 - IRBIN_MIN + 1
!rev21            IRBIN = NINT((obj%tr_rec(tr_do)-obj%rec_init) / &
!rev21              obj%rec_inc+1.0) - IRBIN_MIN + 1
            IOBIN=mth_bin_number(obj%off_init,obj%off_inc,obj%tr_off(tr_do)) &
                                 - IOBIN_MIN + 1
!rev21            IOBIN = NINT((obj%tr_off(tr_do)-obj%off_init) / &
!rev21              obj%off_inc+1.0) - IOBIN_MIN + 1
            SRC_CORR = 1.0
            REC_CORR = 1.0

            IF (obj%use_off) THEN
              IF (IOBIN>=1 .AND. IOBIN<=obj%off_tot) THEN
                OFF_CORR = obj%off_arr(IOBIN)
              ELSE
                OFF_CORR = 1.0
              ENDIF
            ELSE
              OFF_CORR = 1.0
            ENDIF

            obj%tr_amp(tr_do) = SRC_CORR*REC_CORR*OFF_CORR

            IF (obj%lun_corr > 0) then
              WRITE (obj%lun_corr, 1001) &
                obj%tr_src(tr_do), SRC_CORR, &
                obj%tr_rec(tr_do), REC_CORR, &
                obj%tr_off(tr_do), OFF_CORR, &
                FLOAT(tr_do), obj%tr_amp(tr_do)
            end if
          END DO off_loop
        ENDIF rec_factor_block
      ENDIF src_factor_block

      IF (obj%lun_corr > 0) then
        close(obj%lun_corr)
        obj%lun_corr = 0
      ENDIF

      return

 1001 FORMAT(8(1X,G15.5))
      end subroutine scab_correction

!!--------------------------- scab_open_diag -------------------------------!!
!!--------------------------- scab_open_diag -------------------------------!!
!!--------------------------- scab_open_diag -------------------------------!!

      logical function scab_open_diag(obj, used, suffix, lun_diag, &
        card, ncrd)
      IMPLICIT NONE
      type(scab_struct),intent(inout),target :: obj             ! arguments
      logical,intent(in) :: used                                ! arguments
      character(len=*),intent(in) :: suffix                     ! arguments
      integer,intent(out) :: lun_diag                           ! arguments
      character(len=*),dimension(:),intent(in) :: card          ! arguments
      integer,intent(in) :: ncrd                                ! arguments

      ! Open diagnostic print file for SRC, REC or OFF.
      ! return .true. iff an error is detected.

      integer :: status, i  ! local
      character(len=FILENAME_LENGTH) :: tmp_filename            ! local

      if(used) then
        if(obj%passes == 1 .or. obj%mode == 'CALC') then
          tmp_filename = trim(obj%path_diag) // trim(suffix)

          ! get logical unit for diagnostic file
          call getlun(lun_diag, status)

          if (status == 0) then
            open(unit = lun_diag, action = 'WRITE', &
              file = tmp_filename, iostat = status, &
              status = 'REPLACE', recl = MAX_LINE)

            if(status /= 0) then
              call pc_error('iostat=',status,', opening diagnostic file ' // &
                trim(tmp_filename))

              scab_open_diag = .true.

              return
            end if
          else
            call pc_error('getlun failed, can not open diagnostic file')

            scab_open_diag = .true.

            return
          end if

          WRITE (lun_diag, '(A)') '# SCAB "' // trim(suffix) // &
            '" DIAGNOSTIC FILE'
          WRITE (lun_diag, '(A)') '#'

          DO I = 1, NCRD
            WRITE (lun_diag, '(A,A)') '#', CARD(I)
          END DO

          WRITE (lun_diag, '(A)') '#'
          WRITE (lun_diag, '(A)') &
    '#         HEADER       AMPLITUDE         RUNNING      CORRECTION'
          WRITE (lun_diag, '(A)') '# END COMMENTS'
        ENDIF
      ENDIF

      scab_open_diag = .false.

      return

      END function scab_open_diag

!!--------------------------- SCAB_INDEX -------------------------------!!
!!--------------------------- SCAB_INDEX -------------------------------!!
!!--------------------------- SCAB_INDEX -------------------------------!!

      SUBROUTINE SCAB_INDEX(ntr, ARRIN, INDX)
      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ntr
      INTEGER , INTENT(OUT) :: INDX(ntr)
      REAL , INTENT(IN) :: ARRIN(ntr)

      INTEGER :: J1, J, L, IR, INDXT, I
      REAL :: Q

      INDX = (/(J1,J1=1,ntr)/)

      L = ntr/2 + 1
      IR = ntr

   10 CONTINUE

      IF (L > 1) THEN
        L = L - 1
        INDXT = INDX(L)
        Q = ARRIN(INDXT)
      ELSE
        INDXT = INDX(IR)
        Q = ARRIN(INDXT)
        INDX(IR) = INDX(1)
        IR = IR - 1

        IF (IR == 1) THEN
          INDX(1) = INDXT
          RETURN
        ENDIF
      ENDIF

      I = L
      J = L + L

   20 CONTINUE
      IF (J <= IR) THEN
        IF (J < IR) THEN
          IF (ARRIN(INDX(J)) < ARRIN(INDX(J+1))) J = J + 1
        ENDIF

        IF (Q < ARRIN(INDX(J))) THEN
          INDX(I) = INDX(J)
          I = J
          J = J + J
        ELSE
          J = IR + 1
        ENDIF

        GO TO 20
      ENDIF

      INDX(I) = INDXT

      GO TO 10

      END SUBROUTINE SCAB_INDEX

!!--------------------------- SCAB_RUN_AVE -------------------------------!!
!!--------------------------- SCAB_RUN_AVE -------------------------------!!
!!--------------------------- SCAB_RUN_AVE -------------------------------!!

      SUBROUTINE SCAB_RUN_AVE(NRUN, NS, S, A)
      IMPLICIT NONE

      INTEGER , INTENT(IN) :: NRUN
      INTEGER , INTENT(IN) :: NS
      REAL , INTENT(IN) :: S(NS)
      REAL , INTENT(OUT) :: A(NS)

      REAL, PARAMETER :: ZNIL = 0.0

      INTEGER :: I, NRUN2, MRUN, JA, JB, NSUM, J
      REAL :: SUM

!-----------------------------------------------
!     GET RUNNING AVERAGE OF ARRAY VALUES.
!     SHIFTED ENDS OPTION IS USED.
!     S(NS) = INPUT VALUES (NILS ARE ALLOWED) (INPUT).
!     A(NS) = RUNNING AVERAGE OF INPUT VALUES (OUTPUT).
!
!----------DIMENSION STATEMENTS.
!----------GET STARTED.
      A = 0.
      NRUN2 = MIN0(NRUN,NS)
      MRUN = NRUN2/2
      IF (NRUN2 <= 0) RETURN
!----------CALCULATE RUNNING AVERAGE.
      DO I = 1, NS
        JA = MAX0(1,I - MRUN)
        JB = MAX0(JA,MIN0(NS,I + MRUN))
        SUM = 0.
        NSUM = 0

        DO J = JA, JB
          IF (S(J) == ZNIL) CYCLE
          SUM = SUM + S(J)
          NSUM = NSUM + 1
        END DO

        IF (NSUM == 0) CYCLE

        A(I) = SUM/NSUM
      END DO

      RETURN
      END SUBROUTINE SCAB_RUN_AVE

!!--------------------------- SCAB_RUN_MED -------------------------------!!
!!--------------------------- SCAB_RUN_MED -------------------------------!!
!!--------------------------- SCAB_RUN_MED -------------------------------!!

      SUBROUTINE SCAB_RUN_MED(NRUN, NS, S, A, W)
      IMPLICIT NONE

      INTEGER , INTENT(IN) :: NRUN
      INTEGER , INTENT(IN) :: NS
      REAL , INTENT(IN) :: S(NS)
      REAL , intent(out) :: A(NS)
      REAL  :: W(NRUN)

      REAL, PARAMETER :: ZNIL = 0.0

      INTEGER ::     I, NRUN2, MRUN, JA, JB, NW, J 

!-----------------------------------------------
!     GET RUNNING MEDIAN OF ARRAY VALUES.
!     SHIFTED ENDS OPTION IS USED.
!     S(NS)   = INPUT VALUES (NILS ARE ALLOWED) (INPUT).
!     W(NRUN) = WORK ARRAY (INPUT).
!     A(NS)   = RUNNING MEDIAN OF INPUT VALUES (OUTPUT).
!
!----------DIMENSION STATEMENTS.

      A = 0.
      NRUN2 = MIN0(NRUN,NS)
      MRUN = NRUN2/2
      IF (NRUN2 <= 0) RETURN

!----------CALCULATE RUNNING MEDIAN.
      DO I = 1, NS
        JA = MAX0(1,I - MRUN)
        JB = MAX0(JA,MIN0(NS,I + MRUN))
        NW = 0

        DO J = JA, JB
          IF (S(J) == ZNIL) CYCLE
          NW = NW + 1
          W(NW) = S(J)
        END DO

        IF (NW == 0) THEN
          CYCLE
        ELSE IF (NW == 1) THEN
          A(I) = W(NW)
        ELSE
          CALL MEDIAN (W, NW, A(I))
        ENDIF
      END DO

      RETURN
      END SUBROUTINE SCAB_RUN_MED

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine scab_wrapup (obj)
      implicit none
      type(scab_struct),intent(inout) :: obj       ! arguments
      integer :: status  ! local

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (associated(obj%trcio)) status = trcio_close (obj%trcio, .true.)

      return
      end subroutine scab_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module scab_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
