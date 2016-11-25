!<CPS_v1 type="PROCESS"/>

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
! Name       : DIST   (DISTribution function)
! Category   : diagnostics
! Written    : 1993-09-01   by: C I Burch
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Calculate distribution function of trace or header values.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! DIST performs distribution function (histogram) analysis of either trace
! sample values or header word values.  The analysis is done by assigning each
! input value to a bin corresponding to an amplitude range that includes the
! input value.  After all input values have been binned, the number of values
! assigned to a bin as a fraction of the total number of input values may be
! plotted as a function of bin number or bin center value.  Such a graph is
! the probability density function (PDF).
!
!
! Text Output
! If PATH_TROT is "NONE," the graph values are printed in the .rpt
! file.  Other statistics of the binned values are printed in the .rpt file,
! including minimum, maximum, mean, standard deviation and certain percentiles.
!
! All percentiles are based on bin occupation numbers and may be inexact by
! bin_width/2.
!
!
! Graphical Output
! Graphical output is provided by the TRCIO file output.
! Four types of graph can be chosen:
!
!      PDF       Probability Density Function (PDF); fraction of
!                  values falling in a given bin.
!      CDF       Cumulative Distribution Function (CDF); integral of
!                  the PDF.
!      1-PDF     1 - PDF or fraction of values outside each bin.
!      1-CDF     1 - CDF or fraction of values in higher bins.
!
! Each output trace represents one or more bins.  The center value of the bins
! represented by each output trace is the amplitude ordinate of the
! distribution graph.  The number of live samples in each output trace
! represents the probability ordinate of the distribution graph.  Displaying
! these traces in CBYT and using the header dump option provides easy access
! to distribution function information.  Output header words are:
!
!
!    HEADER               DESCRIPTION
!      1      Sequential output trace number
!      3      Fraction of binned values occupying this bin (PDF)
!      4      Fraction of binned values in this or lower bin (CDF)
!      5      Fraction of binned values outside this bin (1 - PDF)
!      6      Fraction of binned values in higher bins (1 - CDF)
!      7      Type of graph represented (header 3-6)
!      8      Bin center value  (x-ordinate)
!      9      Bin width
!      10     Median of binned values
!      11     Total values in this bin
!      12     Total values in this or lower bin
!      13     Total values binned
!      14     Number of live samples in this trace (NSPT*fraction)
!
!
! Mute Time
! DIST ignores all trace samples that lie above the head mute time or below the
! tail mute time.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Traces pass through DIST unchanged.
!
! Input values outside the amplitude range of the bins are ignored.
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
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
! This process does not alter input traces.
!
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
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
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
!
! Output graph traces have globals reset.
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!         HDR_FLAG                   Flag header
!         HDR_IN                     Input header word
! 2       Head mute                  Used
! 64      Tail mute                  Used
!
! Output graph traces have headers reset, see General Description
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author    Description
!     ----        ------    -----------
!031. 2006-06-12  B. Menger   Removed Unused Variables.
! 30. 2005-12-15  Stoeckley Change Val_Count and Vals_Binned_Count from integer
!                            to double precision to allow values > 2Gig.
! 29. 2002-09-19  Goodger   Use mth_module for binning where OPT_BINS=CENTER.
! 28. 2002-08-13  Goodger   Default win_len to trace length and insure it is
!                           not zero if opt_win=mute.
!                           Change bin_size from double precision to real to
!                           solve display problem.
!                           Move some error checking to traps.
!                           Shift header words used to avoid trcio resetting
!                           word 2.
! 27. 2002-06-26  CIBurch/  Correct bin index calculation.
!                 Goodger
! 26. 2002-05-22  Goodger   Change variables to double precision. 
! 25. 2001-09-10  Goodger   Remove extra definitions of Lun.
! 24. 2001-08-30  CIBurch/  Calculate and print RMS of binned values.
!                 Goodger   Change print* to writes.
! 23. 2001-05-14  SMCook    Updated GUI to use new "box labelling system".
! 22. 2001-04-03  SMCook    Bug report #352: doesn't work when OPT_IN = HEADER.
!                            Valid data was being inadvertantly thrown out by
!                            incorrect less than/greater than (< >) logic.
!                           Also found and fixed erroneous BIN CENTER values in
!                            printout.
!                           Also made GUI much more intelligible.
!                           Also caught pointer that wasn't nullified.
! 21. 2001-02-14  B. Kruse  Change name for wrapup flag to SKIP_WRAPUP for
!                            clarity, and slightly change how it is used.
! 20. 2000-11-20  B. Kruse  Bug Report #163, continued.  Correct bin-center
!                            values in Percentiles table.
! 19. 2000-11-15  B. Kruse  Bug report #163: Correct default NUM_TR from 100
!                            to 1000.  Add HELP documentation to clarify
!                            TIM_BEG and TIM_END times are in seconds.
!                            Identify samples rejected as out-of-range.
!                            Correct failure to accumulate data from trace
!                            samples.
! 18. 2000-10-05  B. Kruse  Added help section for BIN_SIZE, a read-only
!                            parameter
! 17. 2000-08-01  B. Kruse  Correct output file description from
!                            'bytefile' to TRCIO (or '.trc32') file.
!16.  2000-06-14 B. Kruse   Update GUI layout.
!15.  2000-06-13 B. Kruse   Bug report: PDF differences, CDF reported NaN
!                           Review comments
!14.  2000-05-15 B. Kruse   Insert GUI layout into source file.
!13.  2000-03-07 B. Kruse   Review comments:  Remove comment lines before the
!.                          prolog, change STROT to TROT.   Use standard
!.                          pathname.  Add PATHCHECK and TROT references
!.                          for the output file.
!12.  1999-11-19 B. Kruse   Removed formfeed characters
!11.  1999-11-05 B. Kruse   Converted to New System
!10.  1999-04-29 Vunderink  Moved check for NTOTB being 0 to before mean
!                           calculation
! 9.  1998-12-14 Vunderink  Begin using the f90 compiler.
! 8.  1998-08-13 C I Burch  Added printout of mean and std. deviation
! 7.  1997-06-30 Vunderink  Added header number to printout
! 6.  1997-06-23 Vunderink  Do not stack printout when FILE=NONE
! 5.  1997-06-23 Vunderink  Add "NONE" option for FILE parameter
! 4.  1996-06-19 Vunderink  Corrected error in saving globals
! 3.  1996-05-24 Vunderink  Changed output header words so that HD(1) is
!                           trace number
! 2.  1996-05-23 Vunderink  Changed output to be STROT file
! 1.  1993-09-01 C I Burch  Original version.
!
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

! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.

!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
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
!
!
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
!<--  XML code for the GUI goes in this section. />
!<gui_def>
!<NS DIST Process/NC=80/NR=30>
!          [/C]Probability DISTribution function of trace or header values.
!
!  `- General ----------------------------------------------------------------
!    OPT_IN=`CCCCCC  HDR_IN=`III                        HDR_FLAG=`III
!
!       TYPE_OUTPUT=`CCC         ABS=`CCC       IGNORE_ZEROS=`CCC
!  `--------------------------------------------------------------------------
!
!  `- Binning Scheme ---------------------------------------------------------
!    OPT_BINS=`CCCCCCC      VAL_MIN=`FFFFFFFFFFF      VAL_MAX=`FFFFFFFFFFF
!
!         NUM_BINS=`IIIIIIII    (Bin Size [BIN_SIZE/EN]=`FFFFFFFFFFF )
!  `--------------------------------------------------------------------------
!
!  `- Windowing Scheme -------------------------------------------------------
!    OPT_WIN=`CCCCC
!
!       Fixed Window~~~   TIM_BEG=`FFFFFFFFFFF     TIM_END=`FFFFFFFFFFF
!       Mute Window~~~~   TIM_ADD=`FFFFFFFFFFF     WIN_LEN=`FFFFFFFFFFF
!  `--------------------------------------------------------------------------
!
!  `- Optional TRCIO Graph ---------------------------------------------------
!    PATH_TROT=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!     Number of diagram bars  NUM_TR=`IIIIIIII  ~~~~~~~~~~~~~~~~~~~
!     Length of diagram bars  NSPT~~=`IIIIIIII  (samples per trace)
!  `--------------------------------------------------------------------------
!<PARMS PATH_TROT[/ML=128/XST]>
!</gui_def>

!!!!!!!<PARMS G[TYPE_OUTPUT]>

!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!<pm>
! If HDR_FLAG = 0, then all traces are processed.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are processed.
!</pm>
!</Help>
!
!<Help KEYWORD="OPT_IN">
!<Tip> Whether to use trace samples or header word values as input. </Tip>
! Default = TRACE
! Allowed = TRACE
! Allowed = HEADER
!</Help>
!
!<Help KEYWORD="HDR_IN">
!<Tip> Number of header word to use for input values. </Tip>
! Default = 1
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="ABS">
!<Tip> Whether to take absolute value of input values. </Tip>
! Default = YES
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="IGNORE_ZEROS">
!<Tip> Whether to ignore (do not bin) zero input values. </Tip>
! Default = YES
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="OPT_WIN">
!<Tip> Method to use for specifying trace time window. </Tip>
! Default = MUTE
! Allowed = MUTE     (Window defined by times measured from mute.)
! Allowed = FIXED    (Window is fixed.)
! DIST uses only the trace samples within the trace time window.
!
! If OPT_WIN = FIXED, then the window is specified by TIM_BEG and TIM_END.
!
! If OPT_WIN = MUTE, then the window is specified by TIM_ADD and WIN_LEN.
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Time for top of trace window if OPT_WIN = FIXED. </Tip>
! Default = TSTRT
! Allowed = real >= TSTRT
! The window top will be set at the larger of TIM_BEG, in seconds,
! or the mute time.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Time for bottom of trace window if OPT_WIN = FIXED. </Tip>
! Default =  end of trace
! Allowed = real >= TIM_BEG
! The window bottom will be set at the smaller of TIM_END, in seconds,
! or the tail mute time.
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Time at top of window = mute time + TIM_ADD. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! TIM_ADD in seconds is added to mute time to find the first sample.
!
! Active only if OPT_WIN = MUTE.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Time at bottom of window = time at top of window + WIN_LEN. </Tip>
! Default = trace length
! Allowed = real > 0.0
! WIN_LEN is the length of the trace window in seconds.
! Bottom of window cannot be below tail mute time.
!
! Active only if OPT_WIN = MUTE.
!</Help>
!
!<Help KEYWORD="OPT_BINS">
!<Tip> Method for specifying first and last bin values. </Tip>
! Default = CENTER
! Allowed = CENTER
! Allowed = OUTSIDE
! If OPT_BINS = CENTER, then VAL_MIN and VAL_MAX specify first and last bin
! center values.
!
! If OPT_BINS = OUTSIDE, then VAL_MIN specifies the lowest value in the first
! bin and VAL_MAX specifies the highest (noninclusive) value in the last bin.
! The OUTSIDE option is useful for specifying circular bin arrays.
!</Help>
!
!<Help KEYWORD="VAL_MIN">
!<Tip> Value used to specify first bin. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="VAL_MAX">
!<Tip> Value used to specify last bin. </Tip>
! Default = 10.0
! Allowed = real > VAL_MIN
!</Help>
!
!<Help KEYWORD="NUM_BINS">
!<Tip> Number of bins used to represent the value range. </Tip>
! Default = 1000
! Allowed = int > 1
!</Help>
!
!<Help KEYWORD="NUM_TR">
!<Tip> Number of output graph traces; NUM_TR must divide NUM_BINS. </Tip>
! Default = 1000
! Allowed = int =< NUM_BINS
!</Help>
!
!<Help KEYWORD="NSPT">
!<Tip> Number of samples in each output trace. </Tip>
! Default = 1000
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="TYPE_OUTPUT">
!<Tip> Type of distribution function graph the output traces will form. </Tip>
! Default = PDF
! Allowed = PDF, CDF, 1-PDF, 1-CDF
! TYPE_OUTPUT = PDF gives a Probability Density Function graph (fraction of
! values falling in a given bin).
!
! TYPE_OUTPUT = CDF gives a Cumulative Distribution Function graph (integral of
! the PDF).
!
! TYPE_OUTPUT = 1-PDF gives a 1 - PDF graph, or fraction of values outside each
! bin.
!
! TYPE_OUTPUT = 1-CDF gives a 1 - CDFgraph, or fraction of values in higher
! bins.
!</Help>
!
!<Help KEYWORD="PATH_TROT">
!<Tip> Pathname for the TRCIO file containing output graph traces. </Tip>
! Default =
! Allowed = char
! If PATH_TROT = NONE, the graph values are printed in the .rpt file.
!</Help>
!
!<Help KEYWORD="BIN_SIZE" TYPE= "DISPLAY_ONLY">
!<Tip> The size of a bin of the specified histogram.</Tip>
!
! The max and min values, and the number of bins, determine the size of
! each bin.  That size is calculated, and displayed for the user.
!
! The value of this field cannot be set directly.
!</Help>
!
!</HelpSection>
!------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module dist_module
  !
  ! - Modules
  !
  use mem_module
  use mth_module
  use named_constants_module
  use pathcheck_module
  use pc_module
  use rcpfile_module
  use string_module
  use trcio_module

  implicit none

  private

  public :: dist_create     ! uses the parameter cache.
  public :: dist_initialize
  public :: dist_update     ! uses the parameter cache.
  public :: dist_delete
  public :: dist_dump_object

!<execute_only>

  public :: dist            ! main execution (trace processing) routine.
  public :: dist_wrapup

!</execute_only>

  character(len=100),public,save :: dist_ident = &
    '$Id: dist.f90,v 1.31 2006/06/12 13:03:50 Menger prod sps $'

  !!--------------------- parameter structure ------------------------------!!
  !!--------------------- parameter structure ------------------------------!!
  !!--------------------- parameter structure ------------------------------!!
  !
  ! - Character string lengths
  !
  integer, parameter :: LEN8         = 8,    &
                        YN_LEN       = 3,    &
                        BIN_LEN      = 7,    &
                        WIN_LEN      = 5,    &
                        IN_LEN       = 6
  integer, parameter :: graph_type_len = 5
  !
  ! - The dist object structure
  !
  type, public :: dist_struct
    private
    logical :: skip_wrapup            ! wrapup flag.
    !
    ! - Common dist1
    !
    integer          :: Ls_Tr_Out              ! Common dist1, LSTROT
 !  integer :: Val_Count              ! Common dist1, NTOT
 !  integer :: Vals_Binned_Count      ! Common dist1, NTOTB
    double precision :: Val_Count              ! Common dist1, NTOT
    double precision :: Vals_Binned_Count      ! Common dist1, NTOTB
    integer          :: Header_Mask_Pos
    !
    logical :: Opt_Abs_Val_Flag       ! Common dist1, ABSV
    logical :: Opt_Ignore_Zeroes      ! Common dist1, IGZ
    logical :: Opt_Mask_Headers       ! Common dist1, NFLAG
    !
    real    :: Bin_Width              ! Common dist1, BW
    double precision    :: Sum                    ! Common dist1, SUM
    double precision    :: Sum_Sqr                ! Common dist1, SUMSQ
    double precision    :: Histo_Max_Val          ! Common dist1, VMAXB
    double precision    :: Histo_Min_Val          ! Common dist1, VMINB
    double precision    :: Histo_Rng_Lo           ! Common dist1, VMIN
    double precision    :: Histo_Rng_Hi           ! Common dist1, VMAX
    !
    ! - Common globals
    !
    integer :: nwih                   ! Common globals, NWIH
    integer :: ndpt                   ! Common globals, NDPT
    real    :: dt                     ! Common globals, DT
    real    :: tstrt                  ! Common globals, TSTRT
    !
    ! - Parameter-Cache User Parameters
    !     These parameters are described in the 'Help' section above
    !
    integer                           :: Hdr_Flag        ! PC
    character (len = IN_LEN)          :: Opt_In          ! PC
    integer                           :: Hdr_In          ! PC
    character (len = WIN_LEN)         :: Opt_Win         ! PC
    real                              :: Tim_Beg         ! PC
    real                              :: Tim_End         ! PC
    real                              :: Tim_Add         ! PC
    real                              :: Win_Len         ! PC
    character (len = BIN_LEN)         :: Opt_Bins        ! Common dist1, IBMODE
    real                              :: Val_Min         ! PC
    real                              :: Val_Max         ! PC
    integer                           :: Num_Bins        ! Common dist1, NBINS
    integer                           :: Num_TR          ! PC
    integer                           :: Nspt            ! PC
    integer                           :: Type_Output     ! PC
    character (len = filename_length) :: path_trot        ! PC
    logical                           :: write_file_flag
    character (len = graph_type_len)  :: Graph_Str
    !
    integer                           :: Missed_Small_Samples
    integer                           :: Missed_Large_Samples
    !
    ! - Internal Histogram
    !
    integer, pointer                  :: Histo (:)
    double precision,    pointer      :: Histo_Center (:)
    double precision                  :: c3
    !
  end type dist_struct


  integer :: Lun

  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!


  type(dist_struct),pointer,save :: object      ! needed for traps.
  !
  ! - Processing Alternatives
  !
  integer, parameter :: Num_Graph_Types = 4
  character (len = graph_type_len), parameter, dimension (Num_Graph_Types)    &
    :: GRAPH_TYPE_LABELS = (/ 'PDF  ', 'CDF  ', '1-PDF', '1-CDF' /)
  !
  ! - 'Omit TROT' filename check
  !
  character (len = *), parameter :: NONE = 'NONE'
  !
  ! - Number of words in the output header
  !
  integer, parameter :: Graph_Out_Hd_Len = 14
  !
contains


  !!---------------------------- create ------------------------------------!!
  !!---------------------------- create ------------------------------------!!
  !!---------------------------- create ------------------------------------!!


  subroutine dist_create (obj)
    !
    ! - Arguments
    !
    type (dist_struct), pointer :: obj
    !
    ! - Begin dist_create
    !
    allocate (obj)
    !
    ! - Nullify pointers in your parameter structure
    !
    nullify (obj%Histo)
    nullify (obj%Histo_Center)      !SMCook, this was missing
    !
    call dist_initialize (obj)
    !
  end subroutine dist_create


  !!------------------------------ delete ----------------------------------!!
  !!------------------------------ delete ----------------------------------!!
  !!------------------------------ delete ----------------------------------!!


  subroutine dist_delete (obj)
    !
    ! - Arguments
    !
    type (dist_struct), pointer :: obj
    !
    ! - Begin dist_delete
    !
!<execute_only>
    !
    call dist_wrapup (obj)
    !
!</execute_only>
    !
    if (associated (obj%Histo)) deallocate (obj%Histo)
    if (associated (obj%Histo_Center)) deallocate (obj%Histo_Center)
    !
    deallocate (obj)
    !
  end subroutine dist_delete


  !!---------------------------- initialize --------------------------------!!
  !!---------------------------- initialize --------------------------------!!
  !!---------------------------- initialize --------------------------------!!


  subroutine dist_initialize (obj)
    !
    ! - Arguments
    !
    type (dist_struct), intent (out) :: obj
    !
    ! - Begin dist_initialize
    !
    call pc_get_global ('NWIH',  obj%nwih)
    call pc_get_global ('DT',    obj%dt)
    call pc_get_global ('NDPT',  obj%ndpt)
    call pc_get_global ('TSTRT', obj%tstrt)
    !
    obj%skip_wrapup       = .true.
    !
    obj%Ls_Tr_Out         = 0
    obj%Val_Count         = 0.0   ! double precision to work beyond 2Gig.
    obj%Vals_Binned_Count = 0.0   ! double precision to work beyond 2Gig.
    obj%Header_Mask_Pos   = 0
    !
    obj%Opt_Abs_Val_Flag  = .true.   ! Yes, use absolute values of the data
    obj%Opt_Ignore_Zeroes = .true.   ! Yes, ignore zero samples
    obj%Opt_Mask_Headers  = .false.  ! No, use all traces without masking
    !
    obj%Bin_Width         = 0.0
    obj%Histo_Rng_Lo      = 0.0
    obj%Histo_Rng_Hi      = 0.0
    obj%Sum               = 0.0
    obj%Sum_Sqr           = 0.0
    obj%Histo_Max_Val     = 1.0E-29
    obj%Histo_Min_Val     = 1.0E+29
    !
    obj%Hdr_Flag        = 0
    obj%Opt_In          = 'TRACE'
    obj%Hdr_In          = 1
    obj%Opt_Win         = 'MUTE'
    obj%Tim_Beg         = obj%tstrt
    obj%Tim_End         = obj%dt * (obj%ndpt - 1)
    obj%Tim_Add         = 0.0
    obj%Win_Len         = obj%tim_end
    obj%Opt_Bins        = 'CENTER'
    obj%Val_Min         = 0.0
    obj%Val_Max         = 10.0
    obj%Num_Bins        = 1000
    obj%Num_Tr          = 1000
    obj%nspt            = 1000
    obj%Type_Output     = 1
    obj%Graph_Str       = GRAPH_TYPE_LABELS (obj%Type_Output)
    obj%path_trot       = PATHCHECK_EMPTY
    obj%write_file_flag = .false.
    !
    obj%Missed_Small_Samples = 0
    obj%Missed_Large_Samples = 0
    Lun = pc_get_lun ()


    !
    call dist_update (obj)
    !
  end subroutine dist_initialize


  !!------------------------ start of update -------------------------------!!
  !!------------------------ start of update -------------------------------!!
  !!------------------------ start of update -------------------------------!!


  subroutine dist_update (obj)
    !
    ! - Arguments
    !
    type (dist_struct), intent (inout), target :: obj
    !
    ! - Local variables for Parameters
    !
    integer :: bin
    integer :: fstatus

    integer :: i


    real    :: Trace_Len
    !
    ! - Begin dist_update
    !
    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.
    obj%Graph_Str   = GRAPH_TYPE_LABELS (obj%Type_Output)


    !!----------------------- read parameters ------------------------------!!
    !!----------------------- read parameters ------------------------------!!
    !!----------------------- read parameters ------------------------------!!

    !
    ! - Get Global parameters
    !
    call pc_get_global ('nwih',  obj%nwih)  ! number of header words.
    call pc_get_global ('ndpt',  obj%ndpt)  ! number of trace samples.
    call pc_get_global ('dt',    obj%dt)    ! trace sample interval (sec).
    call pc_get_global ('tstrt', obj%tstrt) ! time of 1st trace sample (sec).
    !
    ! - Get process parameters
    !
    call pc_get ('HDR_FLAG',   obj%Hdr_Flag)  ! Select trace masking
    call pc_get ('OPT_IN',     obj%Opt_In,  dist_opt_in_trap) 

    call pc_get ('HDR_IN',     obj%Hdr_In,dist_hdr_in_trap)
    call pc_get ('ABS',        obj%Opt_Abs_Val_Flag) ! Select Abs of data

    call pc_get ('IGNORE_ZEROS',obj%Opt_Ignore_Zeroes) ! Select or ignore zeros

    call pc_get ('OPT_WIN',    obj%Opt_Win,dist_opt_win_trap) 

    call pc_get ('TIM_BEG',    obj%Tim_Beg)       ! trace mute time
    call pc_get ('TIM_END',    obj%Tim_End)       ! trace trim time
    call pc_get ('TIM_ADD',    obj%Tim_Add)       ! Tr delay + mute
    call pc_get ('WIN_LEN',    obj%Win_Len)
    call pc_get ('OPT_BINS',   obj%Opt_Bins)      ! set bin range mode

    call pc_get ('VAL_MIN',    obj%Val_Min)       ! Histo range of least bin
    call pc_get ('VAL_MAX',    obj%Val_Max)       ! Histo range of last bin
    call pc_get ('NUM_BINS',   obj%Num_Bins,  dist_num_bins_trap)
    call pc_get ('NUM_TR',     obj%Num_Tr)        ! Number of output traces
    call pc_get ('NSPT',       obj%nspt)          ! Number Out Trace Pts
    call pc_get ('TYPE_OUTPUT',obj%Graph_Str)     ! Selected output function
    call pc_get ('PATH_TROT',  obj%path_trot)     ! Output file, or 'NONE'

    !!------------------------ verify parameters ---------------------------!!
    !!------------------------ verify parameters ---------------------------!!
    !!------------------------ verify parameters ---------------------------!!

    !
    ! - Data mode flags
    !
    if ((obj%Hdr_Flag < 0) .OR. (obj%Hdr_Flag > obj%nwih)) THEN
      !
      call pc_error (MSG1 = 'Hdr FLAG#, ',                           &
                     VAR1 = obj%Hdr_Flag,                            &
                     MSG2 = ' must be an integer between 0 and ',    &
                     VAR2 = obj%nwih)
      !
    else
      !
      obj%Opt_Mask_Headers = obj%Hdr_Flag /= 0
      obj%Header_Mask_Pos  = obj%Hdr_Flag
      !
    end if
    !
    if (obj%nwih < 1) then
      call pc_error (MSG1 = 'NWIH, ',   &
                     VAR1 = obj%nwih,   &
                     MSG2 = ', must not be less than 1')
    end if
    !
    !   -------------------------
    ! - Process Headers or Traces
    !   -------------------------
    !

    if (obj%Opt_In == 'TRACE') then
      !
      !
      if (obj%dt <= 0.0) then
        call pc_error (MSG1 = 'DT, ',   &
                       VAR1 = obj%dt,   &
                       MSG2 = ', must not be <= 0.0')
      end if
      !
      if (obj%ndpt < 1) then
        call pc_error (MSG1 = 'NDPT, ',   &
                       VAR1 = obj%dt,   &
                       MSG2 = ', must not be < 1')

      else if (obj%ndpt > 20000) then
        call pc_error (MSG1 = 'NDPT, ',   &
                       VAR1 = obj%dt,   &
                       MSG2 = ', must be less than, or equal to, 20000')

      end if

!      Trace_Len = obj%dt*obj%ndpt
      Trace_Len = obj%dt*(obj%ndpt-1)

      if (obj%tstrt > Trace_Len) THEN
        call pc_error (MSG1 = 'TSTRT, ',   &
                       VAR1 = obj%tstrt,   &
                       MSG2 = ',  cannot exceed trace length (DT * NDPT), ',   &
                       VAR2 = Trace_Len)

      end if
    endif


    if (obj%nspt < 1) THEN
      call pc_error (MSG1 = 'NSPT, ',   &
                     VAR1 = obj%nspt,   &
                     MSG2 = ', cannot be less than 1.')

    else if (obj%nspt > 20000) THEN
      call pc_error (MSG1 = 'NSPT, ',   &
                     VAR1 = obj%nspt,   &
                     MSG2 = ', cannot be greater than 20,000')
    end if

    if (obj%Opt_Bins == 'CENTER') THEN
      obj%Bin_Width=mth_bin_increment(obj%Val_Min,obj%Val_Max,obj%Num_Bins)
!rev29      obj%Bin_Width     = (obj%Val_Max - obj%Val_Min) / (obj%Num_Bins-1)
      obj%Histo_Rng_Lo  = obj%Val_Min - (obj%Bin_Width / 2.0)
      obj%Histo_Rng_Hi  = obj%Val_Max + (obj%Bin_Width / 2.0)

    else if (obj%Opt_Bins == 'OUTSIDE') THEN
      obj%Bin_Width     = (obj%Val_Max - obj%Val_Min) / obj%Num_Bins
      obj%Histo_Rng_Lo  = obj%Val_Min
      obj%Histo_Rng_Hi  = obj%Val_Max

    else
      call pc_error ('OPT_BINS, '    &
                     // obj%Opt_Bins    &
                     // ', must be CENTER or OUTSIDE')
    end if


    obj%Type_Output = 1
    do i = 2, Num_Graph_Types
      if (obj%Graph_Str == GRAPH_TYPE_LABELS (i)) then
        obj%Type_Output = i
        exit
      end if
    end do

    if ((index (string = obj%path_trot, substring = '.byt')       &
         + index (string = obj%path_trot, substring = '.Byt')     &
         + index (string = obj%path_trot, substring = '.BYT'))    &
        > 0) then
      !
      call pc_warning ('DIST: Output file is type TRCIO. ')
      call pc_warning ('      Filename extension cannot be ".byt" for file '  &
                       // trim (obj%path_trot))
      obj%write_file_flag = (fstatus == PATHCHECK_VALID)
      !
    else
      !
      call pathcheck (keyword  = 'PATH_TROT',        &
                      pathname = obj%path_trot,      &
                      ext      = "trc32",            &
                      required = .false.,            &
                      screen   = "DISTPROCESS",      &
                      status   = fstatus)
      obj%write_file_flag = (fstatus == PATHCHECK_VALID)
      !
    end if

    if(obj%write_file_flag) then
      call pc_put_sensitive_field_flag ('NSPT',   .true.)
    else
      call pc_put_sensitive_field_flag ('NSPT',   .false.)
    end if

    call pc_call_end_trap(dist_end_trap)


    !!----------------------- call processes internally --------------------!!
    !!----------------------- call processes internally --------------------!!
    !!----------------------- call processes internally --------------------!!


    !!----------------------- write parameters -----------------------------!!
    !!----------------------- write parameters -----------------------------!!
    !!----------------------- write parameters -----------------------------!!


    !
    ! - Write Global parameters
    !
    call pc_put_global ('nwih',  obj%nwih)  ! number of header words.
    call pc_put_global ('ndpt',  obj%ndpt)  ! number of trace samples.
    call pc_put_global ('dt',    obj%dt)    ! trace sample interval (sec).
    call pc_put_global ('tstrt', obj%tstrt) ! time of 1st trace sample (sec).
    !
    ! - Write process parameters
    !
    call pc_put ('HDR_FLAG',   obj%Hdr_Flag)  ! Select trace masking
    !
    call pc_put_options_field (keyword  = 'OPT_IN',                   &
                               options  = (/ 'TRACE ', 'HEADER' /),   &
                               noptions = 2)
    call pc_put ('OPT_IN',     obj%Opt_In)    ! Select input data: Hd or Tr
    !
    call pc_put ('HDR_IN',     obj%Hdr_In)    ! Set header data word

    call pc_put_options_field (keyword  = 'ABS',                    &
                               options  = (/ .true., .false. /),    &
                               noptions = 2)
    call pc_put ('ABS',        obj%Opt_Abs_Val_Flag) ! Select Abs of data
    !
    call pc_put_options_field (keyword  = 'IGNORE_ZEROS',           &
                               options  = (/ .true., .false. /),    &
                               noptions = 2)
    call pc_put ('IGNORE_ZEROS',   obj%Opt_Ignore_Zeroes)
    !
    call pc_put_options_field (keyword  = 'OPT_WIN',                    &
                               options  = (/ 'MUTE ', 'FIXED' /),    &
                               noptions = 2)
    call pc_put ('OPT_WIN',    obj%Opt_Win)       ! set Tr window mode
    !
    call pc_put ('TIM_BEG',    obj%Tim_Beg)       ! trace mute time
    call pc_put ('TIM_END',    obj%Tim_End)       ! trace trim time
    call pc_put ('TIM_ADD',    obj%Tim_Add)       ! Tr delay + mute
    call pc_put ('WIN_LEN',    obj%Win_Len)       ! Len of tr window
    !
    call pc_put_options_field (keyword  = 'OPT_BINS',                    &
                               options  = (/ 'CENTER ', 'OUTSIDE' /),    &
                               noptions = 2)
    call pc_put ('OPT_BINS',   obj%Opt_Bins)      ! set bin range mode
    !
    call pc_put ('VAL_MIN',    obj%Val_Min)       ! Histo range of least bin
    call pc_put ('VAL_MAX',    obj%Val_Max)       ! Histo range of last bin
    call pc_put ('NUM_BINS',   obj%Num_Bins)      ! Num Histo Bins
    call pc_put ('NUM_TR',     obj%Num_Tr)        ! Number of output traces
    call pc_put ('NSPT',       obj%nspt)          ! Number Out Trace Pts
    call pc_put ('BIN_SIZE',   obj%Bin_Width)     ! Calculated bin size


    !
    ! - Selected output function
    !
    call pc_put_options_field (keyword  = 'TYPE_OUTPUT',         &
                               options  = GRAPH_TYPE_LABELS,    &
                               noptions = Num_Graph_Types)
    call pc_put ('TYPE_OUTPUT', GRAPH_TYPE_LABELS (obj%Type_Output))
    !
    call pc_put ('PATH_TROT',   trim (obj%path_trot))  ! Output file, or 'NONE'


    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!


    if (associated (obj%Histo)) deallocate (obj%Histo)
    if (associated (obj%Histo_Center)) deallocate (obj%Histo_Center)

!<execute_only>

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.

    call mem_alloc (parray1 = obj%Histo,    &
                    n1      = obj%Num_Bins)

    if (associated (obj%Histo)) then
      obj%Histo = 0      ! Clear the histogram
    end if

    call mem_alloc (parray1 = obj%Histo_Center,    &
                    n1      = obj%Num_Bins)

    if (associated (obj%Histo_Center)) then
      do bin = 1, obj%Num_Bins
        obj%Histo_Center (bin) = dble(obj%Bin_Width) * dble (bin - 1)   &
                                 + dble(obj%Bin_Width) / 2.0
      end do
    end if

    !
    obj%Missed_Small_Samples = 0
    obj%Missed_Large_Samples = 0


    if(obj%Opt_Bins == 'CENTER')  obj%C3 = 1.0

    if(obj%Opt_Bins == 'OUTSIDE') obj%C3 = 0.5




!</execute_only>


    !!----------------------- finish update --------------------------------!!
    !!----------------------- finish update --------------------------------!!
    !!----------------------- finish update --------------------------------!!


  end subroutine dist_update


  !!------------------------------ traps -----------------------------------!!
  !!------------------------------ traps -----------------------------------!!
  !!------------------------------ traps -----------------------------------!!

      subroutine dist_opt_win_trap (keyword)
      character(len=*),intent(in) :: keyword
      if(object%opt_win.eq.'MUTE'.and.object%opt_in.eq.'TRACE')then
        call pc_put_sensitive_field_flag ('TIM_ADD',  .true.)
        call pc_put_sensitive_field_flag ('WIN_LEN',  .true.)
        call pc_put_sensitive_field_flag ('TIM_BEG',  .false.)
        call pc_put_sensitive_field_flag ('TIM_END',  .false.)
      else
        call pc_put_sensitive_field_flag ('TIM_ADD',  .false.)
        call pc_put_sensitive_field_flag ('WIN_LEN',  .false.)
        call pc_put_sensitive_field_flag ('TIM_BEG',  .true.)
        call pc_put_sensitive_field_flag ('TIM_END',  .true.)
      endif
      end subroutine dist_opt_win_trap

      subroutine dist_opt_in_trap (keyword)
      character(len=*),intent(in) :: keyword
      if(object%opt_in.eq.'TRACE')then
        call pc_put_sensitive_field_flag ('HDR_IN',   .false.)
        call pc_put_sensitive_field_flag ('OPT_WIN',  .true.)
        if(object%opt_win.eq.'MUTE')then
          call pc_put_sensitive_field_flag ('TIM_ADD',  .true.)
          call pc_put_sensitive_field_flag ('WIN_LEN',  .true.)
          call pc_put_sensitive_field_flag ('TIM_BEG',  .false.)
          call pc_put_sensitive_field_flag ('TIM_END',  .false.)
        else
          call pc_put_sensitive_field_flag ('TIM_ADD',  .false.)
          call pc_put_sensitive_field_flag ('WIN_LEN',  .false.)
          call pc_put_sensitive_field_flag ('TIM_BEG',  .true.)
          call pc_put_sensitive_field_flag ('TIM_END',  .true.)
        endif
      else
        call pc_put_sensitive_field_flag ('HDR_IN',   .true.)
        call pc_put_sensitive_field_flag ('OPT_WIN',  .false.)
        call pc_put_sensitive_field_flag ('TIM_BEG',  .false.)
        call pc_put_sensitive_field_flag ('TIM_END',  .false.)
        call pc_put_sensitive_field_flag ('TIM_ADD',  .false.)
        call pc_put_sensitive_field_flag ('WIN_LEN',  .false.)
      endif
      
      end subroutine dist_opt_in_trap

      subroutine dist_num_bins_trap (keyword)
      character(len=*),intent(in) :: keyword
      if(object%num_bins.le.1)then
        call pc_error('Number of bins should be greater than 1')
      endif
      end subroutine dist_num_bins_trap

      subroutine dist_hdr_in_trap(keyword)
      character(len=*),intent(in) :: keyword

      if(object%hdr_in.lt.0.or.object%hdr_in.gt.object%nwih)then
        call pc_error('HDR_IN must be between 0 and NWIH')
      endif
      end subroutine dist_hdr_in_trap

      subroutine dist_num_tr_trap (keyword)
      character(len=*),intent(in) :: keyword
      if(object%num_tr.le.0)then
        call pc_error('NUM_TR must be greater than zero')
      endif
      end subroutine dist_num_tr_trap

      subroutine dist_end_trap

      real :: trace_len

      Trace_Len = object%dt*(object%ndpt-1)
      Verify_Trace_Window_Option:   &
        if (object%Opt_Win == 'MUTE'.and.object%opt_in.eq.'TRACE') then
        !

          if (object%Tim_Add < 0.0) THEN
            call pc_error('TIM_ADD cannot be negative')

          else if (object%Tim_Add > Trace_Len - object%tstrt) THEN
            call pc_error (MSG1 = 'ADDTIME, ',   &
                           VAR1 = object%Tim_Add,   &
                         MSG2 = ', must be less than remaining trace period ', &
                           VAR2 = Trace_Len - object%tstrt)

          end if

          if (object%Win_Len <= 0.0) THEN
            call pc_error('WIN_LEN must be greater than zero')

          else if (object%Win_Len > Trace_Len - object%tstrt) THEN
            call pc_error (MSG1 = 'WIN_LEN, ',   &
                           VAR1 = object%Win_Len,   &
                           MSG2 = ',must be less than remaining trace period', &
                           VAR2 = Trace_Len - object%tstrt)

          end if

        else if (object%Opt_Win == 'FIXED'.and.object%opt_in.eq.'TRACE') then 

          if (object%Tim_Beg < object%tstrt) THEN
            call pc_error (MSG1 = 'TIM_BEG, ',   &
                           VAR1 = object%Tim_Beg,   &
                           MSG2 = ',  cannot be less than TSTRT, ',   &
                           VAR2 = object%tstrt)

          else if (object%Tim_Beg > Trace_Len) THEN
            call pc_error (MSG1 = 'TIM_BEG, ',   &
                         VAR1 = object%Tim_Beg,   &
                         MSG2 = ',  cannot exceed trace length (DT * NDPT), ', &
                         VAR2 = Trace_Len)

          end if

          if (object%Tim_End < object%Tim_Beg) THEN
            call pc_error (MSG1 = 'TIM_END, ',   &
                         VAR1 = object%Tim_End,   &
                         MSG2 = ', must be greater than, or equal to,'   &
                                // ' TIM_BEG, ',   &
                         VAR2 = object%Tim_Beg)

          else if (object%Tim_End > Trace_Len) THEN
            call pc_error (MSG1 = 'TIM_END, ',   &
                         VAR1 = object%Tim_End,   &
                         MSG2 = ', cannot exceed trace length (DT * NDPT), ',&
                         VAR2 = Trace_Len)

          end if

      end if Verify_Trace_Window_Option

    if (object%Num_Bins < object%Num_Tr) THEN
      call pc_error (MSG1 = 'Number of bins ',    &
                     VAR1 = object%Num_Bins,         &
                     MSG2 = ' must be greater than or equal '   &
                            // 'to requested traces ',   &
                     VAR2 = object%Num_Tr)
    endif

    if (MOD(object%Num_Bins,object%Num_Tr) /= 0) THEN
      call pc_error (MSG1 = 'Recheck number of Traces out entry.  NUM_TR, ', &
                       VAR1 = object%Num_Tr,   &
                       MSG2 = ', must evenly divide NUM_BINS, ',   &
                       VAR2 = object%Num_Bins)
    end if


    ! - Histogram description
    !
    if (object%Val_Max <= object%Val_Min) THEN
      call pc_error (MSG1 = 'VAL_MAX, ',   &
                     VAR1 = object%Val_Max,   &
                     MSG2 = ', must be greater than VAL_MIN, ',   &
                     VAR2 = object%Val_Min)
    end if


      end subroutine dist_end_trap

  !!-------------------------- main execution ------------------------------!!
  !!-------------------------- main execution ------------------------------!!
  !!-------------------------- main execution ------------------------------!!

!<execute_only>

  subroutine dist (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type(dist_struct), intent(inout) :: obj                  ! arguments
    integer,           intent(inout) :: ntr                  ! arguments
    double precision,  intent(in)    :: hd(:,:)              ! arguments
    real,              intent(in)    :: tr(:,:)              ! arguments
    !
    ! - Begin Dist
    !
    if (ntr == NO_MORE_TRACES) then
      !
      ! - Report on the histogram
      !
      if (obj%Vals_Binned_Count > 0) then
        call dist_Report_Histo (OBJ = obj)
      else
        !
        if (obj%opt_in.eq.'TRACE') then
          call pc_error (MSG1 = 'DIST: Distribution report requested, but '   &
                                // 'no data was accumulated from traces.')
        else
          call pc_error (MSG1 = 'DIST: Distribution report requested, but '   &
                                // 'no data was accumulated from header ',    &
                         VAR1 = obj%Hdr_In)
        end if
        !
      end if
      !
      call pc_print (MSG1 = 'DIST: ',                      &
                     VAR1 = obj%Vals_Binned_Count,         &
                     MSG2 = ' of ',                        &
                     VAR2 = obj%Vals_Binned_Count          &
                            + obj%Missed_Small_Samples     &
                            + obj%Missed_Large_Samples,    &
                     MSG3 = ' samples/traces were counted.')
      call pc_print (MSG1 = '   ',                                       &
                     VAR1 = obj%Missed_Small_Samples,                    &
                     MSG2 = ' Samples were too small to count, and ',    &
                     VAR2 = obj%Missed_Large_Samples,                    &
                     MSG3 = ' samples were too large.')
      !
      ! - We are finished
      !
      call dist_wrapup (obj)
      !
    else
      !
      ! - Accumulate input traces into the histogram
      !
      if (obj%opt_in.eq.'TRACE') then
        !
        call dist_Accumulate_Trace_Histo (OBJ = Obj,   &
                                          NTR = ntr,   &
                                          HD  = hd,   &
                                          TR  = tr)
        !
      else
        !
        call dist_Accumulate_Header_Histo (OBJ = Obj,   &
                                           NTR = ntr,   &
                                           HD  = hd)
        !
      end if
      !
      if (ntr == FATAL_ERROR) then
        call dist_wrapup (obj)
      end if
      !
    end if
    !
  end subroutine dist

  !!--------------------- dist_Accumulate_Trace_Histo ----------------------!!
  !!--------------------- dist_Accumulate_Trace_Histo ----------------------!!
  !!--------------------- dist_Accumulate_Trace_Histo ----------------------!!

  subroutine dist_Accumulate_Trace_Histo (obj, ntr, hd, tr)
    !
    ! - Arguments
    !
    type(dist_struct), intent(inout) :: obj                  ! arguments
    integer,           intent(in)    :: ntr                  ! arguments
    double precision,  intent(in)    :: hd(:,:)              ! arguments
    real,              intent(in)    :: tr(:,:)              ! arguments
    !
    ! - Local variables
    !
    integer            :: Bin
    integer            :: First_Trace_Pt
    integer            :: Last_Trace_Pt
    integer            :: Point
    integer            :: Tim_Add_Pts
    integer            :: Tim_Beg_Pts
    integer            :: Tim_End_Pts
    integer            :: Trace
    integer, parameter :: C1 = 1.0
    !
    double precision               :: Inv_Bin_Width
    double precision               :: Val
    !
    ! - Begin dist_Accumulate_Trace_Histo
    !
    Tim_Add_Pts = NINT (A = obj%Tim_Add / obj%dt) + 1
    Tim_Beg_Pts = INT ((obj%Tim_Beg - obj%tstrt) / obj%dt) + 1
    Tim_End_Pts = INT ((obj%Tim_End - obj%tstrt) / obj%dt) + 1
    Inv_Bin_Width = 1.0 / obj%Bin_Width
    !
    ! - Loop through the traces
    !
    Loop_Thru_Traces:   &
      do Trace = 1, ntr
      !
      ! - Check the header mask
      !
      if (obj%Opt_Mask_Headers) then
        if (hd (obj%Header_Mask_Pos, Trace) == 0.0) CYCLE Loop_Thru_Traces
      end if
      !
      ! - Find the first and last data points
      !
      if (obj%Opt_Win.eq.'MUTE') then                ! WIN_MODE = MUTE
        First_Trace_Pt = hd (HDR_TOP_MUTE, Trace) + Tim_Add_Pts
        Last_Trace_Pt  = MIN (A1 = INT(A = hd (HDR_BOTTOM_MUTE, Trace)),    &
                              A2 = First_Trace_Pt   &
                                   + INT (A = obj%Win_Len / obj%dt))

      else                                      ! WINMODE = FIXED
        First_Trace_Pt = MAX (A1 = Tim_Beg_Pts,    &
                              A2 = INT (hd (HDR_TOP_MUTE, Trace)))
        Last_Trace_Pt  = MIN (A1 = INT (hd (HDR_BOTTOM_MUTE, Trace)),   &
                              A2 = Tim_End_Pts)

      end if
      !
      if (Last_Trace_Pt - First_Trace_Pt < 1) CYCLE Loop_Thru_Traces
      !
      ! - Loop through the selected points for this trace
      !
      Loop_Thru_Samples:   &
        do Point = First_Trace_Pt, Last_Trace_Pt   ! Loop through trace samples
        !
        Val           = tr (Point, Trace)
        obj%Val_Count = obj%Val_Count + 1
        !
        if (obj%Opt_Ignore_Zeroes .AND. (Val == 0.0)) CYCLE Loop_Thru_Samples
        if (obj%Opt_Abs_Val_Flag) Val = ABS (A = Val)
        !
        ! - Calculate bin index
        !
!!Kruse        Bin = INT(A = (Val - obj%Histo_Rng_Lo) * Inv_Bin_Width) + C1

        if(obj%opt_bins.eq.'CENTER')then
          Bin=mth_bin_number(dble(obj%Val_Min),dble(obj%Bin_Width),Val)
        else
          Bin = NINT((Val - obj%Val_Min) * Inv_Bin_Width + obj%C3)
        endif
        !
        IF (Bin < 1) then
          !
          obj%Missed_Small_Samples = obj%Missed_Small_Samples + 1
          !
        else if (Bin > obj%Num_Bins) then
          !
          obj%Missed_Large_Samples = obj%Missed_Large_Samples + 1
          !
        else
          !
          obj%Histo (Bin)       = obj%Histo (Bin) + 1      ! and increment
          obj%Vals_Binned_Count = obj%Vals_Binned_Count + 1
          obj%Histo_Max_Val     = MAX (A1 = obj%Histo_Max_Val, A2 = Val)
          obj%Histo_Min_Val     = MIN (A1 = obj%Histo_Min_Val, A2 = Val)
          obj%Sum               = obj%Sum + Val
          obj%Sum_Sqr           = obj%Sum_Sqr + Val**2
          !
        end if
        !
      end do Loop_Thru_Samples
      !
    end do Loop_Thru_Traces
    !
  end subroutine dist_Accumulate_Trace_Histo


  !!--------------------- dist_Accumulate_Header_Histo ---------------------!!
  !!--------------------- dist_Accumulate_Header_Histo ---------------------!!
  !!--------------------- dist_Accumulate_Header_Histo ---------------------!!

  subroutine dist_Accumulate_Header_Histo (obj, ntr, hd)
    !
    ! - Arguments
    !
    type(dist_struct), intent(inout) :: obj                  ! arguments
    integer,           intent(in)    :: ntr                  ! arguments
    double precision,  intent(in)    :: hd(:,:)              ! arguments
    !
    ! - Local variables
    !
    integer            :: Bin
    integer            :: i
    integer, parameter :: C1 = 1.0
    !
    double precision               :: Inv_Bin_Width
    double precision               :: Val
    !
    ! - Begin dist_Accumulate_Header_Histo
    !
    Inv_Bin_Width = 1.0 / obj%Bin_Width

    Loop_Scan_Headers:   &
      do i = 1, ntr
      !
      ! - Check the header mask
      !
      if (obj%Opt_Mask_Headers) then
        if (HD(obj%Header_Mask_Pos, i) == 0.0) CYCLE Loop_Scan_Headers
      end if
      !
      ! - Accumulate the selected header word values
      !
      Val = DBLE (hd (obj%Hdr_In, i))
      obj%Val_Count = obj%Val_Count + 1
      !
      IF (obj%Opt_Ignore_Zeroes .AND. (Val == 0.0)) CYCLE
      IF (obj%Opt_Abs_Val_Flag)                 Val = ABS(Val)
      !
      ! - Calculate bin index
      !
!Kruse      Bin = INT ((Val - obj%Histo_Rng_Lo) * Inv_Bin_Width) + C1

      if(obj%opt_bins.eq.'CENTER')then
        Bin=mth_bin_number(dble(obj%Val_Min),dble(obj%Bin_Width),Val)
      else
        Bin = NINT((Val - obj%Val_Min) * Inv_Bin_Width + obj%C3)
      endif

      !
      IF (Bin < 1) then
        !
        obj%Missed_Small_Samples = obj%Missed_Small_Samples + 1
        !
      else if (Bin > obj%Num_Bins) then
        !
        obj%Missed_Large_Samples = obj%Missed_Large_Samples + 1
        !
      else
        !
        obj%Histo (Bin)       = obj%Histo (Bin) + 1      ! and increment
        obj%Vals_Binned_Count = obj%Vals_Binned_Count + 1
        obj%Histo_Max_Val     = MAX (A1 = obj%Histo_Max_Val, A2 = Val)
        obj%Histo_Min_Val     = MIN (A1 = obj%Histo_Min_Val, A2 = Val)
        obj%Sum               = obj%Sum + Val
        obj%Sum_Sqr           = obj%Sum_Sqr + Val**2
        !
      end if
      !

!      write(*,*) 'Val=',Val
!      write(*,*) 'Lo =',obj%Histo_Rng_Lo
!      write(*,*) 'IBW=',Inv_Bin_Width
!      write(*,*) 'C1 =',C1
!      write(*,*) 'Bin,Num_Bins=',Bin,',',obj%Num_Bins
!      write(*,*) 'Missed_Small = ',obj%Missed_Small_Samples
!      write(*,*) 'Missed_Large = ',obj%Missed_Large_Samples

    end do Loop_Scan_Headers
    !
  end subroutine dist_Accumulate_Header_Histo

  !!------------------------- dist_Report_Histo ---------------------------!!
  !!------------------------- dist_Report_Histo ---------------------------!!
  !!------------------------- dist_Report_Histo ---------------------------!!

  subroutine dist_Report_Histo (obj)
    !
    ! - Arguments
    !
    type (dist_struct), intent (inout) :: obj
    !
    ! - Output traces
    !
    double precision, dimension (Graph_Out_Hd_Len) :: Graph_Out_Hd
    real,             dimension (obj%nspt)         :: Graph_Out_Tr
    !
    ! - Local parameters
    !
    integer, parameter :: NUM_PERCS = 14    ! Number of Percentile Groups
    character (len =  7), parameter, dimension (NUM_PERCS)    &
      :: Percentile_Tbl_Label = (/ '10 %  ', '20 %  ', '30 %  ', '40 %  ',    &
                                   '50 %  ', '60 %  ', '70 %  ', '80 %  ',    &
                                   '90 %  ', '95 %  ', '98 %  ', '99 %  ',    &
                                   '99.5 %', '99.9 %' /)
    !
    ! - Local variables
    !

    !
    integer              :: Accum_Histo_Bins
    integer              :: Bin
    integer              :: First_Group_Bin

    integer              :: Group_Len
    integer              :: Grp_Graph_Trim
    integer              :: ierr
    integer              :: Last_Group_Bin

    integer              :: Perc
    integer              :: Perc_Grp_Top_Set
    integer              :: pixel
    integer              :: Prob_Dens_Func
    integer              :: Trace              ! Common dist1, NTRC
    integer, parameter   :: C2 = 1.0           ! SMCook changed from 0.5
    !
    double precision                 :: Bin_Center
    double precision                 :: Cum_Dist_Func
    double precision                 :: Histo_Mean
    double precision                 :: Median_Bin
    double precision                 :: rms
    double precision                 :: Std_Dev

    !
    double precision, parameter, dimension (NUM_PERCS) :: Percentile_Tbl   &   
      = (/0.100, 0.200, 0.300, 0.400, 0.500, 0.600, 0.700, 0.800, 0.900,   &
          0.950, 0.980, 0.990, 0.995, 0.999/)
    double precision, dimension(NUM_PERCS) :: Accum_Percentile_Tbl    ! Was PCTL
    !
    type (trcio_struct), pointer :: file
    !
    ! - Begin dist_Report_Histo
    !
    ! - Calculate percentiles here
    !
    Perc = 1
    Accum_Histo_Bins = 0
    Perc_Grp_Top_Set = INT(A = Percentile_Tbl (1)   &
                               *           obj%Vals_Binned_Count)
 !                             * DBLE (A = obj%Vals_Binned_Count))
    Accum_Percentile_Tbl = 0.0
    !
    ! - Summarize the histogram into percentile groups
    !
    Loop_Histo_Bins:   &
      do Bin = 1, obj%Num_Bins
      !
      Accum_Histo_Bins = Accum_Histo_Bins + obj%Histo (Bin)
      !
      do while (Accum_Histo_Bins >= Perc_Grp_Top_Set)
        !
!Kruse        Accum_Percentile_Tbl (Perc) = obj%Histo_Center (Bin)

        if(obj%opt_bins.eq.'CENTER')then
          Accum_Percentile_Tbl(Perc)=mth_bin_center(dble(obj%Val_Min),&
                                                    dble(obj%Bin_Width),Bin)
        else
          Accum_Percentile_Tbl (Perc) = obj%Val_Min  &
                                      + (Bin - obj%C3) * dble(obj%Bin_Width)
        endif



!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        !
        Perc = Perc + 1
        !
        if (Perc > NUM_PERCS) EXIT Loop_Histo_Bins
        !
        Perc_Grp_Top_Set = INT (A = Percentile_Tbl (Perc)   &
  !                                 * DBLE (A = obj%Vals_Binned_Count))
                                    *           obj%Vals_Binned_Count)
        !
      end do
      !
    end do Loop_Histo_Bins
    !
    Median_Bin = Accum_Percentile_Tbl (5)
    Histo_Mean = obj%Sum / obj%Vals_Binned_Count
    Std_Dev    = sqrt (X = obj%Sum_Sqr   &
                           / obj%Vals_Binned_Count    &
                           - Histo_Mean**2)
    rms = sqrt (obj%Sum_Sqr / obj%Vals_Binned_Count)
    !
    write (Lun, *) 'Normal end of DIST process.'
    if (obj%opt_in.eq.'TRACE') then
      write (Lun, *) 'Trace Data examined'
    else
      write (Lun, *) 'Header word examined            = ', obj%Hdr_In
    end if
    write (Lun, *) 'Minimum value binned            = ', obj%Histo_Min_Val
    write (Lun, *) 'Maximum value binned            = ', obj%Histo_Max_Val
    write (Lun, *) 'Mean of binned values           = ', Histo_Mean
    write (Lun, *) 'Std. Dev. of binned values      = ', Std_Dev
    write (Lun, *) 'RMS       of binned values      = ', rms
    write (Lun, *) 'Total number of values binned   = ', obj%Vals_Binned_Count
    write (Lun, *) 'Total number of values examined = ', obj%Val_Count
    write (Lun, *) '      Percentiles'
    !
    do Bin = 1, NUM_PERCS
      write (Lun, 171) Percentile_Tbl_Label (Bin), Accum_Percentile_Tbl(Bin)
    end do
    !
    write (Lun, *) ' '
    171 FORMAT(4X,A7,F32.14)
!!!    171 FORMAT(4X,A7,F16.7)
    172 FORMAT(1X,F9.0,2X,F32.14,2X,F32.14)
!!!    172 FORMAT(1X,F9.0,2X,F16.7,2X,F16.7)
    !
    ! - list the output graph to the report file
    !
    !
    if (obj%write_file_flag) then
      !
      file => trcio_open (FILENAME = trim (obj%path_trot),    &
                          IO_MODE  = 'w+',                &
                          SCRATCH  = .false.,             &
                          nwih     = Graph_Out_Hd_Len,    &
                          ndpt     = obj%nspt,            &
                          nbits    = 32,                  &
                          nbitshd  = 64)
      !
    else
      !
      select case (obj%Type_Output)
        case (1)
          write (Lun, *) ' TRACE #                      BIN CENTER             &
                         &             PDF'
          write (Lun, *) '---------                  ----------------         &
                         &         ----------------'
        case (2)
          write (Lun, *) ' TRACE #                      BIN CENTER             &
                         &             CDF'
          write (Lun, *) '---------                  ----------------         &
                         &         ----------------'
        case (3)
          write (Lun, *) ' TRACE #                      BIN CENTER             &
                         &            (1 - PDF)'
          write (Lun, *) '---------                  ----------------         &
                         &         ----------------'
        case (4)
          write (Lun, *) ' TRACE #                      BIN CENTER             &
                         &            (1 - CDF)'
          write (Lun, *) '---------                  ----------------         &
                         &         ----------------'
      end select
      !
    end if
    !
    Group_Len     = obj%Num_Bins / obj%Num_Tr
    Cum_Dist_Func = 0

!!    Loop_Thru_Out_Traces:   &
      DO Trace = 1, obj%Num_Tr
      !
      Prob_Dens_Func  = 0                        ! Fill output trace headers
      First_Group_Bin = (Trace - 1) * Group_Len + 1
      Last_Group_Bin  = First_Group_Bin + Group_Len - 1
      !
      do Bin = First_Group_Bin, Last_Group_Bin
        Prob_Dens_Func = Prob_Dens_Func + obj%Histo (Bin)
      end do
      !
      Graph_Out_Hd(1)  = DBLE (Trace)
!          trcio resets word 2
      graph_out_hd(2)  = 1.0
  !   Graph_Out_Hd(3)  = DBLE (Prob_Dens_Func) / DBLE (obj%Vals_Binned_Count)
      Graph_Out_Hd(3)  = DBLE (Prob_Dens_Func) /       obj%Vals_Binned_Count
                          
      Cum_Dist_Func    = Cum_Dist_Func + Prob_Dens_Func
      Graph_Out_Hd(4)  = DBLE (A = Cum_Dist_Func)   &
  !                                / DBLE (A = obj%Vals_Binned_Count)
                                   /           obj%Vals_Binned_Count
      Graph_Out_Hd(5)  = 1.0 - Graph_Out_Hd (3)
      Graph_Out_Hd(6)  = 1.0 - Graph_Out_Hd (4)
      Graph_Out_Hd(7)  = DBLE (obj%Type_Output)

      Bin_Center       = obj%Histo_Rng_Lo     &
                          + dble(obj%Bin_Width) * (Trace - 0.5)

!!      Graph_Out_Hd(7)  = Bin_Center   &
!!                          + (DBLE (A = Group_Len - 1) / 2.0) * &
!!                             dble(obj%Bin_Width)
      Graph_Out_Hd(8)  =  obj%Val_Min &
                     + (1 + Group_Len * (Trace - 1) - obj%C3) * &
                       dble(obj%Bin_Width) &
                     +  ((Group_Len - 1) / 2.0) * dble(obj%Bin_Width)

      Graph_Out_Hd(9)  = dble(obj%Bin_Width) * DBLE (A = Group_Len)
      Graph_Out_Hd(10)  = Median_Bin
      Graph_Out_Hd(11) = DBLE (A = Prob_Dens_Func)
      Graph_Out_Hd(12) = DBLE (A = Cum_Dist_Func)
  !   Graph_Out_Hd(13) = DBLE (A = obj%Vals_Binned_Count)
      Graph_Out_Hd(13) =           obj%Vals_Binned_Count
      Graph_Out_Hd(14) = Graph_Out_Hd(obj%Type_Output+2) * DBLE (obj%nspt)
                          
      !
      if (obj%write_file_flag) then
        !
        ! - Fill output trace samples
        !
        Grp_Graph_Trim = obj%nspt - NINT(Graph_Out_Hd(14))
        !
        ! - Clear the top of the trace
        !
        do pixel = 1, Grp_Graph_Trim - 1
           Graph_Out_Tr (pixel) = 0.0
        end do
        !
        ! - Set the bottom of the trace
        !
        do pixel = Grp_Graph_Trim, obj%nspt
           Graph_Out_Tr (pixel) = 1.0
        end do
        ierr = trcio_write_trace (FILE = file,   &
                                  HD   = Graph_Out_Hd,   &
                                  TR   = Graph_Out_Tr)
        !
      else
        !
        write (Lun, 172) Graph_Out_Hd(1), Graph_Out_Hd (8),   &
                         Graph_Out_Hd (obj%Type_Output+2)
        !
      end if
      !
    end do !Loop_Thru_Out_Traces
    !
    if (obj%write_file_flag) THEN
      !
      ierr = trcio_close (FILE = file)
      !
    end if
    !
    write (Lun, *) ' '
    !
  end subroutine dist_Report_Histo

!</execute_only>


  !!------------------------------ wrapup ----------------------------------!!
  !!------------------------------ wrapup ----------------------------------!!
  !!------------------------------ wrapup ----------------------------------!!


!<execute_only>

  subroutine dist_wrapup (obj)
    !
    type(dist_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin dist_wrapup
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
  end subroutine dist_wrapup

!</execute_only>

  !!--------------------------- dist_dump_object ---------------------------!!
  !!--------------------------- dist_dump_object ---------------------------!!
  !!--------------------------- dist_dump_object ---------------------------!!


!<execute_only>

  subroutine dist_dump_object (obj)
    !
    type(dist_struct), intent (in) :: obj       ! arguments
    !
    ! - Local variables
    !

    !
    ! - Begin dist_dump_object
    !
    ! - List the contents of the dist object structure
    !
    write(Lun,*) ""
    write(Lun,*) " - The dist object structure"
    write(Lun,*) ""
    write(Lun,*) "skip_wrapup ", obj%skip_wrapup
    write(Lun,*) ""
    write(Lun,*) " - Common dist1"
    write(Lun,*) ""
    write(Lun,*) "Ls_Tr_Out ", obj%Ls_Tr_Out
    write(Lun,*) "Val_Count ", obj%Val_Count
    write(Lun,*) "Vals_Binned_Count ", obj%Vals_Binned_Count
    write(Lun,*) "Header_Mask_Pos ", obj%Header_Mask_Pos
    write(Lun,*) ""
    write(Lun,*) "Opt_Abs_Val_Flag ", obj%Opt_Abs_Val_Flag
    write(Lun,*) "Opt_Ignore_Zeroes ", obj%Opt_Ignore_Zeroes
    write(Lun,*) "Opt_Mask_Headers ", obj%Opt_Mask_Headers
    write(Lun,*) ""
    write(Lun,*) "Bin_Width ", obj%Bin_Width
    write(Lun,*) "Histo_Rng_Lo ", obj%Histo_Rng_Lo
    write(Lun,*) "Histo_Rng_Hi ", obj%Histo_Rng_Hi
    write(Lun,*) "Sum ", obj%Sum
    write(Lun,*) "Sum_Sqr ", obj%Sum_Sqr
    write(Lun,*) "Histo_Max_Val ", obj%Histo_Max_Val
    write(Lun,*) "Histo_Min_Val ", obj%Histo_Min_Val
    write(Lun,*) ""
    write(Lun,*) " - Common globals"
    write(Lun,*) ""
    write(Lun,*) "nwih ", obj%nwih
    write(Lun,*) "ndpt ", obj%ndpt
    write(Lun,*) "dt ", obj%dt
    write(Lun,*) "tstrt ", obj%tstrt
    write(Lun,*) ""
    write(Lun,*) " - Parameter-Cache User Parameters"
    write(Lun,*) "   These parameters are described in the 'Help' section above"
    write(Lun,*) ""
    write(Lun,*) "Hdr_Flag ", obj%Hdr_Flag
    write(Lun,*) "Opt_In ", obj%Opt_In
    write(Lun,*) "Hdr_In ", obj%Hdr_In
    write(Lun,*) "Opt_Win ", obj%Opt_Win
    write(Lun,*) "Tim_Beg ", obj%Tim_Beg
    write(Lun,*) "Tim_End ", obj%Tim_End
    write(Lun,*) "Tim_Add ", obj%Tim_Add
    write(Lun,*) "Win_Len ", obj%Win_Len
    write(Lun,*) "Opt_Bins ", obj%Opt_Bins
    write(Lun,*) "Val_Min ", obj%Val_Min
    write(Lun,*) "Val_Max ", obj%Val_Max
    write(Lun,*) "Num_Bins ", obj%Num_Bins
    write(Lun,*) "Num_Tr ", obj%Num_Tr
    write(Lun,*) "Nspt ", obj%nspt
    write(Lun,*) "Type_Output ", obj%Type_Output
    write(Lun,*) "path_trot ", trim (obj%path_trot)
    write(Lun,*) ""
    write(Lun,*) " - Internal Histogram"
    write(Lun,*) ""
    !
    if (associated (obj%Histo)) then
      !
      write(Lun,*) "Histo size  ", size (obj%Histo)
      write(Lun,*) "  Histo Shape ", shape (obj%Histo)
      write(Lun,*) "  Histo Min   ", minval (obj%Histo)
      write(Lun,*) "  Histo Max   ", maxval (obj%Histo)
      write(Lun,*) "  Histo Sum   ", Sum (obj%Histo)
      write(Lun,*) "  Histo Count ", count (obj%Histo > 0)
      !
    else
      !
      write(Lun,*) "Histo: Not allocated"
      !
    end if
    !
  end subroutine dist_dump_object

!</execute_only>

  !!---------------------------- end of module -----------------------------!!
  !!---------------------------- end of module -----------------------------!!
  !!---------------------------- end of module -----------------------------!!

end module dist_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

