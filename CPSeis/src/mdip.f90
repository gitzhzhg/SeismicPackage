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

!!------------------------------- mdip.f90 ---------------------------------!!
!!------------------------------- mdip.f90 ---------------------------------!!
!!------------------------------- mdip.f90 ---------------------------------!!

!<brief_doc>
!------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : MDIP
! Category   : filters
! Written    : 1990-08-07   by: Bill Harlan
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Dip filtering by modeling dips in tau-P space.
! Portability: No known limitations.
! Parallel   : Yes for gather input
!
!------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! MDIP is a tau-P dip filter that operates by transforming input data into the
! tau-P domain, then multiplying modeled dip ranges by the scale factor SCALE
! prior to being added back to the original data.
!
!       1.  Dips are modeled by a roll-along tau-P transform using a window of
!           NUM_WIN traces for each transform, with an increment of NUM_INC
!           traces between windows.  All specified dips are used first to model
!           the data as well as possible in the least squares sense.  Only data
!           with times between TIM_BEG and TIM_END are modeled by the
!           transform.
!
!       2.  Specified dip ranges are identified as pass (SCALE=0.0), accentuate
!           (SCALE>0.0) or reject (SCALE=-1.0).
!
!               SCALE = -1.0 causes that dip range to be subtracted from the
!               original data,
!
!               SCALE > 0.0 causes that dip range to be accentuated,
!
!               SCALE = 0.0 prevents contamination by aliased energy from other
!               dips.
!
!           Dips that you wish to preserve in the data should be entered with
!           SCALE = 0.0.  This range of dips will be modeled separately and
!           subtracted from the model before the other dips are modeled.  This
!           prevents aliased energy from other dips from affecting the dip
!           range you wish to preserve.
!
!       3.  Dip ranges are multiplied by the associated SCALE value, then
!           inverse transformed and added to the original data.
!
!
! NUM_DIPS
!
! To insure that the number of dips to model for a dip range (NUM_DIPS)
! does not cause aliasing, set
!
!          NUM_DIPS >= HOR_INC*FMAX*(DIP_MAX-DIP_MIN)*NUM_WIN + 1,
!
! where HOR_INC is the sample interval between traces in units of header word
! HDR_HOR and FMAX is the maximum frequency to be modeled without aliasing.
! If HOR_INC is set to the expected lateral sample interval, and you enter
! NUM_DIPS = 0 (or leave it blank), the front-end will calculate for you the
! minimum value of NUM_DIPS to avoid aliasing (using FMAX = 0.4 * Nyquist).
!
!
! NUM_INC
!
! The NUM_INC parameter plays a dual role. It is the window increment for
! tau-P transforms. It is also the number of traces extracted from the center
! of each window which are passed as output after processing that window.
! Note: For the first and last window of each gather defined by header word
! HDR_GATH, the number of output traces is greater than NUM_INC, to insure
! that MDIP always outputs the same number of traces as are input to it.
!
!
! Avoiding Mixed Appearance
!
! When large ranges of dips are modeled and subtracted from the data, the
! output will have a mixed appearance.  If you wish to suppress some steeply
! dipping coherent noise, it is best to measure the dip and specify as narrow
! a dip range as possible.  If you must use a large dip range and wish to
! minimize the mixed appearance, the CLIP parameter may be helpful.
!
! When CLIP is non-zero, MDIP will attenuate components of the dip model
! with low amplitudes in the tau-P domain. (Roughly speaking, regions of
! the tau-P transform of the dip model with amplitudes smaller than
! CLIP * [standard deviation] are removed from the dip model.) It is assumed
! that these low amplitude portions of the tau-P transform of the dip model
! correspond to noise in your original data. Since this noise is removed
! from the dip model, it is *not* removed from your data when the modeled
! dips are subtracted from your original data (for dip ranges where you've
! set SCALE = -1). Preserving this random noise avoids the mixed appearance.
!
! This works because coherent events focus into isolated strong amplitudes in
! transform space while incoherent noise diffuses into weak amplitudes over
! all dips.  (Same effect as in FKAP.)
!
! Increasing the value of CLIP will increase the amount of noise which is
! removed from the dip model and is therefore *not* removed from your data.
!
!
! Input Data and Header Words
!
! Input data should be sorted in order of the desired functional gathers, such
! as CMPs or shot profiles. Input can be gathers or single traces.
!
! HDR_GATH should be set so that HDR_GATH changes between gathers but is
! constant for a particular gather.  Normally this will be header word 3
! pre-stack and header word 7 or 8 post-stack.  Set HDR_GATH to zero if only
! one set of traces is being dip filtered.
!
! HDR_HOR is the header word that designates individual traces within the set
! of traces that are being dip filtered.  Normally HDR_HOR is set to 8 or 7 for
! post-stack and either 1, 4 or 6 for prestack.  HOR_INC is the expected
! increment in HDR_HOR.  (Dip units are simplified if HDR_HOR is chosen so that
! HOR_INC = 1.)
!
!
! Cost
!
! Cost for MDIP is roughly proportional to the sum of the NUM_DIP values for
! all the dip ranges and also proportional to the number of tau-P transforms.
! Thus cost can be minimized by using the smallest values of NUM_DIP that do
! not produce noticeable aliasing and by making NUM_INC no smaller than
! necessary to achieve the desired dip filtering effect.
!
! F-K filtering will be less expensive than MDIP but may produce undesirable
! artifacts, especially at edges.
!
!------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
!
!------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs one trace at a time.
!
!------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name    Description                       Action taken
! ----    -----------                       ------------
! NWIH    Number of words in trace header   Used but not changed
! NDPT    Number of sample values in trace  Used but not changed
! TSTRT   Starting time on trace            Used but not changed
! DT      Trace sample interval             Used but not changed
! NUMTR   Number of traces passed at once   Checked (must = 1) but not changed
!
!------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#       Description                          Action taken
! ----       -----------                          ------------
! 2          Head mute index                      used but not changed
! 25         Largest absolute value               used and changed
! 64         Tail mute index                      used but not changed
! HDR_GATH   Header labeling gathers or groups    used but not changed
! HDR_HOR    Header labeling individual traces    used but not changed
!
!------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 16. 2006-09-11  Stoeckley    Add pc_register_array_names for SeisSpace.
! 15. 2002-06-10  CC Burch     Modify to support gather input and parallelize
! 14. 2001-01-15  Bob Baumel   Raise NUM_DIPS warning threshold from 51 to 100.
! 13. 2000-12-07  Bob Baumel   Change wrapped_up flag to skip_wrapup.
! 12. 2000-10-13  Bob Baumel   Get process working fully.
! 11. 2000-03-30  Brad Kruse   Converted from old system.
! 10. 1998-11-20  Vunderink    Begin using the f90 compiler.
! 9.  1994-06-29  Harlan       Ignore bottom mute of 0, uninitialized.
! 8.  1994-04-29  Harlan       Add bottom mutes.
! 7.  1992-03-24  Harlan       Add DXINT to calculate defaults for NDIPS.
! 6.  1992-03-10  Harlan       Modify documentation, resort subroutines
! 5.  1990-11-30  Harlan       Check that data are not gathered.
! 4.  1990-11-08  Harlan       Re-zero mute zone when finished.
! 3.  1990-10-03  Harlan       Add NHG for prestack groups.
! 2.  1990-08-30  Harlan       1st, model, subtract dips with 0 scale factors.
! 1.  1990-08-07  Harlan       Original version.
!
!------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH       varies    amount of temporary memory needed.
! NSTORE         varies    amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! This process uses one set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR == 1              means to process the input trace.
!    NTR == NO_MORE_TRACES means there are no more input traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR == 1              if this process is outputting a trace.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!    NTR == NEED_TRACES    if this process needs more traces.
!
!------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!------------------------------------------------------------------------------
!<gui_def>
!<NS MDIP Process/NC=80>
!                                 MDIP Process
!                 Dip filtering by modeling dips in tau-P space.
!
!  `-----------------------------------------------------------------------
!    HDR_GATH=~~`IIIIII          NUM_WIN=`IIIIII          NUM_INC=`IIIIII
!
!    HDR_HOR=~~~`IIIIII          HOR_INC=`FFFFFFFFF
!
!    TIM_BEG=~~~`FFFFFFFFF       TIM_END=`FFFFFFFFF
!
!    SCALE_DATA=`FFFFFFFFF       CLIP=~~~`FFFFFFFFF
!  `-----------------------------------------------------------------------
!
!                          Dip Ranges to Model
!      (You may set NUM_DIPS = 0 or blank for unaliased suggestions)
!
!            DIP_MIN    DIP_MAX    SCALE     NUM_DIPS
!            `FFFFFFFFFF`FFFFFFFFFF`FFFFFFFFF`IIIIIIIII
!            `FFFFFFFFFF`FFFFFFFFFF`FFFFFFFFF`IIIIIIIII
!            `FFFFFFFFFF`FFFFFFFFFF`FFFFFFFFF`IIIIIIIII
!            `FFFFFFFFFF`FFFFFFFFFF`FFFFFFFFF`IIIIIIIII
!            `FFFFFFFFFF`FFFFFFFFFF`FFFFFFFFF`IIIIIIIII
!            `FFFFFFFFFF`FFFFFFFFFF`FFFFFFFFF`IIIIIIIII
!
!<PARMS DIP_MIN_ARRAYSET[/XST/YST]>
!</gui_def>
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_GATH">
!<Tip> Header word designating trace gathers or groups to be filtered. </Tip>
! Default = 3
! Allowed = 0 - NWIH
! Transitions in header word HDR_GATH denote boundaries between gathers (or
! groups) of traces. Each such group will be dip filtered separately; in
! particular, the roll-along windows defined by NUM_WIN and NUM_INC will NOT
! cross these boundaries.
!
! As typical choices, HDR_GATH may be set to 8 for post-stack and 3 or 7
! for prestack data.
!
! You may set HDR_GATH = 0 to indicate that your whole data set should be
! regarded as a single group. In this case input must be as single traces.
!</Help>
!
!<Help KEYWORD="NUM_WIN">
!<Tip> Number of traces in a window for tau-P transforms. </Tip>
! Default = 21
! Allowed = int > 2
! If NUM_WIN is less than about 10 the transform may not be able to distinguish
! dips properly, but if it is too large, events may not appear linear.
! NUM_WIN = 21  seems to work well.
!
! The trace windows defined by NUM_WIN will not cross the group boundaries
! defined by the HDR_GATH parameter.
!</Help>
!
!<Help KEYWORD="NUM_INC">
!<Tip> Increment (in number of traces) from one window to the next. </Tip>
! Default = 7
! Allowed = NUM_WIN > int >= 1
! (Also, NUM_WIN - NUM_INC must be an EVEN number)
!
! NUM_INC plays a dual role: It's the number of traces by which the roll-along
! window is advanced. It's also the number of traces output from the MIDDLE of
! each window after dip modeling (except that more traces must be output from
! the beginning of the first window and end of the last window in a group).
!
! NUM_INC must be less than NUM_WIN, thus forcing overlap between windows. To
! minimize artifacts at window transitions, NUM_INC should ideally be as small
! as possible; however, a smaller value of NUM_INC causes greater execution
! time because dip modeling must be performed individually for each window.
! In practice, setting NUM_INC to about one-third of NUM_WIN works well.
!
! To insure that the subset of NUM_INC traces which are output from the middle
! of each window can be CENTERED within that window, the difference
! (NUM_WIN - NUM_INC) must be an EVEN number.
!</Help>
!
!<Help KEYWORD="HDR_HOR">
!<Tip> Header word designating traces within the set being filtered. </Tip>
! Default = 4
! Allowed = 1 - NWIH
! HDR_HOR is the header word that designates individual traces within the set
! of traces that are being dip filtered. As typical choices, HDR_HOR may be
! set to 7 for post-stack and 1, 4 or 6 for prestack data.
!
! Dips (specified by DIP_MIN and DIP_MAX arrays) are measured in seconds per
! unit of header word HDR_HOR. Thus, if header HDR_HOR contains a distance in
! meters, dips are in seconds per meter. If HDR_HOR increments by 1 between
! successive traces, dips are in seconds per trace.
!</Help>
!
!<Help KEYWORD="HOR_INC">
!<Tip> Expected increment in HDR_HOR between successive traces. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! HOR_INC is the expected increment in header word HDR_HOR between successive
! traces within a group of traces being dip filtered. Your estimated HOR_INC
! is used to suggest unaliased NUM_DIPS values if you enter zero (or blank)
! when filling in the NUM_DIPS array.
!
! The default (HOR_INC = 1) is appropriate when header HDR_HOR is a counter or
! grid index which increments by 1 between successive traces, in which case
! dips are measured in seconds per trace.
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Minimum time in seconds to model for dip filtering. </Tip>
! Default = TSTRT
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Maximum time in seconds to model for dip filtering. </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG
! If you leave TIM_END blank, it will be interpreted as end of trace.
!</Help>
!
!<Help KEYWORD="SCALE_DATA">
!<Tip> Scale factor to multiply data prior to addition of modeled dips. </Tip>
! Default = 1.0
! Allowed = real
! You may specify any real value for SCALE_DATA but, in practice, the only
! useful choices are 1.0 and 0.0. For normal MDIP operation, keep the default
! value (1.0) to avoid altering the data before adding or subtracting the
! modeled dips.
!
! Set SCALE_DATA = 0.0 if you wish to suppress the original data and display
! the modeled dips only. (In this case, you might also wish to use +1 instead
! of -1 in your SCALE array, to avoid reversing polarity of the modeled dips.)
!</Help>
!
!<Help KEYWORD="CLIP">
!<Tip> Amount of noise, in standard deviations, to retain in data. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! The CLIP parameter allows reduction of the mixed appearance of the filtered
! data by preventing noise in the original data from being modeled and
! subtracted. Modeled amplitudes less than CLIP standard deviations (as
! measured in the tau-P transform domain for the modeled dips) are interpreted
! as noise and removed from the dip model; therefore, this noise is *not*
! removed from your data when the modeled dips are subtracted from the
! original data (for dip ranges where SCALE = -1).
!
! If CLIP = 0.0, no attempt is made to preserve noise in the data. Larger
! values of CLIP will remove more noise from the modeled dips, thereby
! preserving more noise in the data when those modeled dips are subtracted.
!
! Suggestion: Try CLIP = 0 first. If too much mixing occurs, try CLIP = 1 or
! CLIP = 2.
!
! Alternatively, see if you can reduce the sizes of the dip ranges specified
! in your DIP_MIN and DIP_MAX arrays by more precise measurement of the dips
! you want to remove. Mixed appearance of MDIP output is primarily a problem
! when large dip ranges are modeled.
!</Help>
!
!<Help KEYWORD="DIP_MIN">
!<Tip> Minimum dip for a particular range. </Tip>
! Default = -
! Allowed = real (linked array)
! Units are seconds per unit of header word HDR_HOR (e.g., seconds per meter
! if HDR_HOR contains distance in meters). If HDR_HOR contains a sequential
! counter, dip units are seconds per trace.
!</Help>
!
!<Help KEYWORD="DIP_MAX">
!<Tip> Maximum dip for a particular range. </Tip>
! Default = -
! Allowed = real (linked array)
! Units are seconds per unit of header word HDR_HOR (e.g., seconds per meter
! if HDR_HOR contains distance in meters). If HDR_HOR contains a sequential
! counter, dip units are seconds per trace.
!</Help>
!
!<Help KEYWORD="SCALE">
!<Tip> Scale factor multiplying modeled dips prior to adding to data. </Tip>
! Default = -
! Allowed = real (linked array)
! SCALE is the scale factor that multiplies the modeled dip range prior to
! adding that modeled dip range back to the original data.
!
! Set SCALE = -1 for dip ranges that you wish to subtract from the data.
!
! Set SCALE = 0 for dip ranges that you wish to preserve (This prevents
! contamination by aliased energy from other dips).
!
! Set SCALE > 0 for dip ranges that you wish to accentuate.
!</Help>
!
!<Help KEYWORD="NUM_DIPS">
!<Tip> Number of dips to model in this range. </Tip>
! Default = -
! Allowed = int (linked array)
! If you enter NUM_DIPS = 0, or leave it blank, the front end will suggest
! a calculated value intended to avoid aliasing (This value is calculated
! when you leave the array-set of dip ranges on the CFE screen).
!</Help>
!
!</HelpSection>
!
!------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

  module mdip_module
    use rollalong_module
    use pc_module
    use named_constants_module
    use mem_module
    use timecheck_module
    use fltr_module
    use lav_module
    use mutehw_module

    implicit none

    private
    public :: mdip_create
    public :: mdip_initialize
    public :: mdip_update
    public :: mdip_delete
!<execute_only>
    public :: mdip
    public :: mdip_wrapup
!</execute_only>

    character(len=100),public,save :: MDIP_IDENT = &
'$Id: mdip.f90,v 1.16 2006/09/11 13:15:47 Stoeckley prod sps $'

!!---------------------- parameter structure -----------------------------!!
!!---------------------- parameter structure -----------------------------!!
!!---------------------- parameter structure -----------------------------!!

  type, public :: mdip_struct

    private
    logical                        :: skip_wrapup     ! wrapup flag.

    integer                        :: hdr_gath        ! process parameters.
    integer                        :: num_win         ! process parameters.
    integer                        :: num_inc         ! process parameters.
    integer                        :: hdr_hor         ! process parameters.
    real                           :: hor_inc         ! process parameters.
    real                           :: tim_beg         ! process parameters.
    real                           :: tim_end         ! process parameters.
    real                           :: scale_data      ! process parameters.
    real                           :: clip            ! process parameters.
    integer                        :: num_ranges      ! arrayset counter.
    real                  ,pointer :: dip_min(:)      ! process parameters.
    real                  ,pointer :: dip_max(:)      ! process parameters.
    real                  ,pointer :: scale(:)        ! process parameters.
    integer               ,pointer :: num_dips(:)     ! process parameters.

    integer                        :: nwih, ndpt      ! globals.
    real                           :: dt, tstrt       ! globals.
    integer                        :: numtr           ! globals.

    real                  ,pointer :: pvals_zero(:)   ! dependent variables.
    real                  ,pointer :: pvals_all(:)    ! dependent variables.
    real                  ,pointer :: scales_all(:)   ! dependent variables.
    integer                        :: len_smooth      ! dependent variables.
    integer                        :: ishift_smooth   ! dependent variables.
    real                  ,pointer :: gauss_smooth(:) ! dependent variables.
    integer                        :: itim_beg        ! dependent variables.
    integer                        :: itim_end        ! dependent variables.
    integer                        :: nall, nzero     ! dependent variables.
    integer                        :: nter            ! dependent variables.
    real                           :: fmax            ! dependent variables.
    type(rollalong_struct),pointer :: rollalong       ! dependent variables.
    double precision      ,pointer :: hd_buf1(:,:)    ! dependent variables.
    real                  ,pointer :: tr_buf1(:,:)    ! dependent variables.
    double precision      ,pointer :: hd_buf2(:,:)    ! dependent variables.
    real                  ,pointer :: tr_buf2(:,:)    ! dependent variables.

  end type mdip_struct

!!------------------------------- data -----------------------------------!!
!!------------------------------- data -----------------------------------!!
!!------------------------------- data -----------------------------------!!

  type(mdip_struct), pointer, save :: object      ! needed for traps.

  integer, parameter :: num_conj_grad = 4

  contains

!!--------------------------- create -------------------------------------!!
!!--------------------------- create -------------------------------------!!
!!--------------------------- create -------------------------------------!!

  subroutine mdip_create (obj)

    implicit none
    type(mdip_struct),pointer :: obj         ! arguments

    allocate (obj)

    nullify (obj%dip_min)
    nullify (obj%dip_max)
    nullify (obj%scale)
    nullify (obj%num_dips)
    nullify (obj%pvals_zero)
    nullify (obj%pvals_all)
    nullify (obj%scales_all)
    nullify (obj%gauss_smooth)
    nullify (obj%rollalong)
    nullify (obj%hd_buf1)
    nullify (obj%tr_buf1)
    nullify (obj%hd_buf2)
    nullify (obj%tr_buf2)

    call mdip_initialize (obj)
    return
  end subroutine mdip_create

!!----------------------------- delete -----------------------------------!!
!!----------------------------- delete -----------------------------------!!
!!----------------------------- delete -----------------------------------!!

  subroutine mdip_delete (obj)
    implicit none
    type(mdip_struct),pointer :: obj       ! arguments

    call mem_free (obj%dip_min)
    call mem_free (obj%dip_max)
    call mem_free (obj%scale)
    call mem_free (obj%num_dips)
    call mem_free (obj%pvals_zero)
    call mem_free (obj%pvals_all)
    call mem_free (obj%scales_all)
    call mem_free (obj%gauss_smooth)
    call mem_free (obj%hd_buf1)
    call mem_free (obj%tr_buf1)
    call mem_free (obj%hd_buf2)
    call mem_free (obj%tr_buf2)
    if (associated(obj%rollalong)) call rollalong_delete (obj%rollalong)

!<execute_only>
    call mdip_wrapup (obj)
!</execute_only>

    deallocate(obj)
    return
  end subroutine mdip_delete

!!------------------------------ initialize ------------------------------!!
!!------------------------------ initialize ------------------------------!!
!!------------------------------ initialize ------------------------------!!

  subroutine mdip_initialize (obj)
    implicit none
    type(mdip_struct),intent(inout) :: obj

    call pc_get_global ('TSTRT', obj%tstrt)

    obj%hdr_gath        = 3
    obj%num_win         = 21
    obj%num_inc         = 7
    obj%hdr_hor         = 4
    obj%hor_inc         = 1.0
    obj%tim_beg         = obj%tstrt
    obj%tim_end         = FNIL
    obj%scale_data      = 1.0
    obj%clip            = 0.0
    obj%num_ranges      = 0

    obj%nter            = num_conj_grad

    call mdip_update (obj)
    return
  end subroutine mdip_initialize


!!------------------------- start of update ------------------------------!!
!!------------------------- start of update ------------------------------!!
!!------------------------- start of update ------------------------------!!

  subroutine mdip_update (obj)
    implicit none
    type (mdip_struct), intent (inout), target :: obj           ! arguments

    integer           :: n1, n2, n3, n4, irow, num_inc_new      ! local
    integer           :: irange, j, jcenter, nscratch, nstore   ! local
    integer           :: icount_zero, icount_all, num_cpus      ! local
    real              :: deltap, rtemp                          ! local
    logical           :: tim_end_nil, calc_num_dips             ! local

    real, parameter   :: fcut = 0.333           ! for Gaussian smoother

    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!-------------------------- read parameters -----------------------------!!
!!-------------------------- read parameters -----------------------------!!
!!-------------------------- read parameters -----------------------------!!

      call pc_register_array_names ("dip_min_arrayset", (/  &
                                    "dip_min ",             &
                                    "dip_max ",             &
                                    "scale   ",             &
                                    "num_dips" /))

    call pc_get_global ('NWIH',  obj%nwih)
    call pc_get_global ('NDPT',  obj%ndpt)
    call pc_get_global ('DT',    obj%dt)
    call pc_get_global ('TSTRT', obj%tstrt)
    call pc_get_global ('NUMTR', obj%numtr)

    call pc_get_jdata('NUM_CPUS',num_cpus)

    obj%fmax = 0.2 / obj%dt ! (Set fmax to 40% Nyquist -- 50 Hz @ dt = 4 ms)

    call pc_get ('HDR_GATH',   obj%hdr_gath)
    call pc_get ('NUM_WIN',    obj%num_win)
    call pc_get ('NUM_INC',    obj%num_inc)
    call pc_get ('HDR_HOR',    obj%hdr_hor)
    call pc_get ('HOR_INC',    obj%hor_inc)
    call pc_get ('TIM_BEG',    obj%tim_beg)
    call pc_get ('TIM_END',    obj%tim_end)
    call pc_get ('SCALE_DATA', obj%scale_data)
    call pc_get ('CLIP',       obj%clip)

    n1 = obj%num_ranges
    n2 = obj%num_ranges
    n3 = obj%num_ranges
    n4 = obj%num_ranges
    call pc_alloc ('DIP_MIN' , obj%dip_min , n1)
    call pc_alloc ('DIP_MAX' , obj%dip_max , n2)
    call pc_alloc ('SCALE'   , obj%scale   , n3)
    call pc_alloc ('NUM_DIPS', obj%num_dips, n4)

!!------------------------- verify parameters ----------------------------!!
!!------------------------- verify parameters ----------------------------!!
!!------------------------- verify parameters ----------------------------!!

    !
    ! - Check HDR_GATH
    !
    if ((obj%hdr_gath < 0) .or. (obj%hdr_gath > obj%nwih)) then  ! 0 - NWIH
      !
      call pc_error (msg1 = 'Bad value for HDR_GATH (',  &
                     var1 = obj%hdr_gath,                &
                     msg2 = ').  Must be between 0 and NWIH')
      obj%hdr_gath = min (max(obj%hdr_gath,0), obj%nwih)
      !
    end if
    !
    ! - Check NUM_WIN and NUM_INC
    !
    if (pc_verify_scalar('NUM_WIN') .or. pc_verify_scalar('NUM_INC')) then
      if (obj%num_win < 3) then
        call pc_info (msg1 = 'NUM_WIN must be at least 3. Original value (', &
                      var1 = obj%num_win, &
                      msg2 = ') reset to 3')
        obj%num_win = 3
      end if
      num_inc_new = min (max(obj%num_inc, 1), obj%num_win - 2)
      if (mod(obj%num_win - num_inc_new, 2) /= 0) then
        if (num_inc_new == 1) then
          num_inc_new = 2
        else
          num_inc_new = num_inc_new - 1
        end if
      end if
      if (num_inc_new /= obj%num_inc) then
        call pc_info (msg1 = 'NUM_WIN - NUM_INC must be even. Original &
                             &NUM_INC (',  &
                      var1 = obj%num_inc,  &
                      msg2 = ') reset to', &
                      var2 = num_inc_new   )
        obj%num_inc = num_inc_new
      end if
      if (obj%num_win < 10) then
        call pc_info (msg1 = 'MDIP may not distinguish dips properly if &
                             &NUM_WIN (',        &
                      var1 = obj%num_win,        &
                      msg2 = ') is less than 10.')
      end if
      if (abs(3*obj%num_inc - obj%num_win) > 4) then
        call pc_info ('Good results are usually obtained when NUM_INC &
                      &is about 1/3 of NUM_WIN.')
        if (3*obj%num_inc > obj%num_win) then
          call pc_info ('Edges of modeled windows will not match properly &
                        &if NUM_INC is too big.')
        end if
      end if
    end if
    !
    ! - Check HDR_HOR
    !
    if ((obj%hdr_hor < 1) .or. (obj%hdr_hor > obj%nwih)) then
      call pc_error (msg1 = 'Bad value for HDR_HOR (',  &
                     var1 = obj%hdr_hor,                &
                     msg2 = '). Must be between 1 and NWIH')
      obj%hdr_hor = min (max(obj%hdr_hor,1), obj%nwih)
    end if
    !
    ! - Check HOR_INC
    !
    if (obj%hor_inc <= 0.0) then   ! real >= 0.0
      call pc_error (msg1 = 'Bad value for HOR_INC (',  &
                     var1 = obj%hor_inc,                &
                     msg2 = '). Must be greater than 0.0')
      obj%hor_inc = 1.0
    end if
    !
    ! - Check TIM_BEG and TIM_END
    !
    tim_end_nil = (obj%tim_end == fnil)
    call timecheck ('TIM_BEG', 'TIM_END', obj%tim_beg, obj%tim_end, &
                    obj%itim_beg, obj%itim_end)
    if (tim_end_nil) obj%tim_end = fnil
    !
    ! - Check CLIP
    !
    if (obj%clip < 0.0) then   ! real >= 0.0
      call pc_error (msg1 = 'Bad value for CLIP (',   &
                     var1 = obj%clip,                 &
                     msg2 = '). Must be non-negative.')
      obj%clip = 0.0
    end if
    !
    ! - Check dip windows array set
    !
    if (n2 /= n1 .or. n3 /= n1 .or. n4 /= n1) then
      call pc_error ('DIP_MIN, DIP_MAX, SCALE, NUM_DIPS arrays have &
                     &different lengths.')
      obj%num_ranges = min (n1, n2, n3, n4)
    else
      obj%num_ranges = n1
    end if
    if (pc_verify_arrayset ('DIP_MIN_ARRAYSET')) then
      do irow = 1, obj%num_ranges
        calc_num_dips = .true.
        if (obj%dip_min(irow) == fnil) then
          call pc_error ("DIP_MIN hasn't been specified for range", irow)
          calc_num_dips = .false.
        end if
        if (obj%dip_max(irow) == fnil) then
          call pc_error ("DIP_MAX hasn't been specified for range", irow)
          calc_num_dips = .false.
        end if
        if (obj%scale(irow) == fnil) then
          call pc_error ("SCALE hasn't been specified for range", irow)
          calc_num_dips = .false.
        end if
        if ((obj%num_dips(irow) == inil) .or. (obj%num_dips(irow) < 1)) then
          if (calc_num_dips) then
            obj%num_dips(irow) = &
                        nint (obj%hor_inc * real(obj%num_win) * obj%fmax  &
                              * abs(obj%dip_max(irow) - obj%dip_min(irow)))
            obj%num_dips(irow) = 2 * ((obj%num_dips(irow) + 1) / 2)  +  1
            if (obj%num_dips(irow) > 200) then
              call pc_warning ('Calculated NUM_DIPS for range', irow,   &
                             'is huge; check HDR_HOR, HOR_INC, DIP_MIN, &
                             &DIP_MAX units.')
            end if
          else
            obj%num_dips(irow) = inil
          end if
        else if (obj%num_dips(irow) > 100) then
          call pc_warning ('Large NUM_DIPS for range', irow, &
                           '. Suggest no bigger than 100.')
        end if
      end do
    end if

!!-------------------------- write parameters -----------------------------!!
!!-------------------------- write parameters -----------------------------!!
!!-------------------------- write parameters -----------------------------!!

    !
    ! - Control parameters
    !
    call pc_put_control ('NEED_LABEL'  , .true.)
    call pc_put_control ('NEED_REQUEST', .true.)
    ! ----------------------------- PCPS Parameters-------------------------

    if(num_cpus>1 .and. obj%numtr>1) then
      call pc_put_control ('PARALLEL_SAFE'         , .true.)
      call pc_put_control ('PCPS_SEND_MODE'        , 'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'     , 'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'       , 'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'    , 'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'    , 'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE' , 'PCPS_RECEIVE_ALL_EOF')
    endif
    !
    ! - Find grand total dips
    !
    obj%nall  = 0
    obj%nzero = 0
    do irange = 1, obj%num_ranges
      if ((obj%num_dips(irange) == INIL) &
             .or. (obj%num_dips(irange) < 1)) cycle
      obj%nall = obj%nall + obj%num_dips(irange)
      if (obj%scale(irange) == 0.0) then
        obj%nzero = obj%nzero + obj%num_dips(irange)
      end if
    end do
    !
    nscratch = 4*obj%num_win + obj%ndpt*(4*obj%num_win + 3*obj%nall)
    call pc_put_control ('nscratch', nscratch)
    !
    nstore = (2*obj%num_win + 1) * (2*obj%nwih + obj%ndpt) &
               + 2*obj%nall + obj%nzero
    call pc_put_control ('nstore',   nstore)
    !
    ! - Globals
    !
    ! We don't change any globals, so no PC_PUT_GLOBAL calls!
    !
    ! - Process parameters
    !
    call pc_put ('HDR_GATH',   obj%hdr_gath)
    call pc_put ('NUM_WIN',    obj%num_win)
    call pc_put ('NUM_INC',    obj%num_inc)
    call pc_put ('HDR_HOR',    obj%hdr_hor)
    call pc_put ('HOR_INC',    obj%hor_inc)
    call pc_put ('TIM_BEG',    obj%tim_beg)
    call pc_put ('TIM_END',    obj%tim_end)
    call pc_put ('SCALE_DATA', obj%scale_data)
    call pc_put ('CLIP',       obj%clip)
    call pc_put ('DIP_MIN' ,   obj%dip_min , obj%num_ranges)
    call pc_put ('DIP_MAX' ,   obj%dip_max , obj%num_ranges)
    call pc_put ('SCALE'   ,   obj%scale   , obj%num_ranges)
    call pc_put ('NUM_DIPS',   obj%num_dips, obj%num_ranges)

!!----------------------- prepare for execution ---------------------------!!
!!----------------------- prepare for execution ---------------------------!!
!!----------------------- prepare for execution ---------------------------!!

    call mem_free (obj%pvals_zero)
    call mem_free (obj%pvals_all)
    call mem_free (obj%scales_all)
    call mem_free (obj%gauss_smooth)
    call mem_free (obj%hd_buf1)
    call mem_free (obj%tr_buf1)
    call mem_free (obj%hd_buf2)
    call mem_free (obj%tr_buf2)
    if (associated(obj%rollalong)) call rollalong_delete (obj%rollalong)

!<execute_only>

    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.            ! needed for the wrapup routine.
    !
    ! - Allocate & fill p-values and scales arrays
    !
    call mem_alloc (obj%pvals_all,  obj%nall)
    call mem_alloc (obj%scales_all, obj%nall)
    call mem_alloc (obj%pvals_zero, obj%nzero)
    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    icount_all  = 0
    icount_zero = 0
    do irange = 1, obj%num_ranges
      deltap = (obj%dip_max(irange) - obj%dip_min(irange)) &
                            / obj%num_dips(irange)
      do j = 1, obj%num_dips(irange)
        icount_all = icount_all + 1
        obj%pvals_all(icount_all) = obj%dip_min(irange) + (j - 0.5)*deltap
        obj%scales_all(icount_all) = obj%scale(irange)
      end do
      if (obj%scale(irange) == 0.0) then
        do j = 1, obj%num_dips(irange)
          icount_zero = icount_zero + 1
          obj%pvals_zero(icount_zero) = obj%dip_min(irange) + (j - 0.5)*deltap
        end do
      end if
    end do
    !
    ! - Set up Rollalong primitive
    !
    call rollalong_create (obj%rollalong, obj%num_win, obj%num_inc, &
                           obj%hdr_gath, obj%nwih, obj%ndpt)
    call mem_alloc (obj%hd_buf1, obj%nwih, obj%num_win)
    call mem_alloc (obj%tr_buf1, obj%ndpt, obj%num_win)
    call mem_alloc (obj%hd_buf2, obj%nwih, obj%num_win)
    call mem_alloc (obj%tr_buf2, obj%ndpt, obj%num_win)
    !
    ! - Set up Gaussian smoother.
    ! - Set span of 3, at width of 1.5  exp(-pi*1.5**2) = 1/1174.
    !   Should be odd number for symmetry
    !
    obj%len_smooth = nint(3.0 / fcut)
    obj%len_smooth = 2 * (obj%len_smooth / 2)  +  1
    jcenter = (1 + obj%len_smooth) / 2
    obj%ishift_smooth = 1 - jcenter
    call mem_alloc (obj%gauss_smooth, obj%len_smooth)
    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    do j = 1, obj%len_smooth
      rtemp = PI * (fcut*(j-jcenter))**2
      obj%gauss_smooth(j) = exp(-rtemp)
    end do
    rtemp = 1.0 / sum(obj%gauss_smooth)
    obj%gauss_smooth = obj%gauss_smooth * rtemp

!</execute_only>

!!--------------------------- finish update -------------------------------!!
!!--------------------------- finish update -------------------------------!!
!!--------------------------- finish update -------------------------------!!

    return
  end subroutine mdip_update

!!------------------------------- traps -----------------------------------!!
!!------------------------------- traps -----------------------------------!!
!!------------------------------- traps -----------------------------------!!


!!---------------------------- main execution -----------------------------!!
!!---------------------------- main execution -----------------------------!!
!!---------------------------- main execution -----------------------------!!

!<execute_only>

  subroutine mdip (obj,ntr,hd,tr)
    implicit none
    type (mdip_struct), intent(inout) :: obj               ! arguments
    integer,            intent(inout) :: ntr               ! arguments
    double precision,   intent(inout) :: hd (:, :)         ! arguments
    real,               intent(inout) :: tr (:, :)         ! arguments
    
    integer                           :: ntr1, i1, i2
    logical                           :: ltransf

    if(obj%numtr.eq.1) then
      call mdip_one_trace(obj,ntr,hd,tr)
      if (ntr.eq.NO_MORE_TRACES .or. ntr.eq.FATAL_ERROR) then
        call mdip_wrapup (obj)
      end if
      return
    endif

! - know numtr>1           gather logic added Sep 2001 by CC Burch

    if(ntr.eq.NO_MORE_TRACES .or. ntr.eq.FATAL_ERROR) then
      call mdip_wrapup (obj)
      return
    else if(ntr.eq.NEED_TRACES) then
      return
    endif   

! - ntr >0 at this point
! - Basically this is treated as a gather-in and gather-out process

    call rollalong_init(obj%rollalong)
    i1=0
    i2=0
    do while(i2.lt.ntr)

! --- get ready to send primer as a trace if available else NO_MORE_TRACES

      if(i1.lt.ntr) then
        i1=i1+1
        ntr1=1 
      else
        ntr1=NO_MORE_TRACES
      endif

! --- after sending the primer, get as many output traces as available

      do while(.true.)

        call rollalong_store (obj%rollalong,ntr1,hd(:,i1:i1),tr(:,i1:i1),  &
          obj%hd_buf1,obj%tr_buf1,ltransf)

        if (ltransf) call mdip_model_dips(obj,ntr1)

        call rollalong_serve (obj%rollalong,ntr1,                         &
          hd(:,i2+1:i2+1),tr(:,i2+1:i2+1), obj%hd_buf2,obj%tr_buf2)

        if(ntr1.gt.0) then
          i2=i2+1                   !process output trace
          call lav_set_hdr (hd(:,i2), tr(:,i2), obj%ndpt)
          call mutehw (hd(:,i2), tr(:,i2), obj%ndpt, 0.0, MUTEHW_BOTH)
          if(i2.eq.ntr) exit        !exit if all output done

          ntr1=NEED_TRACES
        else
          exit                      !get ready to send next primer
        endif
      enddo

    enddo

    return                          !note output ntr same as input ntr
    end subroutine mdip


!!--------------------------- mdip_one_trace  ------------------------------!!
!!--------------------------- mdip_one_trace  ------------------------------!!
!!--------------------------- mdip_one_trace  ------------------------------!!

! --- This is eseentially the old single trace input mdip code 

  subroutine mdip_one_trace (obj,ntr,hd,tr)
    implicit none
    type (mdip_struct), intent(inout) :: obj               ! arguments
    integer,            intent(inout) :: ntr               ! arguments
    double precision,   intent(inout) :: hd (:, :)         ! arguments
    real,               intent(inout) :: tr (:, :)         ! arguments
    !
    logical :: ltransf                                     ! local
    !
    ! - Begin mdip
    !
    call rollalong_store (obj%rollalong, ntr, hd, tr, &
                          obj%hd_buf1, obj%tr_buf1, ltransf)
    !
    if (ltransf) call mdip_model_dips (obj, ntr)
    !
    call rollalong_serve (obj%rollalong, ntr, hd, tr, &
                          obj%hd_buf2, obj%tr_buf2)
    !
    if (ntr == 1) then
      call lav_set_hdr (hd(:,1), tr(:,1), obj%ndpt)
      call mutehw (hd(:,1), tr(:,1), obj%ndpt, 0.0, MUTEHW_BOTH)
    end if

    return
  end subroutine mdip_one_trace

!!--------------------------- mdip_model_dips ------------------------------!!
!!--------------------------- mdip_model_dips ------------------------------!!
!!--------------------------- mdip_model_dips ------------------------------!!

  subroutine mdip_model_dips (obj, ntr)
    implicit none
    type (mdip_struct), intent(inout) :: obj               ! arguments
    integer,            intent(in)    :: ntr               ! arguments

    integer          :: trace, numlive, ip, icount         ! local
    double precision :: xvals_avg                          ! local
    double precision :: xvals_dbl (obj%num_win)            ! local
    real             :: xvals (obj%num_win)                ! local
    integer          :: trlive (obj%num_win)               ! local
    real             :: xdata1 (obj%ndpt, obj%num_win)     ! local
    real             :: xdata2 (obj%ndpt, obj%num_win)     ! local
    real             :: pdata (obj%ndpt, obj%nall)         ! local

!---Begin mdip_model_dips

    obj%hd_buf2(:,:ntr) = obj%hd_buf1(:,:ntr)
    obj%tr_buf2(:,:ntr) = obj%tr_buf1(:,:ntr)
!
!---Locate the live traces and move to xdata1 buffer
!
    numlive = 0
    do trace = 1, ntr
      if (obj%hd_buf1(HDR_LAV,trace) > 0.0D0) then
        numlive = numlive + 1
        trlive(numlive) = trace
        xdata1(:,numlive)  = obj%tr_buf1(:,trace)
        xvals_dbl(numlive) = obj%hd_buf1(obj%hdr_hor,trace)
      end if
    end do
    if (numlive < 2) return
    xvals_avg = sum(xvals_dbl(:numlive)) / numlive
    xvals(:numlive) = real (xvals_dbl(:numlive) - xvals_avg)
!
!---Model and subtract any dips with zero scale factor
!
    if (obj%nzero > 0) then
      call mdip_inv_slst (obj%nter, obj%ndpt, obj%dt, numlive, xvals, &
                          xdata1, obj%nzero, obj%pvals_zero, pdata)
      call mdip_fwd_slst (obj%ndpt, obj%dt, obj%nzero, obj%pvals_zero, &
                          pdata, numlive, xvals, xdata2)
      call mdip_time_window (obj, numlive, xdata2)
      xdata1(:,:numlive) = xdata1(:,:numlive) - xdata2(:,:numlive)
    end if
!
!---Model ALL dips, applying specified scale factors in p domain
!
    call mdip_inv_slst (obj%nter, obj%ndpt, obj%dt, numlive, xvals, &
                        xdata1, obj%nall, obj%pvals_all, pdata)
    if (obj%clip > 0.0) call mdip_clip (obj, pdata)
    do ip = 1, obj%nall
      pdata(:,ip) = pdata(:,ip) * obj%scales_all(ip)
    end do
    call mdip_fwd_slst (obj%ndpt, obj%dt, obj%nall, obj%pvals_all, &
                        pdata, numlive, xvals, xdata2)
    call mdip_time_window (obj, numlive, xdata2)
!
!---Combine scaled, modeled data with the original data
!
    do icount = 1, numlive
      trace = trlive(icount)
      obj%tr_buf2(:,trace) = obj%scale_data * obj%tr_buf1(:,trace) &
                               + xdata2(:,icount)
    end do
    return
  end subroutine mdip_model_dips

!!-------------------------- mdip_time_window ------------------------------!!
!!-------------------------- mdip_time_window ------------------------------!!
!!-------------------------- mdip_time_window ------------------------------!!

  subroutine mdip_time_window (obj, ntr, xdata)
    implicit none
    type (mdip_struct), intent(in)    :: obj               ! arguments
    integer           , intent(in)    :: ntr               ! arguments
    real              , intent(inout) :: xdata(:,:)        ! arguments

    if (obj%itim_beg > 1) then
      xdata (:obj%itim_beg-1, :ntr) = 0.0
    end if
    if (obj%itim_end < obj%ndpt) then
      xdata (obj%itim_end+1:, :ntr) = 0.0
    end if
    return
  end subroutine mdip_time_window

!!------------------------------ mdip_clip ---------------------------------!!
!!------------------------------ mdip_clip ---------------------------------!!
!!------------------------------ mdip_clip ---------------------------------!!

  subroutine mdip_clip (obj, pdata)
    implicit none
    type (mdip_struct), intent(in)    :: obj               ! arguments
    real              , intent(inout) :: pdata(:,:)        ! arguments

    integer        :: ip                                   ! local
    real           :: dev                                  ! local
    real           :: trtemp(obj%ndpt)                     ! local
    real           :: trtemp1(obj%ndpt)                    ! local

    dev = (obj%clip ** 2) * sum(pdata * pdata) / real(obj%ndpt * obj%nall)
    if (dev <= 0.0) return

    do ip = 1, obj%nall
      if (obj%scales_all(ip) == 0.0) cycle
      call fltr_envelope (pdata(:,ip), obj%ndpt, trtemp)
      trtemp = trtemp / (trtemp + dev)
      call fltr_filtrgs (obj%gauss_smooth, obj%len_smooth, &
             trtemp, obj%ndpt, trtemp1, obj%ndpt, 1, obj%ishift_smooth)
      pdata(:,ip) = pdata(:,ip) * trtemp1
    end do
    return
  end subroutine mdip_clip

!!---------------------------- mdip_fwd_slst -------------------------------!!
!!---------------------------- mdip_fwd_slst -------------------------------!!
!!---------------------------- mdip_fwd_slst -------------------------------!!

  subroutine mdip_fwd_slst (ndpt, dt, np, pvals, pdata, nx, xvals, xdata)
!------------------------------------------------------------------------
! The transform from tau-p to x-t domain is regarded as the "forward"
! transform. This is a relatively straightforward slant stack (But, to
! be completely precise, the transform in MDIP_ADJ_SLST is the totally
! standard slant stack, while MDIP_FWD_SLST is the adjoint of that
! transform).
!------------------------------------------------------------------------
    implicit none
    integer   , intent(in)  :: ndpt                        ! arguments
    real      , intent(in)  :: dt                          ! arguments
    integer   , intent(in)  :: np                          ! arguments
    real      , intent(in)  :: pvals (:)                   ! arguments
    real      , intent(in)  :: pdata (:,:)                 ! arguments
    integer   , intent(in)  :: nx                          ! arguments
    real      , intent(in)  :: xvals (:)                   ! arguments
    real      , intent(out) :: xdata (:,:)                 ! arguments

    integer         :: ip, ix, idelta, itau1, itau2        ! local
    real            :: delta, alpha, beta                  ! local

    xdata (:ndpt, :nx) = 0.0

    do ip = 1, np
      do ix = 1, nx
        delta = pvals(ip) * xvals(ix) / dt
        idelta = floor (delta)
        itau1 = max (1, 1-idelta)
        itau2 = min (ndpt, ndpt-idelta-1)
        if (itau2 < itau1) cycle
        beta = delta - idelta
        alpha = 1.0 - beta
        xdata(itau1+idelta:itau2+idelta,ix) = &
          xdata(itau1+idelta:itau2+idelta,ix) + alpha*pdata(itau1:itau2,ip)
        xdata(itau1+idelta+1:itau2+idelta+1,ix) = &
          xdata(itau1+idelta+1:itau2+idelta+1,ix) + beta*pdata(itau1:itau2,ip)
      end do
    end do

    return
  end subroutine mdip_fwd_slst

!!---------------------------- mdip_adj_slst -------------------------------!!
!!---------------------------- mdip_adj_slst -------------------------------!!
!!---------------------------- mdip_adj_slst -------------------------------!!

  subroutine mdip_adj_slst (ndpt, dt, nx, xvals, xdata, np, pvals, pdata)
!------------------------------------------------------------------------
! This is the Hermitean adjoint of the "forward" transform given by
! subroutine MDIP_FWD_SLST, thus, it transforms from x-t to the tau-p
! domain. This present routine is, in fact, a totally standard slant
! stack with 2-point linear interpolation.
!------------------------------------------------------------------------
    implicit none
    integer   , intent(in)  :: ndpt                        ! arguments
    real      , intent(in)  :: dt                          ! arguments
    integer   , intent(in)  :: nx                          ! arguments
    real      , intent(in)  :: xvals (:)                   ! arguments
    real      , intent(in)  :: xdata (:,:)                 ! arguments
    integer   , intent(in)  :: np                          ! arguments
    real      , intent(in)  :: pvals (:)                   ! arguments
    real      , intent(out) :: pdata (:,:)                 ! arguments

    integer         :: ip, ix, idelta, itau1, itau2        ! local
    real            :: delta, alpha, beta                  ! local

    pdata (:ndpt, :np) = 0.0

    do ip = 1, np
      do ix = 1, nx
        delta = pvals(ip) * xvals(ix) / dt
        idelta = floor (delta)
        itau1 = max (1, 1-idelta)
        itau2 = min (ndpt, ndpt-idelta-1)
        if (itau2 < itau1) cycle
        beta = delta - idelta
        alpha = 1.0 - beta
        pdata(itau1:itau2,ip) = pdata(itau1:itau2,ip)                      &
                              + alpha*xdata(itau1+idelta:itau2+idelta,ix)  &
                              + beta*xdata(itau1+idelta+1:itau2+idelta+1,ix)
      end do
    end do

    return
  end subroutine mdip_adj_slst

!!---------------------------- mdip_inv_slst -------------------------------!!
!!---------------------------- mdip_inv_slst -------------------------------!!
!!---------------------------- mdip_inv_slst -------------------------------!!

  subroutine mdip_inv_slst (nter, ndpt, dt, nx, xvals, xdata,  &
                                            np, pvals, pdata)
!-------------------------------------------------------------------------
! This is a least squares inverse of the "forward" transform given by
! subroutine MDIP_FWD_SLST, thus, it transforms from x-t to the tau-p
! domain. The inversion is performed using a conjugate gradient method.
!
! Unfortunately, since we're in the time domain (unlike RMUL which works
! in the frequency domain), we cannot simply call the CONJGRAD primitive
! because "X" and "Y" vectors in the conjugate gradient solution are now
! 2-dimensional instead of 1-dimensional and, more importantly, it
! would use too much memory to explicitly store the "A" matrix, which
! would be very sparse (so, instead, the actions of "A" and its adjoint
! are obtained by subroutine calls: MDIP_FWD_SLST and MDIP_ADJ_SLST).
!
! But, although we can't call the CONJGRAD primitive, we reuse as much
! of the code from CONJGRAD (in particular, from subroutine CONJGRAD_REAL)
! as possible.
!-------------------------------------------------------------------------
    implicit none
!
!---Arguments
!
    integer   , intent(in)  :: nter                        ! arguments
    integer   , intent(in)  :: ndpt                        ! arguments
    real      , intent(in)  :: dt                          ! arguments
    integer   , intent(in)  :: nx                          ! arguments
    real      , intent(in)  :: xvals (:)                   ! arguments
    real      , intent(in)  :: xdata (:, :)                ! arguments
    integer   , intent(in)  :: np                          ! arguments
    real      , intent(in)  :: pvals (:)                   ! arguments
    real      , intent(out) :: pdata (:, :)                ! arguments
!
!---Local automatic arrays
!
    real    :: resid (ndpt, nx)
    real    :: grad (ndpt, np)
    real    :: step (ndpt, np)
    real    :: a_step (ndpt, nx)
!
!---Default control variables for conjugate gradient solution
!
    real, parameter  :: prwh_default = 0.0001
    real, parameter  :: conv_default = 0.000001
!
!---Local scalar variables
!
    integer :: iter
    real    :: eps, ggdot, ggtest, ggprev, ssdot, asasdot, alpha, beta
!
!---Begin MDIP_INV_SLST
!
    pdata(:ndpt,:np) = 0.0
    resid = xdata(:ndpt,:nx)
!
!----------------------------------------------------------------------
!   Note: eps is prwh_default multiplied by average diagonal element
!   of (A* A) matrix (Factor of 0.666667 is approximate).
!----------------------------------------------------------------------
    eps = prwh_default * 0.666667 * real(nx)
!
    do iter = 1, nter
!
       call mdip_adj_slst (ndpt, dt, nx, xvals, resid, np, pvals, grad)
       grad = grad - eps*pdata(:ndpt,:np)
       ggdot = sum (grad * grad)
!
       if (iter == 1) then
         if (ggdot <= 0.) exit
         ggtest = ggdot * conv_default**2
         step = grad
         ssdot = ggdot
       else
         if (ggdot <= ggtest) exit
         beta = ggdot / ggprev
         step = grad + beta*step
         ssdot = sum (step * step)
         if (ssdot <= 0.) exit
       end if
!
       call mdip_fwd_slst (ndpt, dt, np, pvals, step, nx, xvals, a_step)
!
       asasdot  = sum (a_step * a_step)
       alpha = ggdot / (asasdot + eps*ssdot)
       pdata(:ndpt,:np) = pdata(:ndpt,:np) + alpha*step
       resid = resid - alpha*a_step
!
       ggprev = ggdot
!
    end do
    return
  end subroutine mdip_inv_slst

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

  subroutine mdip_wrapup (obj)
    implicit none
    type (mdip_struct), intent (inout) :: obj       ! arguments
    !
    if (obj%skip_wrapup) return
    !
    call mem_free (obj%dip_min)
    call mem_free (obj%dip_max)
    call mem_free (obj%scale)
    call mem_free (obj%num_dips)
    call mem_free (obj%pvals_zero)
    call mem_free (obj%pvals_all)
    call mem_free (obj%scales_all)
    call mem_free (obj%gauss_smooth)
    call mem_free (obj%hd_buf1)
    call mem_free (obj%tr_buf1)
    call mem_free (obj%hd_buf2)
    call mem_free (obj%tr_buf2)
    if (associated(obj%rollalong)) call rollalong_delete (obj%rollalong)
    !
    obj%skip_wrapup = .true.
    return
  end subroutine mdip_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

end module mdip_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
