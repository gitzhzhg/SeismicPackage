!<CPS_v1 type="PROCESS"/>
!!------------------------------- apin.f90 ---------------------------------!!
!!------------------------------- apin.f90 ---------------------------------!!
!!------------------------------- apin.f90 ---------------------------------!!

! --> Edit the line below as appropriate:

        ! other files are:  apin_crou.c  apin_frou.f90  apin.h

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
! Name       : APIN
! Category   : amplitude_mod
! Written    : 2004-02-17   by: mlri
! Revised    : 2006-10-31   by: B. Menger
! Maturity   : production
! Purpose    : To identify AVO anomalies and their AVO class type
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  This tool computes various complex product indicators for AVO analysis. All
!  these indicators are invariant to the phase of the incoming data.
!
!  Inputs and Outputs:
!
!  Minimum input to this tool is a Standard Suite, consisting of the following 
!  traces:
! 
! 
!  Trace                        Header trc_type
!                               (Stored in Header #49) 
!  Real AVO "A" trace           43
!  Real AVO "B" trace           44
!  Imag AVO "A" trace           17
!  Imag AVO "B" trace           18
!  Standard deviation "A"       45
!  Standard deviation "B"       47
!  Real correlation             48
!  Imag correlation             41
!
!  The output of this tool is the selected product indicator described below.
!  This product indicator may also be optionally multiplexed with the real
!  AVO "A" trace as input to the tool. This is the only tool in the AVO Seismic
!  Workbench which does not pass a standard suite as its output.
!
!  Parameters:
! 
!  Part of Complex Product:
!
!  Products of complex numbers generally are also complex. This parameter
!  allows you to choose which part of this complex product to output:
!
!    REAL - Only the real part of the complex product will be passed. This
!    trace will have a trace type of 42 in header #49. This part of the
!    complex product is useful for detecting AVO anomalies.
!
!    IMAG - Only the imaginary part of the complex product will be passed. This
!    trace will have a trace type of 46 in header #49. This part of the
!    complex product is useful for detecting PVO (phase versus offset)
!    anomalies.
!
!    OVERLAY - The real part of the AVO "A" trace (trace type 43) will be
!    multiplexed together with the real part of the complex product (trace
!    type 42).
!
!  Overlay product with Re{A}?
!
!    An affirmative answer to this question will cause the real part of the 
!    AVO "A" trace to be multiplexed with whatever part of the complex 
!    product is selected. Otherwise, only the complex product trace will be 
!    passed. This question is redundant if the answer to the previous question
!    is "OVERLAY." In this case, this question will not be displayed.
!
!  Type of product indicator:
!
!    Various type of complex product indicators are available:
!
!      AB* - This is the conventional AVO product indicator. It works best 
!      for Class III anomalies where the wet sand impedance is roughly equal 
!      to the background shale impedance. The mean value of this indicator is 
!      generally negative, unless the statistics of the AVO data have been 
!      altered by other processes.
!
!      A DB* - This indicator works best for Class III-IV anomalies where 
!      the wet sand impedance is considerably less than the background shale 
!      impedance. Note that there be an equal number of positive and negative
!      events with this indicator.
!
!      DA B* - This indicator works best for Class I-II anomalies where the 
!      wet sand impedance is considerably greater than the background shale 
!      impedance. There will be an equal number of positive and negative 
!      events with this indicator.
!
!      D(AB*) - This indicator is the sum of the A DB* and the DA B* indicators.
!      Although less sensitive than either of its components (and hence more
!      conservative) in detecting Class I and Class IV sands, it has a 
!      chance to detect any type of hydrocarbon deposit regardless of its 
!      class. Consequently, it is the most appropriate product indicator to use 
!      when the sand and shale impedances are unknown or uncertain. Like 
!      DA B* and A DB*, D(AB*) will have zero mean. 
!
!      REAL - Only the real part of the complex product will be passed. This
!      trace will have a trace type of 42. This part of the complex product is
!      useful for detecting AVO anomalies.
!
!      IMAG - Only the imaginary part of the complex product will be 
!      passed. This trace will have a trace type of 46 in header #49. This
!      part of the complex product is useful for detecting PVO (phase versus
!      offset) anomalies.
!
!      OVERLAY - The real part of the AVO "A" trace (trace type 43) will be 
!      multiplexed together with the real part of the complex product (trace
!      type 42).
!
!  Overlay product with Re{A}?
! 
!  An affirmative answer to this question will cause the real part of the 
!  AVO "A" trace to be multiplexed with whatever part of the complex product is 
!  selected. Otherwise, only the complex product trace will be passed. This
!  question is redundant if the answer to the previous question is "OVERLAY."
!  In this case, this question will not be displayed.
!
!  Type of product indicator:
!
!  Various type of complex product indicators are available:
!
!  AB* - This is the conventional AVO product indicator. It works best 
!  for Class III anomalies where the wet sand impedance is roughly equal to
!  the background shale impedance. The mean value of this indicator is
!  generally negative, unless the statistics of the AVO data have been
!  altered by other processes.
!
!  A DB* - This indicator works best for Class III-IV anomalies where 
!  the wet sand impedance is considerably less than the background shale
!  impedance.  Note that there be an equal number of positive and negative
!  events with this indicator.
!
!  DA B* - This indicator works best for Class I-II anomalies where the 
!  wet sand impedance is considerably greater than the background shale
!  impedance. There will be an equal number of positive and negative events
!  with this indicator.
!
!  D(AB*) - This indicator is the sum of the A DB* and the DA B* indicators. 
!  Although less sensitive than either of its components (and hence more 
!  conservative.
!
!  |A|2 - This indicator represents the envelope of the AVO "A" trace. It 
!  can be useful for quantitative bright spot analysis. It is positive
!  everywhere.
!
!  |B|2 - This indicator represents the envelope of the AVO "B" trace. If 
!  principal component analysis has been performed, this trace represents 
!  the distance of an event from the principal data axis of symmetry.
!
!
!  Normalization factor:
!
!  You may normalize the product indicator by one of several factors:
!
!  None - Unity: No normalization will be performed. The product
!  indicators will be output as is.
!
!  |A| - The product indicator will be normalized by the magnitude of A 
!  raised to an optional exponent. This will de-emphasize events with a 
!  large zero offset reflectivity.
!
!  |B| - The product indicator will be normalized by the magnitude of B 
!  raised to an optional exponent. This will de-emphasize events with a 
!  large AVO gradient (slope).
!
!  |AB| - The product indicator will be normalized by both the magnitude 
!  of A and the B (both raised to the same optional exponent). This will 
!  de-emphasize events with a strong magnitude of AB*.
!
!  Radius - The product indicator will be normalized by the distance of 
!  each event from the origin in the complex (A, B) plane, raised to an 
!  optional exponent. This will de-emphasize any event with a large zero 
!  offset response or gradient.
!
!  Normalization exponent:
!
!  The normalization factor may be raised to a power of either one-half, 
!  unity or two. The greater this exponent, the greater will be the effect 
!  of the normalization.
!
!  Normalization smoothing (ms):
!
!  The normalization factor may be smoothed by a sliding moving aver-
!  age window of the desired length. This smoothing is performed before 
!  exponentiation. The smaller this normalization smoothing length 
!  becomes, the more rapid will the normalization become. The default 
!  value is 36 ms.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  Minimum input to this tool is a Standard Suite of AVO attributes, which may 
!  be generated with either the AVOSTS (Standard Suite)or AVOANS (Alternate
!  Norm Suite).  Frequently AVONRM (AVO Normalization) and AVOCORR (AVO
!  Correlation Matching) are applied to the Standard Suite before running
!  APIN (AVO Product Indicators).
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
!  Live seismic data      (Trace type #1 - Optional)
!  Real AVO "A" trace     (Trace type #43)
!  Real AVO "B" trace     (Trace type #44)
!  Imag AVO "A" trace     (Trace type #17)
!  Imag AVO "B" trace     (Trace type #18)
!  Standard deviation "A" (Trace type #45)
!  Standard deviation "B" (Trace type #47)
!  Real correlation       (Trace type #48)
!  Imag correlation       (Trace type #41)
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!  The real product     (Trace type #42)
!  The imag product     (Trace type #46)
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! --> Insert globals that this process uses or changes:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       --> specify action taken.
! GATHERED  whether traces are a legitimate gather  --> specify action taken.
! NWIH      number of words in trace header         --> specify action taken.
! NDPT      number of sample values in trace        --> specify action taken.
! TSTRT     starting time on trace                  --> specify action taken.
! DT        trace sample interval                   --> specify action taken.
! GRID      grid transformation structure           --> specify action taken.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! --> Insert header words used or changed by this process:
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   3    HDR_CURRENT_GROUP          Input
!  49    HDR_USER_49                Input/Output trace type
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
!  2. 2006-10-31  B. Menger     Removed Unused Variables.
!  1. 2005-01-03  Michael Ried  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.  --> Change to add any platform dependencies.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.  --> Change if any special compiler/linking required.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! --> Default values are shown below - edit as needed:
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
! --> Edit the following lines as needed:
!
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
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.  --> Change if this statement is inappropriate.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! --> Insert description of algorithms used.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! --> Insert any useful programming notes here.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS APIN Process/NC=80>
!                       AVO PRODUCT INDICATORS
!`--------------------------------------------------------------------------
!                Part of Complex Product[/R]     PPART=`CCCCCCCCCCCCCCCCCCCC
!            Overlay Product with Re{A}?[/R]    OVRLAY=`KKK                 
!              Type of Product Indicator[/R]     PTYPE=`CCCCCCCCCCCCCCCCCCCC
!                   Normalization Factor[/R]     NTYPE=`CCCCCCCCCCCCCCCCCCCC
!                 Normalization Exponent[/R]       EXP=`CCCCCCCCCCCCCCCCCCCC
!           Normalization Smoothing (ms)[/R]    SMOOTH=`IIIII
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="EXP">
!<Tip> -->Select the type of Normalization exponent</Tip>
! Default = --> 0.5
! Allowed = --> 0.5
! Allowed = --> 1.0
! Allowed = --> 2.0
! --> 0.5 -- Normalize by the square root of the normalization factor
! --> 1.0 -- Normalize by the normalization factor
! --> 2.0 -- Normalize by the square of the normalization factor
! --> When selecting the normalization factor, the greater the normalization
! --> exponent, the stronger the events will be downweighted over the weaker
! --> ones.
!</Help>
!
!<Help KEYWORD="NTYPE">
!<Tip> -->Select the type of Normalization factor</Tip>
! Default = --> No Norm
! Allowed = --> No Norm
! Allowed = --> |A|
! Allowed = --> |B|
! Allowed = --> |AB|
! Allowed = --> Radius
! --> No Norm -- No normalization will be performed
! --> |A| -- Product will be normalized by |A|**exponent
! --> |B| -- Product will be normalized by |B|**exponent
! --> |AB| -- Product will be normalized by |AB|**exponent
! --> Radius -- Product will be normalized by [SQRT(|A|**2 + |B|**2)]**exponent
!</Help>
!
!<Help KEYWORD="OVRLAY">
!<Tip> -->Select to overlay product with zero-offset traces (TICD=43)</Tip>
! Default = --> NO
! Allowed = --> NO
! Allowed = --> YES
! --> NO -- Do Not Output zero-offset traces
! --> YES -- Output zero-offset traces (TICD=43)
!</Help>
!
!<Help KEYWORD="PPART">
!<Tip> -->Select the part of the product trace you wish to output</Tip>
! Default = --> Real
! Allowed = --> Real
! Allowed = --> Imag
! Allowed = --> Overlay
! --> Real - Output the Real part (TICD=42), only
! --> Imag - Output the Imag part (TICD=46), only
! --> Overlay - Output a (TICD=43) with Real part (TICD=42)
!</Help>
!
!<Help KEYWORD="PTYPE">
!<Tip> -->Select the type of DHI product indicator</Tip>
! Default = --> AB*
! Allowed = --> AB*
! Allowed = --> A Delta-B*
! Allowed = --> Delta-A B*
! Allowed = --> Delta-(AB)*
! Allowed = --> |A|**2
! Allowed = --> |B|**2
! Allowed = --> |A|
! Allowed = --> |B|
! --> AB* -- Standard complex product
! --> A Delta-B* -- Low-impedance sand DHI
! --> Delta-A B* -- High-impedance sand DHI
! --> Delta-(AB)* -- Indeterminate sand DHI
! --> |A|**2 -- Squared magnitude of A
! --> |B|**2 -- Squared magnitude of B
! --> |A| -- Magnitude of A
! --> |B| -- Magnitude of B
!</Help>
!
!<Help KEYWORD="SMOOTH">
!<Tip> -->Set the length of the smoothing filter</Tip>
! Default = --> 36
! Allowed = --> Integer
! --> The length of the smoothing filter for the normalization factor
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module apin_module
      use pc_module
      use named_constants_module
      use mem_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.

! --> Insert here any other modules used by this process.

      implicit none
      private
      public :: apin_create
      public :: apin_initialize
      public :: apin_update
      public :: apin_delete
      public :: apin            ! main trace processing routine.
      public :: apin_wrapup

      character(len=100),public,save :: APIN_IDENT = &
'$Id: apin.f90,v 1.2 2006/10/30 14:01:44 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: apin_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

! --> Below are commonly used globals - edit or remove as appropriate:

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        character(len=21)          :: exp
        character(len=21)          :: ntype
        logical                    :: ovrlay
        character(len=21)          :: ppart
        character(len=21)          :: ptype
        integer                    :: smooth

        integer                    :: iexp
        integer                    :: intype
        integer                    :: iovrlay
        integer                    :: ippart
        integer                    :: iptype

        integer                    :: ipt_whdrs
        integer                    :: ipt_hci
        integer                    :: ipt_rvi
        integer                    :: ipt_norm
        integer                    :: ipt_norm1
        integer                    :: max_whdrs

        real              ,pointer :: wa1(:)  ! work array


! --> Insert any other needed variables or pointers here.

      end type apin_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


! --> Include any required interfaces here.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(apin_struct),pointer,save :: object    ! needed for traps.

! --> Insert here any data declarations needed by this process.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine apin_create (obj)
      type(apin_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) call pc_error ("Unable to allocate obj in apin_create")


! --> Nullify any additional pointers in the OBJ data structure here.
      nullify(obj%wa1)   ! must be done for all pointers.

      call apin_initialize (obj)
      end subroutine apin_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine apin_delete (obj)
      type(apin_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call apin_wrapup (obj)


! --> Deallocate any additional pointers in the OBJ data structure here.

      if (associated(obj%wa1)) deallocate(obj%wa1)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in apin_delete")
      end subroutine apin_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine apin_initialize (obj)
      type(apin_struct),intent(inout) :: obj       ! arguments

! --> Change the default values below as needed

      obj%exp    = '0.5'
      obj%ntype  = 'No Norm'
      obj%ovrlay = .FALSE.
      obj%ppart  = 'Real'
      obj%ptype  = 'AB*'
      obj%smooth = 36

      call apin_update (obj)
      end subroutine apin_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine apin_update (obj)
      type(apin_struct),intent(inout),target :: obj             ! arguments

! --> Insert code to declare all required local variables.
      integer :: intype, lwa1, max_samps, max_trcs, max_whdrs

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!



! --> Delete any of the globals below that are not needed:

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('EXP   ', obj%exp)
      call pc_get('NTYPE ', obj%ntype)
      call pc_get('OVRLAY', obj%ovrlay)
      call pc_get('PPART ', obj%ppart)
      call pc_get('PTYPE ', obj%ptype)
      call pc_get('SMOOTH', obj%smooth)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!
! ..... Change options into integer values
!
      call pc_put_options_field('EXP   ', (/'0.5                  ',           &
        '1.0                  ','2.0                  '/) )
      if (obj%ppart.eq.'Imag                 ') then
        obj%ippart=1
      else if (obj%ppart.eq.'Overlay              ') then
        obj%ippart=2
      else
        obj%ippart=0
      end if
!
      if (obj%ovrlay) then
        obj%iovrlay=1
      else
        obj%iovrlay=0
      end if
!
      if (obj%ptype.eq.'A Delta-B*           ') then
        obj%iptype=1
      else if (obj%ptype.eq.'Delta-A B*           ') then
        obj%iptype=2
      else if (obj%ptype.eq.'Delta-(AB)*          ') then
        obj%iptype=3
      else if (obj%ptype.eq.'|A|**2               ') then
        obj%iptype=4
      else if (obj%ptype.eq.'|B|**2               ') then
        obj%iptype=5
      else if (obj%ptype.eq.'|A|                  ') then
        obj%iptype=6
      else if (obj%ptype.eq.'|B|                  ') then
        obj%iptype=7
      else
        obj%iptype=0
      end if
!
      if (obj%ntype.eq.'|A|                  ') then
        obj%intype=1
      else if (obj%ntype.eq.'|B|                  ') then
        obj%intype=2
      else if (obj%ntype.eq.'|AB|                 ') then
        obj%intype=3
      else if (obj%ntype.eq.'Radius               ') then
        obj%intype=4
      else
        obj%intype=0
      end if
!
      if (obj%intype.eq.0) then
        obj%iexp=0
      else if (obj%exp.eq.'1.0                  ') then
        obj%iexp=2
      else if (obj%exp.eq.'2.0                  ') then
        obj%iexp=3
      else
        obj%iexp=1
      end if
!
!      Initialize needed variables
!
      max_trcs = obj%numtr
      max_samps = obj%ndpt
      intype = obj%intype

! --> Insert code to verify process parameters here (and/or in traps).


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


! --> Insert code to call internal processes to verify process parameters.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

! take out items that are not needed when an option is selected
      if (intype.eq.0) then
        call pc_put_sensitive_field_flag ('EXP',    .false.)
        call pc_put_sensitive_field_flag ('SMOOTH', .false.)
      else
        call pc_put_sensitive_field_flag ('EXP',    .true.)
        call pc_put_sensitive_field_flag ('SMOOTH', .true.)
      end if 


      call pc_put_options_field('EXP   ', (/'0.5                  ',           &
        '1.0                  ','2.0                  '/) )
      call pc_put_options_field('NTYPE ', (/'No Norm              ',           &
        '|A|                  ','|B|                  ',                       &
        '|AB|                 ','Radius               '/) )
      call pc_put_options_field('PPART ', (/'Real                 ',           &
        'Imag                 ','Overlay              '/) )
      call pc_put_options_field('PTYPE ', (/'AB*                  ',           &
        'A Delta-B*           ','Delta-A B*           ',                       &
        'Delta-(AB)*          ','|A|**2               ',                       &
        '|B|**2               ','|A|                  ',                       &
        '|B|                  '/) )

! --> Delete any of the globals below that have not changed:

      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('EXP   ', obj%exp)
      call pc_put('NTYPE ', obj%ntype)
      call pc_put('OVRLAY', obj%ovrlay)
      call pc_put('PPART ', obj%ppart)
      call pc_put('PTYPE ', obj%ptype)
      call pc_put('SMOOTH', obj%smooth)

! --> Change the control defaults below as appropriate:

      call pc_put_control ('ntapes'       , 0)
      call pc_put_control ('need_request' , .false.)
      call pc_put_control ('need_label'   , .false.)
      call pc_put_control ('twosets'      , .false.)
      call pc_put_control ('nscratch'     , 0)
      call pc_put_control ('nstore'       , 0)
      call pc_put_control ('iftd'         , .false.)
      call pc_put_control ('ndisk'        , 0)
      call pc_put_control ('setup_only'   , .false.)
      call pc_put_control ('parallel_safe', .false.)

! --> Add here any other parameter cache calls such as to set sensitivities.


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


! --> Insert code to initialize variables for execution or deallocate arrays
! --> which will be reallocated below.

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

! --> Insert code to allocate needed permanent memory.
!
!     Set up the work array     
!
      lwa1 = 0
!
!       Now Increase the size of the work buffer
!
! ..... IPT_WHDRS  :  Wanted Header values
      max_whdrs=3
      obj%max_whdrs=max_whdrs;
      obj%ipt_whdrs = lwa1 + 1
      lwa1 = lwa1 + max_whdrs*max_trcs
! ..... IPT_HCI  :  Storage for the real & imaginary parts of AB*
      obj%ipt_hci = lwa1 + 1
      lwa1 = lwa1 + max_samps
      obj%ipt_rvi = lwa1 + 1
      lwa1 = lwa1 + max_samps
! ..... IPT_NORM,IPT_NORM1  :  Storage for the two normalization arrays
      obj%ipt_norm = lwa1 + 1
      lwa1 = lwa1 + max_samps
      obj%ipt_norm1 = lwa1 + 1
      lwa1 = lwa1 + max_samps
!
!   Allocate your permanent memory like this:
!   Allocation errors will be reported to the parameter cache.
!  
      if (lwa1.gt.0) then
        call mem_alloc (obj%wa1, lwa1)
      end if

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

! --> Insert code to initialize anything needed for actual execution of process.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine apin_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


! --> Insert code for parameter traps here.
! --> (EZCPS with the -t option will insert skeleton code for you)


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine apin (obj,ntr,hd,tr)
      type(apin_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

! --> Insert declarations of local variables.
      integer :: i1, ierror, iexp, intype, iovrlay, ip, irstln, ismooth
      integer :: ippart, ipt_hci, ipt_norm, ipt_norm1, ipt_rvi
      integer :: ipt_whdrs, iptype
      integer :: jerror, max_samps, max_whdrs
      integer :: nhdrs, ntrc_gath_in, ntrc_gath_out
      integer :: irst(20)
      real :: cmp_in, data_trace_type_in, data_trace_type_out, samp_int_in
!


!
      character crst*80
!
! --> Insert code for processing logic.
!
!     The following conditional call to the WRAPUP routine should be made
!     before returning from this subroutine:
!
      if (ntr == no_more_traces .or. ntr == fatal_error) then
        call apin_wrapup(obj)
        return
      end if
!
!      set pointers into the work array
!
      ipt_hci=obj%ipt_hci
      ipt_rvi=obj%ipt_rvi
      ipt_norm=obj%ipt_norm
      ipt_norm1=obj%ipt_norm1
      ipt_whdrs=obj%ipt_whdrs
!
!      INITIALIZE
!      (obj%ndpt is number of sample values in a trace)
!      (obj%nwih is number of header words)
!      (obj%numtr is max number of traces input/output)
!      (obj%dt is trace sample interval)
!
      max_samps=obj%ndpt
      nhdrs=obj%nwih
      ntrc_gath_in=ntr
      samp_int_in=obj%dt*1000.0
!
      ippart=obj%ippart
      iovrlay=obj%iovrlay
      iptype=obj%iptype
      intype=obj%intype
      iexp=obj%iexp
      ismooth=obj%smooth
      max_whdrs=obj%max_whdrs;
!
      jerror   = 0
 
      ntrc_gath_out = 0
 
      do i1 = 1, ntrc_gath_in
!
        nhdrs=0
!
        cmp_in = hd( HDR_CURRENT_GROUP, i1 )
        nhdrs=nhdrs+1
        if (nhdrs.le.max_whdrs) then
          ip=ipt_whdrs+max_whdrs*(i1-1)+nhdrs-1
          obj%wa1(ip)=cmp_in
        end if
!
        data_trace_type_in = hd( HDR_USER_49, i1 )
        nhdrs=nhdrs+1
        if (nhdrs.le.max_whdrs) then
          ip=ipt_whdrs+max_whdrs*(i1-1)+nhdrs-1
          obj%wa1(ip)=data_trace_type_in
        end if
!
        nhdrs=nhdrs+1
        if (nhdrs.le.max_whdrs) then
          ip=ipt_whdrs+max_whdrs*(i1-1)+nhdrs-1
          obj%wa1(ip)=i1
          if (i1.eq.ntrc_gath_in) obj%wa1(ip)=1
        end if
      end do
!
!        Set the # of output traces
!
      ntrc_gath_out = 1
      if (ippart.eq.2) ntrc_gath_out = 2
!
!
!        compute some standard avo product indicators
!        from a group of avo standard suites
!
      call apin_ms(ippart, iovrlay, iptype, intype, iexp, ismooth,    &
        max_samps, samp_int_in, obj%wa1(ipt_hci), obj%wa1(ipt_rvi),   &
        obj%wa1(ipt_norm), obj%wa1(ipt_norm1), nhdrs, ntrc_gath_in,   &
        obj%wa1(ipt_whdrs), tr, jerror, irst, irstln)
      if (jerror.ne.0) then
        call string_hh2cc(irst,crst)
        call pc_error(crst)
        ierror = jerror
        return
      end if
!
!        set the trace types of the output headers
!
      do i1 = 1, ntrc_gath_out
!
        nhdrs=0
!
        nhdrs=nhdrs+1
!
        nhdrs=nhdrs+1
        if (nhdrs.le.max_whdrs) then
          ip=ipt_whdrs+max_whdrs*(i1-1)+nhdrs-1
          data_trace_type_out=obj%wa1(ip)
        end if
!
!       do i2 = 1,max_samps,25
!         time=(i2-1)*samp_int_in
!         write(6,9150) i1,i2,time,data_trace_type_out,tr(i2,i1)
!9150     format(' tr',2(i5,1x),3(f10.3,1x))
!       end do
!
!        save the data trace type
!
        hd(HDR_USER_49,i1) = data_trace_type_out
      end do
!
!      set the number of output traces
      ntr = ntrc_gath_out
!
      if ( jerror .ne. 0 )   then
         ierror = jerror
      end if

      end subroutine apin


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine apin_wrapup (obj)
      type(apin_struct),intent(inout) :: obj       ! arguments

! call the end routine

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

! --> Insert any required wrapup code here, including wrapups of
! --> internally-called processes.

      end subroutine apin_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module apin_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

