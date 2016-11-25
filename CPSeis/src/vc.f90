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
! Name       : VC       Vibroseis Correlation      [Former PVC]
! Category   : filters
! Written    : 1986-07-10   by: John Sinton
! Revised    : 2010-02-02   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Correlate vibroseis field data with recorded sweep.
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
! VC correlates vibroseis field data traces with a recorded (or calculated)
! sweep.  This replaces the wavelet embedded in the field data traces with its
! autocorrelation, which is zero phase (and much shorter).  This is also known 
! as matched filtering.  The length of the correlated trace is the length of 
! the original field data trace minus the length of the sweep.
!
! 
! Detailed Operation
!
!  1. VC takes the Fourier Transform of the sweep and the input field data
!     trace.  To avoid time shifting the output, the transformed sweep is 
!     phase shifted consistent with zero time being at the midpoint of the 
!     sweep.
!
!  2. The phase spectrum of the sweep is subtracted from the phase 
!     spectrum of the input trace.  (It would be convolution if it were 
!     added.)
!
!  3. If OPT_SWEEP_AMP = YES, then multiply the amplitude spectrum of the 
!     input trace by the amplitude spectrum of the sweep.  Omit this step 
!     otherwise.  Normally the sweep amplitude spectrum does not contain 
!     useful information.
!
!  4. Optionally apply a frequency filter in the frequency domain.
!
!  5. Inverse Fourier Transform the correlated trace to the time domain.
! 
!
! TSTRT Value
!
! VC assumes that the input traces and the sweep have a TSTRT value of 0.0, ie,
! that the first sample is at zero time.
!
!
! Note on Conversion
!
! VC is a simplified version of the former PVC, omitting seldom used options.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Output Trace Length
!
! VC produces a correlated trace whose length is the length of the original 
! field data trace minus the length of the sweep.  Call TSEL after VC if you 
! wish to adjust the beginning or ending time of the correlated trace.
!
!
! Deleting Traces
!
! The NUM_DEL parameter allows you to delete auxiliary traces from the 
! beginning of the input shot profile after the correlation step.  If 
! LOC_SWEEP = CHANNEL, the sweep will not be automatically deleted.  It will be
! deleted only if it is included in the NUM_DEL traces that VC will delete 
! after the correlation step.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! If LOC_SWEEP = CHANNEL:
!
! Process is a multiple-trace process.
! This process requires traces to be input in shot profile gathers.
!
! If LOC_SWEEP = FILE or CALC:
!
! Process is a single-trace process.
! Traces can be input in any order, gathered or one at a time.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs traces with same gather status as the input traces.
!
! Auxiliary traces may be deleted.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! GATHERED  whether traces are a legitimate gather  used but not changed
! NDPT      number of sample values in trace        changed
! TSTRT     starting time on trace                  must be zero
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
!  1      Sequential Trace Count     Possibly reset
!  4      Number in current gather   Possibly reset
! 25      LAV                        Reset
! 
! Mute headers are ignored.
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!      Date       Author      Description
!      ----       ------      -----------
! 29. 2010-02-02  Stoeckley   Remove requirement that NWIH == 64.
!028. 2006-10-16  D. Glover   Added NULLIFY statements for Intel compiler.
!027. 2006-01-10  B. Menger   Removed Unused Variables.
! 26.  2003-07-21 Stoeckley   Modify code in main subroutine VC to circumvent
!                              a compiler bug while compiling on sol62.
! 25.  2002-10-25 Goodger     Add use getlun_module and string_module.
! 24.  2001-10-18 Stoeckley   Add file selection box and file status message.
! 23.  2001-02-13 Stoeckley   Fix ezgui help warnings.
! 22.  2000-12-08 Stoeckley   Change wrapup flag.
! 21.  2000-09-15 O'Brien     Implemented PATHCHECK for filename handling
!                             Added ntr checking in subroutine vc()
!                             Improved NUM_DEL traps
! 20.  2000-07-19 O'Brien     Fixed bug when handling dead traces
!                             Add HDR_SEQUENCE and HDR_CURRENT_CHANNEL handling
!                             Add messages when
!                               GATHERED==.false. and LOC_SWEEP=='CHANNEL'
! 19.  2000-07-18 O'Brien     Added run time messages to report progress
! 18.  2000-06-30 O'Brien     Full F90 conversion
! 17.  1998-12-15 Goodger     Begin using the fortran90 compiler.         
! 16.  1993-09-28 Goodger     Recompile with static option.               
! 15.  1992-05-08 Peterson    If the sweep is not retained, we'll have to 
!                             retain the proper HEAD.
! 14.  1992-05-06 Peterson    If CSWP=YES, We may now specify RSWP=NONE.
!                             This says that there is not a sweep at the  
!                             front of each group of data. Consequentialy
!                             PVC will not try to determine zero time by
!                             correlating the computed sweep with the first
!                             trace of a group. Zero time is first sample.     
! 13.  1992-03-03 Troutt      Fix call to STRINI at setup for sweep trace.
!                             Use array "WRK" for headers - can't use HEAD
!                             because its address is not yet defined.
! 12.  1991-11-18 Peterson    Correction to storage memory start of array
!                             CSWEEP.
! 11.  1991-11-15 Peterson    Fix IOWN to be all zeroes; IOWN(1 and 2)=0.
! 10.  1991-06-14 Howard      Fix bug for UNICOS--NZPT not saved.
!  9.  1991-01-16 Peterson-Cox Add PMF and DSIG options.
!  8.  1990-12-26 Peterson    Add retain sweep option.
!  7.  1990-07-12 Peterson    Add computed sweep option.
!  6.  1988-08-16 Sinton      Updated documentation and add NCODE.
!  5.  1988-04-23 Baumel      Add CPSPRT calls.
!  4.  1986-11-13 Sinton      Changed time shift to a circular shift.
!  3.  1986-08-22 Sinton      Converted algorithm to freqency domain
!                             convolution (this is a MAJOR change).
!                             Added bandpass filtering, decimination,
!                             and correlation with  a PARP sweep.
!  2.  1986-07-14 Sinton      A call to RDEOF was added.
!  1.  1986-07-10 Sinton      made call to PVCS reset the value of
!                             NDPT=NCTP as was already in the call to PVC.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS               
!
! No known limitations.
!
! A compiler bug was found on platform sol62.  The code was modified to
! circumvent the bug.  See comments in the main VC subroutine for details.
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
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST    yes      whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
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
! For clarity, to facilitate maintenance, and to maintain consistency with
! variable names in other filtering processes, the following user_parameter
! to program_variable_name translation is used:
!
!        USER_PARAMETER       PROGRAM_VARIABLE_NAME
!        ----------------     ---------------------
!        FREQ_LOW_NONE        f1
!        FREQ_LOW_FULL        f2
!        FREQ_HIGH_NONE       f3
!        FREQ_HIGH_FULL       f4
!        PHASE                ph
!        FILTER_TYPE          ftyp
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS VC Process/NC=80>
!
! Correlate vibroseis field data with recorded sweep.
!
! LOC_SWEEP=`CCCCCC  LEN_SWEEP=~~~`FFFFFFFFFFF
!
! OPT_SWEEP_AMP=`CC  NUM_TR_SWEEP=`IIIIIIII
!
! Select PATHNAME[pathname]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [pathname_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! NUM_DEL=`IIIIIIII
!
!
! Bandpass Filter Parameters[/L]
!    FILTER_TYPE=`CCCCCCCCC
!    FREQ_LOW_NONE= `FFFFFFFFFFF  FREQ_LOW_FULL= `FFFFFFFFFFF
!    FREQ_HIGH_FULL=`FFFFFFFFFFF  FREQ_HIGH_NONE=`FFFFFFFFFFF
!    PHASE=`FFFFFFFFFFF  
!
!
! Calculated Sweep Parameters[/L]
!    FREQ_BEG= `FFFFFFFFFFF  FREQ_END= `FFFFFFFFFFF
!    TAPER_BEG=`FFFFFFFFFFF  TAPER_END=`FFFFFFFFFFF
!<PARMS PATHNAME[/ML=120/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!
!<Help KEYWORD="LOC_SWEEP">
!<Tip> Location of the recorded or calculated sweep. </Tip>
! Default = CHANNEL
! Allowed = CHANNEL  
! Allowed = FILE
! Allowed = CALC
! If LOC_SWEEP = CHANNEL, then the sweep for each input shot profile is read 
! from one of the initial auxiliary traces in each profile. 
!
! If LOC_SWEEP = FILE, then a single sweep is read from a TROT file and used 
! to correlate all the input traces.  
!
! If LOC_SWEEP = CALC, then a single sweep will be calculated and used to 
! correlate all the input traces.
!</Help>
!
!<Help KEYWORD="LEN_SWEEP">
!<Tip> Length of the recorded or calculated sweep, in seconds. </Tip>
! Default = -
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="OPT_SWEEP_AMP">
!<Tip> Use the amplitude spectrum of the sweep in the correlation? </Tip>
! Default = NO
! Allowed = YES/NO
! If OPT_SWEEP_AMP = NO, then the amplitude spectrum of the sweep will not be 
! used in the correlation (the sweep amplitude spectrum is assumed to be 1.0 
! for all frequencies).  This is the usual choice.
!
! If OPT_SWEEP_AMP = YES, then the amplitude spectrum of the correlated trace 
! will be equal to the amplitude spectrum of the input trace multiplied by the 
! amplitude spectrum of the sweep (normal correlation).
!</Help>
!
!<Help KEYWORD="NUM_TR_SWEEP">
!<Tip> Sequential trace number of the sweep trace in the shot profile. </Tip>
! Default = 1
! Allowed = int > 0
! Active if LOC_SWEEP = CHANNEL only.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname for file containing the recorded sweep. </Tip>
! Default = NONE
! Allowed = char
! The sweep trace must be the first trace in the file.
!
! Active if LOC_SWEEP = FILE only.
!</Help>
!
!<Help KEYWORD="NUM_DEL">
!<Tip> Delete NUM_DEL traces from the start of an input shot profile. </Tip>
! Default = 0
! Allowed = int >= 0
! Number of auxiliary traces at the start of an input shot profile.  VC will 
! delete these traces after the correlation step.
!
! If LOC_SWEEP = CHANNEL, the sweep will not be automatically deleted.  It will
! be deleted only if it is included in the NUM_DEL traces that VC will delete 
! after the correlation step.
!</Help>
!
!----------------------parameters for frequency filter--------------------------
!
!           (These are the Standard Frequency Filtering Parameters)
!
!<Help KEYWORD="FILTER_TYPE">
!<Tip> Type of trapezoid frequency filter to apply (or none). </Tip>
! Default = NONE   
! Allowed = BANDPASS    (Pass frequencies between tapers.)
! Allowed = LOWPASS     (Pass frequencies from 0.0 to high taper.)
! Allowed = HIGHPASS    (Pass frequencies from low taper to Nyquist.)
! Allowed = ALLPASS     (Pass all frequencies - may change phase.)
! Allowed = BANDREJECT  (Reject frequencies between tapers.)
! Allowed = NONE        (No trapezoid filter is applied.)
!
! In the FILTER_TYPE descriptions below, frequency parameters are listed in 
! the required order, from low frequency to high frequency.
!
! If FILTER_TYPE=BANDPASS, then reject between 0.0 frequency and FREQ_LOW_NONE,
! pass between FREQ_LOW_FULL and FREQ_HIGH_FULL, reject between FREQ_HIGH_NONE 
! and Nyquist, with linear tapers between pass and reject regions.
!
! If FILTER_TYPE=LOWPASS, then pass between 0.0 frequency and FREQ_HIGH_FULL, 
! reject between FREQ_HIGH_NONE and Nyquist, with a linear taper between pass 
! and reject regions.
!
! If FILTER_TYPE=HIGHPASS, then reject between 0.0 frequency and FREQ_LOW_NONE,
! pass between FREQ_LOW_FULL and Nyquist, with a linear taper between pass and 
! reject regions.
!
! If FILTER_TYPE=ALLPASS, then pass all frequencies from 0.0 frequency to
! Nyquist.  (This option is supplied so that phase rotation may be applied, with
! the PHASE parameter, without affecting the amplitude spectrum.)
!
! If FILTER_TYPE=BANDREJECT, then pass between 0.0 frequency and FREQ_LOW_FULL,
! reject between FREQ_LOW_NONE and FREQ_HIGH_NONE, pass between FREQ_HIGH_FULL 
! and Nyquist, with linear tapers between pass and reject regions.  PHASE is 
! hardwired to 0.0 for this option.
!
! If FILTER_TYPE=NONE, then no trapezoid filter is applied to the data.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Frequency where low frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! Restrictions on numerical order of frequency parameters is shown in the 
! FILTER_TYPE option descriptions.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Frequency where low frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=FREQ_LOW_NONE
! Restrictions on numerical order of frequency parameters is shown in the 
! FILTER_TYPE option descriptions.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> Frequency where high frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=FREQ_LOW_FULL
! Restrictions on numerical order of frequency parameters is shown in the 
! FILTER_TYPE option descriptions.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> Frequency where high frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=FREQ_HIGH_FULL
! Restrictions on numerical order of frequency parameters is shown in the 
! FILTER_TYPE option descriptions.
!</Help>
!
!<Help KEYWORD="PHASE">
!<Tip> Constant phase, in degrees, to be added to the phase spectrum. </Tip>
! Default = 0.0
! Allowed = real
! PHASE is hardwired to 0.0 for FILTER_TYPE = BANDREJECT or NONE.
!</Help>

!----------------------parameters for calculated sweep--------------------------
!
!                     (Active only if LOC_SWEEP = CALC)
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Initial frequency for calculated linear sweep, in Hz. </Tip>
! Default = 5.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="FREQ_END">
!<Tip> Final frequency for calculated linear sweep, in Hz. </Tip>
! Default = 85.0
! Allowed = real > FREQ_BEG
!</Help>
!
!<Help KEYWORD="TAPER_BEG">
!<Tip> Length of initial taper for calculated linear sweep, in seconds. </Tip>
! Default = 0.4
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="TAPER_END">
!<Tip> Length of final taper for calculated linear sweep, in seconds. </Tip>
! Default = 0.4Hz
! Allowed = real > 0.0
!</Help>
!
!</HelpSection>

!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module vc_module

      use getlun_module
      use string_module
      use fft_module
      use bandps_module
      use pc_module
      use sizeof_module
      use named_constants_module
      use pathcheck_module
      use pathchoose_module
      use trin_module
      use lav_module

      implicit none

      private
      public :: vc_create
      public :: vc_initialize
      public :: vc_update
      public :: vc_delete

!<execute_only>

      public :: vc           ! main trace processing routine.
      public :: vc_wrapup

!</execute_only>

      character(len=100),public,save :: VC_IDENT = &
             '$Id: vc.f90,v 1.28 2006/10/17 13:45:49 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: vc_struct

        private
        logical              :: skip_wrapup    ! wrapup flag

        ! Sweep parameters
        character(len=7)     :: loc_sweep      ! Indicates the source of sweep
        real                 :: len_sweep      ! Length of sweep (seconds)
        character(len=3)     :: opt_sweep_amp  ! Flag for sweep amp spect use
        integer              :: num_tr_sweep   ! Trace # with sweep
        character(len=FILENAME_LENGTH) :: pathname ! file with sweep
        integer              :: num_del        ! # of leading traces to delete
        real                 :: freq_beg       ! for computed sweep
        real                 :: freq_end       ! for computed sweep
        real                 :: taper_beg      ! for computed sweep
        real                 :: taper_end      ! for computed sweep

        ! Filter parameters
        character(len=10)    :: ftyp           ! Type of filter
        real                 :: f1             ! Low end zero pass
        real                 :: f2             ! Low end full pass
        real                 :: f3             ! High end full pass
        real                 :: f4             ! High end zero pass
        real                 :: ph             ! Phase of filter

        integer              :: nwih              ! globals.
        integer              :: ndpt_in,ndpt_out  ! globals.
        integer              :: numtr             ! globals.
        real                 :: dt
        logical              :: gathered

        integer              :: prtlu          ! a place to print info
        integer              :: infolu         ! a place to store messages
        integer              :: ngather        ! counts gathers passed thru
        character(len=16)    :: fmta           ! format for messages
        integer              :: ndpt_sweep     ! # of points in the sweep
        integer              :: npo2           ! Power of 2 used for ffts
        integer              :: nw             ! NumSmp in freq domain
        real                 :: df             ! SmpInt in freq domain (Hz)
        real                 :: dw             ! SmpInt in freq domain (w)
        real                 :: fnyq           ! Nyquist frequency
        real                 :: fft_norm       ! FFT normalization factor
        real,pointer         :: rsweep(:)      ! Real sweep
        complex,pointer      :: csweep(:)      ! Complex sweep
        complex,pointer      :: cfilter(:)     ! Complex filter array
        real,pointer         :: rtrace(:)      ! Real trace
        complex,pointer      :: ctrace(:)      ! Complex trace
        complex,pointer      :: fctrace(:)     ! Filtered complex trace
        type(fft_struct),pointer :: rcfft      ! Real to complex fft object
        type(fft_struct),pointer :: crfft      ! Complex to real fft object
        type(trin_struct),pointer:: trin       ! object for trin
        integer              :: sequence       ! counter for HDR_SEQUENCE  
        type(pathchoose_struct),pointer :: dialog

      end type vc_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(vc_struct),pointer,save :: object      ! needed for traps.

      integer,parameter              :: ftyp_nopt=6          ! used in setup
      character(len=10),save         :: ftyp_options(6)      ! used in traps
      integer,parameter              :: loc_sweep_nopt=3     ! used in setup
      character(len=7),save          :: loc_sweep_options(3) ! used in setup
      integer,parameter              :: sweep_amp_nopt=2     ! used in setup
      character(len=3),save          :: sweep_amp_options(2) ! used in setup

      data ftyp_options  /'NONE','BANDPASS','HIGHPASS','LOWPASS', &
                          'ALLPASS','BANDREJECT'/

      data loc_sweep_options  /'CHANNEL','FILE','CALC'/
      data sweep_amp_options  /'YES','NO'/

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine vc_create (obj)
      implicit none
      type(vc_struct),pointer :: obj       ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers
      nullify (obj%rsweep)
      nullify (obj%csweep)
      nullify (obj%cfilter)
      nullify (obj%rtrace)
      nullify (obj%ctrace)
      nullify (obj%fctrace)
      nullify (obj%rcfft)
      nullify (obj%crfft)
      nullify (obj%trin)
      nullify (obj%dialog) ! jpa
   
      call pathchoose_create (obj%dialog, 'pathname', 'trc*')
      call vc_initialize     (obj)
      return
      end subroutine vc_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine vc_delete (obj)
      implicit none
      type(vc_struct),pointer :: obj       ! arguments

!<execute_only>
      call vc_wrapup (obj)
!</execute_only>

      if (associated(obj%rsweep))   deallocate (obj%rsweep)
      if (associated(obj%csweep))   deallocate (obj%csweep)
      if (associated(obj%cfilter))  deallocate (obj%cfilter)
      if (associated(obj%rtrace))   deallocate (obj%rtrace)
      if (associated(obj%ctrace))   deallocate (obj%ctrace)
      if (associated(obj%fctrace))  deallocate (obj%fctrace)
      if (associated(obj%rcfft))    call fft_delete (obj%rcfft)
      if (associated(obj%crfft))    call fft_delete (obj%crfft)
      if (associated(obj%trin))     call trin_delete (obj%trin)

      call pathchoose_delete (obj%dialog)

      deallocate(obj)
      return
      end subroutine vc_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine vc_initialize (obj)
      implicit none
      type(vc_struct),pointer :: obj       ! arguments

! Get global values in initialization
      obj%ndpt_in = INIL
      call pc_get_global ('NDPT', obj%ndpt_in)
      if ( obj%ndpt_in == INIL ) then
        call pc_error('VC: Global param NDPT has not been properly set.')
        return
      endif

      obj%dt = FNIL
      call pc_get_global ('DT', obj%dt)
      if ( obj%dt == FNIL ) then
        call pc_error('VC: Global param DT has not been properly set.')
        return
      endif

      obj%fnyq  = 0.5/obj%dt

! Initialize process variables
      obj%loc_sweep     = 'CHANNEL'
      obj%len_sweep     = real(nint(.667*(obj%ndpt_in-1)*obj%dt))
      obj%opt_sweep_amp = 'NO'
      obj%num_tr_sweep  = 1
      obj%pathname      = PATHCHECK_EMPTY
      obj%num_del       = 0

      obj%ftyp = 'NONE'
      obj%f1   = 0.0
      obj%f2   = 0.0
      obj%f3   = obj%fnyq
      obj%f4   = obj%fnyq
      obj%ph   = 0.0

      obj%freq_beg  =  5.0
      obj%freq_end  = 85.0
      obj%taper_beg =  0.4
      obj%taper_end =  0.4

! Initialize all the dependent variables
      obj%prtlu     = 0
      obj%infolu    = 0
      obj%ngather   = 0
      obj%fmta      = '(a18,i4,a18,i10)'
      obj%npo2      = 0
      obj%nw        = 0
      obj%dw        = 0.0
      obj%df        = 0.0
      obj%fft_norm  = 1.0
      obj%sequence  = 0

      call vc_update (obj)

      return
      end subroutine vc_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine vc_update (obj)
      implicit none
      type(vc_struct),target :: obj            ! Arguments

      integer     :: j,nstore ! Local
      integer     :: ier1,ier2,ier3,ier4,ier5,ier6  ! Local
      integer     :: update_state                   ! Local
      integer     :: ier,ibytes                     ! Local

      ! Must pass variables with proper ranks as dummy argument to trin
      integer          :: ONE = 1
      double precision :: hd_sweep(HDR_NOMINAL_SIZE,1)
      real             :: tmp_sweep(obj%ndpt_in,1)

      real, parameter :: TWOPI = 2.0*PI

      real               :: tmp_f1, tmp_f2, tmp_f3, tmp_f4, tmp_ph
      integer            :: result
      character(len=98)  :: message

      real           :: freq_fctr, time, phase
      integer        :: ntaper, istrt

      integer        :: itr, iw
      integer        :: SIZEOF_REAL
      integer        :: SIZEOF_DOUBLE
      integer        :: SIZEOF_COMPLEX

!---------------------------------------

      SIZEOF_REAL    = sizeof(1.0)
      SIZEOF_DOUBLE  = sizeof(1.0d0)
      SIZEOF_COMPLEX = sizeof(cmplx(1.0,1.0))

      object => obj                ! needed for traps.
      obj%skip_wrapup = .true.
      update_state = pc_get_update_state()

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if (pathchoose_update(obj%dialog, obj%pathname)) return

! First retrieve globals
      call pc_get_global ('NDPT'     , obj%ndpt_in )
      call pc_get_global ('NWIH'     , obj%nwih    )
      call pc_get_global ('DT'       , obj%dt      )
      call pc_get_global ('GATHERED' , obj%gathered)
      call pc_get_global ('NUMTR'    , obj%numtr   )

      if ( obj%ndpt_in <= 0 ) then
        call pc_error('VC: Global parameter NDPT<=0. Nothing to filter.')
      endif
!!!!  if ( obj%nwih /= HDR_NOMINAL_SIZE ) then
!!!!    call pc_error('VC: Global parameter NWIH /= HDR_NOMINAL_SIZE.')
!!!!  endif
      if ( obj%dt <= 0.0 ) then
        call pc_error('VC: Global parameter DT <= 0.0.')
      endif

! Now retrieve user paramerters
      call pc_get ('LOC_SWEEP'     , obj%loc_sweep    , vc_trap)
      call pc_get ('LEN_SWEEP'     , obj%len_sweep    , vc_trap)
      call pc_get ('OPT_SWEEP_AMP' , obj%opt_sweep_amp, vc_trap)
      call pc_get ('NUM_TR_SWEEP'  , obj%num_tr_sweep , vc_trap)
      call pc_get ('PATHNAME'      , obj%pathname     , vc_trap)
      call pc_get ('NUM_DEL'       , obj%num_del      , vc_trap)
      call pc_get ('FILTER_TYPE'   , obj%ftyp         , vc_trap)
      call pc_get ('FREQ_LOW_NONE' , obj%f1           , vc_trap)
      call pc_get ('FREQ_LOW_FULL' , obj%f2           , vc_trap)
      call pc_get ('FREQ_HIGH_FULL', obj%f3           , vc_trap)
      call pc_get ('FREQ_HIGH_NONE', obj%f4           , vc_trap)
      call pc_get ('PHASE'         , obj%ph           , vc_trap)
      call pc_get ('FREQ_BEG'      , obj%freq_beg     , vc_trap)
      call pc_get ('FREQ_END'      , obj%freq_end     , vc_trap)
      call pc_get ('TAPER_BEG'     , obj%taper_beg    , vc_trap)
      call pc_get ('TAPER_END'     , obj%taper_end    , vc_trap)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! Verify presence and veracity of PATHNAME
   !  if ( pc_verify_scalar('PATHNAME') ) then
   !    call pathcheck (KEYWORD='PATHNAME', PATHNAME=obj%pathname, &
   !                    EXT='', REQUIRED=.false., SCREEN='VC',     &
   !                    show=PATHCHECK_INFO_INPUT)
   !  endif
   !  if ( pc_verify_end() .and. obj%loc_sweep == 'FILE' ) then
   !    call pathcheck (KEYWORD='PATHNAME', PATHNAME=obj%pathname, &
   !                    EXT='', REQUIRED=.true., SCREEN='VC',     &
   !                    show=PATHCHECK_INFO_INPUT)
   !  endif

      call pathcheck (KEYWORD='PATHNAME', PATHNAME=obj%pathname, &
                      EXT='', REQUIRED=(obj%loc_sweep=='FILE'),  &
                      SCREEN='VC', show=PATHCHECK_INFO_INPUT)

! Get the fft size
      obj%npo2 = 8
      do while ( obj%npo2 < obj%ndpt_in )
        obj%npo2 = obj%npo2*2
      enddo
      obj%fft_norm = 1.0/obj%npo2

! Set the nw and dw for frequency domain filters.
      obj%nw = obj%npo2/2 + 1
      obj%df = obj%fnyq/(obj%nw-1)
      obj%dw = TWOPI*obj%df

! Get the number of samples in the sweep and in the correated output
      obj%ndpt_sweep = nint(obj%len_sweep/obj%dt) + 1
      obj%ndpt_out   = obj%ndpt_in - obj%ndpt_sweep + 1

! Run the bandpass parameter checker
! The way bandps_check() changes values isn't very friendly on the
! front end so copies of the frequency points are used in place of
! the users frequencies
      if ( obj%ftyp /= 'NONE' ) then
        tmp_f1 = obj%f1
        tmp_f2 = obj%f2
        tmp_f3 = obj%f3
        tmp_f4 = obj%f4
        tmp_ph = obj%ph
        call bandps_check &
              (result, message, obj%fnyq, obj%ftyp, &
               tmp_f1, tmp_f2, tmp_f3, tmp_f4, tmp_ph)

        select case (result)
          case (BANDPS_ERROR);    call pc_error(message)
          case (BANDPS_ENDERROR); call pc_error(message)
        end select
      endif

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

! Create the object for TRIN as needed to flag any errors there
! Only do this on frontend updates
      if ( update_state == PC_FRONTEND ) then
        if ( obj%loc_sweep == 'FILE' ) then
          call pc_clear
          call pc_put('PATHNAME', obj%pathname)
          if ( associated(obj%trin) ) then
            call trin_update(obj%trin)
          else
            call trin_create(obj%trin)
          endif
          call pc_restore
        endif
      endif

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field &
             ('LOC_SWEEP', loc_sweep_options, loc_sweep_nopt)
      call pc_put_options_field &
             ('OPT_SWEEP_AMP', sweep_amp_options, sweep_amp_nopt)
      call pc_put_options_field &
             ('FILTER_TYPE', ftyp_options, ftyp_nopt)

      call pc_put ('LOC_SWEEP'     , obj%loc_sweep    )
      call pc_put ('LEN_SWEEP'     , obj%len_sweep    )
      call pc_put ('OPT_SWEEP_AMP' , obj%opt_sweep_amp)
      call pc_put ('NUM_TR_SWEEP'  , obj%num_tr_sweep )
      call pc_put ('PATHNAME'      , obj%pathname     )
      call pc_put ('NUM_DEL'       , obj%num_del      )
      call pc_put ('FILTER_TYPE'   , obj%ftyp         )
      call pc_put ('FREQ_LOW_NONE' , obj%f1           )
      call pc_put ('FREQ_LOW_FULL' , obj%f2           )
      call pc_put ('FREQ_HIGH_FULL', obj%f3           )
      call pc_put ('FREQ_HIGH_NONE', obj%f4           )
      call pc_put ('PHASE'         , obj%ph           )
      call pc_put ('FREQ_BEG'      , obj%freq_beg     )
      call pc_put ('FREQ_END'      , obj%freq_end     )
      call pc_put ('TAPER_BEG'     , obj%taper_beg    )
      call pc_put ('TAPER_END'     , obj%taper_end    )


!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!

      nstore = 4*obj%nw*SIZEOF_COMPLEX
      nstore = nstore + 2*(obj%npo2+2)*SIZEOF_REAL
      nstore = nstore + 2*fft_mem_usage(obj%npo2,1)*SIZEOF_REAL
      ! convert bytes to words
      nstore = nstore/SIZEOF_REAL

      call pc_put_control ('NSTORE',       nstore      )
      call pc_put_global  ('NDPT'  ,       obj%ndpt_out)
      call pc_put_control ('NEED_REQUEST', 'YES'       )


!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!

      if ( update_state == PC_GUI .or. update_state == PC_FRONTEND ) then
        call vc_set_sensitivities(obj)
      endif
      if ( update_state == PC_FRONTEND ) then
        if ( obj%loc_sweep=='CHANNEL' .and. .not.obj%gathered ) then
          call pc_warning("VC: Data are not GATHERED, yet LOC_SWEEP &
                          &==CHANNEL. This will produce an autocorrelation &
                          &for each trace.")
          call pc_warning("VC: To correlate shot profiles with embedded &
                          &sweeps, you should GATHER your data first.")
        endif
      endif


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

! Run a few miscellaneous traps before launching into the calculations
!   All removed for now.
      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! get an i/o units for printing
      obj%prtlu = pc_get_lun()

! get an i/o units for run time info
      call getlun(obj%infolu,ier)
      if (ier /= 0) then
        call pc_error ("VC failed to get an i/o unit for run time info.")
      else
        open (unit=obj%infolu, status='SCRATCH', iostat=ier, &
              access='SEQUENTIAL', action='READWRITE')
        if (ier /= 0) then
          call pc_error ("VC had trouble opening file for run time info.")
        endif
      endif

! Create forward and reverse fft objects
      ier = fft_create (obj%rcfft,-1,obj%npo2,'rtoc')
      ier = fft_create (obj%crfft, 1,obj%npo2,'ctor')

! Perform a final check on the filter
! This time allow bandps_check() to alter values and report the INFO message
      call bandps_check &
            (result, message, obj%fnyq, obj%ftyp, &
             obj%f1, obj%f2, obj%f3, obj%f4, obj%ph)

      select case (result)
        case (BANDPS_INFO);     call pc_info(message)
        case (BANDPS_ERROR);    call pc_error(message)
        case (BANDPS_ENDERROR); call pc_error(message)
      end select

! Allocate the traces and arrays carried by vc_struct
      allocate (obj%rsweep(obj%npo2+2) , stat= ier1 )
      allocate (obj%csweep(obj%nw)     , stat= ier2 )
      allocate (obj%cfilter(obj%nw)    , stat= ier3 )
      allocate (obj%rtrace(obj%npo2+2) , stat= ier4 )
      allocate (obj%ctrace(obj%nw)     , stat= ier5 )
      allocate (obj%fctrace(obj%nw)    , stat= ier6 )

      ibytes = 2*(obj%npo2+2)*SIZEOF_REAL
      ibytes = ibytes + 4*(obj%nw)*SIZEOF_COMPLEX
      ibytes = ibytes + 2*fft_mem_usage(obj%npo2,1)*SIZEOF_REAL
      if (ier1/=0 .or. ier2/=0 .or. ier3/=0 .or.  &
          ier4/=0 .or. ier5/=0 .or. ier6/=0     ) then
        call pc_error('VC: Unable to allocate memory for workspace', &
                      ibytes,' bytes.')
        return
      else
        call pc_print('VC: Allocated ', ibytes, &
                      ' bytes for filter and workspace.')
      endif

! Initialize vectors that get used in this update routine
      obj%rsweep(:)  = 0.0
      obj%csweep(:)  = cmplx(0.0,0.0)
      obj%cfilter(:) = cmplx(1.0,0.0)

! Precompute the basic filter
      call bandps ( obj%cfilter(:), obj%nw, obj%df, obj%ftyp, &
                    obj%f1, obj%f2, obj%f3, obj%f4, obj%ph )

! Read or precompute the sweep if it's not one of the data channels
      if (obj%loc_sweep /= 'CHANNEL') then

        select case (obj%loc_sweep)

          case ('FILE')
            ! In the future we might actually use NUM_TR_SWEEP to locate
            ! a precalculated sweep in the FILE. For now, only the 1st trace
            ! is allowed.
            obj%num_tr_sweep = 1
            do itr=1,obj%num_tr_sweep
              call trin(obj%trin,ONE,hd_sweep,tmp_sweep)
            enddo
            obj%rsweep(1:obj%ndpt_sweep) = tmp_sweep(1:obj%ndpt_sweep,1)

          case ('CALC')
            freq_fctr = (obj%freq_end-obj%freq_end)/(2.0*obj%len_sweep)
            do j=1,obj%ndpt_sweep
              time = obj%dt*(j-1)
              obj%rsweep(j) = sin(TWOPI*time*(obj%freq_beg+(time*freq_fctr)))
            enddo

            ! Taper values at the beginning of the sweep
            ntaper = int(obj%taper_beg/obj%dt) + 1
            obj%rsweep(1:ntaper) = &
                      obj%rsweep(1:ntaper) &
                    * (/(j,j=0,ntaper-1)/)/ntaper

            ! Taper values at the end of the sweep
            ntaper = int(obj%taper_end/obj%dt) + 1
            istrt = obj%ndpt_sweep-ntaper+1
            obj%rsweep(istrt:obj%ndpt_sweep) = &
                      obj%rsweep(istrt:obj%ndpt_sweep) &
                    * (/(j,j=ntaper-1,0,-1)/)/ntaper

        endselect

        ! Reverse the sweep in time so a complex multiply will be a
        ! correlation rather than a convolution.
        obj%rsweep(1:obj%ndpt_sweep) = obj%rsweep(obj%ndpt_sweep:1:-1)

        ! Transform the sweep to the frequency domain
        call fft_rc_transform(obj%rcfft,obj%rsweep,obj%csweep)

        ! By default, change the amplitude spectrum of the sweep to 1.0
        if ( obj%opt_sweep_amp == 'NO' ) then
          do iw = 2,obj%nw-1
            phase = atan2(aimag(obj%csweep(iw)),real(obj%csweep(iw)))
            obj%csweep(iw) = cmplx(cos(phase),sin(phase))
          enddo
        endif

        ! Fix the endpoints to 0.0
        obj%csweep(1)      = cmplx(0.0,0.0)
        obj%csweep(obj%nw) = cmplx(0.0,0.0)

        ! Apply the filter to the sweep up front
        obj%csweep(:) = obj%csweep(:) * obj%cfilter(:)

      endif

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine vc_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
   
      subroutine vc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

!--------------------------------------------------------
   
      keyword_select: select case (keyword)

        case ('LOC_SWEEP') keyword_select
          call string_to_upper(object%loc_sweep)
          if ( all(loc_sweep_options/=object%loc_sweep)) then
            call pc_warning &
                ('VC: Invalid value for LOC_SWEEP, '//object%loc_sweep//'. &
                 &Resetting to default, CHANNEL.')
            object%loc_sweep = 'CHANNEL'
          endif
          if ( object%loc_sweep=='CHANNEL' .and. .not.object%gathered ) then
            call pc_warning("VC: Data are not GATHERED, yet LOC_SWEEP&
                            &==CHANNEL. This will produce an autocorrelation &
                            &for each trace.")
            call pc_warning("VC: To correlate shot profiles with embedded &
                            &sweeps, you should GATHER your data first.")
          endif

        case ('LEN_SWEEP') keyword_select
          if ( object%len_sweep <= 0.0 ) then
            call pc_error ('VC: LEN_SWEEP must be .GT. 0.0')
          endif

        case ('OPT_SWEEP_AMP') keyword_select
          call string_to_upper(object%opt_sweep_amp)
          if ( all(sweep_amp_options/=object%opt_sweep_amp)) then
            call pc_warning &
                ('VC: Invalid value for OPT_SWEEP_AMP, '&
                 &//object%opt_sweep_amp//'. Resetting to default, NO.')
            object%opt_sweep_amp = 'NO'
          endif

        case ('NUM_TR_SWEEP') keyword_select
          if ( object%loc_sweep=='CHANNEL' .and. object%num_tr_sweep<=0 ) then
            call pc_error &
              ('VC: NUM_TR_SWEEP must be .GT. 0 when LOC_SWEEP = CHANNEL.')
          endif

        case ('PATHNAME') keyword_select
          if ( object%loc_sweep == 'FILE'     .and. &
               object%pathname  == PATHCHECK_EMPTY ) then
            call pc_error &
              ('VC: PATHNAME must be specified when LOC_SWEEP = FILE.')
          endif

        case ('NUM_DEL') keyword_select
          if ( object%num_del<0 ) then
            call pc_warning &
              ('VC: NUM_DEL must be .GE. 0. Resetting to default, 0.')
            object%num_del = 0
          endif
          if ( object%num_del > 0 .and. .not.object%gathered ) then
            call pc_warning &
              ("VC: NUM_DEL must be .EQ. 0 when data are not GATHERed. &
               &Resetting to default, 0.")
            object%num_del = 0
          endif
          if ( object%num_del >= object%numtr ) then
            call pc_warning &
              ("VC: NUM_DEL must be .LT. NUMTR. Resetting to default, 0.")
            object%num_del = 0
          endif

        case ('FILTER_TYPE') keyword_select
          call string_to_upper(object%ftyp)
          if ( all(ftyp_options/=object%ftyp)) then
            call pc_warning &
                ('VC: Invalid value for FILTER_TYPE, '//object%ftyp//'. &
                 &Resetting to default, BANDPASS.')
            object%ftyp = 'BANDPASS'
          endif

        case ('FREQ_LOW_NONE') keyword_select
          if (object%f1 ==   FNIL    ) object%f1 = 0.0
          if (object%f1 <     0.0    ) object%f1 = 0.0
          if (object%f1 > object%fnyq) object%f1 = object%fnyq

        case ('FREQ_LOW_FULL') keyword_select
          if (object%f2 ==   FNIL    ) object%f2 = 0.0
          if (object%f2 <     0.0    ) object%f2 = 0.0
          if (object%f2 > object%fnyq) object%f2 = object%fnyq

        case ('FREQ_HIGH_FULL') keyword_select
          if (object%f3 ==   FNIL    ) object%f3 = object%fnyq
          if (object%f3 <     0.0    ) object%f3 = 0.0
          if (object%f3 > object%fnyq) object%f3 = object%fnyq

        case ('FREQ_HIGH_NONE') keyword_select
          if (object%f4 ==   FNIL    ) object%f4 = object%fnyq
          if (object%f4 <     0.0    ) object%f4 = 0.0
          if (object%f4 > object%fnyq) object%f4 = object%fnyq

        case ('FREQ_BEG') keyword_select
          if (object%freq_beg < 0.0     .or. &
              object%freq_beg > object%fnyq) then
            call pc_warning &
                ('VC: Invalid value for FREQ_BEG, ',object%freq_beg, &
                 'Resetting to default, 5.')
            object%freq_beg = 5.0
          endif

        case ('FREQ_END') keyword_select
          if (object%freq_end < 0.0     .or. &
              object%freq_end > object%fnyq) then
            call pc_warning &
                ('VC: Invalid value for FREQ_END, ',object%freq_end, &
                 'Resetting to MIN(Nyquist,85).')
            object%freq_end = min(object%fnyq,85.0)
          endif

        case ('TAPER_BEG') keyword_select
          if (object%taper_beg < 0.0     .or. &
              object%taper_beg > object%len_sweep) then
            call pc_warning &
                ('VC: Invalid value for TAPER_BEG, ',object%taper_beg, &
                 'Resetting to MIN(LEN_SWEEP,0.4)')
            object%taper_beg = min(object%len_sweep,0.4)
          endif

        case ('TAPER_END') keyword_select
          if (object%taper_end < 0.0     .or. &
              object%taper_end > object%len_sweep) then
            call pc_warning &
                ('VC: Invalid value for TAPER_END, ',object%taper_end, &
                 'Resetting to MIN(LEN_SWEEP,0.4)')
            object%taper_end = min(object%len_sweep,0.4)
          endif

      end select keyword_select

      return
      end subroutine vc_trap


!*******************************************************************************
! Subroutine to handle all the GUI sensitivity settings during update.
!*******************************************************************************
      subroutine vc_set_sensitivities(obj)
      implicit none
      type(vc_struct) :: obj       ! arguments
!----------------------------------------------------------------

      ! These are the sensitivity settings at initializaion
      call pc_put_sensitive_field_flag ('LOC_SWEEP'      , .true. )
      call pc_put_sensitive_field_flag ('LEN_SWEEP'      , .true. )
      call pc_put_sensitive_field_flag ('NUM_TR_SWEEP'   , .true. )
      call pc_put_sensitive_field_flag ('NUM_DEL'        , .true. )
      call pc_put_sensitive_field_flag ('OPT_SWEEP_AMP'  , .true. )
      call pc_put_sensitive_field_flag ('PATHNAME'       , .false.)
      call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false.)
      call pc_put_sensitive_field_flag ('PATHNAME_INFO'  , .false.)
      call pc_put_sensitive_field_flag ('FREQ_BEG'       , .false.)
      call pc_put_sensitive_field_flag ('FREQ_END'       , .false.)
      call pc_put_sensitive_field_flag ('TAPER_BEG'      , .false.)
      call pc_put_sensitive_field_flag ('TAPER_END'      , .false.)
      call pc_put_sensitive_field_flag ('FILTR_TYPE'     , .true. )
      call pc_put_sensitive_field_flag ('FREQ_LOW_NONE'  , .false.)
      call pc_put_sensitive_field_flag ('FREQ_LOW_FULL'  , .false.)
      call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL' , .false.)
      call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE' , .false.)
      call pc_put_sensitive_field_flag ('PHASE'          , .false.)

      ! Handle variations that come with different values of LOC_SWEEP
      select case (obj%loc_sweep)

        case ('CHANNEL')
          !This is the default as set above.

        case ('FILE')
          call pc_put_sensitive_field_flag ('NUM_TR_SWEEP'   , .false.)
          call pc_put_sensitive_field_flag ('PATHNAME'       , .true. )
          call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .true. )
          call pc_put_sensitive_field_flag ('PATHNAME_INFO'  , .true. )

        case ('CALC')
          call pc_put_sensitive_field_flag ('NUM_TR_SWEEP'   , .false.)
          call pc_put_sensitive_field_flag ('FREQ_BEG'       , .true. )
          call pc_put_sensitive_field_flag ('FREQ_END'       , .true. )
          call pc_put_sensitive_field_flag ('TAPER_BEG'      , .true. )
          call pc_put_sensitive_field_flag ('TAPER_END'      , .true. )

      end select

      ! Handle variations that come with different values of FILTER_TYPE
      select case (obj%ftyp)

        case ('NONE')
          !This is the default as set above.

        case ('ALLPASS')
          call pc_put_sensitive_field_flag ('FREQ_LOW_NONE'  , .false.)
          call pc_put_sensitive_field_flag ('FREQ_LOW_FULL'  , .false.)
          call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL' , .false.)
          call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE' , .false.)
          call pc_put_sensitive_field_flag ('PHASE'          , .true. )

        case ('BANDPASS','BANDREJECT')
          call pc_put_sensitive_field_flag ('FREQ_LOW_NONE'  , .true. )
          call pc_put_sensitive_field_flag ('FREQ_LOW_FULL'  , .true. )
          call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL' , .true. )
          call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE' , .true. )
          call pc_put_sensitive_field_flag ('PHASE'          , .true. )

        case ('LOWPASS')
          call pc_put_sensitive_field_flag ('FREQ_LOW_NONE'  , .false.)
          call pc_put_sensitive_field_flag ('FREQ_LOW_FULL'  , .false.)
          call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL' , .true. )
          call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE' , .true. )
          call pc_put_sensitive_field_flag ('PHASE'          , .true. )

        case ('HIGHPASS')
          call pc_put_sensitive_field_flag ('FREQ_LOW_NONE'  , .true. )
          call pc_put_sensitive_field_flag ('FREQ_LOW_FULL'  , .true. )
          call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL' , .false.)
          call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE' , .false.)
          call pc_put_sensitive_field_flag ('PHASE'          , .true. )

      end select

      return
      end subroutine vc_set_sensitivities


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine vc (obj,ntr,hd,tr)
      implicit none
      type(vc_struct)                 :: obj               ! arguments
      integer,         intent(inout)  :: ntr               ! arguments
      double precision,intent(inout)  :: hd(:,:)           ! arguments
      real,            intent(inout)  :: tr(:,:)           ! arguments

      integer                    :: itr_in, itr_out, iw    ! local

      real                       :: phase
      real                       :: csweep1,csweep2

!----------------------------------------------------------------
! Bail out right off if no more input or errors exist
      if ( ntr==NO_MORE_TRACES .or. ntr==FATAL_ERROR .or. ntr==NEED_TRACES) return

! Set up the sweep if it's on a CHANNEL
      if ( obj%loc_sweep == 'CHANNEL' .and. &
           ntr > obj%num_tr_sweep     .and. &
           ntr > obj%num_del                ) then

        ! Fourier transform the sweep trace to the frequency domain
        ! the sweep is time-reversed so a complex multiply will do a
        ! correlation rather than a convolution
        obj%rsweep(obj%ndpt_sweep:1:-1)=tr(1:obj%ndpt_sweep,obj%num_tr_sweep)
        obj%rsweep(obj%ndpt_sweep+1:obj%npo2) = 0.0
        call fft_rc_transform(obj%rcfft,obj%rsweep,obj%csweep)

        ! By default, change the amplitude spectrum of the sweep to 1.0
        if ( obj%opt_sweep_amp == 'NO' ) then
          do iw = 2,obj%nw-1
!!!         phase = atan2(aimag(obj%csweep(iw)),real(obj%csweep(iw)))
!!! The above line causes the following error when compiling in sol62,
!!! but only if phase is used in a subsequent expression:
!!!   f90: Fatal error in iropt: Segmentation Fault
!!! The following three lines work:
            csweep1 = aimag(obj%csweep(iw))
            csweep2 = real (obj%csweep(iw))
            phase = atan2(csweep1,csweep2)
            obj%csweep(iw) = cmplx(cos(phase),sin(phase))
          enddo
        endif

        ! Fix the endpoints to 0.0
        obj%csweep(1)      = cmplx(0.0,0.0)
        obj%csweep(obj%nw) = cmplx(0.0,0.0)

        ! Apply the filter to the sweep up front
        obj%csweep(:) = obj%csweep(:) * obj%cfilter(:)

      endif

! Loop over traces
      do itr_in = obj%num_del+1,ntr

        itr_out = itr_in-obj%num_del
        obj%sequence = obj%sequence+1

        ! Keep the headers in sync with the data
        hd(:,itr_out) = hd(:,itr_in)
        hd(HDR_SEQUENCE,itr_out) = obj%sequence
        hd(HDR_CURRENT_CHANNEL ,itr_out) = itr_out

        ! Certainly we should clear dead traces
        if ( hd(HDR_LAV,itr_out) == 0.0d0 ) then
          tr(:,itr_out) = 0.0
          cycle
        endif

        ! Fourier transform the trace to the frequency domain
        obj%rtrace(1:obj%ndpt_in) = tr(1:obj%ndpt_in,itr_in)
        obj%rtrace(obj%ndpt_in+1:obj%npo2) = 0.0
        call fft_rc_transform(obj%rcfft,obj%rtrace,obj%ctrace)

        ! Convolve the time reversed sweep with the trace
        ! and return to the time domain
        obj%fctrace(1:obj%nw) =  obj%ctrace(1:obj%nw) * obj%csweep(1:obj%nw)
        call fft_cr_transform(obj%crfft,obj%fctrace,obj%rtrace)

        ! Copy the filtered trace back to the trace array with normalization
        ! (time=0 on the correlated trace is at sample ndpt_sweep)
        tr(:obj%ndpt_out,itr_out) = &
              obj%rtrace(obj%ndpt_sweep:obj%ndpt_in) * obj%fft_norm
        tr(obj%ndpt_out+1:obj%ndpt_in,itr_out) = 0.0

        ! Recompute HDR_LAV
        hd(HDR_LAV,itr_out) = lav (tr(1:obj%ndpt_out,itr_out),obj%ndpt_out)

      enddo

      ! Keep ntr up to date
      ntr = ntr - obj%num_del
      if ( ntr <= 0 ) ntr = NEED_TRACES

      obj%ngather = obj%ngather+1
      if ( obj%infolu>0 .and. mod(obj%ngather,100)==0 ) then
        write(obj%infolu,obj%fmta) &
          "   VC: Correlated ",ntr," traces in gather ",obj%ngather
      endif

      return
      end subroutine vc

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine vc_wrapup (obj)
      implicit none
      type(vc_struct)   :: obj       ! arguments
      integer           :: ier
      character(len=80) :: message

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if ( obj%infolu>0 .and. obj%prtlu>0 ) then
          rewind obj%infolu
          write(obj%prtlu,*)""
          do
            read (obj%infolu,'(a80)',iostat=ier) message
            if(ier<0) exit  ! negative iostat means end of file/record
            write(obj%prtlu,*) trim(message)
            cycle
          enddo
          write(obj%prtlu,*)""
          close(obj%infolu)
      endif
      return
      end subroutine vc_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module vc_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
