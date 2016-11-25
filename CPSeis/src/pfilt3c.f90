!<CPS_v1 type="PROCESS"/>
!
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
! Name       : PFILT3C
! Category   : filters 
! Written    : 2006-10-31   by: Stephen Chiu
! Revised    : 2007-03-29   by: Stephen Chiu
! Maturity   : Beta
! Purpose    : Polarization filter on MC/Eigenimage filter 
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! The polarization filter is a time-variant adaptive filter designed to remove
! coherent noise such as groundroll and reverberation on the multi-component 
! (2C/3C/4C) data.  Analytic signals are first computed by combining input 
! data and input data after Hilbert transform. The starting window of the
! noise model is either based on the user-defined mute or noise velocity.  
! A complex SVD first transforms the data into complex eigen space. 
! In general, the groundroll is a dispersive signal with approximately 
! elliptical particle motion, thus the groundroll energy typically concentrates
! on the first two largest eigenvalues and eigenvectors. The groundroll noise
! model can be constructed from the first two largest eigenvalues and 
! eigenvectors. The noise model is then adaptively subtracted from
! the input data. This polarization filter can be extended to P-wave data. 
! For the P-wave data case, three adjacent traces can be used to simulate
! a MC data set.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS 
!                     
! If the input data consists of a gather of 3C data or P data such as a  
! receiver line and if the option is median threshold, a median of amplitude  
! envelopes for the entire gather is computed and this median (has better 
! median statistic) will be used for the entire gather. 
! If the option is SIG_REF, the S/N calculation is computed trace by trace.
!
! For multi-component case, the number(NATRC) is either 2 or 3 traces to 
! model the noise of a single receiver station. For example of a 3C recording, 
! the input gather may consists of 3 traces or a receiver line of 3C data.  
! The preferable order of trace data should: P-wave trace first and  
! followed by 2 shear-wave traces and the number(NATRC) is 3. 
! The gather or BUNCH process can be used to group data traces into a gather. 
! The gather process is recommended to group the traces as a gather. 
! For example, the BUNCH process can be used to combine a number of adjacent 
! traces into a gather with HONOR_HDR = yes, and HDR_TO_HONOR = receiver 
! line number. 
! 
! For P-wave data, the number(NATRC) to model the nosie is usually 3 to 5 
! traces to minimize spatial amplitude smearing.   
! For the 3D data, it is recommended to perform noise attenuation on the 
! receiver-line domain and the entire 3D volume is processed through 
! receiver line by receiver line. At the end of each receiver line, it 
! may not have enough traces to form a gather of NATRC traces, the 
! program automatically accounts for this case.
! For example, a receiver line gather has 11 traces and NATRC is 3 traces.
! At the end of the receiver line, we have only 2 traces, the program
! automatically pads enough data to form a gather of NATRC traces.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! No special requirements.  Traces requires input as a gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alter input traces.
! This process outputs the same traces as it receives.
!
! This process outputs trace ensembles.
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
! NUMTR     max number of traces input/output       used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! GATHERED  whether traces are a legitimate gather  set to true
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! 2       head mute index            Used
! 6       offset index               Used
! 25      LAV                        Reset
! 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!      Date         Author    Description
!      ----         ------    -----------
! 3.  2007-03-29    S Chiu     Fix problems between overlapped windows.
! 2.  2007-02-20    S.Chiu     Update documentation and rename NCHAN to NATRC
!                             otpion to compute median for entire gather.
! 1.  2007-01-03    S.Chiu     Created original version.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
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
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
!
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
!
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
!
!-------------------------------------------------------------------------------
!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>
!
!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS pfilt3c Process/NC=80>
!
!    Polarization filter on MC or eigenimage filter on P-wave data                       
!                                                                  
!    NATRC=~~~`IIII        OPT_DATA=`CCCCCC          NUM_ECOMP=`IIIII 
!    OPT_MUTE=`CCCCCCCC           
!
!    VEL_BEG=`FFFFFFFFFF    VEL_END=`FFFFFFFFFF     TIM_ADD=~~~`FFFFFFFFF   
!    WIN_LEN=`FFFFFFFFFF    WIN_INC=`FFFFFFFFFF     MWIN_NOISE=`FFFFFFFFF
!    MINOFF= `FFFFFFFFFF    MAXOFF= `FFFFFFFFFF     MAXSHFT=~~~`FFFFFFFFF     
!
!    OPT_THRESH=`CCCCCCCCCC THRESH= `FFFFFFFFF      NPT_SMOOTH=`IIIII
!
!                  Groundroll Filtering Option
! `--------------------------------------------------------------
!   GFILTER_TYPE=`CCCCCCCCC
!   GFREQ_LOW_NONE= `FFFFFFFFFFF   GFREQ_LOW_FULL= `FFFFFFFFFFF
!   GFREQ_HIGH_FULL=`FFFFFFFFFFF   GFREQ_HIGH_NONE=`FFFFFFFFFFF
! `--------------------------------------------------------------
!
!                  Signal Filtering Option
! `--------------------------------------------------------------
!   SFILTER_TYPE=`CCCCCCCCC
!   SFREQ_LOW_NONE= `FFFFFFFFFFF   SFREQ_LOW_FULL= `FFFFFFFFFFF
!   SFREQ_HIGH_FULL=`FFFFFFFFFFF   SFREQ_HIGH_NONE=`FFFFFFFFFFF
! `--------------------------------------------------------------
!
!                  Match Filtering Option
! `------------------------------------------------------------
!   LEN_FILT=`IIIIIIIIIII       DIAG_LOAD=`FFFFFFFFFFF
! `------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!
!<Help KEYWORD="OPT_MUTE">  
!<Tip> Whether use velocity, or MUTE to define starting window of noise. </Tip>
! Default = VEL_BEG
! Allowed = VEL_BOTH 
! Allowed = MUTE 
! If OPT_MUTE = VEL_BEG, use vel_beg to compute starting window.
! If OPT_MUTE = VEL_BOTH, use vel_beg and vel_end to compute 
!               starting and ending windows.
! If OPT_MUTE = MUTE, use mute time as the starting window.
! VEL_BEG is the velocity of the coherent noise for starting window.
! VEL_END is the velocity of the coherent noise for ending window.
! VEL_BEG must be greater than VEL_END.
! 
!</Help>
!
!<Help KEYWORD="VEL_BEG">
!<Tip> Velocity that defines starting window of groundroll/noise.</Tip>
! Default =  1000 m/s
! Allowed = real> 0.0
!
! Start time = (offset / VEL_BEG) + TIM_ADD.
!
!</Help>
!
!<Help KEYWORD="VEL_END">
!<Tip> Velocity that defines ending window of groundroll/noise.</Tip>
! Default =  700 m/s
! Allowed = real> 0.0
!
! Start time = (offset / VEL_END) + TIM_ADD.
!
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Additional time to start the window of noise in sec. </Tip>
! Default = 0.0
! Allowed = real
!
! Start time = (offset / VEL_BEG) + TIM_ADD.
! End time   = (offset / VEL_END) + TIM_ADD.
!
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of the noise window to be filtered in sec. </Tip>
! Default = 2.0
! Allowed = real
!
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<Tip> Increment of noise window in sec. </Tip>
! Default = 0.3
! Allowed > 0
! The window has 50% overlapped in time. 
!</Help>
!
!<Help KEYWORD="MWIN_NOISE">
!<Tip> Minimum length of noise model in sec.  </Tip>
! Default = 0.10
! Allowed > = 0 
! If the computed noise model in a given window is less than MWIN_NOISE,
! this portion of the noise model is ignored.
!</Help>
!
!<Help KEYWORD="MINOFF">
!<Tip> Minimum offset to compute noise model. </Tip>
! Default = 0
! Allowed real
!
!</Help>
!
!<Help KEYWORD="MAXOFF">
!<Tip> Maximum offset to compute noise model. </Tip>
! Default = 1000
! Allowed > = MINOFF
!</Help>
!
!<Help KEYWORD="MAXSHFT">
!<Tip> Maximum statics shift in sec. </Tip>
! Default = 0.05 
! Allowed < = WIN_INC/4
! Maximum statics shift is used internally to align the coherent noise.
! The output data has no statics shift applied.
!</Help>
!
!<Help KEYWORD="NATRC">
!<Tip> Number of traces used to compute a noise model. </Tip>
! Default = 3
! Allowed = 2 to Max gather size
! For MC data, either 2 or 3 traces. For P wave, it allows up to 11 traces, 
! but recommend to use 3 to 5 traces to maintain local tranform to avoid
! spatial smearing of amplitudes
!  
!</Help>
!
!<Help KEYWORD="OPT_DATA">  
!<Tip> Whether to use MC data, or use P-wave data only. </Tip>
! Default = MCOMP
! Allowed = P_only 
! If OPT_DATA = MCOMP, use multi-component data.
! If OPT_DATA = P_only, use P-wave data only.
! 
!</Help>
!
!<Help KEYWORD="NUM_ECOMP">
!<Tip> Number of energy components to construct noise model. </Tip>
! Default = 2
! Allowed = 1 to NATRC
!
!</Help>
!
!<Help KEYWORD="OPT_THRESH">  
!<Tip> Whether use SIG_REF, or MEDIAN to threshod noise model. </Tip>
! Default = MEDIAN
! Allowed = SIG_REF 
! If OPT_THRESH = MEDIAN, use median amplitude of data as reference.
! If OPT_THRESH = SIG_REF, use computed signals as reference.
! 
!</Help>
!
!<Help KEYWORD="THRESH">
!<Tip> Multiple of median or N/S value to trigger as coherent noise. </Tip>
! Default = 3.0
! Allowed = real >= 0.0 
! For the SIG_REF option, the data sample is considered to be noise if the 
! noise to signal ratio equals or exceeds THRESH.
! For the MEDIAN option, the data sample is considered to be noise if the 
! absolute amplitude equals or exceeds THRESH times the median absolute value 
! of the samples within the window of WIN_LEN.
!
!</Help>
!
!<Help KEYWORD="NPT_SMOOTH">
!<Tip> The number of points to smooth amplitude envelope. </Tip>
! Default = 7
! Allowed <= 1/4 of WIN_INC in sample
!
!</Help>
!------------------------Groundroll Frequency Filtering--------------------------
!
!<Help KEYWORD="GFILTER_TYPE">
!<Tip> Type of trapezoid gfreq_uency filter to apply (if any). </Tip>
! Default = ALLPASS
! Allowed = ALLPASS     (Pass all frequencies from 0.0 to Nyquist)
! Allowed = BANDPASS    (Pass frequencies from low taper to high taper)
! Allowed = LOWPASS     (Pass frequencies from 0.0 to high taper)
! Allowed = HIGHPASS    (Pass frequencies from low taper to Nyquist)
! Allowed = BANDREJECT  (Reject frequencies from low taper to high taper)
!
! In the GFILTER_TYPE descriptions below, frequency parameters are listed in
! the required order, from low frequency to high frequency.
!
! If GFILTER_TYPE=ALLPASS, then pass all frequencies from 0.0 frequency to
! Nyquist (Specification of GFREQ_LOW_NONE, GFREQ_LOW_FULL, GFREQ_HIGH_FULL, and
! GFREQ_HIGH_NONE parameters irrelevant in this case).
!
! If GFILTER_TYPE=BANDPASS, then reject between 0.0 frequency and GFREQ_LOW_NONE,
! pass between GFREQ_LOW_FULL and GFREQ_HIGH_FULL, reject between GFREQ_HIGH_NONE
! and Nyquist, with linear tapers between the pass and reject regions.
!
! If GFILTER_TYPE=LOWPASS, then pass between 0.0 frequency and GFREQ_HIGH_FULL,
! reject between GFREQ_HIGH_NONE and Nyquist, with a linear taper between pass
! and reject regions (Specification of GFREQ_LOW_NONE and GFREQ_LOW_FULL
! parameters irrelevant in this case).
!
! If GFILTER_TYPE=HIGHPASS, then reject between 0.0 frequency and GFREQ_LOW_NONE,
! pass between GFREQ_LOW_FULL and Nyquist, with a linear taper between pass and
! reject regions (Specification of GFREQ_HIGH_FULL and GFREQ_HIGH_NONE parameters
! irrelevant in this case).
!
! If GFILTER_TYPE=BANDREJECT (also known as a "NOTCH" filter), then pass
! between 0.0 frequency and GFREQ_LOW_NONE, reject between GFREQ_LOW_FULL and
! GFREQ_HIGH_FULL, pass between GFREQ_HIGH_NONE and Nyquist, with linear tapers
! between the pass and reject regions.
!</Help>
!
!<Help KEYWORD="GFREQ_LOW_NONE">
!<Tip> Frequency where low frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the FILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="GFREQ_LOW_FULL">
!<Tip> Frequency where low frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the GFILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="GFREQ_HIGH_FULL">
!<Tip> Frequency where high frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the GFILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="GFREQ_HIGH_NONE">
!<Tip> Frequency where high frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the GFILTER_TYPE parameter.
!</Help>
!
!------------------------Signal Frequency Filtering--------------------------
!
!<Help KEYWORD="SFILTER_TYPE">
!<Tip> Type of trapezoid gfreq_uency filter to apply (if any). </Tip>
! Default = ALLPASS
! Allowed = ALLPASS     (Pass all frequencies from 0.0 to Nyquist)
! Allowed = BANDPASS    (Pass frequencies from low taper to high taper)
! Allowed = LOWPASS     (Pass frequencies from 0.0 to high taper)
! Allowed = HIGHPASS    (Pass frequencies from low taper to Nyquist)
! Allowed = BANDREJECT  (Reject frequencies from low taper to high taper)
!
! In the SFILTER_TYPE descriptions below, frequency parameters are listed in
! the required order, from low frequency to high frequency.
!
! If SFILTER_TYPE=ALLPASS, then pass all frequencies from 0.0 frequency to
! Nyquist (Specification of SFREQ_LOW_NONE, SFREQ_LOW_FULL, SFREQ_HIGH_FULL, and
! SFREQ_HIGH_NONE parameters irrelevant in this case).
!
! If SFILTER_TYPE=BANDPASS, then reject between 0.0 frequency and SFREQ_LOW_NONE,
! pass between SFREQ_LOW_FULL and SFREQ_HIGH_FULL, reject between SFREQ_HIGH_NONE
! and Nyquist, with linear tapers between the pass and reject regions.
!
! If SFILTER_TYPE=LOWPASS, then pass between 0.0 frequency and SFREQ_HIGH_FULL,
! reject between SFREQ_HIGH_NONE and Nyquist, with a linear taper between pass
! and reject regions (Specification of SFREQ_LOW_NONE and SFREQ_LOW_FULL
! parameters irrelevant in this case).
!
! If SFILTER_TYPE=HIGHPASS, then reject between 0.0 frequency and SFREQ_LOW_NONE,
! pass between SFREQ_LOW_FULL and Nyquist, with a linear taper between pass and
! reject regions (Specification of SFREQ_HIGH_FULL and SFREQ_HIGH_NONE parameters
! irrelevant in this case).
!
! If SFILTER_TYPE=BANDREJECT (also known as a "NOTCH" filter), then pass
! between 0.0 frequency and SFREQ_LOW_NONE, reject between SFREQ_LOW_FULL and
! SFREQ_HIGH_FULL, pass between SFREQ_HIGH_NONE and Nyquist, with linear tapers
! between the pass and reject regions.
!</Help>
!
!<Help KEYWORD="SFREQ_LOW_NONE">
!<Tip> Frequency where low frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the FILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="SFREQ_LOW_FULL">
!<Tip> Frequency where low frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the SFILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="SFREQ_HIGH_FULL">
!<Tip> Frequency where high frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the SFILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="SFREQ_HIGH_NONE">
!<Tip> Frequency where high frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the SFILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="LEN_FILT">
!<Tip> Number of points for the match filter. </Tip>
! Default = 5
! Allowed = int >= 0
! It is usually between 3 to 9 points. If the filter length is too long,
! the match filter may remove the primary events as well as noise.
! If LEN_FILT = 0, No match filter applied 
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent. </Tip>
! Default = 3
! Allowed = real>=0.0
! Diagonal load helps to stablize the match filter.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module pfilt3c_module

      use pc_module
      use named_constants_module
      use gathr_module
      use rsvd_module
      use trcio_module
      use lav_module
      use interp_module 
      use bandps_module
      use genfilt_module
      use opfilt_module
      use fltr_module
      use csvd_module
      use median_module
      use statutil_module

      implicit none

      private
      public :: pfilt3c_create
      public :: pfilt3c_initialize
      public :: pfilt3c_update
      public :: pfilt3c_delete
      public :: pfilt3c_storetr
      public :: pfilt3c_wintime
      public :: pfilt3c_filt
      public :: pfilt3c_out

!<execute_only>

      public :: pfilt3c
      public :: pfilt3c_wrapup

!</execute_only>

      character(len=100),public,save :: PFILT3C_IDENT = &
'$Id: pfilt3c.f90,v 1.3 2007/03/30 13:53:59 Chiu beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: pfilt3c_struct

      private
      logical              :: skip_wrapup      ! for wrapup routine

      logical               :: gathered        ! gathered flag.
      integer               :: ndpt            ! globals
      integer               :: nwih            ! globals
      real                  :: dt              ! globals
      real                  :: tstrt           ! globals

      character (len=8)     :: opt_mute        ! Process parameters 
      character (len=10)    :: opt_thresh      ! Process parameters 
      character (len=6)     :: opt_data        ! Process parameters 

      real                  :: win_len         ! Process parameters
      real                  :: win_inc         ! Process parameters
      real                  :: mwin_noise      ! Process parameters

      real                  :: thresh          ! Process parameters
      integer               :: npt_smooth      ! Process parameters

      integer               :: natrc
      integer               :: num_ecomp       ! Process parameters

      character(len=10)     :: gftyp           ! process parameters
      real                  :: gf1             ! process parameters
      real                  :: gf2             ! process parameters
      real                  :: gf3             ! process parameters
      real                  :: gf4   

      character(len=10)     :: sftyp           ! process parameters
      real                  :: sf1             ! process parameters
      real                  :: sf2             ! process parameters
      real                  :: sf3             ! process parameters
      real                  :: sf4   

      integer               :: len_filt
      real                  :: diag_load

      integer               :: gathr_cnt       ! dependent variables
      
      integer               :: start_win       ! dependent variables
      integer               :: nwin2           ! dependent variables

      real                  :: tim_add
      real                  :: vel_beg
      real                  :: vel_end
      real                  :: maxoff 
      real                  :: minoff
      real                  :: maxshft         ! Process parameters

      integer               :: ndpt_win
      integer               :: nwdpt
      integer               :: len_win
      integer               :: inc_win
      integer               :: npts_min

      integer               :: nshift
      integer               :: prt_lun


    double precision, pointer      :: hd_save(:,:)    ! dependent variables
    real,             pointer      :: tr_save(:,:) 

    complex,          pointer      :: cdata_in(:,:)   ! dependent variables 
    complex,          pointer      :: cdata(:,:)      ! dependent variables
    complex,          pointer      :: cdata2(:,:)     ! dependent variables

    complex,          pointer      :: rdata(:,:)      ! dependent variables

    complex,          pointer      :: signal_in(:,:)  ! dependent variables
    complex,          pointer      :: signal(:,:)     ! dependent variables
    complex,          pointer      :: signal2(:,:)    ! dependent variables

    real,             pointer      :: s(:)            ! dependent variables
    complex,          pointer      :: u(:,:)          ! dependent variables
    complex,          pointer      :: v(:,:)          ! dependent variables
    complex,          pointer      :: vt(:,:)         ! dependent variables
    real,             pointer      :: eimage(:,:)     ! dependent variables
    real,             pointer      :: wtimage(:,:)    ! dependent variables
    real,             pointer      :: filt_data(:,:)  ! dependent variables
    real,             pointer      :: winwt(:)        ! dependent variables
    complex,          pointer      :: cwork(:)
    complex,          pointer      :: cwork2(:)
    real,             pointer      :: work(:)
    real,             pointer      :: work2(:)
  
      real,   pointer      ::  tdata(:), tdata2(:)
      real,   pointer      ::  ref(:)
      real,   pointer      ::  xcorr(:)

      real,   pointer      ::  stmp(:,:)
      real,   pointer      ::  envsig(:,:)
      real,   pointer      ::  envnoise(:,:)

      real,   pointer      ::  ampsig(:)
      real,   pointer       :: auto_corr  (:)
      real,   pointer       :: cross_corr (:)
      real,   pointer       :: filt_coeff (:)
      real,   pointer       :: filter     (:)

      real,   pointer       :: ref_out(:)
      real,   pointer       :: ref_wt(:)

      real,   pointer       :: ref_tmp  (:)
      real,   pointer       :: data_tmp (:)

      integer,   pointer    :: shift_save(:)

      integer,   pointer    :: ktmute(:,:)
      integer,   pointer    :: itrseq(:)

      integer               :: iseg(100)
      integer               :: icount              
      integer               :: ntaper
      integer               :: ntr_save
      integer               :: Lwdim 
      real                  :: rmedian

      type(genfilt_struct),pointer    :: genfiltg  ! dependent variables
      type(genfilt_struct),pointer    :: genfilts  ! dependent variables

      end type pfilt3c_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(pfilt3c_struct),pointer,save :: object      ! needed for traps.

!! Option lists for parameters with a specific set of allowed values:

      integer,parameter     :: opt_mute_noptions = 3
      character(len=8),save ::opt_mute_options(opt_mute_noptions)
      data opt_mute_options/'VEL_BEG','VEL_BOTH','MUTE'/

      integer,parameter     :: opt_thresh_noptions = 2
      character(len=7),save ::opt_thresh_options(opt_thresh_noptions)
      data opt_thresh_options/'MEDIAN','SIG_REF'/

      integer,parameter     :: opt_data_noptions = 2
      character(len=6),save ::opt_data_options(opt_data_noptions)
      data opt_data_options/'MCOMP','P_ONLY'/

      ! integer,parameter     :: HRD_OFFSET      = 6     ! CPS default 
      ! integer,parameter     :: HRD_SRC_DEPTH   = 20
      ! integer,parameter     :: HRD_TRC_TYPE    = 23
      ! integer,parameter     :: HRD_RCR_DEPTH   = 77

      real, parameter     :: EPS  = 1.e-25      

      !  groundroll filter options
      integer,parameter      :: gftyp_nopt=5             ! used in setup
      character(len=10),save :: gftyp_options(gftyp_nopt) ! used in traps

      data gftyp_options /'ALLPASS', 'BANDPASS', 'LOWPASS', 'HIGHPASS', &
                         'BANDREJECT'/

      !  signal filter options
      integer,parameter      :: sftyp_nopt=5             ! used in setup
      character(len=10),save :: sftyp_options(sftyp_nopt) ! used in traps

      data sftyp_options /'ALLPASS', 'BANDPASS', 'LOWPASS', 'HIGHPASS', &
                         'BANDREJECT'/

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine pfilt3c_create (obj)
      implicit none
      type(pfilt3c_struct),pointer :: obj              ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers

      nullify  (obj%hd_save)
      nullify  (obj%cdata_in)
      nullify  (obj%cdata)
      nullify  (obj%cdata2)

      nullify  (obj%rdata)
      nullify  (obj%signal_in)
      nullify  (obj%signal)
      nullify  (obj%signal2)      
      nullify  (obj%s)
      nullify  (obj%u)
      nullify  (obj%v)
      nullify  (obj%vt)
      nullify  (obj%eimage)
      nullify  (obj%wtimage)
      nullify  (obj%filt_data)
      nullify  (obj%winwt)
      nullify  (obj%work)
      nullify  (obj%work2)  
      nullify  (obj%cwork)
      nullify  (obj%tr_save)
      
      nullify  (obj%tdata)
      nullify  (obj%tdata2)
      nullify  (obj%ref)
      nullify  (obj%xcorr)
      nullify  (obj%stmp)

      nullify  (obj%envsig)
      nullify  (obj%envnoise)
      nullify  (obj%ampsig)

      nullify  (obj%cwork2)        ! extra 

      nullify  (obj%shift_save)
      nullify  (obj%auto_corr)
      nullify  (obj%cross_corr)
      nullify  (obj%filt_coeff)
      nullify  (obj%filter)
      nullify  (obj%ref_out)
      nullify  (obj%ref_wt)
      nullify  (obj%ref_tmp)
      nullify  (obj%data_tmp)

      nullify  (obj%ktmute)
      nullify  (obj%itrseq)

! Nullify all pointers
      nullify  (obj%genfiltg)
      nullify  (obj%genfilts)

      call pfilt3c_initialize (obj)

      return
      end subroutine pfilt3c_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine pfilt3c_delete (obj)
      implicit none
      type(pfilt3c_struct),pointer :: obj       ! arguments

!<execute_only>
      call pfilt3c_wrapup (obj)
!</execute_only>


      if (associated(obj%hd_save))     deallocate  (obj%hd_save)
      if (associated(obj%cdata_in))    deallocate  (obj%cdata_in)
      if (associated(obj%cdata))       deallocate  (obj%cdata)
      if (associated(obj%cdata2))      deallocate  (obj%cdata2)

      if (associated(obj%signal_in))  deallocate  (obj%signal_in)
      if (associated(obj%signal))     deallocate  (obj%signal)
      if (associated(obj%signal2))    deallocate  (obj%signal2)

      if (associated(obj%s))          deallocate  (obj%s)
      if (associated(obj%u))          deallocate  (obj%u)
      if (associated(obj%v))          deallocate  (obj%v)
      if (associated(obj%vt))         deallocate  (obj%vt)
      if (associated(obj%eimage))     deallocate  (obj%eimage)
      if (associated(obj%wtimage))    deallocate  (obj%wtimage)
      if (associated(obj%filt_data))  deallocate  (obj%filt_data)
      if (associated(obj%winwt))      deallocate  (obj%winwt)
      if (associated(obj%work))       deallocate  (obj%work)
      if (associated(obj%work2))      deallocate  (obj%work2)
      if (associated(obj%cwork))      deallocate  (obj%cwork)
      if (associated(obj%tr_save))    deallocate  (obj%tr_save)

      if (associated(obj%tdata))      deallocate  (obj%tdata)
      if (associated(obj%tdata2))     deallocate  (obj%tdata2)
      if (associated(obj%ref))        deallocate  (obj%ref)
      if (associated(obj%xcorr))      deallocate  (obj%xcorr)
      if (associated(obj%stmp))       deallocate  (obj%stmp)

      if (associated(obj%envsig))     deallocate  (obj%envsig)
      if (associated(obj%envnoise))   deallocate  (obj%envnoise)
      if (associated(obj%ampsig))     deallocate  (obj%ampsig)
      if (associated(obj%cwork2))   deallocate  (obj%cwork2)
      if (associated(obj%shift_save)) deallocate  (obj%shift_save)
      if (associated(obj%auto_corr))  deallocate  (obj%auto_corr)
      if (associated(obj%cross_corr)) deallocate  (obj%cross_corr)
      if (associated(obj%filt_coeff)) deallocate  (obj%filt_coeff)
      if (associated(obj%filter))     deallocate  (obj%filter)
      if (associated(obj%ref_out))    deallocate  (obj%ref_out)
      if (associated(obj%ref_wt))     deallocate  (obj%ref_wt)
      if (associated(obj%ref_tmp))    deallocate  (obj%ref_tmp)
      if (associated(obj%data_tmp))   deallocate  (obj%data_tmp)
      if (associated(obj%ktmute))     deallocate  (obj%ktmute)
      if (associated(obj%itrseq))     deallocate  (obj%itrseq)

      if (associated(obj%genfiltg))     call genfilt_delete (obj%genfiltg)
      if (associated(obj%genfilts))     call genfilt_delete (obj%genfilts)

      deallocate(obj)

      return
      end subroutine pfilt3c_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine pfilt3c_initialize (obj)
      implicit none
      type(pfilt3c_struct),pointer :: obj       ! arguments

      obj%opt_mute     = 'VEL_BEG'
      obj%opt_thresh   = 'MEDIAN'
      obj%opt_data     = 'MCOMP'

      obj%vel_beg      = 1000.
      obj%vel_end      = 700.
      obj%tim_add      = 0.0
      obj%win_len      = 2.0
      obj%win_inc      = 0.3
      obj%mwin_noise   = 0.1
      obj%minoff       = 0.0
      obj%maxoff       = 1000
      obj%maxshft      = 0.05
      obj%natrc        = 3
      obj%num_ecomp    = 2 
      obj%thresh       = 3
      obj%npt_smooth   = 7
    
      obj%gftyp = 'LOWPASS'
      obj%gf1   = FNIL
      obj%gf2   = FNIL
      obj%gf3   = FNIL
      obj%gf4   = FNIL

      obj%sftyp = 'BANDPASS'
      obj%sf1   = FNIL
      obj%sf2   = FNIL
      obj%sf3   = FNIL
      obj%sf4   = FNIL

      obj%len_filt       = 5
      obj%diag_load      = 3

      obj%prt_lun = pc_get_lun()
      call pfilt3c_update (obj)

      return
      end subroutine pfilt3c_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine pfilt3c_update (obj)
      implicit none
      type(pfilt3c_struct),intent(inout),target :: obj      ! Arguments


      integer            :: numtr                     ! local
      integer            :: nstore, nscratch          ! local
      integer            :: ier, nier

      integer            :: result
      character(len=120) :: message                   ! local

      integer            :: update_state              ! local
      integer            :: Lwork
      logical            :: gf1_sens, gf2_sens, gf3_sens, gf4_sens
      logical            :: sf1_sens, sf2_sens, sf3_sens, sf4_sens
      real               :: fnyq
      integer            :: len_filt_tmp
      integer            :: ncorr
      object => obj                               ! needed for traps.

      obj%skip_wrapup = .true.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      update_state = pc_get_update_state()

! Now retrieve user paramerters

      call pc_get_global ('gathered',obj%gathered) ! whether properly gathered.
      call pc_get_global ('nwih'  , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'  , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('tstrt' , obj%tstrt) ! time of 1st trace sample (s).
      call pc_get_global ('dt'    , obj%dt)    ! trace sample interval (s).
      call pc_get_global ('numtr' , numtr)    ! trace sample interval (s).

      call pc_get ('OPT_MUTE'      , obj%opt_mute)
      call pc_get ('VEL_BEG'       , obj%vel_beg)
      call pc_get ('VEL_END'       , obj%vel_end)
      call pc_get ('TIM_ADD'       , obj%tim_add)    
      call pc_get ('WIN_LEN'       , obj%win_len)
      call pc_get ('WIN_INC'       , obj%win_inc)
      call pc_get ('MWIN_NOISE'    , obj%mwin_noise)
      call pc_get ('MINOFF'        , obj%minoff)
      call pc_get ('MAXOFF'        , obj%maxoff)
      call pc_get ('MAXSHFT'       , obj%maxshft)

      call pc_get ('NATRC'         , obj%natrc)
      call pc_get ('OPT_DATA'      , obj%opt_data)
 
      call pc_get ('NUM_ECOMP'     , obj%num_ecomp)

      call pc_get ('OPT_THRESH'    , obj%opt_thresh)
      call pc_get ('THRESH'        , obj%thresh)
      call pc_get ('NPT_SMOOTH'    , obj%npt_smooth)

      call pc_get ('GFILTER_TYPE'   , obj%gftyp)
      call pc_get ('GFREQ_LOW_NONE' , obj%gf1)
      call pc_get ('GFREQ_LOW_FULL' , obj%gf2)
      call pc_get ('GFREQ_HIGH_FULL', obj%gf3)
      call pc_get ('GFREQ_HIGH_NONE', obj%gf4)

      call pc_get ('SFILTER_TYPE'   , obj%sftyp)
      call pc_get ('SFREQ_LOW_NONE' , obj%sf1)
      call pc_get ('SFREQ_LOW_FULL' , obj%sf2)
      call pc_get ('SFREQ_HIGH_FULL', obj%sf3)
      call pc_get ('SFREQ_HIGH_NONE', obj%sf4)

      call pc_get('LEN_FILT'        , obj%len_filt)
      call pc_get('DIAG_LOAD'       , obj%diag_load)

      call string_to_upper (obj%opt_mute)
      call string_to_upper (obj%opt_thresh)
      call string_to_upper (obj%opt_data)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (.not. obj%gathered ) then
          call pc_warning (' PFILT3C error: Require input to be gathered ')
      end if

      if (obj%opt_mute == 'VEL_BEG' .or. obj%opt_mute == 'VEL_BOTH' .or.  &
          obj%opt_mute == 'MUTE') then
          ier = 0
      else 
         call pc_warning                                              &
        (' PFILT3C error: OPT_MUTE on the list, reset to default ',   &
           obj%opt_mute)
        obj%opt_mute = 'VEL_BEG'
      end if 

      if (obj%opt_thresh == 'MEDIAN' .or. obj%opt_thresh == 'SIG_REF' ) then
         ier = 0
      else 
         call pc_warning                                                &
        (' PFILT3C error: OPT_THRESH on the list, reset to default ',   &
           obj%opt_thresh)
        obj%opt_thresh = 'MEDIAN'
      end if 
   
      if (obj%opt_data == 'MCOMP' .or. obj%opt_data == 'P_ONLY' ) then
         ier = 0
      else 
         call pc_warning                                              &
        (' PFILT3C error: OPT_DATA on the list, reset to default ',   &
           obj%opt_data)
        obj%opt_data = 'MCOMP'  
      end if 


      if (obj%vel_beg <= 0.0 ) then   
         call pc_error    &
        (' PFILT3C error: VEL_BEG must be > 0 -> ',obj%vel_beg)
      end if

      if (obj%vel_end <= 0.0 ) then   
         call pc_error    &
        (' PFILT3C error: VEL_END must be > 0 -> ',obj%vel_beg)
      end if

      if (obj%vel_beg <= obj%vel_end ) then   
         call pc_error    &
        (' PFILT3C error: VEL_BEG must be > VEL_END -> ',obj%vel_beg)
      end if

      if (obj%win_len <= 0.0 ) then   
         call pc_error    &
        (' PFILT3C error: WIN_LEN must be > 0  -> ',obj%win_len)
      end if

      if (obj%win_len > obj%ndpt*obj%dt ) then   
         call pc_error    &
        (' PFILT3C error: WIN_LEN must be =< data length -> ',obj%win_len)
      end if

      if (obj%win_inc < 0.0 ) then   
         call pc_error    &
        (' PFILT3C error: WIN_INC must be > 0 -> ',obj%win_inc)
      end if

      if (obj%win_inc > obj%win_len ) then   
         call pc_error    &
        (' PFILT3C error: WIN_INC must be =< WIN_LEN -> ',obj%win_inc)
      end if

      if (obj%mwin_noise < 0.0 ) then   
         call pc_error    &
        (' PFILT3C error: MWIN_NOISE must be >= 0 -> ',obj%mwin_noise)
      end if

      if (obj%mwin_noise > obj%win_len ) then   
         call pc_error    &
        (' PFILT3C error: MWIN_NOISE must be =< WIN_LEN -> ',obj%mwin_noise)
      end if

      if (obj%maxshft > obj%win_inc/4 ) then   
         call pc_warning    &
        (' PFILT3C error: MAXSHFT must be < = WIN_INC/4 -> ',obj%win_inc/4)
         call pc_warning    &
        (' PFILT3C : reset MAXSHFT to max shift = ',obj%win_inc/4) 
         obj%maxshft = obj%win_inc/4      
      end if

      if (obj%maxoff < obj%minoff) then   
         call pc_error    &
        (' PFILT3C error: MAXOFF must be > MINOFF', obj%maxoff)
      end if

       if (obj%natrc < 1 ) then   
         call pc_error    &
        (' PFILT3C error: NATRC must be > 1 -> ', &
           obj%natrc)
       end if

       if (obj%natrc > numtr) then   
         call pc_error    &
        (' FED error: NATRC must be <= MAX gather size  = ', &
           numtr)
       end if

      if (obj%num_ecomp < 1 .or. obj%num_ecomp > obj%natrc ) then   
         call pc_error    &
        (' PFILT3C error: NUM_ECOMP must be between 1 and ', &
           obj%natrc)
      end if

      if (obj%num_ecomp == obj%natrc ) then   
         call pc_warning    &
        (' PFILT3C warning: NUM_ECOMP recommend to be less than NATRC')
      end if

      if (obj%npt_smooth < 0) then   
         call pc_error    &
        (' PFILT3C error: NPT_SMOOTH must be >=  0' )
      end if

      if (obj%npt_smooth > nint(obj%win_inc/(4.0*obj%dt))) then   
         call pc_error    &
        (' PFILT3C error: NPT_SMOOTH must be =< ',nint(obj%win_inc/(3.0*obj%dt)))
      end if


    if( obj%len_filt <  0  ) then
        call pc_error('*** PFILT3C: Invalid LEN_FILT must be >= 0 ', &
        obj%len_filt)           
        obj%len_filt = 5
        return
    endif

    if( obj%len_filt >  nint(0.5*obj%win_len/obj%dt)  ) then
        call pc_error                                              &
         ('*** PFILT3C: Invalid LEN_FILT must be < half of WIN_LEN: ',&
        nint(0.5*obj%win_len/obj%dt))           
        obj%len_filt = 5
        return
    endif


    if( obj%diag_load < 0.0 ) then
        call pc_error('*** PFILT3C: Invalid DIAG_LOAD must be equal or > 0 ',&
           obj%diag_load)
        obj%diag_load = 3
        return
    endif


! Find Nyquist frequency
      fnyq = 0.5 / obj%dt

      call pc_put_sensitive_field_flag ('VEL_BEG' , .true.)
      call pc_put_sensitive_field_flag ('VEL_END' , .true.)

      if ( obj%opt_mute=='VEL_BEG' ) then
         call pc_put_sensitive_field_flag ('VEL_END' , .false.)
      else if ( obj%opt_mute=='MUTE' ) then
         call pc_put_sensitive_field_flag ('VEL_BEG' , .false.)
         call pc_put_sensitive_field_flag ('VEL_END' , .false.)
      end if 

! Check groundroll filter parameters
      call bandps_check (result, message, fnyq, obj%gftyp, &
                         obj%gf1, obj%gf2, obj%gf3, obj%gf4)
      if  (obj%gftyp == 'ALLPASS' .or. obj%gftyp == 'BANDPASS' .or. &
           obj%gftyp == 'LOWPASS' .or. obj%gftyp == 'HIGHPASS' .or. &
           obj%gftyp == 'BANDREJECT') then
        select case (result)
        case (BANDPS_INFO)
          call pc_info (message)
        case (BANDPS_ERROR)
          call pc_error (message)
        case (BANDPS_ENDERROR)
          if (pc_get_update_state() /= PC_GUI) call pc_error (message)
        end select
      else if (obj%gftyp == 'NONE') then
        obj%gftyp= 'ALLPASS'
        call pc_info ("GFILTER_TYPE = NONE isn't allowed for this process; &
                      &changed to ALLPASS.")
        if (result == BANDPS_INFO) then
          call pc_info ("Frequency limits irrelevant when GFILTER_TYPE = &
                        &ALLPASS; values cleared.")
        end if
      else
        call pc_error ("GFILTER_TYPE must be ALLPASS, BANDPASS, LOWPASS, &
                       &HIGHPASS, or BANDREJECT.")
      end if

      call bandps_sensitive (obj%gftyp, gf1_sens, gf2_sens, gf3_sens, gf4_sens)
      call pc_put_sensitive_field_flag ('GFREQ_LOW_NONE'  , gf1_sens)
      call pc_put_sensitive_field_flag ('GFREQ_LOW_FULL'  , gf2_sens)
      call pc_put_sensitive_field_flag ('GFREQ_HIGH_FULL' , gf3_sens)
      call pc_put_sensitive_field_flag ('GFREQ_HIGH_NONE' , gf4_sens)

!   Check signal filter parameters

      if (obj%opt_thresh == 'MEDIAN') then 
        call pc_put_sensitive_field_flag ('SFILTER_TYPE'    , .false.)
        call pc_put_sensitive_field_flag ('SFREQ_LOW_NONE'  , .false.)
        call pc_put_sensitive_field_flag ('SFREQ_LOW_FULL'  , .false.)
        call pc_put_sensitive_field_flag ('SFREQ_HIGH_FULL' , .false.)
        call pc_put_sensitive_field_flag ('SFREQ_HIGH_NONE' , .false.)
        go to 110
      else 
        call pc_put_sensitive_field_flag ('SFILTER_TYPE'    , .true.)
        call pc_put_sensitive_field_flag ('SFREQ_LOW_NONE'  , .true.)
        call pc_put_sensitive_field_flag ('SFREQ_LOW_FULL'  , .true.)
        call pc_put_sensitive_field_flag ('SFREQ_HIGH_FULL' , .true.)
        call pc_put_sensitive_field_flag ('SFREQ_HIGH_NONE' , .true.)
      end if 

      call bandps_check (result, message, fnyq, obj%sftyp, &
                         obj%sf1, obj%sf2, obj%sf3, obj%sf4)
      if  (obj%sftyp == 'ALLPASS' .or. obj%sftyp == 'BANDPASS' .or. &
           obj%sftyp == 'LOWPASS' .or. obj%sftyp == 'HIGHPASS' .or. &
           obj%sftyp == 'BANDREJECT') then
        select case (result)
        case (BANDPS_INFO)
          call pc_info (message)
        case (BANDPS_ERROR)
          call pc_error (message)
        case (BANDPS_ENDERROR)
          if (pc_get_update_state() /= PC_GUI) call pc_error (message)
        end select
      else if (obj%sftyp == 'NONE') then
        obj%sftyp= 'ALLPASS'
        call pc_info ("GFILTER_TYPE = NONE isn't allowed for this process; &
                      &changed to ALLPASS.")
        if (result == BANDPS_INFO) then
          call pc_info ("Frequency limits irrelevant when GFILTER_TYPE = &
                        &ALLPASS; values cleared.")
        end if
      else
        call pc_error ("GFILTER_TYPE must be ALLPASS, BANDPASS, LOWPASS, &
                       &HIGHPASS, or BANDREJECT.")
      end if

      call bandps_sensitive (obj%sftyp, sf1_sens, sf2_sens, sf3_sens, sf4_sens)
      call pc_put_sensitive_field_flag ('SFREQ_LOW_NONE'  , sf1_sens)
      call pc_put_sensitive_field_flag ('SFREQ_LOW_FULL'  , sf2_sens)
      call pc_put_sensitive_field_flag ('SFREQ_HIGH_FULL' , sf3_sens)
      call pc_put_sensitive_field_flag ('SFREQ_HIGH_NONE' , sf4_sens)

 110  continue

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

! genfilt - for groundroll

        call pc_clear

      call pc_put_process ('FILTER_TYPE'   , obj%gftyp)
      call pc_put_process ('FREQ_LOW_NONE' , obj%gf1)
      call pc_put_process ('FREQ_LOW_FULL' , obj%gf2)
      call pc_put_process ('FREQ_HIGH_FULL', obj%gf3)
      call pc_put_process ('FREQ_HIGH_NONE', obj%gf4)
        
        if (associated(obj%genfiltg)) then
            call genfilt_update (obj%genfiltg)
        else
            call genfilt_create (obj%genfiltg)
        end if
        call pc_restore

! genfilt - for signal

      if (obj%opt_thresh == 'SIG_REF') then 

         call pc_clear
         call pc_put_process ('FILTER_TYPE'   , obj%sftyp)
         call pc_put_process ('FREQ_LOW_NONE' , obj%sf1)
         call pc_put_process ('FREQ_LOW_FULL' , obj%sf2)
         call pc_put_process ('FREQ_HIGH_FULL', obj%sf3)
         call pc_put_process ('FREQ_HIGH_NONE', obj%sf4)

           if (associated(obj%genfilts)) then
               call genfilt_update (obj%genfilts)
           else
               call genfilt_create (obj%genfilts)
           end if
           call pc_restore
       end if 

! end genfilt

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
      
     call pc_put_options_field ('opt_mute', opt_mute_options,           &
                                  opt_mute_noptions)

     call pc_put_options_field ('opt_thresh', opt_thresh_options,           &
                                  opt_thresh_noptions)

     call pc_put_options_field ('opt_data', opt_data_options,           &
                                  opt_data_noptions)

      call pc_put_options_field &
             ('GFILTER_TYPE', gftyp_options, gftyp_nopt)

      call pc_put_options_field &
             ('SFILTER_TYPE', sftyp_options, sftyp_nopt)

      call pc_put_global ('gathered', .true. ) 

      call pc_put ('OPT_MUTE'      , obj%opt_mute)
      call pc_put ('VEL_BEG'       , obj%vel_beg)
      call pc_put ('VEL_END'       , obj%vel_end)
      call pc_put ('TIM_ADD'       , obj%tim_add)    
      call pc_put ('WIN_LEN'       , obj%win_len)
      call pc_put ('WIN_INC'       , obj%win_inc)
      call pc_put ('MWIN_NOISE'    , obj%mwin_noise)
      call pc_put ('MINOFF'        , obj%minoff)
      call pc_put ('MAXOFF'        , obj%maxoff)
      call pc_put ('MAXSHFT'       , obj%maxshft)

      call pc_put ('NATRC'         , obj%natrc)
      call pc_put ('OPT_DATA'      , obj%opt_data)

      call pc_put ('NUM_ECOMP'     , obj%num_ecomp)

      call pc_put ('OPT_THRESH'    , obj%opt_thresh)
      call pc_put ('THRESH'        , obj%thresh)   
      call pc_put ('NPT_SMOOTH'    , obj%npt_smooth)

      call pc_put ('GFILTER_TYPE'   , obj%gftyp     )
      call pc_put ('GFREQ_LOW_NONE' , obj%gf1       )
      call pc_put ('GFREQ_LOW_FULL' , obj%gf2       )
      call pc_put ('GFREQ_HIGH_FULL', obj%gf3       )
      call pc_put ('GFREQ_HIGH_NONE', obj%gf4       )
      
      call pc_put ('SFILTER_TYPE'   , obj%sftyp     )
      call pc_put ('SFREQ_LOW_NONE' , obj%sf1       )
      call pc_put ('SFREQ_LOW_FULL' , obj%sf2       )
      call pc_put ('SFREQ_HIGH_FULL', obj%sf3       )
      call pc_put ('SFREQ_HIGH_NONE', obj%sf4       )

      call pc_put('LEN_FILT'        , obj%len_filt)
      call pc_put('DIAG_LOAD'       , obj%diag_load)

! Determine memory usage

      nstore =  100
      nscratch  = 100

      call pc_put_control ('need_label',      'NO')
      call pc_put_control ('need_request',    'NO')
      call pc_put_control ('twosets',         'NO')

    call pc_put_control ('NSTORE',   nstore)
    call pc_put_control ('NSCRATCH', nscratch)
    !
    call pc_put_control ('PARALLEL_SAFE'         , .true.)
    call pc_put_control ('PCPS_SEND_MODE'        , 'PCPS_SEND_FIRST_AVAIL')
    call pc_put_control ('PCPS_RECEIVE_MODE'     , 'PCPS_RECEIVE_PASSTHRU')
    call pc_put_control ('PCPS_BUNCH_MODE'       , 'PCPS_BUNCH_TRACE_GROUPS')
    call pc_put_control ('PCPS_SEND_EOF_MODE'    , 'PCPS_SEND_ALL_EOF')
    call pc_put_control ('PCPS_ALT_SEND_MODE'    , 'PCPS_SEND_ALL')
    call pc_put_control ('PCPS_ALT_RECEIVE_MODE' , 'PCPS_RECEIVE_ALL_EOF')
    !

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!! Conditionally deallocate all arrays

      if (associated(obj%hd_save))     deallocate  (obj%hd_save)
      if (associated(obj%cdata_in))    deallocate  (obj%cdata_in)
      if (associated(obj%cdata))       deallocate  (obj%cdata)
      if (associated(obj%cdata2))      deallocate  (obj%cdata2)

      if (associated(obj%signal_in))  deallocate  (obj%signal_in)
      if (associated(obj%signal))     deallocate  (obj%signal)
      if (associated(obj%signal2))    deallocate  (obj%signal2)

      if (associated(obj%s))          deallocate  (obj%s)
      if (associated(obj%u))          deallocate  (obj%u)
      if (associated(obj%v))          deallocate  (obj%v)
      if (associated(obj%vt))         deallocate  (obj%vt)
      if (associated(obj%eimage))     deallocate  (obj%eimage)
      if (associated(obj%wtimage))    deallocate  (obj%wtimage)
      if (associated(obj%filt_data))  deallocate  (obj%filt_data)
      if (associated(obj%winwt))      deallocate  (obj%winwt)
      if (associated(obj%work))       deallocate  (obj%work)
      if (associated(obj%work2))      deallocate  (obj%work2)
      if (associated(obj%cwork))      deallocate  (obj%cwork)
      if (associated(obj%tr_save))    deallocate   (obj%tr_save)

      if (associated(obj%tdata))      deallocate  (obj%tdata)
      if (associated(obj%tdata2))     deallocate  (obj%tdata2)
      if (associated(obj%ref))        deallocate  (obj%ref)
      if (associated(obj%xcorr))      deallocate  (obj%xcorr)
      if (associated(obj%stmp))       deallocate  (obj%stmp)

      if (associated(obj%envsig))     deallocate  (obj%envsig)
      if (associated(obj%envnoise))   deallocate  (obj%envnoise)
      if (associated(obj%ampsig))     deallocate  (obj%ampsig)
      if (associated(obj%cwork2))     deallocate  (obj%cwork2)
      if (associated(obj%shift_save)) deallocate  (obj%shift_save)
      if (associated(obj%auto_corr))  deallocate  (obj%auto_corr)
      if (associated(obj%cross_corr)) deallocate  (obj%cross_corr)
      if (associated(obj%filt_coeff)) deallocate  (obj%filt_coeff)
      if (associated(obj%filter))     deallocate  (obj%filter)
      if (associated(obj%ref_out))    deallocate  (obj%ref_out)
      if (associated(obj%ref_wt))     deallocate  (obj%ref_wt)
      if (associated(obj%ref_tmp))    deallocate  (obj%ref_tmp)
      if (associated(obj%data_tmp))   deallocate  (obj%data_tmp)
      if (associated(obj%ktmute))     deallocate  (obj%ktmute)
      if (associated(obj%itrseq))     deallocate  (obj%itrseq)

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%icount = 0

      obj%nwdpt    = nint(obj%win_len/obj%dt + 0.499)
      obj%len_win  = nint(obj%win_inc/obj%dt + 0.499) 
      obj%inc_win  = obj%len_win/2
      obj%mwin_noise  = nint(obj%mwin_noise/obj%dt + 0.499) 
      obj%npts_min = obj%len_win/4
   
      obj%diag_load = 1.0 + obj%diag_load/100.
      obj%nshift   = nint(obj%maxshft/obj%dt) 

      !  hardwire taper to 40 ms
      obj%ntaper   = 0.03/obj%dt
      obj%npts_min = max(2*obj%ntaper, obj%npts_min)
                      
    ! offset in CPS must be positive
      obj%minoff  = abs(obj%minoff)
      obj%maxoff  = abs(obj%maxoff)

      ncorr      = 2*obj%nshift+1
      obj%Lwdim  = obj%len_win + ncorr

!! Allocate your permanent memory:

      nier = 0
      allocate  (obj%hd_save(obj%nwih, numtr), stat=ier);  nier = nier+ier
      allocate  (obj%tr_save(obj%ndpt, numtr), stat=ier);  nier = nier+ier
      allocate  (obj%cdata_in(obj%ndpt, numtr), stat=ier); nier = nier+ier
      allocate  (obj%signal_in(obj%ndpt, numtr), stat=ier); nier = nier+ier 

      allocate  (obj%cdata2(obj%ndpt, obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%signal2(obj%ndpt, obj%natrc), stat=ier); nier = nier+ier 

      allocate  (obj%cdata(obj%Lwdim, obj%natrc), stat=ier);  nier = nier+ier
      allocate  (obj%signal(obj%Lwdim, obj%natrc), stat=ier); nier = nier+ier 

      allocate  (obj%s(obj%natrc),  stat=ier); nier = nier+ier
      allocate  (obj%u(obj%Lwdim, obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%v(obj%natrc, obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%vt(obj%natrc, obj%natrc), stat=ier); nier = nier+ier

      Lwork = 3*min(obj%natrc,obj%Lwdim)+ max(obj%natrc,obj%Lwdim)
 
      allocate  (obj%cwork(Lwork), stat=ier); nier = nier+ier
      allocate  (obj%cwork2(obj%Lwdim+obj%natrc), stat=ier); nier = nier+ier

      Lwork = 6*min(obj%natrc,obj%Lwdim)+obj%ndpt
      Lwork = max(Lwork, 3*obj%Lwdim)

      allocate  (obj%work(Lwork),  stat=ier); nier = nier+ier
      allocate  (obj%work2(numtr),  stat=ier); nier = nier+ier

      allocate  (obj%eimage(obj%ndpt, obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%wtimage(obj%ndpt, obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%filt_data(obj%ndpt, obj%natrc), stat=ier); nier = nier+ier

      allocate  (obj%winwt(obj%len_win),    stat=ier); nier = nier+ier
      allocate  (obj%tdata(obj%ndpt+2*obj%Lwdim), stat=ier); nier = nier+ier
      allocate  (obj%tdata2(obj%ndpt), stat=ier); nier = nier+ier
      allocate  (obj%ref(obj%ndpt+2*obj%Lwdim), stat=ier); nier = nier+ier
      allocate  (obj%xcorr(obj%ndpt), stat=ier); nier = nier+ier
      allocate  (obj%stmp(obj%natrc,obj%natrc), stat=ier); nier = nier+ier

      allocate  (obj%envsig(obj%ndpt,obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%envnoise(obj%ndpt,obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%ampsig(obj%natrc), stat=ier); nier = nier+ier

      allocate  (obj%shift_save(obj%natrc), stat=ier); nier = nier+ier
      allocate  (obj%ktmute(2, numtr), stat=ier); nier = nier+ier
      allocate  (obj%itrseq(obj%natrc), stat=ier); nier = nier+ier

      if (obj%len_filt == 0) then 
         len_filt_tmp = 1
      else 
         len_filt_tmp = obj%len_filt
      end if 

      allocate  (obj%auto_corr  (len_filt_tmp),    stat=ier); nier = nier+ier
      allocate  (obj%cross_corr (len_filt_tmp),    stat=ier); nier = nier+ier
      allocate  (obj%filt_coeff (len_filt_tmp),    stat=ier); nier = nier+ier
      allocate  (obj%filter     (len_filt_tmp),    stat=ier); nier = nier+ier

      allocate  (obj%ref_out(obj%ndpt), stat=ier); nier = nier+ier
      allocate  (obj%ref_wt(obj%ndpt), stat=ier); nier = nier+ier

      allocate  (obj%ref_tmp  (obj%Lwdim), stat=ier); nier = nier+ier
      allocate  (obj%data_tmp (obj%Lwdim), stat=ier); nier = nier+ier

      if (nier/=0) call pc_error ('PFILT3C Error: allocate arrays')

      obj%tr_save = 0.0 
      obj%hd_save = 0.0

      call pfilt3c_winwt(obj)

      if (pc_do_not_process_traces()) return ! In case of allocation errors

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine pfilt3c_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine pfilt3c (obj,ntr, hd, tr)
      implicit none
      type(pfilt3c_struct),intent(inout) :: obj       ! arguments

      integer          ,intent(inout) :: ntr
      double precision ,intent(inout) :: hd(:,:)               ! arguments
      real             ,intent(inout) :: tr(:,:)               ! arguments

      integer             :: ier      
      integer             :: nadd, j, jj
      integer             :: itr, nn, last
      real                :: offset
!----------------------------------------------------------------
! Loop over traces
 
      if (ntr == FATAL_ERROR ) then
        call pc_error('FATAL_ERROR in routine PFILT3C ') 
        return
      end if  
                             
      if  ( ntr== NO_MORE_TRACES .or. ntr == FATAL_ERROR )  then
         call pfilt3c_wrapup (obj)
         return
      end if

!.... input gather usually 3 trace.  For MC, 1st trace should be P wave

       nadd = 0

       if ( mod( nint(hd(1,1)),100) == 0 ) write(obj%prt_lun,*) &
           ' trace of header 1 and 9 = ',nint(hd(1,1)),nint(hd(9,1)),' is done '
          
       if (ntr < 2 .and. obj%opt_data /= 'P_ONLY') then
          write(obj%prt_lun, *)    &
              'pfilt3c requires a group of minimum 2 input traces ' 
          ntr = FATAL_ERROR
          return
       end if
          
       ! for P-wave data, may need to pad traces to make input buffer to have NATRC traces.
 
       obj%ntr_save = ntr

       if (ntr < obj%natrc) then 
          nadd = obj%natrc-ntr
          do j = 1, nadd 
            jj = ntr + 1
            tr(1:obj%ndpt,jj) = tr(1:obj%ndpt,ntr)
            hd(1:obj%nwih,jj) = hd(1:obj%nwih,ntr)
         end do
         obj%ntr_save = ntr
         ntr = obj%natrc
       end if 
          
       obj%icount = obj%icount + obj%natrc

       call pfilt3c_wintime(obj, ntr, hd, ier)

       call pfilt3c_storetr(obj, ntr, tr, hd)

       if (ier == 99) then    ! velocity window errors
          write(obj%prt_lun,*) ' exit from wintime ', obj%start_win*obj%dt 
          NTR = FATAL_ERROR    
          return 
       end if  

       nn = 0
       do itr = 1, ntr

          offset = abs(hd(HDR_OFFSET, itr) )                     

          if ( offset < obj%minoff .or. offset > obj%maxoff) cycle
          if (obj%ktmute(1,itr) >= obj%ktmute(2,itr)) cycle 

          nn = nn + 1
          obj%cdata2(1:obj%ndpt, nn) = obj%cdata_in(1:obj%ndpt,itr)
          obj%signal2(1:obj%ndpt,nn) = obj%signal_in(1:obj%ndpt,itr)
          obj%itrseq(nn) = itr

          if ( nn == obj%natrc) then 
             call pfilt3c_filt(obj, obj%cdata2, obj%signal2, ier)
             call pfilt3c_out(obj, obj%natrc, obj%itrseq, tr, ier)
             nn = 0
          end if 

          if (ier == 99) then    
             NTR = FATAL_ERROR    
             return 
          end if    
       end do

       ! do the last panel of traces
       if (nn /= 0 .and. nn < obj%natrc) then 
          nadd = obj%natrc-nn
          last = obj%itrseq(nn)
          do j = 1, nadd 
            jj = nn + j
            obj%cdata2(1:obj%ndpt, jj) = obj%cdata_in(1:obj%ndpt,last)
            obj%signal2(1:obj%ndpt,jj) = obj%signal_in(1:obj%ndpt,last)
            obj%itrseq(jj) = last
         end do

         call pfilt3c_filt(obj, obj%cdata2, obj%signal2, ier)
         call pfilt3c_out(obj, nn, obj%itrseq, tr, ier)
         nn = 0

       end if 

 110   continue

       ntr = obj%ntr_save
       do j = 1, ntr
          call lav_set_hdr (hd(:,j), tr(:,j), obj%ndpt)
       end do 

      return 
      end subroutine pfilt3c


!---------------------------------------------------------------------
!
!     Store a input gather, compute analytic and noise signal.  
!---------------------------------------------------------------------

      subroutine pfilt3c_storetr(obj, ntr, tr, hd)

      type(pfilt3c_struct),intent(inout) :: obj          ! arguments

      integer , intent(inout)  :: ntr                    ! arguments
      real ,    intent(in) :: tr(:,:)                    ! arguments
      double precision , intent(inout) :: hd(:,:)        ! arguments 

      integer   :: ntr1
      real      :: trin(1:obj%ndpt,1),trtmp(obj%ndpt)
 
      integer   :: j, it
      integer   :: ibeg, iend, k, npt 

      !  input buffer has natrc components
      !  extract groundroll by filtering 

        npt = 0
        do j = 1, ntr

          trin(1:obj%ndpt,1) = tr(1:obj%ndpt,j) 
          ntr1 = 1
          call genfilt (obj%genfiltg, ntr1, hd(:,j:j), trin)
          call fltr_hilbert (trin(:,1), obj%ndpt, trtmp)

          k = 0         
          ibeg = obj%ktmute(1,j)
          iend = obj%ktmute(2,j)

          obj%cdata_in(1:obj%ndpt,j) = 0.0

          if ( ibeg >= iend) cycle 

          do it = ibeg, iend 
             k = k + 1
             obj%cdata_in(k,j) = cmplx(trin(it,1), trtmp(it))
          end do
 
          if (obj%opt_thresh == 'MEDIAN') then 
             obj%work(1:k) = abs(obj%cdata_in(1:k,j))  
             npt = npt + 1
             call median (obj%work, k, obj%work2(npt))
          end if 

        end do

        obj%rmedian = 0.0
        if (obj%opt_thresh == 'MEDIAN') then 
           call median (obj%work2, npt, obj%rmedian)
        end if 

        !  compute Hilbert transform  

        if (obj%opt_thresh == 'SIG_REF') then 

          do j = 1, ntr

            trin(1:obj%ndpt,1) = tr(1:obj%ndpt,j) 
            ntr1 = 1
            call genfilt (obj%genfilts, ntr1, hd(:,j:j), trin)
            call fltr_hilbert (trin(:,1), obj%ndpt, trtmp)

            k = 0         
            ibeg = obj%ktmute(1,j)
            iend = obj%ktmute(2,j)
            obj%signal_in(1:obj%ndpt,j) = 0.0

            if ( ibeg >= iend) cycle
 
            do it = ibeg, iend 
               k = k + 1
               obj%signal_in(k,j) = cmplx(trin(it,1),trtmp(it))
             end do
         end do

       else 
 
         obj%signal_in = cmplx(0.0, 0.0) 

       end if 
       
   
      return
      end subroutine pfilt3c_storetr


      subroutine pfilt3c_wintime(obj, ntr, hd, ier)

      !  compute starting and ending window for the noise model

      integer , intent(inout)  :: ntr                         ! arguments
      type(pfilt3c_struct),intent(inout) :: obj       ! arguments
      double precision , intent(inout)   :: hd(:,:)   ! arguments 

      integer          ,intent(inout) :: ier
      
      real    ::  offset, start, tend  
      integer ::  i, ibeg, iend, npts
      integer ::  maxwin, minwin, ndpt_tst

      ier = 0
         
!..... Compute the ground roll  

       minwin = 9999999
       maxwin = 0

       do i = 1, ntr

         if ( obj%opt_mute == 'VEL_BEG' .or. obj%opt_mute == 'VEL_BOTH' ) then

           offset  =  abs( hd(HDR_OFFSET, i) )

   !....... window start time in sample

           start = offset/obj%vel_beg      
           ibeg = (start+obj%tim_add)/obj%dt + 0.5 
           ibeg = max(ibeg, 1)
           ibeg = min(ibeg, obj%ndpt)
           npts = obj%ndpt - ibeg + 1

           if (npts < 2 ) then
              ibeg = obj%ndpt
           end if 

         else 

           ibeg  = nint(hd(HDR_TOP_MUTE,i))
           ibeg  = max(ibeg, 1)

         end if

         iend  = min(obj%ndpt, ibeg+obj%nwdpt)

         obj%ktmute(1,i) = ibeg
         obj%ktmute(2,i) = iend

         minwin = min( minwin, ibeg) 
         maxwin = max( maxwin, iend)

         if ( obj%opt_mute == 'VEL_BOTH' ) then

            if ( i==1) maxwin = 0.0

            tend = offset/obj%vel_end
            iend = (tend+obj%tim_add)/obj%dt + 0.5

            if ( iend < obj%len_win ) iend = obj%len_win
            if ( iend > obj%ndpt )    iend = obj%ndpt

            ibeg = min(ibeg, iend)
            iend = max(ibeg, iend)

            obj%ktmute(2,i) = iend
            maxwin = max( maxwin, iend)

            ndpt_tst = iend - ibeg + 1       

           if ( ndpt_tst < 1 .or. ndpt_tst > obj%ndpt) then 
              write(obj%prt_lun,*) ' Pfilt3C fatal error using VEL_BOTH '
              write(obj%prt_lun,*) ' PFILT3C Fatal error in ndpt_tst ', &
              ndpt_tst
              ier = 99
           end if 

         end if 

      end do          !  end i = 1, ntr

      obj%start_win = 1
      obj%ndpt_win  = maxwin - minwin + 1

      return
      end subroutine pfilt3c_wintime

      subroutine pfilt3c_winwt(obj)
!
!  purpose:   compute overlapped window weight.
!

      type(pfilt3c_struct),intent(inout) :: obj       ! arguments
      !  
      integer :: i                    ! local
      real    :: tmid, dist           ! local

      tmid = 0.5*(1. + obj%len_win)
      dist = 1./(1.1 * tmid)

      do i = 1, obj%len_win
         obj%winwt(i) = 1. - ((i - tmid)*dist)**2
      end do
      
      return
      end subroutine pfilt3c_winwt


      subroutine pfilt3c_filt(obj, cdata_in, signal_in, ier)
!
!  purpose:   filter the data.
!
      implicit none
      type(pfilt3c_struct),intent(inout) :: obj       ! arguments
      complex          ,intent(in) :: cdata_in(:,:)
      complex          ,intent(in) :: signal_in(:,:)
      integer          ,intent(inout) :: ier

      real          :: smax

      character*1  :: jobu  = 'S' 
      character*1  :: jobvt = 'S' 
 
      integer      :: info  
      integer      :: Lda, Ldu, Ldv
      integer      :: m, n    
      integer      :: Lwork

      integer ::  istart, iend, ibeg, ndpt1 
      integer ::  iw, nwin
      integer ::  ip
      integer ::  it, ic, j, k, itt, i

      integer ::  mx, ncorr, kshift, mxshift

      real    ::  tmp, amp_add, threshold, rnoise

      integer ::  npts
      integer ::  is, n1, n2, nlive

      integer, save  :: iprt = -3   ! for debug printing 

      ier = 0

      Lda = obj%Lwdim
      Ldu = obj%Lwdim
      Ldv = obj%natrc
      n   = obj%natrc 
      Lwork = 3*min(obj%natrc,obj%Lwdim)+ max(obj%natrc,obj%Lwdim)

      istart = obj%start_win  
      istart = min(istart, obj%ndpt)
      iend   = istart + obj%len_win - 1
      iend   = min(iend, obj%ndpt)

      ncorr = 2*obj%nshift+1

      ibeg  = istart
      ndpt1 = min( obj%ndpt, istart+obj%ndpt_win-1)

       if ( iend > obj%ndpt) then
          return
       end if

      !   compute amplitude envelope
      obj%envnoise = 0.0

      do ic = 1, obj%natrc
         do j = istart,ndpt1
            obj%envnoise(j,ic) = abs(cdata_in(j,ic))                     
            if (obj%opt_thresh == 'SIG_REF') then 
               obj%envsig(j,ic) = abs(signal_in(j,ic))
            end if 

         end do
      end do

      m = ndpt1 - istart + 1

      npts = min(obj%npt_smooth, m/3)
    
      !  smooth amplitude envelope
      do ic = 1, obj%natrc 
     
         obj%work(1:m) = obj%envnoise(istart:ndpt1,ic)

         if ( npts > 1) then 
            call pfilt3c_smooth_quick (obj%work,m,npts,obj%tdata,obj%icount)
         end if 

         obj%envnoise(istart:ndpt1,ic) = obj%work(1:m)

         if (obj%opt_thresh == 'SIG_REF') then

           obj%work(1:m) = obj%envsig(istart:ndpt1,ic)

           if ( npts > 1) then 
              call pfilt3c_smooth_quick (obj%work,m,npts,obj%tdata,obj%icount)
           end if 

           obj%envsig(istart:ndpt1,ic) = obj%work(1:m)
           obj%ampsig(ic) = maxval(obj%envsig(istart:ndpt1,ic))

         end if 

      end do

      if ( obj%icount == iprt) then    ! debug 
          write(obj%prt_lun,*) ' tracenumber ', obj%icount, obj%thresh
         do j = istart,ndpt1
            if ( sum(abs(obj%envnoise(j,1:n))) > 0.0) then 
              write(obj%prt_lun,'(a,f10.3,10e12.4)') ' ampwt1 ', j*obj%dt, &
              obj%envnoise(j,1:n),obj%envsig(j,1:n)
            end if 
         end do
      end if

      do ic = 1, obj%natrc

         npts = 0

         do j = istart,ndpt1
          
           if (obj%opt_thresh == 'SIG_REF') then

               amp_add = obj%ampsig(ic)*0.01
               tmp = obj%envsig(j,ic)
               if ( tmp < amp_add) tmp = tmp + amp_add

               rnoise = obj%envnoise(j,ic)/tmp
               threshold = obj%thresh

           else if (obj%opt_thresh == 'MEDIAN') then
               threshold = obj%rmedian*obj%thresh
               rnoise = obj%envnoise(j,ic)
           end if
          
           !    build the noise model

           if ( rnoise > threshold) then 
              obj%envnoise(j,ic) = 1.0

              ! check for zero 
              !   edit out noise model of 0 values less than minimum window size
  
              if ( npts > 0) then 
                 n2 = j-1
                 if ( npts < obj%npts_min) then
                    obj%envnoise(n1:n2,ic) = 1.0
                 end if
                 npts = 0
              end if 

            else 
               !   non-noise model region
               obj%envnoise(j,ic) = 0.0
               if ( npts == 0) n1 = j 
               npts = npts + 1  
            end if 
  
         end do  ! ** end j

       end do  ! ** end ic

      !   edit out noise model of 1 values less than minimum window size  
      do ic = 1, obj%natrc

         npts = 0
         nlive = 0
    
         !   edit out window that has a less than minimum threshold 
  
         do j = istart,ndpt1

            if ( obj%envnoise(j,ic) > EPS) then 
               if ( npts == 0) n1 = j 
               npts = npts + 1
               if ( j == ndpt1) then 
                 n2 = j
                  if ( npts < obj%npts_min) then
                     obj%envnoise(n1:n2,ic) = 0.0   
                     npts = 0
                  end if
                  nlive = nlive + npts 
               end if 

            else 

               !   Non-noise region 

               if ( npts > 0) then 
                  n2 = j-1

                  if (npts /= (n2-n1+1) ) then 
                      write(*,*) ' problem in editing points ', npts, n2,n1
                      stop
                  end if

                  if ( npts < obj%npts_min) then
                     obj%envnoise(n1:n2,ic) = 0.0
                     npts = 0
                  end if

                  nlive = nlive + npts 
                  npts = 0 
               end if 

            end if 
  
         end do

         if ( nlive < obj%mwin_noise ) then
            obj%envnoise(istart:ndpt1,ic) = 0.0
         end if 
      end do

      ! apply taper to noise model
      do ic = 1, obj%natrc 

        npts = 0
  
        if (obj%envnoise(istart,ic) > EPS ) then
            npts = npts + 1
            obj%iseg(npts) = istart
         end if
        
        !  determine the start and end of the noise segment (value of 1)
  
        do j = istart+1,ndpt1
            if (abs(obj%envnoise(j,ic)-obj%envnoise(j-1,ic)) > .999) then 
               npts = npts + 1
               if ( obj%envnoise(j,ic)<= EPS ) then
                  obj%iseg(npts) = j-1
               else 
                  obj%iseg(npts) = j
               end if
            end if
          end do

         if ( npts > 100) then 
            write(obj%prt_lun,*) ' Fatal error in PFILT3C: exceed iseg array'
            ier = 99
            return
         end if 

        !  taper the start and end of the noise segment
         do is = 1, npts, 2

           n1 = obj%iseg(is)
           n2 = obj%iseg(is+1)

           if ( n1 < obj%ntaper) n1 = obj%ntaper
           if ( n2+obj%ntaper > ndpt1) n2 = ndpt1 - obj%ntaper + 1
 
           do it = 1,obj%ntaper
              obj%envnoise(n1-it+1,ic) = exp(-it*0.3333)
              obj%envnoise(n2+it-1,ic) = exp(-it*0.3333)
           end do

        end do      ! end is 
       end do        ! end ic

       nwin= 1     
       ip = 0

       ! compute noise model

       obj%filt_data = 0.0
       obj%eimage = 0.0
       obj%s = 0.0

       obj%wtimage = EPS

        do iw = 1,  obj%ndpt

          obj%shift_save = 0
          obj%rdata = 0.0
          obj%ref = 0.0
          obj%tdata = 0.0
          obj%tdata2 = 0.0 

           do ic = 1, obj%natrc
              do it = istart, iend
                 obj%cdata(it-istart+1,ic) = cdata_in(it,ic)  
              end do
           end do 

           m = iend - istart + 1
      
            !   CORRELATE TRACE AND take LARGEST xcorrelation VALUE as statics

            !  ... trace one is used as the reference

              obj%ref(obj%nshift+1:m+obj%nshift) = real(obj%cdata(1:m,1))

            do ic = 2, obj%natrc

              obj%tdata(1:m) = real(obj%cdata(1:m,ic))

              mx = m + 2*obj%nshift 

              obj%xcorr = 0 

              call fltr_filterg (obj%tdata, m, obj%ref, mx, obj%xcorr)
              kshift = mth_ismax(ncorr,obj%xcorr,1)
              kshift = min(kshift, 2*obj%nshift)

             if ( kshift > obj%nshift ) then 
                kshift = kshift-(obj%nshift+1)     ! for positive shifts
             else if ( kshift <= obj%nshift .and. kshift > 0 ) then 
                kshift = -(obj%nshift+1-kshift)    ! for -ve shifts
             end if

             obj%shift_save(ic) = kshift 

            end do  

            if ( obj%icount == iprt .and. ic==3 ) then
               write(obj%prt_lun,*) ' kshift ',  iw,ic,nwin,kshift, obj%nshift
            end if 
  
            mxshift = maxval(abs(obj%shift_save(1:obj%natrc))) 
            mx = m + 2*mxshift
              
            if ( mxshift > obj%nshift) then 
               write(*,*) ' PFILT3C error: kshift_mx ', m, kshift, obj%nshift
               stop
            end if 

                     
            do ic = 1, obj%natrc

              k = mxshift              
              obj%cwork = 0.0

              do it = istart, iend
                 k = k + 1
                 obj%cwork(k) = cdata_in(it,ic)  
              end do

              call pfilt3c_shft (obj%shift_save(ic),mx,obj%cwork, obj%cwork2) 
              
              obj%cdata(1:mx,ic) = obj%cwork2(1:mx)

            end do       ! end of ic loop

           tmp = sum(abs(obj%cdata(1:mx,1:n)))

           if ( tmp == 0.0) then       ! no live data in window
              obj%eimage(istart:iend,1:obj%natrc) = 0.0  !  eigen image

              ! write(obj%prt_lun,*) 'no_live data window ', istart*obj%dt,iend*obj%dt,m,n

              go to 110

           end if
                          
          call csvd_cge( jobu, jobvt, mx, n, obj%cdata, Lda, obj%s, obj%u,   &     
                   Ldu, obj%vt, Ldv, obj%cwork, Lwork, obj%work, info ) 

         ! write(obj%prt_lun,*) 'svdpars ', jobu, jobvt, obj%len_win, n,Lda,Ldu,Ldv,Lwork 


         if ( info /= 0) then
            write(obj%prt_lun,*) ' Fatal error of PFILT3C -- problem in SVD '  
            write(obj%prt_lun,*) ' Fatal error of PFILT3C -- problem in SVD ' 
            write(obj%prt_lun,*) ' Fatal error of PFILT3C -- problem in SVD ' 
            ier = 99
            return
         end if

         smax = maxval(obj%s(1:obj%natrc))

         if ( obj%icount == iprt .and. ic==3 ) then    ! debug 
           write(obj%prt_lun,957)  iw, kshift, m,istart,iend,obj%len_win,obj%s(1:obj%natrc)/smax
   957     format(' iw-s iw, kshift, m,istart,iend,len_win, s(1:n)', 6i6, 4f12.4) 
         end if
      
           obj%stmp = 0.0
           do i = 1, obj%num_ecomp    
             obj%stmp(i,i) = obj%s(i)
           end do

          obj%u = matmul(obj%u(1:obj%Lwdim,1:obj%natrc),obj%stmp(1:obj%natrc,1:obj%natrc))
          obj%cdata = matmul(obj%u(1:obj%Lwdim,1:obj%natrc),obj%vt(1:obj%natrc,1:obj%natrc))

           !   remove statics of the noise model

           obj%shift_save = -obj%shift_save

           do ic = 1, obj%natrc

              obj%cwork2(1:mx) = obj%cdata(1:mx,ic)
               
              call pfilt3c_shft (obj%shift_save(ic),mx,obj%cwork2, obj%cwork) 

              k = mxshift              
              do it = 1, m
                 k = k + 1
                 obj%cdata(it,ic) = obj%cwork(k)  
              end do
            end do 

             do ic = 1, obj%natrc             

               do itt = 1, m

                 it = itt+istart-1
                 obj%eimage(it,ic) = obj%eimage(it,ic)   &
                             + real(obj%cdata(itt,ic))*obj%winwt(itt)
                     
                 obj%wtimage(it,ic) = obj%wtimage(it,ic) + obj%winwt(itt)
                     
 
                 if ( obj%icount == iprt .and. ic==3 ) then     ! debug 
                    write(obj%prt_lun,'(a,5i6, f10.3, 4e12.4)') ' eimage ',iw,ic,itt, &
                             it,kshift,it*obj%dt, obj%eimage(it,ic),       & 
                             real(obj%cdata(itt,ic)),obj%winwt(itt) 
                 end if

             end do
           end do
   
           nwin= nwin+ 1

 110       continue

           !   recompute the starting and ending next noise window
           if (iend >= ndpt1) exit
           istart = istart + obj%inc_win 
           if (istart > ndpt1) exit
           istart = min(istart, ndpt1)
           iend   = iend + obj%inc_win 
           iend   = min(iend, ndpt1)              
        end do
 
         !  keep trace number of noise window 

         obj%nwin2 = nwin - 1
    
       !  compute filtered output

        do ic = 1, obj%natrc
            obj%filt_data(ibeg:ndpt1,ic) = obj%filt_data(ibeg:ndpt1,ic)    &
              + obj%eimage(ibeg:ndpt1,ic)/obj%wtimage(ibeg:ndpt1,ic)       
        end do

        ! shift data back at mute time

        do ic = 1, obj%natrc
          j = obj%itrseq(ic)
          ibeg = obj%ktmute(1,j)
          iend = obj%ktmute(2,j)
          npts = iend-ibeg+1

          obj%work = 0.0
          obj%tdata = 0.0

          do it = 1, npts
            obj%work(ibeg+it-1)  = obj%filt_data(it,ic)   
          end do
          obj%filt_data(1:obj%ndpt,ic) = obj%work(1:obj%ndpt)

          obj%work = 0.0
          obj%tdata = 0.0

          do it = 1, npts
            obj%work(ibeg+it-1)  = obj%envnoise(it,ic)    ! N/S weight apply in output
          end do
          obj%envnoise(1:obj%ndpt,ic) = obj%work(1:obj%ndpt)

      end do

      return
      end subroutine pfilt3c_filt


      subroutine pfilt3c_smooth_quick (array,n,nrun, work,iprt) 
      !  Smooth the array by running average 

      implicit none
      real                     ,intent(inout) :: array(:)         ! arguments
      integer                  ,intent(in)    :: n,nrun           ! arguments
      real                                    :: work(:)  ! local
      integer                                 :: i,j,nhalf,ia,ib    ! local
      integer                                 :: iprt 

      integer, save                           :: ifirst = 1 

      nhalf         = nrun/2
      work          = 0.0

      do i = 1,n

         ia = max(i-nhalf,1)
         ib = min(i+nhalf,n)
         do j = ia, ib
           work(i) = work(i) + array(j) 
         end do
         work(i) = work(i)/(ib-ia)
      end do

      array(1:n) = work(1:n)

      return
      end subroutine pfilt3c_smooth_quick

      subroutine pfilt3c_matchf(obj, it1, it2, ref, data, out_noise, ier)

!  purpose: Compute a time-varying match obj%filter between data and noise model  

      type(pfilt3c_struct),intent(inout) :: obj              ! arguments
      integer          ,intent(in)    :: it1
      integer          ,intent(in)    :: it2
      real             ,intent(in)    :: ref(*)              ! noise model
      real             ,intent(in   ) :: data(*)             ! input data
      real             ,intent(inout) :: out_noise(*)        ! noise model after match filter
      integer          ,intent(inout) :: ier

      integer  :: len_hfilt
      integer  :: lwin_half
      integer  :: istart, iend, ibeg, ndpt1 
      integer  :: iw, j, m

      ier = 0

      len_hfilt = nint(obj%len_filt/2.0)
      lwin_half = obj%len_win/2 

      istart = it1  

      ibeg = istart

      ndpt1 = min( obj%ndpt, it2)

      istart = min(istart, ndpt1)
      iend   = istart + obj%len_win - 1
      iend   = min(iend, ndpt1)

      obj%ref_out = 0.0
      obj%ref_wt  = 0.0

      do iw = 1, obj%nwin2*2 
    
!.... get a window of data
 
         m = iend-istart+1

         obj%ref_tmp(1:m)  = ref(istart:iend)
         obj%data_tmp(1:m) = data(istart:iend)
   
         if ( m < obj%len_win ) then 
           obj%ref_tmp(m+1:obj%len_win) = 0.0
           obj%data_tmp(m+1:obj%len_win) = 0.0
         end  if 

      !   compute match ofilter
      ! - Compute autocorrelation obj%apply%tr into r
      !
         call fltr_filtrgs (filter   = obj%ref_tmp,         &
                         m           = obj%len_win,         &
                         data        = obj%ref_tmp,         &
                         n           = obj%len_win,         &
                         correlation = obj%auto_corr,       &
                         l           = obj%len_filt,        &
                         iflag       = 1,                   &
                         ishift      = 0)

         ! ** no live data in window
         if ( obj%auto_corr(1) <= tiny(obj%auto_corr(1))) go to 110   
 
         obj%auto_corr(1) = obj%auto_corr(1) * obj%diag_load

         call fltr_filtrgs (filter   = obj%ref_tmp,         &
                         m           = obj%len_win,         &
                         data        = obj%data_tmp,        &
                         n           = obj%len_win,         &
                         correlation = obj%cross_corr,      &
                         l           = obj%len_filt,        &
                         iflag       = 1,                   &
                         ishift      = 0)

      !
      ! - get match obj%filter function 
      !         
         call opfilt (N = obj%len_filt,          &
                   A = obj%filt_coeff,           &
                   B = obj%cross_corr,           &
                   C = obj%work,                 &    
                   R = obj%auto_corr)

          obj%filter(1:obj%len_filt:1) = obj%filt_coeff(obj%len_filt:1:-1)

          obj%work = 0.0
          obj%work(obj%len_filt:obj%len_win+obj%len_filt) = obj%ref_tmp(1:obj%len_win)

          call fltr_filtrgs (filter  = obj%filter,                     &
                         m           = obj%len_filt,                   &
                         data        = obj%work,                       &
                         n           = obj%len_win+obj%len_filt-1,     &
                         correlation = obj%ref_tmp,                    &
                         l           = obj%len_win,                    &
                         iflag       = 1,                              &
                         ishift      = 0)
          
 105      continue

          obj%ref_out(istart:iend) = obj%ref_out(istart:iend) + &
                                obj%ref_tmp(1:m)*obj%winwt(1:m)

          ! apply taper weights between zero and 1
          obj%ref_out(istart:iend) = obj%ref_out(istart:iend)  

          obj%ref_wt(istart:iend)  = obj%ref_wt(istart:iend) +obj%winwt(1:m)
 
 110      continue

          if (iend == ndpt1) exit
          istart = istart + lwin_half 

          if (istart > ndpt1) then
            write(obj%prt_lun,*)                                     &
            ' PFILT3C fatal error:istart > ndpt1 in matchfilt ',     &
            istart,iend,ndpt1,istart-lwin_half 
            ier = 99
            return 
          end if 
  
          iend = iend + lwin_half
          if ( iend > ndpt1) then
            iend = ndpt1 
          end if  
 
        end do 
  
  
       do j = ibeg, ndpt1
 
          if(obj%ref_wt(j) /= 0.) then  
            out_noise(j) = obj%ref_out(j)/obj%ref_wt(j) 
          else
            out_noise(j) = obj%ref_out(j) 
          end if 

       end do

      return
      end subroutine pfilt3c_matchf


      subroutine pfilt3c_out(obj, ntr, itrseq, tr, ier)

      !   output buffer  = input - noise model 

      type(pfilt3c_struct),intent(inout) :: obj               ! arguments

      integer , intent(inout)  :: ntr
      integer , intent(inout)  :: itrseq(:)                   
      real ,    intent(inout)  :: tr(:,:)                     ! arguments
      integer,  intent(inout)  :: ier

      integer   :: j, idum, it, itr, it1, it2

        ier = 0

        do j = 1, ntr

          itr = itrseq(j)

          it1 = obj%ktmute(1,itr)
          it2 = obj%ktmute(2,itr)
          
          obj%tdata2 = 0.0
           if (obj%len_filt > 0) then  ! apply match filter
              call pfilt3c_matchf(obj, it1, it2, obj%filt_data(:,j:j), tr(:,itr:itr), &
                   obj%tdata2, ier)              
              if (ier == 99) return
           else 
              ! no match filter applied
              obj%tdata2(1:obj%ndpt) =  obj%filt_data(1:obj%ndpt,j)    
           end if 

           idum = 1
           if ( idum == 0) then   ! debug to output noise model

               !   write(*,*) ' model data only '
               !   tr(1:obj%ndpt,itr) = obj%filt_data(1:obj%ndpt,j) 
                  tr(1:obj%ndpt,itr) = obj%tdata2(1:obj%ndpt)*obj%envnoise(1:obj%ndpt,j) 

           else           
              do it = 1, obj%ndpt
                 tr(it,itr) = tr(it,itr)- obj%tdata2(it)*obj%envnoise(it,j)  
             end do
           end if

        end do

      return
      end subroutine pfilt3c_out

      subroutine pfilt3c_median(obj, ntr)

      !  compute starting and ending window for the noise model

      integer , intent(inout)  :: ntr                        
      type(pfilt3c_struct),intent(inout)   :: obj       
  
      integer                              :: ibeg, iend, itr, j, n

      do itr = 1, ntr

        ibeg = obj%ktmute(1,itr)
        iend = obj%ktmute(2,itr)
        n = 0

        do j = ibeg, iend
           n = n + 1 
           obj%work(n) = abs(obj%cdata_in(j,itr)) 
           call median (obj%work, n, obj%work2(itr))
        end do 
      end do
       
      call median (obj%work2, ntr, obj%rmedian)

      return
      end subroutine pfilt3c_median

    subroutine pfilt3c_shft (ishft,n,a,b)
      implicit none
!----------declarations
      integer                 :: n, ishft
      complex, dimension(1:n) :: a, b
      integer                 :: nn, i1, i2

!----------start
      nn=n
!----------if shift is zero, copy trace.

      if(ishft.eq.0) then
           b(1:nn)=a(1:nn)
           return
      end if

!----------ishift is negative.
      if (ishft.lt.0) then
           i1=1
           i2=max(nn+ishft,1)
           b(i2:nn)=0.0
!----------ishift is positive.
      else
           i1=min(ishft+1,nn)
           i2=nn
           b(1:i1)=0.0
      end if
      if(abs(ishft).ge.nn) return
      b(i1:i2)=a(i1-ishft:i2-ishft)

    return
    end subroutine pfilt3c_shft


!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine pfilt3c_wrapup (obj)
      implicit none
      type(pfilt3c_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine pfilt3c_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module pfilt3c_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
