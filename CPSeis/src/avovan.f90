!<CPS_v1 type="PROCESS"/>
!!------------------------------- avovan.f90 ---------------------------------!!
!!------------------------------- avovan.f90 ---------------------------------!!
!!------------------------------- avovan.f90 ---------------------------------!!

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
! Name       : AVOVAN
! Category   : velocity_analysis
! Written    : 2003-08-26   by: Bill Lucas
! Revised    : 2007-02-05   by: D. Glover
! Maturity   : production
! Purpose    : AVO and Velocity Analysis (AVOVAN) process.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This tool performs amplitude vs. offset (AVO) analysis on pre-stack CDP
! gathers by the method of least squares stacking. AVEL builds on earlier
! methods by compensating for the following known distortions:
!   1. Geometric spreading
!   2. Residual velocity errors
!   3. NMO stretch
!   4. Wavelet phase dependence of product traces
!   5. Incoherent noise
!
! In addition to performing AVO analysis, this tool may also be used to
! perform NMO velocity analysis, with greater velocity resolution than was
! previously possible.
!
! These results are obtained as follows: geometric spreading lossed are
! computed on the bases of the Newman (1973) formulation. This method is more
! complete than trace scaling in a vertically stratified medium, where stacking
! velocities vary with depth. The tool is made robust to residual velocity
! errors, by using the fact that AVO slope errors due to velocity are in
! phase quadrature to the zero offset stack. They may therefore be effectively
! removed from the estimated slope. In addition, the residual velocity
! indicator may be closely examined, to adjust the stacking velocity and
! obtain even more accurate slope estimates.
!
! A side effect of normal moveout (NMO) correction is to stretch the far
! offset wavelet relative to its near offset counter part. This, in turn,
! produces a significant but predictable distortion in the resulting slope
! estimate (Swan, 1987). Accurate slope estimation to the limits of seismic
! resolution requires knowledge of the wavelet amplitude spectrum, and the
! relative phases of its frequency components. It does NOT require absolute
! wavelet phases. Temporal resolution may be sacrificed in order to gain
! greater slope immunity to residual velocity errors.
!
! It is well known that the slope trace of AVO analysis has a lower
! inherent signal-to-noise ratio than the zero offset stacked trace.
! However, by using Wiener-Levinsion filter techniques which minimize the
! mean-squared slope error, S/N ratios comparable to those for the zero
! offset trace can be realized. All of these features have been built
! into the AVEL tool.
!
! The computation procedure may be divided into the following parts:
! inversion filter design, first trace scaling, velocity processing, data
! collection, AVO analysis, corrections to the raw AVO coefficients,
! attribute computations, and final data scaling.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! None.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Input traces must be pre-stack CDP gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Output traces will consist of stacking velocities, the input gathers, and
! a user-defined combination of the following attributes:
!   1. RE{A}   (43 - In-phase zero offset stack trace)
!   2. RE{B}   (44 - In-phase gradient trace)
!   3. RE{AB*} (42 - Hydrocarbon indicator trace)
!   4. IM{AB*} (46 - Residual velocity indicator trace)
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       increased.
! GATHERED  whether traces are a legitimate gather  none.
! NWIH      number of words in trace header         none.
! NDPT      number of sample values in trace        none.
! TSTRT     starting time on trace                  none.
! DT        trace sample interval                   none.
! GRID      grid transformation structure           none.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!    1    HDR_SEQUENCE               Used as trace sequence number.
!    3    HDR_CURRENT_GROUP          Used as CDP number.
!    4    HDR_CURRENT_CHANNEL        Used as sequence number.
!    5    HDR_FOLD                   Used as fold number.
!    6    HDR_OFFSET                 Used as offset value.
!    7    HDR_MIDPOINT_XGRID         Used as x-line number.
!    8    HDR_MIDPOINT_YGRID         Used as inline number.
!   49    HDR_USER_49                Set to trace type (see list below)
!                                       1  = live seismic trace.
!                                       51 = stacking velocity trace.
!                                       43 = real{A} trace.
!                                       44 = real{B} trace.
!                                       42 = real{AB*} trace.
!                                       46 = imag{AB*} trace.
!   50    HDR_USER_50                Set to gather type (seel list below).
!                                       1  = offset.
!                                       2  = angle.
!   51    HDR_USER_51                Set to iteration number.
!   52    HDR_USER_52                Set to AVO angle.
!   52    HDR_USER_53                Set to dominant frequency.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2007-02-05  D. Glover  Added NULLIFY statements for Intel compiler
!002. 2006-06-20  B. Menger  Removed Unused Variables.
!  1. 2005-01-31  B. Lucas   Initial version.
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
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! None.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! This module acts primarily as a wrapper. The computational work is done
! in the B_AVEL and PPAVO primitives.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS General Parameters>
!            AVO and Velocity Analysis Parameters
!
!   `-Analysis Window-----------------------------------------------------------
!   | Start of Window =~~~~~~~~~~~~~~~`IIIIIIIII
!   | End of Window =~~~~~~~~~~~~~~~~~`IIIIIIIII
!   | Window Taper =~~~~~~~~~~~~~~~~~~`IIIIIIIII
!   `---------------------------------------------------------------------------
!   <PARMS Start of Window [STIM_WIN]>
!   <PARMS End of Window   [ETIM_WIN]>
!   <PARMS Window Taper    [TWIN_TPR]>
!   
!   `-Velocity Functions--------------------------------------------------------
!   | Select Stacking Velocity File [SVELNAME] `QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   | Use Curved Raypath =~~~~~~~~~~~~`CC
!   | Use Int. Vel. for Raypaths =~~~~`CC
!   | Select Interval Velocity File [IVELNAME] `QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!   | Number of Velocity Functions =~~`IIIIIIIII
!   | Nominal Velocity Function =~~~~~`IIIIIIIII
!   | Velocity Function Increment =~~~`FFFFFFFFF
!   `---------------------------------------------------------------------------
!   <PARMS Use Curved Raypath           [INCR_OPT]>   
!   <PARMS Use Int. Vel. for Raypaths   [USE_IVEL]>
!   <PARMS Number of Velocity Functions [NUM_VELS]>
!   <PARMS Nominal Velocity Function    [VELF_REF]>
!   <PARMS Velocity Function Increment  [VELF_INC]>
!   <PARMS SVELNAME[/ML=128/XST]>
!   <PARMS IVELNAME[/ML=128/XST]>
!
!   `-Filter Options------------------------------------------------------------
!   | Wavelet Type =~~~~~~~~~~~~~~~~~~`CCCCCCC
!   | Ricker Wavelet Freq. =~~~~~~~~~~`FFFFFF
!   | Bandpass Filter Freqs. =~~~~~~~~`SSSSSSSSSSSSSSS
!   | Length of Vel. Smooth Filter =~~`IIIIIIIII
!   | Length of Product Filter = ~~~~~`IIIIIIIII
!   `---------------------------------------------------------------------------
!   <PARMS Wavelet Type                 [FLT_TYPE]>
!   <PARMS Ricker Wavelet Freq.         [RIC_FREQ]>
!   <PARMS Bandpass Filter Freqs.       [BP_FREQS]>
!   <PARMS Length of Vel. Smooth Filter [LEN_SFLT]>
!   <PARMS Length of Product Filter     [LEN_OFLT]>
!   
!
!<NS Continued...>
!            AVO and Velocity Analysis Parameters
!
!   `-Miscellaneous-------------------------------------------------------------
!   | Gather Type =~~~~~~~~~~~~~~~~~~~`CCCCC
!   | Max. Incidence Angle =~~~~~~~~~~`FFFFFFFFF
!   | AVO Accuracy =~~~~~~~~~~~~~~~~~~`FFFFFFFFF
!   | V0 for Spherical Dev. =~~~~~~~~~`FFFFFFFFF
!   | Pass Pre-Stack Data =~~~~~~~~~~~`CC
!   | Primary Data Attributes =~~~~~~~`CCC
!   | Method of Computing A =~~~~~~~~~`CCCC
!   | Method of Computing B =~~~~~~~~~`C
!   | Norm. Product Traces by |A| =~~~`CC
!   | Length of A Norm. Window =~~~~~~`FFFFFFFFF
!   | Norm. Product Traces by |B| =~~~`CC
!   | Length of B Norm. Window =~~~~~~`FFFFFFFFF
!   | Suppress Scaling =~~~~~~~~~~~~~~`CC
!   | Suppress NMO Stretch Corr. =~~~~`CC
!   | Hori. Divergence Corr. =~~~~~~~~`CC
!   | Vert. Divergence Corr. =~~~~~~~~`CC
!   | Variable Trace Weighting =~~~~~~`CC
!   | Error Analysis =~~~~~~~~~~~~~~~~`CCCCCCC
!   `---------------------------------------------------------------------------
!   <PARMS Gather Type                 [GATH_OPT]>
!   <PARMS Max. Incidence Angle        [ANGL_MAX]>
!   <PARMS AVO Accuracy                [AVO_STAB]>
!   <PARMS V0 for Spherical Dev.       [VEL_ZERO]>
!   <PARMS Pass Pre-Stack Data         [PASS_OPT]>
!   <PARMS Primary Data Attributes     [PDAT_OPT]>
!   <PARMS Method of Computing A       [METHOD_A]>
!   <PARMS Method of Computing B       [METHOD_B]>
!   <PARMS Norm. Product Traces by |A| [NRMA_OPT]>
!   <PARMS Length of A Norm. Window    [AGC_WINA]>
!   <PARMS Norm. Product Traces by |B| [NRMB_OPT]>
!   <PARMS Length of B Norm. Window    [AGC_WINB]>
!   <PARMS Suppress Scaling            [SCAS_OPT]>
!   <PARMS Suppress NMO Stretch Corr.  [NMSC_OPT]>
!   <PARMS Hori. Divergence Corr.      [HDIV_OPT]>
!   <PARMS Vert. Divergence Corr.      [VDIV_OPT]>
!   <PARMS Variable Trace Weighting    [TRFO_OPT]>
!   <PARMS Error Analysis              [ERRS_OPT]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="AGC_WINA">
!<Tip> AGC window length for A normalization (msec).</Tip>
! Default = 50
! Allowed = greater than or equal to -10000, less than or equal to 10000
! AGC window length for A (msec). If negative, normalize by |A|**2.
!</Help>
!
!<Help KEYWORD="AGC_WINB">
!<Tip> AGC window length for B normalization (msec).</Tip>
! Default = 50
! Allowed = greater than or equal to -10000, less than or equal to 10000
! AGC window length for B (msec). If negative, normalize by |A|**2.
!</Help>
!
!<Help KEYWORD="ANGL_MAX">
!<Tip> Maximum incidence angle for AVO (degrees).</Tip>
! Default = 40
! Allowed = greater than or equal to 0, less than or equal to 90
! Maximum incidence angle for AVO (degrees).
!</Help>
!
!<Help KEYWORD="AVO_STAB">
!<Tip> AVO accuracy factor.</Tip>
! Default = 80
! Allowed = greater than or equal to 0, less than or equal to 500
! AVO accuracy factor. The greater the value, the more accurate
! the inversion, but it will be more sensitive to noise.
!</Help>
!
!<Help KEYWORD="BP_FREQS">
!<Tip> Bandpass frequencies: F1(0%)/F2(100%)/F3(100%)/F4(0%).</Tip>
! Default = 8/12.5/40/50
! Allowed = greater than or equal to 0
! Enter the four corner frequencies: F1(0%)/F2(100%)/F3(100%)/F4(0%).
!</Help>
!
!<Help KEYWORD="ERRS_OPT">
!<Tip> Type of error analysis.</Tip>
! Default = NO_ERR
! Allowed = NO_ERR, RESIDUAL, ALL, RUNS
! Type of error analysis:
!   NO_ERR -> No error traces generated.
!   RESIDUAL -> Residual AVO errors generated.
!   ALL -> Residual plus errors in A and B generated.
!   RUNS -> Display RUNS statistic only.
!</Help>
!
!<Help KEYWORD="FLT_TYPE">
!<Tip> Type of filter.</Tip>
! Default = RICKER
! Allowed = RICKER or BANDPASS
! Select the desired type of filter, Ricker or Bandpass.
!</Help>
!
!<Help KEYWORD="GATH_OPT">
!<Tip> Type of gather.</Tip>
! Default = OFFSET
! Allowed = OFFSET or ANGLE
! Select the type of input gather.
!</Help>
!
!<Help KEYWORD="HDIV_OPT">
!<Tip> Perform horizontal divergence correction?</Tip>
! Default = NO
! Allowed = NO, YES
! Perform horizontal divergence correction?
!</Help>
!
!<Help KEYWORD="INCR_OPT">
!<Tip> Use curved raypaths?</Tip>
! Default = NO
! Allowed = NO, YES
! Use curved raypaths? If 'YES', this causes AVO to be done with respect to
! true incidence angles instead of straight ray angles. Strongly affected
! by the length of the stacking velocity smoothing filter.
!</Help>
!
!<Help KEYWORD="IVELNAME">
!<Tip> Name of file containing interval velocities.</Tip>
! Default = none
! Allowed = any velocity file name.
! Name of file containing interval velocities.
!</Help>
!
!<Help KEYWORD="ETIM_WIN">
!<Tip> End time of analysis window (msec).</Tip>
! Default = 99999
! Allowed = greater than or equal to 0, less than or equal to 99999
! End time of analysis window (msec).
!</Help>
!
!<Help KEYWORD="STIM_WIN">
!<Tip> Start time of analysis window (msec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99999
! Start time of analysis window (msec).
!</Help>
!
!<Help KEYWORD="TWIN_TPR">
!<Tip> Taper of analysis window (msec).</Tip>
! Default = 50
! Allowed = greater than or equal to 0, less than or equal to 5000
! Taper of analysis window (msec).
!</Help>
!
!<Help KEYWORD="LEN_OFLT">
!<Tip> Length of product filter (msec).</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 99
! Length of product filter (msec).
!</Help>
!
!<Help KEYWORD="LEN_SFLT">
!<Tip> Length of velocity smoothing filter (msec).</Tip>
! Default = 20
! Allowed = greater than or equal to 0, less than or equal to 2000
! Length of velocity smoothing filter (msec).
!</Help>
!
!<Help KEYWORD="METHOD_A">
!<Tip> Method of computing A attribute.</Tip>
! Default = STACK
! Allowed = STACK, Z4, Z2
! Method of computing A attribute:
!   Z4 -> 4th order zero-offset stack.
!   Z2 -> 2nd order zero-offset stack.
!   Stack -> Conventional 'AFU' RAP stack.
! Z2 is valid only upto 30 degrees, whereas
! Z4 is valid well beyond this.
!</Help>
!
!<Help KEYWORD="METHOD_B">
!<Tip> Method of computing B attribute.</Tip>
! Default = B2
! Allowed = B4, B2, C4
! Method of computing B attribute:
!   B4 -> 4th order slope.
!   B2 -> 2nd order slope.
!   C4 -> 4th order quartic slope.
! B2 is valid only upto 30 degrees, whereas
! B4 is valid well beyond this. B4, however,
! needs angle coverage of at least 20 degrees
! in order to remain stable.
!</Help>
!
!<Help KEYWORD="NMSC_OPT">
!<Tip> Suppress NMO stretch correction?</Tip>
! Default = NO
! Allowed = NO, YES
! Suppress NMO stretch correction?
!</Help>
!
!<Help KEYWORD="NRMA_OPT">
!<Tip> Normalize product traces by |A|?</Tip>
! Default = NO
! Allowed = NO, YES
! Normalize product traces by |A|?
!</Help>
!
!<Help KEYWORD="NRMB_OPT">
!<Tip> Normalize product traces by |B|?</Tip>
! Default = NO
! Allowed = NO, YES
! Normalize product traces by |B|?
!</Help>
!
!<Help KEYWORD="NUM_VELS">
!<Tip> Number of velocity functions to apply.</Tip>
! Default = 0
! Allowed = greater than or equal to 0, less than or equal to 500
! Number of velocity functions to apply.
! Use '0' if the data are already NMO corrected.
!</Help>
!
!<Help KEYWORD="PASS_OPT">
!<Tip> Pass pre-stack data along with AVO attributes?</Tip>
! Default = NO
! Allowed = NO, YES
! Pass pre-stack data along with AVO attributes?
!</Help>
!
!<Help KEYWORD="PDAT_OPT">
!<Tip> Primary data attributes.</Tip>
! Default = AVEL
! Allowed = AB, AVO, AVEL, ANAL
! Selection of primary data attributes:
!   AB   -> RE(A),   RE(B).
!   AVO  -> RE(A),   RE(AB*).
!   AVEL -> RE(A),   IM(AB*).
!   ANAL -> RE(AB*), IM(AB*).
!</Help>
!
!<Help KEYWORD="RIC_FREQ">
!<Tip> Ricker center frequency (Hz).</Tip>
! Default = 30
! Allowed = greater than or equal to 5, less than or equal to 50
! Ricker center frequency (Hz) for NMO de-stretching.
!</Help>
!
!<Help KEYWORD="SCAS_OPT">
!<Tip> Suppress scaling based on first live trace?</Tip>
! Default = NO
! Allowed = NO, YES
! Suppress scaling based on first live trace?
!</Help>
!
!<Help KEYWORD="SELECT_IVELNAME">
!<Tip> Selection dialog for velocity files.</Tip>
! Default = N/A
! Allowed = N/A
! Selection dialog for velocity files.
!</Help>
!
!<Help KEYWORD="SELECT_SVELNAME">
!<Tip> Selection dialog for velocity files.</Tip>
! Default = N/A
! Allowed = N/A
! Selection dialog for velocity files.
!</Help>
!
!<Help KEYWORD="SVELNAME">
!<Tip> Name of file containing stacking velocities.</Tip>
! Default = none
! Allowed = any velocity file name
! Name of file containing stacking velocities.
!</Help>
!
!<Help KEYWORD="TRFO_OPT">
!<Tip> Use variables trace weighting?</Tip>
! Default = NO
! Allowed = NO, YES
! Use variable trace weighting?
!</Help>
!
!<Help KEYWORD="USE_IVEL">
!<Tip> Use interval velocities for raypath calculation?</Tip>
! Default = NO
! Allowed = NO, YES
! Use interval velocities for raypath calculation?
! 'YES', indicates that interval velocities independent of
! the revised stacking velocities will be used in raypath
! calculations.
!</Help>
!
!<Help KEYWORD="VDIV_OPT">
!<Tip> Perform vertical divergence correction?</Tip>
! Default = NO
! Allowed = NO, YES
! Perform vertical divergence correction?
!</Help>
!
!<Help KEYWORD="VELF_INC">
!<Tip> Velocity function increment (ft/s or m/s); may be negative.</Tip>
! Default = 10
! Allowed = greater than or equal to -1000, less than or equal to 1000
! Velocity function increment (ft/s or m/s); may be negative.
!</Help>
!
!<Help KEYWORD="VELF_REF">
!<Tip> Velocity function to be nominal.</Tip>
! Default = 1
! Allowed = greater than or equal to 0, less than or equal to 99999
! Velocity function to be nominal. This may be used to generate
! asymmetric distributions.
!</Help>
!
!<Help KEYWORD="VEL_ZERO">
!<Tip> V0 (ft/s or m/s) for spherical divergence.</Tip>
! Default = 4800
! Allowed = greater than or equal to 0, less than or equal to 99999
! V0 (ft/s or m/s) for spherical divergence.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!----------------------------- start of module ------------------------------!!
!!----------------------------- start of module ------------------------------!!
!!----------------------------- start of module ------------------------------!!


      module avovan_module
      use pc_module
      use named_constants_module
      use memman_module
      use grid_module            ! if you need the grid transformation.
      use pathchoose_module      ! if you use file name parameters.
      use pathcheck_module       ! if you use file name parameters.
      use nmo_module
      use bavel_module
      use ppavo_module

      implicit none
      private
      public :: avovan_create
      public :: avovan_initialize
      public :: avovan_update
      public :: avovan_delete
      public :: avovan            ! main trace processing routine.
      public :: avovan_wrapup

      character(len=100),public,save :: AVOVAN_IDENT = &
'$Id: avovan.f90 1.3 2007-02-05 12.36.28 Glover prod sps $'


!!------------------------ parameter structure -------------------------------!!
!!------------------------ parameter structure -------------------------------!!
!!------------------------ parameter structure -------------------------------!!

      type,public :: bavel_ptr
         type(bavel_struct), pointer :: obj
      end type bavel_ptr

      type,public :: avovan_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        integer                    :: ipn      ! process number.
        integer                    :: numtr    ! max number of input traces.
        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.
        real                       :: tstrt    ! time of 1st trace sample (sec).
        real                       :: dt       ! trace sample interval (sec).
        type(grid_struct)          :: grid     ! grid transform.

        integer                        :: etim_win
        integer                        :: stim_win
        integer                        :: twin_tpr
        integer                        :: len_oflt
        integer                        :: len_sflt
        integer                        :: num_vels
        integer                        :: velf_ref

        real                           :: velf_inc
        real                           :: vel_zero
        real                           :: ric_freq
        real                           :: agc_wina
        real                           :: agc_winb
        real                           :: angl_max
        real                           :: avo_stab

        character(len=16)              :: bp_freqs
        character(len=8)               :: errs_opt
        character(len=8)               :: flt_type
        character(len=6)               :: gath_opt
        character(len=3)               :: hdiv_opt
        character(len=3)               :: incr_opt
        character(len=5)               :: method_a
        character(len=2)               :: method_b
        character(len=3)               :: nmsc_opt
        character(len=3)               :: nrma_opt
        character(len=3)               :: nrmb_opt
        character(len=3)               :: pass_opt
        character(len=4)               :: pdat_opt
        character(len=3)               :: scas_opt
        character(len=3)               :: trfo_opt
        character(len=3)               :: use_ivel
        character(len=3)               :: vdiv_opt
        character(len=20)              :: survey_units
        character(len=FILENAME_LENGTH) :: ivelname
        character(len=FILENAME_LENGTH) :: svelname
        character(len=FILENAME_LENGTH) :: pathname_dir

        type(pathchoose_struct), pointer          :: svelname_pathchoose
        type(pathchoose_struct), pointer          :: ivelname_pathchoose
        type(bavel_ptr), pointer, dimension(:)   :: bavel
        real,             pointer, dimension(:,:) :: tr_g
        double precision, pointer, dimension(:,:) :: hd_g        

      end type avovan_struct

      character(len=FILENAME_LENGTH) :: svelname
      character(len=FILENAME_LENGTH) :: ivelname
      integer                        :: errs_opt
      integer                        :: flt_type
      integer                        :: gath_opt
      integer                        :: hdiv_opt
      integer                        :: incr_opt
      integer                        :: method_a
      integer                        :: method_b
      integer                        :: nmsc_opt
      integer                        :: nrma_opt
      integer                        :: nrmb_opt
      integer                        :: pass_opt
      integer                        :: pdat_opt
      integer                        :: scas_opt
      integer                        :: trfo_opt
      integer                        :: use_ivel
      integer                        :: vdiv_opt
      integer                        :: len_oflt
      real                           :: agc_wina
      real                           :: agc_winb

!!------------------------------- interfaces ---------------------------------!!
!!------------------------------- interfaces ---------------------------------!!
!!------------------------------- interfaces ---------------------------------!!


!!---------------------------------- data ------------------------------------!!
!!---------------------------------- data ------------------------------------!!
!!---------------------------------- data ------------------------------------!!

      integer,                    save :: lunprint  ! unit number for printing.
      type(avovan_struct),pointer,save :: object    ! needed for traps.

      contains


!!------------------------------- create -------------------------------------!!
!!------------------------------- create -------------------------------------!!
!!------------------------------- create -------------------------------------!!


      subroutine avovan_create (obj)
      type(avovan_struct),pointer :: obj       ! arguments
      integer                     :: ierr      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ierr)
      if (ierr /= 0) then
         call pc_error ('Unable to allocate obj in avovan_create.')
      end if

      allocate(obj%bavel(1))

      nullify (obj%hd_g)
      nullify (obj%tr_g)
      nullify (obj%svelname_pathchoose) ! jpa
      nullify (obj%ivelname_pathchoose) ! jpa

      call pathchoose_create (obj%svelname_pathchoose, 'svelname' , '.vel')
      call pathchoose_create (obj%ivelname_pathchoose, 'ivelname' , '.vel')
      call bavel_create (obj%bavel(1)%obj)
      call nmo_create (obj%bavel(1)%obj%svel_nmo)
      call nmo_create (obj%bavel(1)%obj%ivel_nmo)
 
      call avovan_initialize (obj)
      end subroutine avovan_create


!!-------------------------------- delete ------------------------------------!!
!!-------------------------------- delete ------------------------------------!!
!!-------------------------------- delete ------------------------------------!!


      subroutine avovan_delete (obj)
      type(avovan_struct),pointer :: obj       ! arguments
      integer                     :: ierr      ! for error checking

      call avovan_wrapup (obj)

      call pathchoose_delete (obj%svelname_pathchoose)
      call pathchoose_delete (obj%ivelname_pathchoose)
      call bavel_delete (obj%bavel(1)%obj)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning ("error deallocating obj in avovan_delete")
      end subroutine avovan_delete


!!------------------------------ initialize ----------------------------------!!
!!------------------------------ initialize ----------------------------------!!
!!------------------------------ initialize ----------------------------------!!


      subroutine avovan_initialize (obj)
      type(avovan_struct),intent(inout) :: obj       ! arguments

      obj%agc_wina = 50.0
      obj%agc_winb = 50.0
      obj%angl_max = 40.0
      obj%avo_stab = 80.0
      obj%bp_freqs = '8/12.5/40/50'
      obj%errs_opt = 'NO_ERR'
      obj%flt_type = 'RICKER'
      obj%gath_opt = 'OFFSET'
      obj%hdiv_opt = 'NO'
      obj%incr_opt = 'NO'
      obj%ivelname = 'NONE'
      obj%etim_win = 99999
      obj%stim_win = 0
      obj%twin_tpr = 50
      obj%len_oflt = 0
      obj%len_sflt = 20
      obj%method_a = 'STACK'
      obj%method_b = 'B2'
      obj%nmsc_opt = 'NO'
      obj%nrma_opt = 'NO'
      obj%nrmb_opt = 'NO'
      obj%num_vels = 0
      obj%pass_opt = 'NO'
      obj%pdat_opt = 'AVEL'
      obj%ric_freq = 30.0
      obj%scas_opt = 'NO'
      obj%svelname = 'NONE'
      obj%trfo_opt = 'NO'
      obj%use_ivel = 'NO'
      obj%vdiv_opt = 'NO'
      obj%velf_inc = 10.0
      obj%velf_ref = 1
      obj%vel_zero = 4800.0
      
      call bavel_init_parms (obj%bavel(1)%obj)

      call avovan_update (obj)
      end subroutine avovan_initialize


!!--------------------------- start of update --------------------------------!!
!!--------------------------- start of update --------------------------------!!
!!--------------------------- start of update --------------------------------!!


      subroutine avovan_update (obj)
      type(avovan_struct),intent(inout),target :: obj             ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!--------------------------- read parameters --------------------------------!!
!!--------------------------- read parameters --------------------------------!!
!!--------------------------- read parameters --------------------------------!!

      
      if (pathchoose_update (obj%svelname_pathchoose, obj%svelname)) return
      if (pathchoose_update (obj%ivelname_pathchoose, obj%ivelname)) return

      obj%ipn = pc_get_ipn()

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('gathered', obj%gathered)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('tstrt'   , obj%tstrt)
      call pc_get_global ('dt'      , obj%dt)
      call pc_get_global ('grid'    , obj%grid)

      call pc_get('AGC_WINA', obj%agc_wina, avovan_agc_wina)
      call pc_get('AGC_WINB', obj%agc_winb, avovan_agc_winb)
      call pc_get('ANGL_MAX', obj%angl_max, avovan_angl_max)
      call pc_get('AVO_STAB', obj%avo_stab, avovan_avo_stab)
      call pc_get('BP_FREQS', obj%bp_freqs, avovan_bp_freqs)
      call pc_get('ERRS_OPT', obj%errs_opt, avovan_errs_opt)
      call pc_get('FLT_TYPE', obj%flt_type, avovan_flt_type)
      call pc_get('GATH_OPT', obj%gath_opt, avovan_gath_opt)
      call pc_get('HDIV_OPT', obj%hdiv_opt, avovan_hdiv_opt)
      call pc_get('INCR_OPT', obj%incr_opt, avovan_incr_opt)
      call pc_get('IVELNAME', obj%ivelname, avovan_ivelname)
      call pc_get('ETIM_WIN', obj%etim_win, avovan_etim_win)
      call pc_get('STIM_WIN', obj%stim_win, avovan_stim_win)
      call pc_get('TWIN_TPR', obj%twin_tpr, avovan_twin_tpr)
      call pc_get('LEN_OFLT', obj%len_oflt, avovan_len_oflt)
      call pc_get('LEN_SFLT', obj%len_sflt, avovan_len_sflt)
      call pc_get('METHOD_A', obj%method_a, avovan_method_a)
      call pc_get('METHOD_B', obj%method_b, avovan_method_b)
      call pc_get('NMSC_OPT', obj%nmsc_opt, avovan_nmsc_opt)
      call pc_get('NRMA_OPT', obj%nrma_opt, avovan_nrma_opt)
      call pc_get('NRMB_OPT', obj%nrmb_opt, avovan_nrmb_opt)
      call pc_get('NUM_VELS', obj%num_vels, avovan_num_vels)
      call pc_get('PASS_OPT', obj%pass_opt, avovan_pass_opt)
      call pc_get('PDAT_OPT', obj%pdat_opt, avovan_pdat_opt)
      call pc_get('RIC_FREQ', obj%ric_freq, avovan_ric_freq)
      call pc_get('SCAS_OPT', obj%scas_opt, avovan_scas_opt)
      call pc_get('SVELNAME', obj%svelname, avovan_svelname)
      call pc_get('TRFO_OPT', obj%trfo_opt, avovan_trfo_opt)
      call pc_get('USE_IVEL', obj%use_ivel, avovan_use_ivel)
      call pc_get('VDIV_OPT', obj%vdiv_opt, avovan_vdiv_opt)
      call pc_get('VELF_INC', obj%velf_inc, avovan_velf_inc)
      call pc_get('VELF_REF', obj%velf_ref, avovan_velf_ref)
      call pc_get('VEL_ZERO', obj%vel_zero, avovan_vel_zero)

!      *** screen traps ***

      call pc_call_screen_trap('CONTINUED        ', avovan_continued)
      call pc_call_screen_trap('GENERALPARAMETERS', avovan_generalparameters)

!      *** end trap ***

      call pc_call_end_trap(avovan_end)


!!--------------------------- verify parameters ------------------------------!!
!!--------------------------- verify parameters ------------------------------!!
!!--------------------------- verify parameters ------------------------------!!


!!------------------------- call processes internally ------------------------!!
!!------------------------- call processes internally ------------------------!!
!!------------------------- call processes internally ------------------------!!

      call pc_get_pdata('SURVEY_UNITS', obj%survey_units)
      call pc_get_jdata('PATHNAME_DIR', obj%pathname_dir)

      call pc_put_global('OPT_NMO', 'STK_VEL')
      call pc_put_global('PATHNAME', obj%svelname)
      call pc_put_process('OPT_NMO', 'STK_VEL')
      call pc_put_process('PATHNAME', obj%svelname)
      if(associated(obj%bavel(1)%obj%svel_nmo)) then
         call nmo_update(obj%bavel(1)%obj%svel_nmo)
      else
         call nmo_create(obj%bavel(1)%obj%svel_nmo)
      end if
      if(obj%use_ivel .eq. 'YES') then
         call pc_put_global('OPT_NMO', 'DIX_VEL')
         call pc_put_global('PATHNAME', obj%ivelname)
         call pc_put_process('OPT_NMO', 'DIX_VEL')
         call pc_put_process('PATHNAME', obj%ivelname)
         if(associated(obj%bavel(1)%obj%ivel_nmo)) then
            call nmo_update(obj%bavel(1)%obj%ivel_nmo)
         else
            call nmo_create(obj%bavel(1)%obj%ivel_nmo)
         end if
      end if

!!------------------------- write parameters ---------------------------------!!
!!------------------------- write parameters ---------------------------------!!
!!------------------------- write parameters ---------------------------------!!


      call pc_put_options_field('ERRS_OPT', (/'NO_ERR  ', 'RESIDUAL',&
                                              'ALL     ', 'RUNS    '/) )
      call pc_put_options_field('FLT_TYPE', (/'RICKER  ', 'BANDPASS'/) )
      call pc_put_options_field('GATH_OPT', (/'OFFSET', 'ANGLE '/) )
      call pc_put_options_field('HDIV_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('INCR_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('METHOD_A', (/'STACK', 'Z4   ', 'Z2   '/) )
      call pc_put_options_field('METHOD_B', (/'B4', 'B2', 'C4'/) )
      call pc_put_options_field('NMSC_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('NRMA_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('NRMB_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('PASS_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('PDAT_OPT', (/'AB  ', 'AVO ',&
                                              'AVEL', 'ANAL'/) )
      call pc_put_options_field('SCAS_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('TRFO_OPT', (/'NO ', 'YES'/) )
      call pc_put_options_field('USE_IVEL', (/'NO ', 'YES'/) )
      call pc_put_options_field('VDIV_OPT', (/'NO ', 'YES'/) )

      obj%numtr = obj%numtr + (2 * obj%num_vels) + 1
      call pc_put_global ('numtr'   , obj%numtr)
      call pc_put_global ('gathered', obj%gathered)
      call pc_put_global ('nwih'    , obj%nwih)
      call pc_put_global ('ndpt'    , obj%ndpt)
      call pc_put_global ('tstrt'   , obj%tstrt)
      call pc_put_global ('dt'      , obj%dt)
      call pc_put_global ('grid'    , obj%grid)

      call pc_put('AGC_WINA', obj%agc_wina)
      call pc_put('AGC_WINB', obj%agc_winb)
      call pc_put('ANGL_MAX', obj%angl_max)
      call pc_put('AVO_STAB', obj%avo_stab)
      call pc_put('BP_FREQS', obj%bp_freqs)
      call pc_put('ERRS_OPT', obj%errs_opt)
      call pc_put('FLT_TYPE', obj%flt_type)
      call pc_put('GATH_OPT', obj%gath_opt)
      call pc_put('HDIV_OPT', obj%hdiv_opt)
      call pc_put('INCR_OPT', obj%incr_opt)
      call pc_put('IVELNAME', obj%ivelname)
      call pc_put('ETIM_WIN', obj%etim_win)
      call pc_put('STIM_WIN', obj%stim_win)
      call pc_put('TWIN_TPR', obj%twin_tpr)
      call pc_put('LEN_OFLT', obj%len_oflt)
      call pc_put('LEN_SFLT', obj%len_sflt)
      call pc_put('METHOD_A', obj%method_a)
      call pc_put('METHOD_B', obj%method_b)
      call pc_put('NMSC_OPT', obj%nmsc_opt)
      call pc_put('NRMA_OPT', obj%nrma_opt)
      call pc_put('NRMB_OPT', obj%nrmb_opt)
      call pc_put('NUM_VELS', obj%num_vels)
      call pc_put('PASS_OPT', obj%pass_opt)
      call pc_put('PDAT_OPT', obj%pdat_opt)
      call pc_put('RIC_FREQ', obj%ric_freq)
      call pc_put('SCAS_OPT', obj%scas_opt)
      call pc_put('SVELNAME', obj%svelname)
      call pc_put('TRFO_OPT', obj%trfo_opt)
      call pc_put('USE_IVEL', obj%use_ivel)
      call pc_put('VDIV_OPT', obj%vdiv_opt)
      call pc_put('VELF_INC', obj%velf_inc)
      call pc_put('VELF_REF', obj%velf_ref)
      call pc_put('VEL_ZERO', obj%vel_zero)


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

      select case (obj%incr_opt)
         case ('NO')
            incr_opt = 0
         case ('YES')
            incr_opt = 1
      end select

      select case (obj%use_ivel)
         case ('NO')
            use_ivel = 0
         case ('YES')
            use_ivel = 1
      end select 

      select case (obj%flt_type)
         case ('RICKER')
            flt_type = 1
         case ('BANDPASS')
            flt_type = 2
      end select

      select case (obj%gath_opt)
         case ('OFFSET')
            gath_opt = 1
         case ('ANGLE')
            gath_opt = 2
      end select

      select case (obj%pdat_opt)
         case ('AB')
            pdat_opt = 4
         case ('AVO')
            pdat_opt = 3
         case ('AVEL')
            pdat_opt = 2
         case ('ANAL')
            pdat_opt = 1
      end select

      select case (obj%pass_opt)
         case ('NO')
            pass_opt = 0
         case ('YES')
            pass_opt = 1
            if(pdat_opt .ne. 4) pass_opt = 0
      end select

      len_oflt = obj%len_oflt
      if(pdat_opt .eq. 4) len_oflt = 0 

      select case (obj%method_a)
         case ('STACK')
            method_a = 1
         case ('Z2')
            method_a = 2
         case ('Z4')
            method_a = 3
      end select

      select case (obj%method_b)
         case ('B2')
            method_b = 1
         case ('B4')
            method_b = 2
         case ('C4')
            method_b = 3
      end select

      agc_wina = obj%agc_wina
      select case (obj%nrma_opt)
         case ('NO')
            nrma_opt = 0
            agc_wina = 0.0
         case ('YES')
            nrma_opt = 1
      end select

      agc_winb = obj%agc_winb
      select case (obj%nrmb_opt)
         case ('NO')
            nrmb_opt = 0
            agc_winb = 0.0
         case ('YES')
            nrmb_opt = 1
      end select

      select case (obj%scas_opt)
         case ('YES')
            scas_opt = 0
         case ('NO')
            scas_opt = 1
      end select
      if(pass_opt .eq. 1) scas_opt = 0

      select case (obj%nmsc_opt)
         case ('YES')
            nmsc_opt = 0
         case ('NO')
            nmsc_opt = 1
      end select

      select case (obj%hdiv_opt)
         case ('NO')
            hdiv_opt = 0
         case ('YES')
            hdiv_opt = 1
      end select

      select case (obj%vdiv_opt)
         case ('NO')
            vdiv_opt = 0
         case ('YES')
            vdiv_opt = 1
      end select

      select case (obj%trfo_opt)
         case ('NO')
            trfo_opt = 0
         case ('YES')
            trfo_opt = 1
      end select

      select case (obj%errs_opt)
         case ('NO_ERR')
            errs_opt = 0
         case ('RESIDUAL')
            errs_opt = 1
         case ('ALL')
            errs_opt = 3
         case ('RUNS')
            errs_opt = 4
      end select
      
      call bavel_set_parm(obj%bavel(1)%obj, 'stim_win', obj%stim_win)
      call bavel_set_parm(obj%bavel(1)%obj, 'etim_win', obj%etim_win)      
      call bavel_set_parm(obj%bavel(1)%obj, 'twin_tpr', obj%twin_tpr)
      call bavel_set_parm(obj%bavel(1)%obj, 'svelname', obj%svelname)
      call bavel_set_parm(obj%bavel(1)%obj, 'incr_opt', incr_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'use_ivel', use_ivel)
      call bavel_set_parm(obj%bavel(1)%obj, 'ivelname', obj%ivelname)
      call bavel_set_parm(obj%bavel(1)%obj, 'num_vels', obj%num_vels)
      call bavel_set_parm(obj%bavel(1)%obj, 'velf_ref', obj%velf_ref)
      call bavel_set_parm(obj%bavel(1)%obj, 'velf_inc', obj%velf_inc)
      call bavel_set_parm(obj%bavel(1)%obj, 'flt_type', flt_type)
      call bavel_set_parm(obj%bavel(1)%obj, 'gath_opt', gath_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'ric_freq', obj%ric_freq)
      call bavel_set_parm(obj%bavel(1)%obj, 'bp_freqs', obj%bp_freqs)
      call bavel_set_parm(obj%bavel(1)%obj, 'len_sflt', obj%len_sflt)
      call bavel_set_parm(obj%bavel(1)%obj, 'len_oflt', len_oflt)  
      call bavel_set_parm(obj%bavel(1)%obj, 'angl_max', obj%angl_max)
      call bavel_set_parm(obj%bavel(1)%obj, 'avo_stab', obj%avo_stab)
      call bavel_set_parm(obj%bavel(1)%obj, 'vel_zero', obj%vel_zero)
      call bavel_set_parm(obj%bavel(1)%obj, 'pass_opt', pass_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'pdat_opt', pdat_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'method_a', method_a)
      call bavel_set_parm(obj%bavel(1)%obj, 'nrma_opt', nrma_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'agc_wina', agc_wina)
      call bavel_set_parm(obj%bavel(1)%obj, 'method_b', method_b)
      call bavel_set_parm(obj%bavel(1)%obj, 'nrmb_opt', nrmb_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'agc_winb', agc_winb)
      call bavel_set_parm(obj%bavel(1)%obj, 'scas_opt', scas_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'nmsc_opt', nmsc_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'hdiv_opt', hdiv_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'vdiv_opt', vdiv_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'trfo_opt', trfo_opt)
      call bavel_set_parm(obj%bavel(1)%obj, 'errs_opt', errs_opt)

!!------------------------- prepare for execution ----------------------------!!
!!------------------------- prepare for execution ----------------------------!!
!!------------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.


!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!


      end subroutine avovan_update


!!--------------------------------- traps ------------------------------------!!
!!--------------------------------- traps ------------------------------------!!
!!--------------------------------- traps ------------------------------------!!


! *** Trap for variable AGC_WINA ***

      subroutine avovan_agc_wina(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%agc_wina)
      if(object%agc_wina .lt. -10000 .or. object%agc_wina .gt. 10000) then
         write (msg, '(a,a)') keyword,&
     &' must be between -10000 and 10000. &
     & Restoring to default of 50.'
         call pc_error(msg)
         object%agc_wina = 50
         call pc_put(keyword, object%agc_wina)
      end if

      return
      end subroutine avovan_agc_wina

! *** Trap for variable AGC_WINB ***

      subroutine avovan_agc_winb(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%agc_winb)
      if(object%agc_winb .lt. -10000 .or. object%agc_winb .gt. 10000) then
         write (msg, '(a,a)') keyword,&
     &' must be between -10000 and 10000.&
     & Restoring to default of 50.'
         call pc_error(msg)
         object%agc_winb = 50
         call pc_put(keyword, object%agc_winb)
      end if

      return
      end subroutine avovan_agc_winb

! *** Trap for variable ANGL_MAX ***

      subroutine avovan_angl_max(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%angl_max)
      if(object%angl_max .lt. 0 .or. object%angl_max .gt. 90) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 90.&
     & Restoring to default of 40.'
         call pc_error(msg)
         object%angl_max = 40
         call pc_put(keyword, object%angl_max)
      end if

      return
      end subroutine avovan_angl_max

! *** Trap for variable AVO_STAB ***

      subroutine avovan_avo_stab(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%avo_stab)
      if(object%avo_stab .lt. 0 .or. object%avo_stab .gt. 500) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 500.&
     & Restoring to default of 80.'
         call pc_error(msg)
         object%avo_stab = 80
         call pc_put(keyword, object%avo_stab)
      end if

      return
      end subroutine avovan_avo_stab

! *** Trap for variable BP_FREQS ***

      subroutine avovan_bp_freqs(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument


      return
      end subroutine avovan_bp_freqs

! *** Trap for variable ERRS_OPT ***

      subroutine avovan_errs_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_errs_opt

! *** Trap for variable FLT_TYPE ***

      subroutine avovan_flt_type(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('flt_type', object%flt_type)

      if(object%flt_type .eq. 'RICKER') then
         flag = .true.
         call pc_put_sensitive_field_flag('ric_freq', flag)
         flag = .false.    
         call pc_put_sensitive_field_flag('bp_freqs', flag)
      else
         flag = .false.
         call pc_put_sensitive_field_flag('ric_freq', flag)
         flag = .true.    
         call pc_put_sensitive_field_flag('bp_freqs', flag)
      end if

      return
      end subroutine avovan_flt_type

! *** Trap for variable GATH_OPT ***

      subroutine avovan_gath_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_gath_opt

! *** Trap for variable HDIV_OPT ***

      subroutine avovan_hdiv_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('hdiv_opt', object%hdiv_opt)
      call pc_get('vdiv_opt', object%vdiv_opt)
      if(object%hdiv_opt .eq. 'YES' .or. object%vdiv_opt .eq. 'YES') then
         flag = .true.
      else
         flag = .false.
      end if
      call pc_put_sensitive_field_flag('vel_zero', flag)

      return
      end subroutine avovan_hdiv_opt

! *** Trap for variable INCR_OPT ***

      subroutine avovan_incr_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_incr_opt

! *** Trap for variable IVELNAME ***

      subroutine avovan_ivelname(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_ivelname

! *** Trap for variable ETIM_WIN ***

      subroutine avovan_etim_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%etim_win)
      if(object%etim_win .lt. 0 .or. object%etim_win .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 99999.'
         call pc_error(msg)
         object%etim_win = 99999
         call pc_put(keyword, object%etim_win)
      end if

      return
      end subroutine avovan_etim_win

! *** Trap for variable STIM_WIN ***

      subroutine avovan_stim_win(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%stim_win)
      if(object%stim_win .lt. 0 .or. object%stim_win .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%stim_win = 0
         call pc_put(keyword, object%stim_win)
      end if

      return
      end subroutine avovan_stim_win

! *** Trap for variable TWIN_TPR ***

      subroutine avovan_twin_tpr(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%twin_tpr)
      if(object%twin_tpr .lt. 0 .or. object%twin_tpr .gt. 5000) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 5000.&
     & Restoring to default of 50.'
         call pc_error(msg)
         object%twin_tpr = 50
         call pc_put(keyword, object%twin_tpr)
      end if

      return
      end subroutine avovan_twin_tpr

! *** Trap for variable LEN_OFLT ***

      subroutine avovan_len_oflt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%len_oflt)
      if(object%len_oflt .lt. 0 .or. object%len_oflt .gt. 99) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%len_oflt = 0
         call pc_put(keyword, object%len_oflt)
      end if

      return
      end subroutine avovan_len_oflt

! *** Trap for variable LEN_SFLT ***

      subroutine avovan_len_sflt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%len_sflt)
      if(object%len_sflt .lt. 0 .or. object%len_sflt .gt. 2000) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 2000.&
     & Restoring to default of 20.'
         call pc_error(msg)
         object%len_sflt = 20
         call pc_put(keyword, object%len_sflt)
      end if

      return
      end subroutine avovan_len_sflt

! *** Trap for variable METHOD_A ***

      subroutine avovan_method_a(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_method_a

! *** Trap for variable METHOD_B ***

      subroutine avovan_method_b(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_method_b

! *** Trap for variable NMSC_OPT ***

      subroutine avovan_nmsc_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_nmsc_opt

! *** Trap for variable NRMA_OPT ***

      subroutine avovan_nrma_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('nrma_opt', object%nrma_opt)

      if(object%nrma_opt .eq. 'YES') then
         flag = .true.
      else
         flag = .false.
      end if
      call pc_put_sensitive_field_flag('agc_wina', flag)     

      return
      end subroutine avovan_nrma_opt

! *** Trap for variable NRMB_OPT ***

      subroutine avovan_nrmb_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('nrmb_opt', object%nrmb_opt)

      if(object%nrmb_opt .eq. 'YES') then
         flag = .true.
      else
         flag = .false.
      end if
      call pc_put_sensitive_field_flag('agc_winb', flag)     

      return
      end subroutine avovan_nrmb_opt

! *** Trap for variable NUM_VELS ***

      subroutine avovan_num_vels(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg
      logical :: flag

      call pc_get(keyword, object%num_vels)
      if(object%num_vels .lt. 0 .or. object%num_vels .gt. 500) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 500.&
     & Restoring to default of 0.'
         call pc_error(msg)
         object%num_vels = 0
         call pc_put(keyword, object%num_vels)
      end if

      if(object%num_vels .gt. 1) then
         flag = .true.
      else
         object%velf_ref = 1
         call pc_put('velf_ref', object%velf_ref)
         flag = .false.
      end if
      call pc_put_sensitive_field_flag('velf_ref', flag)
      call pc_put_sensitive_field_flag('velf_inc', flag)

      return
      end subroutine avovan_num_vels

! *** Trap for variable PASS_OPT ***

      subroutine avovan_pass_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get(keyword, object%pass_opt)

      if(object%pass_opt .eq. 'YES') then
         object%pdat_opt = 'AB'
         object%scas_opt = 'YES'
         call pc_put('pdat_opt', object%pdat_opt)
         call pc_put('scas_opt', object%scas_opt)
         flag = .false.
      else
         flag = .true.
      end if
      call pc_put_sensitive_field_flag('pdat_opt', flag)
      call pc_put_sensitive_field_flag('scas_opt', flag)

      return
      end subroutine avovan_pass_opt

! *** Trap for variable PDAT_OPT ***

      subroutine avovan_pdat_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get(keyword, object%pdat_opt)

      if(object%pdat_opt .eq. 'AB') then
         object%nrma_opt = 'NO'
         object%nrmb_opt = 'NO'
         call pc_put('nrma_opt', object%nrma_opt)
         call pc_put('nrmb_opt', object%nrmb_opt)
         flag = .false.
         call pc_put_sensitive_field_flag('agc_wina', flag)
         call pc_put_sensitive_field_flag('agc_winb', flag)
      else
         flag = .true.
      end if
      call pc_put_sensitive_field_flag('len_oflt', flag)
      call pc_put_sensitive_field_flag('nrma_opt', flag)
      call pc_put_sensitive_field_flag('nrmb_opt', flag)

      return
      end subroutine avovan_pdat_opt

! *** Trap for variable RIC_FREQ ***

      subroutine avovan_ric_freq(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%ric_freq)
      if(object%ric_freq .lt. 5 .or. object%ric_freq .gt. 50) then
         write (msg, '(a,a)') keyword,&
     &' must be between 5 and 50.&
     & Restoring to default of 30.'
         call pc_error(msg)
         object%ric_freq = 30
         call pc_put(keyword, object%ric_freq)
      end if

      return
      end subroutine avovan_ric_freq

! *** Trap for variable SCAS_OPT ***

      subroutine avovan_scas_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_scas_opt

! *** Trap for variable SVELNAME ***

      subroutine avovan_svelname(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_svelname

! *** Trap for variable TRFO_OPT ***

      subroutine avovan_trfo_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_trfo_opt

! *** Trap for variable USE_IVEL ***

      subroutine avovan_use_ivel(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('use_ivel', object%use_ivel)

      if(object%use_ivel .eq. 'YES') then
         flag = .true.
      else
         flag = .false.
      end if
      call pc_put_sensitive_field_flag('select_ivelname', flag)
      call pc_put_sensitive_field_flag('ivelname', flag)

      return
      end subroutine avovan_use_ivel

! *** Trap for variable VDIV_OPT ***

      subroutine avovan_vdiv_opt(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      logical :: flag

      call pc_get('hdiv_opt', object%hdiv_opt)
      call pc_get('vdiv_opt', object%vdiv_opt)
      if(object%hdiv_opt .eq. 'YES' .or. object%vdiv_opt .eq. 'YES') then
         flag = .true.
      else
         flag = .false.
      end if
      call pc_put_sensitive_field_flag('vel_zero', flag)

      return
      end subroutine avovan_vdiv_opt

! *** Trap for variable VELF_INC ***

      subroutine avovan_velf_inc(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%velf_inc)
      if(object%velf_inc .lt. -1000 .or. object%velf_inc .gt. 1000) then
         write (msg, '(a,a)') keyword,&
     &' must be between -1000 and 1000.&
     & Restoring to default of 10.'
         call pc_error(msg)
         object%velf_inc = 10
         call pc_put(keyword, object%velf_inc)
      end if

      return
      end subroutine avovan_velf_inc

! *** Trap for variable VELF_REF ***

      subroutine avovan_velf_ref(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%velf_ref)
      if(object%velf_ref .lt. 0 .or. object%velf_ref .gt. 500) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 500.&
     & Restoring to default of 1.'
         call pc_error(msg)
         object%velf_ref = 1
         call pc_put(keyword, object%velf_ref)
      end if

      return
      end subroutine avovan_velf_ref

! *** Trap for variable VEL_ZERO ***

      subroutine avovan_vel_zero(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument
      character(len=256) :: msg

      call pc_get(keyword, object%vel_zero)
      if(object%vel_zero .lt. 0 .or. object%vel_zero .gt. 99999) then
         write (msg, '(a,a)') keyword,&
     &' must be between 0 and 99999.&
     & Restoring to default of 4800.'
         call pc_error(msg)
         object%vel_zero = 4800
         call pc_put(keyword, object%vel_zero)
      end if

      return
      end subroutine avovan_vel_zero


! *** Screen trap for  CONTINUED ***

      subroutine avovan_continued(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_continued

! *** Screen trap for  GENERALPARAMETERS ***

      subroutine avovan_generalparameters(keyword)
      implicit none
      character(len=*), intent(in) :: keyword    !argument

      return
      end subroutine avovan_generalparameters

! *** End Trap ***

      subroutine avovan_end
      implicit none

      return
      end subroutine avovan_end



!!---------------------------- main execution --------------------------------!!
!!---------------------------- main execution --------------------------------!!
!!---------------------------- main execution --------------------------------!!


      subroutine avovan (obj,ntr,hd,tr)
      type(avovan_struct),intent(inout) :: obj                    ! arguments
      integer,            intent(inout) :: ntr                    ! arguments
      double precision,   intent(inout) :: hd(:,:)                ! arguments
      real,               intent(inout) :: tr(:,:)                ! arguments

      integer          :: i, j   
      integer          :: nio
      integer          :: nhd

      integer          :: trace_no
      integer          :: seq_no
      integer          :: trc_fold
      integer          :: trc_type
      integer          :: gath_type
      integer          :: stackword
      double precision :: head_mute
      double precision :: tail_mute
      double precision :: cdp
      double precision :: offset
      double precision :: iline_no
      double precision :: xline_no
      double precision :: avo_angle
      double precision :: dom_freq, user
      logical          :: ieog_flag
      logical          :: ieoj_flag

      ieog_flag = .false.
      ieoj_flag = .false.

      if (.not. obj%bavel(1)%obj%initialized) then

         allocate (obj%hd_g(ppavo_nhdrpz+obj%nwih, obj%numtr))
         allocate (obj%tr_g(obj%ndpt, obj%numtr))

         obj%bavel(1)%obj%ipn      = obj%ipn
         obj%bavel(1)%obj%numtr    = obj%numtr
         obj%bavel(1)%obj%gathered = obj%gathered
         obj%bavel(1)%obj%nwih     = obj%nwih
         obj%bavel(1)%obj%ndpt     = obj%ndpt
         obj%bavel(1)%obj%tstrt    = obj%tstrt
         obj%bavel(1)%obj%dt       = obj%dt
         obj%bavel(1)%obj%grid     = obj%grid

         obj%bavel(1)%obj%sampratz = obj%dt * 1000
         if(obj%survey_units .eq. 'FEET') then
            obj%bavel(1)%obj%iunitsz  = ppavo_englishpz
         else
            obj%bavel(1)%obj%iunitsz  = ppavo_metricpz
         end if
         obj%bavel(1)%obj%imultcz  = 0
         obj%bavel(1)%obj%icdpasnz = 1
         obj%bavel(1)%obj%igeoasnz = 1
         obj%bavel(1)%obj%ipsortz  = ppavo_cdppz
         obj%bavel(1)%obj%idtypez  = ppavo_normalpz
         obj%bavel(1)%obj%maxdtrz  = obj%numtr
         obj%bavel(1)%obj%numsmpz  = obj%ndpt
         obj%bavel(1)%obj%nthz     = ppavo_nhdrpz + obj%nwih
         obj%bavel(1)%obj%cleanupz = .false.

         call bavel_init (obj%bavel(1)%obj, nio)
         if(nio .eq. FATAL_ERROR) then
            ntr = FATAL_ERROR
            return
         end if

      end if

!     Copy CPS traces/headers to custom traces/headers.
      do i = 1, ntr
         do j = 1, obj%ndpt
            obj%tr_g(j, i) = tr(j, i)
         end do
         do j = 1, obj%nwih
            obj%hd_g(ppavo_nhdrpz+j, i) = hd(j, i)
         end do
         do j = 1, ppavo_nhdrpz
            obj%hd_g(j, i) = 0.0
         end do

         obj%hd_g(ppavo_trace_noz,  i) = hd(ppavo_trace_noz,  i)
         obj%hd_g(ppavo_head_mutez, i) = hd(ppavo_head_mutez, i)
         obj%hd_g(ppavo_tail_mutez, i) = hd(ppavo_tail_mutez, i)
         obj%hd_g(ppavo_cdpz,       i) = hd(ppavo_cdpz,       i)
         obj%hd_g(ppavo_seq_noz,    i) = hd(ppavo_seq_noz,    i)
         obj%hd_g(ppavo_trc_foldz,  i) = hd(ppavo_trc_foldz,  i)
         obj%hd_g(ppavo_offsetz,    i) = hd(ppavo_offsetz,    i)
         obj%hd_g(ppavo_iline_noz,  i) = hd(ppavo_iline_noz,  i)
         obj%hd_g(ppavo_xline_noz,  i) = hd(ppavo_xline_noz,  i)

         obj%hd_g(ppavo_end_ensz,   i) = ppavo_nlastpz
         obj%hd_g(ppavo_end_jobz,   i) = ppavo_nlastpz
         ieog_flag = obj%hd_g(HDR_SCRATCH_32, i) .ne. 0
         ieoj_flag = obj%hd_g(HDR_SCRATCH_32, i) .eq. 2
         if(ieog_flag) obj%hd_g(ppavo_end_ensz, i) = ppavo_lasttrpz
         if(ieoj_flag) obj%hd_g(ppavo_end_jobz, i) = ppavo_lasttrpz
         obj%hd_g(ppavo_full_sz,    i) = obj%tstrt
         obj%hd_g(ppavo_full_ez,    i) = obj%tstrt + ((obj%ndpt-1) * obj%dt)
         obj%hd_g(ppavo_full_sz,    i) = obj%hd_g(ppavo_full_sz, i) * 1000
         obj%hd_g(ppavo_full_ez,    i) = obj%hd_g(ppavo_full_ez, i) * 1000
         obj%hd_g(ppavo_live_sz,    i) = obj%hd_g(ppavo_full_sz, i)
         obj%hd_g(ppavo_live_ez,    i) = obj%hd_g(ppavo_full_ez, i)
         obj%hd_g(ppavo_line_noz,   i) = 0
         obj%hd_g(ppavo_amp_normz,  i) = 1.0         

         if(hd(HDR_LAV, i) .eq. 0.0) then
            obj%hd_g(ppavo_trc_typez, i) = ppavo_deadpz
         else
            obj%hd_g(ppavo_trc_typez, i) = ppavo_livepz
         end if
         if(obj%gath_opt .eq. 'OFFSET') then
            obj%hd_g(ppavo_gath_typez, i) = 1
         end if
         if(obj%gath_opt .eq. 'ANGLE') then
            obj%hd_g(ppavo_gath_typez, i) = 2
         end if
         obj%hd_g(ppavo_stackwordz, i) = 1
         obj%hd_g(ppavo_avo_anglez, i) = 0.0
         obj%hd_g(ppavo_dom_freqz, i) = 0.0
         if(obj%flt_type .eq. 'RICKER') then
            obj%hd_g(ppavo_dom_freqz, i) = obj%ric_freq
         end if
         obj%hd_g(ppavo_userz, i) = 0

      end do

      nio = ntr
      nhd = ppavo_nhdrpz + obj%nwih
      call bavel_work (obj%bavel(1)%obj, nhd, nio, obj%hd_g, obj%tr_g)
      if(nio .eq. FATAL_ERROR) then
         ntr = FATAL_ERROR
         return
      end if
      ntr = nio

!     Copy custom traces/headers to CPS traces/headers.
      ieog_flag = .false.
      ieoj_flag = .false.
      do i = 1, ntr

         trace_no  = obj%hd_g(ppavo_trace_noz,  i)
         head_mute = obj%hd_g(ppavo_head_mutez, i)
         tail_mute = obj%hd_g(ppavo_tail_mutez, i)
         cdp       = obj%hd_g(ppavo_cdpz,       i)
         seq_no    = obj%hd_g(ppavo_seq_noz,    i)
         offset    = obj%hd_g(ppavo_offsetz,    i)
         trc_fold  = obj%hd_g(ppavo_trc_foldz,  i)
         iline_no  = obj%hd_g(ppavo_iline_noz,  i)
         xline_no  = obj%hd_g(ppavo_xline_noz,  i)

         trc_type  = obj%hd_g(ppavo_trc_typez,  i)
         gath_type = obj%hd_g(ppavo_gath_typez, i)
         stackword = obj%hd_g(ppavo_stackwordz, i)
         avo_angle = obj%hd_g(ppavo_avo_anglez, i)
         dom_freq  = obj%hd_g(ppavo_dom_freqz,  i)
         user      = obj%hd_g(ppavo_userz,      i)

         do j = 1, obj%ndpt
            tr(j, i) = obj%tr_g(j, i)
         end do
         do j = 1, obj%nwih
            hd(j, i) = obj%hd_g(ppavo_nhdrpz+j, i)
         end do

         if(trc_type .ne. 1) offset = 0.0

         hd(ppavo_trace_noz,  i) = trace_no
         hd(ppavo_head_mutez, i) = head_mute
         hd(ppavo_tail_mutez, i) = tail_mute
         hd(ppavo_cdpz,       i) = cdp
         hd(ppavo_seq_noz,    i) = seq_no
         hd(ppavo_trc_foldz,  i) = trc_fold
         hd(ppavo_offsetz,    i) = offset
         hd(ppavo_iline_noz,  i) = iline_no
         hd(ppavo_xline_noz,  i) = xline_no

         hd(ppavo_trc_typez,  i) = trc_type
         hd(ppavo_gath_typez, i) = gath_type
         hd(ppavo_stackwordz, i) = stackword
         hd(ppavo_avo_anglez, i) = avo_angle
         hd(ppavo_dom_freqz,  i) = dom_freq
         hd(ppavo_userz,      i) = user

         ieog_flag = hd(HDR_SCRATCH_32, i) .ne. 0
         ieoj_flag = (ieoj_flag .or. hd(HDR_SCRATCH_32, i) .eq. 2)
         if(ieog_flag) hd(HDR_SCRATCH_32, i) = 1
      end do
      if(ieoj_flag) hd(HDR_SCRATCH_32, ntr) = 2

      end subroutine avovan


!!-------------------------------- wrapup ------------------------------------!!
!!-------------------------------- wrapup ------------------------------------!!
!!-------------------------------- wrapup ------------------------------------!!


      subroutine avovan_wrapup (obj)
      type(avovan_struct),intent(inout) :: obj       ! arguments

      integer              :: ntr, nhd


      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if(obj%bavel(1)%obj%initialized) then
         obj%bavel(1)%obj%cleanupz = .true.
         call bavel_work(obj%bavel(1)%obj, nhd, ntr, obj%hd_g, obj%tr_g)
      end if
      if(associated(obj%hd_g)) deallocate(obj%hd_g)
      if(associated(obj%tr_g)) deallocate(obj%tr_g)

      end subroutine avovan_wrapup


!!----------------------------- end of module --------------------------------!!
!!----------------------------- end of module --------------------------------!!
!!----------------------------- end of module --------------------------------!!


      end module avovan_module


!!---------------------------------- end -------------------------------------!!
!!---------------------------------- end -------------------------------------!!
!!---------------------------------- end -------------------------------------!!

