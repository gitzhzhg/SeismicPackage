!<CPS_v1 type="PROCESS"/>
!!------------------------------- rmul.f90 ---------------------------------!!
!!------------------------------- rmul.f90 ---------------------------------!!
!!------------------------------- rmul.f90 ---------------------------------!!

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
! Name       : RMUL
! Category   : filters
! Written    : 1989-12-06   by: Bill Harlan
! Revised    : 2008-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : Remove multiples by RADON modeling and subtraction.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
!
! RMUL removes multiple events from CMP gathers through use of parabolic,  
! linear, or hyperbolic RADON transforms to model multiples and subtract them 
! from the original data.  RMUL operation follows these steps.
!
!       1.  Transform CMP gather to parabolic, linear, or hyperbolic RADON 
!           space.
!
!       2.  Identify events with residual moveout in CMPs between RMO_PRI_MIN
!           and RMO_PRI_MAX as primaries and mute (zero) them in the transformed
!           data.  (This protects the primaries.)
!
!       3.  Identify events with residual moveout in CMPs between RMO_MUL_MIN
!           and RMO_MUL_MAX as multiples and mute out everything else from
!           parabolic, linear, or hyperbolic RADON space. (Taper the mute from 
!           0.0 at RMO_PRI_MAX to 1.0 at RMO_MUL_MIN.)
!
!       4.  Transform the energy that is identified as multiple energy back
!           from parabolic, linear, or hyperbolic RADON space and subtract it 
!           from the original CMP gather.
!
! The parabolic, linear, or hyperbolic RADON transform is optimized by 
! least-squares to accurately model the phases and amplitudes of multiples in 
! noisy data with limited or irregularly sampled offsets.  CPS process LSSS 
! performs forward and inverse parabolic stacks independently.
!
!
! Parabolic and hyperbolic Scans
!
! RMUL assumes that events on input CMPs, with primary velocity NMO correction
! applied, have a sufficiently small residual moveout that they have an
! approximately parabolic or hyperbolic contour.  Thus, if primaries and 
! multiples have non-overlapping residual moveout on CMPs (and reasonably 
! uniform amplitude and phase), then RMUL can perform a RADON transform and 
! be able to separate primaries from multiples in parabolic or hyperbolic 
! RADON space.
!
! 
! Linear Scans
!
! The linear Radon does not require the input CMPs to have primary velocity 
! NMO correction applied.
!
! The high-resolution Radon transform follows the sparseness constraints of
! Sacchi and Ulrych's paper (1995). This implementation uses a frequency 
! independent sparseness constraints which is an average of Sacchi's frequency 
! dependent Radon solution over a frequency range. Within this frequency
! range, the signals should have the highest amplitudes. The advantage  
! of using the average of the sparseness contraints can focus and preserve   
! the transform amplitude better than the original Sacchi's approach, but it 
! adds more computational time to precompute the average of the       
! sparseness constraints.
!   
! Sacchi, M.D. and Ulrych, T. J. 1995, High-resolution velocity gathers and 
! offset space reconstruction, Geophysics, 60, 4, 1169-1117.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Identifying Primaries And Multiples
!
! Determine the range of residual moveout of primaries at the largest offset
! trace.  Specify these moveouts with RMO_PRI_MIN and RMO_PRI_MAX, in seconds.
! Negative values mean that the overcorrected events curve upwards. Determine
! the range of residual moveout of multiples at the largest offset trace and
! specify these moveouts with RMO_MUL_MIN and RMO_MUL_MAX, in seconds.
! Multiples usually have positive residual moveouts, but they can also be
! negative.
!
! RMO_MUL_MAX should be set as large as necessary to model all multiple events,
! but no larger than necessary to avoid modeling and subtracting background
! noise.
!
!
! RMUL/MDIP
!
! When removing multiples with large residual moveout, better results at lower
! cost is often obtained by applying MDIP, a RADON dip filter, to the far
! offsets and RMUL to the near.  Usually MDIP is applied before RMUL and a
! 5 - 10 trace overlap is used.  this method may be advisable when multiple
! residual moveout approaches 1.0 second or more.
!
!
! Aliased Energy
!
! The description of the operation of RMUL above assumes that events are not
! aliased.  However, dipping events on CMPs often are aliased (and multiples
! with large residual moveout often have a considerable aliased component).
! Since the aliased part of an event has a different dip from the non-aliased
! part, RMUL's separation of primaries from multiples probably will not be
! valid for aliased energy.  Indeed aliased multiple energy may even be
! identified as primary energy.
!
! The only truly satisfactory solution to this problem is to remove as much
! aliased energy as possible from the CMPs prior to performing RMUL.  Trace
! interpolation is usually the best method.
!
!
! Demultiple Time Window
!
! Multiples are modeled only within the demultiple time window. The top of the
! window is given, in seconds, by:
!
!      top_of_window = WIN_BEG_FCTR * (head_mute_time) + WIN_BEG_ADD,
!
! where WIN_BEG_FCTR is the factor multiplying the head mute time (referenced
! to zero time) and WIN_BEG_ADD is an additional time.  (Head mute time refers
! to the lowest offset live trace in the gather.)  This allows flexibility
! in specifying the top of the window.
!
! A constant bottom of window time is specified by WIN_END.
!
! The input P value (residual moveout/slowness) is in second. However,  
! the output P value (in header word 6) is in millisecond. 
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! Data must be input in NMO corrected CMP gathers (must be gathered).
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs traces with same gather status as the input traces
! (which must be gathered).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name       Description                         Action taken
! ----       -----------                         ------------
! GATHERED   whether traces are gathered         checked (must be .true.)
! NUMTR      number of traces in gather          used but not changed
! NDPT       number of sample values in trace    used but not changed
! TSTRT      starting time on trace              used but not changed
! DT         trace sample interval               used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#      Description                 Action taken
! ----      -----------                 ------------
!  2        HDR_TOP_MUTE                Used but not changed
!  3        HDR_CURRENT_GROUP           Used but not changed
!  6        HDR_OFFSET                  Used but not changed
! 25        HDR_LAV                     Reset
! 64        HDR_BOTTOM_MUTE             Used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 38  2008-12-11  Bill Menger  Added more NULLIFY statements for Intel compiler.
! 37  2006-12-04  D. Glover    Added NULLIFY statements for Intel compiler.
! 36  2005-11-29  D. Glover    Changed arrays c_dta and c_mdl to be allocatable
!                              rather than automatic. This moves the arrays from
!                              the stack to the heap to avoid SIGSEGV aborts.
!                              Removed unreferenced vars reported by linuxab90
! 35  2005-01-17  S Chiu       Add a new high-resolution option. Remove the
!                              offset and fold taper options. Change FFT 
!                              length to mixed radix of 2, 3, and 5 instead 
!                              of power of 2. 
! 34  2003-09-22  Goodger      Remove declaration of Dot Product, as this is
!                              a fortran intrinsic.  Problem caught by
!                              portland compiler.
! 33  2003-07-29  S Chiu       Add a new hyperbolic option and forward-inverse
!                              transform. Improve front end.
! 32  2003-05-19  S Chiu       Change DIAG_LOAD in TAU-P option to reflect the
!                              actual DIAG_LOAD instead of DIAG_LOAD*100 
! 31  2003-04-30  S Chiu       Add new option NEAR_MUL_WT, change DIAG_LOAD 
!                              to 5, and change the name MULTIPLE option to 
!                              TAUP_INV 
! 30  2003-04-10  S Chiu       Change the default HDR_PMUTE to 6
! 29  2003-04-03  S Chiu       Incorporate APSE linear Taup-P. Replace 
!                              conjugate gradient solver with Levinson 
!                              algorithm. 
! 28  2003-02-27  S Chiu       Add new functions to output Tau-P section 
!                              and accept multiple picks in mute file.
! 27  2001-12-26  CC Burch     Change to BUNCH_TRACE_GROUPS
! 26  2001-02-15  Baumel       Modify PCPS_ALT_RECEIVE_MODE value.
! 25  2001-01-08  Baumel       Add PCPS (Parallel CPS) control statements.
! 24  2000-12-07  Baumel       Change wrapped_up flag to skip_wrapup.
! 23  2000-09-27  Baumel       Remove tab characters from doc (no code change).
! 22  2000-06-27  Baumel       Eliminate non-robust global usage in initialize.
! 21  2000-05-25  Baumel       New parameters for defining modeling window (no
!                              longer uses LATWIN); remove HDR_OFF parameter.
! 20  2000-05-11  Baumel       Tweak appearance of GUI screen.
! 19  2000-05-11  Baumel       Include GUI_DEF in source file; improve scratch
!                              and storage estimates; fix comments in code;
!                              also fix one potentially serious error [variable
!                              DTA had been declared with wrong intent in
!                              subroutine RMUL_TRANSFORM_TO_PSTK].
! 18  2000-02-24  Baumel       Model and process only LIVE traces, and base
!                              windowing on the LIVE trace with smallest offset
!                              (but still include dead traces when determining
!                              the gather's maximum offset).
! 17  2000-02-08  Baumel       Base windowing on trace with minimum offset,
!                              which isn't necessarily first trace; also some
!                              front-end related cleanup.
! 16  2000-02-04  Bob Baumel   Rework to make algorithm work correctly.
! 15  2000-01-18  Brad Kruse   Converted from old system.
! 14  1998-03-04  Vunderink    Fixed ncode format for opt=2.
! 13  1998-01-12  Vunderink    Fixed opt=1 and added cpu timer.
! 12  1997-01-20  Vunderink    Added options 1 and 2 for determining tmin.
! 11  1995-04-17  Troutt       Move getp call ahead of ntrmax check to make
!                                sure it uses current ntrmax.
! 10  1994-06-29  Harlan       Ignore bottom mute of 0 (uninitialized).
!  9  1993-04-29  Harlan       Add bottom mutes.
!  8  1993-02-12  Harlan       Allow more variable trace length in rmulcomp.
!  7  1992-04-06  Harlan       Fix bug sometimes affecting first 8 traces.
!  6  1991-05-15  Hanson       Improved rmulxval & rmulmaxx, reduce time 30%
!  5  1991-05-08  Hanson       Use cray fourier transforms for 2x speedup.
!  4  1991-04-24  Harlan       Add extra error checking on n and ntrmax.
!                                clean up complex arithmetic in rmulfssd.
!  3  1991-04-05  Harlan       Fix bug in rmulmaxx for negative offsets.
!  2  1990-11-08  Harlan       Re-mute; allow multiples w-neg movout
!  1  1989-12-06  Bill Harlan  Original version
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! no known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! no special requirements.
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
! Input gathers are transformed into parabolic, linear, or hyperbolic radon 
! space. All moveouts and zero-offset times which lie outside of limits 
! specified for multiples are removed, leaving a model of the multiple energy 
! in the gather.
!
! The modeled multiples are transformed back from parabolic radon space
! and subtracted from the original traces. The result is re-muted, using
! the current head and tail mutes specified in the trace headers, and
! then returned.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                              PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS RMUL Process/NC=80>
!  Remove Multiples by Parabolic Radon Modeling and Subtraction
!
!       Note: All time parameters are specified in SECONDS
!     `-------------------------------------------------------
!       RMO_PRI_MIN =`FFFFFFFFFFF  RMO_PRI_MAX =`FFFFFFFFFFF
!
!       RMO_MUL_MIN =`FFFFFFFFFFF  RMO_MUL_MAX =`FFFFFFFFFFF
!
!       SCAN_INC=~~~~`FFFFFFFFFFF  TIM_MUTE=~~~~`FFFFFFFFFFF
!
!       FREQ_BEG=~~~~`FFFFFFFFFFF  FREQ_END=~~~~`FFFFFFFFFFF
!
!       OPT_RMUL=~~~~`CCCCCCCC     OPT_OPER=~~~~`CCCCCCCCCC         
!
!       OPT_OUTPUT=~~`CCCCCCCC     HDR_PMUTE=~~~`II
!
!       DIAG_LOAD=~~~`FFFFFFFFFFF  
!     `-------------------------------------------------------
!
!  WIN_BEG_FCTR=`FFFFFFFF   WIN_BEG_ADD =`FFFFFFFF    WIN_END=`FFFFFFFFF
!
!  POINT_SOURCE=`CC         FREQ_BAL=~~~~`CC          ZREF=~~~`FFFFFFFFF
!
!Select FILE_HEAD[FILE_HEAD]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [FILE_HEAD_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!Select FILE_TAIL[FILE_TAIL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                [FILE_TAIL_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS FILE_HEAD[/ML=128/XST/YST]>
!<PARMS FILE_TAIL[/ML=128/XST/YST]>
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="RMO_PRI_MIN">
!<Tip> Minimum residual moveout at the far offset for primary events. </Tip>
! Default = -0.050
! Allowed = real
! Events will be identified as primaries whose residual moveout, in seconds, at
! the far offset lies between RMO_PRI_MIN and RMO_PRI_MAX.
!</Help>
!
!<Help KEYWORD="RMO_PRI_MAX">
!<Tip> Maximum residual moveout at the far offset for primary events. </Tip>
! Default = 0.040
! Allowed = real
! Events will be identified as primaries whose residual moveout, in seconds, at
! the far offset lies between RMO_PRI_MIN and RMO_PRI_MAX.
!</Help>
!
!<Help KEYWORD="RMO_MUL_MIN">
!<Tip> Minimum residual moveout at the far offset for multiple events. </Tip>
! Default = 0.060
! Allowed = real
! Events will be identified as multiples whose residual moveout, in seconds,
! at the far offset lies between RMO_MUL_MIN and RMO_MUL_MAX.
!</Help>
!
!<Help KEYWORD="RMO_MUL_MAX">
!<Tip> Maximum residual moveout at the far offset for multiple events. </Tip>
! Default = 0.400
! Allowed = real
! Events will be identified as multiples whose residual moveout, in seconds, at
! the far offset lies between RMO_MUL_MIN and RMO_MUL_MAX.
!</Help>
!
!<Help KEYWORD="OPT_RMUL">
!<Tip> Type of operation for the mute process. </Tip>
! Default = RADON
! Allowed = RADON       Radon transform.
! Allowed = HR_RADON    High-resolution Radon transform.
!</Help>
!
!<Help KEYWORD="OPT_OPER">
!<Tip> Type of operation for the Radon transform. </Tip>
! Default = PARABOLIC
! Allowed = PARABOLIC    Parabolic transform.
! Allowed = LINEAR       Linear transform.
! Allowed = HYPERBOLIC   Hyperbolic transform.
!</Help>
!
!<Help KEYWORD="OPT_OUTPUT">
!<Tip> Type of operation for the mute process. </Tip>
! Default = RMUL
! Allowed = RMUL           Multiple/noise attenuation.
! Allowed = TAU-P          Forward Tau-P transform.
! Allowed = MODEL          Output by Radon modelling with mute applied.
! Allowed = TAUP_MUTE      Tau-P section with mute applied. 
! Allowed = FOR_INV_TAUP   Perform a forward and inverse transform.
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent. </Tip>
! Default = 5.0
! Allowed = real>=0.0
! If the OPT_OUTPUT is in TAU-P domain, it generally needs to 
! use a much higher DIAG_LOAD to produce a better TAU-P output 
! for picking.
!</Help>
!
!<Help KEYWORD="ZREF">
!<Tip> Depth reference for Hyperbolic transform. </Tip>
! Default = 
! Allowed = real> 0.0
! ZREF is usually chosen as the maximum offset.
!</Help>
!
!<Help KEYWORD="POINT_SOURCE">
!<Tip> Point source correction for linear TAUP only. </Tip>
! Default = No
! Allowed = Yes/No
!</Help>
!
!<Help KEYWORD="FREQ_BAL">
!<Tip> Frequency balance for linear TAUP only. </Tip>
! Default = No
! Allowed = Yes/No
!</Help>
!
!<Help KEYWORD="SCAN_INC">
!<Tip> Time increment, at the far offset, for scanning the CMP. </Tip>
! Default = 0.020
! Allowed = real
! Time increment, in seconds, at the far offset, for CMP scans within the
! parabolic, linear, or hyperbolic RADON transform.
!</Help>
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Minimum frequency to model and subtract from data, in Hz. </Tip>
! Default = 8
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="FREQ_END">
!<Tip> Maximum frequency to model and subtract from data, in Hz. </Tip>
! Default = 0.75 * Nyquist
! Allowed = Nyquist > real > FREQ_BEG
!</Help>
!
!<Help KEYWORD="TIM_MUTE">
!<Tip> Zero-offset time of the multiple with the greatest moveout. </Tip>
! Default = end of trace
! Allowed = real
! The approximate greatest zero-offset time at which multiples with the
! greatest residual moveout are present. The parabolic, linear, or 
! hyperbolic transform will be muted to exclude unnecessarily large
! moveouts in later events.
!
! If TIM_MUTE = end of trace, no such mute is applied. This is the most
! conservative value of TIM_MUTE to use.
!
! Omitting TIM_MUTE is equivalent to setting TIM_MUTE = end of trace.
!</Help>
!
!<Help KEYWORD="WIN_BEG_FCTR">
!<Tip> Factor to multiply head mute time for top of demultiple window. </Tip>
! Default = 2.0
! Allowed = real >= 0.0
! Factor to multiply head mute time (referenced to zero time) for top of
! demultiple window.  (Head mute time refers to the lowest offset live trace in
! the gather.)  Top of the multiple modeling window, in seconds, is given by:
!
!    top_of_window = WIN_BEG_FCTR * (head_mute_time) + WIN_BEG_ADD.
!</Help>
!
!<Help KEYWORD="WIN_BEG_ADD">
!<Tip> Additional time, in seconds, for top of demultiple window. </Tip>
! Default = 0.0
! Allowed = real
! Top of the multiple modeling window, in seconds, is given by:
!
!    top_of_window = WIN_BEG_FCTR * (head_mute_time) + WIN_BEG_ADD.
!</Help>
!
!<Help KEYWORD="WIN_END">
!<Tip> Constant time, in seconds, for the bottom of the window. </Tip>
! Default = end of trace
! Allowed = TSTRT < real =< end of trace
! Constant time, in seconds, for the bottom of the multiple modeling window.
! Normally this should be set to the end of the trace.
!
! Omitting WIN_END is equivalent to setting WIN_END = end of trace.
!</Help>
!
!<Help KEYWORD="SELECT_FILE_HEAD">
!<Tip> Choose FILE_HEAD using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="FILE_HEAD_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of FILE_HEAD. </Tip>
!</Help>
!
!<Help KEYWORD="FILE_HEAD">
!<Tip> FILE_HEAD for the mute file to be used to remove primary. </Tip>
! Default = NONE
! Allowed = char*60
!</Help>
!
!<Help KEYWORD="SELECT_FILE_TAIL">
!<Tip> Choose FILE_TAIL using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="FILE_TAIL_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of FILE_TAIL. </Tip>
!</Help>
!
!<Help KEYWORD="FILE_TAIL">
!<Tip> FILE_TAIL for the mute file to be used to remove multiple. </Tip>
! Default = NONE
! Allowed = char*60
!</Help>
!
!<Help KEYWORD="HDR_PMUTE">
!<Tip> Header word deSIGNAting P values in mute file. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!-------------------------------------------------------------------------------
!</HelpSection>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module rmul_module

! - Module references

  use pc_module, only:                 &
        pc_do_not_process_traces,      &
        pc_error,                      &
        pc_get,                        &
        pc_get_global,                 &
        pc_print,                      &
        pc_put,                        &
        pc_put_control,                &
        pc_warning

  use named_constants_module

  use getsys_module, only:   &
        getsys_seconds

  use sort_module, only:     &
        sort_inssort

  use fft_module

  use lav_module, only:    &
        lav_set_hdr

  use conjgrad_module
  use gausselim_module
  use mutehw_module

  use mute_module

  use pathcheck_module
  use pathchoose_module
            
  implicit none
  private

! - Public routines

  public :: rmul_create     ! uses the parameter cache.
  public :: rmul_initialize
  public :: rmul_update     ! uses the parameter cache.
  public :: rmul_delete

!<execute_only>

  public :: rmul            ! main execution (trace processing) routine.
  public :: rmul_wrapup

!</execute_only>

  character (len = 100), public, save :: rmul_ident =   &
'$Id: rmul.f90,v 1.37 2006/12/04 13:29:55 Glover prod sps $'

!!------------------------- parameter structure --------------------------!!
!!------------------------- parameter structure --------------------------!!
!!------------------------- parameter structure --------------------------!!

  type, public :: rmul_struct

    private
    logical                    :: skip_wrapup        ! wrapup flag.

! - process parameters available from the parameter cache

   real                        :: rmo_mul_max        ! process parameters
   real                        :: rmo_mul_min        ! process parameters
   real                        :: rmo_pri_max        ! process parameters
   real                        :: rmo_pri_min        ! process parameters
   real                        :: scan_inc           ! process parameters
   real                        :: freq_beg           ! process parameters
   real                        :: freq_end           ! process parameters
   real                        :: tim_mute           ! process parameters
   real                        :: win_beg_fctr       ! process parameters
   real                        :: win_beg_add        ! process parameters
   real                        :: win_end            ! process parameters
   real                        :: diag_load          ! process parameters
   real                        :: zref               ! process parameters

   integer                     :: hdr_pmute          ! process parameters
   logical                     :: point_source       ! process parameters
   logical                     :: freq_bal           ! process parameters

   character(len=8)            :: opt_rmul           ! process parameters
   character(len=10)           :: opt_oper           ! process parameters
   character(len=12)           :: opt_output         ! process parameters
    
! - common globals

   integer                     :: ndpt               ! common globals
   integer                     :: numtr              ! common globals
   integer                     :: nwih               ! common globals
   real                        :: dt                 ! common globals
   real                        :: tstrt              ! common globals

! - Other values

   integer                     :: np                 ! nubmer of p-values
   integer                     :: nfpt               ! Number freqs (Nyquist)
   integer                     :: ntpow              ! FFT size, a fnct of ndpt
   integer                     :: nter               ! # conj grad iterations
   integer                     :: jf1, jf2           ! From freq_beg & freq_end
   integer                     :: itmax              ! From win_end
   integer                     :: nwt_taper          ! near-off multiple taper
   integer                     :: min_tstart         ! minimum start time 

   real                        :: nyquist            ! Nyquist freq (real)
   real                        :: tcpu               ! elapsed time in process
   type (fft_struct), pointer  :: rcfft_obj          ! for real -> complex
   type (fft_struct), pointer  :: crfft_obj          ! for complex -> real
   real                        :: df                 ! frequency increment
   real                        :: pmin, dp           ! minimum p & increment
   real                        :: pcut1, pcut2       ! p cutting thresholds
   real, pointer               :: tsqrt(:)
   real                        :: pnoise             ! prewhitening
   real                        :: pcon               ! Hyperbolic conversion
   ! dependent variables

   logical                         :: fhead_mute
   logical                         :: ftail_mute
   type(pathchoose_struct),pointer :: dialog_head
   type(pathchoose_struct),pointer :: dialog_tail
   character(len=FILENAME_LENGTH)  :: file_head
   character(len=FILENAME_LENGTH)  :: file_tail
                                      ! name of mute file including path

   type(mute_struct), pointer      :: mute_head       ! dependent module 
   type(mute_struct), pointer      :: mute_tail       ! dependent module
  end type rmul_struct


!!------------------------------- data -----------------------------------!!
!!------------------------------- data -----------------------------------!!
!!------------------------------- data -----------------------------------!!


  type (rmul_struct), pointer, save :: object       ! needed for traps
    
  character(len=8),save          :: rmul_options(2) ! used for traps
  integer                        :: rmul_nopt=2     ! used for GUI menu

  data rmul_options/'RADON', 'HR_RADON'/

  character(len=10),save         :: oper_options(3) ! used for traps
  integer                        :: oper_nopt=3     ! used for GUI menu
  data oper_options/'PARABOLIC', 'LINEAR', 'HYPERBOLIC'/

  character(len=12),save         :: output_options(5) ! used for traps
  integer                        :: output_nopt=5     ! used for GUI menu

  data output_options/'RMUL','TAU-P','MODEL','TAUP_MUTE','FOR_INV_TAUP'/
  
  integer, parameter :: NUM_CONJ_GRAD = 3
  real , parameter   :: SIGNA = 0.25
  real , parameter   :: EPSILSON = 1.0e-6

  contains

!!--------------------------- create -------------------------------------!!
!!--------------------------- create -------------------------------------!!
!!--------------------------- create -------------------------------------!!

  subroutine rmul_create (obj)
    !
    ! - Arguments
    !
    type (rmul_struct), pointer :: obj       ! arguments
    !
    ! - Begin rmul_create
    !
    nullify(obj)
    allocate (obj)
    nullify (object)
    !
    nullify (obj%rcfft_obj)
    nullify (obj%crfft_obj)
    nullify (obj%mute_head)
    nullify (obj%mute_tail)
    nullify (obj%tsqrt)
    nullify (obj%dialog_head) ! jpa
    nullify (obj%dialog_tail) ! jpa
    !
    call pathchoose_create (obj%dialog_head, 'FILE_HEAD', 'mute')
    call pathchoose_create (obj%dialog_tail, 'FILE_TAIL', 'mute')

    call rmul_initialize (obj)
    !
  end subroutine rmul_create


  !!----------------------------- delete -----------------------------------!!
  !!----------------------------- delete -----------------------------------!!
  !!----------------------------- delete -----------------------------------!!


  subroutine rmul_delete (obj)
    !
    ! - Arguments
    !
    type (rmul_struct), pointer :: obj       ! arguments
    !
    ! - Begin rmul_delete
    !
!<execute_only>
    call rmul_wrapup (obj)
!</execute_only>
    !
    if (associated (obj%rcfft_obj))  call fft_delete (obj%rcfft_obj)
    if (associated (obj%crfft_obj))  call fft_delete (obj%crfft_obj)
    if (associated(obj%mute_head))   call mute_delete(obj%mute_head)
    if (associated(obj%mute_tail))   call mute_delete(obj%mute_tail)
    if (associated(obj%dialog_head)) call pathchoose_delete (obj%dialog_head)
    if (associated(obj%dialog_tail)) call pathchoose_delete (obj%dialog_tail)
    if (associated(obj%tsqrt))       deallocate (obj%tsqrt)
    !
    deallocate(obj)
    !
  end subroutine rmul_delete
  !


  !!------------------------------ initialize ------------------------------!!
  !!------------------------------ initialize ------------------------------!!
  !!------------------------------ initialize ------------------------------!!

  subroutine rmul_initialize (obj)
    !
    ! - Arguments
    !
    type (rmul_struct), intent(inout) :: obj       ! arguments
    !
    ! - Begin rmul_initialize
    !
    call pc_get_global ('NUMTR', obj%numtr)    ! maximum number of traces.

    obj%rmo_pri_min       = -0.050
    obj%rmo_pri_max       = 0.040
    obj%rmo_mul_min       = 0.060
    obj%rmo_mul_max       = 0.400
    obj%scan_inc          = 0.020
    obj%freq_beg          = 8.0
    obj%freq_end          = fnil      ! will reset in update routine
    obj%tim_mute          = fnil
    obj%win_beg_fctr      = 2.0
    obj%win_beg_add       = 0.0
    obj%win_end           = fnil
    obj%opt_rmul          = 'RADON'
    obj%opt_oper          = 'PARABOLIC'
    obj%opt_output        = 'RMUL'
    obj%diag_load         = 5.0
    obj%zref              = fnil    
    obj%hdr_pmute         = 6

    obj%point_source      = .false.
    obj%freq_bal          = .false.
    !
    obj%nter              = NUM_CONJ_GRAD
    !
    obj%file_head   = pathcheck_empty
    obj%file_tail   = pathcheck_empty
    !
    call rmul_update (obj)
    !
  end subroutine rmul_initialize


  !!--------------------------- start of update ----------------------------!!
  !!--------------------------- start of update ----------------------------!!
  !!--------------------------- start of update ----------------------------!!

  subroutine rmul_update (obj)
    !
    ! - Arguments
    !
    type(rmul_struct),target :: obj                           ! arguments
    !
    ! - Local Variables
    !
    integer :: ier
    integer :: j, maxtr_out
    integer :: nscratch
    integer :: nstore
    !
    logical :: gathered
    !
    real    :: tcpu1            ! starting time stamp
    real    :: r, end_trace     ! local
    real    :: dt4 (4)          ! Temp array for all 4 resid moveouts
    real    :: hdist            ! local

    character(len=60)          :: tmp_char                     ! local
    !
    ! - Begin rmul_update
    !
    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.
    !
    tcpu1 = getsys_seconds()
    obj%tcpu = 0.0

  !!----------------------- read parameters --------------------------------!!
  !!----------------------- read parameters --------------------------------!!
  !!----------------------- read parameters --------------------------------!!

    if (pathchoose_update(obj%dialog_head, obj%file_head)) return
    if (pathchoose_update(obj%dialog_tail, obj%file_tail)) return
      
    call pc_get_global ('GATHERED', gathered)  ! whether traces are gathered.
    call pc_get_global ('NUMTR', obj%numtr)    ! maximum number of traces.
    call pc_get_global ( 'NDPT', obj%ndpt)     ! number of trace samples.
    call pc_get_global ( 'NWIH', obj%nwih)     ! number of header.
    call pc_get_global (   'DT', obj%dt)       ! trace sample interval (sec).
    call pc_get_global ('TSTRT', obj%tstrt)    ! time of 1st trace sample (sec).

    obj%nyquist = 0.5 / obj%dt
    end_trace   = obj%tstrt + (obj%ndpt-1)*obj%dt

    call pc_get ( 'RMO_PRI_MIN', obj%rmo_pri_min)
    call pc_get ( 'RMO_PRI_MAX', obj%rmo_pri_max)
    call pc_get ( 'RMO_MUL_MIN', obj%rmo_mul_min)
    call pc_get ( 'RMO_MUL_MAX', obj%rmo_mul_max)
    call pc_get (    'SCAN_INC', obj%scan_inc)
    call pc_get (    'FREQ_BEG', obj%freq_beg)
    call pc_get (    'FREQ_END', obj%freq_end)
    call pc_get (    'TIM_MUTE', obj%tim_mute)
    call pc_get ('WIN_BEG_FCTR', obj%win_beg_fctr)
    call pc_get ( 'WIN_BEG_ADD', obj%win_beg_add)
    call pc_get (     'WIN_END', obj%win_end)

    call pc_get (     'OPT_RMUL', obj%opt_rmul)
    call pc_get (     'OPT_OPER', obj%opt_oper)
    call pc_get (   'OPT_OUTPUT', obj%opt_output)
    call pc_get (    'DIAG_LOAD', obj%diag_load)
    call pc_get (         'ZREF', obj%zref)

    call pc_get ( 'POINT_SOURCE', obj%point_source)
    call pc_get (     'FREQ_BAL', obj%freq_bal)
    call pc_get (    'FILE_HEAD', obj%file_head )
    call pc_get (    'FILE_TAIL', obj%file_tail )
    call pc_get (    'HDR_PMUTE', obj%hdr_pmute )

    call string_to_upper (obj%opt_rmul)
    call string_to_upper (obj%opt_oper)
    call string_to_upper (obj%opt_output)
     
  !!-------------------------- verify parameters ---------------------------!!
  !!-------------------------- verify parameters ---------------------------!!
  !!-------------------------- verify parameters ---------------------------!!

    if (.not. gathered) then
      call pc_error ('This process must be preceded by a GATHER')
    end if
    if (obj%numtr < 2) then
      call pc_error ('RMUL: Must have more than one trace in gather.')
    end if
    !
    ! - Check RMO_PRI_MAX
    !
  check_rmo_pri_max:   &
    if (obj%rmo_pri_max < obj%rmo_pri_min) then   ! real
      call pc_error (msg1 = 'Bad value RMO_PRI_MAX',                  &
                     var1 = obj%rmo_pri_max,                          &
                     msg2 = '   RMO_PRI_MAX must be greater than ',   &
                     var2 = obj%rmo_pri_min)
    end if check_rmo_pri_max
    !
    ! - Check RMO_MUL_MAX
    !
  check_rmo_mul_max:   &
    if (obj%rmo_mul_max < obj%rmo_mul_min) then   ! real
      call pc_error (msg1 = 'Bad value RMO_MUL_MAX',                  &
                     var1 = obj%rmo_mul_max,                          &
                     msg2 = '   RMO_MUL_MAX must be greater than ',   &
                     var2 = obj%rmo_mul_min)
    end if check_rmo_mul_max
    !
    ! - Check FREQ_BEG
    !
  check_freq_beg:   &
    if ((obj%freq_beg <= 0.0) .or. (obj%freq_beg >= obj%nyquist)) then
      call pc_error (msg1 = 'Bad value FREQ_BEG ',               &
                     var1 = obj%freq_beg,                        &
                     msg2 = '  FREQ_BEG must be > 0.0, and < ',  &
                     var2 = obj%nyquist)
    end if check_freq_beg
    !
    ! - Check FREQ_END
    !
    ! - First fill in default for FREQ_END if it's currently nil
    !
    if (obj%freq_end == fnil) obj%freq_end = 0.75 * obj%nyquist
    !
    ! - Now do error checking for FREQ_END
  check_freq_end:   &
    if ((obj%freq_end <= obj%freq_beg) .or. (obj%freq_end >= obj%nyquist)) then
      call pc_error (msg1 = 'Bad value FREQ_END ',                     &
                     var1 = obj%freq_end,                              &
                     msg2 = '  FREQ_END must be > FREQ_BEG, and < ',   &
                     var2 = obj%nyquist)
    end if check_freq_end
    !
    ! - Check TIM_MUTE
    !
    if (obj%tim_mute <= 0.) obj%tim_mute = FNIL
    if (obj%tim_mute /= FNIL)  &
       obj%tim_mute = min ( max(obj%tim_mute,obj%tstrt) , end_trace )
    !
    ! - Check WIN_BEG_FCTR
    !
    if (obj%win_beg_fctr < 0.0) then
      call pc_error ('WIN_BEG_FCTR must be non-negative.')
    end if
    !
    ! - Check WIN_END
    !
    if (obj%win_end <= 0.) obj%win_end = FNIL
    if (obj%win_end /= FNIL) then
      obj%itmax = &
         min ( max(nint((obj%win_end-obj%tstrt)/obj%dt)+1, 1) , obj%ndpt)
      obj%win_end = obj%tstrt + (obj%itmax-1)*obj%dt
    end if


  !   if ( pc_verify_scalar('file_head') ) then
        call pathcheck (KEYWORD='file_head', PATHNAME=obj%file_head, &
                        EXT='', REQUIRED=.false., SCREEN='RMUL',   &
                        show=PATHCHECK_INFO_INPUT)
  !   endif

! Run the traps needed when GUI users assert the parameters are "OK".
      if ( pc_verify_end() ) then
        if ( obj%file_head /= pathcheck_empty ) then
          call pathcheck (KEYWORD='file_head', PATHNAME=obj%file_head, &
                          EXT='', REQUIRED=.true., SCREEN='RMUL')
          obj%fhead_mute = .true.
        else 
          obj%fhead_mute = .false.
        endif
      end if

  !   if ( pc_verify_scalar('FILE_TAIL') ) then
        call pathcheck (KEYWORD='FILE_TAIL', PATHNAME=obj%file_tail, &
                        EXT='', REQUIRED=.false., SCREEN='RMUL',   &
                        show=PATHCHECK_INFO_INPUT)
  !   endif

! Run the traps needed when GUI users assert the parameters are "OK".
      if ( pc_verify_end() ) then
        if ( obj%file_tail /= pathcheck_empty ) then
          call pathcheck (KEYWORD='FILE_TAIL', PATHNAME=obj%file_tail, &
                          EXT='', REQUIRED=.true., SCREEN='RMUL')
          obj%ftail_mute = .true.
        else
          obj%ftail_mute = .false.
        endif
      end if

      if ( all(rmul_options/=obj%opt_rmul)) then
        call pc_warning('Invalid rmul operation. '//obj%opt_rmul)
        call pc_info('Resetting to default: RADON.')
        obj%opt_rmul = 'RADON'
      endif

      if ( all(oper_options/=obj%opt_oper)) then
        call pc_warning('Invalid operation. '//obj%opt_oper)
        call pc_info('Resetting to default: PARABOLIC.')
        obj%opt_oper = 'PARABOLIC'
      endif

      if ( all(output_options/=obj%opt_output)) then
        call pc_warning('Invalid output operation. '//obj%opt_output)
        call pc_info('Resetting to default: RMUL.')
        obj%opt_output = 'RMUL'
      endif

      if(obj%diag_load < 0.0) then
        call pc_error('RMUL error: DIAG_LOAD must be >= 0.0')
        obj%diag_load = 5.0
      end if

      if (obj%opt_oper=='HYPERBOLIC') then 
        if (pc_get_update_state() /= PC_GUI) then 
          if (obj%zref == fnil) then
    ! 
    ! - Now do error checking for ZREF

            call pc_error (msg1 = 'RMUL error: ZREF must be > 0 ',   &
                     var1 = obj%zref )
          end if
        end if
      end if

      if (obj%hdr_pmute<1 .or. obj%hdr_pmute>obj%nwih) then
        call pc_warning &
          ('RMUL Warning: HDR_PMUTE  not in range 1 to ',obj%nwih, &
           'Resetting to default: HDR_PMUTE to 6')
            obj%hdr_pmute = 6
      end if          
        
      if (obj%opt_oper=='LINEAR') then
        call pc_put_sensitive_field_flag   ('freq_bal',      .true.)
        call pc_put_sensitive_field_flag   ('point_source',  .true.)
        call pc_warning &
          ('RMUL Warning: Resetting HR_RADON  to RADON ' )
        obj%opt_rmul = 'RADON'
      else
        call pc_put_sensitive_field_flag   ('freq_bal',      .false.)
        call pc_put_sensitive_field_flag   ('point_source',  .false.)
      end if

      if (obj%opt_oper=='HYPERBOLIC') then
        call pc_put_sensitive_field_flag   ('zref',      .true.)
      else
        call pc_put_sensitive_field_flag   ('zref',      .false.)
      end if

    !!---------------------- call processes internally ---------------------!!
    !!---------------------- call processes internally ---------------------!!
    !!---------------------- call processes internally ---------------------!!

! mute
  
      if (obj%fhead_mute ) then

        ! reverse the head mute to tail mute for multiple

        tmp_char  = 'FILE_HEAD'
        call pc_clear
        call pc_put_process ('OPT_MUTE',  tmp_char)
        call pc_put_process ('PATHNAME',  obj%file_head)
        call pc_put_process ('LEN_TAPER', 3*obj%dt)
             
        if (associated(obj%mute_head)) then
           call mute_update (obj%mute_head)
        else
           call mute_create (obj%mute_head)
        end if
        call pc_restore

      end if 
 
      if ( obj%ftail_mute ) then

        ! reverse the tail mute to head mute for multiple

        tmp_char  = 'FILE_TAIL'
        call pc_clear
        call pc_put_process ('OPT_MUTE',  tmp_char)
        call pc_put_process ('PATHNAME',  obj%file_tail)
        call pc_put_process ('LEN_TAPER', 3*obj%dt)
         
        if (associated(obj%mute_tail)) then
           call mute_update (obj%mute_tail)
        else
           call mute_create (obj%mute_tail)
        end if
        call pc_restore

     end if 
! end mute


    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!
    !!-------------------------- write parameters --------------------------!!
    !
    ! - More initializations & determine memory usage:

    call pc_put_options_field ('OPT_RMUL', rmul_options, rmul_nopt)
    call pc_put_options_field ('OPT_OPER', oper_options, oper_nopt)
    call pc_put_options_field ('OPT_OUTPUT', output_options, output_nopt)

    !
    ! - Memory consumed by 'object'
    ! 

    obj%ntpow = 8
    !do while (obj%ntpow < obj%ndpt)
    !  obj%ntpow = 2 * obj%ntpow
    !end do

    obj%ntpow = max(fft_nfctr(obj%ndpt), obj%ntpow)

    obj%nfpt = obj%ntpow/2 + 1
    obj%df = 1. / (obj%ntpow * obj%dt)
    obj%jf1 = min ( max(nint(obj%freq_beg/obj%df)+1,1) , obj%nfpt )
    obj%jf2 = min ( max(nint(obj%freq_end/obj%df)+1,obj%jf1) , obj%nfpt )
    !
    nstore = fft_mem_usage(obj%ntpow,'rtoc')  &
               + fft_mem_usage(obj%ntpow,'ctor')
    call pc_put_control ('NSTORE',   nstore)
    !
    ! - Memory used each time called
    !
    dt4 = (/ obj%rmo_pri_min, obj%rmo_pri_max,    &
             obj%rmo_mul_min, obj%rmo_mul_max /)
    !
    call sort_inssort (a = dt4,   &
                       p = 1,     &
                       q = 4)
    !
    obj%pmin = dt4(1)
    obj%scan_inc = abs (obj%scan_inc)
    if (obj%scan_inc > 0.) then
      obj%np   = nint((dt4(4) - dt4(1))/obj%scan_inc) + 1
      if (obj%np > 1) then
        obj%dp = (dt4(4) - dt4(1)) / (obj%np - 1)
      else
        call pc_error ('Moveout range too small or SCAN_INC too big, so&
                      & no moveouts to scan!')
        obj%dp = 0.
      end if
    else
      call pc_error ('SCAN_INC must be positive.')
      obj%np = 1
      obj%dp = 0.
    end if
    !
    r = (obj%rmo_mul_min + obj%rmo_mul_max  &
         - obj%rmo_pri_min - obj%rmo_pri_max)  *  0.5
    !
    if (r >= 0.) then
      obj%pcut1 = dt4(2)
      obj%pcut2 = dt4(3)
    else
      obj%pcut1 = dt4(3)
      obj%pcut2 = dt4(2)
    end if
    !
    if (obj%pcut2 == obj%pcut1) then
      obj%pcut1 = obj%pcut1 - 0.00005*r
      obj%pcut2 = obj%pcut2 - 0.00005*r
    end if
    !
    nscratch = 2 * obj%numtr                 &
               + obj%ndpt * obj%numtr        &
               + obj%ndpt * obj%np           &
               + 2 * obj%nfpt * obj%numtr    &
               + 2 * obj%nfpt * obj%np       &
               + 2 * obj%numtr * obj%np      &
               + obj%ntpow
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
      
    maxtr_out = max(obj%np, obj%numtr)

    call pc_put_global ('NUMTR', maxtr_out)    ! maximum number of traces.

    call pc_put ( 'RMO_PRI_MIN', obj%rmo_pri_min)
    call pc_put ( 'RMO_PRI_MAX', obj%rmo_pri_max)
    call pc_put ( 'RMO_MUL_MIN', obj%rmo_mul_min)
    call pc_put ( 'RMO_MUL_MAX', obj%rmo_mul_max)
    call pc_put (    'SCAN_INC', obj%scan_inc)
    call pc_put (    'FREQ_BEG', obj%freq_beg)
    call pc_put (    'FREQ_END', obj%freq_end)
    call pc_put (    'TIM_MUTE', obj%tim_mute)
    call pc_put ('WIN_BEG_FCTR', obj%win_beg_fctr)
    call pc_put ( 'WIN_BEG_ADD', obj%win_beg_add)
    call pc_put (     'WIN_END', obj%win_end)
    call pc_put (    'OPT_RMUL', obj%opt_rmul)
    call pc_put (    'OPT_OPER', obj%opt_oper)
    call pc_put ('POINT_SOURCE', obj%point_source)
    call pc_put ('FREQ_BAL    ', obj%freq_bal)
    call pc_put (  'OPT_OUTPUT', obj%opt_output)
    call pc_put (        'ZREF', obj%zref)
    call pc_put (   'DIAG_LOAD', obj%diag_load)
    call pc_put (   'FILE_HEAD', obj%file_head)
    call pc_put (   'FILE_TAIL', obj%file_tail)
    call pc_put (   'HDR_PMUTE', obj%hdr_pmute )
    
    !!-------------------- prepare for execution ---------------------------!!
    !!-------------------- prepare for execution ---------------------------!!
    !!-------------------- prepare for execution ---------------------------!!

!  Conditionally deallocate all arrays

    if (associated(obj%tsqrt))    deallocate (obj%tsqrt)

!<execute_only>

      if (pc_do_not_process_traces()) return
      !
      obj%skip_wrapup = .false.    ! needed for the wrapup routine.
      !
!     Allocate your permanent memory:

      allocate  (obj%tsqrt(obj%ndpt),             stat=ier)
      if (ier/=0) call pc_error ('Error creating RMUL - TSQRT array')

      if (obj%tim_mute == FNIL)  obj%tim_mute = end_trace
      !
      if (obj%win_end == FNIL) then
        obj%win_end  = end_trace
        obj%itmax    = obj%ndpt
      end if
      !
      ier = fft_create (obj   = obj%rcfft_obj,   &
                        sign  = -1,              &
                        size  = obj%ntpow,       &
                        ctype = 'rtoc')
      !
      ier = fft_create (obj   = obj%crfft_obj,   &
                        sign  = 1,               &
                        size  = obj%ntpow,       &
                        ctype = 'ctor')

      obj%pnoise = obj%diag_load/100.      

      if (obj%opt_oper =='LINEAR') then
        obj%pmin  = obj%pmin*1.e-3
        obj%dp    = obj%dp*1.e-3
        obj%pcut1 = obj%pcut1*1.e-3
        obj%pcut2 = obj%pcut2*1.e-3
      else if (obj%opt_oper =='HYPERBOLIC') then
        hdist     = 0.4142136*obj%zref
        obj%pcon  = 1.0/hdist
        obj%pmin  = obj%pmin*obj%pcon
        obj%dp    = obj%dp*obj%pcon
        obj%pcut1 = obj%pcut1*obj%pcon
        obj%pcut2 = obj%pcut2*obj%pcon
      end if

      ! Scalars for point source mode

      obj%tsqrt = 1
      if (obj%point_source) then
        do j = 1, obj%ndpt
          obj%tsqrt(j) = sqrt(obj%dt*(j-1))
        end do
        
      end if


!</execute_only>

    !!-------------------------- finish update ---------------------------!!
    !!-------------------------- finish update ---------------------------!!
    !!-------------------------- finish update ---------------------------!!

    !
    obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
    !
  end subroutine rmul_update


  !!----------------------------- traps ------------------------------------!!
  !!----------------------------- traps ------------------------------------!!
  !!----------------------------- traps ------------------------------------!!


  !!------------------------- main execution -------------------------------!!
  !!------------------------- main execution -------------------------------!!
  !!------------------------- main execution -------------------------------!!

!<execute_only>

  subroutine rmul (obj,ntr,hd,tr)
    !
    ! - Arguments
    !
    type(rmul_struct), intent (inout) :: obj             ! arguments
    integer,           intent (inout) :: ntr             ! arguments
    double precision,  intent (inout) :: hd (:,:)        ! arguments
    real,              intent (inout) :: tr (:,:)        ! arguments
    !
    ! - Local variables
    !
    integer :: trace, itmin, numlive, icount
    integer :: trlive (obj%numtr)
    integer :: ixmin (1)
    !
    double precision   :: hd_tmp(obj%nwih, ntr)
    real    :: stk  (obj%ndpt, obj%np)        ! Parabolic radon-space stack
    real    :: dta  (obj%ndpt, obj%numtr)     ! Modeled Multiples

    real    :: offsets (obj%numtr)           ! Copy of trace offsets
    real    :: tcpu1, x2max, top_mute, tmin
    real    :: offmax, offmax_hrd, off_inc
    real    :: rmute_min
    real    :: freq
    integer :: mute_min, numf, jf

    complex, allocatable, save   :: tbl_store (:,:,:)   ! Local variables

    !
    ! - Begin rmul
    !
    tcpu1 = getsys_seconds()
    !
    if (ntr == NO_MORE_TRACES) then
      obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
      call rmul_wrapup (obj)
      return
    end if
    !
    if (ntr == 1) then
      call pc_warning (msg1 = 'RMUL: Gather number ',              &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),      &
                       msg2 = ' has only one trace; not processed.')
      obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
      return
    end if
    !
    if (ntr > obj%numtr) then
      call pc_warning (msg1 = 'RMUL: Gather number ',                  &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),          &
                       msg2 = ' too big - processing only NUMTR traces;&
                              & gather size was ',                     &
                       var2 = ntr )
      ntr = obj%numtr
    end if
    !
    ! - Move live traces to dta array.
    ! - Fill offsets array with SQUARED offsets (normalized
    ! - by square of max offset) for parabolic stack.
    !

    rmute_min = 999999.
    hd_tmp(1:obj%nwih, 1:ntr) = hd(1:obj%nwih, 1:ntr)
    numlive = 0

    do trace = 1, ntr
      if (hd(HDR_LAV,trace) /= 0.) then
        numlive = numlive + 1
        trlive(numlive)  = trace
        dta(1:obj%ndpt, numlive)  = tr(1:obj%ndpt, trace)
        rmute_min = min( real(hd(HDR_TOP_MUTE, trace)),rmute_min)        

        if (obj%opt_oper =='HYPERBOLIC') then
          offsets(numlive) = real(hd(HDR_OFFSET, trace)) ** 2
          offsets(numlive) = sqrt(offsets(numlive)+ (obj%zref*obj%zref))&
                             - obj%zref
        else if (obj%opt_oper =='LINEAR') then     
          offsets(numlive) = real(hd(HDR_OFFSET, trace)) 
        else 
          offsets(numlive) = real(hd(HDR_OFFSET, trace)) ** 2
        end if 
      end if
    end do

    if (numlive < 2) then
      call pc_warning (msg1 = 'RMUL: Gather number ',              &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),      &
                       msg2 = ' has < 2 live traces; not processed.')
      obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
      return
    end if

    if (obj%opt_oper =='PARABOLIC') then
      x2max = maxval (array = real(hd(HDR_OFFSET, :ntr))**2)
      offmax = sqrt(x2max)
      offmax_hrd = maxval (array = real(hd(HDR_OFFSET, :ntr)))
    else 
      offmax = maxval (array = abs(real(hd(HDR_OFFSET, :ntr))))
      offmax_hrd = offmax
      x2max  = 1.
    end if 

    if (x2max > 0.) then
      x2max = 1. / x2max
      offsets(:numlive) = offsets(:numlive) * x2max
    else
      call pc_warning (msg1 = 'RMUL: Gather number ',                    &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),            &
                       msg2 = ' has no non-zero offsets; not processed.' )
      obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
      return
    end if
    !
    ! - Check if we have live window for modeling multiples
    !
    ixmin = minloc (offsets(:numlive))    ! Locate minimum offset
    trace = trlive(ixmin(1))
    top_mute  =  obj%tstrt  +  (nint(hd(HDR_TOP_MUTE,trace)) - 1) * obj%dt
    tmin      =  obj%win_beg_fctr * top_mute  +  obj%win_beg_add
    itmin     =  max (nint((tmin-obj%tstrt)/obj%dt)+1, 1)
    if (itmin >= obj%itmax) then
      call pc_warning (msg1 = 'RMUL: Gather number ',              &
                       var1 = nint(hd(HDR_CURRENT_GROUP, 1)),      &
                       msg2 = ' has empty multiple modeling window;&
                              & not processed.')
      obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
      return
    end if
    tmin = obj%tstrt + (itmin-1)*obj%dt
  
    obj%min_tstart = itmin

    if (obj%opt_oper=='LINEAR') then
      if (obj%point_source) then
         do trace = 1, numlive
           dta(1:obj%ndpt,trace) = dta(1:obj%ndpt,trace)*obj%tsqrt(1:obj%ndpt) 
         end do
      end if
    end if

    numf = obj%jf2 - obj%jf1 + 1
    if (allocated(tbl_store)) deallocate (tbl_store)

    allocate (tbl_store(numlive, obj%np, numf))

     do jf = obj%jf1, obj%jf2
         freq     = (jf-1) * obj%df       
         call rmul_tbl_set (freq  = freq,          &
                         nx       = numlive,       &
                         offsets  = offsets,       &
                         np       = obj%np,        &
                         pmin     = obj%pmin,      &
                         dp       = obj%dp,        &
                         tbl      = tbl_store(:,:,jf-obj%jf1+1) )
    end do

    !
    ! - Transform to parabolic, linear, or hyperbolic stack
    !
    if ( obj%opt_rmul == 'HR_RADON') then

         call rmul_transHires_to_pstk(obj,                      &
                                nx           = numlive,         &
                                offsets      = offsets,         &
                                dta          = dta,             &
                                mdl          = stk,             &
                                tbl_store    = tbl_store)     

     else 

       call rmul_transform_to_pstk(obj,                         &
                                nx           = numlive,         &
                                offsets      = offsets,         &
                                dta          = dta,             &
                                mdl          = stk )
     end if

    if (obj%opt_output == 'TAU-P' .or. obj%opt_output == 'TAUP_MUTE') then  

      ntr     = obj%np

      !  compute source, reciever, and p values in header

      mute_min = rmute_min * obj%win_beg_fctr
      off_inc  = offmax_hrd/ntr

      do icount = 1, ntr

        tr(1:obj%ndpt, icount)   = stk(1:obj%ndpt, icount)
        tr(1:mute_min, icount)   = 0.

        hd(1:obj%nwih, icount)   = hd_tmp(1:obj%nwih, 1)
        hd(obj%hdr_pmute,icount) = 1000.*(obj%pmin + (icount-1)*obj%dp)

        ! UNIT is in micro sec per distance

        if (obj%opt_oper == 'LINEAR') then              
            hd(obj%hdr_pmute,icount) = 1000.*hd(obj%hdr_pmute,icount)
        else if (obj%opt_oper =='HYPERBOLIC') then
            hd(obj%hdr_pmute,icount) = hd(obj%hdr_pmute,icount)/obj%pcon
        end if

        hd(HDR_TOP_MUTE,icount)  = mute_min 
        hd(HDR_SOURCE_XLOC,icount) = hd(HDR_MIDPOINT_XLOC,icount)  &
                                     - 0.5*off_inc*icount
        hd(HDR_SOURCE_YLOC,icount) = hd(HDR_MIDPOINT_YLOC,icount)
        hd(HDR_RECEIVER_XLOC,icount) = hd(HDR_MIDPOINT_XLOC,icount)  &
                                     + 0.5*off_inc*icount
        hd(HDR_RECEIVER_YLOC,icount) = hd(HDR_MIDPOINT_YLOC,icount)
      end do

      ! - Set LAV header
     
      if (obj%opt_output /= 'TAUP_MUTE') then
         call lav_set_hdr (hd, tr, obj%ndpt, ntr)
         return 
      end if
        
    end if   

    !
    ! - Remove primary moveouts from model of multiples
    !
   
    if (obj%opt_output /= 'FOR_INV_TAUP') then

      if (obj%fhead_mute .or. obj%ftail_mute ) then 
        do icount = 1, obj%np
          hd(obj%hdr_pmute,icount) = 1000.*(obj%pmin + (icount-1)*obj%dp)
          if (obj%opt_oper == 'LINEAR') then
             hd(obj%hdr_pmute,icount) = 1000.*hd(obj%hdr_pmute,icount)
          else if (obj%opt_oper =='HYPERBOLIC') then
             hd(obj%hdr_pmute,icount) = hd(obj%hdr_pmute,icount)/obj%pcon
          end if
        end do
      end if
      
      if (obj%fhead_mute .and. obj%ftail_mute ) then            
         call mute (obj%mute_head, obj%np, hd, stk) 
         call mute (obj%mute_tail, obj%np, hd, stk)
      else if (obj%fhead_mute) then
         call mute (obj%mute_head, obj%np, hd, stk)
      else if (obj%ftail_mute) then             
         call mute (obj%mute_tail, obj%np, hd, stk) 
      else
 
        call rmul_tzr2 (stk   = stk,       &
                    ndpt  = obj%ndpt,  &
                    np    = obj%np,    &
                    pmin  = obj%pmin,  &
                    dp    = obj%dp,    &
                    pcut1 = obj%pcut1, &
                    pcut2 = obj%pcut2  )     
    !
    ! - Limit model of multiples to user-specified time window
    !
        call rmul_pmut (stk   = stk,            &
                    ndpt  = obj%ndpt,       &
                    tstrt = obj%tstrt,      &
                    dt    = obj%dt,         &
                    np    = obj%np,         &
                    pmin  = obj%pmin,       &
                    dp    = obj%dp,         &
                    tmute = obj%tim_mute,   &
                    tmin  = tmin,           &
                    tmax  = obj%win_end )

      end if 
    end if 

    if (obj%opt_output == 'TAUP_MUTE') then

      ntr     = obj%np
      do icount = 1, ntr
        tr(1:obj%ndpt, icount) = stk(1:obj%ndpt, icount)
      end do
      call lav_set_hdr (hd, tr, obj%ndpt, ntr)
      return
    end if
    !
    ! - transform back
    !
    call rmul_transform_from_pstk (ndpt      = obj%ndpt,       &
                                   ntpow     = obj%ntpow,      &
                                   nx        = numlive,        &
                                   offsets   = offsets,        &
                                   np        = obj%np,         &
                                   pmin      = obj%pmin,       &
                                   dp        = obj%dp,         &
                                   nf        = obj%nfpt,       &
                                   jf1       = obj%jf1,        &
                                   jf2       = obj%jf2,        &
                                   df        = obj%df,         &
                                   rcfft_obj = obj%rcfft_obj,  &
                                   crfft_obj = obj%crfft_obj,  &
                                   mdl       = stk,            &
                                   dta       = dta,            &
                                  tbl_store  = tbl_store )
   
    !
    ! - subtract from original data and reapply mutes
    !
    hd(1:obj%nwih, 1:ntr) = hd_tmp(1:obj%nwih, 1:ntr)

    do icount = 1, numlive
      trace = trlive(icount)
      if (obj%opt_output == 'FOR_INV_TAUP'     .or.           &
          obj%opt_output == 'MODEL' )  then
        tr(1:obj%ndpt, trace) = dta(1:obj%ndpt, icount)
      else 
        tr(1:obj%ndpt, trace) = tr(1:obj%ndpt, trace) - dta(1:obj%ndpt, icount)
      end if
      call mutehw (hd(:, trace), tr(:, trace), obj%ndpt, 0., MUTEHW_BOTH)
    end do

    ! - Set LAV header
    !
    call lav_set_hdr (hd, tr, obj%ndpt, ntr)
    !
    ! - Record time spent in this process
    !
    obj%tcpu = obj%tcpu + getsys_seconds() - tcpu1
    return
    !
  end subroutine rmul


  subroutine rmul_transform_to_pstk ( obj,   &
                       nx, offsets, dta, mdl )

!
    type(rmul_struct),intent(inout) :: obj            

    integer ,          intent (in)  :: nx
    real    ,          intent (in)  :: offsets (:)

    real    ,          intent (in)  :: dta (:   , : )
    real    ,          intent (out) :: mdl (obj%ndpt, obj%np)
 
! 
    complex,pointer :: c_dta (:,:)          ! Local variables
    complex,pointer :: c_mdl (:,:)          ! Local variables

    integer :: ival                      ! Local variables

    real    :: sw
    nullify(c_dta)
    nullify(c_mdl)
    allocate (c_dta (nx, obj%nfpt) )
    allocate (c_mdl (obj%np, obj%nfpt) )
   
    call rmul_transform_to_freq (ndpt      = obj%ndpt,        &
                                 ntpow     = obj%ntpow,       &
                                 nf        = obj%nfpt,        &
                                 ntr       = nx,              &
                                 fft_obj   = obj%rcfft_obj,   &
                                 time_data = dta,             &
                                 freq_data = c_dta)
!
    c_mdl (:, :obj%jf1-1) = (0., 0.)
    c_mdl (:, obj%jf2+1:) = (0., 0.)
!

    sw       = obj%df*2.*PI
    call rmul_phaser(  nf         = obj%nfpt,       &
                       ra         = c_dta,          &        
                       mdl        = c_mdl,          &
                       if1        = obj%jf1,        &   
                       ifn        = obj%jf2,        &   
                       sw         = sw,             &      
                       nx         = nx,             &      
                       x          = offsets,        & 
                       np         = obj%np,         &  
                       p1         = obj%pmin,       &
                       dp         = obj%dp,         &
                       pnois      = obj%pnoise,     &
                       opt_output = obj%opt_output, &
                       ipr        = 0 )     

    if (obj%opt_oper=='LINEAR') then
      if (obj%point_source) then
         ival = 2
         call rmul_radphs( obj%np, c_mdl, obj%np, obj%nfpt, ival )
      end if

      if (obj%freq_bal) then
         ival = -1
         call rmul_radbal( obj%np, c_mdl, obj%np, obj%nfpt, obj%nfpt, ival)
      end if
    end if

    call rmul_transform_to_time (ndpt      = obj%ndpt,      &
                                 ntpow     = obj%ntpow,     &
                                 nf        = obj%nfpt,      &
                                 ntr       = obj%np,        &
                                 fft_obj   = obj%crfft_obj, &
                                 freq_data = c_mdl,         &
                                 time_data = mdl )
!
    deallocate (c_dta)
    deallocate (c_mdl)

  end subroutine rmul_transform_to_pstk



  !!------------------------ rmul_transHires_to_pstk ------------------------!!
  !!------------------------ rmul_transHires_to_pstk ------------------------!!
  !!------------------------ rmul_transHires_to_pstk ------------------------!!
  !
  ! - High-resolution radon transform.
  !
  !   The model is evenly sampled in ray parameter (p) but the data may, in
  !     general, be irregularly sampled in offset (OFFSETS array).
  !
  !   (Note: to perform parabolic radon transform, RMUL actually supplies
  !      SQUARES of offsets, normalized by square of far offset, in the
  !      OFFSETS array.)
  !
  !   Mapping from (tau,p) to (t,x) is performed by RMUL_TRANSFORM_FROM_PSTK
  !     and is a simple slant stack which, in continuous form, would be:
  !                      /
  !       dta(t,x)  =    | mdl(tau= t - p*x, p) dp
  !                      /
  !     where "x" denotes contents of the OFFSETS array.
  !
  !   Mapping from (t,x) to (tau,p) is performed by RMUL_TRANSFORM_TO_PSTK
  !     which is a least squares inverse of RMUL_TRANSFORM_FROM_PSTK, where
  !     inversion is implemented by conjugate gradients. NTER is the number
  !     of conjugate gradient steps to perform in RMUL_TRANSFORM_TO_PSTK;
  !     use at least 3.
  !
  !   All transforms are actually implemented in the frequency domain, one
  !     frequency at a time. Assumed forward fourier kernel is:
  !                /
  !       dta(f) = | cexp(-i 2 pi f t) dta(t) dt
  !                /
  !
  subroutine rmul_transHires_to_pstk (obj,               &
                               nx, offsets, dta, mdl, tbl_store )
!
    type(rmul_struct),intent(inout) :: obj  

    integer ,          intent (in)  :: nx
    real    ,          intent (in)  :: offsets (:)
    real    ,          intent (in)  :: dta (:   , : )
    real    ,          intent (out) :: mdl (obj%ndpt, obj%np)
    complex ,          intent (in)  :: tbl_store (:,:,:)
!
    complex :: c_dta (nx, obj%nfpt)          ! Local variables
    complex :: c_mdl (obj%np, obj%nfpt)      ! Local variables

    real    :: sum_mdl (obj%np)              ! Local variables
    real    :: sum_mdl2(obj%np)              ! Local variables

    real       dta_tmp(obj%ndpt,nx)

    integer :: jf
    real    :: freq                          ! Local variables
    real    :: model_wt(obj%np)              ! Local variables
    real    :: amodel_wt (obj%np,obj%np)     ! Local variables

    integer :: ip, nter_model                ! Local variables
    integer :: jfc1, jfc2, nfc

    real    :: power
    integer :: iter

    real    :: valmax
!

!   transform data between the mute time and end of window

     dta_tmp(1:obj%min_tstart-1,1:nx) = 0.0
     dta_tmp(obj%min_tstart:obj%ndpt,1:nx) =                 &
     dta(obj%min_tstart:obj%ndpt,1:nx)     

    call rmul_transform_to_freq (ndpt      = obj%ndpt,       &
                                 ntpow     = obj%ntpow,      &
                                 nf        = obj%nfpt,       &
                                 ntr       = nx,             &
                                 fft_obj   = obj%rcfft_obj,  &
                                 time_data = dta_tmp,        &
                                 freq_data = c_dta)


!.. compute frequency bandwidth 

    call rmul_center_band( obj%jf1, obj%jf2, obj%df, nx, c_dta, jfc1, jfc2)
    nfc = jfc2 - jfc1 + 1
 
    c_mdl (:, :obj%jf1-1) = (0., 0.)
    c_mdl (:, obj%jf2+1:) = (0., 0.)
          
    power          = 2
    sum_mdl(1:obj%np)  = 0.0 
    sum_mdl2(1:obj%np) = 0.0
    amodel_wt      = 0.

!.. compute initial Radon solution

    do jf = jfc1, jfc2, 2
       freq     = (jf-1) * obj%df
      
       call conjgrad (nx   = obj%np,           &
                     x    = c_mdl(:, jf),  &
                     ny   = nx,            &
                     y    = c_dta(:, jf),  &
                     a    = tbl_store(:,:,jf-obj%jf1+1),   &
                     nter = obj%nter )            

       sum_mdl(1:obj%np) = sum_mdl(1:obj%np) + abs(c_mdl(1:obj%np, jf))

    end do

    sum_mdl(1:obj%np) = sum_mdl(1:obj%np)**power
    valmax        = maxval(sum_mdl(1:obj%np))
    sum_mdl(1:obj%np) = sum_mdl(1:obj%np)/valmax

!...compute final weights for a range of frequency

    nter_model = 2
    do iter = 1, nter_model

      do jf = jfc1, jfc2, 2

        if ( nx > obj%np) then

          do ip = 1, obj%np
             if ( 1./(sum_mdl(ip)+EPSILSON) < 1./(SIGNA*SIGNA)) then            
                model_wt(ip) = obj%pnoise/(sum_mdl(ip))
             else
               model_wt(ip) = 1./(SIGNA*SIGNA)        
             end if
          end do

          call rmul_Hires_over(                               &
                        a        = tbl_store(:,:,jf-obj%jf1+1),  &
                        x        = c_mdl(:, jf),    &
                        y        = c_dta(:, jf),    &
                        nrows    = nx,              &
                        ncols    = obj%np,              &          
                        model_wt = model_wt,        &
                        nter     = obj%nter) 
        else 
 
          do ip = 1, obj%np
            amodel_wt(ip,ip) = sum_mdl(ip)        
          end do

          call rmul_Hires_under(                              &
                        a        = tbl_store(:,:,jf-obj%jf1+1),  &
                        x        = c_mdl(:, jf),    &
                        y        = c_dta(:, jf),    &
                        nrows    = nx,              &
                        ncols    = obj%np,              &          
                        model_wt = amodel_wt,       &
                        pwhite   = obj%pnoise )

        end if
        
        sum_mdl2(1:obj%np) = sum_mdl2(1:obj%np) + abs(c_mdl(1:obj%np, jf))   

       end do       ! end computing the weights

       sum_mdl2(1:obj%np) = sum_mdl2(1:obj%np)**power     
       valmax        = maxval(sum_mdl2(1:obj%np))
       sum_mdl2(1:obj%np) = sum_mdl2(1:obj%np)/valmax
       sum_mdl = sum_mdl2 
       if (iter /= nter_model) sum_mdl2(1:obj%np) = 0.0       
      end do

!.... compute the high-resolution solution of all frequency

      do jf = obj%jf1, obj%jf2
       freq     = (jf-1) * obj%df      
       if ( nx > obj%np) then      

           do ip = 1, obj%np

             if ( 1./(sum_mdl2(ip)+EPSILSON) < 1./(SIGNA*SIGNA)) then
                model_wt(ip) = obj%pnoise/(sum_mdl2(ip))
             else
               model_wt(ip) = 1./(SIGNA*SIGNA)             
             end if

          end do
        
          call rmul_Hires_over(                     &
                        a        = tbl_store(:,:,jf-obj%jf1+1), &
                        x        = c_mdl(:, jf),    &
                        y        = c_dta(:, jf),    &
                        nrows    = nx,              &
                        ncols    = obj%np,          &          
                        model_wt = model_wt,        &
                        nter     = obj%nter) 
       else 
 
          do ip = 1, obj%np
             amodel_wt(ip,ip) = sum_mdl2(ip)        
          end do

          call rmul_Hires_under(                    &
                        a        = tbl_store(:,:,jf-obj%jf1+1), &
                        x        = c_mdl(:, jf),    &
                        y        = c_dta(:, jf),    &
                        nrows    = nx,              &
                        ncols    = obj%np,              &          
                        model_wt = amodel_wt,       &
                        pwhite   = obj%pnoise) 
       end if

    end do
!

    call rmul_transform_to_time (ndpt      = obj%ndpt,      &
                                 ntpow     = obj%ntpow,     &
                                 nf        = obj%nfpt,        &
                                 ntr       = obj%np,        &
                                 fft_obj   = obj%crfft_obj, &
                                 freq_data = c_mdl,     &
                                 time_data = mdl )

!..... a small taper of beginning and end of the solution

       mdl(1:obj%ndpt,1) = 2./10 * mdl(1:obj%ndpt,1)
       mdl(1:obj%ndpt,2) = 5./10.* mdl(1:obj%ndpt,2)
       mdl(1:obj%ndpt,3) = 8./10.* mdl(1:obj%ndpt,3)

       mdl(1:obj%ndpt,obj%np)   = 2./10.* mdl(1:obj%ndpt,obj%np)
       mdl(1:obj%ndpt,obj%np-1) = 5./10.* mdl(1:obj%ndpt,obj%np-1)
       mdl(1:obj%ndpt,obj%np-2) = 8./10.* mdl(1:obj%ndpt,obj%np-2)
!
  end subroutine rmul_transHires_to_pstk


      subroutine rmul_Hires_over(a, x, y, nrows, ncols, model_wt, nter) 
!
!  purpose:   compute Hi-resolution transform of over-determined system
!
!  input:
!             a          cx   input matrix with dimension nrows*ncols.
!             x          cx   input vector with dimension ncols.
!             y          cx   input vector with dimension nrows.
!             nrows      int  row dimension of the input matrix.
!             ncols      int  col dimension of the input matrix.
!
!  output:
!             ata        cx   output matrix with dimension ncols*ncols.
!             atb        cx   output matrix with dimension 1*ncols.
!
!
      implicit none

      integer , intent(in)    :: nrows                      ! arguments
      integer , intent(in)    :: ncols                      ! arguments
      complex , intent(in)    :: a(nrows,ncols)             ! arguments
      complex , intent(inout) :: x(ncols)                   ! arguments 
      complex , intent(in)    :: y(nrows)                   ! arguments 
      real    , intent(in)    :: model_wt(ncols)            ! arguments
      integer , intent(in)    :: nter                       ! arguments

      integer        :: ia                                  ! local 

      complex        :: at(ncols,nrows)                     ! local
      complex        :: ata(ncols,ncols)                    ! local 
      complex        :: aty(ncols)                          ! local 
      complex        :: xstart(ncols)                       ! local

      logical        :: sing

!.........  form a transpose a & y
 
      at = transpose(a) 
      at =  conjg(at)
      aty = matmul(at,y) 
      ata = matmul(at,a) 

      xstart = x 

!....  add prewhitening to the diagonal ata

      do ia = 1, ncols 
         ata(ia,ia) = ata(ia,ia)*(1.+ model_wt(ia))
      end do 

      call  gausselim_lineqs (ncols, ata, x, aty, sing)
      if ( sing ) write(*,*) ' matrix singular '
 
     return  
     end subroutine rmul_Hires_over 

     subroutine rmul_Hires_under(a, x, y, nrows, ncols, model_wt, pwhite) 
!
!  purpose:   compute Hi-resolution transform of under-determined system
!
!  input:
!             a          cx   input matrix with dimension nrows*ncols.
!             x          cx   input vector with dimension ncols.
!             y          cx   input vector with dimension nrows.
!             nrows      int  row dimension of the input matrix.
!             ncols      int  col dimension of the input matrix.
!
!  output:
!             ata        cx   output matrix with dimension ncols*ncols.
!             atb        cx   output matrix with dimension 1*ncols.
!
!
      implicit none

      integer , intent(in)    :: nrows                      ! arguments
      integer , intent(in)    :: ncols                      ! arguments
      complex , intent(in)    :: a(nrows,ncols)             ! arguments
      complex , intent(inout) :: x(ncols)                   ! arguments 
      complex , intent(in)    :: y(nrows)                   ! arguments 
      real    , intent(inout) :: model_wt(ncols,ncols)          ! arguments
      real    , intent(in)    :: pwhite

      integer        :: ia                                  ! local 
      complex        :: aw(nrows,ncols)                     ! local
      complex        :: at(ncols,nrows)                     ! local
      complex        :: at_aat_inv(ncols,nrows)             ! local
      complex        :: aat(nrows,nrows)                    ! local 
      complex        :: aat_inv(nrows,nrows)                ! local

      logical        :: sing

      real           :: valmax

!.........  form a transpose a & y

      at = transpose(a) 
      at =  conjg(at)

      aw  = matmul(a, model_wt)
      aat = matmul(aw, at)

      valmax = 0.0
      do ia = 1, nrows 
         valmax = max( valmax, abs(aat(ia,ia)) )
      end do 

      do ia = 1, nrows 
           aat(ia,ia) = aat(ia,ia) + valmax*pwhite
      end do 

      call  gausselim_matinv ( nrows, aat, aat_inv, sing)
      if ( sing ) write(*,*) ' matrix singular '

      at = matmul(model_wt, at)

      at_aat_inv = matmul(at, aat_inv)

      at_aat_inv(1,1:nrows) = 1./10*at_aat_inv(1,1:nrows)
      at_aat_inv(2,1:nrows) = 5./10*at_aat_inv(2,1:nrows)
      at_aat_inv(ncols,1:nrows)   = 1./10*at_aat_inv(ncols,1:nrows)
      at_aat_inv(ncols-1,1:nrows) = 5./10*at_aat_inv(ncols-1,1:nrows)

      x = matmul(at_aat_inv, y)
      
      return  
      end subroutine rmul_Hires_under 


  !!------------------------ rmul_center_band ------------------------!!
  !!------------------------ rmul_center_band ------------------------!!
  !!------------------------ rmul_center_band ------------------------!!
  !
  ! ---- find center frequency bandwidth
  !
    subroutine rmul_center_band( jf1, jf2, df, nx, c_dta, jfc1, jfc2)



   integer ,          intent (in)  :: jf1, jf2, nx
   complex ,          intent (in)  :: c_dta (:, :)      
   real    ,          intent (in)  :: df
   integer ,          intent (out) :: jfc1, jfc2

   integer        :: jf
   real           :: fpower, power, tmp, f_center, freq_width, freq

    fpower = 0.0
    power  = 0.0

    do jf = jf1, jf2
       freq     = (jf-1) * df       
       tmp = sum(abs( c_dta(1:nx, jf)*c_dta(1:nx, jf) ))
       power   = power + tmp
       fpower  = fpower + tmp*freq
    end do

    f_center = fpower/power

    fpower = 0.0
    power  = 0.0
    do jf = jf1, jf2
       freq    = (jf-1) * df       
       tmp     = sum(abs( c_dta(1:nx,jf)*c_dta(1:nx, jf) ))
       power   = power + tmp
       fpower  = fpower + tmp*(freq-f_center)*(freq-f_center)
    end do

!... frequency bandwidth of half power

    freq_width = sqrt(fpower/power)
 
    jfc1 = (f_center - 0.75*freq_width)/df   
    jfc2 = (f_center + 0.75*freq_width)/df
  
    return 
    end subroutine rmul_center_band




  !!------------------------ rmul_transform_to_freq ------------------------!!
  !!------------------------ rmul_transform_to_freq ------------------------!!
  !!------------------------ rmul_transform_to_freq ------------------------!!
  !
  ! - Transform from Time to Frequency domain
  !
  subroutine rmul_transform_to_freq (ndpt, ntpow, nf, ntr, fft_obj,  &
                                     time_data, freq_data)
!
    integer,           intent (in)  :: ndpt
    integer,           intent (in)  :: ntpow
    integer,           intent (in)  :: nf
    integer,           intent (in)  :: ntr
    type (fft_struct), pointer      :: fft_obj
    real,              intent (in)  :: time_data (:,   :)
    complex,           intent (out) :: freq_data (ntr, nf)

!
    integer :: i                     ! Local variables
    real    :: scale_factor          ! Local variables
    real    :: r_work (2*(ntpow/2+1))        ! Local variables
    type(fft_struct)                :: local_obj
!
    scale_factor     = 1.0 / real(ntpow)
    r_work (ndpt+1:) = 0.0      ! Clear the padding elements past the trace

    !-wmm added
    local_obj%plan =fft_obj%plan
    local_obj%type =fft_obj%type
    local_obj%size =fft_obj%size
    local_obj%sign =fft_obj%sign
    local_obj%wsiz =fft_obj%wsiz
    local_obj%scale=fft_obj%scale
    local_obj%tsiz =fft_obj%tsiz
    local_obj%stdo =fft_obj%stdo
    local_obj%work =fft_obj%work
    local_obj%table=fft_obj%table
    local_obj%title=fft_obj%title
    !-wmm end

!
    do i = 1, ntr
      r_work (:ndpt) = time_data (:ndpt, i) * scale_factor
      call fft_rc_transform (obj  = local_obj,   &
                             bufi = r_work,    &
                             bufo = freq_data(i, :) )
    end do
!
  end subroutine rmul_transform_to_freq

  !!------------------------ rmul_transform_to_time ------------------------!!
  !!------------------------ rmul_transform_to_time ------------------------!!
  !!------------------------ rmul_transform_to_time ------------------------!!
  !
  ! - Transform from Frequency to Time domain
  !
  subroutine rmul_transform_to_time (ndpt, ntpow, nf, ntr, fft_obj,  &
                                     freq_data, time_data)
!
    integer,           intent (in)  :: ndpt
    integer,           intent (in)  :: ntpow
    integer,           intent (in)  :: nf
    integer,           intent (in)  :: ntr
    type (fft_struct), pointer      :: fft_obj
    complex,           intent (in)  :: freq_data (ntr, nf)
    real,              intent (out) :: time_data (:,   :)
!
    integer :: i                     ! Local variables
    real    :: r_work (2*(ntpow/2+1))        ! Local variables
    type(fft_struct)                :: local_obj

    !-wmm added
    local_obj%plan =fft_obj%plan
    local_obj%type =fft_obj%type
    local_obj%size =fft_obj%size
    local_obj%sign =fft_obj%sign
    local_obj%wsiz =fft_obj%wsiz
    local_obj%scale=fft_obj%scale
    local_obj%tsiz =fft_obj%tsiz
    local_obj%stdo =fft_obj%stdo
    local_obj%work =fft_obj%work
    local_obj%table=fft_obj%table
    local_obj%title=fft_obj%title
    !-wmm end
!
    do i = 1, ntr
       call fft_cr_transform (obj  = local_obj,         &
                              bufi = freq_data(i, :), &
                              bufo = r_work)
      time_data(:ndpt, i) = r_work(:ndpt)
    end do
!
  end subroutine rmul_transform_to_time

!!------------------------------ rmul_tbl_set ------------------------------!!
!!------------------------------ rmul_tbl_set ------------------------------!!
!!------------------------------ rmul_tbl_set ------------------------------!!
!
  subroutine rmul_tbl_set (freq, nx,offsets, np,pmin,dp, tbl)
!
!  Calculate matrix (TBL) that applies transform from pstk space to ordinary
!  offset space at a single frequency.
!  Note - For parabolic stack, "offsets" must contain SQUARES of offsets.
!
    real    , intent (in)   :: freq
    integer , intent (in)   :: nx
    real    , intent (in)   :: offsets (:)
    integer , intent (in)   :: np
    real    , intent (in)   :: pmin
    real    , intent (in)   :: dp
    complex , intent (out)  :: tbl (nx,np)
!
    real, parameter         :: minus_two_pi  =  -2.0 * PI
!
    integer  :: ip, ix            ! local variables
    real     :: pfact, r, arg     ! local variables
!
    do ip = 1, np
      pfact = freq * (pmin + (ip-1)*dp)
      do ix = 1, nx
        r   = pfact * offsets(ix)
        arg = minus_two_pi * (r - aint(r))
        tbl(ix,ip) = cmplx (cos(arg), sin(arg))
      end do
    end do
!
  end subroutine rmul_tbl_set


!!------------------------------- rmul_tzr2 --------------------------------!!
!!------------------------------- rmul_tzr2 --------------------------------!!
!!------------------------------- rmul_tzr2 --------------------------------!!
!
! - zero p's left of pcut1, keep right of pcut2, linear taper between.
!
    subroutine rmul_tzr2 (stk, ndpt, np,pmin,dp, pcut1,pcut2)
!
    real,    intent (inout) :: stk (ndpt, np)
    integer, intent (in)    :: ndpt
    integer, intent (in)    :: np
    real,    intent (in)    :: pmin
    real,    intent (in)    :: dp
    real,    intent (in)    :: pcut1
    real,    intent (in)    :: pcut2
!
    integer :: ip                      ! local variables
    real    :: dir, pfact, p, r        ! local variables
!
    if (pcut2 /= pcut1) then
      pfact = 1. / (pcut2 - pcut1)
      dir = sign (1., pfact)
    else
      pfact = 0.
      dir = 1.
    end if
!
    do ip = 1, np
      p = pmin + (ip-1)*dp
      if ((p - pcut1) * (p - pcut2) < 0.) then   ! between two cutoffs.
        r = (p - pcut1) * pfact
        stk(:, ip) = stk(:, ip) * r
      else if ((p - pcut1) * dir <= 0.) then     ! to "left" of pcut1.
        stk(:, ip) = 0.
      endif
    end do
!
    end subroutine rmul_tzr2


!!------------------------------- rmul_pmut --------------------------------!!
!!------------------------------- rmul_pmut --------------------------------!!
!!------------------------------- rmul_pmut --------------------------------!!
!
! - Apply a velocity mute to a parabolic stack.  Assume the maximum
!   p (curvature) is appropriate for the time at tmute.
!
    subroutine rmul_pmut (stk, ndpt,tstrt,dt, np,pmin,dp, tmute,tmin,tmax)
!
    real,    intent(inout) :: stk (ndpt, np)
    integer, intent(in)    :: ndpt
    real,    intent(in)    :: tstrt, dt
    integer, intent(in)    :: np
    real,    intent(in)    :: pmin, dp
    real,    intent(in)    :: tmute, tmin, tmax
!
    integer :: ip, it1, it2                               ! local
    real    :: end_trace, pmax, ptprod, ptest, p, tlast   ! local
!
    end_trace = tstrt + (ndpt-1)*dt
    pmax      = max ( abs(pmin) , abs(pmin + (np-1)*dp) )
    ptprod    = pmax * tmute
    ptest     = ptprod / end_trace
!
    it1 = min ( max(nint((tmin-tstrt)/dt)+1,1) , ndpt )
!
    do ip = 1, np
      stk(1:it1-1, ip) = 0.
      p = pmin + (ip-1)*dp
      if (abs(p) <= ptest) then
        tlast = end_trace
      else
        tlast = ptprod / abs(p)
      endif
      tlast = min (tlast, tmax)
      it2 = min ( max(nint((tlast-tstrt)/dt)+1,1) , ndpt )
      stk(it2+1:ndpt, ip) = 0.
    end do
!
    end subroutine rmul_pmut


  !!----------------------- rmul_transform_from_pstk -----------------------!!
  !!----------------------- rmul_transform_from_pstk -----------------------!!
  !!----------------------- rmul_transform_from_pstk -----------------------!!
  !
  !   This routine implements the mapping from the (tau,p) to (t,x) domain,
  !     which is a simple slant stack.  See the comments in subroutine
  !     RMUL_TRANSFORM_TO_PSTK which implements the least squares inverse
  !     of the present subroutine.
  !
  subroutine rmul_transform_from_pstk (ndpt,ntpow, nx,offsets, np,pmin,dp, &
                              nf,jf1,jf2,df, rcfft_obj,crfft_obj, mdl,dta, &
                              tbl_store )
!
    integer ,          intent (in)  :: ndpt, ntpow, nx
    real    ,          intent (in)  :: offsets (:)
    integer ,          intent (in)  :: np
    real    ,          intent (in)  :: pmin, dp
    integer ,          intent (in)  :: nf, jf1, jf2
    real    ,          intent (in)  :: df
    type (fft_struct), pointer      :: rcfft_obj
    type (fft_struct), pointer      :: crfft_obj
    real    ,          intent (in)  :: mdl (ndpt, np)
    real    ,          intent (out) :: dta (:,    :)
    complex ,          intent (in)  :: tbl_store (:,:,:)
!
    complex,pointer :: c_mdl (:,:)      ! Local variables
    complex,pointer :: c_dta (:,:)      ! Local variables

    integer :: jf, numf            ! Local variables
    real    :: freq                ! Local variables
!
    nullify(c_mdl)
    nullify(c_dta)
    allocate (c_mdl(np, nf))
    allocate (c_dta(nx, nf))


    numf = jf2 - jf1 + 1
    call rmul_transform_to_freq (ndpt      = ndpt,      &
                                 ntpow     = ntpow,     &
                                 nf        = nf,        &
                                 ntr       = np,        &
                                 fft_obj   = rcfft_obj, &
                                 time_data = mdl,       &
                                 freq_data = c_mdl)
!
    c_dta (:, :jf1-1) = (0., 0.)
    c_dta (:, jf2+1:) = (0., 0.)
!
    do jf = jf1, jf2
!
      freq     = (jf-1) * df
!

      c_dta (:, jf) = matmul (tbl_store(1:nx,1:np,jf-jf1+1), c_mdl(:, jf))
!
    end do
!
    call rmul_transform_to_time (ndpt      = ndpt,      &
                                 ntpow     = ntpow,     &
                                 nf        = nf,        &
                                 ntr       = nx,        &
                                 fft_obj   = crfft_obj, &
                                 freq_data = c_dta,     &
                                 time_data = dta )
    deallocate (c_mdl)
    deallocate (c_dta)

!
  end subroutine rmul_transform_from_pstk

! Compute the multiplication of a complex hermetian toeplitz matrix
! specified by its top row (a), with a complex vector (x).  Output
! a complex vector (y).
! Original C code by John Anderson
! Modified and written in Fortran by Ted Shuck
! 
      subroutine rmul_htmul( n, a, x, y)
      implicit none
      complex czero
      
      integer ,       intent (in)     :: n
      complex ,       intent (in)     :: a(n)
      complex ,       intent (in)     :: x(n)
      complex ,       intent (inout)  :: y(n)

      integer   :: j, irow
      complex   :: ctemp

      czero=cmplx(0.0,0.0)

      do 300 irow=1,n
         ctemp = czero

         do 100 j=1,irow-1
            ctemp = ctemp + x(j)*conjg(a(irow-j+1))
 100     continue

         do 200 j=irow,n
            ctemp = ctemp + x(j)*a(j-irow+1)
 200     continue

         y(irow) = ctemp
 300  continue
      return
      end subroutine rmul_htmul
 
!
!-----------------------------------------------------------------------
!
!***********************************************************************
! 
  subroutine rmul_phaser(nf,ra,mdl,if1,ifn,sw,nx,x,np,p1,dp,pnois, &
                        opt_output,ipr)
  implicit none

!  frequency domain radon transform
!  nf - first dimension of transform array as declared in calling
!         program
!  ra -   input complex array containing fourier transformed traces
!         output complex array containing freq domain radon transform
!  if1 -  start frequency to be processed
!  if1 -  end   frequency to be processed
!  sw -   sample frequency in radians per second
!  nx -   number of traces
!  x  -   array containing distance for each trace
!  np -   number of p values
!  p1 -   first p value (units are slowness, i.e. sec/ft )
!  dp -   p increment
!  opt_output -- output option
!  ipr   - print unit number, 0 for no print
!
!  On input, ra must contain the fourier transformed input traces:
!    ((ra(i,j),i=1,nx),j=1,nf)
!
!  On output, ra contains the radon transform:
!    ((ra(i,j),i=1,np),j=1,nf)
!
!  Note The leading dimension nf of array ra must be greater than nf.

!  Arguments
        integer  :: nf, if1, ifn, nx, np, ipr
        real     :: sw, x(nx), p1, dp, pnois
        complex  :: ra(nx,nf), mdl(np,nf)
        character(len=9),  intent (in)  :: opt_output

!  Local variables
        integer   :: i,j,k,l
        real      :: rhz, w, wx, dpwx, arg, rsum, threshold
        real      :: freqs(10)
        complex   :: lt, ltltmp, ltp1, cdpwx

!  Dynamic
        complex   :: ltl(np), ltd(np), sa(np)
        complex   :: wk1(np)
  
!  Convert radians to hertz
        rhz = 1./(8.*atan(1.))

        if (ipr.ne.0) write (ipr,9010)
 9010 format (/,' Forward Radon transform - frequencies processed:')

!  Threshold for diagonal dominance (ala John Anderson, Mobil)
        threshold = 1.0 + 0.25*np
       
!  loop over frequency
        do j=if1,ifn

!     Initialize
                ltd(1:np) = (0.,0.)
                ltl(1:np) = (0.,0.)

!     set frequeny
            w = sw*(j-1)

!     loop over p and x, compute transform at this frequency
            do i=1,nx
!  Initialize constants for constructing matrices
                        wx = w * x(i)
                        dpwx = dp * wx
                        arg = p1 * wx
                        ltp1 = cmplx( cos(arg), sin(arg) )
                        cdpwx = cmplx( cos(dpwx), sin(dpwx) )
                        lt = ltp1
                        ltd(1) = ltd(1) + lt * ra(i,j)
                        ltltmp = 1.0
                        ltl(1) = ltl(1) + ltltmp
!  Loop over p and construct transform and R*R
                        do k=2,np
                                lt = lt * cdpwx
                                ltd(k) = ltd(k) + lt * ra(i,j)
                                ltltmp  = ltltmp * cdpwx
                                ltl(k) = ltl(k) + ltltmp
                        enddo
                enddo

!  Check for diagonal dominance (i.e. Anderson, Mobil)
                rsum = 0.0
     
                do k=2,np
                   rsum = rsum + cabs(ltl(k))        
                enddo
                rsum = rsum / real(ltl(1))

!  Call routines based on threshhold

                if (rsum.gt.threshold) then

                       ltl(1) = ltl(1) * (1.0 + pnois*10)
                       call rmul_ctoepdv( np, ltl, ltd, sa, wk1)

!                       call rmul_ctoepcg( np, np, ltl, sa, ltd, wk1,     &
!                       wk2, wk3, wk4, niter)  

                else

                       ltl(1) = ltl(1) * (1.0 + pnois)
                       call rmul_ctoepdv( np, ltl, ltd, sa, wk1)  
                endif

!  Move to output array

       mdl(1:np,j) = sa(1:np)

!     print frequencies processed
            if (ipr.ne.0) then
                l        = mod(j-2,10) + 1
                freqs(l) = w*rhz
                if (l.eq.10) write (ipr,'(10f7.1)') freqs
            endif

        enddo

        if (ipr.ne.0 .and. l.ne.10)                 &
         write (ipr,'(10f7.1)') freqs(1:l)

        return
        end subroutine rmul_phaser
!
! Solve A x = y for the vector x.
! Uses hestenes and stiefel conjugate gradient algorithm for
! hermetian toeplitz complex matrix solution
! Original c code by John Anderson
! Modified and written in Fortran by Ted Shuck
!
!     niter : maximum number of iterations to allow
!     n     : dimension of problem (dimension of all arrays)
!     a     : complex conjugate of top row of matrix A
!     x     : output parameter vector
!     y     : input data vector (right hand side of A x = y)
!     s,ss,g,rr : work space all of dimension n
!     iter  : number of iterations actually performed
!
      subroutine rmul_ctoepcg( niter, n, a, x, y, s, ss, g, rr, iter)
      implicit none
      complex czero
      real   eps
      parameter ( eps=1.0e-6 )
      
!  Arguments
      integer :: iter, n
      complex :: a(n), x(n), y(n), s(n), ss(n), g(n), rr(n)
      
      integer :: j, niter
      double precision :: alpha, beta, gamma, gammam, rsq, rp, test
      double precision :: rcdot
      
      czero = cmplx(0.0,0.0)

      rp = DOT_PRODUCT(y, y)
      test = n*eps*eps*rp
 
!     take conjugate of a to be compatible with ctoepdv_MFS()

      do j =1,n
         a(j) = conjg(a(j))
      end do
     
      do 100 j=1,n
         x(j)  = czero
         rr(j) = y(j)
 100  continue

      call rmul_htmul( n, a, rr, g)
      
      do 200 j=1,n
         s(j) = g(j)
 200  continue

      gammam = DOT_PRODUCT(g, g)

      do 500 iter=1, niter
         call rmul_htmul( n, a, s, ss)
         rcdot = DOT_PRODUCT(ss, ss)
         alpha = gammam/rcdot
         do 300 j=1,n
            x(j)  = x(j) + s(j)*alpha
            rr(j) = rr(j) - ss(j)*alpha
 300     continue
!                                  close enough, return
!
         rsq = DOT_PRODUCT(rr, rr)
         if( iter.gt.1 .and. ( rsq.eq.rp .or. rsq.lt.test)) goto 999

         rp = rsq
         call rmul_htmul( n, a, rr, g )
         gamma = DOT_PRODUCT(g, g)
!                                  close enough, return
         if( gamma.lt.eps ) goto 999
         beta = gamma/gammam
         gammam = gamma
         
         do 400 j=1,n
            s(j) = g(j) + s(j) * beta
 400     continue
 500  continue
!                             return point
 999  continue
      return
      end subroutine rmul_ctoepcg

      subroutine rmul_ctoepdv ( m, cr, cg, cf, ca )
!
!     solves the complex hermetian toeplitz linear system of
!     A x = b
!     Where:  r = the conjugate of the top row of input matrix A
!             g = the data vector b
!             f = the output parameter vector x
!             a = work space
!     All vectors are of length m, complex
!
      implicit none

!     ... subroutine variables ...

!  Arguments
      integer  :: m
      complex  :: cr(m)  , cg(m)  , cf(m)  , ca(m)

!     ... local variables ...

      real      ::       r(m+m)  , g(m+m)  , f(m+m)  , a(m+m)
      integer   :: j       , n       , nh      , nplus1  
      integer   :: jr      , ji      , kr      , ki      
      integer   :: nr      , ni
      double precision  ::                                &
                betar   , betai   , kappar  , kappai  ,   &
                tempr   , tempi   , qr      , qi      ,   &
                alpha   , alphai  , gammar  , gammai


!     ... paramter definitions ...
      real      ::    fzero   , fhalf   , fone
      parameter ( fzero   = 0.0     ,                     &
                  fhalf   = 0.5     ,                     &
                  fone    = 1.0     )

      double precision :: fdzero  , fdone   
                                          
      parameter ( fdzero  = 0.0d0   ,                     &
                  fdone   = 1.0d0   )

      do n=1,m
        r(n*2-1)   = real(cr(n))
        r(n*2-1+1) = aimag(cr(n))
        g(n*2-1)   = real(cg(n))
        g(n*2-1+1) = aimag(cg(n))
      end do
  
      f = 0.0
      a = 0.0

      a(1)   = fone
      a(2)   = fzero
      alpha  = r(1)
      alphai = fdone / alpha
      kappar = - fdone
      kappai = fdzero
      f(1)   = dble(g(1)) * alphai
      f(2)   = dble(g(2)) * alphai

      do n = 2, m
         nplus1 = n + 1
         ni     = n + n
         nr     = ni - 1
         betar  = fdzero
         betai  = fdzero
         do j = 2, n
            ji   = j + j
            jr   = ji - 1
            kr   = ni - jr
            ki   = kr + 1

!           betac = betac + r*a

            betar = betar + dble(r(jr)) * dble(a(kr))     &
                          - dble(r(ji)) * dble(a(ki))
            betai = betai + dble(r(jr)) * dble(a(ki))     &
                          + dble(r(ji)) * dble(a(kr))
         end do

         kappar = - betar * alphai
         kappai = - betai * alphai
         alpha  = alpha + betar * kappar + betai * kappai
         alphai = fdone / alpha
!         nh     = fhalf * nplus1
         nh     = nplus1/2
         a(nr)  = fzero
         a(ni)  = fzero
         do j = 1, nh
            ji    = j + j
            jr    = ji - 1
            kr    = ni - jr
            ki    = kr + 1
            tempr = dble(a(kr)) + kappar * dble(a(jr))   &
                                + kappai * dble(a(ji))
            tempi = dble(a(ki)) - kappar * dble(a(ji))   &
                                + kappai * dble(a(jr))
            a(jr) = dble(a(jr)) + kappar * dble(a(kr))   &
                                + kappai * dble(a(ki))
            a(ji) = dble(a(ji)) - kappar * dble(a(ki))   &
                                + kappai * dble(a(kr))
            a(kr) = tempr
            a(ki) = tempi
         end do


         gammar = fdzero
         gammai = fdzero
         do j = 2, n
            ji   = j + j
            jr   = ji - 1
            kr   = ni - jr
            ki   = kr + 1
            gammar = gammar + dble(r(jr)) * dble(f(kr))  &
                            - dble(r(ji)) * dble(f(ki))
            gammai = gammai + dble(r(jr)) * dble(f(ki))  &
                            + dble(r(ji)) * dble(f(kr))
         end do

         qr    = ( dble(g(nr)) - gammar ) * alphai
         qi    = ( dble(g(ni)) - gammai ) * alphai
         f(nr) = fzero
         f(ni) = fzero
         do j = 1, n
            ji    = j + j
            jr    = ji - 1
            kr    = ni - jr
            ki    = kr + 1
            f(jr) = dble(f(jr)) + qr * dble(a(kr))      &
                                + qi * dble(a(ki))
            f(ji) = dble(f(ji)) - qr * dble(a(ki))      &
                                + qi * dble(a(kr))
         end do

      end do

      do n=1,m
         cf(n)  = cmplx( f(n*2-1),f(n*2-1+1) )   
      end do

      return
      end subroutine rmul_ctoepdv

!
!***********************************************************************
!
        subroutine rmul_radbal(ldimc,otr,np,nyq,nf,ibal)

!  apply frequency balancing operator for slant stack

!  ldimc - complex leading dimension of transform array
!  otr   - input/output transform array
!  nf    - number of frequencies to process
!  nyq   - nyquist sample
!  np    - number of p values in transform
!  ibal  - balancing flag, 2=apply abs(omega)
!                          1=apply sqrt(omega)
!                         -1=remove sqrt(omega)
!                         -2=remove abs(omega)
!

!  Arguments
        integer   :: ldimc, nf, nyq, np, ibal
        complex   :: otr(ldimc,nf)

!  Locals
        integer   :: i
        real      :: arg, omega

!  set operator period
        omega = 4.*atan(1.)/float(nyq-1)

!  apply abs operator
        if (ibal.eq.2) then
            do i=1,nf
                arg = sin(omega*float(i-1))
                otr(1:np,i) = arg * otr(1:np,i)
                enddo

        else if (ibal.eq.1) then
!     apply sqrt operator
            do i=1,nf
                arg = sqrt( sin(omega*float(i-1)) )
                otr(1:np,i) = arg * otr(1:np,i)
                enddo

        else if (ibal.eq.-1) then
!     remove sqrt operator
            do i=2,min(nf,nyq-1)
                arg = 1./sqrt( sin(omega*float(i-1)) )
                otr(1:np,i) = arg * otr(1:np,i)
                enddo

        else if (ibal.eq.-2) then
!     remove omega operator
            do i=2,min(nf,nyq-1)
                arg = 1./sin(omega*float(i-1))
                otr(1:np,i) = arg * otr(1:np,i)
                enddo

        endif

        return
        end subroutine rmul_radbal

!
!***********************************************************************
!
        subroutine rmul_radphs( ldimc, otr, np, nf, iphs )

!
!  apply phase correction factor for slant stacks

!  ldimc - leading dimension of complex transform array
!  otr   - input/output transform array
!  nf    - number of frequencies to process
!  np    - number of transform parameters
!  iphs  - phase application flag:
!           -2 = remove pi/4
!           -1 = remove pi/8
!            1 = apply  pi/8
!            2 = apply  pi/4

!  Argumants
        integer  ::  ldimc, nf, np, iphs
        complex  ::  otr(ldimc,nf), fact

!  Locals
        real      arg, pi4

!  set phase value
        pi4 = atan(1.)
        if (iphs.eq.-2) then
            arg = -pi4
        else if (iphs.eq.-1) then
            arg = -pi4/2.
        else if (iphs.eq.1) then
            arg = pi4/2.
        else if (iphs.eq.2) then
            arg = pi4
        else
            return
        endif


!  apply phase shift
        fact = cmplx( cos(arg), sin(arg) )
        otr(1:np, 1:nf) = fact * otr(1:np, 1:nf)
        
        return
        end subroutine rmul_radphs

!</execute_only>

  !!----------------------------- wrapup -----------------------------------!!
  !!----------------------------- wrapup -----------------------------------!!
  !!----------------------------- wrapup -----------------------------------!!


!<execute_only>

  subroutine rmul_wrapup (obj)
    !
    ! - Arguments
    !
    type(rmul_struct) :: obj       ! arguments
    !
    ! - Begin rmul_wrapup
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
    call pc_print (msg1 = 'RMUL: User cpu time = ',    &
                   var1 = obj%tcpu)
    !
    if (associated (obj%rcfft_obj))  call fft_delete (obj%rcfft_obj)
    if (associated (obj%crfft_obj))  call fft_delete (obj%crfft_obj)
    if (associated(obj%mute_head))   call mute_wrapup(obj%mute_head)
    if (associated(obj%mute_tail))   call mute_wrapup(obj%mute_tail)
    !
  end subroutine rmul_wrapup

!</execute_only>

  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!

 end module rmul_module

!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!
!!---------------------------------- end -----------------------------------!!

