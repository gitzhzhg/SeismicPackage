!<CPS_v1 type="PROCESS"/>
!!------------------------------- madc.f90 ---------------------------------!!
!!------------------------------- madc.f90 ---------------------------------!!
!!------------------------------- madc.f90 ---------------------------------!!

!<-- This documentation header was last revised by CI Burch on 2000-04-28. />

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
! Name       : MADC   (Multiple Attenuation by Downward Continuation)
! Category   : filters
! Written    : 1989-02-08   by: Bill Troutt  (Conseis version by John Reed)
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : 2D Multiple Attenuation by Downward Continuation.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Function
!
! MADC is designed to remove all orders of reverberation and peg-leg multiples
! in 2D data caused by a shallow dipping reflector (typically the water
! bottom).  The process does not exploit differences in velocities between
! primary and multiple events, so multiples with velocities comparable to the
! primary velocity and short offset data can be processed successfully.
!
!
! Algorithm
!
! MADC works by downward continuing the recorded wavefield forward in time to
! the specified boundary, then back to the surface. This operation generates a
! new profile in which the timing of each event should match the peg-leg
! multiple of that event in the original profile.  Wiener least-squares transfer
! functions are derived which convert each downward-continued trace to the same
! trace in the original profile.  These transfer functions represent the phase
! and amplitude of the multiple response.  For water bottom multiples, they are
! about 180 degrees.  After convolving the downward-continued traces with the
! transfer functions, the modeled multiple energy is subtracted from the
! original data and this profile is output.
!
!
! Modes of Operation
!
! MADC will operate in five different modes; the last four produce diagnostic
! output.
!
!       MODE                Operation
!
!       DEMULT  Output demultiple data.  This is the normal production mode.
!
!       MULT    Output predicted multiples.
!
!       ACORR   Output the autocorrelation of the downward continued traces.
!
!       CCORR   Output the crosscorrelation of the downward continued traces
!               with the input traces.
!
!       OPER    Output transfer function convolutional operators.
!
!
! One or Two Multiple Estimation Windows
!
! MADC allows the multiple predicting Weiner filter operation to be run with
! one time window or with two separate windows.  If two are used, multiples are
! estimated and subtracted twice (once for each window.  This is done after the
! expensive step of downward continuation, adding only a small incremental cost
! for the second window.
!
! Since reverberation and peg-leg multiples may have different responses, it
! should be possible to remove both types by using two windows.  Substantial
! improvement has been seen with two windows as compared with one.
!
!
! Reference
!
! Wiggins, J., W., 1988, Attenuation of Complex Water-Bottom Multiples by Wave-
! Equation-Based Prediction and Subtraction: Geophysics, 53, 1527-1539.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! One or Two Passes of MADC
!
! For nearly flat multiple generators, one pass of the program on shot profiles
! should be sufficient. If the multiple generator is structurally complex, it
! will be necessary to run the program twice, once on shot profiles and once on
! receiver profiles. This ensures that peg-leg multiples from both source and
! receiver contributions are removed.
!
!
! Pre-Processing for MADC
!
! MADC should be used early in the processing sequence and should be preceded
! by a deterministic expansion, such as TPOW or GDIV.  Data should be frequency
! filtered with a high-cut frequency of FREQ_MAX.  The head mute should be
! placed above the water bottom event.
!
! Data input to MADC must be either shot gathers with increasing receiver
! surveyed coordinate or in receiver gathers with increasing shot surveyed
! coordinate.  There are no special processing requirements after MADC has been
! run. If two passes of MADC are necessary, then it is recommended that each
! pass be a separate job (common shot MADC followed by common receiver MADC).
!
!
! Sample MADC Processing Sequence
!
!     Input shot profile data with headers applied
!
!     Apply any gun delays
!
!     Apply an initial MUTE to input data above the water bottom
!
!     Apply deterministic amplitude balance (such as TPOW, PWR = 2.0)
!
!     GATHER (shot profiles)
!
!     MADC - on shot gathers, HDR_LOC = 14  (continue normal processing here if
!            common receiver MADC is not necessary)
!
!     TSORT, HDR_PRI = 14,  HDR_SEC = 11
!
!     GATHER (receiver profiles)
!
!     MADC - on receiver gathers, HDR_LOC = 11
!
!     Continue normal processing sequence
!
!
! Suggestions for Making the Pickfile
!
! 1.  A pickfile containing times to the water bottom (or other multiple
! producing reflector) is used, together with the multiple velocity, to
! automatically fill in entries in the DEPTHS and COORS arrays which are used
! by the back-end process.  The pickfile is specified by PATHNAME_PICK and the
! multiple velocity by VEL_CONT; their use is optional.
!
! 2.  Migrate the near trace section and output to a byte file.  Use FKMIG and
! the water velocity for this step.  The migration step is necessary if the
! water bottom is not very flat.  MADC is based on image rays while the zero
! offset section is based on normal incidence rays.  If the data are not
! migrated the wrong water depth could be assigned to a particular shot or
! receiver location.  For very flat water bottoms, the migration step is not
! necessary.
!
! 3. In CBYT, pick the water bottom.
!
!       Select MUTE PICKING option.  (Set offset = 14 for common source MADC or
!       offset = 11 for common receiver MADC and inline bin = 0 for both cases.)
!       Use the option to interpolate/extrapolate fully.  Pick the dominant
!       water bottom lobe.  (Note that you are allowed to have a maximum of 200
!       picks.)
!
! If you are going to run two passes of MADC (source and receiver), then you
! must pick two separate files, one for sources and one for receivers.  These
! can be picked from the same near trace data, but the picking options must be
! different for the two files.
!
! The pickfile should be specified in the PATHNAME_PICK parameter.  VEL_MUTE
! should be set to the water velocity in the area, typically 1500 m/s.
!
! 4. To run common source MADC the receiver surveyed coordinate (header word
! 14) must be in increasing order.
!
! 5. To run common receiver MADC the source surveyed coordinate (header word
! 11) must be in increasing order.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a all-trace (loop-splitting) process.
!
! Process requires traces to be input in shot or receiver gathers.
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
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! GATHERED  whether traces are a legitimate gather  used but not changed
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
! 2       Head mute index            Used
! 11      Receiver surveyed coor     Used
! 14      Source surveyed coor       Used
! 64      Tail mute index            Used
!
! HDR_LOC surveyed coordinate for traces within input profile
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author       Description
!     ----       ------       -----------
! 25. 2006-09-11 Stoeckley    Add call to pc_register_array_names for SeisSpace.
!024. 2006-01-10  B. Menger   Removed Unused Variables.
! 23. 2001-12-10 Selzler     Change "(*)" arrays to "(:)" for Intel compiler
! 22. 2001-01-29 Selzler     Work around Absoft compiler bug.
! 21. 2000-12-14 Selzler     Conversion to f90 and new cps.
! 20. 1999-05-19 Wuenscher   Documentation change only.
! 19. 1998-11-10 Vunderink   Begin using the f90 compiler.
! 18. 1997-07-02 Vunderink   Added CARDS parameter and 2nd DCODE card
!                            to correct problem of more than 200 cards
!                            in one DCODE.
! 17. 1997-03-19 Vunderink   Increased memory used by X2KC! and T2FRC
!                            primitives
! 16. 1997-02-18 Vunderink   Change check for dead trace from HW(2) to
!                            ISAMAX. Also added CPU timing.
! 15. 1995-05-04 Troutt      Increase array size XS,ZS 100 to 200.
! 14. 1992-04-02 Troutt      Correct User Doc (no code changed here - just
!                            the CFE) regarding OPRL and LAG.  It is LAG
!                            that must be less than minimum ZS, not OPRL.
! 13. 1992-03-19 Troutt      Add tail mute restore (call MUTEHW).  The mute
!                            restoration here reapplies a 60-mil taper.
!                            This code has been changed to match MUTE's
!                            algorithm.
! 12. 1991-09-18  Troutt     Check for dead traces in DO 19900 and skip to
!                            end of loop. This is really only needed when
!                            whole group is dead which causes OPFILT to
!                            blow up on a zero autocorrelation.
! 11. 1990-04-25  Howard     Make compatible with STRIN change.
! 10. 1990-03-14  Troutt     Add logic for only 1 trace in 1st group(s).
!                            The 1st group(s) is(are) deleted. This could
!                            most likely happen for common receiver
!                            gathers in a taper zone. The program is
!                            unable to compute DX until it has at least
!                            a 2-trace group.
! 9.  1989-12-13  Troutt     Fix formatted writes of TMAX and FMAX for
!                            DCODE
! 8.  1989-09-20  Troutt     Moved code for applying taper to ends of oper-
!                            ator so that MODE=OPER shows taper.
!                            Changed logic to use storage for working
!                            trace (and hdr) arrays instead of using the
!                            2ndary arrays in the MAD! call. 2ndary arrays
!                            were renamed to HDR2 and TR2 and are used to
!                            output one trace at a time via an internal
!                            call to GATHR.  All of the main logic for the
!                            old 2ndary arrays (HDR1, TR1) remains intact,
!                            except that they now reside in storage via
!                            pointers.
! 7.  1989-05-02  Troutt     Corrected value of NPARM to 268.
! 6.  1989-03-20  Troutt     Removed all references to scratch array SCR4.
! 5.  1989-03-09  Troutt     Fixed indexing bug in clearing D! frequency.
!                            Also corrected call to REPP to reference VEL
!                            instead of NTRT.
! 4.  1989-03-02  Troutt     Turned off debug printout and finished
!                            documentation. Ready for user checkout.
! 3.  1989-02-27  Troutt     Checkout of all modes of output finished.
!                            Implemented parameters OFFMAX and HZ#.
! 2.  1989-02-17  Troutt     Checkout of MODE=TRACE finished. Began adding
!                            additional options.  Use STROT,STRINI to
!                            handle unaltered input traces.
! 1.  1989-02-08  Troutt     Began original "conversion" (redesign) from
!                            CONSEIS. Used FKAP (CPS) for starting code.
!                            Began with MODE=TRACE only.
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
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES         n/a       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        n/a       whether this process needs two trace/header arrays.
! NSCRATCH         ?       amount of temporary memory needed.
! NSTORE           ?       amount of permanent memory needed.
! IFTD           n/a       whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     n/a       whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >  1              means to process the input traces.
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
!<NS MADC Process/NC=80>
!
!           Multiple Attenuation by Downward Continuation
!
!  VEL_CONT=`FFFFFF  FREQ_MAX=`FFFF  DEPTH_INC=`FFFFFFF  OFF_MAX=`FFFFFFF
!
!  HDR_LOC=~`III     NUM_CHANNELS=`IIIII
!
!  MODE=~~~~`CCCCC   LEN_OP=~~~~~~`FFFF    LAG=`FFFFFF   DIAG_LOAD=`FFFFF
!
!  WIN_LEN1=`FFFFFF  WIN_BEG1=`FFFFF   VEL_WIN1=`FFFFF
!  WIN_LEN2=`FFFFFF  WIN_BEG2=`FFFFF   VEL_WIN2=`FFFFF
!
!  `-------------------------------
!  DEPTHS   COORS
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `FFFFFFFF`FFFFFFFFF
!  `-------------------------------
!
!  CALC_DEPTHS `P
!  PATHNAME_PICK=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!<PARMS PATHNAME_PICK[/ML=128/XST]>
!<PARMS DEPTHS_ARRAYSET[/XST/YST]>
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Mode of MADC operation (type of output traces). </Tip>
! Default = DEMULT
! Allowed = DEMULT  (Output demultiple data - normal production mode.)
! Allowed = MULT    (Output predicted multiples.)
! Allowed = ACORR   (Output autocorrelation of downward continued traces.)
! Allowed = CCORR   (Crosscorrelation of continued traces with input traces.)
! Allowed = OPER    (Output transfer function operators.)
! DEMULT mode is the normal production mode.  The other modes produce diagnostic
! output.
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency for MADC to process. </Tip>
! Default = 0.5 * Nyquist
! Allowed = Nyquist > real > 0.0
! Do not set FREQ_MAX higher than necessary as runtime is proportional to
! FREQ_MAX.
!</Help>
!
!<Help KEYWORD="NUM_CHANNELS">
!<Tip> Maximum number of traces in an input (shot or receiver) profile. </Tip>
! Default = 32
! Allowed = int > 1
! NUM_CHANNELS is used to set the size of the spatial FFT.  If an input shot or
! receiver profile is found with more traces than NUM_CHANNELS, MADC will abort.
!</Help>
!
!<Help KEYWORD="DEPTH_INC">
!<Tip> Depth increment for the downward continuation. </Tip>
! Default = 10.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="CALC_DEPTHS">
!<Tip> Recalculate DEPTHS and COORS from PATHNAME_PICK and VEL_CONT.</Tip>
! Default = -
!
! PATHNAME_PICK is a front-end parameter only.  Its used to
! automatically fill in entries in the DEPTHS and COORS arrays which are used
! by the back-end process.  Use of PATHNAME_PICK is optional.
!</Help>
!
!<Help KEYWORD="PATHNAME_PICK">
!<Tip> Pathname for the lower boundary pickfile. </Tip>
! Default = NONE
! Allowed = char
! PATHNAME_PICK is a front-end parameter only.  Its used to
! automatically fill in entries in the DEPTHS and COORS arrays which are used
! by the back-end process.  Use of PATHNAME_PICK is optional.
!</Help>
!
!<Help KEYWORD="VEL_CONT">
!<Tip> Velocity to use for downward continuation and reflector boundary. </Tip>
! Default = 1500
! Allowed = real > 0.0
! VEL_CONT is the RMS velocity from the surface to the multiple generating
! reflector.
!</Help>
!
!<Help KEYWORD="DEPTHS">
!<Tip> Array of depths from the surface to the lower boundary. </Tip>
! Default = -
! Allowed = real > 0.0 (array)
! DEPTHS is an array (maximum of 200 entries) of depths from the surface to the
! lower boundary corresponding to entries in COORS.  DEPTHS entries should have
! units of feet or meters.
!</Help>
!
!<Help KEYWORD="COORS">
!<Tip> Array of coordinates corresponding to entries in DEPTHS. </Tip>
! Default = -
! Allowed = real > 0.0 (array)
! COORS is an array (maximum of 200 entries) of values of HDR_LOC corresponding
! to entries in DEPTHS.  COORS entries must be increasing.
!</Help>
!
!<Help KEYWORD="HDR_LOC">
!<Tip> Header word for surveyed coordinate of traces in input profile. </Tip>
! Default = 14
! Allowed = 11, 14
! HDR_LOC is the header word for surveyed coordinate corresponding to the
! entries in COORS.  If input is shot profiles, HDR_LOC should be set to 14
! (receiver inline surveyed coordinate).  If input is receiver profiles,
! HDR_LOC should be set to 11 (source inline surveyed coordinate).
!</Help>
!
!<Help KEYWORD="LEN_OP">
!<Tip> Operator length, in seconds, for Weiner filter transfer function. </Tip>
! Default = 0.2
! Allowed = real > LAG and LEN_OP < 1.0
! Operator length, in seconds, for the least squares transfer function
! calculation (Weiner filter).  LEN_OP should be long enough to include complex
! water bottom events.
!</Help>
!
!<Help KEYWORD="LAG">
!<Tip> Operator lag, in seconds. </Tip>
! Default = 0.04
! Allowed = real >= 0.0 and < LEN_OP
!                      [we need help understanding this one]
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent, for the Wiener filter calculation. </Tip>
! Default = 10.0
! Allowed = real > 0.0 and < 100.0
! Normally DIAG_LOAD should be 10% or greater.
!</Help>
!
!<Help KEYWORD="OFF_MAX">
!<Tip> Maximum offset in the input profiles. </Tip>
! Default = 100.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="WIN_LEN1">
!<Tip> Length of the first window for transfer function calculation. </Tip>
! Default = trace length
! Allowed = real > 0.0 and <= end time
! Length, in seconds, of the first window for transfer function calculation.
!</Help>
!
!<Help KEYWORD="WIN_LEN2">
!<Tip> Length of the second window for transfer function calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! Length, in seconds, of the second window for transfer function calculation.
! If WIN_BEG2 = 0.0, then the second window (hence WIN_LEN2) is not used.
!</Help>
!
!<Help KEYWORD="WIN_BEG1">
!<Tip> Top of the first window for transfer function calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! Beginning time, in seconds, of the first window for transfer function
! calculation.  If WIN_BEG1 = 0.0, then use the the water bottom time from the
! boundary information.
!</Help>
!
!<Help KEYWORD="WIN_BEG2">
!<Tip> Top of the second window for transfer function calculation. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! Beginning time, in seconds, of the second window for transfer function
! calculation.
! If WIN_BEG2 = 0.0, then the second window is not used.
!</Help>
!
!<Help KEYWORD="VEL_WIN1">
!<Tip> Moveout velocity for the first window. </Tip>
! Default = 2000.0
! Allowed = real > 0.0
! VEL_WIN1 is used to determine the top of the first window as a function of
! offset.
!</Help>
!
!<Help KEYWORD="VEL_WIN2">
!<Tip> Moveout velocity for the second window. </Tip>
! Default = 2000.0
! Allowed = real > 0.0
! VEL_WIN2 is used to determine the top of the second window as a function of
! offset.
! If WIN_BEG2 = 0.0, then the second window (hence VEL_WIN2) is not used.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! NOTES FOR CONVERSION PROGRAMMER
!
! 1.  WHAT DOES THE LAG PARAMETER DO??
!
! 2.  Spatial FFT size should be determined from NUM_CHANNELS.  Probably it
! should be the next possible FFT size larger than 2.0*NUM_CHANNELS.
! We may have to experiment with the factor.

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module madc_module
      use pc_module
      use named_constants_module
      use mem_module
      use lav_module
      use mth_module
      use fltr_module
      use muteio_module
      use pathcheck_module
      use opfilt_module
      use mutehw_module
      use fft_module
      implicit none
      private
      public :: madc_create
      public :: madc_initialize
      public :: madc_update
      public :: madc_delete
!<execute_only>
      public :: madc            ! main execution (trace processing) routine.
      public :: madc_wrapup
!</execute_only>

      character(len=100),public,save :: madc_IDENT = &
'$Id: madc.f90,v 1.25 2006/09/11 13:15:47 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer, parameter :: MAX_PICK = 200

      type,public :: madc_struct
        private
        logical                    :: skip_wrapup       ! wrapup flag.

        character(len=6)           :: mode             ! process parameter
        real                       :: freq_max         ! process parameter
        integer                    :: num_channels     ! process parameter
        real                       :: depth_inc        ! process parameter
        character(len=FILENAME_LENGTH) :: pathname_pick ! process parameter
        real                       :: vel_cont         ! process parameter
        real, dimension(MAX_PICK)  :: depths           ! process parameter
        real, dimension(MAX_PICK)  :: coors            ! process parameter
        integer                    :: hdr_loc          ! process parameter
        real                       :: len_op           ! process parameter
        real                       :: lag              ! process parameter
        real                       :: diag_load        ! process parameter
        real                       :: off_max          ! process parameter
        real                       :: win_len1         ! process parameter
        real                       :: win_len2         ! process parameter
        real                       :: win_beg1         ! process parameter
        real                       :: win_beg2         ! process parameter
        real                       :: vel_win1         ! process parameter
        real                       :: vel_win2         ! process parameter

        integer                    :: ndpt             ! global parameter
        integer                    :: nwih             ! global parameter
        real                       :: dt               ! global parameter
        real                       :: tstrt            ! global parameter

        integer                     :: depths_cnt      ! dependent parameter

        integer                     :: nt_fft          ! real fft size in time
        integer                     :: nt_fft_p2       ! nt_fft + 2
        integer                     :: nt_fft_d2p1     ! nt_fft/2 + 1
        integer                     :: ntx_fft         ! band limited fft size
        integer                     :: nx_fft          ! fft size in space
        integer                     :: len_op_cnt      ! len_opt pt count
        integer                     :: num_gather      ! number of gathers
        integer                     :: taper_pt_cnt    ! cosine taper pnt count
        integer                     :: win_len1_cnt    ! win_len1 / dt
        integer                     :: win_len2_cnt    ! win_len2 / dt
        integer                     :: lag_sample_cnt  ! lag / dt
        real                        :: off_max_p100    ! off_max + 100
        real                        :: diag_fact       ! 1 + diag_load / 100
        real                        :: fft_nt_scale    ! 1.0 / nt_fft
        real                        :: fft_nx_scale    ! 1.0 / nx_fft
        real                        :: dw_freq_inc     ! fft freq increment
        real                        :: dx_space_inc    ! fft freq increment
        real                        :: dp              ! wave z-x ratio
        real, dimension(:), pointer :: taper           ! cosine taper weights
        real, dimension(:,:), pointer :: ezb           ! spline coefficients
                                                       ! D1 = depths_cnt
                                                       ! D2 = 5
                                                       !    1 = coors copy
                                                       !    2 = depths copy
                                                       !    3 = Z1
                                                       !    4 = Z2
                                                       !    5 = Z3
                                                       ! coors and depths array
                                                       ! are destroyed after
                                                       ! calling madc_spline
                                                       ! which initializes ezb.
        real, dimension(:), pointer :: ez2             ! group boundary value
                                                       ! D1 = num_channels
        real, dimension(:), pointer :: auto_corr       ! auto correlation
                                                       ! D1 = len_op_cnt
        real, dimension(:), pointer :: cross_corr      ! cross correlation
                                                       ! D1 = len_op_cnt
        real, dimension(:), pointer :: oper_filt       ! operator filter
                                                       ! D1 = len_op_cnt
        real, dimension(:), pointer :: conv_scr        ! convolution scratch
                                                       ! D1 = ndpt
        complex, dimension(:), pointer :: scr1         ! scratch 1
                                                       ! D1 = nx_fft
        complex, dimension(:), pointer :: scr2         ! scratch 2
                                                       ! D1 = nx_fft
        complex, dimension(:), pointer :: scr3         ! scratch 3
                                                       ! D1 = nx_fft
        complex, dimension(:), pointer :: er           ! sin & cos array ?
                                                       ! D1 = nx_fft
        complex, dimension(:), pointer :: ern          ! sin & cos array ?
                                                       ! D1 = nx_fft
        real, dimension(:,:), pointer :: tr_save       ! copy of input gather
                                                       ! D1 = ndpt
                                                       ! D2 = num_channels
        real, dimension(:), pointer :: rfft_nt         ! real 1D fft buffer
                                                       ! D1 = nt_fft_p2
        complex, dimension(:), pointer :: cfft_nt      ! complex 1D fft buffer
                                                       ! D1 = nt_fft_d2p1
        complex, dimension(:,:), pointer :: cfft_2d    ! complex 2D fft buffer
                                                       ! D1 = nx_fft
                                                       ! D2 = ntx_fft

        type(muteio_struct),pointer :: muteio

        type(fft_struct),pointer :: fft_nt_forward
        type(fft_struct),pointer :: fft_nt_inverse
        type(fft_struct),pointer :: fft_nx_forward
        type(fft_struct),pointer :: fft_nx_inverse
      end type madc_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(madc_struct),pointer,save :: object      ! needed for traps.

      character(len=6),dimension(5),parameter :: mode_options = &
        (/'DEMULT','MULT  ','ACORR ','CCORR ','OPER  '/)

      character(len=2),dimension(2),parameter :: hdr_loc_options = &
        (/'11','14'/)

      ! need to experiment with this factor to reduce aliasing
      ! greater than or equal to 1.0 is required
      real, parameter :: FFT_CHAN_FACTOR = 1.5
      real, parameter :: FFT_TIME_FACTOR = 1.0

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine madc_create (obj)
      implicit none
      type(madc_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%taper)
      nullify(obj%ezb)
      nullify(obj%ez2)
      nullify(obj%auto_corr)
      nullify(obj%cross_corr)
      nullify(obj%oper_filt)
      nullify(obj%conv_scr)
      nullify(obj%scr1)
      nullify(obj%scr2)
      nullify(obj%scr3)
      nullify(obj%er)
      nullify(obj%ern)
      nullify(obj%tr_save)
      nullify(obj%rfft_nt)
      nullify(obj%cfft_nt)
      nullify(obj%cfft_2d)
      nullify(obj%muteio)
      nullify(obj%fft_nt_forward)
      nullify(obj%fft_nt_inverse)
      nullify(obj%fft_nx_forward)
      nullify(obj%fft_nx_inverse)

      call madc_initialize (obj)

      return
      end subroutine madc_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine madc_delete (obj)
      implicit none
      type(madc_struct),pointer :: obj       ! arguments

!<execute_only>
      call madc_wrapup (obj)
!</execute_only>

      call mem_free(obj%taper)
      call mem_free(obj%ezb)
      call mem_free(obj%ez2)
      call mem_free(obj%auto_corr)
      call mem_free(obj%cross_corr)
      call mem_free(obj%oper_filt)
      call mem_free(obj%conv_scr)
      call mem_free(obj%scr1)
      call mem_free(obj%scr2)
      call mem_free(obj%scr3)
      if(associated(obj%er )) deallocate (obj%er)
      if(associated(obj%ern)) deallocate (obj%ern)
      call mem_free(obj%tr_save)
      call mem_free(obj%rfft_nt)
      call mem_free(obj%cfft_nt)
      call mem_free(obj%cfft_2d)

      if (associated(obj%muteio)) call muteio_close (obj%muteio)
      if (associated(obj%fft_nt_forward)) call fft_delete (obj%fft_nt_forward)
      if (associated(obj%fft_nt_inverse)) call fft_delete (obj%fft_nt_inverse)
      if (associated(obj%fft_nx_forward)) call fft_delete (obj%fft_nx_forward)
      if (associated(obj%fft_nx_inverse)) call fft_delete (obj%fft_nx_inverse)

      deallocate(obj)

      return
      end subroutine madc_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine madc_initialize (obj)
      implicit none
      type(madc_struct),intent(inout) :: obj       ! arguments

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      obj%mode = 'DEMULT'
      obj%freq_max = 0.25 / obj%dt
      obj%num_channels = 32
      obj%depth_inc = 10.0
      obj%pathname_pick = PATHCHECK_EMPTY
      obj%vel_cont = 1500.0
      obj%depths = 0.0 
      obj%coors = 0.0 
      obj%hdr_loc = 14 
      obj%len_op = 0.2 
      obj%lag = 0.04 
      obj%diag_load = 10.0 
      obj%off_max = 100.0 
      obj%win_len1 = (obj%ndpt - 1) * obj%dt
      obj%win_len2 = 0.0 
      obj%win_beg1 = 0.0 
      obj%win_beg2 = 0.0 
      obj%vel_win1 = 2000.0 
      obj%vel_win2 = 2000.0 

      obj%depths_cnt = 0

      obj%nt_fft = 0
      obj%nt_fft_p2 = 0
      obj%nt_fft_d2p1 = 0
      obj%ntx_fft = 0
      obj%nx_fft = 0
      obj%len_op_cnt = 0
      obj%num_gather = 0
      obj%taper_pt_cnt = 0
      obj%win_len1_cnt = 0
      obj%win_len2_cnt = 0
      obj%lag_sample_cnt = 0
      obj%off_max_p100 = obj%off_max + 100.0 
      obj%diag_fact = 0.0
      obj%fft_nt_scale = 0.0
      obj%fft_nx_scale = 0.0
      obj%dw_freq_inc = 0.0
      obj%dx_space_inc = 0.0

      call madc_update (obj)

      return
      end subroutine madc_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine madc_update (obj)
      implicit none
      type(madc_struct),intent(inout),target :: obj             ! arguments

      integer :: state
      logical :: verify
      logical :: gathered
      real :: end_time, taper_length, taper_factor, zmin
      integer :: depths_cnt2, status, ier1
      integer :: depths_do, taper_do
      integer :: nho, nhx, nhy
      character :: msg*120
      real offset, xcoord, ycoord, time

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

      call pc_register_array_names ("depths_arrayset", (/  &
                                    "depths",              &
                                    "coors " /))

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)
      call pc_get_global('gathered', gathered)

      end_time = (obj%ndpt - 1) * obj%dt

      call pc_get ('MODE', obj%mode)
      call string_to_upper (obj%mode)

      call pc_get ('FREQ_MAX', obj%freq_max)
      call pc_get ('NUM_CHANNELS', obj%num_channels)
      call pc_get ('DEPTH_INC', obj%depth_inc)
      call pc_get ('PATHNAME_PICK', obj%pathname_pick)
      call pc_get ('VEL_CONT', obj%vel_cont)

      depths_cnt2 = obj%depths_cnt
      call pc_get ('DEPTHS', obj%depths, obj%depths_cnt)
      call pc_get ('COORS', obj%coors, depths_cnt2)

      call pc_get ('HDR_LOC', obj%hdr_loc)
      call pc_get ('LEN_OP', obj%len_op)
      call pc_get ('LAG', obj%lag)
      call pc_get ('DIAG_LOAD', obj%diag_load)
      call pc_get ('OFF_MAX', obj%off_max)
      call pc_get ('WIN_LEN1', obj%win_len1)
      call pc_get ('WIN_LEN2', obj%win_len2)
      call pc_get ('WIN_BEG1', obj%win_beg1)
      call pc_get ('WIN_BEG2', obj%win_beg2)
      call pc_get ('VEL_WIN1', obj%vel_win1)
      call pc_get ('VEL_WIN2', obj%vel_win2)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(.not.gathered) then
        call pc_error('This process must be preceded by a gather.')
      end if

      if(all(mode_options /= obj%mode)) then
        call pc_error( &
          'Invalid MODE value. Valid values are DEMULT, MULT, ACORR, ' // &
          'CCORR and OPER')
        obj%mode = 'DEMULT'
      end if

      if(obj%freq_max <= 0.0 .or. obj%freq_max >= 0.5 / obj%dt) then
        call pc_error('Invalid FREQ_MAX.  Must be > 0 and < NYQUEST, i.e. ', &
        0.5 / obj%dt)

        obj%freq_max = 0.25 / obj%dt
      end if

      if(obj%num_channels <= 1) then
        call pc_error('Invalid NUM_CHANNELS.  Must be > 1')

        obj%num_channels = 32
      end if

      if(obj%depth_inc <= 0.0) then
        call pc_error('Invalid DEPTH_INC.  Must be > 0.0')

        obj%depth_inc = 10.0
      end if

      if(obj%vel_cont <= 0.0) then
        call pc_error('Invalid VEL_CONT.  Must be > 0.0')

        obj%vel_cont = 1500.0
      end if

      if(pc_pressed("calc_depths")) then
        call pathcheck('pathname_pick', obj%pathname_pick, ext='.mute', &
          required=.false., status=status)

        if(status == PATHCHECK_VALID) then
          ! update depths and coors, given pathname_pick and vel_cont
          call muteio_open_read(obj%muteio, obj%pathname_pick, ier1, msg, &
            nho, nhx, nhy)

          if(ier1 == MUTEIO_OK) then
            obj%depths_cnt = 0
            obj%coors = 0.0
            obj%depths = 0.0

            do while(.true.)
              call muteio_read_card(obj%muteio, offset, &
                xcoord, ycoord, time, ier1, msg)

              if(ier1 == MUTEIO_OK .and. obj%depths_cnt < MAX_PICK) then
                obj%depths_cnt = obj%depths_cnt + 1
                obj%coors(obj%depths_cnt) = offset
                obj%depths(obj%depths_cnt) = 0.5 * time * obj%vel_cont
              else
                if(obj%depths_cnt >= MAX_PICK) then
                  call pc_error( &
                  "MADC: muteio_read_card: card count exceeds max ", MAX_PICK)
                else if(ier1 /= MUTEIO_EOF) then
                  call pc_error("MADC: muteio_read_card: " // msg)
                end if

                exit
              end if
            end do
          else
            call pc_error("MADC: muteio_open_read: " // msg)
          end if
        else
          call pc_error("MADC: invalid, PATHNAME_PICK= ", obj%pathname_pick)
        end if
      end if

      if(verify .and. obj%depths_cnt < 2) then
        call pc_error("Must have at least 2 entries in DEPTHS and COORS.")
      else if(obj%depths_cnt == depths_cnt2) then
        if(verify .or. pc_verify_arrayset("depths_arrayset") .or. &
          pc_pressed("calc_depths")) then
          do depths_do = 1, obj%depths_cnt
            if(obj%depths(depths_do) <= 0.0) then
              call pc_error("Invalid DEPTHS (non-positive), index= ", depths_do)
            end if

            if(depths_do > 1) then
              if(obj%coors(depths_do) <= obj%coors(depths_do - 1)) then
                call pc_error("Entries in COORS not increasing, index=", &
                  depths_do)
              end if
            end if
          end do
        end if
      else
        call pc_error("DEPTHS and COORS arrays not linked (count differs)")
        call pc_error("depths_cnt=",obj%depths_cnt,", cnt2=",depths_cnt2)
      end if

      if(obj%hdr_loc /= 11 .and. obj%hdr_loc /= 14) then
        call pc_error("Invalid HDR_LOC.  Must be 11 or 14.")

        obj%hdr_loc = 14
      end if

      obj%lag_sample_cnt = obj%lag / obj%dt
      obj%len_op_cnt = NINT(obj%len_op/obj%dt)

      if(obj%len_op <= 0.0 .or.  obj%len_op >= 1.0 .or. &
        obj%len_op_cnt <= obj%lag_sample_cnt) then
        call pc_error("Invalid LEN_OP.  Must be > (LAG / 1000) and < 1.0 ")

        obj%len_op = 0.2
        obj%len_op_cnt = NINT(obj%len_op/obj%dt)
      end if

      if(obj%lag < 0.0 .or. obj%len_op_cnt <= obj%lag_sample_cnt) then
        call pc_error("Invalid LAG.  Must be >= 0.0 and < 1000 * LEN_OP.")

        obj%lag = min(0.04, obj%dt * (obj%len_op_cnt - 1))
        obj%lag_sample_cnt = obj%lag / obj%dt
      end if

      if(obj%diag_load <= 0.0 .or. obj%diag_load >= 100.0) then
        call pc_error("Invalid DIAG_LOAD.  Must be > 0.0 and < 100.0.")

        obj%diag_load = 10.0
      end if

      if(obj%off_max <= 0.0) then
        call pc_error("Invalid OFF_MAX.  Must be greater than 0.0.")

        obj%off_max = 100.0
      end if

      if(obj%win_len1 <= 0.0 .or. obj%win_len1 > end_time) then
        call pc_error("Invalid WIN_LEN1.  Must be > 0.0 and <= end_time.")

        obj%win_len1 = end_time
      end if

      if(obj%win_beg1 < 0.0) then
        call pc_error("Invalid WIN_BEG1.  Must be >= 0.0.")

        obj%win_beg1 = 0.0
      end if

      if(obj%vel_win1 <= 0.0) then
        call pc_error("Invalid VEL_WIN1.  Must be > 0.0.")

        obj%vel_win1 = 2000.0
      end if

      if(obj%win_beg2 .lt. 0.0 .or. obj%win_beg2 > end_time) then
        call pc_error("Invalid WIN_BEG2.  Must be >= 0.0 and < end time.")

        obj%win_beg2 = 0.0
      else if(obj%win_beg2 .gt. 0.0) then
        ! second window is processed and must be validated.
        if(obj%win_len2 <= 0.0 .or. obj%win_len2 > end_time) then
          call pc_error("Invalid WIN_LEN2.  Must be > 0.0 and <= end_time.")

          obj%win_len2 = 0.0
        end if

        if(obj%vel_win2 <= 0.0) then
          call pc_error("Invalid VEL_WIN2.  Must be > 0.0.")

          obj%vel_win2 = 2000.0
        end if
      end if

      ! Check operator lag versus minimum water depth.
      zmin = obj%depths(1)

      do depths_do = 2, obj%depths_cnt
        zmin = min(zmin,obj%depths(depths_do))
      end do

      ! Convert to min 2-way water time (sec).
      zmin = 2.0 * zmin / obj%vel_cont

      if(obj%lag >= zmin) then
        call pc_error('LAG is not less than minimum water time')
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('mode', mode_options, 5)
      call pc_put ('MODE', obj%mode)

      call pc_put ('FREQ_MAX', obj%freq_max)
      call pc_put ('NUM_CHANNELS', obj%num_channels)
      call pc_put ('DEPTH_INC', obj%depth_inc)
      call pc_put ('PATHNAME_PICK', obj%pathname_pick)
      call pc_put ('VEL_CONT', obj%vel_cont)
      call pc_put ('DEPTHS', obj%depths, obj%depths_cnt)
      call pc_put ('COORS', obj%coors, obj%depths_cnt)

      call pc_put_options_field('hdr_loc', hdr_loc_options, 2)
      call pc_put ('HDR_LOC', obj%hdr_loc)

      call pc_put ('LEN_OP', obj%len_op)
      call pc_put ('LAG', obj%lag)
      call pc_put ('DIAG_LOAD', obj%diag_load)
      call pc_put ('OFF_MAX', obj%off_max)
      call pc_put ('WIN_LEN1', obj%win_len1)
      call pc_put ('WIN_LEN2', obj%win_len2)
      call pc_put ('WIN_BEG1', obj%win_beg1)
      call pc_put ('WIN_BEG2', obj%win_beg2)
      call pc_put ('VEL_WIN1', obj%vel_win1)
      call pc_put ('VEL_WIN2', obj%vel_win2)

      call pc_put_control('need_request', .true.)
      call pc_put_control('need_label', .true.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%nt_fft  = fft_nfctr(nint(FFT_TIME_FACTOR * obj%ndpt))
      obj%nt_fft_p2  = obj%nt_fft + 2
      obj%nt_fft_d2p1  = (obj%nt_fft / 2) + 1

      ! Note: rationale for ntx_fft algorithm...
      !   Nyquest for time to freq tranform is 1.0 / (2 * dt)
      !   Nyquest corresponds to complex element named by
      !     cfft_nt(nt_fft_d2p1).
      !   FREQ_MAX corresponds to complex element named by
      !     cfft_nt(nint((FREQ_MAX / Nyquest) * nt_fft_d2p1))
      !   Therefore ntx_fft = nint(FREQ_MAX * dt * 2 * nt_fft_d2p1)
      !   The old CPS code used a count that is effectively one less.
      obj%ntx_fft = nint(obj%freq_max * obj%dt * 2 * obj%nt_fft_d2p1)

      obj%nx_fft  = fft_nfctr(nint(FFT_CHAN_FACTOR * obj%num_channels))

      ! debug aid
      ! print*,'nt_fft, nt_fft_d2p1, ntx_fft, nx_fft=', &
      ! obj%nt_fft,obj%nt_fft_d2p1,obj%ntx_fft,obj%nx_fft

      taper_length = 0.06
      obj%taper_pt_cnt = taper_length / obj%dt - 0.000001
      taper_factor = PI * obj%dt / (2.0 * taper_length)

      obj%win_len1_cnt = obj%win_len1 / obj%dt
      obj%win_len2_cnt = obj%win_len2 / obj%dt

      obj%off_max_p100 = obj%off_max + 100.0

      obj%diag_fact = 1.0 + obj%diag_load / 100.0

      obj%fft_nt_scale = 1.0 / obj%nt_fft
      obj%fft_nx_scale = 1.0 / obj%nx_fft

      obj%dw_freq_inc = 4. * obj%depth_inc / &
        (obj%nt_fft * obj%dt * obj%vel_cont)

      call mem_alloc(obj%taper, obj%taper_pt_cnt)

      DO taper_do = 1, obj%taper_pt_cnt
        obj%taper(taper_do) = SIN(taper_factor*taper_do)**2
      END DO

      call mem_alloc(obj%ezb, obj%depths_cnt, 5)

      obj%ezb(:,1) = obj%coors(:obj%depths_cnt)
      obj%ezb(:,2) = obj%depths(:obj%depths_cnt)

      ! Note: coors and depths are used for scratch in this call
      ! and their contents are now destroyed.
!??? 2000-12-14 SELZLER, Solaris Workshop hit an RUI
!??? for the last element overruns an input array
!??? within madc_spline, when "I" == NPTS, then IP == NPTS+1 and X(IP) burps
!??? This may result in one or more bad points in the output array.
!??? The erroneous output value(s) may or may not actually be used.
!??? After inspecting the code, I don't think the bad value is acually used.
      call madc_spline(obj%ezb(:,2), obj%ezb(:,1), obj%depths_cnt, &
        obj%ezb(:,3), obj%ezb(:,4), obj%ezb(:,5), &
        obj%coors(:obj%depths_cnt-1), obj%depths(:obj%depths_cnt-1))

      call mem_alloc(obj%ez2, obj%num_channels)

      call mem_alloc(obj%auto_corr,  obj%len_op_cnt)
      call mem_alloc(obj%cross_corr, obj%len_op_cnt)
      call mem_alloc(obj%oper_filt,  obj%len_op_cnt)

      call mem_alloc(obj%conv_scr, obj%ndpt)

      if(obj%mode /= 'MULT' .and. obj%mode /= 'ACORR') then
        ! need to save a copy of input traces
        call mem_alloc(obj%tr_save, obj%ndpt, obj%num_channels)
      end if

      allocate(obj%rfft_nt(obj%nt_fft_p2))
      allocate(obj%cfft_nt(obj%nt_fft_d2p1))
      allocate(obj%cfft_2d(obj%nx_fft,obj%ntx_fft))

      ier1 = fft_create(obj%fft_nt_forward, -1, obj%nt_fft, 'rtoc')

      ier1 = fft_create(obj%fft_nt_inverse, +1, obj%nt_fft, 'ctor')

      ier1 = fft_create(obj%fft_nx_forward, -1, obj%nx_fft, 'ctoc')

      ier1 = fft_create(obj%fft_nx_inverse, +1, obj%nx_fft, 'ctoc')

      allocate(obj%ern(obj%nx_fft))
      allocate(obj%er (obj%nx_fft))

      call mem_alloc(obj%scr1, obj%nx_fft)
      call mem_alloc(obj%scr2, obj%nx_fft)
      call mem_alloc(obj%scr3, obj%nx_fft)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine madc_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine madc (obj,ntr,hd,tr)
      implicit none
      type(madc_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      real :: w_freq, dx_tmp, x0_start, min_time_2way
      integer :: min_depth_idx, max_depth_idx

      INTEGER :: IT, IV, IX, IZ
      integer I1, NV1, NPAS, NAP, N1, I2, NV2, NAP2
      integer ITMAX, NXSF, NT, IMUT
      REAL :: SCALE, EPS, FIZ, TEMP, TWN
      integer :: ntr_do, ntx_do, freq_do

      if(ntr == NEED_TRACES) then
        ! from below... madc needs more traces before it can output any.
        return
      else if(ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        ! one way or another, we're outa here.
        call madc_wrapup (obj)
        return
      end if

      ! process another gather
      obj%num_gather = obj%num_gather + 1

      if(ntr == 1) then
        ! oops, can't compute DX -- skip it
        call pc_warning('MADC: deleting gather ', obj%num_gather, &
          ' (only 1 trace)')
        ntr = NEED_TRACES
        return
      else if(ntr > obj%num_channels) then
        call pc_error('MADC: gather size ', ntr, &
          ' exceeds NUM_CHANNELS, gather= ', obj%num_gather)
        ntr = FATAL_ERROR
        return
      end if

      if(obj%dx_space_inc == 0.0) then
        ! first gather with more than one trace, capture spatial increment

        obj%dx_space_inc = hd(obj%hdr_loc,2) - hd(obj%hdr_loc,1)

        IF (obj%dx_space_inc <= 0.0) THEN
          call pc_error('MADC: Data out of order, negative DX= ', &
            obj%dx_space_inc)
          ntr = FATAL_ERROR
          return
        ENDIF

        obj%dp = 4. * obj%depth_inc / (obj%nx_fft * obj%dx_space_inc)
      else 
        dx_tmp = hd(obj%hdr_loc,2) - hd(obj%hdr_loc,1)

        ! The following check is new (2000-11-29 SELZLER)
        ! validity check, single point variation
        if(0 /= mth_compare(dx_tmp, obj%dx_space_inc)) then
          call pc_error('MADC: DX variation, gather= ', obj%num_gather)
          call pc_error('variation percentage= ', &
            100.0 * (dx_tmp - obj%dx_space_inc) / obj%dx_space_inc)
        end if
      ENDIF

      ! The following check is new (2000-11-29 SELZLER)
      ! validity check, all points variation
      do ntr_do = 3, ntr
        dx_tmp = hd(obj%hdr_loc,ntr_do) - hd(obj%hdr_loc,ntr_do - 1)

        if(0 /= mth_compare(dx_tmp, obj%dx_space_inc)) then
          call pc_error('MADC: DX variation, gather= ', obj%num_gather)
          call pc_error('variation percentage= ', &
            100.0 * (dx_tmp - obj%dx_space_inc) / obj%dx_space_inc)
        end if
      end do

      if(obj%mode /= 'MULT' .and. obj%mode /= 'ACORR') then
        ! save a copy of input traces
        obj%tr_save(:,:ntr) = tr(:obj%ndpt,:ntr)
      end if

      DO ntr_do = 1, NTR
        ! APPLY TAPERED WEIGHTS TO TRACES PRIOR TO TRANSFORM
        CALL madc_taper (hd(6,ntr_do), SCALE, obj%off_max_p100)

        ! scale trace samples
        obj%rfft_nt(:obj%ndpt) = tr(:obj%ndpt,ntr_do) * SCALE

        ! fft zero padding in time
        obj%rfft_nt(obj%ndpt + 1:) = 0.0

        ! transform from time (t) to frequency (w)
obj%cfft_nt = 0.0
        call fft_rc_transform(obj%fft_nt_forward, obj%rfft_nt, obj%cfft_nt, &
          opt_scale=obj%fft_nt_scale)

        ! transpose into 2d buffer
        obj%cfft_2d(ntr_do,:) = obj%cfft_nt(:obj%ntx_fft)
      END DO

      ! fft zero padding in space
      obj%cfft_2d(ntr+1:,:) = 0.0

      x0_start = hd(obj%hdr_loc,1)

      ! FILL UP EZ2 ARRAY WITH DEPTHS
      CALL madc_depths (obj%ezb, obj%depths_cnt, obj%ez2, x0_start, &
        obj%dx_space_inc, ntr, obj%depth_inc, min_depth_idx, max_depth_idx)

      !-DEBUG 
      !print*,'<MADC> GROUP #', obj%num_gather, ' OF ', ntr, ' TRACES', &
      !  ' MIN DEPTH=', min_depth_idx*obj%depth_inc, &
      !  ' MAX DEPTH=', max_depth_idx*obj%depth_inc, &
      !  ' dx_space_inc,dw_freq_inc,dp=', &
      !  obj%dx_space_inc, obj%dw_freq_inc, obj%dp

      ! finish forward 2D fft from (x,w) to (k,w)
      do ntx_do = 2, obj%ntx_fft
        call fft_cc_transform(obj%fft_nx_forward, obj%cfft_2d(:,ntx_do), &
          opt_scale=obj%fft_nx_scale)
      end do

      ! Special case for zero freq (DC, freq_do == 1)
      ! force amplitude to zero for all wave numbers
      obj%cfft_2d(:,1) = 0.0

      ! downward continue wavefield for this gather.

      do freq_do = 2, obj%ntx_fft
        w_freq = (freq_do - 1) * obj%dw_freq_inc

        ! build sin & cos arrays (ER & ERN) for nx_fft points only.
        call madc_sin_cos(w_freq, obj%dp, obj%nx_fft, &
          obj%er, obj%ern, min_depth_idx)

        if(min_depth_idx /= 0) then
          obj%cfft_2d(:,freq_do) = obj%cfft_2d(:,freq_do) * obj%ern
        end if

        reflector_choice: &
        if(min_depth_idx == max_depth_idx) then
          ! don't need CFTMR and FTCMR if WB is "flat", but do need to
          ! round trip transform while re-killing missing traces.

          ! transform from wave number back to space
          call fft_cc_transform(obj%fft_nx_inverse, obj%cfft_2d(:,freq_do), &
            opt_scale=obj%fft_nx_scale)

          ! zero offset greater than actual number of channels
          obj%cfft_2d(ntr+1:,freq_do) = 0.0

          ! transform from space back to wave number
          call fft_cc_transform(obj%fft_nx_forward, obj%cfft_2d(:,freq_do))
        else ! reflector_choice
          ! simulate CFTMR (continue from shallowest WB to deepest WB)
          EPS = SIN(2.*PI/obj%nx_fft)**2

          ! clear result
          obj%SCR3(:ntr) = (0.0,0.0)

          do iz = min_depth_idx, max_depth_idx
            fiz = float(iz)
!??? 2001-01-03 Selzler, the following version fails on the absoft
!??? Linux compiler, but only for some very specific test cases.
!???        obj%cfft_2d(:,freq_do) = obj%cfft_2d(:,freq_do) * obj%er(:)
!???        obj%scr2(:) = obj%cfft_2d(:,freq_do)
!??? This version, which is functionally equivalent, works.
            obj%scr2(:) = obj%cfft_2d(:,freq_do) * obj%er(:)
            obj%cfft_2d(:,freq_do) = obj%scr2(:)

            ! transform from wave number back to space
            call fft_cc_transform(obj%fft_nx_inverse, obj%scr2(:), &
              opt_scale=obj%fft_nx_scale)

            do ix = 1, ntr
              temp = eps / ((obj%ez2(ix) - fiz)**2 + eps)
              obj%scr3(ix) = obj%scr3(ix) + obj%scr2(ix) * cmplx(temp)
            end do
          end do

          ! simulate FTCMR (continue from deepest WB to shallowest WB)
          ! clear result
          obj%cfft_2d(:,freq_do) = (0.0,0.0)

          do iz = max_depth_idx, min_depth_idx, -1
            fiz = float(iz)

            do ix = 1, ntr
              temp = eps / ((obj%ez2(ix) - fiz)**2 + eps)
              obj%scr2(ix) = obj%scr3(ix) * cmplx(temp)
            end do

            ! clear input to cfft
            obj%scr2(ntr + 1:) = (0.0,0.0)

            ! transform from space back to wave number
            call fft_cc_transform(obj%fft_nx_forward, obj%scr2(:))

            obj%cfft_2d(:,freq_do) = (obj%cfft_2d(:,freq_do) + obj%scr2)*obj%er
          end do
        end if reflector_choice

        if(min_depth_idx /= 0) then
          ! perform complex multiply that was done in CONMR
          obj%cfft_2d(:,freq_do) = obj%cfft_2d(:,freq_do) * obj%ern
        end if
      end do

      ! start reverse 2D fft from (k,w) to (x,w)
      do ntx_do = 1, obj%ntx_fft
        call fft_cc_transform(obj%fft_nx_inverse, obj%cfft_2d(:,ntx_do))
      end do

      DO ntr_do = 1, NTR
        ! untranspose from 2d buffer
        obj%cfft_nt(:obj%ntx_fft) = obj%cfft_2d(ntr_do,:)

        ! fft zero padding in frequency, beyond FREQ_MAX
        obj%cfft_nt(obj%ntx_fft + 1:) = 0.0

        ! transform from frequency (w) to time (t)
        call fft_cr_transform(obj%fft_nt_inverse, obj%cfft_nt, obj%rfft_nt)

        ! APPLY INVERSE TAPERED WEIGHTS TO TRACES AFTER TRANSFORM
        CALL madc_taper (hd(6,ntr_do), SCALE, obj%off_max_p100)

        ! inverse scaling, limited to 100 (Selzler, strange limit ???)
        scale = min(1.0 / scale, 100.0)

        ! scale fft buffer and transpose back into trace samples
        tr(:obj%ndpt,ntr_do) = obj%rfft_nt(:obj%ndpt) * scale
      END DO

      ! filter as needed

      if(obj%mode == 'MULT') then
        ! nothing more to do, if mode is MULT
        return
      end if

      min_time_2way = min_depth_idx * obj%depth_inc * 2.0 / obj%vel_cont

      IF (obj%win_beg1 == 0.0) THEN
        TWN = min_time_2way
      ELSE
        TWN = obj%win_beg1
      ENDIF

      it_ntr_loop: &
      do it = 1, ntr
        I1 = (SQRT(TWN**2 + (hd(6,IT)/obj%vel_win1)**2)-obj%tstrt)/obj%dt
        NV1 = MIN0(obj%win_len1_cnt,obj%ndpt - I1)

        ! nominal values for a single window
        NPAS = 1
        N1 = 1
        NAP = obj%ndpt

        IF (obj%win_beg2 > 0.0) then
          ! set up for two windows if input (not if 2nd window too deep)
          I2 = (SQRT(obj%win_beg2**2 + &
            (hd(6,IT)/obj%vel_win2)**2)-obj%tstrt)/obj%dt

          IF (I2 < obj%ndpt) THEN
            NPAS = 2
            N1 = I1
            NAP = NV1
            NV2 = MIN0(obj%win_len2_cnt,obj%ndpt - I2)
            NAP2 = obj%ndpt - I2
          ENDIF
        else
          i2 = obj%ndpt
        ENDIF

        ! AUTOCORRELATION OF CONTINUED TRACE
        CALL fltr_filtrgs (tr(i1:obj%ndpt,it), NV1, tr(i1:obj%ndpt,it), NV1, &
          obj%auto_corr, obj%len_op_cnt, 1, 0)

        if(obj%mode == 'ACORR') then
          tr(:obj%len_op_cnt,it) = obj%auto_corr
          tr(obj%len_op_cnt+1:obj%ndpt,it) = 0.0
          cycle
        end if

        ! CROSS CORRELATION OF CONTINUED TRACE AND ORIGINAL
        ITMAX = mth_isamax(obj%ndpt,obj%tr_save(:,it),1)

        IF (obj%tr_save(ITMAX,it) == 0.0) THEN
          ! for AUTO or CCOR clear result here
          tr(:obj%ndpt,it) = 0.0
          CYCLE  ! don't waste time on dead traces
        ENDIF

        NXSF = -obj%lag_sample_cnt
        CALL fltr_filtrgs (tr(i1:obj%ndpt,it), NV1, &
                  obj%tr_save(I1:obj%ndpt,it), NV1, &
          obj%cross_corr, obj%len_op_cnt, 1, NXSF)

        if(obj%mode == 'CCORR') then
          tr(:obj%len_op_cnt,it) = obj%cross_corr
          tr(obj%len_op_cnt+1:,it) = 0
          cycle
        end if

        ! add diagonal load
        obj%auto_corr(1) = obj%auto_corr(1) * obj%diag_fact

        ! CONV is used for scratch
        CALL OPFILT (obj%len_op_cnt, obj%oper_filt, obj%cross_corr, &
          obj%conv_scr, obj%auto_corr)

        ! TAPER ENDS OF OPERATOR
        DO IV = 1, 4
          SCALE = FLOAT(IV)*0.2
          obj%oper_filt(IV) = obj%oper_filt(IV)*SCALE
          obj%oper_filt(obj%len_op_cnt+1-IV) = &
          obj%oper_filt(obj%len_op_cnt+1-IV)*SCALE
        END DO

        if(obj%mode == 'OPER') then
          tr(:obj%len_op_cnt,it) = obj%oper_filt
          tr(obj%len_op_cnt+1:,it) = 0.0
          cycle
        end if

        ! FLIP OPERATOR FOR CONVOLUTION
        obj%auto_corr(:) = obj%oper_filt(obj%len_op_cnt:1:(-1))

        ! CONVOLVE CONTINUED TRACE W/ FILTER AND SUBTRACT RESULT FROM ORIG.
        NXSF = obj%lag_sample_cnt - obj%len_op_cnt + 1
        CALL fltr_filtrgs (obj%auto_corr, obj%len_op_cnt, &
          tr(n1:obj%ndpt,it), NAP, obj%conv_scr(N1:), NAP, 1, NXSF)

        IF (NPAS == 1) THEN
          ! only one window, do whole trace
          tr(:obj%ndpt,it) = obj%tr_save(:,it) - obj%conv_scr
        ELSE
          ! for 2 windows, adjust orig for win #1
          obj%tr_save(n1:nap-1+n1,it) = &
          obj%tr_save(n1:nap-1+n1,it) - obj%conv_scr(n1:nap-1+n1) 

          ! AUTOCORRELATION OF CONTINUED TRACE  ! repeat process for win #2
          CALL fltr_filtrgs (tr(i2:obj%ndpt,it), NV2, tr(i2:obj%ndpt,it), &
            NV2, obj%auto_corr, obj%len_op_cnt, 1, 0)

          ! CROSS CORRELATION OF CONTINUED TRACE AND ORIGINAL
          NXSF = -obj%lag_sample_cnt
          CALL fltr_filtrgs (tr(i2:obj%ndpt,it), NV2, obj%tr_save(i2:,it), &
            NV2, obj%cross_corr, obj%len_op_cnt, 1, NXSF)

          ! OPER
          ! add diagonal load
          obj%auto_corr(1) = obj%auto_corr(1)*obj%diag_fact

          CALL OPFILT (obj%len_op_cnt, obj%oper_filt, obj%cross_corr, &
            obj%conv_scr, obj%auto_corr) ! obj%conv_scr is scratch

          ! FLIP OPERATOR FOR CONVOLUTION AND TAPER ENDS
          obj%auto_corr(:) = obj%oper_filt(obj%len_op_cnt:1:(-1))

          DO IV = 1, 4
            SCALE = FLOAT(IV)*0.2
            obj%auto_corr(IV) = obj%auto_corr(IV)*SCALE
            obj%auto_corr(obj%len_op_cnt+1-IV) = &
            obj%auto_corr(obj%len_op_cnt+1-IV)*SCALE
          END DO

          ! CONVOLVE CONTINUED TRACE W/ FILTER AND SUBTRACT RESULT FROM ORIG.
          NXSF = obj%lag_sample_cnt - obj%len_op_cnt + 1
          CALL fltr_filtrgs(obj%auto_corr, obj%len_op_cnt, tr(i2:obj%ndpt,it), &
            NAP2, obj%conv_scr(I2:), NAP2, 1, NXSF)

          obj%tr_save(i2:nap2-1+i2,it) = &
          obj%tr_save(i2:nap2-1+i2,it) - obj%conv_scr(i2:nap2-1+i2)
          tr(:obj%ndpt,it) = obj%tr_save(:,it)
        ENDIF

      END DO it_ntr_loop

      demult_only_mode_choice: &
      IF (obj%mode == 'DEMULT') THEN 
        ! REAPPLY INPUT MUTES TO DEMULT TRACES
        mute_ntr_loop: &
        DO NT = 1, NTR
          ! honor head and tail mutes (no taper)
          CALL MUTEHW (hd(:,nt), tr(:,nt), obj%ndpt, 0.0, MUTEHW_BOTH)

          ! set header LAV (dead trace flag)
          call lav_set_hdr (hd(:,nt), tr(:,nt), obj%ndpt)
          ! if mutes indicate a dead trace we're through; else taper
          if(hd(25,nt) == 0.0) cycle

          ! head mute taper
          IMUT = NINT(hd(2,NT)) - 1
          tr(IMUT+MAX(1,1-IMUT):MIN(obj%taper_pt_cnt,obj%ndpt-IMUT)+IMUT,nt) = &
            obj%taper(MAX(1,1-IMUT):MIN(obj%taper_pt_cnt,obj%ndpt-IMUT)) * &
            tr(IMUT+MAX(1,1-IMUT):MIN(obj%taper_pt_cnt,obj%ndpt-IMUT) + IMUT,nt)

          ! tail mute taper
          IMUT = NINT(hd(64,NT)) + 1
          tr(IMUT-MAX(1,IMUT-obj%ndpt):IMUT-MIN(obj%taper_pt_cnt, &
            IMUT-1):(-1),nt) = &
            obj%taper(MAX(1,IMUT-obj%ndpt):MIN(obj%taper_pt_cnt,IMUT-1)) * &
            tr(IMUT-MAX(1,IMUT-obj%ndpt)&
            :IMUT-MIN(obj%taper_pt_cnt,IMUT-1):(-1),nt)

        END DO mute_ntr_loop

      ENDIF demult_only_mode_choice

      return
      end subroutine madc

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine madc_wrapup (obj)
      implicit none
      type(madc_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine madc_wrapup

!</execute_only>

!!-------------------------- madc_spline -----------------------------------!!
!!-------------------------- madc_spline -----------------------------------!!
!!-------------------------- madc_spline -----------------------------------!!

!<execute_only>
      SUBROUTINE madc_spline(Z, X, npts, Z1, Z2, Z3, P, Q)
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: npts
      REAL , INTENT(IN) :: Z(npts)
      REAL , INTENT(IN) :: X(npts)
      REAL , INTENT(OUT) :: Z1(npts)
      REAL , INTENT(OUT) :: Z2(npts)
      REAL , INTENT(OUT) :: Z3(npts)
      REAL , INTENT(OUT) :: P(npts-1)
      REAL , INTENT(OUT) :: Q(npts-1)

      INTEGER ::         NM, I, IP, IM  
      REAL :: ALPH, DEN
!-----------------------------------------------
!**** THIS ROUTINE COPIED UNCHANGED FROM CYBER VERSION OF "SPLN" IN MADC
!   P,Q ARE SCRATCH ARRAYS OF DIMENSION npts-1.
!   P MAY OVERLAY Z1.
!
!   FIRST PASS.  ESTABLISH LINEAR RELATION BETWEEN Z1"S.
!
      Z2(1) = X(2) - X(1)

      IF (Z2(1) == 0.) then
        Z2(1) = 0.001 * (X(3) - X(1))
      end if

      Z3(1) = (Z(2) - Z(1)) / Z2(1)
      P(1) = 0.5
      Q(1) = 1.5 * Z3(1)
      NM = npts - 1
      IM = 1
      I  = 2
      IP = 3
      Z2(I) = X(IP) - X(I)

      IF (Z2(I) == 0.) then
        Z2(I) = 0.001 * (X(IP) - X(IM))
      end if

      Z3(I) = (Z(IP) - Z(I)) / Z2(I)
      ALPH = Z2(IM) / Z2(I)
      DEN = 2.*(1. + ALPH) - P(IM)
      P(I) = ALPH/DEN
      Q(I) = (3.*(Z3(IM) + ALPH * Z3(I)) - Q(IM)) / DEN

      DO WHILE(I < npts)
        IM = I
        I = IP
        IP = IP + 1
        Z2(I) = X(IP) - X(I)

        IF (Z2(I) == 0.) then
          Z2(I) = 0.001 * (X(IP) - X(IM))
        end if

        Z3(I) = (Z(IP) - Z(I)) / Z2(I)
        ALPH = Z2(IM) / Z2(I)
        DEN = 2.*(1. + ALPH) - P(IM)
        P(I) = ALPH/DEN
        Q(I) = (3.*(Z3(IM) + ALPH*Z3(I)) - Q(IM)) / DEN
      END DO

      ! SECOND PASS. FIND Z1,Z2,Z3.

      Z1(npts) = (3. * Z3(NM) - Q(NM)) / (2. - P(NM))
      Z3(npts) = 0.
      Z2(npts) = 0.

      DO I = 1, npts - 1
        Z1(npts-I) = Q(npts-I) - P(npts-I) * Z1(npts+1-I)
      END DO

      Z3(npts-1:1:(-1)) = 6./(Z2(npts-1:1:(-1)) * Z2(npts-1:1:(-1))) * &
        (Z1(npts:2:(-1)) + Z1(npts-1:1:(-1)) - 2. * Z3(npts-1:1:(-1)))
      Z2(npts-1:1:(-1)) = (Z1(npts:2:(-1)) - Z1(npts-1:1:(-1))) / &
        Z2(npts-1:1:(-1)) - Z2(npts-1:1:(-1)) * Z3(npts-1:1:(-1)) * 0.5

      RETURN

      END SUBROUTINE madc_spline
!</execute_only>

!!-------------------------- madc_taper -----------------------------------!!
!!-------------------------- madc_taper -----------------------------------!!
!!-------------------------- madc_taper -----------------------------------!!

!<execute_only>
      SUBROUTINE madc_taper(OFFSET, TAPER, OMAX)
      IMPLICIT NONE
      double precision , INTENT(IN) :: OFFSET
      REAL , INTENT(OUT) :: TAPER
      REAL , INTENT(IN) :: OMAX

      REAL, PARAMETER :: PI2 = 2 * PI
      INTEGER :: I
      REAL , DIMENSION(4) :: A
      REAL :: ARG, XIM

      ! FOUR TERM BLACKMAN-HARRIS TAPER

      DATA A/ 0.35875, 0.48829, 0.14128, 0.01168/

      TAPER = A(1)
      ARG = 0.5 * (OFFSET + OMAX) / OMAX * PI2

      DO I = 2, 4
        XIM = FLOAT(I - 1)
        TAPER = TAPER + A(I) * FLOAT((-1)**(I - 1)) * COS(XIM*ARG)
      END DO

      RETURN

      END SUBROUTINE madc_taper

!</execute_only>

!!-------------------------- madc_depths -----------------------------------!!
!!-------------------------- madc_depths -----------------------------------!!
!!-------------------------- madc_depths -----------------------------------!!

!<execute_only>

      SUBROUTINE madc_depths(ZB, npts, Z2, X0, DX, NX, depth_inc, IZL, IZG)
      IMPLICIT NONE
      INTEGER  :: npts
      INTEGER , INTENT(IN) :: NX
      INTEGER , INTENT(OUT) :: IZL
      INTEGER , INTENT(OUT) :: IZG
      REAL , INTENT(IN) :: X0
      REAL , INTENT(IN) :: DX
      REAL , INTENT(IN) :: depth_inc
      REAL  :: ZB(*)
      REAL , INTENT(OUT) :: Z2(*)

      INTEGER :: IZB, IX, IZN
      REAL :: X, ZN

      ! THIS ROUTINE GENERATES THE DEPTH ARRAY Z2(IX) FOR NX
      ! X VALUES STARTING AT X0 FROM THE BOUNDARY INFORMATION
      ! STORED IN THE npts POINTS OF ZB.

      X = X0

      ! IZL,IZG ARE THE EXTREMAL VALUES OF Z2
      IZL = 10000000
      IZG = -10000000

      IZB = 0

      ! LOOP OVER ALL COORDINATES X
      DO IX = 1, NX
        ! CALCULATE DEPTH AT X   !madc_vl=VAL(Cyber)
        CALL madc_vl (X, ZN, ZB(npts+1), ZB(2*npts+1), ZB(3*npts+1), &
          ZB(4*npts+1), ZB, npts, IZB)

        ! EXPRESS DEPTH IN UNITS OF depth_inc
        IZN = INT(ZN/depth_inc + 0.5)

        ! UPDATE MIIMUM AND MAXIMUM DEPTHS
        IZL = MIN0(IZL,IZN)
        IZG = MAX0(IZG,IZN)

        ! STORE DEPTH IN Z2
        Z2(IX) = IZN

        ! INCREMENT X
        X = X + ABS(DX)
      END DO

      RETURN

      END SUBROUTINE madc_depths

!</execute_only>

!!-------------------------- madc_vl -----------------------------------!!
!!-------------------------- madc_vl -----------------------------------!!
!!-------------------------- madc_vl -----------------------------------!!

!<execute_only>

      SUBROUTINE MADC_VL(XV, ZV, Z, Z1, Z2, Z3, X, npts, IX)
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: npts
      INTEGER , INTENT(INOUT) :: IX
      REAL , INTENT(IN) :: XV
      REAL , INTENT(OUT) :: ZV
      REAL , INTENT(IN) :: Z(*)
      REAL , INTENT(IN) :: Z1(*)
      REAL , INTENT(IN) :: Z2(*)
      REAL , INTENT(IN) :: Z3(*)
      REAL , INTENT(IN) :: X(*)

      INTEGER :: IXP
      REAL :: XR

      IF (XV > X(1)) THEN
        IF (XV > X(npts)) THEN
          ZV = Z(npts)
          XR = 0.
        ELSE
          IF (IX<=0 .OR. IX>=npts) IX = 1
          IXP = IX
          IX = IXP
          IXP = IX + 1

          DO WHILE(XV>=X(IXP) .AND. IXP<npts)
            IX = IXP
            IXP = IX + 1
          END DO

          XR = XV - X(IX)
          ZV = Z(IX) + XR * (Z1(IX) + XR * 0.5 * (Z2(IX) + &
            XR * 0.333333 * Z3(IX)))
        ENDIF

        RETURN
      ENDIF

      ZV = Z(1)

      RETURN

      END SUBROUTINE MADC_VL

!</execute_only>

!!-------------------------- madc_sin_cos -----------------------------------!!
!!-------------------------- madc_sin_cos -----------------------------------!!
!!-------------------------- madc_sin_cos -----------------------------------!!

!<execute_only>

      SUBROUTINE madc_sin_cos(W, DP, nx_fft, ER, ERN, NSTEP)
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: nx_fft
      INTEGER , INTENT(IN) :: NSTEP
      REAL , INTENT(IN) :: W
      REAL , INTENT(IN) :: DP
      complex, INTENT(OUT) :: ER(nx_fft)
      complex , INTENT(OUT) :: ERN(nx_fft)

      INTEGER :: J
      REAL :: XPIH, W2, ARG, ARR, CS, SN, P
      COMPLEX :: ZARG

      ! BUILD SIN & COS ARRAYS FOR LAYER PROPAGATION
      ! SUBSTITUTE FOR MAP ROUTINE "CSSET"

      ! DATA XPIH/ 1.570796327/ !!! too precise for ifc 11/9/2001 RLS
      DATA XPIH/ 1.57079632/
      ! WAVE-NUMBER LOOP

      W2 = W**2
      ER(:) = 0.0
      ERN(:) = 0.0

      ARG = W2
      ARR = SQRT(ARG)*XPIH
      CS = COS(ARR)
      SN = -SQRT(1.0 - CS**2)
      ZARG = CMPLX(CS,SN)
      er(1) = zarg
      ZARG = ZARG**NSTEP
      ern(1) = zarg

      DO J = 2, nx_fft / 2       !1st calc j=1, then loop 2,nx_fft / 2
        P = (J - 1)*DP
        ARG = W2 - P**2

        IF (ARG <= 0.0) EXIT

        ARR = SQRT(ARG)*XPIH
        CS = COS(ARR)
        SN = -SQRT(1.0 - CS**2)

        ! COMPUTE N STEP PROPAGATOR

        ZARG = CMPLX(CS,SN)
        er(j) = zarg
        er(nx_fft-(j-2)) = zarg
        ZARG = ZARG**NSTEP
        ern(j) = zarg
        ern(nx_fft-(j-2)) = zarg
      END DO

      RETURN

      END SUBROUTINE madc_sin_cos

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module madc_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
