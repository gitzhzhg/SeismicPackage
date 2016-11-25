!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 1999-07-15. />

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
! Name       : DECON    (DECONvolution)
! Category   : filters
! Written    : 1986-07-03   by: Bob Baumel
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Time domain predictive (statistical) deconvolution.
! Portability: No known limitations.
! Parallel   : Yes
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! DECON performs traditional minimum phase predictive (statistical)
! deconvolution, either "spiking" decon (prediction length is one sample) or
! "gap" decon (prediction length is greater than one sample).
!
! DECON first calculates the autocorrelation within the trace design window,
! calculates the decon operator from the autocorrelation and then
! performs the deconvolution by convolving the operator with the trace.
!
! <left>DECON parameters fall into four categories</left>
! DECON TYPE: traditional decon, "normalizing" decon (flattens amplitude
! spectrum only) and/or "saile" (trace integration).
!
! OPERATOR BUILD MODE:  one operator for each input trace, one for each input
! gather or one for all input traces.
!
! OPERATOR DESIGN PARAMETERS: operator length, gap and diagonal load.
!
! DESIGN WINDOW PARAMETERS: parameters for specifying beginning and ending
! times for a possibly offset varying trace window (this trace window is used
! for calculating autocorrelations for the operator design).
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!<left>Single- and Multiple-Trace Deconvolution</left>
! For single-trace decon, select OPT_TR_MODE = SINGLE. For multiple-trace or
! gather-consistent decon, select OPT_TR_MODE = MULT.  In this mode input
! traces must be gathered.  Autocorrelation is calculated on traces within the
! gather defined by the TR_INIT, TR_INC and TR_LAST parameters.
! Autocorrelations are averaged to achieve an improved estimate and a single
! operator is calculated and applied to all traces in the gather.
!
!<left>All-Trace Option</left>
! When OPT_TR_MODE = ALL, a single operator is calculated from the autocor-
! relations of all input traces and written to a file where it can be read by
! TRIN, FILTR or DSIG.  For consistency with FILTR and DSIG, TMIN of this
! TRCIO file contains the time of the first sample in this trace (ZERO if
! OPT_DECON_TYPE = DECON or -LEN_OP if OPT_DECON_TYPE = ZERO_PHASE ).
!
!<left>Improved Efficiency Trace Selection for Autocorrelatio</left>
! When OPT_TR_MODE = MULT or OPT_TR_MODE = ALL, autocorrelation is calculated
! only on traces within the input gather defined by the TR_INIT, TR_INC and
! TR_LAST parameters.  If TR_INC is set > 1, run-time and cost is lowered and
! typically the operator calculated is not significantly different than if all
! the traces had been used.
!
!<left>Window Options</left>
! Of the window options, the MUTE option is simpler to set up than the
! more complicated OFF_GRID method and may give adequate results.
!
!<left>Tau-P Deconvolution</left>
! For GAPPED decon in the TAU_P domain, the user-specified GAP is multiplied by
! the quantity    SQRT ( 1. - (V*P)**2 )    where V is the reference velocity
! (TAU_P_VEL), and P is the slowness or ray parameter of the trace.  The
! P-values are taken from header word HDR_OFF (the same word as would normally
! hold the offset) and are assumed to be in MICROseconds per meter or foot
! (same conventions as in process SLST).  TAU_P_VEL should be in standard
! velocity units (meters or feet per second).  This option is used only when
! OPT_DECON_TYPE = DECON, OPT_TR_MODE = SINGLE, GAP>0, and TAU_P_VEL>0.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process (only when OPT_TR_MODE = SINGLE) or a
! multiple-trace process.
!
! This process requires traces to be input in gathers only for
! OPT_TR_MODE = MULT.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces except for OPT_TR_MODE = ALL.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
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
! NDPT     number of sample values in trace      used but not changed
! GATHERED traces are gathered (true) or not     used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! NWIH     number of words in trace header       used but not changed
!
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
! 2       head mute                  use in defining design window
! 25      largest absolute value     recomputed
! 64      tail mute                  use in defining design window
! HDR_INL inline grid header word
! HDR_CRL crossline header word
! HDR_OFF header for offset
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!      Date       Author     Description
!      ----       ------     -----------
!  47. 2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
!046. 2006-01-10  B. Menger   Removed Unused Variables.
!  45. 2005-01-17 Chiu       Added option to use zero crossing of the
!                            autocorrelation for determining the GAP parameter
!  44. 2002-05-06 Vunderink  Added parallel control parameters
!  43. 2001-10-18 Selzler    Temporarily back out parallel support
!  42. 2001-07-31 Vunderink  Added parallel control parameters
!  41. 2000-12-11 Selzler    Changed wrapup logic to use skip_wrapup
!  40. 2000-10-03 Selzler    Corrected bug, linked array length initialization.
!                            Changed default for OP_LEN per user request.
!  39. 2000-05-09 Selzler    Change offsets to double precision
!  38. 2000-03-27 Selzler    Verify DT before using it
!  37. 2000-03-10 Selzler    Modified functionality per user feedback
!  36. 2000-03-01 Selzler    Corrected documentation for header 3 and 4
!  35. 2000-02-25 Selzler    Corrected bug when writing operator to file
!  34. 2000-02-10 Selzler    Corrected bug in wrapup logic
!  33. 2000-02-09 Selzler    synchronized source with CIB's latest newdoc.
!  32. 2000-02-07 Selzler    improved gui support
!  31. 2000-02-02 Selzler    Added support for GUI and general cleanup
!  30. 2000-01-14 Selzler    Corrected two bugs
!  29. 2000-01-12 Selzler    Corrected FFT scale factor
!  28. 2000-01-07 Selzler    Corrected 'MULT' constant bug and
!                            changed OPLEN to LEN_OP and
!                            changed FILENAME to PATHNAME per request.
!  27. 1999-11-19 Selzler    Corrected naming and date usage.
!  26. 1999-10-27 Selzler    Added RCS "Id" strings to tag executeable
!  25  1999-08-10 Selzler    Conversion to fortran 90 compiler.
!  24  1998-11-10 Vunderink  Begin using the f90 compiler.
!  23  1998-06-29 Vunderink  Added OPT, OFFSET, ADDTIM, WINLEN parameters
!  22  1997-11-10 Vunderink  Added spatial window
!  21  1995-09-05 Vunderink  Double LEN_OP when FILTYP=NORM.
!  20  1992-04-06 Troutt     Add tail mute logic. Don't use input values
!                            greater than tail mute index for operator
!                            design (input values above head mute index
!                            were already excluded). Call MUTEHW on input
!                            to assure that tail mute is set.
!  19  1990-12-05 Baumel     Add STRIDE option in multi-trace decon.
!  18  1990-09-27 Ball       Doc. chg only. LTR parameter default.
!  17  1990-02-28 Peterson   Allow traces with dead window to pass thru
!                            without DECON (S/M=S or M). As per JOHN REED.
!  16  1989-08-31 Baumel     Fix GAPPED decon bug (introduced 1989/04/07).
!  15  1989-07-17 Baumel     Fix bug of incorrect wrapup messages.
!  14  1989-04-07 Baumel     Add NORMALIZE/SAILE & PTAU_VEL options.
!  13  1989-01-19 Howard     New DTRIN & DTROT Mod.
!  12  1988-09-26 Howard     NWIH and NWPT conversion.
!  11  1988-07-26 Baumel     Use new DCODE/NCODE tables.
!  10  1988-06-03 Baumel     New convention for mute header word.
!   9  1988-04-22 Baumel     Add CPSPRT calls.
!   8  1987-10-04 Baumel     Remove "apply" mode & match disk file
!                            convention to FILTR process.
!   7  1987-06-09 Baumel     Add S/M = B and S/M = A options (now
!                            internally callable, MEMPRT removed).
!   6  1987-03-31 Hanson     Add NCODE call for history records.
!   5  1987-01-15 Baumel     Add IPRT parameter (print switch).
!   4  1986-10-24 Baumel     Add multi-channel capability (S/M param).
!   3  1986-08-18 Baumel     Call to MEMPRT added.
!   2  1986-07-18 Baumel     Allow Offset-dependent Time Window.
!   1  1986-07-03 Baumel     Original working version.
!
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
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
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
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
! This illustrates the dependence of some keywords upon the value of others.
!
!                                OPT_WIN                       scalar
!                                   |
!                      ?------------+-------------?
!                      |                          |
!                 "OFF_GRID"                   "MUTE"
!                      |                          |
!                     OFF                      OFFSET        + arrayset
!                 WIN_TIM_BEG                  TIM_ADD       + arrayset
!                 WIN_TIM_END                  WIN_LEN       + arrayset
!                      |
!             +--------+--------------+
!             |                       |
!          HDR_INL                 HDR_CRL                     scalar
!             |                       |
!        ?----+-----?            ?----+----?
!        |          |            |         |
!   "non-zero"    "zero"    "non-zero"   "zero"
!        |                       |
!    INLINES                CROSSLINES                       + arrayset
!        |                       |                           +
!        +--------+--------------+                           +
!                 |                                          +
!           TIME_REF_BEG                                     + arrayset
!           TIME_REF_END                                     + arrayset
!
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS DECON Process/NC=80>
! 
!            Time domain predictive deconvolution
! 
! OPT_DECON_TYPE=`CCCCCCCC    SAILE=`CC    OPT_TR_MODE=`CCCCC
! 
! PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! 
! LEN_OP= `FFFFFFFFFFF  GAP=~~~~`FFFFFFFFFFF  DIAG_LOAD= `FFFFFFFFFFF
! 
! TR_INIT=`IIIIIIIIII   TR_INC= `IIIIIIIIII   TR_LAST=~~~`IIIIIIIIII
! 
! HDR_INL=`IIIIIIIII    HDR_CRL=`IIIIIIIIII   TAU_P_VEL= `FFFFFFFFFFF 
! 
! HDR_OFF=`IIIIIIIIII   OPT_WIN=`CCCCCCC      ZERO_CROSS=`I
!
! HDR_ZCROSS=`IIIIIII
! 
! OFF         WIN_TIM_BEG WIN_TIM_END
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
! 
! INLINES     CROSSLINES  TIM_REF_BEG TIM_REF_END
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF`FFFFFFFFFFF
! 
! OFFSET      TIM_ADD      WIN_LEN
! `FFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFF
! `FFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFF
!<PARMS PATHNAME[/ML=128/XST]>
!<PARMS OFF_ARRAYSET[/XST/YST]>
!<PARMS INLINES_ARRAYSET[/XST/YST]>
!<PARMS OFFSET_ARRAYSET[/XST/YST]>
!</gui_def>
!<HelpSection>
!
!
!<Help KEYWORD="OPT_DECON_TYPE">
!<Tip> Option of type of decon operator to use. </Tip>
! Default = DECON
! Allowed = DECON      (traditional statistical decon)
! Allowed = ZERO_PHASE (whiten amplitude spectrum only, preserve phase
! spectrum)
! Allowed = NONE       (use no decon operator - if only SAILE is desired)
! Normally the DECON option is chosen.  ZERO_PHASE is used when it is desired
! to whiten the amplitude spectrum while maintaining the phase spectrum
! unchanged.  The NONE option is available if the SAILE operation is desired
! without decon.
!
! For ZERO_PHASE, a spiking decon operator of length 2*LEN_OP is first derived.
! This is then converted (by FFT and inverse FFT) to a zero-phase operator of
! the same length (lag times from -LEN_OP to +LEN_OP) and with the same
! amplitude spectrum.
!</Help>
!
!<Help KEYWORD="SAILE">
!<Tip> Option whether to perform SAILE (integrate trace). </Tip>
! Default = NO
! Allowed = YES  (Perform SAILE after the operation chosen in OPT_DECON_TYPE.)
! Allowed = NO   (Do not perform SAILE.)
!
! The SAILE operation is performed after the decon operation chosen in
! OPT_DECON_TYPE.  If SAILE is desired, OPT_TR_MODE must be either SINGLE or
! MULT.
!
!</Help>
!
!<Help KEYWORD="OPT_TR_MODE">
!<Tip> Option whether to use single trace, multiple trace or all trace. </Tip>
! Default = SINGLE
! Allowed = SINGLE  (Build one operator for each input trace.)
! Allowed = MULT    (Build one operator for each input trace gather.)
! Allowed = ALL     (Build a single operator for all input traces.)
! The SINGLE option performs traditional single-trace decon.  The MULT option
! designs a single operator from the autocorrelations of all traces in a gather
! and applies that operator to all traces in the gather ("gather-consistent"
! decon).  The ALL option designs a single operator from the autocorrelations
! of all the input traces and writes that operator to a file.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Filename for TROT file containing operator when OPT_TR_MODE = ALL. </Tip>
! Default = blank
! Allowed = char
!</Help>
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of decon operator, in seconds. </Tip>
! Default = 0.200
! Allowed = real>0.0
! LEN_OP is the entire length of the decon operator, in seconds, including any
! gap.
!</Help>
!
!<Help KEYWORD="GAP">
!<Tip> Prediction distance for operator, in seconds. </Tip>
! Default = 0.0
! Allowed = real>=0.0
! GAP = 0.0 produces "spiking decon", GAP > 0.0 produces "gap decon."
! (Actually, a GAP of one sample means spiking decon and a GAP of more than one
! sample means gap decon.)
!
! GAP must be 0.0 for OPT_DECON_TYPE = ZERO_PHASE.
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent. </Tip>
! Default = 2.0
! Allowed = real>=0.0
!</Help>
!
!<Help KEYWORD="TR_INIT">
!<Tip> First trace in gather to use in calculating decon operator. </Tip>
! Default = 1
! Allowed = int>0
! Sequential number of first trace in gather to use in calculating decon
! operator when OPT_TR_MODE is MULT or ALL.
!</Help>
!
!<Help KEYWORD="TR_INC">
!<Tip> Increment for trace in gather to use in calculating decon operator.</Tip>
! Default = 1
! Allowed = int>0
! Increment for sequential number of trace in gather to use in calculating
! decon operator when OPT_TR_MODE is MULT or ALL.
!</Help>
!
!<Help KEYWORD="TR_LAST">
!<Tip> Last trace in gather to use in calculating decon operator . </Tip>
! Default = number of traces in gather
! Allowed = int>=TR_INIT
! Sequential number of last trace in gather to use in calculating decon
! operator when OPT_TR_MODE is MULT or ALL.
!</Help>
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word designating offset. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OPT_WIN">
!<Tip> Option of method to use for specifying autocorrelation window. </Tip>
! Default = OFF_GRID
! Allowed = OFF_GRID (Window varies with offset and grid.)
! Allowed = MUTE     (Window defined by time measured from mute.)
! Option OFF_GRID specifies the autocorrelation window by a linked array of
! offset, window top and window bottom values.  In addition these values may
! be varied over grid coordinates if desired.  (If HDR_INL = 0 and HDR_CRL = 0
! then no grid variation occurs.)
!
! Option MUTE specifies the autocorrelation window by a linked array of offset,
! time from mute to window top and length of window.
!</Help>
!
!<Help KEYWORD="ZERO_CROSS">
!<Tip> Use zero_crossing to determine the GAP. </Tip>
! Default = 0
! Allowed = 0 - 2
! If ZERO_CROSS = 0, use user-defined GAP.
! If ZERO_CROSS = 1, use first zero crossing as GAP.
! If ZERO_CROSS = 2, use second zero crossing as GAP.
!</Help>
!
!<Help KEYWORD="HDR_ZCROSS">
!<Tip> Header word for GAP determined from the zero crossing. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Header word to store the GAP determined from zero crossing of the 
! autocorrelation.
!
! If HDR_ZCROSS = 0, the GAP parameter is not stored in the header.
!
!</Help>
!
!<Help KEYWORD="OFF">
!<Tip> Array of offsets for specifying offset varying window times. </Tip>
! Default =  -
! Allowed = real (array)
! OFF is an array of offsets for specifying offset varying windows.  It is
! linked with WIN_TIM_BEG and WIN_TIM_END.
!</Help>
!
!<Help KEYWORD="WIN_TIM_BEG">
!<Tip> Array of window top times, in seconds . </Tip>
! Default = TSTRT
! Allowed = real>=TSTRT (array)
! WIN_TIM_BEG is an array of window top times for specifying offset varying
! windows.  It is linked with OFF and WIN_TIM_END.
!</Help>
!
!<Help KEYWORD="WIN_TIM_END">
!<Tip> Array of window bottom times, in seconds. </Tip>
! Default =  -
! Allowed = real (array)
! WIN_TIM_END is an array of window bottom times for specifying offset varying
! windows.  It is linked with OFF and WIN_TIM_BEG.
!</Help>
!
!<Help KEYWORD="HDR_INL">
!<Tip> Header word for inline coordinate. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Header word for inline coordinate to use in specifying grid and offset
! varying windows.
!
! HDR_INL must be non-zero for window variation in the inline direction.
!</Help>
!
!<Help KEYWORD="HDR_CRL">
!<Tip> Header word for crossline coordinate. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Header word for crossline coordinate to use in specifying grid and offset
! varying windows.
!
! HDR_CRL must be non-zero for window variation in the crossline direction.
!</Help>
!
!<Help KEYWORD="INLINES">
!<Tip> Array of inline coordinate values for spatially varying windows. </Tip>
! Default = 0
! Allowed = real (array)
! Array of inline coordinate values for specifying spatially varying windows.
!</Help>
!
!<Help KEYWORD="CROSSLINES">
!<Tip> Array of crossline coordinate values for spatially varying windows.</Tip>
! Default = 0
! Allowed = real (array)
! Array of crossline coordinate values for specifying spatially varying windows.
!</Help>
!
!<Help KEYWORD="TIM_REF_BEG">
!<Tip> Array of top window reference times for spatially varying windows .</Tip>
! Default = 0
! Allowed = real (array)
! Actual spatially varying window top times are the sum of the times specified
! by OFF and WIN_TIM_BEG and the spatially varying reference time TIM_REF_BEG.
!
! Local spatially varying reference time is interpolated between entries of
! TIM_REF_BEG.
!</Help>
!
!<Help KEYWORD="TIM_REF_END">
!<Tip> Window bottom reference times for spatially varying windows. </Tip>
! Default = 0
! Allowed = real (array)
! Actual spatially varying window bottom times are the sum of the times
! specified by OFF and WIN_TIM_END and the spatially varying reference time
! TIM_REF_END.
!
! Local spatially varying reference time is interpolated between entries of
! TIM_REF_END.
!</Help>
!
!<Help KEYWORD="OFFSET">
!<Tip> Array of offsets for specifying window times based on mute time. </Tip>
! Default =  -
! Allowed = real (array)
! OFFSET is an array of offsets for specifying window times based on mute time.
! It is linked with TIM_ADD and WIN_TIM_END.
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Time at top of window = mute time + TIM_ADD. </Tip>
! Default =  -
! Allowed = real (array)
! TIM_ADD is an array of times linked with OFFSET and WIN_LEN such that
! time at top of window = mute time + TIM_ADD.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Time at bottom of window = time at top of window + WIN_LEN. </Tip>
! Default =  -
! Allowed = real (array)
! WIN_LEN is an array of times linked with OFFSET and TIM_ADD such that
! time at bottom of window = time at top of window + WIN_LEN.
!</Help>
!
!<Help KEYWORD="TAU_P_VEL">
!<Tip> Reference velocity for gap decon in Tau-P domain. </Tip>
! Default = 0.0
! Allowed = real>=0.0
! See Advice for Users for details.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module decon_module
      use pc_module
      use named_constants_module
      use pathcheck_module
      use lav_module
      USE fft_module
      USE mth_module
      USE trcio_module
      USE opfilt_module
      USE mutehw_module
      USE interp_module
      USE fltr_module
      USE rcsum_module
      implicit none
      private
      public :: decon_create     ! uses the parameter cache.
      public :: decon_initialize
      public :: decon_update     ! uses the parameter cache.
      public :: decon_delete
!<execute_only>
      public :: decon            ! main execution (trace processing) routine.
      public :: decon_wrapup
!</execute_only>

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer, parameter :: MAX_OFF = 10
      integer, parameter :: MAX_OFFSET = 50
      integer, parameter :: MAX_COORD = 100

      type,public :: decon_struct
      private
      logical                      :: skip_wrapup      ! dependent parameter

      character(len=10)            :: opt_decon_type   ! process parameter.
      logical                      :: saile            ! process parameter.
      character(len=6)             :: opt_tr_mode      ! process paramater.
      character(len=FILENAME_LENGTH) :: pathname       ! process parameter.
      real                         :: len_op           ! process parameter.
      real                         :: gap              ! process parameter.
      real                         :: diag_load        ! process parameter.
      integer                      :: tr_init          ! process parameter.
      integer                      :: tr_inc           ! process parameter.
      integer                      :: tr_last          ! process parameter.
      integer                      :: hdr_off          ! process parameter.
      character(len=8)             :: opt_win          ! process paramater.
      double precision, dimension(MAX_OFF) :: off      ! process parameter.
      real, dimension(MAX_OFF)     :: win_tim_beg      ! process parameter.
      real, dimension(MAX_OFF)     :: win_tim_end      ! process parameter.
      integer                      :: hdr_inl          ! process parameter.
      integer                      :: hdr_crl          ! process parameter.
      real, dimension(MAX_COORD)   :: inlines          ! process parameter.
      real, dimension(MAX_COORD)   :: crosslines       ! process parameter.
      real, dimension(MAX_COORD)   :: tim_ref_beg      ! process parameter.
      real, dimension(MAX_COORD)   :: tim_ref_end      ! process parameter.
      double precision, dimension(MAX_OFFSET) :: offset ! process parameter.
      real, dimension(MAX_OFFSET)  :: tim_add          ! process parameter.
      real, dimension(MAX_OFFSET)  :: win_len          ! process parameter.
      real                         :: tau_p_vel        ! process parameter.
      integer                      :: zero_cross       ! process parameter.
      integer                      :: hdr_zcross       ! process parameter.

      integer                      :: nwih             ! global parameter
      integer                      :: ndpt             ! global parameter
      real                         :: tstrt            ! global parameter
      real                         :: dt               ! global parameter

      integer                      :: zcgap            ! dependent parameter.
      integer                      :: oplen_pts        ! dependent parameter.
      integer                      :: oplen2_pts       ! dependent parameter.
      integer                      :: gap_pts          ! dependent parameter.
      integer                      :: off_pts          ! dependent parameter.
      integer                      :: count_win        ! dependent parameter.
      integer                      :: offset_pts       ! dependent parameter.
      integer                      :: pred_len         ! dependent parameter.
      integer                      :: npow2            ! dependent parameter.
      real                         :: fftfact          ! dependent parameter

      type(fft_struct),pointer     :: fft_inverse      ! dependent parameter
      type(fft_struct),pointer     :: fft_forward      ! dependent parameter
      type(trcio_struct),pointer   :: trcio_file       ! dependent parameter
      real                         :: diag_fact        ! dependent parameter
      real                         :: tau_p_fact       ! dependent parameter
      integer                      :: traces_processed ! dependent parameter.
      real, pointer, dimension(:)  :: cross_filt       ! dependent parameter.
      real, pointer, dimension(:)  :: pred_filt        ! dependent parameter.
      real, pointer, dimension(:)  :: real_filt        ! dependent parameter.
      integer                      :: process_num      ! dependent parameter.
      end type decon_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(decon_struct),pointer,save :: object      ! needed for traps.

      integer :: print_lun = 0                       ! state variable
                 ! default = pc_get_lun()
                 ! valid = 0, do not print
                 !       > 0, Fortran print LUN for system output

      integer, parameter :: MAX_LINE = 132            ! parameter

      double precision :: double_dummy
      integer, parameter :: KIND_DBLE = kind(double_dummy)

      character(len=100),public :: decon_ident = &
        "$Id: decon.f90,v 1.47 2006/09/11 13:15:44 Stoeckley prod sps $"

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine decon_create (obj)
      implicit none
      type(decon_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%cross_filt)
      nullify (obj%pred_filt)
      nullify (obj%real_filt)
      nullify (obj%fft_inverse)
      nullify (obj%fft_forward)
      nullify (obj%trcio_file)

      call decon_initialize (obj)

      return
      end subroutine decon_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine decon_delete (obj)
      implicit none
      type(decon_struct),pointer :: obj       ! arguments

!<execute_only>
      call decon_wrapup (obj)

      if (associated(obj%cross_filt )) deallocate (obj%cross_filt)
      if (associated(obj%pred_filt )) deallocate (obj%pred_filt)
      if (associated(obj%real_filt )) deallocate (obj%real_filt)

      call fft_delete(obj%fft_inverse)
      call fft_delete(obj%fft_forward)
!</execute_only>

      deallocate(obj)

      return
      end subroutine decon_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine decon_initialize (obj)
      implicit none
      type(decon_struct),pointer :: obj       ! arguments

      call pc_get_global ('tstrt'  , obj%tstrt)
      call pc_get_global ('dt'  , obj%dt)
      call pc_get_global ('ndpt'  , obj%ndpt)

      obj%opt_decon_type = 'DECON'
      obj%saile = .false.
      obj%opt_tr_mode = 'SINGLE'
      obj%pathname = PATHCHECK_EMPTY
      obj%len_op = obj%dt * nint(0.200 / obj%dt)
      obj%gap = 0.0
      obj%diag_load = 2.0
      obj%tr_init = 1
      obj%tr_inc = 1
      obj%tr_last = HUGE(obj%tr_last)
      obj%hdr_off = 6
      obj%opt_win = 'OFF_GRID'
      obj%off = 0.0
      obj%win_tim_beg = 0.0
      obj%win_tim_end = 0.0
      obj%win_tim_beg(1) = obj%tstrt
      obj%win_tim_end(1) = obj%tstrt + (obj%ndpt - 1)*obj%dt
      obj%hdr_inl = 0
      obj%hdr_crl = 0
      obj%inlines = 0.0
      obj%crosslines = 0.0
      obj%tim_ref_beg = 0.0
      obj%tim_ref_end = 0.0
      obj%offset = 0.0
      obj%tim_add = 0.0
      obj%win_len = 0.0
      obj%tau_p_vel = 0.0
      
      obj%zero_cross = 0
      obj%hdr_zcross = 0
      obj%zcgap = 0

      obj%oplen_pts = 0
      obj%oplen2_pts = 0
      obj%gap_pts = 0
      obj%off_pts = 1
      obj%count_win = 1
      obj%offset_pts = 0
      obj%pred_len = 0
      obj%npow2 = 0
      obj%fftfact = 0.0
      obj%diag_fact = 0.0
      obj%tau_p_fact = 0.0
      obj%traces_processed = 0
      obj%process_num = 0

      print_lun = 0

      call decon_update (obj)

      return
      end subroutine decon_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine decon_update (obj)
      implicit none
      type(decon_struct),target :: obj                           ! arguments


      integer  :: off_do ! local

      integer  :: offset_do ! local

      integer :: IWORKSIZ ! local
      integer :: NTEST ! local
      integer :: gap1_pts ! local
      integer :: oplen1_pts ! local
      integer :: off_pts2, off_pts3
      integer :: offset_pts2, offset_pts3
      integer :: count_win2, count_win3, count_win4
      integer :: permanent_ram_words, temporary_ram_words
      integer :: ier1, ier2, status
      integer :: state
      logical gathered

      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

      state = pc_get_update_state()

!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!

      call pc_register_array_names ("off_arrayset", (/  &
                                    "off        ",      &
                                    "win_tim_beg",      &
                                    "win_tim_end" /))

      call pc_register_array_names ("inlines_arrayset", (/  &
                                    "inlines    ",          &
                                    "crosslines ",          &
                                    "tim_ref_beg",          &
                                    "tim_ref_end" /))

      call pc_register_array_names ("offset_arrayset", (/  &
                                    "offset ",             &
                                    "tim_add",             &
                                    "win_len" /))

      call pc_get_global ('tstrt'  , obj%tstrt)
      call pc_get_global ('dt'  , obj%dt)
      call pc_get_global ('ndpt'  , obj%ndpt)
      call pc_get_global ('nwih'  , obj%nwih)
      call pc_get_global ('gathered'  , gathered)

      call pc_get ('opt_decon_type', obj%opt_decon_type)
      call string_to_upper(obj%opt_decon_type)
      call pc_get ('saile', obj%saile)
      call pc_get ('opt_tr_mode', obj%opt_tr_mode)
      call string_to_upper(obj%opt_tr_mode)
      call pc_get ('pathname', obj%pathname)
      call pc_get ('len_op', obj%len_op)
      call pc_get ('gap', obj%gap)
      call pc_get ('diag_load', obj%diag_load)
      call pc_get ('tr_init', obj%tr_init)
      call pc_get ('tr_inc', obj%tr_inc)
      call pc_get ('tr_last', obj%tr_last)
      call pc_get ('hdr_off', obj%hdr_off)
      call pc_get ('opt_win', obj%opt_win)
      call string_to_upper(obj%opt_win)

      off_pts2 = obj%off_pts
      off_pts3 = obj%off_pts
      call pc_get ('off', obj%off, obj%off_pts)
      call pc_get ('win_tim_beg', obj%win_tim_beg, off_pts2)
      call pc_get ('win_tim_end', obj%win_tim_end, off_pts3)

      call pc_get ('hdr_inl', obj%hdr_inl)
      call pc_get ('hdr_crl', obj%hdr_crl)

      count_win2 = obj%count_win
      count_win3 = obj%count_win
      count_win4 = obj%count_win
      call pc_get ('inlines', obj%inlines, obj%count_win)
      call pc_get ('crosslines', obj%crosslines, count_win2)
      call pc_get ('tim_ref_beg', obj%tim_ref_beg, count_win3)
      call pc_get ('tim_ref_end', obj%tim_ref_end, count_win4)

      offset_pts2 = obj%offset_pts
      offset_pts3 = obj%offset_pts
      call pc_get ('offset', obj%offset, obj%offset_pts)
      call pc_get ('tim_add', obj%tim_add, offset_pts2)
      call pc_get ('win_len', obj%win_len, offset_pts3)

      call pc_get ('tau_p_vel', obj%tau_p_vel)

      call pc_get ('zero_cross', obj%zero_cross)
      call pc_get ('hdr_zcross', obj%hdr_zcross)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%dt <= 0.0) then
        call pc_error('DT must be greater than zero')
        return
      end if

      if(obj%opt_decon_type /= 'NONE' .and. &
        (obj%len_op <= 0.0 .or. obj%len_op == FNIL)) then
        call pc_warning('defaulting LEN_OP to be greater than 0.0')
        obj%len_op = obj%dt * nint(0.200 / obj%dt)
      end if

      select case (obj%opt_decon_type(1:1))
      case ('D')
        obj%opt_decon_type = 'DECON'
        obj%oplen_pts = MAX(NINT(obj%len_op/obj%dt + 1.),4)
        obj%len_op = (obj%oplen_pts - 1)*obj%dt
      case ('Z')
        obj%opt_decon_type = 'ZERO_PHASE'
        obj%oplen2_pts = MAX(NINT(obj%len_op/obj%dt),2)
        obj%oplen_pts = 2*obj%oplen2_pts + 1
        obj%len_op = obj%oplen2_pts*obj%dt
        obj%gap = 0.
      case ('N')
        obj%opt_decon_type = 'NONE'
        obj%opt_tr_mode = 'SINGLE'
        obj%oplen_pts = 1
        obj%len_op = 0.0
        obj%gap = 0.
      case default
        call pc_error('OPT_DECON_TYPE must be Decon, Zero_phase or None')
        obj%opt_decon_type = 'DECON'
        obj%oplen_pts = 1
      end select

      select case (obj%opt_tr_mode(1:1))
      case ('S')
        obj%opt_tr_mode = 'SINGLE'
        obj%tr_init = 1
        obj%tr_inc = 1
      case ('M')
        obj%opt_tr_mode = 'MULT'
        if(.not.gathered) then
          call pc_error('this process must be preceded by a gather, ' // &
            'when OPT_TR_MODE=MULT')
        end if
      case ('A')
        obj%opt_tr_mode = 'ALL'

        if(state == PC_FRONTEND .or. state == PC_BACKEND .or. &
          pc_verify_scalar('pathname')) then
          call pathcheck('pathname', obj%pathname, ext='.trcio', &
            required=.true., status=status)

          if(status /= PATHCHECK_VALID) then
            call pc_error('valid PATHNAME is required when OPT_TR_MODE = ALL')
          end if
        end if
      case default
        call pc_error('OPT_TR_MODE must be Single, Mult or All')
        obj%opt_tr_mode = 'SINGLE'
      end select

      if(obj%saile .and. obj%opt_tr_mode == 'ALL') then
        call pc_error('SAILE=YES not allowed with OPT_TR_MODE=ALL')
      end if

      if(obj%diag_load < 0.0) then
        call pc_error('DIAG_LOAD must be greater than or equal to 0.0')
        obj%diag_load = 2.0
      end if

      if(obj%opt_tr_mode == 'MULT' .or. &
         obj%opt_tr_mode == 'ALL') then
        if(obj%tr_init <= 0) then
          call pc_error('TR_INIT must be greater than 0')
          obj%tr_init = 1
        end if

        if(obj%tr_inc <= 0) then
          call pc_error('TR_INC must be greater than 0')
          obj%tr_inc = 1
        end if

        if(obj%tr_last < obj%tr_init) then
          call pc_error('TR_LAST must be greater than or equal to TR_INIT')
        end if
      end if

      select case (obj%opt_win(1:1))
      case ('O')
        obj%opt_win = 'OFF_GRID'
      case ('M')
        obj%opt_win = 'MUTE'
      case default
        call pc_error('OPT_WIN must be Off_grid or Mute')
        obj%opt_win = 'OFF_GRID'
      end select

      if(obj%opt_win == 'OFF_GRID') then
        if(obj%off_pts /= off_pts2 .or. &
          obj%off_pts /= off_pts3) then
          call pc_error("arrays not linked (OFF, WIN_TIM_BEG, WIN_TIM_END)")
        end if

        !! 2000-03-10 Selzler, based upon user feedback and consultation
        !! with Chuck I Burch, the following logic from the old CPS has
        !! been commented out (one "if)
        !! if(obj%off_pts == 1) obj%off(1) = 0.0

        if(obj%hdr_inl < 0 .or. obj%hdr_inl > obj%nwih) then
          call pc_error('HDR_INL must be 0 through NWIH')
          obj%hdr_inl = 0
        end if

        if(obj%hdr_crl < 0 .or. obj%hdr_crl > obj%nwih) then
          call pc_error('HDR_CRL must be 0 through NWIH')
          obj%hdr_crl = 0
        end if

        if(obj%count_win /= count_win2 .or. &
           obj%count_win /= count_win3 .or. &
           obj%count_win /= count_win4) then
          call pc_error("arrays not linked (IN/CROSSLINES, TIM_REF_BEG/END)")
        end if
      else if(obj%opt_win == 'MUTE') then
        !! 2000-03-10 Selzler, based upon user feedback and consultation
        !! with Chuck I Burch, the following logic from the old CPS has
        !! been commented out (one "if)
        !! if(obj%offset_pts == 1) obj%offset(1) = 0.0

        if(obj%offset_pts /= offset_pts2 .or. &
           obj%offset_pts /= offset_pts3) then
         call pc_error("arrays not linked (OFFSET, TIM_ADD, WIN_LEN)")
        end if
      end if

      if(obj%hdr_off <= 0 .or. obj%hdr_off > obj%nwih) then
        call pc_error('HDR_OFF must be 1 through NWIH')
        obj%hdr_off = 6
      end if

      if(obj%gap < 0.0) then
        call pc_error('GAP must be greater than or equal to 0.0')
        obj%gap = 0.0
      else if(obj%gap /= 0.0 .and. obj%opt_decon_type == 'ZERO_PHASE') then
        call pc_error('GAP must be zero, if OPT_DECON_TYPE is Zero_phase')
        obj%gap = 0.0
      end if

      obj%gap_pts = NINT(obj%gap/obj%dt)

      if(obj%zero_cross < 0 .or. obj%zero_cross > 2) then
        call pc_error('ZERO_CROSS must be between 0 and 2')
        obj%zero_cross = 0
      end if

      if (obj%zero_cross > 0) then
        obj%gap = 0.0
        obj%gap_pts = NINT(obj%gap/obj%dt)
      end if

      if(obj%hdr_zcross < 0 .or. obj%hdr_zcross > obj%nwih) then
        call pc_error('HDR_ZCROSS must be 0 through NWIH')
        obj%hdr_zcross = 0
      end if

      if(0 /= mth_compare(obj%gap, obj%gap_pts*obj%dt)) then
        call pc_warning('Rounding GAP to nearest DT')
        obj%gap = obj%gap_pts*obj%dt
      end if

      IF (obj%gap==0.0 .OR. obj%opt_tr_mode/='SINGLE') obj%tau_p_vel = 0.

      obj%pred_len = obj%oplen_pts - (obj%gap_pts + 1)

      IF (obj%opt_decon_type/='NONE' .AND. obj%pred_len<1) THEN
        call pc_error('GAP too long for given LEN_OP')
      ENDIF

!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!

      call pc_put_options_field('opt_decon_type', &
        (/ "DECON     ", "ZERO_PHASE", "NONE      " /), 3)
      call pc_put ('opt_decon_type', obj%opt_decon_type)
      call pc_put_options_field('saile', (/ "YES", "NO " /), 2)
      call pc_put ('saile', obj%saile)
      call pc_put_options_field('opt_tr_mode', &
        (/ "SINGLE", "MULT  ", "ALL   " /), 3)
      call pc_put ('opt_tr_mode', obj%opt_tr_mode)

      call pc_put ('pathname', obj%pathname)
      if(obj%opt_tr_mode == 'ALL') then
        call pc_put_sensitive_field_flag('pathname', .true.)
      else
        call pc_put_sensitive_field_flag('pathname', .false.)
      end if

      call pc_put ('len_op', obj%len_op)
      call pc_put ('gap', obj%gap)
      call pc_put ('diag_load', obj%diag_load)
      call pc_put ('tr_init', obj%tr_init)
      call pc_put ('tr_inc', obj%tr_inc)
      call pc_put ('tr_last', obj%tr_last)

      if(obj%opt_tr_mode == 'MULT' .or. obj%opt_tr_mode == 'ALL') then
        call pc_put_sensitive_field_flag('tr_init', .true.)
        call pc_put_sensitive_field_flag('tr_inc', .true.)
        call pc_put_sensitive_field_flag('tr_last', .true.)

      else if(obj%opt_tr_mode == 'SINGLE') then
        call pc_put_sensitive_field_flag('tr_init', .false.)
        call pc_put_sensitive_field_flag('tr_inc', .false.)
        call pc_put_sensitive_field_flag('tr_last', .false.)
      end if

     if (obj%zero_cross > 0) then
        call pc_put_sensitive_field_flag('gap', .false.)
        call pc_put_sensitive_field_flag('hdr_zcross', .true.)
        obj%gap = 0.0
     else 
        call pc_put_sensitive_field_flag('gap', .true.)
        call pc_put_sensitive_field_flag('hdr_zcross', .false.)
     end if

     if (obj%tau_p_vel > 0) then
        call pc_put_sensitive_field_flag('zero_cross', .false.)
     else 
        call pc_put_sensitive_field_flag('zero_cross', .true.)
     end if

      call pc_put ('hdr_off', obj%hdr_off)
      call pc_put_options_field('opt_win', (/ "OFF_GRID", "MUTE    " /), 2)
      call pc_put ('opt_win', obj%opt_win)
      call pc_put ('off', obj%off, obj%off_pts)
      call pc_put ('win_tim_beg', obj%win_tim_beg, obj%off_pts)
      call pc_put ('win_tim_end', obj%win_tim_end, obj%off_pts)
      call pc_put ('hdr_inl', obj%hdr_inl)
      call pc_put ('hdr_crl', obj%hdr_crl)
      call pc_put ('inlines', obj%inlines, obj%count_win)
      call pc_put ('crosslines', obj%crosslines, obj%count_win)
      call pc_put ('tim_ref_beg', obj%tim_ref_beg, obj%count_win)
      call pc_put ('tim_ref_end', obj%tim_ref_end, obj%count_win)
      call pc_put ('offset', obj%offset, obj%offset_pts)
      call pc_put ('tim_add', obj%tim_add, obj%offset_pts)
      call pc_put ('win_len', obj%win_len, obj%offset_pts)
      call pc_put ('tau_p_vel', obj%tau_p_vel)
      call pc_put ('zero_cross', obj%zero_cross)
      call pc_put ('hdr_zcross', obj%hdr_zcross)

      if(obj%opt_win == 'OFF_GRID') then
        call pc_put_sensitive_arrayset_flag('off_arrayset', .true.)
        call pc_put_sensitive_field_flag('hdr_inl', .true.)
        call pc_put_sensitive_field_flag('hdr_crl', .true.)
        call pc_put_sensitive_arrayset_flag('offset_arrayset', .false.)
        call pc_put_sensitive_arrayset_flag('inlines_arrayset', .true.)
      else if(obj%opt_win == 'MUTE') then
        call pc_put_sensitive_arrayset_flag('offset_arrayset', .true.)

        call pc_put_sensitive_arrayset_flag('off_arrayset', .false.)
        call pc_put_sensitive_field_flag('hdr_inl', .false.)
        call pc_put_sensitive_field_flag('hdr_crl', .false.)
        call pc_put_sensitive_arrayset_flag('inlines_arrayset', .false.)
      end if

      if (obj%opt_tr_mode == 'MULT' .or. obj%opt_tr_mode == 'SINGLE') then
        call pc_put_control ('PARALLEL_SAFE'        ,.true.)
        call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
        call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
        call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
        call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
        call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
        call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')
      endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      print_lun = pc_get_lun()

      obj%diag_fact = 1.0 + obj%diag_load / 100.0

      IF (obj%opt_tr_mode == 'ALL') THEN
        allocate(obj%cross_filt(obj%oplen_pts))
        obj%cross_filt = 0.0
        allocate(obj%pred_filt(obj%oplen_pts))
        permanent_ram_words = obj%oplen_pts
        temporary_ram_words = obj%oplen_pts
      ELSE
        IF (obj%opt_decon_type /= 'NONE') THEN
          allocate(obj%cross_filt(obj%oplen_pts))
          allocate(obj%pred_filt(obj%oplen_pts))
          temporary_ram_words = 2*obj%oplen_pts
        ELSE
          temporary_ram_words = 0
        ENDIF
        permanent_ram_words = 0
      endif

      IF (obj%opt_decon_type /= 'NONE') THEN
        IWORKSIZ = MAX(2*obj%oplen_pts,obj%ndpt)
        IF (obj%opt_decon_type == 'ZERO_PHASE') THEN
          NTEST = 2*obj%oplen_pts
          obj%npow2 = 4
          obj%npow2 = 2*obj%npow2

          DO WHILE(obj%npow2 < NTEST)
            obj%npow2 = 2*obj%npow2
          END DO

          obj%fftfact = 1.0/obj%npow2
          IWORKSIZ = MAX(IWORKSIZ,2*obj%npow2 + 2)

          ier1 = fft_create(obj%fft_inverse, -1, obj%npow2, 'rtoc')
          ier2 = fft_create(obj%fft_forward, +1, obj%npow2, 'ctor')
        endif
      ELSE IF (obj%saile) THEN
        IWORKSIZ = obj%ndpt
      ELSE
        GO TO 800
      ENDIF

      allocate(obj%real_filt(IWORKSIZ))

      temporary_ram_words = temporary_ram_words + IWORKSIZ

      IF (obj%opt_decon_type /= 'NONE') THEN
        IF (obj%opt_tr_mode == 'ALL') temporary_ram_words = &
          MAX(temporary_ram_words,12336)

        IF (obj%tau_p_vel > 0.) obj%tau_p_fact = (obj%tau_p_vel/1.E6)**2

        IF (obj%opt_win == 'OFF_GRID') THEN
          IF (obj%off_pts == 1) THEN
            obj%win_tim_beg(1) = MAX(obj%win_tim_beg(1),obj%tstrt)
            obj%win_tim_end(1) = MIN(obj%win_tim_end(1),obj%tstrt + &
              (obj%ndpt-1)*obj%dt)
            obj%win_tim_beg(1) = (obj%win_tim_beg(1)-obj%tstrt)/obj%dt + 1.
            obj%win_tim_end(1) = (obj%win_tim_end(1)-obj%tstrt)/obj%dt + 1.
            gap1_pts = NINT(obj%gap_pts*SQRT( &
              MAX(1._KIND_DBLE - obj%tau_p_fact*obj%off(1)**2,0._KIND_DBLE)))
            oplen1_pts = gap1_pts + obj%pred_len + 1

            IF (MIN(NINT(obj%win_tim_end(1)),obj%ndpt) - &
              MAX(NINT(obj%win_tim_beg(1)),1) + 1 < oplen1_pts) then
              call pc_error('=>DECONS:   Window Length less than LEN_OP')
              RETURN
            endif
          ELSE
            DO off_do = 1, obj%off_pts
              obj%win_tim_beg(off_do) = (obj%win_tim_beg(off_do)- &
                obj%tstrt)/obj%dt + 1.
              obj%win_tim_end(off_do) = (obj%win_tim_end(off_do)- &
                obj%tstrt)/obj%dt + 1.
              gap1_pts = NINT(obj%gap_pts*SQRT( &
                MAX(1._KIND_DBLE - obj%tau_p_fact*obj%off(off_do)**2, &
                  0._KIND_DBLE)))
              oplen1_pts = gap1_pts + obj%pred_len + 1

              IF (MIN(NINT(obj%win_tim_end(off_do)),obj%ndpt) -  &
                MAX(NINT(obj%win_tim_beg(off_do)),1) + 1 >= oplen1_pts) CYCLE

              write(print_lun, *) &
                ' *** DECONS WARNING ***    At Offset number ', off_do, &
                ',   Window Length < LEN_OP'
              write(print_lun, *) '    (DECON will kill traces when window ', &
                'inadequate)'
            END DO
          ENDIF
        ELSE
          ! OPT_WIN == "MUTE"
          IF (obj%offset_pts == 1) THEN
            gap1_pts = NINT(obj%gap_pts*SQRT( &
              MAX(1._KIND_DBLE - obj%tau_p_fact*obj%offset(1)**2, &
                0._KIND_DBLE)))
            oplen1_pts = gap1_pts + obj%pred_len + 1

            IF (NINT(obj%win_len(1)/obj%dt+1.) < oplen1_pts) then
              call pc_error('=>DECONS:   Window Length less than LEN_OP')
              RETURN
            endif
          ELSE
            DO offset_do = 1, obj%offset_pts
              gap1_pts = NINT(obj%gap_pts*SQRT( &
                MAX(1._KIND_DBLE - obj%tau_p_fact*obj%offset(offset_do)**2, &
                  0._KIND_DBLE)))
              oplen1_pts = gap1_pts + obj%pred_len + 1

              IF (NINT(obj%win_len(offset_do)/obj%dt+1.) >= oplen1_pts) CYCLE

              write(print_lun, *) &
                ' *** DECONS WARNING ***    At Offset number ', offset_do, &
                ',   Window Length < LEN_OP'
              write(print_lun, *) '    (DECON will kill traces when window ', &
                'inadequate)'
            END DO
          ENDIF
        ENDIF
      ENDIF

  800 CONTINUE

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine decon_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine decon (obj,ntr,hd,tr)
      implicit none
      type(decon_struct),intent(inout) :: obj           ! arguments
      integer,intent(inout)            :: ntr           ! arguments
      double precision,intent(inout)   :: hd(:,:)       ! arguments
      real   ,intent(inout)            :: tr(:,:)       ! arguments

      integer :: off_do
      INTEGER ::         ntr_do, tr_stop 
      integer :: filtrgs_0_sum_or_1_set_results, gap1_pts, oplen1_pts
      integer :: idx_win_beg, idx_win_end, idx_beg, idx_end, idx_win_pts
      integer :: offset_do, LTRACE
      double precision :: tr_offset
      real :: dh_dt, time_beg, time_end
      REAL, dimension(1) :: tmp_beg, tmp_end

      IF (ntr == 0) THEN
        call decon_wrapup(obj)
        return
      ENDIF
!
!  Come here if not end of job yet:
!
      DO ntr_do = 1, ntr
!       !sure mute hw set
        CALL MUTEHW (HD(:,ntr_do), TR(:obj%ndpt,ntr_do), obj%ndpt, &
          0.0, MUTEHW_SET)
      END DO

      IF (obj%opt_decon_type /= 'NONE') THEN
        IF (obj%opt_tr_mode == 'SINGLE') THEN
          tr_stop = ntr
          filtrgs_0_sum_or_1_set_results = 1
        ELSE
          tr_stop = MIN(obj%tr_last,ntr)
          filtrgs_0_sum_or_1_set_results = 0
        ENDIF

        IF (obj%tau_p_vel == 0.) THEN
          gap1_pts = obj%gap_pts
          oplen1_pts = obj%oplen_pts
        ENDIF

        IF (obj%opt_tr_mode == 'MULT') obj%cross_filt(:obj%oplen_pts) = 0.0

        window_loop: DO ntr_do = obj%tr_init, tr_stop, obj%tr_inc
!
!  Compute autocorrelation from appropriate window of trace:
!
          tr_offset = HD(obj%hdr_off,ntr_do)

          IF (obj%opt_win == 'OFF_GRID') THEN
            IF (obj%hdr_inl>0 .AND. obj%hdr_crl>0) THEN
              CALL interp_2d_var_lin_real (obj%inlines, obj%crosslines, &
                obj%count_win, obj%count_win, obj%tim_ref_beg, 1, &
                sngl(HD(obj%hdr_inl,ntr_do)), &
                sngl(HD(obj%hdr_crl,ntr_do)), tmp_beg)

              idx_win_beg = NINT((tmp_beg(1) - obj%tstrt)/obj%dt)

              CALL interp_2d_var_lin_real (obj%inlines, obj%crosslines, &
                obj%count_win, obj%count_win, obj%tim_ref_end, 1, &
                sngl(HD(obj%hdr_inl,ntr_do)), &
                sngl(HD(obj%hdr_crl,ntr_do)), tmp_end)

              idx_win_end = NINT((tmp_end(1) - obj%tstrt)/obj%dt)
            ELSE
              idx_win_beg = 0
              idx_win_end = 0
            ENDIF

            IF (obj%off_pts==1 .OR. tr_offset<=obj%off(1)) THEN
              idx_beg = idx_win_beg + NINT(obj%win_tim_beg(1))
              idx_end = idx_win_end + NINT(obj%win_tim_end(1))
            ELSE IF (tr_offset >= obj%off(obj%off_pts)) THEN
              idx_beg = idx_win_beg + NINT(obj%win_tim_beg(obj%off_pts))
              idx_end = idx_win_end + NINT(obj%win_tim_end(obj%off_pts))
            ELSE
              DO off_do = 2, obj%off_pts
                IF (tr_offset < obj%off(off_do)) EXIT
              END DO

              dh_dt = (tr_offset - obj%off(off_do-1))/ &
                (obj%off(off_do)- obj%off(off_do-1))

              idx_beg = idx_win_beg + &
                NINT(obj%win_tim_beg(off_do-1)+dh_dt* &
                  (obj%win_tim_beg(off_do)-obj%win_tim_beg(off_do-1)))

              idx_end = idx_win_end + &
                NINT(obj%win_tim_end(off_do-1)+ &
                dh_dt*(obj%win_tim_end(off_do)-obj%win_tim_end(off_do-1)))
            ENDIF

            idx_beg = MAX(idx_beg,NINT(HD(2,ntr_do)),1)
            idx_end = MIN(idx_end,NINT(HD(64,ntr_do)),obj%ndpt)
            idx_win_pts = idx_end - idx_beg + 1
          ELSE
            ! OPT_WIN == "MUTE"
            IF (obj%offset_pts==1 .OR. tr_offset<=obj%offset(1)) THEN
              time_beg = HD(2,ntr_do)*obj%dt + obj%tim_add(1)
              time_end = time_beg + obj%win_len(1)
            ELSE IF (tr_offset >= obj%offset(obj%offset_pts)) THEN
              time_beg = HD(2,ntr_do)*obj%dt + obj%tim_add(obj%offset_pts)
              time_end = time_beg + obj%win_len(obj%offset_pts)
            ELSE
              DO offset_do = 2, obj%offset_pts
                IF (tr_offset < obj%offset(offset_do)) EXIT
              END DO

              dh_dt = (tr_offset - obj%offset(offset_do-1))/ &
                (obj%offset(offset_do)-obj%offset(offset_do-1))

              time_beg = HD(2,ntr_do)*obj%dt + (obj%tim_add(offset_do-1)+ &
                dh_dt*(obj%tim_add(offset_do)-obj%tim_add(offset_do-1)))

              time_end = time_beg + (obj%win_len(offset_do-1)+dh_dt* &
                (obj%win_len(offset_do)-obj%win_len(offset_do-1)))
            ENDIF

            idx_beg = MIN(NINT(time_beg/obj%dt) + 1,obj%ndpt)
            idx_end = MIN(NINT(time_end/obj%dt) + 1,obj%ndpt)
            idx_win_pts = idx_end - idx_beg + 1
          ENDIF

          IF (obj%tau_p_vel > 0.) THEN
            gap1_pts = NINT(obj%gap_pts*SQRT( &
              MAX(1._KIND_DBLE - obj%tau_p_fact*tr_offset**2, &
                0._KIND_DBLE)))

            oplen1_pts = gap1_pts + obj%pred_len + 1
          ENDIF

          IF (idx_win_pts < oplen1_pts) CYCLE

          CALL fltr_filtrgs (TR(idx_beg:,ntr_do), idx_win_pts, &
            TR(idx_beg:,ntr_do), idx_win_pts, obj%cross_filt, &
            oplen1_pts, filtrgs_0_sum_or_1_set_results, 0)

          IF (obj%cross_filt(1) == 0.) CYCLE

          IF (obj%opt_tr_mode == 'SINGLE') THEN
!
!  Derive operator & apply it if doing single-trace decon:
!
     
            if (obj%zero_cross > 0) then
              call decon_zero_cross (obj, oplen1_pts)       
              obj%gap_pts = obj%zcgap 
              gap1_pts = obj%gap_pts
              obj%pred_len = obj%oplen_pts - (obj%gap_pts + 1)
            end if
     
            obj%cross_filt(1) = obj%cross_filt(1)*obj%diag_fact

            CALL OPFILT (obj%pred_len, obj%pred_filt, &
              obj%cross_filt(gap1_pts+2:gap1_pts+1+obj%pred_len), &
              obj%real_filt, obj%cross_filt)

            IF (obj%opt_decon_type == 'DECON') THEN
              obj%cross_filt(:obj%pred_len) = &
                obj%pred_filt(obj%pred_len:1:(-1))
              LTRACE = obj%ndpt - gap1_pts - 1

              CALL fltr_filtrgs (obj%cross_filt, obj%pred_len, TR(1:,ntr_do), &
                LTRACE, obj%real_filt(gap1_pts+2:), &
                LTRACE, 1, 1 - obj%pred_len)

              TR(gap1_pts+2:obj%ndpt,ntr_do) = &
                TR(gap1_pts+2:obj%ndpt,ntr_do) - &
                obj%real_filt(gap1_pts+2:obj%ndpt)
            ELSE
              ! opt_decon_type == 'ZERO_PHASE'
              CALL decon_nrm (obj)

              obj%real_filt(:obj%ndpt) = TR(:obj%ndpt,ntr_do)

              CALL fltr_filtrgs (obj%pred_filt, obj%oplen_pts, &
                obj%real_filt, obj%ndpt, TR(1:,ntr_do), &
                obj%ndpt, 1, (-obj%oplen2_pts))
            ENDIF
          ELSE IF (obj%opt_tr_mode == 'ALL') THEN
            obj%traces_processed = obj%traces_processed + 1
          ENDIF

        END DO window_loop

        IF (obj%opt_tr_mode == 'MULT') THEN
!
!  Derive operator & apply it if doing multi-trace decon:
!

          IF (obj%cross_filt(1) /= 0.) THEN

            if (obj%zero_cross > 0) then
              call decon_zero_cross (obj, oplen1_pts)
              obj%gap_pts = obj%zcgap 
              gap1_pts = obj%gap_pts
              obj%pred_len = obj%oplen_pts - (obj%gap_pts + 1)
            end if

            obj%cross_filt(1) = obj%cross_filt(1)*obj%diag_fact

            CALL OPFILT (obj%pred_len, obj%pred_filt, &
              obj%cross_filt(obj%gap_pts+2:obj%gap_pts+1+obj%pred_len), &
              obj%real_filt, obj%cross_filt)

            IF (obj%opt_decon_type == 'DECON') THEN
              obj%cross_filt(:obj%pred_len) = obj%pred_filt(obj%pred_len:1:(-1))

              DO ntr_do = 1, ntr
                LTRACE = obj%ndpt - obj%gap_pts - 1

                CALL fltr_filtrgs (obj%cross_filt, obj%pred_len, &
                  TR(1:,ntr_do), LTRACE, obj%real_filt(obj%gap_pts+2:), &
                  LTRACE, 1, 1 - obj%pred_len)

                TR(obj%gap_pts+2:obj%ndpt,ntr_do) = &
                  TR(obj%gap_pts+2:obj%ndpt,ntr_do) - &
                  obj%real_filt(obj%gap_pts+2:obj%ndpt)
              END DO
            ELSE
              ! opt_decon_type == 'ZERO_PHASE'
              CALL decon_nrm (obj)

              DO ntr_do = 1, ntr
                obj%real_filt(:obj%ndpt) = TR(:obj%ndpt,ntr_do)

                CALL fltr_filtrgs (obj%pred_filt, obj%oplen_pts, &
                  obj%real_filt, obj%ndpt, TR(1:,ntr_do), obj%ndpt, 1, &
                    (-obj%oplen2_pts))
              END DO
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF (obj%saile) THEN
        DO ntr_do = 1, ntr
          obj%real_filt(:obj%ndpt) = TR(:obj%ndpt,ntr_do)

          CALL RCSUM (obj%ndpt, obj%real_filt, TR(:obj%ndpt,ntr_do))
        END DO
      ENDIF

      if (obj%hdr_zcross > 0 .and. obj%zero_cross > 0)  & 
         hd(obj%hdr_zcross,1:ntr) = obj%gap_pts*obj%dt

      if(ntr > 0 ) call lav_set_hdr(hd, tr, obj%ndpt, ntr)

      IF (obj%opt_tr_mode /= 'ALL') obj%traces_processed = &
        obj%traces_processed + ntr

      RETURN

!-----------------------------------------------------------------------

      END SUBROUTINE DECON

      SUBROUTINE decon_nrm (obj)
      IMPLICIT NONE

      type(decon_struct),intent(inout) :: obj  ! argument

      COMPLEX, DIMENSION(1+obj%npow2/2) :: cmpx_filt
!-----------------------------------------------------------------------
!  This subroutine computes the normalization operator for OPT_DECON_TYPE=NORM.
!  Input for this routine is taken from the array pred_filt which is assumed
!  to contain the spiking decon "prediction" operator of length obj%pred_len =
!  obj%oplen_pts-1.  The output is a two-sided (zero phase) operator which
!  overwrites the array pred_filt.  Its central (zero-time) value is located
!  in pred_filt(obj%oplen2_pts+1),  where obj%oplen_pts = 2*obj%oplen2_pts+1 .
!-----------------------------------------------------------------------
      obj%real_filt(:obj%pred_len) = obj%pred_filt(:obj%pred_len)

      obj%real_filt(obj%oplen_pts:obj%npow2 - 1) = 0.0

      obj%real_filt(obj%npow2) = -1.

      call fft_rc_transform(obj%fft_inverse, obj%real_filt, cmpx_filt)

      cmpx_filt = obj%fftfact*CABS(cmpx_filt)

      call fft_cr_transform(obj%fft_forward, cmpx_filt, obj%real_filt)

      obj%pred_filt(:obj%oplen2_pts) = obj%real_filt(obj%oplen2_pts+1:2:(-1))
      obj%pred_filt(obj%oplen2_pts+1:obj%oplen_pts) = &
        obj%real_filt(:obj%oplen_pts-obj%oplen2_pts)

      RETURN

      END SUBROUTINE decon_nrm

      SUBROUTINE decon_zero_cross (obj, oplen_pts)
      IMPLICIT NONE

      type(decon_struct),intent(inout) :: obj  ! argument

      integer                          :: oplen_pts   ! argument
      integer                          :: i, icount    ! local 
 
!-----------------------------------------------------------------------
!  This subroutine find the zero crossing of the autocorrelation
!-----------------------------------------------------------------------
      
      icount = 0 
      do i = 1, oplen_pts-1
        if (obj%cross_filt(i)*obj%cross_filt(i+1) < 0.0) then
          icount = icount + 1
          if (abs(obj%cross_filt(i))>abs(obj%cross_filt(i+1))) then
             obj%zcgap = i + 1
          else 
             obj%zcgap = i
          end if 
        end if
        if (obj%zero_cross == icount) exit
      end do
             
      RETURN

      END SUBROUTINE decon_zero_cross

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine decon_wrapup (obj)
      implicit none
      type(decon_struct),intent(inout) :: obj       ! arguments

      real, dimension(obj%ndpt) :: TR ! local
      double precision, dimension(obj%nwih) :: HD ! local
      character(len=80) :: prefix ! local

      integer :: ier

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      write(print_lun, *)

      if(obj%opt_decon_type == 'ZERO_PHASE') THEN
        write(print_lun,'(a,f7.4,a)') &
          ' =>DECON: Two-sided zero phase operator length is ', &
            2*obj%len_op, ' seconds'
      ENDIF

      prefix = ' =>DECON (OPT_TR_MODE=' // obj%opt_tr_mode // &
        ', OPT_DECON_TYPE=' // obj%opt_decon_type // &
        ', SAILE=' // string_ll2ss(obj%saile)

      IF (obj%opt_tr_mode == 'ALL') THEN
        IF (obj%cross_filt(1) == 0.) THEN
          write(print_lun, '(A80)') trim(prefix) // &
             'No live traces within windows.'

          WRITE (print_lun, '(44X,''No operator written.'')')
          return
        ENDIF


        if (obj%zero_cross > 0) then
          call decon_zero_cross (obj, obj%oplen_pts)
          obj%gap_pts = obj%zcgap 
          obj%pred_len = obj%oplen_pts - (obj%gap_pts + 1)
          obj%gap = obj%gap_pts*obj%dt
        end if

        obj%cross_filt(1) = obj%cross_filt(1)*obj%diag_fact

        CALL OPFILT (obj%pred_len, obj%pred_filt, &
          obj%cross_filt(obj%gap_pts+2:), obj%real_filt, obj%cross_filt)

        tr = 0.0

        IF (obj%opt_decon_type == 'DECON') THEN
          TR(1) = 1.
          TR(2+obj%gap_pts:obj%pred_len+1+obj%gap_pts) = &
            -obj%pred_filt(:obj%pred_len)
        ELSE
          CALL decon_nrm (obj)

          TR(:obj%oplen_pts) = obj%pred_filt(:obj%oplen_pts)
        ENDIF

        hd = 0.0

        HD(1) = 1.
        HD(2) = 1.
        HD(3) = 1.
        HD(4) = 1.
        if (obj%hdr_zcross > 0 .and. obj%zero_cross > 0)   &
           HD(obj%hdr_zcross) = obj%gap

        obj%trcio_file => trcio_open(obj%pathname, 'w')

        if(.not. associated(obj%trcio_file )) then
          call pc_error("trcio_open: error, pathname= " // obj%pathname)
          return
        end if

        obj%trcio_file%num_values = obj%oplen_pts

        if(obj%opt_decon_type == 'ZERO_PHASE') then
          obj%trcio_file%tmin = -obj%oplen2_pts*obj%dt
        else
          obj%trcio_file%tmin = 0.0
        end if

        obj%trcio_file%dt = obj%dt
        obj%trcio_file%nwih = obj%nwih
        obj%trcio_file%nbits = 32
        obj%trcio_file%nbits_hd = 64

        ier = trcio_writeheader(obj%trcio_file)

        if(ier /= TRCIO_OK) then
          call pc_error("trcio_writeheader: error, pathname= " // obj%pathname)
          return
        end if

        ier = trcio_write_trace(obj%trcio_file, HD, TR)
        if(ier /= TRCIO_OK) then
          call pc_error("trcio_write_trace: error, pathname= " // obj%pathname)
        end if

        ier = trcio_close(obj%trcio_file)

        if(ier /= TRCIO_OK) then
          call pc_error("trcio_close: error, pathname= " // obj%pathname)
        end if

        write(print_lun, '(A80)') 'Operator written to file ' // obj%pathname

        write(print_lun, '(44x,a,i12,a)') 'Based on', &
           obj%traces_processed, ' traces'
      ELSE
        IF (obj%opt_decon_type=='NONE' .AND. .not. obj%saile) THEN
          write(print_lun, '(A80)') trim(prefix) // 'No traces processed.'
          return
        ENDIF

        write(print_lun, '(a,i12)') trim(prefix) // 'Traces processed =', &
          obj%traces_processed
      ENDIF

      return
      end subroutine decon_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module decon_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
