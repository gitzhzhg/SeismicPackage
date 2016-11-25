!<CPS_v1 type="PROCESS"/>
!!------------------------------ scdecon.f90 -------------------------------!!
!!------------------------------ scdecon.f90 -------------------------------!!
!!------------------------------ scdecon.f90 -------------------------------!!
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
! Name       : SCDECON    (Surface Consistent DECONvolution)
! Category   : filters
! Written    : 1989-03-22   by: Greg Lazear
! Revised    : 2010-08-13   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : 2D and 3D surface consistent deconvolution and amplitude scaling.
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
! SCDECON calculates and applies to each trace a surface consistent (SC) decon
! operator and/or a SC amplitude scale factor.
!
! SCDECON can operate as either a one-pass or two-pass process.  If MODE = BOTH,
! SCDECON works in one pass by saving both the spectra and a copy of the traces
! on disk.  When operating as a two-pass process, the first pass (MODE = PREP)
! serves mainly to store the power spectra on disk.  The second pass (MODE =
! APPLY) then reads these spectra from disk, calculates the decon operators,
! and applies them.  The two pass method saves on disk space but the same data
! must be input in both passes.
!
!
! Simplified Operation Outline
!
!   For MODE = PREP or BOTH:
!
!       1.  For each input trace, calculate the autocorrelation of the trace 
!       within the specified window.  From the autocorrelation, calculate the 
!       log power spectrum.  (SCDECON uses a trick here to avoid negative
!       values.  We use the autocorrelation to produce a short sequence of
!       values for further computation - the trace window is probably much
!       longer than the autocorrelation.)
!
!       2.  Write the log power spectra to a disk file.
!
!       3.  If MODE = BOTH, also copy the input traces to a disk file.
!
!   For MODE = APPLY or BOTH:
!
!       4.  Read the saved spectra back in and group them so that associated
!       SC element types are together.  (For example, collect the log power
!       spectra associated with a given source together and collect the log
!       power spectra associated with a given offset together.)
!
!       5.  For each group of spectra, perform a Gauss-Seidel iteration to form
!       a SC solution.
!
!       6.  For each SC solution, calculate the autocorrelation and from the
!       autocorrelation calculate the minimum phase decon operator.
!
!       7.  Apply the decon operators to the input traces using the operators
!       associated with each trace's SC element values.  (For example, apply the
!       operator associated with the trace's source and then apply the operator
!       associated with the trace's offset.)  If MODE = BOTH, the traces at this
!       stage will be coming from the disk file generated previously.
!
!
! Surface Consistent Elements
!
! Users have a choice of source and offset (SO), source and receiver (SR) or 
! source, offset and receiver (SOR) as SC elements.  Normally for land work
! source and receiver will be chosen since offset is unlikely to be significant.
! And for marine work normally source and offset will be chosen since offset 
! labels hydrophone groups and receiver ground position is unlikely to be 
! significant.
!
! If offset is chosen as a SC element, any near or far traces lying outside of
! the user-defined offset binning scheme will have the closest operator applied.
! 
! 
! Surface Consistent Element Coordinates
!
! SCDECON uses header word 9 as the source coordinate and header word 6 as the 
! offset coordinate.  (Usually the offset parameters are set so that each 
! hydrophone group is in its own offset bin.  This allows phone amplitude 
! variations to be corrected.)
!
! If REC_COOR = GRID, header words 35 & 36 are used as receiver coordinates.
! If REC_COOR = SEQU, header word 47 is used as receiver coordinate.
!
!
! Surface Consistent Amplitude Correction
!
! SCDECON can apply a SC amplitude correction only or a SC decon only or both 
! together.
!
! If OPT_AMPL = D   (Perform surface consistent decon.)
! If OPT_AMPL = A   (Perform surface consistent amplitude correction.) 
! If OPT_AMPL = DA  (Perform surface consistent decon and amplitude correction.)
!
!
! Deconvolution Types
!
! If TYPE_DECON = DECON, then traditional minimum phase statistical decon 
! operators are calculated.
! If TYPE_DECON = ZERO_PHASE, then the amplitude spectrum is whitened and the
! phase spectrum is preserved.
!
! For ZERO_PHASE, a spiking decon operator of length 2*LEN_OP is first derived.
! This is then converted (by FFT and inverse FFT) to a zero-phase operator of
! the same length (lag times from -LEN_OP to +LEN_OP) and with the same 
! amplitude spectrum.
!
! Normally the DECON option is chosen.  ZERO_PHASE is used when it is desired
! to whiten the amplitude spectrum while maintaining the phase spectrum 
! unchanged. 
!
!
! Note on Surface Consistency
!
! A SC decon process calculates an operator for each SC element (such as source
! or offset) that depends only on the particular value of that SC element.  
! (For example a source operator that depends only on the particular source and
! an offset operator that depends only on the particular offset value.)  For 
! any given trace, the SC decon process applies the decon operators that are 
! associated with the SC element values for that trace.  (For example, apply 
! the source operator associated with the trace's source and apply the offset 
! operator associated with the trace's offset.) 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Amplitude Preprocessing
!
! If SC amplitude correction in SCDECON is desired, then data input to this
! process should have only a deterministic amplitude correction applied, such as
! GDIV or TPOW. 
!
!
! Stability
!
! The DIAG_LOAD setting may be a good place to look if the output from
! SCDECON is unstable.  Also, take care that the correlation windows are
! sufficiently long relative to the operator.  SCDECON only uses calculations
! from traces where the active window is at least 5 times LEN_OP.
!
!
! Two Arguments for Using Surface Consistent Deconvolution
!
! 1.  You may believe that major components of the embedded wavelet are SC.  On
! land you may believe that the wavelet is characterized by the details of the
! weathered layer beneath the source and receiver.  In that case a SC decon 
! using source and receiver would be indicated.  Or in the marine case you may 
! be concerned about source signature or amplitude varying with each pop and 
! hydrophone groups with different gain characteristics.  A SC decon using 
! source and offset should be appropriate here.
!
! 2.  You may believe that the dominant challenge of decon is to obtain an 
! accurate estimate of the trace autocorrelation.  SC decon is an alternative 
! to gather consistent decons that average trace autocorrelations over a gather
! (to reduce noise effects) and compute one operator.  SC decon averages trace
! spectra and uses Gauss-Seidel iteration to form SC operators.  It is possible
! that this averaging scheme produces a more accurate autocorrelation estimate 
! than other methods.
!
!
! Disk Usage
!
! As a final step, the file(s) created by SCDECON are removed after the
! operators have been applied.  In some circumstances, it is possible that these
! files won't be properly removed.  It may be prudent for the user to do some
! manual disk management with respect to the .spectra and .trctemp disk files
! created so that files intended for temporary use are not inadvertantly
! retained.  (The .params file is small and is not automatically removed.)
!
!
! Number of Source and Receiver Ground Positions
!
! Unlike it's predecessor SCDT, SCDECON does not require the user to specify
! this information.  An estimate of the total number of traces is requested, but
! this parameter (MAX_TRACES) can be quite rough and err in either direction.
!
!
!
!
! Example Jobs
!
!     One-pass approach             Two-pass approach
!     -----------------             -----------------
!     PROJECT_DATA                  PROJECT_DATA
!     JOB_DATA                      JOB_DATA
!     TRIN                          TRIN
!     TPOW                          TPOW
!     SCDECON (MODE = BOTH)         SCDECON (MODE = PREP)
!      .                            TRIN
!     etc.                          TPOW
!                                   SCDECON (MODE = APPLY)
!                                    .
!                                   etc.
!
!
!     Two-job approach
!     -----------------
!
!    First job:
!
!     PROJECT_DATA
!     JOB_DATA
!     TRIN
!     TPOW
!     SCDECON (MODE = PREP)
!
!
!            Second job:
!
!             PROJECT_DATA
!             JOB_DATA
!             TRIN
!             TPOW
!             SCDECON (MODE = APPLY)
!              .
!             etc.
!
!  
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
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
! This process does not alter input traces in PREP mode. This process alters
! input traces in APPLY and BOTH modes.
!
! This process outputs the same traces as it receives (altered).
!
! This process outputs one trace at a time.
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
! NUMTR     max number of traces input/output       used but not changed
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
! Hwd#    Description                              Action taken
! ----    -----------                              ------------
! 6       Offset                                   Used, not changed
! 9       Source number (HDR_ORIGINAL_GROUP)       Used, not changed
! 35, 36  Receiver grid                            Used, not changed
! 47      Receiver GP                              Used, not changed
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY                    
! 
!     Date       Author     Description
!     ----       ------     -----------
! 21. 2010-08-13 Stoeckley  Move automatic arrays src, rec, and off, which were
!                            in scdecon_apply, to scdecon_struct.  These arrays
!                            were being populated in the first call to scdecon_apply,
!                            and then applied to the first trace gather.  In
!                            subsequent calls to scdecon_apply, these same arrays,
!                            which had been populated in the first call, were
!                            applied to the subsequent gathers.  Since these
!                            were automatic arrays, they were being allocated and
!                            freed in each call to scdecon_apply; therefore did
!                            not contain valid information on subsequent calls.
! 20. 2007-10-18 Stoeckley  Remove test for invalid option for TYPE_DECON;
!                            always retain all files unless MODE = BOTH.
! 19. 2007-10-09 Stoeckley  Add AUTO to the list of options for 4 parameters.
! 18. 2007-07-03 Stoeckley  Move call latwin_update after the pc_put calls
!                            so the first tab will be the main tab in SeisSpace.
!017. 2006-06-20  B. Menger   Removed Unused Variables.
! 16. 2002-09-11 Goodger    Use mth module for binning.  Rename function 
!                           istatr to scdecon_istatr.
! 15. 2001-02-05 Cook       Changed .trc work file name to .trctemp to avoid
!                            potential conflicts with names users might use
!                            elsewhere in the job (e.g. with TROT).
!                           Relaxed LEN_OP vs. correlation window requirement
!                            when ZERO_PHASE requested.  Formerly 10, now 5.
!                            Criteria is still 5 times for DECON.
! 14. 2001-01-26 Cook       Fixed problem where last output trace was dropped
!                            when using BOTH mode.
!                           Fixed problem where array index was violated in
!                            scdecon_get_trace_or_gather() when data was input
!                            in gathers.
!                           Added option to create a file containing the final
!                            decon operators (a potential user benefit plus
!                            useful for debugging).
!                           Performed extensive synthetic tests.  Nearly matches
!                            old CPS output, some questions perhaps remain.
!                           Re-activated "operator length times 5" requirement
!                            in effort to improve stability.
! 13. 2000-12-11 Cook       Removed hard-wired "operator length times 5"
!                            restriction.
!                           Changed 'wrapup_flag' to 'skip_wrapup'.
!                           Dead traces no longer are thrown out in BOTH mode.
!                           Fixed bug related to possible confusion concerning
!                            appropriate window tops and bottoms returned from
!                            LATWIN.
!                           Incorporated clean.f90 to reduce probability of bad
!                            values creeping into calculations.  This appears to
!                            be needed in addition to other protective code such
!                            as avoiding divide-by-zero steps, etc.
! 12. 2000-11-14 Cook       (MAJOR) Converted to new CPS system.
! 11. 1998-12-15 Goodger    Begin using the fortran90 compiler.              
! 10. 1997-03-03 Goodger    Change default of DLD parameter to 2.            
!  9. 1995-09-23 Vunderink  Changed internal STRINI trace count parameter 
!                           from N to N1.
!  8. 1995-09-05 Vunderink  Double OPLEN when FILTYP=NORM
!  7. 1995-08-23 Goodger    Remove mail to goodger.                       
!  6. 1995-07-18 Goodger    Add parameter MODE.  See Note 6.              
!  5. 1995-05-18 Troutt     Added parameter FILTYP (add normalize option).
!                           Also modified code for obtaining the input power
!                           spectrum from the autocorrelation because the
!                           power spectra of the truncated autocorrelations
!                           can have negative real values.  We obtain a
!                           non-truncated AC by 1st finding the min-phase
!                           inverse of the truncated AC and then computing 
!                           the complete autocorrelation of the inverse.  
!                           This AC is then FFTd and we use the reciprocal 
!                           of the real part to get back to the power 
!                           spectrum of the input.
!  4. 1994-02-11 Troutt     Added checks for errors in HPALLOC calls.
!  3. 1991-01-14 Troutt     Revised USER DOC to not go beyond column 72.
!  2. 1990-10-23 Peterson   Include error and abort arguments on calls to
!                           HPALLOC and HPDEALLC.
!  1. 1989-03-22 Lazear     Account for ground position increment in
!                          receivers. 
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
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST    true     whether this process ever needs to request traces.
! NEED_LABEL      true     whether this process needs a label.     
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
! GUI Notes
!
! The parameter file is used to communicate between the stages of operation.
! The 'job file' thus contains null values like 'AUTO', -999, and 1e30.  This
! is intended to prevent the user from being confused by the job file--sensible
! values only occur in one place.
! 
!
! Other
!
! The BOTH mode shares most of its logic with the PREP and APPLY modes.  The
! main difference is found in the scdecon (main execution) routine.  With the
! PREP/APPLY approach, the user supplies traces twice (uses TRIN twice, for
! example).  With the BOTH approach, the use only supplies the traces once--
! the traces are then sent automatically by looping through the trcio file
! containing the copied traces.
!
! The NDISK control parameter is calculated and reported.  The memory resources
! required are negligible and are not reported as of this writing.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS SCDECON Process/NC=80>
!
! 2D and 3D surface consistent deconvolution and amplitude scaling.
!
!
!  MODE~~~~~~=`CCCC
!  OPT_AMPL~~=`CCCC
!  TYPE_DECON=`CCCCCCCCC
!
!  OPT_SC~~=`CCCC
!  REC_COOR=`CCCC
!
!  ITERATIONS=`II
!  LEN_OP~~~~=`FFFFF
!  DIAG_LOAD~=`FFFFF
!
!  MAX_TRACES=`IIIIIIIIIIII
!
!  PARAM_PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  SPECT_PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!  TRACE_PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!  FILT_PATHNAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!  OFF_INIT=`FFFFFF  OFF_INC=`FFFFFF  OFF_LAST=`FFFFFFFF  OFF_TOT=`IIIII
!
!<NS Window Parameters/NC=80>
!<include latwin.f90>
!
!<PARMS Window Parameters [screen2]>
!<PARMS PARAM_PATHNAME[/ML=120/XST]>
!<PARMS SPECT_PATHNAME[/ML=120/XST]>
!<PARMS TRACE_PATHNAME[/ML=120/XST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Mode to operate in:  prepare, apply or both. </Tip>
! Default = PREP
! Allowed = PREP    (Stage 1 of the two-pass approach.)
! Allowed = APPLY   (Stage 2 of the two-pass approach.)
! Allowed = BOTH    (One-pass approach.)
! In MODE = BOTH, both the log power spectra and the input traces are saved on
! disk.  Otherwise, only the log power spectra are saved on disk.
!</Help>
!
!<Help KEYWORD="OPT_AMPL">
!<Tip> Whether to perform SC decon, amplitude correction or both. </Tip>
! Default = DA
! Allowed = D   (Perform surface consistent decon.)
! Allowed = A   (Perform surface consistent amplitude correction.) 
! Allowed = DA  (Perform surface consistent decon and amplitude correction.) 
!</Help>
!
!<Help KEYWORD="TYPE_DECON">
!<Tip> Whether to perfom minimum phase decon or zero phase decon. </Tip>
! Default = DECON
! Allowed = DECON      (Traditional minimum phase statistical decon.)
! Allowed = ZERO_PHASE (Whiten amplitude spectrum but preserve phase spectrum.)
! Normally the DECON option is chosen.  ZERO_PHASE is used when it is desired
! to whiten the amplitude spectrum while maintaining the phase spectrum 
! unchanged. 
!
! For ZERO_PHASE, a spiking decon operator of length 2*LEN_OP is first derived.
! This is then converted (by FFT and inverse FFT) to a zero-phase operator of
! the same length (lag times from -LEN_OP to +LEN_OP) and with the same 
! amplitude spectrum.
!</Help>
!
!<Help KEYWORD="OPT_SC">
!<Tip> Option on surface consistent elements to use. </Tip>
! Default = SO
! Allowed = SO     (Use source and offset as surface consistent elements.)
! Allowed = SR     (Use source and receiver as surface consistent elements.) 
! Allowed = SOR    (Use source, offset and receiver as SC elements.)    
! Normally in marine work, SO should be chosen and in land work, SR should be 
! chosen.
!</Help>
!
!<Help KEYWORD="ITERATIONS">
!<Tip> Number of iterations for the surface consistent decomposition. </Tip>
! Default = 10
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of operator, in sec, for each SC element. </Tip>
! Default = 0.2
! Allowed = real > 0.0
! If WIN_TIM_LEN must be at least 5 times LEN_OP.
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent, to stabilize the operator calculation. </Tip>
! Default = 2.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="MAX_TRACES">
!<Tip> Approximate number of traces to be processed. </Tip>
! Default = 99999
! Allowed = int > 0
! Used for estimating the amount of disk space required.  Does not need to be
! exact, and does not need to exceed the true value if unknown.
!</Help>
!
!<Help KEYWORD="PARAM_PATHNAME">
!<Tip> Pathname of file to facilitate internal transfer of parameters. </Tip>
! Default = -
! Allowed = char
! If MODE = APPLY, this MUST refer to the exact same file as in the PREP stage.
! This file facilitates parameter communication between the two stages of
! processing.  It is a very small ASCII file written at the end of PREP mode
! and read in at the start of APPLY mode (and works behind the scenes in BOTH
! mode).  It contains a few GUI parameters from the PREP mode plus information
! such as min/max ranges, source counts, increments, etc. to allow for proper
! memory allocation and SC groupings.
!</Help>
!
!<Help KEYWORD="SPECT_PATHNAME">
!<Tip> Pathname of temp file to store (retrieve) spectra to (from) disk. </Tip>
! Default = Automatically generated from PARAM_PATHNAME.
! Allowed = Automatically generated exclusively.
!</Help>
!
!<Help KEYWORD="TRACE_PATHNAME">
!<Tip> Pathname of temp file to store traces on disk (MODE = BOTH only). </Tip>
! Default = Automatically generated from PARAM_PATHNAME.
! Allowed = Automatically generated exclusively.
!</Help>
!
!<Help KEYWORD="FILT_PATHNAME">
!<Tip> Pathname of optional file containing final decon operators. </Tip>
! Default = NONE
! Allowed = char
! A .trc file containing the final source, receiver, and/or offset decon
! operators.  Provided for informational purposes and for debugging.  This is
! potentially a large file.
!</Help>
!
!<Help KEYWORD="REC_COOR">
!<Tip> Header word(s) to use for receiver ground positions in this job. </Tip>
! Default = GRID
! Allowed = GRID  (Use header word 35 & 36 for receivers)
! Allowed = SEQU  (Use header word 47 for receivers.)
! Active only if OPT_SC = SR or SOR.
!</Help>
!
!--------offset bin parameters are only active if OPT_SC = SO or SOR------------
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Offset (header word 6) value at center of first offset bin. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! Offset (header word 6) value, in feet or meters, at center of first offset 
! bin.
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment of offset values between offset bins. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! Increment of offset values, in feet or meters, between offset bins.
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Offset (header word 6) value at center of last offset bin. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! Offset (header word 6) value, in feet or meters, at center of last offset bin.
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offset bins. </Tip>
! Default = 1
! Allowed = int > 0
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module scdecon_module
      use cio_module
      use clean_module
      use fft_module
      use fltr_module
      use latwin_module
      use lav_module
      use mem_module
      use named_constants_module
      use pathcheck_module
      use pattern_module
      use pc_module
      use opfilt_module
      use string_module
      use trcio_module

      implicit none
      private

      public :: scdecon_create
      public :: scdecon_initialize
      public :: scdecon_update
      public :: scdecon_delete
!<execute_only>
      public :: scdecon          ! main execution (trace processing) routine.
      public :: scdecon_wrapup
!</execute_only>


      character(len=100),public,save :: SCDECON_IDENT = &
'$Id: scdecon.f90,v 1.20 2007/10/18 15:19:56 Stoeckley beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
      integer, parameter              :: IVAL1     = 1
      integer, parameter              :: MAXGPINT  = 12
      integer, parameter              :: NSTACK    = 10

      type,public :: scdecon_struct              
 
        private

        logical                        :: skip_wrapup      ! wrapup flag

        logical                        :: gathered         ! globals
        integer                        :: nwih,ndpt,numtr  ! globals  
        real                           :: tstrt,dt         ! globals

        character(len=5)               :: mode             ! process parameter
        character(len=4)               :: opt_ampl         ! process parameter
        character(len=10)              :: type_decon       ! process parameter
        character(len=4)               :: opt_sc           ! process parameter
        character(len=4)               :: rec_coor         ! process parameter

        character(len=FILENAME_LENGTH) :: param_pathname   ! process parameter
        character(len=FILENAME_LENGTH) :: spect_pathname   ! process parameter
        character(len=FILENAME_LENGTH) :: trace_pathname   ! process parameter

        character(len=FILENAME_LENGTH) :: filt_pathname    ! process parameter
        integer                        :: filt_ndisk

        integer                        :: iterations       ! process parameter
        real                           :: diag_load        ! process parameter
        real                           :: len_op           ! process parameter

        integer                        :: max_traces       ! process parameter

        real                           :: off_init         ! process parameter
        real                           :: off_inc          ! process parameter
        real                           :: off_last         ! process parameter
        integer                        :: off_tot          ! process parameter

        type(latwin_struct), pointer   :: latwin           ! process parameter
        integer                        :: nwin             ! process parameter

        logical                        :: operators_already_calculated

        type(fft_struct), pointer      :: fftrc,fftcr             ! dependent

        integer                        :: parm_file_lun           ! dependent
        integer                        :: spec_file_lun           ! dependent
        type(trcio_struct), pointer    :: trace_file              ! dependent
        type(trcio_struct), pointer    :: filt_file               ! dependent

        integer                        :: bothCount
        integer                        :: nspec_disk              ! dependent
        integer                        :: ntraces_disk            ! dependent

        integer                        :: nbypass1                ! dependent
        integer                        :: nbypass2                ! dependent
        integer                        :: nbypass3                ! dependent
        integer                        :: nbypass4                ! dependent
        integer                        :: nbypass5                ! dependent
        integer                        :: nbypass6                ! dependent
        integer                        :: nbypass7                ! dependent

        integer                        :: allzero1, notallzero1   ! dependent
        integer                        :: allzero2, notallzero2   ! dependent
        integer                        :: allzero3, notallzero3   ! dependent

        integer                        :: irxgp,irygp             ! dependent
        integer                        :: nfft,nyq,naclh          ! dependent

        integer                        :: nsrc,nrec               ! dependent
        integer                        :: nrecx,nrecy             ! dependent

        real                           :: oprl_times_5            ! dependent

        integer                        :: minxgp,minygp           ! dependent

        real                           :: gmin, gmax              ! dependent
        real                           :: rminx,rmaxx,rxinc       ! dependent
        real                           :: rminy,rmaxy,ryinc       ! dependent

        real              , pointer    :: src(:)                      
        real              , pointer    :: rec(:)                      
        real              , pointer    :: off(:)                      

      end type scdecon_struct


!!----------------------------- interfaces ---------------------------------!!
!!----------------------------- interfaces ---------------------------------!!
!!----------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(scdecon_struct),pointer,save :: object      ! needed for traps.


      integer,parameter         :: mode_noptions = 3
      character(len=5),save     :: mode_options(mode_noptions)
      data mode_options/'PREP', 'APPLY', 'BOTH'/

      integer,parameter         :: opt_ampl_noptions = 4
      character(len=4),save     :: opt_ampl_options(opt_ampl_noptions)
      data opt_ampl_options/'D', 'A', 'DA', 'AUTO'/

      integer,parameter         :: type_decon_noptions = 3
      character(len=10),save    :: type_decon_options(type_decon_noptions)
      data type_decon_options/'DECON', 'ZERO_PHASE', 'AUTO'/

      integer,parameter         :: opt_sc_noptions = 4
      character(len=4),save     :: opt_sc_options(opt_sc_noptions)
      data opt_sc_options/'SO', 'SR', 'SOR', 'AUTO'/

      integer,parameter         :: rec_coor_noptions = 3
      character(len=4),save     :: rec_coor_options(rec_coor_noptions)
      data rec_coor_options/'GRID', 'SEQU', 'AUTO'/


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine scdecon_create (obj)
      implicit none
      type(scdecon_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify(obj%latwin)
      nullify(obj%fftrc)
      nullify(obj%fftcr)
      nullify(obj%trace_file)
      nullify(obj%filt_file)
      nullify(obj%src)
      nullify(obj%rec)
      nullify(obj%off)

      call latwin_create (obj%latwin)

      call scdecon_initialize (obj)

      return
      end subroutine scdecon_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine scdecon_delete (obj)
      implicit none
      type(scdecon_struct),pointer :: obj       ! arguments

      integer     :: ier


!<execute_only>

      call pc_print('SCDECON: Explicit wrapup call #1 (this is normal).')

      call scdecon_wrapup (obj)
!</execute_only>

      if(associated(obj%latwin)) call latwin_delete(obj%latwin)
      if(associated(obj%fftrc))  call fft_delete   (obj%fftrc)
      if(associated(obj%fftcr))  call fft_delete   (obj%fftcr)

      if(associated(obj%trace_file)) then
        ier = trcio_close(obj%trace_file)
        if(ier > 0) call pc_error('error closing trace file')
      end if

      if(associated(obj%filt_file)) then
        ier = trcio_close(obj%filt_file)
        if(ier > 0) call pc_error('error closing filt file')
      end if

      if(associated(obj%src)) deallocate(obj%src)
      if(associated(obj%rec)) deallocate(obj%rec)
      if(associated(obj%off)) deallocate(obj%off)

      deallocate(obj)

      return
      end subroutine scdecon_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine scdecon_initialize (obj)
      implicit none
      type(scdecon_struct),intent(inout) :: obj       ! arguments

!---- globals
      obj%ndpt       = 0
      obj%nwih       = 0
      obj%numtr      = 0
      obj%tstrt      = 0.
      obj%dt         = 0.

!---- parameters
      obj%mode       = 'PREP'
      obj%opt_ampl   = 'DA'
      obj%type_decon = 'DECON'
      obj%opt_sc     = 'SO'
      obj%rec_coor   = 'GRID'

      obj%param_pathname = PATHCHECK_EMPTY
      obj%spect_pathname = PATHCHECK_EMPTY
      obj%trace_pathname = PATHCHECK_EMPTY

      obj%filt_pathname = PATHCHECK_EMPTY

      obj%nwin       = 1

      obj%iterations = 10
      obj%diag_load  = 2.
      obj%len_op     = .2
      obj%max_traces = 99999

      obj%off_init   = 0.
      obj%off_inc    = 0.
      obj%off_last   = 0.
      obj%off_tot    = 1

!---- other
      obj%operators_already_calculated = .false.
      obj%bothCount  = 0

      call latwin_initialize (obj%latwin)

      call scdecon_update (obj)

      return
      end subroutine scdecon_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine scdecon_update (obj)
      implicit none
      type(scdecon_struct),intent(inout),target :: obj             ! arguments

      integer                  :: ier                 ! local
      integer                  :: nacl
      real                     :: oprl
      double precision         :: ddisk

      logical     :: read_file_mode


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('nwih'     , obj%nwih)
      call pc_get_global ('ndpt'     , obj%ndpt)
      call pc_get_global ('numtr'    , obj%numtr)
      call pc_get_global ('tstrt'    , obj%tstrt)
      call pc_get_global ('dt'       , obj%dt)
      call pc_get_global ('gathered' , obj%gathered)

      call pc_get ('param_pathname', &
        obj%param_pathname, scdecon_param_pathname_trap)

      call pc_get ('filt_pathname', &
        obj%filt_pathname, scdecon_filt_pathname_trap)

      call pc_get ('mode'       ,obj%mode)
      call pc_get ('opt_ampl'   ,obj%opt_ampl)
      call pc_get ('type_decon' ,obj%type_decon)
      call pc_get ('opt_sc'     ,obj%opt_sc, scdecon_opt_sc_trap)
      call pc_get ('rec_coor'   ,obj%rec_coor)

      call pc_get ('iterations' ,obj%iterations)
      call pc_get ('len_op'     ,obj%len_op, scdecon_len_op_trap)
      call pc_get ('diag_load'  ,obj%diag_load)
      call pc_get ('max_traces' ,obj%max_traces)

      call pc_get ('off_init'   ,obj%off_init)
      call pc_get ('off_inc'    ,obj%off_inc)
      call pc_get ('off_last'   ,obj%off_last)
      call pc_get ('off_tot'    ,obj%off_tot)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
      if(obj%opt_sc /= 'SR') then
        ier = pattern_stop2('SCDECON:', .true.,                        &
          obj%off_init, obj%off_inc, obj%off_last, obj%off_tot,        &
          'OFF_INIT', 'OFF_INC', 'OFF_LAST', 'OFF_TOT',                &
          pc_verify_scalar('off_init'), pc_verify_scalar('off_inc'),   & 
          pc_verify_scalar('off_last'), pc_verify_scalar('off_tot'),   &
          inc_min=1.0)
      end if

      if(pc_verify_screen('screen1')) call scdecon_verify_screen('screen1')


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
      if(obj%mode == 'BOTH') then
        call pc_put_control ('need_label',.true.)
        call pc_put_control ('need_request',.true.)
      else
        call pc_put_control ('need_label',.false.)
        call pc_put_control ('need_request',.false.)
      end if

!---- disk space needed for spectra (ddisk is bytes, double precision)
!---- conditionally add disk space needed for trace copies (mode = BOTH)
      ddisk = obj%max_traces * 4 * (2.0*obj%len_op/obj%dt + 5)

      if(obj%mode == 'BOTH') then
        ddisk = ddisk + obj%max_traces * 4 * (2.0*obj%nwih + obj%ndpt)
        call pc_put_control ('ndisk',nint(ddisk/1024/1024))
      else if(obj%mode == 'PREP') then
        call pc_put_control ('ndisk',nint(ddisk/1024/1024))
      end if

      call pc_put_options_field ( &
        'mode'      , mode_options      , mode_noptions)
      call pc_put_options_field ( &
        'opt_ampl'  , opt_ampl_options  , opt_ampl_noptions)
      call pc_put_options_field ( &
        'type_decon', type_decon_options, type_decon_noptions)
      call pc_put_options_field ( &
        'opt_sc'    , opt_sc_options    , opt_sc_noptions)
      call pc_put_options_field ( &
        'rec_coor'  , rec_coor_options  , rec_coor_noptions)

      call pc_put ('param_pathname' ,obj%param_pathname)
      call pc_put ('spect_pathname' ,obj%spect_pathname)
      call pc_put ('trace_pathname' ,obj%trace_pathname)

      call pc_put ('filt_pathname' ,obj%filt_pathname)

      call pc_put ('mode'       ,obj%mode)

      call pc_put ('opt_ampl'   ,obj%opt_ampl)
      call pc_put ('type_decon' ,obj%type_decon)
      call pc_put ('opt_sc'     ,obj%opt_sc)
      call pc_put ('rec_coor'   ,obj%rec_coor)

      call pc_put ('iterations' ,obj%iterations)
      call pc_put ('len_op'     ,obj%len_op)
      call pc_put ('diag_load'  ,obj%diag_load)
      call pc_put ('max_traces' ,obj%max_traces)

      call pc_put ('off_init'   ,obj%off_init)
      call pc_put ('off_inc'    ,obj%off_inc)
      call pc_put ('off_last'   ,obj%off_last)
      call pc_put ('off_tot'    ,obj%off_tot)


!---- In the case of mode 'APPLY', all parameters are sent via a disk file
!---- (except parameter file name) rather than provided by the GUI.
      if(obj%mode == 'APPLY') then
        call pc_put_visible_flag('opt_ampl'  , .false.)
        call pc_put_visible_flag('type_decon', .false.)
        call pc_put_visible_flag('opt_sc'    , .false.)
        call pc_put_visible_flag('rec_coor'  , .false.)

        call pc_put_visible_flag('iterations', .false.)
        call pc_put_visible_flag('len_op'    , .false.)
        call pc_put_visible_flag('diag_load' , .false.)
        call pc_put_visible_flag('max_traces', .false.)

        call pc_put_visible_flag('off_init'  , .false.)
        call pc_put_visible_flag('off_inc'   , .false.)
        call pc_put_visible_flag('off_last'  , .false.)
        call pc_put_visible_flag('off_tot'   , .false.)

        call pc_put_sensitive_screen_flag('screen2', .false.)

        call pc_put ('opt_ampl'  , 'AUTO')
        call pc_put ('type_decon', 'AUTO')
        call pc_put ('opt_sc'    , 'AUTO')
        call pc_put ('rec_coor'  , 'AUTO')

        call pc_put ('len_op'    , 99999)
        call pc_put ('diag_load' , 99999)
        call pc_put ('iterations', 99999)

        call pc_put ('off_init'  , 99999)
        call pc_put ('off_inc'   , 99999)
        call pc_put ('off_last'  , 99999)
        call pc_put ('off_tot'   , 1)

      else
        call pc_put_visible_flag('opt_ampl'  , .true.)
        call pc_put_visible_flag('type_decon', .true.)
        call pc_put_visible_flag('opt_sc'    , .true.)
        call pc_put_visible_flag('rec_coor'  , .true.)

        call pc_put_visible_flag('iterations', .true.)
        call pc_put_visible_flag('len_op'    , .true.)
        call pc_put_visible_flag('diag_load' , .true.)
        call pc_put_visible_flag('max_traces', .true.)

        call pc_put_visible_flag('off_init'  , .true.)
        call pc_put_visible_flag('off_inc'   , .true.)
        call pc_put_visible_flag('off_last'  , .true.)
        call pc_put_visible_flag('off_tot'   , .true.)

        call pc_put_sensitive_screen_flag('screen2', .true.)

        call pc_put_sensitive_field_flag('trace_pathname' ,.false.)

      end if

!---- trace pathname is only relevant for BOTH mode
      if(obj%mode == 'BOTH') then
        call pc_put_visible_flag        ('trace_pathname' ,.true.)
        call pc_put_sensitive_field_flag('trace_pathname' ,.true.)
      else
        call pc_put_visible_flag        ('trace_pathname' ,.false.)
      end if

!---- these are never sensitive (they're generated from parameter file name)
      call pc_put_sensitive_field_flag('spect_pathname', .false.)
      call pc_put_sensitive_field_flag('trace_pathname', .false.)

!---- decon type is irrelevant for amp-only situation
      if(obj%opt_ampl == 'A') then
        call pc_put_sensitive_field_flag('type_decon', .false.)
      else
        call pc_put_sensitive_field_flag('type_decon', .true.)
      end if

!---- offsets are irrelevant for SR mode
      if(obj%opt_sc == 'SR') then
        call pc_put_sensitive_field_flag('off_init'  , .false.)
        call pc_put_sensitive_field_flag('off_inc'   , .false.)
        call pc_put_sensitive_field_flag('off_last'  , .false.)
        call pc_put_sensitive_field_flag('off_tot'   , .false.)
      else
        call pc_put_sensitive_field_flag('off_init'  , .true.)
        call pc_put_sensitive_field_flag('off_inc'   , .true.)
        call pc_put_sensitive_field_flag('off_last'  , .true.)
        call pc_put_sensitive_field_flag('off_tot'   , .true.)
      end if

      call latwin_update (obj%latwin, obj%nwin)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! memory allocation here?

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

      obj%operators_already_calculated = .false.

!-------------------------------------------------------------------------------
! Ascertain whether we're reading or writing at this juncture.
!-------------------------------------------------------------------------------
      if(obj%mode == 'APPLY') read_file_mode = .true.
      if(obj%mode == 'PREP')  read_file_mode = .false.
      if(obj%mode == 'BOTH')  read_file_mode = .false.

!-------------------------------------------------------------------------------
! Call subroutine to open files (in proper mode).
!-------------------------------------------------------------------------------
      call scdecon_open_files(obj,read_file_mode)

!-------------------------------------------------------------------------------
! Initializations (conditional because, depending on mode, some are taken from
! parameter file).
!-------------------------------------------------------------------------------

      obj%allzero1      = 0
      obj%allzero2      = 0
      obj%allzero3      = 0
      obj%notallzero1   = 0
      obj%notallzero2   = 0
      obj%notallzero3   = 0

      if(.not. read_file_mode) then
        obj%nbypass1    = 0
        obj%nbypass2    = 0
        obj%nbypass3    = 0
        obj%nbypass4    = 0
        obj%nbypass5    = 0
        obj%nbypass6    = 0
        obj%nbypass7    = 0

!---- extremes
        obj%minxgp=  0
        obj%minygp=  0

        obj%gmin  =  1e30
        obj%rminx =  1e30
        obj%rminy =  1e30

        obj%gmax  = -1e30
        obj%rmaxx = -1e30
        obj%rmaxy = -1e30

        obj%nspec_disk   = 0
        obj%ntraces_disk = 0
      end if

      obj%filt_ndisk = 0

!---- obj%len_op comes from GUI or from param file, depending on mode
!---- various lengths are then calculated based on len_op
      if(obj%opt_ampl == 'A') then
        oprl = obj%dt
      else
        oprl = obj%len_op
      end if

      if(obj%type_decon == 'DECON') then
        obj%naclh = nint(oprl/obj%dt)
        obj%oprl_times_5 = 5 * oprl
      else
        oprl  = 2*oprl
        obj%naclh = nint(oprl/obj%dt) + 1
        obj%oprl_times_5 = (5 * oprl)/2      ! too strict
      end if

      nacl = obj%naclh*2 - 1
      obj%nfft = alog(float(nacl))/alog(2.0) + 1
      obj%nfft = 2**obj%nfft
      obj%nfft = max0(8,obj%nfft)
      obj%nyq  = obj%nfft/2 + 1

!---- coordinate source
      if (obj%rec_coor == 'GRID') then 
        obj%irxgp = HDR_RECEIVER_XGRID
        obj%irygp = HDR_RECEIVER_YGRID
      else 
        obj%irxgp = HDR_RECEIVER_GP
        obj%irygp = 0 
      endif 

!---- fft's
      if(.not. associated(obj%fftrc)) &
        ier = fft_create(obj%fftrc, 1,obj%nfft,'rtoc')

      if(ier > 0) then
        call pc_error('fft_create error')
        return
      end if

      if(.not. associated(obj%fftcr)) &
        ier = fft_create(obj%fftcr,-1,obj%nfft,'ctor')

      if(ier > 0) then
        call pc_error('fft_create error')
        return
      end if

      call scdecon_parameter_dump(obj)

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine scdecon_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!-------------------------------------------------------------------------------
! Force LEN_OP to be a multiple of the sample rate.
!-------------------------------------------------------------------------------
      subroutine scdecon_len_op_trap(keyword)
      implicit none
      character(len=*), intent(in) :: keyword


      object%len_op = object%dt * nint(object%len_op/object%dt)

      return
      end subroutine scdecon_len_op_trap

!-------------------------------------------------------------------------------
! Pathname trap #1.
!-------------------------------------------------------------------------------
      subroutine scdecon_filt_pathname_trap(keyword)
      implicit none
      character(len=*), intent(in)    :: keyword

      call pathcheck('filt_pathname', object%filt_pathname,'.trc',.false.)

      end subroutine scdecon_filt_pathname_trap

!-------------------------------------------------------------------------------
! Pathname trap #2.
!-------------------------------------------------------------------------------
      subroutine scdecon_param_pathname_trap(keyword)
      implicit none
      character(len=*), intent(in)    :: keyword
      integer                         :: i,lastDot,lastColon

!---- return if empty
      call pathcheck('param_pathname', &
        object%param_pathname,'.params',.true.)

      if(object%param_pathname == PATHCHECK_EMPTY) then
        object%spect_pathname = PATHCHECK_EMPTY
        object%trace_pathname = PATHCHECK_EMPTY
        return
      end if

!---- automatically generate spectra and trace pathnames
      lastDot = 0
      do i=1,FILENAME_LENGTH
        if (object%param_pathname(i:i) == '.') lastDot = i
      end do

      object%spect_pathname = &
        object%param_pathname(1:lastDot) // 'spectra'

      object%trace_pathname = &
        object%param_pathname(1:lastDot) // 'trctemp'

!---- get rid of rcp syntax (if present)
      lastColon = 0
      do i=1,FILENAME_LENGTH
        if (object%param_pathname(i:i) == ':') lastColon = i
      end do

      if(lastColon /= 0) then
        object%param_pathname = &
          object%param_pathname(lastColon+1:FILENAME_LENGTH)
        object%spect_pathname = &
          object%spect_pathname(lastColon+1:FILENAME_LENGTH)
        object%trace_pathname = &
          object%trace_pathname(lastColon+1:FILENAME_LENGTH)
      end if

!      do i=1,FILENAME_LENGTH
!        write(*,*) 'fnames: ', i, &
!          ', ', object%param_pathname(i:i), &
!          ', ', object%spect_pathname(i:i), &
!          ', ', object%trace_pathname(i:i)
!      end do

      object%param_pathname = trim(object%param_pathname)
      object%spect_pathname = trim(object%spect_pathname)
      object%trace_pathname = trim(object%trace_pathname)

      return
      end subroutine scdecon_param_pathname_trap


!-------------------------------------------------------------------------------
! Trap for OPT_SC.
!-------------------------------------------------------------------------------
      subroutine scdecon_opt_sc_trap(keyword)
      implicit none
      character(len=*), intent(in)    :: keyword


!---- make sure off_inc is reset if 'O'ffset is not involved
!---- (off_inc is used as a flag in the Gauss-Seidel loop)
      if(object%opt_sc == 'SR') then
        object%off_init   = 0.
        object%off_inc    = 0.
        object%off_last   = 0.
        object%off_tot    = 1
      end if

      return
      end subroutine scdecon_opt_sc_trap

!-------------------------------------------------------------------------------
! Traps at screen verification time.
!-------------------------------------------------------------------------------
      subroutine scdecon_verify_screen(keyword)
      implicit none
      character(len=*), intent(in)    :: keyword

!---- iterations > 0
      if(object%iterations <= 0) then
        call pc_error('ITERATIONS must be greater than zero.')
        return
      end if

!---- diag_load > 0
      if(object%diag_load <= 0) then
        call pc_error('DIAG_LOAD must be greater than zero.')
        return
      end if

!---- max_traces > 0
      if(object%max_traces <= 0) then
        call pc_error('MAX_TRACES must be greater than zero.')
        return
      end if

!---- force to DECON mode if opt_ampl == 'A' so that only 1 program flow
!---- is possible when amplitude-only is requested
      if(object%opt_ampl == 'A') object%type_decon='DECON'

!!---- add some protection for users editing job file
!      if(object%type_decon /= 'DECON'      .and. &
!         object%type_decon /= 'ZERO_PHASE') then
!        call pc_error('SCDECON: Invalid type_decon = ',object%type_decon)
!      end if

      return
      end subroutine scdecon_verify_screen

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine scdecon (obj,ntr,hd,tr)
      implicit none
      type(scdecon_struct),intent(inout) :: obj                 ! arguments
      integer          ,intent(inout)    :: ntr                 ! arguments
      double precision ,intent(inout)    :: hd(:,:)             ! arguments
      real             ,intent(inout)    :: tr(:,:)             ! arguments


!-------------------------------------------------------------------------------
! Branch based on MODE and NTR.
!-------------------------------------------------------------------------------
      if(obj%mode == 'BOTH') then                         ! 1-pass mode

!------ BOTH mode, case 1 (prep phase):
!------ call scdecon_prep routine until NO_MORE_TRACES is encountered
        if     (obj%bothCount == 0  .and.  ntr /= NO_MORE_TRACES) then
!!!!!!     write(*,*) 'case 1, bothCount = ',obj%bothCount

          call scdecon_prep(obj,ntr,hd,tr)
          if(ntr == FATAL_ERROR) go to 999

          ntr = NEED_TRACES
          return

!------ BOTH mode, case 2 (should occur only once):
!------ need to make final call to scdecon_prep with ntr == NO_MORE_TRACES
!------ also open trace file
!------ also make first call to scdecon_apply

        else if(obj%bothCount == 0  .and.  ntr == NO_MORE_TRACES) then
!!!!!!     write(*,*) 'case 2, bothCount = ',obj%bothCount

          call scdecon_prep(obj,ntr,hd,tr)
          if(ntr == FATAL_ERROR) go to 999

          call scdecon_open_files(obj, .true.)

!-------- read first trace or gather from disk and "manually"
!-------- then call scdecon_apply and return
          obj%bothCount = 1

          ntr = scdecon_get_trace_or_gather(obj,hd,tr)
          if(ntr == FATAL_ERROR) go to 999

!          factor = 0.;
!          do j=1,obj%ndpt
!            write(*,*) tr(j,1)
!            if(abs(tr(j,1)) > factor) factor=abs(tr(j,1))
!          end do
!          write(*,*) 'after get, max was ',factor

          call scdecon_apply(obj,ntr,hd,tr)
          if(ntr == FATAL_ERROR) go to 999
          return

!------ BOTH mode, case 3 (continue with apply phase):
        else
!!!!!!     write(*,*) 'case 3, bothCount = ',obj%bothCount

          if(obj%bothCount >= obj%ntraces_disk + 1) then
            ntr = NO_MORE_TRACES
            go to 1000
          end if

!-------- read next trace or gather from disk and "manually"
!-------- then call scdecon_apply and continue looping until finished
          ntr = scdecon_get_trace_or_gather(obj,hd,tr)
          if(ntr == FATAL_ERROR) go to 999

          call scdecon_apply(obj,ntr,hd,tr)
          if(ntr == FATAL_ERROR) go to 999

        end if

      else                                               ! 2-pass mode

        if(obj%mode == 'PREP') then
          call scdecon_prep(obj,ntr,hd,tr)
        else if(obj%mode == 'APPLY') then
          call scdecon_apply(obj,ntr,hd,tr)
        else
          call pc_error('SCDECON logic error #2.')
          go to 999
        end if

        if(ntr == FATAL_ERROR) go to 999
        if(ntr == NO_MORE_TRACES) go to 1000

      end if

!-------------------------------------------------------------------------------
! Process next trace.
!-------------------------------------------------------------------------------
      return

!-------------------------------------------------------------------------------
! Fatal errors and ntr==NO_MORE_TRACES come here.
!-------------------------------------------------------------------------------
  999 ntr = FATAL_ERROR

 1000 call pc_print('SCDECON: Explicit wrapup call #2 (this is normal).')

      call scdecon_wrapup (obj)

      return
      end subroutine scdecon


!-------------------------------------------------------------------------------
!
! Subroutine for PREP mode.
!
!-------------------------------------------------------------------------------
      subroutine scdecon_prep(obj,ntr,hd,tr)
      implicit none
      type(scdecon_struct),intent(inout) :: obj                 ! arguments
      integer          ,intent(inout)    :: ntr                 ! arguments
      double precision ,intent(inout)    :: hd(:,:)             ! arguments
      real             ,intent(inout)    :: tr(:,:)             ! arguments

      character(len=120)              :: str
      integer                         :: i,j  ,l 
      integer                         :: ier,lun,icount
      integer                         :: nlive     ,itop,ibot 

      double precision                :: xgp,ygp

      real                            :: factor, modx,mody

      real                            :: ac(obj%nfft)    ! ???
      complex                         :: spec(obj%nfft)  ! ???

      integer                         :: ixinc(MAXGPINT),iyinc(MAXGPINT)

      real                            :: scrp(4*obj%naclh)


!-------------------------------------------------------------------------------
! Go directly to "summary portion" of subroutine if ntr==NO_MORE_TRACES.
!-------------------------------------------------------------------------------
      ixinc = 1
      iyinc = 1

      if(ntr == NO_MORE_TRACES) go to 1000

!-------------------------------------------------------------------------------
! This loop collects traces or their spectra on disk.
!-------------------------------------------------------------------------------
      do i=1,ntr

!------ skip spectra calculations for dead traces, but DO include dead traces in
!------ the BOTH mode trace file
        if(hd(HDR_LAV,i) == 0.) then
          obj%nbypass1 = obj%nbypass1 + 1
!          write(*,*) 'bypass1 (dead trace) hd(1)=',hd(1,i); ! pause
          go to 35
        end if

!-------------------------------------------------------------------------------
! Figure out where the top and base of the window will be for this trace.
!-------------------------------------------------------------------------------
        call latwin_get_window ( &
          obj%latwin, hd(:,i), tr(:,i), index1=itop, index2=ibot)
        nlive = ibot - itop + 1

!------
!------ hardcoded debugging code 
!------ to emulate the old CPS windowing system (for direct comparisons)
!------
!        write(*,*) ' '
!        write(*,*) 'HDR6, HDR7, HDR8: ',hd(6,i),',',hd(7,i),',',hd(8,i)
!        write(*,*) '1: itop,ibot,nlive=',itop,',',ibot,',',nlive

!        itop =  26 + nint(hd(6,i)/5500.)
!        ibot = 626 + nint(hd(6,i)/5500.)
!        nlive= ibot - itop + 1

!        write(*,*) '2: itop,ibot,nlive=',itop,',',ibot,',',nlive

        if(ibot > obj%ndpt) then
          call pc_error('ibot exceeds obj%ndpt');
          go to 999
        end if

!-------------------------------------------------------------------------------
! window length criteria for stability
!-------------------------------------------------------------------------------
        if(obj%opt_ampl == 'A') then
          if (nlive < 1) then
            obj%nbypass2 = obj%nbypass2 + 1
            go to 35
          end if
        else if (nlive*obj%dt < obj%oprl_times_5) then
            obj%nbypass2 = obj%nbypass2 + 1
            go to 35
        end if

!-------------------------------------------------------------------------------
! Autocorrelate the data in the window.
!-------------------------------------------------------------------------------
        ac(:)  = 0.0
        call fltr_filterg(                                        &
          tr(itop:ibot,i), nlive,                                 &
          tr(itop:ibot,i), nlive + obj%naclh - 1, ac)

!-------------------------------------------------------------------------------
! Normalize based on number of live samples (and add diagonal load).
!-------------------------------------------------------------------------------
        ac(1)  = ac(1) /nlive*(1. + obj%diag_load/100.)
        ac(2:) = ac(2:)/nlive 

!-------------------------------------------------------------------------------
! Create minimum phase inverse of autocorrelation.
!-------------------------------------------------------------------------------
        if (obj%naclh > 1) then 

          scrp(:) = 0.0
          scrp(obj%naclh+1) = 1.0 
          call opfilt(                          &
            obj%naclh,                          &
            scrp(            1 :   obj%naclh),  &
            scrp(  obj%naclh+1 : 2*obj%naclh),  & 
            scrp(2*obj%naclh+1 : 4*obj%naclh),  &
            ac)

!------ quality check
          j=clean_zero(scrp(:obj%naclh))
          if(j>0) then
            obj%nbypass3 = obj%nbypass3 + 1
!            write(*,*) 'clean bypass at hd(1)=',hd(1,i); ! pause
            go to 35
          end if

!          if(scrp(1) < 0) then
!            do j=1,5
!              write(*,*)'raw : ac,scr=',ac(j),' ',scrp(j)
!            end do
!            pause
!          end if

!-------------------------------------------------------------------------------
! Normalize the inverse.
!-------------------------------------------------------------------------------
          if     (scrp(1) > 0.) then
            factor =  sqrt( scrp(1))
          else
            obj%nbypass4 = obj%nbypass4 + 1
!            write(*,*) 'bypass4 at hd(1)=',hd(1,i);
            go to 35
          end if

          factor = 1./factor
          scrp(:obj%naclh) = scrp(:obj%naclh)*factor

!-------------------------------------------------------------------------------
! Autocorrelate the inverse wavelet (all positive lags).
!-------------------------------------------------------------------------------
          scrp(obj%naclh+1:) = 0.0
          call fltr_filterg (scrp, obj%naclh, scrp, 2*obj%naclh - 1, ac)

        endif 

!        do j=1,obj%nfft
!          write(*,*) 'ac(j)=',ac(j)
!        end do
!        write(*,*)'after autocorrelating inverse'
!        pause

!-------------------------------------------------------------------------------
! Make autocorrelation two-sided.
!-------------------------------------------------------------------------------
        ac(obj%naclh+1:) = 0.0

        if (obj%naclh > 1) then 
          ac(obj%nfft:obj%nfft+2-obj%naclh:(-1)) = ac(2:obj%naclh) 
        endif 

!        do j=1,obj%nfft
!          write(*,*) 'ac(j)=',ac(j)
!        end do
!        write(*,*)'after 2-sided'
!        pause

!-------------------------------------------------------------------------------
! FFT the autocorrelation to get real spectrum of the inverse.
!-------------------------------------------------------------------------------
        spec(:) = cmplx(0.,0.)
        call fft_rc_transform(obj%fftrc,ac,spec)

!-------------------------------------------------------------------------------
! Store the real part of the spectrum of the input (i.e. 1/inverse) into the
! ac array (assume spectrum is real-valued and positive).
!-------------------------------------------------------------------------------
        ac(:) = 0.0
        if (obj%naclh > 1) then 
          do j = 1, obj%nyq 
            if(real(spec(j))==0) then      ! old cps did not check for 0
              ac(j) = 0.
            else
              ac(j) = alog(1./real(spec(j)))
            end if
          end do 
        else 
          do j = 1, obj%nyq 
            ac(j) = alog(real(spec(j))) 
          end do 
        endif 

!        do j=1, obj%nfft
!          write(*,*) 'ac(j)=',ac(j)
!        end do
!        write(*,*)'after real part of spectrum'

!-------------------------------------------------------------------------------
! Write spectrum to disk file.
! Additional information is included at the base of the spectra.
!-------------------------------------------------------------------------------
!------ final quality check
        j=clean_zero(ac)
        if(j>0) then
          obj%nbypass6 = obj%nbypass6 + 1
!          write(*,*) 'bypass6 at hd(1)=',hd(1,i); ! pause
          go to 35
        end if

        ac(obj%nyq+1) = hd(HDR_OFFSET,i) 
        ac(obj%nyq+2) = hd(HDR_ORIGINAL_GROUP,i) 
        ac(obj%nyq+3) = nint(hd(obj%irxgp,i)) 
        if(obj%irygp > 0) then 
          ac(obj%nyq+4) = nint(hd(obj%irygp,i))
        else 
          ac(obj%nyq+4) = 0.0 
        endif

        obj%nspec_disk = obj%nspec_disk + 1

        icount = cio_fwrite(ac,4,obj%nyq+4,obj%spec_file_lun)
        if(icount /= obj%nyq+4) then
          call pc_error( &
            'SCDECON: error writing to spect file, icount was ', icount)
          go to 999
        end if

!        do j=1, obj%nfft
!          write(*,*) 'ac(j)=',ac(j)
!        end do
!        write(*,*)'after hdr info added to array'
!        pause

!-------------------------------------------------------------------------------
! Save extreme values of parameters.
!-------------------------------------------------------------------------------
35      obj%gmin = amin1(obj%gmin,real(hd(HDR_ORIGINAL_GROUP,i)))
        obj%gmax = amax1(obj%gmax,real(hd(HDR_ORIGINAL_GROUP,i)))

!        write(*,*) 'gmin,gmax=',obj%gmin,',',obj%gmax

        xgp = hd(obj%irxgp,i) 
        ygp = 0.0
        if(obj%irygp /= 0) ygp = hd(obj%irygp,i)

        if(obj%nspec_disk == 1) then      ! initialized first time through
          obj%minxgp = nint(xgp) 
          obj%minygp = nint(ygp)
        else 
          do l = 1, MAXGPINT 
            if (mod(nint(xgp) - obj%minxgp,l) /= 0) ixinc(l) = 1 
            if (mod(nint(ygp) - obj%minygp,l) /= 0) iyinc(l) = 1
          end do 
        endif 

        obj%rminx = amin1(obj%rminx,float(nint(xgp))) 
        obj%rmaxx = amax1(obj%rmaxx,float(nint(xgp))) 
        obj%rminy = amin1(obj%rminy,float(nint(ygp))) 
        obj%rmaxy = amax1(obj%rmaxy,float(nint(ygp))) 

!        write(*,*) 'xgp,ygp ', xgp,', ',ygp
!        write(*,*) 'rec_coor=', obj%rec_coor
!        write(*,*) 'rminx,rmaxx ', obj%rminx,', ', obj%rmaxx
!        write(*,*) 'rminy,rmaxy ', obj%rminy,', ', obj%rmaxy
!        pause

!-------------------------------------------------------------------------------
! If mode == BOTH, write trace to disk also.
!-------------------------------------------------------------------------------
        if(obj%mode == 'BOTH') then
          obj%ntraces_disk = obj%ntraces_disk + 1 
          ier = trcio_write_trace( &
            obj%trace_file,hd(:,i),tr(:,i),obj%ntraces_disk)
        end if

      end do


!-------------------------------------------------------------------------------
! The rest of this subroutine pertains to the last pass (ntr == NO_MORE_TRACES).
!-------------------------------------------------------------------------------

      return

!-------------------------------------------------------------------------------
! FATAL_ERRORS come here.
!-------------------------------------------------------------------------------
999   ntr = FATAL_ERROR

      return


!-------------------------------------------------------------------------------
! Write summary stats to file.
!-------------------------------------------------------------------------------
1000  continue

!-------------------------------------------------------------------------------
! Automatic logic to find gp increment for receivers.
!-------------------------------------------------------------------------------
      obj%rxinc = 1.0 
      obj%ryinc = 1.0 
      do i = 2, MAXGPINT 
        l = MAXGPINT - i + 2 
        if (ixinc(l) == 1) obj%rxinc = l - 1 
        if (iyinc(l) == 1) obj%ryinc = l - 1 
      end do 

!-------------------------------------------------------------------------------
! Calculate number of sources and receivers.
!-------------------------------------------------------------------------------
      obj%nsrc  = obj%gmax  - obj%gmin + 1 
      obj%nrecx = obj%rmaxx - obj%rminx 
      obj%nrecy = obj%rmaxy - obj%rminy 

      modx = mod(obj%nrecx,nint(obj%rxinc))
      mody = mod(obj%nrecy,nint(obj%ryinc))

      if (modx /= 0 .or. mody /= 0) then 
        if(obj%irxgp == HDR_RECEIVER_XGRID) then
          str = 'SCDECON: Check header word 35 and 36.'
        else
          str = 'SCDECON: Check header word 47.'
        end if
        call pc_print('SCDECON: nrecx = ',obj%nrecx)
        call pc_print('SCDECON: nrecy = ',obj%nrecy)
        call pc_print('SCDECON: rxinc = ',obj%rxinc)
        call pc_print('SCDECON: ryinc = ',obj%ryinc)
        call pc_print('SCDECON: modx  = ',modx)
        call pc_print('SCDECON: mody  = ',mody)
        str = trim(str) // '  Error in receiver gp range and increment.'
        call pc_error(str)
        ntr = FATAL_ERROR
        return
      else 
        obj%nrecx = obj%nrecx/obj%rxinc + 1 
        obj%nrecy = obj%nrecy/obj%ryinc + 1 
      endif 

      obj%nrec = obj%nrecx * obj%nrecy 

!-------------------------------------------------------------------------------
! Write parameters to file.
!-------------------------------------------------------------------------------
      lun = obj%parm_file_lun

      j = cio_fputline('***********************************************', &
          120, lun)
      j = cio_fputline('*         DO NOT HAND-EDIT THIS FILE          *', &
          120, lun)
      j = cio_fputline('* IT IS FORMATTED FOR INTERNAL USE BY SCDECON *', &
          120, lun)
      j = cio_fputline('*********#*************************************', &
          120, lun)

      j = cio_fputline(obj%mode,           120, lun)

      j = cio_fputline(obj%opt_ampl,       120, lun)
      j = cio_fputline(obj%type_decon,     120, lun)
      j = cio_fputline(obj%opt_sc,         120, lun)
      j = cio_fputline(obj%rec_coor,       120, lun)

      call string_ii2cc(obj%iterations,str)
      j = cio_fputline('iterations                        = ' // str, 120, lun)

      call string_ff2cc(obj%len_op,str)
      j = cio_fputline('len_op                            = ' // str, 120, lun)

      call string_ff2cc(obj%diag_load,str)
      j = cio_fputline('diag_load                         = ' // str, 120, lun)

      call string_ii2cc(obj%max_traces,str)
      j = cio_fputline('max_traces                        = ' // str, 120, lun)

      call string_ff2cc(obj%off_init,str)
      j = cio_fputline('off_init                          = ' // str, 120, lun)

      call string_ff2cc(obj%off_inc,str)
      j = cio_fputline('off_inc                           = ' // str, 120, lun)

      call string_ii2cc(obj%off_tot,str)
      j = cio_fputline('off_tot                           = ' // str, 120, lun)

      j = cio_fputline(obj%param_pathname, 120, lun)
      j = cio_fputline(obj%spect_pathname, 120, lun)
      j = cio_fputline(obj%trace_pathname, 120, lun)

      call string_ii2cc(obj%nsrc,str)
      j = cio_fputline('# of sources                      = ' // str, 120, lun)

      call string_ii2cc(obj%nrec,str)
      j = cio_fputline('# of receivers                    = ' // str, 120, lun)

      call string_ii2cc(obj%nrecx,str)
      j = cio_fputline('nrecx                             = ' // str, 120, lun)

      call string_ii2cc(obj%nrecy,str)
      j = cio_fputline('nrecy                             = ' // str, 120, lun)

      call string_ii2cc(obj%nyq,str)
      j = cio_fputline('nyq                               = ' // str, 120, lun)

      call string_ii2cc(obj%nspec_disk,str)
      j = cio_fputline('# of spectra written to disk      = ' // str, 120, lun)

      call string_ii2cc(obj%ntraces_disk,str)
      j = cio_fputline('# of traces written to disk       = ' // str, 120, lun)

      call string_ff2cc(obj%gmin,str)
      j = cio_fputline('minimum source number             = ' // str, 120, lun)

      call string_ff2cc(obj%gmax,str)
      j = cio_fputline('maximum source number             = ' // str, 120, lun)

      call string_ff2cc(obj%rminx,str)
      j = cio_fputline('receiver x group minimum          = ' // str, 120, lun)

      call string_ff2cc(obj%rmaxx,str)
      j = cio_fputline('receiver x group maximum          = ' // str, 120, lun)

      call string_ff2cc(obj%rxinc,str)
      j = cio_fputline('receiver x group increment        = ' // str, 120, lun)

      call string_ff2cc(obj%rminy,str)
      j = cio_fputline('receiver y group minimum          = ' // str, 120, lun)

      call string_ff2cc(obj%rmaxy,str)
      j = cio_fputline('receiver y group maximum          = ' // str, 120, lun)

      call string_ff2cc(obj%ryinc,str)
      j = cio_fputline('receiver y group increment        = ' // str, 120, lun)

      call string_ii2cc(obj%naclh,str)
      j = cio_fputline('# of points in operator           = ' // str, 120, lun)

      call string_ii2cc(obj%nfft,str)
      j = cio_fputline('# of points in transform          = ' // str, 120, lun)


!-------------------------------------------------------------------------------
! Close files.
!-------------------------------------------------------------------------------
      if(obj%mode == 'BOTH') then
        if(associated(obj%trace_file)) then
          ier = trcio_close(obj%trace_file)
          if(ier > 0) then
            call pc_error('SCDECON: error closing trace file')
            ntr = FATAL_ERROR
            return
          end if
        end if
      end if

!---- flush and close spectra file
      ier = cio_fflush(obj%spec_file_lun)
      if(ier > 0) then
        call pc_error('SCDECON: error flushing spec file')
        ntr = FATAL_ERROR
        return
      end if

      ier = cio_fclose(obj%spec_file_lun)
      if(ier > 0) then
        call pc_error('SCDECON: error closing spec file')
        ntr = FATAL_ERROR
        return
      end if

!---- flush and close parameter file
      ier = cio_fflush(lun)
      if(ier > 0) then
        call pc_error('SCDECON: error flushing param file')
        ntr = FATAL_ERROR
        return
      end if

      ier = cio_fclose(lun)
      if(ier > 0) then
        call pc_error('SCDECON: error closing param file')
        ntr = FATAL_ERROR
        return
      end if

      return
      end subroutine scdecon_prep


!-------------------------------------------------------------------------------
!
! Subroutine for APPLY mode.
!
!-------------------------------------------------------------------------------
      subroutine scdecon_apply(obj,ntr,hd,tr)
      implicit none
      type(scdecon_struct),intent(inout) :: obj                 ! arguments
      integer          ,intent(inout)    :: ntr                 ! arguments
      double precision ,intent(inout)    :: hd(:,:)             ! arguments
      real             ,intent(inout)    :: tr(:,:)             ! arguments


      integer                         :: i,j,k    ,ier 
      integer                         :: it
      integer                         :: isrc,irec,ioff
      integer                         :: idx,idxi,idxo
      integer                         :: ictr

      integer                         :: nyq,nadj

      real                            :: factor,denom
      double precision                ::         xgpt,ygpt  

      real                            :: omin

      integer,allocatable             :: cnt(:)

      real                            :: scra(4*(obj%ndpt + obj%naclh + 1))

      real                            :: filt_buf(obj%naclh + 2)

      real                            :: ac(obj%nfft)
      complex                         :: spec(obj%nfft)

      double precision                :: filt_hd(obj%nwih)          ! debug

      if (.not.associated(obj%src)) allocate (obj%src(obj%nsrc    * obj%nyq))
      if (.not.associated(obj%rec)) allocate (obj%rec(obj%nrec    * obj%nyq))
      if (.not.associated(obj%off)) allocate (obj%off(obj%off_tot * obj%nyq))

!-------------------------------------------------------------------------------
! Bypass entirely if no more traces.
!-------------------------------------------------------------------------------
      if(ntr == NO_MORE_TRACES) go to 1000

!      factor = 0.;
!      do j=1,obj%ndpt
!        write(*,*) tr(j,1)
!        if(abs(tr(j,1)) > factor) factor=abs(tr(j,1))
!      end do
!      write(*,*) 'start of apply, max was ',factor

!-------------------------------------------------------------------------------
! Let's simplify some variable names.
!-------------------------------------------------------------------------------
      nyq  = obj%nyq

!-------------------------------------------------------------------------------
! If this is the first call to the apply function, allocate memory and calculate
! operators.  Otherwise, bypass this entire section.
!-------------------------------------------------------------------------------
      if(obj%operators_already_calculated) then
        go to 500
      else
        obj%operators_already_calculated = .true.
        call scdecon_parameter_dump(obj)
      end if

!-------------------------------------------------------------------------------
! More initializations and checks.
!-------------------------------------------------------------------------------
      filt_hd(:) = 0.

!---- check for empty file
      if(obj%nspec_disk == 0) then
        call pc_error('There do not appear to be any spectra to read.')
        go to 996
      end if

!---- check filesize and see if it makes sense relative to nspec and len_op?
!     ???

!---- continue
      obj%src(:) = 0.
      obj%rec(:) = 0.
      obj%off(:) = 0.

      if(obj%opt_sc == 'SR') then
        obj%off_inc = 0.       ! this is used as a flag
        omin = 0
      else
        omin = obj%off_init - .5*obj%off_inc
      end if

!---- safety
      if(nyq /= obj%nfft/2+1) then
        call pc_error('nyq mismatch')
        go to 996
      end if


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Perform Gauss-Siedel iterations.
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!-------------------------------------------------------------------------------
! Start looping.
!-------------------------------------------------------------------------------
      call pc_print('NSRC=', obj%nsrc)
      call pc_print('NREC=', obj%nrec)
      call pc_print('NOFF=', obj%off_tot)

      do it = 1, obj%iterations

        call pc_print('SCDECON: Working on Gauss-Seidel iteration # ', it)

!-------------------------------------------------------------------------------
! Do source operators first (mandatory).
!-------------------------------------------------------------------------------
        ier = cio_fseek(obj%spec_file_lun,0,0)

        allocate(cnt(obj%nsrc))

        obj%src(:) = 0
        cnt(:) = 0

        do j = 1, obj%nspec_disk

          ac(:) = 0.
          ier = cio_fread(ac,4,nyq+4,obj%spec_file_lun)

          isrc = nint(ac(nyq+2)-obj%gmin)*nyq 

          irec = nyq * (scdecon_istatr(obj,ac(nyq+3),ac(nyq+4)) - 1)

          if(obj%off_inc /= 0) then
            ioff = int((ac(nyq+1)-omin)/obj%off_inc)
          else
            ioff = 0
          end if

          if(ioff > obj%off_tot) ioff = obj%off_tot
          ioff=ioff*nyq

          idx = nint(ac(nyq+2)-obj%gmin) + 1 

          if(idx < 1 .or. idx > obj%nsrc) then
            call pc_error('SCDECON: bad source idx')
            go to 996
          end if

          cnt(idx) = cnt(idx) + 1

!-------- loop over all frequencies
          obj%src(isrc+1:nyq+isrc) = obj%src(isrc+1:nyq+isrc) + &
            (ac(:nyq)-obj%rec(irec+1:nyq+irec)-obj%off(ioff+1:nyq+ioff) - &
                      obj%src(isrc+1:nyq+isrc)) / cnt(idx) 

!          call debug_print(obj%src,isrc+1,nyq)
!          write(*,*) 'src after isrc=',isrc
!          call debug_print(obj%rec,irec+1,nyq)
!          write(*,*) 'rec after irec=',irec
!          call debug_print(obj%off,ioff+1,nyq)
!          write(*,*) 'off after ioff=',ioff
!          write(*,*) 'it = ',it

!          write(*,*) 'ioff1 = ',ioff

!          write(*,*) '1: ',j,' ',isrc,' ',irec,' ',ioff,' ',idx,' ',cnt(idx)
!          pause

        end do 

        deallocate(cnt)

!-------------------------------------------------------------------------------
! Solve for receiver operators (optional).
!-------------------------------------------------------------------------------
        if(obj%opt_sc /= 'SO') then

          ier = cio_fseek(obj%spec_file_lun,0,0)

          allocate(cnt(obj%nrec))

          obj%rec(:) = 0.0
          cnt(:) = 0.0

          do j = 1, obj%nspec_disk
          
            ac(:) = 0.
            ier = cio_fread(ac,4,nyq+4,obj%spec_file_lun)

            isrc = nint(ac(nyq+2)-obj%gmin)*nyq 

            irec = nyq * (scdecon_istatr(obj,ac(nyq+3),ac(nyq+4)) - 1)

            if(obj%off_inc /= 0) then
              ioff = int((ac(nyq+1)-omin)/obj%off_inc)
            else
              ioff = 0
            end if

            if(ioff > obj%off_tot) ioff = obj%off_tot
            ioff=ioff*nyq

            idx = scdecon_istatr(obj,ac(nyq+3),ac(nyq+4))

            if(idx < 1 .or. idx > obj%nrec) then
              call pc_error('SCDECON: bad receiver idx')
              go to 996
            end if

            cnt(idx) = cnt(idx) + 1

!---------- loop over all frequencies
            obj%rec(irec+1:nyq+irec) = obj%rec(irec+1:nyq+irec) + &
              (ac(:nyq)-obj%src(isrc+1:nyq+isrc)-obj%off(ioff+1:nyq+ioff) - &
                        obj%rec(irec+1:nyq+irec)) / cnt(idx)


          end do

          deallocate(cnt)

        end if

!-------------------------------------------------------------------------------
! Solve for offset operators (optional).
!-------------------------------------------------------------------------------
        if(obj%opt_sc /= 'SR') then

          ier = cio_fseek(obj%spec_file_lun,0,0)

          allocate(cnt(obj%off_tot))

          obj%off(:) = 0.0
          cnt(:) = 0.0

          do j = 1, obj%nspec_disk

            ac(:) = 0.
            ier = cio_fread(ac,4,nyq+4,obj%spec_file_lun)

            isrc = nint(ac(nyq+2)-obj%gmin)*nyq 

            irec = nyq * (scdecon_istatr(obj,ac(nyq+3),ac(nyq+4)) - 1)

            if(obj%off_inc /= 0) then
              ioff = int((ac(nyq+1)-omin)/obj%off_inc)
            else
              ioff = 0
            end if

            if(ioff > obj%off_tot) ioff = obj%off_tot
            idx = ioff + 1
            ioff=ioff*nyq

            if(idx < 1 .or. idx > obj%off_tot) then
              call pc_error('SCDECON: bad offset idx')
              go to 996
            end if

            cnt(idx) = cnt(idx) + 1

!---------- loop over all frequencies
            obj%off(ioff+1:nyq+ioff) = obj%off(ioff+1:nyq+ioff) + &
              (ac(:nyq)-obj%src(isrc+1:nyq+isrc)-obj%rec(irec+1:nyq+irec) - &
                        obj%off(ioff+1:nyq+ioff)) / cnt(idx)


          end do 

          deallocate(cnt)

        end if

      end do   ! end of Gauss-Siedel iterations

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Convert decomposed spectra into decon operators.
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Loop over all source operators (mandatory).
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        call pc_print( &
          'SCDECON: Converting src spectra to decon operators, nsrc = ', &
          obj%nsrc)

        do j = 1, obj%nsrc 
          idx = (j - 1)*nyq 

          if(scdecon_all_zeroes(obj%src(idx+1:idx+nyq+1),nyq)) then
            obj%allzero1    = obj%allzero1 + 1
            cycle
          else
            obj%notallzero1 = obj%notallzero1 + 1
          end if

!-------------------------------------------------------------------------------
!   go from log spectra to power spectrum (decon)
!                   or
!   go from log spectra to inverse amplitude spectrum (norm)
!-------------------------------------------------------------------------------
          if (obj%opt_ampl == 'A') then 
            scra(1) = 1.0/exp(obj%src(idx+1)) 
          else
            spec(:) = cmplx(0.,0.)
            if (obj%type_decon == 'DECON') then 
              do k = 1, nyq
                spec(k) = cmplx(exp(obj%src(idx+k)),0.) 
              end do 
            else
              do k = 1, nyq 
                denom = sqrt(exp(obj%src(idx+k)))
                if(denom /= 0.) then
                  spec(k) = cmplx(1./denom,0.)
                else
                  spec(k) = cmplx(0.,0.)
                end if
              end do 
            end if 

!---------- prevent NaN values
            spec(obj%nfft/2+1) = cmplx(0.,0.)

!            write(*,*) 'fftsize   = ', obj%fftcr%size
!            write(*,*) 'obj%naclh = ', obj%naclh
!            write(*,*) 'obj%nfft  = ', obj%nfft
!            do k=1,size(spec)
!              write(*,*) 'src, spec ',k, '->', obj%src(idx+k),', ',real(spec(k))
!            end do
!            write(*,*) 'after src'
!            pause

!-------------------------------------------------------------------------------
!   transform power spectrum into autocorrelation (decon)
!                       or
!   transform amplitude spectrum into inverse operator (norm)
!-------------------------------------------------------------------------------
            ac(:) = 0
            call fft_cr_transform(obj%fftcr,spec,ac)

!            do k=1,size(ac)
!              write(*,*) 'ac ',k, '->', ac(k)
!            end do
!            write(*,*) 'end of autocorr'
!            pause

!-------------------------------------------------------------------------------
! Normalize ac by transform size.
!-------------------------------------------------------------------------------
            ac(:) = ac(:)/obj%nfft 

!-------------------------------------------------------------------------------
! Create minimum phase inverse to ac or store zero phase operator for filterg.
!-------------------------------------------------------------------------------
            if (obj%type_decon == 'DECON') then
              scra(:) = 0.
              scra(obj%naclh+1)  = 1.0 
              call opfilt(                          &
                obj%naclh,                          &
                scra(            1 :   obj%naclh),  &
                scra(  obj%naclh+1 : 2*obj%naclh),  & 
                scra(2*obj%naclh+1 : 4*obj%naclh),  &
                ac)

            else
              call scdecon_get_compensation_factor(obj,ac(1),factor,ier)
              if(ier > 0) go to 996

              ictr = idx + obj%naclh/2 + 1   ! center of operator (naclh is odd)
              obj%src(ictr)                         = factor*ac(1) 
              obj%src(ictr+1:obj%naclh/2+ictr)      = factor*ac(2:obj%naclh/2+1) 
              obj%src(ictr-1:ictr-obj%naclh/2:(-1)) = factor*ac(2:obj%naclh/2+1) 

              cycle
            end if
          end if 

!-------------------------------------------------------------------------------
! Scale to compensate for source.
!-------------------------------------------------------------------------------
          call scdecon_get_compensation_factor(obj,scra(1),factor,ier)
          if(ier > 0) go to 996

          obj%src(idx+obj%naclh:idx+1:(-1)) = scra(:obj%naclh)*factor

        end do 

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Loop over all receiver operators (optional).
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        if(obj%opt_sc /= 'SO') then

          call pc_print( &
            'SCDECON: Converting rec spectra to decon operators, nrec = ', &
            obj%nrec)

          do j = 1, obj%nrec 
            idx = (j - 1)*nyq 

            if(scdecon_all_zeroes(obj%rec(idx+1:idx+nyq-1),nyq)) then
              obj%allzero2    = obj%allzero2 + 1
              cycle
            else
              obj%notallzero2 = obj%notallzero2 + 1
            end if

!-------------------------------------------------------------------------------
!   go from log spectra to power spectrum (decon)
!                   or
!   go from log spectra to inverse amplitude spectrum (zero_phase)
!-------------------------------------------------------------------------------
            if (obj%opt_ampl == 'A') then 
              scra(1) = 1.0/exp(obj%rec(idx+1)) 
            else 
              if (obj%type_decon == 'DECON') then 
                spec(:) = cmplx(0.,0.)
                do k = 1, nyq 
                  spec(k) = cmplx(exp(obj%rec(idx+k)),0.) 
                end do 
              else 
                do k = 1, nyq 
                  denom = sqrt(exp(obj%rec(idx+k)))
                  if(denom /= 0.) then
                    spec(k) = cmplx(1./denom,0.)
                  else
                    spec(k) = cmplx(0.,0.)
                  end if
                end do 
              end if 

!------------ prevent NaN values
              spec(obj%nfft/2+1) = cmplx(0.,0.)

!              write(*,*) 'fftsize   = ', obj%fftcr%size
!              write(*,*) 'obj%naclh = ', obj%naclh
!              write(*,*) 'obj%nfft  = ', obj%nfft
!              do k=1,size(spec)
!                write(*,*) 'rec, spec ',k, '->', obj%rec(idx+k),', ',real(spec(k))
!              end do
!              pause

!-------------------------------------------------------------------------------
!   transform power spectrum into autocorrelation (decon)
!                       or
!   transform amplitude spectrum into inverse operator (norm)
!-------------------------------------------------------------------------------
              call fft_cr_transform(obj%fftcr,spec,ac)

!-------------------------------------------------------------------------------
! Normalize ac by transform size.
!-------------------------------------------------------------------------------
              ac(:) = ac(:)/obj%nfft 

!-------------------------------------------------------------------------------
! Create minimum phase inverse to ac or store zero phase operator for filterg.
!-------------------------------------------------------------------------------
              if (obj%type_decon == 'DECON') then
                scra(:) = 0.
                scra(obj%naclh+1)  = 1.0 
                call opfilt(                          &
                  obj%naclh,                          &
                  scra(            1 :   obj%naclh),  &
                  scra(  obj%naclh+1 : 2*obj%naclh),  & 
                  scra(2*obj%naclh+1 : 4*obj%naclh),  &
                  ac)

              else
                call scdecon_get_compensation_factor(obj,ac(1),factor,ier)
                if(ier > 0) go to 996

                ictr = idx + obj%naclh/2 + 1 ! center of operator (naclh is odd)
                obj%rec(ictr)                         = factor*ac(1) 
                obj%rec(ictr+1:obj%naclh/2+ictr)      = factor*ac(2:obj%naclh/2+1) 
                obj%rec(ictr-1:ictr-obj%naclh/2:(-1)) = factor*ac(2:obj%naclh/2+1) 

                cycle  
              end if 
            end if 

!-------------------------------------------------------------------------------
! Scale to compensate for receiver.
!-------------------------------------------------------------------------------
            call scdecon_get_compensation_factor(obj,scra(1),factor,ier)
            if(ier > 0) go to 996

            obj%rec(idx+obj%naclh:idx+1:(-1)) = scra(:obj%naclh)*factor

          end do 

        end if


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Loop over all offset operators (optional).
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        if(obj%opt_sc /= 'SR') then

          call pc_print( &
            'SCDECON: Converting off spectra to decon operators, noff = ', &
            obj%off_tot)

          do j = 1, obj%off_tot 
            idx = (j - 1)*nyq 

            if(scdecon_all_zeroes(obj%off(idx+1:idx+nyq-1),nyq)) then
              obj%allzero3    = obj%allzero3 + 1
              cycle
            else
              obj%notallzero3 = obj%notallzero3 + 1
            end if

!-------------------------------------------------------------------------------
!   go from log spectra to power spectrum (decon)
!                   or
!   go from log spectra to inverse amplitude spectrum (zero_phase)
!-------------------------------------------------------------------------------
            if (obj%opt_ampl == 'A') then 
              scra(1) = 1.0/exp(obj%off(idx+1)) 
            else 
              if (obj%type_decon == 'DECON') then 
                spec(:) = cmplx(0.,0.)
                do k = 1, nyq 
                  spec(k) = cmplx(exp(obj%off(idx+k)),0.)
                end do 
              else 
                do k = 1, nyq 
                  denom = sqrt(exp(obj%off(idx+k)))
                  if(denom /= 0.) then
                    spec(k) = cmplx(1./denom,0.)
                  else
                    spec(k) = cmplx(0.,0.)
                  end if
                end do 
              end if 

!------------ prevent NaN values
              spec(obj%nfft/2+1) = cmplx(0.,0.)

!              write(*,*) 'fftsize   = ', obj%fftcr%size
!              write(*,*) 'obj%naclh = ', obj%naclh
!              write(*,*) 'obj%nfft  = ', obj%nfft
!              do k=1,size(spec)
!                write(*,*) 'off, spec ',k, '->', obj%off(idx+k),', ',real(spec(k))
!              end do
!              pause

!-------------------------------------------------------------------------------
!   transform power spectrum into autocorrelation (decon)
!                       or
!   transform amplitude spectrum into inverse operator (zero_phase)
!-------------------------------------------------------------------------------
              call fft_cr_transform(obj%fftcr,spec,ac)

!-------------------------------------------------------------------------------
! Normalize ac by transform size.
!-------------------------------------------------------------------------------
              ac(:) = ac(:)/obj%nfft 

!-------------------------------------------------------------------------------
! Create minimum phase inverse to ac or store zero phase operator for filterg.
!-------------------------------------------------------------------------------
              if (obj%type_decon == 'DECON') then
                scra(:) = 0.
                scra(obj%naclh+1)  = 1.0 
                call opfilt(                          &
                  obj%naclh,                          &
                  scra(            1 :   obj%naclh),  &
                  scra(  obj%naclh+1 : 2*obj%naclh),  & 
                  scra(2*obj%naclh+1 : 4*obj%naclh),  &
                  ac)

              else
                call scdecon_get_compensation_factor(obj,ac(1),factor,ier)
                if(ier > 0) go to 996

                ictr = idx + obj%naclh/2 + 1 ! center of operator (naclh is odd)
                obj%off(ictr)                         = factor*ac(1) 
                obj%off(ictr+1:obj%naclh/2+ictr)      = factor*ac(2:obj%naclh/2+1) 
                obj%off(ictr-1:ictr-obj%naclh/2:(-1)) = factor*ac(2:obj%naclh/2+1) 

                cycle
              end if 
            end if 

!-------------------------------------------------------------------------------
! Scale to compensate for offset.
!-------------------------------------------------------------------------------
            call scdecon_get_compensation_factor(obj,scra(1),factor,ier)
            if(ier > 0) go to 996

            obj%off(idx+obj%naclh:idx+1:(-1)) = scra(:obj%naclh)*factor

          end do 

        end if

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Can write operators to debug file.  Add a zero-value sample at the top and
!  bottom so that cbyt will still be able to plot the obj%naclh=1 case.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       if(obj%filt_pathname /= PATHCHECK_EMPTY) then

          filt_buf(1)           = 0
          filt_buf(obj%naclh+1) = 0
          do k=1,obj%nsrc
            obj%filt_ndisk = obj%filt_ndisk + 1
            j = 1 + (k-1)*nyq
            filt_hd(1) = obj%filt_ndisk
            filt_hd(3) = 1
            filt_hd(4) = k
            filt_buf(2:obj%naclh+1) = obj%src(j:j+obj%naclh-1)
            ier = trcio_write_trace( &
              obj%filt_file,filt_hd,filt_buf,obj%filt_ndisk)
          end do

          filt_buf(1)           = 0
          filt_buf(obj%naclh+1) = 0
          do k=1,obj%nrec
            obj%filt_ndisk = obj%filt_ndisk + 1
            j = 1 + (k-1)*nyq
            filt_hd(1) = obj%filt_ndisk
            filt_hd(3) = 2
            filt_hd(4) = k
            filt_buf(2:obj%naclh+1) = obj%rec(j:j+obj%naclh-1)
            ier = trcio_write_trace( &
              obj%filt_file,filt_hd,filt_buf,obj%filt_ndisk)
          end do

          filt_buf(1)           = 0
          filt_buf(obj%naclh+1) = 0
          do k=1,obj%off_tot
            obj%filt_ndisk = obj%filt_ndisk + 1
            filt_hd(1) = obj%filt_ndisk
            filt_hd(3) = 3
            filt_hd(4) = k
            j = 1 + (k-1)*nyq
            filt_buf(2:obj%naclh+1) = obj%off(j:j+obj%naclh-1)
            ier = trcio_write_trace( &
              obj%filt_file,filt_hd,filt_buf,obj%filt_ndisk)
          end do

       end if

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Finally, we're ready to apply operators to traces and output.
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       call pc_print('SCDECON: Applying operators...')

!-------------------------------------------------------------------------------
! Initializations.
!-------------------------------------------------------------------------------
500   continue

      do i=1,ntr

!------ skip application to dead traces
        if(hd(HDR_LAV,i) == 0.) then
          tr(:,i) = 0.
          cycle
        end if

        scra(:) = 0.
        nadj = obj%naclh 
        if (obj%type_decon == 'ZERO_PHASE') nadj = obj%naclh/2 + 1 

!-------------------------------------------------------------------------------
! Set indices for operators to use.
!-------------------------------------------------------------------------------
        xgpt = hd(obj%irxgp,i)
        ygpt = 0.0 
        if (obj%irygp > 0) ygpt = hd(obj%irygp,i)

!        write(*,*) ' '
!        write(*,*) 'NYQs=',obj%nyq,', ',nyq
!        write(*,*) 'gmin, omin, off_inc=',obj%gmin,', ',omin,', ',obj%off_inc

!        write(*,*) 'xgp,ygp=',xgp,',',ygp

        isrc=mth_bin_number(dble(obj%gmin),dble(1.0/nyq),&
                            hd(HDR_ORIGINAL_GROUP,i))
!rev16        isrc = nint(hd(HDR_ORIGINAL_GROUP,i)-obj%gmin)*nyq + 1 
        irec = (scdecon_istatr(obj,real(xgpt),real(ygpt)) - 1)*nyq + 1
        ioff = int((hd(HDR_OFFSET,i)-omin)/obj%off_inc)*nyq + 1 

!        write(*,*) isrc, ', ', irec, ', ', ioff
!        pause

        idxi = 1 
        idxo = idxi + obj%ndpt + obj%naclh - 1

!-------------------------------------------------------------------------------
! Copy input trace to scratch.
!-------------------------------------------------------------------------------
        scra(:) = 0.
        scra(nadj:nadj+obj%ndpt-1) = tr(:,i)

!        factor = 0.;
!        do j=1,obj%ndpt
!          write(*,*) tr(j,i)
!          if(abs(tr(j,i)) > factor) factor=abs(tr(j,i))
!        end do
!        write(*,*) 'end of trace 1, max was ',factor

!-------------------------------------------------------------------------------
! Apply source operator (all cases).
!-------------------------------------------------------------------------------
        call fltr_filterg ( &
            obj%src(isrc:), obj%naclh, &
            scra(idxi:), obj%naclh + obj%ndpt - 1, &
            scra(idxo+nadj-1:))

!        do j=1,obj%naclh
!          write(*,*) obj%src(isrc+j-1)
!        end do
!        write(*,*) 'end of source operator, isrc=',isrc
!        write(*,*) 'nyq,naclh were ',obj%nyq,',',obj%naclh
!        pause

!-------------------------------------------------------------------------------
! Conditionally apply receiver operator.
!-------------------------------------------------------------------------------
        if(obj%opt_sc /= 'SO') then
          idxi = idxo
          idxo = idxi + obj%ndpt + obj%naclh - 1

!          do j=1,obj%naclh
!            write(*,*) obj%rec(irec+j-1)
!          end do
!          write(*,*) 'end of receiver operator, irec=',irec
!          pause

          call fltr_filterg ( &
            obj%rec(irec:), obj%naclh, &
            scra(idxi:), obj%naclh + obj%ndpt - 1, &
            scra(idxo+nadj-1:))
        end if

!-------------------------------------------------------------------------------
! Conditionally apply offset operator.
!-------------------------------------------------------------------------------
        if(obj%opt_sc /= 'SR') then
          idxi = idxo
          idxo = idxi + obj%ndpt + obj%naclh - 1


          call fltr_filterg ( &
            obj%off(ioff:), obj%naclh, &
            scra(idxi:), obj%naclh + obj%ndpt - 1, &
            scra(idxo+nadj-1:))
        endif 

!-------------------------------------------------------------------------------
! Copy scratch back to trace.  Different modes require different shifts.
!-------------------------------------------------------------------------------
        if(obj%type_decon == 'DECON') then
          if(obj%opt_ampl == 'A') then
            idxo = idxo + obj%naclh - 1
          else
            idxo = idxo + obj%naclh - 1
          end if
        else if(obj%type_decon == 'ZERO_PHASE') then
          if(obj%opt_ampl == 'A') then
            idxo = idxo + obj%naclh/2 - 2
          else
            idxo = idxo + obj%naclh/2
          end if
        else                       ! safety net
          call pc_error( &
            'SCDECON: program logic error, type_decon was ', obj%type_decon)
          go to 996
        end if

        tr(:,i) = scra(idxo:idxo+obj%ndpt-1)

!        factor = 0.;
!        do j=1,obj%ndpt
!          write(*,*) tr(j,i)
!          if(abs(tr(j,i)) > factor) factor=abs(tr(j,i))
!        end do
!        write(*,*) 'end of trace 2, max was ',factor
!        pause

      end do

!-------------------------------------------------------------------------------
! Reset LAV and return.
!-------------------------------------------------------------------------------

      call lav_set_hdr(hd, tr, obj%ndpt, ntr)


      return

!-------------------------------------------------------------------------------
! Fatal errors come here.
!-------------------------------------------------------------------------------
  996 call pc_error('SCDECON: fatal error occurred.')
      ntr=FATAL_ERROR

 1000 continue

      return

      end subroutine scdecon_apply


!-------------------------------------------------------------------------------
! Subroutine to read a trace or gather from disk (pertains to BOTH mode only).
!   (returns ntr > 0 or FATAL_ERROR)
!-------------------------------------------------------------------------------
      function scdecon_get_trace_or_gather(obj,hd,tr) result(ntr)
      implicit none
      type(scdecon_struct),intent(inout) :: obj
      double precision ,intent(inout)    :: hd(:,:)
      real             ,intent(inout)    :: tr(:,:)
 
      integer                            :: i,j  ,istat,current_gather 
      integer                            :: ntr


!---- for gathered data, loop breaks when hd(3) changes
!---- for data that isn't gathered, loop only reads 1 trace
      j = 0
      do i=obj%bothCount,obj%ntraces_disk

        j = j + 1

        if(j > obj%numtr) go to 100

        istat = trcio_read_trace(obj%trace_file,hd(:,j),tr(:,j),i)

!        if(j==1) then
!          max = 0.;
!          do k=1,obj%ndpt
!            write(*,*) tr(k,j)
!            if(abs(tr(k,j)) > max) max=abs(tr(k,j))
!          end do
!          write(*,*) 'at trace input stage, max was ',max
!        end if

        if(istat == TRCIO_ERROR) then
          call pc_error('SCDECON: TRCIO_ERROR encountered at i=',i)
          ntr = FATAL_ERROR
          return
        else if(istat == TRCIO_EOF) then
          call pc_error('SCDECON: TRCIO_EOF encountered at i=',i)
          ntr = FATAL_ERROR
          return
        else if(istat == TRCIO_OK) then
          if(.not. obj%gathered) then
            ntr = 1
            go to 100
          else if(j==1) then
            current_gather = hd(3,j)
            ntr = 1
          else
            if(hd(3,j) == current_gather) then
              ntr = j
            else
              go to 100
            end if
          end if
        else
          call pc_error('SCDECON: Unhandled TRCIO status at i=',i)
          ntr = FATAL_ERROR
          return
        end if

      end do

 100  obj%bothCount = obj%bothCount + ntr

!      write(*,*) 'paused in get_trace_or_gather' ; pause

      end function scdecon_get_trace_or_gather


!-------------------------------------------------------------------------------
! Subroutine to calculate final compensation factor.
!-------------------------------------------------------------------------------
      subroutine scdecon_get_compensation_factor(obj,max,factor,ier)
      implicit none
      type(scdecon_struct),intent(in) :: obj
      real, intent(in)                :: max
      real, intent(out)               :: factor
      integer, intent(out)            :: ier
 
      ier = 0

      if(obj%type_decon == 'DECON') then
        if(obj%opt_ampl ==  'D') factor = 1.0/max
        if(obj%opt_ampl ==  'A') factor = 1.0/sqrt(max)
        if(obj%opt_ampl == 'DA') factor = 1.0/sqrt(max)
      else if(obj%type_decon == 'ZERO_PHASE') then
        if(obj%opt_ampl ==  'D') factor = 1.0/sqrt(max)
        if(obj%opt_ampl ==  'A')  ier = 1                ! should not occur
        if(obj%opt_ampl == 'DA') factor = 1
      else ! should never occur
        call pc_error('SCDECON: logic error in compensate calc.')
        ier = 2
      end if

      end subroutine scdecon_get_compensation_factor

!-------------------------------------------------------------------------------
! Subroutine to open files (read or write, depending on situation).
!-------------------------------------------------------------------------------
      subroutine scdecon_open_files (obj,read_file_mode)
      implicit none
      type(scdecon_struct),intent(inout) :: obj
      logical,intent(in)                 :: read_file_mode

      integer                            :: ier

!      obj%param_pathname = trim(obj%param_pathname)
!      obj%spect_pathname = trim(obj%spect_pathname)
!      obj%trace_pathname = trim(obj%trace_pathname)

!-------------------------------------------------------------------------------
! If read situation, open files for reading spectra and (optionally) traces.
!-------------------------------------------------------------------------------
      if(read_file_mode) then

        call pc_print(' ')
        call pc_print('SCDECON will read files =')
        call pc_print('    ',obj%param_pathname)
        call pc_print('    ',obj%spect_pathname)
        if(obj%mode == 'BOTH') &
        call pc_print('    ',obj%trace_pathname)

        obj%spec_file_lun = cio_fopen(obj%spect_pathname, 'r')
        if(obj%spec_file_lun <= 0) then
          call pc_error ( &
            'read open error, spect_pathname = ', obj%spect_pathname)
        end if

        if(obj%mode == 'BOTH') then

          obj%trace_file => trcio_open(obj%trace_pathname, 'r')
          if(.not. associated(obj%trace_file)) then
            call pc_error('read open error, trace_pathname = ', &
              obj%trace_pathname)
            return
          end if

          if(obj%filt_pathname /= PATHCHECK_EMPTY) then
            obj%filt_file => trcio_open(obj%filt_pathname, 'w')
            if(.not. associated(obj%filt_file)) then
              call pc_error('write open error, filt_pathname = ', &
                obj%filt_pathname)
              return
            end if

            obj%filt_file%num_values = obj%naclh + 2
            obj%filt_file%tmin       = 0.
            obj%filt_file%dt         = obj%dt
            obj%filt_file%nwih       = obj%nwih
            obj%filt_file%nbits      = 32
            obj%filt_file%nbits_hd   = 64
            ier = trcio_writeheader(obj%filt_file)
          end if

        end if

      else

        call pc_print(' ')
        call pc_print('SCDECON will write files =')
        call pc_print('    ',obj%param_pathname)
        call pc_print('    ',obj%spect_pathname)
        if(obj%mode == 'BOTH') &
        call pc_print('    ',obj%trace_pathname)
        call pc_print(' ')
 

!-------------------------------------------------------------------------------
! If write situation, open files for writing spectra and (optionally) traces.
!-------------------------------------------------------------------------------
        obj%spec_file_lun = cio_fopen(obj%spect_pathname, 'w')
        if(obj%spec_file_lun <= 0) then
          call pc_error ( &
            'write open error, spect_pathname = ', obj%spect_pathname)
        end if

        if(obj%mode == 'BOTH') then
          obj%trace_file => trcio_open(obj%trace_pathname, 'w')
          if(.not. associated(obj%trace_file)) then
            call pc_error ( &
              'write open error, trace_pathname = ', obj%trace_pathname)
            return
          end if

          obj%trace_file%num_values = obj%ndpt
          obj%trace_file%tmin       = obj%tstrt
          obj%trace_file%dt         = obj%dt
          obj%trace_file%nwih       = obj%nwih
          obj%trace_file%nbits      = 32
          obj%trace_file%nbits_hd   = 64

          ier = trcio_writeheader(obj%trace_file)

          if(ier /= TRCIO_OK) then
            call pc_error ( &
              "trcio_writeheader: trace_pathname= " // obj%trace_pathname)
            return
          end if

        end if

      end if

!-------------------------------------------------------------------------------
! Prepare to read or write parameter file (mandatory for all modes).
!-------------------------------------------------------------------------------
      if(read_file_mode) then
        obj%parm_file_lun = cio_fopen(obj%param_pathname, 'r')
      else
        obj%parm_file_lun = cio_fopen(obj%param_pathname, 'w')
      end if

      if(obj%parm_file_lun <= 0) then
        call pc_error ( &
          'file error, param_pathname = ', obj%param_pathname)
      end if

!-------------------------------------------------------------------------------
! Finally, read parameter file if necessary.
!-------------------------------------------------------------------------------
      if(read_file_mode) call scdecon_read_parameter_file(obj)

      return
      end subroutine scdecon_open_files


!-------------------------------------------------------------------------------
! Subroutine to read parameter file (already open).
!-------------------------------------------------------------------------------
      subroutine scdecon_read_parameter_file (obj)
      implicit none
      type(scdecon_struct),intent(inout) :: obj                 ! arguments

      character(len=120)              :: str
      integer                         :: ier,j,lun

!---- assign to local variable for readibility
      lun = obj%parm_file_lun

!---- initialize
      str = ''

!---- heading (discard)
      j = cio_fgetline(str, 120, lun)
      j = cio_fgetline(str, 120, lun)
      j = cio_fgetline(str, 120, lun)
      j = cio_fgetline(str, 120, lun)

!---- 'mode' when file was written (discard)
      j = cio_fgetline(str, 120, lun)

!---- more string choices
      j = cio_fgetline(str, 120, lun)
      obj%opt_ampl   = trim(str)

      j = cio_fgetline(str, 120, lun)
      obj%type_decon = trim(str)

      j = cio_fgetline(str, 120, lun)
      obj%opt_sc     = trim(str)

      j = cio_fgetline(str, 120, lun)
      obj%rec_coor   = trim(str)

!---- more parameters
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%iterations) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%len_op) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%diag_load) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%max_traces) 

!---- offset scheme (off_last is redundant)
      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%off_init) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%off_inc) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%off_tot) 

!---- filenames (can discard)
      j = cio_fgetline(str, 120, lun)
      j = cio_fgetline(str, 120, lun)
      j = cio_fgetline(str, 120, lun)

!---- number of sources
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nsrc) 

!---- number of receivers, nrecx, nrecy
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nrec) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nrecx) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nrecy) 

!---- nyq
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nyq) 

!---- nspec
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nspec_disk) 

!---- ntraces
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%ntraces_disk) 

!---- gmin and gmax (gmax for information only)
      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%gmin) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%gmax) 

!---- rminx, rmaxx, rxinc
      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%rminx) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%rmaxx) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%rxinc) 

!---- rminy, rmaxy, ryinc
      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%rminy) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%rmaxy) 

      j = cio_fgetline(str, 120, lun)
      call string_cc2ff(str(36:120),obj%ryinc) 

!---- naclh and nfft
      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%naclh)

      j = cio_fgetline(str, 120, lun)
      call string_cc2ii(str(36:120),obj%nfft)

!---- close it
      ier = cio_fclose(obj%parm_file_lun)

!      write(*,*) 'nsrc   = ', obj%nsrc
!      write(*,*) 'nrec   = ', obj%nrec
!      write(*,*) 'nrecx  = ', obj%nrecx
!      write(*,*) 'nrecy  = ', obj%nrecy
!      write(*,*) 'noff   = ', obj%off_tot
!      write(*,*) 'len_op = ', obj%len_op
!      write(*,*) 'nyq    = ', obj%nyq
!      write(*,*) 'nspec  = ', obj%nspec_disk
!      write(*,*) 'gmin   = ', obj%gmin
!      write(*,*) 'gmax   = ', obj%gmax
!      write(*,*) 'rminx  = ', obj%rminx
!      write(*,*) 'rmaxx  = ', obj%rmaxx
!      write(*,*) 'rxinc  = ', obj%rxinc
!      write(*,*) 'rminy  = ', obj%rminy
!      write(*,*) 'rmaxy  = ', obj%rmaxy
!      write(*,*) 'ryinc  = ', obj%ryinc
!      write(*,*) 'naclh  = ', obj%naclh
!      write(*,*) 'nfft   = ', obj%nfft
!      pause

      end subroutine scdecon_read_parameter_file


!-------------------------------------------------------------------------------
! Convenience function looking for empty array.
!-------------------------------------------------------------------------------
      logical function scdecon_all_zeroes(array,n)
      implicit none
      real, intent(in)     :: array(:)
      integer, intent(in)  :: n
      integer              :: i

      scdecon_all_zeroes=.true.

      do i=1,n
        if(array(i) /= 0) then
          scdecon_all_zeroes = .false.
          return
        end if        
      end do

      return
      end function scdecon_all_zeroes


!-------------------------------------------------------------------------------
! Convenience function.
!-------------------------------------------------------------------------------
      integer function scdecon_istatr (obj, xgp, ygp)
      implicit none
      type(scdecon_struct),intent(in)   :: obj
      real, intent(in)                  :: xgp,ygp

      scdecon_istatr = mth_bin_number(obj%rminx,obj%rxinc,xgp) + &
               nint((nint(ygp) - obj%rminy)/obj%ryinc) * obj%nrecx
!rev16      scdecon_istatr = nint((nint(xgp) - obj%rminx)/obj%rxinc) + 1 + &
!rev16               nint((nint(ygp) - obj%rminy)/obj%ryinc) * obj%nrecx

      end function scdecon_istatr

!-------------------------------------------------------------------------------
! Subroutine for parameter structure dump.
!-------------------------------------------------------------------------------
      subroutine scdecon_parameter_dump(obj)
      implicit none
      type(scdecon_struct)       :: obj
      
      call pc_print('********** SCDECON Parameter List *************')
      call pc_print('mode=',obj%mode)

      call pc_print(' ')
      call pc_print('opt_ampl   =' ,obj%opt_ampl)
      call pc_print('type_decon =' ,obj%type_decon)
      call pc_print('opt_sc     =' ,obj%opt_sc)
      call pc_print('rec_coor   =' ,obj%rec_coor)

      call pc_print(' ')
      call pc_print('iterations =' ,obj%iterations)
      call pc_print('len_op     =' ,obj%len_op)
      call pc_print('diag_load  =' ,obj%diag_load)
      call pc_print('max_traces =' ,obj%max_traces)

      call pc_print(' ')
      call pc_print('off_init =' ,obj%off_init)
      call pc_print('off_inc  =' ,obj%off_inc)
      call pc_print('off_tot  =' ,obj%off_tot)

      call pc_print(' ')
      call pc_print('param_pathname =', obj%param_pathname)
      call pc_print('spect_pathname =', obj%spect_pathname)
      call pc_print('trace_pathname =', obj%trace_pathname)
      call pc_print('filt_pathname  =', obj%filt_pathname)

      call pc_print(' ')
      call pc_print('obj%nfft       =', obj%nfft)
      call pc_print('obj%fftrc%size =', obj%fftrc%size)
      call pc_print('obj%fftcr%size =', obj%fftcr%size)

      call pc_print('obj%nspec_disk   =', obj%nspec_disk)
      call pc_print('obj%ntraces_disk =', obj%ntraces_disk)

      call pc_print('************ End Parameter List ***************')

      end subroutine scdecon_parameter_dump

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine scdecon_wrapup (obj)
      implicit none
      type(scdecon_struct),intent(inout) :: obj       ! arguments

      integer   :: ier


      if (obj%skip_wrapup) then
        call pc_print('SCDECON: Already wrapped up.')
        return
      end if
      obj%skip_wrapup = .true.


      call pc_print(' ')
      call pc_print( &
       '*********************************************************************')
      call pc_print('SCDECON WRAPUP:')
      call pc_print(' MODE       = ',obj%mode)
      call pc_print(' OPT_AMPL   = ',obj%opt_ampl)

      call pc_print(' ')
      call pc_print(  'Note:  Retaining parameter file = ')
      call pc_print(  '         ',obj%param_pathname)

!---- remove work files
      if(obj%mode == 'BOTH') then
        call pc_print(' ')
        call pc_print('Note:  Removing work file       = ')
        call pc_print('         ',obj%spect_pathname)
        ier = cio_remove(obj%spect_pathname)

        call pc_print(' ')
        call pc_print('Note:  Removing work file       = ')
        call pc_print('         ',obj%trace_pathname)
        ier = cio_remove(obj%trace_pathname)
      end if

!---- information for the user
      if(obj%mode /= 'APPLY') then
        call pc_print(' ')
        call pc_print('# of spectra put to disk        = ', obj%nspec_disk)
        call pc_print('# of traces put to disk         = ', obj%ntraces_disk)
        call pc_print('# bypass1 due to dead trace     = ', obj%nbypass1)
        call pc_print('# bypass2 due to nlive          = ', obj%nbypass2)
        call pc_print('# bypass3 due to sqrt(negative) = ', obj%nbypass3)
        call pc_print('# bypass4 due to zero denom     = ', obj%nbypass4)
        call pc_print('# bypass5 due to sqrt(negative) = ', obj%nbypass5)
        call pc_print('# bypass6 due to sqrt(negative) = ', obj%nbypass6)
        call pc_print('# bypass7 due to NaN values     = ', obj%nbypass7)
      end if

!---- efficiency monitor
      if(obj%mode /= 'PREP') then
        call pc_print(' ')
        call pc_print(' TYPE_DECON = ',obj%type_decon)
        call pc_print(' OPT_SC     = ',obj%opt_sc)
        call pc_print('# of empty scdecon "src bins"  = ', obj%allzero1)
        call pc_print('# of valid scdecon "src bins"  = ', obj%notallzero1)
        call pc_print('# of empty scdecon "rec bins"  = ', obj%allzero2)
        call pc_print('# of valid scdecon "rec bins"  = ', obj%notallzero2)
        call pc_print('# of empty scdecon "off bins"  = ', obj%allzero3)
        call pc_print('# of valid scdecon "off bins"  = ', obj%notallzero3)
      end if

      call pc_print( &
       '*********************************************************************')
      call pc_print(' ')

      return
      end subroutine scdecon_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module scdecon_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

