!<CPS_v1 type="PROCESS"/>
!!------------------------------- dsig.f90 ---------------------------------!!
!!------------------------------- dsig.f90 ---------------------------------!!
!!------------------------------- dsig.f90 ---------------------------------!!

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
! Name       : DSIG
! Category   : filters
! Written    : 1987-08-08   by: Bob Baumel
! Revised    : 2007-01-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Deterministically remove (or apply) source signature.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! DSIG takes a known signature, calculates its inverse, and applies that inverse
! operator to input traces.  DSIG operates in the frequency domain; i.e., all
! input traces are Fourier transformed, then multiplied by an operator in the
! frequency domain (equivalent to convolution in the time domain), then
! transformed back to the time domain. Options are available for specifying
! signature location, how to handle the signature traces (if any), whether to
! apply the inverse of the signature or the signature itself as a frequency-
! domain operator, whether to apply both the amplitude and phase spectra of the
! operator or just its phase spectrum, and whether to include a bandpass filter
! (which can be done at no extra cost because all traces are transformed to the
! frequency domain in any case).
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Signature Locations:
!
! When OPT_LOC=EVERY, signatures are assumed to be embedded in each group of
! input traces (usually shot profiles), and each group is designatured
! separately.  Traces must be GATHERED in this case.
!
! When OPT_LOC=FIRST, signatures are assumed to be located only in the FIRST
! input trace gather, and these are used for designaturing the whole data set.
! Traces need not be gathered if there is only one signature, located in the
! very first trace.
!
! For OPT_LOC=FIRST or OPT_LOC=EVERY, if signatures appear in more than one
! trace of the gather (i.e., if  TR_SIG_END > TR_SIG_BEG) they are combined
! into a single stacked signature before computing the inverse operator.
!
! When OPT_LOC=FILE, the signature is read from a file. Any file format which
! is readable by the TRIN process may also be used in DSIG. Only one trace
! is read from the file.
!
! Signature Centering:
!
! The location of zero-time for a signature determines whether a bulk shift
! occurs when that signature or inverse signature operator is applied to traces.
! Typically, the bulk shift can be minimized if the zero-time for the signature
! is associated with a central high amplitude sample of the signature.
!
! The TIM_FIRST parameter allows the signature to be centered so that DSIG
! will not cause a bulk shift. Normally TIM_FIRST will be approximately
! -0.5 * length of signature. When OPT_LOC=FIRST or OPT_LOC=EVERY, it's up to
! you to specify an appropriate TIM_FIRST. When OPT_LOC=FILE, the front end
! will initially set TIM_FIRST to match the TMIN value in the file (if the
! file exists when you build the job), but you may override that value.
!
! Remove or Apply Signature?
!
! The value of INVERT determines whether DSIG removes the signature from or
! applies the signature to the input traces. For normal designature operation,
! set INVERT=YES to remove signatures. Setting INVERT=NO allows you to apply
! any convolutional operator (such as a transfer function derived by TFUN or a
! wavelet that you wish to attach to synthetics) and, because DSIG operates in
! the frequency domain, this may be faster than a time-domain convolution in
! cases when the time-domain function would be a LONG convolutional operator.
!
! Phase-Only Operation:
!
! If PHASE_ONLY=YES, then only the PHASE spectra of the input traces are
! affected. This may produce more stable results (assuming INVERT=YES) when
! there are holes in the signature amplitude spectrum. Also, the combination
! of PHASE_ONLY=YES with INVERT=NO provides the option to apply only the phase
! spectrum of ANY convolutional operator, such as a transfer function from TFUN.
!
! Bandpass Filter Cost:
!
! Because DSIG works in the frequency domain, it can apply a bandpass filter
! at no extra cost.  A bandpass filter is recommended when using INVERT=YES
! with PHASE_ONLY=NO because, in this case, the designature operation tends to
! amplify noise outside the bandwidth of the signature.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                         TRACE INPUT REQUIREMENTS
!
! Process is a single-trace or multiple-trace process.
!
! Gathering requirements depend on value of input parameters:
! For OPT_LOC = FILE, input traces may be gathered or ungathered.
! For OPT_LOC = EVERY, input traces MUST be gathered (usually in shot gathers).
! For OPT_LOC = FIRST, input traces must be gathered if TR_SIG_END > 1, but
!                      may be either gathered or ungathered if
!                      TR_SIG_BEG = TR_SIG_END = 1 (meaning that the signature
!                      is just the very first input trace).
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered) except
! for OPT_OUT = DELETE which deletes the signature traces.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NUMTR    max number of traces input/output     used but not changed
! GATHERED whether traces properly gathered      used but not changed
! NWIH     number of words in trace header       used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                     TRACE HEADER WORDS USED OR CHANGED
! Hwd#   Description                 Action taken
! ----   -----------                 ------------
!   1    Sequential Trace Count      Renumbered if OPT_OUT = DELETE.
!   4    Trace number within group   Renumbered if OPT_OUT = DELETE.
!  25    Largest absolute value      Reset.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 29. 2007-01-03  Stoeckley    Do not reset pathname to NONE when DT does not
!                               match the value on the file; always check for
!                               incompatible DT.
! 28. 2006-06-12  B. Menger    Removed Unused Variables.
! 27. 2002-08-12  Baumel       Add file selection box in GUI; additional bug
!                              fix for flag trace option.
! 26. 2002-08-01  Goodger      Fix bug with flagged traces being passed
!                              incorrectly.
! 25. 2002-06-12  Goodger      Pass out all traces rather than flagged traces
!                              only.
! 24. 2002-06-06  Goodger      Add HDR_FLAG parameter.
! 23. 2001-02-15  Baumel       In CFE, don't reset TIM_FIRST from signature
!                              file except in GUI updates.
! 22. 2000-12-07  Baumel       Change wrapped_up flag to skip_wrapup.
! 21. 2000-04-10  Baumel       Add gui_def to doc.
! 20. 2000-02-17  Baumel       Make sure front-end message doesn't show up
!                              inappropriately in a back-end update.
! 19. 2000-02-14  Baumel       Restore ability of front end to set TIM_FIRST
!                              from signature file, but don't require file to
!                              be present & warn user when TIM_FIRST is reset.
! 18. 2000-02-01  Baumel       For now, remove front end attempt to set
!                              TIM_FIRST from signature file when OPT_LOC=FILE.
! 17. 2000-01-28  Baumel       Add call to LAV primitive; some other cleanup.
! 16. 2000-01-17  Baumel       Make compatible with new PATHCHECK primitive.
! 15. 2000-01-11  Baumel       Move pc_put_options_field calls ahead of pc_put
!                              calls for proper front end operation.
! 14. 2000-01-07  Baumel       Change parameter name FILENAME to PATHNAME;
!                              add call to PATHCHECK primitive.
! 13. 1999-12-21  Baumel       Add OPT_LOC = FILE option.
! 12. 1999-11-30  Baumel       New Convention for specifying bandpass filters.
! 11. 1999-10-15  Bob Baumel   Converted from old system (postponing the
!                              OPT_LOC=FILE option, for now).
! 10. 1998-12-15  Vunderink    Begin using the f90 compiler.
!  9. 1995-05-24  Baumel       Allow PHASONLY=YES with INVERT=NO (so can
!                              apply a transfer function in PHASONLY mode).
!  8. 1991-10-02  Peterson     Add COMMENT parameter.
!  7. 1991-02-03  Baumel       Add LOCSIG,OUTSIG,TFIRST,FILE,INVERT options.
!  6. 1990-03-28  Baumel       Add PHASONLY option.
!  5. 1988-10-21  Ball         NWIH and NWPT Conversion
!  4. 1988-08-17  Baumel       Match change in TVFBPS primitive.
!  3. 1988-06-10  Baumel       New convention for mute header word.
!  2. 1988-04-22  Baumel       Add CPSPRT calls.
!  1. 1987-08-08  Bob Baumel   Initial version.
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
!    NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!    NTR == NEED_TRACES    if this process needs more traces (may happen
!                              if OPT_OUT = DELETE).
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
! <NS DSIG Process/NC=80>
!
!         Deterministically remove (or apply) source signature.
!
! OPT_LOC=`CCCC         HDR_FLAG= `IIIIIIIII
!
! Select PATHNAME[PATHNAME]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [PATHNAME_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!  TR_SIG_BEG=`IIIIIIIIII   TR_SIG_END=`IIIIIIIIII
! TIM_SIG_BEG=`FFFFFFFFFF  TIM_SIG_END=`FFFFFFFFFF  TIM_FIRST=`FFFFFFFFFF
!
!     OPT_OUT=`CCCCCC   INVERT=`CC  PHASE_ONLY=`CC  DIAG_LOAD=`FFFFFFFFFF
!
! `-----------------------------------------------------------
!  OPT_BANDPASS=`CCCCCCCCC
! 
!   FREQ_LOW_NONE=`FFFFFFFFFFF    FREQ_LOW_FULL=`FFFFFFFFFFF
!  FREQ_HIGH_FULL=`FFFFFFFFFFF   FREQ_HIGH_NONE=`FFFFFFFFFFF
!           PHASE=`FFFFFFFFFFF
! `-----------------------------------------------------------
!
! COMMENT= `SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! 
!<PARMS PATHNAME[/ML=140/XST]>
!<PARMS PATHNAME_INFO[/ML=140/XST]>
!<PARMS COMMENT[/ML=128/XST]>
!</gui_def>

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_LOC">
!<Tip> Option for location of signature. </Tip>
! Default = FIRST
! Allowed = FIRST (Signatures are only in first gather of input traces.)
! Allowed = EVERY (Signatures are in every gather of input traces.)
! Allowed = FILE  (Signature is in a TROT file.)
! Allowed = NONE  (No signature at all -- just do bandpass filter.)
!
! If OPT_LOC = EVERY, the input traces must arrive in gathers.
! If OPT_LOC = FIRST, input traces must be gathered if TR_SIG_END > 1 but
!    DON'T need to be gathered if TR_SIG_BEG = TR_SIG_END = 1, meaning that
!    you have just one signature, located in your very first input trace.
! If OPT_LOC = FILE, input traces may be gathered or ungathered.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 or 2 - NWIH
!
! If HDR_FLAG = 0, then all traces are filtered.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are filtered.
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME">
!<Tip> Choose PATHNAME using a file selection dialog box. </Tip>
! Active only when OPT_LOC = FILE.
!</Help>
!
!<Help KEYWORD="PATHNAME">
!<Tip> Pathname of trace file containing the signature if OPT_LOC=FILE. </Tip>
! Default = NONE
! Allowed = CHARACTER
! DSIG reads only one trace from this file, and uses it as the signature to
! remove or apply to traces (If your file contains more than one trace, all
! but the first are ignored). Any file format readable by TRIN is also
! readable by DSIG. The DT global in this file must match DT in the current
! job. The TMIN value in the file is used for setting an initial default for
! TIM_FIRST, although you may override that TIM_FIRST value when setting
! parameters for DSIG.
!
! Active only when OPT_LOC = FILE.
!</Help>
!
!<Help KEYWORD="PATHNAME_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME. </Tip>
!</Help>
!
!<Help KEYWORD="TR_SIG_BEG">
!<Tip> Sequential number of first trace in gather recording a signature. </Tip>
! Default = 1
! Allowed = int>0
! This parameter is used only when OPT_LOC=FIRST or OPT_LOC=EVERY.
!</Help>
!
!<Help KEYWORD="TR_SIG_END">
!<Tip> Sequential number of last trace in gather recording a signature. </Tip>
! Default = 1
! Allowed = int>=TR_SIG_BEG
! This parameter is used only when OPT_LOC=FIRST or OPT_LOC=EVERY.
!</Help>
!
!<Help KEYWORD="TIM_SIG_BEG">
!<Tip> Time in trace for start of signature, in seconds. </Tip>
! Default = TSTRT
! Allowed = real>=TSTRT
! This parameter is used only when OPT_LOC=FIRST or OPT_LOC=EVERY.
!</Help>
!
!<Help KEYWORD="TIM_SIG_END">
!<Tip> Time in trace for end of signature, in seconds. </Tip>
! Default = TSTRT + 0.5
! Allowed = real>TIM_SIG_BEG
! This parameter is used only when OPT_LOC=FIRST or OPT_LOC=EVERY.
!</Help>
!
!<Help KEYWORD="TIM_FIRST">
!<Tip> Time of first sample in signature, in seconds. </Tip>
! Default = 0.0
! Allowed = real
! Time of first sample in signature is used for proper centering of the
! signature.  TIM_FIRST can be adjusted so that the operator produces
! no bulk shift.  Normally TIM_FIRST will be approximately
! -0.5 * length of signature.
! When OPT_LOC=FILE, the front end tries to initially set TIM_FIRST to
! match TMIN in the file, but you may override that value.
!</Help>
!
!<Help KEYWORD="OPT_OUT">
!<Tip> Option for output of signature traces. </Tip>
! Default = ZERO
! Allowed = ZERO     (Zero signature part of trace and apply operator to rest.)
! Allowed = NOTHING  (Apply operator to whole trace, zeroing nothing.)
! Allowed = PASS     (Pass signature traces out unchanged.)
! Allowed = DELETE   (Delete signature traces.)
! This parameter is relevant only when OPT_LOC=EVERY or OPT_LOC=FIRST.
!</Help>
!
!<Help KEYWORD="INVERT">
!<Tip> Option whether to invert signature before applying to traces.</Tip>
! Default = YES
! Allowed = YES (Apply inverted signature to traces).
! Allowed = NO  (Apply signature itself to traces).
!</Help>
!
!<Help KEYWORD="PHASE_ONLY">
!<Tip> Option whether to apply only phase spectrum of the operator. </Tip>
! Default = YES
! Allowed = YES  (Apply operator only to phase spectrum of traces.)
! Allowed = NO   (Apply operator to phase and amplitude spectra of traces.)
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent, for signature inverse calculation. </Tip>
! Default = 2.0
! Allowed = real>=0.0
! Diagonal load is only needed if INVERT=YES and PHASE_ONLY=NO.
!</Help>
!
!<Help KEYWORD="OPT_BANDPASS">
!<Tip> Option whether to bandpass filter the traces. </Tip>
! Default = NONE
! Allowed = NONE       (No bandpass filter)
! Allowed = BANDPASS   (Pass frequencies between tapers)
! Allowed = HIGHPASS   (Pass frequencies from low taper to Nyquist)
! Allowed = LOWPASS    (Pass frequencies from 0.0 to high taper)
! Allowed = ALLPASS    (Pass all frequencies [may still change phase])
! Allowed = BANDREJECT (Reject frequencies between tapers)
! Because DSIG operates in the frequency domain, it can perform a bandpass
! filter at no additional cost.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Frequency (in Hz) where low frequency taper passes nothing. </Tip>
! Default = -
! Allowed = real>=0.0
! This parameter is only used when OPT_BANDPASS has a value of BANDPASS,
! HIGHPASS, or BANDREJECT.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Frequency (in Hz) where low freq taper passes full amplitude. </Tip>
! Default = -
! Allowed = real>=FREQ_LOW_NONE
! This parameter is only used when OPT_BANDPASS has a value of BANDPASS,
! HIGHPASS, or BANDREJECT.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> Frequency (in Hz) where high freq taper passes full amplitude. </Tip>
! Default = -
! Allowed = real>=FREQ_LOW_FULL
! This parameter is only used when OPT_BANDPASS has a value of BANDPASS,
! LOWPASS, or BANDREJECT.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> Frequency (in Hz) where high frequency taper passes nothing. </Tip>
! Default = -
! Allowed = Nyquist >= real >= FREQ_HIGH_FULL
! This parameter is only used when OPT_BANDPASS has a value of BANDPASS,
! LOWPASS, or BANDREJECT.
!</Help>
!
!<Help KEYWORD="PHASE">
!<Tip> Phase of filter, in degrees. </Tip>
! Default = 0.0
! Allowed = real
! This parameter is only used when OPT_BANDPASS has a value of BANDPASS,
! HIGHPASS, LOWPASS, or ALLPASS.
!</Help>
!
!<Help KEYWORD="COMMENT">
!<Tip> One line comment for history file. </Tip>
! Default = blank
! Allowed = char
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module dsig_module
      use pc_module
      use named_constants_module
      use mem_module
      use fft_module
      use bandps_module
      use string_module
      use pathcheck_module
      use pathchoose_module
      use trcio_module
      use lav_module
      use ameq_module

      implicit none
      private
      public :: dsig_create
      public :: dsig_initialize
      public :: dsig_update
      public :: dsig_delete
      public :: dsig            ! main execution (trace processing) routine.
      public :: dsig_wrapup

      character(len=100),public,save :: DSIG_IDENT = &
'$Id: dsig.f90,v 1.29 2007/01/03 14:01:38 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type, public :: dsig_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.
        logical                    :: gathered         ! gathered flag
        character(len=8)           :: opt_loc          ! process parameters.
        integer                    :: hdr_flag         ! process parameters.
        integer                    :: tr_sig_beg       ! process parameters.
        integer                    :: tr_sig_end       ! process parameters.
        real                       :: tim_sig_beg      ! process parameters.
        real                       :: tim_sig_end      ! process parameters.
        real                       :: tim_first        ! process parameters.
        character(len=8)           :: opt_out          ! process parameters.
        character(len=FILENAME_LENGTH) :: pathname     ! process parameters.
        logical                    :: invert           ! process parameters.
        logical                    :: phase_only       ! process parameters.
        real                       :: diag_load        ! process parameters.
        character(len=10)          :: opt_bandpass     ! process parameters.
        real                       :: freq_low_none    ! process parameters.
        real                       :: freq_low_full    ! process parameters.
        real                       :: freq_high_full   ! process parameters.
        real                       :: freq_high_none   ! process parameters.
        real                       :: phase            ! process parameters.
        character(len=128)         :: comment          ! process parameters.
        integer                    :: nwih,ndpt        ! globals.
        real                       :: dt,tstrt         ! globals.
        integer                    :: isgs,isgsm1,lsig ! dependent variables.
        integer                    :: npow2,nnyq       ! dependent variables.
        real                       :: dld,df           ! dependent variables.
        logical                    :: set_filter       ! dependent variables.
        logical                    :: first_time       ! dependent variables.
        integer                    :: numtrc_tot       ! dependent variables.
        integer                    :: numtrc_flag      ! dependent variables.
        type(fft_struct)  ,pointer :: fftrc_obj        ! dependent variables.
        type(fft_struct)  ,pointer :: fftcr_obj        ! dependent variables.
        real              ,pointer :: dumtr(:)         ! dependent variables.
        complex           ,pointer :: comtr(:)         ! dependent variables.
        complex           ,pointer :: filter(:)        ! dependent variables.
        complex           ,pointer :: bandfilter(:)    ! dependent variables.
        double precision  ,pointer :: hdtmp(:)         ! dependent variables.
        type(trcio_struct),pointer :: trcfile          ! dependent variables.
        type(pathchoose_struct),pointer :: pathchoose  ! dependent variables.

      end type dsig_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(dsig_struct),pointer,save :: object      ! needed for traps.

      integer    ,parameter :: opt_loc_noptions = 4
      character(len=8)      :: opt_loc_options(opt_loc_noptions)        &
                = (/ 'FILE    ', 'EVERY   ', 'FIRST   ', 'NONE    ' /)

      integer    ,parameter :: opt_out_noptions = 4
      character(len=8)      :: opt_out_options(opt_out_noptions)        &
              = (/ 'ZERO    ', 'NOTHING ', 'PASS    ' , 'DELETE  ' /)

      integer    ,parameter :: opt_bandpass_noptions = 6
      character(len=10)     :: opt_bandpass_options(opt_bandpass_noptions) &
           = (/ 'NONE      ', 'BANDPASS  ', 'HIGHPASS  ' ,'LOWPASS   ',    &
                'ALLPASS   ', 'BANDREJECT' /)

      CONTAINS

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine dsig_create (obj)
      implicit none
      type(dsig_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%fftrc_obj)
      nullify  (obj%fftcr_obj)
      nullify  (obj%dumtr)
      nullify  (obj%comtr)
      nullify  (obj%filter)
      nullify  (obj%bandfilter)
      nullify  (obj%hdtmp)
      nullify  (obj%trcfile)
      nullify  (obj%pathchoose)

      call pathchoose_create (obj%pathchoose, 'PATHNAME', 'trc')

      call dsig_initialize (obj)
      end subroutine dsig_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine dsig_delete (obj)
      implicit none
      type(dsig_struct),pointer :: obj       ! arguments
      integer                   :: ier1      ! local

      call dsig_wrapup (obj)

      if (associated(obj%fftrc_obj)) call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj)) call fft_delete (obj%fftcr_obj)
      call mem_free (obj%dumtr)
      call mem_free (obj%comtr)
      call mem_free (obj%filter)
      call mem_free (obj%bandfilter)
      call mem_free (obj%hdtmp)
      if (associated(obj%trcfile)) ier1 = trcio_close (obj%trcfile)
      call pathchoose_delete (obj%pathchoose)

      deallocate(obj)
      end subroutine dsig_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine dsig_initialize (obj)
      implicit none
      type(dsig_struct),intent(inout) :: obj       ! arguments

      obj%opt_loc        = 'FIRST'
      obj%hdr_flag       = 0
      obj%tr_sig_beg     = 1
      obj%tr_sig_end     = 1
      call pc_get_global ('TSTRT' , obj%tim_sig_beg)
      obj%tim_sig_end    = obj%tim_sig_beg + 0.5
      obj%tim_first      = 0.0
      obj%opt_out        = 'ZERO'
      obj%pathname       = PATHCHECK_EMPTY
      obj%invert         = .true.
      obj%phase_only     = .true.
      obj%diag_load      = 2.0
      obj%opt_bandpass   = 'NONE'
      obj%freq_low_none  = fnil
      obj%freq_low_full  = fnil
      obj%freq_high_full = fnil
      obj%freq_high_none = fnil
      obj%phase          = 0.0
      obj%comment        = cnil

      call dsig_update (obj)
      end subroutine dsig_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine dsig_update (obj)
      implicit none
      type(dsig_struct),intent(inout),target :: obj             ! arguments


      logical   :: freq_low_none_sens, freq_low_full_sens       ! local
      logical   :: freq_high_full_sens, freq_high_none_sens     ! local
      logical   :: phase_sens                                   ! local
      integer   :: numtr       ,nscratch,nstore      ,ier1,i ! local
      integer   :: result, status, update_state                 ! local
      integer   :: tim_sig_beg_samp, tim_sig_end_samp           ! local
      real      :: fact,dt_prev                                 ! local
      logical   :: invert_prev, phase_only_prev                 ! local
      character(len=80) :: message                              ! local
      character(len=FILENAME_LENGTH) :: pathname_prev           ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      invert_prev     = obj%invert
      phase_only_prev = obj%phase_only
      pathname_prev   = obj%pathname
      dt_prev         = obj%dt

      if (pathchoose_update(obj%pathchoose, obj%pathname)) return

      update_state = pc_get_update_state()

      call pc_get_global ('NUMTR'    , numtr)
      call pc_get_global ('GATHERED' , obj%gathered)
      call pc_get_global ('NWIH'     , obj%nwih)
      call pc_get_global ('NDPT'     , obj%ndpt)
      call pc_get_global ('TSTRT'    , obj%tstrt)
      call pc_get_global ('DT'       , obj%dt)

      call pc_get ('OPT_LOC'        , obj%opt_loc)
      call pc_get ('HDR_FLAG'       , obj%hdr_flag   , dsig_hdr_flag_trap)
      call pc_get ('PATHNAME'       , obj%pathname)
      call pc_get ('TR_SIG_BEG'     , obj%tr_sig_beg)
      call pc_get ('TR_SIG_END'     , obj%tr_sig_end)
      call pc_get ('TIM_SIG_BEG'    , obj%tim_sig_beg)
      call pc_get ('TIM_SIG_END'    , obj%tim_sig_end)
      call pc_get ('TIM_FIRST'      , obj%tim_first)
      call pc_get ('OPT_OUT'        , obj%opt_out)
      call pc_get ('INVERT'         , obj%invert)
      call pc_get ('PHASE_ONLY'     , obj%phase_only)
      call pc_get ('DIAG_LOAD'      , obj%diag_load)
      call pc_get ('OPT_BANDPASS'   , obj%opt_bandpass)
      call pc_get ('FREQ_LOW_NONE'  , obj%freq_low_none)
      call pc_get ('FREQ_LOW_FULL'  , obj%freq_low_full)
      call pc_get ('FREQ_HIGH_FULL' , obj%freq_high_full)
      call pc_get ('FREQ_HIGH_NONE' , obj%freq_high_none)
      call pc_get ('PHASE'          , obj%phase)
      call pc_get ('COMMENT'        , obj%comment)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      obj%tr_sig_beg = max(obj%tr_sig_beg, 1)
      obj%tr_sig_end = max(obj%tr_sig_end, obj%tr_sig_beg)
      if (numtr > 1) then
        obj%tr_sig_beg = min(obj%tr_sig_beg, numtr)
        obj%tr_sig_end = min(obj%tr_sig_end, numtr)
      end if

      call string_to_upper (obj%opt_loc)
      if (obj%opt_loc(1:3) == 'FIL') then
        obj%opt_loc = 'FILE'
      else if (obj%opt_loc(1:3) == 'FIR') then
        obj%opt_loc = 'FIRST'
        if (obj%tr_sig_end > 1  .and.  &
              (.not.obj%gathered .or. numtr < 2)) then
          call pc_error ('Traces must be gathered when OPT_LOC = FIRST and&
                        & TR_SIG_END > 1.')
        end if
      else if (obj%opt_loc(1:2) == 'EV') then
        obj%opt_loc = 'EVERY'
        if (.not.obj%gathered .or. numtr < 2) then
          call pc_error ('Traces must be gathered when OPT_LOC = EVERY.')
        end if
      else if (obj%opt_loc(1:1) == 'N') then
        obj%opt_loc = 'NONE'      ! Option to just filter
      else
        call pc_error ('OPT_LOC must be FILE, FIRST, EVERY or NONE.')
      end if

      if (obj%opt_loc /= 'FILE') then
        call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .false.)
        call pc_put_sensitive_field_flag ('PATHNAME'       , .false.)
        if (update_state /= PC_GUI) obj%pathname = PATHCHECK_EMPTY
        pathname_prev = PATHCHECK_EMPTY
        call pathcheck ('PATHNAME', pathname_prev, status=status, &
                         show=PATHCHECK_INFO_INPUT)
      else
        call pc_put_sensitive_field_flag ('SELECT_PATHNAME', .true.)
        call pc_put_sensitive_field_flag ('PATHNAME'       , .true.)
        call pathcheck ('PATHNAME', obj%pathname, 'trc', status=status, &
                         show=PATHCHECK_INFO_INPUT)
        if (status /= PATHCHECK_VALID) then
          if (update_state /= pc_gui) then
            call pc_error ('PATHNAME must be set when OPT_LOC = FILE.')
          end if
          goto 100
        end if
        if (obj%pathname == pathname_prev) goto 100
        if (update_state == PC_BACKEND) goto 100
        obj%trcfile => trcio_open (obj%pathname, 'r')
        if (.not.associated(obj%trcfile)) then
          call pc_warning("Your signature file doesn't seem to exist yet.&
             & Be sure its name is right and TIM_FIRST is set correctly.")
          goto 100
        end if
        if (ameq(obj%trcfile%dt, obj%dt, 0.0001*obj%dt)) then
          if (update_state == PC_GUI) then
            obj%tim_first = obj%trcfile%tmin
            call pc_info('TIM_FIRST set to signature file TMIN = ',  &
                          obj%trcfile%tmin,                       &
                         ' but you may override this value if necessary.')
          end if
  !     else
  !       call pc_error('DT in signature file = ', obj%trcfile%dt, &
  !         ' differs from current DT global = ', obj%dt)
  !       obj%pathname = PATHCHECK_EMPTY
  !       call pathcheck ('PATHNAME', obj%pathname, status=status, &
  !                        show=PATHCHECK_INFO_INPUT)
        end if
        status = trcio_close (obj%trcfile)
      end if
 100  continue

      if (obj%opt_loc == 'FILE') then
        if (obj%pathname /= pathname_prev .or. obj%dt /= dt_prev .or. &
                        update_state /= PC_GUI) then
          obj%trcfile => trcio_open (obj%pathname, 'r')
          if (associated(obj%trcfile)) then
            if (.not.ameq(obj%trcfile%dt, obj%dt, 0.0001*obj%dt)) then
              call pc_error('DT in signature file = ', obj%trcfile%dt, &
                ' differs from current DT global = ', obj%dt)
            end if
            status = trcio_close (obj%trcfile)
          end if
        end if
      end if

      tim_sig_beg_samp = nint((obj%tim_sig_beg - obj%tstrt)/obj%dt) + 1
      tim_sig_beg_samp = min (max(tim_sig_beg_samp, 1), obj%ndpt)
      obj%tim_sig_beg  = obj%tstrt  +  obj%dt * (tim_sig_beg_samp - 1)
      tim_sig_end_samp = nint((obj%tim_sig_end - obj%tstrt)/obj%dt) + 1
      tim_sig_end_samp = min (max(tim_sig_end_samp, tim_sig_beg_samp), &
                              obj%ndpt)
      obj%tim_sig_end  = obj%tstrt  +  obj%dt * (tim_sig_end_samp - 1)
      if (obj%opt_loc=='FIRST' .or. obj%opt_loc=='EVERY') then
        call pc_put_sensitive_field_flag ('TR_SIG_BEG' , .true.)
        call pc_put_sensitive_field_flag ('TR_SIG_END' , .true.)
        call pc_put_sensitive_field_flag ('TIM_SIG_BEG', .true.)
        call pc_put_sensitive_field_flag ('TIM_SIG_END', .true.)
        call pc_put_sensitive_field_flag ('OPT_OUT'    , .true.)
        call string_to_upper (obj%opt_out)
        if (obj%opt_out(1:1) == 'Z') then
          obj%opt_out = 'ZERO'
        else if (obj%opt_out(1:1) == 'N') then
          obj%opt_out = 'NOTHING'
        else if (obj%opt_out(1:1) == 'P') then
          obj%opt_out = 'PASS'
        else if (obj%opt_out(1:1) == 'D') then
          obj%opt_out = 'DELETE'
        else
          call pc_error ('OPT_OUT must be ZERO, NOTHING, PASS, or DELETE.')
        end if
      else
        call pc_put_sensitive_field_flag ('TR_SIG_BEG' , .false.)
        call pc_put_sensitive_field_flag ('TR_SIG_END' , .false.)
        call pc_put_sensitive_field_flag ('TIM_SIG_BEG', .false.)
        call pc_put_sensitive_field_flag ('TIM_SIG_END', .false.)
        call pc_put_sensitive_field_flag ('OPT_OUT'    , .false.)
        obj%opt_out = 'NOTHING'
      end if

      obj%diag_load = abs (obj%diag_load)

      if (obj%opt_loc /= 'NONE') then
        call pc_put_sensitive_field_flag ('TIM_FIRST' , .true.)
        call pc_put_sensitive_field_flag ('INVERT'    , .true.)
        call pc_put_sensitive_field_flag ('PHASE_ONLY', .true.)
        if (obj%invert .and. .not.obj%phase_only) then
          call pc_put_sensitive_field_flag ('DIAG_LOAD', .true.)
          if (obj%diag_load == 0. .and. update_state == PC_GUI .and. &
             (.not.invert_prev .or. phase_only_prev))  obj%diag_load = 2.0
        else
          call pc_put_sensitive_field_flag ('DIAG_LOAD', .false.)
          if (update_state /= PC_GUI) obj%diag_load = 0.
        end if
      else
        call pc_put_sensitive_field_flag ('TIM_FIRST' , .false.)
        call pc_put_sensitive_field_flag ('INVERT'    , .false.)
        call pc_put_sensitive_field_flag ('PHASE_ONLY', .false.)
        call pc_put_sensitive_field_flag ('DIAG_LOAD' , .false.)
      end if

      call bandps_check (result, message, 0.5/obj%dt, obj%opt_bandpass,  &
             obj%freq_low_none, obj%freq_low_full, obj%freq_high_full,   &
             obj%freq_high_none, obj%phase)
      select case (result)
      case (bandps_info)
        call pc_info(message)
      case (bandps_error)
        call pc_error(message)
      case (bandps_enderror)
        if (update_state /= pc_gui) call pc_error(message)
      end select

      call bandps_sensitive (obj%opt_bandpass, freq_low_none_sens,   &
                         freq_low_full_sens, freq_high_full_sens,    &
                         freq_high_none_sens, phase_sens )
      call pc_put_sensitive_field_flag ('FREQ_LOW_NONE' ,freq_low_none_sens)
      call pc_put_sensitive_field_flag ('FREQ_LOW_FULL' ,freq_low_full_sens)
      call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL',freq_high_full_sens)
      call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE',freq_high_none_sens)
      call pc_put_sensitive_field_flag ('PHASE'         ,phase_sens)

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('OPT_LOC', opt_loc_options, opt_loc_noptions)
      call pc_put_options_field ('OPT_OUT', opt_out_options, opt_out_noptions)
      call pc_put_options_field ('OPT_BANDPASS', opt_bandpass_options, &
                                                 opt_bandpass_noptions)
      call pc_put ('OPT_LOC'        , obj%opt_loc)
      call pc_put ('HDR_FLAG'       , obj%hdr_flag)
      call pc_put ('PATHNAME'       , obj%pathname)
      call pc_put ('TR_SIG_BEG'     , obj%tr_sig_beg)
      call pc_put ('TR_SIG_END'     , obj%tr_sig_end)
      call pc_put ('TIM_SIG_BEG'    , obj%tim_sig_beg)
      call pc_put ('TIM_SIG_END'    , obj%tim_sig_end)
      call pc_put ('TIM_FIRST'      , obj%tim_first)
      call pc_put ('OPT_OUT'        , obj%opt_out)
      call pc_put ('INVERT'         , obj%invert)
      call pc_put ('PHASE_ONLY'     , obj%phase_only)
      call pc_put ('DIAG_LOAD'      , obj%diag_load)
      call pc_put ('OPT_BANDPASS'   , obj%opt_bandpass)
      call pc_put ('FREQ_LOW_NONE'  , obj%freq_low_none)
      call pc_put ('FREQ_LOW_FULL'  , obj%freq_low_full)
      call pc_put ('FREQ_HIGH_FULL' , obj%freq_high_full)
      call pc_put ('FREQ_HIGH_NONE' , obj%freq_high_none)
      call pc_put ('PHASE'          , obj%phase)
      call pc_put ('COMMENT'        , obj%comment)

      obj%npow2 = 8
      do while (obj%npow2 < obj%ndpt)
        obj%npow2 = 2 * obj%npow2
      end do
      obj%df = 1./(obj%npow2*obj%dt)
      obj%nnyq = obj%npow2/2 + 1
      nscratch = 0
      nstore = obj%npow2 + 6*obj%nnyq + fft_mem_usage(obj%npow2, 'rtoc')  &
                                      + fft_mem_usage(obj%npow2, 'ctor')
      call pc_put_control ('nscratch'    , nscratch)
      call pc_put_control ('nstore'      , nstore)
      call pc_put_control ('need_request', obj%opt_out=='DELETE')

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      if (associated(obj%fftrc_obj)) call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj)) call fft_delete (obj%fftcr_obj)
      call mem_free (obj%dumtr)
      call mem_free (obj%comtr)
      call mem_free (obj%filter)
      call mem_free (obj%bandfilter)
      call mem_free (obj%hdtmp)
      if (associated(obj%trcfile)) then
        ier1 = trcio_close (obj%trcfile)
        if (ier1 == trcio_error) call pc_error ('Error closing signature file.')
      end if
      obj%numtrc_tot = 0
      obj%numtrc_flag= 0

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.    ! needed for the wrapup routine.

!! memory allocation:

      call mem_alloc (obj%dumtr      , obj%npow2)
      call mem_alloc (obj%comtr      , obj%nnyq)
      call mem_alloc (obj%bandfilter , obj%nnyq)
      call mem_alloc (obj%filter     , obj%nnyq)

      if (obj%opt_loc == 'FILE') call mem_alloc (obj%hdtmp, obj%nwih)

      ier1 =  fft_create (obj%fftrc_obj, -1, obj%npow2, 'rtoc')
      if (ier1/=0) call pc_error ('Error creating rtoc FFT object')

      ier1 =  fft_create (obj%fftcr_obj, +1, obj%npow2, 'ctor')
      if (ier1/=0) call pc_error ('Error creating ctor FFT object')

!! calculate bandpass filter

      call bandps (obj%bandfilter, obj%nnyq, obj%df, obj%opt_bandpass,  &
          obj%freq_low_none, obj%freq_low_full, obj%freq_high_full,     &
          obj%freq_high_none, obj%phase, 1./obj%npow2 )
      if (obj%tim_first /= 0.) then
        fact = 2. * pi * obj%df * obj%tim_first
        if (.not. obj%invert) fact = -fact
        do i = 1, obj%nnyq
          obj%bandfilter(i) = obj%bandfilter(i) &
                            * cmplx(cos((i-1)*fact),sin((i-1)*fact))
        end do
      end if
      obj%bandfilter(1) = cmplx(real(obj%bandfilter(1)),0.)
      obj%bandfilter(obj%nnyq) = cmplx(real(obj%bandfilter(obj%nnyq)),0.)

!! set initial filter

      obj%filter = obj%bandfilter

!! other initializations

      obj%isgs = nint((obj%tim_sig_beg - obj%tstrt)/obj%dt) + 1
      obj%isgsm1 = obj%isgs - 1
      obj%lsig = nint((obj%tim_sig_end - obj%tstrt)/obj%dt) + 1 - obj%isgsm1
      if (obj%diag_load == 0.) then
        obj%dld = 1.e-6
      else
        obj%dld = obj%diag_load
      end if
      obj%dld = obj%dld / 100.
      obj%first_time = .true.
      if (obj%opt_loc /= 'NONE') then
        obj%set_filter = .false.
      else
        obj%set_filter = .true.
        deallocate (obj%bandfilter)
      end if

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      end subroutine dsig_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

      subroutine dsig_hdr_flag_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

      if (object%hdr_flag < 0 .or. object%hdr_flag > object%nwih) then
        call pc_error('HDR_FLAG must be between zero and ',object%nwih)
      else if (object%hdr_flag == 1) then
        call pc_error('HDR_FLAG must not be 1')
      endif

      end subroutine dsig_hdr_flag_trap

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

      subroutine dsig (obj,ntr,hd,tr)
      implicit none
      type(dsig_struct),intent(inout)  :: obj                   ! arguments
      integer          ,intent(inout)  :: ntr                   ! arguments
      double precision ,intent(inout)  :: hd(:,:)               ! arguments
      real             ,intent(inout)  :: tr(:,:)               ! arguments

      integer                    :: numsig,isig,j,l,ncnt,ier1   ! local
      real                       :: fact                        ! local

      if (ntr == NO_MORE_TRACES) goto 27
!
!   Build filter from signature traces
!
      hd(HDR_SEQUENCE,1:ntr) = 1.0   ! Will use hdr word HDR_SEQUENCE (1)
                                     ! to identify signature traces
      if (.not. obj%set_filter) then
        if (obj%opt_loc /= 'FILE') then   ! Extract signatures from traces
          numsig = 0
          obj%dumtr = 0.
          do isig = obj%tr_sig_beg, min(obj%tr_sig_end,ntr)
            hd(HDR_SEQUENCE,isig) = -1.0   ! Flag to identify signature traces
            do j = 1, obj%lsig
              if (tr(obj%isgsm1+j,isig) /= 0.) go to 2
            end do
            cycle
    2       continue
            numsig = numsig + 1
            obj%dumtr(:obj%lsig) = obj%dumtr(:obj%lsig)  &
                       + tr(obj%isgs:obj%isgsm1+obj%lsig, isig)
          end do
        else                              ! Extract signature from file
          obj%trcfile => trcio_open (obj%pathname, 'r')
          if (.not.associated(obj%trcfile)) then
            call pc_print ('DSIG: Unable to open signature file '  &
                            // obj%pathname)
            goto 26
          end if
          if (.not.ameq(obj%trcfile%dt, obj%dt, 0.0001*obj%dt)) then
            call pc_print ('DSIG: DT in signature file = ', obj%trcfile%dt, &
               ' differs from current DT global = ', obj%dt)
            goto 26
          end if
          obj%lsig = min ( obj%npow2, obj%trcfile%num_values )
          ier1 = trcio_read_trace (obj%trcfile, obj%hdtmp, obj%dumtr)
          if (ier1 /= trcio_ok) then
            call pc_print ('DSIG: Error reading signature trace from file')
            goto 26
          end if
          call mem_free (obj%hdtmp)
          ier1 = trcio_close(obj%trcfile)
          numsig = 1
        end if
        if (numsig > 0) then              ! Derive operator from signature
          obj%dumtr(:obj%lsig) = (1.0/numsig) * obj%dumtr(:obj%lsig)
          fact = sum ( obj%dumtr(:obj%lsig)**2 )
          if (fact /= 0.) then
            fact = fact * obj%dld
            call fft_rc_transform(obj%fftrc_obj, obj%dumtr ,obj%comtr  )
            if (obj%invert) then
              obj%filter = obj%bandfilter * conjg(obj%comtr) &
                       / (real(obj%comtr)**2 + aimag(obj%comtr)**2 + fact)
              if (obj%phase_only) then
                obj%filter = abs(obj%comtr) * obj%filter
              endif
            else
              obj%filter = obj%bandfilter * obj%comtr
              if (obj%phase_only) then
                obj%filter = abs(obj%comtr) * obj%filter &
                       / (real(obj%comtr)**2 + aimag(obj%comtr)**2 + fact)
              endif
            endif
            if (obj%opt_loc /= 'EVERY') then
              deallocate (obj%bandfilter)
              obj%set_filter = .true.
            else if (obj%first_time) then
              obj%set_filter = .true.
            end if
          end if
        end if
      end if

      if (obj%first_time) then
        if (.not. obj%set_filter) then
          if (obj%opt_loc == 'FILE') then
            call pc_print('DSIG: Unable to derive operator from &
                           &signature in file.')
          else
            call pc_print('DSIG: No signature found in first trace gather')
          end if
          goto 26
        else
          if (obj%opt_loc == 'EVERY') obj%set_filter = .false.
        end if
        obj%first_time = .false.
      end if
!
!   Apply filter to all traces in the group
!
      ncnt = 0
      do l = 1, ntr
        obj%dumtr(:obj%ndpt) = tr(:obj%ndpt,l)
        if (obj%opt_out /= 'NOTHING') then
          if (hd(HDR_SEQUENCE,l) < 0.0) then
            if (obj%opt_out == 'ZERO') then
              obj%dumtr(obj%isgs:obj%isgsm1+obj%lsig) = 0.0
            else if (obj%opt_out == 'PASS') then
              go to 17
            else ! if (obj%opt_out == 'DELETE') then
              cycle
            endif
          endif
        endif
        if (obj%hdr_flag > 1) then
          if (ameq(hd(obj%hdr_flag,l),0.0,0.00001)) then
            obj%numtrc_flag = obj%numtrc_flag + 1
            go to 17
          endif
        endif
        obj%dumtr(obj%ndpt+1:obj%npow2) = 0.0
        call fft_rc_transform(obj%fftrc_obj, obj%dumtr ,obj%comtr)
        obj%comtr = obj%filter * obj%comtr
        call fft_cr_transform(obj%fftcr_obj, obj%comtr, obj%dumtr)
   17   continue
        ncnt = ncnt + 1
        tr(:obj%ndpt,ncnt) = obj%dumtr(:obj%ndpt)
        if (ncnt /= l) hd(:obj%nwih,ncnt) = hd(:obj%nwih,l)
        hd(HDR_SEQUENCE,ncnt) = obj%numtrc_tot + ncnt
        if (obj%gathered) hd(HDR_CURRENT_CHANNEL,ncnt) = ncnt
      end do
      obj%numtrc_tot = obj%numtrc_tot + ncnt
      if (ncnt == 0) then
        ntr = NEED_TRACES
      else
        ntr = ncnt
        call lav_set_hdr (hd, tr, obj%ndpt, ntr)
      endif
      return

   26 ntr = FATAL_ERROR
   27 call dsig_wrapup (obj)

      end subroutine dsig

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

      subroutine dsig_wrapup (obj)
      implicit none
      type(dsig_struct),intent(inout) :: obj       ! arguments
      integer :: tot

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      tot=obj%numtrc_tot-obj%numtrc_flag
      call pc_print('DSIG: designatured ', tot, ' traces')

      return
      end subroutine dsig_wrapup

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module dsig_module
