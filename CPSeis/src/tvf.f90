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
! Name       : TVF     (Time Varying Filter)  [simplified version of TSVF]
! Category   : filters
! Written    : 1986-07-17   by: Bob Baumel and Shein Wang
! Revised    : 2009-10-02   by: Douglas Hanson Fix hdr_flag=0 if test 
! Maturity   : production
! Purpose    : Perform time and head mute varying bandpass frequency filtering.
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
! TVF is a simple time varying bandpass frequency filter process operating in
! the time domain.  It is designed to perform the majority of routine bandpass
! filter tasks while being easy to use.  Individual trapezoid filter bands are
! specified by the parameters:  FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL,
! FREQ_HIGH_NONE, and PHASE.  TVF can also apply a bandpass filter that varies
! laterally according to the head mute time.
!
!
! Filter Band Transitions
!
! Each filter band is active over a particular time range with the time range
! boundaries defined by "transition times." The first filter band starts at the
! beginning of the trace and the last filter band ends at the end of the trace.
!
! Adjacent filter bands overlap in regions ("transition zones") where the
! filter bands blend.  Within these zones, the trace is filtered by both filter
! bands and the results combined by a linear taper.
!
!
! Time and Frequency Domain
!
! TVF does all filtering by convolution in the time domain, using operators
! created by FFT while in setup mode.
!
!
! Mute Header Words
!
! Although filter tails are added to traces by TVF, the values of the mute
! header words are not changed.
!
!
! Space Modes of Operation
!
! In the TIME_ONLY mode, the filter may vary in time only.  There is only one
! set of frequencies and times.
!
! In the MUTE mode, the specified times are relative to the mute time, so that
! the mute time acts as "zero."  Otherwise this mode is the same as the
! TIME_ONLY mode.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Filter Types
!
! TVF is designed to apply bandpass filters, but it will apply a low-cut
! (high-pass) filter if FREQ_HIGH_FULL and FREQ_HIGH_NONE are set to Nyquist
! and it will apply a high-cut (low-pass) filter if FREQ_LOW_NONE and
! FREQ_LOW_FULL are set to zero.
!
!
! Operator Length at Low Frequency
!
!   Normally, values of LEN_OP should be chosen such that
!
!      LEN_OP >= 1.0/(lowest frequency in filter).
!
!   For very low frequencies LEN_OP may be prohibitively long; in these
!   cases a frequency domain filter, such as GENFILT, should be used.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                   GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 2       Head mute index            Used if OPT_MODE = MUTE
! 25      LAV                        Reset
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 45. 2009-10-02 Hanson     Modify hdr_flag if test to avoid compiler problem.
! 44. 2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
!043. 2006-01-10  B. Menger   Removed Unused Variables.
! 42. 2002-05-06 Vunderink  Added parallel control parameters
! 41. 2001-11-05 Stoeckley  Move trap subroutine to different location in code
!                            to make the intel compiler happy.
! 40. 2001-04-30 Stoeckley  Fix to allow NWIH > 64.
! 39. 2000-12-08 Stoeckley  Changed wrapup flag.
! 38. 2000-09-15 O'Brien    Enhanced much of the GUI interaction with lists
! 37. 2000-08-30 O'Brien    Converted TSVF to TVF by stripping out options
!                             for 3-D operation and removing control points
!                             options.
! 36. 2000-08-25 O'Brien    Fixed bug in tsvf_rsamp_tran_pts(), where
!                             transition times of the lowest XY grid
!                             coordinate were used everywhere, preventing
!                             spacially varying transition zones when
!                             MODE_SPACE==GRID and MODE_TIME==TRANSITIONS
! 35. 2000-08-10 O'Brien    Documentation update to accomodate changes to
!                             header word naming convention... variable names
!                             made to be consistent with new convention.
!                             Missing grid values now interpolated/replicated
!                             along the X axis.
!                           Fix bug when retrieving filters for
!                             MODE_SPACE=='GRID'
!                           Added filter parameter GUI sensitivity settings
!                           Added messages to assist users wanting
!                             time-invariant filters
!                           Adjusted traps to accomodate non-essential filter
!                             parameters.
! 34. 2000-06-16 O'Brien    Fix FRONTEND_UPDATE traps for filter parameters
!                           Improve accounting for NSTORE, NSCRATCH values
! 33. 2000-05-24 O'Brien    Implement EzGUI Layout
!                           Adjust link list handling
! 32. 2000-03-16 O'Brien    Force LEN_OP to be an integer number of samples
!                             long (cosmetics for front end)
! 31. 2000-03-01 O'Brien    Straightened out inquire() logic in wrapup that
!                             was causing 'Unconnected unit' error
!                           Brought documentation closer to a current state
! 30. 2000-02-21 O'Brien    TROT_QC impelemented.
!                           Add inline interpolation/replication of filter
!                             parameters to missing grid nodes.
! 29. 2000-02-11 O'Brien    Revised array traps
! 28. 2000-02-08 O'Brien    Put in some GUI sensitivity flags for testing
! 27. 2000-02-03 O'Brien    Implemented MODE_SPACE='GRID' options.
!                           Changed array association tests to adapt to
!                             new behavior in pc_module.
!                           PATHNAME_QC implemented.
! 26. 2000-01-14 O'Brien    Remove debugging routine fltr_filtrgs
!                             reinstate use fltr_module
! 25. 2000-01-13 O'Brien    Revised traps, added GUI pull-down lists
! 24. 1999-12-29 O'Brien    Force array sizes .ge. 0 in tsvf_trap
! 23. 1999-12-21 O'Brien    Made tsvf_update and tsvf_initialize public
! 22. 1999-12-17 O'Brien    Full f90 conversion.
! 21. 1998-11-10 Vunderink  Begin using the f90 compiler.
! 20. 1998-05-18 Vunderink  Added OPT=6
! 19. 1997-11-19 Vunderink  Fixed NCODE to output names for F1, F2, and
!                           TZL parameters in OPT=4 and 5 which are unique
!                           from parameters for OPT= 1, 2, and 3.
! 18. 1997-11-17 Vunderink  Fixed bug in saving filters for reuse in
!                           OPT=4 and 5.
! 17. 1997-07-17 Vunderink  Changed assign command for temporary files
! 16. 1997-07-16 Vunderink  Added OPT=4 and 5
! 15. 1997-02-25 Goodger    Fix ncode to output unique names on TTIM,TZL
!                           parameters for ATB. Calculate nparm.
! 14. 1996-07-02 Goodger    Put variable IT2 in a SAVE statement.
! 13. 1989-07-19 Troutt     Swapped locations of input trace and filter
!                           output in SCRATCH to avoid addressing problems
!                           in FILTGS.  Also cleared NTZ in setup before
!                           main DCODE, fixed problem in History File
!                           with multiple TSVF's-job.
! 12. 1989-07-10 Troutt     Put END-OF-TRACE time in TTIM(nfilt) for
!                           history file.
! 11. 1989-06-21 Troutt     Added parameter HF# for flagged traces.
! 10. 1989-06-01 Troutt     Renamed program from TVF to TSVF
!                           (Began w- USER10:[CPS.FILTERS]TVF.CFT;15).
!                           Deleted code which accomodated old-old jobs
!                           (prior to linked arrays), including #FILT
!                           and IPRT parameters.
!                           Allowed user to spatially vary the times
!                           for filter application as in CONSEIS TVF:
!                           added parameters OPT, HB#, HL#, BAS, & LIN;
!                           replaced TLAST array w- TTIM array, and
!                           OVLP parameter w- TZL array.
! 9.  1988-09-23 Howard     NWIH and NWPT conversion.
! 8.  1988-08-17 Baumel     Match change in TVFBPS primitive.
! 7.  1988-07-28 Baumel     Use new DCODE-NCODE tables.
! 6.  1988-06-02 Baumel     TVF leaves mute header word alone.
! 5.  1988-04-23 Baumel     Add CPSPRT calls.
! 4.  1987-04-14 Wang       Fixed bug involving zero in mute header word.
! 3.  1987-04-09 Hanson     Added NCODE for history records.
! 2.  1987-01-16 Baumel     Added IPRT (print switch).
! 1.  1986-07-17 Baumel     Original Version.
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
!                  ALTERNATE INTERNAL CALLING METHODS
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
!                          PROGRAMMING NOTES
!
!   The main trace processing routine tvf() is relatively simple. The
!   flow follows this template:
!
!      if (something looks wrong) return
!
!      Loop over NTR
!
!        call tvf_get_local_filters() to retrieve time varying filters
!                                     for current trace
!        Loop over filters
!          convolve each filter with the appropriate trace time window
!          accumulate the filtered trace fragment to an output trace
!        end of loop over filters
!
!      end of loop over traces
!
!      return
!
!
!   Any variation in filter application due to OPT_MODE is handled inside the
!   routine tvf_get_local_filteres(). Once the filters are available, they are
!   applied to the traces without further consideration of any flags or modes.
!
!   The way tvf_get_local_filters() is implemented, it will be fairly easy to
!   add back functionality that was stripped out when TVF was reinvented from
!   TSVF. Old options like CTRL_POINTS and HEADERS can be easily implemented.
!
!   Implementing a 3-D mode of operation will be somewhat more difficult.
!   To do this, there should probably be only one additional parameter called
!   PATHNAME. Users would then specify a file that contains a description of
!   how filter parameters vary in space. TVF could pass this information along
!   to a new primitive that would deal with all the issues of finding new
!   filter parameters on and between a spatial grid. This primitive could be
!   setup in the normal way inside tvf_update(), and then its main function
!   would be called from tvf_get_local_filters().
!
!
! For clarity, to facilitate maintenance, and to maintain consistency with
! variable names in other filtering processes, the following user_parameter
! to program_backend_name translation is used:
!
!        USER_PARAMETER       PROGRAM_BACKEND_NAME
!        ----------------     --------------------
!        FREQ_LOW_NONE        f1
!        FREQ_LOW_FULL        f2
!        FREQ_HIGH_NONE       f3
!        FREQ_HIGH_FULL       f4
!        PHASE                ph
!        FILTER_TYPE          ftyp
!        TIMES                time
!        LEN_ZONE             zlen
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS TVF Process/NC=80>
!
!     Perform time and head mute varying bandpass frequency filtering.
!
!    `---------------------------------------------------------------------
!     OPT_MODE=`CCCCCCCC      HDR_FLAG= `IIIIIIIII      LEN_OP= `FFFFFFFF
!    `---------------------------------------------------------------------
!
!
!FREQ_LOW_NONEFREQ_LOW_FULLFREQ_HIGH_FULLFREQ_HIGH_NONEPHASE  TIMES   LEN_ZONE
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!`FFFFFFFFFFFF`FFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFFFFFFFFF`FFFFFF`FFFFFFF`FFFFFFF
!
!<PARMS FREQ_LOW_NONE_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="OPT_MODE">
!<Tip> Mode for specifying spatial variation of the filter. </Tip>
! Default = TIME_ONLY
! Allowed = TIME_ONLY (Filter may vary in time only.)
! Allowed = MUTE      (Specified times are relative to the mute time.)
! If OPT_MODE = TIME_ONLY, filter may vary in time only.  There is only one
! set of frequencies and times.
! If OPT_MODE = MUTE, same as TIME_ONLY but specified times are relative to
! the mute time, so that mute time acts as "zero."
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! If HDR_FLAG = 0, then all traces are filtered.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are filtered.
!</Help>
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of convolutional operators, in seconds. </Tip>
! Default = 0.4
! Allowed = real > 0.0
!
! Normally LEN_OP >= 1.0/(lowest frequency in filter).  For very low
! frequencies, the required values of LEN_OP may be prohibitively long.
! In these cases a frequency domain filter, such as FILTR, should be used.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Low frequency limit where amp spectrum diminishes to 0.0, in Hz. </Tip>
! Default =  -
! Allowed = real array
! Reject between 0.0 frequency and FREQ_LOW_NONE, pass between FREQ_LOW_FULL
! and FREQ_HIGH_FULL, reject between FREQ_HIGH_NONE and Nyquist, with linear
! tapers between pass and reject regions.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Low frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default =  -
! Allowed = real array
! Reject between 0.0 frequency and FREQ_LOW_NONE, pass between FREQ_LOW_FULL
! and FREQ_HIGH_FULL, reject between FREQ_HIGH_NONE and Nyquist, with linear
! tapers between pass and reject regions.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> High frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default =  -
! Allowed = real array
! Reject between 0.0 frequency and FREQ_LOW_NONE, pass between FREQ_LOW_FULL
! and FREQ_HIGH_FULL, reject between FREQ_HIGH_NONE and Nyquist, with linear
! tapers between pass and reject regions.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> High frequency limit where amp spectrum diminishes to 0.0, in Hz. </Tip>
! Default =  -
! Allowed = real array
! Reject between 0.0 frequency and FREQ_LOW_NONE, pass between FREQ_LOW_FULL
! and FREQ_HIGH_FULL, reject between FREQ_HIGH_NONE and Nyquist, with linear
! tapers between pass and reject regions.
!</Help>
!
!<Help KEYWORD="PHASE">
!<Tip> Array of filter band phase, in degrees. </Tip>
! Default = 0.0
! Allowed = real array
!</Help>
!
!<Help KEYWORD="TIMES">
!<Tip> Array of increasing transition times between filter bands. </Tip>
! Default = 0.0
! Allowed = real array
! Array of increasing transition times between filter bands, in seconds.
! Transition times are the centers of transition zones where adjacent filter
! bands blend.
!</Help>
!
!<Help KEYWORD="LEN_ZONE">
!<Tip> Array of transition zone lengths, in seconds. </Tip>
! Default = 0.2
! Allowed = real array
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!---------------------------- start module here -----------------------------!!
!!---------------------------- start module here -----------------------------!!
!!---------------------------- start module here -----------------------------!!


      module tvf_module

      use fft_module
      use bandps_module
      use fltr_module
      use sort_module
      use lav_module
      use pc_module
      use named_constants_module
      use sizeof_module

      implicit none

!<execute_only>

      private
      public tvf_create, tvf, tvf_wrapup, tvf_delete
      public tvf_update, tvf_initialize

!</execute_only>

      character(len=100),public,save :: TVF_IDENT = &
             '$Id: tvf.f90,v 1.44 2006/09/11 13:15:52 Stoeckley prod sps $'

!!-------------------------- parameter structure -----------------------------!!
!!-------------------------- parameter structure -----------------------------!!
!!-------------------------- parameter structure -----------------------------!!

      type,public :: tvf_struct

        private

        ! Wrapup param
        logical              :: skip_wrapup     ! Wrapup flag

        ! Execution mode info
        character(len=9)     :: opt_mode        ! User parameters

        ! Headers interogated
        integer              :: hdr_flag        ! User parameters

        ! Filter definitions
        real,pointer         :: f1(:)           ! User parameters
        real,pointer         :: f2(:)           ! User parameters
        real,pointer         :: f3(:)           ! User parameters
        real,pointer         :: f4(:)           ! User parameters
        real,pointer         :: ph(:)           ! User parameters
        real                 :: len_op          ! User parameters

        ! Transition zone params
        real,pointer         :: time(:)         ! User parameters
        real,pointer         :: zlen(:)         ! User parameters

        ! Globals
        integer              :: ndpt,nwih       ! Global variables
        real                 :: tstrt           ! Global variables
        real                 :: dt              ! Global variables


        ! Sizes of arrays retrieves through parameter cache
        integer              :: size_f1         ! Dependent variables
        integer              :: size_f2         ! Dependent variables
        integer              :: size_f3         ! Dependent variables
        integer              :: size_f4         ! Dependent variables
        integer              :: size_ph         ! Dependent variables
        integer              :: size_time       ! Dependent variables
        integer              :: size_zlen       ! Dependent variables
        integer              :: nlist           ! Dependent variables

        ! Other dependent variables
        integer              :: nfilt           ! Dependent variables
        integer              :: filt_len        ! Dependent variables
        integer              :: ncenter         ! Dependent variables
        integer              :: nshift          ! Dependent variables
        real                 :: dti             ! Dependent variables
        real                 :: tend            ! Dependent variables
        integer              :: ntin,ntfl       ! Dependent variables
        integer              :: npo2            ! Dependent variables
        integer              :: nw              ! Dependent variables
        real                 :: fnyq,dw         ! Dependent variables
        character(len=10),pointer :: ftyp(:)    ! Dependent variables
        real,pointer         :: tts(:),tzs(:)   ! Dependent variables
        real,pointer         :: tlast(:),ovlp(:)! Dependent variables

        ! Filter construction structure
        type(fft_struct),pointer:: crfft        ! Dependent variables

        ! An array of filters to be applied to traces
        real,pointer         :: tfilters(:,:)   ! Dependent variables

        ! Vectors to hold input and filtered traces
        real,pointer         :: trc_in(:)       ! Dependent variables
        real,pointer         :: trc_filt(:)     ! Dependent variables

        ! Logical units
        integer              :: prtlu           ! Utility variables
        integer              :: debuglu         ! Utility variables

      end type tvf_struct

!!----------------------------- data ---------------------------------------!!
!!----------------------------- data ---------------------------------------!!
!!----------------------------- data ---------------------------------------!!

      type(tvf_struct),pointer,save  :: object             ! needed for traps

      integer,parameter              :: MAX_FFT_SIZE=8192   ! used in setup

      integer,parameter              :: opt_mode_nopt=2     ! used in setup
      character(len=9),save          :: opt_mode_options(2) ! used in traps

      data opt_mode_options /'TIME_ONLY','MUTE'/

      contains

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
   
      subroutine tvf_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments
   
!---------------------------------------------------
! Run through the different keyword cases
      keyword_select: select case (keyword)

      case ('OPT_MODE') keyword_select
        call string_to_upper(object%opt_mode)

        if ( all(opt_mode_options/=object%opt_mode)) then
          call pc_warning('TVF: Invalid value for OPT_MODE: ' &
                          //object%opt_mode//                 &
                          ' Resetting to default: TIME_ONLY.')
          object%opt_mode = 'TIME_ONLY'
        endif

      case ('LEN_OP') keyword_select
        object%len_op = nint(object%len_op/object%dt) * object%dt
        if (object%len_op <= 0.0) then
          object%len_op = nint(0.4/object%dt) * object%dt
          call pc_info('TVF: LEN_OP is .LE. 0.0. Resetting to default: ', &
                        object%len_op)
        endif

      case ('HDR_FLAG') keyword_select
        if (object%hdr_flag<0 .or. object%hdr_flag>object%nwih) then
          call pc_info('TVF: HDR_FLAG is out of range 0 to ', object%nwih, &
                       ' Reseting to default: 0.')
          object%hdr_flag = 0
        endif

      case ('FREQ_LOW_NONE') keyword_select
        ! frequencies must be between 0.0 and Nyquist.
        where (object%f1 > object%fnyq) object%f1 = object%fnyq
        where (object%f1 <    0.0     ) object%f1 = 0.0
          
      case ('FREQ_LOW_FULL') keyword_select
        ! frequencies must be between 0.0 and Nyquist.
        where (object%f2 > object%fnyq) object%f2 = object%fnyq
        where (object%f2 <    0.0     ) object%f2 = 0.0
          
      case ('FREQ_HIGH_NONE') keyword_select
        ! frequencies must be between 0.0 and Nyquist.
        where (object%f3 > object%fnyq) object%f3 = object%fnyq
        where (object%f3 <    0.0     ) object%f3 = 0.0
          
      case ('FREQ_HIGH_FULL') keyword_select
        ! frequencies must be between 0.0 and Nyquist.
        where (object%f4 > object%fnyq) object%f4 = object%fnyq
        where (object%f4 <    0.0     ) object%f4 = 0.0

      case ('TIMES') keyword_select
        ! times must be between TSTRT and TEND
        where (object%time < object%tstrt) object%time = object%tstrt
        where (object%time > object%tend ) object%time = object%tend 
        if ( object%nfilt > 2 .and. &
             all(object%time(2:object%nfilt-1)==object%time(1)) )then
          if ( object%time(1) > object%tend-object%dt ) then
            call pc_warning ("TVF: All values of TIMES list equal TEND. &
                             &Only the first filter will be active.")
          else
            call pc_warning ("TVF: All values of TIMES list are the same. &
                             &Only the first and last filters will be active.")
          endif
        endif

      case ('LEN_ZONE') keyword_select
        ! transition zone lengths should be no longer than tend-tstrt
        where (object%zlen > object%tend-object%tstrt) &
                        object%zlen = object%tend-object%tstrt

      end select keyword_select

      return
      end subroutine tvf_trap


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine tvf_create (obj)
      implicit none
      type(tvf_struct),pointer :: obj       ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers
      nullify (obj%f1)
      nullify (obj%f2)
      nullify (obj%f3)
      nullify (obj%f4)
      nullify (obj%ph)
      nullify (obj%time)
      nullify (obj%zlen)
      nullify (obj%ftyp)
      nullify (obj%tfilters)
      nullify (obj%tts)
      nullify (obj%tzs)
      nullify (obj%tlast)
      nullify (obj%ovlp)
      nullify (obj%crfft)
      nullify (obj%trc_in)
      nullify (obj%trc_filt)

      call tvf_initialize (obj)

      return
      end subroutine tvf_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine tvf_delete (obj)
      implicit none
      type(tvf_struct),pointer :: obj       ! arguments

!<execute_only>
      call tvf_wrapup (obj)
!</execute_only>

      if (associated(obj%f1))         deallocate (obj%f1)
      if (associated(obj%f2))         deallocate (obj%f2)
      if (associated(obj%f3))         deallocate (obj%f3)
      if (associated(obj%f4))         deallocate (obj%f4)
      if (associated(obj%ph))         deallocate (obj%ph)
      if (associated(obj%time))       deallocate (obj%time)
      if (associated(obj%zlen))       deallocate (obj%zlen)
      if (associated(obj%ftyp))       deallocate (obj%ftyp)
      if (associated(obj%tfilters))   deallocate (obj%tfilters)
      if (associated(obj%tts))        deallocate (obj%tts)
      if (associated(obj%tzs))        deallocate (obj%tzs)
      if (associated(obj%tlast))      deallocate (obj%tlast)
      if (associated(obj%ovlp))       deallocate (obj%ovlp)
      if (associated(obj%crfft))      call fft_delete (obj%crfft)
      if (associated(obj%trc_in))     deallocate (obj%trc_in)
      if (associated(obj%trc_filt))   deallocate (obj%trc_filt)

      deallocate(obj)

      return
      end subroutine tvf_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine tvf_initialize (obj)
      implicit none

      type (tvf_struct)   :: obj            ! Arguments
!-------------------------------------------
! Initialize scalar user parameters
      obj%opt_mode = 'TIME_ONLY'
      obj%hdr_flag = 0
      obj%len_op   = 0.4

! Initialize Globals
      obj%ndpt = INIL
      obj%nwih = INIL
      obj%dt   = FNIL
      obj%tstrt= 0.0

! A few globals are needed for initializing dependent variables
      call pc_get_global ('NDPT', obj%ndpt)
      call pc_get_global ('DT',   obj%dt)
      call pc_get_global ('TSTRT',obj%tstrt)

! Initialize dependent variables
      obj%dti   = 1./obj%dt
      obj%tend  = obj%tstrt + obj%dt*(obj%ndpt-1)
      obj%fnyq  = 0.5/obj%dt

      obj%ntin  = 0    ! counter for traces input
      obj%ntfl  = 0    ! counter for traces filtered (different if hdr_flag>0)

      obj%size_f1    = 0
      obj%size_f2    = 0
      obj%size_f3    = 0
      obj%size_f4    = 0
      obj%size_ph    = 0
      obj%size_time  = 0
      obj%size_zlen  = 0
      obj%nlist      = 0
      obj%nfilt      = 0

      obj%filt_len   = 0
      obj%ncenter    = 0
      obj%nshift     = 0
      obj%npo2       = 0
      obj%nw         = 0
      obj%dw         = 0.0

      obj%prtlu      = 0
      obj%debuglu    = 0

      call tvf_update(obj)

      return
      end subroutine tvf_initialize


!!------------------------ start of update -----------------------------------!!
!!------------------------ start of update -----------------------------------!!
!!------------------------ start of update -----------------------------------!!

      subroutine tvf_update(obj)
      implicit none

      type (tvf_struct),target:: obj       ! Arguments
      integer          :: update_state
      integer          :: max_fft_pts, nscratch, nstore   
      integer          :: ier1,ier2,ier3,ier4,ier5,ier6,ier7       

      real     :: ovlp_max

      integer  :: SIZEOF_INT
      integer  :: SIZEOF_REAL
      integer  :: SIZEOF_DOUBLE
      integer  :: SIZEOF_COMPLEX
      integer  :: SIZEOF_CHAR

!------------------------------------------------------------------

      SIZEOF_INT     = sizeof(1)
      SIZEOF_REAL    = sizeof(1.0)
      SIZEOF_DOUBLE  = sizeof(1.0d0)
      SIZEOF_COMPLEX = sizeof(cmplx(1.0,1.0))
      SIZEOF_CHAR    = 1               ! sizeof_module doesn't do characters

      object => obj
      obj%skip_wrapup = .true.
      update_state = pc_get_update_state()


!!-------------------------- read parameters ---------------------------------!!
!!-------------------------- read parameters ---------------------------------!!
!!-------------------------- read parameters ---------------------------------!!


      call pc_register_array_names ("freq_low_none_arrayset", (/  &
                                    "freq_low_none ",             &
                                    "freq_low_full ",             &
                                    "freq_high_full",             &
                                    "freq_high_none",             &
                                    "phase         ",             &
                                    "times         ",             &
                                    "len_zone      " /))

      call pc_get_global ('NDPT', obj%ndpt)
      call pc_get_global ('NWIH', obj%nwih)
      call pc_get_global ('DT',   obj%dt)
      call pc_get_global ('TSTRT',obj%tstrt)

      call pc_get ('OPT_MODE', obj%opt_mode, tvf_trap)
      call pc_get ('HDR_FLAG', obj%hdr_flag, tvf_trap)
      call pc_get ('LEN_OP',   obj%len_op,   tvf_trap)

      call pc_alloc ('FREQ_LOW_NONE',  obj%f1,   obj%size_f1  )
      call pc_alloc ('FREQ_LOW_FULL',  obj%f2,   obj%size_f2  )
      call pc_alloc ('FREQ_HIGH_FULL', obj%f3,   obj%size_f3  )
      call pc_alloc ('FREQ_HIGH_NONE', obj%f4,   obj%size_f4  )
      call pc_alloc ('PHASE',          obj%ph,   obj%size_ph  )
      call pc_alloc ('TIMES',          obj%time, obj%size_time)
      call pc_alloc ('LEN_ZONE',       obj%zlen, obj%size_zlen)

!!------------------------------ verify parameters -------------------------!!
!!------------------------------ verify parameters -------------------------!!
!!------------------------------ verify parameters -------------------------!!

! pc_alloc always allocates at least 1 element, but returns size=0 as
! appropriate. Unfortunately, the allocated memory isn't initialized to NIL
! so the workings of subroutine tvf_list_consistency wants to reset the sizes
! to non-zero length. Here the "hidden" array element is initialized to FNIL
! so tvf_list_consistency doesn't detect bogus non-NIL values.
! It's done here to provide documentation and to avoid obtuse logic elsewhere.
      if ( obj%size_f1   == 0 ) obj%f1(1)   = FNIL
      if ( obj%size_f2   == 0 ) obj%f2(1)   = FNIL
      if ( obj%size_f3   == 0 ) obj%f3(1)   = FNIL
      if ( obj%size_f4   == 0 ) obj%f4(1)   = FNIL
      if ( obj%size_ph   == 0 ) obj%ph(1)   = FNIL
      if ( obj%size_time == 0 ) obj%time(1) = FNIL
      if ( obj%size_zlen == 0 ) obj%zlen(1) = FNIL

! Lists of filter parameters should be consistent
      if ( update_state /= PC_GUI ) then
        call tvf_list_consistency (obj,ier1)
        if ( ier1 /= 0 ) return
        call tvf_verify_filter_parms(obj)
        call tvf_trap ( 'TIMES' )
      endif

! Get the FFT size for filter construction
      obj%filt_len = int(obj%len_op/obj%dt/2)*2 + 1      ! this _must_ be odd
      ! Intel has a tendency to get it wrong so add a test and adjustment
      if(float(obj%filt_len)<obj%len_op/obj%dt) obj%filt_len=obj%filt_len+2
      max_fft_pts = min(max(obj%ndpt,obj%filt_len),MAX_FFT_SIZE)

      obj%npo2 = 512
      do while ( obj%npo2 < max_fft_pts )
        obj%npo2 = obj%npo2*2
      enddo

! Set nw and dw for frequency domain filters.
      obj%nw = obj%npo2/2 + 1
      obj%dw = obj%fnyq/(obj%nw-1)

! We will need the central sample number for the filters and the number
! of samples to shift the filter to line it up at time=0 in the correlation
      obj%ncenter= (obj%filt_len+1)/2
      obj%nshift = obj%ncenter-1



!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!-------------------------- write parameters ------------------------------!!
!!-------------------------- write parameters ------------------------------!!
!!-------------------------- write parameters ------------------------------!!

      call pc_put_options_field ('OPT_MODE', opt_mode_options, opt_mode_nopt)

      call pc_put ('OPT_MODE', obj%opt_mode )
      call pc_put ('HDR_FLAG', obj%hdr_flag )
      call pc_put ('LEN_OP',   obj%len_op   )

      call pc_put ('FREQ_LOW_NONE',  obj%f1,   obj%size_f1  )
      call pc_put ('FREQ_LOW_FULL',  obj%f2,   obj%size_f2  )
      call pc_put ('FREQ_HIGH_FULL', obj%f3,   obj%size_f3  )
      call pc_put ('FREQ_HIGH_NONE', obj%f4,   obj%size_f4  )
      call pc_put ('PHASE',          obj%ph,   obj%size_ph  )
      call pc_put ('TIMES',          obj%time, obj%size_time)
      call pc_put ('LEN_ZONE',       obj%zlen, obj%size_zlen)


!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!

! Get a number for scratch space resources
      nscratch = 0

      ! work space used for filter parameter lists in tvf_update
      nscratch = nscratch + 7*obj%nlist*SIZEOF_REAL

      ! work space allocated in tvf_list_consistency
      nscratch = nscratch + obj%nlist*SIZEOF_REAL
      nscratch = nscratch + obj%nfilt*SIZEOF_INT

      ! space for ffts (used only at setup)
      nscratch = nscratch + fft_mem_usage(obj%npo2,1)


! NSCRATCH is finished. Now do NSTORE.
      nstore = 0

    ! space allocated in tvf_update
      ! input trace and filtered companion
      nstore = nstore + 2*obj%ndpt*SIZEOF_REAL

      ! the array of filters
      nstore = nstore + obj%filt_len*obj%nfilt*SIZEOF_REAL

      ! tlast, ovlp, tts, and tzs
      nstore = nstore + 4*obj%nfilt*SIZEOF_REAL

      ! automatic arrays in tvf_build_filter_array
      nstore = nstore + obj%npo2*SIZEOF_REAL
      nstore = nstore + obj%nw  *SIZEOF_COMPLEX

      ! automatic array in tvf_mix
      ovlp_max = max( obj%dt,maxval(obj%zlen,MASK=(obj%zlen/=FNIL)) )
      nstore = nstore + (ovlp_max/obj%dt)*SIZEOF_REAL

! Done with memory resource estimates. Stash them in the pc.
      call pc_put_control ('NSTORE', nstore/SIZEOF_REAL)
      call pc_put_control ('NSCRATCH', nscratch/SIZEOF_REAL)
      call pc_put_control ('nstore'  , nstore)
      call pc_put_control ('PARALLEL_SAFE'        ,.true.)
      call pc_put_control ('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')

!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!
!!---------------------- set GUI sensitivity flags -------------------------!!

      if ( update_state == PC_GUI .or. &
           update_state == PC_FRONTEND ) then
        call tvf_set_sensitivities(obj)
      endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! get i/o units
      obj%prtlu = pc_get_lun()

!      call getlun (obj%debuglu, ier1)
!      open (unit=obj%debuglu, status='REPLACE', iostat=ier1)

! Create inverse fft object
      ier1 = fft_create (obj%crfft, 1,obj%npo2,'ctor')

! Get storage for copy of input trace and its filtered companion
      allocate (obj%trc_in(obj%ndpt),   stat=ier1)
      allocate (obj%trc_filt(obj%ndpt), stat=ier2)

! Set up precomputed vectors and params.

      ! Get storage for tfilters() array.
      allocate (obj%tfilters(obj%filt_len,obj%nfilt), stat=ier3)

      ! Call routine to populate tfilters() array
      call tvf_build_filter_array &
             (obj%tfilters, obj%filt_len, obj%nfilt,      &
              obj%crfft, obj%nw, obj%dw, obj%npo2,        &
              obj%f1(:), obj%f2(:), obj%f3(:), obj%f4(:), &
              obj%ph(:), obj%ftyp(:))

      ! Space used for ffts can now be reclaimed
      call fft_delete (obj%crfft)
 
      ! Get storage for transition zone information
      allocate (obj%tlast(obj%nfilt), stat=ier4)
      allocate (obj%ovlp(obj%nfilt),  stat=ier5)
      allocate (obj%tts(obj%nfilt),   stat=ier6)
      allocate (obj%tzs(obj%nfilt),   stat=ier7)

      ! Put trace-end-time in for history
      ! time() and zlen() only have nfilt-1 user entries
      obj%time(obj%nfilt:) = obj%tend 
      obj%zlen(obj%nfilt:) = 0.0    
      call pc_put_process ('TIMES', obj%time(:), obj%nfilt)
      call pc_put_process ('TZL',   obj%zlen(:), obj%nfilt)

      ! Always fix the last entry in tlast() and ovlp() vectors
      obj%tlast(obj%nfilt) = obj%tend
      obj%ovlp(obj%nfilt)  = 0.0   

      ! Get transition zone times and lengths 
      if (obj%opt_mode == 'TIME_ONLY') then
        !use input filter times "as is" 
        obj%tlast(:obj%nfilt-1) = obj%time(:obj%nfilt-1) 
        obj%ovlp(:obj%nfilt-1) = obj%zlen(:obj%nfilt-1) 
      else
        !put input times into storage for use later. 
        obj%tts(:) = obj%time(:) 
        obj%tzs(:) = obj%zlen(:) 
      endif 

      write(obj%prtlu,*) &
        '=>TVF: Number of filter operators saved = ', obj%nfilt
      write(obj%prtlu,*) &
        '=>TVF: Each operator has length ', obj%filt_len, ' samples.'

!</execute_only>

!!----------------------- finish update ------------------------------------!!
!!----------------------- finish update ------------------------------------!!
!!----------------------- finish update ------------------------------------!!

      return
      end subroutine tvf_update



!*******************************************************************************
! Subroutine to ensure consistent filter parameter list length
!*******************************************************************************
      subroutine tvf_list_consistency (obj,ier)

      implicit none

      type(tvf_struct)     :: obj
      integer              :: ier, j, i_out, ilist, nlist_in
      real, allocatable    :: rtmp(:)
      integer, allocatable :: indices(:)
!------------------------------------------------------------------------------

! Initialize ier to 0
      ier = 0

! Hold on to the maximum list size on input
      nlist_in = max(obj%size_f1,obj%size_f2,obj%size_f3,obj%size_f4, &
                     obj%size_ph,obj%size_time,obj%size_zlen)

! On initial update all sizes should be 0 and we don't need the
! rest of this garbage. ( Forging ahead anyway caused CFE to crater!!! )
      if ( nlist_in == 0 ) return

! Now go make sure all arrays are the same length regardless of any errors

      allocate(rtmp(nlist_in))

      if ( obj%size_f1 < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%f1)) then
          rtmp(1:obj%size_f1) = obj%f1(1:obj%size_f1)
          deallocate(obj%f1)
        endif
        allocate(obj%f1(nlist_in))
        obj%f1(1:nlist_in) = rtmp(1:nlist_in)
      endif

      if ( obj%size_f2 < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%f2)) then
          rtmp(1:obj%size_f2) = obj%f2(1:obj%size_f2)
          deallocate(obj%f2)
        endif
        allocate(obj%f2(nlist_in))
        obj%f2(1:nlist_in) = rtmp(1:nlist_in)
      endif

      if ( obj%size_f3 < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%f3)) then
          rtmp(1:obj%size_f3) = obj%f3(1:obj%size_f3)
          deallocate(obj%f3)
        endif
        allocate(obj%f3(nlist_in))
        obj%f3(1:nlist_in) = rtmp(1:nlist_in)
      endif

      if ( obj%size_f4 < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%f4)) then
          rtmp(1:obj%size_f4) = obj%f4(1:obj%size_f4)
          deallocate(obj%f4)
        endif
        allocate(obj%f4(nlist_in))
        obj%f4(1:nlist_in) = rtmp(1:nlist_in)
      endif

      if ( obj%size_ph < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%ph)) then
          rtmp(1:obj%size_ph) = obj%ph(1:obj%size_ph)
          deallocate(obj%ph)
        endif
        allocate(obj%ph(nlist_in))
        obj%ph(1:nlist_in) = rtmp(1:nlist_in)
      endif

      if ( obj%size_time < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%time)) then
          rtmp(1:obj%size_time) = obj%time(1:obj%size_time)
          deallocate(obj%time)
        endif
        allocate(obj%time(nlist_in))
        obj%time(1:nlist_in) = rtmp(1:nlist_in)
      endif

      if ( obj%size_zlen < nlist_in ) then
        rtmp(:) = FNIL
        if (associated(obj%zlen)) then
          rtmp(1:obj%size_zlen) = obj%zlen(1:obj%size_zlen)
          deallocate(obj%zlen)
        endif
        allocate(obj%zlen(nlist_in))
        obj%zlen(1:nlist_in) = rtmp(1:nlist_in)
      endif

      deallocate(rtmp)

! Remove indices with all FNILs and establish the maximum list length
      i_out = 0
      do ilist = 1,nlist_in
        if ( obj%f1  (ilist) == FNIL .and. &
             obj%f2  (ilist) == FNIL .and. &
             obj%f3  (ilist) == FNIL .and. &
             obj%f4  (ilist) == FNIL .and. &
             obj%ph  (ilist) == FNIL .and. &
             obj%time(ilist) == FNIL .and. &
             obj%zlen(ilist) == FNIL  ) cycle
        i_out = i_out+1
        obj%f1  (i_out) = obj%f1  (ilist)
        obj%f2  (i_out) = obj%f2  (ilist)
        obj%f3  (i_out) = obj%f3  (ilist)
        obj%f4  (i_out) = obj%f4  (ilist)
        obj%ph  (i_out) = obj%ph  (ilist)
        obj%time(i_out) = obj%time(ilist)
        obj%zlen(i_out) = obj%zlen(ilist)
      enddo
      obj%nlist = i_out

! Not all lists have to be user-defined. PHASE has a default of 0.0,
! LEN_ZONE has a default of 0.2 and TIMES has a default of TEND
      where ( obj%ph(1:obj%nlist)   == FNIL ) obj%ph(1:obj%nlist)   = 0.0
      where ( obj%zlen(1:obj%nlist) == FNIL ) obj%zlen(1:obj%nlist) = 0.2
      where ( obj%time(1:obj%nlist) == FNIL ) obj%time(1:obj%nlist) = obj%tend

! Get the non-NIL length of each input frequeny list list
      obj%size_f1 = count( obj%f1(1:obj%nlist) /= FNIL )
      obj%size_f2 = count( obj%f2(1:obj%nlist) /= FNIL )
      obj%size_f3 = count( obj%f3(1:obj%nlist) /= FNIL )
      obj%size_f4 = count( obj%f4(1:obj%nlist) /= FNIL )

! Complain if there are discrepancies.
      call tvf_squawk (obj,ier)

! If an error was found, no need to prepare for backend processing
      if ( ier /= 0 ) return

! Now the lists are all the same length, adjust them for backend processing

      ! get nfilt once and for all
      obj%nfilt = obj%nlist

      ! set up a vector for ftyp
      if (associated(obj%ftyp)) deallocate(obj%ftyp)
      allocate(obj%ftyp(obj%nfilt))
      obj%ftyp(1:obj%nfilt) = 'BANDPASS'

      ! make sure transition times are in ascending order
      if(allocated(indices)) deallocate(indices)
      if(allocated(rtmp))    deallocate(rtmp)

      allocate(indices(obj%nfilt))
      allocate(rtmp(obj%nfilt))

      indices(:) = (/(j,j=1,obj%nfilt)/)
      call sort_insisort(obj%time(:),indices,1,obj%nfilt)

      rtmp(:) = obj%f1(indices(:))
      obj%f1(:) = rtmp(:)

      rtmp(:) = obj%f2(indices(:))
      obj%f2(:) = rtmp(:)

      rtmp(:) = obj%f3(indices(:))
      obj%f3(:) = rtmp(:)

      rtmp(:) = obj%f4(indices(:))
      obj%f4(:) = rtmp(:)

      rtmp(:) = obj%ph(indices(:))
      obj%ph(:) = rtmp(:)

      rtmp(:) = obj%time(indices(:))
      obj%time(:) = rtmp(:)

      rtmp(:) = obj%zlen(indices(:))
      obj%zlen(:) = rtmp(:)

      deallocate(indices)
      deallocate(rtmp)

! Ensure the sizes for each list are known
      obj%size_f1   = obj%nfilt
      obj%size_f2   = obj%nfilt
      obj%size_f3   = obj%nfilt
      obj%size_f4   = obj%nfilt
      obj%size_ph   = obj%nfilt
      obj%size_time = obj%nfilt
      obj%size_zlen = obj%nfilt

      return
      end subroutine tvf_list_consistency


!*******************************************************************************
! Subroutine that squawks about errors in linked lists
!*******************************************************************************
      subroutine tvf_squawk ( obj, ier )
      implicit none

      type(tvf_struct), intent(in)   :: obj
      integer,           intent(out) :: ier
!----------------------------------------------------------

! Verify the existence of parameter lists
      if (obj%nlist == 0) then
        call pc_error("TVF: No filter parameters are present. FATAL.")
        ier=-1
      endif

! Check the lists for length consistency
      if (obj%size_f1 /= 0 .and. obj%size_f1 /= obj%nlist) then
        call pc_error('TVF: Filter parameter FREQ_LOW_NONE &
                      &has an incomplete list')
        ier=-1
      endif

      if (obj%size_f2 /= 0 .and. obj%size_f2 /= obj%nlist) then
        call pc_error('TVF: Filter parameter FREQ_LOW_FULL &
                      &has an incomplete list')
        ier=-1
      endif

      if (obj%size_f3 /= 0 .and. obj%size_f3 /= obj%nlist) then
        call pc_error('TVF: Filter parameter FREQ_HIGH_FULL &
                      &has an incomplete list')
        ier=-1
      endif

      if (obj%size_f4 /= 0 .and. obj%size_f4 /= obj%nlist) then
        call pc_error('TVF: Filter parameter FREQ_HIGH_NONE &
                      &has an incomplete list')
        ier=-1
      endif


! Now check for the existence of various parameters and notify the user
! if something is missing. LEN_ZONE has acceptable defaults so its absence
! is no cause for return. Same for PHASE.
      if(obj%size_f1 == 0) then
        call pc_error('TVF: FREQ_LOW_NONE is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_f2 == 0) then
        call pc_error('TVF: FREQ_LOW_FULL is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_f3 == 0) then
        call pc_error('TVF: FREQ_HIGH_FULL is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_f4 == 0) then
        call pc_error('TVF: FREQ_HIGH_NONE is missing and must be supplied')
        ier=-1
      endif

! All done
      return
      end subroutine tvf_squawk



!*******************************************************************************
! Verify validity of filters parameters
!*******************************************************************************
      subroutine tvf_verify_filter_parms (obj)

      implicit none

      type(tvf_struct)   :: obj               ! Arguments

      integer            :: ifilt             ! Local
      integer            :: result            ! Local
      character(len=80)  :: message           ! Local
!---------------------------------------

! Loop over the number of filters
      do ifilt = 1,obj%nfilt

! Pass this filter into bandps_check
        call bandps_check &
              (result,message,obj%fnyq,obj%ftyp(ifilt), &
               obj%f1(ifilt),obj%f2(ifilt),obj%f3(ifilt),obj%f4(ifilt), &
               obj%ph(ifilt))

! Finally, evaluate the result from bandps_check and act accordingly
        select case (result)
          case (BANDPS_INFO);     call pc_info(message)
          case (BANDPS_ERROR);    call pc_error(message)
          case (BANDPS_ENDERROR); call pc_error(message)
        end select

      enddo

      return
      end subroutine tvf_verify_filter_parms

   
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine tvf (obj,ntr,hd,tr)
      implicit none
      type(tvf_struct)               :: obj                  ! Arguments
      integer,          intent(inout):: ntr                  ! Arguments
      double precision, intent(inout):: hd(:,:)              ! Arguments
      real,             intent(inout):: tr(:,:)              ! Arguments

      integer                :: itrc, ifilt                    ! local
      integer                :: nfilt_here                     ! local
      integer                :: it1, it2, k1, ik1, k2, ik2     ! local
      integer                :: lovlah, lovla, lovlbh, lovlb   ! local
      real                   :: rinc
!----------------------------------------------------------
! Detect completion or problems
      if ( ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call tvf_wrapup(obj)
        return
      endif

! Increment traces handled counter
      obj%ntin = obj%ntin + ntr

! Loop over traces passed in
      do itrc = 1, ntr

! Check for flag and skip trace as required.
        if (obj%hdr_flag/=0 .and. hd(max(1,obj%hdr_flag),itrc)==0.0) cycle

! Increment filtered trace counter
        obj%ntfl = obj%ntfl + 1

! Copy input trace to working trace
        obj%trc_in(:obj%ndpt) = tr(:obj%ndpt,itrc)

! Establish the number of filters that will apply to this trace
! and fill tfilters (if necessary), ovlp and tlast
        call tvf_get_local_filters (obj, hd(:,itrc), nfilt_here)

! Loop over filters
        do ifilt = 1, nfilt_here

          ! it1 and it2 are the sample of transition zone midpoint
          ! at the top and bottom of trace fragment for filter tfilters(ifilt)
          it1 = it2
          it2 = min(nint((obj%tlast(ifilt)-obj%tstrt)*obj%dti+1.),obj%ndpt)

          ! lovlb and lovlbh are the length and half-length of lower
          ! transition zone
          lovlbh = nint(obj%ovlp(ifilt)*0.5*obj%dti)
          lovlb = lovlbh*2 + 1

          ! k2 is the last sample in trc_in to filter with the ifilt filter
          ! ik2 is the last sample in trc_in for filter roll-out 
          k2 = min(it2+lovlbh,obj%ndpt)
          ik2 = min(k2+obj%nshift,obj%ndpt)

          if (ifilt == 1) then
            ! for first filter, the output goes to tr()
            call fltr_filtrgs &
                  (obj%tfilters(:,ifilt), obj%filt_len, & ! filter and size
                   obj%trc_in(:), ik2,                  & ! data and size
                   tr(:,itrc), k2,                      & ! corr. and size
                   1, (-obj%nshift))                      ! sum flag, shift
          else
            ! for ifilt > 1, upper transition zone information is needed.
            ! lovla and lovlah are the length and half-length of upper
            ! transition zone
            lovlah = nint(obj%ovlp(ifilt-1)*0.5*obj%dti)
            lovla = lovlah*2 + 1
            ! rinc is the sample-by-sample weight in th upper trans-zone
            rinc = 1./(lovla + 1)

            ! k1 is the 1st sample in trc_in to filter with the ifilt filter
            ! ik1 is the 1st sample in trc_in for filter roll-in 
            k1 = max(it1-lovlah,1)
            ik1 = max(k1-obj%nshift,1)

            ! for ifilt > 1, the output goes to trc_filt and is mixed with
            ! the accumulating output trace
            call fltr_filtrgs &
                  (obj%tfilters(:,ifilt), obj%filt_len, &
                   obj%trc_in(ik1:ik2), ik2-ik1+1,      &
                   obj%trc_filt(k1:k2), k2-k1+1,        &
                   1, k1-ik1-obj%nshift)
            call tvf_mix (tr(:,itrc),obj%trc_filt(:),k1,it1,k2,lovlah,rinc)
          endif
        enddo

        ! recalculate trace LAV
        hd(HDR_LAV,itrc) = lav(tr(:,itrc),obj%ndpt)

      enddo

      return
      end subroutine tvf

!!--------------------------- internal subroutines--------------------------!!
!!--------------------------- internal subroutines--------------------------!!
!!--------------------------- internal subroutines--------------------------!!


!...|....!....|....2....|....3....|....4....|....5....|....6....|....7....|....8
!*******************************************************************************
! Retrieve a time varying set of filters to operate on the local trace.
! Return the number of filters to be applied.
!*******************************************************************************
      subroutine tvf_get_local_filters &
             (obj, hd, nfilt_here)

      implicit none

      type(tvf_struct)     :: obj                          ! Arguments
      integer, intent(out) :: nfilt_here                   ! Arguments
      double precision, intent(in) :: hd(:)                ! Arguments

      integer           :: izone                        ! local variables
      real              :: tadd                         ! local variables
!-------------------------------------------------------
! Branch to the specific case based on opt_mode
      select case (obj%opt_mode)

      case ( 'TIME_ONLY' )
        ! For opt_mode='TIME_ONLY', the arrays
        ! TLAST() and OVLP() were filled at setup time.
        nfilt_here = obj%nfilt

      case ( 'MUTE' )
        !Adjust input filter times based on the mute word.
        tadd = obj%tstrt + obj%dt*(hd(HDR_TOP_MUTE)-1)

        ! Count the number of filter transition zones within the time
        ! range of the trace and set tlast() and ovlp() values.
        izone = 0
        do
          izone = izone + 1
          if (izone > obj%nfilt-1 .or. &
              obj%tts(izone)+tadd >= obj%tend) exit
          obj%tlast(izone) = obj%tts(izone)+tadd
          obj%ovlp(izone) = obj%tzs(izone)
          cycle
        enddo
        nfilt_here = izone
        obj%tlast(nfilt_here) = obj%tend
        obj%ovlp(nfilt_here)  = 0.0

      end select

      return
      end subroutine tvf_get_local_filters



!*******************************************************************************
! Subroutine to build an array of filters
!*******************************************************************************
      subroutine tvf_build_filter_array             &
                   (filt_array, nt, nfilt,          &
                    fft_object, nw, dw, fft_length, &
                    f1, f2, f3, f4, phase, filt_type)

      implicit none

      integer, intent(in)   :: nt, nfilt, nw, fft_length
      real,    intent(in)   :: dw, f1(:), f2(:), f3(:), f4(:), phase(:)
      real,    intent(inout):: filt_array(:,:)
      type(fft_struct)      :: fft_object
      character(len=*), intent(in) :: filt_type(:)

      integer               :: shift, ifilt
      real                  :: td_filt(fft_length)
      complex               :: fd_filt(nw)
!------------------------------------------------------------
! Compute the shift needed to put all filter lag times at front of td_filt.
! nt should always be odd so the shift should be (nt/2)-1. This equation
! makes allowances for nt being even.
      shift = (nt+1)/2 - 1

! Loop to build an array of filters
      do ifilt = 1, nfilt

        call bandps ( fd_filt, nw, dw, filt_type(ifilt), &
                      f1(ifilt), f2(ifilt), f3(ifilt), f4(ifilt), &
                      phase(ifilt), 1./fft_length )

        call fft_cr_transform(fft_object,fd_filt,td_filt)

        ! Time=0 for filter is at sample 1, negative times are wrapped
        ! around at the end. Simply shift the filter vector to get all
        ! neg. and pos. filter lag times at the begining of td_filt() vector.
        td_filt = cshift(td_filt,-shift)

        ! filters are stored in reverse order because they are applied with
        ! a simple correlation.
        filt_array(nt:1:-1,ifilt) = td_filt(1:nt)

      enddo
      return
      end subroutine tvf_build_filter_array


!*******************************************************************************
! Mix B into A from index n1 to n2, with transition region
! of length 2*lovlph centered at nt.  It is assumed that the
! low end of the transition zone is not greater than n1.
!*******************************************************************************
      subroutine tvf_mix(A, B, n1, nt, n2, lovlph, rinc)
      implicit none

      integer, intent(in)    :: n1,n2,nt, lovlph   ! Arguments
      real,    intent(in)    :: rinc               ! Arguments
      real,    intent(inout) :: A(:)               ! Arguments
      real,    intent(in)    :: B(:)               ! Arguments

      integer           :: j1, ntend  ! Local variables

      real, allocatable :: fac1u(:)           ! Local variables

!-----------------------------------------------

      ntend = min(nt+lovlph, n2)

! Allocate and build vector of transition weights
      allocate (fac1u(ntend-n1+1))
      fac1u = (/(j1, j1 = n1-nt, ntend-nt)/)*rinc + 0.5

! Mix B into A in the transition zone
      A(n1:ntend) = A(n1:ntend)*(1.-fac1u) + B(n1:ntend)*fac1u

! Copy B into A everywhere else
      A(ntend+1:n2) = B(ntend+1:n2)

! Reclaim allocated space
      deallocate (fac1u)

! All Done
      return
      end subroutine tvf_mix

!</execute_only>


!*******************************************************************************
! Subroutine to handle all the GUI sensitivity settings during update.
!*******************************************************************************
      subroutine tvf_set_sensitivities(obj)
      implicit none
      type(tvf_struct) :: obj       ! arguments
!----------------------------------------------------------------

      ! These are the sensitivity settings at initializaion
      call pc_put_sensitive_field_flag ('OPT_MODE',       .true. )
      call pc_put_sensitive_field_flag ('HDR_FLAG',       .true. )
      call pc_put_sensitive_field_flag ('LEN_OP',         .true. )
      call pc_put_sensitive_array_flag ('FREQ_LOW_NONE',  .true. )
      call pc_put_sensitive_array_flag ('FREQ_LOW_FULL',  .true. )
      call pc_put_sensitive_array_flag ('FREQ_HIGH_FULL', .true. )
      call pc_put_sensitive_array_flag ('FREQ_HIGH_NONE', .true. )
      call pc_put_sensitive_array_flag ('PHASE',          .true. )
      call pc_put_sensitive_array_flag ('TIMES',          .true. )
      call pc_put_sensitive_array_flag ('LEN_ZONE',       .true. )

      ! Handle variations that come with different opt_mode values
      ! select case (obj%opt_mode)

        ! Nothing to do here... yet.

      ! end select

      return
      end subroutine tvf_set_sensitivities


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine tvf_wrapup (obj)
      implicit none
      type(tvf_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine tvf_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tvf_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
