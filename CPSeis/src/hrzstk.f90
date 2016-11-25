!<CPS_v1 type="PROCESS"/>
!!------------------------------- hrzstk.f90 ---------------------------------!!
!!------------------------------- hrzstk.f90 ---------------------------------!!
!!------------------------------- hrzstk.f90 ---------------------------------!!


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
! Name       : HRZSTK
! Category   : stacks
! Written    : 2003-07-21   by: SMCook
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : To align and stack reflections such as waterbottom events. 
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! HRZSTK aligns and stacks windows of traces.  The alignment is defined and
! implemented on a subsample basis.
!
! A "seed time" is provided either via a header word, or by HRZSTK itself using
! the LOCAL_LAV_THRESHOLD option.  In USE_SEED_AS_IS mode, HRZSTK takes this
! seed time literally to define the alignment of traces for stacking (the
! seed time can be coincident with a time sample, but it doesn't need to be).
!
! In AUTOMATIC mode, a suite of hardwired "local lav" thresholds are used, and
! after all traces are read in, a recommended best stack is output based on
! the most consistent result throughout the suite of trials.  Specifically, the
! recommended "wavelet" is the stack at a given threshold that is most similar
! to the "mode" of all trials (measured by a median stack of all attempts at
! all trial thresholds).  Put still another way, it's the stack that turns out
! to be the most similar to the "most common signature" encountered.
! 
! In ALIGN_PEAKS mode, the program "snaps" to the peak most proximal to the
! seed time to define the alignment (ALIGN_TROUGHS works in an analogous
! manner).  Cubic spline temporal interpolation is used to determine the
! precise location of the maximum/minimum.
!
! Finally, using the original seed time or the modified (snapped) time as an
! anchor point, cubic spline interpolation is used to generate a subtrace for
! stacking.  The SEC_ABOVE and SEC_BELOW parameters define how far above and
! below the anchor point is the interpolation and stacking window.
!
! In AUTOMATIC mode, HRZSTK outputs 10 traces containing what it believes to be
! the best wavelet, followed by groups of traces that are the input traces
! aligned at 19 different test thresholds -- .05, .10, .15, ... .95 (some traces
! may be skipped -- do not expect a predictable, full count in all cases):
!
! In the other modes, HRZSTK outputs a trace file containing the 5 stacked
! traces, 5 median-stacked traces, and the individual traces that contributed.
! It also prints an ASCII stacked trace and an ASCII median-stacked trace to the
! report file.
!
! AUTOMATIC mode:
! <img src="hrzstk_automatic.jpg" alt="hrzstk_automatic.jpg">
!
! Other modes:
! <img src="hrzstk_nonautomatic.jpg" alt="hrzstk_nonautomatic.jpg">
!
! DSIG can use these files directly (it takes the first trace).
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! When using AUTOMATIC or LOCAL_LAV_THRESHOLD modes, be sure that your choices
! are optimized to match your data -- if the peak is strongest, you should
! choose ALIGN_PEAKS.
!
! HRZSTK is designed for accuracy and simplicity, not speed.  Although the
! process is not memory intensive and can handle "any" number of traces, it is
! really geared toward processing a few thousand input traces, not hundreds of
! thousands.  To this end, please note that LMRKIN has a random trace selection
! option, and is perhaps the 'archetypical' trace supplier for HRZSTK.
!
! The most common use of HRZSTK is likely to be for stacking waterbottom
! reflections in deepwater environments for use as far-field source signature
! estimates.  In this case, the user will typically want to choose ALIGN_PEAKS
! mode, and then choose the SOURCE_OF_SEED based on the circumstances.
! If the waterbottom is easily and consistently autopicked, employing the
! approach of LOCAL_LAV_THRESHOLD is normally easy and more than adequate.  If
! the autopicking can't be trusted, you can instead use an interpreted Landmark
! waterbottom horizon imported into CPS via LMRKHRZ in HRZ_TO_HDR mode.  In
! this case, you can make the pick as "perfect" as you care to.  Very often,
! a pick like this will already exist.
!
! When using a HDR_SEED value, be sure it has units of seconds (i.e. use a
! value of .001 for HDR_FACTOR in LMRKHRZ, if that's where the pick was
! imported from, since Landmark horizons are stored in milliseconds).
!
! Using an interpreted horizon is potentially "safer", but may be overkill.  A
! good stack doesn't require an absolutely perfect seed.  The main thing to
! avoid is SYSTEMATIC picking error, wherein (for example) a precursor
! waterbottom side lobe could be inadvertently autopicked "too often",
! introducing a systematic error in the stack.  It is better in this case to
! raise the threshold, causing the stray picks to occur in the deep data
! instead, where they'll be more random, and hence more effectively removed by
! stacking.
!
! Using an interpreted horizon also allows you to use HRZSTK as a means of
! stacking top salt or other events, for whatever reason.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process does not alter input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! TSTRT     starting time on trace                  Used but not changed.
! DT        trace sample interval                   Used but not changed.
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
!  1      HDR_SEQUENCE               Output file
!  2      HDR_TOP_MUTE               Used for hanging windows.
!  30     HDR_SCRATCH_30             Used internally to categorize traces.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!004. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!003. 2006-01-10  B. Menger  Removed Unused Variables.
!  2. 2004-12-15  SMCook     Added AUTOMATIC run mode, thereby changing the gui,
!                             the doc, and some logic portions significantly.
!                            Also is first-ever illustrated doc submittal.
!  1. 2003-08-06  SMCook     Initial version.
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
! See also the hrzpck.f90 primitive.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! A 10-trace gap is left at the beginning of the output trace file.  Upon
! successful completion in AUTOMATIC mode, it will eventually hold the wavelet
! estimate, repeated 10 times.  In the other modes, it will hold 5 (identical)
! stacked waveforms and 5 (identical) median-stacked waveforms.
!
! Scratch header 30 is used to group traces picked at different thresholds.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS HRZSTK Process/NC=80>
!
!                  [/C] HRZSTK -- HoRiZon-based STacK
!
!  RUN_MODE~~~~~~~~=`CCCCCCCCCCCCCCCC
!  ALIGNMENT_MODE~~=`CCCCCCCCCCCCC
!
!  SHALLOW_TIM~~=`FFFFF
!
!  SEC_ABOVE~~~~=`FFFFF
!  SEC_BELOW~~~~=`FFFFF
!
!  WAVEFORMS_OUT=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! `- Automatic / Local LAV ----------------------------------------------------
!
!   LOCAL_LAV_THRESHOLD~~=`FFFF
!   LOCAL_LAV_WIN_LEN~~~~=`FFFF
! `----------------------------------------------------------------------------
!
! `- Seed From Header ---------------------------------------------------------
!
!  HDR_SEED=`IIII
! `----------------------------------------------------------------------------
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="RUN_MODE">
!<Tip> Picks passed to HRZSTK via a header word or picked by HRZSTK. </Tip>
! Default = AUTOMATIC
! Allowed = AUTOMATIC, LOCAL_LAV_THRESHOLD, HDR_CONTAINS_SEED_TIME
! AUTOMATIC - runs a suite of thresholds, and makes a wavelet decision for you
! LOCAL_LAV_THRESHOLD - seed is based on a single threshold you select
! HDR_CONTAINS_SEED_TIME - seed is an interpreter's time horizon (for example)
!</Help>
!
!<Help KEYWORD="ALIGNMENT_MODE">
!<Tip> Defines how you want to align the traces before stacking them. </Tip>
! Default = ALIGN_PEAKS
! Allowed = ALIGN_PEAKS, ALIGN_TROUGHS, USE_SEED_AS_IS
! HRZSTK can "snap" to the peak or trough nearest your seed time, and use this
! highly accurate interpolated peak or trough time to align the traces for
! stacking.  In HDR_SEED mode, it is also possible to take the seed as the final
! definition of the stack alignment, i.e. the snapping step can be bypassed.
! This is provided to accommodate circumstances such as if you desired to stack
! an event based on its envelope amplitude peak rather than its "real" peak, in
! which case you could supply your own precise alignment definition.
!</Help>
!
!<Help KEYWORD="HDR_SEED">
!<Tip> If a header word is supplying the seed pick, specify it here. </Tip>
! Default = 48
! Allowed = 1 thru NWIH
! This applies if your input came from an interpreted pick, etc. 
!</Help>
!
!<Help KEYWORD="LOCAL_LAV_THRESHOLD">
!<Tip> If SOURCE_OF_SEED is LOCAL_LAV_THESHOLD, specify percent here. </Tip>
! Default = .4
! Allowed = .01 thru .99
! Success requires a good value, perhaps one based on a LMRKHRZ THRESHOLD_TEST.
!</Help>
!
!<Help KEYWORD="LOCAL_LAV_WIN_LEN">
!<Tip> Window length in seconds if LOCAL_LAV_THRESHOLD applies. </Tip>
! Default = .4
! Allowed = .1 thru 1.6
! Success requires a good value, perhaps one based on a LMRKHRZ THRESHOLD_TEST.
!</Help>
!
!<Help KEYWORD="SHALLOW_TIM">
!<Tip> Reject horizon values shallower than this. </Tip>
! Default = .8
! Allowed = .6 to 2.0
! The default of .8 is based on experience with stacking waterbottom
! reflections in deepwater environments, and is already generous.  Don't push
! it shallower without a very good reason (if that's what you're using HRZSTK
! for in the first place).
!</Help>
!
!<Help KEYWORD="SEC_ABOVE">
!<Tip> Time in seconds above peak amplitude to stack/retain. </Tip>
! Default = .2
! Allowed = .1 to 1.0
! SEC_ABOVE and SEC_BELOW together define the length of the output stack.
!</Help>
!
!<Help KEYWORD="SEC_BELOW">
!<Tip> Time in seconds below peak amplitude to stack/retain. </Tip>
! Default = .6
! Allowed = .1 to 1.0
! SEC_ABOVE and SEC_BELOW together define the length of the output stack.
!</Help>
!
!<Help KEYWORD="WAVEFORMS_OUT">
!<Tip> HRZSTK outputs a .trc file.  Specify the name of it here. </Tip>
! Default =
! Allowed = Writable trace file name.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module hrzstk_module
      use cio_module
      use hrzpck_module
      use pathcheck_module
      use pathchoose_module
      use pc_module
      use lav_module
      use median_module
      use named_constants_module
      use trcio_module


      implicit none
      private
      public :: hrzstk_create
      public :: hrzstk_initialize
      public :: hrzstk_update
      public :: hrzstk_delete
      public :: hrzstk            ! main trace processing routine.
      public :: hrzstk_wrapup

      character(len=100),public,save :: HRZSTK_IDENT = &
'$Id: hrzstk.f90,v 1.4 2006/10/17 13:45:44 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: hrzstk_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        real                       :: tstrt   ! time of 1st trace sample (sec).
        real                       :: dt      ! trace sample interval (sec).

        character(len=30)              :: run_mode              ! process prm
        character(len=30)              :: alignment_mode        !
        character(len=30)              :: source_of_seed        !
        integer                        :: hdr_seed              !
        real                           :: local_lav_threshold   !
        real                           :: local_lav_win_len     !

        real                           :: shallow_tim           !
        real                           :: sec_above             !
        real                           :: sec_below             !
        character(len=FILENAME_LENGTH) :: waveforms_out         !

        real, pointer              :: wavefinal(:)           ! other
        real, pointer              :: wavenormal(:)          !
        real, pointer              :: wavemedian(:)          !
        integer                    :: alignment              !
        real                       :: minamp, maxamp         !
        integer                    :: nup, ndown, nsmp       !

        integer                    :: errcount1, errcount2   !
        integer                    :: stack_count            !

        type(pathchoose_struct),pointer :: dialog_out
        type(trcio_struct),pointer      :: trc_out

      end type hrzstk_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(hrzstk_struct),pointer,save :: object  ! needed for traps.


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine hrzstk_create (obj)
      type(hrzstk_struct),pointer :: obj       ! arguments
      integer                   :: ier      ! for error checking

      lunprint = pc_get_lun()
      allocate (obj, stat=ier)
      if (ier /= 0) call pc_error ("Unable to allocate obj in hrzstk_create")

      nullify (obj%wavefinal) ! jpa
      nullify (obj%wavenormal) ! jpa
      nullify (obj%wavemedian) ! jpa
      nullify (obj%dialog_out) ! jpa
      nullify (obj%trc_out) ! jpa

      call pathchoose_create(obj%dialog_out, 'WAVEFORMS_OUT', 'trc')

      call hrzstk_initialize (obj)
      end subroutine hrzstk_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine hrzstk_delete (obj)
      type(hrzstk_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call hrzstk_wrapup (obj)

      call pathchoose_delete (obj%dialog_out)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning("error deallocating obj in hrzstk_delete")
      end subroutine hrzstk_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine hrzstk_initialize (obj)
      type(hrzstk_struct),intent(inout) :: obj       ! arguments

      obj%run_mode            = 'AUTOMATIC'
      obj%alignment_mode      = 'ALIGN_PEAKS'
      obj%hdr_seed            = 48
      obj%local_lav_threshold = .4
      obj%local_lav_win_len   = .4

      obj%shallow_tim         = .8
      obj%sec_above           = .2
      obj%sec_below           = .6
      obj%waveforms_out       = ' '

      nullify(obj%wavefinal)
      nullify(obj%wavenormal)
      nullify(obj%wavemedian)

      obj%errcount1 = 0
      obj%errcount2 = 0

      obj%stack_count = 0

      call hrzstk_update (obj)
      end subroutine hrzstk_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine hrzstk_update (obj)
      type(hrzstk_struct),intent(inout),target :: obj             ! arguments

      integer          :: istat                                   ! local


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if(pathchoose_update(obj%dialog_out,obj%waveforms_out)) return

      call pc_get_global ('tstrt' , obj%tstrt)
      call pc_get_global ('dt'    , obj%dt)

      call pc_get('RUN_MODE'           , obj%run_mode)
      call pc_get('ALIGNMENT_MODE'     , obj%alignment_mode)
      call pc_get('HDR_SEED'           , obj%hdr_seed)
      call pc_get('LOCAL_LAV_THRESHOLD', obj%local_lav_threshold)
      call pc_get('LOCAL_LAV_WIN_LEN'  , obj%local_lav_win_len)

      call pc_get('SHALLOW_TIM'  , obj%shallow_tim)
      call pc_get('SEC_ABOVE'    , obj%sec_above)
      call pc_get('SEC_BELOW'    , obj%sec_below)
      call pc_get('WAVEFORMS_OUT', obj%waveforms_out, hrzstk_waveforms_out_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if     (obj%alignment_mode == 'ALIGN_PEAKS') then
        obj%alignment = HRZPCK_ALIGN_PEAKS
      else if(obj%alignment_mode == 'ALIGN_TROUGHS') then
        obj%alignment = HRZPCK_ALIGN_TROUGHS
      else if(obj%alignment_mode == 'USE_SEED_AS_IS') then
        obj%alignment = HRZPCK_USE_SEED_AS_IS
      else
        call pc_error('HRZSTK: unknown alignment_mode')
        return
      end if

      if(pc_verify_screen('screen1')) call hrzstk_screen_trap()


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field('RUN_MODE' ,      &
                  (/'AUTOMATIC             ',     &
                    'LOCAL_LAV_THRESHOLD   ',     &
                    'HDR_CONTAINS_SEED_TIME'/) )

      call pc_put_options_field('ALIGNMENT_MODE', &
                  (/'ALIGN_PEAKS   ',             &
                    'ALIGN_TROUGHS ',             &
                    'USE_SEED_AS_IS'/) )

      call pc_put_global ('tstrt', obj%tstrt)
      call pc_put_global ('dt'   , obj%dt)

      call pc_put('RUN_MODE'           , obj%run_mode)
      call pc_put('ALIGNMENT_MODE'     , obj%alignment_mode)
      call pc_put('HDR_SEED'           , obj%hdr_seed)
      call pc_put('LOCAL_LAV_THRESHOLD', obj%local_lav_threshold)
      call pc_put('LOCAL_LAV_WIN_LEN'  , obj%local_lav_win_len)

      call pc_put('SHALLOW_TIM'  , obj%shallow_tim)
      call pc_put('SEC_ABOVE'    , obj%sec_above)
      call pc_put('SEC_BELOW'    , obj%sec_below)
      call pc_put('WAVEFORMS_OUT', obj%waveforms_out)


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


      if     (obj%run_mode=='AUTOMATIC') then
        call pc_put_sensitive_field_flag('HDR_SEED'           , .false.)
        call pc_put_sensitive_field_flag('LOCAL_LAV_THRESHOLD', .false.)
        call pc_put_sensitive_field_flag('LOCAL_LAV_WIN_LEN'  , .true.)
      else if(obj%run_mode=='LOCAL_LAV_THRESHOLD') then
        call pc_put_sensitive_field_flag('HDR_SEED'           , .false.)
        call pc_put_sensitive_field_flag('LOCAL_LAV_THRESHOLD', .true.)
        call pc_put_sensitive_field_flag('LOCAL_LAV_WIN_LEN'  , .true.)
      else if(obj%run_mode=='HDR_CONTAINS_SEED_TIME') then
        call pc_put_sensitive_field_flag('HDR_SEED'           , .true.)
        call pc_put_sensitive_field_flag('LOCAL_LAV_THRESHOLD', .false.)
        call pc_put_sensitive_field_flag('LOCAL_LAV_WIN_LEN'  , .false.)
      else
        call pc_error('HRZSTK: Error')
        return
      end if


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%nup   = nint(obj%sec_above / obj%dt)
      obj%ndown = nint(obj%sec_below / obj%dt)
      obj%nsmp  = obj%nup + 1 + obj%ndown

      allocate(obj%wavefinal(obj%nsmp))
      allocate(obj%wavenormal(obj%nsmp))
      allocate(obj%wavemedian(obj%nsmp))

!---- not needed until later, but better to catch trouble early?
      istat = cio_set_file_ext_size(50000000)
      if(istat /= cio_ok) then
        call pc_error('HRZSTK: Error setting file extent size.')
      end if
      call cio_set_file_space_commit(1)    ! reserve disk space to avoid abort

      call pc_print('HRZSTK: opening ' // trim(obj%waveforms_out) // ' ...')

!---- truncate manually because cio w+ mode isn't really w+ mode
      obj%trc_out => trcio_open(                                   &
        trim(obj%waveforms_out), 'w', srate=obj%dt,                &
        nwih=HDR_NOMINAL_SIZE, ndpt=obj%nsmp, nbits=32, nbitshd=64 )
      istat = trcio_close(obj%trc_out)
      if(istat /= trcio_ok) then
        call pc_error('HRZSTK: trcio_close istat = ', istat)
        return
      end if

!---- here's the real open call
      obj%trc_out => trcio_open(                                   &
        trim(obj%waveforms_out), 'w+', srate=obj%dt,               &
        nwih=HDR_NOMINAL_SIZE, ndpt=obj%nsmp, nbits=32, nbitshd=64 )
      call pc_print('HRZSTK: ... successful open ...')


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine hrzstk_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
      subroutine hrzstk_screen_trap()
      implicit none


      if(object%sec_above < 0.1 .or. object%sec_above > 1.0) then
        call pc_error('HRZSTK: Parameter SEC_ABOVE is out of bounds.')
      end if

      if(object%sec_below < 0.1 .or. object%sec_below > 1.0) then
        call pc_error('HRZSTK: Parameter SEC_BELOW is out of bounds.')
      end if

      end subroutine hrzstk_screen_trap


      subroutine hrzstk_waveforms_out_trap()
      implicit none

      call pathcheck('WAVEFORMS_OUT', object%waveforms_out,   &
             ext='trc', required=.true., show=PATHCHECK_INFO_OUTPUT)

      end subroutine hrzstk_waveforms_out_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
      subroutine hrzstk (obj,ntr,hd,tr)
      type(hrzstk_struct),intent(inout) :: obj                  ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer            :: i                     ! local




      if(obj%run_mode=='AUTOMATIC') then
        do i=1,19
          if(obj%alignment_mode=='ALIGN_PEAKS') then
            call hrzstk_calc(obj,ntr,hd,tr, i/20.)
          else
            call hrzstk_calc(obj,ntr,hd,tr,-i/20.)
          end if
        end do
      else
        call hrzstk_calc(obj,ntr,hd,tr,obj%local_lav_threshold)
      end if

      end subroutine hrzstk


!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
      subroutine hrzstk_calc (obj,ntr,hd,tr,threshold)
      type(hrzstk_struct),intent(inout) :: obj                  ! arguments
      integer          ,intent(inout)   :: ntr                  !
      double precision ,intent(in)      :: hd(:,:)              !
      real             ,intent(in)      :: tr(:,:)              !
      real             ,intent(in)      :: threshold            !

      integer            :: i, itop, ibot, index, istat         ! local
      real               :: seed_time, mintest, maxtest         !
      real, allocatable  :: time(:), amp(:)                     !
      double precision   :: hd_local(HDR_NOMINAL_SIZE)          !


      allocate(time(obj%nsmp), amp(obj%nsmp))

      do i=1,ntr

!---- obtain a "seed time"
        if(obj%source_of_seed == 'HDR_CONTAINS_SEED_TIME') then
          seed_time = hd(obj%hdr_seed, i)
          if(seed_time == 1.e37) cycle     ! bypass nulls
          if(seed_time == 0) cycle         ! zeroes are unlikely to be legit
        else
          itop = hd(HDR_TOP_MUTE, i)
          ibot = itop + obj%local_lav_win_len/obj%dt
          call hrzpck_hot_proximal_sample (                    &
            tr(:,i), itop, ibot, threshold, index)
          if(index < 0) cycle
          seed_time = obj%tstrt + (index-1)*obj%dt
        end if

!---- honor shallow limit
        if(seed_time < obj%shallow_tim) cycle

!---- use it as is, or refine it (snap to minima/maxima)
        call hrzpck_proximal_spline (                             &
               tr(:,i), obj%tstrt, obj%dt, seed_time,             &
               obj%alignment, obj%nup, obj%ndown, time, amp, istat)

        if(istat /= 0) then
          if(istat == 1) obj%errcount1 = obj%errcount1 + 1
          if(istat == 2) obj%errcount2 = obj%errcount2 + 1
          cycle
        end if

!---- write individual contributing trace
        mintest = minval(amp)
        maxtest = maxval(amp)
        if(mintest < obj%minamp) obj%minamp = mintest
        if(maxtest > obj%maxamp) obj%maxamp = maxtest

        obj%stack_count = obj%stack_count + 1
        hd_local = 0.0D0
        hd_local(HDR_SEQUENCE) = obj%stack_count + 10 
        hd_local(HDR_SCRATCH_30) = nint(abs(threshold)*20)
        istat = trcio_write_trace(obj%trc_out, hd_local,      &
                        amp, tnum=nint(hd_local(HDR_SEQUENCE)))
        if(istat /= trcio_ok) then
          call pc_error('HRZSTK: trcio_write_trace istat = ', istat)
          return
        end if

      end do

      deallocate(time, amp)

      end subroutine hrzstk_calc


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine hrzstk_wrapup (obj)
      type(hrzstk_struct),intent(inout) :: obj               ! arguments

      integer               :: i, istat                      ! local
      double precision      :: hd_local(HDR_NOMINAL_SIZE)    !
      real                  :: tr_local(obj%nsmp)            !
      real                  :: fterm                         ! 

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print('HRZSTK:')
      call pc_print('HRZSTK: FYI: spline error count #1 = ', obj%errcount1)
      call pc_print('HRZSTK: FYI: spline error count #2 = ', obj%errcount2)
      call pc_print('HRZSTK:')

!---- compute the stats
      call hrzstk_compute(obj)

!---- write beginning traces based on run_mode
      if(obj%run_mode=='AUTOMATIC') then

!------ 10 identical
        call pc_print('HRZSTK: writing normal stacked traces')
        do i=1,10
          hd_local = 0.0D0
          hd_local(HDR_SEQUENCE) = i
          if(i == 1) hd_local(HDR_FOLD) = 1
          fterm = lav(obj%wavefinal, obj%nsmp)
          hd_local(HDR_LAV) = fterm
          istat = trcio_write_trace(obj%trc_out, hd_local,     &
               obj%wavefinal, tnum=nint(hd_local(HDR_SEQUENCE)))
          if(istat /= trcio_ok) then
            call pc_error('HRZSTK: trcio_write_trace fin istat = ', istat)
            return
          end if
        end do

      else

!------ write 5 (identical) stacked traces (already normalized)
        call pc_print('HRZSTK: writing normal stacked traces')
        do i=1,5
          hd_local = 0.0D0
          hd_local(HDR_SEQUENCE) = i
          if(i == 1) hd_local(HDR_FOLD) = 1
          fterm = lav(obj%wavenormal, obj%nsmp)
          hd_local(HDR_LAV) = fterm
          istat = trcio_write_trace(obj%trc_out, hd_local,      &
               obj%wavenormal, tnum=nint(hd_local(HDR_SEQUENCE)))
          if(istat /= trcio_ok) then
            call pc_error('HRZSTK: trcio_write_trace reg istat = ', istat)
            return
          end if
        end do

!------ write 5 (identical) median-stacked traces (already normalized)
        call pc_print('HRZSTK: writing median stacked traces')
        do i=6,10
          hd_local = 0.0D0
          hd_local(HDR_SEQUENCE) = i
          fterm = lav(obj%wavemedian, obj%nsmp)
          hd_local(HDR_LAV) = fterm
          istat = trcio_write_trace(obj%trc_out, hd_local,      &
               obj%wavemedian, tnum=nint(hd_local(HDR_SEQUENCE)))
          if(istat /= trcio_ok) then
            call pc_error('HRZSTK: trcio_write_trace med istat = ', istat)
            return
          end if
        end do

      end if

!---- now that all traces are in the file, overwrite it with normalized traces
      call pc_print('HRZSTK: normalizing file based on its hottest sample')
      do i=11,obj%stack_count + 10
        istat = trcio_read_trace(obj%trc_out, hd_local, tr_local, tnum=i) 
        if(istat /= trcio_ok) then
          call pc_error('HRZSTK: trcio_read_trace reread istat = ', istat)
          return
        end if

        if(abs(obj%minamp) > abs(obj%maxamp)) then
          tr_local =   tr_local / obj%maxamp
        else
          tr_local = - tr_local / obj%minamp
        end if

        fterm = lav(tr_local, obj%nsmp)
        hd_local(HDR_LAV) = fterm
        istat = trcio_write_trace(obj%trc_out, hd_local, tr_local, tnum=i)
        if(istat /= trcio_ok) then
          call pc_error('HRZSTK: trcio_write_trace final istat = ', istat)
          return
        end if

      end do

!---- close the trace file
      istat = trcio_close(obj%trc_out)
      if(istat /= trcio_ok) then
        call pc_error('HRZSTK: trcio_close istat = ', istat)
        return
      end if

!---- finally, print ASCII versions to report file
      if(obj%run_mode=='AUTOMATIC') then

        call pc_print('')
        call pc_print('HRZSTK: ASCII final waveform follows:')
        do i=1,obj%nsmp
          call pc_print('   ', obj%wavefinal(i))
        end do

      else

        call pc_print('')
        call pc_print('HRZSTK: ASCII stacked waveform follows:')
        do i=1,obj%nsmp
          call pc_print('   ', obj%wavenormal(i))
        end do

        call pc_print('')
        call pc_print('HRZSTK: ASCII median-stacked waveform follows:')
        do i=1,obj%nsmp
          call pc_print('   ', obj%wavemedian(i))
        end do

        if(associated(obj%wavefinal))  deallocate(obj%wavefinal)
        if(associated(obj%wavenormal)) deallocate(obj%wavenormal)
        if(associated(obj%wavemedian)) deallocate(obj%wavemedian)

      end if

      end subroutine hrzstk_wrapup


!!----------------------------- stack routine ------------------------------!!
!!----------------------------- stack routine ------------------------------!!
!!----------------------------- stack routine ------------------------------!!

      subroutine hrzstk_compute (obj)
      type(hrzstk_struct),intent(inout) :: obj             ! arguments

      integer            :: i, istat, n, nbest             ! local
      double precision   :: hd_local(HDR_NOMINAL_SIZE)     !
      real               :: tr_local(obj%nsmp)             !
      real               :: amp(obj%nsmp)                  !
      real               :: med, max, min, corr            !
      real               :: max_corr, asum, bsum           !
      real               :: ftmp(obj%stack_count - 10)     !

!---- preparations
      obj%wavefinal = 0.0
      obj%wavenormal = 0.0
      obj%wavemedian = 0.0

      hd_local = 0.0D0
      tr_local = 0.0
      ftmp     = 0.0

!---- median stack (simple, slow logic re-reads the file)
      call pc_print('HRZSTK: doing median stack...')

      do n=1,obj%nsmp
        do i=11,obj%stack_count
          istat = trcio_read_trace(obj%trc_out, hd_local, tr_local, tnum=i) 
          if(istat /= trcio_ok) then
            call pc_error('HRZSTK: trcio_read_trace med istat = ', istat)
            return
          end if
          ftmp(i - 10) = tr_local(n)
        end do
        call median(ftmp, obj%stack_count - 10, med, obj%wavenormal(n))
        obj%wavemedian(n) = med
      end do
      max = maxval(obj%wavemedian)
      min = minval(obj%wavemedian)
      if(max < -min) max = -min
      if(max /= 0) obj%wavemedian = obj%wavemedian / max

!---- in automatic mode, do multiple stacks, compare to median stack
      if(obj%run_mode=='AUTOMATIC') then
        call pc_print('HRZSTK: analyzing traces, computing recommended stack')

        max_corr = 0.0
        do n=1,19

          amp = 0.0
          do i=11,obj%stack_count
            istat = trcio_read_trace(obj%trc_out, hd_local, tr_local, tnum=i) 
            if(istat /= trcio_ok) then
              call pc_error('HRZSTK: trcio_read_trace norm istat = ', istat)
              return
            end if
            if(hd_local(HDR_SCRATCH_30)==n) then
              amp = amp + tr_local
            end if
          end do

          corr = 0.0
          asum = 0.0
          bsum = 0.0
          do i=1,obj%nsmp
            corr = corr + amp(i)*obj%wavemedian(i);
            asum = asum + amp(i)*amp(i)
            bsum = bsum + obj%wavemedian(i)*obj%wavemedian(i)
          end do
          if(asum==0 .or. bsum==0) then
            call pc_error('HRZSTK: zero denom')
            return
          end if 
          corr = corr / sqrt(asum*bsum)
          if(abs(corr) > max_corr) then
            max_corr = abs(corr)
            nbest = n
          end if
          write(lunprint,*) 'HRZSTK: nbest, max_corr = ',nbest,', ',max_corr

        end do

        do i=11,obj%stack_count
          istat = trcio_read_trace(obj%trc_out, hd_local, tr_local, tnum=i) 
          if(istat /= trcio_ok) then
            call pc_error('HRZSTK: trcio_read_trace norm istat = ', istat)
            return
          end if
          if(nint(hd_local(HDR_SCRATCH_30))==nbest) then
            obj%wavefinal = obj%wavefinal + tr_local
          end if
        end do
        max = maxval(obj%wavefinal)
        min = minval(obj%wavefinal)
        if(max < -min) max = -min
        if(max /= 0) obj%wavefinal = obj%wavefinal / max

      else

!---- ordinary stack
        call pc_print('HRZSTK: doing traditional stack...')

        do i=11,obj%stack_count
          istat = trcio_read_trace(obj%trc_out, hd_local, tr_local, tnum=i) 
          if(istat /= trcio_ok) then
            call pc_error('HRZSTK: trcio_read_trace norm istat = ', istat)
            return
          end if
          obj%wavenormal = obj%wavenormal + tr_local
        end do
        max = maxval(obj%wavenormal)
        min = minval(obj%wavenormal)
        if(max < -min) max = -min
        if(max /= 0) obj%wavenormal = obj%wavenormal / max

      end if

      call pc_print('HRZSTK: hrzstk_compute is complete...')


      end subroutine hrzstk_compute


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module hrzstk_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

