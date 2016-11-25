!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-05-05. />

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
! Name       : FKTR   (Generate F-K Plane Traces)
! Category   : transforms
! Written    : 1989-01-12   by: Bill Troutt
! Revised    : 2001-01-04   by: Stephen Chiu
! Maturity   : production   2001-01-15
! Purpose    : Produce F-K amplitude traces for generating F-K color plots.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! FKTR is used to transform X-T domain traces to F-K traces for producing F-K
! color plots.  Inputs are usually shot profiles, receiver profiles, CMP
! gathers or segments of stacked or migrated sections.  Normally input panels
! comprise no more than a few hundred traces each, but the number of traces in
! an input panel could, in principle, be many thousands of traces.
!
! FKTR outputs FFT_X + 1 traces for each input panel, where FFT_X is the
! smallest possible spatial FFT size greater than TR_MAX.  (FFT size can be any
! product of whole number powers of 2, 3 or 5.)  FFT_T (the temporal FFT size)
! is the smallest possible FFT size greater than the input NDPT.
!
!
! Output Globals
!
! FKTR modifies the globals for the sample interval and number of samples
! (DT and NDPT) as follows:
!
!       DT(out)      = 1. / (DT(in)*FFT_T*HZPS)
!
!       NDPT(out)    = FREQ_MAX / (1./(DT(in)*FFT_T)) + 1
!
! These new values imply output_trace_length = [(NDPT(out)-1) * DT(out)].  You
! will find the new values for sample interval and trace length listed in the
! SETUP section of the .rpt file.  You will need them should you output the
! results of FKTR to disk or tape for later input by CPS. Note that TSTRT is
! always set to 0.0 on output.
!
!
! Output Trace Headers
!
! Since this process always outputs more traces than it inputs, headers for the
! additional traces must be created.  The headers for the last input trace in a
! panel are repeated as needed to provide the needed headers.  In addition,
! header words 2(mute), 3(gather), 4(tr# in gather) and 64(mute) are updated.
! Header word 6 is set to the "wavenumber" (range -FFT_X/2 to +FFT_X/2) for
! each output trace.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!
! CPS dip convention is that positive dips correspond to positive K
! (wavenumbers).  An event with positive dip increases in time with increasing
! trace number.  The sketches illustrate positive dip.
!
!         INPUT                     OUTPUT
!         -----                     ------
!       trace number ----->        "trace" number ----->
!                                         -Kn  -  - 0  +  +  +Kn
!       t    \                    "  Fo             \
!       i     \                   t   .              \
!       m      \                  i   .               \
!       e       \ (+)             m   .                \
!                \                e   .                 \
!       |         \               "  Fmax                \
!       |          \              |
!       V           \             V
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! This process requires traces to be input one at a time.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
!
! This process alters input traces.
!
! This process outputs one trace at a time.
!
! This is a trace-supplying process (after doing the F-K transform).
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
! GATHERED  whether traces are a legitimate gather  used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        changed
! TSTRT     starting time on trace                  set to 0.0
! DT        trace sample interval                   changed
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
! 2       Head mute                  Changed
! 3       Gather number              Changed
! 4       Trace number in gather     Changed
! 6                                  Set to K (wavenumber) value
! 64      Tail mute                  Changed
!

!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!
! 13. 2001-01-15 Chiu       Fix problem in FK spectrum.
! 12. 2000-12-08 Stoeckley  Change wrapup flag.
! 11. 2000-07-14 Coleman    Converted to new system.
! 10. 1998-12-18 Vunderink  Begin using the f90 compiler.
! 9.  1997-03-19 Vunderink  Increased memory used by X2KCC and T2FRC
!                           primitives
! 8.  1993-10-08 Troutt     Set HW6 to the "wavenumber" (-N-2 to N-2) on
!                           output.  Add to Note 3.
! 7.  1992-03-20 Troutt     Set HW64 to NDPT.
! 6.  1991-03-26 Troutt     Make sure that TSTRT global is 0.0 on output.
! 5.  1989-12-13 Troutt     Fix formatted write of FMAX for DCODE.
! 4.  1989-09-21 Troutt     Changed logic to use storage for working trace
!                           (and header) arrays instead of using  2ndary
!                           arrays in the FKTR call.  The 2ndary arrays
!                           were renamed to HDR2 and TR2 and are used to
!                           output 1 trace at a time via an internal call
!                           to GATHR.  All of the main logic for the old
!                           2ndary arrays (HDR1, TR1) remains intact,
!                           except that they now reside in storage via
!                           pointers.
! 3.  1989-07-12 Troutt     Reversed order of output "traces" to match dip
!                           convention in other CPS programs (negative
!                           wave numbers come out 1st).
! 2.  1989-03-08 Troutt     Fixed call to REPP to reference HZPS instead
!                           of NTRT.
! 1.  1989-01-12 Troutt     Original version (no transpose to disk).
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
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
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS fktr Process/NC=80>
!
!                          Generate F-K Plane Traces
!          Produce F-K amplitude traces for generating F-K color plots
!
!                           FREQ_MAX =~~~`FFFFFFFFF
!
!                           HZPS =~~~~~~~`FFFFFFFFF
!
!                           TR_MAX =~~~~~`IIIIIIIII
!
!                           HDR_PANEL =~~`IIIIIIIII
!
!                           PANEL_INIT = `FFFFFFFFF
!
!                           PANEL_INC =~~`FFFFFFFFF
!</gui_def>
!
!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency to use in the F-K transform. </Tip>
! Default = 0.5 * Nyquist
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="HZPS">
!<Tip> Frequency range, in Hz, for each second on output traces. </Tip>
! Default = 10.0
! Allowed = real > 0.0
! Frequency range on output traces is HZPS Hz for each second.  TSTRT for output
! traces is always 0.0 since traces start with zero frequency.
!</Help>
!
!<Help KEYWORD="HDR_PANEL">
!<Tip> Header word used to define input panels. </Tip>
! Default = 3
! Allowed = 1 - NWIH
! HDR_PANEL value should be a constant within an input panel and change between
! input panels.
!
! Normally HDR_PANEL should be set to 3 for shot gathers and CMPS, etc. and set
! to 8 for stacked and migrated sections.
!</Help>
!
!<Help KEYWORD="PANEL_INIT">
!<Tip> Value of HDR_PANEL for the first panel. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="PANEL_INC">
!<Tip> Increment of HDR_PANEL values between panels. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="TR_MAX">
!<Tip> Maximum number of traces in any input panel. </Tip>
! Default = -
! Allowed = int > 0
! TR_MAX is used for memory and disk management and not for defining an input
! panel.  Only the first TR_MAX traces of any input panel will be used.
!</Help>
!
!</HelpSection>

!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
! FKTR will not be able to call GATHR internally for option 4 since it is
! unavailable.  FKTR will have to keep track of input traces by itself (unless
! a primitive is written).



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module fktr_module

    use pc_module
    use named_constants_module
    use fft_module
    use mutehw_module
    use lav_module

    implicit none

    private
    public  :: fktr_create
    public  :: fktr_initialize
    public  :: fktr_update
    public  :: fktr_delete
    public  :: fktr_dump_object
!<execute_only>
    public  :: fktr            ! main execution (trace processing) routine.
    private :: fktr_apply_operator
    private :: fktr_put_trace
    private :: fktr_get_trace
    public  :: fktr_wrapup
!</execute_only>


    character(len=100),public,save :: fktr_IDENT = &
       '$Id: fktr.f90,v 1.13 2001/01/12 14:22:26 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    type,public :: fktr_struct

        private
        logical                     :: skip_wrapup      ! wrapup flag.
        real                        :: hzps             ! process parameter
        real                        :: freq_max         ! process parameter
        integer                     :: tr_max           ! process parameter
        integer                     :: hdr_panel        ! process parameter
        real                        :: panel_init       ! process parameter
        real                        :: panel_inc        ! process parameter
        integer                     :: nwih             ! global parameter
        integer                     :: ndpt             ! global parameter
        real                        :: dt               ! global parameter
        logical                     :: more             ! dependent variable
        logical                     :: inmode           ! dependent variable
        integer                     :: igrp             ! dependent variable
        integer                     :: ncount           ! dependent variable
        integer                     :: ntrout           ! dependent variable
        integer                     :: nout             ! dependent variable
        integer                     :: nseq             ! dependent variable
        integer                     :: nrcfftr          ! dependent variable
        integer                     :: nrcfftc          ! dependent variable
        integer                     :: nccfft           ! dependent variable
        integer                     :: nfkeep           ! dependent variable
        integer                     :: lu_print         ! dependent variable
        double precision, pointer   :: hdsav(:)         ! dependent variable
        real, pointer               :: trsav(:)         ! dependent variable
        double precision, pointer   :: hdbuf(:,:)       ! dependent variable
        complex, pointer            :: trbuf(:,:)       ! dependent variable
        type(fft_struct), pointer   :: frfft_obj        ! dependent variable
        type(fft_struct), pointer   :: fcfft_obj        ! dependent variable

    end type fktr_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine fktr_create( obj )
    implicit none
    type(fktr_struct),pointer :: obj

    allocate( obj )

    nullify( obj%hdsav     )
    nullify( obj%trsav     )
    nullify( obj%hdbuf     )
    nullify( obj%trbuf     )
    nullify( obj%frfft_obj )
    nullify( obj%fcfft_obj )

    call fktr_initialize( obj )

    return
end subroutine fktr_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine fktr_delete( obj )
    implicit none
    type(fktr_struct),pointer :: obj       ! arguments

!<execute_only>
    call fktr_wrapup( obj )
!</execute_only>

    if( associated( obj%hdsav ) ) deallocate( obj%hdsav )
    if( associated( obj%trsav ) ) deallocate( obj%trsav )
    if( associated( obj%hdbuf ) ) deallocate( obj%hdbuf )
    if( associated( obj%trbuf ) ) deallocate( obj%trbuf )

    if( associated( obj%frfft_obj ) ) call fft_delete( obj%frfft_obj )
    if( associated( obj%fcfft_obj ) ) call fft_delete( obj%fcfft_obj )

    deallocate(obj)

    return
end subroutine fktr_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine fktr_initialize( obj )
    implicit none
    type(fktr_struct),intent(inout) :: obj

    obj%lu_print = pc_get_lun()

    call pc_get_global( 'dt', obj%dt )
    obj%hzps       = 10.0
    obj%freq_max   =  0.25 / obj%dt   ! = 0.5 * fnyquist
    obj%tr_max     =  4
    obj%hdr_panel  =  3
    obj%panel_init =  1.0
    obj%panel_inc  =  1.0
    obj%dt         =  0.0
    obj%ndpt       =  0
    obj%nwih       =  0
    obj%igrp       =  0
    obj%ncount     =  0
    obj%nout       =  0
    obj%nseq       =  0
    obj%nrcfftr    =  0
    obj%nrcfftc    =  0
    obj%nccfft     =  0
    obj%nfkeep     =  0
    obj%more       = .true.
    obj%inmode     = .true.

    call fktr_update( obj )

    return
end subroutine fktr_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine fktr_update( obj )
    implicit none

    type(fktr_struct),intent(inout),target :: obj

!-------------------------------------------------------------
! LOCAL VARIABLES
!-------------------------------------------------------------
    real    :: dtout
    real    :: fnyq
    integer :: i, n2
    integer :: ierr1
    integer :: ierr2
    integer :: ierr3
    integer :: ierr4
    integer :: ierr5
!-------------------------------------------------------------------------------

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get_global( 'ndpt' , obj%ndpt  )
    call pc_get_global( 'nwih' , obj%nwih  )
    call pc_get_global( 'dt'   , obj%dt    )

    call pc_get( 'freq_max'  , obj%freq_max   )
    call pc_get( 'hzps'      , obj%hzps       )
    call pc_get( 'tr_max'    , obj%tr_max     )
    call pc_get( 'hdr_panel' , obj%hdr_panel  )
    call pc_get( 'panel_init', obj%panel_init )
    call pc_get( 'panel_inc' , obj%panel_inc  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


    if( obj%dt <= 0.0 ) then
        call pc_error( '*** FKTR: Invalid value for global dt: ', obj%dt )
        return
    endif

    if( obj%ndpt <= 0 ) then
        call pc_error( '*** FKTR: Invalid value for global ndpt: ', obj%ndpt )
        return
    endif

    if( obj%nwih <= 0 ) then
        call pc_error( '*** FKTR: Invalid value for global nwih: ', obj%nwih )
        return
    endif

    if( obj%hzps <= 0.0 ) then
        call pc_error( '*** FKTR: Invalid value for hzps: ', obj%hzps )
        obj%hzps = 10.0
    endif

    fnyq = 0.5 / obj%dt
    if( obj%freq_max <= 0.0 .or. obj%freq_max > fnyq ) then
        call pc_error( '*** FKTR: Invalid value for freq_max: ', obj%freq_max )
        obj%freq_max = 0.5 * fnyq
    endif

    if( obj%tr_max <= 0.0 ) then
        call pc_error( '*** FKTR: Invalid value for tr_max: ', obj%tr_max )
        obj%tr_max = 0
    endif

    if( obj%hdr_panel <= 0 .or. obj%hdr_panel > obj%nwih ) then
        call pc_error( '*** FKTR: Invalid value for hdr_panel: ', obj%hdr_panel)
        obj%hdr_panel = 3
    endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

!    obj%nrcfftr = fft_nfctr( obj%ndpt + mod(obj%ndpt,2) )

    obj%nrcfftr = (obj%ndpt + mod(obj%ndpt,2) )
    n2 = nint(log10(float(obj%nrcfftr))/log10(2.)+0.4999)
    obj%nrcfftr = 2**n2

    obj%nfkeep  = nint( obj%freq_max * obj%dt * obj%nrcfftr ) + 1
    dtout       = 1.0 / ( obj%dt * obj%nrcfftr * obj%hzps )

    call pc_put_global( 'dt'   , dtout      )
    call pc_put_global( 'ndpt' , obj%nfkeep )
    call pc_put_global( 'tstrt', 0.0        )

    call pc_put_control( 'need_request', .true. )
    call pc_put_control( 'need_label'  , .true. )
    call pc_put_control( 'twosets'     , .true. )

    call pc_put( 'hzps'      , obj%hzps       )
    call pc_put( 'freq_max'  , obj%freq_max   )
    call pc_put( 'tr_max'    , obj%tr_max     )
    call pc_put( 'hdr_panel' , obj%hdr_panel  )
    call pc_put( 'panel_init', obj%panel_init )
    call pc_put( 'panel_inc' , obj%panel_inc  )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    if( associated( obj%hdsav ) ) deallocate( obj%hdsav )
    if( associated( obj%trsav ) ) deallocate( obj%trsav )
    if( associated( obj%hdbuf ) ) deallocate( obj%hdbuf )
    if( associated( obj%trbuf ) ) deallocate( obj%trbuf )

    obj%igrp    = -HUGE(obj%igrp)
    obj%ncount  = 0
    obj%nout    = 0
    obj%nseq    = 0
    obj%nrcfftc = obj%nrcfftr / 2 + 1

!    obj%nccfft  = fft_nfctr( obj%tr_max )

    n2 = nint(log10(float(obj%tr_max))/log10(2.)+0.4999)
    obj%nccfft = 2**n2

    obj%ntrout  = obj%nccfft + 1 - mod(obj%nccfft,2)
    obj%more    = .true.
    obj%inmode  = .true.

!<execute_only>

    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

    allocate( obj%hdsav(obj%nwih)             , stat=ierr1 )
    allocate( obj%trsav(obj%nrcfftr)          , stat=ierr2 )
    allocate( obj%hdbuf(obj%nwih  ,obj%tr_max), stat=ierr3 )
    allocate( obj%trbuf(obj%nccfft,obj%nfkeep), stat=ierr4 )

    if( ierr1 /= 0 ) call pc_error( '*** FKTR: Error allocating HDBUF to ', &
                                    8 * obj%nwih, ' bytes' )
    if( ierr2 /= 0 ) call pc_error( '*** FKTR: Error allocating TRBUF to ', &
                                    4 * obj%nrcfftr, ' bytes' )
    if( ierr3 /= 0 ) call pc_error( '*** FKTR: Error allocating CHD to ',   &
                                    8 * obj%nwih * obj%tr_max, ' bytes' )
    if( ierr4 /= 0 ) call pc_error( '*** FKTR: Error allocating CTR to ',   &
                                    8 * obj%nrcfftc * obj%tr_max, ' bytes' )

    ierr1 = fft_create( obj%frfft_obj, -1, obj%nrcfftr, 'rtoc' )
    ierr2 = fft_create( obj%fcfft_obj, -1, obj%nccfft , 'ctoc' )

    if( ierr1 /= 0 .or. ierr2 /= 0 ) &
        call pc_error( '*** FKTR: Error creating fft object' )

    if( pc_do_not_process_traces() ) return   ! in case of allocation errors.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine fktr_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

subroutine fktr( obj, ntr, hdi, tri, hdo, tro )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fktr_struct),intent(inout) :: obj
    integer          ,intent(inout) :: ntr
    double precision ,intent(in)    :: hdi(:,:)
    real             ,intent(in)    :: tri(:,:)
    double precision ,intent(inout) :: hdo(:,:)
    real             ,intent(inout) :: tro(:,:)
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    complex, dimension(obj%nrcfftc) :: cv
    integer                         :: jgrp
!-------------------------------------------------------------------------------

    select case( ntr )

    case( 1 )
        ! do a validity check
        if( .not. obj%inmode ) then
            ntr = FATAL_ERROR
            call fktr_wrapup( obj )
            return
        endif

        if( obj%ncount == 0 .and. obj%nseq > 0 ) then
            call fktr_put_trace( obj, obj%hdsav, obj%trsav )
            obj%igrp = nint( ( obj%hdsav(obj%hdr_panel) - obj%panel_init ) / &
                             obj%panel_inc + 1.0 )
        endif

        obj%trsav(1:obj%ndpt)  = tri(:,1)
        obj%trsav(obj%ndpt+1:) = 0.0

        jgrp = nint( ( hdi(obj%hdr_panel,1) - obj%panel_init ) / &
                     obj%panel_inc + 1.0 )
        if( obj%ncount == 0 ) obj%igrp = jgrp

        if( jgrp == obj%igrp ) then
            call fktr_put_trace( obj, hdi(:,1), obj%trsav )
            ntr = NEED_TRACES
        else
            obj%hdsav  = hdi(:,1)
            obj%inmode = .false.
            obj%nout   = 0
            call fktr_apply_operator( obj )
            call fktr_get_trace( obj, ntr, hdo(:,1), tro(:,1) )
        endif

    case( NO_MORE_TRACES )
        ! do a validity check
        if( .not. obj%inmode ) then
            ntr = FATAL_ERROR
            call fktr_wrapup( obj )
            return
        endif

        obj%more   = .false.
        obj%inmode = .false.
        obj%nout   = 0
        call fktr_apply_operator( obj )
        call fktr_get_trace( obj, ntr, hdo(:,1), tro(:,1) )

    case( NEED_TRACES )
        ! do a validity check
        if( obj%inmode ) then
            ntr = FATAL_ERROR
            call fktr_wrapup( obj )
            return
        endif

        if( obj%nout < obj%ntrout ) then
            call fktr_get_trace( obj, ntr, hdo(:,1), tro(:,1) )
        else
            if( obj%more ) then
                obj%inmode = .true.
                obj%ncount = 0
                obj%nout   = 0
                ntr        = NEED_TRACES
            else
                ntr = NO_MORE_TRACES
                call fktr_wrapup( obj )
            endif
        endif

    case default
        ntr = FATAL_ERROR
        call fktr_wrapup( obj )

    end select

    return
end subroutine fktr


!</execute_only>


!!----------------------------- put trace ----------------------------------!!
!!----------------------------- put trace ----------------------------------!!
!!----------------------------- put trace ----------------------------------!!


!<execute_only>

subroutine fktr_put_trace( obj, hd, tr )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fktr_struct),intent(inout) :: obj
    double precision ,intent(in)    :: hd(:)
    real             ,intent(in)    :: tr(:)
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    complex, dimension(obj%nrcfftc) :: cv
!-------------------------------------------------------------------------------

    if( obj%ncount >= obj%tr_max ) return

    obj%ncount = obj%ncount + 1

    call fft_rc_transform( obj%frfft_obj, tr, cv )   ! tx -> fx

    obj%trbuf(obj%ncount,:) = cv(1:obj%nfkeep)       ! NOTE: trbuf is transposed
    obj%hdbuf(:,obj%ncount) = hd

    return
end subroutine fktr_put_trace

!</execute_only>


!!----------------------------- get trace ----------------------------------!!
!!----------------------------- get trace ----------------------------------!!
!!----------------------------- get trace ----------------------------------!!


!<execute_only>

subroutine fktr_get_trace( obj, ntr, hd, tr )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fktr_struct),intent(inout) :: obj
    integer          ,intent(inout) :: ntr
    double precision ,intent(inout) :: hd(:)
    real             ,intent(inout) :: tr(:)
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    integer                         :: i
!-------------------------------------------------------------------------------

    obj%nout = obj%nout + 1
    obj%nseq = obj%nseq + 1

    if( obj%nout <= obj%nccfft/2+1 ) then
        i = (obj%nccfft/2) + 2 - obj%nout 
    else
        i = obj%nccfft + obj%nccfft/2 + 2 - obj%nout     
    endif

    tr(1:obj%nfkeep) = cabs( obj%trbuf(i,:) )

    if( obj%nout <= obj%ncount ) then
        hd = obj%hdbuf(:,obj%nout)
    else
        hd = obj%hdbuf(:,obj%ncount)
    endif

    hd( 1) = obj%nseq
    hd( 2) = 1
    hd( 3) = obj%igrp
    hd( 4) = obj%nout
    hd( 6) = obj%nout - 1 - obj%nccfft/2
    hd(64) = obj%nfkeep

    call lav_set_hdr( hd, tr, obj%nfkeep )

    ntr    = 1

    return
end subroutine fktr_get_trace

!</execute_only>



!!--------------------------- apply operator -------------------------------!!
!!--------------------------- apply operator -------------------------------!!
!!--------------------------- apply operator -------------------------------!!


!<execute_only>

subroutine fktr_apply_operator( obj )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fktr_struct),intent(inout) :: obj
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    integer                         :: j
    real                            :: scale
!-------------------------------------------------------------------------------

    obj%trbuf(obj%ncount+1:obj%nccfft,:) = ( 0.0, 0.0 )

    do j = 1, obj%nfkeep
        call fft_cc_transform( obj%fcfft_obj, obj%trbuf(:,j) )    ! fx -> fk
    end do

    scale     = 1.0 / float( obj%nccfft * obj%nrcfftr )
    obj%trbuf = obj%trbuf * scale

    return
end subroutine fktr_apply_operator

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

subroutine fktr_wrapup( obj )
    implicit none
    type(fktr_struct),intent(inout) :: obj       ! arguments

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.


    return
end subroutine fktr_wrapup

!</execute_only>


!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!


subroutine fktr_dump_object( obj )
    implicit none

    type(fktr_struct),intent(inout) :: obj

    write( obj%lu_print, '( / " ***** DUMP OBJECT ***** " / )' )

    write( obj%lu_print, '( " skip_wrapup = ", l1       )' ) obj%skip_wrapup
    write( obj%lu_print, '( " freq_max    = ", f8.3     )' ) obj%freq_max
    write( obj%lu_print, '( " hzps        = ", f8.3     )' ) obj%hzps
    write( obj%lu_print, '( " tr_max      = ", i4       )' ) obj%tr_max
    write( obj%lu_print, '( " hdr_panel   = ", i4       )' ) obj%hdr_panel
    write( obj%lu_print, '( " panel_init  = ", f8.3     )' ) obj%panel_init
    write( obj%lu_print, '( " panel_inc   = ", f8.3     )' ) obj%panel_inc
    write( obj%lu_print, '( " nwih        = ", i4       )' ) obj%nwih
    write( obj%lu_print, '( " ndpt        = ", i4       )' ) obj%ndpt
    write( obj%lu_print, '( " dt          = ", f8.3     )' ) obj%dt
    write( obj%lu_print, '( " more        = ", l1       )' ) obj%more
    write( obj%lu_print, '( " inmode      = ", l1       )' ) obj%inmode
    write( obj%lu_print, '( " ncount      = ", i4       )' ) obj%ncount
    write( obj%lu_print, '( " nout        = ", i4       )' ) obj%nout
    write( obj%lu_print, '( " nseq        = ", i4       )' ) obj%nseq
    write( obj%lu_print, '( " nrcfftr     = ", i4       )' ) obj%nrcfftr
    write( obj%lu_print, '( " nrcfftc     = ", i4       )' ) obj%nrcfftc
    write( obj%lu_print, '( " nccfft      = ", i4       )' ) obj%nccfft
    write( obj%lu_print, '( " nfkeep      = ", i4       )' ) obj%nfkeep

    write( obj%lu_print, '( / " ***** END OF DUMP OBJECT ***** " / )' )

    return
end subroutine fktr_dump_object


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module fktr_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

