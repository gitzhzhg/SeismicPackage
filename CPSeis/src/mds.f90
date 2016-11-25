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
! Name       : MDS    (Median DeSpike)
! Category   : amplitude_mod
! Written    : 1998-05-20   by: Donna K. Vunderink & Chuck I. Burch
! Revised    : 2010-08-02   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Despike samples exceeding some factor times the local median.
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
! MDS attenuates noise bursts by clipping or zeroing samples whose absolute 
! amplitude exceed a threshold defined as a user specified factor times the 
! local median absolute amplitude.  Data should be sorted to gathers in which 
! the noise is not laterally coherent.
! 
! MDS estimates the local median absolute amplitude on a trace by interpolating
! between median absolute amplitude values calculated in windows centered on 
! the trace.  Windows are NUM_TR_WIN traces wide, have a length of WIN_LEN 
! seconds and an increment of WIN_INC seconds, where WIN_INC may be less than,
! equal to or greater than WIN_LEN. 
!
! Despiking occurs only on the center trace of the window.  MDS despikes 
! samples whose absolute amplitude exceeds THRSH times the local median 
! absolute amplitude.  Despiking can either ZERO the despiked sample or CLIP it
! to THRSH times the local median absolute amplitude.  (Clipping maintains
! the sample polarity.)
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Data should be sorted to gathers in which the noise is not laterally 
! coherent.  Noise from other crews will normally be coherent on shot profiles
! but not on common offsets or CMPs.  Data must be input as single traces.
!
!
! Smaller values of NUM_TR_WIN, the number of traces in the window, will reduce
! run time, while larger values may produce a more stable median estimate.  
! NUM_TR_WIN = 1 may give acceptable results.
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
! This process may alter input traces.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs one trace at a time.
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
! 2       Head mute index            Used, not changed
! 25      LAV                        Reset
! 64      Tail mute index            Used, not changed
! 
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!      Date        Author     Description
!      ----        ------     -----------
! 17.  2010-08-02  Stoeckley  Modify to allow multiple trace input.  This change
!                              means that the original oneset execution subroutine
!                              mds was renamed mds_single_trace, a new twosets
!                              subroutine was created which calls mds_single_trace,
!                              and the restriction of the global NUMTR being 1 was
!                              removed.
!016. 2006-06-20  B. Menger   Removed Unused Variables.
! 15.  2005-05-31  SChiu      Add options of WIN_BEG_FCTR and WIN_BEG_ADD to
!                             allow time varying window.
! 14.  2004-03-17  Stoeckley  Change the CLIP option to clip at the threshhold
!                              instead of the median value; ignore dead traces
!                              when calculating the median and determining the
!                              starting and ending mute times.
! 13.  2001-08-01  SMCook     Fixed TIM_BEG problem -- statistics were being
!                              computed in the proper user-specified windows,
!                              but TIM_BEG and TIM_END were being ignored at the
!                              final application step, causing despiking to
!                              occur over the entire length of the trace, i.e.
!                              potentially outside of the time range specified
!                              by the user.
! 12.  2001-06-07  SMCook     Fixed MODE=ZERO -- was not zeroing samples as
!                              advertised.
!                             Fixed WIN_INC trap problem -- was erroneously
!                              being reset to WIN_LEN.
!                             Added stats in .rpt file of number of traces that
!                              were despiked.
!                             Also, LAV is only recalculated if a despike
!                              operation actually occurred.
!                             Added check so TIM_END can't exceed trace length.
! 11.  2000-12-08  Stoeckley  Change wrapup flag.
! 10.  2000-08-28  Coleman    Converted to new system.
!  9.  1999-01-18  Vunderink  Begin using the f90 compiler.
!                             Moved from newlib to conlib.
!  8.  1998-09-30  Vunderink  Added header flag parameter
!  7.  1998-07-28  Vunderink  Commented out printout
!  6.  1998-07-17  Vunderink  Fixed above/below logic when NTR=1
!  5.  1998-07-16  Vunderink  Fixed to check for gathered traces only if
!                              entry is from above.
!  4.  1998-07-16  Vunderink  Fixed median interpolation and changed
!                              to use MEDIAN primitive.
!  3.  1998-07-15  Vunderink  Fixed to apply despike only in window.
!  2.  1998-06-09  Vunderink  Fixed to handle dead traces.
!  1.  1998-05-20  Vunderink  Original version.
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
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NEED_TRACES    means a subsequent process needs more traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS fkap Process/NC=80>
!
!                                Median DeSpike
!         Despike samples exceeding some factor times the local median
!
!         MODE =~~~~~~~`CCCC
!
!         TIM_BEG =~~~~`FFFFFFFFFFFF        THRSH =~~~~~~`FFFFFFFFFFFF
!
!         TIM_END =~~~~`FFFFFFFFFFFF        HDR_FLAG =~~~`IIIIIIIIIIII
!
!         WIN_LEN =~~~~`FFFFFFFFFFFF        HDR_PANEL =~~`IIIIIIIIIIII
!
!         WIN_INC =~~~~`FFFFFFFFFFFF        PANEL_INIT = `FFFFFFFFFFFF
!
!         NUM_TR_WIN = `IIIIIIIIIIII        PANEL_INC =~~`FFFFFFFFFFFF
!
!         WIN_BEG_FCTR=`FFFFFFFF            WIN_BEG_ADD =`FFFFFFFF
!</gui_def>
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Start despiking at greater of TIM_BEG or head mute time. </Tip>
! Default = 0.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> End despiking at smaller of TIM_END or tail mute time. </Tip>
! Default = end of trace
! Allowed = real
!</Help>
!
!<Help KEYWORD="NUM_TR_WIN">
!<Tip> Total number of traces in the median estimation window. </Tip>
! Default = 3
! Allowed = odd int > 0
! NUM_TR_WIN traces are used in the median estimation window.  Despiking is 
! done on the center trace only.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length of median estimation windows, in seconds. </Tip>
! Default = 0.5
! Allowed = real>10*DT
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<Tip> Time increment for median estimation window locations, in seconds. </Tip>
! Default = WIN_LEN
! Allowed = real>=DT
!</Help>
!
!<Help KEYWORD="THRSH">
!<Tip> Threshold for despike is THRSH times local median estimate. </Tip>
! Default = 3.0
! Allowed = real > 0.0
! MDS despikes samples whose absolute amplitude exceeds THRSH times the local 
! median absolute amplitude.
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to despike by clipping (CLIP) or zeroing (ZERO). </Tip>
! Default = CLIP
! Allowed = CLIP   (Despike by reducing amplitude to the threshhold amplitude.)
! Allowed = ZERO   (Despike by zeroing despiked samples.)
! Despiked samples have their polarity preserved in MODE = CLIP.
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0 
! Allowed = 0 - NWIH
! If HDR_FLAG = 0, then all traces will be despiked.  Otherwise, only traces 
! with a flag set in header word HDR_FLAG will be despiked. 
!
! Regardless of the value of HDR_FLAG, all traces will be used for the median 
! calculations. 
!</Help>
!
!<Help KEYWORD="HDR_PANEL">
!<Tip> Header word denoting panels. </Tip>
! Default = 7
! Allowed = 1 - NWIH
! MDS windows will not cross panel boundaries.  
!</Help>
!
!<Help KEYWORD="PANEL_INIT">
!<Tip> Value of header word HDR_PANEL for the first panel. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="PANEL_INC">
!<Tip> Increment of HDR_PANEL value between panels. </Tip>
! Default = 1.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="WIN_BEG_FCTR">
!<Tip> Factor to multiply head mute time for top of demultiple window. </Tip>
! Default = 1.0
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
!</HelpSection>

!-------------------------------------------------------------------------------
!


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module mds_module

    use singletracesupport_module
    use pc_module
    use named_constants_module
    use string_module
    use median_module
    use interp_module
    use lav_module

    implicit none
    
    private
    public :: mds_create
    public :: mds_initialize
    public :: mds_update
    public :: mds_delete
    public :: mds            ! main execution (trace processing) routine.
    public :: mds_wrapup
    public :: mds_dump_object


    character(len=100),public,save :: mds_IDENT = &
       '$Id: mds.f90,v 1.16 2006/06/20 13:11:59 Menger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    type,public :: mds_struct              
        private
        logical                     :: skip_wrapup      ! wrapup flag.
        real                        :: tim_beg          ! process parameter
        real                        :: tim_end          ! process parameter
        integer                     :: num_tr_win       ! process parameter
        real                        :: win_len          ! process parameter
        real                        :: win_inc          ! process parameter
        real                        :: thrsh            ! process parameter
        character(len=4)            :: mode             ! process parameter
        integer                     :: hdr_flag         ! process parameter
        integer                     :: hdr_panel        ! process parameter
        real                        :: panel_init       ! process parameter
        real                        :: panel_inc        ! process parameter
        real                        :: win_beg_fctr     ! process parameters
        real                        :: win_beg_add      ! process parameters

        real                        :: tstrt            ! global parameter
        real                        :: dt               ! global parameter
        integer                     :: nwih             ! global parameter
        integer                     :: ndpt             ! global parameter

        logical                     :: first            ! dependent variable
        logical                     :: lastin           ! dependent variable
        integer                     :: nbin_inp         ! dependent variable
        integer                     :: nbin_out         ! dependent variable
        integer                     :: itbeg            ! dependent variable
        integer                     :: itend            ! dependent variable
        integer                     :: nwlen            ! dependent variable
        integer                     :: nwinc            ! dependent variable
        integer                     :: nwin             ! dependent variable
        integer                     :: ntwin2           ! dependent variable

        integer                     :: ntraces          ! dependent variable
        integer                     :: ntraces_despiked ! dependent variable
        integer                     :: nsamps_despiked  ! dependent variable

        real                        :: pcur             ! dependent variable
        real                        :: pbin             ! dependent variable
        real, pointer               :: trbuf(:,:)       ! dependent variable
        real, pointer               :: trsav(:)         ! dependent variable
        double precision, pointer   :: hdbuf(:,:)       ! dependent variable
        double precision, pointer   :: hdsav(:)         ! dependent variable

        type(singletracesupport_struct),pointer :: sts

!       integer                     :: itr_in           ! for single trace I/O support.
!       integer                     :: itr_out          ! for single trace I/O support.
!       integer                     :: ntr_inout        ! for single trace I/O support.
!       double precision, pointer   :: hd_single(:,:)   ! for single trace I/O support.
!       real, pointer               :: tr_single(:,:)   ! for single trace I/O support.
!       logical                     :: finished         ! for single trace I/O support.
    end type mds_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


    type(mds_struct),pointer,save :: object         ! needed for traps.
    integer                       :: print_lun = 6


contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine mds_create( obj )
    implicit none
    type(mds_struct),pointer :: obj

    allocate( obj )

    nullify( obj%hdbuf )
    nullify( obj%trbuf )
    nullify( obj%hdsav )
    nullify( obj%trsav )
    nullify( obj%sts )
!    nullify( obj%hd_single )  ! for single trace I/O support.
!    nullify( obj%tr_single )  ! for single trace I/O support.

    call mds_initialize( obj )
    return
end subroutine mds_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine mds_delete( obj )
    implicit none
    type(mds_struct),pointer :: obj

    call mds_wrapup( obj )

    if( associated( obj%trbuf ) ) deallocate( obj%trbuf )
    if( associated( obj%hdbuf ) ) deallocate( obj%hdbuf )
    if( associated( obj%trsav ) ) deallocate( obj%trsav )
    if( associated( obj%hdsav ) ) deallocate( obj%hdsav )

!   if( associated( obj%tr_single ) ) deallocate( obj%tr_single )  ! for single trace I/O support.
!   if( associated( obj%hd_single ) ) deallocate( obj%hd_single )  ! for single trace I/O support.

    deallocate( obj )
    
    return
end subroutine mds_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine mds_initialize( obj )
    implicit none
    type(mds_struct),intent(inout) :: obj

    real :: tend

    call pc_get_global( 'NDPT' , obj%ndpt  )
    call pc_get_global( 'TSTRT', obj%tstrt )
    call pc_get_global( 'DT'   , obj%dt    )
    
    tend = obj%tstrt + obj%dt * ( obj%ndpt - 1 )

!---initialize process parameters
    obj%tim_beg    = 0.0
    obj%tim_end    = tend
    obj%num_tr_win = 3
    obj%win_len    = 0.5
    obj%win_inc    = obj%win_len
    obj%thrsh      = 3.0
    obj%mode       = 'CLIP'
    obj%hdr_flag   = 0
    obj%hdr_panel  = 7
    obj%panel_init = 1.0
    obj%panel_inc  = 1.0

    obj%win_beg_fctr  = 1.0
    obj%win_beg_add   = 0.0

!---adjust window parameters
    obj%nwlen   = nint( obj%win_len / obj%dt )
    obj%nwinc   = nint( obj%win_inc / obj%dt )
    obj%win_len = obj%nwlen * obj%dt
    obj%win_inc = obj%nwinc * obj%dt

!---initialize some of the dependent variables
    obj%pcur       = 0.0
    obj%nbin_inp   = 0
    obj%nbin_out   = 0
    obj%lastin     = .false.
    obj%first      = .true.

!---initialize global parameters
    obj%tstrt      = -HUGE( obj%tstrt )
    obj%dt         = 0.0
    obj%ndpt       = 0
    obj%nwih       = 0

!---initialize counts (a la SMCook)
    obj%ntraces = 0
    obj%ntraces_despiked = 0
    obj%nsamps_despiked = 0

    call mds_update( obj )

    return
end subroutine mds_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine mds_update( obj )
    implicit none
    type(mds_struct),intent(inout),target :: obj

!-----------------------------------------------------------
! LOCAL VARIABLES
!-----------------------------------------------------------
    integer           :: ierr
    character(len=80) :: msg
    integer           :: nstore
    integer           :: nscratch
    real              :: tend
!-----------------------------------------------------------

    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get_global( 'NDPT' , obj%ndpt  )
    call pc_get_global( 'NWIH' , obj%nwih  )
    call pc_get_global( 'TSTRT', obj%tstrt )
    call pc_get_global( 'DT'   , obj%dt    )

    call pc_get( 'TIM_BEG'     , obj%tim_beg    )
    call pc_get( 'TIM_END'     , obj%tim_end    )
    call pc_get( 'NUM_TR_WIN'  , obj%num_tr_win )
    call pc_get( 'WIN_LEN'     , obj%win_len    , mds_win_len_trap )
    call pc_get( 'WIN_INC'     , obj%win_inc    , mds_win_inc_trap )
    call pc_get( 'THRSH'       , obj%thrsh      )
    call pc_get( 'MODE'        , obj%mode       )
    call pc_get( 'HDR_FLAG'    , obj%hdr_flag   )
    call pc_get( 'HDR_PANEL'   , obj%hdr_panel  )
    call pc_get( 'PANEL_INIT'  , obj%panel_init )
    call pc_get( 'PANEL_INC'   , obj%panel_inc  )
    call pc_get('WIN_BEG_FCTR' , obj%win_beg_fctr)
    call pc_get( 'WIN_BEG_ADD' , obj%win_beg_add)
    
    call string_to_upper( obj%mode )

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


    if( obj%dt <= 0.0 ) then
        call pc_error( '*** MDS: Invalid value for global dt: ', obj%dt )
        return
    endif

    if( obj%ndpt <= 0 ) then
        call pc_error( '*** MDS: Invalid value for global ndpt: ', obj%ndpt )
        return
    endif

    if( obj%nwih <= 0 ) then
        call pc_error( '*** MDS: Invalid value for global nwih: ', obj%nwih )
        return
    endif

    tend = obj%tstrt + obj%dt * ( obj%ndpt - 1 )
    if( obj%tim_end < obj%tim_beg ) then
        call pc_error( '*** MDS: Invalid value for tim_end: ', obj%tim_end )
        obj%tim_end = tend
    endif

    if( obj%tim_end > tend ) then            ! SMCook added
        call pc_warning( &
          '*** MDS: TIM_END was reset so as not to exceed trace length.')
        obj%tim_end = tend
    end if

    if( obj%num_tr_win < 1 ) then
        call pc_error( '*** MDS: Invalid value for num_tr_win: ',obj%num_tr_win)
        obj%num_tr_win = 3
    else if( mod( obj%num_tr_win, 2 ) == 0 ) then
        obj%num_tr_win = obj%num_tr_win + 1            ! num_tr_win must be odd
    endif

    if( obj%win_len <= 10*obj%dt ) then
        call pc_error( '*** MDS: Invalid value for win_len: ', obj%win_len )
        obj%win_len = 0.5
    endif

    if( obj%win_inc < obj%dt ) then
        call pc_error( '*** MDS: Invalid value for win_inc: ', obj%win_inc )
        obj%win_inc = obj%win_len
    endif

    if( obj%thrsh <= 0 ) then
        call pc_error( '*** MDS: Invalid value for thrsh: ', obj%thrsh )
        obj%thrsh = 3.0
    endif

    if( obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih ) then
        call pc_error( '*** MDS: Invalid value for hdr_flag: ', obj%hdr_flag)
        obj%hdr_flag = 0
    endif

    if( obj%hdr_panel <= 0 .or. obj%hdr_panel > obj%nwih ) then
        call pc_error( '*** MDS: Invalid value for hdr_panel: ', obj%hdr_panel)
        obj%hdr_panel = 7
    endif

    if    ( obj%mode(1:1) == 'C' ) then
        obj%mode = 'CLIP'
    elseif( obj%mode(1:1) == 'Z' ) then
        obj%mode = 'ZERO'
    else
        call pc_error( '*** MDS: Invalid value for mode: ', obj%mode )
        obj%mode = 'CLIP'
    endif

    !
    ! - Check WIN_BEG_FCTR
    !
    if (obj%win_beg_fctr < 0.0) then
      call pc_error ('WIN_BEG_FCTR must be non-negative.')
    end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

    call pc_put_options_field( 'MODE', (/ 'CLIP', 'ZERO' /), 2 )

    call pc_put( 'TIM_BEG'     , obj%tim_beg    )
    call pc_put( 'TIM_END'     , obj%tim_end    )
    call pc_put( 'NUM_TR_WIN'  , obj%num_tr_win )
    call pc_put( 'WIN_LEN'     , obj%win_len    )
    call pc_put( 'WIN_INC'     , obj%win_inc    )
    call pc_put( 'THRSH'       , obj%thrsh      )
    call pc_put( 'MODE'        , obj%mode       )
    call pc_put( 'HDR_FLAG'    , obj%hdr_flag   )
    call pc_put( 'HDR_PANEL'   , obj%hdr_panel  )
    call pc_put( 'PANEL_INIT'  , obj%panel_init )
    call pc_put( 'PANEL_INC'   , obj%panel_inc  )
    call pc_put( 'WIN_BEG_FCTR', obj%win_beg_fctr)
    call pc_put( 'WIN_BEG_ADD' , obj%win_beg_add)

    obj%itbeg  = max( 1, nint( (obj%tim_beg - obj%tstrt ) / obj%dt ) + 1 )
    obj%itend  = min( obj%ndpt, nint( (obj%tim_end - obj%tstrt )/obj%dt ) + 1 )
    obj%nwin   = float( obj%itend - obj%itbeg + 1 ) / float( obj%nwinc ) + 0.5
    obj%ntwin2 = obj%num_tr_win / 2

    nstore     = (obj%num_tr_win + 1) * ( 2*obj%nwih + obj%ndpt )
    nscratch   = obj%num_tr_win * obj%nwlen + 2 * obj%nwin + 2 * obj%ndpt

    call pc_put_control( 'NSCRATCH'    , nscratch )
    call pc_put_control( 'NSTORE'      , nstore   )
    call pc_put_control( 'NEED_REQUEST', .true.   )
    call pc_put_control( 'NEED_LABEL'  , .true.   )
    call pc_put_control( 'TWOSETS'     , .true.   )  ! for single trace I/O support.

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!   if( associated( obj%tr_single ) ) deallocate( obj%tr_single )  ! for single trace I/O support.
!   if( associated( obj%hd_single ) ) deallocate( obj%hd_single )  ! for single trace I/O support.

    call singletracesupport_delete (obj%sts)

    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

    call singletracesupport_create (obj%sts, obj%nwih, obj%ndpt)

!   allocate( obj%hd_single(obj%nwih,1), stat=ierr )   ! for single trace I/O support.
!   allocate( obj%tr_single(obj%ndpt,1), stat=ierr )   ! for single trace I/O support.

!   obj%itr_in    = 0         ! for single trace I/O support.
!   obj%itr_out   = 0         ! for single trace I/O support.
!   obj%ntr_inout = 0         ! for single trace I/O support.
!   obj%finished  = .false.   ! for single trace I/O support.

    print_lun = pc_get_lun()

    allocate( obj%hdbuf(obj%nwih,obj%num_tr_win), stat=ierr )
    if( ierr /= 0 ) then
        write( msg, * ) '*** MDS: Error allocating HDBUF to ', &
                        8 * obj%nwih * obj%num_tr_win, ' bytes'
        call pc_error( msg )
    endif

    allocate( obj%trbuf(obj%ndpt,obj%num_tr_win), stat=ierr )
    if( ierr /= 0 ) then
        write( msg, * ) '*** MDS: Error allocating TRBUF to ', &
                        4 * obj%ndpt * obj%num_tr_win, ' bytes'
        call pc_error( msg )
    endif

    allocate( obj%hdsav(obj%nwih), stat=ierr )
    if( ierr /= 0 ) then
        write( msg, * ) '*** MDS: Error allocating HDSAV to ', 8 * obj%nwih, &
                        ' bytes'
        call pc_error( msg )
    endif

    allocate( obj%trsav(obj%ndpt), stat=ierr )
    if( ierr /= 0 ) then
        write( msg, * ) '*** MDS: Error allocating TRSAV to ', 4 * obj%ndpt, &
                        ' bytes'
        call pc_error( msg )
    endif

    if( pc_do_not_process_traces() ) return   ! in case of allocation errors.


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine mds_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


subroutine mds_win_len_trap( keyword )              ! scalar trap
    implicit none
    character(len=*),intent(in) :: keyword          ! arguments

    object%nwlen   = nint( object%win_len / object%dt )
    object%win_len = object%nwlen * object%dt

!
! SMCook added call to pc_gui_action_present -- previously user WIN_INC value
! was overwritten every time this trap was called, the net effect being that the
! user was effectively powerless to specifically set WIN_INC
!
    if(pc_gui_action_present('WIN_LEN', 'ModifyField')) then
      object%nwinc   = object%nwlen
      object%win_inc = object%win_len
      call pc_jump_field( 'WIN_INC' )
    end if

    return
end subroutine mds_win_len_trap


subroutine mds_win_inc_trap( keyword )              ! scalar trap
    implicit none
    character(len=*),intent(in) :: keyword          ! arguments

    object%nwinc   = nint( object%win_inc / object%dt )
    object%win_inc = object%nwinc * object%dt

    return
end subroutine mds_win_inc_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

! This twosets subroutine receives a gather and outputs a gather.
! This twosets subroutine is the glue between the outside world and
! the corresponding mds_single_trace subroutine which accepts only single trace input.
! The assumptions are:
!  (1) This process outputs the same traces input, and in the same order.
!  (2) The output gather will be the same size as the input gather.
!  (3) The trace and header lengths NWIH and NDPT are not changed.


subroutine mds (obj, ntr, hdi, tri, hdo, tro)
    implicit none
    type(mds_struct), intent(inout) :: obj
    integer         , intent(inout) :: ntr
    double precision, intent(inout) :: hdi(:,:)
    real            , intent(inout) :: tri(:,:)
    double precision, intent(out)   :: hdo(:,:)
    real            , intent(out)   :: tro(:,:)

!    if (ntr == NO_MORE_TRACES) then
!        continue  ! do nothing here.
!    else if (ntr == NEED_TRACES) then
!        if (obj%finished) then    ! the last gather has already been output.
!            ntr = NO_MORE_TRACES
!            return
!        endif
!        obj%itr_out = 0   ! need to start refilling the output gather.
!    else
!        obj%ntr_inout = ntr
!        obj%itr_in = 1
!        obj%hd_single(1:obj%nwih,1) = hdi(1:obj%nwih,obj%itr_in)
!        obj%tr_single(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%itr_in)
!        ntr = 1
!    endif

     if (singletracesupport_input(obj%sts, ntr, hdi, tri)) return

 22  call mds_single_trace (obj, ntr, hdi, tri)

     if (singletracesupport_output(obj%sts, ntr, hdi, tri, hdo, tro)) go to 22

!22  call mds_single_trace (obj, ntr, obj%hd_single, obj%tr_single)
!
!    if (ntr > 0) then
!        obj%itr_out = obj%itr_out + 1
!        hdo(1:obj%nwih,obj%itr_out) = obj%hd_single(1:obj%nwih,1)
!        tro(1:obj%ndpt,obj%itr_out) = obj%tr_single(1:obj%ndpt,1)
!        if (obj%itr_out == obj%ntr_inout) then
!            ntr = obj%ntr_inout    ! return a full output gather.
!            obj%itr_out = 0      ! will begin to refill the output gather.
!            return
!        endif
!        ntr = NEED_TRACES
!        goto 22
!    else if (ntr == FATAL_ERROR) then
!        return
!    else if (ntr == NO_MORE_TRACES) then  ! return what remains of the output gather, or 0.
!        if (obj%itr_out == 0) return   ! no more traces to output.
!        obj%finished = .true.
!        ntr = obj%itr_out    ! outputting whatever is in the last output gather ever.
!        return
!    else if (ntr == NEED_TRACES) then
!        if (obj%itr_in == obj%ntr_inout) return  ! go get more traces since the input gather is used up.
!        obj%itr_in = obj%itr_in + 1
!        obj%hd_single(1:obj%nwih,1) = hdi(1:obj%nwih,obj%itr_in)
!        obj%tr_single(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%itr_in)
!        ntr = 1
!        goto 22
!    endif

end subroutine mds


!!-------------------------- mds single trace ------------------------------!!
!!-------------------------- mds single trace ------------------------------!!
!!-------------------------- mds single trace ------------------------------!!


subroutine mds_single_trace (obj, ntr, hd, tr)
    implicit none

!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    type(mds_struct), intent(inout) :: obj
    integer         , intent(inout) :: ntr
    double precision, intent(inout) :: hd(:,:)
    real            , intent(inout) :: tr(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    real    :: a_med(obj%nwin)
    logical :: bin_done
    logical :: dspike
    logical :: despike_occurred
    integer :: i
    integer :: i1
    integer :: i2
    integer :: indx
    integer :: iwin
    integer :: j
    integer :: jcnt
    integer :: k

    logical :: newtrace
    integer :: nmed
    integer :: ntr_buf
    real    :: trmed(obj%ndpt)
    logical :: tr_done
    real    :: tstep(obj%ndpt)
    real    :: t_med(obj%nwin)
    real    :: wrka(2*obj%nwlen*obj%num_tr_win)
    real    :: xs
    real    :: xe
    integer :: mute_start
!-----------------------------------------------

    bin_done = obj%nbin_inp == obj%nbin_out

    select case( ntr )

    case( NO_MORE_TRACES )
        newtrace   = .false.
        obj%lastin = .true.
        if( bin_done ) then
            call mds_wrapup( obj )
            return
        endif

    case( 1 )
        newtrace = .true.
        obj%pbin = nint( (hd(obj%hdr_panel,1) - obj%panel_init ) / &
                         obj%panel_inc + 1.0 )

        if( obj%first ) then
            obj%pcur  = obj%pbin
            obj%first = .false.
        endif

        if( obj%pbin /= obj%pcur ) then
            if( bin_done ) then
                obj%nbin_inp = 0
                obj%nbin_out = 0
                obj%pcur     = obj%pbin
            else
                !save input trace
                obj%trsav = tr(1:,1)
                obj%hdsav = hd(1:,1)
                newtrace  = .false.
            endif
        endif

    case( NEED_TRACES )
        newtrace = .false.

        if( obj%lastin ) then
            if( bin_done ) then
                call mds_wrapup( obj )
                ntr = NO_MORE_TRACES
                return
            endif
        else
            if( obj%pbin /= obj%pcur ) then
                if( bin_done ) then
                    !restore input trace
                    tr(1:,1)     = obj%trsav
                    hd(1:,1)     = obj%hdsav
                    newtrace     = .true.
                    obj%nbin_inp = 0
                    obj%nbin_out = 0
                    obj%pcur     = obj%pbin
                endif
            else
                if( obj%nbin_inp <= obj%nbin_out + obj%ntwin2 ) then
                    ntr = NEED_TRACES
                    return
                endif
            endif
        endif

    case default
        ntr = FATAL_ERROR
        call mds_wrapup( obj )
        return

    end select

!====== if newtrace, then put the input trace into the buffer ========

    if( newtrace ) then
        indx              = mod( obj%nbin_inp, obj%num_tr_win ) + 1
        obj%nbin_inp      = obj%nbin_inp + 1
        obj%hdbuf(:,indx) = hd(1:,1)
        obj%trbuf(:,indx) = tr(1:,1)

        if( obj%nbin_inp < obj%num_tr_win ) then
            ntr     = NEED_TRACES
            return
        endif
    endif

!================== output a trace from the buffer ===================

    indx             = mod( obj%nbin_out, obj%num_tr_win ) + 1
    tr(1:obj%ndpt,1) = obj%trbuf(:,indx)
    hd(1:obj%nwih,1) = obj%hdbuf(:,indx)
    hd(24,1)         = obj%pcur
    ntr              = 1
    obj%nbin_out     = obj%nbin_out + 1
    ntr_buf          = min( obj%nbin_inp, obj%num_tr_win )

    if( hd(25,1) == 0.0 ) return      !output the unmodified dead trace

!---determine beginning and ending for despiking
    i1 = obj%itbeg
    i2 = obj%itend
    do i = 1, ntr_buf
        if( obj%hdbuf(25,i) == 0.0) cycle               !skip dead trace
        mute_start = nint(  obj%hdbuf(2,i)*obj%win_beg_fctr   &
                          + obj%win_beg_add/obj%dt)
        i1 = max( obj%itbeg, mute_start )
        i2 = min( obj%itend, nint( obj%hdbuf(64,i) ) )
    end do

    if( i1 >= i2 ) return             !output the unmodified trace

!---determine median within windows
    iwin    = obj%nwlen
    tr_done = .false.
    nmed    = 0
    do i = i1, i2, obj%nwinc
        if( i2 - i + 1 < 1.5 * obj%nwlen ) then
            iwin    = i2 - i + 1
            tr_done = .true.
        endif

        jcnt = 0
        do j = 1, ntr_buf
            if( obj%hdbuf(25,j) == 0.0) cycle        !skip dead trace
            do k = 1, iwin
                jcnt = jcnt + 1
                wrka(jcnt) = abs( obj%trbuf(i+k-1,j) )
            end do
        end do
        
        nmed = nmed + 1
        call median( wrka, jcnt, a_med(nmed) )
        t_med(nmed) = i + 0.5 * ( iwin - 1 )
        if( tr_done ) exit
    end do

    xs = 1.0
    xe = obj%ndpt
    call interp_1d_var_lin_real( &
        t_med, a_med, nmed, tstep, trmed, obj%ndpt, xs, xe )

!---perform actual despike here
    dspike   = .true.
    if( obj%hdr_flag /= 0 ) then
        if( dabs( hd(obj%hdr_flag,1) ) < 1.0D-6 ) dspike = .false.
    endif

    if( dspike ) then
        despike_occurred = .false.
        do i=i1,i2
            if(abs(tr(i,1)) > obj%thrsh*trmed(i) ) then
                despike_occurred = .true.
                obj%nsamps_despiked = obj%nsamps_despiked + 1
                if( obj%mode(1:4) == 'CLIP' ) then
                    tr(i,1) = sign( obj%thrsh*trmed(i), obj%trbuf(i,indx) )
                else
                    tr(i,1) = 0.0
                end if
            endif
        end do
    endif

!---SMCook -- update "user info" counts (and LAV if despike occurred)

    obj%ntraces = obj%ntraces + 1

    if( despike_occurred ) then
      obj%ntraces_despiked = obj%ntraces_despiked + 1
      call lav_set_hdr( hd(:,1), tr(:,1), obj%ndpt )
    end if

    return
end subroutine mds_single_trace


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


subroutine mds_wrapup( obj )
    implicit none
    type(mds_struct),intent(inout) :: obj       ! arguments

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    call singletracesupport_delete (obj%sts)

    call pc_print('MDS: ********************************************')
    call pc_print('MDS:   Number of traces processed = ',obj%ntraces)
    call pc_print('MDS:   Number of traces  despiked = ',obj%ntraces_despiked)
    call pc_print('MDS:   Number of samples affected = ',obj%nsamps_despiked)
    call pc_print('MDS: ********************************************')

    return
end subroutine mds_wrapup


!!------------------------------ dump object -----------------------------!!
!!------------------------------ dump object -----------------------------!!
!!------------------------------ dump object -----------------------------!!


subroutine mds_dump_object( obj )

    type(mds_struct), intent(in) :: obj

    write( print_lun, '( /"***** BEGIN MDS OBJECT DUMP ", 51("*") )' )
    write( print_lun, '( "    skip_wrapup   = ", l1   )' ) obj%skip_wrapup
    write( print_lun, '( "    mode          = ", a4   )' ) obj%mode
    write( print_lun, '( "    tim_beg       = ", f8.3 )' ) obj%tim_beg
    write( print_lun, '( "    tim_end       = ", f8.3 )' ) obj%tim_end
    write( print_lun, '( "    win_len       = ", f8.3 )' ) obj%win_len
    write( print_lun, '( "    win_inc       = ", f8.3 )' ) obj%win_inc
    write( print_lun, '( "    thrsh         = ", f8.3 )' ) obj%thrsh
    write( print_lun, '( "    num_tr_win    = ", i4   )' ) obj%num_tr_win
    write( print_lun, '( "    hdr_flag      = ", i4   )' ) obj%hdr_flag
    write( print_lun, '( "    hdr_panel     = ", i4   )' ) obj%hdr_panel
    write( print_lun, '( "    panel_init    = ", f8.3 )' ) obj%panel_init
    write( print_lun, '( "    panel_inc     = ", f8.3 )' ) obj%panel_inc
    write( print_lun, '( "    ndpt          = ", i4   )' ) obj%ndpt
    write( print_lun, '( "    nwih          = ", i4   )' ) obj%nwih
    write( print_lun, '( "    tstrt         = ", f8.3 )' ) obj%tstrt
    write( print_lun, '( "    dt            = ", f8.3 )' ) obj%dt
    write( print_lun, '( "    first         = ", l1   )' ) obj%first
    write( print_lun, '( "    lastin        = ", l1   )' ) obj%lastin
    write( print_lun, '( "    nbin_inp      = ", i4   )' ) obj%nbin_inp
    write( print_lun, '( "    nbin_out      = ", i4   )' ) obj%nbin_out
    write( print_lun, '( "    itbeg         = ", i4   )' ) obj%itbeg
    write( print_lun, '( "    itend         = ", i4   )' ) obj%itend
    write( print_lun, '( "    nwlen         = ", i4   )' ) obj%nwlen
    write( print_lun, '( "    nwinc         = ", i4   )' ) obj%nwinc
    write( print_lun, '( "    nwin          = ", i4   )' ) obj%nwin
    write( print_lun, '( "    ntwin2        = ", i4   )' ) obj%ntwin2
    write( print_lun, '( "    pcur          = ", f8.3 )' ) obj%pcur
    write( print_lun, '( "    pbin          = ", f8.3 )' ) obj%pbin
    write( print_lun, '( "***** END MDS OBJECT DUMP ", 53("*") / )' )

end subroutine mds_dump_object


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module mds_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

