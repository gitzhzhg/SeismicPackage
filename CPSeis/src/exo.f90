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
! Name       : EXO   (EXpansion by Offset)
! Category   : amplitude_mod
! Written    : 2000-07-14   by: R.D. Coleman, Axian, Inc.
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Calculate/apply time-varying expansion, uniform for each offset.
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
! (EXO is designed primarily as a diagnostic for relative amplitude processing.)
!
! EXO can operate in three modes (CALC, APPLY and REMOVE).
!
! If MODE = CALC, EXO will measure an amplitude for each time window for each
! input trace.  The measurement is the average or median of the absolute
! amplitude of the live samples within the trace window.  These trace amplitude
! measurements are combined (using either the average or median of the trace
! measurements) to form an amplitude measurement that is separate and
! independent for each window and for each specified offset bin.  These
! amplitude measurements are optionally written to a textfile and optionally
! printed in the .rpt file.
!
! If MODE = APPLY, EXO will read the amplitude textfile and will apply a gain
! function to each input trace.  This gain function is uniform for a given
! offset and distinct for different offsets.
!
!    EXO applies a gain to each trace, at the center of the windows, that is
!    the reciprocal of the amplitude measurement for the appropriate window
!    and offset bin for that trace.  The applied gain is interpolated
!    between window centers and extrapolated as a constant above the first
!    window center and below the last window center.  (This the XP
!    algorithm.)
!
! If MODE = REMOVE, EXO will read the amplitude textfile and will remove the
! gain that was applied in APPLY mode.  APPLY and RESTORE modes are inverse
! operations.
!
!
! Windows
!
! EXO, in all three modes, determines window locations for each input trace
! individually.  The first window always starts at the head mute time and the
! last window ends at the tail mute time.  Window length and window overlap are
! determined by the WIN_LEN and WIN_INC parameters.
!
! In the CALC mode, only the trace samples within the windows are used.
! In the APPLY and REMOVE modes, the gain is applied to (or removed from) the
! entire trace.  A constant gain is extrapolated above the first window center
! and below the last window center.
!
! Because the head mute time will, in general, vary laterally and with offset,
! the EXO windows cannot be assigned absolute times (except with respect to the
! head mute time).
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Recommended Use
!
! The recommended use of EXO is as an amplitude diagnostic in relative
! amplitude processing.
!
! WARNING!!  EXO assumes amplitudes do not vary laterally.  Using EXO as an
! amplitude balance in relative amplitude processing can have unintended
! consequences if the field data has 1) amplitude characteristics that vary
! laterally, 2) structural complexity or 3) a water bottom that varies
! significantly in time.
!
!
! Hint
!
! Normally, unless the dataset is very small, a subset of the entire dataset is
! used in MODE = CALC.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
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
! This process does not alter input traces in CALC mode.
!
! This process outputs the same traces as it receives (altered in APPLY or
! REMOVE mode).
!
! This process outputs traces with same gather status as the input traces.
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
! Hwd#      Description                Action taken
! ----      -----------                ------------
! 2         head mute index            used
! 64        tail mute index            used
! 25        LAV                        reset
! HDR_OFF   hw containing offset       used
! HDR_FLAG  hw containing flag         used
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author    Description
!     ----       ------    -----------
!013. 2006-09-18  D. Glover Added NULLIFY statements for Intel compiler.
!012. 2006-01-10  B. Menger Removed Unused Variables.
! 11. 2004-06-14 Stoeckley  Put in protective code to detect when TR_MAX is
!                            too small before writing beyond the end of array
!                            obj%buf.
! 10. 2004-05-24 Stoeckley  Fix bug whereby dead windows caused program to hang.
!  9. 2004-01-07 Stoeckley  Fix improper mute times when applying gain.
!  8. 2002-09-17 Goodger    Use mth_module for binning.
!  7. 2002-05-08 Stoeckley  Fix obsolescent character declaration features.
!                           Add HDR_FLAG parameter.
!  6. 2002-03-04 Stoeckley  Remove bug preventing setting WIN_INC < WIN_LEN.
!  5. 2001-10-18 Stoeckley  Add file selection box and file status message.
!  4. 2001-04-26 Stoeckley  Remove unused reference to the BYTE process.
!  3. 2000-12-08 Stoeckley  Change wrapup flag.
!  2. 2000-09-05 Coleman    Changed open of output text file to overwrite an
!                            existing file.  Also, ensured that PATH_EXO can
!                            equal NONE when mode equal CALC.
!  1. 2000-07-14 Coleman    Original version.
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
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
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
!<gui_def>
!<NS EXO Process/NC=80>
!
!                      EXO  (EXpansion by Offset)
!    Calculate/apply time-varying expansion, uniform for each offset
!
!    MODE = `CCCCCC    STAT_TR = `CCC    STAT_WIN = `CCC    OPT_PRINT = `CCC
!
!    Select PATH_EXO[path_exo]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                    [path_exo_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!          HDR_FLAG=~~`IIIIIIIIIIII
!
!          HDR_OFF =~~`IIIIIIIIIIII           OFF_INIT = `FFFFFFFFFFFF
!
!          WIN_LEN =~~`FFFFFFFFFFFF           OFF_INC =~~`FFFFFFFFFFFF
!
!          WIN_INC =~~`FFFFFFFFFFFF           OFF_LAST = `FFFFFFFFFFFF
!
!          TR_MAX =~~~`IIIIIIIIIIII           OFF_TOT =~~`IIIIIIIIIIII
!
!          GAIN_MAX = `FFFFFFFFFFFF
!
!<PARMS PATH_EXO[/ML=128/XST]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
! OPT_PRINT, STAT_TR, STAT_WIN, TR_MAX, HDR_OFF, OFF_INIT, OFF_INC, OFF_LAST,
! OFF_TOT, WIN_LEN, and WIN_INC are active for MODE = CALC only.  GAIN_MAX is
! active for MODE = APPLY and REMOVE only.
!
!<Help KEYWORD="SELECT_PATH_EXO">
!<Tip> Choose PATH_EXO using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATH_EXO_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_EXO. </Tip>
!</Help>
!
!
!<Help KEYWORD="MODE">
!<Tip> Whether EXO calculates, applies or removes the amplitude balance. </Tip>
! Default = CALC
! Allowed = CALC
! Allowed = APPLY
! Allowed = REMOVE
! EXO is a two pass operation.  In the first pass EXO CALCulates the amplitudes
! for each window for each offset bin.  In the second pass EXO can APPLY the
! amplitude balance or REMOVE a previously applied amplitude balance.
!</Help>
!
!<Help KEYWORD="GAIN_MAX">
!<Tip> Maximum allowed gain in the amplitude balance. </Tip>
! Default = 1.0 E10
! Allowed = real > 0.0
! Any calculated amplitude balance exceeding GAIN_MAX will be reduced to
! GAIN_MAX.
!</Help>
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Whether to print amplitude measurements in the .rpt file. </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="STAT_TR">
!<Tip> Statistic for measuring amplitude of individual traces. </Tip>
! Default = MED
! Allowed = MED   (Median of trace absolute amplitudes.)
! Allowed = AVE   (Average of trace absolute amplitudes.)
! Statistic for measuring amplitude of individual traces when calculating the
! amplitude measurement for a particular window.
!</Help>
!
!<Help KEYWORD="STAT_WIN">
!<Tip> Statistic for calculating amplitude of a particular window. </Tip>
! Default = MED
! Allowed = MED   (Median)
! Allowed = AVE   (Average)
! STAT_WIN specifies the method used to combine individual trace amplitude
! measurements into an amplitude measurement for a particular window at a
! particular offset.
!</Help>
!
!<Help KEYWORD="TR_MAX">
!<Tip> Maximum number of input traces to expect in any offset bin. </Tip>
! Default = 100
! Allowed = int > 0
! Active for STAT_WIN = MED only.
!</Help>
!
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 1 - NWIH
!
! If HDR_FLAG = 0, then all traces are shifted.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are shifted.
!</Help>
!
!
!<Help KEYWORD="HDR_OFF">
!<Tip> Header word labeling offset. </Tip>
! Default = 6
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Value of HDR_OFF at center of first offset bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment between, and width of, offset bins. </Tip>
! Default = 1.0
! Allowed = real > 0.0
! Normally offset bin increments (widths) are set to the natural offset
! increment in a shot profile.  In some cases it may be desirable to make the
! offset bin widths larger than the natural offset increment if fewer
! independent EXO amplitude calculations are desired.
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Value of HDR_OFF at center of last offset bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offset bins. </Tip>
! Default = 1
! Allowed = int
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<tip> EXO window length, in seconds. </tip>
!  Default = 0.5
!  Allowed = real>10*DT
!  Window length, in seconds.
!
! The first EXO window for an offset bin always starts at the head mute time.
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<tip> Time increment for EXO window locations, in seconds. </tip>
!  Default = WIN_LEN
!  Allowed = real>=DT
!  Time increment for window location, in seconds.
!</Help>
!
!<Help KEYWORD="PATH_EXO">
!<tip> Pathname for textfile containing window amplitude measurements. </tip>
!  Default = NONE
!  Allowed = char
! PATH_EXO is the pathname for a textfile containing amplitude measurements for
! each window in each offset.  If PATH_EXO = NONE, no file is written.  PATH_EXO
! can equal NONE only if MODE = CALC.
!
! In MODE = CALC, this file is written.
! In MODE = APPLY and RESTORE, this file is used.
!
! Within the file, window times given are the window-center time of the
! first trace encountered for a particular offset.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!
! Begin Fortran here.
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module exo_module

    use pc_module
    use named_constants_module
    use getlun_module
    use string_module
    use pattern_module
    use median_module
    use interp_module
    use lav_module
    use pathcheck_module
    use pathchoose_module
    use mth_module

    implicit none

    private
    public   :: exo_create     ! uses the parameter cache.
    public   :: exo_initialize
    public   :: exo_update     ! uses the parameter cache.
    public   :: exo_delete
    public   :: exo_dump_struct
    public   :: exo            ! main execution (trace processing) routine.
    private  :: exo_get_real
    private  :: exo_get_int
    private  :: exo_get_data
    private  :: exo_write_file
    private  :: exo_calc
    private  :: exo_apply
    public   :: exo_wrapup

    character(len=100),public,save :: exo_IDENT = &
       "$Id: exo.f90,v 1.13 2006/09/18 13:32:46 Glover prod sps $"

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    type,public :: exo_struct
        private
        logical                        :: skip_wrapup  ! wrapup flag
        character(len=6)               :: mode        ! process parameter
        real                           :: gain_max    ! process parameter
        logical                        :: opt_print   ! process parameter
        character(len=3)               :: stat_tr     ! process parameter
        character(len=3)               :: stat_win    ! process parameter
        integer                        :: tr_max      ! process parameter
        integer                        :: hdr_flag    ! process parameter
        integer                        :: hdr_off     ! process parameter
        real                           :: off_init    ! process parameter
        real                           :: off_inc     ! process parameter
        real                           :: off_last    ! process parameter
        integer                        :: off_tot     ! process parameter
        real                           :: win_len     ! process parameter
        real                           :: win_inc     ! process parameter
        character(len=FILENAME_LENGTH) :: path_exo    ! process parameter

        integer                        :: nwih        ! global
        integer                        :: ndpt        ! global
        real                           :: tstrt       ! global
        real                           :: dt          ! global

        integer                        :: noff_max    ! dependent variable
        integer                        :: nwin_max    ! dependent variable
        integer                        :: nwin        ! dependent variable
        integer                        :: ntrc        ! dependent variable
        integer                        :: maxtrc      ! dependent variable
        integer                        :: nwlen       ! dependent variable
        integer                        :: nwinc       ! dependent variable
        integer                        :: lu_data     ! dependent variable
        integer                        :: lu_tpnt     ! dependent variable
        real, pointer                  :: times(:)    ! dependent variable
        real, pointer                  :: amp(:,:)    ! dependent variable
        real, pointer                  :: buf(:,:,:)  ! dependent variable
        type(pathchoose_struct),pointer :: dialog

    end type exo_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


    type(exo_struct),pointer,save :: object              ! needed for traps
    integer,save                  :: lu_print =   6
    integer,parameter             :: MAX_LINE = 160

contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine exo_create( obj )
    implicit none
    type(exo_struct),pointer :: obj       ! arguments

    allocate( obj )

!---nullify pointers
    nullify( obj%times )
    nullify( obj%amp   )
    nullify( obj%buf   )
    nullify (obj%dialog) ! jpa

    call pathchoose_create (obj%dialog, 'path_exo', 'exo')
    call exo_initialize    (obj)
    return
end subroutine exo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine exo_delete( obj )
    implicit none
    type(exo_struct),pointer :: obj       ! arguments

    call exo_wrapup( obj )

    if( associated( obj%times   ) ) deallocate      ( obj%times   )
    if( associated( obj%amp     ) ) deallocate      ( obj%amp     )
    if( associated( obj%buf     ) ) deallocate      ( obj%buf     )

    call pathchoose_delete (obj%dialog)

    deallocate(obj)
    return
end subroutine exo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine exo_initialize( obj )
    implicit none
    type(exo_struct),intent(inout) :: obj       ! arguments

    obj%mode        = 'CALC'
    obj%opt_print   = .false.
    obj%stat_tr     = 'MED'
    obj%stat_win    = 'MED'
    obj%tr_max      = 100
    obj%hdr_flag    = 0
    obj%hdr_off     =   6
    obj%off_init    =   1.0
    obj%off_inc     =   1.0
    obj%off_last    =   1.0
    obj%off_tot     =   1
    obj%win_len     =   0.5
    obj%win_inc     = obj%win_len
    obj%gain_max    =   1.0e10
    obj%path_exo    = PATHCHECK_EMPTY
!
! other dependent variables
!
    obj%nwin_max    =  0
    obj%noff_max    =  0
    obj%lu_data     = -1

!   clear globals then check later to insure that they are updated

    obj%ndpt        = 0
    obj%nwih        = 0
    obj%dt          = 0.0
    obj%tstrt       = -huge( obj%tstrt )

    call exo_update( obj )

    return
end subroutine exo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

subroutine exo_update( obj )
    implicit none
    type(exo_struct),intent(inout),target :: obj

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real      :: dt_sav
    integer   :: i
    integer   :: ierr(3)
    integer   :: ierr0
    real      :: ts_sav

    character(len=FILENAME_LENGTH) :: userid
    character(len=FILENAME_LENGTH) :: node
    character(len=FILENAME_LENGTH) :: dir
    character(len=FILENAME_LENGTH) :: file
!
!-------------------------------------------------------------------------------
!
    object => obj               ! needed for traps
    obj%skip_wrapup = .true.    ! needed for the wrapup routine

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    if (pathchoose_update(obj%dialog, obj%path_exo)) return

    call pc_get_global( 'NDPT'    , obj%ndpt  )
    call pc_get_global( 'NWIH'    , obj%nwih  )
    call pc_get_global( 'TSTRT'   , obj%tstrt )
    call pc_get_global( 'DT'      , obj%dt    )

    call pc_get( 'MODE'     , obj%mode      )
    call pc_get( 'GAIN_MAX' , obj%gain_max  )
    call pc_get( 'OPT_PRINT', obj%opt_print )
    call pc_get( 'STAT_TR'  , obj%stat_tr   )
    call pc_get( 'STAT_WIN' , obj%stat_win  )
    call pc_get( 'TR_MAX'   , obj%tr_max    )
    call pc_get( 'HDR_FLAG' , obj%hdr_flag  )
    call pc_get( 'HDR_OFF'  , obj%hdr_off   )
    call pc_get( 'OFF_INIT' , obj%off_init, exo_off_pattern_trap )
    call pc_get( 'OFF_INC'  , obj%off_inc , exo_off_pattern_trap )
    call pc_get( 'OFF_LAST' , obj%off_last, exo_off_pattern_trap )
    call pc_get( 'OFF_TOT'  , obj%off_tot , exo_off_pattern_trap )
    call pc_get( 'WIN_LEN'  , obj%win_len , exo_win_len_trap )
    call pc_get( 'WIN_INC'  , obj%win_inc , exo_win_inc_trap )
    call pc_get( 'PATH_EXO' , obj%path_exo  )

    call string_to_upper( obj%mode     )
    call string_to_upper( obj%stat_tr  )
    call string_to_upper( obj%stat_win )

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    if( obj%ndpt <= 0 ) then
        call pc_error( msg1='Global NDPT has an invalid value', var1=obj%ndpt )
        return
    endif

    if( obj%nwih <= 0 ) then
        call pc_error( msg1='Global NWIH has an invalid value', var1=obj%nwih )
        return
    endif

    if( obj%dt <= 0.0 ) then
        call pc_error( msg1='Global DT has an invalid value', var1=obj%dt )
        return
    endif

    if( obj%tstrt == -huge(obj%tstrt) ) then
        call pc_error( msg1='Global TSTRT has an invalid value', var1=obj%tstrt)
        return
    endif

    if     ( obj%mode(1:1) == 'C' ) then
        obj%mode = 'CALC'
    else if( obj%mode(1:1) == 'A' ) then
        obj%mode = 'APPLY'
    else if( obj%mode(1:1) == 'R' ) then
        obj%mode = 'REMOVE'
    else
        call pc_error( msg1='Invalid value for MODE', var1=obj%mode )
                obj%mode = 'CALC'
    endif

    if     ( obj%stat_tr(1:1) == 'M' ) then
        obj%stat_tr = 'MED'
    else if( obj%stat_tr(1:1) == 'A' ) then
        obj%stat_tr = 'AVE'
    else
        call pc_error( msg1='Invalid value for STAT_TR', var1=obj%stat_tr )
                obj%stat_tr = 'MED'
    endif

    if     ( obj%stat_win(1:1) == 'M' ) then
        obj%stat_win = 'MED'
    else if( obj%stat_win(1:1) == 'A' ) then
        obj%stat_win = 'AVE'
    else
        call pc_error( msg1='Invalid value for STAT_WIN', var1=obj%stat_win )
        obj%stat_win = 'MED'
    endif

    if( obj%gain_max <= 0.0 ) then
        call pc_error( msg1='Invalid value for GAIN_MAX', var1=obj%gain_max )
        obj%gain_max = 1.0e10
    endif

    if( obj%tr_max <= 0 ) then
        call pc_error( msg1='Invalid value for TR_MAX', var1=obj%tr_max )
        obj%tr_max = 100
    endif

    if( obj%win_len <= 10.0*obj%dt ) then
        call pc_error( msg1='Invalid value for WIN_LEN', var1=obj%win_len )
        obj%win_len = 0.5
    endif

    if( obj%win_inc < obj%dt ) then
        call pc_error( msg1='Invalid value for WIN_INC', var1=obj%win_inc )
        obj%win_inc = obj%win_len
    endif

    if( (obj%hdr_flag < 0) .or. (obj%hdr_flag > obj%nwih) ) then
        call pc_error( msg1='Invalid value for HDR_FLAG', var1=obj%hdr_flag )
        obj%hdr_flag = 0
    endif

    if( (obj%hdr_off < 1) .or. (obj%hdr_off > obj%nwih) ) then
        call pc_error( msg1='Invalid value for HDR_OFF', var1=obj%hdr_off )
        obj%hdr_off = 6
    endif

    if( obj%mode == 'CALC' ) then
        call pathcheck( 'PATH_EXO', obj%path_exo, '.exo', REQUIRED=.false., &
                             show=PATHCHECK_INFO_OUTPUT )
    else
        call pathcheck( 'PATH_EXO', obj%path_exo,  'exo', REQUIRED=.true., &
                             show=PATHCHECK_INFO_INPUT )
    endif

    if( obj%path_exo /= PATHCHECK_EMPTY ) then
        call path_parse( obj%path_exo, userid, node, dir, file )
        userid = ' '
        node   = ' '
        call path_build( obj%path_exo, userid, node, dir, file )
    endif

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

    call pc_put_options_field( 'MODE', (/ 'CALC  ', 'APPLY ', 'REMOVE' /), 3 )
    call pc_put_options_field( 'STAT_TR'  , (/ 'MED'  , 'AVE'   /), 2 )
    call pc_put_options_field( 'STAT_WIN' , (/ 'MED'  , 'AVE'   /), 2 )
    call pc_put_options_field( 'OPT_PRINT', (/ 'YES'  , 'NO '   /), 2 )

    call pc_put( 'MODE'     , obj%mode      )
    call pc_put( 'GAIN_MAX' , obj%gain_max  )
    call pc_put( 'OPT_PRINT', obj%opt_print )
    call pc_put( 'STAT_TR'  , obj%stat_tr   )
    call pc_put( 'STAT_WIN' , obj%stat_win  )
    call pc_put( 'TR_MAX'   , obj%tr_max    )
    call pc_put( 'HDR_FLAG' , obj%hdr_flag  )
    call pc_put( 'HDR_OFF'  , obj%hdr_off   )
    call pc_put( 'OFF_INIT' , obj%off_init  )
    call pc_put( 'OFF_INC'  , obj%off_inc   )
    call pc_put( 'OFF_LAST' , obj%off_last  )
    call pc_put( 'OFF_TOT'  , obj%off_tot   )
    call pc_put( 'WIN_LEN'  , obj%win_len   )
    call pc_put( 'WIN_INC'  , obj%win_inc   )
    call pc_put( 'PATH_EXO' , obj%path_exo  )

    if( obj%mode == 'CALC' ) then
        call pc_put_sensitive_field_flag( 'OPT_PRINT', .true.  )
        call pc_put_sensitive_field_flag( 'STAT_TR'  , .true.  )
        call pc_put_sensitive_field_flag( 'STAT_WIN' , .true.  )
        call pc_put_sensitive_field_flag( 'TR_MAX'   , .true.  )
        call pc_put_sensitive_field_flag( 'OFF_INIT' , .true.  )
        call pc_put_sensitive_field_flag( 'OFF_INC'  , .true.  )
        call pc_put_sensitive_field_flag( 'OFF_LAST' , .true.  )
        call pc_put_sensitive_field_flag( 'OFF_TOT'  , .true.  )
        call pc_put_sensitive_field_flag( 'WIN_LEN'  , .true.  )
        call pc_put_sensitive_field_flag( 'WIN_INC'  , .true.  )
        call pc_put_sensitive_field_flag( 'GAIN_MAX' , .false. )
    else
        call pc_put_sensitive_field_flag( 'OPT_PRINT', .false. )
        call pc_put_sensitive_field_flag( 'STAT_TR'  , .false. )
        call pc_put_sensitive_field_flag( 'STAT_WIN' , .false. )
        call pc_put_sensitive_field_flag( 'TR_MAX'   , .false. )
        call pc_put_sensitive_field_flag( 'OFF_INIT' , .false. )
        call pc_put_sensitive_field_flag( 'OFF_INC'  , .false. )
        call pc_put_sensitive_field_flag( 'OFF_LAST' , .false. )
        call pc_put_sensitive_field_flag( 'OFF_TOT'  , .false. )
        call pc_put_sensitive_field_flag( 'WIN_LEN'  , .false. )
        call pc_put_sensitive_field_flag( 'WIN_INC'  , .false. )
        call pc_put_sensitive_field_flag( 'GAIN_MAX' , .true.  )
    endif

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

!---get logical unit numbers for printing and open temporary print file

    lu_print = pc_get_lun()
    call getlun( obj%lu_tpnt, ierr0 )

    if( ierr0 == 0 ) then
        open( unit=obj%lu_tpnt, action='READWRITE', iostat=ierr0, &
              status='SCRATCH' )
        if( ierr0 /= 0 ) then
            call pc_error( 'temp print file open failed' )
            return
        endif
    else
        call pc_error( 'temp print getlun failed' )
        return
    endif

    if( obj%mode == 'CALC' ) then

    !---open output text file for write
        if( obj%path_exo /= PATHCHECK_EMPTY ) then
            call getlun( obj%lu_data, ierr0 )

            if( ierr0 == 0 ) then
                open( unit=obj%lu_data, file=obj%path_exo, action='WRITE', &
                      iostat=ierr0, status='REPLACE' )
                if( ierr0 /= 0 ) then
                    call pc_error( 'output text file open failed' )
                    return
                endif
            else
                call pc_error( 'output getlun failed' )
                return
            endif
        endif

    !---Calculate remaining dependent variables

        obj%nwin = ( obj%ndpt - obj%nwlen - 2 ) / obj%nwinc + 2

        if( obj%stat_tr == 'MED' ) then
            obj%ntrc = obj%tr_max
            obj%maxtrc = 0
        else
            obj%ntrc = 1
            obj%maxtrc = 1
        endif

    !---allocate storage

        allocate( obj%times(obj%off_tot)                  , stat = ierr(1) )
        allocate( obj%amp  (obj%nwin,obj%off_tot)         , stat = ierr(2) )
        allocate( obj%buf  (obj%nwin,obj%off_tot,obj%ntrc), stat = ierr(3) )

        do i = 1, 3
            if( ierr(i) /= 0 ) then
                call pc_error( 'exo_update: memory allocation error' )
                return
            endif
        end do

        obj%times = -HUGE( obj%times(1) )
        obj%amp   =  0.0
        obj%buf   =  0.0

    else

    !---open input text file for read
        call getlun( obj%lu_data, ierr0 )

        if( ierr0 == 0 ) then
            open( unit=obj%lu_data, file=obj%path_exo, action='READ', &
                  iostat=ierr0, status='OLD' )
            if( ierr0 /= 0 ) then
                call pc_error( 'input text file open failed' )
                return
            endif
        else
            call pc_error( 'input getlun failed' )
            return
        endif

        dt_sav = obj%dt
        ts_sav = obj%tstrt

        call exo_get_real( obj%lu_data, 'TSTRT'   , obj%tstrt    )
        call exo_get_real( obj%lu_data, 'DT'      , obj%dt       )

        if( obj%tstrt /= ts_sav .or. obj%dt /= dt_sav ) then
            call pc_error( 'Either the global TSTRT or DT does not agree with&
                           &the exo data file' )
            return
        endif

        call exo_get_real( obj%lu_data, 'WIN_LEN' , obj%win_len  )
        call exo_get_real( obj%lu_data, 'WIN_INC' , obj%win_inc  )
        call exo_get_real( obj%lu_data, 'OFF_INIT', obj%off_init )
        call exo_get_real( obj%lu_data, 'OFF_INC' , obj%off_inc  )
        call exo_get_int ( obj%lu_data, 'OFF_TOT' , obj%off_tot  )
        call exo_get_int ( obj%lu_data, 'HDR_OFF' , obj%hdr_off  )
        call exo_get_int ( obj%lu_data, 'NWIN'    , obj%nwin     )
        call exo_get_int ( obj%lu_data, 'NWLEN'   , obj%nwlen    )
        call exo_get_int ( obj%lu_data, 'NWINC'   , obj%nwinc    )

        if( pc_do_not_process_traces() ) return

    !---allocate storage

        allocate( obj%times(obj%off_tot)                  , stat = ierr(1) )
        allocate( obj%amp  (obj%ndpt,obj%off_tot)         , stat = ierr(2) )

        do i = 1, 2
            if( ierr(i) /= 0 ) then
                call pc_error( 'exo_update: memory allocation error' )
                return
            endif
        end do

        call exo_get_data( obj )
    endif


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine exo_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


subroutine exo_off_pattern_trap( keyword )          ! scalar trap
    implicit none
    character(len=*),intent(in) :: keyword          ! arguments

    character(len=MAX_LINE)     :: msg              ! local

    call pattern_init_last( keyword, object%off_init, object%off_last,         &
           object%off_inc, object%off_tot, msg,                                &
           inc_min  = tiny(object%off_inc), tot_min = 1)

    if(msg(1:1) /= ' ') then
        call pc_error(msg)
        call pc_jump_field( keyword )
    end if

    return
end subroutine exo_off_pattern_trap


subroutine exo_win_len_trap( keyword )              ! scalar trap
    implicit none
    character(len=*),intent(in) :: keyword          ! arguments

    object%nwlen   = nint( object%win_len / object%dt )
    object%win_len = object%nwlen * object%dt
    object%nwinc   = object%nwlen
    if (pc_get_update_state() == PC_GUI) then
         object%win_inc = object%win_len
         call pc_jump_field( 'WIN_INC' )
    end if

    return
end subroutine exo_win_len_trap


subroutine exo_win_inc_trap( keyword )              ! scalar trap
    implicit none
    character(len=*),intent(in) :: keyword          ! arguments

    object%nwinc   = nint( object%win_inc / object%dt )
    object%win_inc = object%nwinc * object%dt

    return
end subroutine exo_win_inc_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


subroutine exo( obj, ntr, hd, tr )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(exo_struct), intent(inout) :: obj
    integer,          intent(inout) :: ntr
    double precision, intent(inout) :: hd(:,:)
    real,             intent(inout) :: tr(:,:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!
!-------------------------------------------------------------------------------
!
    if( ntr <= 0 ) then
        call exo_wrapup( obj )
        return
    end if

    if( obj%mode == 'CALC' ) then
        call exo_calc( obj, ntr, hd, tr )
    else
        call exo_apply( obj, ntr, hd, tr )
    endif

    if( ntr == FATAL_ERROR ) then
        call exo_wrapup( obj )
        return
    end if
!
!-------------------------------------------------------------------------------
!
    return
end subroutine exo


!!------------------------------ get real ----------------------------------!!
!!------------------------------ get real ----------------------------------!!
!!------------------------------ get real ----------------------------------!!


subroutine exo_get_real( lun, keyword, value )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer          :: lun
    character(len=*) :: keyword
    real             :: value
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer           :: ierr
    integer           :: k
    integer           :: l
    character(len=80) :: line
!
!-------------------------------------------------------------------------------
!
    l = len( keyword )

    do
        read( lun, '(A80)', IOSTAT=ierr ) line

        if( ierr /= 0 ) then
            call pc_error( '*** EXO_GET_REAL I/O ERROR' )
            exit
        endif

        if( line(1:4) == 'DATA' ) exit

        if( line(1:l) == keyword ) then
            k = index( line, '=' )
            read( line(k+1:80), *, IOSTAT=ierr ) value
            if( ierr /= 0 ) call pc_error( '*** EXO_GET_REAL CONVERSION ERROR' )
            exit
        endif
    end do
!
!-------------------------------------------------------------------------------
!
    rewind( lun )
    return
end subroutine exo_get_real


!!------------------------------ get integer -------------------------------!!
!!------------------------------ get integer -------------------------------!!
!!------------------------------ get integer -------------------------------!!


subroutine exo_get_int( lun, keyword, value )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer          :: lun
    character(len=*) :: keyword
    integer          :: value
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer           :: ierr
    integer           :: k
    integer           :: l
    character(len=80) :: line
!
!-------------------------------------------------------------------------------
!
    l = len( keyword )

    do
        read( lun, '(A80)', IOSTAT=ierr ) line

        if( ierr /= 0 ) then
            call pc_error( '*** EXO_GET_INT I/O ERROR' )
            exit
        endif

        if( line(1:4) == 'DATA' ) exit

        if( line(1:l) == keyword ) then
            k = index( line, '=' )
            read( line(k+1:80), *, IOSTAT=ierr ) value
            if( ierr /= 0 ) call pc_error( '*** EXO_GET_INT CONVERSION ERROR' )
            exit
        endif
    end do
!
!-------------------------------------------------------------------------------
!
    rewind( lun )
    return
end subroutine exo_get_int


!!------------------------------ get data ----------------------------------!!
!!------------------------------ get data ----------------------------------!!
!!------------------------------ get data ----------------------------------!!


subroutine exo_get_data( obj )

    implicit none

!--------------------------------------------------
!   D u m m y   A r g u m e n t s
!--------------------------------------------------
    type(exo_struct), intent(inout)       :: obj
!--------------------------------------------------
!   L o c a l   V a r i a b l e s
!--------------------------------------------------
    real, dimension(obj%nwin+2)           :: a
    real, dimension(obj%nwin,obj%off_tot) :: a0
    real                                  :: ampl

    integer                               :: i1
    integer                               :: i2
    integer                               :: ierr
    integer                               :: iwin
    integer                               :: joff

    character(len=80)                     :: line
    integer                               :: n
    integer, dimension(obj%off_tot)       :: n0
    real                                  :: offset
    real, dimension(obj%nwin+2)           :: t
    real, dimension(obj%nwin,obj%off_tot) :: t0
    real                                  :: time
    real                                  :: tlast
    real, dimension(obj%ndpt)             :: tp
!
!-------------------------------------------------------------------------------
!
    a0 = 0.0
    t0 = 0.0
    n0 = 0.0

!---move to record after 'DATA'
    do
        read( obj%lu_data, '(A80)', IOSTAT=ierr ) line

        if( ierr /= 0 ) then
            call pc_error( '*** EXO_GET_ARRAYS I/O ERROR' )
            return
        endif

        if( line(1:4) == 'DATA' ) exit
    end do

!---read the data until end-of-file
    obj%times(:) = FNIL

    do
        read( obj%lu_data, *, IOSTAT=ierr ) joff, iwin, offset, time, ampl

        if     ( ierr > 0 ) then
            call pc_error( '*** EXO_GET_ARRAYS I/O ERROR' )
            return
        else if( ierr < 0 ) then
            exit
        endif

        if (iwin == 1) obj%times(joff) = time

        a0(iwin,joff) = ampl
        t0(iwin,joff) = time
        n0(joff)      = n0(joff) + 1
    end do

    close( obj%lu_data )

!---interpolate amplitudes for each offset
    tlast = obj%tstrt + obj%dt * (obj%ndpt - 1)

    do joff = 1, obj%off_tot

        n = n0(joff)

        if( t0(1,joff) > obj%tstrt ) then
            t(1) = obj%tstrt
            a(1) = a0(1,joff)
            i1   = 2
            i2   = n + 1
        else
            i1   = 1
            i2   = n
        endif

        t(i1:i2) = t0(1:n,joff)
        a(i1:i2) = a0(1:n,joff)

        if( t0(i2,joff) < tlast ) then
            i2    = i2 + 1
            t(i2) = tlast
            a(i2) = a0(n,joff)
        endif

        call interp_1d_var_lin_real( t, a, i2, tp, obj%amp(:,joff), &
                                     obj%ndpt, obj%tstrt, tlast )

    end do

    if( obj%mode == 'APPLY' ) then
        where( obj%amp > 0.0 ) obj%amp = 1.0 / obj%amp
    endif

    where( obj%amp > obj%gain_max ) obj%amp = obj%gain_max
!
!-------------------------------------------------------------------------------
!
    return
end subroutine exo_get_data


!!----------------------------- write file ---------------------------------!!
!!----------------------------- write file ---------------------------------!!
!!----------------------------- write file ---------------------------------!!


subroutine exo_write_file( obj )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(exo_struct), intent(inout) :: obj
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: iwin
    integer :: joff
    integer :: n
    real    :: offset
    real    :: time
!
!-------------------------------------------------------------------------------
!
    if( obj%lu_data > 0 ) then
        write( obj%lu_data, '( "WIN_LEN  = ", e15.5 )' ) obj%win_len
        write( obj%lu_data, '( "WIN_INC  = ", e15.5 )' ) obj%win_inc
        write( obj%lu_data, '( "HDR_OFF  = ", i5    )' ) obj%hdr_off
        write( obj%lu_data, '( "OFF_INIT = ", e15.5 )' ) obj%off_init
        write( obj%lu_data, '( "OFF_INC  = ", e15.5 )' ) obj%off_inc
        write( obj%lu_data, '( "OFF_TOT  = ", i5    )' ) obj%noff_max
        write( obj%lu_data, '( "TSTRT    = ", e15.5 )' ) obj%tstrt
        write( obj%lu_data, '( "DT       = ", e15.5 )' ) obj%dt
        write( obj%lu_data, '( "NWIN     = ", i5    )' ) obj%nwin_max
        write( obj%lu_data, '( "NWLEN    = ", i5    )' ) obj%nwlen
        write( obj%lu_data, '( "NWINC    = ", i5    )' ) obj%nwinc
        write( obj%lu_data, '( "DATA     = "        )' )
    endif

    if( obj%opt_print ) then
        write( obj%lu_tpnt, '( / 80("*") // "EXO CALC MODE RESULTS" / )' )
        write( obj%lu_tpnt, '( "WIN_LEN  = ", e15.5 )' ) obj%win_len
        write( obj%lu_tpnt, '( "WIN_INC  = ", e15.5 )' ) obj%win_inc
        write( obj%lu_tpnt, '( "HDR_OFF  = ", i5    )' ) obj%hdr_off
        write( obj%lu_tpnt, '( "OFF_INIT = ", e15.5 )' ) obj%off_init
        write( obj%lu_tpnt, '( "OFF_INC  = ", e15.5 )' ) obj%off_inc
        write( obj%lu_tpnt, '( "OFF_TOT  = ", i5    )' ) obj%noff_max
        write( obj%lu_tpnt, '( "TSTRT    = ", e15.5 )' ) obj%tstrt
        write( obj%lu_tpnt, '( "DT       = ", e15.5 )' ) obj%dt
        write( obj%lu_tpnt, '( "NWIN     = ", i5    )' ) obj%nwin_max
        write( obj%lu_tpnt, '( "NWLEN    = ", i5    )' ) obj%nwlen
        write( obj%lu_tpnt, '( "NWINC    = ", i5    )' ) obj%nwinc
        write( obj%lu_tpnt, '( / " IOFF IWIN      OFFSET   &
                                 &      TIME           AMPLITUDE" / )' )
    endif

    do joff = 1, obj%off_tot
        offset=mth_bin_center(obj%off_init,obj%off_inc,joff)
!rev8        offset = obj%off_init + (joff - 1) * obj%off_inc
        time   = obj%times(joff)
        do iwin = 1, obj%nwin
            n = obj%amp(iwin,joff)
            if( n > 0 ) then
                if( obj%stat_tr == 'MED' ) then
                    call median( obj%buf(iwin,joff,1:n), n, obj%amp(iwin,joff) )
                else
                    obj%amp(iwin,joff) = obj%buf(iwin,joff,1) / n
                endif

                if( obj%lu_data > 0 ) write( obj%lu_data, '(2i5,3e15.7)' ) &
                                    joff, iwin, offset, time, obj%amp(iwin,joff)
                if( obj%opt_print   ) write( obj%lu_tpnt, '(2i5,3e15.5)' ) &
                                    joff, iwin, offset, time, obj%amp(iwin,joff)
            endif
            time = time + obj%nwinc * obj%dt
        end do
    end do
!
!-------------------------------------------------------------------------------
!
    if( obj%opt_print   ) write( obj%lu_tpnt, '( / 80("*") / )' )
    if( obj%lu_data > 0 ) close( obj%lu_data )

    return
end subroutine exo_write_file


!!------------------------------ calculate ---------------------------------!!
!!------------------------------ calculate ---------------------------------!!
!!------------------------------ calculate ---------------------------------!!


subroutine exo_calc( obj, ntr, hd, tr )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(exo_struct), intent(inout) :: obj
    integer,          intent(inout) :: ntr
    double precision, intent(inout) :: hd(:,:)
    real,             intent(inout) :: tr(:,:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i
    integer :: i1
    integer :: i2
    integer :: ibot
    integer :: itop
    integer :: iwin
    integer :: joff
    integer :: jtr
    integer :: ktrc
    integer :: n
    real    :: stat
    real    :: sum
    real    :: tmp(obj%ndpt)
!
!-------------------------------------------------------------------------------
!
    do jtr = 1, ntr
        if( hd(25,jtr) <= 0.0D0 ) cycle

        if (obj%hdr_flag > 0) then
          if (hd(obj%hdr_flag,jtr) == 0.0d0) cycle
        end if

        itop = hd( 2,jtr)
        ibot = hd(64,jtr)
        if( itop < 1 .or. ibot > obj%ndpt .or. itop > ibot ) then
            write( obj%lu_tpnt, * )
            write( obj%lu_tpnt, * ) '***** ERROR: Invalid mutes - trace ignored&
                                    & *****'
            write( obj%lu_tpnt, '( "***** TOP_MUTE, BOT_MUTE = ", 2i5 )' ) &
                                itop, ibot
            write( obj%lu_tpnt, '( "***** HD(1), HD(3) = ", 2f10.2 )' ) &
                                hd(1,jtr), hd(3,jtr)
            write( obj%lu_tpnt, '( 80("*")/ )' )
            cycle
        endif

        joff=mth_bin_number(dble(obj%off_init),dble(obj%off_inc),&
                            hd(obj%hdr_off,jtr))
!rev8     joff = nint( (hd(obj%hdr_off,jtr) - obj%off_init) / obj%off_inc ) + 1
        if( joff < 1 .or. joff > obj%off_tot ) cycle

        iwin = 0
   !    i1   = itop                                ! removed 2004-04-14
        i1   = itop - obj%nwinc                    ! added 2004-04-14
        i2   = 0
        do while( i2 < ibot )
            iwin = iwin + 1
            i1   = i1 + obj%nwinc                  ! added 2004-04-14
            i2   = min0( i1+obj%nwlen, ibot )

            if( obj%times(joff) == -HUGE( obj%times(1) ) ) then
                obj%times(joff) = obj%tstrt + (i1 - 1. + 0.5*obj%nwlen) * obj%dt
            endif

            if( obj%stat_win == 'MED' ) then
                n = 0
                do i = i1, i2
                    if( tr(i,jtr) /= 0.0 ) then
                        n      = n + 1
                        tmp(n) = abs( tr(i,jtr) )
                    endif
                end do

                if( n > 0 ) then
                    call median( tmp, n, stat )
                else
                    cycle
                endif
            else
                n   = 0
                sum = 0.0
                do i = i1, i2
                    if( tr(i,jtr) /= 0.0 ) then
                        n   = n + 1
                        sum = sum + abs( tr(i,jtr) )
                    endif
                end do

                if( n > 0 ) then
                    stat = sum / n
                else
                    cycle
                endif
            endif

            obj%amp(iwin,joff) = obj%amp(iwin,joff) + 1.0

            if( obj%stat_tr == 'MED' ) then
                ktrc = obj%amp(iwin,joff)
                if (ktrc > obj%ntrc) then
                     call pc_error ('EXO: Too many traces in offset bin.')
                     call pc_error ('EXO: Parameter TR_MAX is too small.')
                     call pc_error ('EXO: Parameter TR_MAX =',obj%tr_max)
                     ntr = FATAL_ERROR
                     return
                end if
                obj%buf(iwin,joff,ktrc) = stat
                obj%maxtrc = max(obj%maxtrc,ktrc)
            else
                obj%buf(iwin,joff,1) = obj%buf(iwin,joff,1) + stat
            endif

            if( iwin > obj%nwin_max ) obj%nwin_max = iwin
            if( joff > obj%noff_max ) obj%noff_max = joff

  !         i1 = i1 + obj%nwinc                  ! removed 2004-04-14
        end do
    end do
!
!-------------------------------------------------------------------------------
!
    return
end subroutine exo_calc


!!--------------------------- apply or remove ------------------------------!!
!!--------------------------- apply or remove ------------------------------!!
!!--------------------------- apply or remove ------------------------------!!


subroutine exo_apply( obj, ntr, hd, tr )

    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(exo_struct), intent(inout) :: obj
    integer,          intent(inout) :: ntr
    double precision, intent(inout) :: hd(:,:)
    real,             intent(inout) :: tr(:,:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: joff
    integer :: jtr
    integer :: ishift,indx,indx2,itop
!
!-------------------------------------------------------------------------------
!
    do jtr = 1, ntr
        joff=mth_bin_number(dble(obj%off_init),dble(obj%off_inc),&
                            hd(obj%hdr_off,jtr))
!rev8     joff = nint( (hd(obj%hdr_off,jtr) - obj%off_init) / obj%off_inc ) + 1
        if( joff < 1 .or. joff > obj%off_tot ) cycle

        if (obj%hdr_flag > 0) then
          if (hd(obj%hdr_flag,jtr) == 0.0d0) cycle
        end if

!rev9   tr(:,jtr) = tr(:,jtr) * obj%amp(:,joff)

        itop   = nint((obj%times(joff)-obj%tstrt)/obj%dt) + 1 - obj%nwlen/2
        ishift = nint(hd(2,jtr)) - itop

        do indx = 1,obj%ndpt
             indx2 = indx - ishift
             call mth_constrain (indx2,1,obj%ndpt)
             tr(indx,jtr) = tr(indx,jtr) * obj%amp(indx2,joff)
        end do
    end do

    call lav_set_hdr( hd, tr, obj%ndpt, ntr )
!
!-------------------------------------------------------------------------------
!
    return
end subroutine exo_apply


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


subroutine exo_wrapup( obj )
    implicit none
    type(exo_struct),intent(inout) :: obj       ! dummy variable

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    if( pc_do_not_process_traces() ) return

    if( obj%mode == 'CALC' ) call exo_write_file( obj )

    call exo_print( obj )

    return
end subroutine exo_wrapup


!!--------------------------- print temp file ------------------------------!!
!!--------------------------- print temp file ------------------------------!!
!!--------------------------- print temp file ------------------------------!!


subroutine exo_print( obj )
    implicit none
    type(exo_struct),intent(inout) :: obj       ! dummy variable

    integer                        :: i         ! local
    integer                        :: ierr      ! local - return condition code
    character(len=MAX_LINE)        :: line      ! local - line buffer
    integer                        :: n         ! local

!---dump temporary printer file to the printer

    endfile obj%lu_tpnt
    rewind  obj%lu_tpnt

    write( lu_print, '( " ", 79("*") )' )
    write( lu_print, '( " -------> EXO PROCESS OUTPUT <------- " / )' )

    do
        read(  obj%lu_tpnt, '( A132 )', iostat=ierr ) line
        if( ierr /= 0 ) exit

        n = MAX_LINE
        do while( line(n:n) == ' ' .and. n > 1 )
            n = n - 1
        end do

        write( lu_print, '( 160A )' ) (line(i:i), i = 1, n)
    end do

    if( obj%stat_tr == 'MED' ) then
         call pc_print (' ')
         call pc_print ('Maximum number of offset bins =',obj%maxtrc)
         call pc_print ('Parameter TR_MAX was set to',obj%tr_max)
         call pc_print (' ')
    end if

    write( lu_print, '( / " -------> END EXO PROCESS <------- " )' )
    write( lu_print, '( " ", 79("*") )' )

    return
end subroutine exo_print


!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!


subroutine exo_dump_struct( obj )
    implicit none

    type(exo_struct),intent(inout) :: obj       ! dummy variable

    write( lu_print, '( / " ***** DUMP STRUCTURE ***** " / )' )

    write( lu_print, '( " skip_wrapup = ", l1       )' ) obj%skip_wrapup
    write( lu_print, '( " mode        = ", a6       )' ) obj%mode
    write( lu_print, '( " gain_max    = ", e15.5    )' ) obj%gain_max
    write( lu_print, '( " opt_print   = ", l1       )' ) obj%opt_print
    write( lu_print, '( " stat_tr     = ", a3       )' ) obj%stat_tr
    write( lu_print, '( " stat_win    = ", a3       )' ) obj%stat_win
    write( lu_print, '( " tr_max      = ", i6       )' ) obj%tr_max
    write( lu_print, '( " win_len     = ", f10.3    )' ) obj%win_len
    write( lu_print, '( " win_inc     = ", f10.3    )' ) obj%win_inc
    write( lu_print, '( " hdr_off     = ", i6       )' ) obj%hdr_off
    write( lu_print, '( " off_init    = ", f10.3    )' ) obj%off_init
    write( lu_print, '( " off_inc     = ", f10.3    )' ) obj%off_inc
    write( lu_print, '( " off_last    = ", f10.3    )' ) obj%off_last
    write( lu_print, '( " off_tot     = ", i6       )' ) obj%off_tot
    write( lu_print, '( " path_exo    = ", a60      )' ) obj%path_exo
    write( lu_print, '( " nwih        = ", i6       )' ) obj%nwih
    write( lu_print, '( " ndpt        = ", i6       )' ) obj%ndpt
    write( lu_print, '( " tstrt       = ", f10.3    )' ) obj%tstrt
    write( lu_print, '( " dt          = ", f10.3    )' ) obj%dt
    write( lu_print, '( " ntrc        = ", i6       )' ) obj%ntrc
    write( lu_print, '( " maxtrc      = ", i6       )' ) obj%maxtrc
    write( lu_print, '( " nwin        = ", i6       )' ) obj%nwin
    write( lu_print, '( " nwlen       = ", i6       )' ) obj%nwlen
    write( lu_print, '( " nwinc       = ", i6       )' ) obj%nwinc
    write( lu_print, '( " lu_tpnt     = ", i6       )' ) obj%lu_tpnt
    write( lu_print, '( " lu_data     = ", i6       )' ) obj%lu_data
    write( lu_print, * )

    write( lu_print, '( / " ***** END OF DUMP STRUCTURE ***** " / )' )

    return
end subroutine exo_dump_struct

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module exo_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

