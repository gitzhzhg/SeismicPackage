!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 1999-12-15. />

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
! Name       : FKAP   (raise F-K Amplitudes to a Power)
! Category   : filters
! Written    : 1989-01-11   by: Bill Troutt
! Revised    : 2000-12-08   by: Tom Stoeckley
! Maturity   : production   2001-04-30
! Purpose    : Coherence enhancement by raising F-K amplitudes to a power.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! FKAP does coherence enhancement by taking trace panels in x-t space, 
! transforming them to F-K space, raising the F-K amplitudes to a power and 
! then inverse transforming back to x-t space.  
!
! This works because random noise in x-t space has no preferred dip or 
! frequency and thus is spread over the F-K plane in low amplitude values, with
! coherent x-t events being mapped to high amplitude F-K events.  Raising the 
! F-K amplitudes to a power greater than 1.0 accentuates the amplitude 
! distinction between coherent events and noise.  (Raising the F-K amplitudes 
! to a power less than 1.0 reduces the amplitude distinction between coherent 
! events and noise.) 
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Input traces should be ungathered.  Input trace panels are defined by 
! HDR_PANEL, PANEL_INIT and PANEL_INC.  The number of traces in a panel should
! not exceed TR_MAX.
!
! Normally FKAP is used on shot profiles, receiver profiles and stacked 
! sections.
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
! This process outputs the same traces as it receives (altered).
!
! This process outputs one trace at a time.
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
! NWIH     number of words in header             used but not changed
! NDPT     number of sample values in trace      used but not changed
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
!
! 2       Head mute index            used but not changed
! 25      largest absolute value     set
! 64      Tail mute index            used but not changed
! 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author     Description
!     ----       ------     -----------
! 13. 2001-04-30 Stoeckley  Change wrapup flag.
! 12. 2000-07-26 Coleman    Changed variable names and other housekeeping.
! 11. 2000-05-19 Coleman    Converted to new system.
! 10. 1998-12-18 Vunderink  Begin using the f90 compiler.
! 9.  1997-03-19 Vunderink  Increased memory used by X2KCC and T2FRC
!                           primitives
! 8.  1992-03-19 Troutt     Add tail mute restore (call MUTEHW).  The mute
!                           restoration here reapplies a 60-mil taper.
!                           This code has been changed to match MUTE's
!                           algorithm.
! 7.  1989-11-16 Troutt     Added explicit wrap-up logic for NTR=0 (report
!                           number of groups processed).
! 6.  1989-09-12 Troutt     Changed logic to use storage for working trace
!                           (and header) arrays instead of using the 2ndary
!                           arrays in the FKAP call.  The 2ndary arrays
!                           were renamed to HDR2 and TR2 and are used to
!                           output one trace at a time via an internal call
!                           to GATHR.  All of the main logic for the old
!                           2ndary arrays (HDR1, TR1) remains intact,
!                           except that they now reside in storage via
!                           pointers.
! 5.  1989-08-18 Troutt     Honor TSTRT in TMAX calculation.
! 4.  1989-03-08 Troutt     Fixed call to REPP to reference PWR instead of
!                           NTRT.
! 3.  1989-02-14 Troutt     Fixed check on header word 32.  HDR1 was HDR.
! 2.  1989-01-30 Troutt     Changed from using process FKPWR to using new
!                           internal subroutine FKAPWR.  Eliminates use of
!                           one extra IPN(6), and should reduce overhead.
! 1.  1989-01-11 Troutt     Added mute re-application and test to see that
!                           #TPG not violated.
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
!
! This process uses two sets of trace and header arrays.
!
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!    NTR == NEED_TRACES    if this process needs more traces.
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
!
!<gui_def>
!<NS fkap Process/NC=80>
!
!                       Raise F-K Amplitudes to a Power
!          Coherence enhancement by raising F-K amplitudes to a power
!
!                           PWR =~~~~~~~~`FFFFFFFFF
!
!                           FREQ_MAX =~~~`FFFFFFFFF
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
!
!<HelpSection>
!
!<Help KEYWORD="PWR">
!<Tip> Power to which F-K amplitudes are raised. </Tip>
! Default = 1.25
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency to use in the F-K transform. </Tip>
! Default = 0.8 * Nyquist
! Allowed = 0.0 < real =< Nyquist 
!</Help>
!
!<Help KEYWORD="TR_MAX">
!<Tip> Maximum number of traces per panel. </Tip>
! Default = -
! Allowed = int > 0
! TR_MAX is for storage allocation and not for defining input trace panels.  
! FKAP will abort if any input panel exceeds TR_MAX traces.
!</Help>
!
!<Help KEYWORD="HDR_PANEL">
!<Tip> Header word designating input panels. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="PANEL_INIT">
!<Tip> Value of HDR_PANEL for the first panel. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="PANEL_INC">
!<Tip> Increment in HDR_PANEL between panels. </Tip>
! Default = 1.0
! Allowed = real
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
! FKAP will not be able to call GATHR internally for option 4 since it is 
! unavailable.  FKAP will have to keep track of input traces by itself (unless
! a primitive is written).



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module fkap_module

    use pc_module
    use named_constants_module
    use fft_module
    use mutehw_module
    use lav_module
    
    implicit none

    private
    public  :: fkap_create
    public  :: fkap_initialize
    public  :: fkap_update
    public  :: fkap_delete
    public  :: fkap_dump_object
!<execute_only>
    public  :: fkap            ! main execution (trace processing) routine.
    private :: fkap_apply_operator
    private :: fkap_put_trace
    private :: fkap_get_trace
    public  :: fkap_wrapup
!</execute_only>


    character(len=100),public,save :: fkap_IDENT = &
       '$Id: fkap.f90,v 1.13 2001/04/26 15:34:17 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    type,public :: fkap_struct              
 
        private
        logical                     :: skip_wrapup      ! wrapup flag.
        real                        :: pwr              ! process parameter
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
        integer                     :: nout             ! dependent variable
        integer                     :: nseq             ! dependent variable
        integer                     :: ntap             ! dependent variable
        integer                     :: nrcfftr          ! dependent variable
        integer                     :: nrcfftc          ! dependent variable
        integer                     :: nccfft           ! dependent variable
        integer                     :: nfkeep           ! dependent variable
        double precision, pointer   :: hdsav(:)         ! dependent variable
        real, pointer               :: trsav(:)         ! dependent variable
        double precision, pointer   :: hdbuf(:,:)       ! dependent variable
        complex, pointer            :: trbuf(:,:)       ! dependent variable
        real, pointer               :: taper(:)         ! dependent variable
        type(fft_struct), pointer   :: frfft_obj        ! dependent variable
        type(fft_struct), pointer   :: irfft_obj        ! dependent variable
        type(fft_struct), pointer   :: fcfft_obj        ! dependent variable
        type(fft_struct), pointer   :: icfft_obj        ! dependent variable

    end type fkap_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


    integer, save :: lu_print = 6


contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine fkap_create( obj )
    implicit none
    type(fkap_struct),pointer :: obj

    allocate( obj )

    nullify( obj%hdsav     )
    nullify( obj%trsav     )
    nullify( obj%hdbuf     )
    nullify( obj%trbuf     )
    nullify( obj%taper     )
    nullify( obj%frfft_obj )
    nullify( obj%irfft_obj )
    nullify( obj%fcfft_obj )
    nullify( obj%icfft_obj )

    call fkap_initialize( obj )
    
    return
end subroutine fkap_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine fkap_delete( obj )
    implicit none
    type(fkap_struct),pointer :: obj       ! arguments

!<execute_only>
    call fkap_wrapup( obj )
!</execute_only>

    if( associated( obj%hdsav ) ) deallocate( obj%hdsav )
    if( associated( obj%trsav ) ) deallocate( obj%trsav )
    if( associated( obj%hdbuf ) ) deallocate( obj%hdbuf )
    if( associated( obj%trbuf ) ) deallocate( obj%trbuf )
    if( associated( obj%taper ) ) deallocate( obj%taper )

    if( associated( obj%frfft_obj ) ) call fft_delete( obj%frfft_obj )
    if( associated( obj%irfft_obj ) ) call fft_delete( obj%irfft_obj )
    if( associated( obj%fcfft_obj ) ) call fft_delete( obj%fcfft_obj )
    if( associated( obj%icfft_obj ) ) call fft_delete( obj%icfft_obj )

    deallocate(obj)

    return
end subroutine fkap_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine fkap_initialize( obj )
    implicit none
    type(fkap_struct),intent(inout) :: obj

    lu_print = pc_get_lun()

    call pc_get_global( 'dt', obj%dt )
    obj%pwr        = 1.25
    obj%freq_max   = 0.4 / obj%dt   ! = 0.8 * fnyquist
    obj%tr_max     = 4
    obj%hdr_panel  = 8
    obj%panel_init = 1.0
    obj%panel_inc  = 1.0
    obj%dt         = 0.0
    obj%ndpt       = 0
    obj%nwih       = 0
    obj%igrp       = 0
    obj%ncount     = 0
    obj%nout       = 0
    obj%nseq       = 0
    obj%ntap       = 0
    obj%nrcfftr    = 0
    obj%nrcfftc    = 0
    obj%nccfft     = 0
    obj%nfkeep     = 0
    obj%more       = .true.
    obj%inmode     = .true.

    call fkap_update( obj )

    return
end subroutine fkap_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine fkap_update( obj )
    implicit none

    type(fkap_struct),intent(inout),target :: obj

!-------------------------------------------------------------
! LOCAL VARIABLES
!-------------------------------------------------------------
    integer :: i
    integer :: ierr1
    integer :: ierr2
    integer :: ierr3
    integer :: ierr4
    integer :: ierr5
    real    :: fac
    real    :: fnyq
    real    :: tpl
    real    :: twopi
!-------------------------------------------------------------------------------

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get_global( 'ndpt' , obj%ndpt  )
    call pc_get_global( 'nwih' , obj%nwih  )
    call pc_get_global( 'dt'   , obj%dt    )

    call pc_get( 'pwr'       , obj%pwr        )
    call pc_get( 'freq_max'  , obj%freq_max   )
    call pc_get( 'tr_max'    , obj%tr_max     )
    call pc_get( 'hdr_panel' , obj%hdr_panel  )
    call pc_get( 'panel_init', obj%panel_init )
    call pc_get( 'panel_inc' , obj%panel_inc  )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


    if( obj%dt <= 0.0 ) then
        call pc_error( '*** FKAP: Invalid value for global dt: ', obj%dt )
        return
    endif

    if( obj%ndpt <= 0 ) then
        call pc_error( '*** FKAP: Invalid value for global ndpt: ', obj%ndpt )
        return
    endif

    if( obj%nwih <= 0 ) then
        call pc_error( '*** FKAP: Invalid value for global nwih: ', obj%nwih )
        return
    endif

    if( obj%pwr <= 0.0 ) then
        call pc_error( '*** FKAP: Invalid value for pwr: ', obj%pwr )
        obj%pwr = 1.25
    endif

    fnyq = 0.5 / obj%dt
    if( obj%freq_max <= 0.0 .or. obj%freq_max > fnyq ) then
        call pc_error( '*** FKAP: Invalid value for freq_max: ', obj%freq_max )
        obj%freq_max = 0.8 * fnyq
    endif

    if( obj%tr_max <= 0.0 ) then
        call pc_error( '*** FKAP: Invalid value for tr_max: ', obj%tr_max )
        obj%tr_max = 0
    endif

    if( obj%hdr_panel <= 0 .or. obj%hdr_panel > obj%nwih ) then
        call pc_error( '*** FKAP: Invalid value for hdr_panel: ', obj%hdr_panel)
        obj%hdr_panel = 8
    endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

    call pc_put_control( 'need_request', .true. )
    call pc_put_control( 'need_label'  , .true. )

    call pc_put( 'pwr'       , obj%pwr        )
    call pc_put( 'freq_max'  , obj%freq_max   )
    call pc_put( 'tr_max'    , obj%tr_max      )
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
    obj%nrcfftr = fft_nfctr( obj%ndpt + mod(obj%ndpt,2) )
    obj%nrcfftc = obj%nrcfftr / 2 + 1
    obj%nccfft  = fft_nfctr( obj%tr_max )
    obj%nfkeep  = nint( obj%freq_max * obj%dt * obj%nrcfftr ) + 1
    obj%nfkeep  = min( obj%nfkeep + 4, obj%nrcfftc - 1 )
    obj%more    = .true.
    obj%inmode  = .true.

    twopi       = 2.0 * acos( -1.0 )
    tpl         = 0.06
    fac         = 0.25 * obj%dt / tpl
    obj%ntap    = tpl / obj%dt - 1.0e-6

!<execute_only>

    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

    allocate( obj%hdsav(obj%nwih)             , stat=ierr1 )
    allocate( obj%trsav(obj%nrcfftr)          , stat=ierr2 )
    allocate( obj%hdbuf(obj%nwih  ,obj%tr_max), stat=ierr3 )
    allocate( obj%trbuf(obj%tr_max,obj%nfkeep), stat=ierr4 )
    allocate( obj%taper(obj%ntap)             , stat=ierr5 )

    if( ierr1 /= 0 ) call pc_error( '*** FKAP: Error allocating HDBUF to ', &
                                    8 * obj%nwih, ' bytes' )
    if( ierr2 /= 0 ) call pc_error( '*** FKAP: Error allocating TRBUF to ', &
                                    4 * obj%nrcfftr, ' bytes' )
    if( ierr3 /= 0 ) call pc_error( '*** FKAP: Error allocating CHD to ',   &
                                    8 * obj%nwih * obj%tr_max, ' bytes' )
    if( ierr4 /= 0 ) call pc_error( '*** FKAP: Error allocating CTR to ',   &
                                    8 * obj%nrcfftc * obj%tr_max, ' bytes' )
    if( ierr5 /= 0 ) call pc_error( '*** FKAP: Error allocating TAPER to ', &
                                    4 * obj%ntap, ' bytes' )

    ierr1 = fft_create( obj%frfft_obj, -1, obj%nrcfftr, 'rtoc' )
    ierr2 = fft_create( obj%fcfft_obj, -1, obj%nccfft , 'ctoc' )
    ierr3 = fft_create( obj%icfft_obj,  1, obj%nccfft , 'ctoc' )
    ierr4 = fft_create( obj%irfft_obj,  1, obj%nrcfftr, 'ctor' )

    if( ierr1 /= 0 .or. ierr2 /= 0 .or. ierr3 /= 0 .or. ierr4 /= 0 ) &
        call pc_error( '*** FKAP: Error creating fft object' )

    if( pc_do_not_process_traces() ) return   ! in case of allocation errors.

    do i = 1, obj%ntap
        obj%taper(i) = sin( twopi * fac * i )
    end do

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine fkap_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

subroutine fkap( obj, ntr, hd, tr )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fkap_struct),intent(inout) :: obj
    integer          ,intent(inout) :: ntr
    double precision ,intent(inout) :: hd(:,:)
    real             ,intent(inout) :: tr(:,:)
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    integer                         :: jgrp
!-------------------------------------------------------------------------------

    select case( ntr )

    case( 1 )
        ! do a validity check
        if( .not. obj%inmode ) then
            ntr = FATAL_ERROR
            call fkap_wrapup( obj )
            return
        endif

        if( obj%ncount == 0 .and. obj%nseq > 0 ) then
            call fkap_put_trace( obj, ntr, obj%hdsav, obj%trsav )
            if( ntr == FATAL_ERROR ) return
            obj%igrp = nint( ( obj%hdsav(obj%hdr_panel) - obj%panel_init ) / &
                             obj%panel_inc + 1.0 )
        endif

        obj%trsav(1:obj%ndpt)  = tr(:,1)
        obj%trsav(obj%ndpt+1:) = 0.0

        jgrp = nint( ( hd(obj%hdr_panel,1) - obj%panel_init ) / &
                     obj%panel_inc + 1.0 )
        if( obj%ncount == 0 ) obj%igrp = jgrp

        if( jgrp == obj%igrp ) then
            call fkap_put_trace( obj, ntr, hd(:,1), obj%trsav )
            if( ntr == FATAL_ERROR ) return
            ntr = NEED_TRACES
        else
            obj%hdsav  = hd(:,1)
            obj%inmode = .false.
            obj%nout   = 0
            call fkap_apply_operator( obj )
            call fkap_get_trace( obj, ntr, hd(:,1), tr(:,1) )
        endif

    case( NO_MORE_TRACES )
        ! do a validity check
        if( .not. obj%inmode ) then
            ntr = FATAL_ERROR
            call fkap_wrapup( obj )
            return
        endif

        obj%more   = .false.
        obj%inmode = .false.
        obj%nout   = 0
        call fkap_apply_operator( obj )
        call fkap_get_trace( obj, ntr, hd(:,1), tr(:,1) )

    case( NEED_TRACES )
        ! do a validity check
        if( obj%inmode ) then
            ntr = FATAL_ERROR
            call fkap_wrapup( obj )
            return
        endif

        if( obj%nout < obj%ncount ) then
            call fkap_get_trace( obj, ntr, hd(:,1), tr(:,1) )
        else
            if( obj%more ) then
                obj%inmode = .true.
                obj%ncount = 0
                obj%nout   = 0
                ntr        = NEED_TRACES
            else
                ntr = NO_MORE_TRACES
                call fkap_wrapup( obj )
            endif
        endif

    case default
        ntr = FATAL_ERROR
        call fkap_wrapup( obj )

    end select

    return
end subroutine fkap


!</execute_only>


!!----------------------------- put trace ----------------------------------!!
!!----------------------------- put trace ----------------------------------!!
!!----------------------------- put trace ----------------------------------!!


!<execute_only>

subroutine fkap_put_trace( obj, ntr, hd, tr )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fkap_struct),intent(inout) :: obj
    integer          ,intent(inout) :: ntr
    double precision ,intent(inout) :: hd(:)
    real             ,intent(inout) :: tr(:)
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    complex, dimension(obj%nrcfftc) :: cv
    integer                         :: i
    integer                         :: i2
    real                            :: ramp
!-------------------------------------------------------------------------------


    obj%ncount = obj%ncount + 1
    if( obj%ncount > obj%tr_max ) then
        ntr = FATAL_ERROR
        call fkap_wrapup( obj )
        return
    endif

    call fft_rc_transform( obj%frfft_obj, tr, cv )   ! tx -> fx

!---apply a 10% ramp
    i2   = max( 1, obj%nfkeep - 8 )
    ramp = 0.0
    do i = obj%nfkeep, i2, -1
        ramp = ramp + 0.1
        cv(i) = cv(i) * ramp
    end do
    
    obj%trbuf(obj%ncount,:) = cv(1:obj%nfkeep)       ! NOTE: trbuf is transposed
    obj%hdbuf(:,obj%ncount) = hd

    return
end subroutine fkap_put_trace

!</execute_only>


!!----------------------------- get trace ----------------------------------!!
!!----------------------------- get trace ----------------------------------!!
!!----------------------------- get trace ----------------------------------!!


!<execute_only>

subroutine fkap_get_trace( obj, ntr, hd, tr )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fkap_struct),intent(inout) :: obj
    integer          ,intent(inout) :: ntr
    double precision ,intent(inout) :: hd(:)
    real             ,intent(inout) :: tr(:)
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    complex, dimension(obj%nrcfftc) :: cv
    integer                         :: i
    integer                         :: imute
    real                            :: scale
    real, dimension(obj%nrcfftr)    :: trout
!-------------------------------------------------------------------------------

    scale = 1.0 / float( obj%nrcfftr )

    obj%nout = obj%nout + 1
    obj%nseq = obj%nseq + 1

    cv(1:obj%nfkeep)  = obj%trbuf(obj%nout,:)
    cv(obj%nfkeep+1:) = ( 0.0, 0.0 )

    call fft_cr_transform( obj%irfft_obj, cv, trout, scale )   ! fx -> tx

    tr    = trout(1:obj%ndpt)
    hd    = obj%hdbuf(:,obj%nout)
    hd(1) = obj%nseq
    ntr   = 1

    call mutehw( hd, tr, obj%ndpt, 0.0, MUTEHW_BOTH )

!---if trace is not dead, then apply taper
    if( hd(HDR_TOP_MUTE) < obj%ndpt ) then

!-------apply top taper
        imute = nint( hd(HDR_TOP_MUTE) ) - 1
        do i = 1, min( obj%ntap, obj%ndpt - imute )
            tr(imute+i) = tr(imute+i) * obj%taper(i)
        end do

!-------apply bottom taper
        imute = nint( hd(HDR_BOTTOM_MUTE) ) + 1
        do i = 1, min( obj%ntap, imute - 1 )
            tr(imute-i) = tr(imute-i) * obj%taper(i)
        end do

    endif

    call lav_set_hdr( hd, tr, obj%ndpt )

    return
end subroutine fkap_get_trace

!</execute_only>



!!--------------------------- apply operator -------------------------------!!
!!--------------------------- apply operator -------------------------------!!
!!--------------------------- apply operator -------------------------------!!


!<execute_only>

subroutine fkap_apply_operator( obj )
    implicit none
!-----------------------------------------------------------
!   DUMMY VARIABLES
!-----------------------------------------------------------
    type(fkap_struct),intent(inout) :: obj
!-----------------------------------------------------------
!   LOCAL VARIABLES
!-----------------------------------------------------------
    complex, dimension(obj%nccfft)  :: cv
    integer                         :: j
    real                            :: pwrm
    real                            :: scale
!-------------------------------------------------------------------------------

    scale = 1.0 / float( obj%nccfft )

    pwrm = obj%pwr - 1
    do j = 1, obj%nfkeep
        cv(1:obj%ncount)  = obj%trbuf(1:obj%ncount,j)
        cv(obj%ncount+1:) = ( 0.0, 0.0 )

        call fft_cc_transform( obj%fcfft_obj, cv )    ! fx -> fk

        cv = cv * cabs( cv ) ** pwrm                  ! apply operator

        call fft_cc_transform( obj%icfft_obj, cv )    ! fk -> fx

        obj%trbuf(1:obj%ncount,j) = cv(1:obj%ncount) * scale
    end do

    return
end subroutine fkap_apply_operator

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

subroutine fkap_wrapup( obj )
    implicit none
    type(fkap_struct),intent(inout) :: obj       ! arguments

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.


    return
end subroutine fkap_wrapup

!</execute_only>


!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!


subroutine fkap_dump_object( obj )
    implicit none

    type(fkap_struct),intent(inout) :: obj

    write( lu_print, '( / " ***** DUMP OBJECT ***** " / )' )

    write( lu_print, '( " skip_wrapup = ", l1       )' ) obj%skip_wrapup
    write( lu_print, '( " pwr         = ", f8.3     )' ) obj%pwr
    write( lu_print, '( " freq_max    = ", f8.3     )' ) obj%freq_max
    write( lu_print, '( " tr_max      = ", i4       )' ) obj%tr_max
    write( lu_print, '( " hdr_panel   = ", i4       )' ) obj%hdr_panel
    write( lu_print, '( " panel_init  = ", f8.3     )' ) obj%panel_init
    write( lu_print, '( " panel_inc   = ", f8.3     )' ) obj%panel_inc
    write( lu_print, '( " nwih        = ", i4       )' ) obj%nwih
    write( lu_print, '( " ndpt        = ", i4       )' ) obj%ndpt
    write( lu_print, '( " dt          = ", f8.3     )' ) obj%dt
    write( lu_print, '( " more        = ", l1       )' ) obj%more
    write( lu_print, '( " inmode      = ", l1       )' ) obj%inmode
    write( lu_print, '( " ncount      = ", i4       )' ) obj%ncount
    write( lu_print, '( " nout        = ", i4       )' ) obj%nout
    write( lu_print, '( " nseq        = ", i4       )' ) obj%nseq
    write( lu_print, '( " ntap        = ", i4       )' ) obj%ntap
    write( lu_print, '( " nrcfftr     = ", i4       )' ) obj%nrcfftr
    write( lu_print, '( " nrcfftc     = ", i4       )' ) obj%nrcfftc
    write( lu_print, '( " nccfft      = ", i4       )' ) obj%nccfft 
    write( lu_print, '( " nfkeep      = ", i4       )' ) obj%nfkeep 

    write( lu_print, '( / " ***** END OF DUMP OBJECT ***** " / )' )

    return
end subroutine fkap_dump_object


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module fkap_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

