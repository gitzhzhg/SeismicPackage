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
! Name       : RFAB   (Residual Phase Balance)
! Category   : filters
! Written    : 1990-05-31   by: Bill Harlan
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Balance event phases in CMP gathers.
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
! RFAB will adjust the phases of events locally in order to optimize the 
! coherence of flattened events in the CMP gather.
!
!   1.  Data are modeled with flat events and with convolutional wavelets that 
!   change over both offset and time.   The data are modeled with the transform
!
!          data(x,t) = (sum over t') stack(t') wavelet(t - t', t', x).
!
!   Parameter defaults are set so that wavelets are allowed to change slowly
!   over the second dimension, t'.
!
!   2.  The estimated wavelets are used to deconvolve the original data and
!   encourage flat moveout.  The wavelets and flat events are estimated by 
!   least-squares inversion.  Deconvolving the original data optimizes its 
!   phase.
!
!
! Mode of Operation
!
! RFAB can operate in six different modes.
!
!   PHBAL     (Output phase optimized CMP gathers.)
!
!   STACK     (Output estimated stacked trace, one stacked trace for each CMP.)
!
!   EVENTS    (Output estimated stacked trace repeated over offset.)
!
!   MODEL     (Output modeled CMP gathers.  The model CMP gathers are created
!             by convolving the estimated wavelet for each offset with the 
!             stacked trace.)
!
!   WAVELETS  (Output estimated wavelets that vary with time and offset.)
!
!   RESID     (Output the input CMP gathers minus the modeled CMP gathers.)
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Input Traces
!
! RFAB requires input traces to be gathered into CMP gathers with NMO 
! correction applied.
!
! 
! Run-time
!
! RFAB is very CPU intensive.  Run-time may be reduced by limiting the time
! range of the input data and/or by reducing the number of wavelets.
!
!
! Setting NUM_WAVELETS
!
! For the same input trace length, a larger value of NUM_WAVELETS will increase
! output event coherence and a decrease in the value of NUM_WAVELETS will
! decrease output event coherence.  Wavelets are allowed to vary slowly in time
! with this variation determined by the input trace length and the value of
! NUM_WAVELETS.  The default value of NUM_WAVELETS is equivalent to one wavelet
! for each 0.5 second of trace length.  This is usually an appropriate trial
! value for testing.
!
!
! Data Integrity
!
! RFAB is intended to increase event continuity within output CMP gathers.
! Depending on the parameterization, coherence enhancement may be excessive.  
! Always compare the RFAB output with the input CMP gathers to insure that data
! integrity has not be compromised.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
! 
! This process requires traces to be gathered in CMP gathers with NMO corrected.
!
! CMP's with only one trace require special processing, based upon MODE.
! They should not be discarded.
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
! This process outputs the same number of traces that it receives at a time
! except when using STACK mode.  In the STACK mode it outputs one trace for each
! input gather.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! GATHERED  whether traces are a legitimate gather  used and changed
! NDPT      number of sample values in trace        used but not changed
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
! 1       Sequential Trace Count     Renumbered if mode = STACK only.
! 2       Head mute index            Set to 1.0 if mode = STACK, else used.
! 4       Seq. trace # within group  Set to 1.0 if mode = STACK only
! 6       Offset                     Set to 0.0 if mode = STACK only
! 25      Largest absolute value     Set
! 64      Tail mute index            Used 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
! 17. 2006-06-12  B. Menger    Removed Unused Variables.
! 16. 2005-01-17  B Lucas      Fixed problem so that defaults still appear
!                              if GATHER is not in the process list.
! 15. 2004-03-03  R Selzler    Fixed fix for 1 trace CMP input.
! 14. 2004-02-11  R Selzler    Fixed problem with 1 trace CMP input.
! 13. 2002-08-26  Goodger      Change default on NUM_WAVELETS parameter.
!                              Add pcps control cards for parallel processing.
! 12. 2001-01-29  Chiu         Fix mismatch between old and new CPS.
! 11. 2000-12-08  Stoeckley    Change wrapup flag.
! 10. 2000-09-22  Coleman      Convert to new system.
!  9. 1999-02-22  Goodger      Begin using the fortran90 compiler.    
!  8. 1994-06-29  Harlan       Ignore bottom mute of 0, uninitialized.
!  7. 1994-04-29  Harlan       Add bottom mutes. 
!  6. 1993-02-12  Harlan       Fix RFABCOMP for bottom zero padding.
!  5. 1992-11-16  Harlan       Minor adjustment of previous change.
!  4. 1992-11-06  Harlan       Change spacing of wavelets for IDO=3.
!  3. 1992-10-26  Harlan       Add automatic remuting of all possible outputs.
!  2. 1990-11-09  Ball         CFT77 change  IWRK was INTEGER & POINTER
!  1. 1990-05-31  Harlan       ORIGINAL VERSION  
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
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
!<NS RFAB Process/NC=80>
!
!                            Residual Phase Balance
!                       Balance event phases in CMP gathers
!
!                            MODE =~~~~~~~~~`CCCCCCCC
!
!                            NUM_WAVELETS = `IIIIIIII
!
!                            LEN_WAVELET =~~`FFFFFFFF
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Option of type of trace(s) to output. </Tip>
! Default = PHOPT
! Allowed = PHOPT     (Output phase optimized CMP gathers.)
! Allowed = STACK     (Output estimated stacked trace, one for each CMP.)
! Allowed = MODEL     (Output modeled CMP gathers.  The model CMP gathers are
!                     created by convolving the estimated wavelet for each
!                     offset with the stacked trace.)
! Allowed = WAVELETS  (Output estimated wavelets - vary with time and offset.)
! Allowed = RESID     (Output the input CMP minus the modeled CMP gathers.)
! Allowed = EVENTS    (Output estimated stacked trace repeated over offset.)
!</Help>
!
!<Help KEYWORD="LEN_WAVELET">
!<Tip> Length of wavelets, in seconds. </Tip>
! Default = 0.10 
! Allowed = real > 0
! LEN_WAVELET must be a whole number multiple of DT.
!</Help>
!
!<Help KEYWORD="NUM_WAVELETS">
!<Tip> Number of wavelets to model per trace. </Tip>
! Default = nint(2.0 * trace_length)
! Allowed = int > 0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module rfab_module

    use pc_module
    use named_constants_module
    use mutehw_module
    use string_module
    use lav_module

    implicit none

    private
    public  :: rfab_create
    public  :: rfab_initialize
    public  :: rfab_update
    public  :: rfab_delete
!<execute_only>
    public  :: rfab            ! main execution (trace processing) routine.
    public  :: rfab_wrapup
    private :: rfab_exec
    private :: rfab_fnd_wavelets
    private :: rfab_fnd_stack
    private :: rfab_decon
    private :: rfab_conv_stkd
    private :: rfab_conv_unstkd
    private :: rfab_conj
    private :: rfab_rmut
!</execute_only>


    character(len=100),public,save :: rfab_IDENT = &
       '$Id: rfab.f90,v 1.17 2006/06/12 13:03:55 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    type,public :: rfab_struct              
 
        private
        logical                    :: skip_wrapup      ! wrapup flag.
        character(len=8)           :: mode             ! process parameter
        real                       :: len_wavelet      ! process parameter
        integer                    :: num_wavelets     ! process parameter
        integer                    :: ndpt             ! global parameter
        real                       :: dt               ! global parameter
        integer                    :: ntw              ! dependent variable
        integer                    :: grpcnt           ! dependent variable

    end type rfab_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


!---number of iterations: cost is proportional to NTER*(NTERS+NTERW)
    integer, parameter :: NTER  =  2
    integer, parameter :: NTERS =  3
    integer, parameter :: NTERW =  3


    contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine rfab_create (obj)
    implicit none
    type(rfab_struct),pointer :: obj

    allocate (obj)


    call rfab_initialize (obj)

    return
end subroutine rfab_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine rfab_delete (obj)
    implicit none
    type(rfab_struct),pointer :: obj

!<execute_only>
    call rfab_wrapup (obj)
!</execute_only>

    deallocate(obj)

    return
end subroutine rfab_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine rfab_initialize (obj)
    implicit none
    type(rfab_struct),intent(inout) :: obj       ! arguments

    call pc_get_global( 'DT', obj%dt )
    call pc_get_global( 'NDPT'    , obj%ndpt )

!---initialize process parameters
    obj%mode         = 'PHOPT'
    obj%num_wavelets = nint(obj%ndpt*obj%dt*2.0)
    obj%len_wavelet  = 0.1

    if( obj%dt > 0.0 ) then
        obj%ntw         = nint( obj%len_wavelet / obj%dt + 0.49 )
        obj%len_wavelet = obj%ntw * obj%dt
    else
        obj%ntw = 0
    endif

    obj%grpcnt = 0

!---clear globals
    obj%ndpt = 0
    obj%dt   = 0.0

    call rfab_update (obj)

    return
end subroutine rfab_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine rfab_update (obj)
    implicit none
    type(rfab_struct),intent(inout),target :: obj
    logical                                :: gathered
    logical                                :: gathered_ok

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    call pc_get_global( 'GATHERED', gathered )
    call pc_get_global( 'NDPT'    , obj%ndpt )
    call pc_get_global( 'DT'      , obj%dt   )

    call pc_get( 'NUM_WAVELETS', obj%num_wavelets )
    call pc_get( 'LEN_WAVELET' , obj%len_wavelet  )
    call pc_get( 'MODE'        , obj%mode         )



!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    gathered_ok = .true.
    if( .not. gathered ) then
        gathered_ok = .false.
!        call pc_error( '*** RFAB ERROR: input data must be gathered ***' )
!        return
    endif

    if( obj%ndpt <= 0 ) then
        call pc_error( '*** RFAB: Invalid value for global ndpt: ', obj%ndpt )
        return
    endif

    if( obj%dt <= 0.0 ) then
        call pc_error( '*** RFAB: Invalid value for global dt: ', obj%dt )
        return
    endif

    if( obj%num_wavelets <= 0 ) then
        call pc_error( '*** RFAB: Invalid value for num_wavelets: ', &
                       obj%num_wavelets )
        obj%num_wavelets = 8
    endif

    if( obj%len_wavelet <= 0.0 ) then
        call pc_error( '*** RFAB: Invalid value for len_wavelet: ', &
                       obj%len_wavelet )
        obj%len_wavelet = 0.1
    else
        obj%ntw         = nint( obj%len_wavelet / obj%dt + 0.49 )
        obj%len_wavelet = obj%ntw * obj%dt
    endif

    select case( obj%mode(1:1) )

    case( 'P' )
        obj%mode = 'PHOPT'
    case( 'S' )
        obj%mode = 'STACK'
    case( 'M' )
        obj%mode = 'MODEL'
    case( 'W' )
        obj%mode = 'WAVELETS'
    case( 'R' )
        obj%mode = 'RESID'
    case( 'E' )
        obj%mode = 'EVENTS'
    case default
        call pc_error( '*** RFAB: Invalid value for mode: ', obj%mode )
        obj%mode = 'PHOPT'

    end select

    call pc_put_control ('PARALLEL_SAFE'         , .true.)
    call pc_put_control ('PCPS_SEND_MODE'        , 'PCPS_SEND_FIRST_AVAIL')
    call pc_put_control ('PCPS_RECEIVE_MODE'     , 'PCPS_RECEIVE_PASSTHRU')
    call pc_put_control ('PCPS_BUNCH_MODE'       , 'PCPS_BUNCH_TRACE_GROUPS')
    call pc_put_control ('PCPS_SEND_EOF_MODE'    , 'PCPS_SEND_ALL_EOF')
    call pc_put_control ('PCPS_ALT_SEND_MODE'    , 'PCPS_SEND_ALL')
    call pc_put_control ('PCPS_ALT_RECEIVE_MODE' , 'PCPS_RECEIVE_ALL_EOF')

    if(obj%mode.eq.'STACK')then
      call pc_put_control('PCPS_RESEQUENCE_MODE','PCPS_RESEQUENCE_TRACES')
    endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

    if( obj%mode == 'STACK' ) call pc_put_global( 'GATHERED', .false. )

    call pc_put_options_field( 'MODE', (/ 'PHOPT   ', 'STACK   ', 'MODEL   ', &
                              'WAVELETS', 'RESID   ', 'EVENTS  ' /), 6 )

    call pc_put( 'MODE'        , obj%mode         )
    call pc_put( 'NUM_WAVELETS', obj%num_wavelets )
    call pc_put( 'LEN_WAVELET' , obj%len_wavelet  )


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

    if( .not. gathered_ok ) then
        call pc_error( '*** RFAB ERROR: input data must be gathered ***' )
        return
    endif

    obj%skip_wrapup = .false.

    return
end subroutine rfab_update


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

subroutine rfab( obj, ntr, hd, tr )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    type(rfab_struct), intent(inout) :: obj
    integer,           intent(inout) :: ntr
    double precision,  intent(inout) :: hd(:,:)
    real,              intent(inout) :: tr(:,:)
!-------------------------------------------------------------------------------
    real :: rndpt

    if     ( ntr <= 0 ) then
        call rfab_wrapup( obj )
        return
    else if( ntr == 1 ) then
        ! Special processing for input CMP with only one trace.
        if(obj%mode == 'WAVELETS' .or. obj%mode == 'RESID') then
            ! Zero trace samples, reset MUTE and LAV
            tr(:,1) = 0.0
            rndpt = obj%ndpt
            call mutehw(hd(:,1), tr(:,1), obj%ndpt, rndpt, MUTEHW_SET)
            call lav_set_hdr( hd, tr, obj%ndpt, ntr )
        end if

        ! Pass this one trace CMP to output
        return
    endif

!---do it
    call rfab_exec( tr, obj%ndpt, ntr, obj%ntw, obj%num_wavelets, NTERS, &
                    NTERW, NTER, obj%mode, hd )

!  if desired, pass the estimated reflectivity as the output
!  this trace can be considered an implicitly stacked and
!  deconvolved trace, but it descriminates more against
!  events at other velocities than does a simple stack.

    obj%grpcnt = obj%grpcnt + 1
    if( obj%mode == 'STACK' ) then
        hd(1,1) = obj%grpcnt
        hd(2,1) = 1.0D0
        hd(4,1) = 1.0D0
        hd(6,1) = 0.0D0
        ntr     = 1
    endif

!---remute
    call rfab_rmut( tr, hd, obj%ndpt, ntr )

!---set LAV
    call lav_set_hdr( hd, tr, obj%ndpt, ntr )

    return
end subroutine rfab

!</execute_only>


!!----------------------------- executive ----------------------------------!!
!!----------------------------- executive ----------------------------------!!
!!----------------------------- executive ----------------------------------!!


!<execute_only>

subroutine rfab_exec( tr, ndpt, ntr, ntw, nwav, nters, nterw, nter, mode, hd )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer                      :: ndpt
    integer                      :: ntr
    integer                      :: ntw
    integer                      :: nwav
    integer                      :: nters
    integer                      :: nterw
    integer, intent(in)          :: nter
    character(len=8), intent(inout) :: mode   !***** be sure to change this back
    real                         :: tr(:,:)
    double precision             :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: iter, mw, i
    real    :: vard, r
    real    :: s(ndpt)
    real    :: w(ntw,nwav,ntr)
    real    :: x(ndpt,ntr)
!-----------------------------------------------
! find stacked trace and time/offset dep. wavelet, then decon data.
! residual phase balancing.
!
! if mode = 'PHOPT',    then output deconvolved data;
!         = 'STACK',    then output stack;
!         = 'MODEL',    then output modeled data.
!         = 'WAVELETS', then output estimated wavelets.
!         = 'RESID',    then output residuals of modeled data.
!         = 'EVENTS',   then output duplicates of stacked trace.
!-------------------------------------------------------------------------------

    mw = ( ntw + 1 ) / 2

!---give data a variance of unity.
    call rfab_rmut( tr, hd, ndpt, ntr )
    vard = sum( tr(1:ndpt,1:ntr) * tr(1:ndpt,1:ntr) ) / float( ndpt * ntr )
    
    if( vard /= 0.0 ) then
        r = sqrt( 1.0 / vard )
    else
        r =0.0
    endif

    tr(1:ndpt,1:ntr) = r * tr(1:ndpt,1:ntr)

!---initialize wavelet
    call rfab_fnd_wavelets( w, tr, s, ndpt, ntr, ntw, nwav, mw, 0, hd )

    do iter = 1, nter
!-------update stack.
        call rfab_fnd_stack( s, tr, w, ndpt, ntr, ntw, nwav, mw, nters, hd )
!-------update wavelets.
        call rfab_fnd_wavelets( w, tr, s, ndpt, ntr, ntw, nwav, mw, nterw, hd )
    end do

    select case( mode )

    case( 'PHOPT' )     ! deconvolve with estimated wavelet.
        call rfab_decon( tr, tr, w, ndpt, ntr, ntw, nwav, mw, nterw, hd )

    case( 'STACK' )     ! copy stacked trace
        tr(1:ndpt,1) = s

    case( 'MODEL' )     ! model data
        call rfab_conv_stkd( tr, w, s, ndpt, ntr, ntw, nwav, mw, hd, 1 )

    case( 'WAVELETS' )  ! model wavelets only
        s = 0.0
        i = 1 + int( (1.5 * float( mw ) ) / 25. + 0.5 ) * 25 + 1
        do while( i <= 1 + ndpt - mw )
            s(i) = 1.0
            i = i + max( 25, int( (1.5 * float( ntw ) ) / 25. + 0.5 ) * 25 )
        end do
        call rfab_conv_stkd( tr, w, s, ndpt, ntr, ntw, nwav, mw, hd, 1 )

    case( 'RESID' )     ! model residuals
        call rfab_conv_stkd( x, w, s, ndpt, ntr, ntw, nwav, mw, hd , 1 )
        tr(1:ndpt,1:ntr) = tr(1:ndpt,1:ntr) - x

    case( 'EVENTS' )    ! duplicate stacked trace. initialize wavelet then model
        call rfab_fnd_wavelets( w, tr, s, ndpt, ntr, ntw, nwav, mw, 0, hd )
        call rfab_conv_stkd( tr, w, s, ndpt, ntr, ntw, nwav, mw, hd, 1 )

    end select

!---restore original amplitude
    if( mode == 'STACK' ) then
        r = sum( tr(1:ndpt,1) * tr(1:ndpt,1) ) / float( ndpt )
        if( r /= 0.0 ) r = sqrt( vard / r )
        tr(1:ndpt,1) = r * tr(1:ndpt,1)
    else
        r = sum( tr(1:ndpt,1:ntr) * tr(1:ndpt,1:ntr) ) / float( ndpt*ntr )
        if( r /= 0.0 ) r = sqrt( vard / r )
        tr(1:ndpt,1:ntr) = r * tr(1:ndpt,1:ntr)
    endif

    return
end subroutine rfab_exec

!</execute_only>


!!---------------------------- find wavelets -------------------------------!!
!!---------------------------- find wavelets -------------------------------!!
!!---------------------------- find wavelets -------------------------------!!


!<execute_only>

subroutine rfab_fnd_wavelets( w, tr, s, ndpt, ntr, ntw, nwav, mw, nter, hd )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer             :: ndpt
    integer             :: ntr
    integer             :: ntw
    integer             :: nwav
    integer             :: mw
    integer, intent(in) :: nter
    real                :: w(ntw,nwav,ntr)
    real                :: tr(:,:)
    real                :: s(ndpt)
    double precision    :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: iter, idir, nd, nm
    real    :: alpha
    real    :: e(ndpt,ntr)
    real    :: f(ndpt,ntr)
    real    :: g(ntw,nwav,ntr)
    real    :: p(ntw,nwav,ntr)
!-----------------------------------------------
! find a time/offset dep. wavelet to model data from stacked trace.
! initialize wavelet with impulses if nter.eq.0.0
!-------------------------------------------------------------------------------

    if( nter == 0 ) then
        w = 0.0
        w(mw,:nwav,:ntr) = 1.0
        return
    endif

    e = tr(1:ndpt,1:ntr)

    nd    = ndpt * ntr
    nm    = ntw * nwav * ntr
    alpha = 0.01
    iter  = 0
    do while( iter <= nter )
        call rfab_conj( e, f, g, w, p, nd, nm, idir, iter, alpha )
        if( idir >  0 ) then
            call rfab_conv_stkd( f, p, s, ndpt, ntr, ntw, nwav, mw, hd, idir )
        else
            call rfab_conv_stkd( e, g, s, ndpt, ntr, ntw, nwav, mw, hd, idir )
        endif
    end do

    return
end subroutine rfab_fnd_wavelets

!</execute_only>


!!----------------------------- find stack ---------------------------------!!
!!----------------------------- find stack ---------------------------------!!
!!----------------------------- find stack ---------------------------------!!


!<execute_only>

subroutine rfab_fnd_stack( s, tr, w, ndpt, ntr, ntw, nwav, mw, nter, hd )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer             :: ndpt
    integer             :: ntr
    integer             :: ntw
    integer             :: nwav
    integer             :: mw
    integer, intent(in) :: nter

    real                :: s(ndpt)
    real                :: tr(:,:)
    real                :: w(ntw,nwav,ntr)
    double precision    :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: iter, idir, nd, nm
    real    :: alpha
    real    :: e(ndpt,ntr)
    real    :: f(ndpt,ntr)
    real    :: g(ndpt)
    real    :: p(ndpt)
!-----------------------------------------------
! find a stacked trace from data and a time/offset dep. wavelet.
!------------------------------------------------------------------------------

    e = tr(1:ndpt,1:ntr)

    nd    = ndpt * ntr
    nm    = ndpt
    alpha = 0.01
    iter  = 0
    do while( iter <= nter )
        call rfab_conj( e, f, g, s, p, nd, nm, idir, iter, alpha)
        if( idir > 0 ) then
            call rfab_conv_stkd( f, w, p, ndpt, ntr, ntw, nwav, mw, hd,  1 )
        else
            call rfab_conv_stkd( e, w, g, ndpt, ntr, ntw, nwav, mw, hd, -2 )
        endif
    end do

    return
end subroutine rfab_fnd_stack

!</execute_only>


!!----------------------------- deconvolve ---------------------------------!!
!!----------------------------- deconvolve ---------------------------------!!
!!----------------------------- deconvolve ---------------------------------!!


!<execute_only>

subroutine rfab_decon( s, tr, w, ndpt, ntr, ntw, nwav, mw, nter, hd )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer             :: ndpt
    integer             :: ntr
    integer             :: ntw
    integer             :: nwav
    integer             :: mw
    integer, intent(in) :: nter
    real                :: s(ndpt,ntr)
    real                :: tr(:,:)
    real                :: w(ntw,nwav,ntr)
    double precision    :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: nm, nd, iter, idir, j, k
    real    :: alpha, q, r
    real    :: e(ndpt,ntr)
    real    :: f(ndpt,ntr)
    real    :: p(ndpt,ntr)
!-----------------------------------------------
! deconvolve traces of data with a time/offset dependent wavelet.
!-------------------------------------------------------------------------------

!---first balance wavelets
    r = 1.0 / float( ntw )
    do k = 1, ntr
        do j = 1, nwav
            q = r * sum( w(:,j,k) * w(:,j,k) )
            if( q /= 0.0 ) q = sqrt( 1.0 / q )
            w(:,j,k) = q * w(:,j,k)
        end do
    end do

    e = tr(1:ndpt,1:ntr)
    
    nm    = ndpt * ntr
    nd    = ndpt * ntr
    iter  = 0
    alpha = 0.01
    do while( iter <= nter )
        call rfab_conj(e, f, f, s, p, nd, nm, idir, iter, alpha )
        if( idir > 0 ) then
            call rfab_conv_unstkd( f, w, p, ndpt, ntr, ntw, nwav, mw, hd,  1 )
        else
            call rfab_conv_unstkd( e, w, f, ndpt, ntr, ntw, nwav, mw, hd, -2 )
        endif
    end do

    return
end subroutine rfab_decon

!</execute_only>


!!----------------------- convolve stacked trace ---------------------------!!
!!----------------------- convolve stacked trace ---------------------------!!
!!----------------------- convolve stacked trace ---------------------------!!


!<execute_only>

subroutine rfab_conv_stkd( d, w, s, ndpt, ntr, ntw, nwav, mw, hd, idir )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer             :: ndpt
    integer             :: ntr
    integer, intent(in) :: ntw
    integer, intent(in) :: nwav
    integer, intent(in) :: mw
    integer, intent(in) :: idir
    real                :: d(:,:)
    real                :: w(ntw,nwav,ntr)
    real                :: s(ndpt)
    double precision    :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: i1d     , i1w, i1s, i2w 
    real    :: r2w, d2w
!-----------------------------------------------
! convolve stacked trace with time- and offset-dependent wavelet.
! d(t,x) = (sum over t')  S(T')*w(t-t',T',x).  w changes slowly
! with respect to its third dimension.
! idir=1, output d; -1, adjoint onto w; -2, adjoint onto s..
! recommend mw = (ntw+1)/2 as the zero-lag of the wavelet.
! try ntw=14,  nwav=ndpt/250+1
!-------------------------------------------------------------------------------

    select case( idir )

    case( 1 )
        d(1:ndpt,1:ntr) = 0.0
        do i1d = 1, ndpt
            do i1w = 1, ntw
                i1s = max( 1, min( ndpt, i1d - i1w + mw ) )
                r2w = 1.0 + ( 1.1 * (i1s-1) * (nwav-1) ) / ( 1.1 * ndpt )
                i2w = r2w
                d2w = r2w - i2w

                d(i1d,:ntr) = d(i1d,:ntr) + s(i1s) *                           &
                             ( (1.0-d2w) * w(i1w,i2w,:) + d2w * w(i1w,i2w+1,:) )
            end do
        end do

        call rfab_rmut( d, hd, ndpt, ntr )

    case( -1 )
        call rfab_rmut( d, hd, ndpt, ntr )

        w = 0.0
        do i1d = 1, ndpt
            do i1w = 1, ntw
                i1s = max( 1, min( ndpt, i1d - i1w + mw ) )
                r2w = 1.0 + ( 1.1 * (i1s-1) * (nwav-1) ) / ( 1.1 * ndpt )
                i2w = r2w
                d2w = r2w - i2w

                w(i1w,i2w,  :) = w(i1w,i2w,:) + (1.0-d2w) * d(i1d,:ntr) * s(i1s)
                w(i1w,i2w+1,:) = w(i1w,i2w+1,:) +    d2w  * d(i1d,:ntr) * s(i1s)
            end do
        end do

    case( -2 )
        call rfab_rmut( d, hd, ndpt, ntr )

        s = 0.0
        do i1d = 1, ndpt
            do i1w = 1, ntw
                i1s = max( 1, min( ndpt, i1d - i1w + mw ) )
                r2w = 1.0 + ( 1.1 * (i1s-1) * (nwav-1) ) / ( 1.1 * ndpt )
                i2w = r2w
                d2w = r2w - i2w

                s(i1s) = s(i1s) + sum( d(i1d,:ntr) * &
                         ( (1.0-d2w) * w(i1w,i2w,:) + d2w * w(i1w,i2w+1,:) ) )

            end do
        end do

    end select
    
    return
end subroutine rfab_conv_stkd

!</execute_only>


!!---------------------- convolve unstacked trace --------------------------!!
!!---------------------- convolve unstacked trace --------------------------!!
!!---------------------- convolve unstacked trace --------------------------!!


!<execute_only>

subroutine rfab_conv_unstkd( d, w, s, ndpt, ntr, ntw, nwav, mw, hd, idir )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer             :: ndpt
    integer             :: ntr
    integer, intent(in) :: ntw
    integer, intent(in) :: nwav
    integer, intent(in) :: mw
    integer, intent(in) :: idir
    real                :: d(:,:)
    real                :: w(ntw,nwav,ntr)
    real                :: s(ndpt,ntr)
    double precision    :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: i1d     , i1w, i1s, i2w 
    real    :: r2w, d2w
!-----------------------------------------------
! convolve unstacked trace with time- and offset-dependent wavelet.
! d(t,x) = (sum over t')  S(T',x)*w(t-t',T',x).  w changes slowly
! with respect to its third dimension.
! idir=1, output d; -1, adjoint onto w; -2, adjoint onto s.
! recommend mw = (ntw+1)/2 as the zero-lag of the wavelet.
! try ntw=10; nwav=10.
!-------------------------------------------------------------------------------

    select case( idir )

    case( 1 )
        d(1:ndpt,1:ntr) = 0.0
        
        do i1d = 1, ndpt
            do i1w = 1, ntw
                i1s = max( 1, min( ndpt, i1d - i1w + mw ) )
                r2w = 1.0 + ( 1.1 * (i1s-1) * (nwav-1) ) / ( 1.1 * ndpt )
                i2w = r2w
                d2w = r2w - i2w

                d(i1d,:ntr) = d(i1d,:ntr) + s(i1s,:) *                         &
                            ( (1.0-d2w ) * w(i1w,i2w,:) + d2w * w(i1w,i2w+1,:) )
            end do
        end do

        call rfab_rmut( d, hd, ndpt, ntr )
        
    case( -1 )
        w = 0.0

        do i1d = 1, ndpt
            do i1w = 1, ntw
                i1s = max( 1, min( ndpt, i1d - i1w + mw ) )
                r2w = 1.0 + ( 1.1 * (i1s-1) * (nwav-1) ) / ( 1.1 * ndpt )
                i2w = r2w
                d2w = r2w - i2w

                w(i1w,i2w,:) = w(i1w,i2w,:) + (1.0-d2w) * d(i1d,:ntr) * s(i1s,:)
                w(i1w,i2w+1,:) = w(i1w,i2w+1,:) +  d2w  * d(i1d,:ntr) * s(i1s,:)
            end do
        end do

        call rfab_rmut( d, hd, ndpt, ntr )
        
    case( -2 )
        s = 0.0

        do i1d = 1, ndpt
            do i1w = 1, ntw
                i1s = max( 1, min( ndpt, i1d - i1w + mw ) )
                r2w = 1.0 + ( 1.1 * (i1s-1) * (nwav-1) ) / ( 1.1 * ndpt )
                i2w = r2w
                d2w = r2w - i2w

                s(i1s,:) = s(i1s,:) + d(i1d,:ntr) *                           &
                           ( (1.0-d2w) * w(i1w,i2w,:) + d2w * w(i1w,i2w+1,:) )
            end do
        
        end do

        call rfab_rmut( d, hd, ndpt, ntr )
        
    end select

    return
end subroutine rfab_conv_unstkd

!</execute_only>


!!------------------------- conjugate gradient -----------------------------!!
!!------------------------- conjugate gradient -----------------------------!!
!!------------------------- conjugate gradient -----------------------------!!


!<execute_only>

subroutine rfab_conj( e, f, g, m, p, nd, nm, idir, iter, alpha )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: nd
    integer, intent(in)    :: nm
    integer, intent(inout) :: idir
    integer, intent(inout) :: iter
    real   , intent(inout) :: e(nd)
    real   , intent(in)    :: f(nd)
    real   , intent(inout) :: g(nm)
    real   , intent(inout) :: m(nm)
    real   , intent(inout) :: p(nm)
    real   , intent(in)    :: alpha
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    real :: rm1, rm2, eps, r, beta, d1, d2, d3, d4, gamma, vard, varm
!-----------------------------------------------
    data rm1 / 1.0 /
    data rm2 / 1.0 /
    data eps / 1.0 /
!-------------------------------------------------------------------------------
!  conjugate gradient algorithm finds the m that inverts d = f m
!   by minimizing (d - f m)'(D - F M)/VARD + M'm/varm
!  f is a linear transformation of a model array m
!  d is the data array
!  apostrophes indicate the transpose (adjoint) of matrix multiplication
!  vard is the variance of the error (noise)
!  varm is the variance of the model
!  use in a loop as follows:
!     vard = #
!     varm = #
!     nter = #
!     nd = #
!     nm = #
!     iter = 0
!     d = data
!     m = initial model (perhaps all zeros)
!  10 continue
!     call rfab_conj(d,m,f,p,e,g,nd,nm,idir,iter,vard,varm)
!     if(iter.gt.nter) goto 20
!     if(idir.eq.-1) g = f'E
!     if(idir.eq.1) f = f p
!     goto 10
!  20 continue
!  set nter to a number of iterations greater than 0
!     nd and nm are the number of samples in d and m
!     can use data and error in same memory: destroys data
!     can also use f and g in same memory
! if initial m is not zero then d must have subtracted non-zero
!  modeled data  d - fm
!-------------------------------------------------------------------------------

    varm = 1.0
    vard = ( alpha * alpha * nd ) / nm
    
    if( iter == 0 ) then

        m    =  0.0
        p    =  0.0
        idir = -1
        iter =  1
        rm1  =  1.0
        r    =  1.0e10

        call rfab_divm( eps, vard, varm, r )

    else if( idir == -1 ) then

        g    = -g + eps * m
        rm2  = sum( g * g ) / float( nm )

        call rfab_divm( beta, rm2, rm1, 1.0 )

        p    = beta * p - g
        rm1  = rm2
        idir = 1

    else

        d1 = sum( f * e )
        d2 = sum( p * m )
        d3 = sum( f * f )
        d4 = sum( p * p )

        if( d4 /= 0 ) then
            gamma = ( d1 - eps * d2 ) / ( d3 + eps * d4 )
        else
            gamma = 0.0
        endif

        m    = m + gamma * p
        r    = -gamma
        e    = e + r * f
        idir = -1
        iter = iter + 1

    endif

    return
end subroutine rfab_conj

!</execute_only>


!!--------------------------- divide arrays --------------------------------!!
!!--------------------------- divide arrays --------------------------------!!
!!--------------------------- divide arrays --------------------------------!!


!<execute_only>

subroutine rfab_divm( c, ar, br, rmaxr )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    real, intent(out) :: c
    real, intent(in)  :: ar
    real, intent(in)  :: br
    real, intent(in)  :: rmaxr
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    real :: rmax, a, b, r, r2
!-----------------------------------------------
! divide arrays with clip = rmaxr, c = a/b
! b or a can occupy same memory as c
!-------------------------------------------------------------------------------

    rmax = abs( rmaxr )
    a    = ar
    b    = br

    if( abs( a ) >= 1.0e10 .and. abs( b ) >= 1.0e10 ) then
        a = a / 1.0e10
        b = b / 1.0e10

        if( abs( a ) >= 1.0e10 .and. abs( b ) >= 1.0e10 ) then
            a = a / 1.0e10
            b = b / 1.0e10
        endif
    endif

    r2 = rmax * abs( b )
    if( abs( a ) < r2 ) then
        r = a / b
        if( abs( r ) > rmax ) r = 0.0     ! true only if a & b are very small
    else
        r = rmax
        if( a > 0.0 .and. b < 0.0 .or. a < 0.0 .and. b > 0.0 ) r = - r
    endif

    if( a == 0.0 ) r = 0.0
    c = r

    return
end subroutine rfab_divm

!</execute_only>


!!------------------------------- remute -----------------------------------!!
!!------------------------------- remute -----------------------------------!!
!!------------------------------- remute -----------------------------------!!


!<execute_only>

subroutine rfab_rmut( tr, hd, ndpt, ntr )
    implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
    integer         , intent(in)    :: ndpt
    integer         , intent(in)    :: ntr
    real            , intent(inout) :: tr(:,:)
    double precision, intent(inout) :: hd(:,:)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
    integer :: jtr
!-----------------------------------------------
! remute data from header values 2 and 64.
!-------------------------------------------------------------------------------

    do jtr = 1, ntr
        call mutehw( hd(:,jtr), tr(:,jtr), ndpt, 0.0, MUTEHW_BOTH )
    end do

    return
end subroutine rfab_rmut

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

subroutine rfab_wrapup (obj)
    implicit none
    type(rfab_struct),intent(inout) :: obj       ! arguments

    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.


    return
end subroutine rfab_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module rfab_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

