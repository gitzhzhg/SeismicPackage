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
! Name       : GDIV  (Geometric DIVergence correction)
! Category   : amplitude_mod
! Written    : 1998-05-20   by: Bob Baumel
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : Apply amplitude gain to compensate for geometric spreading.
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
! GDIV corrects for the wavefront spreading component of amplitude decay in
! seismic field data.  The wavefront spreading component of amplitude decay is
! variously known as geometric spreading, spherical divergence or geometric
! divergence.
!
! If OFF_DEP = NO, then GDIV does a simple offset independent gain calculation
! ((V^2)*T), where T is the time in the trace and V is the stacking velocity
! at time T.  This calculation ignores offset (and is the correction done by
! the former CPS process AMPR).
!
! If OFF_DEP = YES, then GDIV does the following more correct offset dependent
! gain calculation.
!
! Trace values are scaled by the factor:
!
!         (T * V^2 / V1) * SQRT(1 + A), where
!
!            A = (V^2 - V1^2) * X^2 / (T0^2 * V^4), and
!            X  = offset of this trace,
!            T  = trace time at offset X,
!            T0 = zero-offset time of this event,
!            V  = stacking velocity, extracted at time T0,
!            V1 = surface velocity (extracted from vel fun at time 0),
!            and we assume that T and T0 are related by the NMO equation:
!            T^2 = T0^2 + (X/V)^2 .
!
!
! Reference
! Newman, P., 1973, Divergence Effects in a Layered Earth: Geophysics, 38,
! 481 - 488.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Overall Scaling
! If TIM_REF >= TSTRT, then the GDIV expansion is scaled so that the gain at
! time TIM_REF is approximately unity.  GDIV applies a single scale factor to
! the entire datase, selected so the expansion value at the center of the
! velocity grid for a zero-offset trace is unity at time TIM_REF.  (Normally
! TIM_REF is set to a time corresponding to the middle or shallow part of the
! section to avoid the large scale factors caused by squaring velocity.)
!
! If TIM_REF < TSTRT, then the gain function is not normalized.
!
!
! Multiples
! GDIV cannot correct the amplitudes accurately for both primaries and
! multiples.  The correction can only be accurate for events with moveout
! matching your velocity functions.  Therefore using GDIV with primary stacking
! velocities will not accurately compensate for the amplitude decay of
! multiples.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! Input traces must not have been NMO corrected.
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
! This process outputs traces with same gather status as the input traces.
!
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
!  25     LAV                        Set
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 12. 2006-12-04  D. Glover  Added NULLIFY statements for Intel compiler.
! 11. 2001-10-18  Stoeckley  Add file selection box and file status message.
! 10. 2001-02-15  Stoeckley  Remove extraneous XML tag.
!  9. 2000-12-08  Stoeckley  Change wrapup flag.
!  8. 2000-07-20  Coleman    Fixed line wrap (line > 80 characters)
!  7. 2000-05-16  Coleman    Added GUI and set header word 25 to LAV
!  6. 2000-04-17  Coleman    Added RCS Ident string
!  5. 2000-03-09  Coleman    Added pathcheck,FILENAME_LENGTH,combo box support.
!  4. 1999-12-13  Coleman    Converted from old system
!  3. 1998-06-02  Baumel     Renamed GDIV. Add offset-dependent correc-
!                            tion, removal option, many other changes;
!                            also modernize code for F90.
!                            Moved to conlib.
!  2. 1989-07-12  Howard     Correct call to GETP when PUTP not called.
!  1. 1989-02-28  Tippett    AMPR - The converted CONSEIS version.
!
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
! Trace values are scaled by the factor:
!
!         (T * V^2 / V1) * SQRT(1 + A), where
!
!            A = (V^2 - V1^2) * X^2 / (T0^2 * V^4), and
!            X  = offset of this trace,
!            T  = trace time at offset X,
!            T0 = zero-offset time of this event,
!            V  = stacking velocity, extracted at time T0,
!            V1 = surface velocity (extracted from vel fun at time 0),
!            and we assume that T and T0 are related by the NMO equation:
!            T^2 = T0^2 + (X/V)^2 .
!
! The equations above are based on ray tracing through a horizontally
! layered medium. The EXACT divergence correction for such a medium can be
! expressed in terms of moveout curves as:
!
!          SQRT ( X * (1 - (V1*P)^2) / (V1^2 * P * (dP/dX)) ),
!
! where:  P = ray parameter = dT/dX = slope of moveout curve, and therefore,
! dP/dX = 2nd derivative of T with respect to X.  The specific equations above
! are the approximation obtained by assuming that the moveout curves are NMO
! hyperbolas.  One could presumably derive a still better approximation by
! assuming more accurate moveout curves (e.g., 4th order NMO).
!
! The approximate equations above require inverting T to T0, but this cannot be
! done uniquely for shallow events because moveout curves cross each other.
! GDIV attempts to resolve this ambiguity by picking the event with larger T0,
! but generally, the correction is less accurate for shallow events.  This
! problem is, however, limited to a region that is probably muted out.
!
!
! Implementation of the offset-dependent calculation utilizes an algorithm for
! time interpolation of velocity data which is somewhat slower than the
! velocity interpolation used previously in AMPR.
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
!<NS GDIV Process/NC=80>
!
!                       Geometric DIVergence correction
!          Apply amplitude gain to compensate for geometric spreading
!
!         MODE = `CCCCCC
!
!      OPT_AVE = `CCC
!
!      OFF_DEP = `CCC
!
!      TIM_REF = `FFFFFFFFF
!
! Select PATHNAME_VEL[pathname_vel]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                     [pathname_vel_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS PATHNAME_VEL[/ML=128/XST]>
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="SELECT_PATHNAME_VEL">
!<Tip> Choose PATHNAME_VEL using a file selection dialog box. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_VEL_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_VEL. </Tip>
!</Help>
!
!
!<Help KEYWORD="PATHNAME_VEL">
!<Tip> Pathname for the velocity file to be used by GDIV. </Tip>
! Default = NONE
! Allowed = char
! Pathname for the velocity file to be used in calculating the divergence
! correction.  Velocity functions must be stacking or RMS velocities.
!</Help>
!
!<Help KEYWORD="OPT_AVE">
!<Tip> Whether to average all velocity functions in the file into one. </Tip>
! Default = YES
! Allowed = YES/NO
! If OPT_AVE = YES, and your velocity file contains more than one function,
! GDIV uses an average of all velocity functions in calculating the divergence
! correction.
!
! For OPT_AVE = NO, use the spatially varying grid of velocity functions in the
! velocity file in calculating the divergence correction.
!</Help>
!
!<Help KEYWORD="OFF_DEP">
!<Tip> Whether to use the offset dependent divergence correction. </Tip>
! Default = YES
! Allowed = YES/NO
! For OFF_DEP = YES, GDIV performs an offset dependent geometric divergence
! correction.
!
! For OFF_DEP = NO, GDIV scales trace values by (V^2)*T, where T is the
! time in the trace and V is the stacking velocity at time T.  This calculation
! ignores offset (and is the correction done by the former CPS process AMPR).
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to APPLY or REMOVE divergence correction. </Tip>
! Default = APPLY
! Allowed = APPLY   (Apply correction for amplitude decay.)
! Allowed = REMOVE  (Remove correction for amplitude decay, ie restore decay.)
!</Help>
!
!<Help KEYWORD="TIM_REF">
!<Tip> Reference time, in seconds, for unity gain. </Tip>
! Default = 1.0
! Allowed = real
! If TIM_REF >= TSTRT, then the GDIV expansion is scaled so that the gain at
! time TIM_REF is approximately unity.  (Normally TIM_REF is set to a time
! corresponding to the middle or shallow part of the section to avoid the large
! scale factors caused by squaring velocity.)
!
! If TIM_REF < TSTRT, then do not normalize the gain function.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module gdiv_module
    use pc_module
    use named_constants_module
    use string_module
    use velio_module
    use interp_module
    use gridcheck_module
    use pathcheck_module
    use pathchoose_module
    use lav_module

    implicit none

    private
    public :: gdiv_create
    public :: gdiv_initialize
    public :: gdiv_update
    public :: gdiv_delete

!<execute_only>

    public :: gdiv            ! main execution (trace processing) routine.
    private:: gdiv_readvels
    public :: gdiv_wrapup

!</execute_only>


    character(len=100),public,save :: gdiv_IDENT = &
     '$Id: gdiv.f90,v 1.12 2006/12/04 13:29:53 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


    type,public :: gdiv_struct

        private
        logical                        :: skip_wrapup      ! wrapup flag.
        character(len=6)               :: mode             ! process parameter
        logical                        :: opt_ave          ! process parameter
        logical                        :: off_dep          ! process parameter
        real                           :: tim_ref          ! process parameter
        character(len=FILENAME_LENGTH) :: pathname_vel     ! process parameter
        integer                        :: ndpt             ! global
        real                           :: tstrt            ! global
        real                           :: dt               ! global
        integer                        :: i1plus           ! dependent variable
        integer                        :: nxb              ! dependent variable
        integer                        :: nyb              ! dependent variable
        integer                        :: nhxb             ! dependent variable
        integer                        :: nhyb             ! dependent variable
        real                           :: tlast            ! dependent variable
        real                           :: xlast            ! dependent variable
        real                           :: ylast            ! dependent variable
        real                           :: scale            ! dependent variable
        real,pointer                   :: xb(:)            ! dependent variable
        real,pointer                   :: yb(:)            ! dependent variable
        real,pointer                   :: time0(:)         ! dependent variable
        real,pointer                   :: time1(:)         ! dependent variable
        real,pointer                   :: vsave(:)         ! dependent variable
        real,pointer                   :: expfac(:)        ! dependent variable
        real,pointer                   :: vtab(:,:,:)      ! dependent variable
        type(pathchoose_struct),pointer :: dialog

    end type gdiv_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


    integer,parameter :: NIVP    =   50  ! t-v pairs in interpolated vel funcs
    integer,parameter :: MAX_MSG =  256  ! maximum message length

! the following parameters are used solely to estimate memory requirements
    integer,parameter :: MAX_TVP =  200  ! maximum # of t-v pairs in input vfunc
    integer,parameter :: MAX_NXY =  100  ! maximum value for nxb and nyb
    integer,parameter :: MAX_NVF = 1000  ! maximum # of velocity functions


contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine gdiv_create( obj )
    implicit none
    type(gdiv_struct),pointer :: obj

    allocate( obj )

    nullify( obj%xb )
    nullify( obj%yb )
    nullify( obj%time0 )
    nullify( obj%time1 )
    nullify( obj%vsave )
    nullify( obj%expfac )
    nullify( obj%vtab )
    nullify (obj%dialog) ! jpa

    call pathchoose_create (obj%dialog, 'pathname_vel', 'vel')
    call gdiv_initialize   (obj)
    return
end subroutine gdiv_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine gdiv_delete (obj)
    implicit none
    type(gdiv_struct),pointer :: obj       ! arguments

!<execute_only>
    call gdiv_wrapup (obj)
!</execute_only>

    if( associated( obj%xb     ) ) deallocate( obj%xb )
    if( associated( obj%yb     ) ) deallocate( obj%yb )
    if( associated( obj%time0  ) ) deallocate( obj%time0 )
    if( associated( obj%time1  ) ) deallocate( obj%time1 )
    if( associated( obj%vsave  ) ) deallocate( obj%vsave )
    if( associated( obj%expfac ) ) deallocate( obj%expfac )
    if( associated( obj%vtab   ) ) deallocate( obj%vtab )

    call pathchoose_delete (obj%dialog)

    deallocate(obj)
    return
end subroutine gdiv_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine gdiv_initialize (obj)
    implicit none
    type(gdiv_struct),pointer :: obj
    real :: x

    obj%mode          = 'APPLY'
    obj%opt_ave       = .true.
    obj%off_dep       = .true.
    obj%tim_ref       = 1.0
    obj%pathname_vel  = PATHCHECK_EMPTY
    obj%i1plus        = 0
    obj%nxb           = 0
    obj%nyb           = 0
    obj%nhxb          = 7
    obj%nhyb          = 8
    obj%tlast         = 0.0
    obj%xlast         = 0.0
    obj%ylast         = 0.0
    obj%scale         = 0.0
    obj%ndpt          = 0          ! test later to ensure it has been reset.
    obj%dt            = -1.0       ! test later to ensure it has been reset.
    obj%tstrt         = -HUGE(x)   ! test later to ensure it has been reset.

    call gdiv_update (obj)

    return
end subroutine gdiv_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine gdiv_update (obj)
    implicit none
    type(gdiv_struct),intent(inout) :: obj

    integer                 :: ierr         ! local
    integer                 :: ierr1        ! local
    integer                 :: ierr2        ! local
    integer                 :: ierr3        ! local
    integer                 :: ierr4        ! local
    integer                 :: ierr5        ! local
    character(len=MAX_MSG)  :: msg          ! local
    integer                 :: mxpics       ! local
    integer                 :: nscratch     ! local
    integer                 :: nstorage     ! local
    real                    :: nmosign      ! local
    real                    :: nmoexp       ! local
    integer                 :: nvfuncs      ! local
    integer                 :: nxb0         ! local
    integer                 :: nyb0         ! local
    real                    :: x            ! local
    real,pointer            :: xcoords(:)   ! local
    real,pointer            :: ycoords(:)   ! local

    nullify (xcoords) ! jpa
    nullify (ycoords) ! jpa

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


    if (pathchoose_update(obj%dialog, obj%pathname_vel)) return

    call pc_get_global( 'ndpt' , obj%ndpt  )  ! number of data point per trace
    call pc_get_global( 'tstrt', obj%tstrt )  ! start time (time at 1st sample)
    call pc_get_global( 'dt'   , obj%dt    )  ! time between samples

    call pc_get( 'mode'        , obj%mode         )
    call pc_get( 'opt_ave'     , obj%opt_ave      )
    call pc_get( 'off_dep'     , obj%off_dep      )
    call pc_get( 'tim_ref'     , obj%tim_ref      )
    call pc_get( 'pathname_vel', obj%pathname_vel )

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


    if( obj%ndpt  <= 0 ) call pc_error( 'NDPT is less than or equal to zero' )
    if( obj%dt    <= 0 ) call pc_error( 'DT is less than or equal to zero' )
    if( obj%tstrt == -HUGE(x) ) call pc_error( 'TSTRT is invalid' )

    call string_to_upper( obj%mode )
    select case( obj%mode(1:1) )
        case( 'A' )
            obj%mode = 'APPLY'
        case( 'R' )
            obj%mode = 'REMOVE'
        case default
            call pc_error( 'MODE must be "APPLY" or "REMOVE"' )
            obj%mode = 'APPLY'
    end select

    call pathcheck( 'PATHNAME_VEL', obj%pathname_vel, 'vel', .true., &
                             show=PATHCHECK_INFO_INPUT )


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


    call pc_put_options_field( 'MODE'   , (/ 'APPLY ', 'REMOVE' /), 2 )
    call pc_put_options_field( 'OPT_AVE', (/ 'YES'   , 'NO '    /), 2 )
    call pc_put_options_field( 'OFF_DEP', (/ 'YES'   , 'NO '    /), 2 )

    call pc_put( 'mode'        , obj%mode         )
    call pc_put( 'opt_ave'     , obj%opt_ave      )
    call pc_put( 'off_dep'     , obj%off_dep      )
    call pc_put( 'tim_ref'     , obj%tim_ref      )
    call pc_put( 'pathname_vel', obj%pathname_vel )

! the following memory requirement calculations are approximations
    nstorage = NIVP * MAX_NVF + 2 * ( obj%ndpt + NIVP + MAX_NXY )
    nscratch = 2 * max( NIVP, MAX_TVP )

    call pc_put_control( 'nscratch', nscratch )
    call pc_put_control( 'nstore'  , nstorage )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    if( associated( obj%xb     ) ) deallocate( obj%xb )
    if( associated( obj%yb     ) ) deallocate( obj%yb )
    if( associated( obj%time0  ) ) deallocate( obj%time0 )
    if( associated( obj%time1  ) ) deallocate( obj%time1 )
    if( associated( obj%vsave  ) ) deallocate( obj%vsave )
    if( associated( obj%expfac ) ) deallocate( obj%expfac )
    if( associated( obj%vtab   ) ) deallocate( obj%vtab )

!<execute_only>

    if (pc_do_not_process_traces()) return
    obj%skip_wrapup = .false.

    obj%tlast  = obj%tstrt + (obj%ndpt - 1) * obj%dt
    obj%i1plus = max( nint( 2.0 - obj%tstrt / obj%dt ), 1)

    if( obj%i1plus > obj%ndpt ) then
        call pc_error( 'No positive trace times' )
        return
    end if

    if( obj%tim_ref <= max( obj%tstrt, 0.0 ) ) then
        obj%tim_ref = -1.0
    else
        obj%tim_ref = min( max( obj%tim_ref, obj%dt ), obj%tlast )
    end if

    call velio_scan_alloc( obj%pathname_vel, nvfuncs, ierr, msg, &
                           obj%nhxb, obj%nhyb, nmosign, nmoexp, mxpics, &
                           xcoords, ycoords, obj%xb, obj%yb, nxb0, nyb0 )
    if( ierr /= VELIO_OK ) then
        call pc_error( msg )
        return
    endif

    if( obj%opt_ave ) then
        obj%nxb = 1
        obj%nyb = 1
    else
        obj%nxb = nxb0
        obj%nyb = nyb0
    endif

    allocate( obj%vtab(NIVP,obj%nxb,obj%nyb), stat=ierr1 )
    allocate( obj%vsave(NIVP)               , stat=ierr2 )
    allocate( obj%time0(NIVP)               , stat=ierr3 )
    allocate( obj%time1(obj%ndpt)           , stat=ierr4 )
    allocate( obj%expfac(obj%ndpt)          , stat=ierr5 )

    if( (ierr1 /= 0) .or. (ierr2 /= 0) .or. (ierr3 /= 0) .or. &
        (ierr4 /= 0) .or. (ierr5 /= 0) ) then
        call pc_error( 'giv_update: memory allocation error' )
        return
    endif

    call gdiv_readvels( obj, mxpics, nxb0, nyb0, ierr )

    if (ierr /= 0) then
        call pc_error( 'gdiv_readvels was unsuccessful' )
        return
    end if

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


    return
end subroutine gdiv_update


!!------------------------ read velocities ---------------------------------!!
!!------------------------ read velocities ---------------------------------!!
!!------------------------ read velocities ---------------------------------!!


!<execute_only>

subroutine gdiv_readvels( obj, mxpics, nxb0, nyb0, ierr )
    implicit none

    type(gdiv_struct) ,intent(inout)  :: obj        !  argument
    integer           ,intent(in)     :: mxpics     !  argument
    integer           ,intent(in)     :: nxb0       !  argument
    integer           ,intent(in)     :: nyb0       !  argument
    integer           ,intent(out)    :: ierr       !  argument

    type(velio_struct),pointer  :: velio_obj        ! local
    integer                     :: i                ! local
    integer                     :: j                ! local
    integer                     :: k                ! local
    integer                     :: npicks           ! local
    integer                     :: nvfunc           ! local
    integer                     :: iref             ! local
    real,dimension(mxpics)      :: tpicks           ! local
    real,dimension(mxpics)      :: vpicks           ! local
    character(len=MAX_MSG)      :: msg              ! local

    nullify (velio_obj) ! jpa

    ierr = 0
    if( obj%opt_ave ) then
        obj%xb(1) = 0.0
        obj%yb(1) = 0.0
        obj%vsave = 0.0
    endif

    call velio_open_read( velio_obj, obj%pathname_vel, nvfunc, ierr, msg )
    if( ierr /= VELIO_OK ) then
        call pc_error( 'velio_open_read was unsuccessful: ' // msg )
        return
    endif

    do j = 1, nyb0
        do i = 1, nxb0
            call velio_read_velfun( velio_obj, obj%xlast, obj%ylast,  &
                                    npicks, tpicks, vpicks, ierr, msg )
            if( ierr /= VELIO_OK ) then
                call pc_error( 'velio_open_read was unsuccessful: ' // msg )
                return
            endif

            if( obj%opt_ave ) then
                call interp_1d_var_lin_real( tpicks, vpicks, npicks, &
                        obj%time0, obj%vtab(1:NIVP,1,1), NIVP, 0.0, obj%tlast )
                obj%vsave = obj%vsave + obj%vtab(1:NIVP,1,1)
            else
                call interp_1d_var_lin_real( tpicks, vpicks, npicks, &
                        obj%time0, obj%vtab(1:NIVP,i,j), NIVP, 0.0, obj%tlast )
            endif
        end do
    end do

    call velio_close( velio_obj )

    if( obj%opt_ave ) then
        obj%xlast = 0.0
        obj%ylast = 0.0
        obj%vsave = obj%vsave / (nxb0*nyb0)
        obj%vtab(1:NIVP,1,1) = obj%vsave
    else
        obj%xlast = 0.5 * (obj%xb(1) + obj%xb(obj%nxb))
        obj%ylast = 0.5 * (obj%yb(1) + obj%yb(obj%nyb))
        call interp_2d_var_lin_real( obj%xb, obj%yb, obj%nxb, obj%nyb, &
                               obj%vtab, NIVP, obj%xlast, obj%ylast, obj%vsave )
    endif

    if( obj%tim_ref>0. .or. .not. obj%off_dep ) then
        call interp_1d_var_lin_real( obj%time0, obj%vsave, NIVP, obj%time1, &
                                    obj%expfac, obj%ndpt, obj%tstrt, obj%tlast )
        if( .not. obj%off_dep ) then
            obj%expfac(1:obj%i1plus-1) = 0.0
            if( obj%mode == 'APPLY' ) then
                do k = obj%i1plus, obj%ndpt
                    obj%expfac(k) = obj%time1(k) * obj%expfac(k)**2
                end do
            else
                do k = obj%i1plus, obj%ndpt
                    obj%expfac(k) = 1. / (obj%time1(k) * obj%expfac(k)**2)
                end do
            endif
        endif
    endif

    if( obj%tim_ref > 0.0 ) then
        iref = max( nint( (obj%tim_ref-obj%tstrt)/obj%dt )+1, obj%i1plus )
        if( .not. obj%off_dep ) then
            obj%scale  = 1./obj%expfac(iref)
            obj%expfac = obj%scale * obj%expfac
        else if( obj%mode == 'APPLY' ) then
            obj%scale = obj%vsave(1)/ (obj%time1(iref) * obj%expfac(iref)**2)
        else
            obj%scale = obj%time1(iref) * obj%expfac(iref)**2 / obj%vsave(1)
        endif
    else
        obj%scale = 1.0
    endif

    return
end subroutine gdiv_readvels

!</execute_only>


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

subroutine gdiv( obj, ntr, hd, tr )
    implicit none

    type(gdiv_struct),intent(inout)  :: obj         ! arguments
    integer          ,intent(in)     :: ntr         ! arguments
    double precision ,intent(inout)  :: hd(:,:)     ! arguments
    real             ,intent(inout)  :: tr(:,:)     ! arguments

    integer               :: it         ! local
    integer               :: k          ! local
    integer               :: k1         ! local
    real                  :: xoff       ! local
    real                  :: v1         ! local
    real                  :: v1sq       ! local
    real                  :: aa         ! local
    real,dimension(NIVP)  :: timex      ! local
    real,dimension(NIVP)  :: expfac0    ! local

!-------------------------------------------------------------------------------

    ! wrapup and return if there are no traces to process
    if( ntr <= 0 ) then
        call gdiv_wrapup (obj)
        return
    end if

    do it = 1, ntr
        if( .not. obj%opt_ave ) then
            if( (hd(obj%nhxb,it) /= obj%xlast) .or. &
                (hd(obj%nhyb,it) /= obj%ylast)      ) then
                obj%xlast = hd(obj%nhxb,it)
                obj%ylast = hd(obj%nhyb,it)
                call interp_2d_var_lin_real( obj%xb, obj%yb, obj%nxb, obj%nyb, &
                               obj%vtab, NIVP, obj%xlast, obj%ylast, obj%vsave )

                if( .not. obj%off_dep ) then
                    call interp_1d_var_lin_real( obj%time0, obj%vsave, NIVP, &
                         obj%time1, obj%expfac, obj%ndpt, obj%tstrt, obj%tlast )
                    obj%expfac(1:obj%i1plus-1) = 0.0
                    if( obj%mode == 'APPLY' ) then
                        do k = obj%i1plus, obj%ndpt
                            obj%expfac(k) = obj%scale * &
                                            (obj%time1(k) * obj%expfac(k)**2)
                        end do
                    else
                        do k = obj%i1plus, obj%ndpt
                            obj%expfac(k) = obj%scale / &
                                            (obj%time1(k) * obj%expfac(k)**2)
                        end do
                    endif
                endif

            endif
        endif

        if( .not. obj%off_dep ) then
            tr(1:obj%ndpt,it) = obj%expfac * tr(1:obj%ndpt,it)
        else
            xoff = hd(6,it)
            v1 = obj%vsave(1)
            v1sq = obj%vsave(1) ** 2
            timex(NIVP) = sqrt (obj%time0(NIVP)**2 + (xoff/obj%vsave(NIVP))**2)
            do k = NIVP, 2, -1
                aa         = (obj%vsave(k)**2-v1sq) &
                           * (xoff/(obj%time0(k)*obj%vsave(k)**2))**2
                expfac0(k) = (obj%vsave(k)**2/v1) * sqrt( max( 1.0+aa, 0.0 ) )
                timex(k-1) = sqrt(obj%time0(k-1)**2 + (xoff/obj%vsave(k-1))**2)
                if( timex(k-1) >= timex(k) ) then
                    timex(k-1) = 0.0
                    exit
                endif
            end do

            k1 = max( k-1, 1 )
            expfac0(k1) = v1
            call interp_1d_var_lin_real( timex(k1:NIVP), expfac0(k1:NIVP), &
                    NIVP-k1+1, obj%time1, obj%expfac, obj%ndpt, obj%tstrt, &
                    obj%tlast )
            tr(1:obj%i1plus-1,it) = 0.0

            if( obj%mode == 'APPLY' ) then
                do k = obj%i1plus, obj%ndpt
                    tr(k,it) = obj%scale * obj%expfac(k) * obj%time1(k) &
                             *  tr(k,it)
                end do
            else
                do k = obj%i1plus, obj%ndpt
                    if( obj%expfac(k) /= 0.0 ) then
                        tr(k,it) = obj%scale * tr(k,it) &
                                 / (obj%expfac(k) * obj%time1(k))
                    else
                        tr(k,it) = 0.0
                    endif
                end do
            endif
        endif

    end do

    call lav_set_hdr( hd, tr, obj%ndpt, ntr )

    return
end subroutine gdiv

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

subroutine gdiv_wrapup (obj)
    implicit none
    type(gdiv_struct),intent(inout) :: obj

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    return
end subroutine gdiv_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module gdiv_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

