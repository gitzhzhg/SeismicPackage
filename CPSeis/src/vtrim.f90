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
! Name       : VTRIM
! Category   : velocity_analysis
! Written    : 1991-04-23   by: Bill Harlan
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Optimize NMO on NMO-corrected CMP gathers.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! VTRIM performs a residual moveout adjustment to optimize the moveout of the
! input CMPs to achieve the flattest possible events within a specified
! constraint.  The CMPs with correction applied will be output from VTRIM.
! Input CMPs must be NMO corrected.  VTRIM neither inputs nor outputs a
! velocity file.
!
! (Automatic velocity picking of CMPs that have not been NMO corrected is
! provided by the VPICK process.)
!
! (The VTRIM process was formerly part of the old Cray VPICK process.)
!
!-------------------------------------------------------------------------------
!                     CONSTRAINTS ON MOVEOUT ADJUSTMENT
!
! The allowed range of residual moveout correction is specified by a linked
! array of RMO_MIN, RMO_MAX, TIMES and OFF_MAX values where:
!
!       TIMES is the array of zero-offset times in seconds.
!
!       OFF_MAX is the array of maximum offset values.
!
!       RMO_MIN is the array of the smallest (negative) residual moveout
!       correction allowed, in seconds, for the specified zero-offset time
!       at the specified far offset.
!
!       RMO_MAX is the array of the largest (positive) residual moveout
!       correction allowed, in seconds, for the specified zero-offset time
!       at the specified far offset.
!
! At least two constraints per second should be made.  VTRIM converts
! constraints to squared slowness for interpolation.
!
!-------------------------------------------------------------------------------
!          SMOOTHING OF OUTPUT RESIDUAL MOVEOUT CORRECTION 
!
! TIM_SMOOTH is the length of a temporal running average smoothing operator
! applied to the estimated residual moveouts.
!
!-------------------------------------------------------------------------------
!                             RESTRICTIONS
!
! Moveout adjustment will not be performed on a CMP for the following reasons:
!  (1) the CMP gather contains fewer than eight live traces.
!  (2) the nearest live trace offset is more than half the farthest live
!       trace offset.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Typical use of VTRIM is as a final "trim" of NMO after the last velocity
! analysis of the processing sequence.
!
! RMO_MIN and RMO_MAX are interpreted as residual moveouts (in seconds)
! after the NMO correction has been made with a user-specified velocity
! file.  Residual moveouts are measured as the zero-offset time of a
! reflection minus its corrected time at offset OFF_MAX.
!
! To choose values for RMO_MIN and RMO_MAX:
!
!  (1) Apply normal moveout corrections to a midpoint gather, then plot.
!
!  (2) For a particular zero-offset time, specify the largest unmuted
!      offset OFF_MAX.
!
!  (3) Set RMO_MIN to the smallest (most negative) residual moveout.
!      This will normally be a negative number corresponding to an
!      over-corrected reflection.
!
!  (4) Set RMO_MAX to the largest (most positive) residual moveout.
!      This will normally be a positive number corresponding to an
!      under-corrected reflection.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
! Process requires traces to be input in CMP gathers WITH NMO correction.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs NMO corrected traces with same gather status as the
! input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GATHERED gathered flag                         used but not changed
! NUMTR    maximum number of traces per gather   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                  Action taken
! ----    -----------                  ------------
!   2     Top mute                     Modified
!   6     Offset                       Used
!   7     Midpoint X grid coordinate   Used
!   8     Midpoint Y grid coordinate   Used
!  25     LAV                          Reset
!  64     Bottom mute                  Modified
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!040. 2006-06-12  B. Menger   Removed Unused Variables.
! 39. 2001-04-04  Stoeckley  Changed the RMO_INC default from 0.3 (which was
!                             a mistake) to 0.012; replaced MINLOC and MAXLOC
!                             calls with old-fashioned do loop code to get
!                             around an apparent problem with these intrinsic
!                             functions.
! 38. 2001-02-09  Stoeckley  Completely rewrote the code from scratch, using
!                             a number of existing primitives.  The new code
!                             corrects a number of problems, contains only
!                             a small fraction as much code, and runs several
!                             times faster.  Some of the problems in the
!                             previous code included incorrect treatment of
!                             negative times, mute adjustment problems, and
!                             inadequate number of iterations (the new
!                             code does not use multiple iterations).
!                            If there are problems with this new code, I can
!                             try to improve the old code.
! 37. 2000-12-15  Coleman    Corrected some spelling and grammer in messages.
! 36. 2000-12-14  Coleman    Changed some the subroutines from PRIVATE to
!                             PUBLIC so that they can be called by VPICK.
! 35. 2000-12-12  Coleman    Convert to new system from a subset of VPICK.
! 34. 1996-06-20  Vunderink  Added SAVE FILENO to VTRIMFNEW subroutine.
! 33. 1995-04-03  Harlan     Allow more than 1999 picks per function.
! 32. 1994-10-17  Harlan     Shorten dcode card to 80 characters. Fortran!
! 31. 1994-10-07  Harlan     Yet another typo in internal NMC dcode card.
! 30. 1994-10-06  Harlan     Fix problem interpolating input file headers.
! 29. 1994-08-29  Harlan     Update documentation.
! 28. 1994-07-18  Harlan     Fix another NCODE typo calling NMC.
! 27. 1994-07-15  Harlan     Fix NCODE problem calling NMC internally.
! 26. 1994-07-15  Harlan     Bad input interpolation default, header 7&8.
! 25. 1994-07-15  Harlan     Add more scratch memory.
! 24. 1994-06-29  Harlan     Fix bottom mute.
! 23. 1994-05-06  Harlan     Allow OUTFILE="NONE"
! 22. 1994-04-27  Harlan     Change name to VPICK; add header word NHYVEL
!         for 3D; call NMC internally for input velocity interpolation;
!         get header words from VELFILE.
! .....................................................................
! Lower revisions refer to earlier name VPIK
! 21. 1994-03-04  Harlan     Double resolution of semblances, slower by 2.
! 20. 1994-02-23  Harlan     Avoid SQRT of negative number in VTRIMRMT3.
! 19. 1993-05-04  Harlan     Make variable IMIN static in VTRIMPARM.
! 18. 1993-04-29  Harlan     Initialization in VTRIMSPCK for new compiler.
! 17. 1993-02-12  Harlan     Fix VTRIMCOMP for bottom zero padding.
! 16. 1992-09-03  Harlan     Allow initial velocities outside constraints
! 15. 1992-07-31  Harlan     Add "FLAT" option for VFILE.
! 14. 1992-03-24  Harlan     I-O negative moveouts as negative velocities
! 13. 1992-02-14  Harlan     Ignore dead traces when averaging headers.
! 12. 1992-02-03  Harlan     Increase resolution of internal semblances.
! 11. 1992-01-24  Harlan     Reduce significant digits in veloc. file.
! 10. 1992-01-24  Harlan     Changes to documentation only.
! 9.  1991-12-30  Harlan     If can't pick, apply moveout anyway.
! 8.  1991-12-20  Harlan     Output of VTRIM uses smoothed velocities.
! 7.  1991-11-26  Harlan     Minor changes in comments to *.cpr file.
! 6.  1991-10-31  Harlan     Check for dead gathers. Insure N < NTRMAX.
! 5.  1991-10-02  Harlan     ASCII semblance plots to *.cpr file.
! 4.  1991-08-01  Harlan     Add smoothing of picks over midpoint.
! 3.  1991-06-20  Harlan     Adjust to UNICOS changes in CLOSFIL.
! 2.  1991-06-05  Harlan     Add new constraint options.
! 1.  1991-04-23  Harlan     Original version.
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
! Debug printplots can be turned on by uncommenting the lines which are
! identified with the characters !DEBUG at the right end of the line.
! The primitive being used for these printplots is in Tom Stoeckley's
! directory but probably not in ~sps.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS VTRIM Process/NC=80/NR=20>
!
!                                Velocity TRIM
!                  Optimize NMO on NMO-corrected CMP gathers.
!
! TIM_SMOOTH=`FFFFFFFF seconds                    RMO_INIT=`XXXXXXXX seconds
!                                                 RMO_INC =`FFFFFFFF seconds
! Effective offset used for                       RMO_LAST=`XXXXXXXX seconds
! display-only parameters:  OFF_BIG=`XXXXXXXX     RMO_TOT =`XXXXXXXX
!
!   TIMES    RMO_MIN  RMO_MAX   OFF_MAX   ADJUSTED_RMO_MIN  ADJUSTED_RMO_MAX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!   `FFFFFFFF`FFFFFFFF`FFFFFFFFF`FFFFFFFFF`XXXXXXXXXXXXXXXXX`XXXXXXXXXXXXXXX
!
!<PARMS TIMES_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="TIM_SMOOTH">
!<Tip> Length of time running average smoothing for residual moveout. </Tip>
! Default = 0.3
! Allowed = real > 0.0
!
! Length of the temporal running average smoothing, in seconds, for residual
! moveout adjustments.
!</Help>
!
!
!<Help KEYWORD="RMO_INIT" TYPE="DISPLAY_ONLY">
!<Tip> Minimum adjusted value of residual moveout to test for (seconds). </Tip>
!
! This is the smallest (most negative) value in the ADJUSTED_RMO_MIN array.
!</Help>
!
!
!<Help KEYWORD="RMO_INC">
!<Tip> Increment between values of residual moveout to test for (secs). </Tip>
! Default =  0.012
! Allowed = real > 0.0
!
! RMO_INC is the increment in residual moveout values to test.  This increment
! refers to the residual moveout at the offset given by OFF_BIG, which is the
! biggest offset specified in the OFF_MAX array.
!
! All moveouts between RMO_INIT and RMO_LAST in increments of RMO_INC
! will be tested.  These moveouts are effective at the offset given by
! OFF_BIG.  A total of RMO_TOT moveouts will be tested.
!
! After all moveout tests are made, the residual NMO corrections actually
! made to the traces will be determined from the maximum semblance determined
! within the range of moveouts constrained by the RMO_MIN and RMO_MAX arrays.
!</Help>
!
!
!<Help KEYWORD="RMO_LAST" TYPE="DISPLAY_ONLY">
!<Tip> Maximum adjusted value of residual moveout to test for (seconds). </Tip>
!
! This is the largest (most positive) value in the ADJUSTED_RMO_MAX array.
!</Help>
!
!
!<Help KEYWORD="RMO_TOT" TYPE="DISPLAY_ONLY">
!<Tip> Total number of residual moveout values to test for. </Tip>
!</Help>
!
!
!<Help KEYWORD="OFF_BIG" TYPE="DISPLAY_ONLY">
!<Tip> Biggest offset specified in the OFF_MAX array. </Tip>
!
! This biggest offset is used to calculate the adjusted lower and upper
! moveout constraints displayed in the arrays.  These adjusted constraints
! are extended from the specified constraints by the ratio (OFF_BIG/OFF_MAX)
! squared.
!</Help>
!
!
!<Help KEYWORD="TIMES">
!<Tip> Array of zero offset times for specifying moveout constraints. </Tip>
! Default =  -
! Allowed = real (linked array)
!
! These zero offset times should be specified in seconds.
! There should normally be at least two constraints per second.
!</Help>
!
!
!<Help KEYWORD="RMO_MIN">
!<Tip> Array of lower moveout constraints (seconds). </Tip>
! Default =  -
! Allowed = real (linked array)
!
! RMO_MIN is the smallest (most negative) residual moveout correction allowed,
! in seconds, for the specified zero-offset time and the corresponding
! maximum offset specified for that time.
!
! RMO_MIN should be determined from over-corrected (concave up) events.
! RMO_MIN should normally be a negative number.
!</Help>
!
!
!<Help KEYWORD="RMO_MAX">
!<Tip> Array of upper moveout constraints (seconds). </Tip>
! Default =  -
! Allowed = real (linked array)
!
! RMO_MAX is the largest (most positive) residual moveout correction allowed,
! in seconds, for the specified zero-offset time and the corresponding
! maximum offset specified for that time.
!
! RMO_MAX should be determined from under-corrected (concave down) events.
! RMO_MAX should normally be a positive number.
!</Help>
!
!
!<Help KEYWORD="OFF_MAX">
!<Tip> Array of maximum unmuted offsets at specified zero offset times. </Tip>
! Default =  -
! Allowed = real (linked array)
!</Help>
!
!
!<Help KEYWORD="ADJUSTED_RMO_MIN" TYPE="DISPLAY_ONLY">
!<Tip> Array of adjusted lower moveout constraints (seconds). </Tip>
!
! ADJUSTED_RMO_MIN is the smallest (most negative) residual moveout correction
! allowed, in seconds, for the specified zero-offset time, adjusted from the
! corresponding RMO_MIN value to the very biggest offset found in the OFF_MAX
! array, and then adjusted slightly to be a multiple of RMO_INC.
!</Help>
!
!
!<Help KEYWORD="ADJUSTED_RMO_MAX" TYPE="DISPLAY_ONLY">
!<Tip> Array of adjusted upper moveout constraints (seconds). </Tip>
!
! ADJUSTED_RMO_MAX is the largest (most positive) residual moveout correction
! allowed, in seconds, for the specified zero-offset time, adjusted from the
! corresponding RMO_MAX value to the very biggest offset found in the OFF_MAX
! array, and then adjusted slightly to be a multiple of RMO_INC.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module vtrim_module

    use pc_module
    use mem_module
    use lav_module
    use terputil_module
    use statcc_module
    use dyncc_module
    use named_constants_module
    use statutil_module
!   use printplot_module                                           !DEBUG
    
    implicit none
    
    private
    public  :: vtrim_create
    public  :: vtrim_initialize
    public  :: vtrim_update
    public  :: vtrim_delete
!<execute_only>
    public  :: vtrim 
    public  :: vtrim_wrapup
!</execute_only>
    public  :: vtrim_dump_object


    character(len=100),public,save :: vtrim_IDENT = &
'$Id: vtrim.f90,v 1.40 2006/06/12 13:03:57 Menger prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    integer, parameter :: MAX_TIMES = 51

    type,public :: vtrim_struct

        private
        logical                     :: skip_wrapup      ! skip wrapup flag.
        integer                     :: lun

        integer                     :: ndpt             ! global parameter
        real                        :: dt               ! global parameter
        real                        :: tstrt            ! global parameter

        real                        :: tim_smooth       ! process parameter
        real                        :: rmo_inc          ! process parameter
        integer                     :: ntimes           ! process parameter
        real, dimension(MAX_TIMES)  :: times            ! process parameter
        real, dimension(MAX_TIMES)  :: rmo_min          ! process parameter
        real, dimension(MAX_TIMES)  :: rmo_max          ! process parameter
        real, dimension(MAX_TIMES)  :: off_max          ! process parameter

        integer                     :: ndips            ! dependent variable
        real                        :: dipmin           ! dependent variable
        real                        :: dipinc           ! dependent variable
        real                        :: bigoff           ! dependent variable
        integer                     :: nsmooth          ! dependent variable
        real            , pointer   :: mindips(:)       ! dependent variable
        real            , pointer   :: maxdips(:)       ! dependent variable
        integer                     :: received         ! dependent variable
        integer                     :: processed        ! dependent variable

    end type vtrim_struct


!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!


    integer,parameter :: MINFOLD    = 8   ! minimum live fold to adjust gather.
    integer,parameter :: MINSAMPLES = 20  ! minimum number of trace samples.

    real   ,parameter :: DEFAULT_TIM_SMOOTH = 0.3
    real   ,parameter :: DEFAULT_RMO_INC    = 0.012

contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


    subroutine vtrim_create( obj )
    implicit none
    type(vtrim_struct),pointer :: obj

    allocate( obj )

    nullify (obj%mindips)
    nullify (obj%maxdips)

    call vtrim_initialize( obj )

    return
    end subroutine vtrim_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


    subroutine vtrim_delete( obj )
    implicit none
    type(vtrim_struct),pointer :: obj

!<execute_only>
    call vtrim_wrapup( obj )
!</execute_only>

    call mem_free (obj%mindips)
    call mem_free (obj%maxdips)

    deallocate( obj )

    return
    end subroutine vtrim_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


    subroutine vtrim_initialize( obj )
    implicit none
    type(vtrim_struct),intent(inout) :: obj

    obj%tim_smooth = DEFAULT_TIM_SMOOTH
    obj%rmo_inc    = DEFAULT_RMO_INC
    obj%ntimes     = 1
    obj%times  (:) = 0.0
    obj%rmo_min(:) = 0.0
    obj%rmo_max(:) = 0.0
    obj%off_max(:) = 0.0

    call vtrim_update( obj )

    return
    end subroutine vtrim_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


    subroutine vtrim_update( obj )
    implicit none
    type(vtrim_struct),intent(inout),target :: obj

!-------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------

    logical           :: gathered
    integer           :: numtr,indx
    integer           :: n1,n2,n3,n4
    real              :: factor,dipmax,rmomin,rmomax

    real                       :: rmo_init           ! display-only
    real                       :: rmo_last           ! display-only
    integer                    :: rmo_tot            ! display-only
    real                       :: off_big            ! display-only
    real, dimension(MAX_TIMES) :: adjusted_rmo_min   ! display-only
    real, dimension(MAX_TIMES) :: adjusted_rmo_max   ! display-only


!-------------------------------------------------------------------------------

    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

    obj%lun = pc_get_lun()

    call pc_register_array_names( 'TIMES_ARRAYSET',  (/ &
                                  'TIMES           ' ,  &
                                  'RMO_MIN         ' ,  &
                                  'RMO_MAX         ' ,  &
                                  'OFF_MAX         ' ,  &
                                  'ADJUSTED_RMO_MIN' ,  &
                                  'ADJUSTED_RMO_MAX'  /), 6)

    call pc_get_global( 'NDPT'    , obj%ndpt  )
    call pc_get_global( 'DT'      , obj%dt    )
    call pc_get_global( 'TSTRT'   , obj%tstrt )
    call pc_get_global( 'GATHERED', gathered  )
    call pc_get_global( 'NUMTR'   , numtr     )

    n1 = obj%ntimes
    n2 = obj%ntimes
    n3 = obj%ntimes
    n4 = obj%ntimes

    call pc_get( 'tim_smooth', obj%tim_smooth  )
    call pc_get( 'rmo_inc'   , obj%rmo_inc     )
    call pc_get( 'TIMES'     , obj%times  , n1 )
    call pc_get( 'RMO_MIN'   , obj%rmo_min, n2 )
    call pc_get( 'RMO_MAX'   , obj%rmo_max, n3 )
    call pc_get( 'OFF_MAX'   , obj%off_max, n4 )


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


!----------verify global parameters:

    if( .not. gathered .or. numtr <= 1 ) then
        call pc_error( 'Input traces must be gathered by CMP')
    endif

!----------verify scalar parameters:

    if( obj%tim_smooth <= 0.0 .or. obj%tim_smooth == FNIL ) then
        obj%tim_smooth = DEFAULT_TIM_SMOOTH
        call pc_warning ('Invalid TIM_SMOOTH - changed to',obj%tim_smooth)
    endif

    if( obj%rmo_inc <= 0.0 .or. obj%rmo_inc == FNIL ) then
        obj%rmo_inc = DEFAULT_RMO_INC
        call pc_warning ('Invalid RMO_INC - changed to',obj%rmo_inc)
    endif

!----------verify array parameters:

    if( n1 /= n2 .or. n2 /= n3 .or. n3 /= n4 ) then
        obj%ntimes = min( n1, n2, n3, n4 )
        call pc_error ('The linked arrays have different lengths')
    else
        obj%ntimes = n1
    endif

    if( obj%ntimes == 0 ) then
        obj%ntimes     = 1
        obj%times  (:) = 0.0
        obj%rmo_min(:) = 0.0
        obj%rmo_max(:) = 0.0
        obj%off_max(:) = 0.0
    endif

    do indx = 1,obj%ntimes
        if (obj%times  (indx) == FNIL) obj%times  (indx) = 0.0
        if (obj%rmo_min(indx) == FNIL) obj%rmo_min(indx) = 0.0
        if (obj%rmo_max(indx) == FNIL) obj%rmo_max(indx) = 0.0
        if (obj%off_max(indx) == FNIL) obj%off_max(indx) = 0.0
    end do

    if (pc_verify_arrayset('times_arrayset')) then

        do indx = 1,obj%ntimes
            if (obj%off_max(indx) > 0.0) cycle
            call pc_error ('The offsets must all be greater than zero')
            exit
        end do

        do indx = 2,obj%ntimes
            if (obj%times(indx) > obj%times(indx-1)) cycle
            call pc_error ('The trace times must all be ascending')
            exit
        end do

    end if

!----------calculate display only parameters:

  ! off_big  = maxval(obj%off_max, obj%ntimes)   ! replaced with do loop.

    off_big = 0.0
    do indx = 1,obj%ntimes
         off_big  = max(off_big, obj%off_max(indx))
    end do

    rmo_init = 0.0
    rmo_last = 0.0
    do indx = 1,obj%ntimes
        if (obj%off_max(indx) >  0.0 .and. &
            obj%off_max(indx) /= FNIL) then
            factor = (off_big / obj%off_max(indx))**2
            rmomin = obj%rmo_min(indx) * factor
            rmomax = obj%rmo_max(indx) * factor
            adjusted_rmo_min(indx) = obj%rmo_inc * nint(rmomin / obj%rmo_inc)
            adjusted_rmo_max(indx) = obj%rmo_inc * nint(rmomax / obj%rmo_inc)
        else
            adjusted_rmo_min(indx) = FNIL
            adjusted_rmo_max(indx) = FNIL
        end if
        rmo_init = min(rmo_init,adjusted_rmo_min(indx))
        rmo_last = max(rmo_last,adjusted_rmo_max(indx))
    end do

  ! rmo_init = minval(adjusted_rmo_min, obj%ntimes)   ! replaced with do loop.
  ! rmo_last = maxval(adjusted_rmo_max, obj%ntimes)   ! replaced with do loop.

    rmo_tot  = 1 + nint((rmo_last - rmo_init) / obj%rmo_inc)

! Note: The three calls to minval and maxval above were replaced with
! old-fashioned do loops because one of them (which calculates rmo_last)
! returned garbage when used in CFE.  It worked correctly in my interactive
! test program.  I have not investigated the reasons for the failure.


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


    call pc_put( 'tim_smooth', obj%tim_smooth          )
    call pc_put( 'rmo_inc'   , obj%rmo_inc             )
    call pc_put( 'TIMES'     , obj%times  , obj%ntimes )
    call pc_put( 'RMO_MIN'   , obj%rmo_min, obj%ntimes )
    call pc_put( 'RMO_MAX'   , obj%rmo_max, obj%ntimes )
    call pc_put( 'OFF_MAX'   , obj%off_max, obj%ntimes )

    call pc_put_gui_only( 'rmo_init'        , rmo_init    ,ndec=3)
    call pc_put_gui_only( 'rmo_last'        , rmo_last    ,ndec=3)
    call pc_put_gui_only( 'rmo_tot'         , rmo_tot            )
    call pc_put_gui_only( 'off_big'         , off_big     ,ndec=0)
    call pc_put_gui_only( 'ADJUSTED_RMO_MIN', adjusted_rmo_min, obj%ntimes )
    call pc_put_gui_only( 'ADJUSTED_RMO_MAX', adjusted_rmo_max, obj%ntimes )

    call pc_put_minsize_arrayset( 'TIMES_ARRAYSET', 1 )
    call pc_put_maxsize_arrayset( 'TIMES_ARRAYSET', MAX_TIMES )


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


    call mem_free (obj%mindips)
    call mem_free (obj%maxdips)

!<execute_only>

    if( pc_do_not_process_traces() ) return
    obj%skip_wrapup = .false.

    call mem_alloc (obj%mindips, obj%ndpt)  ! min moveout at each trace sample.
    call mem_alloc (obj%maxdips, obj%ndpt)  ! max moveout at each trace sample.

    if( pc_do_not_process_traces() ) return   ! in case of allocation errors.

!----------resample the adjusted moveout arrays to each trace sample:

    call terputil_resample (obj%times,       obj%ntimes, adjusted_rmo_min, &
                            obj%tstrt, obj%dt, obj%ndpt, obj%mindips)

    call terputil_resample (obj%times,       obj%ntimes, adjusted_rmo_max, &
                            obj%tstrt, obj%dt, obj%ndpt, obj%maxdips)

!----------adjust from trace times to trace samples:

    obj%mindips(:) = obj%mindips(:) / obj%dt
    obj%maxdips(:) = obj%maxdips(:) / obj%dt
    obj%dipinc     = obj%rmo_inc    / obj%dt
    obj%dipmin     =     rmo_init   / obj%dt
        dipmax     =     rmo_last   / obj%dt
    obj%ndips      =     rmo_tot
    obj%nsmooth    = nint(obj%tim_smooth / obj%dt)
    obj%bigoff     = off_big
    obj%received   = 0
    obj%processed  = 0

!----------print information:

    write (obj%lun,3000)
    write (obj%lun,1002) nint(off_big)
    write (obj%lun,1003)      rmo_init , obj%dipmin
    write (obj%lun,1004)  obj%rmo_inc  , obj%dipinc
    write (obj%lun,1005)      rmo_last ,     dipmax
    write (obj%lun,1006)      rmo_tot
    write (obj%lun,1007) obj%tim_smooth, obj%nsmooth
    write (obj%lun,3000)
    write (obj%lun,2000)
    do indx = 1,obj%ntimes
        write (obj%lun,3000) indx,obj%times(indx),obj%rmo_min(indx),    &
                             obj%rmo_max(indx),nint(obj%off_max(indx)), &
                             adjusted_rmo_min(indx),adjusted_rmo_max(indx)
    end do
    write (obj%lun,3000)

1002 format ('  OFF_BIG  = biggest specified offset          = ',i6)
1003 format ('  RMO_INIT = minimum adjusted residual moveout = ',f7.3,  &
                                          ' seconds = ',f7.1,' trace samples')
1004 format ('  RMO_INC  = residual moveout increment        = ',f7.3,  &
                                          ' seconds = ',f7.1,' trace samples')
1005 format ('  RMO_LAST = maximum adjusted residual moveout = ',f7.3,  &
                                          ' seconds = ',f7.1,' trace samples')
1006 format ('  RMO_TOT  = number of moveouts to test        = ',i6)
1007 format ('  TIM_SMOOTH = smoothing parameter             = ',f7.3,  &
                                          ' seconds = ',i5,'   trace samples')

2000 format ('         TIMES   RMO_MIN  RMO_MAX    OFF_MAX &
                 &  ADJUSTED_RMO_MIN  ADJUSTED_RMO_MAX')
3000 format (1x,i3,1x,f9.3,1x,2f9.3,3x,i8,9x,2f9.3)


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

!</execute_only>


    return
    end subroutine vtrim_update


!<execute_only>

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


    subroutine vtrim( obj, ntr, hd, tr )
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vtrim_struct), intent(inout) :: obj
    integer           , intent(inout) :: ntr
    double precision  , intent(inout) :: hd(:,:)
    real              , intent(inout) :: tr(:,:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer           :: istart,istop
!-------------------------------------------------------------------------------

    if( ntr == NO_MORE_TRACES ) then
        call vtrim_wrapup( obj )
        return
    end if

    obj%received = obj%received  + 1

    call vtrim_range (obj,ntr,hd,tr,istart,istop)

    if (istart == 0 .or. istop == 0) return

    obj%processed = obj%processed + 1

    call vtrim_semblance (obj,ntr,hd,tr,istart,istop)
    call lav_set_hdr     (hd,tr,obj%ndpt,ntr)
    return
    end subroutine vtrim


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


    subroutine vtrim_wrapup( obj )
    implicit none
    type(vtrim_struct),intent(inout) :: obj       ! arguments

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    call pc_print ('VTRIM:',obj%received ,'trace gathers received')
    call pc_print ('VTRIM:',obj%processed,'trace gathers processed')
    call pc_print ('VTRIM:',obj%received - obj%processed, &
                                          'trace gathers unchanged')

    return
    end subroutine vtrim_wrapup


!!--------------------------- range ----------------------------------------!!
!!--------------------------- range ----------------------------------------!!
!!--------------------------- range ----------------------------------------!!

! Sets istart = first index on traces to use for semblance search.
! Sets istop  = last  index on traces to use for semblance search.
! Sets istart = istop = 0 in the following cases:
!  (1) the time range over which the live unmuted fold is at least 8
!       is less than 20 trace samples.
!  (2) the minimum offset for live traces is less than twice the maximum
!       offset.


    subroutine vtrim_range (obj,ntr,hd,tr,istart,istop)
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vtrim_struct), intent(inout) :: obj
    integer           , intent(inout) :: ntr
    double precision  , intent(inout) :: hd(:,:)
    real              , intent(inout) :: tr(:,:)
    integer           , intent(out)   :: istart,istop
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer           :: ntot,itr,imute1,imute2,indx
    real              :: offmin,offmax,offset
    integer           :: ifold(obj%ndpt)
!-------------------------------------------------------------------------------

    offmin   = 0.0
    offmax   = 0.0
    ntot     = 0
    ifold(:) = 0

    do itr = 1,ntr
        offset = hd(HDR_OFFSET     ,itr)
        imute1 = hd(HDR_TOP_MUTE   ,itr)
        imute2 = hd(HDR_BOTTOM_MUTE,itr)
        if(hd(HDR_LAV,itr) <=       0.0) cycle
        if(imute2 - imute1 < MINSAMPLES) cycle
        do indx = imute1,imute2
             if (tr(indx,itr) /= 0.0) ifold(indx) = ifold(indx) + 1
        end do
        ntot = ntot + 1
        if(ntot == 1) then
            offmin = offset
            offmax = offset
        else
            offmin = min(offmin, offset)
            offmax = max(offmax, offset)
        endif
    end do

    istart = 0
    istop  = 0

    if (offmax <= 2.0 * offmin .or. ntot < MINFOLD) return

    do indx = 1,obj%ndpt
        if (ifold(indx) <= MINFOLD/2) cycle
        if (istart == 0) istart = indx
                         istop  = indx
    end do

    if (istop - istart >= MINSAMPLES) return

    istart = 0
    istop  = 0
    return
    end subroutine vtrim_range


!!--------------------------- semblance ------------------------------------!!
!!--------------------------- semblance ------------------------------------!!
!!--------------------------- semblance ------------------------------------!!


    subroutine vtrim_semblance (obj,ntr,hd,tr,istart,istop)
    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    type(vtrim_struct), intent(inout) :: obj
    integer           , intent(in)    :: ntr
    double precision  , intent(inout) :: hd(:,:)
    real              , intent(inout) :: tr(:,:)
    integer           , intent(in)    :: istart,istop
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer        :: indx,itr,idip                  ,nmute  
    real           :: offset2,dip,shft,xmute1,xmute2
    integer        :: i1,i2,i3,i4
    real           :: w1,w2,w3,w4
    real,parameter :: DOP = 0.0
    real           :: tr2        (obj%ndpt)   ! scratch trace array.
    real           :: ta         (obj%ndpt)   ! exact input indices.
    real           :: stacked    (obj%ndpt)   ! stacked power.
    real           :: unstacked  (obj%ndpt)   ! unstacked power.
    real           :: semblance  (obj%ndpt)   ! semblances for a given dip.
    real           :: semblances (obj%ndpt)   ! best semblances.
    real           :: dips       (obj%ndpt)   ! best dips.

!----------initialize semblance arrays.

!   write(obj%lun,*) 'time range = ',obj%tstrt + (istart-1) * obj%dt, & !DEBUG
!                             ' to ',obj%tstrt + (istop -1) * obj%dt    !DEBUG

    semblances(:) = 0.0
    dips      (:) = FNIL

!----------look at each dip sequentially:

    do idip = 1,obj%ndips
        dip = obj%dipmin + (idip-1) * obj%dipinc

!----------shift each trace according to this dip and its offset:

        stacked  (:) = 0.0
        unstacked(:) = 0.0
        do itr = 1,ntr
            offset2 = (hd(HDR_OFFSET,itr) / obj%bigoff)**2
            shft = - dip * offset2
            call statcc (shft, obj%ndpt, tr(:,itr), tr2)
            do indx = istart,istop
                stacked  (indx) = stacked  (indx) + tr2(indx)
                unstacked(indx) = unstacked(indx) + tr2(indx)**2
            end do
        end do
        stacked(:) = stacked(:)**2

        where (unstacked(:) > 0.0)
                    semblance(:) = stacked(:) / (ntr * unstacked(:))
        elsewhere
                    semblance(:) = FNIL
        end where

!----------smooth, fill in, and extrapolate the semblance:

!       stacked(:) = semblance(:)                                     !DEBUG

        call statutil_1d_smooth (semblance, obj%ndpt, obj%nsmooth, wild=.true.)

!       if (idip == obj%ndips) then                                   !DEBUG
!           write(obj%lun,*) 'idip = ',idip                           !DEBUG
!           call printplot_histograms (stacked,semblance,      &      !DEBUG
!                           obj%ndpt,obj%tstrt,obj%dt,         &      !DEBUG
!                           'semb before','semb after',22,1.0)        !DEBUG
!       end if                                                        !DEBUG

!----------save the best semblance and dip for each trace sample:
!----------do not use dips outside of the desired range:

        do indx = istart,istop
            if (dip < obj%mindips(indx)) cycle
            if (dip > obj%maxdips(indx)) cycle
            if (semblance(indx) >= semblances(indx)) then
                semblances(indx) = semblance(indx)
                dips      (indx) = dip
            end if
        end do
    end do

!----------smooth, fill in, and extrapolate the best dips:

!   stacked(:) = dips(:)                                              !DEBUG

    call statutil_1d_smooth (dips, obj%ndpt, obj%nsmooth, wild=.true.)

!----------truncate the dips to the desired maxima:

    do indx = 1,obj%ndpt
        dips(indx) = max(dips(indx),obj%mindips(indx))
        dips(indx) = min(dips(indx),obj%maxdips(indx))
    end do

!   call printplot_histograms (stacked,dips,obj%ndpt,obj%tstrt, &     !DEBUG
!                      obj%dt,'dips before','dips after',44,1.0)      !DEBUG

!----------look at each trace sequentially:

    do itr = 1,ntr
        offset2 = (hd(HDR_OFFSET,itr) / obj%bigoff)**2

!----------get an array of input trace sample indices for each output index:

        do indx = 1,obj%ndpt
            shft = - dips(indx) * offset2
            ta(indx) = indx - shft
!           stacked(indx) = shft                                      !DEBUG
        end do

!----------make sure there are no time reversals in the array:

        do indx = 2,obj%ndpt
            ta(indx) = max(ta(indx), ta(indx-1) + 0.01)
        end do

!       write(obj%lun,*) 'itr = ',itr                                 !DEBUG
!       if (itr == ntr) then                                          !DEBUG
!       call printplot_histograms &                                   !DEBUG
!         (ta,stacked,obj%ndpt,obj%tstrt,obj%dt,'ta','shft',33,1.0)   !DEBUG
!       else                                                          !DEBUG
!       call printplot_histograms &                                   !DEBUG
!         (ta,stacked,obj%ndpt,obj%tstrt,obj%dt,'ta','shft',7,1.0)    !DEBUG
!       end if                                                        !DEBUG

!----------adjust the traces according to the best smoothed dips:

        call dyncc_forward (DOP, obj%ndpt, tr(:,itr), ta, tr2, nmute)
        tr(:,itr) = tr2(:)

!----------adjust the mute header words:

        xmute1 = hd(HDR_TOP_MUTE   ,itr)
        xmute2 = hd(HDR_BOTTOM_MUTE,itr)

!       write(obj%lun,*) 'nmute,xmute1,xmute2 = ',nmute,xmute1,xmute2  !DEBUG

        call terputil_binary_search (xmute1,ta,obj%ndpt,i1,i2,w1=w1,w2=w2)
        call terputil_binary_search (xmute2,ta,obj%ndpt,i3,i4,w1=w3,w2=w4)
        xmute1 = nint(i1 * w1 + i2 * w2)
        xmute2 = nint(i3 * w3 + i4 * w4)
        hd(HDR_TOP_MUTE   ,itr) = max(1,min(nint(xmute1),obj%ndpt))
        hd(HDR_BOTTOM_MUTE,itr) = max(1,min(nint(xmute2),obj%ndpt))

!       write(obj%lun,*) 'nmute,xmute1,xmute2 = ',nmute,xmute1,xmute2  !DEBUG

    end do
    return
    end subroutine vtrim_semblance


!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!
!!---------------------------- dump object ---------------------------------!!

!</execute_only>


    subroutine vtrim_dump_object( obj )
    implicit none
    type(vtrim_struct),intent(inout) :: obj
    integer                          :: i

901 format( / 1x, '   I    TIMES(I)  RMO_MIN(I)  RMO_MAX(I)  OFF_MAX(I)' / )
902 format( 1x, i4, 4f12.2 )

!-------------------------------------------------------------------------------

    write( obj%lun, '( / " ***** DUMP VTRIM OBJECT ***** " / )' )

    write( obj%lun, '( " skip_wrapup = ", l1       )' ) obj%skip_wrapup
    write( obj%lun, '( " ndpt        = ", i4       )' ) obj%ndpt
    write( obj%lun, '( " dt          = ", f8.3     )' ) obj%dt
    write( obj%lun, '( " tstrt       = ", f8.3     )' ) obj%tstrt
    write( obj%lun, '( " ntimes      = ", i4       )' ) obj%ntimes
    write( obj%lun, '( " tim_smooth  = ", f8.3     )' ) obj%tim_smooth

    write( obj%lun, 901 )
    do i = 1, obj%ntimes
        write( obj%lun, 902 ) &
                i, obj%times(i), obj%rmo_min(i), obj%rmo_max(i), obj%off_max(i)
    end do

    write( obj%lun, '( / " ***** END OF DUMP VTRIM OBJECT ***** " / )' )

    return
    end subroutine vtrim_dump_object


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module vtrim_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

