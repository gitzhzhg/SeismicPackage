!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- vtrimmer.f90 --------------------------------!!
!!--------------------------- vtrimmer.f90 --------------------------------!!
!!--------------------------- vtrimmer.f90 --------------------------------!!


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
!                        C P S   P R I M I T I V E
!
! Name       : VTRIMMER
! Category   : velocity
! Written    : 2001-02-09   by: Tom Stoeckley
! Revised    : 2006-06-12   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Utility routines for the VTRIM and VPICK processes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is a utility which is used by the VTRIM and VPICK process
! modules.  This utility previously resided as public routines within VTRIM.
!
! WARNING: Negative trace times are treated incorrectly by this module.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!   call vtrimmer_verify_scalars
!                     (tim_smooth, constraint, cmp_smooth, warning_msg)
!                          b           b           b            o
!
!                                i   i   i   i
!   call vtrimmer_verify_arrays (n1, n2, n3, n4,
!                     (times, rmo_min, rmo_max, off_max, ntimes, error_msg)
!                        b       b        b        b       o        o
!
!                   b    b   b    i     i     i    i       i
!   call vtrimmer (slow, hd, tr, ndpt, ntr, tstrt, dt, tim_smooth,
!
!                  times, rmo_min, rmo_max, off_max, ntimes, constraint, 
!                    i       i        i        i       i         i
!
!                  slast, cmp_smooth, iters, undo, ierr)
!                    b        i         b     i     o
!
! real              slow(ndpt)      = slowness squared at each trace sample.
! double precision  hd(nwih,ntr)    = trace headers.
! real              tr(ndpt,ntr)    = traces.
! integer           ndpt            = number of samples on each trace.
! integer           ntr             = number of traces.
! real              tstrt           = starting time on trace (seconds).
! real              dt              = sample interval on trace (seconds).
! real              tim_smooth      = length of temporal smoother (seconds).
! integer           n1,n2,n3,n4     = lengths of the following four arrays.
! real              times  (n1)     = zero-offset times (seconds).
! real              rmo_min(n2)     = lower velocity contraints.
! real              rmo_max(n3)     = upper velocity contraints.
! real              off_max(n4)     = maximum offset values.
! integer           ntimes          = number of zero-offset times.
! character(len=*)  constraint      = contraint for moveout adjustments.
! real              slast(ndpt)     = last slowness squared.
! integer           cmp_smooth      = number of CMPs to smooth over.
! integer           iters           = iterations.
! logical           undo            = whether to undo the previous NMO.
! integer           ierr            = error flag.
! character(len=*)  warning_msg     = warning message.
! character(len=*)  error_msg       = error message.
!
!   ierr = VTRIMMER_OK    if the gather was successfully adjusted.
!   ierr = VTRIMMER_SKIP  if the gather is unchanged.
!   ierr = VTRIMMER_ERROR if an error occurred.
!
! n1,n2,n3,n4 should all be the same, which is set into ntimes.
!
!   constraint               rmo_min and rmo_max
!   ----------   -----------------------------------------------------------
!   'RMO'        smallest (neg) and largest (pos) residual moveout (seconds)
!   'MINMAX'     minimum and maximum velocity
!   'TOLERANCE'  smallest (neg) and largest (pos) velocity change
!   'FRACTION'   smallest (neg) and largest (pos) fractional velocity change
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2006-06-12  Stoeckley  Allow CMP_SMOOTH to be zero.
!  3. 2001-02-26  Stoeckley  Add warning in documentation about the fact that
!                             negative trace times are treated incorrectly.
!  2. 2001-02-09  Goodger    Comment out DEBUG statements.
!  1. 2001-02-09  Stoeckley  Initial version, made from code in VTRIM which
!                             was called from VPICK.  The following changes
!                             were made to the code which was moved into this
!                             new primitive: Change RMO_MIN and RMO_MAX to be
!                             in seconds instead of millisecs (matching the
!                             VTRIM and VPICK documentation); add
!                             bottom mute header word adjustments; change
!                             header word named constants to those in the
!                             NAMED_CONSTANTS module; and increased number of
!                             iterations to fix under-corrections over some
!                             sections of the traces.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
! Debug printplots can be turned on by uncommenting the lines which are
! identified with the characters !DEBUG at the right end of the line.
! The primitive being used for these printplots is in Tom Stoeckley's
! directory but probably not in ~sps.
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module vtrimmer_module
      use named_constants_module
      use string_module
      use lav_module
      use terputil_module
!      use printplot_module                                           !DEBUG
      implicit none

      private
      public :: vtrimmer_verify_scalars
      public :: vtrimmer_verify_arrays
      public :: vtrimmer

      character(len=100),public,save :: VTRIMMER_IDENT = &
'$Id: vtrimmer.f90,v 1.4 2006/06/12 13:03:58 Stoeckley prod sps $'

      integer,public,parameter :: VTRIMMER_OK    = 0
      integer,public,parameter :: VTRIMMER_SKIP  = 1
      integer,public,parameter :: VTRIMMER_ERROR = 2

!      integer,private,parameter :: NROWS = 17                        !DEBUG
!      real   ,private,parameter :: FACTOR = 1.0e10                   !DEBUG

      contains


!----------------------- vtrimmer verify scalars ---------------------------!!
!----------------------- vtrimmer verify scalars ---------------------------!!
!----------------------- vtrimmer verify scalars ---------------------------!!


    subroutine vtrimmer_verify_scalars &
                     (tim_smooth, constraint, cmp_smooth, warning_msg)
    implicit none
    real            , intent(inout) :: tim_smooth
    character(len=*), intent(inout) :: constraint
    integer         , intent(inout) :: cmp_smooth
    character(len=*), intent(out)   :: warning_msg

    call string_to_upper (constraint)
    if      (constraint(1:1) == 'R') then ; constraint = 'RMO'
    else if (constraint(1:1) == 'M') then ; constraint = 'MINMAX'
    else if (constraint(1:1) == 'T') then ; constraint = 'TOLERANCE'
    else if (constraint(1:1) == 'F') then ; constraint = 'FRACTION'
    else                                  ; constraint = 'RMO'
    end if

    if( tim_smooth <= 0.0 ) then
        tim_smooth = 0.3
        warning_msg = 'Invalid value for parameter TIM_SMOOTH - changed to 0.3'
        return
    endif

    if( cmp_smooth < 0 ) then
        cmp_smooth = 3
        warning_msg = 'Invalid value for parameter CMP_SMOOTH - changed to 3'
        return
    endif

    warning_msg = ' '
    return
    end subroutine vtrimmer_verify_scalars


!----------------------- vtrimmer verify arrays ---------------------------!!
!----------------------- vtrimmer verify arrays ---------------------------!!
!----------------------- vtrimmer verify arrays ---------------------------!!


    subroutine vtrimmer_verify_arrays (n1,n2,n3,n4, &
                         times, rmo_min, rmo_max, off_max, ntimes, error_msg)
    implicit none
    integer         , intent(in)    :: n1,n2,n3,n4     ! should all == ntimes.
    real            , intent(inout) :: times(:)        ! (ntimes)
    real            , intent(inout) :: rmo_min(:)      ! (ntimes)
    real            , intent(inout) :: rmo_max(:)      ! (ntimes)
    real            , intent(inout) :: off_max(:)      ! (ntimes)
    integer         , intent(out)   :: ntimes
    character(len=*), intent(out)   :: error_msg
    integer                         :: indx                    ! local
    logical                         :: ascending               ! local
    integer                         :: indices  (n1)           ! local
    real                            :: temporary(n1)           ! local

!----------test equality of array lengths.

    if( n1 /= n2 .or. n2 /= n3 .or. n3 /= n4 ) then
        ntimes = min( n1, n2, n3, n4 )
        error_msg = 'The linked arrays have different lengths'
        return
    else
        ntimes = n1
    endif

!----------make sure the arrays are not empty.

    if( ntimes == 0 ) then
        error_msg = 'The linked arrays must have at least one row'
        return
    endif

!----------make sure the offsets are all positive.

    do indx = 1,ntimes
        if (off_max(indx) <= 0.0) then
            error_msg = 'The offsets must all be greater than zero'
            return
        end if
    end do

!----------test for ascending order of times.

    ascending = .true.
    do indx = 2,ntimes
        if (times(indx) <= times(indx-1)) then
             ascending = .false.
             exit
        end if
    end do

!----------sort into ascending order of times.

    if (.not.ascending) then
        call vtrimmer_shli( indices, ntimes, times )
        temporary = times(indices)
        times     = temporary
        temporary = rmo_min(indices)
        rmo_min   = temporary
        temporary = rmo_max(indices)
        rmo_max   = temporary
        temporary = off_max(indices)
        off_max   = temporary
    end if
    
    error_msg = ' '
    return
    end subroutine vtrimmer_verify_arrays


!!-------------------------------- vtrimmer -------------------------------!!
!!-------------------------------- vtrimmer -------------------------------!!
!!-------------------------------- vtrimmer -------------------------------!!


subroutine vtrimmer ( slow, hd, tr, ndpt, ntr, tstrt, dt, tim_smooth,       &
                      times, rmo_min, rmo_max, off_max, ntimes, constraint, &
                      slast, cmp_smooth, iters, undo, ierr )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer         , intent(in)    :: ndpt,ntr,ntimes
    real            , intent(in)    :: tstrt
    real            , intent(in)    :: dt,tim_smooth
    real            , intent(inout) :: slow(:)         ! (ndpt)
    double precision, intent(inout) :: hd(:,:)         ! (nwih,ntr)
    real            , intent(inout) :: tr(:,:)         ! (ndpt,ntr)
    real            , intent(in)    :: times(:)        ! (ntimes)
    real            , intent(in)    :: rmo_min(:)      ! (ntimes)
    real            , intent(in)    :: rmo_max(:)      ! (ntimes)
    real            , intent(in)    :: off_max(:)      ! (ntimes)
    character(len=*), intent(in)    :: constraint
    real            , intent(inout) :: slast(:)        ! (ndpt)
    integer         , intent(in)    :: cmp_smooth
    integer         , intent(inout) :: iters
    logical         , intent(in)    :: undo
    integer         , intent(out)   :: ierr
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real                            :: offmin,offmax,rnhalf
    integer                         :: indx,ntot
    real                            :: offsets(ntr)
!    real                            :: slowtemp(ndpt)                !DEBUG
!-----------------------------------------------

!----------SKIP IF NOT ENOUGH LIVE TRACES OR INSUFFICIENT OFFSET RANGE.

    offmin = 0.0
    offmax = 0.0
    ntot   = 0

    do indx = 1, ntr
        if( hd(HDR_LAV,indx)      <= 0.0 )                      cycle
        if( hd(HDR_TOP_MUTE,indx) >= hd(HDR_BOTTOM_MUTE,indx) ) cycle
        ntot = ntot + 1
        if( ntot == 1 ) then
            offmin = abs( hd(HDR_OFFSET,indx) )
            offmax = abs( hd(HDR_OFFSET,indx) )
        else
            offmin = amin1( abs( offmin ), real( hd(HDR_OFFSET,indx) ) )
            offmax = amax1( abs( offmax ), real( hd(HDR_OFFSET,indx) ) )
        endif
    end do

    if (offmax <= 2.0 * offmin .or. ntot < 8) then
         ierr = VTRIMMER_SKIP
         return
    end if

!----------SET UP OFFSETS(NTR) ARRAY WITH OFFSETS FROM HEADERS.

    do indx = 1, ntr
        offsets(indx) = hd(HDR_OFFSET,indx)
    end do

!----------UPDATE THE SLOWNESS FUNCTION.

    call vtrimmer_1 ( slow, tr, ndpt, ntr, tstrt, offsets, dt,      &
                      tim_smooth, times, rmo_min, rmo_max, off_max, &
                      ntimes, constraint, ierr )

    if (ierr /= VTRIMMER_OK) return

!----------APPLY LEAKY INTEGRATION OVER MIDPOINT TO THE SLOWNESS FUNCTION.

!slowtemp(1:ndpt) = slow(1:ndpt)                                       !DEBUG

    rnhalf = float(cmp_smooth)

    call vtrimmer_mvav( slow, slast, rnhalf, iters )

!call printplot_histograms &                                           !DEBUG
!       (slow,slowtemp,ndpt,tstrt,dt,'slow','slowtemp',NROWS,FACTOR)   !DEBUG

!----------APPLY MOVEOUT WITH SMOOTHED SLOWNESS FUNCTION.

    call vtrimmer_movt( hd, tr, ndpt, ntr, slow, dt, tstrt, offsets, undo )

!----------RESET LAV HEADER WORDS.

    call lav_set_hdr( hd, tr, ndpt, ntr )

    return
end subroutine vtrimmer


!!--------------------------- vtrimmer 1 -----------------------------------!!
!!--------------------------- vtrimmer 1 -----------------------------------!!
!!--------------------------- vtrimmer 1 -----------------------------------!!

! UPDATE STACKING SLOWNESS FROM GATHER, WITH LIMITED RESIDUAL MOVEOUTS

! CONSTRAINT = 'RMO':
!    RMO_MIN and RMO_MAX are interpreted as residual moveouts (in seconds)
!    after the NMO correction has been made with a user-specified velocity
!    file.  To choose values for RMO_MIN and RMO_MAX, apply normal moveout
!    corrections to a midpoint gather, then plot.  For a particular zero-offset
!    time, specify the largest unmuted offset OFF_MAX.  Residual moveouts
!    are measured as the zero-offset time of a reflection minus its corrected
!    time at offset OFF_MAX.  Set RMO_MIN to the smallest (most negative)
!    residual moveout, RMO_MAX to the largest (most positive).  An over-
!    corrected reflection (concave up) will have a negative residual
!    moveout, and an under-corrected reflection (concave down) will have
!    a positive residual moveout.
!
! CONSTRAINT = 'MINMAX':
!    RMO_MIN == minimum velocity for the corresponding zero-offset time.
!    RMO_MAX == maximum velocity for the corresponding zero-offset time.
!
! CONSTRAINT = 'TOLERANCE':
!    RMO_MIN == smallest (most neg) adjustment to reference velocity function.
!    RMO_MAX == largest  (most pos) adjustment to reference velocity function.
!
! CONSTRAINT = 'FRACTION':
!    RMO_MIN == smallest (most neg) fractional change to ref vel function.
!    RMO_MAX == largest  (most pos) fractional change to ref vel function.


subroutine vtrimmer_1                                               &
             ( slow, tr, ndpt, ntr, tstrt, offsets, dt, tim_smooth, &
               times, rmo_min, rmo_max, off_max, ntimes, constraint, ierr )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer         , intent(in)    :: ndpt,ntr,ntimes
    real            , intent(in)    :: tstrt
    real            , intent(in)    :: offsets(:)      ! (ntr)
    real            , intent(in)    :: dt,tim_smooth
    real            , intent(inout) :: slow(ndpt)
    real            , intent(inout) :: tr(:,:)         ! (ndpt,ntr)
    real            , intent(in)    :: times(:)        ! (ntimes)
    real            , intent(in)    :: rmo_min(:)      ! (ntimes)
    real            , intent(in)    :: rmo_max(:)      ! (ntimes)
    real            , intent(in)    :: off_max(:)      ! (ntimes)
    character(len=*), intent(in)    :: constraint
    integer         , intent(out)   :: ierr
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer, parameter         :: n2s = 81
    integer                    :: n1s, i1d, it0
    real                       :: o1s, o2s, d1s, d2s, e1s, e2s, t0z
    real                       :: dtminz, dtmaxz, xmaxz, r1, r2
    real   , dimension(ntimes) :: smin
    real   , dimension(ntimes) :: smax
    real   , dimension(ndpt)   :: smin2
    real   , dimension(ndpt)   :: smax2
    real   , allocatable       :: semb(:,:)
    real   , allocatable       :: slow2(:)

!-------------------------------------------------------------------------------

!----------SET UP SMIN AND SMAX ARRAYS.

    do it0 = 1, ntimes
        i1d = max(1,min(ndpt,int((times(it0)-tstrt)/dt+0.499)))
        xmaxz = off_max(it0)
        t0z = times(it0)

!----------DTMIN AND DTMAX ARE MOVED-OUT TIMES MINUS CORRECT ZERO-OFFSET TIMES.
!----------T*T = T0*T0 + X*X*S = (T0+DT0)*(T0+DT0) + X*X*(S+DS);
!----------DS = -(2*T0*DT0 + DT0*DT0)/(X*X);  SMIN AND SMAX ARE PERTURBATIONS
!----------TO THE REFERENCE SQUARED SLOWNESS, TO BE INTERPOLATED.
!----------THE ABOVE DT0'S ARE THE CORRECTIONS WE WOULD LIKE TO MAKE TO THE
!----------PRESENT T0'S.  THE USER-SPECIFIED RESIDUAL MOVEOUTS ARE THE ERRORS
!----------INTRODUCED INTO THE CORRECT VALUES.

        if (constraint == 'RMO') then
            dtmaxz    = - rmo_min(it0)
            dtminz    = - rmo_max(it0)
            t0z       = amax1( t0z, amax1( abs( dtmaxz ), abs( dtminz ) ) )
            smin(it0) = - ( 2.0 * t0z * dtmaxz ) / ( xmaxz * xmaxz )
            smax(it0) = - ( 2.0 * t0z * dtminz ) / ( xmaxz * xmaxz )

!----------DTMIN AND DTMAX ARE MIN AND MAX VELOCITIES.

        else if (constraint == 'MINMAX') then
            smin(it0) = 1.0 / ( rmo_max(it0) * rmo_max(it0) ) - slow(i1d)
            smax(it0) = 1.0 / ( rmo_min(it0) * rmo_min(it0) ) - slow(i1d)

!----------DTMIN AND DTMAX ARE FRACTIONAL CHANGES IN VELOCITY.

        else if (constraint == 'FRACTION') then
            dtmaxz    = ( 1.0 + rmo_max(it0) ) / sqrt( abs( slow(i1d) ) )
            dtminz    = ( 1.0 + rmo_min(it0) ) / sqrt( abs( slow(i1d) ) )
            smax(it0) = 1.0 / ( dtminz * dtminz ) - slow(i1d)
            smin(it0) = 1.0 / ( dtmaxz * dtmaxz ) - slow(i1d)

!----------DTMIN AND DTMAX ARE DELTA VELOCITIES.

        else if (constraint == 'TOLERANCE') then
            dtmaxz    = 1.0 / sqrt( abs( slow(i1d) ) ) + rmo_max(it0)
            dtminz    = 1.0 / sqrt( abs( slow(i1d) ) ) + rmo_min(it0)
            smax(it0) = 1.0 / ( dtminz * dtminz ) - slow(i1d)
            smin(it0) = 1.0 / ( dtmaxz * dtmaxz ) - slow(i1d)

!----------CRAZY CONSTRAINT VALUE.

        else
            ierr = VTRIMMER_ERROR
            return
        endif
    end do

!call printplot_histograms &                                           !DEBUG
!            (smin,smax,ntimes,1.0,1.0,'smin','smax',NROWS,FACTOR)     !DEBUG

    do i1d = 1, ndpt
        t0z = tstrt + float( i1d-1 ) * dt
        smin2(i1d) = terputil_interp( t0z, times, ntimes, smin )
        smax2(i1d) = terputil_interp( t0z, times, ntimes, smax )
        smin2(i1d) = smin2(i1d) + slow(i1d)
        smax2(i1d) = smax2(i1d) + slow(i1d)
    end do

!call printplot_histograms &                                           !DEBUG
!          (smin2,smax2,ndpt,tstrt,dt,'smin2','smax2',NROWS,FACTOR)    !DEBUG

!----------SET UP SEMBLANCE SAMPLING.

    n1s = min( 200, max( 20, int( 6.0 * float( ndpt ) * dt / tim_smooth ) ) )
    o1s = tstrt
    d1s = dt * float( ndpt-1 ) / float( max( 1, n1s-1 ) )
    e1s = o1s + float( n1s-1 ) * d1s

!----------GET ADEQUATE MIN AND MAX SLOWNESS TO SPAN STARTING W/CONSTRAINTS.

    o2s = minval( smin2 )
    e2s = maxval( smin2 )
    r1  = minval( smax2 )
    r2  = maxval( smax2 )
    o2s = amin1( o2s, r1 )
    e2s = amax1( e2s, r2 )
    r1  = minval( slow )
    r2  = maxval( slow )
    o2s = amin1( o2s, r1 )
    e2s = amax1( e2s, r2 )

!----------BROADEN SLIGHTLY.

    r1  = 0.01 * (e2s - o2s)
    o2s = o2s - r1
    e2s = e2s + r1
    d2s = (e2s - o2s) / float( max( 1, n2s-1 ) )

!----------ALLOCATE ARRAYS.

    allocate( semb(n1s,n2s) )
    allocate( slow2(n1s) )

!----------GET SEMBLANCE.

    call vtrimmer_semb( tr, ndpt, ntr, tstrt, offsets, dt, semb, &
                        n1s, n2s, o1s, o2s, d1s, d2s )

!call printplot_panel (semb,n1s,n2s,'semblance')                      !DEBUG

!----------ZERO OUT UNWANTED PART OF SEMBLANCES.

    call vtrimmer_zros( semb, smin2, smax2, n1s, n2s, o1s, o2s, d1s, d2s, &
                     ndpt, tstrt, dt )

!----------MAKE SURE STARTING FUNCTION IS WITHIN CONSTRAINTS.

    slow = amax1( smin2, amin1( smax2, slow ) )

!----------SUBSAMPLE ORIGINAL SLOWNESS.

!!call printplot_histograms &                                         !DEBUG
!           (slow,slow,ndpt,tstrt,dt,'slow','slow',NROWS,FACTOR)     !DEBUG

    call terputil_resample( tstrt,dt,ndpt,slow,   o1s,d1s,n1s,slow2 )

!call printplot_histograms &                                         !DEBUG
!           (slow2,slow2,n1s,o1s,d1s,'slow2','slow2',NROWS,FACTOR)   !DEBUG

!----------IMPROVE THE SLOWNESS FUNCTION.

    call vtrimmer_spck &
               ( slow2, semb, n1s, n2s, o1s, o2s, d1s, d2s, tim_smooth, 0 )

!call printplot_histograms &                                          !DEBUG
!            (slow2,slow2,n1s,o1s,d1s,'slow2','slow2',NROWS,FACTOR)   !DEBUG

!----------FULL SAMPLING.

    call terputil_resample( o1s,d1s,n1s,slow2,   tstrt,dt,ndpt,slow )

    slow = amax1( smin2, amin1( smax2, slow ) )

!call printplot_histograms &                                          !DEBUG
!            (slow,slow,ndpt,tstrt,dt,'slow','slow',NROWS,FACTOR)     !DEBUG

    deallocate( semb )
    deallocate( slow2 )

    ierr = VTRIMMER_OK
    return
end subroutine vtrimmer_1


!!---------------------------- vtrimmer mov --------------------------------!!
!!---------------------------- vtrimmer mov --------------------------------!!
!!---------------------------- vtrimmer mov --------------------------------!!

! UNDO = .true. UNDO NMO; ANYTHING ELSE FLATTENS HYPERBOLAS
! SHOULD VECTORIZE SECOND LOOP
! IF SLOW(1) == -1, THEN SCALE BY SIN(THETA)**2
! FIRST SAMPLE OF DATA IS LOST (FOR CONVENIENCE)
! (SLOW is the square of the slowness)


subroutine vtrimmer_mov( hd, tr, n1, slow, o1, d1, undo )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer         , intent(in)    :: n1
    logical         , intent(in)    :: undo
    real            , intent(in)    :: o1
    real            , intent(in)    :: d1
    double precision, intent(inout) :: hd  (:)   ! (n1)
    real            , intent(inout) :: tr  (:)   ! (n1)
    real            , intent(in)    :: slow(:)   ! (n1)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer           :: i1, it, n1last
    real              :: tau, dt, r, tau2, offset
    real              :: trnew(n1),indices(n1),scale(n1)

!----------PUT INPUT TRACE INTO TRNEW(:).

    trnew(1:n1) = tr(1:n1)

!----------INITIALIZE OUTPUT TRACE.

    tr(1:n1) = 0.0
    offset   = hd(HDR_OFFSET)

!----------SET UP MOVEOUT TIME TABLE IN INDICES(:).
!----------SCALE(:) IS FOR SCALING AMPLITUDES WITH OFFSET.

    do i1 = 2, n1
        tau     = o1 + (i1-1) * d1                   ! zero offset time.
 !      tau     = max(tau,0.0)                       ! added 1/23/01
        tau2    = tau*tau + offset*offset*slow(i1)   ! moved out time squared.
        indices(i1) = (sqrt(tau2) - o1) / d1 + 1.0
 !      indices(i1) = max(indices(i1),float(  1 ))  ! added 1/23/01
 !      indices(i1) = min(indices(i1),float(n1-1))  ! added 1/23/01
        if (tau2 == 0.0) then
            scale(i1) = 1.0
                   ! need this to be the same as for first positive tau?
        else if (slow(1) == -1) then
            scale(i1) = tau*tau / tau2
        else if (slow(1) == -2) then
            scale(i1) = offset*offset * slow(i1) / tau2
        else
            scale(i1) = 1.0
        endif
    end do

!----------SEE WHICH SAMPLE GOES OFF END OF GATHER.

    r = n1
    n1last = 1
    do i1 = 2, n1
        if( indices(i1) >= r ) exit
        n1last = i1
    end do

!----------APPLY INVERSE MOVEOUT BY CREATING HYPERBOLAS.

    if( undo ) then

        do i1 = 2, n1last
            it       = indices(i1)
            dt       = indices(i1) - it
            tr(it)   = tr(it)   + (1.0-dt) * trnew(i1)
            tr(it+1) = tr(it+1) +      dt  * trnew(i1)
        end do

!----------APPLY MOVEOUT BY FLATTENING HYPERBOLAS (LINEAR INTERPOLATION).

    else

        do i1 = 2, n1last
            it       = indices(i1)
            dt       = indices(i1) - it
            if (it < n1) then
              tr(i1) = ( (1.0-dt) * trnew(it) + dt * trnew(it+1) )
            else if (it == n1) then
              tr(i1) = trnew(it)
            end if
        end do

    endif

!----------MULTIPLY BY SCALE FACTOR.

    tr(2:n1) = tr(2:n1) * scale(2:n1)

!----------ADJUST MUTE HEADER WORDS.

    hd(HDR_TOP_MUTE)    = hd(HDR_TOP_MUTE)    + 2      - nint(indices(2     ))
    hd(HDR_BOTTOM_MUTE) = hd(HDR_BOTTOM_MUTE) + n1last - nint(indices(n1last))

    return
end subroutine vtrimmer_mov


!!---------------------------- vtrimmer movt -------------------------------!!
!!---------------------------- vtrimmer movt -------------------------------!!
!!---------------------------- vtrimmer movt -------------------------------!!

! MOVEOUT AN ENTIRE GATHER WITH SPECIFIED SQUARED SLOWNESS


subroutine vtrimmer_movt ( hd, tr, ndpt, ntr, slow, dt, tstrt, offsets, undo )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer         , intent(in)    :: ndpt,ntr
    logical         , intent(in)    :: undo
    real            , intent(in)    :: dt,tstrt
    real            , intent(inout) :: tr(:,:)     ! (ndpt,ntr)
    double precision, intent(inout) :: hd(:,:)     ! (nwih,ntr)
    real            , intent(in)    :: slow(:)     ! (ndpt)
    real            , intent(in)    :: offsets(:)  ! (ntr)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i2
!-------------------------------------------------------------------------------

    do i2 = 1, ntr
        call vtrimmer_mov  &
                 ( hd(:,i2), tr(:,i2), ndpt, slow, tstrt, dt, undo )
    end do

    return
end subroutine vtrimmer_movt


!!----------------------------- vtrimmer mvav ------------------------------!!
!!----------------------------- vtrimmer mvav ------------------------------!!
!!----------------------------- vtrimmer mvav ------------------------------!!

! CALCULATE MOVING AVERAGE FROM NEW DATA DNEW AND LAST AVERAGED DATA
! LEAKY INTEGRATION, RUNNING AVERAGE.  LAST AVERAGED DATA: DLAST.
! RNHALF IS THE NUMBER OF ITERATIONS AT WHICH
! THE PRESENT ELEMENT SHOULD CONTRIBUTE HALF AS MUCH
! BEGIN WITH ITER = 0, NO NEED TO SET DLAST


subroutine vtrimmer_mvav( dnew, dlast, rnhalf, iter )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(inout) :: iter
    real   , intent(in)    :: rnhalf
    real   , intent(inout) :: dnew(:)
    real   , intent(inout) :: dlast(:)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real :: alpha, r1, rhuse
!-------------------------------------------------------------------------------

    if( rnhalf < 0.1 ) return

    if( iter > 0 ) then

!-------IN EARLY ITERATIONS, SHORTEN DECAY HALFWIDTH TO HALF THE SAMPLES SEEN.

        rhuse = float( iter ) / 2.0
        rhuse = amin1( rnhalf, rhuse )
        alpha = exp( log(0.5) / rhuse )
        r1    = 1.0 - alpha
        dnew  = dnew * r1 + dlast * alpha
    else
        iter = 0
    endif

    dlast = dnew
    iter  = iter + 1

    return
end subroutine vtrimmer_mvav


!!----------------------------- vtrimmer cnv2 -------------------------------!!
!!----------------------------- vtrimmer cnv2 -------------------------------!!
!!----------------------------- vtrimmer cnv2 -------------------------------!!

! VECTORIZED CONVOLUTION AND ADJOINTS (CORRELATION)
! IF INIT = 1, THEN INITIALIZE OUTPUT WITH ZEROS.
! IF IDIR = 1 C IS OUTPUT (FORWARD CONVOLUTION) C = A*B
! IF IDIR = -1 A IS OUTPUT (ADJOINT CORRELATION)
! IF IDIR = -2 B IS OUTPUT (ADJOINT CORRELATION)
! IF INIT = 1 THEN INITIALIZE OUTPUT WITH ZEROS; OTHERWISE ADD ONTO THEM
! M1,M2, M3 ARE INDICES OF SAMPLES WITH ZERO LAG: A(M1), B(M2), C(M3)


subroutine vtrimmer_cnv2( a, b, c, n1, n2, n3, m1, m2, m3, idir, init )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n1,n2,n3
    integer, intent(in)    :: m1,m2,m3
    integer, intent(in)    :: idir,init
    real   , intent(inout) :: a(:)  ! (n1)
    real   , intent(inout) :: b(:)  ! (n2)
    real   , intent(inout) :: c(:)  ! (n3)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i1, i2, i3, k1a, k1b, k2a, k2b, k3a, k3b, m123
!-------------------------------------------------------------------------------

    if( init == 1 ) then
        if( idir ==  1 ) c = 0
        if( idir == -1 ) a = 0
        if( idir == -2 ) b = 0
    endif

    m123 = m1 + m2 - m3

    select case(idir)

    case(1)
        do i2 = 1, n2
            k3a = max(  1, i2 - m123 +  1 )
            k3b = min( n3, i2 - m123 + n1 )
            if( k3a <= k3b ) then
                i1 = (-i2) + k3a + m123
                c(k3a:k3b) = c(k3a:k3b) + a(i1:k3b-k3a+i1) * b(i2)
            endif
        end do

    case(-1)
        do i3 = 1, n3
            k1b = min( n1, i3 + m123 -  1 )
            k1a = max(  1, i3 + m123 - n2 )
            if( k1a <= k1b ) then
                i2 = i3 - k1a + m123
                a(k1a:k1b) = a(k1a:k1b) + b(i2:i2+k1a-k1b:(-1)) * c(i3)
            endif
        end do

    case(-2)
        do i1 = 1, n1
            k2a = max(  1, -i1 + m123 +  1 )
            k2b = min( n2, -i1 + m123 + n3 )
            if( k2a <= k2b ) then
                i3 = i1 + k2a - m123
                b(k2a:k2b) = b(k2a:k2b) + a(i1) * c(i3:k2b-k2a+i3)
            endif
        end do

    end select
    
    return
end subroutine vtrimmer_cnv2


!!------------------------------ vtrimmer fsm -------------------------------!!
!!------------------------------ vtrimmer fsm -------------------------------!!
!!------------------------------ vtrimmer fsm -------------------------------!!

! SMOOTH A ONE-DIMENSIONAL ARRAY A(N) WITH A GAUSSIAN
! FCUT IS THE CUTOFF FREQUENCY BANDWIDTH (HALF-MAX), ASSUMES SAMPLING
!   RATE EQUAL TO 1
! 1/FCUT IS THE WIDTH OF THE GAUSSIAN IN SAMPLES (FOR WHICH
!   AMPLITUDE > 0.5*MAX AMPLITUDE)


subroutine vtrimmer_fsm( a, n, fcutr )
    implicit none

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n
    real   , intent(in)    :: fcutr
    real   , intent(inout) :: a(:)  ! (n)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer              :: i, mean, ns
    real, dimension(401) :: s
    real                 :: r, fcutl, pi, fcut, sum1
    real                 :: wrk(n)
!-----------------------------------------------
    data ns    / 0 /
    data fcutl / 0.0 /
    data mean  / 0 /
    data s     / 401 * 0.0 /
    data pi    / 3.14159265359 /
!-------------------------------------------------------------------------------

!----------PROTECT INPUT VALUE.

    fcut = fcutr

!----------DON'T SMOOTH IF FCUT EQUAL TO ZERO.

    if( fcut == 0.0 .or. n <= 1 ) return

!----------IF HALFWIDTH MORE THAN 100 SAMPLES, THEN TRUCATE.  TOO BIG.

    if( 1.0/fcut > 100.0 ) fcut = 0.01

!----------INITIALIZE SMOOTHING FUNCTION IF NOT THE SAME AS THE LAST ONE USED.

    if( fcut /= fcutl ) then
        fcutl = fcut

!----------SET SPAN OF 3, AT WIDTH OF 1.5  EXP(-PI*1.5**2) = 1/1174.
!----------SHOULD BE ODD NUMBER FOR SYMMETRY.

        ns = int( 3.0/fcut + 0.5 )
        ns = 2 * int( ns/2 ) + 1

!----------MEAN IS THE POSITION OF THE ZERO IN THE SMOOTHING WAVELET.

        mean = ns / 2 + 1

!----------S(NS) IS THE SMOOTHING GAUSSIAN.

        do i = 1, ns
            r    = i - mean
            r    = - r * r * fcut * fcut * pi
            s(i) = exp( r )
        end do

!----------NORMALIZE TO UNIT AREA, WILL PRESERVE DC FREQUENCY AT FULL AMPLITUDE.
!----------FREQUENCY AT FCUT WILL BE HALF AMPLITUDE.

        sum1  = abs( sum( s(1:ns) ) )
        sum1  = amax1( sum1, 1.0e-16 )
        s(1:) = s(1:ns) / sum1
    endif

!----------REPLACE DRASTIC SMOOTHING BY AVERAGING.

    if( 1.01/fcutr > float(n) ) then
        r = sum( a ) / float( n )
        a = r

!----------CONVOLVE WITH GAUSSIAN.

    else
        call vtrimmer_cnv2( s, a, wrk, ns, n, n, mean, mean, mean, 1, 1 )
        a = wrk
    endif

    return
end subroutine vtrimmer_fsm


!!----------------------------- vtrimmer fsm1 -------------------------------!!
!!----------------------------- vtrimmer fsm1 -------------------------------!!
!!----------------------------- vtrimmer fsm1 -------------------------------!!

! IDIR = 1, 1D SMOOTHING WITH ZERO-SLOPE BOUNDS; -1, ADJOINT; 2, MIRRORS
! SET IDIR = -1 FOR ADJOINT OPERATION
! SET IDIR = 2, FOR MIRRORS AT BOUNDARIES (NO ADJOINT)
! NWRK = 9*NA; TESTED BY DOT PRODUCT TEST FOR TRUE ADJOINT.


subroutine vtrimmer_fsm1( a, na, fcut, idir )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: na,idir
    real   , intent(in)    :: fcut
    real   , intent(inout) :: a(:)  ! (na)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer           :: np, ns, kp1, kp2, ka, i
    real              :: r
    real, allocatable :: wrk(:)
!-------------------------------------------------------------------------------

    np   = 2.0 / (fcut + 1.0e-20) + 1.5
    np   = min( 2*na, np )
    ns   = na + 2 * np

    allocate( wrk(ns) )

    kp1 = 1
    ka  = kp1 + np
    kp2 = ka + na

    wrk(ka:ka+na-1) = a

    select case(idir)
    case(1)
        wrk(kp1:kp1+np-1) = a(1)
        wrk(kp2:kp2+np-1) = a(na)

        call vtrimmer_fsm( wrk(kp1:kp1+np-1), ns, fcut )

        a = wrk(ka:ka+na-1)

    case(-1)
        wrk(kp1:kp1+np-1) = 0.0
        wrk(kp2:kp2+np-1) = 0.0

        call vtrimmer_fsm( wrk, ns, fcut )

        a     = wrk(ka:ka+na-1)
        r     = sum( wrk(kp1:kp1+np-1) )
        a(1)  = a(1) + r
        r     = sum( wrk(kp2:kp2+np-1) )
        a(na) = a(na) + r

    case(2)
        i = min( np, na )
        wrk(kp1:kp1+np-1) = 0.0
        wrk(kp1:kp1+i -1) = a(1:i)

        call vtrimmer_rev1( wrk(kp1:kp1+np-1), np )

        wrk(kp2:kp2+np-1) = 0.0
        wrk(kp2:kp2+i -1) = a(na-np+1:na-np+i)

        call vtrimmer_rev1( wrk(kp2:kp2+np-1), np )
        call vtrimmer_fsm( wrk, ns, fcut )

        a = wrk(ka:ka+na-1)
    end select

    deallocate( wrk )
    return
end subroutine vtrimmer_fsm1


!!----------------------------- vtrimmer fsm2 -------------------------------!!
!!----------------------------- vtrimmer fsm2 -------------------------------!!
!!----------------------------- vtrimmer fsm2 -------------------------------!!

! TWO DIMENSIONAL SMOOTHING BY GAUSSIANS, SEE FSM FOR DOCUMENTATION


subroutine vtrimmer_fsm2( a, n1, n2, fcut1, fcut2 )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n1,n2
    real   , intent(in)    :: fcut1,fcut2
    real   , intent(inout) :: a(:,:)   ! (n1,n2)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: n2p, i1, i2
    real    :: wrk(n2)
!-------------------------------------------------------------------------------

    if( fcut1 > 0.0 .and. n1 > 1 ) then
        do i2 = 1, n2
            call vtrimmer_fsm( a(:,i2), n1, fcut1 )
        end do
    endif

    if( fcut2 <= 0.0 .or. n2 <= 1 ) return

    n2p = n2 + 1
    do i1 = 1, n1
        wrk = a(i1,:)
        call vtrimmer_fsm( wrk, n2, fcut2 )
        a(i1,:) = wrk
    end do

    return
end subroutine vtrimmer_fsm2


!!----------------------------- vtrimmer parm -------------------------------!!
!!----------------------------- vtrimmer parm -------------------------------!!
!!----------------------------- vtrimmer parm -------------------------------!!

! SEARCH AN INTERVAL (0,1) FOR A MINIMUM, PARABOLAS AND GOLDEN SECTION


subroutine vtrimmer_parm( xmin, xerr, xnew, fnew, iter )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(inout) :: iter
    real   , intent(out)   :: xmin,xerr,xnew
    real   , intent(in)    :: fnew
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer               :: inew
    integer, dimension(4) :: ix, ifx
    integer               :: imin
    integer, dimension(3) :: ix3
    integer               :: ip1, ip2, ip3, igood
    real   , dimension(4) :: x, fx
    real                  :: rgold
    real   , dimension(3) :: dx
    real                  :: r1, r2
    logical               :: ltenth, lgold, ll
!-----------------------------------------------
    data inew  / 0 /
    data x     / 4 * 0.0 /
    data fx    / 4 * 0.0 /
    data rgold / 0.0 /
    data dx    / 3 * 0.0 /
    data imin  / 1 /
!-------------------------------------------------------------------------------

    select case( iter )

!----------FIRST ITERATION INITIALIZATION.

    case( 0 )
        imin = 1
        inew = 1
        rgold = 0.5 * (sqrt( 5.0 ) - 1.0)

!----------STARTING SAMPING POINTS.

        x(1) = 0.0
        x(2) = 1.0 - rgold
        x(3) = rgold
        x(4) = 1.0

!----------DX ARE ERRORS.

        dx = 1.0

!----------2ND AND 3RD ITERATION, FILL IN FX.

    case( 1 : 3 )
        fx(inew) = fnew
        inew = inew + 1

!----------FOUR AND MORE ITERATIONS, F'S ARE FULL.

    case default
        fx(inew) = fnew

!----------INDEX BY INCREASING X AND BY INCREASING FX.

        call vtrimmer_shli( ix , 4, x  )
        call vtrimmer_shli( ifx, 4, fx )

!----------MINIMUM VALUE OF FUNCTION.

        imin = ifx(1)

!----------X(INEW) SHOULD BE REPLACED.

        inew = ix(1)
        if( imin == ix(1) .or. imin == ix(2) ) inew = ix(4)

!----------SET FLAG IF SMALLEST X WAS ORIGINAL MINIMUM OF INTERVAL.

        ltenth = x(imin) == 0.0

!----------IF WE HAVE A SMALLEST OR LARGEST X, THEN WE DO NOT HAVE ENOUGH
!----------POINTS FOR A PARABOLIC METHOD.  SET FLAG FOR GOLDEN SECTION.

        lgold = imin == ix(4) .or. imin == ix(1) .or. ltenth
    
!----------DX'S ARE THE SUCCESSIVE ERRORS, DX(2) LAST, DX(3) BEFORE LAST,
!----------DX(1) TO BE FOR THIS ONE.

        dx(3) = dx(2)
        dx(2) = dx(1)

!----------LOOK AT THREE POINTS NOT TO BE REPLACED.

        if( inew == ix(1) ) ix3 = ix(2:4)
        if( inew == ix(4) ) ix3 = ix(1:3)

!----------IP1, IP2, AND IP3 ARE INDICES FOR THREE POINTS TO BE SAVED, IN
!----------ASCENDING ORDER OF X.

        ip1 = ix3(1)
        ip2 = ix3(2)
        ip3 = ix3(3)

!----------CALCULATE DISTANCE BETWEEN TWO POINTS THAT SPAN MINIMUM = ERROR.

        dx(1) = x(ip3) - x(ip1)

!----------USE GOLDEN SECTION IF ERRORS HAVE NOT BEEN DECREASING RAPIDLY ENOUGH.
!----------(ERROR MUST BE LESS THAN HALF AFTER TWO PREVIOUS ITERATIONS)

        lgold = lgold .or. dx(1) > 0.5 * dx(3)

!----------R1 IS THE SPAN OF LEFT INTERVAL, R1 OF THE RIGHT.

        r1 = x(ip2) - x(ip1)
        r2 = x(ip3) - x(ip2)

!----------USE PARABOLIC METHOD, IF HAVE NO PREVIOUS OBJECTIONS.

        if( .not. lgold ) then
            call vtrimmer_prz( x(inew), igood, x(ip1), fx(ip1), &
                                  x(ip2), fx(ip2), x(ip3), fx(ip3) )

!----------IF PARABOLIC SEARCH WAS NUMERICALLY STABLE, THEN RETURN.
!----------(STRAIGHT LINES HAVE NO INTERIOR MINIMUM, FOR INSTANCE)
!----------IF THE NEW POINT TO BE SEARCHED IS ALREADY THE MINIMUM POINT,
!----------THEN MIGHT HAVE ALREADY CONVERGED.  IN CASE NOT, GO ON AND
!----------DO GOLDEN SECTION TO REDUCE THE WIDTH OF THE INTERVAL SEARCHED.

            lgold = .not. ( (igood == 1) .and. (x(inew) /= x(imin)) )
        endif

!----------IF GOLDEN FLAG SET OR PARABOLIC SEARCH FAILED, DO GOLDEN SEARCH.

        if( lgold  ) then
            ll = r1 > r2

!----------FIND NEW X TO INTERPOLATE IN LARGER LEFT INTERVAL.

            if( ll ) then
                x(inew) = x(ip1) + rgold * (x(ip2)-x(ip1))

!----------FIND NEW X TO INTERPOLATE IN LARGER RIGHT INTERVAL.

            else
                x(inew) = x(ip3) - rgold * (x(ip3)-x(ip2))
            endif

!----------IF SMALLEST X HAS THE MINIMUM, THEN SCALE DOWN DRASTICALLY.
!----------WE MAY HAVE ORDERS OF MAGNITUDE TO GO.

            if( ltenth ) x(inew) = x(ip1) + 0.1 * (x(ip2)-x(ip1))
        endif

    end select

!----------THE FOLLOWING IS THE ONLY VALID EXIT FROM THIS ROUTINE.

    iter = iter + 1

!----------NOTE PRESENT BEST VALUE OF X.

    xmin = x(imin)

!----------NOTE PRESENT SIZE OF INTERVAL SPANNING THE MINIMUM.

    xerr = dx(1)

!----------GIVE THE VALUE OF X THAT NEEDS TO BE SEARCHED, USER WILL RETURN FNEW.

    xnew = x(inew)

    return
end subroutine vtrimmer_parm


!!------------------------------ vtrimmer prz -------------------------------!!
!!------------------------------ vtrimmer prz -------------------------------!!
!!------------------------------ vtrimmer prz -------------------------------!!

! FITS PARABOLA TO THREE POINTS AND RETURNS MINIMUM
! CALLED BY PARMIN
! X'S SHOULD BE IN ASCENDING ORDER
! WILL FIT PARABOLA TO THREE POINTS AND RETURN THE MINIMUM VALUE XZ
! IF THREE POINTS DEFINE A POOR PARABOLA, THEN WILL RETURN IGOOD = -1
! LESS THAN X1


subroutine vtrimmer_prz( xz, igood, x1, f1, x2, f2, x3, f3 )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(out) :: igood
    real   , intent(out) :: xz
    real   , intent(in)  :: x1,f1,x2,f2,x3,f3
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    real :: a, b, xm
!-------------------------------------------------------------------------------

    igood = 1
    xm    = ( x2 - x1 ) / ( x3 - x1 )

!----------FIND MINIMUM OF PARABOLA.
!----------DON'T TRY IF XM TOO CLOSE TO ENDPOINTS.

    if( xm <= 0.99 .and. xm >= 0.01 ) then
        a = ( (f3-f1)*xm - (f2-f1) ) / (xm - xm*xm)
        b = f3 - f1 - a
        if( b*a < 0.0 .and. -0.5*b < a ) then
            xz = - 0.5 * b / a
        else
            igood = -1
            xz    =  0.5
        endif
    endif

    xz = xz * (x3 - x1) + x1

    return
end subroutine vtrimmer_prz


!!----------------------------- vtrimmer rev1 -------------------------------!!
!!----------------------------- vtrimmer rev1 -------------------------------!!
!!----------------------------- vtrimmer rev1 -------------------------------!!

! REVERSE THE ORDER OF A ONE-DIMENSIONAL ARRAY, IN PLACE


subroutine vtrimmer_rev1( x, n )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n
    real   , intent(inout) :: x(:)     ! (n)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i1, i2, nh
    real    :: r
!-------------------------------------------------------------------------------

    nh = n / 2
    do i1 = 1, nh
        i2    = n - i1 + 1
        r     = x(i1)
        x(i1) = x(i2)
        x(i2) = r
    end do

    return
end subroutine vtrimmer_rev1


!!----------------------------- vtrimmer semb -------------------------------!!
!!----------------------------- vtrimmer semb -------------------------------!!
!!----------------------------- vtrimmer semb -------------------------------!!

! FIND SEMBLANCE VELOCITY STACK. SLOW(N1S,N2S) IS ONLY OUTPUT.
! NW IS WIDTH OF WAVELET LENGTH


subroutine vtrimmer_semb( tr, ndpt, ntr, tstrt, offsets, dt, slow, &
                          n1s, n2s, o1s, o2s, d1s, d2s )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: ndpt,ntr,n1s,n2s
    real   , intent(in)    :: tstrt,dt,o1s,o2s,d1s,d2s
    real   , intent(inout) :: tr(:,:)        ! (ndpt,ntr)
    real   , intent(in)    :: offsets(:)     ! (ntr)
    real   , intent(out)   :: slow(:,:)      ! (n1s,n2s)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i2s, i1d, nd, i2d, i1s, nwh, nw
    real    :: r, offset, rdiv, s, tau, t, eps
    real    :: wrk1(ndpt), wrk2(ndpt)
!-------------------------------------------------------------------------------

    nw   = int( d1s/dt + 0.1 )
    rdiv = 1.0 / float( ntr )
    nd   = ndpt * ntr
    nwh  = int( float( nw )/2.0 + 0.6 )

    do i2s = 1, n2s
        s = (i2s-1) * d2s + o2s

!----------INITIALIZE THE SUMMING ARRAYS.
!----------SET THE MIN OF THE DENOMINATOR TO 1/100TH OF THE DATA VARIANCE.

        wrk1 = 0.0
        wrk2 = 0.0

        do i2d = 1, ntr
            offset = offsets(i2d)
            tr(1,i2d)   = 0.0
            tr(ndpt,i2d) = 0.0
            do i1s = 1,ndpt
                tau       = (i1s-1) * dt + tstrt
                t         = sqrt( amax1( 0.0, tau*tau + s*offset*offset ) )
                i1d       = max( 1, min( ndpt, int( (t-tstrt)/dt + 1.5) ) )
                wrk1(i1s) = wrk1(i1s) + rdiv * tr(i1d,i2d)
                wrk2(i1s) = wrk2(i1s) + rdiv * tr(i1d,i2d) * tr(i1d,i2d)
            end do
        end do

        wrk1 = wrk1 * wrk1
        
!----------ADD UP WAVELET WINDOW BEFORE DIVIDING.

        r = 1.0 / float( nw )
        call vtrimmer_fsm( wrk1, ndpt, r )
        call vtrimmer_fsm( wrk2, ndpt, r )

!----------CALCULATE WHITENING FOR DENOMINATOR.

        eps = sum( wrk2 )
        eps = 0.01 * eps / float( ndpt )

        if( eps == 0.0 ) then
            slow = 0.0
            return
        endif

!----------DIVIDE STACKED ENERGY BY TOTAL ENERGY.

        do i1s = 1,ndpt
            wrk1(i1s) = amin1( wrk2(i1s), wrk1(i1s) ) / (wrk2(i1s) + eps)
        end do

        r = 0.7 * float( n1s ) / float( ndpt )
        call vtrimmer_fsm     ( wrk1, ndpt, r )
        call terputil_resample( tstrt,dt,ndpt,wrk1,   o1s,d1s,n1s,slow(:,i2s) )
    end do

    return
end subroutine vtrimmer_semb


!!----------------------------- vtrimmer shl2 -------------------------------!!
!!----------------------------- vtrimmer shl2 -------------------------------!!
!!----------------------------- vtrimmer shl2 -------------------------------!!

! REORDERS INDX SO THAT ARR(INDX(I)) IS A MONTONICALLY INCREASING
!  FUNCTION OF I; INDX IS ALREADY INITIALIZED BY USER


subroutine vtrimmer_shl2( indx, n, arr, narr )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n
    integer, intent(in)    :: narr
    integer, intent(inout) :: indx(:)  ! (n)
    real   , intent(in)    :: arr(:)   ! (n)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: m, k, i, l, it, j
!-------------------------------------------------------------------------------

    m = n / 2

    do while( m > 0 )
        k = n - m

        do j = 1, k
            i = j
            l = i + m

            if( arr(indx(l)) >= arr(indx(i)) ) cycle

            it      = indx(i)
            indx(i) = indx(l)
            indx(l) = it
            i       = i - m

            do while( i >= 1 )
                l = i + m
                if( arr(indx(l)) >= arr(indx(i)) ) exit

                it      = indx(i)
                indx(i) = indx(l)
                indx(l) = it
                i       = i - m
            end do

        end do

        m = m / 2
    end do

    return
end subroutine vtrimmer_shl2


!!----------------------------- vtrimmer shli -------------------------------!!
!!----------------------------- vtrimmer shli -------------------------------!!
!!----------------------------- vtrimmer shli -------------------------------!!

! REORDERS INDX SO THAT ARR(INDX(I)) IS A MONTONICALLY INCREASING
!  FUNCTION OF I; ASSUMES INDX SPANS 1 <= INDX <= N


subroutine vtrimmer_shli( indx, n, arr )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)  :: n
    integer, intent(out) :: indx(:)  ! (n)
    real   , intent(in)  :: arr(:)   ! (n)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: j1
!-------------------------------------------------------------------------------

    indx = (/ ( j1, j1 = 1, n ) /)

    if( n > 1 ) call vtrimmer_shl2( indx, n, arr, n )

    return
end subroutine vtrimmer_shli


!!----------------------------- vtrimmer spck -------------------------------!!
!!----------------------------- vtrimmer spck -------------------------------!!
!!----------------------------- vtrimmer spck -------------------------------!!

! PICK SMOOTH SLOW2(S1) TO MAXIMIZE INTEGRAL SEMB(S1, SLOW2(S1)) DS1
! SET INITIA = 1 TO INITIALIZE SLOW2(N1S) IN THE CENTER OF THE SECTION.


subroutine vtrimmer_spck &
             ( slow2, semb, n1s, n2s, o1s, o2s, d1s, d2s, tim_smooth, initia )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n1s
    integer, intent(in)    :: n2s
    integer, intent(in)    :: initia
    real   , intent(in)    :: o1s
    real   , intent(in)    :: o2s
    real   , intent(in)    :: d1s
    real   , intent(in)    :: d2s
    real   , intent(in)    :: tim_smooth
    real   , intent(inout) :: slow2(:)      ! (n1s)
    real   , intent(in)    :: semb(:,:)  ! (n1s,n2s)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer                     :: nter, ns2s, nparm, is2s, i1s, iparm, iter
    real                        :: s2s0, s2sn, s2sfac, s2s, fcut1, fcut2, r
    real   , dimension(2)       :: x
    real                        :: f, v
    real   , dimension(2)       :: dfds
    real                        :: xmin, xerr, xnew, fnew, okerr, var, okerr2
    real   , dimension(n1s,n2s) :: sem2
    real   , dimension(n1s)     :: best
    real   , dimension(n1s)     :: grad
    real   , dimension(n1s)     :: test
!    character(len=12)           :: header                            !DEBUG
!-----------------------------------------------
    data nter   / 3 /
    data ns2s   / 4 /
    data nparm  / 20 /
    data okerr  / 0.001 /
    data okerr2 / 0.0001 /
!-------------------------------------------------------------------------------

!nter = 10      ! temporary
ns2s = 10      ! temporary

!----------INITIALIZE WORK ARRAYS TO ZERO.

    sem2 = 0.0
    best = 0.0
    grad = 0.0
    test = 0.0

!----------AUTOMATIC PARAMETERS.

    s2s0   = float( n2s ) / 3.0
    s2sn   = 1.0
    s2sfac = exp( log( s2sn/s2s0 ) / float( max( 1, ns2s-1 ) ) )
    s2s    = s2s0
    fcut1  = d1s / tim_smooth

!----------INITIALIZE BEST FUNCTION SLOW2(N1S) INTO BEST.

    if( initia == 1 ) then
        r = o2s + 0.5 * (n2s-1) * d2s
        best = r
    else
        best = slow2
    endif

!call printplot_histograms &                                          !DEBUG
!            (slow2,best,n1s,o1s,d1s,'slow2','best',NROWS,FACTOR)     !DEBUG

!----------BIG ITERATIONS, WITH SMOOTHING OVER SLOW2.

    do is2s = 1, ns2s
        fcut2 = 1.0 / s2s

!----------SMOOTH SEMBLANCE PANEL, INTO SEM2.

        sem2 = semb
        call vtrimmer_fsm2( sem2, n1s, n2s, 0.0, fcut2 )

!write(header,"('is2s=',i3)") is2s                                    !DEBUG
!call printplot_panel (sem2,n1s,n2s,header)                           !DEBUG

!----------ITERATE WITH NTER DIFFERENT GRADIENT DESCENTS.

        do iter = 1,nter

!----------GET GRADIENT.

            do i1s = 1, n1s
                x(1) = o1s + (i1s-1) * d1s
                x(2) = best(i1s)
                f = terputil_interp             &
                        ( x(1), o1s, d1s, n1s,  &
                          x(2), o2s, d2s, n2s, sem2, dfds(1), dfds(2))
                grad(i1s) = dfds(2)
            end do

!call printplot_histograms &                                          !DEBUG
!            (grad,grad,n1s,o1s,d1s,'grad','grad',NROWS,FACTOR)       !DEBUG

            call vtrimmer_fsm1( grad, n1s, fcut1, -1 )

!call printplot_histograms &                                          !DEBUG
!            (grad,grad,n1s,o1s,d1s,'grad','grad',NROWS,FACTOR)       !DEBUG

!----------SET MAXIMUM DEVIATION OF GRADIENT TO S2S.

            var = s2s * d2s
            v   = sum( grad * grad ) / float( n1s )
            if( v /= 0.0 ) v = sqrt( var / v )
            grad = v * grad

!call printplot_histograms &                                          !DEBUG
!            (grad,grad,n1s,o1s,d1s,'grad','grad',NROWS,FACTOR)       !DEBUG

!----------FIND BEST SCALE FACTOR FOR GRADIENT.

            iparm = 0
            call vtrimmer_parm( xmin, xerr, xnew, fnew, iparm )

            do while( xerr>=okerr2 .and. xerr>=okerr*xmin .and. iparm<=nparm )

!----------PUT TEST FUNCTION INTO TEST, SMOOTH PERTURBATION FIRST.

                test = grad
                call vtrimmer_fsm1( test, n1s, fcut1, 1 )
                test = best + test * xnew

!----------INTEGRATE SEMBLANCE ALONG THIS TEST LINE.

                fnew = 0.0
                do i1s = 1, n1s
                    x(1) = o1s + (i1s-1) * d1s
                    x(2) = test(i1s)
                    f = terputil_interp         &
                        ( x(1), o1s, d1s, n1s,  &
                          x(2), o2s, d2s, n2s, sem2, dfds(1), dfds(2))
                    fnew = fnew + f
                end do
                fnew = - fnew
                call vtrimmer_parm( xmin, xerr, xnew, fnew, iparm )
            end do

!----------UPDATE FUNCTION WITH BEST SCALED PERTURBATION.

            test = grad
            call vtrimmer_fsm1( test, n1s, fcut1, 1 )
            test = best + test * xmin
            best = test

!call printplot_histograms &                                        !DEBUG
!            (best,best,n1s,o1s,d1s,'best','best',NROWS,FACTOR)     !DEBUG

        end do

!----------TRY AGAIN WITH LESS SMOOTHING.

        s2s = s2s * s2sfac
    end do

!----------COPY FINAL BEST FUNCTION.

    slow2 = best

    return
end subroutine vtrimmer_spck


!!----------------------------- vtrimmer zros -------------------------------!!
!!----------------------------- vtrimmer zros -------------------------------!!
!!----------------------------- vtrimmer zros -------------------------------!!

! ZERO PART OF A SLOWNESS MATRIX
! SET SMIN OR SMAX TO ZERO TO IGNORE THESE LIMITS


subroutine vtrimmer_zros( sp, smin, smax, n1s, n2s, o1s, o2s, d1s, d2s, &
                       ndpt, tstrt, dt )

    implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
    integer, intent(in)    :: n1s
    integer, intent(in)    :: n2s
    integer, intent(in)    :: ndpt
    real   , intent(in)    :: o1s
    real   , intent(in)    :: o2s
    real   , intent(in)    :: d1s
    real   , intent(in)    :: d2s
    real   , intent(in)    :: tstrt
    real   , intent(in)    :: dt
    real   , intent(inout) :: sp(:,:)  ! (n1s,n2s)
    real   , intent(in)    :: smin(:)  ! (ndpt)
    real   , intent(in)    :: smax(:)  ! (ndpt)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
    integer :: i1s, i2s
    real    :: s, t, rsmin, rsmax
!-------------------------------------------------------------------------------

    if( smin(1) == -1.0 .and. smax(1) == -1.0 ) return

    do i2s = 1, n2s
        s = o2s + float(i2s-1) * d2s
        
        do i1s = 1, n1s
          t = o1s + float(i1s-1) * d1s
          
          rsmin = terputil_interp( t, tstrt, dt, ndpt, smin )
          rsmax = terputil_interp( t, tstrt, dt, ndpt, smax )

          if( (s-rsmin)*(s-rsmax) > 0.0 ) sp(i1s,i2s) = 0.0
        end do
    end do
    
    return
end subroutine vtrimmer_zros


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module vtrimmer_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

