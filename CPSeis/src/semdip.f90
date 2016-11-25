
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- semdip.f90 --------------------------------!!
!!------------------------------- semdip.f90 --------------------------------!!
!!------------------------------- semdip.f90 --------------------------------!!


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
! Name       : SEMDIP 
! Category   : math
! Written    : 1988-12-01   by: Tom Stoeckley
! Revised    : 2004-06-08   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Semblance dip search and mix.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!        Generates an output trace that is a weighted sum of the
!        input traces, summed along dips found with a semblance
!        search.
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
!                   i   i   b    i      i      i        i    i    i
!     CALL SEMDIP (NTR, HD, TR, ndpt,   dt, win_len,   nhx, nhy, nhw,
!                  DIPX, DIPY, deltax, deltay, HD2, TR2, adjust,
!                   i     i      i       i      i    o     i
!                                                   opt   opt
!
!                        opt                opt      
!                         i                  i        
!                  quick_dip_weights, quick_dip_search,
!                  semb_trace, xdip_trace, ydip_trace, maxdip_trace, azim_trace)
!                      o           o           o            o            o
!                     opt         opt         opt          opt          opt
!
! WRONG:           semb_trace, xdip_trace, ydip_trace, win_moveup, win_smooth)
! WRONG:               o           o           o           i           i
! WRONG:              opt         opt         opt         opt         opt
!
!----------PARAMETERS DESCRIBING THE INPUT TRACES:
!
! integer NTR          = number of input traces to mix together.
! double  HD(nwih,ntr) = headers of input traces (first dim may exceed NWIH).
! real    TR(ndpt,ntr) = input traces            (first dim may exceed NDPT).
! integer NDPT         = number of data samples in each trace.
! real    DT           = trace sample interval (seconds).
!
! NWIH (number of words in header) is not explicitly needed by this subroutine.
!
!----------HEADER WORD PARAMETERS:
!
! integer nhx = header# (>0) in HD and HD2 containing X coordinate.
! integer nhy = header# (>0) in HD and HD2 containing Y coordinate.
! integer nhw = header# (>0) in HD containing weight given to the trace.
!
!----------CONTROL PARAMETERS:
!
! real win_len    = semblance window length in seconds.
! real win_moveup = semblance window moveup dist in secs (default win_len/2).  (NOT USED)
! real win_smooth = semblance smoothing distance in secs (default win_len/2).  (NOT USED)
! real DIPX       = maximum X-dip in milliseconds/unit header word value (>=0).
! real DIPY       = maximum Y-dip in milliseconds/unit header word value (>=0).
! real deltax     = X-dip increment in DIPX units (>0).
! real deltay     = Y-dip increment in DIPY units (>0).
! integer ADJUST  = which input traces to dip-adjust.
!
!   If SEMDIP_ADJUST_ALL     : all input traces are dip-adjusted.
!   If SEMDIP_ADJUST_POSITIVE: input traces with weight>0 are dip-adjusted.
!   If SEMDIP_ADJUST_NONE    : the input traces are unchanged (default).
!
! logical quick_dip_weights = true to skip some traces for a faster dip search.
! logical quick_dip_search  = true to do a faster two-pass dip search.
!
!   Default value of QUICK_DIP_WEIGHTS is FALSE.
!   Default value of QUICK_DIP_SEARCH  is FALSE.
!
!----------PARAMETERS DESCRIBING THE OUTPUT TRACES:
!
! double HD2(nwih) = header of output trace (must be pre-set upon input).
! real   TR2(ndpt) = output trace (mixed or interpolated along dips).
!
! real   semb_trace(ndpt) = semblance for best dips found (0 to 1).
! real   xdip_trace(ndpt) = best X-dips found (DIPX units).
! real   ydip_trace(ndpt) = best Y-dips found (DIPY units).
! real maxdip_trace(ndpt) = best max-dips found.
! real   azim_trace(ndpt) = azimuth of best max-dips found.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!  1. The output trace can be thought of as a stacked trace, a mixed
!     trace, or an interpolated trace, depending on the weights.  This
!     output trace is located at the position given by the the (X,Y)
!     coordinates preset by the user in HD2(NHX) and HD2(NHY).
!
!  2. This primitive is used by processes SDIP, SDIP3D, EDA3D, TERP,
!     IMS, RTC, and potentially other processes.
!
!  3. Traces with NEGATIVE weight are NOT used either in the dip search
!     or in generating the mixed trace.  Dead traces are not used regardless
!     of their weights.
!
!  4. The dip search is done using all traces equally, regardless
!     of weights.  Weights are used in generating the mixed (or
!     interpolated) trace.  Traces with ZERO weights ARE used in
!     the dip search, but NOT in the mix.
!
!  5. Dip search in the X direction is not done if DIPX < DELTAX/2.
!     Dip search in the Y direction is not done if DIPY < DELTAY/2.
!     For 2-D data, DIPX (or DIPY) should be set to zero.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!!!!! future revision:
! 14. 2003-04-30  Stoeckley    Replace DYNCC_QUICK call with CUBETERP_CUBIC
!                               as a result of streamlining the DYNCC primitive
!                               and moving some of its code to the new CUBETERP
!                               primitive.


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
! 13. 2004-06-08  Stoeckley    Change method of calculating semblances when
!                               quick_dip_search is true.
! 12. 2002-09-04  Stoeckley    Fix bug regarding zeroing muted data.
! 11. 2001-12-10  Stoeckley    Honor mute time for speedup.
! 10. 2001-11-14  Stoeckley    Add optional arguments to provide additional
!                               dip search capabilities (moved from SDIPUTIL);
!                               change the way output diagnostic arrays are
!                               returned; return diagnostic arrays with same
!                               length as input traces (so number of allowed
!                               windows is no longer relevant); replace
!                               semblance windows with sample-by-sample
!                               calculation plus smoothing; remove TSTRT
!                               argument; move location of WIN_LEN argument;
!                               speed up dynamic trace adjustments by calling
!                               new faster routine in the DYNCC primitive.
!  9. 2001-08-17  Stoeckley    Increase the number of allowed windows from 200
!                               to 1200, and make this number a public named
!                               constant; change to always calculate semblance
!                               even if not doing a dip search; make sure that
!                               the semblance never falls outside of the range
!                               0 to 1 even due to roundoff limitations.
!  8. 2000-10-20  Stoeckley    Add missing required documentation section.
!  7. 2000-04-25  Stoeckley    Changed public output variables to private ones
!                               with an access subroutine.
!  6. 1999-11-24  Stoeckley    Converted from old system.
!  5. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  4. 1993-09-20  Stoeckley    Fix bug relating to the dip in the Y direction.
!  3. 1989-04-11  Stoeckley    Correct DT/TSTRT bug.
!  2. 1989-05-01  Stoeckley    Removed some parameters, added negative
!                               weights, added common block SEMDIP1, and
!                               made some simplifications.
!  1. 1988-12-01  Stoeckley    Initial version.
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module semdip_module
      use named_constants_module
      use statcc_module
      use dyncc_module      ! to be removed later.
  !   use cubeterp_module   ! future replacement for dyncc.
      use statutil_module
      implicit none
      private
      public :: semdip

      character(len=100),public,save :: SEMDIP_IDENT = &
       '$Id: semdip.f90,v 1.13 2004/06/08 12:59:35 Stoeckley prod sps $'

      integer,parameter,public :: SEMDIP_ADJUST_ALL      = 3
      integer,parameter,public :: SEMDIP_ADJUST_POSITIVE = 2
      integer,parameter,public :: SEMDIP_ADJUST_NONE     = 1

      contains


!!----------------------------- semdip ------------------------------------!!
!!----------------------------- semdip ------------------------------------!!
!!----------------------------- semdip ------------------------------------!!


      subroutine semdip (NTR,HD,TR,ndpt,   dt,win_len,   nhx,nhy,nhw,  &
                         DIPX,DIPY,deltax,deltay,                      &
                         HD2,TR2,adjust,                               &
                         quick_dip_weights,quick_dip_search,           &
                         semb_trace, xdip_trace, ydip_trace,           &
                         maxdip_trace, azim_trace)
      implicit none
      integer         ,intent(in)    :: ntr,ndpt           ! arguments
      double precision,intent(in)    :: HD(:,:)            ! arguments(nwih,ntr)
      real            ,intent(inout) :: TR(:,:)            ! arguments(ndpt,ntr)
      real            ,intent(in)    :: dt,win_len         ! arguments
      integer         ,intent(in)    :: nhx,nhy,nhw        ! arguments
      real            ,intent(in)    :: dipx,dipy          ! arguments
      real            ,intent(in)    :: deltax,deltay      ! arguments
      double precision,intent(in)    :: HD2(:)             ! arguments (nwih)
      real   ,optional,intent(out)   :: TR2(:)             ! arguments (ndpt)
      integer,optional,intent(in)    :: ADJUST             ! arguments
      logical,optional,intent(in)    :: quick_dip_weights  ! arguments
      logical,optional,intent(in)    :: quick_dip_search   ! arguments
      real   ,optional,intent(out)   :: semb_trace(:)      ! arguments (ndpt)
      real   ,optional,intent(out)   :: xdip_trace(:)      ! arguments (ndpt)
      real   ,optional,intent(out)   :: ydip_trace(:)      ! arguments (ndpt)
      real   ,optional,intent(out)   :: maxdip_trace(:)    ! arguments (ndpt)
      real   ,optional,intent(out)   :: azim_trace(:)      ! arguments (ndpt)
      real                           :: semblances(ndpt)   ! local
      real                           :: xdips     (ndpt)   ! local
      real                           :: ydips     (ndpt)   ! local
      real                           :: xdummy    (ndpt)   ! local
      real                           :: ydummy    (ndpt)   ! local
      real                           :: sembx     (ndpt)   ! local
      real                           :: semby     (ndpt)   ! local
      real                           :: xcoords(ntr)       ! local
      real                           :: ycoords(ntr)       ! local
      real                           :: weights(ntr)       ! local
      integer                        :: nsize,imute        ! local
      logical                        :: quick_dip_weights2 ! local
      logical                        :: quick_dip_search2  ! local
      real                           :: TR3(ndpt,ntr)      ! local
  !   integer                        :: i,j                ! local (debug)
  !   logical,save                   :: debug = .true.     ! local (debug)

!----------initialize variables:

      call semdip_init   (NTR,HD,TR,ndpt,  hd2,              &
                          dt,win_len,nhx,nhy,nhw,            &
                          nsize,xcoords,ycoords,weights,imute)

      quick_dip_weights2 = .false.
      quick_dip_search2  = .false.

      if (present(quick_dip_weights)) quick_dip_weights2 = quick_dip_weights
      if (present(quick_dip_search )) quick_dip_search2  = quick_dip_search

!----------do two orthogonal dip searches for speedup:

      if (quick_dip_search2) then

           call semdip_search (NTR,TR,ndpt,nsize,imute,       &
                               xcoords,ycoords,weights,       &
                               DIPX,0.0,deltax,deltay,        &
                               sembx,xdips,ydummy,            &
                               quick_dip_weights2)

           call semdip_search (NTR,TR,ndpt,nsize,imute,       &
                               xcoords,ycoords,weights,       &
                               0.0,DIPY,deltax,deltay,        &
                               semby,xdummy,ydips,            &
                               quick_dip_weights2)

    !      semblances(1:ndpt) = 0.5 * (sembx(1:ndpt) + semby(1:ndpt))

           tr3(:,:) = tr(1:ndpt,1:ntr)

           call semdip_mix    (NTR,TR3,ndpt,imute,        &
                               xcoords,ycoords,weights,   &
                               dipx,dipy,                 &
                               xdips,ydips,               &
                               SEMDIP_ADJUST_ALL)

           call semdip_search (NTR,TR3,ndpt,nsize,imute,      &
                               xcoords,ycoords,weights,       &
                               0.0,0.0,deltax,deltay,         &
                               semblances,xdummy,ydummy,      &
                               quick_dip_weights2)

!----------do a full dip search:

      else

           call semdip_search (NTR,TR,ndpt,nsize,imute,       &
                               xcoords,ycoords,weights,       &
                               DIPX,DIPY,deltax,deltay,       &
                               semblances,xdips,ydips,        &
                               quick_dip_weights2)
      end if

!----------debug printouts:

   !  if (debug) then
   !    if (.not. quick_dip_search2) then
   !         sembx(:) = 0.0
   !         semby(:) = 0.0
   !    end if
   !    print *, ' '
   !    print 2000, weights
   !    2000 format (7f5.1)
   !    print *, ' '
   !    print *, quick_dip_search2,quick_dip_weights2
   !    print *, ntr,ndpt,dipx,dipy,deltax,deltay
   !    print *, ' '
   !    j = min(ndpt/2+40,ndpt)
   !    do i = ndpt/2,j
   !         print 1000, i,xdips(i),ydips(i),sembx(i),semby(i), &
   !                         0.5*(sembx(i)+semby(i)),semblances(i)
   !         1000 format (i4,3(f9.2,f7.2))
   !    end do
   !    print *, ' '
   !    debug = .false.
   !  end if

!----------do the dip mix:

      call semdip_mix    (NTR,TR,ndpt,imute,         &
                          xcoords,ycoords,weights,   &
                          dipx,dipy,                 &
                          xdips,ydips,               &
                          ADJUST,  TR2)

!----------output optional diagnostic traces:

      if (present(semb_trace))   semb_trace  (1:ndpt) = semblances(1:ndpt)
      if (present(xdip_trace))   xdip_trace  (1:ndpt) = xdips     (1:ndpt)
      if (present(ydip_trace))   ydip_trace  (1:ndpt) = ydips     (1:ndpt)
      if (present(maxdip_trace)) maxdip_trace(1:ndpt) = sqrt(xdips(1:ndpt)**2 + ydips(1:ndpt)**2)
      if (present(azim_trace))   azim_trace  (1:ndpt) = atan2(xdips(1:ndpt), ydips(1:ndpt)) * DEGREES_PER_RADIAN
      return
      end subroutine semdip


!!----------------------------- semdip init -------------------------------!!
!!----------------------------- semdip init -------------------------------!!
!!----------------------------- semdip init -------------------------------!!

! xcoords (ntr)  = modified X coords of each trace wrt center coordinate.
! ycoords (ntr)  = modified Y coords of each trace wrt center coordinate.
! weights (ntr)  = weights of each trace (only weights >= 0 are used).


      subroutine semdip_init (NTR,HD,TR,ndpt,  hd2,              &
                              dt,win_len,nhx,nhy,nhw,            &
                              nsize,xcoords,ycoords,weights,imute)
      implicit none
      integer         ,intent(in)    :: ntr,ndpt         ! arguments
      double precision,intent(in)    :: HD(:,:)          ! arguments (nwih,ntr)
      real            ,intent(in)    :: TR(:,:)          ! arguments (ndpt,ntr)
      double precision,intent(in)    :: HD2(:)           ! arguments (nwih)
      real            ,intent(in)    :: dt,win_len       ! arguments
      integer         ,intent(in)    :: nhx,nhy,nhw      ! arguments
      integer         ,intent(out)   :: nsize            ! arguments
      real            ,intent(out)   :: xcoords(:)       ! arguments (ntr)
      real            ,intent(out)   :: ycoords(:)       ! arguments (ntr)
      real            ,intent(out)   :: weights(:)       ! arguments (ntr)
      integer         ,intent(out)   :: imute            ! arguments
      integer                        :: i                ! local
      real                           :: wtot,dtms        ! local

!----------get nsize.

      nsize = nint(win_len/DT) + 1
      if (nsize <    1) nsize = 1
      if (nsize > ndpt) nsize = ndpt

!----------get xcoords(:), ycoords(:), and weights(:).

      wtot = 0.0
      dtms = 1000.0 * dt
      DO I=1,NTR
           xcoords(i) = (hd(nhx,i) - hd2(nhx)) / dtms
           ycoords(i) = (hd(nhy,i) - hd2(nhy)) / dtms
           weights(i) = hd(nhw,i)
           if (xcoords(i) > -0.01 .and. xcoords(i) < 0.01) xcoords(i) = 0.0
           if (ycoords(i) > -0.01 .and. ycoords(i) < 0.01) ycoords(i) = 0.0
           if (weights(i) < 0.0) then
                continue
           else if (hd(25,i) == 0.0) then
                weights(i) = -1.0
           else if (weights(i) > 0.0) then
                wtot = wtot + weights(i)
           end if
      end do
      if (wtot > 0.0) then
           where (weights(1:ntr) > 0.0) weights(1:ntr) = weights(1:ntr) / wtot
      end if

!----------get imute.

      imute = ndpt
      DO I=1,NTR
           if (weights(i) >= 0.0) imute = min(imute,nint(hd(2,i)))
      end do
      imute = max(imute-3,1)      ! keep a little bit of filter tail.
      return
      end subroutine semdip_init


!!----------------------------- semdip search -----------------------------!!
!!----------------------------- semdip search -----------------------------!!
!!----------------------------- semdip search -----------------------------!!

! xcoords (ntr)    = modified X coords of each trace wrt center coordinate.
! ycoords (ntr)    = modified Y coords of each trace wrt center coordinate.
! weights (ntr)    = weights of each trace (only weights >= 0 are used).
! dipx,dipy        = maximum dips to search   (ms per unit X or Y coord).
! deltax,deltay    = dip increments to search (ms per unit X or Y coord).
! semblances(ndpt) = trace of semblances.
! xdips     (ndpt) = trace of dips in X direction (ms per unit X coord).
! ydips     (ndpt) = trace of dips in Y direction (ms per unit Y coord).


      subroutine semdip_search (NTR,TR,ndpt,nsize,imute,     &
                                xcoords,ycoords,weights,     &
                                DIPX,DIPY,deltax,deltay,     &
                                semblances,xdips,ydips,      &
                                quick_dip_weights2)
      implicit none
      integer  ,intent(in)  :: ntr,ndpt,nsize,imute      ! arguments
      real     ,intent(in)  :: TR(:,:)                   ! arguments (ndpt,ntr)
      real     ,intent(in)  :: xcoords(:)                ! arguments (ntr)
      real     ,intent(in)  :: ycoords(:)                ! arguments (ntr)
      real     ,intent(in)  :: weights(:)                ! arguments (ntr)
      real     ,intent(in)  :: dipx,dipy                 ! arguments
      real     ,intent(in)  :: deltax,deltay             ! arguments
      real     ,intent(out) :: semblances(:)             ! arguments (ndpt)
      real     ,intent(out) :: xdips     (:)             ! arguments (ndpt)
      real     ,intent(out) :: ydips     (:)             ! arguments (ndpt)
      logical  ,intent(in)  :: quick_dip_weights2        ! arguments
      integer               :: i,j,ngood,nhalf           ! local
      integer               :: midx,midy,nx,ny,kx,ky     ! local
      real                  :: xx,yy,ww                  ! local
      real                  :: xdip,ydip,semb,shft       ! local
      real                  :: flattened(NDPT)           ! local
      real                  :: stacked  (NDPT)           ! local
      real                  :: unstacked(NDPT)           ! local

!----------INITIALIZE LOCAL VARIABLES.

      MIDX  = nint(DIPX/deltax) + 1
      MIDY  = nint(DIPY/deltay) + 1
      NX    = 2*MIDX - 1
      NY    = 2*MIDY - 1
      nhalf = nsize/2

!----------INITIALIZE OUTPUT ARRAYS.

      semblances(1:ndpt) = 0.0
      xdips     (1:ndpt) = 0.0
      ydips     (1:ndpt) = 0.0

!----------LOOK AT EACH DIP SEQUENTIALLY.

      DO KY = 1,NY
      DO KX = 1,NX
           YDIP         = (MIDY-KY)*deltay
           XDIP         = (MIDX-KX)*deltax
           stacked  (:) = 0.0
           unstacked(:) = 0.0
           NGOOD        = 0

!----------SHIFT EACH TRACE ACCORDING TO THIS DIP AND ITS COORDINATES.

           DO I = 1,NTR
                xx = xcoords(i)
                yy = ycoords(i)
                ww = weights(i)
                if (ww < 0.0) cycle

                if (ww == 0.0 .and. quick_dip_weights2) then
      !!          if (nx       == 1 .and. ny       == 1) cycle ! removed 5/13/03
                  if (nint(xx) /= 0 .and. nint(yy) /= 0) cycle
      !!          if (nx       == 1 .and. nint(xx) /= 0) cycle ! removed 5/13/03
      !!          if (ny       == 1 .and. nint(yy) /= 0) cycle ! removed 5/13/03
                end if

                SHFT = - xx*XDIP - yy*YDIP
                CALL STATCC (SHFT,NDPT-imute+1,TR(imute:,I), flattened(imute:))
                DO J = imute,NDPT
                     stacked  (J) = stacked  (J) + flattened(J)
                     unstacked(J) = unstacked(J) + flattened(J)**2
                end do
                NGOOD = NGOOD + 1
           end do

           DO J = imute,NDPT
                stacked  (J) = stacked(J)**2
                unstacked(J) = ngood * unstacked(J)
           end do

!----------SMOOTH THE STACKED AND UNSTACKED TRACES.

           call statutil_1d_smooth_quick (  stacked(imute:),ndpt-imute+1,nsize)
           call statutil_1d_smooth_quick (unstacked(imute:),ndpt-imute+1,nsize)

!----------CALCULATE SEMBLANCE FOR EACH TRACE SAMPLE.

           DO J = imute,ndpt
                if (unstacked(j) > 0.0) then
                     semb = stacked(j) / unstacked(j)
                else
                     semb = 0.0
                end if

!----------SAVE THESE DIPS IF THIS SEMBLANCE IS THE BEST SO FAR.

                if (SEMB > semblances(J)) then
                     semblances(J) = SEMB
                     xdips     (J) = XDIP
                     ydips     (J) = YDIP
                end if
           end do
      end do
      end do

!----------smooth the output arrays.

      call statutil_1d_smooth_quick (semblances(imute:), ndpt-imute+1, nsize)
      call statutil_1d_smooth_quick (xdips     (imute:), ndpt-imute+1, nsize)
      call statutil_1d_smooth_quick (ydips     (imute:), ndpt-imute+1, nsize)

      call statutil_1d_smooth_quick (semblances(imute:), ndpt-imute+1, nsize/2)
      call statutil_1d_smooth_quick (xdips     (imute:), ndpt-imute+1, nsize/2)
      call statutil_1d_smooth_quick (ydips     (imute:), ndpt-imute+1, nsize/2)
      return
      end subroutine semdip_search


!!----------------------------- semdip mix --------------------------------!!
!!----------------------------- semdip mix --------------------------------!!
!!----------------------------- semdip mix --------------------------------!!

! xcoords (ntr) = modified X coords of each trace wrt center coordinate.
! ycoords (ntr) = modified Y coords of each trace wrt center coordinate.
! weights (ntr) = weights of each trace (only weights > 0 are used).
! xdips  (ndpt) = trace of dips in X direction (ms per unit X coord).
! ydips  (ndpt) = trace of dips in Y direction (ms per unit Y coord).


      subroutine semdip_mix (NTR,TR,ndpt,imute,             &
                             xcoords,ycoords,weights,       &
                             dipx,dipy,                     &
                             xdips,ydips,                   &
                             adjust,   TR2)
      implicit none
      integer         ,intent(in)    :: ntr,ndpt,imute   ! arguments
      real            ,intent(inout) :: TR(:,:)          ! arguments (ndpt,ntr)
      real            ,intent(in)    :: xcoords(:)       ! arguments (ntr)
      real            ,intent(in)    :: ycoords(:)       ! arguments (ntr)
      real            ,intent(in)    :: weights(:)       ! arguments (ntr)
      real            ,intent(in)    :: dipx,dipy        ! arguments
      real            ,intent(in)    :: xdips  (:)       ! arguments (ndpt)
      real            ,intent(in)    :: ydips  (:)       ! arguments (ndpt)
      integer,optional,intent(in)    :: adjust           ! arguments
      real   ,optional,intent(out)   :: TR2(:)           ! arguments
      integer                        :: i,j              ! local
      real                           :: ww,xx,yy         ! local
      real                           :: flattened(NDPT)  ! local
      real                           :: exact            ! local
      logical                        :: adjust_all       ! local
      logical                        :: adjust_pos       ! local
      logical                        :: already_flat     ! local
      logical                        :: need_adjust      ! local
      logical                        :: need_mixing      ! local

!----------INITIALIZE OUTPUT ARRAY.

      if (present(adjust)) then
           adjust_all = (adjust == SEMDIP_ADJUST_ALL)
           adjust_pos = (adjust == SEMDIP_ADJUST_POSITIVE)
      else
           adjust_all = .false.
           adjust_pos = .false.
      end if

      if (present(tr2)) TR2(1:ndpt) = 0.0

!----------DIP ADJUSTMENT AND MIX.

      DO I = 1,NTR
           XX = xcoords(I)
           YY = ycoords(I)
           WW = weights(I)

           already_flat = (xx*dipx == 0.0 .and. yy*dipy == 0.0)
           need_adjust  = (adjust_all .or. (adjust_pos .and. ww > 0.0))
           need_adjust  = (need_adjust .and. .not.already_flat)
           need_mixing  = (present(tr2) .and. ww > 0.0)

           if (.not.need_adjust .and. .not.need_mixing) cycle

           if (already_flat) then
                flattened(1:ndpt) = tr(1:ndpt,i)
           else
                flattened(1:imute-1) = tr(1:imute-1,i)
                DO j = imute,ndpt
                     exact        = j + XX*xdips(j) + YY*ydips(j) - imute + 1
                     flattened(j) = dyncc_quick  &
                                        (tr(imute:,i),ndpt-imute+1,exact)
   ! future:         flattened(j) = cubeterp_cubic  &
   ! future:                            (tr(imute:,i),ndpt-imute+1,exact)
                end do
           end if

           if (need_adjust) then
                  TR(1:ndpt,i) = flattened(1:ndpt)
           end if

           if (need_mixing) then
                DO J = 1,NDPT
                     TR2(J) = TR2(J) + WW*flattened(J)
                end do
           end if
      end do
      return
      end subroutine semdip_mix


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module semdip_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

