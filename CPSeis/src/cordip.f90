
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- cordip.f90 --------------------------------!!
!!------------------------------- cordip.f90 --------------------------------!!
!!------------------------------- cordip.f90 --------------------------------!!


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
! Name       : CORDIP 
! Category   : math
! Written    : 1989-05-01   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Build base trace (pilot trace) by alignment by correlation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive generates a base trace from individual traces by combining
! them after alignment by correlation.  Separate subroutines correlate and
! align an individual trace with a base trace.
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
!                    CALLING SEQUENCE FOR CORDIP
!
!                    i    b     i    i        i   i      i   i   i   i
!      CALL CORDIP (NTR,HDMIX,TRMIX,NDPT,   TSTRT,DT,   NHX,NHY,NHW,NHB,
!                   MSCTFB,DIPX,DIPY,TWIN,NWT,   HDBASE,TRBASE)
!                     i     i    i    i    i       i      o
!
!  This subroutine creates a base trace from the input traces.
!
!----------PARAMETERS DESCRIBING THE INPUT TRACES:
!
! integer NTR           = number of input traces to use for building base trace.
! double  HDMIX(nwih,NTR) = headers of input traces (first dim may exceed NWIH).
! real    TRMIX(ndpt,NTR) = input traces            (first dim may exceed NDPT).
! integer NDPT          = number of data samples in each trace.
! real    TSTRT         = time of first trace sample (seconds).
! real    DT            = trace sample interval (seconds).
!
!----------HEADER WORD PARAMETERS:
!
! integer NHX = header# (>0) in HDMIX and HDBASE containing X coordinate.
! integer NHY = header# (>0) in HDMIX and HDBASE containing Y coordinate.
! integer NHW = header# (>0) in HDMIX containing a flag given to the trace.
! integer NHB = header# (>0) in HDMIX containing a flag given to the trace.
!                           See notes 3 and 4.
!
!----------CONTROL PARAMETERS:
!
! real MSCTFB      = Maximum shift (milliseconds) for central input trace in
!                      forming the base trace.
! real DIPX        = additional shift in X dir to allow in forming the base
!                      trace, in milliseconds/unit header word value (>=0).
! real DIPY        = additional shift in Y dir to allow in forming the base
!                      trace, in milliseconds/unit header word value (>=0).
! real TWIN(2,NWT) = top and bottom of correlation window in seconds,
!                      specified in pairs, for one or more windows.
! integer NWT      = number of correlation windows (one or more - no limit).
!
!----------PARAMETERS DESCRIBING THE OUTPUT BASE TRACE:
!
! double HDBASE(nwih) = header of output trace (must be pre-set upon input).
! real   TRBASE(ndpt) = output base trace.
!
!----------NOTES:
!
! NWIH (number of words in header) is not explicitly needed by this subroutine.
!
!-------------------------------------------------------------------------------
!                  CALLING SEQUENCE FOR CORDIP_CORR
!
!                           i      i    i       i     i   o   i
!      CALL CORDIP_CORR (SHIFTMAX,TWIN,NWT,   TRBASE,TRI,TRO,NDPT,
!                        TSTRT,DT,   STATIC,NGOOD)
!                          i   i       o      o
!
!  This subroutine correlates a trace with a base trace, and adjusts
!  this trace (statically or dynamically) to match the base trace.
!
! real    SHIFTMAX     = Maximum shift (milliseconds) to allow.
! real    TWIN(2,NWT)  = top and bottom of correlation window in seconds,
!                          specified in pairs, for one or more windows.
! integer NWT          = number of correlation windows (one or more - no limit).
! real    TRBASE(NDPT) = base trace to correlate with.
! real    TRI   (NDPT) = trace to correlate with base.
! real    TRO   (NDPT) = static or dynamic adjustment of trace TRI to match
!                          base trace (RETURNED).
! integer NDPT         = number of data samples in trace.
! real    TSTRT        = time of first trace sample (seconds).
! real    DT           = trace sample interval (seconds).
! real    STATIC       = average static in sample points (RETURNED).
! integer NGOOD        = number of good correlations (RETURNED).
!
!-------------------------------------------------------------------------------
!                 CALLING SEQUENCE FOR CORDIP_MORE
!
!                          i      i      i    i      i    i   o   i
!      CALL CORDIP_MORE (CCMIN,SHIFTMAX,TWIN,NWT,   BASE,TRI,TRO,NDPT,
!                        TSTRT,DT,   STATIC,CCBEST,NGOOD,
!                          i   i       o      o      o
!                        AVCC,NTOTAL,NPASS,AVST,STLO,STUP,CCWIN,STWIN)
!                         b     b      b    b    b    b     o     o
!
!  All parameters are the same as for CORDIP_CORR, with the following
!  additions:
!
! real    CCMIN  = minimum correlation coefficient to use for getting a shift.
! real    CCBEST = best correlation coefficient of all windows (RETURNED).
!
!  The following statistical summary arrays (one value for each window)
!  are UPDATED (input values are incremented):
!
! real    AVCC  (NWT) = sum of correlation coefficients.
! integer NTOTAL(NWT) = sum of total number of traces correlated.
! integer NPASS (NWT) = sum of number of traces that passed (i.e. whose
!                       correlation coefficient was not less than CCMIN).
! real    AVST  (NWT) = sum of the static values (ms) for traces that passed.
! real    STLO  (NWT) = minimum absolute static value (ms) for traces that
!                         passed.
! real    STUP  (NWT) = maximum absolute static value (ms) for traces that
!                         passed.
!
!  The following arrays (one value for each window) are RETURNED:
!
! real    CCWIN (NWT) = correlation coefficient for each window.
! real    STWIN (NWT) = static value (milliseconds) for each window (failed
!                         windows have the average static of windows that
!                         passed).
!
!-------------------------------------------------------------------------------
!                  CALLING SEQUENCE FOR CORDIP_SET_FLAGS
!
!                              i    b    i      i   i     i   i
!      CALL CORDIP_SET_FLAGS (NTR,HDMIX,MID,   NHX,NHY,  NHW,NHB,
!                             NXCDP,NYCDP,NXGAP,NYGAP,NXICB,NYICB)
!                               i     i     i     i     i     i
!
!  This subroutine sets the flags in header words HD(NHW) and HD(NHB).
!  This subroutine should be called prior to calling CORDIP.
!
! integer NTR     = number of input traces to use for building base trace.
! double  HDMIX(NWIH,NTR) = headers of input traces (first dim may exceed NWIH).
! integer NHX     = header# (>0) in HDMIX containing X coordinate.
! integer NHY     = header# (>0) in HDMIX containing Y coordinate.
! integer NHW     = header# (>0) in HDMIX containing a flag given to the trace.
! integer NHB     = header# (>0) in HDMIX containing a flag given to the trace.
! integer MID         = trace and header index (1 <= MID <= N) of center trace.
! integer NXCDP,NYCDP = maximum allowed (X,Y) distance from center trace.
! integer NXGAP,NYGAP = (X,Y) gap size centered on center trace.
! integer NXICB,NYICB = (X,Y) initial base size centered on center trace.
!
! NWIH (number of words in header) is not explicitly needed by this subroutine.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!  1. The output base trace is located at the position given by the
!     (X,Y) coords preset by the user in HDBASE(NHX) and HDBASE(NHY).
!
!  2. This primitive is used by processes IMS and RTC.
!
!  3. Flag HDMIX(NHW) should be set as follows (can be done by
!     CORDIP_SET_FLAGS):
!       NEGATIVE for traces that are not to be used at all (such as
!          those too far from the bin center).  This routine resets
!          this flag to -1 for dead traces that do not already have
!          a negative flag.
!       ZERO for traces that fall within a central gap (not to be used
!          in the final base).
!       POSITIVE otherwise.
!
!  4. Flag HDMIX(NHB) should be set as follows (can be done by
!     CORDIP_SET_FLAGS):
!       POSITIVE for traces to be used in forming an initial brute
!          stack base to which the other traces are correlated.
!       ZERO or NEGATIVE otherwise.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!008. 2006-06-20  B. Menger   Removed Unused Variables.
!  7. 2000-10-20  Stoeckley  Add missing required documentation section.
!  6. 2000-09-27  Stoeckley  Improve consistency of variable names; change
!                             array argument declarations to (:) notation;
!                             change some formal arguments from intent(out)
!                             to intent(inout).
!  5. 1999-11-24  Stoeckley  Converted from old system.
!                             Subroutine CORDIP still has the same name.
!                             Subroutine CORDIPA is now CORDIP_CORR.
!                             Subroutine CORDIPB is now CORDIP_MORE.
!                             Subroutine CORDIPF is now CORDIP_SET_FLAGS.
!  4. 1999-01-11  Goodger    Begin using the fortran90 compiler.
!  3. 1993-03-25  Troutt     Correct mispelled parameters XDIP and YDIP
!                             to DIPX and DIPY to match their names in
!                             the argument list for CORDIP.
!  2. 1993-02-11  Troutt     Increase array sizes to allow for up to 25
!                             correlation windows (was 10).
!  1. 1989-05-01  Stoeckley  Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module cordip_module
      use statcc_module
      use dyncc_module
      use fltr_module
      use mth_module
      implicit none
      public
      private :: cordip_private

      character(len=100),public,save :: cordip_ident = &
       '$Id: cordip.f90,v 1.8 2006/06/20 13:11:50 Menger prod sps $'


      contains


!!-------------------------- cordip -------------------------------------!!
!!-------------------------- cordip -------------------------------------!!
!!-------------------------- cordip -------------------------------------!!


      subroutine cordip (ntr,hdmix,trmix,ndpt,  tstrt,dt,  nhx,nhy,nhw,nhb,  &
                         msctfb,dipx,dipy,twin,nwt,   hdbase,trbase)
      implicit none
      integer         ,intent(in)    :: ntr,ndpt,nwt         ! arguments
      double precision,intent(inout) :: hdmix(:,:)           ! arguments
      real            ,intent(in)    :: trmix(:,:)           ! arguments
      real            ,intent(in)    :: tstrt,dt             ! arguments
      integer         ,intent(in)    :: nhx,nhy,nhw,nhb      ! arguments
      real            ,intent(in)    :: msctfb               ! arguments
      real            ,intent(in)    :: dipx,dipy            ! arguments
      real            ,intent(in)    :: twin(:,:)            ! arguments
      double precision,intent(in)    :: hdbase(:)            ! arguments
      real            ,intent(out)   :: trbase(:)            ! arguments
      real                           :: tro(ndpt)            ! automatic array
      integer                        :: ngood,lgood,i ! local
      real                        :: xx,yy,shiftmax,static   ! local

!----------FLAG THE DEAD TRACES BY SETTING THEIR WEIGHT NEGATIVE.

      ngood = 0
      lgood = 1
      do i = 1, ntr
        if (hdmix(nhw,i) < 0.0) cycle
        if (mth_search_unequal(ndpt,trmix(:,i),1,0.0) == 0) then
          hdmix(nhw,i) = -1.0
          cycle
        endif
        ngood = ngood + 1
        lgood = i
      end do

!----------HANDLE TRIVIAL CASE OF ONLY ONE USEABLE TRACE.

      if (ngood <= 1) then
        trbase(:ndpt) = trmix(:ndpt,lgood)
        return
      endif

!----------GET INITIAL BASE.

      trbase(:ndpt) = 0.
      do i = 1, ntr
        if (hdmix(nhw,i) < 0.0 .or. hdmix(nhb,i) <= 0.0) cycle
        trbase(:ndpt) = trbase(:ndpt) + trmix(:ndpt,i)
      end do

!----------ALIGN NEXT TRACE AND ADD TO BASE.

      do i = 1, ntr
        if (hdmix(nhw,i) <= 0.0 .or. hdmix(nhb,i) > 0.0) cycle
        xx = abs(hdmix(nhx,i)-hdbase(nhx))
        yy = abs(hdmix(nhy,i)-hdbase(nhy))
        shiftmax = xx*dipx + yy*dipy + msctfb
        call cordip_corr (shiftmax,twin,nwt,  trbase,trmix(1:,i),tro,ndpt,  &
                          tstrt,dt,  static,ngood)
        trbase(:ndpt) = trbase(:ndpt) + tro(:ndpt)
      end do

!----------SUBTRACT GAP.

      do i = 1, ntr
        if (hdmix(nhw,i) /= 0.0 .or. hdmix(nhb,i) <= 0.0) cycle
        trbase(:ndpt) = trbase(:ndpt) - trmix(:ndpt,i)
      end do
      return
      end subroutine cordip



!!-------------------------- cordip corr --------------------------------!!
!!-------------------------- cordip corr --------------------------------!!
!!-------------------------- cordip corr --------------------------------!!

!     CORRELATE WINDOWS OF TRACE  TR  WITH WINDOWS OF BASE TRACE.

!     SHIFTMAX = MAXIMUM ALLOWED SHIFT IN MILLISECONDS.
!     TWIN(2,NWT) = TOP AND BOTTOM OF CORRELATION WINDOWS IN SECONDS.
!     NWT = NUMBER OF WINDOWS (ONE OR MORE WITHOUT LIMIT).
!     TRBASE(NDPT) = BASE TRACE TO CORRELATE WITH.
!     TRI   (NDPT) = TRACE TO CORRELATE WITH BASE TRACE.
!     TRO   (NDPT) = DYNAMIC ADJUSTMENT OF TRACE TRI TO MATCH BASE (RETURNED).
!     STATIC = AVERAGE STATIC IN SAMPLE POINTS (RETURNED).
!     NGOOD = NUMBER OF GOOD CORRELATIONS (RETURNED).


      subroutine cordip_corr (shiftmax,twin,nwt,   trbase,tri,tro,ndpt,  &
                              tstrt,dt,   static,ngood)
      implicit none
      real            ,intent(in)    :: shiftmax        ! arguments
      real            ,intent(in)    :: twin(:,:)       ! arguments
      integer         ,intent(in)    :: nwt,ndpt        ! arguments
      real            ,intent(in)    :: trbase(:)       ! arguments
      real            ,intent(in)    :: tri   (:)       ! arguments
      real            ,intent(out)   :: tro   (:)       ! arguments
      real            ,intent(in)    :: tstrt,dt        ! arguments
      real            ,intent(out)   :: static          ! arguments
      integer         ,intent(out)   :: ngood           ! arguments
      real    :: avcc(nwt),avst(nwt),stlo(nwt)          ! local (not used)
      integer :: ntotal(nwt),npass(nwt)                 ! local (not used)
      real    :: stup(nwt),ccwin(nwt),stwin(nwt)        ! local (not used)
      real    :: ccmin,ccbest                           ! local (not used)
      logical :: update                                 ! local

      update = .false.
      call cordip_private                                                &
               (update,  ccmin,shiftmax,twin,nwt,  trbase,tri,tro,ndpt,  &
                tstrt,dt,static,ccbest,ngood,                            &
                avcc,ntotal,npass,avst,stlo,stup,ccwin,stwin)
      return
      end subroutine cordip_corr


!!-------------------------- cordip more --------------------------------!!
!!-------------------------- cordip more --------------------------------!!
!!-------------------------- cordip more --------------------------------!!

!     SHIFTMAX = MAXIMUM ALLOWED SHIFT IN MILLISECONDS.
!     TWIN(2,NWT) = TOP AND BOTTOM OF CORRELATION WINDOWS IN SECONDS.
!     NWT = NUMBER OF WINDOWS (ONE OR MORE WITHOUT LIMIT).
!     TRBASE(NDPT) = BASE TRACE TO CORRELATE WITH.
!     TRI   (NDPT) = TRACE TO CORRELATE WITH BASE TRACE.
!     TRO   (NDPT) = DYNAMIC ADJUSTMENT OF TRACE  TR  TO MATCH BASE (RETURNED).
!     STATIC = AVERAGE STATIC IN SAMPLE POINTS (RETURNED).
!     NGOOD = NUMBER OF GOOD CORRELATIONS (RETURNED).

!     CCMIN = MINIMUM ALLOWED CORRELATION COEFFICIENT TO GET A STATIC.
!     CCBEST = BEST CORRELATION COEFFICIENT OF ALL WINDOWS (RETURNED).
!     AVCC,NTOTAL,NPASS,AVST,STLO,STUP = STATISTICAL SUMMARY ARRAYS (UPDATED).
!     CCWIN = CORRELATION COEFFICIENT FOR EACH WINDOW (RETURNED).
!     STWIN = STATIC IN MILLISECONDS FOR EACH WINDOW (RETURNED).


      subroutine cordip_more                                         &
                     (ccmin,shiftmax,twin,nwt,  trbase,tri,tro,ndpt, &
                      tstrt,dt,   static,ccbest,ngood,               &
                      avcc,ntotal,npass,avst,stlo,stup,ccwin,stwin)
      implicit none
      real            ,intent(in)    :: ccmin,shiftmax         ! arguments
      real            ,intent(in)    :: twin(:,:)              ! arguments
      integer         ,intent(in)    :: nwt,ndpt               ! arguments
      real            ,intent(in)    :: trbase(:)              ! arguments
      real            ,intent(in)    :: tri   (:)              ! arguments
      real            ,intent(out)   :: tro   (:)              ! arguments
      real            ,intent(in)    :: tstrt,dt               ! arguments
      real            ,intent(out)   :: static,ccbest          ! arguments
      integer         ,intent(out)   :: ngood                  ! arguments
      real   ,intent(inout) :: avcc(:),avst(:),stlo(:),stup(:) ! arguments
      integer,intent(inout) :: ntotal(:),npass(:)              ! arguments
      real   ,intent(out)   :: ccwin(:),stwin(:)               ! arguments
      logical               :: update                          ! local

      ccbest       = 0.0
      ccwin(1:nwt) = 0.0
      stwin(1:nwt) = 0.0
      update       = .true.
      call cordip_private                                               &
               (update,  ccmin,shiftmax,twin,nwt,  trbase,tri,tro,ndpt, &
                tstrt,dt,static,ccbest,ngood,                           &
                avcc,ntotal,npass,avst,stlo,stup,ccwin,stwin)
      return
      end subroutine cordip_more


!!---------------------- cordip private ---------------------------------!!
!!---------------------- cordip private ---------------------------------!!
!!---------------------- cordip private ---------------------------------!!


      subroutine cordip_private                                            &
                  (update,  ccmin,shiftmax,twin,nwt,  trbase,tri,tro,ndpt, &
                   tstrt,dt,static,ccbest,ngood,                           &
                   avcc,ntotal,npass,avst,stlo,stup,ccwin,stwin)
      implicit none
      logical,intent(in)    :: update                            ! arguments
      real   ,intent(in)    :: ccmin,shiftmax                    ! arguments
      real   ,intent(in)    :: twin(:,:)                         ! arguments
      integer,intent(in)    :: nwt,ndpt                          ! arguments
      real   ,intent(in)    :: trbase(:)                         ! arguments
      real   ,intent(in)    :: tri   (:)                         ! arguments
      real   ,intent(out)   :: tro   (:)                         ! arguments
      real   ,intent(in)    :: tstrt,dt                          ! arguments
      real   ,intent(out)   :: static                            ! arguments
      real   ,intent(inout) :: ccbest                            ! arguments
      integer,intent(out)   :: ngood                             ! arguments
      real   ,intent(inout) :: avcc(:),avst(:),stlo(:),stup(:)   ! arguments
      integer,intent(inout) :: ntotal(:),npass(:)                ! arguments
      real   ,intent(out)   :: ccwin(:),stwin(:)                 ! arguments
      real                  :: ttta(nwt+2),tttb(nwt+2)           ! local
      real                  :: dtms,good,sum,sumbase             ! local
      integer               :: ishft,ncorr,k,i,itwin,ibwin,nwin  ! local
      integer               :: itwin2,ibwin2,nwin2,j             ! local
      real                  :: peak,a,b,xmin,statms,cc,bmute     ! local

!----------GET STARTED.

      dtms  = 1000.0*dt
      good  = 0.0
      ngood = 0
      ishft = abs(nint(shiftmax/dtms))
      if (ishft.eq.0) go to 130
      ncorr = 2*ishft+1

!----------START LOOP OVER WINDOWS.

      do k = 1,nwt
           tttb(k+1) = 99999.

!----------GET WINDOW INDICES.

           itwin     = max(  1+nint(  (twin(1,k)-tstrt)/dt  )  ,     1+ishft  )
           ibwin     = min(  1+nint(  (twin(2,k)-tstrt)/dt  )  ,  ndpt-ishft  )
           ttta(k+1) = 0.5*(itwin+ibwin)
           nwin = ibwin - itwin + 1
           if (nwin <= 1) cycle     
           itwin2 = itwin - ishft
           ibwin2 = ibwin + ishft
           nwin2  = ibwin2 - itwin2 + 1

!----------TEST FOR LIVE WINDOW.

           if (update) then
                sum = 0.0
                do i = itwin2,ibwin2
                     sum = sum + tri(i)**2
                end do
                if(sum.eq.0.0) cycle     
                sum = sqrt(sum/nwin2)

!----------FIND RMS AMPLITUDE OF BASE TRACE FOR USE IN CORRELATION COEF.

                sumbase = 0.0
                do i = itwin,ibwin
                     sumbase = sumbase + trbase(i)**2
                end do
                if (sumbase == 0.0) cycle     
                sumbase= sqrt(sumbase/nwin)
           end if

!----------CORRELATE TRACE WITH BASE AND GET LARGEST SAMPLE VALUE.

           call fltr_filterg (trbase(itwin:),nwin,tri(itwin2:),nwin2,tro)
           j= mth_ismax(ncorr,tro,1)

!----------FIND THE STATIC AND PEAK VALUE BY PARABOLIC INTERPOLATION.

           static = 0.0
           peak   = tro(j)
           if (peak > 0.0) then
                static = ishft + 1 - j
                if (j > 1 .and. j < ncorr) then
                     a = 2.0*peak - tro(j+1) - tro(j-1)
                     if (a > 0.0) then
                          b      = tro(j-1)-tro(j+1)
                          xmin   = -0.5*b/a
                          static = static - xmin
                          peak   = peak - 0.5*(xmin*(a*xmin+b))
                    end if
                end if
           end if

!----------COMPUTE CORRELATION COEFFICIENT.

           if (update) then
                statms = static*dtms
                cc     = peak/(nwin*sumbase*sum)
                ccbest = max(cc,ccbest)

!----------UPDATE STATISTICAL INFORMATION.

                avcc  (k) = avcc(k) + cc
                ntotal(k) = ntotal(k) + 1
                ccwin (k) = cc
                stwin (k) = statms

!----------TEST FOR SUFFICIENTLY LARGE CORRELATION COEFFICIENT.

                if(cc <= ccmin) cycle     

!----------SAVE GOOD STATIC FOR THIS WINDOW.

                npass(k) = npass(k) + 1
                avst (k) = avst(k) + abs(statms)
                stlo (k) = min(stlo(k),statms)
                stup (k) = max(stup(k),statms)
           end if

!----------FINISH UP FOR THIS WINDOW.

           good      = good + static
           ngood     = ngood + 1
           tttb(k+1) = static
      end do

!----------DO NOT SHIFT TRACE IF ALL CORRELATIONS FAIL.

130   if (ngood == 0) then
           static = 0.0
           tro(1:ndpt) = tri(1:ndpt)

!----------DO A STATIC SHIFT IF ONLY ONE CORRELATION IS GOOD.

      else if (ngood == 1) then
           static = good
           call statcc (static,ndpt,tri,tro)

!----------DYNAMICALLY SHIFT THE TRACE.

      else
           static = good/ngood
           do k = 1,nwt
                if (tttb(k+1) == 99999.0) tttb(k+1) = static
           end do
           tttb(1)     = tttb(2)
           tttb(nwt+2) = tttb(nwt+1)
           ttta(1)     = 1.0
           ttta(nwt+2) = ndpt
           do k = 1,nwt+2
                tttb(k) = ttta(k) + tttb(k)
           end do
           call dyncc (2.0,nwt+2,ttta,tttb,ndpt,tri,tro,bmute)
      end if
      return
      end subroutine cordip_private


!!-------------------------- cordip set flags ------------------------------!!
!!-------------------------- cordip set flags ------------------------------!!
!!-------------------------- cordip set flags ------------------------------!!

!     USES COORDINATES HDMIX(NHX,I) AND HDMIX(NHY,I).
!     SETS FLAGS HD(NHW,I) AND HD(NHB,I).


      subroutine cordip_set_flags (ntr,hdmix,mid,   nhx,nhy,  nhw,nhb,    &
                                   nxcdp,nycdp,nxgap,nygap,nxicb,nyicb)
      implicit none
      double precision,intent(inout) :: hdmix(:,:)               ! arguments
      integer         ,intent(in)    :: ntr,mid,nhx,nhy,nhw,nhb  ! arguments
      integer         ,intent(in)    :: nxcdp,nycdp              ! arguments
      integer         ,intent(in)    :: nxgap,nygap              ! arguments
      integer         ,intent(in)    :: nxicb,nyicb              ! arguments
      integer                        :: nxgap2,nygap2,i,ix,iy    ! local

!----------CORRECT FOR ONE GAP ZERO AND THE OTHER ONE NONZERO.

      nxgap2 = nxgap
      nygap2 = nygap
      if (nxgap==0 .and. nygap>0) nxgap2 = 1
      if (nygap==0 .and. nxgap>0) nygap2 = 1

!----------GO THRU LOOP OF ALL TRACES.

      do i = 1, ntr
        hdmix(nhw,i) = 1.0
        hdmix(nhb,i) = 1.0
        ix = abs(nint(hdmix(nhx,i)) - nint(hdmix(nhx,mid)))
        iy = abs(nint(hdmix(nhy,i)) - nint(hdmix(nhy,mid)))
        if (ix > nxcdp .or. iy > nycdp)            hdmix(nhw,i) = -1.0
        if (ix < 0.5*nxgap2 .and. iy < 0.5*nygap2) hdmix(nhw,i) = 0.0
        if (ix <= 0.5*nxicb .and. iy <= 0.5*nyicb) cycle
        hdmix(nhb,i) = 0.0
      end do
      return
      end subroutine cordip_set_flags


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cordip_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

