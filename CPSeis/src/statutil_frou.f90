!<CPS_v1 type="AUXILIARY_FILE"/>
!!--------------------------- statutil_frou.f90 -----------------------------!!
!!--------------------------- statutil_frou.f90 -----------------------------!!
!!--------------------------- statutil_frou.f90 -----------------------------!!

   ! other files are:  statutil_wrapper.cc  statutil_wrapper.hh  statutil.f90


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

 
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2002-04-10  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


 
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module statutil_frou_module
      use statutil_module
      use convert_module
      use named_constants_module
      implicit none

      character(len=100),public,save :: STATUTIL_FROU_IDENT = &
'$Id: statutil_frou.f90,v 1.1 2002/04/10 17:10:22 Stoeckley prod sps $'

      contains


!!--------------------- convert endflag to endopt ------------------------!!
!!--------------------- convert endflag to endopt ------------------------!!
!!--------------------- convert endflag to endopt ------------------------!!


      subroutine statutil_endflag_convert (endflag, endopt)
      implicit none
      integer         ,intent(in)  :: endflag                ! argument
      character(len=*),intent(out) :: endopt                 ! argument
      integer         ,parameter   :: lunprint = 6           ! local

      select case (endflag)
           case (1)     ; endopt = 'NN'      ! must match ENDFLAG_N
           case (2)     ; endopt = 'TT'      ! must match ENDFLAG_T
           case (3)     ; endopt = 'EE'      ! must match ENDFLAG_E
           case (4)     ; endopt = 'SS'      ! must match ENDFLAG_S
           case default
                  write(lunprint,*) 'illegal value of endflag = ',endflag
                  write(lunprint,*) 'detected in statutil_frou'
                  write(lunprint,*) 'this is an error in the calling program'
                  stop
      end select
      return
      end subroutine statutil_endflag_convert


!!-------------------------- end of module --------------------------------!!
!!-------------------------- end of module --------------------------------!!
!!-------------------------- end of module --------------------------------!!


      end module statutil_frou_module


!!--------------------------- get1 get2 get3 ------------------------------!!
!!--------------------------- get1 get2 get3 ------------------------------!!
!!--------------------------- get1 get2 get3 ------------------------------!!


      real function statutil_frou_get1 &
               (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      use statutil_frou_module
      implicit none
      integer         ,intent(in) :: nhx,nhx2,nx,nhy,nhy2,ny     ! arguments
      real            ,intent(in) :: x1,xinc, y1,yinc            ! arguments
      double precision,intent(in) :: hd(*)                       ! arguments
      real            ,intent(in) :: statics(nx,ny)              ! arguments

      statutil_frou_get1 = statutil_get1 &
               (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      return
      end function statutil_frou_get1



      real function statutil_frou_get2 &
               (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      use statutil_frou_module
      implicit none
      integer         ,intent(in) :: nhx,nhx2,nx,nhy,nhy2,ny     ! arguments
      real            ,intent(in) :: x1,xinc, y1,yinc            ! arguments
      double precision,intent(in) :: hd(*)                       ! arguments
      real            ,intent(in) :: statics(nx,ny)              ! arguments

      statutil_frou_get2 = statutil_get2 &
               (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      return
      end function statutil_frou_get2



      real function statutil_frou_get3 &
               (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      use statutil_frou_module
      implicit none
      integer         ,intent(in) :: nhx,nhx2,nx,nhy,nhy2,ny     ! arguments
      real            ,intent(in) :: x1,xinc, y1,yinc            ! arguments
      double precision,intent(in) :: hd(*)                       ! arguments
      real            ,intent(in) :: statics(nx,ny)              ! arguments

      statutil_frou_get3 = statutil_get3 &
               (hd, statics, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      return
      end function statutil_frou_get3


!!-------------------------------- grade ----------------------------------!!
!!-------------------------------- grade ----------------------------------!!
!!-------------------------------- grade ----------------------------------!!


      subroutine statutil_frou_grade (nx, ny, statics, &
                                      ixmin, ixmax, iymin, iymax)
      use statutil_frou_module
      implicit none
      integer,         intent(in)    :: nx,ny                    ! arguments
      real   ,         intent(inout) :: statics(nx,ny)           ! arguments
      integer,         intent(in)    :: ixmin,ixmax,iymin,iymax  ! arguments

      call statutil_grade (nx, ny, statics, ixmin+1,ixmax+1,iymin+1,iymax+1)
      return
      end subroutine statutil_frou_grade


!!-------------------------------- integrate ------------------------------!!
!!-------------------------------- integrate ------------------------------!!
!!-------------------------------- integrate ------------------------------!!


      subroutine statutil_frou_integrate (nx, ny, statics, preserve)
      use statutil_frou_module
      implicit none
      integer,         intent(in)    :: nx,ny                    ! arguments
      real   ,         intent(inout) :: statics(nx,ny)           ! arguments
      integer,         intent(in)    :: preserve                 ! arguments
      logical                        :: preserve2                ! local

      call convert_ii2ll      (preserve, preserve2)
      call statutil_integrate (nx, ny, statics, preserve2)
      return
      end subroutine statutil_frou_integrate


!!-------------------------------- rep nilx ------------------------------!!
!!-------------------------------- rep nilx ------------------------------!!
!!-------------------------------- rep nilx ------------------------------!!


      subroutine statutil_frou_rep_nilx (nx, ny, statics)
      use statutil_frou_module
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call statutil_rep_nilx (nx, ny, statics)
      return
      end subroutine statutil_frou_rep_nilx


!!-------------------------------- rep nily ------------------------------!!
!!-------------------------------- rep nily ------------------------------!!
!!-------------------------------- rep nily ------------------------------!!


      subroutine statutil_frou_rep_nily (nx, ny, statics)
      use statutil_frou_module
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call statutil_rep_nily (nx, ny, statics)
      return
      end subroutine statutil_frou_rep_nily


!!-------------------------------- rep nilx only -------------------------!!
!!-------------------------------- rep nilx only -------------------------!!
!!-------------------------------- rep nilx only -------------------------!!


      subroutine statutil_frou_rep_nilx_only (nx, ny, statics)
      use statutil_frou_module
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call statutil_rep_nilx_only (nx, ny, statics)
      return
      end subroutine statutil_frou_rep_nilx_only


!!-------------------------------- rep nily only -------------------------!!
!!-------------------------------- rep nily only -------------------------!!
!!-------------------------------- rep nily only -------------------------!!


      subroutine statutil_frou_rep_nily_only (nx, ny, statics)
      use statutil_frou_module
      implicit none
      integer, intent(in)     :: nx,ny                 ! arguments
      real,    intent(inout)  :: statics(nx,ny)        ! arguments

      call statutil_rep_nily_only (nx, ny, statics)
      return
      end subroutine statutil_frou_rep_nily_only


!!--------------------------- rep nearby nils ----------------------------!!
!!--------------------------- rep nearby nils ----------------------------!!
!!--------------------------- rep nearby nils ----------------------------!!


      subroutine statutil_frou_rep_nearby_nils (nx, ny, statics, &
                                                ixdist, iydist, require)
      use statutil_frou_module
      implicit none
      integer, intent(in)     :: nx,ny                       ! arguments
      real,    intent(inout)  :: statics(nx,ny)              ! arguments
      integer, intent(in)     :: ixdist,iydist,require       ! arguments
      logical                 :: require2                    ! arguments

      call convert_ii2ll            (require, require2)
      call statutil_rep_nearby_nils (nx, ny, statics, ixdist, iydist, require2)
      return
      end subroutine statutil_frou_rep_nearby_nils


!!-------------------------------- runav ----------------------------------!!
!!-------------------------------- runav ----------------------------------!!
!!-------------------------------- runav ----------------------------------!!


      subroutine statutil_frou_runav &
                    (nx,ny,statics,nxsmooth,nysmooth,endflag,trim,preserve,wild)
      use statutil_frou_module
      implicit none
      integer         ,         intent(in)    :: nx,ny             ! arguments
      real            ,         intent(inout) :: statics(nx,ny)    ! arguments
      integer         ,         intent(in)    :: nxsmooth,nysmooth ! arguments
      integer         ,         intent(in)    :: endflag           ! arguments
      real            ,         intent(in)    :: trim              ! arguments
      integer         ,         intent(in)    :: preserve,wild     ! arguments
      character(len=8)                        :: endopt            ! local
      logical                                 :: preserve2,wild2   ! local

      call statutil_endflag_convert (endflag , endopt)
      call convert_ii2ll            (preserve, preserve2)
      call convert_ii2ll            (wild    , wild2)
      call statutil_runav &
                (nx,ny,statics,nxsmooth,nysmooth,endopt,trim,preserve2,wild2)
      return
      end subroutine statutil_frou_runav


!!-------------------------------- smooth ----------------------------------!!
!!-------------------------------- smooth ----------------------------------!!
!!-------------------------------- smooth ----------------------------------!!


      subroutine statutil_frou_smooth &
                    (nx,ny,statics,nxsmooth,nysmooth,endflag,trim,preserve,wild)
      use statutil_frou_module
      implicit none
      integer         ,         intent(in)    :: nx,ny             ! arguments
      real            ,         intent(inout) :: statics(nx,ny)    ! arguments
      integer         ,         intent(in)    :: nxsmooth,nysmooth ! arguments
      integer         ,         intent(in)    :: endflag           ! arguments
      real            ,         intent(in)    :: trim              ! arguments
      integer         ,         intent(in)    :: preserve,wild     ! arguments
      character(len=8)                        :: endopt            ! local
      logical                                 :: preserve2,wild2   ! local

      call statutil_endflag_convert (endflag , endopt)
      call convert_ii2ll            (preserve, preserve2)
      call convert_ii2ll            (wild    , wild2)
      call statutil_smooth &
                (nx,ny,statics,nxsmooth,nysmooth,endopt,trim,preserve2,wild2)
      return
      end subroutine statutil_frou_smooth


!!-------------------------------- reverse ----------------------------------!!
!!-------------------------------- reverse ----------------------------------!!
!!-------------------------------- reverse ----------------------------------!!


      subroutine statutil_frou_reverse (x1,y1,xinc,yinc,nx,ny,statics)
      use statutil_frou_module
      implicit none
      real            ,         intent(inout) :: x1,y1,xinc,yinc   ! arguments
      integer         ,         intent(in)    :: nx,ny             ! arguments
      real            ,         intent(inout) :: statics(nx,ny)    ! arguments

      call statutil_reverse (x1,y1,xinc,yinc,nx,ny,statics)
      return
      end subroutine statutil_frou_reverse


!!------------------------- bld1 bld2 bld3 ---------------------------------!!
!!------------------------- bld1 bld2 bld3 ---------------------------------!!
!!------------------------- bld1 bld2 bld3 ---------------------------------!!


      subroutine statutil_frou_bld1 (nx, ny, statics, kounts)
      use statutil_frou_module
      implicit none
      integer, intent(in)    :: nx,ny            ! arguments
      real,    intent(inout) :: statics(nx,ny)   ! arguments
      integer, intent(inout) :: kounts (nx,ny)   ! arguments

      call statutil_bld1 (nx, ny, statics, kounts)
      return
      end subroutine statutil_frou_bld1



      subroutine statutil_frou_bld3 (nx, ny, statics, kounts)
      use statutil_frou_module
      implicit none
      integer, intent(in)    :: nx,ny          ! arguments
      real,    intent(inout) :: statics(nx,ny) ! arguments
      integer, intent(in)    :: kounts (nx,ny) ! arguments

      call statutil_bld3 (nx, ny, statics, kounts)
      return
      end subroutine statutil_frou_bld3



      subroutine statutil_frou_bld2 &
   (hd, statval, statics, kounts, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      use statutil_frou_module
      implicit none
      integer,         intent(in)    :: nhx,nhx2,nx, nhy,nhy2,ny   ! arguments
      real,            intent(in)    :: x1,xinc, y1,yinc, statval  ! arguments
      double precision,intent(in)    :: hd(*)                      ! arguments
      real,            intent(inout) :: statics(nx,ny)             ! arguments
      integer,         intent(inout) :: kounts (nx,ny)             ! arguments

      call statutil_bld2 &
   (hd, statval, statics, kounts, nhx,nhy, nhx2,nhy2, x1,y1, xinc,yinc, nx,ny)
      return
      end subroutine statutil_frou_bld2


!!------------------------------ taper -----------------------------------!!
!!------------------------------ taper -----------------------------------!!
!!------------------------------ taper -----------------------------------!!


      subroutine statutil_frou_taper (taper, tapr, ncorr)
      use statutil_frou_module
      implicit none
      integer,         intent(in)    :: ncorr                ! arguments
      real,            intent(in)    :: taper                ! arguments
      real,            intent(inout) :: tapr(ncorr)          ! arguments

      call statutil_taper (taper, tapr, ncorr)
      return
      end subroutine statutil_frou_taper


!!------------------------------------ corr -------------------------------!!
!!------------------------------------ corr -------------------------------!!
!!------------------------------------ corr -------------------------------!!


      subroutine statutil_frou_corr (trace,rtrace,nwin,corr,ncorr,denom)
      use statutil_frou_module
      implicit none
      integer,          intent(in)  :: nwin,ncorr               ! arguments
      real   ,          intent(in)  :: trace(nwin),rtrace(nwin) ! arguments
      real   ,          intent(out) :: corr(ncorr)              ! arguments
      real   ,          intent(out) :: denom                    ! arguments

      call statutil_corr (trace,rtrace,nwin,corr,ncorr,denom)
      return
      end subroutine statutil_frou_corr


!!---------------------------------- corr enhanced -----------------------!!
!!---------------------------------- corr enhanced -----------------------!!
!!---------------------------------- corr enhanced -----------------------!!


      subroutine statutil_frou_corr_enhanced &
           (trace,rtrace,nwin,corr,ncorr,normalize,subtract,tapr,ccoef,denom)
      use statutil_frou_module
      implicit none
      integer,          intent(in) :: nwin,ncorr               ! arguments
      real   ,          intent(in) :: trace(nwin),rtrace(nwin) ! arguments
      real   ,          intent(out):: corr(ncorr)              ! arguments
      integer,          intent(in) :: normalize,subtract       ! arguments
      real   ,          intent(in) :: tapr(ncorr)              ! arguments
      real   ,          intent(out):: ccoef,denom              ! arguments
      logical                      :: normalize2,subtract2     ! local

      call convert_ii2ll (normalize, normalize2)
      call convert_ii2ll (subtract , subtract2)
      call statutil_corr_enhanced &
           (trace,rtrace,nwin,corr,ncorr,normalize2,subtract2,tapr,ccoef,denom)
      return
      end subroutine statutil_frou_corr_enhanced


!!----------------------------------- pick --------------------------------!!
!!----------------------------------- pick --------------------------------!!
!!----------------------------------- pick --------------------------------!!


      subroutine statutil_frou_pick (corr, ncorr, shft, peak)
      use statutil_frou_module
      implicit none
      integer,intent(in)  :: ncorr                  ! arguments
      real   ,intent(in)  :: corr(ncorr)            ! arguments
      real   ,intent(out) :: shft, peak             ! arguments

      call statutil_pick (corr, ncorr, shft, peak)
      return
      end subroutine statutil_frou_pick


!!-------------------------------- pick enhanced ---------------------------!!
!!-------------------------------- pick enhanced ---------------------------!!
!!-------------------------------- pick enhanced ---------------------------!!


      subroutine statutil_frou_pick_enhanced &
                      (corr, ncorr, npick, ccmin, denom, shft, ccoef)
      use statutil_frou_module
      implicit none
      integer         ,intent(in)  :: ncorr,npick            ! arguments
      real            ,intent(in)  :: corr(ncorr)            ! arguments
      real            ,intent(in)  :: ccmin,denom            ! arguments
      real            ,intent(out) :: shft, ccoef            ! arguments

      call statutil_pick_enhanced &
                      (corr, ncorr, npick, ccmin, denom, shft, ccoef)
      return
      end subroutine statutil_frou_pick_enhanced


!!---------------------------------- 1d integrate --------------------------!!
!!---------------------------------- 1d integrate --------------------------!!
!!---------------------------------- 1d integrate --------------------------!!


      subroutine statutil_frou_1d_integrate (array, n, preserve)
      use statutil_frou_module
      implicit none
      integer         ,intent(in)    :: n                   ! arguments
      real            ,intent(inout) :: array(n)            ! arguments
      integer         ,intent(in)    :: preserve            ! arguments
      logical                        :: preserve2           ! local

      call convert_ii2ll         (preserve, preserve2)
      call statutil_1d_integrate (array, n, preserve2)
      return
      end subroutine statutil_frou_1d_integrate


!!---------------------------------- 1d interpolate ------------------------!!
!!---------------------------------- 1d interpolate ------------------------!!
!!---------------------------------- 1d interpolate ------------------------!!


      subroutine statutil_frou_1d_interpolate (array, n)
      use statutil_frou_module
      implicit none
      integer, intent(in)      :: n                   ! arguments
      real,    intent(inout)   :: array(n)            ! arguments

      call statutil_1d_interpolate (array, n)
      return
      end subroutine statutil_frou_1d_interpolate



!!------------------------------- trimmed mean ---------------------------!!
!!------------------------------- trimmed mean ---------------------------!!
!!------------------------------- trimmed mean ---------------------------!!


      function statutil_frou_trimmed_mean (array,n,trim) result (trimmed_mean)
      use statutil_frou_module
      implicit none
      integer         ,intent(in) :: n                        ! arguments
      real            ,intent(in) :: array(n)                 ! arguments
      real            ,intent(in) :: trim                     ! arguments
      real                        :: trimmed_mean             ! result

      trimmed_mean = statutil_trimmed_mean (array,n,trim)
      return
      end function statutil_frou_trimmed_mean


!!----------------------------- 1d smooth no nils ---------------------------!!
!!----------------------------- 1d smooth no nils ---------------------------!!
!!----------------------------- 1d smooth no nils ---------------------------!!


      subroutine statutil_frou_1d_smooth_no_nils (array,n,nrun,endflag,trim)
      use statutil_frou_module
      implicit none
      integer                  ,intent(in)    :: n,nrun           ! arguments
      real                     ,intent(inout) :: array(n)         ! arguments
      integer                  ,intent(in)    :: endflag          ! arguments
      real                     ,intent(in)    :: trim             ! arguments
      character(len=8)                        :: endopt           ! local

      call statutil_endflag_convert   (endflag, endopt)
      call statutil_1d_smooth_no_nils (array,n,nrun,endopt,trim)
      return
      end subroutine statutil_frou_1d_smooth_no_nils


!!-------------------------------- 1d runav no nils -------------------------!!
!!-------------------------------- 1d runav no nils -------------------------!!
!!-------------------------------- 1d runav no nils -------------------------!!


      subroutine statutil_frou_1d_runav_no_nils (array,n,nrun,endflag,trim)
      use statutil_frou_module
      implicit none
      integer                  ,intent(in)    :: n,nrun           ! arguments
      real                     ,intent(inout) :: array(n)         ! arguments
      integer                  ,intent(in)    :: endflag          ! arguments
      real                     ,intent(in)    :: trim             ! arguments
      character(len=8)                        :: endopt           ! local

      call statutil_endflag_convert  (endflag, endopt)
      call statutil_1d_runav_no_nils (array,n,nrun,endopt,trim)
      return
      end subroutine statutil_frou_1d_runav_no_nils


!!--------------------------------- 1d smooth ------------------------------!!
!!--------------------------------- 1d smooth ------------------------------!!
!!--------------------------------- 1d smooth ------------------------------!!


      subroutine statutil_frou_1d_smooth &
                                (array,n,nrun,endflag,trim,preserve,wild)
      use statutil_frou_module
      implicit none
      integer                  ,intent(in)    :: n,nrun          ! arguments
      real                     ,intent(inout) :: array(n)        ! arguments
      integer                  ,intent(in)    :: endflag         ! arguments
      real                     ,intent(in)    :: trim            ! arguments
      integer                  ,intent(in)    :: preserve,wild   ! arguments
      character(len=8)                        :: endopt          ! local
      logical                                 :: preserve2,wild2 ! local

      call statutil_endflag_convert (endflag , endopt)
      call convert_ii2ll            (preserve, preserve2)
      call convert_ii2ll            (wild    , wild2)
      call statutil_1d_smooth       (array,n,nrun,endopt,trim,preserve2,wild2)
      return
      end subroutine statutil_frou_1d_smooth


!!--------------------------------- 1d runav -------------------------------!!
!!--------------------------------- 1d runav -------------------------------!!
!!--------------------------------- 1d runav -------------------------------!!


      subroutine statutil_frou_1d_runav &
                                (array,n,nrun,endflag,trim,preserve,wild)
      use statutil_frou_module
      implicit none
      integer                  ,intent(in)    :: n,nrun          ! arguments
      real                     ,intent(inout) :: array(n)        ! arguments
      integer                  ,intent(in)    :: endflag         ! arguments
      real                     ,intent(in)    :: trim            ! arguments
      integer                  ,intent(in)    :: preserve,wild   ! arguments
      character(len=8)                        :: endopt          ! local
      logical                                 :: preserve2,wild2 ! local

      call statutil_endflag_convert (endflag , endopt)
      call convert_ii2ll            (preserve, preserve2)
      call convert_ii2ll            (wild    , wild2)
      call statutil_1d_runav        (array,n,nrun,endopt,trim,preserve2,wild2)
      return
      end subroutine statutil_frou_1d_runav


!!---------------------------------- scan statics -------------------------!!
!!---------------------------------- scan statics -------------------------!!
!!---------------------------------- scan statics -------------------------!!


      subroutine statutil_frou_scan_statics (n,array,statmin,statmax,numnils)
      use statutil_frou_module
      implicit none
      integer                  ,intent(in)  :: n                ! arguments
      real                     ,intent(in)  :: array(n)         ! arguments
      real                     ,intent(out) :: statmin,statmax  ! arguments
      integer                  ,intent(out) :: numnils          ! arguments

      call statutil_scan_statics (n,array,statmin,statmax,numnils)
      return
      end subroutine statutil_frou_scan_statics


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
