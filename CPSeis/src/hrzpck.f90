!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- hrzpck.f90 -------------------------------!!
!!----------------------------- hrzpck.f90 -------------------------------!!
!!----------------------------- hrzpck.f90 -------------------------------!!


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
! Name       : SMCook
! Category   : miscellaneous
! Written    : 2003-08-06   by: SMCook
! Revised    : 2003-08-13   by: SMCook
! Maturity   : production
! Purpose    : Basic horizon routines.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! HRZPCK contains basic routines one might use when autopicking, such as
! seeking a lav-based threshold, snapping to a peak value using a spline, etc.
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
!                                     i    i     i       i        o
!   call hrzpck_hot_proximal_sample (tr, itop, ibot, threshold, index)
!
!     Given input trace and sample range, finds the hottest sample (negative
!      if threshold is negative, positive if threshold is positive).
!
!   call hzpck_proximal_spline (  i    i     i      i 
!                                tr, tstrt, dt, seed_time,
!                                    i       i     i      o    o    o
!                                alignment, nup, ndown, time, amp, ier)
!
!                                   i    i   i     i      o
!   call hrzpck_threshold_sample (ndpt, hd, tr, factor, index)
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
!  2. 2003-11-19  SMCook     Fixed logic error in hrzpck_hot_proximal_sample
!                             causing systematic degraded result in hrzstk.
!                            Added code to handle case where window is dead.
!  1. 2003-08-06  SMCook     Initial version.
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
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module hrzpck_module
      use named_constants_module
      use spline_module
      implicit none


      public

      character(len=100),public,save :: HRZPCK_IDENT = &
'$Id: hrzpck.f90,v 1.2 2003/11/19 13:01:09 SMCook prod sps $'

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer, parameter :: HRZPCK_ALIGN_PEAKS    = 0
      integer, parameter :: HRZPCK_ALIGN_TROUGHS  = 1
      integer, parameter :: HRZPCK_USE_SEED_AS_IS = 2


      contains


!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!


!-----------------------------------------------------------------------------
! Hot proximal sample routine.
!-----------------------------------------------------------------------------
      subroutine hrzpck_hot_proximal_sample ( &
        tr, itop, ibot, threshold, index)
      real             ,intent(in)  :: tr(:)                  ! arguments
      integer          ,intent(in)  :: itop, ibot             !
      real             ,intent(in)  :: threshold              !
      integer          ,intent(out) :: index                  !

      integer      :: i                                       ! local
      real         :: amp, amp_previous                       !
      real         :: hot_val                                 !
      logical      :: case_peak                               !


!---- this index value will be returned unless reset in loops below
      index = -1

!---- define a logical variable to improve readability of code
      if     (threshold < 0) then
        case_peak = .false.
      else if(threshold > 0) then
        case_peak = .true.
      else
        return
      end if

!---- determine local lav's
      if(case_peak) then
        hot_val = -9.e37
        do i=itop,ibot
          if(tr(i) > hot_val) hot_val = tr(i)
        end do
      else
        hot_val =  9.e37
        do i=itop,ibot
          if(tr(i) < hot_val) hot_val = tr(i)
        end do
      end if

      hot_val = hot_val * abs(threshold)

      if(hot_val==0.0) return
 
!---- find first sample at or above hot_val
      amp = 0.0
      do i=itop,ibot

        if(case_peak) then
          if(tr(i) >= hot_val) then
            amp = tr(i)
            index = i
            exit
          end if
        else
          if(tr(i) <= hot_val) then
            amp = tr(i)
            index = i
            exit
          end if
        end if

      end do

      if(amp==0.0) return

!---- find hottest proximal (deeper) sample
      if(index > 0) then
        amp_previous = amp

        do i=index+1,ibot
          amp = tr(i)

          if(case_peak) then
            if(amp > amp_previous) then
              index = i
            else
              exit
            end if
          else
            if(amp < amp_previous) then
              index = i
            else
              exit
            end if
          end if

          amp_previous = amp
        end do

      end if

      end subroutine hrzpck_hot_proximal_sample


!-----------------------------------------------------------------------------
! Proximal peak routine.
!-----------------------------------------------------------------------------
      subroutine hrzpck_proximal_spline (                         &
                             tr, tstrt, dt, seed_time,            &
                             alignment, nup, ndown, time, amp, ier)
      real             ,intent(in)  :: tr(:)                  ! arguments
      real             ,intent(in)  :: tstrt, dt              !
      real             ,intent(in)  :: seed_time              !
      integer          ,intent(in)  :: alignment              !
      integer          ,intent(in)  :: nup, ndown             !
      real             ,intent(out) :: time(*), amp(*)        !
      integer          ,intent(out) :: ier                    !

      real, allocatable   :: x(:), y(:), b(:), c(:), d(:)     ! local
      real                :: slope, t, max                    !
      real                :: tpos, tneg, tmid                 !
      real                :: mpos, mneg, mmid                 !
      integer             :: i, itop, ibot, n, ntot, count    !


      ier = 0

      ntot = nup + 1 + ndown

!---- figure out sample range (pad both ends)
      itop = nint((seed_time - tstrt) / dt) - nup - 3
      if(itop < 1) itop = 1
      ibot = itop + ntot + 6

      n = ibot - itop + 1

!---- check for sensibility and allocate
      allocate(x(n), y(n), b(n), c(n), d(n))

!---- populate (simplify logic by converting troughs to peaks)
      do i=1,n
        x(i) = tstrt + dt * (itop - 2 + i)
        y(i) =            tr(itop - 1 + i)
        if(alignment == HRZPCK_ALIGN_TROUGHS) then
          y(i) = - y(i)
        end if
        !write(*,*) x(i), y(i)
      end do
!---- calculate spline
      call spline(n, x, y, b, c, d)

!---- use positive and negative slope regions to converge
      t = seed_time

      if(alignment /= HRZPCK_USE_SEED_AS_IS) then

        call spline_v(n, x, y, b, c, d, t, 1, slope)

!---- first bracket area around peak or trough
        count = 0
        if (slope > 0) then
          tpos = t
          tneg = t
          mpos = slope
          mneg = slope
          do while(mneg >= 0 .and. count < 5)
            tneg = tneg + dt/2
            call spline_v(n, x, y, b, c, d, tneg, 1, mneg)
            count = count + 1
          end do
        else if (slope < 0) then
          tpos = t
          tneg = t
          mpos = slope
          mneg = slope
          do while(mpos <= 0 .and. count < 5)
            tpos = tpos - dt/2
            call spline_v(n, x, y, b, c, d, tpos, 1, mpos)
            count = count + 1
          end do
        else
          tpos = t
          tneg = t
        end if

        if(count >= 5) then
          ier = 1
          return
        end if

!---- now converge using slopes to decide what to do
        count = 0
        do while(abs(tpos - tneg) > dt/100 .and. count < 20)
          tmid = (tpos + tneg) / 2
          !write(*,*) 'tpos, tneg = ', tpos, ', ', tneg
          call spline_v(n, x, y, b, c, d, tmid, 0, mmid)

          if(mmid < 0) then
            tneg = tmid
          else if(mmid > 0) then
            tpos = tmid
          else
            t = tmid
            exit
          end if

        end do

        if(count >= 20) then
          ier = 2
          return
        end if

      end if

!---- now interpolate as many values as were requested
      max = 0
      t = t - nup * dt
      do i=1,ntot
        time(i) = t
        call spline_v(n, x, y, b, c, d, t, 0, amp(i))
        if(alignment == HRZPCK_ALIGN_TROUGHS) then
          amp(i) = -amp(i)
        end if
        t = t + dt
        if(amp(i) > max) max = amp(i)
      end do

!     do i=1,ntot
!       write(*,*) i, time(i), amp(i) / max
!     end do
!     pause

!---- deallocate
      deallocate(x, y, b, c, d)

      end subroutine hrzpck_proximal_spline

!-------------------------------------------------------------------------------
! subroutine to find mute sample, relative mode
!-------------------------------------------------------------------------------
      subroutine hrzpck_threshold_sample (ndpt, hd, tr, factor, index)
      implicit none
      integer           ,intent(in)       :: ndpt                ! arguments
      double precision  ,intent(in)       :: hd(:)               !
      real              ,intent(in)       :: tr(:)               !
      real              ,intent(in)       :: factor              !
      integer           ,intent(out)      :: index               !

      integer               :: i                                 ! local
      double precision      :: big, testamp                      !


      big = hd(HDR_LAV)
      if (big /= 0.0) then
        testamp = factor * big
        do i=1,ndpt
          if (abs(tr(i)) >= testamp) then
            index = i
            return                  ! threshold was met or exceeded
          end if
        end do
      end if

      index = ndpt                  ! threshold wasn't exceeded

      end subroutine hrzpck_threshold_sample


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module hrzpck_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

