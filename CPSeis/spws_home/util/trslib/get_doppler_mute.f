C      get_doppler_mute.f
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C     written in fortran -- designed to be called from fortran or c
C
C     Utility Name:  get_doppler_mute    (get CPS doppler mute time)
C          Written:  93/03/17  by:  Tom Stoeckley
C     Last revised:  93/03/17  by:  Tom Stoeckley
C
C  Purpose:   This routine should give the same doppler mute time
C             as the CPS process NMC.  
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine            source code directory       library
C  -------            ---------------------       -------
C  ultrix             ~spws/util/trslib           trslib.a
C
C  c files     c++ files     fortran files         other files
C  -------     ---------     -------------         -----------
C  none        none          get_doppler_mute.f    get_doppler_mute.h
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C                                none
C-----------------------------------------------------------------------
C           FORTRAN ROUTINES ON SOURCE FILE get_doppler_mute.f
C
C  Subroutines:        get_doppler_mute
C  Subroutine entries: none
C  Functions:          none
C  Function entries:   none
C  Common blocks:      none
C-----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:     trslib.a  cpsprim.a
C  Header files:  none
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C
C                        get_fnil      terp1
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/03/17  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C-----------------------------------------------------------------------
C                        CALLING SEQUENCE
C
C                             i   i    i      i       i i i      o
C     call get_doppler_mute (tmax,dt,dopler,offset,   t,v,n,   tmute)
C
c  real   tmax   = maximum mute time (seconds) to search for.
c  real   dt     = desired resolution in mute time (seconds).
c  real   dopler = CPS doppler mute parameter (usual value 1.7).
c  real   offset = offset of trace.
c  real   t(n)   = velocity function zero-offset time picks (seconds).
c  real   v(n)   = velocity function stacking velocity pick.
c  integer n     = number of velocity function picks.
c  real   tmute  = mute time (between 0 and tmax) (seconds) (returned).
C-----------------------------------------------------------------------
C                         INTERNAL VARIABLES
C
C   time = zero-offset time (seconds)          = (i-1)*dt 
C   vnmo = stacking velocity                   = terp1(time,t,n,v) 
C  evloc = normalized inverse velocity squared = 1/(vnmo*dt)**2
C    ttt = normalized zero-offset time squared = (time/dt)**2 = (i-1)**2
C evlsum = argument under square root          = (i-1)**2 + (offset*evloc)**2
C  tnmoi = sample index before nmo correction  = 1 + sqrt(evlsum) 
C      i = sample index after nmo correction   = 1 + time/dt
C
C  It is assumed that the first sample (i=1) is at zero time.
C-----------------------------------------------------------------------
C                      DOPPLER MUTE DETAILS
C
C  DOPLER   1.7    rel>1    Doppler mute factor.  Increasing values of
C                           DOPLER allow more stretch, thus less severe
C                           mute.  You may set
C                           DOPLER=0 to avoid stretch muting, although
C                           some muting will be done anyway (NOTE 10).
C                           You can completely DISABLE ALL
C                           MUTING by setting DOPLER negative.
C
C 10. You may set DOPLER=0 to avoid muting based on a stretch criterion.
C     Some muting will, however, still be applied in an attempt to avoid
C     crossing events and refractions. If you absolutely want no muting,
C     you can set DOPLER negative.
C     In this latter case, you're on your own as far as the validity
C     of the preserved data!
C
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC

      subroutine get_doppler_mute
     $                     (tmax,dt,dopler,offset,   t,v,n,   tmute)

      implicit none
      real tmax,dt,dopler,offset,t(*),v(*),tmute
      integer n
      real    EVLSUM(1)   ,evloc(1)   ,TNMOI(1)
      integer evlsum_off  ,evloc_off  ,tnmoi_off
      integer evlsum_point,evloc_point,tnmoi_point
      integer i,ndpt,istrt,istop,istrt0
      real fnil,terp1,offs,time,vnmo,strt02,tesval,dopler2

      fnil = -1.0e-30
      tmute=0.
      if (n.le.0.or.dt.le.0..or.dopler.lt.0.) return

      do i=1,n
           if (t(i).eq.fnil.or.v(i).eq.fnil.or.v(i).le.0.) return
           if (i.gt.1) then
                if (t(i).le.t(i-1)) return
           end if
      end do
      dopler2 = dopler
      if (dopler.gt.0.) dopler2=1./dopler
      
      ndpt=1+nint(tmax/dt)
      if (ndpt.le.1) return
      call alloc_words_offset (evlsum_off,evlsum,evlsum_point,ndpt)
      call alloc_words_offset ( evloc_off, evloc, evloc_point,ndpt)
      call alloc_words_offset ( tnmoi_off, tnmoi, tnmoi_point,ndpt)
      offs=offset**2
      istrt = 1

c        print *, tmax,dt,dopler,offset
c               do i=1,n
c                  print *, i,t(i),v(i)
c               end do
         do i=1,ndpt
           time=(i-1)*dt               ! time at this sample
           vnmo=terp1(time,t,n,v)      ! stacking velocity at this sample
           evloc ( evloc_off+i) = 1./(vnmo*dt)**2
           evlsum(evlsum_off+i) = (i-1)**2 + offs*evloc(evloc_off+i)
           tnmoi ( tnmoi_off+i) =  1. + sqrt(evlsum(evlsum_off+i))
c             if (mod(i,200).eq.1)
c    $         print *, i,time,vnmo,evloc(evloc_off+i),
c    $            evlsum(evlsum_off+i),tnmoi(tnmoi_off+i)
           if (tnmoi(tnmoi_off+i) .gt. ndpt-0.001)  go to 21
         end do

  21     istop = i-1
         if (dopler2 .gt. 0.)  then           ! Find Doppler Mute.
           istrt0=istrt+1
           do i=istop,istrt0,-1
             if (tnmoi(tnmoi_off+i)-tnmoi(tnmoi_off+i-1) .le. dopler2)
     $                                                            then
               istrt = i
               go to 225
             end if
           end do

         else if(dopler2.lt.0.) then         ! Skip rest of muting logic.
             go to 246
         end if

  225    if (dopler2 .lt. 0.5)  then          ! Find Refraction Mute.
           istrt0=istrt+1
           do i=istop,istrt0,-1
             if (evlsum(evlsum_off+i)-evlsum(evlsum_off+i-1) .le. i-1.5)
     $                                                            then
               istrt = i
               go to 235
             end if
           end do
         end if
                                             ! Find Event-Crossing Mute.
  235    strt02 = max ( 0. , - offs*evloc(evloc_off+1) )
         istrt0 = max ( nint(1.+sqrt(strt02)) , 1 )
         if (istrt0 .lt. istrt)  then
           tesval=0.
           do i=istrt0,istrt-1
                tesval=max(tnmoi(tnmoi_off+i),tesval)
           end do
           do i=istrt,istop
             if (tnmoi(tnmoi_off+i) .gt. tesval)  go to 245
           end do
  245      istrt = i
         end if

  246 tmute=(istrt-1)*dt
      call dealloc_memory (evlsum_point)
      call dealloc_memory ( evloc_point)
      call dealloc_memory ( tnmoi_point)
      return
      end

