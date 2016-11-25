!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ tdfilter.f90 ------------------------------!!
!!------------------------------ tdfilter.f90 ------------------------------!!
!!------------------------------ tdfilter.f90 ------------------------------!!


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
! Name       : TDFILTER              (Time Domain FILTER)
! Category   : filters
! Written    : 2000-12-14   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : Time domain filter primitive for the TVF and TSVF processes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is a time domain filter object used by the TVF and TSVF
! processes.  This primitive uses the BANDPS and FFT primitives.
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
!                                i          b      b  b  b  b    b
!     call tdfilter_constrain (nyquist,filter_type,f1,f2,f3,f4,phase)
!
!                                                   opt         opt
!                            o     i       i         i           i
!     call tdfilter_create (obj, ncorr, nyquist, delta_freq, delta_phase)
!     call tdfilter_delete (obj)
!                            b
!
!                                  i       i      i  i  i  i    i     o
!     call tdfilter_build_filter (obj,filter_type,f1,f2,f3,f4,phase,filter)
!
!
!                                          i   i   i     o
!     call tdfilter_make_lowpass_filter  (obj, f3, f4, filter)
!     call tdfilter_make_highpass_filter (obj, f1, f2, filter)
!     call tdfilter_make_phase_filter    (obj,  phase, filter)
!                                          i      i      o
!
!
! type(tdfilter_struct)   obj           = pointer to the TDFILTER structure.
! real                    nyquist       = Nyquist frequency (hz).
! character(len=*)        filter_type   = filter type (see below).
! real                    f1,f2,f3,f4   = corner frequencies (see below).
! real                    phase         = phase (degrees).
! integer                 ncorr         = number of points in filter.
! real                    delta_freq    = frequency increment (hz).
! real                    delta_phase   = phase increment (degrees).
! real                    filter(ncorr) = time domain filter.
!
!-------------------------------------------------------------------------------
!                     FILTER TYPES AND FREQUENCIES          
!
! To filter a seismic trace, the trace should be correlated with the time
! domain filter.
!
! The allowed filter types and corner frequencies are these:
!
!                      0 <= f1 <= f2 <= f3 <= f4 <= Nyquist
!
!   filter type        f1             f2             f3              f4
!                 FREQ_LOW_NONE  FREQ_LOW_FULL  FREQ_HIGH_FULL  FREQ_HIGH_NONE
!
!   'BANDPASS'       used           used           used            used   
!   'LOWPASS'       ignored        ignored         used            used   
!   'HIGHPASS'       used           used          ignored         ignored
!   'ALLPASS'       ignored        ignored        ignored         ignored
!
!      ^                   #----------------#
!      |                  /                  \
!      |                 /                    \
!  amplitude            /                      \
!                      /                        \
!           ----------#--------------------------#------------
!                     f1   f2              f3    f4
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE DETAILS                 
!
! TDFILTER_CONSTRAIN:
!  (1) call at any time to verify the arguments for later use.
!  (2) FILTER_TYPE is expanded to one of the four full-name values based
!       on the first character.  (example:  'b' is expanded to 'BANDPASS')
!  (3) F1,F2,F3,F4 are adjusted to satisfy the constraints.
!  (4) nil values are replaced by all-pass zero-phase defaults.
!
! TDFILTER_CREATE:
!  (1) allocates the data structure.
!  (2) if DELTA_FREQ  is present and >0, calculates table of lowpass filters.
!  (3) if DELTA_PHASE is present and >0, calculates table of  phase  filters.
!  (4) if an error occurs, deallocates the data structure.
!
! TDFILTER_CREATE:
!  (1) deallocates the data structure (unless already deallocated).
!
! TDFILTER_BUILD_FILTER:
!  (1) builds a filter with the specified features.
!
! TDFILTER_MAKE_LOWPASS_FILTER:
! TDFILTER_MAKE_HIGHPASS_FILTER:
!  (1) if DELTA_FREQ >0, makes filter from the pre-calculated lowpass filters.
!  (2) otherwise calls TDFILTER_BUILD_FILTER to build the filter.
!
! TDFILTER_MAKE_PHASE_FILTER:
!  (1) if DELTA_PHASE >0, returns a pre-calculated phase filter.
!  (2) otherwise calls TDFILTER_BUILD_FILTER to build the filter.
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
!003. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  2. 2002-02-04  Stoeckley  Fix problems with negative phases and zero tapers.
!  1. 2001-02-22  Stoeckley  Initial version.
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


      module tdfilter_module
      use bandps_module
      use fft_module
      use mth_module
      use named_constants_module
      implicit none
      public

      character(len=100),public,save :: TDFILTER_IDENT = &
'$Id: tdfilter.f90,v 1.3 2006/10/17 13:45:48 Glover prod sps $'

      type,public :: tdfilter_struct
        private
        integer          :: ncorr           ! number of points in filters.
        integer          :: npower2         ! power of two.
        real             :: nyquist         ! nyquist frequency.
        real             :: delta_freq      ! lowpass filter freq increment.
        real             :: delta_phase     ! phase filter phase increment.
        integer          :: number_freq     ! number of lowpass filters.
        integer          :: number_phase    ! number of phase filters.

        type(fft_struct),pointer :: fft  
        real            ,pointer :: lowpasses    (:,:)  ! lowpass filters.
        real            ,pointer :: phases       (:,:)  ! phase filters.
        real            ,pointer :: lowpasses_power(:)  ! lowpass filter power.

      end type tdfilter_struct

      contains


!!----------------------------- constrain ----------------------------------!!
!!----------------------------- constrain ----------------------------------!!
!!----------------------------- constrain ----------------------------------!!


      subroutine tdfilter_constrain (nyquist,filter_type,f1,f2,f3,f4,phase)
      implicit none
      real               ,intent(in)    :: nyquist             ! arguments
      character(len=*)   ,intent(inout) :: filter_type         ! arguments
      real               ,intent(inout) :: f1,f2,f3,f4,phase   ! arguments

      if (f1    == FNIL) f1    = 0.0
      if (f2    == FNIL) f2    = 0.0
      if (f3    == FNIL) f3    = nyquist
      if (f4    == FNIL) f4    = nyquist
      if (phase == FNIL) phase = 0.0

      call mth_constrain (f1, 0.0, nyquist)
      call mth_constrain (f2,  f1, nyquist)
      call mth_constrain (f3,  f2, nyquist)
      call mth_constrain (f4,  f3, nyquist)

      select case (filter_type(1:1))
           case ('B')   ;  filter_type = 'BANDPASS'
           case ('b')   ;  filter_type = 'BANDPASS'
           case ('H')   ;  filter_type = 'HIGHPASS'
           case ('h')   ;  filter_type = 'HIGHPASS'
           case ('L')   ;  filter_type = 'LOWPASS'
           case ('l')   ;  filter_type = 'LOWPASS'
           case ('A')   ;  filter_type = 'ALLPASS'
           case ('a')   ;  filter_type = 'ALLPASS'
           case default ;  filter_type = 'BANDPASS'
      end select

      select case (filter_type)
           case ('HIGHPASS')
                   f3 = nyquist
                   f4 = nyquist
           case ('LOWPASS')
                   f1 = 0.0
                   f2 = 0.0
           case ('ALLPASS')
                   f1 = 0.0
                   f2 = 0.0
                   f3 = nyquist
                   f4 = nyquist
      end select
      return
      end subroutine tdfilter_constrain


!!------------------------------- create ------------------------------------!!
!!------------------------------- create ------------------------------------!!
!!------------------------------- create ------------------------------------!!


      subroutine tdfilter_create (obj, ncorr, nyquist, delta_freq, delta_phase)
      implicit none
      type(tdfilter_struct),pointer    :: obj                      ! arguments
      integer              ,intent(in) :: ncorr                    ! arguments
      real                 ,intent(in) :: nyquist                  ! arguments
      real        ,optional,intent(in) :: delta_freq               ! arguments
      real        ,optional,intent(in) :: delta_phase              ! arguments
      integer                          :: npower2,ncorr2,ier       ! local
      integer                          :: ifreq,iphase             ! local
      real                             :: f1,f2,f3,f4,phase,power  ! local

!----------get started:

      allocate (obj)
      nullify (obj%fft) ! jpa
      nullify (obj%lowpasses) ! jpa
      nullify (obj%phases) ! jpa
      nullify (obj%lowpasses_power) ! jpa
      obj%ncorr   = ncorr
      obj%nyquist = nyquist

      ncorr2  = min(obj%ncorr,8192)
      npower2 = max(obj%ncorr, 512)
      do while (npower2 < ncorr2)
        npower2 = npower2*2
      end do
      obj%npower2 = npower2

!----------create fft object:

      ier = fft_create (obj%fft, 1, obj%npower2, 'ctor')

      if (ier == -1) then
           deallocate (obj)
           return
      end if

!----------use delta_freq:

      if (present(delta_freq)) then
           obj%delta_freq = delta_freq
      else
           obj%delta_freq = 0.0
      end if

      if (obj%delta_freq > 0.0) then
           obj%number_freq = 1 + nint(obj%nyquist / obj%delta_freq)
           allocate (obj%lowpasses(obj%ncorr,obj%number_freq))
           allocate (obj%lowpasses_power(obj%number_freq))
      else
           obj%number_freq = 0
           obj%delta_freq  = 0.0
           nullify (obj%lowpasses)
           nullify (obj%lowpasses_power)
      end if

!----------use delta_phase:

      if (present(delta_phase)) then
           obj%delta_phase = delta_phase
      else
           obj%delta_phase = 0.0
      end if

      if (obj%delta_phase > 0.0) then
           obj%number_phase = nint(360.0 / obj%delta_phase)
           allocate (obj%phases(obj%ncorr,obj%number_phase))
      else
           obj%number_phase = 0
           obj%delta_phase  = 0.0
           nullify (obj%phases)
      end if

!----------build the lowpass filters:

      f1    = 0.0
      f2    = 0.0
      phase = 0.0

      do ifreq = 1,obj%number_freq

           f3 = (ifreq - 1) * obj%delta_freq
           f4 = (ifreq - 1) * obj%delta_freq

           call tdfilter_build_filter  &
                   (obj,'LOWPASS',f1,f2,f3,f4,phase,obj%lowpasses(:,ifreq))

           power = 0.0
           call mth_power (obj%lowpasses(:,ifreq),obj%ncorr,power)
           if (power == 0.0) power = 1.0

           obj%lowpasses_power(ifreq) = power

      end do

!----------build the phase filters:

      f1 = 0.0
      f2 = 0.0
      f3 = obj%nyquist
      f4 = obj%nyquist

      do iphase = 1,obj%number_phase

           phase = (iphase - 1) * obj%delta_phase

           call tdfilter_build_filter  &
                   (obj,'ALLPASS',f1,f2,f3,f4,phase,obj%phases(:,iphase))

           power = 0.0
           call mth_power (obj%phases(:,iphase),obj%ncorr,power)
           if (power == 0.0) power = 1.0

           obj%phases(:,iphase) = obj%phases(:,iphase) / power

      end do
      return
      end subroutine tdfilter_create


!!------------------------------- delete ------------------------------------!!
!!------------------------------- delete ------------------------------------!!
!!------------------------------- delete ------------------------------------!!


      subroutine tdfilter_delete (obj)
      implicit none
      type(tdfilter_struct),pointer :: obj                     ! arguments

      if (associated(obj)) then
           call fft_delete (obj%fft)
           if (associated(obj%lowpasses      )) deallocate (obj%lowpasses)
           if (associated(obj%phases         )) deallocate (obj%phases)
           if (associated(obj%lowpasses_power)) deallocate (obj%lowpasses_power)
           deallocate (obj)
      end if
      return
      end subroutine tdfilter_delete


!!------------------------------- build filter ------------------------------!!
!!------------------------------- build filter ------------------------------!!
!!------------------------------- build filter ------------------------------!!


      subroutine tdfilter_build_filter  &
                             (obj,filter_type,f1,f2,f3,f4,phase,filter)
      implicit none
      type(tdfilter_struct),intent(in)  :: obj                     ! arguments
      character(len=*)     ,intent(in)  :: filter_type             ! arguments
      real                 ,intent(in)  :: f1,f2,f3,f4,phase       ! arguments
      real                 ,intent(out) :: filter(:)               ! arguments
      complex                           :: fd_filter(obj%npower2/2+1) ! local
      real                              :: td_filter(obj%npower2)     ! local
      integer                           :: num_freq,ishift            ! local
      real                              :: freq_inc,amp_fact          ! local

      num_freq = obj%npower2/2 + 1
      freq_inc = obj%nyquist/(num_freq-1)
      amp_fact = 1.0 / obj%npower2

      call bandps  &
         (fd_filter,num_freq,freq_inc,filter_type,f1,f2,f3,f4,phase,amp_fact)

      call fft_cr_transform (obj%fft,fd_filter,td_filter)

! Compute the shift needed to put all filter lag times at front of td_filter.
! Ncorr should always be odd so the shift should be (ncorr/2)-1. This equation
! makes allowances for ncorr being even.

      ishift = (obj%ncorr+1)/2 - 1

! Time=0 for filter is at sample 1, negative times are wrapped around at
! the end.  Simply shift the filter vector to get all negative and positive
! filter lag times at the begining of the td_filter() vector.

      td_filter = cshift (td_filter, -ishift)

! The filter is stored in reverse order because it is applied with
! a simple correlation.

      filter(obj%ncorr:1:-1) = td_filter(1:obj%ncorr)
      return
      end subroutine tdfilter_build_filter


!!----------------------- make lowpass filter ----------------------------!!
!!----------------------- make lowpass filter ----------------------------!!
!!----------------------- make lowpass filter ----------------------------!!

                             ! (highcut)

      subroutine tdfilter_make_lowpass_filter (obj,f3,f4,filter)
      implicit none
      type(tdfilter_struct),intent(in)  :: obj                   ! arguments
      real                 ,intent(in)  :: f3,f4                 ! arguments
      real                 ,intent(out) :: filter(:)             ! arguments
      integer                           :: imid,itaper           ! local
      real                              :: fmid,ftaper           ! local

      if (obj%number_freq > 0) then
           fmid   = 0.5 * (f4+f3)
           ftaper = 0.5 * (f4-f3)
           imid   = 1 + nint(fmid   / obj%delta_freq)
           itaper = 1 + nint(ftaper / obj%delta_freq)
           if (itaper == 1) then
                filter(1:obj%ncorr) =     obj%lowpasses(:,imid)
           else
                filter(1:obj%ncorr) =     obj%lowpasses(:,imid)    &
                                        * obj%lowpasses(:,itaper)  &
                                        / obj%lowpasses_power(itaper)
           end if
      else
           call tdfilter_build_filter (obj,'LOWPASS',0.0,0.0,f3,f4,0.0,filter)
      end if
      return
      end subroutine tdfilter_make_lowpass_filter


!!----------------------- make highpass filter ----------------------------!!
!!----------------------- make highpass filter ----------------------------!!
!!----------------------- make highpass filter ----------------------------!!

                             ! (lowcut)

      subroutine tdfilter_make_highpass_filter (obj,f1,f2,filter)
      implicit none
      type(tdfilter_struct),intent(in)  :: obj                   ! arguments
      real                 ,intent(in)  :: f1,f2                 ! arguments
      real                 ,intent(out) :: filter(:)             ! arguments
      integer                           :: imid,itaper           ! local
      real                              :: fmid,ftaper           ! local

      if (obj%number_freq > 0) then
           fmid   = 0.5 * (f2+f1)
           ftaper = 0.5 * (f2-f1)
           imid   = 1 + nint(fmid   / obj%delta_freq)
           itaper = 1 + nint(ftaper / obj%delta_freq)
           if (itaper == 1) then
                filter(1:obj%ncorr) =    obj%lowpasses(:,obj%number_freq)  &
                                       - obj%lowpasses(:,imid)
           else
                filter(1:obj%ncorr) =    obj%lowpasses(:,obj%number_freq)  &
                                       - obj%lowpasses(:,imid)             &
                                       * obj%lowpasses(:,itaper)           &
                                       / obj%lowpasses_power(itaper)
           end if
      else
           call tdfilter_build_filter &
                   (obj,'HIGHPASS',f1,f2,obj%nyquist,obj%nyquist,0.0,filter)
      end if
      return
      end subroutine tdfilter_make_highpass_filter


!!-------------------------- make phase filter ----------------------------!!
!!-------------------------- make phase filter ----------------------------!!
!!-------------------------- make phase filter ----------------------------!!


      subroutine tdfilter_make_phase_filter (obj,phase,filter)
      implicit none
      type(tdfilter_struct),intent(in)  :: obj                   ! arguments
      real                 ,intent(in)  :: phase                 ! arguments
      real                 ,intent(out) :: filter(:)             ! arguments
      integer                           :: iphase                ! local
      real                              :: phase2                ! local

      if (obj%number_phase > 0) then
           phase2 = modulo (phase, 360.0)
           iphase = 1 + nint(phase2 / obj%delta_phase)
           filter(1:obj%ncorr) = obj%phases(:,iphase)
      else
           call tdfilter_build_filter &
                  (obj,'ALLPASS',0.0,0.0,obj%nyquist,obj%nyquist,phase,filter)
      end if
      return
      end subroutine tdfilter_make_phase_filter


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module tdfilter_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

