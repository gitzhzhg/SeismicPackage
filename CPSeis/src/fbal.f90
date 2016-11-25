!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 1999-09-10. />

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
! Name       : FBAL     (Frequency BALance)
! Category   : filters
! Written    : 1986-10-17   by: John Sinton
! Revised    : 2005-10-24   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Perform window oriented time varying spectral normalization.
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
! FBAL performs time varying spectral normalization by the following steps:
!
!       1.  Each input trace is band-pass filtered in the frequency domain with
!           each frequency band to produce, in the time domain, one filtered 
!           version of the input trace for each frequency band.
!
!       2.  XP (or MVXP) is called internally and applied to each bandpass 
!           filtered version of the input trace to normalize amplitudes in time
!           for each frequency band.
!
!       3.  The amplitude balanced bandpass filtered versions of the input 
!           trace are composited and passed out.
!
! Definition of Frequency Bands
! The first frequency band begins at FREQ_BEG and has a width of BAND_ONE.
! Subsequent frequency bands have a width FCTR_BAND times the width of the next
! lower frequency band, so that if FCTR_BAND = 1.0, then all bands have the same
! width and if FCTR_BAND = 2.0, bands will be octaves.
!
! Amplitude Balancing
! If OPT_BAL = AVE, then XP is called internally to balance amplitudes (average
! statistic is used).  If OPT_BAL = MED, then MVXP is called internally to 
! balance amplitudes (median statistic is used).   XP is used with DEBRI = YES
! and MVXP is used with DEBRI = NO.  WIN_LEN and WIN_INC for XP or MVXP are
! specified by the user.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
! Phase
! FBAL has no effect on the phase of input data.
!
! FBAL and Decon 
! FBAL allows the use of shorter time windows than is ordinarily the case with
! time domain statistical deconvolution and may be able to do more whitening.
! Usually FBAL is used after deconvolution in the processing sequence.  
! Deconvolution is a convolutional process and is less prone to excessive 
! whitening. 
!
! Control of Whitening and Danger of Over-Whitening
! FBAL is capable of extreme whitening because it requires equal amplitude 
! contribution for all frequency bands within each time window.   Values of
! FCTR_BAND > 1.0 produce less whitening than FCTR_BAND = 1.0 because they give
! lower weight to the higher frequencies.  FCTR_BAND is probably the most 
! critical parameter in controlling the amount of whitening.  The default 
! parameter values give reasonably strong whitening.
!
! Lower values of BAND_ONE, FCTR_BAND and WIN_LEN produce greater whitening.
!
! Excessive whitening is often controlled by following FBAL with a mild 
! coherence enhancement process.
! 
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process.
!
! No special requirements.
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
! NWIH     number of entries in header           used but not changed
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
!
! 2       head mute                  used to set XP windows
! 64      tail mute
! 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!      Date       Author     Description
!      ----       ------     -----------
! 14.  2005-10-24 Stoeckley  Add parallel control parameters.
! 13.  2001-04-26 Stoeckley  Change wrapup flag.
! 12.  2000-07-26 O'Brien    Updated NSTORE calculation to result in 'words'
!                            Moved fft_create()s to backend-only updates
!                            Added MVXP as alternate to XP as per document
!                            Consolidated redundant code in main loop
!                            Corrected OPT_BAL option AVG to be AVE as per doc
! 11.  2000-06-08 O'Brien    Fixed problem with poorly initialized variables
! 10.  2000-05-12 O'Brien    Fixed inconsistency in handling endpoints of
!                              input data Amp and Phase spectra.
!  9.  2000-04-05 O'Brien    Brought xml tags up to date
!                            Added RCS character ID variable
!                            Implemented combo box for OPT_BAL
!                            Implemented EzGUI Layout
!                            Added calls to LAV_MODULE
!                            Switched from bandpass_module to bandps_module
!  8.  1999-09-15 O'Brien    Full f90 conversion.
!  7.  1998-11-13 Vunderink  Begin using the f90 compiler.
!  6.  1992-12-18 Reed       Added TYPEX parameter
!  5.  1992-09-28 Troutt     Fixed problem with mute indeces and indexing
!                            of HEAD for XP (added call to MUTEHW).
!  4.  1992-08-17 Reed       Fixed up faulty taper logic
!                            Changed to FSLC and BANDF parameters
!                            Added octave band whitening option
!                            Renamed FBAL to avoid confusion with TVSN
!  3.  1988-09-23 Ball       NWIH and NWPT Conversion
!  2.  1988-04-22 Baumel     Add CPSPRT calls & fix internal call.
!  1.  1986-10-17 Sinton     1st usable routine.
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
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS             
!
! None.
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
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS FBAL Process/NC=80>
!
!                 Frequency BALance Process
! Perform window oriented time varying spectral normalization.
!
!    BAND_ONE= `FFFFFFFFFFF   FCTR_BAND=`FFFFFFFFFFF
!
!    FREQ_BEG= `FFFFFFFFFFF   FREQ_END= `FFFFFFFFFFF
!
!    LEN_TAPER=`FFFFFFFFFFF    
!
!    WIN_LEN=~~`FFFFFFFFFFF   WIN_INC=~~`FFFFFFFFFFF
!
!    OPT_BAL=`CCC
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="BAND_ONE">
!<Tip> Width of first (lowest) frequency band, in Hz. </Tip>
! Default = 10.0
! Allowed = real > 0.0
!
! BAND_ONE is the width of the frequency band measured from the midpoint in
! the low frequency taper to the midpoint in the high frequency taper of the
! lowest frequency band used.
!
!</Help>
!
!<Help KEYWORD="FCTR_BAND">
!<Tip> Ratio of widths of adjacent frequency bands. </Tip>
! Default = 1.5
! Allowed = real > 0.0
! 
! Frequency bands have a width FCTR_BAND times the width of the next lower 
! frequency band, so that if FCTR_BAND = 1.0, then all bands have the same
! width and if FCTR_BAND = 2.0, bands will be octaves. 
! 
!</Help>
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Lowest frequency to use, in Hz. </Tip>
! Default = 5.0
! Allowed = real > 0.0
! FBAL operation constitutes a bandpass filter on the input data from FREQ_BEG 
! at the low frequency end to FREQ_END at the high frequency end.
!
! FREQ_BEG is defined as the half-amplitude point in the range of the LOWest
! frequency taper.
!
! FREQ_END is defined as the half-amplitude point in the range of the HIGHest
! frequency taper.
!
!</Help>
!
!<Help KEYWORD="FREQ_END">
!<Tip> Maximum frequency to use, in Hz. </Tip>
! Default =  -
! Allowed = real > FREQ_BEG
! 
! FBAL operation constitutes a bandpass filter on the input data from FREQ_BEG 
! at the low frequency end to FREQ_END at the high frequency end. 
!
! FREQ_BEG is defined as the half-amplitude point in the range of the LOWest
! frequency taper.
!
! FREQ_END is defined as the half-amplitude point in the range of the HIGHest
! frequency taper.
! 
!</Help>
!
!<Help KEYWORD="LEN_TAPER">
!<Tip> Length of linear taper at each end of frequency bands. </Tip>
! Default =  4.0
! Allowed = real > 0.0
! 
! Linear taper zones are centered on the nominal beginning and ending 
! frequencies of each frequency band.
! 
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<tip> XP (or MVXP) window length, in seconds. </tip>
!  Default = 2.0
!  Allowed = real > 10*DT
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<tip> Time increment for XP (or MVXP) window locations, in seconds. </tip>
!  Default = 1.0
!  Allowed = real >= DT
!</Help>
!
!<Help KEYWORD="OPT_BAL">
!<Tip> Whether to use average or median based amplitude balance. </Tip>
! Default = AVE
! Allowed = AVE  (Use XP - average statistic.)
! Allowed = MED  (Use MVXP - median statistic.)
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module fbal_module

      use pc_module
      use mutehw_module
      use lav_module
      use xp_module
      use mvxp_module
      use fft_module
      use bandps_module
      use sizeof_module
      use pcps_module

      implicit none

      private
      public :: fbal_create     ! uses the parameter cache.
      public :: fbal_initialize
      public :: fbal_update     ! uses the parameter cache.
      public :: fbal_delete
      public :: fbal            ! main execution (trace processing) routine.
      public :: fbal_wrapup

      character(len=100),public,save :: FBAL_IDENT = &
'$Id: fbal.f90,v 1.14 2005/10/24 11:32:15 Stoeckley prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: fbal_struct              
 
        private
        logical              :: skip_wrapup       ! Wrappup flag

        real                 :: band_one          ! process parameters
        real                 :: fctr_band         ! process parameters
        real                 :: freq_beg          ! process parameters
        real                 :: freq_end          ! process parameters
        real                 :: len_taper         ! process parameters
        real                 :: xp_win_len        ! process parameters
        real                 :: xp_win_inc        ! process parameters
        character(len=3)     :: xp_type           ! process parameters

        integer              :: nwih,ndpt         ! globals.
        real                 :: dt                ! globals.

        integer                   :: npo2         ! Power of 2 for ffts
        integer                   :: nw           ! Number of fltr frequencies
        integer                   :: nfilt        ! Number of fltrs applied
        complex,pointer           :: filters(:,:) ! Array of complex filters
        real,pointer              :: rtrace(:)    ! Real data trace
        complex,pointer           :: ctrace(:)    ! Complex data trace
        complex,pointer           :: ftrace(:)    ! Filtered complex trace
        type(fft_struct),pointer  :: rcfft        ! Forward fft object
        type(fft_struct),pointer  :: crfft        ! Inverse fft object
        type(xp_struct),pointer   :: xp           ! Structure for XP
        type(mvxp_struct),pointer :: mvxp         ! Structure for MVXP

      end type fbal_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(fbal_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine fbal_create (obj)
      implicit none
      type(fbal_struct),pointer :: obj       ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers
      nullify (obj%filters)
      nullify (obj%ctrace)
      nullify (obj%ftrace)
      nullify (obj%rtrace)
      nullify (obj%rcfft)
      nullify (obj%crfft)
      nullify (obj%xp)
      nullify (obj%mvxp)
   
      call fbal_initialize (obj)

      return
      end subroutine fbal_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine fbal_delete (obj)
      implicit none
      type(fbal_struct),pointer :: obj       ! arguments

      call fbal_wrapup (obj)

      if (associated(obj%filters)) deallocate      (obj%filters)
      if (associated(obj%ctrace )) deallocate      (obj%ctrace)
      if (associated(obj%ftrace )) deallocate      (obj%ftrace)
      if (associated(obj%rtrace )) deallocate      (obj%rtrace)
      if (associated(obj%rcfft  )) call fft_delete (obj%rcfft)
      if (associated(obj%crfft  )) call fft_delete (obj%crfft)
      if (associated(obj%xp     )) call xp_delete  (obj%xp)
      if (associated(obj%mvxp   )) call mvxp_delete(obj%mvxp)

      deallocate(obj)

      return
      end subroutine fbal_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine fbal_initialize (obj)
      implicit none
      type(fbal_struct),pointer :: obj       ! arguments

! Initialize global values
      obj%nwih = HDR_NOMINAL_SIZE
      obj%ndpt = 1
      obj%dt   = 0.001
      call pc_get_global ('NWIH',obj%nwih)
      call pc_get_global ('NDPT',obj%ndpt)
      call pc_get_global ('DT'  ,obj%dt  )

! Initialize process parameters
      obj%band_one   = 10.0
      obj%fctr_band  = 1.0
      obj%freq_beg   = 5.0
      obj%freq_end   = 1.0/(4.0*obj%dt)    ! half nyquist
      obj%len_taper  = 4.0
      obj%xp_win_len = 2.0
      obj%xp_win_inc = 1.0
      obj%xp_type    = 'AVE'

      call fbal_update (obj)

      return
      end subroutine fbal_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine fbal_update (obj)
      implicit none
      type(fbal_struct),target :: obj               ! Arguments

      integer     :: ifilt                          ! Local
      integer     :: nstore                         ! Local
      integer     :: ier1,ier2,ier3,ier4            ! Local
      integer     :: update_state                   ! Local
      real        :: f1,f2, f3, f4                  ! Local
      real        :: f_lower, f_upper, band_width   ! Local
      real        :: dw, fnyq                       ! Local

      integer           :: mem_size               ! Local
      integer           :: result                 ! Local
      character(len=80) :: message                ! Local
      character(len=11) :: filter_type            ! Local

      integer   :: SIZEOF_REAL
      integer   :: SIZEOF_COMPLEX
!-------------------------------------------------
      object => obj               ! needed for traps.
      obj%skip_wrapup  = .true.

      update_state = pc_get_update_state()

      SIZEOF_REAL    = sizeof(1.0)
      SIZEOF_COMPLEX = sizeof(cmplx(1.0,1.0))

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get ('BAND_ONE' , obj%band_one,   fbal_trap)
      call pc_get ('FCTR_BAND', obj%fctr_band,  fbal_trap)
      call pc_get ('FREQ_BEG' , obj%freq_beg ,  fbal_trap)
      call pc_get ('FREQ_END' , obj%freq_end ,  fbal_trap)
      call pc_get ('LEN_TAPER', obj%len_taper,  fbal_trap)
      call pc_get ('WIN_LEN'  , obj%xp_win_len, fbal_trap)
      call pc_get ('WIN_INC'  , obj%xp_win_inc, fbal_trap)
      call pc_get ('OPT_BAL'  , obj%xp_type  ,  fbal_trap)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! Get the fft size
      obj%npo2 = 4
      do while ( obj%npo2 < obj%ndpt )
        obj%npo2 = obj%npo2*2
      enddo

! Set nw and dw for frequency domain filters
      obj%nw = obj%npo2/2 + 1
      fnyq   = 0.5/obj%dt
      dw     = fnyq/(obj%nw-1)

! Nail down the number of frequency bands
      f1 = obj%freq_beg
      f2 = obj%freq_beg
      band_width = obj%band_one/obj%fctr_band
      obj%nfilt = 0
      do
        if(f2 >= obj%freq_end) exit
        band_width = band_width * obj%fctr_band
        f1 = f2
        f2 = f1 + band_width
        obj%nfilt = obj%nfilt + 1
        cycle
      enddo
      if(f2 > obj%freq_end) obj%nfilt = obj%nfilt-1

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

! Create xp or mvxp object
      call pc_clear
      call pc_put('WIN_LEN',obj%xp_win_len)
      call pc_put('WIN_INC',obj%xp_win_inc)
      if (obj%xp_type == 'AVE') then
        call pc_put('DEBRI','YES')
        if (associated(obj%xp)) then
          call xp_update (obj%xp)
        else
          call xp_create (obj%xp)
        endif
      else
        call pc_put('DEBRI', 'NO' )
        if (associated(obj%mvxp)) then
          call mvxp_update (obj%mvxp)
        else
          call mvxp_create (obj%mvxp)
        endif
      endif
      call pc_restore

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('OPT_BAL',(/'AVE','MED'/), 2)

      call pc_put ('BAND_ONE' , obj%band_one  )
      call pc_put ('FCTR_BAND', obj%fctr_band )
      call pc_put ('FREQ_BEG' , obj%freq_beg  )
      call pc_put ('FREQ_END' , obj%freq_end  )
      call pc_put ('LEN_TAPER', obj%len_taper )
      call pc_put ('WIN_LEN'  , obj%xp_win_len)
      call pc_put ('WIN_INC'  , obj%xp_win_inc)
      call pc_put ('OPT_BAL'  , obj%xp_type   )

!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!

! Update control parameters
      nstore   = obj%nw*(obj%nfilt+2)*SIZEOF_COMPLEX
      nstore   = nstore + (obj%npo2+2)*SIZEOF_REAL
      nstore   = nstore + fft_mem_usage(obj%npo2,1)*SIZEOF_REAL
      nstore   = nstore + fft_mem_usage(obj%npo2,1)*SIZEOF_REAL
      call pc_put_control ('NSTORE', nstore/SIZEOF_REAL)

      call pc_put_control ('PARALLEL_SAFE'        , .true.)
      call pc_put_control ('PCPS_SEND_MODE'       , 'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control ('PCPS_RECEIVE_MODE'    , 'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control ('PCPS_BUNCH_MODE'      , 'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control ('PCPS_SEND_EOF_MODE'   , 'PCPS_SEND_ALL_EOF')
      call pc_put_control ('PCPS_ALT_SEND_MODE'   , 'PCPS_SEND_ALL')
      call pc_put_control ('PCPS_ALT_RECEIVE_MODE', 'PCPS_RECEIVE_ALL_EOF')

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! Create forward and reverse fft objects
      ier1 = fft_create (obj%rcfft,-1,obj%npo2,'rtoc')
      ier2 = fft_create (obj%crfft, 1,obj%npo2,'ctor')

! Allocate memory at structure pointers
      allocate (obj%filters(obj%nw,obj%nfilt), stat=ier1)
      allocate (obj%ctrace(obj%nw), stat=ier2)
      allocate (obj%ftrace(obj%nw), stat=ier3)
      allocate (obj%rtrace(obj%npo2+2), stat=ier4)
      if (ier1/=0 .or. ier2/=0 .or. ier3/=0 .or. ier4/=0) then
        mem_size = obj%nw*(2+obj%nfilt)*SIZEOF_COMPLEX
        mem_size = mem_size + (2+obj%npo2)*SIZEOF_REAL
        call pc_error('Unable to allocate space for filters and workspace ',&
                       mem_size,' bytes.')
      endif

! Setup filters for different frequency bands using bandps.
      filter_type = 'BANDPASS'
      band_width = obj%band_one/obj%fctr_band
      f_upper = obj%freq_beg
      f3 = max(  0.0, obj%freq_beg - 0.5*obj%len_taper )
      f4 = min( fnyq, obj%freq_beg + 0.5*obj%len_taper )

      do ifilt = 1,obj%nfilt

        band_width = band_width * obj%fctr_band
        f_lower = f_upper
        f_upper = min(f_lower+band_width,obj%freq_end)
        if (ifilt == obj%nfilt ) f_upper=obj%freq_end
        f1 = f3
        f2 = f4
        f3 = max(  0.0, f_upper - 0.5*obj%len_taper )
        f4 = min( fnyq, f_upper + 0.5*obj%len_taper )

! Pass this filter into bandps_check
        call bandps_check(result,message,fnyq,filter_type,f1,f2,f3,f4)

! Evaluate the result from bandps_check and act accordingly
        select case (result)
          case (BANDPS_INFO)
            call pc_info(message)
            call bandps(obj%filters(1:obj%nw,ifilt),obj%nw,dw,filter_type, &
                        f1,f2,f3,f4)
          case (BANDPS_ERROR)
            call pc_error(message)
          case (BANDPS_ENDERROR)
            call pc_error(message)
          case default
            call bandps(obj%filters(1:obj%nw,ifilt),obj%nw,dw,filter_type, &
                        f1,f2,f3,f4)
        end select

      enddo

!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!
!!--------------------------- finish update ----------------------------------!!

      return
      end subroutine fbal_update

!!--------------------------------- traps ------------------------------------!!
!!--------------------------------- traps ------------------------------------!!
!!--------------------------------- traps ------------------------------------!!
   
      subroutine fbal_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! Arguments
      real                        :: fnyq              ! Local
!------------------------------------------------------------

! It's useful to know Nyquist
      fnyq = 0.5/object%dt
   
      select case (keyword)

        case('BAND_ONE')
          if (object%band_one <= 0.0) &
            call pc_error ('BAND_ONE must be greater than 0')

        case('FCTR_BAND')
          if (object%fctr_band <= 0.0) &
            call pc_error ('FCTR_BAND must be greater than 0')

        case('FREQ_BEG')
          if (object%freq_beg <= 0.0) &
            call pc_error ('FREQ_BEG must be greater than 0.0')
          if (object%freq_beg > fnyq) &
            call pc_error ('FREQ_BEG cannot be greater than Nyquist:',fnyq)

        case('FREQ_END')
          if (object%freq_end < object%freq_beg) &
            call pc_error ('FREQ_END must be greater than FREQ_BEG')
          if (object%freq_end > fnyq) &
            call pc_error ('FREQ_END cannot be greater than Nyquist:',fnyq)

        case('LEN_TAPER')
          if (object%len_taper <= 0) &
            call pc_error ('LEN_TAPER must be greater than 0.0')

        case('WIN_LEN')
          if (object%xp_win_len <= 0.0) &
            call pc_error ('WIN_LEN must be greater than 0.0')

        case('WIN_INC')
          if (object%xp_win_inc <= 0.0) &
            call pc_error ('WIN_INC must be greater than 0.0')

        case('OPT_BAL')
          call string_to_upper(object%xp_type)
          if (object%xp_type /= 'AVE' .and. object%xp_type /= 'MED') &
            call pc_error ('OPT_BAL must be AVE or MED')

      end select

      return
      end subroutine fbal_trap
   
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

      subroutine fbal (obj,ntr,hd,tr)
      implicit none
      type(fbal_struct)               :: obj                ! arguments
      integer,         intent(inout)  :: ntr                ! arguments
      double precision,intent(inout)  :: hd(:,:)            ! arguments
      real,            intent(inout)  :: tr(:,:)            ! arguments

      integer                     :: itrc,ifilt             ! local
      real                        :: trc_2d(obj%ndpt,1)     ! local

      integer           :: ONE    ! one trace at a time goes into (MV)XP
      data  ONE/1/                ! but f90 requires a variable be passed
                                  ! to a dummy arg with intent(inout).
!----------------------------------------------------------------

! Do nothing if there are no filters
      if (obj%nfilt == 0) return

! Loop over traces
      do itrc = 1, ntr

! Fourier transform the trace to the frequency domain
        obj%rtrace(1:obj%ndpt) = tr(1:obj%ndpt,itrc)
        obj%rtrace(obj%ndpt+1:obj%npo2+2) = 0.0
        call fft_rc_transform(obj%rcfft,obj%rtrace,obj%ctrace)
        obj%ctrace(1) = cmplx(0.0,0.0)
        obj%ctrace(obj%nw) = cmplx(0.0,0.0)
 
! Zero the output trace prior to accumulating filter bands
        tr(1:obj%ndpt,itrc) = 0.0

! Now accumulate results for the remaining pass bands into tr().
        do ifilt = 1, obj%nfilt
 
          obj%ftrace(:) = obj%ctrace(:)*obj%filters(:,ifilt)

          call fft_cr_transform(obj%crfft,obj%ftrace,obj%rtrace)

          call mutehw (hd(:,itrc), obj%rtrace, obj%ndpt, 0.0, MUTEHW_BOTH)

          ! XP and MVXP need hd() and tr() to be rank 2 arrays, so
          ! tmp_2d is needed to hold a copy of filtered real trace.
          trc_2d(:,1) = obj%rtrace(1:obj%ndpt)
          if ( obj%xp_type == 'AVE' ) then
            call xp (obj%xp, ONE, hd(:,itrc:itrc), trc_2d)
          else
            call mvxp (obj%mvxp, ONE, hd(:,itrc:itrc), trc_2d)
          endif
 
! Accumulate the output trace
          tr(1:obj%ndpt,itrc) = tr(1:obj%ndpt,itrc) + trc_2d(1:obj%ndpt,1)

        enddo
      enddo

! Finish up by recomputing trace LAV values
      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      return
      end subroutine fbal

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

      subroutine fbal_wrapup (obj)
      implicit none
      type(fbal_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine fbal_wrapup

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module fbal_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
