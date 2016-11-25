!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-09-08. />

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
! Name       : FXDECON    (F-X Decon coherence enhancement)
! Category   : filters
! Written    : 2000-11-29   by: Stephen Chiu
! Revised    : 2010-08-02   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : 2D coherence enhancement by the F-X Deconvolution method.
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
! FXDECON coherence enhancement is done separately for each time window for
! each input trace.  Time windows overlap by 50%.
! 
! Each trace subset of NUM_TR traces is Fourier transformed over time to form 
! the F-X window.  Then each frequency slice is autocorrelated and prediction
! filters are derived.  These filters are applied to the frequency slice
! in forward and backward prediction and the outputs averaged to give an output
! slice of prediction signal (output with noise removed). This process is 
! repeated for each frequency independently, and the processed volume is
! inverse Fourier transformed to give the output trace time window. 
! 
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! FXDECON is a 2D process only.  For 3D work use FXYDECON.
!
! 
! Noise Attenuation and Parameter Choice
!
! Generally, fewer points in the filter design produce more noise attenuation 
! and worse data fidelity, while more points in the filter design produce less
! noise attenuation and better data fidelity.
!
! Fewer traces in the filter design produce less noise attenuation, while more 
! traces in the filter design produce more noise attenuation.  (Avoid setting
! the number of traces so large that curved events become smeared by the 
! linear prediction filter.)
!-------------------------------------------------------------------------------
!</advice_doc>
!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! Process is a multiple-trace process.
!
! This process requires traces to be input one at a time.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
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
! 2       Head mute index            Used
! 25      LAV                        Reset
! 64      Tail mute index            Used
!         HDR_LINE                   Used
!       
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author      Description
!     ----        ------      -----------
! 8.  2010-08-02  Stoeckley   Modify to allow multiple trace input.  This change
!                              means that the original oneset execution subroutine
!                              fxdecon was renamed fxdecon_single_trace, a new twosets
!                              subroutine was created which calls fxdecon_single_trace,
!                              and the restriction of the global NUMTR being 1 was
!                              removed.
! 7.  2010-01-12  Stoeckley   Add pc_error messages if not single trace input;
!                             add return statement in main execution routine
!                             if not single trace input.
! 6.  2009-11-18  Bill Menger Added test for divide by zero that was causing NaN's
!                             in the output on rare occasions.
! 5.  2006-01-03  S.Chiu      Fix problem when autocorrelation is zero
! 4.  2004-02-05  S.Chiu      Fix problem when the number of traces in a CMP 
!                             gather is less than NUM_TR.
! 3.  2002-05-07  S.Chiu      Add option of KEEP_LO_FREQ_DATA.
! 2.  2001-04-02  S.Chiu      Change wrapped_up to skip_wrapup.
! 1.  2000-11-29  S.Chiu      Initial version
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
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.     
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
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
!
!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS FXDECON Process/NC=80>
!
!        F-X trace interpolation for regularly sampled traces.
!
! HDR_LINE= `IIIIII       NUM_PTS= `IIIIIIII      NUM_TR= `IIIIIIII    
!
! WIN_LEN=~~`FFFFFFFF    FREQ_BEG= `FFFFFFFF    FREQ_END= `FFFFFFFF
!
! KEEP_LO_FREQ_DATA =`CCC
!
!</gui_def>
!
!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="HDR_LINE">
!<Tip> Header word defining input lines or other trace panels. </Tip>
! Default = 8
! Allowed = 1 - NWIH
! HDR_LINE should be a different whole number for each distinct input line or 
! other trace panel.
!</Help>
!
!<Help KEYWORD="NUM_PTS">
!<Tip> Number of points for the filter design. </Tip>
! Default = 5
! Allowed = odd int, 3 - 29
! Number of points for the prediction filter design.  Normally NUM_PTS should 
! be in the range of 3 - 11.
!</Help>
!
!<Help KEYWORD="NUM_TR">
!<Tip> Number of traces for the prediction filter. </Tip>
! Default = 21 
! Allowed = int > 6 - 200
! NUM_TR is normally 3 or 4 times NUM_PTS, and the minimum is
! NUM_TR must be > 2 times NUM_PTS. 
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length, in seconds, of the local time window. </Tip>
! Default = 0.6
! Allowed = real > 0.0
! Each trace group is broken into overlapping time windows of length WIN_LEN 
! seconds for F-X filtering.  It is not usually necessary to change the default.
!</Help>
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Minimum frequency for the F-X filter. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!</Help>
!
!<Help KEYWORD="FREQ_END">
!<Tip> Maximum frequency for the F-X filter. </Tip>
! Default = Nyquist
! Allowed = Nyquist >= real > FREQ_BEG
!</Help>
!
!<Help KEYWORD="KEEP_LO_FREQ_DATA">
!<Tip> Keep original data from frquency 0 to FREQ_BEG-FREQ_INC </Tip>
! Default = NO
! Allowed = YES/NO  
! If KEEP_LO_FREQ_DATA = YES, then no FXdecon noise attenuation applies 
! to data from frquency 0 to FREQ_BEG-FREQ_INC. This helps to keep 
! the low-frequency fault plane reflections. 
! FREQ_INC = Nyquist frequency /(length of the FFT/2+1) 
! If KEEP_LO_FREQ_DATA = NO, then FXdecon noise attenuation applies 
! to data from frquency 0 to FREQ_BEG-FREQ_INC.
!</Help>
!
!</HelpSection>

!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module fxdecon_module

      use singletracesupport_module
      use pc_module
      use named_constants_module
      use getlun_module
      use string_module
      use lav_module
      Use sort_module
      use fft_module
      use mutehw_module
      use gausselim_module

      implicit none
      private
      public :: fxdecon_create
      public :: fxdecon_initialize
      public :: fxdecon_update
      public :: fxdecon_delete
!<execute_only>
      public :: fxdecon            ! main execution (trace processing) routine.
      public :: fxdecon_wrapup
!</execute_only>


      character(len=100),public,save :: fxdecon_IDENT = &
       '$Id: fxdecon.f90,v 1.5 2006/01/03 12:36:22 S.Chiu prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
                                    

    type,public :: fxdecon_struct              
 
    private

    logical               :: skip_wrapup         ! wrapup flag.
!   logical               :: gathered           ! gathered flag.
 
    integer               :: hdr_line           ! Process parameters
    integer               :: num_pts            ! Process parameters
    integer               :: num_tr             ! Process parameters
    real                  :: freq_beg           ! Process parameters
    real                  :: freq_end           ! Process parameters
    real                  :: win_len            ! Process parameters
    logical               :: keep_lo_freq_data  ! Process parameters
   
    integer               :: ndpt               ! globals
    integer               :: nwih               ! globals
    real                  :: dt                 ! globals
    real                  :: tstrt              ! globals
    
    logical               :: ifirst             ! dependent variables
    logical               :: ilast              ! dependent variables
    logical               :: eodata             ! dependent variables
    integer               :: done               ! dependent variables
    integer               :: ieout              ! dependent variables
    integer               :: ifreq1             ! dependent variables
    integer               :: ifreq2             ! dependent variables
    integer               :: iout               ! dependent variables
    integer               :: ioinpt             ! dependent variables
    integer               :: isout              ! dependent variables
    integer               :: itrseq             ! dependent variables
    integer               :: hdval              ! dependent variables
    integer               :: hdval_old          ! dependent variables
    integer               :: neq                ! dependent variables
    integer               :: nfftp2             ! dependent variables
    integer               :: nhftp1             ! dependent variables
    integer               :: nout               ! dependent variables
    integer               :: ntr_panel          ! dependent variables
    integer               :: ntpts              ! dependent variables
    integer               :: nwin               ! dependent variables
    integer               :: nxstor             ! dependent variables
    integer               :: ntr_gather         ! dependent variables
    integer               :: nread              ! dependent variables
    integer               :: print_lun          ! dependent variables.
    real                  :: pwhite             ! dependent variables
   
    double precision, pointer      :: hd_save(:)    ! dependent variables
    double precision, pointer      :: hdstor(:,:)   ! dependent variables
    real,             pointer      :: tr_save(:)    ! dependent variables
    real,             pointer      :: trstor(:,:)   ! dependent variables
    integer,          pointer      :: indx_tr(:)    ! dependent variables
    integer,          pointer      :: iseq(:)       ! dependent variables
    real,             pointer      :: outtr(:,:)    ! dependent variables
    real,             pointer      :: outwt(:,:)    ! dependent variables
    real,             pointer      :: winwt(:)      ! dependent variables

    type(fft_struct),pointer :: fftrc_obj           ! dependent variables.
    type(fft_struct),pointer :: fftcr_obj           ! dependent variables.

    type(singletracesupport_struct),pointer :: sts

!   integer                     :: itr_in           ! for single trace I/O support.
!   integer                     :: itr_out          ! for single trace I/O support.
!   integer                     :: ntr_inout        ! for single trace I/O support.
!   double precision, pointer   :: hd_single(:,:)   ! for single trace I/O support.
!   real, pointer               :: tr_single(:,:)   ! for single trace I/O support.
!   logical                     :: finished         ! for single trace I/O support.

    end type fxdecon_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(fxdecon_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine fxdecon_create (obj)
      implicit none
      type(fxdecon_struct),pointer :: obj       ! arguments

      allocate (obj)

!    Nullify ALL POINTERS in your parameter structure 

      nullify  (obj%hd_save)
      nullify  (obj%hdstor)
      nullify  (obj%tr_save)
      nullify  (obj%trstor)
      nullify  (obj%indx_tr)
      nullify  (obj%iseq)
      nullify  (obj%outtr) 
      nullify  (obj%outwt) 
      nullify  (obj%winwt)
      nullify  (obj%fftrc_obj)
      nullify  (obj%fftcr_obj)
      nullify  (obj%sts)
!     nullify  (obj%hd_single)  ! for single trace I/O support.
!     nullify  (obj%tr_single)  ! for single trace I/O support.

      call fxdecon_initialize (obj)
      return
      end subroutine fxdecon_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine fxdecon_delete (obj)
      implicit none
      type(fxdecon_struct),pointer :: obj       ! arguments

!<execute_only>
      call fxdecon_wrapup (obj)
!</execute_only>

!! Make sure ALL POINTERS in parameter structure are deallocated

      if (associated(obj%hd_save)) deallocate (obj%hd_save)
      if (associated(obj%hdstor))  deallocate (obj%hdstor)
      if (associated(obj%tr_save)) deallocate (obj%tr_save)
      if (associated(obj%trstor))  deallocate (obj%trstor)
      if (associated(obj%indx_tr)) deallocate (obj%indx_tr)
      if (associated(obj%iseq))    deallocate (obj%iseq)
      if (associated(obj%outtr))   deallocate (obj%outtr)
      if (associated(obj%outwt))   deallocate (obj%outwt)
      if (associated(obj%winwt))   deallocate (obj%winwt)

      if (associated(obj%fftrc_obj))  call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj))  call fft_delete (obj%fftcr_obj)

!     if (associated(obj%tr_single)) deallocate (obj%tr_single)  ! for single trace I/O support.
!     if (associated(obj%hd_single)) deallocate (obj%hd_single)  ! for single trace I/O support.

      deallocate(obj)
      return
      end subroutine fxdecon_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine fxdecon_initialize (obj)
      implicit none
      type(fxdecon_struct),intent(inout) :: obj         ! arguments

      real                               :: rnyquist    ! local

!     Initialize ALL NON-POINTER VARIABLES in parameter structure

      call pc_get_global ('dt'    , obj%dt)    ! trace sample interval (s).
      rnyquist = (1.0/(2*obj%dt))
 
      obj%hdr_line  = 8
      obj%freq_beg  = 0.0
      obj%freq_end  = rnyquist
      obj%num_pts   = 5
      obj%num_tr    = 21 
      obj%win_len   = 0.6
      obj%keep_lo_freq_data = .false.

      obj%print_lun = pc_get_lun()
      call fxdecon_update (obj)
      return
      end subroutine fxdecon_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine fxdecon_update (obj)
      implicit none
      type(fxdecon_struct),intent(inout),target :: obj               ! arguments
   

!! Declare all required local variables as follows:

!     integer             :: numtr,ntapes,nscratch,nstore,ndisk      ! local
      integer             :: ntapes,nscratch,nstore,ndisk            ! local
      logical             :: iftd                                    ! local
      character(len=3)    :: need_label,need_request                 ! local
      character(len=3)    :: twosets                                 ! local    

      integer             :: nyquist, nfft                           ! local 
      integer             :: ier, len, i                             ! local
      real                :: tmid,  dist                             ! local
 

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


!     numtr = inil
!     call pc_get_global ('numtr'   ,numtr)        ! maximum number of traces.
!     call pc_get_global ('gathered',obj%gathered) ! whether properly gathered.
      call pc_get_global ('nwih'  , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'  , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('tstrt' , obj%tstrt) ! time of 1st trace sample (s).
      call pc_get_global ('dt'    , obj%dt)    ! trace sample interval (s).

!------------Check that globals are set:

!     if (numtr == inil) call pc_error ("NUMTR global hasn't been set.")
      if (obj%nwih == inil) call pc_error ("NWIH global hasn't been set.")
      if (obj%ndpt == inil) call pc_error ("NDPT global hasn't been set.")
      if (obj%tstrt == fnil) call pc_error ("TSTRT global hasn't been set.")
      if (obj%dt   == fnil) call pc_error ("DT global hasn't been set.")
!     if (numtr > 1)       call pc_error ('FXDECON requires single trace input.')  ! no longer a restriction.

      call pc_get ('HDR_LINE',  obj%hdr_line)
      call pc_get ('FREQ_BEG',  obj%freq_beg)
      call pc_get ('FREQ_END',  obj%freq_end)
      call pc_get ('NUM_PTS',   obj%num_pts)
      call pc_get ('NUM_TR',    obj%num_tr)
      call pc_get ('WIN_LEN',   obj%win_len)
      call pc_get ('KEEP_LO_FREQ_DATA', obj%keep_lo_freq_data)
 
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!


     nyquist = nint(1.0/(2*obj%dt))
     if (obj%hdr_line < 1 .or. obj%hdr_line > obj%nwih) then   
       call pc_error (' FXDECON error: HDR_LINE must be between 1 and ', &
                            obj%nwih)
     end if 
     
     if (obj%freq_beg < 0.0 .or. obj%freq_beg > nyquist) then   
       call pc_error (' FXDECON error: FREQ_BEG must be between 0 and ', &
                        nyquist)
     end if

     if (obj%freq_end < obj%freq_beg ) then
       call pc_error (' FXDECON error: FREQ_END must be >= FREQ_BEG ')
     end if 

     if (obj%freq_end > nyquist ) then
       call pc_error (' FXDECON error: FREQ_END must be <= Nyquist of ', &
                        nyquist)
     end if 

     if (obj%num_pts < 3 .or. obj%num_pts > 29 ) then   
        call pc_error (' FXDECON error: NUM_PTS must be between 3 and 29 ')
     end if 

     if (mod(obj%num_pts,2) == 0) then   
        call pc_error (' FXDECON error: NUM_PTS must be odd number ')
     end if 
 
     if (obj%num_tr < 6 .or. obj%num_tr > 200 ) then   
       call pc_error (' FXDECON error: NUM_TR must be between 6 and 200')
     end if 

     if(obj%num_pts*2 >= obj%num_tr) then
       call pc_error (' FXDECON error: NUM_TR must be > 2*NUM_PTS')
     end if

     if (obj%win_len <= 0.0 ) then   
       call pc_error (' FXDECON error: WIN_LEN must be > 0')
     end if 

     if (obj%win_len > (obj%ndpt-1)*obj%dt ) then   
       call pc_error (' FXDECON error: WIN_LEN must be <= trace length of ', &
                        (obj%ndpt-1)*obj%dt)
     end if

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


!     call pc_put_global ('numtr'   , 1)          ! now numtr can be > 0 and is not changed.
!     call pc_put_global ('gathered', .false. ) 

      call pc_put ('HDR_LINE',  obj%hdr_line)
      call pc_put ('FREQ_BEG',  obj%freq_beg)
      call pc_put ('FREQ_END',  obj%freq_end)
      call pc_put ('NUM_PTS',   obj%num_pts)
      call pc_put ('NUM_TR',    obj%num_tr)
      call pc_put ('WIN_LEN',   obj%win_len)
      call pc_put ('KEEP_LO_FREQ_DATA', obj%keep_lo_freq_data)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!! Conditionally deallocate all arrays 
  
      if (associated(obj%hd_save)) deallocate (obj%hd_save)
      if (associated(obj%hdstor))  deallocate (obj%hdstor)
      if (associated(obj%tr_save)) deallocate (obj%tr_save)
      if (associated(obj%trstor))  deallocate (obj%trstor)
      if (associated(obj%indx_tr)) deallocate (obj%indx_tr)
      if (associated(obj%iseq))    deallocate (obj%iseq)
      if (associated(obj%outtr))   deallocate (obj%outtr)
      if (associated(obj%outwt))   deallocate (obj%outwt)
      if (associated(obj%winwt))   deallocate (obj%winwt)

      if (associated(obj%fftrc_obj))  call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj))  call fft_delete (obj%fftcr_obj)

!     if (associated(obj%tr_single)) deallocate (obj%tr_single)  ! for single trace I/O support.
!     if (associated(obj%hd_single)) deallocate (obj%hd_single)  ! for single trace I/O support.

      call singletracesupport_delete (obj%sts)

!     obj%itr_in    = 0         ! for single trace I/O support.
!     obj%itr_out   = 0         ! for single trace I/O support.
!     obj%ntr_inout = 0         ! for single trace I/O support.
!     obj%finished  = .false.   ! for single trace I/O support.

!   obj%nout     --  number of output traces.
!   obj%isout    --  starting index of output traces.
!   obj%ieout    --  starting index of output traces.
!   obj%iout     --  current index of output traces.
!   obj%nxstor   --  number of stored traces in memory buffer.
!   obj%trstor   --  stored traces in memory buffer.
!   obj%hdstor   --  stored headers in memory buffer.
!   obj%indx_tr  --  sorted of sequential index of the circular
!   obj%outtr    --  output interpolated traces in memory buffer.
!   obj%outwt    --  weights of traces in memory buffer.
!   obj%pwhite   --  prewhitening.

!.... Preset some initial values

      obj%eodata = .false.
      obj%ifirst = .true.
      obj%ilast  = .false.
      obj%done = 0 
      obj%ioinpt = 1 
      obj%ntr_panel = 0 
      obj%itrseq = 0  
      obj%nread = obj%num_tr + obj%num_pts

      obj%pwhite = 0.01
      obj%iout = 0 
 
      obj%ntpts = min(obj%ndpt,obj%ntpts) 
 
      obj%nxstor = obj%num_tr + obj%num_pts*2  
      obj%ntpts = nint(obj%win_len/obj%dt + 0.499) 
      if (obj%ntpts*1.5 > obj%ndpt) obj%ntpts = obj%ndpt 
 
!..... fft in mixed radix
 
      nfft = fft_nfctr(obj%ntpts*6/5)  
      obj%nfftp2 = nfft + 2  
      obj%nhftp1 = obj%nfftp2/2 

      nyquist = nint(1./(2*obj%dt)) 
      obj%ifreq1 = nint(obj%freq_beg*obj%nfftp2/(2*nyquist)) 
      obj%ifreq2 = nint(obj%freq_end*obj%nfftp2/(2*nyquist)) 
      obj%ifreq1 = max(obj%ifreq1,1) 
      obj%ifreq2 = min(obj%ifreq2,obj%nfftp2/2) 
 
      nstore = (obj%nwih+1)*obj%nxstor + (4*obj%ndpt+3)*obj%nxstor 
      nscratch = 1          
 
      need_label   = 'YES'
      need_request = 'YES'
      twosets      = 'YES'     ! for single trace I/O support.
      iftd         = .false.
      ndisk        = 0
      ntapes       = 0 
               
      call pc_put_control ('nstore',             nstore)
      call pc_put_control ('nscratch',         nscratch)
      call pc_put_control ('need_label',     need_label)
      call pc_put_control ('need_request', need_request)
      call pc_put_control ('twosets',           twosets)


!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!! Allocate your permanent memory:

      allocate  (obj%hd_save(obj%nwih),             stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON hd_save array')
 
      allocate  (obj%hdstor(obj%nwih, obj%nxstor),  stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON hdstor array') 

      allocate  (obj%tr_save(obj%ndpt),             stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON tr_save array') 

      allocate  (obj%trstor(obj%ndpt, obj%nxstor),  stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON trstor array') 

      allocate  (obj%indx_tr(obj%nxstor),              stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON hd_save array') 

      allocate  (obj%iseq(obj%nxstor),                  stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON iseq array') 

      allocate  (obj%outtr(obj%ndpt, obj%nxstor ),     stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON outtr array') 
 
      allocate  (obj%outwt(obj%ndpt, obj%nxstor),      stat=ier) 
      if (ier/=0) call pc_error ('Error creating FXDECON outwt array') 

      allocate  (obj%winwt(obj%ntpts),  stat=ier)
      if (ier/=0) call pc_error ('Error creating FXDECON winwt array') 

      call singletracesupport_create (obj%sts, obj%nwih, obj%ndpt)

!     allocate (obj%hd_single(obj%nwih,1), stat=ier)   ! for single trace I/O support.
!     allocate (obj%tr_single(obj%ndpt,1), stat=ier)   ! for single trace I/O support.

!.... initialize fft tables.
      ier =  fft_create (obj%fftrc_obj,  1, nfft, 'rtoc')
      if (ier/=0) call pc_error ('Error creating rtoc FFT object')
      ier =  fft_create (obj%fftcr_obj, -1, nfft, 'ctor')
      if (ier/=0) call pc_error ('Error creating ctor FFT object')

!.... initialize the iseq array to increment by 1

      obj%iseq = (/(i, i=1,obj%nxstor)/) 
 
!..... compute number of overlapped windows.
 
       len = obj%win_len/obj%dt + 1
       obj%nwin = nint(1.*obj%ndpt/len + 0.4999)  
       obj%nwin = 2*obj%nwin -1 
       if (mod(obj%ndpt,len) <= nint(0.5*len)) then
         obj%nwin = obj%nwin -1
       end if

!
!....  compute the weights of the a time window.
! 
      tmid = 0.5*(1. + obj%ntpts)  
      dist  = 1./(1.01*(obj%ntpts - tmid))
      do i = 1, obj%ntpts 
        obj%winwt(i) = 1. - abs((i - tmid))*dist 
        obj%winwt(i) =  obj%winwt(i)* obj%winwt(i)
      end do 
 
      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine fxdecon_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!



subroutine fxdecon (obj, ntr, hdi, tri, hdo, tro)
    implicit none
    type(fxdecon_struct), intent(inout) :: obj
    integer         , intent(inout) :: ntr
    double precision, intent(inout) :: hdi(:,:)
    real            , intent(inout) :: tri(:,:)
    double precision, intent(out)   :: hdo(:,:)
    real            , intent(out)   :: tro(:,:)

!    if (ntr == NO_MORE_TRACES) then
!        continue  ! do nothing here.
!    else if (ntr == NEED_TRACES) then
!        if (obj%finished) then    ! the last gather has already been output.
!            ntr = NO_MORE_TRACES
!            return
!        endif
!        obj%itr_out = 0   ! need to start refilling the output gather.
!    else
!        obj%ntr_inout = ntr
!        obj%itr_in = 1
!        obj%hd_single(1:obj%nwih,1) = hdi(1:obj%nwih,obj%itr_in)
!        obj%tr_single(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%itr_in)
!        ntr = 1
!    endif

     if (singletracesupport_input(obj%sts, ntr, hdi, tri)) return

 22  call fxdecon_single_trace (obj, ntr, hdi, tri)

     if (singletracesupport_output(obj%sts, ntr, hdi, tri, hdo, tro)) go to 22

!22  call fxdecon_single_trace (obj, ntr, obj%hd_single, obj%tr_single)
!
!    if (ntr > 0) then
!        obj%itr_out = obj%itr_out + 1
!        hdo(1:obj%nwih,obj%itr_out) = obj%hd_single(1:obj%nwih,1)
!        tro(1:obj%ndpt,obj%itr_out) = obj%tr_single(1:obj%ndpt,1)
!        if (obj%itr_out == obj%ntr_inout) then
!            ntr = obj%ntr_inout    ! return a full output gather.
!            obj%itr_out = 0      ! will begin to refill the output gather.
!            return
!        endif
!        ntr = NEED_TRACES
!        goto 22
!    else if (ntr == FATAL_ERROR) then
!        return
!    else if (ntr == NO_MORE_TRACES) then  ! return what remains of the output gather, or 0.
!        if (obj%itr_out == 0) return   ! no more traces to output.
!        obj%finished = .true.
!        ntr = obj%itr_out    ! outputting whatever is in the last output gather ever.
!        return
!    else if (ntr == NEED_TRACES) then
!        if (obj%itr_in == obj%ntr_inout) return  ! go get more traces since the input gather is used up.
!        obj%itr_in = obj%itr_in + 1
!        obj%hd_single(1:obj%nwih,1) = hdi(1:obj%nwih,obj%itr_in)
!        obj%tr_single(1:obj%ndpt,1) = tri(1:obj%ndpt,obj%itr_in)
!        ntr = 1
!        goto 22
!    endif

end subroutine fxdecon


!!----------------------- fxdecon single trace -----------------------------!!
!!----------------------- fxdecon single trace -----------------------------!!
!!----------------------- fxdecon single trace -----------------------------!!


!! Upon input, NTR will have one of these values:
!!   NTR >= 1              means to process the input traces.
!!   NTR == NO_MORE_TRACES means there are no more imput traces.
!!   NTR == NEED_TRACES    means someone from below needs more traces.
!!   NTR == NEED_TRACES    might mean this is a trace-supplying process.
!!   NTR == NEED_TRACES    will not occur unless this process has a label.
!!
!! Upon output, NTR must have one of these values:
!!   NTR >= 1              if you are outputting traces.
!!   NTR == NO_MORE_TRACES if there are no more traces to output.
!!   NTR == FATAL_ERROR    if you have a fatal error.
!!   NTR == NEED_TRACES    if you need another trace before passing any out.
!!   NTR == NEED_TRACES    must not occur unless you specified that you
!!                           might need to request more traces.
!!
!<execute_only>


!! Need one set  of traces and headers, 

      subroutine fxdecon_single_trace (obj, ntr, hd, tr)
      implicit none
      type(fxdecon_struct),intent(inout) :: obj                ! arguments

      integer          ,intent(inout) :: ntr                   ! arguments
      double precision ,intent(inout) :: hd(:,:)               ! arguments
      real             ,intent(inout) :: tr(:,:)               ! arguments

      integer          ::  ipos, ierr, i, iadd                 ! local
        
      if  ( (ntr== NO_MORE_TRACES .and. obj%done==1 .and.       & 
             obj%iout>=obj%nout) .or. ntr == FATAL_ERROR )  then
         call fxdecon_wrapup (obj)
         return
      end if
 
!.... ioinpt = 1 -- input phase

      if (obj%ioinpt == 1) then 
          if (ntr > 1) then
             call pc_error ('FXDECON requires single trace input.')
             ntr = FATAL_ERROR
             return
          end if

!....     first trace in the gather
          if (obj%itrseq == 0) obj%hdval_old = nint(hd(obj%hdr_line,ntr))
              
          if (ntr /= 0) then 
            obj%hdval = nint(hd(obj%hdr_line,ntr))  
          else 
            obj%hdval = -99999 
          endif 
 
!..... gather a panel of traces.
 
          if (ntr/=0 .and. obj%hdval_old==obj%hdval) then 
            obj%ntr_panel = obj%ntr_panel + 1 
            obj%itrseq = obj%itrseq + 1 
            ipos = mod(obj%itrseq,obj%nxstor) 
            if (ipos == 0) ipos = obj%nxstor 
            obj%iseq(ipos) = obj%itrseq 

            !write(obj%print_lun,910) obj%itrseq,obj%ntr_panel,ipos,ntr, &
            !  obj%ioinpt,obj%nxstor,hd(7,ntr),hd(8,ntr)
  910       format('itrseq,ntr_panel,ipos,ntr,ioinpt,nxstor,hd7-8 ',6i7,2f9.1) 
 
             obj%hdstor(1:obj%nwih, ipos) = hd(1:obj%nwih,ntr) 
             obj%trstor(1:obj%ndpt, ipos) = tr(1:obj%ndpt,ntr)

            ierr  = 0
            if (obj%ntr_panel == obj%nread) then

!..... Apply FXdecon Filter.
 
              call fxdecon_group (obj, ierr)
              if ( ierr /= 0) then
                 ntr = FATAL_ERROR
                 write(obj%print_lun,*) ' FATAL_ERROR in fxdecon_group '
                 return
              end if

              obj%iout = 0 
              obj%ioinpt = 0 
              obj%ntr_panel = 0

              if ( obj%ifirst) then
                obj%isout = 1 
                obj%ieout = obj%num_tr
              else 
                obj%isout = 1 + obj%num_pts
                obj%ieout = obj%num_tr+ obj%num_pts
              end if 
              obj%nout = obj%ieout - obj%isout + 1
              obj%ifirst = .false.
              obj%nread = obj%num_tr
                
            else 
              obj%hdval_old = obj%hdval  
              ntr = NEED_TRACES
              return  
            endif 
 
          else 

!..... check the case if number of input traces <  num_tr in the 
!           same gather

            iadd  = 0

            if (obj%itrseq >= (obj%num_tr + obj%num_pts) ) then
              if (obj%ntr_panel <= 0) then
                if (obj%ifirst) then
                  return 
                else if (obj%itrseq <= obj%num_tr + obj%num_pts) then
                  obj%ntr_panel = obj%num_tr + obj%num_pts
                  iadd = obj%num_tr
                   obj%ifirst = .true.
                else 
                 obj%ntr_panel = obj%num_tr + 2*obj%num_pts
                 iadd = obj%num_tr+obj%num_pts
                 obj%ifirst = .true.
                end if
              end if
            end if              

            if (obj%hdval_old/=obj%hdval .and. ntr==0) obj%done = 1 
            if (ntr /= 0) then 
              obj%hd_save(1:obj%nwih) = hd(1:obj%nwih,ntr)
              obj%tr_save(1:obj%ndpt) = tr(1:obj%ndpt,ntr)
            endif 
 
!..... Apply FXdecon Filter if number of input traces > num_tr.
 
            ierr  = 0
            obj%ilast = .true. 
            if (obj%itrseq < (obj%num_tr + obj%num_pts) .and. obj%ifirst) then
              obj%outtr(1:obj%ndpt, 1:obj%ntr_panel) =      &
              obj%trstor(1:obj%ndpt, 1:obj%ntr_panel) 
              obj%outwt(1:obj%ndpt, 1:obj%ntr_panel) = 1.0 
              obj%indx_tr = (/(i, i=1,obj%nxstor)/) 
            else 

!.....         catch this special case

               if (obj%nxstor > obj%itrseq ) then
                 do ipos = obj%itrseq+1, obj%nxstor
                   obj%iseq(ipos) = ipos
                   obj%hdstor(1:obj%nwih, ipos) = 0.0 
                   obj%trstor(1:obj%ndpt, ipos) = 0.0 
                 end do
               end if
  
               call fxdecon_group (obj, ierr)
            endif 

            if ( ierr /= 0) then
              ntr = FATAL_ERROR
                 write(obj%print_lun,*) ' FATAL_ERROR after fxdecon_group'
              return
            end if 
            
            obj%iout = 0 
            obj%ioinpt = 0 

            if (obj%ifirst .and. obj%ilast) then        
              obj%isout = 1 + iadd
              obj%ieout = obj%ntr_panel 
            else 
              obj%isout = min(obj%itrseq,obj%nxstor) -    &
                          (obj%ntr_panel+obj%num_pts) + 1 
              obj%ieout = min(obj%itrseq,obj%nxstor) 
            end if
            obj%nout = obj%ieout - obj%isout + 1
            obj%ntr_panel = 0  
          endif 

      endif 
 
!..... ioinpt = 0 -- output filtered traces.
 
      if (obj%ioinpt == 0) then 
 
        if (obj%iout < obj%nout) then 
          ntr = 1 
  
          call fxdecon_out (obj, ntr, tr, hd)
 
          !write(obj%print_lun,935) hd(7,ntr), hd(3,ntr),hd(4,ntr),   &
          !                         obj%isout, obj%ieout
  935     format(' output cdp,isout,ieout ',3f8.1,2i6) 
 
          
        else 
 
!.... start a new line or gather

          if (obj%hdval_old /= obj%hdval) then 
 
           !write(obj%print_lun,*)' change of cdp line ',   &
           !     obj%hdval_old, obj%hdval
 
            obj%iseq = (/(i, i=1,obj%nxstor)/)
            obj%nread = obj%num_tr + obj%num_pts
            obj%ilast = .false.
            obj%ifirst = .true.

            obj%ntr_panel = 1 
            obj%itrseq = 1 
            ipos = mod(obj%itrseq,obj%nxstor) 
            if (ipos == 0) ipos = obj%nxstor 
            obj%iseq(ipos) = obj%itrseq 

            obj%hdstor(1:obj%nwih, ipos) = obj%hd_save(1:obj%nwih)
            obj%trstor(1:obj%ndpt, ipos) = obj%tr_save(1:obj%ndpt)
 
          endif 
 
          obj%ioinpt = 1 
          obj%nout = 0 
          obj%iout = 0 
 
          if (obj%done == 1) then 
            ntr = NO_MORE_TRACES 
            return  
          else 
            obj%hdval_old = obj%hdval 
            ntr = NEED_TRACES
            return  
          endif 
        endif 
      endif 
      !write(6,'(a,i3,a,i7,a,f14.7)')'RETURN fxdecon.f90:1055: ntr=',ntr,' hd(1) = ',nint(hd(1,ntr)),' hd(25)=',hd(25,ntr)
      return
      end subroutine fxdecon_single_trace

 
      subroutine fxdecon_group(obj, ierr) 

!  purpose:   Filter a group of input data.

      implicit none
      type(fxdecon_struct),intent(inout) :: obj          ! arguments
      integer, intent(inout)          :: ierr            ! arguments

      integer  :: nthpts, nfft                           ! local
      integer  :: ixst_in, ixend_in, itst, itend         ! local
      integer  :: ixst_out, ixend_out                    ! local
      integer  :: iw, ii, i, j                           ! local
      real     :: mute_min, rmute, rmsamp, scale         ! local

      real     :: rw1fft(obj%nfftp2,obj%nxstor)          ! local
      complex  :: cw1fft(obj%nhftp1,obj%nxstor)          ! local

      real     :: work1(max(obj%nfftp2,obj%nxstor))      ! local
      complex  :: cwork(max(obj%nfftp2,obj%nxstor))      ! local
      real     :: seq(obj%nxstor)                        ! local

      ierr = 0

      nfft  =  obj%nfftp2 - 2
      scale = 1.0/(nfft) 

      seq(1:obj%nxstor) = obj%iseq(1:obj%nxstor)
      obj%indx_tr = (/(j, j=1,obj%nxstor)/)
 
      call sort_qkisort (obj%nxstor, seq, obj%indx_tr)

!.... set the output weight and buffer to zero.
 
      obj%outtr = 0.0 
      obj%outwt = 0.0

!.... Load a group of traces into buffer

      if (obj%ifirst .and. obj%ilast) then 
        ixst_in = 1
        ixend_in = obj%ntr_panel
        ixst_out = 1
        ixend_out = obj%ntr_panel
      else 
        ixst_in = 1
        if ( obj%ifirst) then 
          ixend_in = obj%num_tr + obj%num_pts
          ixst_out = 1
          ixend_out = obj%num_tr
        else 
          ixend_in = obj%num_tr + 2*obj%num_pts
          ixst_out = obj%num_pts+1
          ixend_out = obj%num_pts+obj%num_tr
        end if
        if (obj%ilast) ixend_out = obj%num_tr + 2*obj%num_pts
      end if

      itst = 1 
      itend = obj%ntpts
      nthpts = obj%ntpts/2 
      mute_min = 9999 
 
      rmute = minval(obj%hdstor(2,obj%indx_tr(ixst_in:ixend_in)))
      mute_min = min(rmute, mute_min) 
 
!.... Perform filter in a time window

       do iw = 1, obj%nwin 
  
          if (mute_min <= itend) then  
          rmsamp = 0.

          do j = ixst_in, ixend_in  
            work1(1:itend-itst+1) = obj%trstor(itst:itend,obj%indx_tr(j)) 
            rmsamp = rmsamp +     &
                    sum(work1(1:itend-itst+1)*work1(1:itend-itst+1))
            rmsamp = sqrt(rmsamp)  
 
!....... pad zeros to the data before calling fft
 
            work1(obj%ntpts+1:obj%nfftp2) = 0. 

!.... compute fft of windowed data.
 
            call fft_rc_transform(obj%fftrc_obj, work1, cwork) 
            cw1fft(1:obj%nhftp1,j) = cwork(1:obj%nhftp1)
 
          end do 
 
          if (rmsamp <= 1.e-10) then
           !print*,'Skipping a group because rmsamp < 1.e-10)'
           !print*,'integer  :: ixst_in, ixend_in, itst, itend',ixst_in,ixend_in,itst,itend 
           !print*,'integer  :: ixst_out, ixend_out           ',ixst_out,ixend_out
           !obj%outtr(itst:itend,ixst_out:ixend_out) = 0.0
           !obj%outwt(itst:itend,ixst_out:ixend_out) = 1.0
           go to 100 
          endif

 
          call fxdecon_sol (obj, ixst_in, ixend_in, ixst_out, ixend_out, &
               cw1fft, ierr) 
          if ( ierr /= 0) then
              return
          end if
 
!... inverse FFT 
          do j = ixst_out, ixend_out

            call fft_cr_transform(obj%fftcr_obj, cw1fft(1,j), rw1fft(1,j), &
            scale)

            ii = 0 
            do i = itst, itend
              ii = ii + 1
              obj%outtr(i,j) = obj%outtr(i,j)+ rw1fft(ii,j)*obj%winwt(ii)
              obj%outwt(i,j) = obj%outwt(i,j)+ obj%winwt(ii)
            end do
          end do 
 
        endif 
  100   continue 
 
        itst = itst + nthpts 
        itend = itend + nthpts 
        if (itend <= obj%ndpt) cycle  
        itend = obj%ndpt 
        itst = obj%ndpt - obj%ntpts + 1 
      end do

      return  
      end subroutine fxdecon_group 

 
      subroutine fxdecon_out(obj, ntr, tr, hd) 
!
!  purpose:   output filtered traces.
!
!  input:     trstor    real    buffer of a group of input traces.
!             hdstor    dble    buffer of a group of input trace headers.

      implicit none
      type(fxdecon_struct),intent(inout) :: obj              ! arguments

      integer , intent(in)  :: ntr                           ! arguments
      real ,    intent(inout) :: tr(obj%ndpt,ntr)            ! arguments
      double precision , intent(inout) :: hd(obj%nwih,ntr)   ! arguments 
 
      integer :: jtr, jhd                                    ! local
      real , allocatable :: tmp1u(:)                         ! local
 

      jhd = obj%indx_tr(obj%isout+obj%iout)
      jtr = obj%isout + obj%iout  
      obj%iout = obj%iout + 1

      allocate (tmp1u(obj%ndpt)) 
      tmp1u = obj%outwt(:,jtr) 
      where (tmp1u /= 0.)  
        tr(:,ntr) = obj%outtr(:,jtr)/tmp1u 
      elsewhere 
        tr(:,ntr) = obj%outtr(:,jtr) 
      end where 

      deallocate (tmp1u) 
 
      hd(1:obj%nwih,ntr) = obj%hdstor(1:obj%nwih,jhd) 
 
!..... restore front and end mute

      call mutehw (hd, tr, obj%ndpt, 0.0, ikill = MUTEHW_BOTH)
      call lav_set_hdr (hd, tr, obj%ndpt,ntr)
    
      return  
      end subroutine fxdecon_out 


      subroutine fxdecon_sol(obj, ixst_in, ixend_in, ixst_out, ixend_out, &
            cw1fft, ierr)

!  purpose:   Compute qnd apply prediction filter for each frequency slice.

!
!  input:     ixst_in    int     starting location of input array.
!             ixend_in   int     ending location of input array.
!             ixst_out   int     starting location of output array.
!             ixend_out  int     ending location of output array.
!             cw1fft    cmplx    array containing fft of the window.

      implicit none

      type(fxdecon_struct),intent(inout) :: obj                 ! arguments
      integer, intent(in)          :: ixst_in, ixend_in         ! arguments
      integer, intent(in)          :: ixst_out, ixend_out       ! arguments
      integer, intent(inout)       :: ierr                      ! arguments
                 
      complex , intent(inout) :: cw1fft(obj%nhftp1,obj%nxstor)  ! arguments
                                        
      integer :: i, i1, i2, ix, kf                              ! local
      integer :: ndsign, ir1, ir2                               ! local
      integer :: nfirst_freq

      complex  :: cfilt(obj%num_pts)                            ! local
      complex  :: cfilt2(obj%num_pts*2+1)                       ! local
      complex  :: cdata1(obj%nxstor)                            ! local 
      complex  :: cdata2(obj%nxstor)                            ! local
      complex  :: cwork(2*obj%nxstor)                           ! local
      complex  :: cwork2(2*obj%nxstor)                          ! local

      real      :: taper_weight                                 ! local
      real      :: taper_len1, taper_len2 
       
!...... initialize parameters
 
      ierr = 0 
      ndsign = ixend_in-ixst_in+1 
       
      taper_len1 = obj%ifreq1 + 1
      taper_len2 = obj%nhftp1 - obj%ifreq2 + 1

      if (obj%keep_lo_freq_data) then
         nfirst_freq = obj%ifreq1
      else 
         nfirst_freq = 1
      end if 
      
      do kf = nfirst_freq, obj%nhftp1  
 
!     compute frequency taper weight

         taper_weight = 1.0
         if ( kf < obj%ifreq1) then
           taper_weight = kf /taper_len1
         else if ( kf  > obj%ifreq2 ) then
           taper_weight = (obj%nhftp1-kf)/taper_len2
         end if

         taper_weight = taper_weight*taper_weight
        cdata1(ixst_in:ixend_in) = cw1fft(kf,ixst_in:ixend_in)  
!
!   build complex prediction filter
!
        call fxdecon_wien (cdata1, ndsign, obj%pwhite, cwork2, cwork,  &
               obj%num_pts+1, ierr) 

        if ( ierr /= 0) then   ! reset the filter coeffs to zero
          cwork(1:obj%num_pts+1) = cmplx(0.0,0.0)
          ierr = 0       
           ! return            
        end if 
 
        cfilt(1:obj%num_pts) = -cwork(2:obj%num_pts+1)*cmplx(taper_weight,0.0) 
        cdata2 = 0.0 

!... correlate filter with input data

!...  handle beginning of the data
        if ( obj%ifirst .and. .not. obj%ilast ) then 

          cfilt2 = 0.0
          i1 = obj%num_pts + 2
          i2 = i1 + obj%num_pts -1
          cfilt2(i1:i2) = conjg(cfilt(:obj%num_pts))

          do i = 1, obj%num_pts
             do ix = -obj%num_pts, obj%num_pts
               if ((i+ix) < 1) cycle
               cdata2(i) = cdata2(i) + cdata1(i+ix)*(cfilt2(ix+obj%num_pts+1))
             end do
          end do
          ir1 = obj%num_pts + 1
          ir2 = ixend_out

        else if (obj%ilast .and. .not. obj%ifirst) then

!...  handle end of the data

          cfilt2 = 0.0
          cfilt2(1:obj%num_pts) = cfilt(obj%num_pts:1:(-1))

          do i = ixend_out-obj%num_pts+1, ixend_out 
             do ix = -obj%num_pts, obj%num_pts
               if ((i+ix) > ixend_out) cycle
               cdata2(i) = cdata2(i) + cdata1(i+ix)*(cfilt2(ix+obj%num_pts+1))
             end do
          end do 

          ir1 = 1
          ir2 = ixend_out - obj%num_pts

        else if (obj%ilast .and. obj%ifirst) then

!...  handle data of both edges

          cfilt2 = 0.0
          i1 = obj%num_pts + 2
          i2 = i1 + obj%num_pts -1
          cfilt2(i1:i2) = conjg(cfilt(:obj%num_pts))

          do i = 1, obj%num_pts
             do ix = -obj%num_pts, obj%num_pts
               if ((i+ix) < 1) cycle
               cdata2(i) = cdata2(i) + cdata1(i+ix)*(cfilt2(ix+obj%num_pts+1))
             end do
          end do
          ir1 = obj%num_pts + 1
      
          cfilt2 = 0.0
          cfilt2(1:obj%num_pts) = cfilt(obj%num_pts:1:(-1))

          do i = ixend_out-obj%num_pts+1, ixend_out 
             do ix = -obj%num_pts, obj%num_pts
               if ((i+ix) > ixend_out) cycle
               cdata2(i) = cdata2(i) + cdata1(i+ix)*(cfilt2(ix+obj%num_pts+1))
             end do
          end do 

          ir2 = ixend_out - obj%num_pts
 
        else

          ir1 = ixst_out
          ir2 = ixend_out

        end if

!...  handle data with no edge problem

          cfilt2(1:obj%num_pts) = cfilt(obj%num_pts:1:(-1))              
          i1 = obj%num_pts + 2
          i2 = i1 + obj%num_pts -1
          cfilt2(i1:i2) = conjg(cfilt(1:obj%num_pts))
          cfilt2(i1-1) = 0.0

          do i =ir1, ir2
             do ix = -obj%num_pts, obj%num_pts
               cdata2(i) = cdata2(i) + cdata1(i+ix)*cfilt2(ix+obj%num_pts+1)
             end do
             cdata2(i) = 0.5*cdata2(i)
          end do

         cw1fft(kf,ixst_out:ixend_out) = cdata2(ixst_out:ixend_out) 
           
      end do 
 
      return  
      end subroutine fxdecon_sol 

 
      subroutine fxdecon_matrx(a, b, nrows, ncols, ata, atb, pwhite) 
!
!  purpose:   compute complex correlation matrix (ata, atb)
!
!  input:
!             a          cx   input matrix with dimension nrows*ncols.
!             b          cx   input vector with dimension nrows.
!             (nrows*ncols)
!             nrows      int  row dimension of the input matrix.
!             ncols      int  col dimension of the input matrix.
!
!  output:
!             ata        cx   output matrix with dimension ncols*ncols.
!             atb        cx   output matrix with dimension 1*ncols.
!
!
      implicit none

      integer , intent(in)    :: nrows                      ! arguments
      integer , intent(in)    :: ncols                      ! arguments
      real ,    intent(in)    :: pwhite                     ! arguments
      complex , intent(in)    :: a(nrows,ncols)             ! arguments
      complex , intent(in)    :: b(nrows)                   ! arguments 
      complex , intent(inout) :: ata(ncols,ncols)           ! arguments 
      complex , intent(out)   :: atb(ncols)                 ! arguments


      integer :: ia                                         ! local 
      real :: dmax                                          ! local
      complex, dimension(ncols,nrows) :: at                 ! local
 
!.........  form a transpose a & b
 
      at = transpose(a) 
      at =  conjg(at)
      atb = matmul(at,b) 
      ata = matmul(at,a) 
  
!....  add prewhitening to the diagonal ata
 
      dmax = 0.0 
      do ia = 1, ncols 
        dmax = max(dmax,real(ata(ia,ia))) 
      end do 
      dmax = dmax*pwhite 
      do ia = 1, ncols 
        ata(ia,ia) = ata(ia,ia) + dmax 
      end do 
 
      return  
      end subroutine fxdecon_matrx 
    

      subroutine fxdecon_wien(cdata, ndata, pwhite, r, filt, n, ierr) 

!
!  purpose:   compute complex weiner filter.
!
!  input:     cdata    cmplx    input array of the data after fft.
!             ndata    int      number of sample in input array.
!             pwhite   real     prewhitening.
!             n        int      filter length in sample.
!             r        cmplx    working buffers.
!  output:
!             filt     cmplx    array containing inverse fft of the window.

      implicit none

      integer ,  intent(in)       :: ndata             ! arguments
      integer ,  intent(in)       :: n                 ! arguments
      real ,     intent(in)       :: pwhite            ! arguments
      complex ,  intent(in)       :: cdata(0:ndata-1)  ! arguments
      complex  , intent(inout)    :: r(n)              ! arguments
      complex ,  intent(inout)    :: filt(n)           ! arguments
      integer,   intent(inout)    :: ierr              ! arguments

      integer       :: i, j                            ! local
      real          :: xnorm                           ! local
      complex       :: a1, a2, c, e, v                 ! local
      complex       :: sum                             ! local

!...... compute autocorrelation.
 
      ierr = 0
 
      do i = 0, n - 1 
        sum = cmplx(0.0,0.0) 
        do j = 0, ndata - 1 - i 
          sum = sum + conjg(cdata((i+j)))*cdata(j) 
        end do 
        r(i+1) = sum 
      end do 
 
      r(1) = (1. + pwhite)*r(1) 
      xnorm = real(r(1)) 
      if( xnorm /= 0.0 ) then 
        r(:n) = r(:n)/xnorm 
      else           !-wmm added 11/18/09
        r(:n) = 0.0  !-wmm added 11/18/09
                     !print*,' Added r(:n) = 0.0 and xnorm=',xnorm
      endif          !-wmm added 11/18/09
 
      if (r(1) == 0.0) then
      !   write (pc_get_lun(), *)' fxdecon: autocorrelation is zero '
         ierr = -1
         return
      end if
 
!........ initialize a and v.
 
      filt(1) = cmplx(1.0,0.0) 
      do i = 2, n 
        filt(i) = cmplx(0.0,0.0) 
      end do 
      v = r(1) 
 
!........ compute wiener solution
 
      do i = 2, n 
        e = cmplx(0.0,0.0) 
        do j = 1, i - 1 
          e = e + conjg(r(i-j+1))*filt(j) 
        end do 
        if (real(cabs(e)) >= real(cabs(v))) then
            write (pc_get_lun(), *) ' fxdecon: wiener solution fails'
            ierr = -1
            return
        end if
 
!............ update filt and v.
 
        c = e/conjg(v) 
        do j = 1, (i + 1)/2 
          a1 = filt(j) 
          a2 = filt(i-j+1) 
          filt(j) = a1 - c*conjg(a2) 
          filt(i-j+1) = a2 - c*conjg(a1) 
        end do 
        v = v - conjg(e)*c 
      end do 
 
      return  
      end subroutine fxdecon_wien 

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine fxdecon_wrapup (obj)
      implicit none
      type(fxdecon_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call singletracesupport_delete (obj%sts)
      return
      end subroutine fxdecon_wrapup

!</execute_only>

!!
!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module fxdecon_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!


