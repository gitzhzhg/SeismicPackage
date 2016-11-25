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
! Name       : FILTP
! Category   : filters
! Written    : 1989-05-08   by: John Sinton
! Revised    : 2007-02-22   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Generate filter panels.
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
! FILTP produces filter panel diagnostics by generating a filtered version of 
! each input trace with each filter band specified.  It then calls BINSORT
! internally so that the filtered traces are output in functional panels, with 
! each panel being the input dataset filtered by one of the specified filter
! bands.  Filter panels are output with the filters in the same order as 
! specified by the parameters.  FILTP does all filtering in the frequency 
! domain.
!
! Output Header Words
! Each output panel is labeled sequentially by HDR_PANEL, the panel number.
! It is also labeled with the low frequency limit in HDR_SCRATCH_30 and the
! high frequency limit in HDR_SCRATCH_31 (for help in annotation).
! 
! FILTP does not alter the contents of HDR_TOP_MUTE or HDR_BOTTOM_MUTE, even
! though filter tails get added to the traces.
! 
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
! FILTP duplicates parameters found in TSVF, which is normally the filter 
! process used for production work.   
!
! Because FILTP is a frequency domain process and TSVF is a time domain 
! process, there may be a difference in their operation at very low 
! frequencies.   In the case of very low frequencies, it may be necessary to 
! use an operator in TSVF longer than the default in order to obtain similar 
! results from both processes.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! This process outputs one trace at a time
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.  
!
! This process outputs one trace at a time.
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
! GATHERED Key to whether traces are gathered    Changed to .false.
! NUMTR    Number of traces gathered together    Changed to 1
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
! 1       Sequential Trace Count     Renumbered.
! 3       Current gather             Reset
! 4       Number within gather       Reset
! 24      Panel number               Set
! 30      Low frequency limit        Set
! 31      High frequency limit       Set
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author     Description
!     ----       ------     -----------
! 14. 2007-02-22 Stoeckley  Allow number of header words to be > 64.
! 13. 2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
!012. 2006-01-10  B. Menger   Removed Unused Variables.
! 11. 2002-01-07 Stoeckley  Move trap subroutine to different location in code
!                            to make the intel compiler happy.
! 10. 2001-04-30 Stoeckley  Change wrapup flag.
!  9. 2000-11-17 Stoeckley  Divide output trace by scale factor to return
!                            the trace to its original amplitude range.
!  8. 2000-06-19 O'Brien    Replaced TSORT with BINSORT
!                           Allow multiple traces on input
!                           Changed to a ONESET process
!                           Reworked array traps
!  7. 2000-04-12 O'Brien    Change filter definitions to new convention
!                           Implemented EzGUI layout definitions
!                           Brought xml tags up to date
!                           Added RCS character ID variable
!  6. 1999-10-26 O'Brien    Full f90 conversion.
!  5. 1998-11-10 Vunderink  Begin using the f90 compiler.
!  4. 1991-12-17 Peterson   Increase filter arrays size from 15 to 24.
!  3. 1990-11-08 Ball       CFT77 change  to CALL TVFBPS (INDX,1)
!  2. 1989-05-31 Baumel     Fix so FTAPER and PHASE work properly.
!  1. 1989-05-08 Sinton     First version.
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
!
! This process uses a single set of trace and header arrays.
! 
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
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! FILTP uses BINSORT internally to sort the output to constant frequency-band
! panels.
!
! For clarity, to facilitate maintenance, and to maintain consistency with
! variable names in other filtering processes, the following user_parameter
! to program_variable_name translation is used:
!
!        USER_PARAMETER       PROGRAM_VARIABLE_NAME
!        ----------------     ---------------------
!        FREQ_LOW_NONE        f1
!        FREQ_LOW_FULL        f2
!        FREQ_HIGH_NONE       f3
!        FREQ_HIGH_FULL       f4
!        PHASE                ph
!        FILTER_TYPE          ftyp
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS FILTP Process/NC=80>
!
!             Generate filter panels.
!
! FILTER_TYPE `CCCCCCCCC
!
! FREQ_LOW_NONE FREQ_LOW_FULL FREQ_HIGH_FULL FREQ_HIGH_NONE PHASE 
! `FFFFFFFFFFFF `FFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFF `FFFFF
! `FFFFFFFFFFFF `FFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFF `FFFFF
! `FFFFFFFFFFFF `FFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFF `FFFFF
! `FFFFFFFFFFFF `FFFFFFFFFFFF `FFFFFFFFFFFFF `FFFFFFFFFFFF `FFFFF
!
!<PARMS FREQ_LOW_NONE_ARRAYSET[/XST/YST]>
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="FILTER_TYPE">
!<Tip> Type or filter to use. BANDPASS, HIGHPASS, LOWPASS, ... </Tip>
! Default = BANDPASS
! Allowed = character scalar
! Several filter types are available. They include:
!   NONE, BANDPASS, HIGHPASS, LOWPASS, ALLPASS, and BANDREJECT.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Low frequency limit where amp spectrum diminishes to 0.0, in Hz. </Tip>
! Default =  -
! Allowed = real array
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Low frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default =  -
! Allowed = real array
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> High frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default =  -
! Allowed = real array
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> High frequency limit where amp spectrum diminishes to 0.0, in Hz. </Tip>
! Default =  -
! Allowed = real array
!</Help>
!
!<Help KEYWORD="PHASE">
!<Tip> Array of filter band phase, in degrees. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module filtp_module

      use fft_module
      use bandps_module
      use pc_module
      use named_constants_module
      use binsort_module
      use sizeof_module
      use lav_module

      implicit none

      private
      public :: filtp_create     ! uses the parameter cache.
      public :: filtp_initialize
      public :: filtp_update     ! uses the parameter cache.
      public :: filtp_delete

!<execute_only>

      public :: filtp            ! main execution (trace processing) routine.
      public :: filtp_wrapup

!</execute_only>

      character(len=100),public,save :: FILTP_IDENT = &
             '$Id: filtp.f90,v 1.14 2007/02/23 14:19:24 Stoeckley beta sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: filtp_struct

        private

        logical              :: skip_wrapup       ! wrapup flag

        character(len=10)    :: ftyp              ! process parameters
        real,pointer         :: f1(:)             ! process parameters
        real,pointer         :: f2(:)             ! process parameters
        real,pointer         :: f3(:)             ! process parameters
        real,pointer         :: f4(:)             ! process parameters
        real,pointer         :: ph(:)             ! process parameters

        integer              :: nwih,ndpt         ! globals.
        real                 :: dt

        integer                 :: prtlu          ! dependent variables
        integer                 :: ntr_in         ! ntr on input to filtp()
        integer                 :: npo2           ! Power of 2 used for ffts
        integer                 :: nfilt          ! Number of filters to apply
        integer                 :: nlist          ! length of linked lists
        integer                 :: size_f1        ! dependent variables
        integer                 :: size_f2        ! dependent variables
        integer                 :: size_f3        ! dependent variables
        integer                 :: size_f4        ! dependent variables
        integer                 :: size_ph        ! dependent variables
        integer                 :: nw             ! NumSmp in freq domain
        real                    :: dw             ! SmpInt in freq domain (Hz)
        real                    :: fnyq           ! Nyquist frequency
        complex,pointer         :: filters(:,:)   ! Array of complex filters
        real,pointer            :: rtrace(:)      ! Real trace
        complex,pointer         :: ctrace(:)      ! Complex trace
        complex,pointer         :: fctrace(:)     ! Filtered complex trace
        real,pointer            :: frtrace(:,:)   ! nfilt filtered real traces
        double precision,pointer:: hdr(:,:)       ! nfilt filtered trace hdrs
        type(fft_struct),pointer:: rcfft          ! Forward fft object
        type(fft_struct),pointer:: crfft          ! Inverse fft object
        type(binsort_struct),pointer:: binsort    ! Structure for BINSORT
        logical                 :: more_input     ! Flag for input expectation

      end type filtp_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(filtp_struct),pointer,save :: object      ! needed for traps.

      integer,parameter              :: ftyp_nopt=6     ! used in setup
      character(len=10),save         :: ftyp_options(6) ! used in traps

      data ftyp_options  /'NONE','BANDPASS','HIGHPASS','LOWPASS', &
                          'ALLPASS','BANDREJECT'/

      contains

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
   
      subroutine filtp_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword           ! arguments

! Variables for arrayset test
      integer            :: ifilt, jfilt
      integer            :: result
      real               :: nyq
      real               :: tmp_f1, tmp_f2, tmp_f3, tmp_f4, tmp_ph
      character(len=98)  :: message

!--------------------------------------------------------
   
      keyword_select: select case (keyword)

        case ('FILTER_TYPE') keyword_select
          call string_to_upper(object%ftyp)

          if ( all(ftyp_options/=object%ftyp)) then
            call pc_warning &
                ('Invalid value for FILTER_TYPE: '//object%ftyp//'. &
                 &Resetting to default: BANDPASS.')
            object%ftyp = 'BANDPASS'
          endif


        case ('FREQ_LOW_NONE_ARRAYSET') keyword_select
          ! frequencies must be between 0.0 and Nyquist.
          nyq = object%fnyq    ! used for code readability
          where (object%f1 /= FNIL .and. object%f1 < 0.0) object%f1 = 0.0
          where (object%f1 /= FNIL .and. object%f1 > nyq) object%f1 = nyq
          where (object%f2 /= FNIL .and. object%f2 < 0.0) object%f2 = 0.0
          where (object%f2 /= FNIL .and. object%f2 > nyq) object%f2 = nyq
          where (object%f3 /= FNIL .and. object%f3 < 0.0) object%f3 = 0.0
          where (object%f3 /= FNIL .and. object%f3 > nyq) object%f3 = nyq
          where (object%f4 /= FNIL .and. object%f4 < 0.0) object%f4 = 0.0
          where (object%f4 /= FNIL .and. object%f4 > nyq) object%f4 = nyq

          ! Loop over filters to verify bandpass parameters
          do ifilt = 1,object%nfilt

            ! bandps_check() has an unfriendly way of clearing out values
            ! it thinks are inappropriate or unnecessary. This is fine on
            ! the back end, but for GUI users it can be distressing. For
            ! this reason, the users' frequency params are copied to local
            ! variables before the call to bandps_check.

            tmp_f1 = object%f1(ifilt)
            tmp_f2 = object%f2(ifilt)
            tmp_f3 = object%f3(ifilt)
            tmp_f4 = object%f4(ifilt)
            tmp_ph = object%ph(ifilt)

            call bandps_check &
                  (result, message, object%fnyq, object%ftyp,  &
                   tmp_f1, tmp_f2, tmp_f3, tmp_f4, tmp_ph)

            message = trim(adjustl(message))//' At filter number '
            select case (result)
              case (BANDPS_INFO);     call pc_info(message,ifilt)
              case (BANDPS_ERROR);    call pc_error(message,ifilt)
              case (BANDPS_ENDERROR); call pc_error(message,ifilt)
            end select

          enddo


        case ('FIND_NULL') keyword_select

          ! Find list indices where the filter is completely absent and remove
          ifilt = 0
          do
            ifilt = ifilt + 1
            if ( ifilt > object%nfilt ) exit
            if ( object%f1(ifilt) == FNIL .and. &
                 object%f2(ifilt) == FNIL .and. &
                 object%f3(ifilt) == FNIL .and. &
                 object%f4(ifilt) == FNIL .and. & 
                 object%ph(ifilt) == FNIL ) then
              do jfilt = ifilt,object%nfilt-1
               object%f1(jfilt) = object%f1(jfilt+1)
               object%f2(jfilt) = object%f2(jfilt+1)
               object%f3(jfilt) = object%f3(jfilt+1)
               object%f4(jfilt) = object%f4(jfilt+1)
               object%ph(jfilt) = object%ph(jfilt+1)
              enddo
              object%nfilt = object%nfilt - 1
              ifilt = ifilt - 1
            endif
            cycle
          enddo
          object%size_f1 = object%nfilt
          object%size_f2 = object%nfilt
          object%size_f3 = object%nfilt
          object%size_f4 = object%nfilt
          object%size_ph = object%nfilt

        case ('FILTP_CHECK') keyword_select

          ! Loop over filters to verify bandpass parameters
          ! In this case change the users' input if warranted
          ! Messages should have already been issued so omit those

          do ifilt = 1,object%nfilt
            call bandps_check &
                  (result, message, object%fnyq, object%ftyp,            &
                   object%f1(ifilt), object%f2(ifilt), object%f3(ifilt), &
                   object%f4(ifilt), object%ph(ifilt))
          enddo

      end select keyword_select

      return
      end subroutine filtp_trap


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine filtp_create (obj)
      implicit none
      type(filtp_struct),pointer :: obj       ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers
      nullify (obj%f1)
      nullify (obj%f2)
      nullify (obj%f3)
      nullify (obj%f4)
      nullify (obj%ph)
      nullify (obj%filters)
      nullify (obj%rtrace)
      nullify (obj%ctrace)
      nullify (obj%fctrace)
      nullify (obj%frtrace)
      nullify (obj%hdr)
      nullify (obj%rcfft)
      nullify (obj%crfft)
      nullify (obj%binsort)
   
      call filtp_initialize (obj)

      return
      end subroutine filtp_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine filtp_delete (obj)
      implicit none
      type(filtp_struct),pointer :: obj       ! arguments

!<execute_only>
      call filtp_wrapup (obj)
!</execute_only>

      if (associated(obj%f1))       deallocate (obj%f1)
      if (associated(obj%f2))       deallocate (obj%f2)
      if (associated(obj%f3))       deallocate (obj%f3)
      if (associated(obj%f4))       deallocate (obj%f4)
      if (associated(obj%ph))       deallocate (obj%ph)
      if (associated(obj%filters))  deallocate (obj%filters)
      if (associated(obj%rtrace))   deallocate (obj%rtrace)
      if (associated(obj%ctrace))   deallocate (obj%ctrace)
      if (associated(obj%fctrace))  deallocate (obj%fctrace)
      if (associated(obj%frtrace))  deallocate (obj%frtrace)
      if (associated(obj%hdr))      deallocate (obj%hdr)
      if (associated(obj%rcfft))    call fft_delete (obj%rcfft)
      if (associated(obj%crfft))    call fft_delete (obj%crfft)
      if (associated(obj%binsort))  call binsort_close (obj%binsort)

      deallocate(obj)

      return
      end subroutine filtp_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine filtp_initialize (obj)
      implicit none
      type(filtp_struct),pointer :: obj       ! arguments

! Get global values in initialization
      obj%dt = FNIL
      call pc_get_global ('DT',  obj%dt  )
      if ( obj%dt == FNIL ) then
        call pc_error('FILTP: Global parameter DT has not been properly set.')
        return
      endif

! Initialize process parameters
      obj%fnyq = 0.5/obj%dt

! Initialize all the dependent variables
      obj%prtlu   = 0
      obj%npo2    = 0
      obj%nfilt   = 0
      obj%nlist   = 0
      obj%size_f1 = 0
      obj%size_f2 = 0
      obj%size_f3 = 0
      obj%size_f4 = 0
      obj%size_ph = 0
      obj%nw      = 0
      obj%dw      = 0.0

      obj%more_input = .true.

      call filtp_update (obj)

      return
      end subroutine filtp_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine filtp_update (obj)
      implicit none
      type(filtp_struct),target :: obj               ! Arguments

      integer     ::   nstore       ! Local
      integer     :: ier1,ier2,ier3,ier4,ier5,ier6  ! Local
      integer     :: update_state                   ! Local
      integer     :: ifilt, ier                     ! Local

      integer   :: SIZEOF_REAL
      integer   :: SIZEOF_DOUBLE
      integer   :: SIZEOF_COMPLEX

      SIZEOF_REAL    = sizeof(1.0)
      SIZEOF_DOUBLE  = sizeof(1.0d0)
      SIZEOF_COMPLEX = sizeof(cmplx(1.0,1.0))

      object => obj              ! needed for traps.
      obj%skip_wrapup = .true.

      update_state = pc_get_update_state()

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("freq_low_none_arrayset", (/  &
                                    "freq_low_none ",             &
                                    "freq_low_full ",             &
                                    "freq_high_full",             &
                                    "freq_high_none",             &
                                    "phase         " /))

! First retrieve globals
      call pc_get_global ('NDPT'    , obj%ndpt)
      call pc_get_global ('NWIH'    , obj%nwih)
      call pc_get_global ('DT'      , obj%dt  )

      if ( obj%ndpt <= 0 ) then
        call pc_error('FILTP: Global parameter NDPT<=0. Nothing to filter.')
      endif
  !   if ( obj%nwih /= HDR_NOMINAL_SIZE ) then
  !     call pc_error('FILTP: Global parameter NWIH /= HDR_NOMINAL_SIZE.')
  !   endif
      if ( obj%dt <= 0.0 ) then
        call pc_error('FILTP: Global parameter DT <= 0.0.')
      endif

! Now retrieve user paramerters
      call pc_get ('FILTER_TYPE', obj%ftyp, filtp_trap)

      call pc_alloc ('FREQ_LOW_NONE' , obj%f1 , obj%size_f1)
      call pc_alloc ('FREQ_LOW_FULL' , obj%f2 , obj%size_f2)
      call pc_alloc ('FREQ_HIGH_FULL', obj%f3 , obj%size_f3)
      call pc_alloc ('FREQ_HIGH_NONE', obj%f4 , obj%size_f4)
      call pc_alloc ('PHASE'         , obj%ph , obj%size_ph)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! Lists of filter parameters should be consistent because when accessed
! from the front end, users can only alter values up to the known size of
! eack list
      call filtp_list_consistency (obj)

! Establish nfilt
      obj%nfilt = obj%nlist

! Run the traps needed when GUI users assert the parameters are "OK".
      if ( pc_verify_end() ) then
        ! Search and destroy NULL filters
        call filtp_trap ('FIND_NULL')
        ! Check frequency parameters without alteration
        call filtp_trap('FREQ_LOW_NONE_ARRAYSET')
        if ( .not. pc_do_not_process_traces() ) then
          ! Check frequency parameters _with_ alteration
          call filtp_trap ('FILTP_CHECK')
        endif
      endif

! Verify parameters in the linked lists
      if ( update_state == PC_BACKEND .or. &
           update_state == PC_EXECUTE ) then
        call filtp_verify_linked_lists (obj)
      endif

! Get the fft size
      obj%npo2 = 8
      do while ( obj%npo2 < obj%ndpt )
        obj%npo2 = obj%npo2*2
      enddo

! Set the nw and dw for frequency domain filters.
      obj%nw   = obj%npo2/2 + 1
      obj%fnyq = 0.5/obj%dt
      obj%dw   = obj%fnyq/(obj%nw-1)

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field &
             ('FILTER_TYPE', ftyp_options, ftyp_nopt)

      call pc_put ('FILTER_TYPE', obj%ftyp)

      call pc_put ('FREQ_LOW_NONE' , obj%f1, obj%size_f1)
      call pc_put ('FREQ_LOW_FULL' , obj%f2, obj%size_f2)
      call pc_put ('FREQ_HIGH_FULL', obj%f3, obj%size_f3)
      call pc_put ('FREQ_HIGH_NONE', obj%f4, obj%size_f4)
      call pc_put ('PHASE'         , obj%ph, obj%size_ph)

!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!
!!------------------------- end session trap -------------------------------!!

      nstore = obj%nw * (obj%nfilt+2) * SIZEOF_COMPLEX
      nstore = nstore + (obj%npo2+2) * SIZEOF_REAL
      nstore = nstore + obj%nfilt * obj%ndpt * SIZEOF_REAL
      nstore = nstore + obj%nfilt * obj%nwih * SIZEOF_DOUBLE
      nstore = nstore + fft_mem_usage(obj%npo2,1) ! second argument not
      nstore = nstore + fft_mem_usage(obj%npo2,1) ! used in fxn.

      call pc_put_control ('NSTORE'       , nstore)
      call pc_put_control ('NEED_LABEL'   , 'YES' )
      call pc_put_control ('NEED_REQUEST' , 'YES' )

      call pc_put_global ( 'GATHERED' , .false. )
      call pc_put_global ( 'NUMTR'    ,    1    )

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.

! Get an i/o unit for printing
      obj%prtlu = pc_get_lun()

! Create forward and reverse fft objects
      ier = fft_create (obj%rcfft,-1,obj%npo2,'rtoc')
      ier = fft_create (obj%crfft, 1,obj%npo2,'ctor')

! Set up binsort
      if (associated(obj%binsort))    call binsort_close (obj%binsort)

      call binsort_open (obj%binsort, obj%nfilt, 'FILPT_TMP', &
                         obj%nwih, obj%ndpt, obj%prtlu, ier1)
      if ( ier1 == BINSORT_ERROR ) then
        call pc_error('FILTP: Failure trying to open the BINSORT object.')
        return
      endif

! Allocate the traces and arrays carried by filtp_struct
      allocate (obj%filters(obj%nw,obj%nfilt)  , stat= ier1 )
      allocate (obj%rtrace(obj%npo2+2)         , stat= ier2 )
      allocate (obj%ctrace(obj%nw)             , stat= ier3 )
      allocate (obj%fctrace(obj%nw)            , stat= ier4 )
      allocate (obj%frtrace(obj%ndpt,obj%nfilt), stat= ier5 )
      allocate (obj%hdr(obj%nwih,obj%nfilt)    , stat= ier6 )

      if (ier1/=0 .or. ier2/=0 .or. ier3/=0 .or. &
          ier4/=0 .or. ier5/=0 .or. ier6/=0) then
        call pc_error('FILTP: Unable to allocate space for filters &
                      &and workspace ',nstore,' bytes.')
        return
      endif

! Generate precomputed filters
      do ifilt = 1, obj%nfilt
        call bandps (obj%filters(:,ifilt), obj%nw, obj%dw, obj%ftyp, &
                     obj%f1(ifilt), obj%f2(ifilt), obj%f3(ifilt),    &
                     obj%f4(ifilt), obj%ph(ifilt))
      enddo

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine filtp_update


!*******************************************************************************
! Subroutine to ensure consistent filter parameter list length
!*******************************************************************************
      subroutine filtp_list_consistency (obj)

      implicit none

      type(filtp_struct) :: obj
      real, allocatable :: rtemp(:)
!------------------------------------------------------------------------------
      obj%nlist = max(obj%size_f1, obj%size_f2, obj%size_f3, obj%size_f4, &
                      obj%size_ph)

      allocate(rtemp(obj%nlist))

      if ( obj%size_f1 < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%f1)) then
          rtemp(1:obj%size_f1) = obj%f1(1:obj%size_f1)
          deallocate(obj%f1)
        endif
        allocate(obj%f1(obj%nlist))
        obj%f1(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%size_f1 = obj%nlist
      endif

      if ( obj%size_f2 < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%f2)) then
          rtemp(1:obj%size_f2) = obj%f2(1:obj%size_f2)
          deallocate(obj%f2)
        endif
        allocate(obj%f2(obj%nlist))
        obj%f2(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%size_f2 = obj%nlist
      endif

      if ( obj%size_f3 < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%f3)) then
          rtemp(1:obj%size_f3) = obj%f3(1:obj%size_f3)
          deallocate(obj%f3)
        endif
        allocate(obj%f3(obj%nlist))
        obj%f3(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%size_f3 = obj%nlist
      endif

      if ( obj%size_f4 < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%f4)) then
          rtemp(1:obj%size_f4) = obj%f4(1:obj%size_f4)
          deallocate(obj%f4)
        endif
        allocate(obj%f4(obj%nlist))
        obj%f4(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%size_f4 = obj%nlist
      endif

      if ( obj%size_ph < obj%nlist ) then
        rtemp(:) = FNIL
        if (associated(obj%ph)) then
          rtemp(1:obj%size_ph) = obj%ph(1:obj%size_ph)
          deallocate(obj%ph)
        endif
        allocate(obj%ph(obj%nlist))
        obj%ph(1:obj%nlist) = rtemp(1:obj%nlist)
        obj%size_ph = obj%nlist
      endif

      deallocate(rtemp)

      return
      end subroutine filtp_list_consistency


!*******************************************************************************
! Subroutine to convert linked list info to 3D arrays of filter parameters
! (Only called from backend)
!*******************************************************************************
      subroutine filtp_verify_linked_lists (obj)

      implicit none

      type(filtp_struct) :: obj

      integer            :: ier=0
!------------------------------------------------------------------------------
      obj%nfilt = max(obj%size_f1, obj%size_f2, obj%size_f3, obj%size_f4, &
                      obj%size_ph)

! Verify the existence of parameter lists
      if (obj%nfilt == 0) then
        call pc_error("FILTP: No filter parameters are present. FATAL.")
        ier=-1
      endif

! Check the lists for length consistency
      if (obj%size_f1 /= 0 .and. obj%size_f1 /= obj%nfilt) then
        call pc_error('FILTP: Filter parameter FREQ_LOW_NONE has an &
                      &inconsistent list length')
        ier=-1
      endif

      if (obj%size_f2 /= 0 .and. obj%size_f2 /= obj%nfilt) then
        call pc_error('FILTP: Filter parameter FREQ_LOW_FULL has an &
                      &inconsistent list length')
        ier=-1
      endif

      if (obj%size_f3 /= 0 .and. obj%size_f3 /= obj%nfilt) then
        call pc_error('FILTP: Filter parameter FREQ_HIGH_FULL has an &
                      &inconsistent list length')
        ier=-1
      endif

      if (obj%size_f4 /= 0 .and. obj%size_f4 /= obj%nfilt) then
        call pc_error('FILTP: Filter parameter FREQ_HIGH_NONE has an &
                      &inconsistent list length')
        ier=-1
      endif

      if (obj%size_ph /= 0 .and. obj%size_ph /= obj%nfilt) then
        call pc_error('FILTP: Filter parameter PHASE has an &
                      &inconsistent list length')
        ier=-1
      endif

! Now check for the existence of various parameters and notify the user
! if something is missing.
      if(obj%size_f1 == 0) then
        call pc_error('FILTP: FREQ_LOW_NONE is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_f2 == 0) then
        call pc_error('FILTP: FREQ_LOW_FULL is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_f3 == 0) then
        call pc_error('FILTP: FREQ_HIGH_FULL is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_f4 == 0) then
        call pc_error('FILTP: FREQ_HIGH_NONE is missing and must be supplied')
        ier=-1
      endif

      if(obj%size_ph == 0) then
        call pc_error('FILTP: PHASE is missing and must be supplied')
        ier=-1
      endif

! Finally, all of the filters need to be verified
      call filtp_verify_filter_parms(obj)

! All done
      return
      end subroutine filtp_verify_linked_lists


!*******************************************************************************
! Verify validity of filters parameters
! (Only called from backend)
!*******************************************************************************
      subroutine filtp_verify_filter_parms (obj)

      implicit none

      type(filtp_struct)  :: obj               ! Arguments

      integer            :: ifilt             ! Local
      integer            :: result            ! Local
      character(len=98)  :: message           ! Local
!---------------------------------------

! Loop over the number of filters
      do ifilt = 1,obj%nfilt

! Pass this filter into bandps_check
        call bandps_check &
              (result,message,obj%fnyq,obj%ftyp, &
               obj%f1(ifilt),obj%f2(ifilt), obj%f3(ifilt),obj%f4(ifilt), &
               obj%ph(ifilt))

! Finally, evaluate the result from bandps_check and act accordingly
        message = trim(adjustl(message))//' At filter number '
        select case (result)
          case (BANDPS_INFO);     call pc_info(message,ifilt)
          case (BANDPS_ERROR);    call pc_error(message,ifilt)
          case (BANDPS_ENDERROR); call pc_error(message,ifilt)
        end select

      enddo

      return
      end subroutine filtp_verify_filter_parms



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine filtp (obj,ntr,hd,tr)
      implicit none
      type(filtp_struct)              :: obj                ! arguments
      integer,         intent(inout)  :: ntr                ! arguments
      double precision,intent(inout)  :: hd(:,:)            ! arguments
      real,            intent(inout)  :: tr(:,:)            ! arguments

      integer                     :: itr,ifilt              ! local
      real                        :: freq_beg, freq_end     ! local
!----------------------------------------------------------------

! Keep track of ntr on input... BINSORT will change the value
      obj%ntr_in = ntr

! The first encounter with NO_MORE_TRACES is handled differently then all
! ensuing encounters, BINSORT requires one instance of NO_MORE_TRACES to
! switch from trace collecting mode to trace releasing mode.
      if ( ntr == NO_MORE_TRACES .and. obj%more_input ) then
        obj%more_input = .false.
        call binsort ( obj%binsort, ntr, hd, tr )
        if (ntr == FATAL_ERROR) then
          write(obj%prtlu,*) 'FILTP: Fatal error in BINSORT'
          call filtp_wrapup (obj)
        endif
        return
      endif

! If there are no more input traces, we still need to flush BINSORTs tmp space
      if (.not.obj%more_input) then
        ntr = NEED_TRACES
        call binsort ( obj%binsort, ntr, hd, tr )
        if (ntr == FATAL_ERROR) then
          write(obj%prtlu,*) 'FILTP: Fatal error in BINSORT'
          call filtp_wrapup (obj)
        endif
        return
      endif

! If ntr=NEED_TRACES and more_input=.true., send the NEED_TRACES request
! back to main.
      if (obj%more_input .and. ntr==NEED_TRACES) return

! If we made it this far, we need to filter traces.
! Loop over input traces
      do itr = 1,obj%ntr_in

! Fourier transform the trace to the frequency domain
        obj%rtrace(:obj%ndpt) = tr(:obj%ndpt,1)
        obj%rtrace(obj%ndpt+1:obj%npo2) = 0.0
        call fft_rc_transform(obj%rcfft,obj%rtrace,obj%ctrace)

! Do a blanket copy of input trace headers to the obj%hdr()
        do ifilt = 1,obj%nfilt
          obj%hdr(:,ifilt) = hd(:,1)
        enddo

! Initialize the filter counter
        ifilt = 0

! Loop over filters
        do

          ! Increment the filter counter and test for exit condition
          ifilt = ifilt + 1
          if (ifilt > obj%nfilt) exit

          ! Filter the trace and transform to time domain
          obj%fctrace(:) = obj%ctrace(:) * obj%filters(:,ifilt)
          call fft_cr_transform(obj%crfft,obj%fctrace,obj%rtrace)

          ! Divide by scale factor to get back to original trace amplitudes
          obj%rtrace(:) = obj%rtrace(:) / obj%npo2

          ! Copy the trace to a filtered trace buffer and adjust a few headers
          obj%frtrace(:obj%ndpt,ifilt) = obj%rtrace(:obj%ndpt)
          freq_beg = (obj%f1(ifilt)+obj%f2(ifilt))/2.0
          freq_end = (obj%f3(ifilt)+obj%f4(ifilt))/2.0
          obj%hdr(HDR_PANEL,ifilt) = ifilt
          obj%hdr(HDR_SCRATCH_30,ifilt) = max(0.0,freq_beg)
          obj%hdr(HDR_SCRATCH_31,ifilt) = min(obj%fnyq,freq_end)
          obj%hdr(HDR_LAV,ifilt) = lav (obj%frtrace(:,ifilt), obj%ndpt)

          cycle

        enddo

        ! sort everything on HDR_PANEL
        ntr = obj%nfilt
        call binsort ( obj%binsort, ntr, obj%hdr, obj%frtrace )
        if (ntr == FATAL_ERROR) then
          write(obj%prtlu,*) 'FILTP: Fatal error in BINSORT'
          call filtp_wrapup (obj)
          return
        endif

      enddo

      return
      end subroutine filtp

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine filtp_wrapup (obj)
      implicit none
      type(filtp_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine filtp_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module filtp_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
