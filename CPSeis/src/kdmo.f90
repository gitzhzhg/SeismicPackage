!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-06-27. />

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
! Name       : KDMO
! Category   : migrations
! Written    : 1989-11-20   by: Greg Lazear
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Perform anti-aliasing DMO on NMO corrected data.
! Portability: No known limitations.
! Parallel   : NO
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! 
! KDMO is a Kirchhoff DMO process that uses the Hale algorithm to prevent
! operator aliasing.  KDMO accepts only ONE input trace each time it is called.
! For each input trace the program computes the DMO corrected contribution to 
! all CMPs along the azimuth containing the source and receiver, but outputs 
! these one at a time. Thus one trace comes in and KDMO becomes a trace 
! supplying process. The center trace of the DMO response is output by itself, 
! then each symmetric pair of DMO broadcast traces are output one at a time.
!
! Normally KDMO is followed by GSTK to collect and stack the broadcast traces. 
! Production use of KDMO is normally within the macros CODMO (2D) and DMO3D 
! (3D).
!
!
! Coordinates
!
! The parameter COORD controls which header words are used to locate the trace 
! and its source and receiver positions. 
!
! If COORD=2D the offset and midpoint surveyed coordinate (HDR 17) are used.
! (Header word 17 is assumed to carry the midpoint inline location.)  Source 
! and receiver surveyed coordinates in the input trace are ignored and pass 
! unchanged to the output traces.  The midpoints of the output traces in 
! surveyed coordinates range from HDR(17) - HDR(6)/2. to HDR(17) + HDR(6)/2.
!
! If COORD=3D the source and receiver surveyed coordinates are used.  Offset 
! and midpoint coordinates are computed from the source and receiver surveyed 
! coordinates.  Equations for each are given below.
!
!       Offset      = SQRT( (HDR(11)-HDR(14))**2 +
!                           (HDR(12)-HDR(15))**2  )
!
!       Midpoint easting  =  HDR(11) + (HDR(14)-HDR(11))/2.
!       Midpoint northing =  HDR(12) + (HDR(15)-HDR(12))/2.
!       
! For both 2D and 3D operation, KDMO determines the location of input traces 
! and broadcast traces using surveyed coordinates.  Midpoint grid coordinates 
! are calculated and set for output traces using the grid global.
!  
!
! REFERENCE
! Hale, D., 1988, Dip Moveout Processing:  Society of Exploration Geophysicists.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
!
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
!
! This process requires traces to be input one at a time.
!
! Input data must be NMO corrected with horizontal dip velocities.
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
! This process outputs the broadcast gather of the input trace.
!
! This is a trace-supplying process.
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
! MAXTR    max number of traces input/output     used but not changed
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! GRID     grid transformation structure         used 
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
! 1       Sequential Trace Count     Renumbered
! 2       Head mute                  Used
! 5       Fold                       Set
! 6       Offset                     Used
! 7       CMP x grid                 Used, possibly set
! 8       CMP y grid                 Used, possibly set
! 11      Src surveyed easting       Used, possibly set
! 12      Src surveyed northing      Used, possibly set
! 14      Rec surveyed easting       Used, possibly set
! 15      Rec surveyed northing      Used, possibly set
! 17      MP surveyed easting        Used, possibly set
! 18      MP surveyed northing       Used, possibly set
! 25      LAV                        Used
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>  
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author      Description
!     ----       ------      -----------
!019. 2006-06-20  B. Menger   Removed Unused Variables.
! 18. 2002-03-07 Chiu        Fix frontend problem of CALC_OFFSET parameter.
! 17. 2001-06-18 Chiu        Fix large data values after interp. PRODUCTION.
! 16. 2001-04-30 Chiu        Clear a temporay work array properly.
! 15. 2001-02-14 Chiu        Change wrapped_up to skip_wrapup.
! 14. 2000-08-30 Chiu        Normalize the 45 degree phase filter and
!                            set the end mute.
! 13. 2000-07-20 Chiu        Use new coordinate for header 7 and 8.
! 12  2000-06-14 Chiu        Added Gui parameters.
! 11. 2000-03-21 Chiu        Remove PC_BACKEND check for pc_put_contol.
! 10. 2000-01-27 Chiu        For 3D, added an option either to compute
!                            offsets or to use header word for offsets. 
! 9.  1999-12-13 Chiu        Convert to new CPS.
! 8.  1999-01-11 Vunderink   Begin using the f90 compiler.
! 7.  1995-04-03 Troutt      Minor change.  Add check for NWPT>=NDPT+2.
!                            "TRACE" must be dimensioned at least ndpt+2
!                            for INTBRC call.
! 6.  1990-03-22 Howard      Change to avoid round off error.        
! 5.  1990-02-26 Lazear      Install Tom Hill's code speed-up in loop 40
! 4.  1990-02-20 Lazear      Change to single trace output
! 3.  1990-02-13 Lazear      Modify velocity cut-off logic
! 2.  1990-01-15 Lazear      Optimizations of algorithm installed
! 1.  1989-11-20 Lazear      Wrote  KDMO algorithm to
!                            eliminate operator aliasing over CMP. Used
!                            Sinton's KDMO3 as basic framework for new
!                            program. This algorithm comes from Dave Hale. 
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
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES 
!                
!  The design of the bandpass filter was changed. The DMO output will not    
!  match exactly as the output from the old CPS. 
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!
!<gui_def>
!<NS KDMO Process/NC=80>
!
!        Perform anti-aliasing DMO on NMO corrected data.
!
! COORD=~~~`CC           BIN_WID= `FFFFFFFFFFF    OFF_MAX=~~~~`FFFFFFFFFFF
!
! VEL_DMO= `FFFFFFFFFFF  FREQ_MAX=`FFFFFFFFFFF    CALC_OFFSET=`CCC
!
! HDR_INL= `IIIIII       HDR_CRL= `IIIIII
!</gui_def>
!
!
!<HelpSection>
!
!<Help KEYWORD="COORD">
!<Tip> Whether to use 2D or 3D header words for location. </Tip>
! Default = 3D
! Allowed = 2D
! Allowed = 3D
! If COORD = 2D, then use offset and midpoint surveyed inline coordinate.
! If COORD = 3D, then use source and receiver surveyed coordinates.
!</Help>
!
!<Help KEYWORD="BIN_WID">
!<Tip> Width of the stack bins. </Tip>
! Default =  1
! Allowed = real > 0.0
! Width of the stack bins along the line connecting source and receiver.  This 
! is normally the CMP interval in the inline direction.
!</Help>
!
!<Help KEYWORD="OFF_MAX">
!<Tip> Maximum offset in dataset. </Tip>
! Default =  1
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="VEL_DMO">
!<Tip> Minimum propagation velocity in medium. </Tip>
! Default = 1500
! Allowed = real > 0.0
! VEL_DMO is used to calculate the maximum dip to be preserved in the data.
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency to be preserved in data, in Hz. </Tip>
! Default = 90
! Allowed = real > 0.0
! FREQ_MAX is used in the DMO operator calculation as the highest frequency to 
! preserve in the data.  Larger values of FREQ_MAX require significantly more 
! run-time.
!</Help>
!
!<Help KEYWORD="CALC_OFFSET">
!<Tip> Re-calculate offsets for 3D data only. </Tip>
! Default = YES
! Allowed = YES
! Allowed = NO
! If CALC_OFFSET = YES, offsets are re-calculated as in the General Description
! section. 
!
! If CALC_OFFSET = NO, offsets are assigned from values of header word 6.
!</Help>
!
!<Help KEYWORD="HDR_CRL">
!<Tip> Header word for crossline coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="HDR_INL">
!<Tip> Header word for inline coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
!  Check on item 7 in revision history.  Yes, this should be the case.
!
!
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module kdmo_module
      use pc_module
      use named_constants_module
      use grid_module           ! if you need the grid transformation.
      use fltr_module
      use bandps_module
      use fft_module
      use interp_module 
      use string_module
      use lav_module

      implicit none
      private
      public :: kdmo_create     ! uses the parameter cache.
      public :: kdmo_initialize
      public :: kdmo_update     ! uses the parameter cache.
      public :: kdmo_delete

!<execute_only>

      public :: kdmo            ! main execution (trace processing) routine.
      public :: kdmo_wrapup

!</execute_only>

      character(len=100),public,save :: kdmo_IDENT = &
       '$Id: kdmo.f90,v 1.19 2006/06/20 13:11:57 Menger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      integer,parameter :: INTT = 4
      real   ,parameter :: FINT = 4.0
                                     
      type,public :: kdmo_struct              
 
        private
        logical                  :: skip_wrapup           ! wrapup flag.
        real                     :: bin_wid,freq_max     ! process parameters.
        character(len=8)         :: coord                ! process parameters.
        real                     :: off_max,vel_dmo      ! process parameters.
        character(len=3)         :: calc_offset          ! process parameters.
        integer                  :: hdr_inL              ! process parameters.
        integer                  :: hdr_crl             ! process parameters. 

        integer                  :: nwih,ndpt            ! globals.  
        type(grid_struct)        :: grid                 ! globals.

        integer,         pointer :: itwin(:),ndt(:)      ! dependent variables. 
        integer,         pointer :: idt(:),jdt(:)        ! dependent variables.
        real,            pointer :: filt(:)              ! dependent variables.
        real,            pointer :: bwc(:),delt(:)       ! dependent variables.

        integer                  :: ngrp,ndelt,maxbi     ! dependent variables.
        integer                  :: nf,nfh,ntrc          ! dependent variables.
        integer                  :: nhbx,nhby,nhsx,nhsy  ! dependent variables.
        integer                  :: nhrx,nhry,nhmx,nhmy  ! dependent variables.
        real                     :: xmppmi,xmppma        ! dependent variables.
        real                     :: ymppmi,ymppma        ! dependent variables.
        real                     :: xhmi,xhma,xdh        ! dependent variables.

        integer                  :: print_lun            ! dependent variables. 
        type(fft_struct),pointer :: fftrc_obj            ! dependent variables.
        type(fft_struct),pointer :: fftcr_obj            ! dependent variables.

      end type kdmo_struct
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(kdmo_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: coord_noptions = 2
      character(len=2),save :: coord_options(coord_noptions)
      data coord_options/'2D','3D'/

      integer,parameter     :: calc_offset_noptions = 2
      character(len=3),save :: calc_offset_options(calc_offset_noptions)
      data calc_offset_options/'YES','NO'/


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine kdmo_create (obj)
      implicit none
      type(kdmo_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%filt) 
      nullify (obj%itwin) 
      nullify (obj%bwc) 
      nullify (obj%ndt) 
      nullify (obj%delt) 
      nullify (obj%idt) 
      nullify (obj%jdt)
      nullify (obj%fftrc_obj)
      nullify (obj%fftcr_obj)
 
      call kdmo_initialize (obj)

      return
      end subroutine kdmo_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine kdmo_delete (obj)
      implicit none
      type(kdmo_struct),pointer :: obj       ! arguments

!<execute_only>
      call kdmo_wrapup (obj)
!</execute_only>

!! Make sure ALL POINTERS in your parameter structure are deallocated

      if (associated(obj%filt))  deallocate (obj%filt)
      if (associated(obj%itwin)) deallocate (obj%itwin)
      if (associated(obj%bwc))   deallocate (obj%bwc)
      if (associated(obj%ndt))   deallocate (obj%ndt)
      if (associated(obj%delt))  deallocate (obj%delt)
      if (associated(obj%idt))   deallocate (obj%idt)
      if (associated(obj%jdt))   deallocate (obj%jdt)
      if (associated(obj%fftrc_obj ))  call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj ))  call fft_delete (obj%fftcr_obj)

      deallocate(obj)

      return
      end subroutine kdmo_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine kdmo_initialize (obj)
      implicit none
      type(kdmo_struct),pointer :: obj       ! arguments

      call grid_initialize(obj%grid)

      obj%bin_wid     =  1
      obj%freq_max    = 90.
      obj%coord       = '2D'
      obj%off_max     = 1
      obj%vel_dmo     = 1500.
      obj%calc_offset = 'YES'
      obj%hdr_inL     = 7
      obj%hdr_crl     = 8

      obj%print_lun = pc_get_lun()
      call grid_initialize(obj%grid)
      call kdmo_update (obj)

      return
      end subroutine kdmo_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine kdmo_update (obj)
      implicit none
      type(kdmo_struct),target :: obj                          ! arguments
                                    
      logical                    :: gathered                   ! local
      real                       :: dt,tstrt                   ! local
      integer                    :: ier,numtr                  ! local
      integer                    :: nstore, nscratch           ! local
      character(len=3)           :: need_label,need_request    ! local
      character(len=3)           :: twosets                    ! local

      integer                    :: maxb,nbmax,npow2           ! local
      integer                    :: nfq,i,ii,kk,nn,j,itw       ! local
      real                       :: dtdi,tmut,dxdh             ! local
      real                       :: dtmax,fnyq,finc            ! local

      real                       :: temp,atemp,asqr,denp       ! local
      real                       :: tp1,tp2,f1,f2              ! local

      complex,allocatable,dimension(:) :: ctrace               ! local
      real,allocatable,dimension(:)    :: work                 ! local
                     
      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('nwih'  , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'  , obj%ndpt)  ! number of trace samples.
      call pc_get_global ('dt'    , dt)        ! trace sample interval (sec).
      call pc_get_global ('tstrt' , tstrt)     ! time of 1st trace sample (sec).
      call pc_get_global ('grid'  , obj%grid)  ! grid transform data structure.

      call pc_get ('bin_wid'     , obj%bin_wid)
      call pc_get ('freq_max'    , obj%freq_max)
      call pc_get ('coord'       , obj%coord)
      call pc_get ('off_max'     , obj%off_max)
      call pc_get ('vel_dmo'     , obj%vel_dmo)
      call pc_get ('calc_offset' , obj%calc_offset)
      call pc_get ('hdr_inL'     , obj%hdr_inL)
      call pc_get ('hdr_crl'     , obj%hdr_crl)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%bin_wid <= 0.) then
         call pc_error('Invalid in KDMO: BIN_WID MUST BE > 0')
      end if 

      if(obj%freq_max <= 0) then
         call pc_error                                                   &
         ('Invalid in KDMO: FREQ_MAX MUST BE > 0')
      end if

      if( obj%freq_max > nint(0.5/dt) ) then
         obj%freq_max =  (0.5/dt)
         call pc_warning(' Reset FREQ_MAX in KDMO to Nyquist frequency ', &
                           (0.5/dt)  )
      end if

      if(obj%off_max <=0) then
         call pc_error('Invalid in KDMO: OFF_MAX MUST BE > 0')
      end if
      if(obj%vel_dmo <=0.) then
         call pc_error('Invalid in KDMO: VEL_DMO MUST BE > 0')
      end if

      call string_to_upper (obj%coord)
      if(obj%coord /= '2D'.and. obj%coord /= '3D') then
         call pc_error('Invalid in KDMO: COORD MUST BE 2D OR 3D')
      end if

      call string_to_upper (obj%calc_offset)
      if(obj%calc_offset /= 'YES'.and. obj%calc_offset /= 'NO') then
         call pc_error('Invalid in KDMO: CALC_OFFSET MUST BE YES OR NO')
      end if

      if(obj%hdr_inL < 1) then 
        call pc_error('HDR_INL MUST BE >=  1')  
      else if(obj%hdr_inL > obj%nwih) then 
        call pc_error('HDR_INL MUST BE =< 64 ')  
      end if

      if(obj%hdr_crl < 1) then 
        call pc_error('HDR_CRL MUST BE >=  1')  
      else if(obj%hdr_crl > obj%nwih) then 
        call pc_error('HDR_CRL MUST BE =< 64 ')  
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

     call pc_put_options_field ('coord', coord_options,   &
                                 coord_noptions)

     call pc_put_options_field ('calc_offset', calc_offset_options,   &
                                   calc_offset_noptions)

      numtr = 2*nint(obj%off_max/obj%bin_wid) + 1
      gathered = .false.
      call pc_put_global ('numtr'     ,numtr) 
      call pc_put_global ('gathered'  ,gathered)
   
      call pc_put ('bin_wid'     , obj%bin_wid)
      call pc_put ('freq_max'    , obj%freq_max)
      call pc_put ('coord'       , obj%coord)
      call pc_put ('off_max'     , obj%off_max)
      call pc_put ('vel_dmo'     , obj%vel_dmo)
      call pc_put ('calc_offset' , obj%calc_offset)
      call pc_put ('hdr_inL'     , obj%hdr_inL)
      call pc_put ('hdr_crl'    , obj%hdr_crl)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      if (associated(obj%filt))  deallocate(obj%filt) 
      if (associated(obj%itwin)) deallocate(obj%itwin) 
      if (associated(obj%bwc))   deallocate(obj%bwc) 
      if (associated(obj%ndt))   deallocate(obj%ndt) 
      if (associated(obj%delt))  deallocate(obj%delt) 
      if (associated(obj%idt))   deallocate(obj%idt) 
      if (associated(obj%jdt))   deallocate(obj%jdt)

      if (associated(obj%fftrc_obj ))  call fft_delete (obj%fftrc_obj)
      if (associated(obj%fftcr_obj ))  call fft_delete (obj%fftcr_obj)

!! Initialize all variables (except allocated arrays) needed for execution

      obj%ntrc = 0
      obj%ngrp = 0 
      obj%nhbx = obj%hdr_inL 
      obj%nhby = obj%hdr_crl 
      obj%nhsx = 11 
      obj%nhsy = 12 
      obj%nhrx = 14 
      obj%nhry = 15 
      obj%nhmx = 17 
      obj%nhmy = 18
      obj%xmppmi = 1.e+10 
      obj%xmppma = -1.e+10 
      obj%ymppmi = 1.e+10 
      obj%ymppma = -1.e+10 
      obj%xhmi   = 1.e+10 
      obj%xhma   = 0.0 


!   set delta t shift parameters 
!   x/h is set to 0.7 assuming obj%vel_dmo= obj%mute velocity 
! 
!    define local variables.

      tp1 = 4. 
      tp2 = 4. 
      f1 = 4. 
      f2 = obj%freq_max 
      dtdi = dt/float(INTT)

      obj%xdh = 0.7 
      tmut = obj%off_max/obj%vel_dmo 
      dxdh = dt/tmut/(f2*2.0*dt) 
      obj%ndelt = obj%xdh/dxdh + 1 
      dtmax = tmut*(1.0 - sqrt(1.0 - ((obj%ndelt - 1)*dxdh)**2)) 
      maxb = ifix(obj%off_max/obj%bin_wid/2.0 + 1) 
      obj%maxbi = (maxb - 1)*FINT + 1 
      nbmax = obj%maxbi*obj%ndelt 

      npow2 = 2**int(0.99999999 + log(real(obj%ndpt))/log(2.0)) 
      nfq = npow2/2 + 1 
      fnyq = 1./(2.*dt) 
      finc = fnyq/(nfq - 1) 
      obj%nf = nint(fnyq/f2*0.1/dt/2.0)*2 + 1 
      obj%nfh = (obj%nf - 1)/2 
 
      write(obj%print_lun, *) ' MAX. NUMBER OF DMO TRACES = ',                & 
        ifix(obj%off_max*obj%xdh/obj%bin_wid/2.0)*2 + 1 
      write(obj%print_lun, *) ' MAXIMUM DMO TIME SHIFT (SEC) = ', dtmax 
      write(obj%print_lun, *) ' NUMBER OF DELTA T SCANS = ', obj%ndelt 
      write(obj%print_lun, *) ' NBMAX (SIZE OF WINDOW TABLES) = ', nbmax 

 
        need_label   = 'NO'
        need_request = 'YES'
        twosets      = 'NO'
 
        nstore = max0(obj%ndpt*INTT,npow2) + obj%nf + obj%nwih*2*obj%maxbi   &
               + nbmax+INTT*5 + obj%maxbi +obj%ndelt*3 + obj%ndpt*2*obj%maxbi
        nscratch = npow2 + 2 + 2*(3*npow2/2 + 2)
 
        call pc_put_control ('nstore',             nstore)
        call pc_put_control ('nscratch',         nscratch)
        call pc_put_control ('need_label',     need_label)
        call pc_put_control ('need_request', need_request)
        call pc_put_control ('twosets',           twosets)
 

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!! Allocate permanent memory

      allocate(obj%filt(obj%nf),stat=ier) 
      if (ier/=0) call pc_error ('Error creating KDMO:filt array')
      allocate(obj%itwin(nbmax),stat=ier) 
      if (ier/=0) call pc_error ('Error creating KDMO:itwin array')
      allocate(obj%bwc(INTT*5),stat=ier) 
      if (ier/=0) call pc_error ('Error creating KDMO:bwc array')
      allocate(obj%ndt(obj%maxbi),stat=ier) 
      if (ier/=0) call pc_error ('Error creating KDMO:ndt array')
      allocate(obj%delt(obj%ndelt),stat=ier)
      if (ier/=0) call pc_error ('Error creating KDMO:delt array') 
      allocate(obj%idt(obj%ndelt),stat=ier)
      if (ier/=0) call pc_error ('Error creating KDMO:idt array')
      allocate(obj%jdt(obj%ndelt),stat=ier)
      if (ier/=0) call pc_error ('Error creating KDMO:jdt array')

!! Allocate atemporary memory 

      allocate(ctrace(npow2),stat=ier)
      if (ier/=0) call pc_error ('Error creating KDMO:ctrace array')
      allocate(work(npow2),stat=ier)
      if (ier/=0) call pc_error ('Error creating KDMO:work array')

      ier =  fft_create (obj%fftrc_obj, -1, npow2, 'rtoc')
      if (ier/=0) call pc_error ('Error creating rtoc FFT object')

      ier =  fft_create (obj%fftcr_obj, +1, npow2, 'ctor')
      if (ier/=0) call pc_error ('Error creating ctor FFT object')
 
 
!   create the i-omega filter 
! 
      call bandps (ctrace, nfq, finc, 'BANDPASS', f1-tp1, f1,     &
          f2, f2+tp2, 0.0, 1.0)
 
      do i = 1, nfq  
        ctrace(i) = ctrace(i)*csqrt(cmplx(0.,float(i - 1)*finc))
      end do 
!  
!   initialize the fft and interpolation routines 
!   
      call interp_1d_con_bw_real (INTT, obj%ndpt, -1, obj%bwc, work, work) 
!  
!   convert the i-omega filter to time domain centered at obj%nfh+1 and 
!   obj%nf points long in obj%filt. use +1 in fft to time reverse for
!   fltr_filterg 
!  
      call fft_cr_transform(obj%fftcr_obj, ctrace, work)
      
      obj%filt(1:obj%nfh) = work(npow2-obj%nfh+1:npow2) 
      obj%filt(obj%nfh+2:2*obj%nfh+1) = work(2:obj%nfh+1) 
      obj%filt(obj%nfh+1) = work(1)

      i = mth_isamax( 2*obj%nfh+1, obj%filt, 1)
      temp = abs(obj%filt(i))
      obj%filt(1:2*obj%nfh+1) =  obj%filt(1:2*obj%nfh+1)/temp
 
! 
!   compute table of delta t shifts 
! 
      do i = 1, obj%ndelt 
        obj%delt(i) = tmut*(1.0 - sqrt(1.0 - ((i - 1)*dxdh)**2)) 
      end do 
! 
!   obj%idt contains the fractional shift (remainder) in samples of the 
!       interpolated trace 
!   obj%jdt contains the whole shift in samples of the original sample rate. 
! 
      do i = 1, obj%ndelt 
        ii = nint(obj%delt(i)/dtdi) + 1 
        obj%jdt(i) = (ii - 1)/float(INTT) 
        obj%idt(i) = ii - obj%jdt(i)*INTT 
      end do 
! 
!   compute table of windows for the max. offset for each obj%delt and x 
! 
      temp = obj%bin_wid*2.0/FINT/obj%off_max 
      if (temp >= 1.0) temp = 0.999999 
! 
!   set the window times for the center trace to be past the end of trace 
! 
      obj%itwin(1:obj%ndelt) = obj%ndpt - 1 
      obj%ndt(1) = 0 
! 
!   this loop computes window times for traces beyond the center 
! 
      do i = 2, obj%maxbi 
        kk = (i - 1)*obj%ndelt 
        atemp = (i - 1)*temp 
        atemp = amin1(1.,atemp) 
        asqr = sqrt(1.0 - atemp**2) 
        denp = 1.0/(1.0 - asqr) 
        nn = 0 
! 
        do j = 1, obj%ndelt 
          itw = max0(nint((obj%delt(j)*denp-tstrt)/dt)+1,1) 
          if (itw > obj%ndpt - 1) then 
            itw = obj%ndpt - 1 
          else 
            nn = nn + 1 
          endif 
          obj%itwin(j+kk) = itw 
        end do 
        obj%ndt(i) = nn  
      end do 
      if (pc_do_not_process_traces()) return
!  
!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine kdmo_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!




!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


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

      subroutine kdmo (obj,ntr,hd,tr)
      implicit none
      type(kdmo_struct)                      :: obj                 ! arguments
      integer         ,intent(inout)         :: ntr                 ! arguments
      double precision,intent(inout)         :: hd(:,:)             ! arguments
      real            ,intent(inout)         :: tr(:,:)             ! arguments

      real,dimension((obj%ndpt+2)*INTT+2*obj%nf) :: trint           ! local
      double precision,dimension(obj%nwih)   :: head                ! local
      real,dimension(obj%ndpt+2*obj%nf)      :: work                ! local
      integer                                :: ipos,nbmh,nx        ! local
      integer                                :: nbidxb,nbidxt       ! local
      integer                                :: idxt,idxb           ! local

      integer                                :: mute,itw,ibw        ! local
      integer                                :: ii,jj,i,jmjj1       ! local
      integer                                :: jmjj2,k             ! local
      integer                                :: ifmute,ibmute       ! local
      real                                   :: dxh,dyh,xh,dxsr,dysr! local
      real                                   :: bi,bih,xmpp,ympp    ! local
      real                                   :: xmp,ymp,x1,y1       ! local
      real                                   :: delxmp,delymp       ! local
 



      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then

         write(obj%print_lun, 1010) obj%xmppmi, obj%xmppma, obj%ymppmi,      & 
            obj%ymppma, obj%xhmi, obj%xhma, obj%ngrp, obj%ntrc 

 1010     format(10x,' summary of data processing for KDMO. '/,11x,           &
            'xmppmi  = ', f12.4,                                          &
            ' minimum input  x basement.'/,11x,'xmppma  = ',f12.4,        & 
            ' maximum input  x basement.'/,11x,'ymppmi  = ',f12.4,        & 
            ' minimum input  y basement.'/,11x,'ymppma  = ',f12.4,        & 
            ' maximum input  y basement.'/,11x,'xhmi    = ',f12.4,        & 
            ' minimum input offset.'/,11x,'xhma    = ',f12.4,             & 
            ' maximum input offset.'/,11x,'ngrp    = ',i10,               & 
            ' # of live input traces.'/,11x,'ntrc    = ',i10,             & 
            ' # of output traces.'/) 

         call kdmo_wrapup (obj)
         return
      end if
! 
!   check for more than one input trace 
! 
        if (ntr > 1) then 
          write(obj%print_lun, *) 'KDMO cannot accept more than one  ',      & 
            'trace at a time - Abort' 
          ntr = FATAL_ERROR 
          return  
        endif 
!  
!   check for a dead trace. if dead then do not process this trace. 
! 
        if (hd(25,1)==0.0) then 
          ntr = NEED_TRACES 
          return  
        endif 
!  
!  setup offset from headers based on #coord 
! 
        head(1:obj%nwih) = hd(1:obj%nwih,1)
        if (obj%coord == '3D') then  

          dxh = head(obj%nhrx) - head(obj%nhsx) 
          dyh = head(obj%nhry) - head(obj%nhsy)

          if (obj%calc_offset == 'NO') then
            xh = abs(head(6))
          else  
            xh = sqrt(dxh**2 + dyh**2)
          end if
   
          xmpp = head(obj%nhsx) + dxh/2. 
          ympp = head(obj%nhsy) + dyh/2. 
        else 
          dxh = abs(head(6)) 
          dyh = 0.0 
          xh = dxh 
          xmpp = head(obj%nhmx) 
          ympp = head(obj%nhmy) 
        endif 
        obj%ngrp = obj%ngrp + 1 
        obj%xmppmi = amin1(obj%xmppmi, xmpp) 
        obj%xmppma = amax1(obj%xmppma, xmpp) 
        obj%ymppmi = amin1(obj%ymppmi, ympp) 
        obj%ymppma = amax1(obj%ymppma, ympp) 
        obj%xhmi = amin1(obj%xhmi,xh) 
        obj%xhma = amax1(obj%xhma,xh) 
        if (xh /= 0) then 
          dxsr = dxh*obj%bin_wid/xh 
          dysr = dyh*obj%bin_wid/xh 
        else 
          dxsr = 0. 
          dysr = 0. 
        endif 
! 
!   set xh to half offset 
! 
        xh = xh/2.0 
! 
!   from offset of trace find max. bins and table pointers 
! 
        mute = head(2) 
        nbmh = ifix(xh*obj%xdh/obj%bin_wid) + 1 
        if ( xh /= 0.0) then
          bi = obj%bin_wid/xh*(obj%maxbi - 1)
          bih = bi/2.0
        else 
          bi = 0.0
        end if

        if (nbmh == 1) bih = obj%maxbi - 1         
! 
!   apply sqrt(iw) filter to input trace 
!  
        trint(1:obj%nfh) = 0.0 
        trint(obj%ndpt+obj%nfh+1:2*obj%nfh+obj%ndpt) = 0.0 
        trint(obj%nfh+1:obj%ndpt+obj%nfh) = tr(1:obj%ndpt,1)
        
        call fltr_filterg (obj%filt, obj%nf, trint, obj%ndpt+obj%nf-1, work) 

        work(obj%ndpt+1:obj%ndpt+obj%nf) = 0.0
! 
!   interpolate the input trace by INTT times 
! 
        call interp_1d_con_bw_real (INTT, obj%ndpt+2, 1, obj%bwc, work, trint) 
! 
!   save input trace header 
! 
       ipos = 0   
       do nx = 0, nbmh - 1
         ipos = ipos + 1
         hd(1:obj%nwih,ipos) = head(1:obj%nwih) 
         hd(5,ipos) = 0. 
 
!   create another dmo trace at position nx  
! 
        tr(1:obj%ndpt,ipos) = 0.0 
! 
!   loop over time shifts 
! 
        nbidxb = max0(nint(nx*bi - bih) + 1,1) 
        nbidxt = min0(nint(nx*bi + bih) + 1,obj%maxbi) 
        idxt = (nbidxt - 1)*obj%ndelt 
        idxb = (nbidxb - 1)*obj%ndelt 
        ifmute = obj%ndpt
        ibmute = 0 
! 
!  create the zero shift contribution 
! 
        itw = max0(obj%itwin(idxt+1),mute) 
        if (nx > 0) then 
          ibw = (obj%itwin(idxt+2)+obj%itwin(idxb+2))/2.0 
        else 
          ibw = obj%itwin(idxb+1)
        endif 

        if (itw <= ibw) then 
          ii = obj%idt(1) 
          jj = obj%jdt(1) 
 
          ifmute = min( itw - jj, ifmute)
          ibmute = max( ibw - jj, ibmute)
! 
!   form the output dmo trace as sum of shifted windows 
! 
          tr(itw-jj:ibw-jj,ipos) = tr(itw-jj:ibw-jj,ipos)          & 
            + trint((itw-1)*INTT+ii:(ibw-1)*INTT+ii:INTT) 
        endif 
! 
        do i = obj%ndt(nbidxt), 2, -1 
          itw = max0(obj%itwin(idxt+i),mute) 
          ibw = obj%itwin(idxb+i) 

          if (itw > ibw) exit  
          ii = obj%idt(i) 
          jj = obj%jdt(i) 
 
          ifmute = min( itw - jj, ifmute)
          ibmute = max( ibw - jj, ibmute)
! 
!   form the output dmo trace as sum of shifted windows 
! 
          jmjj1 = itw - jj 
          jmjj2 = ibw - jj 
          k = (itw - 1)*INTT + ii
          tr(jmjj1:jmjj2,ipos) = tr(jmjj1:jmjj2,ipos)              & 
           + trint(k:(jmjj2-jmjj1)*INTT+k:INTT ) 
        end do  
! 
!  this is a dead trace so do not output it 
! 
        if (ifmute == obj%ndpt) then
          ipos = ipos - 1 
          cycle 
        endif 
! 
!  set output trace headers and move dmo trace into trace 
! 
        delxmp = nx*dxsr 
        delymp = nx*dysr 

        if (nx > 0) then 
          hd(5,ipos) = 0.0 
          do i = 1,2      
            if (i == 1) then 
              xmp = xmpp + delxmp 
              ymp = ympp + delymp 
            else 
              ipos = ipos + 1
              xmp = xmpp - delxmp 
              ymp = ympp - delymp 
              tr(1:obj%ndpt,ipos) = tr(1:obj%ndpt,ipos-1)
              hd(1:obj%nwih,ipos) = hd(1:obj%nwih,ipos-1)
            endif 
            call grid_get_grid_coords(obj%grid,xmp,ymp,x1,y1)
            hd(32, ipos)      = -1.                                
            hd(obj%nhbx,ipos) = x1
            hd(obj%nhby,ipos) = y1                        
            hd(obj%nhmx,ipos) = xmp 
            hd(obj%nhmy,ipos) = ymp
            hd(2, ipos) = ifmute
            hd(64,ipos) = ibmute
            obj%ntrc = obj%ntrc + 1
          end do
        else 
          obj%ntrc = obj%ntrc + 1
          xmp = xmpp  
          ymp = ympp
          call grid_get_grid_coords(obj%grid,xmp,ymp,x1,y1) 
          hd(32, ipos)      = 1.                                           
          hd(obj%nhbx,ipos) = x1
          hd(obj%nhby,ipos) = y1                        
          hd(obj%nhmx,ipos) = xmp 
          hd(obj%nhmy,ipos) = ymp 

          hd(2, ipos) = ifmute
          hd(5,ipos)  = 1.0
          hd(64,ipos) = ibmute
 
          tr(1:obj%ndpt,ipos) = tr(1:obj%ndpt,ipos)*2.0 
        endif 

      end do

      ntr = ipos

      call lav_set_hdr (hd, tr, obj%ndpt, ntr)
 
      return 
      end subroutine kdmo 

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine kdmo_wrapup (obj)
      implicit none
      type(kdmo_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine kdmo_wrapup

!</execute_only>

!!
!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module kdmo_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

