!<CPS_v1 type="PROCESS"/>

!<-- This documentation header was last revised by CI Burch on 2000-07-17. />

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
! Name       : RESTH       (RESTore Headers)  [Formerly STKH]
! Category   : headers 
! Written    : 1989-06-15   by: Mike Howard
! Revised    : 2001-05-03   by: Stephen Chiu
! Maturity   : production   2001-05-14   
! Purpose    : Restore headers for traces processed by DMO or BS migration.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
!
! RESTH calculates and applies headers for traces that have been stacked with 
! GSTK as a part of a DMO, before-stack migration or other similar process.  
! It is typically used only within the macros CODMO and DMO3D.  
!
! RESTH operates in a two-step fashion.  In MODE = CALC, before DMO, it will 
! calculate and store appropriate headers.  In MODE = APPLY, after GSTK, it
! will apply the stored headers to the traces. 
!
!
! 2D vs. 3D Operation
!
! If CRL_TOT = 1, the default, then RESTH operates in a 2D mode with a 
! one-dimensional bin array in the inline direction.
!
!
! Header Words Used
!
! The 14 header words that are calculated by this process are: 2, 5, 19, 37-45,
! 56, and 57.
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
!
! This process does not alter input traces in MODE = CALC, but resets headers in
! MODE = APPLY.
!
! This process outputs the same traces as it receives.
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

! NWIH     number of words in trace header       used but not changed
! 
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Header words 2, 5, 19, 37-45, 56, and 57 are calculated and reset.       
! 
! Header word HDR_INL and HDR_CRL are used to associate traces with stack bins.
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!      Date       Author     Description
!      ----       ------     -----------
! 11.  2001-05-14 S Chiu     Fix frontend problem in pattern_stop2
! 10.  2001-04-04 S Chiu     Add pc_get_update_state() /= PC_GUI in checking
!                            parameters.
! 9.   2000-12-07 S Chiu     Change wrapped_up to skip_wrapup.
! 8.   2000-08-23 S Chiu     Use new coordinate for header 7 and 8. 
! 7.   2000-06-14 S Chiu     Added Gui parameters. 
! 6.   2000-01-27 S Chiu     Added a global variable to access mode option
!                            in the DMO Macro.
! 5.   1999-11-09 S Chiu     Convert to new CPS.
! 4.   1999-09-27 CI Burch   Name changed from STKH to RESTH
! 3.   1998-11-11 Goodger    Begin using f90 compiler.                     
! 2.   1993-02-18 Troutt     Added header words 44 & 45 to list of headers
!                            being handled and changed logic to use the
!                            1st header value in each bin rather than
!                            taking the average of all values in each bin.
! 1.   1989-06-15 Howard     Original version 
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
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<gui_def>
!<NS RESTH Process>
!
!  Restore headers for traces processed by DMO or BS migration.
!
!  MODE=~~~~`CCCCC    
!
!  HDR_INL= `IIIIII    
!
!  INL_INIT=`FFFFFFFFFFF   INL_INC=`FFFFFFFFFFF    INL_WID=`FFFFFFFFFFF 
!  INL_LAST=`FFFFFFFFFFF   INL_TOT=`IIIIIIII
!
!  HDR_CRL= `IIIIII    
!
!  CRL_INIT=`FFFFFFFFFFF   CRL_INC=`FFFFFFFFFFF    CRL_WID=`FFFFFFFFFFF
!  CRL_LAST=`FFFFFFFFFFF   CRL_TOT=`IIIIIIII
!</gui_def>


!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to calculate (CALC) or apply (APPLY) headers. </Tip>
! Default = CALC
! Allowed = CALC
! Allowed = APPLY
! In the CALC mode, RESTH will examine header values in input traces, calculate
! appropriate values and store them.
!
! In the APPLY mode, RESTH will reset headers in input traces using the stored
! values.
!</Help>
!
!<Help KEYWORD="HDR_INL">
!<Tip> Header word for inline coordinate. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="INL_INIT">
!<Tip> Value of HDR_INL for center of first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="INL_INC">
!<Tip> Increment between bins in inline direction. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="INL_WID">
!<Tip> Width of bins in inline direction. </Tip>
! Default = INL_INC
! Allowed = INL_INC >= real > 0.0
!</Help>
!
!<Help KEYWORD="INL_LAST">
!<Tip> Value of HDR_INL for center of last bin. </Tip>
! Default = 1.0
! Allowed = real>=INL_INIT
!</Help>
!
!<Help KEYWORD="INL_TOT">
!<Tip> Total number of bins in inline direction. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="HDR_CRL">
!<Tip> Header word for crossline coordinate. </Tip>
! Default = 8
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="CRL_INIT">
!<Tip> Value of HDR_CRL for center of first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="CRL_INC">
!<Tip> Increment between bins in crossline direction. </Tip>
! Default = 1.0
! Allowed = real>0.0
!</Help>
!
!<Help KEYWORD="CRL_WID">
!<Tip> Width of bins in crossline direction. </Tip>
! Default = CRL_INC
! Allowed = CRL_INC >= real > 0.0
!</Help>
!
!<Help KEYWORD="CRL_LAST">
!<Tip> Value of HDR_CRL for center of last bin. </Tip>
! Default = 1.0
! Allowed = real>=CRL_INIT
!</Help>
!
!<Help KEYWORD="CRL_TOT">
!<Tip> Total number of bins in crossline direction. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! Begin Fortran here.
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
!  
!-------------------------------------------------------------------------------
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module resth_module
      use pc_module
      use named_constants_module
      use string_module
      use pattern_module
         
      implicit none
      private
      public :: resth_create     ! uses the parameter cache.
      public :: resth_initialize
      public :: resth_update     ! uses the parameter cache.
      public :: resth_delete

!<execute_only>

      public :: resth            ! main execution (trace processing) routine.
      public :: resth_wrapup

!</execute_only>

      character(len=100),public,save :: resth_IDENT = &
       '$Id: resth.f90,v 1.11 2001/05/10 20:37:33 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer,parameter  :: n_headers = 14
      real,parameter     :: const = 1.e32
      integer, parameter :: MAX_LINE = 132 ! Maximum number of characters
                                  
      type,public :: resth_struct              
 

        private            
        logical                    :: skip_wrapup       ! wrapup flag.
        character(len=8)           :: mode             ! process parameters.
        integer                    :: hdr_crl          ! process parameters.
        real                       :: crl_init         ! process parameters.
        real                       :: crl_inc          ! process parameters.
        real                       :: crl_wid          ! process parameters.
        real                       :: crl_last         ! process parameters.
        integer                    :: crl_tot          ! process parameters.

        integer                    :: hdr_inL          ! process parameters.
        real                       :: inL_init         ! process parameters.
        real                       :: inL_inc          ! process parameters.
        real                       :: inL_wid          ! process parameters. 
        real                       :: inL_last         ! process parameters.
        integer                    :: inL_tot          ! process parameters.
  
        integer                    :: nwih,ndpt        ! globals.  
        logical                    :: build            ! dependent variables.
        integer                    :: nxy              ! dependent variables.
        integer                    :: nhead            ! dependent variables.
        integer                    :: print_lun        ! dependent variables.
 
      end type resth_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      real              ,pointer,save :: resth_hds(:) ! header storage.
      character(len=8)  ,public,save  :: resth_mode   ! header option.
      type(resth_struct),pointer,save :: object       ! needed for traps.

      integer,parameter     :: mode_noptions = 2
      character(len=5),save :: mode_options(mode_noptions)
      data mode_options/'CALC','APPLY'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine resth_create (obj)
      implicit none
      type(resth_struct),pointer :: obj       ! arguments

      allocate (obj)

!! Nullify ALL POINTERS in your parameter structure as follows:

      nullify  (resth_hds)              

      call resth_initialize (obj)

      return
      end subroutine resth_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine resth_delete (obj)
      implicit none
      type(resth_struct),pointer :: obj       ! arguments

!<execute_only>
      call resth_wrapup (obj)
!</execute_only>

!! Make sure ALL POINTERS in your parameter structure are deallocated

      if (associated(resth_hds)) deallocate (resth_hds)
      deallocate(obj)

      return
      end subroutine resth_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine resth_initialize (obj)
      implicit none
      type(resth_struct),pointer :: obj       ! arguments

!! Initialize ALL NON-POINTER VARIABLES in your parameter structure

      call pc_get_global ('ndpt', obj%ndpt)      
      call pc_get_global ('nwih', obj%nwih)

      obj%mode      = 'CALC'

      obj%hdr_inL  = 7
      obj%inL_init = 1.
      obj%inL_inc  = 1.
      obj%inL_wid  = obj%inL_inc 
      obj%inL_last = 1. 
      obj%inL_tot  = 1

      obj%hdr_crl  = 8 
      obj%crl_init = 1. 
      obj%crl_inc  = 1.
      obj%crl_wid  = obj%crl_inc
      obj%crl_last = 1.
      obj%crl_tot  = 1

      call resth_update (obj)

      return
      end subroutine resth_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine resth_update (obj)
      implicit none
      type(resth_struct),target :: obj                            ! arguments

      integer                    :: ier, jr                       ! local
      integer                    :: nstore, nscratch              ! local
      integer                    :: istatus                       ! local

      integer                    :: state                         ! local
      logical                    :: verify                        ! local

      character(len=3)           :: need_label,need_request       ! local
      character(len=3)           :: twosets                       ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('nwih'  , obj%nwih)  ! number of header words.
      call pc_get_global ('ndpt'  , obj%ndpt)  ! number of trace samples.

      call pc_get ('mode'      , obj%mode)
 
      call pc_get ('hdr_inL'   , obj%hdr_inL)
      call pc_get ('inL_init'  , obj%inL_init)   
      call pc_get ('inL_inc'   , obj%inL_inc) 
      call pc_get ('inL_wid'   , obj%inL_wid)  
      call pc_get ('inL_last'  , obj%inL_last)   
      call pc_get ('inL_tot'   , obj%inL_tot)   

      call pc_get ('hdr_crl'  , obj%hdr_crl)
      call pc_get ('crl_init' , obj%crl_init)  
      call pc_get ('crl_inc'  , obj%crl_inc) 
      call pc_get ('crl_wid'  , obj%crl_wid)   
      call pc_get ('crl_last' , obj%crl_last)   
      call pc_get ('crl_tot'  , obj%crl_tot)   

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if(obj%mode /= 'CALC' .and. obj%mode /= 'APPLY') then
        call pc_error('Invalid in RESTH: MODE MUST BE CALC OR APPLY')
      end if

      if(obj%hdr_inL < 1) then 
        call pc_error('HDR_INL must be >= 1')  
      else if(obj%hdr_inL > obj%nwih) then 
        call pc_error('HDR_INL must be <= 64 ')  
      end if

      if(obj%inL_inc <= 0.0) then 
        call pc_error('INL_INC must be > 0')  
      end if

      if(obj%inL_wid <= 0.0) then 
        call pc_error('INL_WID MUST BE > 0')  
      end if

      if(obj%inL_tot <= 0) then 
        call pc_error('INL_TOT must be > 0')  
      end if

      if(obj%hdr_crl < 1) then 
        call pc_error('HDR_CRL must be >= 1')  
      else if(obj%hdr_crl > obj%nwih) then 
        call pc_error('HDR_CRL must be <= 64 ')  
      end if

      if(obj%crl_inc <= 0.0) then 
        call pc_error('CRL_INC must be > 0.0')  
      end if

      if(obj%crl_wid <= 0.0) then 
        call pc_error('CRL_WID MUST BE > 0')  
      end if

      if(obj%crl_tot <= 0) then 
        call pc_error('CRL_TOT must be > 0')  
      end if

       istatus = pattern_stop2('RESTH:', verify, &
       obj%inL_init, obj%inL_inc, obj%inL_last, obj%inL_tot, &
       'INL_INIT', 'INL_INC', 'INL_LAST', 'INL_TOT', &
       pc_verify_scalar('INL_INIT'), pc_verify_scalar('INL_INC'), &
       pc_verify_scalar('INL_LAST'), pc_verify_scalar('INL_TOT')) 

       istatus = pattern_stop2('RESTH:', verify, &
       obj%crl_init, obj%crl_inc, obj%crl_last, obj%crl_tot, &
       'CRL_INIT', 'CRL_INC', 'CRL_LAST', 'CRL_TOT', &
       pc_verify_scalar('CRL_INIT'), pc_verify_scalar('CRL_INC'), &
       pc_verify_scalar('CRL_LAST'), pc_verify_scalar('CRL_TOT')) 
   

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('mode', mode_options,   &
                                   mode_noptions)

      call pc_put ('hdr_inL',   obj%hdr_inL)
      call pc_put ('inL_init',  obj%inL_init)
      call pc_put ('inL_inc',   obj%inL_inc)
      call pc_put ('inL_wid'   , obj%inL_wid)
      call pc_put ('inL_last',  obj%inL_last)
      call pc_put ('inL_tot',   obj%inL_tot)

      call pc_put ('mode'    ,  obj%mode)
      call pc_put ('hdr_crl',  obj%hdr_crl)
      call pc_put ('crl_init', obj%crl_init) 
      call pc_put ('crl_inc',  obj%crl_inc)
      call pc_put ('crl_wid'  , obj%crl_wid)
      call pc_put ('crl_last', obj%crl_last)
      call pc_put ('crl_tot',  obj%crl_tot)

      obj%nhead = n_headers
      obj%nxy   = obj%inL_tot*obj%crl_tot 
      nstore    = obj%nhead*obj%nxy 
      nscratch  = n_headers
 
        need_label   = 'NO'
        need_request = 'NO'
        twosets      = 'NO' 
        call pc_put_control ('nstore',             nstore)
        call pc_put_control ('nscratch',         nscratch)
        call pc_put_control ('need_label',     need_label)
        call pc_put_control ('need_request', need_request)
        call pc_put_control ('twosets',           twosets)
 
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!! Conditionally deallocate all arrays like this:

      if (associated(resth_hds)) deallocate (resth_hds)   

!! Initialize all variables (except allocated arrays) needed for execution

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if (obj%crl_tot == 1) then 
        obj%crl_tot = 1 
        obj%crl_inc = const 
      end if

      allocate(resth_hds(nstore),stat=ier)
      if (ier /= 0) call pc_error ('Resth: allocate error of HDS')
      resth_hds = 0
      do jr = 1, obj%nxy 
        resth_hds(obj%nhead*(jr-1)+1) = float(obj%ndpt) 
      end do 
      resth_mode = obj%mode

      if (pc_do_not_process_traces()) return

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine resth_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine resth (obj,ntr,hd,tr)
      implicit none
      type(resth_struct)              :: obj                    ! arguments
      integer         ,intent(inout)  :: ntr                    ! arguments
      double precision,intent(inout)  :: hd(:,:)                ! arguments
      real            ,intent(inout)  :: tr(:,:)                ! arguments
          
      real             :: hdt(obj%nhead)                        ! local
      integer          :: j,jr,k,kx,ky                          ! local

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR .or. ntr > 1) then
           call resth_wrapup (obj)
      end if 

      if (resth_mode == 'CALC') then
         object%build = .true.
      else 
         object%build = .false.
      end if

      ! compute the locations of the headers

      if (obj%build .and. ntr>0) then 
        do k = 1, ntr                               
          kx = nint((hd(obj%hdr_inL,k)-obj%inL_init)/obj%inL_inc)
          ky = nint((hd(obj%hdr_crl,k)-obj%crl_init)/obj%crl_inc)

          if (kx<0 .or. ky<0 .or. kx>=obj%inL_tot .or. ky>=obj%crl_tot   &
            .or. abs(obj%inL_init+kx*obj%inL_inc-hd(obj%hdr_inL,k)) >    &
                obj%inL_wid                                              & 
            .or. abs(obj%crl_init+ky*obj%crl_inc-hd(obj%hdr_crl,k)) >    & 
                obj%crl_inc) cycle  

          jr = ky*obj%inL_tot + kx 
          do j = 1, obj%nhead 
            hdt(j) = resth_hds(obj%nhead*jr+j)  
          end do 
   
        !  store the header values

         if (nint(hdt(2)) == 0) then
            hdt(1) = hd(2,k)  
            hdt(3) = hd(56,k) 
            hdt(4) = hd(19,k) 
            hdt(5) = hd(37,k) 
            hdt(6) = hd(38,k) 
            hdt(7) = hd(39,k) 
            hdt(8) = hd(40,k) 
            hdt(9) = hd(41,k) 
            hdt(10) = hd(42,k) 
            hdt(11) = hd(43,k) 
            hdt(12) = hd(57,k) 
            hdt(13) = hd(44,k) 
            hdt(14) = hd(45,k)
            resth_hds(obj%nhead*jr+1) = hdt(1) 
            do j = 3, obj%nhead
              resth_hds(obj%nhead*jr+j) = hdt(j)  
            end do 
          endif 

          hdt(2) = hdt(2) + 1. 
          resth_hds(obj%nhead*jr+2) = hdt(2)
          if ( hdt(2) > 1) then
            hdt(1) = min( hdt(1),real(hd(2,k)))
            resth_hds(obj%nhead*jr+1) = hdt(1)
          end if               
        end do 
      else if (ntr == 0) then 
        obj%build = .false. 
      else 

! reassign the header values

        do k = 1, ntr 
          kx = nint((hd(obj%hdr_inL,k)-obj%inL_init)/obj%inL_inc)
          ky = nint((hd(obj%hdr_crl,k)-obj%crl_init)/obj%crl_inc) 
          if (kx<0 .or. ky<0 .or. kx>=obj%inL_tot .or. ky>=obj%crl_tot   &
            .or. abs(obj%inL_init+kx*obj%inL_inc-hd(obj%hdr_inL,k)) >    &
                obj%inL_wid                                              & 
            .or. abs(obj%crl_init+ky*obj%crl_inc-hd(obj%hdr_crl,k)) >    & 
                obj%crl_inc) cycle  

          jr = ky*obj%inL_tot + kx 
          do j = 1, obj%nhead
            hdt(j) = resth_hds(obj%nhead*jr+j)   
          end do 
          hd(2,k) = hdt(1) 
          hd(5,k) = hdt(2) 
          hd(56,k) = hdt(3) 
          hd(19,k) = hdt(4) 
          hd(37,k) = hdt(5) 
          hd(38,k) = hdt(6) 
          hd(39,k) = hdt(7) 
          hd(40,k) = hdt(8) 
          hd(41,k) = hdt(9) 
          hd(42,k) = hdt(10) 
          hd(43,k) = hdt(11) 
          hd(57,k) = hdt(12) 
          hd(44,k) = hdt(13) 
          hd(45,k) = hdt(14) 
        end do 
      endif

      return
      end subroutine resth 

!</execute_only>
  

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
 

!<execute_only>

      subroutine resth_wrapup (obj)
      implicit none
      type(resth_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine resth_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module resth_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

