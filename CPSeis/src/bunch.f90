!<CPS_v1 type="PROCESS"/>
!
!<-- This documentation header was last revised by CI Burch on 2001-07-03. />
!
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
! Name       : BUNCH
! Category   : sorts 
! Written    : 2001-07-13   by: Stephen Chiu
! Revised    : 2002-08-20   by: Karen Goodger
! Maturity   : production   2002-08-26
! Purpose    : Bunch traces in ensembles to improve parallel process I/O.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION                   
!
! If HONOR_HDR = NO, 
!
!     then BUNCH outputs gathers of traces containing NUM_TR_BUNCH traces
!     (except the last output gather may be smaller if there are fewer than 
!     NUM_TR_BUNCH traces remaining in the input datset).  
!
! If HONOR_HDR = YES, 
!
!     then BUNCH attempts to output gathers of traces with NUM_TR_BUNCH 
!     traces in each, however the number of traces in the output gather will
!     be reduced subject to these constraints.
!
!         1.  Output gathers of traces will honor header word HDR_TO_HONOR. 
!         That is, output gathers will not contain traces with more than one 
!         value in header word HDR_TO_HONOR.
!
!         2.  Output gathers of traces cannot exceed the number of traces 
!         remaining in the input datset.  
!
!  
! BUNCH will reset header words 3 and 4 and the globals GATHERED and NUMTR to
! insure that they are consistent with the output gathers.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
! Using BUNCH in Parallel Jobs
!
! BUNCH should be placed in the process sequence just before parallel blocks
! containing only single trace processes (such as IQ). This improves the trace
! I/O efficiency and allows the single trace parallel processes to run faster. 
! (See the ParallelCPS_Guide for more details.)
!
! BUNCH should not be used with parallel processes such as RMUL that require a
! geophysical gather for input.
!
! An UNGATHER process can be placed in the process sequence after the single
! trace parallel block.  The UNGATHER process will output traces one at a 
! time.
!
!
! Using BUNCH when Compressing Output
!
! The compression algorithm operates best when gathers input to it include only
! traces that are similar in character.  This means that such gathers should 
! not include changes from one geophysical gather to another.  For example, 3D
! pre-stack data in common offset order (sorted 6, 8, 7) may be prepared with
! BUNCH using HDR_TO_HONOR = 8 to insure that no gather that is output from
! BUNCH includes a transistion from one inline to another.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! No special requirements.  Traces may be input as gathers or as single traces.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process does not alter input traces.
! This process outputs the same traces as it receives.
!
! This process outputs trace ensembles.
!
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       may be changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! GATHERED  whether traces are a legitimate gather  set to true
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
!
!
! Header word 3 is used to recognize input gathers.
!
! Header words 3 and 4 will be reset to insure consistency with output gathers.
!
! If HONOR_HDR = YES, then output gathers will not contain traces with more
! than one value in header word HDR_TO_HONOR.
! 
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author       Description
!     ----        ------       -----------
! 5.  2002-08-26  Goodger      Make HDR_TO_HONOR insensitive when HONOR_HEADER
!                              equals NO.
! 4.  2002-08-16  Chiu         Add an option of HDR_TO_HONOR.
! 3.  2002-07-29  Chiu         Set gathered to true, requested by C.Emmons.
! 2.  2002-01-28  Chiu         Set NEED_REQUEST and NEED_LABEL to true
!                              and set gathered to false.
! 1.  2001-07-13  Chiu         Created original version.  
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
! TWOSETS        false     whether this process needs two trace/header arrays.
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
!
!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS         
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>
!
!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! 
!
!-------------------------------------------------------------------------------
!</programming_doc>
!
!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!<gui_def>
!<NS BUNCH Process/NC=80>
!
!      Output trace ensembles to improve single trace parallel process I/O.
!
!
!
!      NUM_TR_BUNCH =`IIIIIII     HONOR_HDR =`CCC     HDR_TO_HONOR =`II
!
!</gui_def>
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="NUM_TR_BUNCH">
!<Tip> Maximum number of traces to put in each output trace gather. </Tip>
! Default = 1
! Allowed = int >= 1
!</Help>
!
!<Help KEYWORD="HONOR_HDR">
!<Tip> Whether output gathers should honor header word HDR_TO_HONOR. </Tip>
! Default = NO
! Allowed = YES/NO
!
! If HONOR_HDR = NO, 
!
!     then BUNCH outputs gathers of traces containing NUM_TR_BUNCH traces
!     (except the last output gather may be smaller if there are fewer than 
!     NUM_TR_BUNCH traces remaining in the input datset).   
!
! If HONOR_HDR = YES, 
!
!     then BUNCH attempts to output gathers of traces with NUM_TR_BUNCH 
!     traces in each, however the number of traces in the output gather will
!     be reduced subject to these constraints.
!
!         1.  Output gathers of traces will honor header word HDR_TO_HONOR. 
!         That is, output gathers will not contain traces with more than one 
!         value in header word HDR_TO_HONOR.
!
!         2.  Output gathers of traces cannot exceed the number of traces 
!         remaining in the input datset.    
!</Help>
!
!<Help KEYWORD="HDR_TO_HONOR">
!<Tip> Header word to honor if HONOR_HDR = YES. </Tip>
! Default = 3
! Allowed = 1 - NWIH
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
!
! NOTES FOR CONVERSION PROGRAMMER
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module bunch_module

      use pc_module
      use named_constants_module
      use gathr_module

      implicit none

      private
      public :: bunch_create
      public :: bunch_initialize
      public :: bunch_update
      public :: bunch_delete

!<execute_only>

      public :: bunch
      public :: bunch_wrapup

!</execute_only>

      character(len=100),public,save :: BUNCH_IDENT = &
'$Id: bunch.f90,v 1.5 2002/08/22 15:38:24 Goodger prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: bunch_struct

      private
      logical              :: skip_wrapup         ! for wrapup routine

      character (len=3)    :: honor_hdr           ! process parameters
      integer              :: num_tr_bunch        ! process parameters
      integer              :: hdr_to_honor        ! process parameters.      

      logical              :: done                ! dependent variables 
      integer              :: gathr_cnt           ! dependent variables
      
      type (gathr_struct), pointer  :: gathr_obj  ! dependent variables

      end type bunch_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(bunch_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: honor_hdr_noptions = 2
      character(len=3),save :: honor_hdr_options(honor_hdr_noptions)
      data honor_hdr_options/'YES','NO '/

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine bunch_create (obj)
      implicit none
      type(bunch_struct),pointer :: obj              ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers

      nullify  (obj%gathr_obj)         
      call bunch_initialize (obj)

      return
      end subroutine bunch_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine bunch_delete (obj)
      implicit none
      type(bunch_struct),pointer :: obj       ! arguments

!<execute_only>
      call bunch_wrapup (obj)
!</execute_only>

      if (associated(obj%gathr_obj))  call gathr_delete (obj%gathr_obj) 
      deallocate(obj)

      return
      end subroutine bunch_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine bunch_initialize (obj)
      implicit none
      type(bunch_struct),intent(inout) :: obj       ! arguments

      obj%num_tr_bunch  = 1
      obj%honor_hdr     = 'NO'
      obj%hdr_to_honor  = 3
      
      call bunch_update (obj)

      return
      end subroutine bunch_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine bunch_update (obj)
      implicit none
      type(bunch_struct),intent(inout),target :: obj      ! Arguments

      logical            :: gathered                      ! local
      integer            :: n_max, nwih                   ! local
      integer            :: nstore, nscratch              ! local

      object => obj                               ! needed for traps.

      obj%skip_wrapup = .true.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

! Now retrieve user paramerters

      call pc_get_global ('NWIH'     ,  nwih)
      
      call pc_get ('NUM_TR_BUNCH'    , obj%num_tr_bunch)
      call pc_get ('honor_hdr'       , obj%honor_hdr)
      call pc_get ('hdr_to_honor'    , obj%hdr_to_honor)      

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%honor_hdr.eq.'YES')then
        call pc_put_sensitive_field_flag('HDR_TO_HONOR',.true.)
      else
        call pc_put_sensitive_field_flag('HDR_TO_HONOR',.false.)
        obj%hdr_to_honor=3
      endif
      if(obj%num_tr_bunch < 1) then 
        call pc_error('NUM_TR_BUNCH Must Be >= 1')  
      end if

      if(obj%hdr_to_honor < 1 .or. obj%hdr_to_honor > nwih  ) then 
        call pc_error('HDR_TO_HONOR Must Be >= 1 and <= NWIH')  
      end if
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('honor_hdr', honor_hdr_options,   &
                                  honor_hdr_noptions)

      gathered = .true.
      
      call pc_put ('NUM_TR_BUNCH'    , obj%num_tr_bunch)
      call pc_put ('honor_hdr'       , obj%honor_hdr)
      call pc_put ('hdr_to_honor'    , obj%hdr_to_honor)
      
      call pc_put_global ('numtr'    , obj%num_tr_bunch) ! if changed.
      call pc_put_global ('gathered' , gathered)         ! if changed.
 
! Determine memory usage

      nstore =  0
      nscratch  = 0


      call pc_put_control ('NSTORE', nstore)
      call pc_put_control ('NSCRATCH', nscratch)
      call pc_put_control ('NEED_REQUEST' , .true.)
      call pc_put_control ('NEED_LABEL' ,   .true.)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%gathr_cnt = 0
      
      n_max = 9999
      if(obj%honor_hdr == "YES") then
        call gathr_create(obj%gathr_obj, n_max, obj%num_tr_bunch, .true., &
                           obj%hdr_to_honor)
      else
        call gathr_create(obj%gathr_obj, n_max, obj%num_tr_bunch, .false.,&
                           obj%hdr_to_honor)
      endif

      obj%done = .false.
 
      if (pc_do_not_process_traces()) return ! In case of allocation errors

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine bunch_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine bunch (obj,ntr,hd,tr)
      implicit none
      type(bunch_struct),intent(inout) :: obj              ! arguments
      integer             ,intent(inout) :: ntr            ! arguments
      double precision    ,intent(inout) :: hd(:,:)        ! arguments
      real                ,intent(inout) :: tr(:,:)        ! arguments

      integer             :: itr                           ! local

!----------------------------------------------------------------
! Loop over traces
 
      if (ntr == FATAL_ERROR ) then
        call pc_error('FATAL_ERROR in routine BUNCH ') 
        return
      end if  

      if (obj%done) then
        ntr = NO_MORE_TRACES
        call bunch_wrapup (obj)
        return
      end if
  
      call gathr(obj%gathr_obj, ntr, hd, tr)
      
      if ( ntr >= 1) then
         obj%gathr_cnt = obj%gathr_cnt + 1
         do itr = 1, ntr
           hd(3,itr) = obj%gathr_cnt 
           hd(4,itr) = itr
         end do
      end if
      
      if (ntr == NO_MORE_TRACES) then
         obj%done = .true.
       end if
 
      return
      end subroutine bunch

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine bunch_wrapup (obj)
      implicit none
      type(bunch_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine bunch_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module bunch_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
