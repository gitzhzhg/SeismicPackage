!<CPS_v1 type="PROCESS"/>
!!--------------------------- pairmerge.f90 ---------------------------------!!
!!--------------------------- pairmerge.f90 ---------------------------------!!
!!--------------------------- pairmerge.f90 ---------------------------------!!


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
! Name       : PAIRMERGE
! Category   : miscellaneous
! Written    : 2001-09-21   by: Tom Stoeckley
! Revised    : 2001-09-21   by: Tom Stoeckley
! Maturity   : production   2001-12-13
! Purpose    : Merge a pair of traces into a single trace.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This process merges a pair of traces into a single trace by setting each
! sample of the output trace to a scaled version of the input sample of
! one of the two input traces, using a specified criterion for choosing
! which input trace sample to use.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
! Here is a summary of how the input trace amplitudes will map to the
! output trace:
!
!    Trace 1 amplitudes:  MIN_VALUE_ONE  maps to  -1.0
!    Trace 1 amplitudes:  MAX_VALUE_ONE  maps to   0.0
!
!    Trace 2 amplitudes:  MIN_VALUE_TWO  maps to   0.0
!    Trace 2 amplitudes:  MAX_VALUE_ONE  maps to  +1.0
!
!    If the original trace 2 amplitude exceeds CUTOFF_VALUE_TWO,
!    the output trace will receive the mapped value from trace 2.
!    Otherwise the output trace will receive the mapped value from trace 1.
!
!
! To use this process to merge EDA3D traces with the original traces to
! produce an enhanced version of an EDA (edge detection attribute) 3D data
! volume, let the first trace be the original data trace and the second
! trace be the EDA trace, and then set the parameters as follows:
!
!          MIN_VALUE_ONE = minimum original data trace amplitude.
!          MAX_VALUE_ONE = maximum original data trace amplitude.
!
!          MIN_VALUE_TWO = approximately 0.2
!          MAX_VALUE_TWO = 1.0
!
!       CUTOFF_VALUE_TWO = the same value as MIN_VALUE_TWO.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a single-trace process.
! This process requires traces to be input in pairs.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! This process outputs one trace at a time.
! Output traces will have amplitudes between -1 and +1.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! GATHERED  whether traces are a legitimate gather  set to false
! NDPT      number of sample values in trace        used but not changed
!
! The input value of NUMTR must be 2.
! The input value of GATHERED is irrelevant.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! The output trace contains the same header word values as the first trace
! of each pair of input traces, except for header word 1, which gets reset.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  2.
!  1. 2001-12-13  Stoeckley  Initial version.
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR == 2              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
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


!-------------------------------------------------------------------------------
!<gui_def>
!<NS PAIRMERGE Process/NC=80>
!
!                        Merge a Pair of Traces
!
! MIN_VALUE_ONE~~~=`FFFFFFFF [/L](negative of MAX_VALUE_ONE for edge detection)
! MAX_VALUE_ONE~~~=`FFFFFFFF [/L](maximum data trace value for edge detection)
!
! MIN_VALUE_TWO~~~=`FFFFFFFF [/L](between 0.0 and 1.0 for edge detection)
! MAX_VALUE_TWO~~~=`FFFFFFFF [/L](1.0 for edge detection)
!
! CUTOFF_VALUE_TWO=`FFFFFFFF [/L](same as MIN_VALUE_TWO for edge detection)
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="min_value_one">
!<Tip> Minimum sample value to keep from trace 1. </Tip>
! Default = -1.0    
! Allowed = any value
!
! The sample values from trace 1 will be linearly scaled so that input
! values from MIN_VALUE_ONE to MAX_VALUE_ONE will be mapped to values from
! -1.0 to 0.0 for possible placing on the output trace.
!
! Any sample values from trace 1 which are less than MIN_VALUE_ONE will
! be treated as if they are equal to MIN_VALUE_ONE.
!
! For edge detection, set this value to the minimum original data value.
!</Help>
!
!
!<Help KEYWORD="max_value_one">
!<Tip> Maximum sample value to keep from trace 1. </Tip>
! Default = +1.0    
! Allowed = any value > MIN_VALUE_ONE
!
! The sample values from trace 1 will be linearly scaled so that input
! values from MIN_VALUE_ONE to MAX_VALUE_ONE will be mapped to values from
! -1.0 to 0.0 for possible placing on the output trace.
!
! Any sample values from trace 1 which are greater than MAX_VALUE_ONE will
! be treated as if they are equal to MAX_VALUE_ONE.
!
! For edge detection, set this value to the maximum original data value.
!</Help>
!
!
!<Help KEYWORD="min_value_two">
!<Tip> Minimum sample value to keep from trace 2. </Tip>
! Default = -1.0    
! Allowed = any value
!
! The sample values from trace 2 will be linearly scaled so that input
! values from MIN_VALUE_TWO to MAX_VALUE_TWO will be mapped to values from
! 0.0 to +1.0 for possible placing on the output trace.
!
! Any sample values from trace 2 which are less than MIN_VALUE_TWO will
! be treated as if they are equal to MIN_VALUE_TWO.
!
! For edge detection, set this value to a number between 0.0 and 1.0 (for
! example 0.2).
!</Help>
!
!
!<Help KEYWORD="max_value_two">
!<Tip> Maximum sample value to keep from trace 2. </Tip>
! Default = +1.0    
! Allowed = any value > MIN_VALUE_TWO
!
! The sample values from trace 2 will be linearly scaled so that input
! values from MIN_VALUE_TWO to MAX_VALUE_TWO will be mapped to values from
! 0.0 to +1.0 for possible placing on the output trace.
!
! Any sample values from trace 2 which are greater than MAX_VALUE_TWO will
! be treated as if they are equal to MAX_VALUE_TWO.
!
! For edge detection, set this value to 1.0.
!</Help>
!
!
!<Help KEYWORD="cutoff_value_two">
!<Tip> Cutoff sample value from trace 2. </Tip>
! Default = 0.0    
! Allowed = any value between MIN_VALUE_TWO and MAX_VALUE_TWO
!</Help>
!
! If the sample value from trace 2 is >= this value, the scaled value from
! trace two will be placed onto the output trace.
!
! If the sample value from trace 2 is < this value, the scaled value from
! trace one will be placed onto the output trace.
!
! For edge detection, set this value to MIN_VALUE_TWO.
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module pairmerge_module
      use pc_module
      use named_constants_module
      use mth_module
      implicit none
      private
      public :: pairmerge_create
      public :: pairmerge_initialize
      public :: pairmerge_update
      public :: pairmerge_delete
!<execute_only>
      public :: pairmerge   
      public :: pairmerge_wrapup
!</execute_only>


      character(len=100),public,save :: PAIRMERGE_IDENT = &
'$Id: pairmerge.f90,v 1.1 2001/12/12 18:17:56 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: pairmerge_struct              
 
        private
        logical        :: skip_wrapup                  ! wrapup flag.
        integer        :: ndpt                         ! globals.  
        real           :: min_value_one                ! process parameters.
        real           :: max_value_one                ! process parameters.
        real           :: min_value_two                ! process parameters.
        real           :: max_value_two                ! process parameters.
        real           :: cutoff_value_two             ! process parameters.

      end type pairmerge_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(pairmerge_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine pairmerge_create (obj)
      implicit none
      type(pairmerge_struct),pointer :: obj       ! arguments

      allocate (obj)
      call pairmerge_initialize (obj)
      return
      end subroutine pairmerge_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine pairmerge_delete (obj)
      implicit none
      type(pairmerge_struct),pointer :: obj       ! arguments

!<execute_only>
      call pairmerge_wrapup (obj)
!</execute_only>
      deallocate(obj)
      return
      end subroutine pairmerge_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine pairmerge_initialize (obj)
      implicit none
      type(pairmerge_struct),intent(inout) :: obj       ! arguments

      obj%min_value_one    = -1.0
      obj%max_value_one    =  1.0
      obj%min_value_two    = -1.0
      obj%max_value_two    =  1.0
      obj%cutoff_value_two =  0.0

      call pairmerge_update (obj)
      return
      end subroutine pairmerge_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine pairmerge_update (obj)
      implicit none
      type(pairmerge_struct),intent(inout),target :: obj         ! arguments
      integer                                     :: numtr       ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_get_global ('numtr'   , numtr)
      call pc_get_global ('ndpt'    , obj%ndpt)

      call pc_get        ('min_value_one'   , obj%min_value_one   )
      call pc_get        ('max_value_one'   , obj%max_value_one   )
      call pc_get        ('min_value_two'   , obj%min_value_two   )
      call pc_get        ('max_value_two'   , obj%max_value_two   )
      call pc_get        ('cutoff_value_two', obj%cutoff_value_two)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (numtr /= 2) then
           call pc_error ('global NUMTR is',numtr,'when it should be 2')
           call pc_error ('input traces must be received in pairs')
      end if

      if (obj%max_value_one <= obj%min_value_one) &
        call pc_error ('MAX_VALUE_ONE must be greater than MIN_VALUE_ONE')

      if (obj%max_value_two <= obj%min_value_two) &
        call pc_error ('MAX_VALUE_TWO must be greater than MIN_VALUE_TWO')

      if (obj%cutoff_value_two < obj%min_value_two .or. &
          obj%cutoff_value_two > obj%max_value_two) &
        call pc_error &
         ('CUTOFF_VALUE_TWO must be between MIN_VALUE_TWO and MAX_VALUE_TWO')


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!



!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_global ('numtr'       , 1)
      call pc_put_global ('gathered'    , .false.)

      call pc_put        ('min_value_one'   , obj%min_value_one   )
      call pc_put        ('max_value_one'   , obj%max_value_one   )
      call pc_put        ('min_value_two'   , obj%min_value_two   )
      call pc_put        ('max_value_two'   , obj%max_value_two   )
      call pc_put        ('cutoff_value_two', obj%cutoff_value_two)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine pairmerge_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

      subroutine pairmerge (obj,ntr,hd,tr)
      implicit none
      type(pairmerge_struct),intent(inout) :: obj              ! arguments
      integer               ,intent(inout) :: ntr              ! arguments
      double precision      ,intent(inout) :: hd(:,:)          ! arguments
      real                  ,intent(inout) :: tr(:,:)          ! arguments
      integer                              :: itrace,indx      ! local
      real                                 :: value1,value2    ! local

      if (ntr == NO_MORE_TRACES) then
           call pairmerge_wrapup (obj)
           return
      end if
      
      if (ntr /= 2) then
           call pc_error &
             ('PAIRMERGE: received',ntr,'traces when 2 traces were expected')
           call pairmerge_wrapup (obj)
           ntr = FATAL_ERROR
           return
      end if
      
      do indx = 1,obj%ndpt
           value1 = tr(indx,1)
           value2 = tr(indx,2)

           call mth_constrain (value1, obj%min_value_one, obj%max_value_one)
           call mth_constrain (value2, obj%min_value_two, obj%max_value_two)

           if (value2 > obj%cutoff_value_two) then
                tr(indx,1) = (value2 - obj%min_value_two) / &
                                (obj%max_value_two - obj%min_value_two)
           else
                tr(indx,1) = (value1 - obj%min_value_one) / &
                                (obj%max_value_one - obj%min_value_one) - 1.0
           end if
      end do

      itrace = nint(hd(1,1))
      hd(1,1) = (itrace + 1) / 2
      ntr = 1
      return
      end subroutine pairmerge

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine pairmerge_wrapup (obj)
      implicit none
      type(pairmerge_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.
      return
      end subroutine pairmerge_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module pairmerge_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

