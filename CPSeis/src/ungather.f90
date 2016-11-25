!<CPS_v1 type="PROCESS"/>
!!----------------------------- ungather.f90 --------------------------------!!
!!----------------------------- ungather.f90 --------------------------------!!
!!----------------------------- ungather.f90 --------------------------------!!


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
!                        C P S   P R O C E S S
!
! Name       : UNGATHER
! Category   : sorts
! Written    : 1999-09-10   by: Tom Stoeckley
! Revised    : 2001-03-14   by: Tom Stoeckley
! Maturity   : production   2001-05-15
! Purpose    : A simple process to ungather traces.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                   
!
! The UNGATHER process takes input traces and outputs them individually, it has
! no effect on the order of traces.  Input traces may be either gathered or
! ungathered.  UNGATHER has no parameters.
!
! UNGATHER can be called either from a processing system or a process module.
! There is a companion process called GATHER for gathering traces.
!
! If a process module requires input traces to be ungathered, it can simply
! contain code like the following:
!
!      call pc_get_global  ('numtr', numtr)
! 
!      if (numtr > 1) then
!           call pc_error ('this process must be preceded by an ungather')
!      end if
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS                
!
! Process is a multiple-trace process.
!
! Input traces can be gathered in any unspecified manner, or ungathered.
! This algorithm works even if the input traces are already ungathered,
! but it would be more efficient to eliminate calling this process in such
! circumstances.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS              
!
! This process does not alter input traces.
! This process outputs traces individually.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED           
!
! Name       Description                            Action taken
! ----       -----------                            ------------
! NWIH       number of words in trace header        used but not changed
! NDPT       number of sample values in trace       used but not changed
! NUMTR      max number of traces input/output      set to 1
! GATHERED   whether traces are gathered on hwd 3   set to false
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED      
!
! No trace header words are used or changed.
! The values of trace header words are irrelevant to this process.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
! 11. 2001-05-15  Stoeckley  Add missing bracket to header_word_doc.
! 10. 2000-12-08  Stoeckley  Change wrapup flag.
!  9. 2000-10-13  Stoeckley  Add missing context-sensitive help.
!  8. 2000-10-06  Stoeckley  Add missing required documentation sections.
!  7. 2000-04-10  Stoeckley  Add GUI definition section; change to be setup
!                             only when ungathering is not needed.
!  6. 2000-03-10  Stoeckley  Fix pc_put_global to reset GATHERED to false.
!  5. 2000-01-28  Stoeckley  Add checks on input global values.
!  4. 2000-01-07  Stoeckley  Add wrapup flag. 
!  3. 1999-12-21  Stoeckley  Incorporate documentation supplied by Chuck I.
!                             Burch.
!  2. 1999-11-17  Stoeckley  Add ident string for RCS.
!  1. 1999-09-10  Stoeckley  Initial version.
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
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! SETUP_ONLY is set to true if the ungather could have been omitted.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR == 1              if this process is outputting a trace.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == NEED_TRACES    if this process needs more traces.
!
! NTR >      1      is never output by this process.
! NTR = FATAL_ERROR is never output by this process.
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
!<NS UNGATHER Process/NC=72>
!
!                        Ungather the Traces
!
!Note: `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS Note[message/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="MESSAGE">
!<Tip> Information about the gathering status. </Tip>
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module ungather_module
      use pc_module
      use named_constants_module
      implicit none
      private
      public :: ungather_create     ! uses the parameter cache.
      public :: ungather_initialize
      public :: ungather_update     ! uses the parameter cache.
      public :: ungather_delete
!<execute_only>
      public :: ungather            ! main execution (trace processing) routine.
      public :: ungather_wrapup
!</execute_only>

      character(len=100),public,save :: UNGATHER_IDENT = &
       '$Id: ungather.f90,v 1.11 2001/05/15 18:28:20 sps prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: ungather_struct              
           private
           logical :: skip_wrapup         ! wrapup flag.
           integer :: nwih                ! global parameter.
           integer :: ndpt                ! global parameter.
           integer :: ntrkeep             ! dependent variable.
           integer :: kount               ! dependent variable.
      end type ungather_struct


!!------------------------- interfaces -----------------------------------!!
!!------------------------- interfaces -----------------------------------!!
!!------------------------- interfaces -----------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(ungather_struct),pointer,save :: object      ! needed for traps.

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine ungather_create (obj)
      implicit none
      type(ungather_struct),pointer :: obj       ! arguments

      allocate (obj)
      call ungather_initialize (obj)
      return
      end subroutine ungather_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ungather_delete (obj)
      implicit none
      type(ungather_struct),pointer :: obj       ! arguments

!<execute_only>
      call ungather_wrapup (obj)
!</execute_only>

      deallocate(obj)
      return
      end subroutine ungather_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine ungather_initialize (obj)
      implicit none
      type(ungather_struct),intent(inout) :: obj       ! arguments

      obj%nwih        = 0
      obj%ndpt        = 0
      obj%ntrkeep     = 0
      obj%kount       = 0
      call ungather_update (obj)
      return
      end subroutine ungather_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

!!! Decide later whether to call PC_INFO and/or PC_PUT_GUI_ONLY in this
!!! routine.  If PC_PUT_GUI_ONLY is called, the XML should set the label
!!! for the 'message' keyword to blank.


      subroutine ungather_update (obj)
      implicit none
      type(ungather_struct),intent(inout),target :: obj       ! arguments
      integer                                    :: numtr     ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      numtr = 0

      call pc_get_global  ('nwih' , obj%nwih)
      call pc_get_global  ('ndpt' , obj%ndpt)
      call pc_get_global  ('numtr'   , numtr)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if (obj%nwih <= 0 .or. obj%ndpt <= 0) then
        call pc_error ('The NWIH and NDPT globals should be > 0')
      end if

      if (.not.pc_global_keyword_present('numtr')) then
        call pc_error ('The NUMTR global is not set by any previous process')
      else if (numtr <= 0) then
        call pc_error ('The NUMTR global is',numtr,'but should be >= 1')
      end if

      if (numtr == 1) then
           call pc_put_gui_only ('message',  &
             'This UNGATHER is not needed since traces are already ungathered.')
           call pc_info  &
            ('This UNGATHER is not needed since traces are already ungathered.')
      else
           call pc_put_gui_only ('message',  &
                                  'This UNGATHER process has no parameters.')
      end if


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_global  ('numtr'       , 1)
      call pc_put_global  ('gathered'    , .false.)
      call pc_put_control ('need_request', .true.)
      call pc_put_control ('need_label'  , .true.)
      call pc_put_control ('twosets'     , .true.)
      call pc_put_control ('setup_only'  , numtr == 1)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%ntrkeep = 0
      obj%kount   = 0

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine ungather_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!----------------------- main execution -----------------------------------!!
!!----------------------- main execution -----------------------------------!!
!!----------------------- main execution -----------------------------------!!

!<execute_only>

      subroutine ungather (obj, ntr,hdi,tri,hdo,tro)
      implicit none
      type(ungather_struct),intent(inout) :: obj       ! arguments
      integer              ,intent(inout) :: ntr       ! arguments
      double precision     ,intent(in)    :: hdi(:,:)  ! arguments
      real                 ,intent(in)    :: tri(:,:)  ! arguments
      double precision     ,intent(inout) :: hdo(:,:)  ! arguments
      real                 ,intent(inout) :: tro(:,:)  ! arguments

      if (ntr >= 1) then
           obj%ntrkeep = ntr
           obj%kount   = 1
           hdo(1:obj%nwih, 1) = hdi(1:obj%nwih, obj%kount)
           tro(1:obj%ndpt, 1) = tri(1:obj%ndpt, obj%kount)
           ntr = 1
      else if (ntr == NEED_TRACES) then
           if (obj%kount < obj%ntrkeep) then
                obj%kount = obj%kount + 1
                hdo(1:obj%nwih, 1) = hdi(1:obj%nwih, obj%kount)
                tro(1:obj%ndpt, 1) = tri(1:obj%ndpt, obj%kount)
                ntr = 1
           end if
      end if

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
           call ungather_wrapup (obj)
      end if
      return
      end subroutine ungather

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine ungather_wrapup (obj)
      implicit none
      type(ungather_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      !!! nothing to do here.

      return
      end subroutine ungather_wrapup

!</execute_only>


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module ungather_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

