!<CPS_v1 type="PROCESS"/>
!!------------------------------- randgth.f90 --------------------------------!!
!!------------------------------- randgth.f90 --------------------------------!!
!!------------------------------- randgth.f90 --------------------------------!!
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
! Name       : RANDGTH ( Randomize traces within a gather)
! Category   : miscellaneous
! Written    : 2005-09-20   by: Stephen Chiu
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Randomize traces within a gather.
! Portability: No known limitations. 
! Parallel   : YES. 
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! For each input gather, the order of input traces is randomized. 
! After the RANDGTH process, the output gather contains the randomized traces.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
! Gathered trace input is required.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters the order of input traces. 
! This process outputs the same number of traces as the input gather.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------       
! GATHERED  whether traces are a legitimate gather  used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        changed
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2007-11-29  Stoeckley  Remove unused reference to memman.
!  1. 2006-01-03  S.chiu     Initial version.
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
!
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.
! NSTORE           0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
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
!                   ALTERNATE INTERNAL CALLING METHODS
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS RANDGTH Process/NC=80>
!
!          Randomize traces within a gather: No input parameter
!
!</gui_def>                   
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module randgth_module
      use pc_module
      use named_constants_module
      use grid_module            ! if you need the grid transformation.
      Use sort_module

      implicit none
      private
      public :: randgth_create
      public :: randgth_initialize
      public :: randgth_update
      public :: randgth_delete
      public :: randgth            ! main trace processing routine.
      public :: randgth_wrapup

      character(len=100),public,save :: RANDGTH_IDENT = &
'$Id: randgth.f90,v 1.2 2007/11/30 13:55:19 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: randgth_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        logical                    :: gathered ! whether properly gathered.
        integer                    :: nwih     ! number of header words.
        integer                    :: ndpt     ! number of trace samples.

        integer                    :: iseed         ! dependent variables

        integer                    :: print_lun     ! dependent variables.

      end type randgth_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      integer                  ,save :: lunprint  ! unit number for printing.
      type(randgth_struct),pointer,save :: object    ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine randgth_create (obj)
      type(randgth_struct),pointer :: obj       ! arguments
      integer                   :: ier         ! for error checking

      allocate (obj, stat=ier)
      if (ier /= 0) call pc_error ("Unable to allocate obj in randgth_create")

      call randgth_initialize (obj)

      lunprint = pc_get_lun()

      end subroutine randgth_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine randgth_delete (obj)
      type(randgth_struct),pointer :: obj       ! arguments
      integer                   :: ierr      ! for error checking

      call randgth_wrapup (obj)

      deallocate(obj, stat=ierr)
      if (ierr /= 0) call pc_warning  &
              ("Unable to deallocate obj in randgth_delete")

      end subroutine randgth_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine randgth_initialize (obj)
      type(randgth_struct),intent(inout) :: obj       ! arguments


      obj%print_lun = pc_get_lun()

      call randgth_update (obj)
      end subroutine randgth_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine randgth_update (obj)
      type(randgth_struct),intent(inout),target :: obj             ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)
      call pc_get_global ('gathered',obj%gathered) ! whether properly gathered.

!------------Check that globals are set:

      if (obj%nwih == inil) call pc_error ("NWIH global hasn't been set.")
      if (obj%ndpt == inil) call pc_error ("NDPT global hasn't been set.")
 
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


     if (.not. obj%gathered ) then
       call pc_warning (' RANDGTH warning: Require input to be gathered ')
     end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_global ('gathered', .true.)

      call pc_put_control('PARALLEL_SAFE'        ,.true.)
      call pc_put_control('PCPS_SEND_MODE'       ,'PCPS_SEND_FIRST_AVAIL')
      call pc_put_control('PCPS_RECEIVE_MODE'    ,'PCPS_RECEIVE_PASSTHRU')
      call pc_put_control('PCPS_BUNCH_MODE'      ,'PCPS_BUNCH_TRACE_GROUPS')
      call pc_put_control('PCPS_SEND_EOF_MODE'   ,'PCPS_SEND_ALL_EOF')
      call pc_put_control('PCPS_ALT_SEND_MODE'   ,'PCPS_SEND_ALL')
      call pc_put_control('PCPS_ALT_RECEIVE_MODE','PCPS_RECEIVE_ALL_EOF')
      call pc_put_control('PCPS_RESEQUENCE_MODE' ,'PCPS_RESEQUENCE_TRACES')
      call pc_put_control('PCPS_GENERATOR_MODE'  ,'PCPS_NO_TRACE_GEN')

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%iseed = 100               ! initial seed for random number

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.
  
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine randgth_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine randgth (obj, ntr, hdo, tro)
      type(randgth_struct),intent(inout) ::   obj                 ! arguments
      integer          ,intent(inout) ::   ntr                    ! arguments
      double precision ,intent(inout) ::   hdo(:,:)               ! arguments
      real             ,intent(inout) ::   tro(:,:)               ! arguments

      double precision  ::   hd_save(obj%nwih,ntr)                ! local
      real              :: trsave(obj%ndpt,ntr)                   ! local
      real              :: rnum(ntr)                              ! local
 
      integer           :: indx(ntr)                              ! local
      integer           :: itr                                    ! local


      if  ( (ntr== NO_MORE_TRACES )        & 
             .or. ntr == FATAL_ERROR )  then
         call randgth_wrapup (obj)
         return
      end if

      if ( .not. obj%gathered ) then    !  check GATHER mode 
          write(obj%print_lun, *) 'RANDGTH- requires gathered data' 
          ntr = FATAL_ERROR
          return
      endif

      if (ntr<=2 ) return

      ! Randomize one group of traces 

      hd_save(1:obj%nwih,1:ntr) = hdo(1:obj%nwih,1:ntr)
      trsave(1:obj%ndpt,1:ntr) = tro(1:obj%ndpt,1:ntr)

      do itr = 1, ntr
         indx(itr) = itr
         rnum(itr) = randgth_rand(obj%iseed)*10.
      end do

      call sort_qkisort (ntr, rnum, indx)

      do itr = 1, ntr
         tro(1:obj%ndpt,itr) = trsave(1:obj%ndpt,indx(itr))      
         hdo(1:obj%nwih,itr) = hd_save(1:obj%nwih,indx(itr))
      end do

      return
      end subroutine randgth

      real function randgth_rand(iseed)
      implicit none

      integer  iseed 

      iseed = 2045*iseed + 1
      iseed = iseed -(iseed/1048576)*1048576
      randgth_rand =  float( iseed + 1)/1048577.0
 
      return
      end function randgth_rand   


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine randgth_wrapup (obj)
      type(randgth_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine randgth_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module randgth_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

