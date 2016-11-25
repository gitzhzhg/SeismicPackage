!<CPS_v1 type="PROCESS"/>
!!------------------------------ headswap.f90 --------------------------------!!
!!------------------------------ headswap.f90 --------------------------------!!
!!------------------------------ headswap.f90 --------------------------------!!

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
! Name       : HEADSWAP           (swap a pair of trace headers)
! Category   : headers
! Written    : 2004-06-17   by: Tom Stoeckley
! Revised    : 2005-01-17   by: Tom Stoeckley
! Maturity   : production
! Purpose    : This process swaps trace header values of two input traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This process requires two input traces at a time.
! This process swaps specified trace header values of the two input traces.
! This process then optionally deletes one of the two traces.
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
! Traces must be input two at a time (in pairs).
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! Output traces will be the same as the input traces with some headers swapped.
! One of the two input traces may optionally be deleted.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed.
! NWIH      number of words in trace header         used but not changed.
! NDPT      number of sample values in trace        used but not changed.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   1     trace sequence number      reset as appropriate.
!
! Header words specified by the HDR_FIRST and HDR_SECOND arrays are swapped
! between the two input traces.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2005-01-17  Stoeckley  Initial version.
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
!  NTR == 2              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1 or 2         if this process is outputting traces.
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
!
!<NS HEADSWAP Process/NC=80>
!
!                    HEADSWAP (swap a pair of trace headers)
!
! [/L]This process requires two input traces at a time.
! [/L]This process swaps specified trace header values of the two input traces.
! [/L]This process then optionally deletes one of the two traces.
!
!
!                    DELETE_TRACE=`CCCCCCCCCCCCCCCCC
!
!                    HDR_FIRSTHDR_SECOND
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII     SWAP_ALL_HEADERS`P
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII     DELETE_ALL_HEADERS`P
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII
!                    `IIIIIIII`IIIIIIIIII
!
!<PARMS HDR_FIRST_ARRAYSET[/XST/YST]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="DELETE_TRACE">
!<Tip>Whether to delete one of the input traces after swapping headers.</Tip>
! Default = NEITHER TRACE
! Allowed = NEITHER TRACE
! Allowed = FIRST TRACE
! Allowed = SECOND TRACE
!</Help>
!
!
!<Help KEYWORD="HDR_FIRST">
!<Tip>Header words on first trace to swap with second trace.</Tip>
! Default = none
! Allowed = 1-NWIH
!
! HDR_FIRST and HDR_SECOND on the same row in the table will be swapped.
! The two header word numbers on the same row can be the same or different.
!
! If HDR_FIRST or HDR_SECOND on the same row in the table is blanked out,
! it will be set to the other header on that row.
!
! No header words will be swapped if arrays HDR_FIRST and HDR_SECOND are
! empty.
!</Help>
!
!
!<Help KEYWORD="HDR_SECOND">
!<Tip>Header words on second trace to swap with first trace.</Tip>
! Default = none
! Allowed = 1-NWIH
!
! HDR_FIRST and HDR_SECOND on the same row in the table will be swapped.
! The two header word numbers on the same row can be the same or different.
!
! If HDR_FIRST or HDR_SECOND on the same row in the table is blanked out,
! it will be set to the other header on that row.
!
! No header words will be swapped if arrays HDR_FIRST and HDR_SECOND are
! empty.
!</Help>
!
!
!<Help KEYWORD="SWAP_ALL_HEADERS">
!<Tip>Press this button to swap all header words.</Tip>
!
! When this button is pressed, the arrays HDR_FIRST and HDR_SECOND are
! set to all the header words, with the effect that the entire trace
! headers are to be swapped between the two traces.
!
! When this button is pressed, any previous contents of arrays HDR_FIRST
! and HDR_SECOND are wiped out.
!
! After this button is pressed, the newly-placed header words in arrays
! HDR_FIRST and HDR_SECOND can be edited as desired.
!</Help>
!
!
!<Help KEYWORD="DELETE_ALL_HEADERS">
!<Tip>Press this button to delete all header words in the table.</Tip>
!
! When this button is pressed, the arrays HDR_FIRST and HDR_SECOND are
! cleared.
!
! No header words will be swapped if arrays HDR_FIRST and HDR_SECOND are
! left empty.
!</Help>
!
!
!<Help KEYWORD="DELETE_TRACE">
!<Tip>Whether to delete one of the input traces after swapping headers.</Tip>
! Default = NEITHER TRACE
! Allowed = NEITHER TRACE
! Allowed = FIRST TRACE
! Allowed = SECOND TRACE
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module headswap_module
      use pc_module
      use named_constants_module

      implicit none
      private
      public :: headswap_create
      public :: headswap_initialize
      public :: headswap_update
      public :: headswap_delete
      public :: headswap            ! main trace processing routine.
      public :: headswap_wrapup

      character(len=100),public,save :: HEADSWAP_IDENT = &
'$Id: headswap.f90,v 1.1 2005/01/17 13:46:27 Stoeckley prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: headswap_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.

        integer                    :: numtr            ! global parameter
        integer                    :: nwih             ! global parameter
        integer                    :: ndpt             ! global parameter

        character(len=18)          :: delete_trace     ! process parameter
        integer,pointer            :: hdr_first(:)     ! process parameter
        integer,pointer            :: hdr_second(:)    ! process parameter
        integer                    :: nhdrs            ! process parameter

        integer                    :: kount            ! dependent parameter

      end type headswap_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer                      ,save :: lunprint ! unit number for printing.
      type(headswap_struct),pointer,save :: object   ! needed for traps.

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine headswap_create (obj)
      type(headswap_struct),pointer :: obj       ! arguments

      lunprint = pc_get_lun()
      allocate (obj)

      nullify (obj%hdr_first)
      nullify (obj%hdr_second)

      call headswap_initialize (obj)
      end subroutine headswap_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine headswap_delete (obj)
      type(headswap_struct),pointer :: obj       ! arguments

      call headswap_wrapup (obj)

      if (associated(obj%hdr_first))  deallocate (obj%hdr_first)
      if (associated(obj%hdr_second)) deallocate (obj%hdr_second)

      deallocate(obj)
      end subroutine headswap_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine headswap_initialize (obj)
      type(headswap_struct),intent(inout) :: obj       ! arguments

      obj%delete_trace = 'NEITHER TRACE'
      obj%nhdrs        = 0

      call headswap_update (obj)
      end subroutine headswap_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine headswap_update (obj)
      type(headswap_struct),intent(inout),target :: obj         ! arguments
      integer                                    :: indx        ! arguments

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!


      call pc_register_array_names ('hdr_first_arrayset', (/'hdr_first  ',  &
                                                            'hdr_second '/))

      call pc_get_global ('numtr'   , obj%numtr)
      call pc_get_global ('nwih'    , obj%nwih)
      call pc_get_global ('ndpt'    , obj%ndpt)

      call pc_alloc ('hdr_first' , obj%hdr_first , obj%nhdrs)
      call pc_alloc ('hdr_second', obj%hdr_second, obj%nhdrs)

      call pc_get ('DELETE_TRACE', obj%delete_trace)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (pc_verify_end()) then
      if (obj%numtr /= 2) then
           call pc_error ('Traces must be gathered in pairs for HEADSWAP')
      end if
      end if

      select case (obj%delete_trace)
           case ('NEITHER TRACE')
           case ('FIRST TRACE')
           case ('SECOND TRACE')
           case default
                  obj%delete_trace = 'NEITHER TRACE'
      end select

      if (pc_pressed('swap_all_headers')) then
           if (associated(obj%hdr_first))  deallocate (obj%hdr_first)
           if (associated(obj%hdr_second)) deallocate (obj%hdr_second)
           obj%nhdrs = obj%nwih
           allocate (obj%hdr_first (obj%nhdrs))
           allocate (obj%hdr_second(obj%nhdrs))
           do indx = 1,obj%nhdrs
                obj%hdr_first (indx) = indx
                obj%hdr_second(indx) = indx
           end do
      end if

      if (pc_pressed('delete_all_headers')) then
           obj%nhdrs = 0
      end if

      do indx = 1,obj%nhdrs
           if (obj%hdr_first(indx) == INIL) then
                obj%hdr_first(indx) = obj%hdr_second(indx)
           end if
           if (obj%hdr_second(indx) == INIL) then
                obj%hdr_second(indx) = obj%hdr_first(indx)
           end if
           call mth_constrain (obj%hdr_first (indx), 1,obj%nwih)
           call mth_constrain (obj%hdr_second(indx), 1,obj%nwih)
      end do


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      call pc_put_options_field ('DELETE_TRACE', (/ 'NEITHER TRACE ',  &
                                                    'FIRST TRACE   ',  &
                                                    'SECOND TRACE  ' /) )

      call pc_put ('hdr_first' , obj%hdr_first , obj%nhdrs)
      call pc_put ('hdr_second', obj%hdr_second, obj%nhdrs)

      call pc_put ('DELETE_TRACE', obj%delete_trace)

      call pc_put_maxsize_arrayset ('hdr_first_arrayset', obj%nwih)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.     ! to run wrapup code after processing.

      obj%kount = 0


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      end subroutine headswap_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


      subroutine headswap (obj,ntr,hd,tr)
      type(headswap_struct),intent(inout) :: obj               ! arguments
      integer              ,intent(inout) :: ntr               ! arguments
      double precision     ,intent(inout) :: hd(:,:)           ! arguments
      real                 ,intent(inout) :: tr(:,:)           ! arguments
      integer                             :: itr,ihdr          ! local
      integer                             :: indx1,indx2       ! local
      double precision                    :: swap1,swap2       ! local

      if (ntr == 2) then

           do ihdr = 1,obj%nhdrs
                indx1 = obj%hdr_first (ihdr)
                indx2 = obj%hdr_second(ihdr)
                swap1       = hd(indx1,1)
                swap2       = hd(indx2,2)
                hd(indx1,1) = swap2
                hd(indx2,2) = swap1
           end do

           select case (obj%delete_trace)
                case ('FIRST TRACE')
                         hd(1:obj%nwih,1) = hd(1:obj%nwih,2)
                         tr(1:obj%ndpt,1) = tr(1:obj%ndpt,2)
                         ntr              = 1
                case ('SECOND TRACE')
                         ntr              = 1
                case default
           end select

      else if (ntr == NO_MORE_TRACES) then

           call headswap_wrapup (obj)

      else

           call pc_error ('HEADSWAP received an illegal value',ntr,'for NTR')
           call headswap_wrapup (obj)
           ntr = FATAL_ERROR

      end if

      do itr = 1,ntr
           obj%kount = obj%kount + 1
           hd(1,itr) = obj%kount
      end do

      end subroutine headswap


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


      subroutine headswap_wrapup (obj)
      type(headswap_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      end subroutine headswap_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module headswap_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

