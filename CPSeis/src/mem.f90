!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- mem.f90 --------------------------------!!
!!------------------------------- mem.f90 --------------------------------!!
!!------------------------------- mem.f90 --------------------------------!!


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
!                        C P S   P R I M I T I V E        
!
! Name       : MEM 
! Category   : memory
! Written    : 2000-03-09   by: Tom Stoeckley
! Revised    : 2001-05-17   by: Tom Stoeckley
! Maturity   : production   2001-06-11
! Purpose    : allocate and free memory, reporting errors to parameter cache.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is a convenience for process modules to use to allocate
! and free memory associated with Fortran-90 pointers to 1D, 2D, and 3D
! arrays of type integer, real, double precision, complex, character (any
! length), or logical.
!
! Specifically, this primitive performs the following tasks:
!
!  (1) tests for association before deallocating.
!  (2) tests for association and deallocates (if necessary) before allocating.
!  (3) always allocates at least one array element, even if the desired number
!       of array elements is zero, to protect against passing a deallocated
!       or nullified array to a subroutine which receives the array without
!       the pointer attribute.
!  (4) reports errors to the parameter cache.
!
! If you want the functionality of this primitive, but do not want it to call
! the parameter cache, use the ARRAY primitive instead.  The ARRAY primitive
! returns an optional error message instead of passing it to the parameter
! cache.  This MEM primitive calls the ARRAY primitive to do its work.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                                                      opt
!                                  b      i   i   i     o  
!              call mem_alloc   (parray1, n1,         status)
!              call mem_alloc   (parray2, n1, n2,     status)
!              call mem_alloc   (parray3, n1, n2, n3, status)
!
!              call mem_realloc (parray1, n1,         status)
!              call mem_realloc (parray2, n1, n2,     status)
!              call mem_realloc (parray3, n1, n2, n3, status)
!
!              call mem_free    (parray1)
!              call mem_free    (parray2)
!              call mem_free    (parray3)
!
! (any type),pointer parray1(:)     = 1D array to be allocated or deallocated.
! (any type),pointer parray2(:,:)   = 2D array to be allocated or deallocated.
! (any type),pointer parray3(:,:,:) = 3D array to be allocated or deallocated.
! integer            n1             = first  array dimension to allocate.
! integer            n2             = second array dimension to allocate.
! integer            n3             = third  array dimension to allocate.
! integer            status         = status of the action (0 means no errors).
!
! The allowed variable types for PARRAY are integer, real, character (any
! length), double precision, complex, and logical.
!
! MEM_ALLOC:
!  (1) tests for association.
!  (2) deallocates old memory if associated.
!  (3) allocates new memory.
!  (4) always allocates at least one array element.
!  (5) reports errors to the parameter cache.
!
! MEM_REALLOC:
!  (1) if not associated, simply calls MEM_ALLOC.
!  (2) if already associated:
!  (3) allocates new memory with MEM_ALLOC.
!  (4) copies old memory to new memory up to the lesser of the old and
!       new dimensions.
!  (5) deallocates old memory.
!  (6) sets pointer to point to new memory.
!
! MEM_FREE:
!  (1) tests for association.
!  (2) deallocates memory if associated.
!
! NOTE: When allocating memory during trace processing, the STATUS argument
! should be present so that NTR can be set to FATAL_ERROR if an error
! occurs.  But when allocating memory from the UPDATE routine, the STATUS
! argument may need to be present only if the memory is being both allocated
! and initialized before the PC_DO_NOT_PROCESS_TRACES function is called.
!
! NOTE: If you are allocating many arrays from your UPDATE routine, you
! can safely omit the STATUS argument if you follow these steps:
!   (1) call pc_do_not_process_traces() and return if true.
!   (2) allocate your memory needed for trace processing.
!   (3) call pc_do_not_process_traces() again and return if true.
!   (4) initialize your memory needed for trace processing.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2001-06-11  Stoeckley    Move the guts of the code to the new ARRAY
!                               primitive.  PRODUCTION.
!  4. 2000-05-08  Stoeckley    Add workaround code for Portland Group compiler
!                               bug (for reallocating character arrays).
!  3. 2000-05-03  Stoeckley    Add reallocate routines.
!  2. 2000-03-16  Stoeckley    Add complex variable type and 2D and 3D arrays.
!  1. 2000-03-09  Stoeckley    Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module mem_module
      use pc_module
      use array_module
      implicit none
      public
      private :: mem_helper

      character(len=100),public,save :: MEM_IDENT = &
'$Id: mem.f90,v 1.5 2001/06/07 13:34:42 sps prod sps $'


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface mem_alloc
           module procedure mem_alloc_integer_1d
           module procedure mem_alloc_real_1d
           module procedure mem_alloc_double_1d
           module procedure mem_alloc_complex_1d
           module procedure mem_alloc_char_1d
           module procedure mem_alloc_logical_1d

           module procedure mem_alloc_integer_2d
           module procedure mem_alloc_real_2d
           module procedure mem_alloc_double_2d
           module procedure mem_alloc_complex_2d
           module procedure mem_alloc_char_2d
           module procedure mem_alloc_logical_2d

           module procedure mem_alloc_integer_3d
           module procedure mem_alloc_real_3d
           module procedure mem_alloc_double_3d
           module procedure mem_alloc_complex_3d
           module procedure mem_alloc_char_3d
           module procedure mem_alloc_logical_3d
      end interface


      interface mem_realloc
           module procedure mem_realloc_integer_1d
           module procedure mem_realloc_real_1d
           module procedure mem_realloc_double_1d
           module procedure mem_realloc_complex_1d
           module procedure mem_realloc_char_1d
           module procedure mem_realloc_logical_1d

           module procedure mem_realloc_integer_2d
           module procedure mem_realloc_real_2d
           module procedure mem_realloc_double_2d
           module procedure mem_realloc_complex_2d
           module procedure mem_realloc_char_2d
           module procedure mem_realloc_logical_2d

           module procedure mem_realloc_integer_3d
           module procedure mem_realloc_real_3d
           module procedure mem_realloc_double_3d
           module procedure mem_realloc_complex_3d
           module procedure mem_realloc_char_3d
           module procedure mem_realloc_logical_3d
      end interface


      interface mem_free
           module procedure mem_free_integer_1d
           module procedure mem_free_real_1d
           module procedure mem_free_double_1d
           module procedure mem_free_complex_1d
           module procedure mem_free_char_1d
           module procedure mem_free_logical_1d

           module procedure mem_free_integer_2d
           module procedure mem_free_real_2d
           module procedure mem_free_double_2d
           module procedure mem_free_complex_2d
           module procedure mem_free_char_2d
           module procedure mem_free_logical_2d

           module procedure mem_free_integer_3d
           module procedure mem_free_real_3d
           module procedure mem_free_double_3d
           module procedure mem_free_complex_3d
           module procedure mem_free_char_3d
           module procedure mem_free_logical_3d
      end interface


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!-------------------------- mem helper ---------------------------------!!
!!-------------------------- mem helper ---------------------------------!!
!!-------------------------- mem helper ---------------------------------!!


      subroutine mem_helper (msg)
      implicit none
      character(len=*),intent(in)            :: msg         ! arguments

      if (msg /= ' ') call pc_error (msg)
      return
      end subroutine mem_helper


!!----------------------------- mem alloc ---------------------------------!!
!!----------------------------- mem alloc ---------------------------------!!
!!----------------------------- mem alloc ---------------------------------!!


      subroutine mem_alloc_integer_1d (parray1,n1,status)
      implicit none
      integer            ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_alloc (parray1,n1,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_integer_1d


      subroutine mem_alloc_real_1d (parray1,n1,status)
      implicit none
      real               ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_alloc (parray1,n1,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_real_1d


      subroutine mem_alloc_double_1d (parray1,n1,status)
      implicit none
      double precision   ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_alloc (parray1,n1,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_double_1d


      subroutine mem_alloc_complex_1d (parray1,n1,status)
      implicit none
      complex            ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_alloc (parray1,n1,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_complex_1d


      subroutine mem_alloc_char_1d (parray1,n1,status)
      implicit none
      character(len=*)   ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_alloc (parray1,n1,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_char_1d


      subroutine mem_alloc_logical_1d (parray1,n1,status)
      implicit none
      logical            ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_alloc (parray1,n1,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_logical_1d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine mem_alloc_integer_2d (parray2,n1,n2,status)
      implicit none
      integer            ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_alloc (parray2,n1,n2,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_integer_2d


      subroutine mem_alloc_real_2d (parray2,n1,n2,status)
      implicit none
      real               ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_alloc (parray2,n1,n2,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_real_2d


      subroutine mem_alloc_double_2d (parray2,n1,n2,status)
      implicit none
      double precision   ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_alloc (parray2,n1,n2,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_double_2d


      subroutine mem_alloc_complex_2d (parray2,n1,n2,status)
      implicit none
      complex            ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_alloc (parray2,n1,n2,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_complex_2d


      subroutine mem_alloc_char_2d (parray2,n1,n2,status)
      implicit none
      character(len=*)   ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_alloc (parray2,n1,n2,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_char_2d


      subroutine mem_alloc_logical_2d (parray2,n1,n2,status)
      implicit none
      logical            ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_alloc (parray2,n1,n2,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_logical_2d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine mem_alloc_integer_3d (parray3,n1,n2,n3,status)
      implicit none
      integer            ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_alloc (parray3,n1,n2,n3,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_integer_3d


      subroutine mem_alloc_real_3d (parray3,n1,n2,n3,status)
      implicit none
      real               ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_alloc (parray3,n1,n2,n3,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_real_3d


      subroutine mem_alloc_double_3d (parray3,n1,n2,n3,status)
      implicit none
      double precision   ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_alloc (parray3,n1,n2,n3,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_double_3d


      subroutine mem_alloc_complex_3d (parray3,n1,n2,n3,status)
      implicit none
      complex            ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_alloc (parray3,n1,n2,n3,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_complex_3d


      subroutine mem_alloc_char_3d (parray3,n1,n2,n3,status)
      implicit none
      character(len=*)   ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_alloc (parray3,n1,n2,n3,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_char_3d


      subroutine mem_alloc_logical_3d (parray3,n1,n2,n3,status)
      implicit none
      logical            ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_alloc (parray3,n1,n2,n3,status,msg)
      call mem_helper  (msg)
      return
      end subroutine mem_alloc_logical_3d


!!----------------------------- mem realloc ---------------------------------!!
!!----------------------------- mem realloc ---------------------------------!!
!!----------------------------- mem realloc ---------------------------------!!


      subroutine mem_realloc_integer_1d (parray1,n1,status)
      implicit none
      integer            ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_realloc (parray1,n1,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_integer_1d


      subroutine mem_realloc_real_1d (parray1,n1,status)
      implicit none
      real               ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_realloc (parray1,n1,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_real_1d


      subroutine mem_realloc_double_1d (parray1,n1,status)
      implicit none
      double precision   ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_realloc (parray1,n1,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_double_1d


      subroutine mem_realloc_complex_1d (parray1,n1,status)
      implicit none
      complex            ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_realloc (parray1,n1,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_complex_1d


      subroutine mem_realloc_char_1d (parray1,n1,status)
      implicit none
      character(len=*)   ,pointer             :: parray1(:)       ! arguments
      integer,intent(in)                      :: n1               ! arguments
      integer,intent(out),optional            :: status           ! arguments
      character(len=80)                       :: msg              ! local

      call array_realloc (parray1,n1,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_char_1d


      subroutine mem_realloc_logical_1d (parray1,n1,status)
      implicit none
      logical            ,pointer   :: parray1(:)   ! arguments
      integer,intent(in)            :: n1           ! arguments
      integer,intent(out),optional  :: status       ! arguments
      character(len=80)             :: msg          ! local

      call array_realloc (parray1,n1,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_logical_1d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine mem_realloc_integer_2d (parray2,n1,n2,status)
      implicit none
      integer            ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_realloc (parray2,n1,n2,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_integer_2d


      subroutine mem_realloc_real_2d (parray2,n1,n2,status)
      implicit none
      real               ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_realloc (parray2,n1,n2,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_real_2d


      subroutine mem_realloc_double_2d (parray2,n1,n2,status)
      implicit none
      double precision   ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_realloc (parray2,n1,n2,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_double_2d


      subroutine mem_realloc_complex_2d (parray2,n1,n2,status)
      implicit none
      complex            ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_realloc (parray2,n1,n2,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_complex_2d


      subroutine mem_realloc_char_2d (parray2,n1,n2,status)
      implicit none
      character(len=*)   ,pointer             :: parray2(:,:)     ! arguments
      integer,intent(in)                      :: n1,n2            ! arguments
      integer,intent(out),optional            :: status           ! arguments
      character(len=80)                       :: msg              ! local

      call array_realloc (parray2,n1,n2,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_char_2d


      subroutine mem_realloc_logical_2d (parray2,n1,n2,status)
      implicit none
      logical            ,pointer   :: parray2(:,:)   ! arguments
      integer,intent(in)            :: n1,n2          ! arguments
      integer,intent(out),optional  :: status         ! arguments
      character(len=80)             :: msg            ! local

      call array_realloc (parray2,n1,n2,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_logical_2d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine mem_realloc_integer_3d (parray3,n1,n2,n3,status)
      implicit none
      integer            ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_realloc (parray3,n1,n2,n3,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_integer_3d


      subroutine mem_realloc_real_3d (parray3,n1,n2,n3,status)
      implicit none
      real               ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_realloc (parray3,n1,n2,n3,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_real_3d


      subroutine mem_realloc_double_3d (parray3,n1,n2,n3,status)
      implicit none
      double precision   ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_realloc (parray3,n1,n2,n3,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_double_3d


      subroutine mem_realloc_complex_3d (parray3,n1,n2,n3,status)
      implicit none
      complex            ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_realloc (parray3,n1,n2,n3,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_complex_3d


      subroutine mem_realloc_char_3d (parray3,n1,n2,n3,status)
      implicit none
      character(len=*)   ,pointer             :: parray3(:,:,:)   ! arguments
      integer,intent(in)                      :: n1,n2,n3         ! arguments
      integer,intent(out),optional            :: status           ! arguments
      character(len=80)                       :: msg              ! local

      call array_realloc (parray3,n1,n2,n3,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_char_3d


      subroutine mem_realloc_logical_3d (parray3,n1,n2,n3,status)
      implicit none
      logical            ,pointer   :: parray3(:,:,:)   ! arguments
      integer,intent(in)            :: n1,n2,n3         ! arguments
      integer,intent(out),optional  :: status           ! arguments
      character(len=80)             :: msg              ! local

      call array_realloc (parray3,n1,n2,n3,status,msg)
      call mem_helper    (msg)
      return
      end subroutine mem_realloc_logical_3d


!!----------------------------- mem free ---------------------------------!!
!!----------------------------- mem free ---------------------------------!!
!!----------------------------- mem free ---------------------------------!!


      subroutine mem_free_integer_1d (parray1)
      implicit none
      integer            ,pointer   :: parray1(:)   ! arguments

      call array_free (parray1)
      return
      end subroutine mem_free_integer_1d


      subroutine mem_free_real_1d (parray1)
      implicit none
      real               ,pointer   :: parray1(:)   ! arguments

      call array_free (parray1)
      return
      end subroutine mem_free_real_1d


      subroutine mem_free_double_1d (parray1)
      implicit none
      double precision   ,pointer   :: parray1(:)   ! arguments

      call array_free (parray1)
      return
      end subroutine mem_free_double_1d


      subroutine mem_free_complex_1d (parray1)
      implicit none
      complex            ,pointer   :: parray1(:)   ! arguments

      call array_free (parray1)
      return
      end subroutine mem_free_complex_1d


      subroutine mem_free_char_1d (parray1)
      implicit none
      character(len=*)   ,pointer   :: parray1(:)   ! arguments

      call array_free (parray1)
      return
      end subroutine mem_free_char_1d


      subroutine mem_free_logical_1d (parray1)
      implicit none
      logical            ,pointer   :: parray1(:)   ! arguments

      call array_free (parray1)
      return
      end subroutine mem_free_logical_1d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine mem_free_integer_2d (parray2)
      implicit none
      integer            ,pointer   :: parray2(:,:)   ! arguments

      call array_free (parray2)
      return
      end subroutine mem_free_integer_2d


      subroutine mem_free_real_2d (parray2)
      implicit none
      real               ,pointer   :: parray2(:,:)   ! arguments

      call array_free (parray2)
      return
      end subroutine mem_free_real_2d


      subroutine mem_free_double_2d (parray2)
      implicit none
      double precision   ,pointer   :: parray2(:,:)   ! arguments

      call array_free (parray2)
      return
      end subroutine mem_free_double_2d


      subroutine mem_free_complex_2d (parray2)
      implicit none
      complex            ,pointer   :: parray2(:,:)   ! arguments

      call array_free (parray2)
      return
      end subroutine mem_free_complex_2d


      subroutine mem_free_char_2d (parray2)
      implicit none
      character(len=*)   ,pointer   :: parray2(:,:)   ! arguments

      call array_free (parray2)
      return
      end subroutine mem_free_char_2d


      subroutine mem_free_logical_2d (parray2)
      implicit none
      logical            ,pointer   :: parray2(:,:)   ! arguments

      call array_free (parray2)
      return
      end subroutine mem_free_logical_2d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine mem_free_integer_3d (parray3)
      implicit none
      integer            ,pointer   :: parray3(:,:,:)   ! arguments

      call array_free (parray3)
      return
      end subroutine mem_free_integer_3d


      subroutine mem_free_real_3d (parray3)
      implicit none
      real               ,pointer   :: parray3(:,:,:)   ! arguments

      call array_free (parray3)
      return
      end subroutine mem_free_real_3d


      subroutine mem_free_double_3d (parray3)
      implicit none
      double precision   ,pointer   :: parray3(:,:,:)   ! arguments

      call array_free (parray3)
      return
      end subroutine mem_free_double_3d


      subroutine mem_free_complex_3d (parray3)
      implicit none
      complex            ,pointer   :: parray3(:,:,:)   ! arguments

      call array_free (parray3)
      return
      end subroutine mem_free_complex_3d


      subroutine mem_free_char_3d (parray3)
      implicit none
      character(len=*)   ,pointer   :: parray3(:,:,:)   ! arguments

      call array_free (parray3)
      return
      end subroutine mem_free_char_3d


      subroutine mem_free_logical_3d (parray3)
      implicit none
      logical            ,pointer   :: parray3(:,:,:)   ! arguments

      call array_free (parray3)
      return
      end subroutine mem_free_logical_3d


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module mem_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

