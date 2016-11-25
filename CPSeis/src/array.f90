!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- array.f90 --------------------------------!!
!!------------------------------- array.f90 --------------------------------!!
!!------------------------------- array.f90 --------------------------------!!


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
! Name       : ARRAY 
! Category   : memory
! Written    : 2001-05-17   by: Tom Stoeckley
! Revised    : 2004-09-01   by: Tom Stoeckley
! Maturity   : production
! Purpose    : array manipulation (including allocation and deallocation).
! Portability: No known limitations, but see below regarding pgf90 compiler bug.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains various routines for manipulating 1D, 2D, and 3D
! arrays of type integer, real, double precision, complex, character (any
! length), or logical.
!
! The routines currently provided are convenient memory-management routines
! which can be used to allocate, deallocate, and reallocate arrays.
! Specifically, these routines perform the following tasks:
!
!  (1) test for association before deallocating.
!  (2) test for association and deallocate (if necessary) before allocating.
!  (3) always allocate at least one array element, even if the desired number
!       of array elements is zero, to protect against passing a deallocated
!       or nullified array to a subroutine which receives the array without
!       the pointer attribute.
!  (4) return error messages.
!  (5) insert or remove or append an array element, with associated copying
!       from one part of the array to another as required, and with any
!       required memory reallocation handled automatically, in chunks for
!       efficiency.
!
! NOTE: This primitive is the same as the MEM primitive except that this
! primitive returns an error message instead of reporting it to the parameter
! cache, and also has additional options.  This primitive should be used
! instead of the MEM primitive if it is called from a primitive which otherwise
! is not associated with the parameter cache.  The MEM primitive has been
! modified to call this primitive in order to eliminate duplicate code.
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
!                                              opt   opt   opt     opt    opt
!                         b       i   i   i     o     o     i       i      i
!   call array_alloc   (parray1, na1,         status,msg,keyword,multiple,fill)
!   call array_alloc   (parray2, na1,na2,     status,msg,keyword,multiple)
!   call array_alloc   (parray3, na1,na2,na3, status,msg,keyword,multiple)
!
!   call array_realloc (parray1, na1,         status,msg,keyword,multiple,fill)
!   call array_realloc (parray2, na1,na2,     status,msg,keyword,multiple)
!   call array_realloc (parray3, na1,na2,na3, status,msg,keyword,multiple)
!
!   call array_free    (parray1)
!   call array_free    (parray2)
!   call array_free    (parray3)
!
!                                                          opt
!                                b      b    i       i      i
!   call array_insert_element (parray1, n1, indx, element, fill)
!   call array_append_element (parray1, n1,       element, fill)
!   call array_remove_element (parray1, n1, indx,          fill)
!
!
! (any type),pointer parray1(:)     = 1D array to be allocated or deallocated.
! (any type),pointer parray2(:,:)   = 2D array to be allocated or deallocated.
! (any type),pointer parray3(:,:,:) = 3D array to be allocated or deallocated.
! integer            na1            = first  array dimension to allocate.
! integer            na2            = second array dimension to allocate.
! integer            na3            = third  array dimension to allocate.
! integer            status         = status of the action (0 means no errors).
! character(len=*)   msg            = error message (blank means no errors).
! character(len=*)   keyword        = identifying name for error message.
! logical            multiple       = true if multiple allocations (see below).
! (any type)         fill           = value to set extended part of array to.
! integer            n1             = number of elements in first  array dim.
! integer            n2             = number of elements in second array dim.
! integer            n3             = number of elements in third  array dim.
! integer            indx           = index where to insert or remove element.
! (any type)         element        = a single array element to insert.
!
!
! The allowed variable types for PARRAY and FILL and ELEMENT are integer,
! real, double precision, complex, character (any length), and logical.
!
! If MULTIPLE is present and true, then STATUS and MSG (if present) must be
! preset to zero and blank, respectively, and they will be intent(inout)
! rather than intent(out).  Then they will be reset only if and when the
! first error occurs.  This is useful if you have a long list of allocations,
! because you would then have to test STATUS and/or MSG only once, after all
! of the allocations, rather than after each allocation.
!
! If FILL is present, a newly allocated array, or the extended part of a
! reallocated array (if it has grown), or the abandoned element of an array
! (if an array element has been removed) is set to this value.  If FILL is
! always specified in every call, the entire allocated array will never
! contain any undefined values.
!
!
! ARRAY_ALLOC:
!  (1) tests for association.
!  (2) deallocates old memory if associated.
!  (3) allocates new memory to the specified size.
!  (4) always allocates at least one array element.
!
! ARRAY_REALLOC:
!  (1) if not associated, simply calls ARRAY_ALLOC.
!  (2) if already associated:
!  (3) allocates new memory with ARRAY_ALLOC.
!  (4) copies old memory to new memory up to the lesser of the old and
!       new dimensions.
!  (5) deallocates old memory.
!  (6) sets pointer to point to new memory.
!
! ARRAY_FREE:
!  (1) tests for association.
!  (2) deallocates memory if associated.
!
! ARRAY_INSERT_ELEMENT:
!  (1) inserts the specified element into the array at location INDX.
!  (2) previous elements from INDX thru N1 are moved up one place.
!  (3) N1 is incremented by 1 to the correct new value.
!  (4) the array is reallocated in chunks to a larger size if necessary.
!
! ARRAY_APPEND_ELEMENT:
!  (1) appends the specified element to the array at location N1+1.
!  (2) N1 is incremented by 1 to the correct new value.
!  (3) the array is reallocated in chunks to a larger size if necessary.
!
! ARRAY_REMOVE_ELEMENT:
!  (1) removes one element from the array at location INDX.
!  (2) previous elements from INDX+1 thru N1 are moved down one place.
!  (3) N1 is decremented by 1 to the correct new value.
!  (4) the array is not reallocated.
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
!  3. 2004-09-01  Stoeckley    Fix bug (found by Faqi Liu) when reallocating
!                               character arrays where the old array was not
!                               being deallocated.
!  2. 2001-12-10  Stoeckley    Add optional arguments KEYWORD, MULTIPLE, FILL.
!  1. 2001-06-11  Stoeckley    Initial version, made from the guts of the MEM
!                               primitive plus portions from other primitives.
!                              PRODUCTION.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                           PORTABILITY ISSUES
!
! The reallocate routines for character variables were changed to work around
! a bug in the Portland Group compiler.  The following code would set the
! length of the character variables in the TEMP automatic array to an
! incorrect number which would change with different compilations.  Passing
! the length explicitly through the argument list also misbehaved similarly,
! as did setting the length into a module variable before calling the
! subroutine.  The only thing that would work would be to set the length to
! a parameter or to a constant.
!
!   subroutine array_realloc_char_3d (parray3,na1,na2,na3,status)
!   implicit none
!   character(len=*)   ,pointer                :: parray3(:,:,:)  ! arguments
!   integer,intent(in)                         :: na1,na2,na3     ! arguments
!   integer,intent(out),optional               :: status          ! arguments
!   character(len=len(parray3(1,1,1))),pointer :: temp(:,:,:)     ! local
!
! To fix this problem, the automatic array TEMP was given a length of 200,
! and less efficient code (requiring two array copies instead of one) was
! used.  The consequence is that if PARRAY3 has a length > 200, the copied
! values will be truncated to a length of 200.
!
! This problem required changing the code in the following three subroutines:
!             array_private_realloc_char_1d
!             array_private_realloc_char_2d
!             array_private_realloc_char_3d
! The old code is shown commented out above the new code for each subroutine.
!
! Note also that the Portland Group compiler requires the length to be
! obtained from a single array element, not an entire array, as follows:
!         len(array(1,1))   ! OK with any compiler.
!         len(array)        ! OK with any compiler except Portland Group.
!
!-------------------------------------------------------------------------------
!
! Additional note dated 5/17/01: The above-mentioned Portland Group compiler
! bug still exists.  Also, this same bug now also exists in the sun compiler
! on poepsn03.  But in the case of the sun compiler, a compiler error occurs
! (instead of a run-time error) with the following message, where YYYYYY is
! the subroutine name and XXXXXXX is the file name:
!
!  cf90-964 f90comp: INTERNAL YYYYYY, File = XXXXXXX.f90, Line = 7, Column = 1 
!  Non constant character in gen_static_dv_whole_def.
!
! This bug has been fixed in the newer sun compilers on poepsn25 and poepsn56.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module array_module
      use string_module
      implicit none
      public
      private :: array_helper

      character(len=100),public,save :: ARRAY_IDENT = &
'$Id: array.f90,v 1.3 2004/09/01 13:09:58 Stoeckley prod sps $'


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface array_alloc
           module procedure array_alloc_integer_1d
           module procedure array_alloc_real_1d
           module procedure array_alloc_double_1d
           module procedure array_alloc_complex_1d
           module procedure array_alloc_char_1d
           module procedure array_alloc_logical_1d

           module procedure array_alloc_integer_2d
           module procedure array_alloc_real_2d
           module procedure array_alloc_double_2d
           module procedure array_alloc_complex_2d
           module procedure array_alloc_char_2d
           module procedure array_alloc_logical_2d

           module procedure array_alloc_integer_3d
           module procedure array_alloc_real_3d
           module procedure array_alloc_double_3d
           module procedure array_alloc_complex_3d
           module procedure array_alloc_char_3d
           module procedure array_alloc_logical_3d
      end interface


      interface array_realloc
           module procedure array_realloc_integer_1d
           module procedure array_realloc_real_1d
           module procedure array_realloc_double_1d
           module procedure array_realloc_complex_1d
           module procedure array_realloc_char_1d
           module procedure array_realloc_logical_1d

           module procedure array_realloc_integer_2d
           module procedure array_realloc_real_2d
           module procedure array_realloc_double_2d
           module procedure array_realloc_complex_2d
           module procedure array_realloc_char_2d
           module procedure array_realloc_logical_2d

           module procedure array_realloc_integer_3d
           module procedure array_realloc_real_3d
           module procedure array_realloc_double_3d
           module procedure array_realloc_complex_3d
           module procedure array_realloc_char_3d
           module procedure array_realloc_logical_3d
      end interface


      interface array_free
           module procedure array_free_integer_1d
           module procedure array_free_real_1d
           module procedure array_free_double_1d
           module procedure array_free_complex_1d
           module procedure array_free_char_1d
           module procedure array_free_logical_1d

           module procedure array_free_integer_2d
           module procedure array_free_real_2d
           module procedure array_free_double_2d
           module procedure array_free_complex_2d
           module procedure array_free_char_2d
           module procedure array_free_logical_2d

           module procedure array_free_integer_3d
           module procedure array_free_real_3d
           module procedure array_free_double_3d
           module procedure array_free_complex_3d
           module procedure array_free_char_3d
           module procedure array_free_logical_3d
      end interface


      interface array_insert_element
           module procedure array_insert_element_integer
           module procedure array_insert_element_real
           module procedure array_insert_element_double
           module procedure array_insert_element_complex
           module procedure array_insert_element_char
           module procedure array_insert_element_logical
      end interface


      interface array_remove_element
           module procedure array_remove_element_integer
           module procedure array_remove_element_real
           module procedure array_remove_element_double
           module procedure array_remove_element_complex
           module procedure array_remove_element_char
           module procedure array_remove_element_logical
      end interface


      interface array_append_element
           module procedure array_append_element_integer
           module procedure array_append_element_real
           module procedure array_append_element_double
           module procedure array_append_element_complex
           module procedure array_append_element_char
           module procedure array_append_element_logical
      end interface


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!-------------------------- array helper ---------------------------------!!
!!-------------------------- array helper ---------------------------------!!
!!-------------------------- array helper ---------------------------------!!


      subroutine array_helper (ier,word,nalloc,status,msg,keyword,multiple)
      implicit none
      integer         ,intent(in)             :: ier          ! arguments
      character(len=*),intent(in)             :: word         ! arguments
      integer         ,intent(in)             :: nalloc       ! arguments
      integer         ,intent(inout),optional :: status       ! arguments
      character(len=*),intent(inout),optional :: msg          ! arguments
      character(len=*),intent(in)   ,optional :: keyword      ! arguments
      logical         ,intent(in)   ,optional :: multiple     ! arguments

      if (ier == 0) then
           if (.not.present(multiple)) then
                 if (present(status)) status = 0
                 if (present(msg))    msg    = ' '
           else if (.not.multiple) then
                 if (present(status)) status = 0
                 if (present(msg))    msg    = ' '
           end if
           return
      end if

      if (present(status)) status = ier
      if (present(msg)) then
           msg = 'error allocating'//string_ii2ss(nalloc)// &
                                   trim(word)//' array elements'
           if (present(keyword)) msg = trim(msg)//' ('//trim(keyword)//')'
      end if
      return
      end subroutine array_helper


!!----------------------------- array alloc ---------------------------------!!
!!----------------------------- array alloc ---------------------------------!!
!!----------------------------- array alloc ---------------------------------!!


      subroutine array_alloc_integer_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      integer         ,pointer                :: parray1(:)     ! arguments
      integer         ,intent(in)             :: na1            ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer         ,intent(in)   ,optional :: fill           ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray1)
      allocate          (parray1(max(na1,1)),stat=ier)
      call array_helper (ier,'integer 1D',na1,status,msg,keyword,multiple)
      if (present(fill) .and. ier == 0) parray1(:) = fill
      return
      end subroutine array_alloc_integer_1d



      subroutine array_alloc_real_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      real            ,pointer                :: parray1(:)     ! arguments
      integer         ,intent(in)             :: na1            ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      real            ,intent(in)   ,optional :: fill           ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray1)
      allocate          (parray1(max(na1,1)),stat=ier)
      call array_helper (ier,'real 1D',na1,status,msg,keyword,multiple)
      if (present(fill) .and. ier == 0) parray1(:) = fill
      return
      end subroutine array_alloc_real_1d



      subroutine array_alloc_double_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      double precision,pointer                :: parray1(:)     ! arguments
      integer         ,intent(in)             :: na1            ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      double precision,intent(in)   ,optional :: fill           ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray1)
      allocate          (parray1(max(na1,1)),stat=ier)
      call array_helper &
              (ier,'double precision 1D',na1,status,msg,keyword,multiple)
      if (present(fill) .and. ier == 0) parray1(:) = fill
      return
      end subroutine array_alloc_double_1d



      subroutine array_alloc_complex_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      complex         ,pointer                :: parray1(:)     ! arguments
      integer         ,intent(in)             :: na1            ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      complex         ,intent(in)   ,optional :: fill           ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray1)
      allocate          (parray1(max(na1,1)),stat=ier)
      call array_helper (ier,'complex 1D',na1,status,msg,keyword,multiple)
      if (present(fill) .and. ier == 0) parray1(:) = fill
      return
      end subroutine array_alloc_complex_1d



      subroutine array_alloc_char_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      character(len=*),pointer                :: parray1(:)     ! arguments
      integer         ,intent(in)             :: na1            ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      character(len=*),intent(in)   ,optional :: fill           ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray1)
      allocate          (parray1(max(na1,1)),stat=ier)
      call array_helper (ier,'character 1D',na1,status,msg,keyword,multiple)
      if (present(fill) .and. ier == 0) parray1(:) = fill
      return
      end subroutine array_alloc_char_1d



      subroutine array_alloc_logical_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      logical         ,pointer                :: parray1(:)     ! arguments
      integer         ,intent(in)             :: na1            ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      logical         ,intent(in)   ,optional :: fill           ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray1)
      allocate          (parray1(max(na1,1)),stat=ier)
      call array_helper (ier,'logical 1D',na1,status,msg,keyword,multiple)
      if (present(fill) .and. ier == 0) parray1(:) = fill
      return
      end subroutine array_alloc_logical_1d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine array_alloc_integer_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      integer         ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray2)
      allocate          (parray2(max(na1,1),max(na2,1)),stat=ier)
      call array_helper (ier,'integer 2D',na1*na2,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_integer_2d



      subroutine array_alloc_real_2d &
                               (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      real            ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray2)
      allocate          (parray2(max(na1,1),max(na2,1)),stat=ier)
      call array_helper (ier,'real 2D',na1*na2,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_real_2d



      subroutine array_alloc_double_2d &
                               (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      double precision,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray2)
      allocate          (parray2(max(na1,1),max(na2,1)),stat=ier)
      call array_helper &
               (ier,'double precision 2D',na1*na2,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_double_2d



      subroutine array_alloc_complex_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      complex         ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray2)
      allocate          (parray2(max(na1,1),max(na2,1)),stat=ier)
      call array_helper (ier,'complex 2D',na1*na2,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_complex_2d



      subroutine array_alloc_char_2d &
                               (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      character(len=*),pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray2)
      allocate          (parray2(max(na1,1),max(na2,1)),stat=ier)
      call array_helper &
                  (ier,'character 2D',na1*na2,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_char_2d



      subroutine array_alloc_logical_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      logical         ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: ier            ! local

      call array_free   (parray2)
      allocate          (parray2(max(na1,1),max(na2,1)),stat=ier)
      call array_helper (ier,'logical 2D',na1*na2,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_logical_2d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine array_alloc_integer_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      integer         ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: ier              ! local

      call array_free   (parray3)
      allocate          (parray3(max(na1,1),max(na2,1),max(na3,1)),stat=ier)
      call array_helper &
                 (ier,'integer 3D',na1*na2*na3,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_integer_3d



      subroutine array_alloc_real_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      real            ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: ier              ! local

      call array_free   (parray3)
      allocate          (parray3(max(na1,1),max(na2,1),max(na3,1)),stat=ier)
      call array_helper (ier,'real 3D',na1*na2*na3,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_real_3d



      subroutine array_alloc_double_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      double precision,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: ier              ! local

      call array_free   (parray3)
      allocate          (parray3(max(na1,1),max(na2,1),max(na3,1)),stat=ier)
      call array_helper &
            (ier,'double precision 3D',na1*na2*na3,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_double_3d



      subroutine array_alloc_complex_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      complex         ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: ier              ! local

      call array_free   (parray3)
      allocate          (parray3(max(na1,1),max(na2,1),max(na3,1)),stat=ier)
      call array_helper &
              (ier,'complex 3D',na1*na2*na3,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_complex_3d



      subroutine array_alloc_char_3d &
                             (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      character(len=*),pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: ier              ! local

      call array_free   (parray3)
      allocate          (parray3(max(na1,1),max(na2,1),max(na3,1)),stat=ier)
      call array_helper &
                 (ier,'character 3D',na1*na2*na3,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_char_3d



      subroutine array_alloc_logical_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      logical         ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: ier              ! local

      call array_free   (parray3)
      allocate          (parray3(max(na1,1),max(na2,1),max(na3,1)),stat=ier)
      call array_helper &
                 (ier,'logical 3D',na1*na2*na3,status,msg,keyword,multiple)
      return
      end subroutine array_alloc_logical_3d


!!----------------------------- array realloc -------------------------------!!
!!----------------------------- array realloc -------------------------------!!
!!----------------------------- array realloc -------------------------------!!


      subroutine array_realloc_integer_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      integer         ,pointer                :: parray1(:)   ! arguments
      integer         ,intent(in)             :: na1          ! arguments
      integer         ,intent(inout),optional :: status       ! arguments
      character(len=*),intent(inout),optional :: msg          ! arguments
      character(len=*),intent(in)   ,optional :: keyword      ! arguments
      logical         ,intent(in)   ,optional :: multiple     ! arguments
      integer         ,intent(in)   ,optional :: fill         ! arguments
      integer                                 :: k1,keep      ! local
      integer         ,pointer                :: temp(:)      ! local

      if (.not.associated(parray1)) then
           call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,status,msg,keyword,multiple)
      keep = size(parray1)
      k1 = min(na1,keep)
      temp(1:k1) = parray1(1:k1)
      if (present(fill) .and. na1 > keep) temp(keep+1:) = fill
      call array_free (parray1)
      parray1 => temp
      return
      end subroutine array_realloc_integer_1d



      subroutine array_realloc_real_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      real            ,pointer                :: parray1(:)   ! arguments
      integer         ,intent(in)             :: na1          ! arguments
      integer         ,intent(inout),optional :: status       ! arguments
      character(len=*),intent(inout),optional :: msg          ! arguments
      character(len=*),intent(in)   ,optional :: keyword      ! arguments
      logical         ,intent(in)   ,optional :: multiple     ! arguments
      real            ,intent(in)   ,optional :: fill         ! arguments
      integer                                 :: k1,keep      ! local
      real            ,pointer                :: temp(:)      ! local

      if (.not.associated(parray1)) then
           call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,status,msg,keyword,multiple)
      keep = size(parray1)
      k1 = min(na1,keep)
      temp(1:k1) = parray1(1:k1)
      if (present(fill) .and. na1 > keep) temp(keep+1:) = fill
      call array_free (parray1)
      parray1 => temp
      return
      end subroutine array_realloc_real_1d



      subroutine array_realloc_double_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      double precision   ,pointer             :: parray1(:)   ! arguments
      integer         ,intent(in)             :: na1          ! arguments
      integer         ,intent(inout),optional :: status       ! arguments
      character(len=*),intent(inout),optional :: msg          ! arguments
      character(len=*),intent(in)   ,optional :: keyword      ! arguments
      logical         ,intent(in)   ,optional :: multiple     ! arguments
      double precision,intent(in)   ,optional :: fill         ! arguments
      integer                                 :: k1,keep      ! local
      double precision   ,pointer             :: temp(:)      ! local

      if (.not.associated(parray1)) then
           call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,status,msg,keyword,multiple)
      keep = size(parray1)
      k1 = min(na1,keep)
      temp(1:k1) = parray1(1:k1)
      if (present(fill) .and. na1 > keep) temp(keep+1:) = fill
      call array_free (parray1)
      parray1 => temp
      return
      end subroutine array_realloc_double_1d



      subroutine array_realloc_complex_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      complex         ,pointer                :: parray1(:)   ! arguments
      integer         ,intent(in)             :: na1          ! arguments
      integer         ,intent(inout),optional :: status       ! arguments
      character(len=*),intent(inout),optional :: msg          ! arguments
      character(len=*),intent(in)   ,optional :: keyword      ! arguments
      logical         ,intent(in)   ,optional :: multiple     ! arguments
      complex         ,intent(in)   ,optional :: fill         ! arguments
      integer                                 :: k1,keep      ! local
      complex         ,pointer                :: temp(:)      ! local

      if (.not.associated(parray1)) then
           call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,status,msg,keyword,multiple)
      keep = size(parray1)
      k1 = min(na1,keep)
      temp(1:k1) = parray1(1:k1)
      if (present(fill) .and. na1 > keep) temp(keep+1:) = fill
      call array_free (parray1)
      parray1 => temp
      return
      end subroutine array_realloc_complex_1d



!!!!  subroutine array_realloc_char_1d &
!!!!                            (parray1,na1,status,msg,keyword,multiple,fill)
!!!!  implicit none
!!!!  character(len=*),pointer                :: parray1(:)   ! arguments
!!!!  integer         ,intent(in)             :: na1          ! arguments
!!!!  integer         ,intent(inout),optional :: status       ! arguments
!!!!  character(len=*),intent(inout),optional :: msg          ! arguments
!!!!  character(len=*),intent(in)   ,optional :: keyword      ! arguments
!!!!  logical         ,intent(in)   ,optional :: multiple     ! arguments
!!!!  character(len=*),intent(in)   ,optional :: fill         ! arguments
!!!!  integer                                 :: k1,keep      ! local
!!!!  character(len=len(parray1(1))) ,pointer :: temp(:)      ! local
!!!!
!!!!  if (.not.associated(parray1)) then
!!!!       call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
!!!!       return
!!!!  end if
!!!!
!!!!  nullify (temp)
!!!!  call array_alloc (temp,na1,status,msg,keyword,multiple)
!!!!  keep = size(parray1)
!!!!  k1 = min(na1,keep)
!!!!  temp(1:k1) = parray1(1:k1)
!!!!  if (present(fill) .and. na1 > keep) temp(keep+1:) = fill
!!!!  call array_free (parray1)
!!!!  parray1 => temp
!!!!  return
!!!!  end subroutine array_realloc_char_1d



      subroutine array_realloc_char_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      character(len=*),pointer                :: parray1(:)       ! arguments
      integer         ,intent(in)             :: na1              ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      character(len=*),intent(in)   ,optional :: fill             ! arguments
      integer                                 :: k1,status2,keep  ! local
      character(len=200) ,pointer             :: temp(:)          ! local

      if (.not.associated(parray1)) then
           call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
           return
      end if

      nullify (temp)
      keep = size(parray1)
      k1 = keep
      call array_alloc (temp,k1,status2,msg,keyword,multiple)
      if (status2 /= 0) then
           if (present(status)) status = status2
           return
      end if
      temp = parray1
      deallocate (parray1)
      call array_alloc (parray1,na1,status2,msg,keyword,multiple)
      if (status2 /= 0) then
           call array_free (temp)
           if (present(status)) status = status2
           return
      end if
      k1 = min(na1,k1)
      parray1 = ' '
      parray1(1:k1) = temp(1:k1)
      if (present(fill) .and. na1 > keep) parray1(keep+1:) = fill
      call array_free (temp)
      if (present(status)) status = 0
      if (present(msg   )) msg    = ' '
      return
      end subroutine array_realloc_char_1d



      subroutine array_realloc_logical_1d &
                                (parray1,na1,status,msg,keyword,multiple,fill)
      implicit none
      logical         ,pointer                :: parray1(:)   ! arguments
      integer         ,intent(in)             :: na1          ! arguments
      integer         ,intent(inout),optional :: status       ! arguments
      character(len=*),intent(inout),optional :: msg          ! arguments
      character(len=*),intent(in)   ,optional :: keyword      ! arguments
      logical         ,intent(in)   ,optional :: multiple     ! arguments
      logical         ,intent(in)   ,optional :: fill         ! arguments
      integer                                 :: k1,keep      ! local
      logical         ,pointer                :: temp(:)      ! local

      if (.not.associated(parray1)) then
           call array_alloc (parray1,na1,status,msg,keyword,multiple,fill)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,status,msg,keyword,multiple)
      keep = size(parray1)
      k1 = min(na1,keep)
      temp(1:k1) = parray1(1:k1)
      if (present(fill) .and. na1 > keep) temp(keep+1:) = fill
      call array_free (parray1)
      parray1 => temp
      return
      end subroutine array_realloc_logical_1d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine array_realloc_integer_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      integer         ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: k1,k2          ! local
      integer         ,pointer                :: temp(:,:)      ! local

      if (.not.associated(parray2)) then
           call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,status,msg,keyword,multiple)
      k1 = min(na1,size(parray2,1))
      k2 = min(na2,size(parray2,2))
      temp(1:k1,1:k2) = parray2(1:k1,1:k2)
      call array_free (parray2)
      parray2 => temp
      return
      end subroutine array_realloc_integer_2d



      subroutine array_realloc_real_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      real            ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: k1,k2          ! local
      real            ,pointer                :: temp(:,:)      ! local

      if (.not.associated(parray2)) then
           call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,status,msg,keyword,multiple)
      k1 = min(na1,size(parray2,1))
      k2 = min(na2,size(parray2,2))
      temp(1:k1,1:k2) = parray2(1:k1,1:k2)
      call array_free (parray2)
      parray2 => temp
      return
      end subroutine array_realloc_real_2d



      subroutine array_realloc_double_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      double precision,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: k1,k2          ! local
      double precision,pointer                :: temp(:,:)      ! local

      if (.not.associated(parray2)) then
           call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,status,msg,keyword,multiple)
      k1 = min(na1,size(parray2,1))
      k2 = min(na2,size(parray2,2))
      temp(1:k1,1:k2) = parray2(1:k1,1:k2)
      call array_free (parray2)
      parray2 => temp
      return
      end subroutine array_realloc_double_2d



      subroutine array_realloc_complex_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      complex         ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: k1,k2          ! local
      complex         ,pointer                :: temp(:,:)      ! local

      if (.not.associated(parray2)) then
           call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,status,msg,keyword,multiple)
      k1 = min(na1,size(parray2,1))
      k2 = min(na2,size(parray2,2))
      temp(1:k1,1:k2) = parray2(1:k1,1:k2)
      call array_free (parray2)
      parray2 => temp
      return
      end subroutine array_realloc_complex_2d



!!!!  subroutine array_realloc_char_2d &
!!!!                            (parray2,na1,na2,status,msg,keyword,multiple)
!!!!  implicit none
!!!!  character(len=*),pointer                 :: parray2(:,:)   ! arguments
!!!!  integer         ,intent(in)              :: na1,na2        ! arguments
!!!!  integer         ,intent(inout),optional  :: status         ! arguments
!!!!  character(len=*),intent(inout),optional  :: msg            ! arguments
!!!!  character(len=*),intent(in)   ,optional  :: keyword        ! arguments
!!!!  logical         ,intent(in)   ,optional  :: multiple       ! arguments
!!!!  integer                                  :: k1,k2          ! local
!!!!  character(len=len(parray2(1,1))),pointer :: temp(:,:)      ! local
!!!!
!!!!  if (.not.associated(parray2)) then
!!!!       call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
!!!!       return
!!!!  end if
!!!!
!!!!  nullify (temp)
!!!!  call array_alloc (temp,na1,na2,status,msg,keyword,multiple)
!!!!  k1 = min(na1,size(parray2,1))
!!!!  k2 = min(na2,size(parray2,2))
!!!!  temp(1:k1,1:k2) = parray2(1:k1,1:k2)
!!!!  call array_free (parray2)
!!!!  parray2 => temp
!!!!  return
!!!!  end subroutine array_realloc_char_2d



      subroutine array_realloc_char_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      character(len=*),pointer                :: parray2(:,:)     ! arguments
      integer         ,intent(in)             :: na1,na2          ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,status2    ! local
      character(len=200) ,pointer             :: temp(:,:)        ! local

      if (.not.associated(parray2)) then
           call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      k1 = size(parray2,1)
      k2 = size(parray2,2)
      call array_alloc (temp,k1,k2,status2,msg,keyword,multiple)
      if (status2 /= 0) then
           if (present(status)) status = status2
           return
      end if
      temp = parray2
      deallocate (parray2)
      call array_alloc (parray2,na1,na2,status2,msg,keyword,multiple)
      if (status2 /= 0) then
           call array_free (temp)
           if (present(status)) status = status2
           return
      end if
      k1 = min(na1,k1)
      k2 = min(na2,k2)
      parray2 = ' '
      parray2(1:k1,1:k2) = temp(1:k1,1:k2)
      call array_free (temp)
      if (present(status)) status = 0
      if (present(msg   )) msg    = ' '
      return
      end subroutine array_realloc_char_2d



      subroutine array_realloc_logical_2d &
                                (parray2,na1,na2,status,msg,keyword,multiple)
      implicit none
      logical         ,pointer                :: parray2(:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2        ! arguments
      integer         ,intent(inout),optional :: status         ! arguments
      character(len=*),intent(inout),optional :: msg            ! arguments
      character(len=*),intent(in)   ,optional :: keyword        ! arguments
      logical         ,intent(in)   ,optional :: multiple       ! arguments
      integer                                 :: k1,k2          ! local
      logical         ,pointer                :: temp(:,:)      ! local

      if (.not.associated(parray2)) then
           call array_alloc (parray2,na1,na2,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,status,msg,keyword,multiple)
      k1 = min(na1,size(parray2,1))
      k2 = min(na2,size(parray2,2))
      temp(1:k1,1:k2) = parray2(1:k1,1:k2)
      call array_free (parray2)
      parray2 => temp
      return
      end subroutine array_realloc_logical_2d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine array_realloc_integer_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      integer         ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,k3         ! local
      integer         ,pointer                :: temp(:,:,:)      ! local

      if (.not.associated(parray3)) then
           call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,na3,status,msg,keyword,multiple)
      k1 = min(na1,size(parray3,1))
      k2 = min(na2,size(parray3,2))
      k3 = min(na3,size(parray3,3))
      temp(1:k1,1:k2,1:k3) = parray3(1:k1,1:k2,1:k3)
      call array_free (parray3)
      parray3 => temp
      return
      end subroutine array_realloc_integer_3d



      subroutine array_realloc_real_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      real            ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,k3         ! local
      real            ,pointer                :: temp(:,:,:)      ! local

      if (.not.associated(parray3)) then
           call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,na3,status,msg,keyword,multiple)
      k1 = min(na1,size(parray3,1))
      k2 = min(na2,size(parray3,2))
      k3 = min(na3,size(parray3,3))
      temp(1:k1,1:k2,1:k3) = parray3(1:k1,1:k2,1:k3)
      call array_free (parray3)
      parray3 => temp
      return
      end subroutine array_realloc_real_3d



      subroutine array_realloc_double_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      double precision,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,k3         ! local
      double precision,pointer                :: temp(:,:,:)      ! local

      if (.not.associated(parray3)) then
           call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,na3,status,msg,keyword,multiple)
      k1 = min(na1,size(parray3,1))
      k2 = min(na2,size(parray3,2))
      k3 = min(na3,size(parray3,3))
      temp(1:k1,1:k2,1:k3) = parray3(1:k1,1:k2,1:k3)
      call array_free (parray3)
      parray3 => temp
      return
      end subroutine array_realloc_double_3d



      subroutine array_realloc_complex_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      complex         ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,k3         ! local
      complex         ,pointer                :: temp(:,:,:)      ! local

      if (.not.associated(parray3)) then
           call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,na3,status,msg,keyword,multiple)
      k1 = min(na1,size(parray3,1))
      k2 = min(na2,size(parray3,2))
      k3 = min(na3,size(parray3,3))
      temp(1:k1,1:k2,1:k3) = parray3(1:k1,1:k2,1:k3)
      call array_free (parray3)
      parray3 => temp
      return
      end subroutine array_realloc_complex_3d



!!!!  subroutine array_realloc_char_3d &
!!!!                       (parray3,na1,na2,na3,status,msg,keyword,multiple)
!!!!  implicit none
!!!!  character(len=*),pointer                   :: parray3(:,:,:) ! arguments
!!!!  integer         ,intent(in)                :: na1,na2,na3    ! arguments
!!!!  integer         ,intent(inout),optional    :: status         ! arguments
!!!!  character(len=*),intent(inout),optional    :: msg            ! arguments
!!!!  character(len=*),intent(in)   ,optional    :: keyword        ! arguments
!!!!  logical         ,intent(in)   ,optional    :: multiple       ! arguments
!!!!  integer                                    :: k1,k2,k3       ! local
!!!!  character(len=len(parray3(1,1,1))),pointer :: temp(:,:,:)    ! local
!!!!
!!!!  if (.not.associated(parray3)) then
!!!!       call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
!!!!       return
!!!!  end if
!!!!
!!!!  nullify (temp)
!!!!  call array_alloc (temp,na1,na2,na3,status,msg,keyword,multiple)
!!!!  k1 = min(na1,size(parray3,1))
!!!!  k2 = min(na2,size(parray3,2))
!!!!  k3 = min(na3,size(parray3,3))
!!!!  temp(1:k1,1:k2,1:k3) = parray3(1:k1,1:k2,1:k3)
!!!!  call array_free (parray3)
!!!!  parray3 => temp
!!!!  return
!!!!  end subroutine array_realloc_char_3d



      subroutine array_realloc_char_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      character(len=*),pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,k3,status2 ! local
      character(len=200)           ,pointer   :: temp(:,:,:)      ! local

      if (.not.associated(parray3)) then
           call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      k1 = size(parray3,1)
      k2 = size(parray3,2)
      k3 = size(parray3,3)
      call array_alloc (temp,k1,k2,k3,status2,msg,keyword,multiple)
      if (status2 /= 0) then
           if (present(status)) status = status2
           return
      end if
      temp = parray3
      deallocate (parray3)
      call array_alloc (parray3,na1,na2,na3,status2,msg,keyword,multiple)
      if (status2 /= 0) then
           call array_free (temp)
           if (present(status)) status = status2
           return
      end if
      k1 = min(na1,k1)
      k2 = min(na2,k2)
      k3 = min(na3,k3)
      parray3 = ' '
      parray3(1:k1,1:k2,1:k3) = temp(1:k1,1:k2,1:k3)
      call array_free (temp)
      if (present(status)) status = 0
      if (present(msg   )) msg    = ' '
      return
      end subroutine array_realloc_char_3d



      subroutine array_realloc_logical_3d &
                           (parray3,na1,na2,na3,status,msg,keyword,multiple)
      implicit none
      logical         ,pointer                :: parray3(:,:,:)   ! arguments
      integer         ,intent(in)             :: na1,na2,na3      ! arguments
      integer         ,intent(inout),optional :: status           ! arguments
      character(len=*),intent(inout),optional :: msg              ! arguments
      character(len=*),intent(in)   ,optional :: keyword          ! arguments
      logical         ,intent(in)   ,optional :: multiple         ! arguments
      integer                                 :: k1,k2,k3         ! local
      logical         ,pointer                :: temp(:,:,:)      ! local

      if (.not.associated(parray3)) then
           call array_alloc (parray3,na1,na2,na3,status,msg,keyword,multiple)
           return
      end if

      nullify (temp)
      call array_alloc (temp,na1,na2,na3,status,msg,keyword,multiple)
      k1 = min(na1,size(parray3,1))
      k2 = min(na2,size(parray3,2))
      k3 = min(na3,size(parray3,3))
      temp(1:k1,1:k2,1:k3) = parray3(1:k1,1:k2,1:k3)
      call array_free (parray3)
      parray3 => temp
      return
      end subroutine array_realloc_logical_3d


!!----------------------------- array free ---------------------------------!!
!!----------------------------- array free ---------------------------------!!
!!----------------------------- array free ---------------------------------!!


      subroutine array_free_integer_1d (parray1)
      implicit none
      integer            ,pointer   :: parray1(:)   ! arguments

      if (associated(parray1)) deallocate (parray1)
      return
      end subroutine array_free_integer_1d



      subroutine array_free_real_1d (parray1)
      implicit none
      real               ,pointer   :: parray1(:)   ! arguments

      if (associated(parray1)) deallocate (parray1)
      return
      end subroutine array_free_real_1d



      subroutine array_free_double_1d (parray1)
      implicit none
      double precision   ,pointer   :: parray1(:)   ! arguments

      if (associated(parray1)) deallocate (parray1)
      return
      end subroutine array_free_double_1d



      subroutine array_free_complex_1d (parray1)
      implicit none
      complex            ,pointer   :: parray1(:)   ! arguments

      if (associated(parray1)) deallocate (parray1)
      return
      end subroutine array_free_complex_1d



      subroutine array_free_char_1d (parray1)
      implicit none
      character(len=*)   ,pointer   :: parray1(:)   ! arguments

      if (associated(parray1)) deallocate (parray1)
      return
      end subroutine array_free_char_1d



      subroutine array_free_logical_1d (parray1)
      implicit none
      logical            ,pointer   :: parray1(:)   ! arguments

      if (associated(parray1)) deallocate (parray1)
      return
      end subroutine array_free_logical_1d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine array_free_integer_2d (parray2)
      implicit none
      integer            ,pointer   :: parray2(:,:)   ! arguments

      if (associated(parray2)) deallocate (parray2)
      return
      end subroutine array_free_integer_2d



      subroutine array_free_real_2d (parray2)
      implicit none
      real               ,pointer   :: parray2(:,:)   ! arguments

      if (associated(parray2)) deallocate (parray2)
      return
      end subroutine array_free_real_2d



      subroutine array_free_double_2d (parray2)
      implicit none
      double precision   ,pointer   :: parray2(:,:)   ! arguments

      if (associated(parray2)) deallocate (parray2)
      return
      end subroutine array_free_double_2d



      subroutine array_free_complex_2d (parray2)
      implicit none
      complex            ,pointer   :: parray2(:,:)   ! arguments

      if (associated(parray2)) deallocate (parray2)
      return
      end subroutine array_free_complex_2d



      subroutine array_free_char_2d (parray2)
      implicit none
      character(len=*)   ,pointer   :: parray2(:,:)   ! arguments

      if (associated(parray2)) deallocate (parray2)
      return
      end subroutine array_free_char_2d



      subroutine array_free_logical_2d (parray2)
      implicit none
      logical            ,pointer   :: parray2(:,:)   ! arguments

      if (associated(parray2)) deallocate (parray2)
      return
      end subroutine array_free_logical_2d


                          !!!!!!!!!!!!!!!!!!!!!


      subroutine array_free_integer_3d (parray3)
      implicit none
      integer            ,pointer   :: parray3(:,:,:)   ! arguments

      if (associated(parray3)) deallocate (parray3)
      return
      end subroutine array_free_integer_3d



      subroutine array_free_real_3d (parray3)
      implicit none
      real               ,pointer   :: parray3(:,:,:)   ! arguments

      if (associated(parray3)) deallocate (parray3)
      return
      end subroutine array_free_real_3d



      subroutine array_free_double_3d (parray3)
      implicit none
      double precision   ,pointer   :: parray3(:,:,:)   ! arguments

      if (associated(parray3)) deallocate (parray3)
      return
      end subroutine array_free_double_3d



      subroutine array_free_complex_3d (parray3)
      implicit none
      complex            ,pointer   :: parray3(:,:,:)   ! arguments

      if (associated(parray3)) deallocate (parray3)
      return
      end subroutine array_free_complex_3d



      subroutine array_free_char_3d (parray3)
      implicit none
      character(len=*)   ,pointer   :: parray3(:,:,:)   ! arguments

      if (associated(parray3)) deallocate (parray3)
      return
      end subroutine array_free_char_3d



      subroutine array_free_logical_3d (parray3)
      implicit none
      logical            ,pointer   :: parray3(:,:,:)   ! arguments

      if (associated(parray3)) deallocate (parray3)
      return
      end subroutine array_free_logical_3d


!!---------------------------- insert element -------------------------------!!
!!---------------------------- insert element -------------------------------!!
!!---------------------------- insert element -------------------------------!!


      subroutine array_insert_element_integer (parray1, n1, indx, element, fill)
      implicit none
      integer                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      integer                ,intent(in)    :: element         ! arguments
      integer       ,optional,intent(in)    :: fill            ! arguments
      integer                ,pointer       :: temp(:)         ! local
      integer                               :: nalloc          ! local

      if (indx < 1 .or. indx > n1+1) return

      if (.not.associated(parray1)) then
           allocate(parray1(1))
      else if (size(parray1) > n1) then
           if (indx <= n1) parray1(indx+1:n1+1) = parray1(indx:n1)
      else
           nalloc = n1 + 1 + n1/5
           allocate(temp(nalloc))
           if (indx >   1)    temp(1:indx-1)    = parray1(1:indx-1)
           if (indx <= n1)    temp(indx+1:n1+1) = parray1(indx:n1)
           if (present(fill)) temp(n1+2:)       = fill
           deallocate (parray1)
           parray1 => temp
      end if
      parray1(indx) = element
      n1            = n1 + 1
      return
      end subroutine array_insert_element_integer



      subroutine array_insert_element_real (parray1, n1, indx, element, fill)
      implicit none
      real                   ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      real                   ,intent(in)    :: element         ! arguments
      real          ,optional,intent(in)    :: fill            ! arguments
      real                   ,pointer       :: temp(:)         ! local
      integer                               :: nalloc          ! local

      if (indx < 1 .or. indx > n1+1) return

      if (.not.associated(parray1)) then
           allocate(parray1(1))
      else if (size(parray1) > n1) then
           if (indx <= n1) parray1(indx+1:n1+1) = parray1(indx:n1)
      else
           nalloc = n1 + 1 + n1/5
           allocate(temp(nalloc))
           if (indx >   1)    temp(1:indx-1)    = parray1(1:indx-1)
           if (indx <= n1)    temp(indx+1:n1+1) = parray1(indx:n1)
           if (present(fill)) temp(n1+2:)       = fill
           deallocate (parray1)
           parray1 => temp
      end if
      parray1(indx) = element
      n1            = n1 + 1
      return
      end subroutine array_insert_element_real



      subroutine array_insert_element_double (parray1, n1, indx, element, fill)
      implicit none
      double precision         ,pointer       :: parray1(:)      ! arguments
      integer                  ,intent(inout) :: n1              ! arguments
      integer                  ,intent(in)    :: indx            ! arguments
      double precision         ,intent(in)    :: element         ! arguments
      double precision,optional,intent(in)    :: fill            ! arguments
      double precision         ,pointer       :: temp(:)         ! local
      integer                                 :: nalloc          ! local

      if (indx < 1 .or. indx > n1+1) return

      if (.not.associated(parray1)) then
           allocate(parray1(1))
      else if (size(parray1) > n1) then
           if (indx <= n1) parray1(indx+1:n1+1) = parray1(indx:n1)
      else
           nalloc = n1 + 1 + n1/5
           allocate(temp(nalloc))
           if (indx >   1)    temp(1:indx-1)    = parray1(1:indx-1)
           if (indx <= n1)    temp(indx+1:n1+1) = parray1(indx:n1)
           if (present(fill)) temp(n1+2:)       = fill
           deallocate (parray1)
           parray1 => temp
      end if
      parray1(indx) = element
      n1            = n1 + 1
      return
      end subroutine array_insert_element_double



      subroutine array_insert_element_complex (parray1, n1, indx, element, fill)
      implicit none
      complex                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      complex                ,intent(in)    :: element         ! arguments
      complex       ,optional,intent(in)    :: fill            ! arguments
      complex                ,pointer       :: temp(:)         ! local
      integer                               :: nalloc          ! local
      integer                               :: stat            ! local

      if (indx < 1 .or. indx > n1+1) return

      if (.not.associated(parray1)) then
           allocate(parray1(1))
      else if (size(parray1) > n1) then
           if (indx <= n1) parray1(indx+1:n1+1) = parray1(indx:n1)
      else
           nalloc = n1 + 1 + n1/5
           allocate(temp(nalloc),stat=stat)
           if(stat /= 0 ) print*,'memory problem 1806:array.f90'
           if (indx >   1)    temp(1:indx-1)    = parray1(1:indx-1)
           if (indx <= n1)    temp(indx+1:n1+1) = parray1(indx:n1)
           if (present(fill)) temp(n1+2:)       = fill
           deallocate (parray1,stat=stat)
           if(stat /= 0 )print*,'memory leak 1811:array.f90'
           parray1 => temp
      end if
      parray1(indx) = element
      n1            = n1 + 1
      return
      end subroutine array_insert_element_complex



! This routine also suffers from the compiler bugs described in the
! documentation section:

      subroutine array_insert_element_char (parray1, n1, indx, element, fill)
      implicit none
      character(len=*)         ,pointer       :: parray1(:)      ! arguments
      integer                  ,intent(inout) :: n1              ! arguments
      integer                  ,intent(in)    :: indx            ! arguments
      character(len=*)         ,intent(in)    :: element         ! arguments
      character(len=*),optional,intent(in)    :: fill            ! arguments
      character(len=200)                      :: temp(n1)        ! local
      integer                                 :: nalloc          ! local
      integer                                 :: stat            ! local

      if (indx < 1 .or. indx > n1+1) return

      if (.not.associated(parray1)) then
           allocate(parray1(1),stat=stat)
           if(stat /= 0 ) print*,'array.f90:1839'
      else if (size(parray1) > n1) then
           if (indx <= n1) parray1(indx+1:n1+1) = parray1(indx:n1)
      else
           temp(1:n1) = parray1(1:n1)
           deallocate (parray1,stat=stat)
           if(stat /= 0 ) print*,'array.f90:1845'
           nalloc = n1 + 1 + n1/5
           allocate(parray1(nalloc),stat=stat)
           if(stat /= 0 ) print*,'array.f90:1848'
           if (indx >   1)    parray1(1:indx-1)    = temp(1:indx-1)
           if (indx <= n1)    parray1(indx+1:n1+1) = temp(indx:n1)
           if (present(fill)) parray1(n1+2:)       = fill
      end if
      parray1(indx) = element
      n1            = n1 + 1
      return
      end subroutine array_insert_element_char



      subroutine array_insert_element_logical (parray1, n1, indx, element, fill)
      implicit none
      logical                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      logical                ,intent(in)    :: element         ! arguments
      logical       ,optional,intent(in)    :: fill            ! arguments
      logical                ,pointer       :: temp(:)         ! local
      integer                               :: nalloc          ! local

      if (indx < 1 .or. indx > n1+1) return

      if (.not.associated(parray1)) then
           allocate(parray1(1))
      else if (size(parray1) > n1) then
           if (indx <= n1) parray1(indx+1:n1+1) = parray1(indx:n1)
      else
           nalloc = n1 + 1 + n1/5
           allocate(temp(nalloc))
           if (indx >   1)    temp(1:indx-1)    = parray1(1:indx-1)
           if (indx <= n1)    temp(indx+1:n1+1) = parray1(indx:n1)
           if (present(fill)) temp(n1+2:)       = fill
           deallocate (parray1)
           parray1 => temp
      end if
      parray1(indx) = element
      n1            = n1 + 1
      return
      end subroutine array_insert_element_logical


!!----------------------------- append element ----------------------------!!
!!----------------------------- append element ----------------------------!!
!!----------------------------- append element ----------------------------!!


      subroutine array_append_element_integer (parray1, n1, element, fill)
      implicit none
      integer                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: element         ! arguments
      integer       ,optional,intent(in)    :: fill            ! arguments

      call array_insert_element (parray1, n1, n1 + 1, element, fill)
      return
      end subroutine array_append_element_integer



      subroutine array_append_element_real (parray1, n1, element, fill)
      implicit none
      real                   ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      real                   ,intent(in)    :: element         ! arguments
      real          ,optional,intent(in)    :: fill            ! arguments

      call array_insert_element (parray1, n1, n1 + 1, element, fill)
      return
      end subroutine array_append_element_real



      subroutine array_append_element_double (parray1, n1, element, fill)
      implicit none
      double precision         ,pointer       :: parray1(:)      ! arguments
      integer                  ,intent(inout) :: n1              ! arguments
      double precision         ,intent(in)    :: element         ! arguments
      double precision,optional,intent(in)    :: fill            ! arguments

      call array_insert_element (parray1, n1, n1 + 1, element, fill)
      return
      end subroutine array_append_element_double



      subroutine array_append_element_complex (parray1, n1, element, fill)
      implicit none
      complex                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      complex                ,intent(in)    :: element         ! arguments
      complex       ,optional,intent(in)    :: fill            ! arguments

      call array_insert_element (parray1, n1, n1 + 1, element, fill)
      return
      end subroutine array_append_element_complex



      subroutine array_append_element_char (parray1, n1, element, fill)
      implicit none
      character(len=*)         ,pointer       :: parray1(:)      ! arguments
      integer                  ,intent(inout) :: n1              ! arguments
      character(len=*)         ,intent(in)    :: element         ! arguments
      character(len=*),optional,intent(in)    :: fill            ! arguments

      call array_insert_element (parray1, n1, n1 + 1, element, fill)
      return
      end subroutine array_append_element_char



      subroutine array_append_element_logical (parray1, n1, element, fill)
      implicit none
      logical                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      logical                ,intent(in)    :: element         ! arguments
      logical       ,optional,intent(in)    :: fill            ! arguments

      call array_insert_element (parray1, n1, n1 + 1, element, fill)
      return
      end subroutine array_append_element_logical


!!----------------------------- remove element ----------------------------!!
!!----------------------------- remove element ----------------------------!!
!!----------------------------- remove element ----------------------------!!


      subroutine array_remove_element_integer (parray1, n1, indx, fill)
      implicit none
      integer                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      integer       ,optional,intent(in)    :: fill            ! arguments

      if (indx < 1 .or. indx > n1) return
      if (.not.associated(parray1)) return

      if (indx < n1) parray1(indx:n1-1) = parray1(indx+1:n1)
      if (present(fill)) parray1(n1) = fill
      n1 = n1 - 1
      return
      end subroutine array_remove_element_integer



      subroutine array_remove_element_real (parray1, n1, indx, fill)
      implicit none
      real                   ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      real          ,optional,intent(in)    :: fill            ! arguments

      if (indx < 1 .or. indx > n1) return
      if (.not.associated(parray1)) return

      if (indx < n1) parray1(indx:n1-1) = parray1(indx+1:n1)
      if (present(fill)) parray1(n1) = fill
      n1 = n1 - 1
      return
      end subroutine array_remove_element_real



      subroutine array_remove_element_double (parray1, n1, indx, fill)
      implicit none
      double precision         ,pointer       :: parray1(:)      ! arguments
      integer                  ,intent(inout) :: n1              ! arguments
      integer                  ,intent(in)    :: indx            ! arguments
      double precision,optional,intent(in)    :: fill            ! arguments

      if (indx < 1 .or. indx > n1) return
      if (.not.associated(parray1)) return

      if (indx < n1) parray1(indx:n1-1) = parray1(indx+1:n1)
      if (present(fill)) parray1(n1) = fill
      n1 = n1 - 1
      return
      end subroutine array_remove_element_double



      subroutine array_remove_element_complex (parray1, n1, indx, fill)
      implicit none
      complex                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      complex       ,optional,intent(in)    :: fill            ! arguments

      if (indx < 1 .or. indx > n1) return
      if (.not.associated(parray1)) return

      if (indx < n1) parray1(indx:n1-1) = parray1(indx+1:n1)
      if (present(fill)) parray1(n1) = fill
      n1 = n1 - 1
      return
      end subroutine array_remove_element_complex



      subroutine array_remove_element_char (parray1, n1, indx, fill)
      implicit none
      character(len=*)         ,pointer       :: parray1(:)      ! arguments
      integer                  ,intent(inout) :: n1              ! arguments
      integer                  ,intent(in)    :: indx            ! arguments
      character(len=*),optional,intent(in)    :: fill            ! arguments

      if (indx < 1 .or. indx > n1) return
      if (.not.associated(parray1)) return

      if (indx < n1) parray1(indx:n1-1) = parray1(indx+1:n1)
      if (present(fill)) parray1(n1) = fill
      n1 = n1 - 1
      return
      end subroutine array_remove_element_char



      subroutine array_remove_element_logical (parray1, n1, indx, fill)
      implicit none
      logical                ,pointer       :: parray1(:)      ! arguments
      integer                ,intent(inout) :: n1              ! arguments
      integer                ,intent(in)    :: indx            ! arguments
      logical       ,optional,intent(in)    :: fill            ! arguments

      if (indx < 1 .or. indx > n1) return
      if (.not.associated(parray1)) return

      if (indx < n1) parray1(indx:n1-1) = parray1(indx+1:n1)
      if (present(fill)) parray1(n1) = fill
      n1 = n1 - 1
      return
      end subroutine array_remove_element_logical



!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module array_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

