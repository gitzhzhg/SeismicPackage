
!<CPS_v1 type="AUXILIARY_FILE"/>
!!---------------------------- pjar_frou.f90 --------------------------------!!
!!---------------------------- pjar_frou.f90 --------------------------------!!
!!---------------------------- pjar_frou.f90 --------------------------------!!

               ! other files are:  pjar.f90  pjar.cc  pjar.hh


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
!                   C P S   A U X I L I A R Y   F I L E
!
! Name       : PJAR_FROU
! Category   : character
! Written    : 2002-04-11   by: Tom Stoeckley
! Revised    : 2002-07-19   by: Tom Stoeckley
! Maturity   : production   2002-08-12
! Purpose    : Parameter container using a CARDSETLIST object.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2002-08-12  Stoeckley  Add pjar_frou_copy.
!  2. 2002-04-22  Stoeckley  Fix compiler warnings.
!  1. 2002-04-11  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!


      module pjar_frou_module

      use pjar_module
      use grid_module
      use grid_frou_module
      use string_module
      use convert_module
      implicit none
      public

      character(len=100),public,save :: PJAR_FROU_IDENT = &
'$Id: pjar_frou.f90,v 1.3 2002/08/07 16:14:09 Stoeckley prod sps $'

      type :: pjar_frou_struct
        type(pjar_struct),pointer :: obj
      end type

      end module pjar_frou_module


!!------------------------ create and delete and clear --------------------!!
!!------------------------ create and delete and clear --------------------!!
!!------------------------ create and delete and clear --------------------!!


      subroutine pjar_frou_create (fpoint)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(out) :: fpoint            ! argument
      type(pjar_struct)     ,pointer     :: obj               ! local

      call pjar_create (obj)
      fpoint%obj => obj
      return
      end subroutine



      subroutine pjar_frou_delete (fpoint)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call pjar_delete (obj)
      fpoint%obj => obj
      return
      end subroutine



      subroutine pjar_frou_clear (fpoint)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call pjar_clear (obj)
      return
      end subroutine



      subroutine pjar_frou_copy (fpoint1,fpoint2)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(in)    :: fpoint1           ! argument
      type(pjar_frou_struct),intent(inout) :: fpoint2           ! argument
      type(pjar_struct)     ,pointer       :: obj1,obj2         ! local

      obj1 => fpoint1%obj
      obj2 => fpoint2%obj
      call pjar_copy (obj1,obj2)
      return
      end subroutine



      subroutine pjar_frou_print (fpoint)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call pjar_print (obj, 6)
      return
      end subroutine


!!------------------------ choose an active section ------------------------!!
!!------------------------ choose an active section ------------------------!!
!!------------------------ choose an active section ------------------------!!


      function pjar_frou_num_sections (fpoint)  result  (nsections)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer                              :: nsections         ! result
      type(pjar_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      nsections = pjar_num_sections (obj)
      return
      end function



      function pjar_frou_find_section (fpoint,secname)  result  (indx)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: secname(*)        ! argument
      integer                              :: indx              ! result
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: secname9          ! local

      obj => fpoint%obj
      call string_hh2cc        (secname,secname9)
      indx = pjar_find_section (obj,secname9) - 1
      return
      end function



      subroutine pjar_frou_choose_section_indx (fpoint,indx)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: indx              ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call pjar_choose_section (obj,indx+1)
      return
      end subroutine



      subroutine pjar_frou_choose_section_name (fpoint,secname)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: secname(*)        ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: secname9          ! local

      obj => fpoint%obj
      call string_hh2cc        (secname,secname9)
      call pjar_choose_section (obj,secname9)
      return
      end subroutine



      subroutine pjar_frou_choose_no_section (fpoint)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call pjar_choose_no_section (obj)
      return
      end subroutine



      subroutine pjar_frou_get_secname (fpoint,secname)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(out)   :: secname(*)        ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: secname9          ! local

      obj => fpoint%obj
      call pjar_get_secname (obj,secname9)
      call string_cc2hh     (secname9,secname)
      return
      end subroutine



      subroutine pjar_frou_get_secnames &
                                (fpoint,secnames,nsections,nalloc,nwords)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint                  ! arg
      integer               ,intent(out)   :: nsections               ! arg
      integer               ,intent(in)    :: nalloc                  ! arg
      integer               ,intent(in)    :: nwords                  ! arg
      integer               ,intent(out)   :: secnames(nwords,nalloc) ! arg
      type(pjar_struct)     ,pointer       :: obj                     ! local
      character(len=40)                    :: secnames9(nalloc)       ! local

      obj => fpoint%obj
      call pjar_get_secnames  (obj,secnames9,nsections)
      call string_cc2hh_array (secnames9,secnames,nwords,nsections)
      return
      end subroutine


!!--------------------------------- status ----------------------------------!!
!!--------------------------------- status ----------------------------------!!
!!--------------------------------- status ----------------------------------!!


      subroutine pjar_frou_status (fpoint,errmsg)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(out)   :: errmsg(*)         ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=80)                    :: errmsg9           ! local

      obj => fpoint%obj
      call pjar_status  (obj,errmsg9)
      call string_cc2hh (errmsg9,errmsg)
      return
      end subroutine


!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!


      function pjar_frou_num_elements (fpoint,keyword) result (nelements)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer                              :: nelements         ! result
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc             (keyword,keyword9)
      nelements = pjar_num_elements (obj,keyword9)
      return
      end function


!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!


      function pjar_frou_nature (fpoint,keyword) result (nature)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer                              :: nature            ! result
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc    (keyword,keyword9)
      nature = pjar_nature (obj,keyword9)
      return
      end function



!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!


      subroutine pjar_frou_getg (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint           ! argument
      integer               ,intent(in)    :: keyword(*)       ! argument
      type(grid_frou_struct),intent(inout) :: scalar           ! argument
      type(pjar_struct)     ,pointer       :: obj              ! local
      character(len=40)                    :: keyword9         ! local
      type(grid_struct)     ,pointer       :: scalar9          ! local

      obj     => fpoint%obj
      scalar9 => scalar%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,scalar9)
      scalar%obj => scalar9
      return
      end subroutine



      subroutine pjar_frou_geti (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(out)   :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,scalar)
      return
      end subroutine



      subroutine pjar_frou_getf (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      real                  ,intent(out)   :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,scalar)
      return
      end subroutine



      subroutine pjar_frou_getd (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      double precision      ,intent(out)   :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,scalar)
      return
      end subroutine



      subroutine pjar_frou_getl (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(out)   :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      logical                              :: scalar9           ! local

      obj => fpoint%obj
      call string_hh2cc  (keyword,keyword9)
      call pjar_get      (obj,keyword9,scalar9)
      call convert_ll2ii (scalar9,scalar)
      return
      end subroutine



      subroutine pjar_frou_getc (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(out)   :: scalar(*)         ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      character(len=PJAR_LENGTH)           :: scalar9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,scalar9)
      call string_cc2hh (scalar9,scalar)
      return
      end subroutine


!!---------------------- get elements -------------------------------------!!
!!---------------------- get elements -------------------------------------!!
!!---------------------- get elements -------------------------------------!!


      subroutine pjar_frou_geti_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      integer               ,intent(out)   :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_get_element (obj,keyword9,indx+1,element)
      return
      end subroutine



      subroutine pjar_frou_getf_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      real                  ,intent(out)   :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_get_element (obj,keyword9,indx+1,element)
      return
      end subroutine



      subroutine pjar_frou_getd_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      double precision      ,intent(out)   :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_get_element (obj,keyword9,indx+1,element)
      return
      end subroutine



      subroutine pjar_frou_getl_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      integer               ,intent(out)   :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      logical                              :: element9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_get_element (obj,keyword9,indx+1,element9)
      call convert_ll2ii    (element9,element)
      return
      end subroutine



      subroutine pjar_frou_getc_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      integer               ,intent(out)   :: element(*)        ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      character(len=PJAR_LENGTH)           :: element9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_get_element (obj,keyword9,indx+1,element9)
      call string_cc2hh     (element9,element)
      return
      end subroutine


!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!


      subroutine pjar_frou_getii (fpoint,keyword,array,nelements,nalloc)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(out)   :: nelements          ! argument
      integer               ,intent(in)    :: nalloc             ! argument
      integer               ,intent(out)   :: array(nalloc)      ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_getff (fpoint,keyword,array,nelements,nalloc)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(out)   :: nelements          ! argument
      integer               ,intent(in)    :: nalloc             ! argument
      real                  ,intent(out)   :: array(nalloc)      ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_getdd (fpoint,keyword,array,nelements,nalloc)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(out)   :: nelements          ! argument
      integer               ,intent(in)    :: nalloc             ! argument
      double precision      ,intent(out)   :: array(nalloc)      ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_get     (obj,keyword9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_getll (fpoint,keyword,array,nelements,nalloc)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(out)   :: nelements          ! argument
      integer               ,intent(in)    :: nalloc             ! argument
      integer               ,intent(out)   :: array(nalloc)      ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local
      logical                              :: array9(nalloc)     ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call pjar_get            (obj,keyword9,array9,nelements)
      call convert_ll2ii_array (array9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_getcc (fpoint,keyword,array,nelements,nalloc,nwords)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint                ! argument
      integer               ,intent(in)    :: keyword(*)            ! argument
      integer               ,intent(out)   :: nelements             ! argument
      integer               ,intent(in)    :: nalloc,nwords         ! argument
      integer               ,intent(out)   :: array(nwords,nalloc)  ! argument
      type(pjar_struct)     ,pointer       :: obj                   ! local
      character(len=40)                    :: keyword9              ! local
      character(len=PJAR_LENGTH)           :: array9(nalloc)        ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call pjar_get           (obj,keyword9,array9,nelements)
      call string_cc2hh_array (array9,array,nwords,nelements)
      return
      end subroutine


!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!


      subroutine pjar_frou_putg (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      type(grid_frou_struct),intent(in)    :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      type(grid_struct)     ,pointer       :: scalar9           ! local

      obj     => fpoint%obj
      scalar9 => scalar%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,scalar9)
      return
      end subroutine



      subroutine pjar_frou_puti (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,scalar)
      return
      end subroutine



      subroutine pjar_frou_putf (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      real                  ,intent(in)    :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,scalar)
      return
      end subroutine



      subroutine pjar_frou_putd (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      double precision      ,intent(in)    :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,scalar)
      return
      end subroutine



      subroutine pjar_frou_putc (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: scalar(*)         ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      character(len=PJAR_LENGTH)           :: scalar9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call string_hh2cc (scalar,scalar9)
      call pjar_put     (obj,keyword9,scalar9)
      return
      end subroutine



      subroutine pjar_frou_putl (fpoint,keyword,scalar)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: scalar            ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      logical                              :: scalar9           ! local

      obj => fpoint%obj
      call string_hh2cc  (keyword,keyword9)
      call convert_ii2ll (scalar,scalar9)
      call pjar_put      (obj,keyword9,scalar9)
      return
      end subroutine


!!------------------------- put elements -----------------------------------!!
!!------------------------- put elements -----------------------------------!!
!!------------------------- put elements -----------------------------------!!


      subroutine pjar_frou_puti_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      integer               ,intent(in)    :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_put_element (obj,keyword9,indx+1,element)
      return
      end subroutine



      subroutine pjar_frou_putf_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      real                  ,intent(in)    :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_put_element (obj,keyword9,indx+1,element)
      return
      end subroutine



      subroutine pjar_frou_putd_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      double precision      ,intent(in)    :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call pjar_put_element (obj,keyword9,indx+1,element)
      return
      end subroutine



      subroutine pjar_frou_putc_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      integer               ,intent(in)    :: element(*)        ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      character(len=PJAR_LENGTH)           :: element9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call string_hh2cc     (element,element9)
      call pjar_put_element (obj,keyword9,indx+1,element9)
      return
      end subroutine



      subroutine pjar_frou_putl_element (fpoint,keyword,indx,element)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      integer               ,intent(in)    :: element           ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local
      logical                              :: element9          ! local

      obj => fpoint%obj
      call string_hh2cc     (keyword,keyword9)
      call convert_ii2ll    (element,element9)
      call pjar_put_element (obj,keyword9,indx+1,element9)
      return
      end subroutine


!!-------------------------- access buffer ----------------------------------!!
!!-------------------------- access buffer ----------------------------------!!
!!-------------------------- access buffer ----------------------------------!!


      subroutine pjar_frou_insert_element (fpoint,keyword,indx)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call pjar_insert_element (obj,keyword9,indx+1)
      return
      end subroutine



      subroutine pjar_frou_remove_element (fpoint,keyword,indx)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      integer               ,intent(in)    :: indx              ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call pjar_remove_element (obj,keyword9,indx+1)
      return
      end subroutine



      subroutine pjar_frou_clear_buffer (fpoint,keyword)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint            ! argument
      integer               ,intent(in)    :: keyword(*)        ! argument
      type(pjar_struct)     ,pointer       :: obj               ! local
      character(len=40)                    :: keyword9          ! local

      obj => fpoint%obj
      call string_hh2cc      (keyword,keyword9)
      call pjar_clear_buffer (obj,keyword9)
      return
      end subroutine


!!---------------------------- find ------------------------------------!!
!!---------------------------- find ------------------------------------!!
!!---------------------------- find ------------------------------------!!


      function pjar_frou_find (fpoint,keyword,element) result (indx)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(in)    :: element(*)         ! argument
      integer                              :: indx               ! result
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local
      character(len=40)                    :: element9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call string_hh2cc (element,element9)
      indx = pjar_find  (obj,keyword9,element9) - 1
      return
      end function


!!---------------------------- find add --------------------------------!!
!!---------------------------- find add --------------------------------!!
!!---------------------------- find add --------------------------------!!


      function pjar_frou_find_add (fpoint,keyword,element) result (indx)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(in)    :: element(*)         ! argument
      integer                              :: indx               ! result
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local
      character(len=40)                    :: element9           ! local

      obj => fpoint%obj
      call string_hh2cc    (keyword,keyword9)
      call string_hh2cc    (element,element9)
      indx = pjar_find_add (obj,keyword9,element9) - 1
      return
      end function


!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!


      subroutine pjar_frou_putii (fpoint,keyword,array,nelements)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(in)    :: nelements          ! argument
      integer               ,intent(in)    :: array(nelements)   ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_putff (fpoint,keyword,array,nelements)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(in)    :: nelements          ! argument
      real                  ,intent(in)    :: array(nelements)   ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_putdd (fpoint,keyword,array,nelements)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(in)    :: nelements          ! argument
      double precision      ,intent(in)    :: array(nelements)   ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc (keyword,keyword9)
      call pjar_put     (obj,keyword9,array,nelements)
      return
      end subroutine



      subroutine pjar_frou_putcc (fpoint,keyword,array,nelements,nwords)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint                  ! argument
      integer               ,intent(in)    :: keyword(*)              ! argument
      integer               ,intent(in)    :: nelements,nwords        ! argument
      integer               ,intent(in)    :: array(nwords,nelements) ! argument
      type(pjar_struct)     ,pointer       :: obj                     ! local
      character(len=40)                    :: keyword9                ! local
      character(len=PJAR_LENGTH)           :: array9(nelements)       ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call pjar_put           (obj,keyword9,array9,nelements)
      return
      end subroutine



      subroutine pjar_frou_putll (fpoint,keyword,array,nelements)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      integer               ,intent(in)    :: nelements          ! argument
      integer               ,intent(in)    :: array(nelements)   ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local
      logical                              :: array9(nelements)  ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call pjar_put            (obj,keyword9,array9,nelements)
      return
      end subroutine


!!------------------------ get and put data cards -------------------------!!
!!------------------------ get and put data cards -------------------------!!
!!------------------------ get and put data cards -------------------------!!


      function pjar_frou_num_cards (fpoint) result (ncards)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint           ! argument
      integer                              :: ncards           ! result
      type(pjar_struct)     ,pointer       :: obj              ! local

      obj => fpoint%obj
      ncards = pjar_num_cards (obj)
      return
      end function



      subroutine pjar_frou_clear_cards (fpoint)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint                 ! argument
      type(pjar_struct)     ,pointer       :: obj                    ! local

      obj => fpoint%obj
      call pjar_clear_cards (obj)
      return
      end subroutine



      subroutine pjar_frou_get_cards (fpoint,cards,ncards,nalloc,nwords)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint                 ! argument
      integer               ,intent(out)   :: ncards                 ! argument
      integer               ,intent(in)    :: nalloc,nwords          ! argument
      integer               ,intent(out)   :: cards(nwords,nalloc)   ! argument
      type(pjar_struct)     ,pointer       :: obj                    ! local
      character(len=PJAR_DATACARD_LENGTH)  :: cards9(nalloc)         ! local

      obj => fpoint%obj
      call pjar_get_cards     (obj,cards9,ncards)
      call string_cc2hh_array (cards9,cards,nwords,ncards)
      return
      end subroutine


 
      subroutine pjar_frou_put_cards (fpoint,cards,ncards,nwords,progname)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint                 ! argument
      integer               ,intent(in)    :: ncards,nwords          ! argument
      integer               ,intent(in)    :: cards(nwords,ncards)   ! argument
      integer               ,intent(in)    :: progname(*)            ! argument
      type(pjar_struct)     ,pointer       :: obj                    ! local
      character(len=PJAR_DATACARD_LENGTH)  :: cards9(ncards)         ! local
      character(len=40)                    :: progname9              ! local

      obj => fpoint%obj
      call string_hh2cc_array (cards,cards9,nwords,ncards)
      call string_hh2cc       (progname,progname9)
      call pjar_put_cards     (obj,cards9,ncards,progname9)
      return
      end subroutine



      subroutine pjar_frou_get_card (fpoint,indx,card)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint              ! argument
      integer               ,intent(in)    :: indx                ! argument
      integer               ,intent(out)   :: card(*)             ! argument
      type(pjar_struct)     ,pointer       :: obj                 ! local
      character(len=80)                    :: card9               ! local

      obj => fpoint%obj
      call pjar_get_card (obj,indx+1,card9)
      call string_cc2hh  (card9,card)
      return
      end subroutine



      subroutine pjar_frou_add_card (fpoint,card)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint              ! argument
      integer               ,intent(in)    :: card(*)             ! argument
      type(pjar_struct)     ,pointer       :: obj                 ! local
      character(len=80)                    :: card9               ! local

      obj => fpoint%obj
      call string_hh2cc  (card,card9)
      call pjar_add_card (obj,card9)
      return
      end subroutine



      subroutine pjar_frou_new_card (fpoint,progname)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint              ! argument
      integer               ,intent(in)    :: progname(*)         ! argument
      type(pjar_struct)     ,pointer       :: obj                 ! local
      character(len=80)                    :: progname9           ! local

      obj => fpoint%obj
      call string_hh2cc  (progname,progname9)
      call pjar_new_card (obj,progname9)
      return
      end subroutine


!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!


      function pjar_frou_keyword_present (fpoint,keyword) result (present)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint              ! argument
      integer               ,intent(in)    :: keyword(*)          ! argument
      integer                              :: present             ! result  
      type(pjar_struct)     ,pointer       :: obj                 ! local
      character(len=40)                    :: keyword9            ! local
      logical                              :: present9            ! local

      obj => fpoint%obj
      call string_hh2cc               (keyword,keyword9)
      present9 = pjar_keyword_present (obj,keyword9)
      call convert_ll2ii              (present9,present)
      return
      end function



      function pjar_frou_num_keywords (fpoint) result (nkeys)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer                              :: nkeys              ! result  
      type(pjar_struct)     ,pointer       :: obj                ! local

      obj => fpoint%obj
      nkeys = pjar_num_keywords (obj)
      return
      end function



      subroutine pjar_frou_get_keyword (fpoint,indx,keyword)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: indx               ! argument
      integer               ,intent(out)   :: keyword(*)         ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      keyword9 = pjar_get_keyword (obj,indx+1)
      call string_cc2hh           (keyword9,keyword)
      return
      end subroutine



      subroutine pjar_frou_remove_keyword (fpoint,keyword)
      use pjar_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: fpoint             ! argument
      integer               ,intent(in)    :: keyword(*)         ! argument
      type(pjar_struct)     ,pointer       :: obj                ! local
      character(len=40)                    :: keyword9           ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call pjar_remove_keyword (obj,keyword9)
      return
      end subroutine


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

