!<CPS_v1 type="AUXILIARY_FILE"/>
!!------------------------------ parlist_frou.f90 -------------------------------!!
!!------------------------------ parlist_frou.f90 -------------------------------!!
!!------------------------------ parlist_frou.f90 -------------------------------!!

     ! other files are:  parlist.f90  parameter_list.hh  parameter_list.cc

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
! Name       : PARLIST
! Category   : character
! Written    : 2010-07-08   by: Tom Stoeckley
! Revised    : 2010-07-08   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Parameter list.
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
!  1. 2010-07-08  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!
!!----------------------------- module ------------------------------------!!

      module parlist_frou_module

      use parlist_module
      use string_module
      use convert_module
      implicit none
      public

      character(len=100),public,save :: PARLIST_FROU_IDENT = &
'$Id: parlist_frou.f90,v 1.3 2007/09/19 14:02:25 Stoeckley beta sps $'

      integer,parameter,public :: PARLIST_MESSAGE_LENGTH = 80
      integer,parameter,public :: PARLIST_KEYWORD_LENGTH = 80

      type,public :: parlist_frou_struct
        type(parlist_struct),pointer :: obj
      end type parlist_frou_struct

      end module parlist_frou_module

!!----------------------- create and delete -----------------------------!!
!!----------------------- create and delete -----------------------------!!
!!----------------------- create and delete -----------------------------!!

      subroutine parlist_frou_create (fpoint)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(out) :: fpoint         ! argument
      type(parlist_struct)     ,pointer     :: obj            ! local

      nullify (obj)
      call parlist_create (obj)
      fpoint%obj => obj
      end subroutine parlist_frou_create



      subroutine parlist_frou_delete (fpoint)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local

      obj => fpoint%obj
      call parlist_delete (obj)
      fpoint%obj => obj
      end subroutine parlist_frou_delete

!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!

      subroutine parlist_frou_clear (fpoint)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local

      obj => fpoint%obj
      call parlist_clear (obj)
      end subroutine parlist_frou_clear

!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!

      function parlist_frou_num_elements (fpoint,keyword) result (nelements)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(in)  :: fpoint         ! argument
      integer                  ,intent(in)  :: keyword(*)     ! argument
      integer                               :: nelements      ! result
      type(parlist_struct)     ,pointer     :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH) :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc                (keyword,keyword9)
      nelements = parlist_num_elements (obj,keyword9)
      end function parlist_frou_num_elements

!!---------------------- is scalar and is array ---------------------------!!
!!---------------------- is scalar and is array ---------------------------!!
!!---------------------- is scalar and is array ---------------------------!!

      function parlist_frou_is_scalar (fpoint,keyword) result (is_scalar)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(in)  :: fpoint         ! argument
      integer                  ,intent(in)  :: keyword(*)     ! argument
      integer                               :: is_scalar      ! result
      integer                               :: nature         ! local
      logical                               :: is_scalar9     ! local
      type(parlist_struct)     ,pointer     :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH) :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      nature = parlist_nature (obj,keyword9)
      is_scalar = (nature == PARLIST_SCALAR);
      call convert_ll2ii      (is_scalar9,is_scalar)
      end function parlist_frou_is_scalar


      function parlist_frou_is_array (fpoint,keyword) result (is_array)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(in)  :: fpoint         ! argument
      integer                  ,intent(in)  :: keyword(*)     ! argument
      integer                               :: is_array       ! result
      integer                               :: nature         ! local
      logical                               :: is_array9      ! local
      type(parlist_struct)     ,pointer     :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH) :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      nature = parlist_nature (obj,keyword9)
      is_array = (nature == PARLIST_ARRAY);
      call convert_ll2ii      (is_array9,is_array)
      end function parlist_frou_is_array

!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!

      subroutine parlist_frou_get_iscalar (fpoint,keyword,scalar,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(out)   :: scalar         ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call parlist_get_iscalar (obj,keyword9,scalar,errmsg9)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine parlist_frou_get_iscalar



      subroutine parlist_frou_get_fscalar (fpoint,keyword,scalar,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      real                     ,intent(out)   :: scalar         ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call parlist_get_fscalar (obj,keyword9,scalar,errmsg9)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine parlist_frou_get_fscalar



      subroutine parlist_frou_get_dscalar (fpoint,keyword,scalar,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      double precision         ,intent(out)   :: scalar         ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call parlist_get_dscalar (obj,keyword9,scalar,errmsg9)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine parlist_frou_get_dscalar



      subroutine parlist_frou_get_lscalar (fpoint,keyword,scalar,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(inout) :: scalar         ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local
      logical                                 :: scalar9        ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll       (scalar,scalar9)
      call parlist_get_lscalar (obj,keyword9,scalar9,errmsg9)
      call convert_ll2ii       (scalar9,scalar)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine parlist_frou_get_lscalar



      subroutine parlist_frou_get_cscalar (fpoint,keyword,scalar,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(inout) :: scalar(*)      ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: scalar9        ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (scalar,scalar9)
      call parlist_get_cscalar (obj,keyword9,scalar9,errmsg9)
      call string_cc2hh        (scalar9,scalar)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine parlist_frou_get_cscalar

!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!

      subroutine parlist_frou_get_iarray (fpoint,keyword,nsize,array,nelements,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: nsize          ! argument
      integer                  ,intent(out)   :: array(nsize)   ! argument
      integer                  ,intent(out)   :: nelements      ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call parlist_get_iarray (obj,keyword9,array,nelements,errmsg9)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine parlist_frou_get_iarray




      subroutine parlist_frou_get_farray (fpoint,keyword,nsize,array,nelements,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: nsize          ! argument
      real                     ,intent(out)   :: array(nsize)   ! argument
      integer                  ,intent(out)   :: nelements      ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call parlist_get_farray (obj,keyword9,array,nelements,errmsg9)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine parlist_frou_get_farray




      subroutine parlist_frou_get_darray (fpoint,keyword,nsize,array,nelements,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: nsize          ! argument
      double precision         ,intent(out)   :: array(nsize)   ! argument
      integer                  ,intent(out)   :: nelements      ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call parlist_get_darray (obj,keyword9,array,nelements,errmsg9)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine parlist_frou_get_darray




      subroutine parlist_frou_get_carray (fpoint,keyword,nsize,array,nelements,nwords,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: nsize          ! argument
      integer                  ,intent(inout) :: array(*)       ! argument
      integer                  ,intent(inout) :: nelements      ! argument
      integer                  ,intent(in)    :: nwords         ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local
      character(len=PARLIST_LENGTH)           :: array9(nsize)  ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call parlist_get_carray (obj,keyword9,array9,nelements,errmsg9)
      call string_cc2hh_array (array9,array,nwords,nelements)
      call string_cc2hh       (errmsg9,errmsg)
      end subroutine parlist_frou_get_carray




      subroutine parlist_frou_get_larray (fpoint,keyword,nsize,array,nelements,errmsg)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: nsize          ! argument
      integer                  ,intent(inout) :: array(*)       ! argument
      integer                  ,intent(inout) :: nelements      ! argument
      integer                  ,intent(out)   :: errmsg(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: errmsg9        ! local
      logical                                 :: array9(nsize)  ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call parlist_get_larray  (obj,keyword9,array9,nelements,errmsg9)
      call convert_ll2ii_array (array9,array,nelements)
      call string_cc2hh        (errmsg9,errmsg)
      end subroutine parlist_frou_get_larray

!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!

      subroutine parlist_frou_put_iscalar (fpoint,keyword,scalar)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: scalar         ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call parlist_put_iscalar (obj,keyword9,scalar)
      end subroutine parlist_frou_put_iscalar



      subroutine parlist_frou_put_fscalar (fpoint,keyword,scalar)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      real                     ,intent(in)    :: scalar         ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call parlist_put_fscalar (obj,keyword9,scalar)
      end subroutine parlist_frou_put_fscalar



      subroutine parlist_frou_put_dscalar (fpoint,keyword,scalar)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      double precision         ,intent(in)    :: scalar         ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call parlist_put_dscalar (obj,keyword9,scalar)
      end subroutine parlist_frou_put_dscalar



      subroutine parlist_frou_put_cscalar (fpoint,keyword,scalar)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                  ,intent(in)    :: scalar(*)      ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      character(len=PARLIST_LENGTH)           :: scalar9        ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call string_hh2cc        (scalar,scalar9)
      call parlist_put_cscalar (obj,keyword9,scalar9)
      end subroutine parlist_frou_put_cscalar



      subroutine parlist_frou_put_lscalar (fpoint,keyword,scalar)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint           ! argument
      integer                  ,intent(in)    :: keyword(*)       ! argument
      integer                  ,intent(in)    :: scalar           ! argument
      type(parlist_struct)     ,pointer       :: obj              ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9         ! local
      logical                                 :: scalar9          ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll       (scalar,scalar9)
      call parlist_put_lscalar (obj,keyword9,scalar9)
      end subroutine parlist_frou_put_lscalar

!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!

      subroutine parlist_frou_put_iarray (fpoint,keyword,array,nelements)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint                ! argument
      integer                  ,intent(in)    :: keyword(*)            ! argument
      integer                  ,intent(in)    :: nelements             ! argument
      integer                  ,intent(in)    :: array(nelements)      ! argument
      type(parlist_struct)     ,pointer       :: obj                   ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9              ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call parlist_put_iarray (obj,keyword9,array,nelements)
      end subroutine parlist_frou_put_iarray


      subroutine parlist_frou_put_farray (fpoint,keyword,array,nelements)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint                ! argument
      integer                  ,intent(in)    :: keyword(*)            ! argument
      integer                  ,intent(in)    :: nelements             ! argument
      real                     ,intent(in)    :: array(nelements)      ! argument
      type(parlist_struct)     ,pointer       :: obj                   ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9              ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call parlist_put_farray (obj,keyword9,array,nelements)
      end subroutine parlist_frou_put_farray


      subroutine parlist_frou_put_darray (fpoint,keyword,array,nelements)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint                ! argument
      integer                  ,intent(in)    :: keyword(*)            ! argument
      integer                  ,intent(in)    :: nelements             ! argument
      double precision         ,intent(in)    :: array(nelements)      ! argument
      type(parlist_struct)     ,pointer       :: obj                   ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9              ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call parlist_put_darray (obj,keyword9,array,nelements)
      end subroutine parlist_frou_put_darray


      subroutine parlist_frou_put_carray (fpoint,keyword,array,nelements,nwords)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint                ! argument
      integer                  ,intent(in)    :: keyword(*)            ! argument
      integer                  ,intent(in)    :: array(*)              ! argument
      integer                  ,intent(in)    :: nelements             ! argument
      integer                  ,intent(in)    :: nwords                ! argument
      type(parlist_struct)     ,pointer       :: obj                   ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9              ! local
      character(len=PARLIST_LENGTH)           :: array9(nelements)     ! local

      obj => fpoint%obj
      call string_hh2cc       (keyword,keyword9)
      call string_hh2cc_array (array,array9,nwords,nelements)
      call parlist_put_carray (obj,keyword9,array9,nelements)
      end subroutine parlist_frou_put_carray


      subroutine parlist_frou_put_larray (fpoint,keyword,array,nelements)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint                ! argument
      integer                  ,intent(in)    :: keyword(*)            ! argument
      integer                  ,intent(in)    :: nelements             ! argument
      integer                  ,intent(in)    :: array(nelements)      ! argument
      type(parlist_struct)     ,pointer       :: obj                   ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9              ! local
      logical                                 :: array9(nelements)     ! local

      obj => fpoint%obj
      call string_hh2cc        (keyword,keyword9)
      call convert_ii2ll_array (array,array9,nelements)
      call parlist_put_larray  (obj,keyword9,array9,nelements)
      end subroutine parlist_frou_put_larray

!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!

      function parlist_frou_keyword_present (fpoint,keyword) result (present)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      integer                                 :: present        ! result
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local
      logical                                 :: present9       ! local

      obj => fpoint%obj
      call string_hh2cc                  (keyword,keyword9)
      present9 = parlist_keyword_present (obj,keyword9)
      call convert_ll2ii                 (present9,present)
      end function parlist_frou_keyword_present



      function parlist_frou_num_keywords (fpoint) result (num)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                                 :: num            ! result
      type(parlist_struct)     ,pointer       :: obj            ! local

      obj => fpoint%obj
      num = parlist_num_keywords (obj)
      end function parlist_frou_num_keywords



      subroutine parlist_frou_get_keyword (fpoint,indx,keyword)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: indx           ! argument
      integer                  ,intent(out)   :: keyword(*)     ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local

      obj => fpoint%obj
      keyword9 = parlist_get_keyword (obj,indx+1)
      call string_cc2hh              (keyword9,keyword)
      end subroutine parlist_frou_get_keyword



      subroutine parlist_frou_remove_keyword (fpoint,keyword)
      use parlist_frou_module
      implicit none
      type(parlist_frou_struct),intent(inout) :: fpoint         ! argument
      integer                  ,intent(in)    :: keyword(*)     ! argument
      type(parlist_struct)     ,pointer       :: obj            ! local
      character(len=PARLIST_KEYWORD_LENGTH)   :: keyword9       ! local

      obj => fpoint%obj
      call string_hh2cc           (keyword,keyword9)
      call parlist_remove_keyword (obj,keyword9)
      end subroutine parlist_frou_remove_keyword

!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
