!<CPS_v1 type="AUXILIARY_FILE"/>
!!--------------------- parallelsort_superwrap.f90 ----------------------!!
!!--------------------- parallelsort_superwrap.f90 ----------------------!!
!!--------------------- parallelsort_superwrap.f90 ----------------------!!


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
!                           C P S   P R O C E S S     
!
! Name       : parallelsort_superwrap
! Category   : cfe
! Written    : 2010-02-23   by: Tom Stoeckley
! Revised    : 2010-02-23   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Fortran wrapper around CPS process (for superproc).
! Portability: No known limitations.
!
! Manually generated.
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2010-02-23  Stoeckley  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>


!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module parallelsort_superwrap_module
      use parallelsort_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: parallelsort_superwrap_ident = &
'$Id: fproc_superwrap_template,v 1.3 2006/06/23 18:41:47 Stoeckley prod sps $'

      type :: parallelsort_superwrap_struct
        type(parallelsort_struct),pointer :: obj
      end type parallelsort_superwrap_struct

      end module parallelsort_superwrap_module


!!------------------------------ rcs --------------------------------------!!
!!------------------------------ rcs --------------------------------------!!
!!------------------------------ rcs --------------------------------------!!


      subroutine parallelsort_superwrap_rcs (ident)           ! called from fortran
      use parallelsort_superwrap_module
      implicit none
      character(len=*),intent(out) :: ident           ! argument

      ident = parallelsort_ident

      end subroutine parallelsort_superwrap_rcs


!!---------------------------- create ------------------------------------!!
!!---------------------------- create ------------------------------------!!
!!---------------------------- create ------------------------------------!!


      subroutine parallelsort_superwrap_create (fpoint)  
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(out)   :: fpoint
      type(parallelsort_struct)          ,pointer       :: obj           ! local

      nullify (obj)                 ! for intel compiler.
      call parallelsort_create (obj)
      fpoint%obj => obj

      end subroutine parallelsort_superwrap_create


!!---------------------------- delete ------------------------------------!!
!!---------------------------- delete ------------------------------------!!
!!---------------------------- delete ------------------------------------!!


      subroutine parallelsort_superwrap_delete (fpoint)   
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(inout) :: fpoint
      type(parallelsort_struct)          ,pointer       :: obj           ! local

      obj => fpoint%obj
      if (.not. associated(obj)) return
      call parallelsort_delete (obj)
      fpoint%obj => obj

      end subroutine parallelsort_superwrap_delete


!!------------------------------- init ------------------------------------!!
!!------------------------------- init ------------------------------------!!
!!------------------------------- init ------------------------------------!!


      subroutine parallelsort_superwrap_init (fpoint)      
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(inout) :: fpoint
      type(parallelsort_struct)          ,pointer       :: obj           ! local

      obj => fpoint%obj
      if (.not. associated(obj)) return
      call parallelsort_initialize (obj)

      end subroutine parallelsort_superwrap_init


!!---------------------------- update ------------------------------------!!
!!---------------------------- update ------------------------------------!!
!!---------------------------- update ------------------------------------!!


      subroutine parallelsort_superwrap_update (fpoint)     
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(inout) :: fpoint
      type(parallelsort_struct)          ,pointer       :: obj           ! local

      obj => fpoint%obj
      if (.not. associated(obj)) return
      call parallelsort_update (obj)

      end subroutine parallelsort_superwrap_update


!!---------------------------- wrapup ------------------------------------!!
!!---------------------------- wrapup ------------------------------------!!
!!---------------------------- wrapup ------------------------------------!!


      subroutine parallelsort_superwrap_wrapup (fpoint)      
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(inout) :: fpoint
      type(parallelsort_struct)          ,pointer       :: obj           ! local

      obj => fpoint%obj
      if (.not. associated(obj)) return
      call parallelsort_wrapup (obj)

      end subroutine parallelsort_superwrap_wrapup


!!---------------------------- oneset ------------------------------------!!
!!---------------------------- oneset ------------------------------------!!
!!---------------------------- oneset ------------------------------------!!


      subroutine parallelsort_superwrap_oneset (fpoint,ntr,  &
                                        hd,tr,lenhd,lentr,num)
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(inout) :: fpoint
      integer                    ,intent(in)    :: lenhd,lentr,num  ! argument
      integer                    ,intent(inout) :: ntr              ! argument
      double precision           ,intent(inout) :: hd(lenhd,num)    ! argument
      real                       ,intent(inout) :: tr(lentr,num)    ! argument
      type(parallelsort_struct)          ,pointer       :: obj              ! local

      obj => fpoint%obj
      if (.not. associated(obj)) return
!!!   call parallelsort (obj,ntr,hd,tr)                ! might need commenting out.

      end subroutine parallelsort_superwrap_oneset


!!---------------------------- twosets ------------------------------------!!
!!---------------------------- twosets ------------------------------------!!
!!---------------------------- twosets ------------------------------------!!


      subroutine parallelsort_superwrap_twosets (fpoint,ntr,                 &
                                         hd1,tr1,lenhd1,lentr1,num1, &
                                         hd2,tr2,lenhd2,lentr2,num2)
      use parallelsort_superwrap_module
      implicit none
      type(parallelsort_superwrap_struct),intent(inout) :: fpoint
      integer                    ,intent(in)    :: lenhd1,lentr1,num1 ! argument
      integer                    ,intent(in)    :: lenhd2,lentr2,num2 ! argument
      integer                    ,intent(inout) :: ntr                ! argument
      double precision           ,intent(inout) :: hd1(lenhd1,num1)   ! argument
      real                       ,intent(inout) :: tr1(lentr1,num1)   ! argument
      double precision           ,intent(inout) :: hd2(lenhd2,num2)   ! argument
      real                       ,intent(inout) :: tr2(lentr2,num2)   ! argument
      type(parallelsort_struct)          ,pointer       :: obj                ! local

      obj => fpoint%obj
      if (.not. associated(obj)) return
   call parallelsort (obj,ntr,hd1,tr1,hd2,tr2)      ! might need commenting out.

      end subroutine parallelsort_superwrap_twosets


!!----------------------------- end ---------------------------------------!!
!!----------------------------- end ---------------------------------------!!
!!----------------------------- end ---------------------------------------!!

