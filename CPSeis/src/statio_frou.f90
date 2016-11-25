!<CPS_v1 type="AUXILIARY_FILE"/>
!!---------------------------- statio_frou.f90 -----------------------------!!
!!---------------------------- statio_frou.f90 -----------------------------!!
!!---------------------------- statio_frou.f90 -----------------------------!!

      ! other files are:  statio_wrapper.cc  statio_wrapper.hh  statio.f90


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

 
!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2002-04-10  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


 
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module statio_frou_module
      use statio_module
      use pjar_module
      use pjar_frou_module
      use string_module
      use named_constants_module

      implicit none

      character(len=100),public,save :: STATIO_FROU_IDENT = &
'$Id: statio_frou.f90,v 1.1 2002/04/10 17:03:28 Stoeckley prod sps $'

      end module statio_frou_module


!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!


      subroutine statio_frou_verify (pjar,secname,err,msg)
      use statio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc  (secname, secname9)
      call statio_verify (pjar9,secname9,err,msg9)
      call string_cc2hh  (msg9, msg)
      return
      end subroutine statio_frou_verify


!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!


      subroutine statio_frou_augment (pjar,secname)
      use statio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local

      pjar9 => pjar%obj
      call string_hh2cc   (secname, secname9)
      call statio_augment (pjar9,secname9)
      return
      end subroutine statio_frou_augment


!!--------------------------- read header -----------------------------------!!
!!--------------------------- read header -----------------------------------!!
!!--------------------------- read header -----------------------------------!!


      subroutine statio_frou_read_header (filename,pjar,secname,err,msg)
      use statio_frou_module
      implicit none
      integer               ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)       :: filename9         ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc        (filename, filename9)
      call string_hh2cc        (secname , secname9)
      call statio_read_header  (filename9,pjar9,secname9,err,msg9)
      call string_cc2hh        (msg9, msg)
      return
      end subroutine statio_frou_read_header


!!------------------------------- read file ---------------------------------!!
!!------------------------------- read file ---------------------------------!!
!!------------------------------- read file ---------------------------------!!


      subroutine statio_frou_read_file (filename,pjar,secname,statics,err,msg)
      use statio_frou_module
      implicit none
      integer               ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      real                  ,intent(out)   :: statics(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)       :: filename9         ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local
      real                      ,pointer   :: pstatics9(:)      ! local
      integer                              :: nx,ny             ! local

      pjar9 => pjar%obj
      nullify (pstatics9)

      call string_hh2cc     (filename, filename9)
      call string_hh2cc     (secname , secname9)
      call statio_read_file (filename9,pjar9,secname9,pstatics9,err,msg9)
      call string_cc2hh     (msg9, msg)

      if (err == STATIO_OK) then
           call pjar_choose_section (pjar9,secname9)
           call pjar_get            (pjar9, 'nx', nx)
           call pjar_get            (pjar9, 'ny', ny)
           statics(1:nx*ny) = pstatics9(1:nx*ny)
      end if

      if (associated(pstatics9)) deallocate(pstatics9)
      return
      end subroutine statio_frou_read_file


!!----------------------------- write file ---------------------------------!!
!!----------------------------- write file ---------------------------------!!
!!----------------------------- write file ---------------------------------!!


      subroutine statio_frou_write_file (filename,pjar,secname,statics,err,msg)
      use statio_frou_module
      implicit none
      integer               ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      real                  ,intent(in)    :: statics(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)       :: filename9         ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local
      real                      ,pointer   :: pstatics9(:)      ! local
      integer                              :: nx,ny             ! local

      pjar9 => pjar%obj
      call string_hh2cc        (secname,secname9)
      call pjar_choose_section (pjar9,secname9)
      call pjar_get            (pjar9, 'nx', nx)
      call pjar_get            (pjar9, 'ny', ny)
      allocate (pstatics9(nx*ny))
      pstatics9(1:nx*ny) = statics(1:nx*ny)

      call string_hh2cc      (filename, filename9)
      call statio_write_file (filename9,pjar9,secname9,pstatics9,err,msg9)
      call string_cc2hh      (msg9, msg)

      if (associated(pstatics9)) deallocate(pstatics9)
      return
      end subroutine statio_frou_write_file


!!--------------------------- scan foreign ---------------------------------!!
!!--------------------------- scan foreign ---------------------------------!!
!!--------------------------- scan foreign ---------------------------------!!


      subroutine statio_frou_scan_foreign (filename,pjar,secname,err,msg)
      use statio_frou_module
      implicit none
      integer               ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)       :: filename9         ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc        (filename, filename9)
      call string_hh2cc        (secname , secname9)
      call statio_scan_foreign (filename9,pjar9,secname9,err,msg9)
      call string_cc2hh        (msg9, msg)
      return
      end subroutine statio_frou_scan_foreign


!!--------------------------- read foreign ---------------------------------!!
!!--------------------------- read foreign ---------------------------------!!
!!--------------------------- read foreign ---------------------------------!!


      subroutine statio_frou_read_foreign &
                                    (filename,pjar,secname,statics,err,msg)
      use statio_frou_module
      implicit none
      integer               ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      real                  ,intent(out)   :: statics(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)       :: filename9         ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local
      real                      ,pointer   :: pstatics9(:)      ! local
      integer                              :: nx,ny             ! local

      pjar9 => pjar%obj
      nullify (pstatics9)

      call string_hh2cc        (filename, filename9)
      call string_hh2cc        (secname , secname9)
      call statio_read_foreign (filename9,pjar9,secname9,pstatics9,err,msg9)
      call string_cc2hh        (msg9, msg)

      if (err == STATIO_OK) then
           call pjar_choose_section (pjar9,secname9)
           call pjar_get            (pjar9, 'nx', nx)
           call pjar_get            (pjar9, 'ny', ny)
           statics(1:nx*ny) = pstatics9(1:nx*ny)
      end if

      if (associated(pstatics9)) deallocate(pstatics9)
      return
      end subroutine statio_frou_read_foreign


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
