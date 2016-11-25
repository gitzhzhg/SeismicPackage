!<CPS_v1 type="AUXILIARY_FILE"/>
!!---------------------------- floatio_frou.f90 -----------------------------!!
!!---------------------------- floatio_frou.f90 -----------------------------!!
!!---------------------------- floatio_frou.f90 -----------------------------!!

    ! other files are:  floatio_wrapper.cc  floatio_wrapper.hh  floatio.f90


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
!  2. 2002-04-10  Stoeckley  Remove unneeded SKIP argument.
!  1. 2002-01-03  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


 
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module floatio_frou_module
      use floatio_module
      use pjar_module
      use pjar_frou_module
      use string_module
      use convert_module
      use named_constants_module

      implicit none

      character(len=100),public,save :: FLOATIO_FROU_IDENT = &
'$Id: floatio_frou.f90,v 1.2 2002/04/10 14:46:51 Stoeckley prod sps $'

      type :: floatio_frou_struct
        type(floatio_struct),pointer :: obj
      end type

      end module floatio_frou_module


!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!


      subroutine floatio_frou_verify (pjar,secname,err,msg)
      use floatio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc   (secname, secname9)
      call floatio_verify (pjar9,secname9,err,msg9)
      call string_cc2hh   (msg9, msg)
      return
      end subroutine floatio_frou_verify


!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!


      subroutine floatio_frou_augment (pjar,secname)
      use floatio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local

      pjar9 => pjar%obj
      call string_hh2cc    (secname, secname9)
      call floatio_augment (pjar9,secname9)
      return
      end subroutine floatio_frou_augment


!!--------------------------- open read -------------------------------------!!
!!--------------------------- open read -------------------------------------!!
!!--------------------------- open read -------------------------------------!!


      subroutine floatio_frou_open_read (fpoint,filename,pjar,secname,err,msg)
      use floatio_frou_module
      implicit none
      type(floatio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                  ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct)   ,intent(inout) :: pjar              ! arguments
      integer                  ,intent(in)    :: secname(*)        ! arguments
      integer                  ,intent(out)   :: err               ! arguments
      integer                  ,intent(out)   :: msg(*)            ! arguments
      type(floatio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)        ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)          :: filename9         ! local
      character(len=40)                       :: secname9          ! local
      character(len=80)                       :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call floatio_open_read  (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine floatio_frou_open_read


!!--------------------------- open foreign ----------------------------------!!
!!--------------------------- open foreign ----------------------------------!!
!!--------------------------- open foreign ----------------------------------!!


      subroutine floatio_frou_open_foreign &
                                      (fpoint,filename,pjar,secname,err,msg)
      use floatio_frou_module
      implicit none
      type(floatio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                  ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct)   ,intent(inout) :: pjar              ! arguments
      integer                  ,intent(in)    :: secname(*)        ! arguments
      integer                  ,intent(out)   :: err               ! arguments
      integer                  ,intent(out)   :: msg(*)            ! arguments
      type(floatio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)        ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)          :: filename9         ! local
      character(len=40)                       :: secname9          ! local
      character(len=80)                       :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc         (filename, filename9)
      call string_hh2cc         (secname , secname9)
      call floatio_open_foreign (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh         (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine floatio_frou_open_foreign


!!--------------------------- open write ----------------------------------!!
!!--------------------------- open write ----------------------------------!!
!!--------------------------- open write ----------------------------------!!


      subroutine floatio_frou_open_write (fpoint,filename,pjar,secname,err,msg)
      use floatio_frou_module
      implicit none
      type(floatio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                  ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct)   ,intent(inout) :: pjar              ! arguments
      integer                  ,intent(in)    :: secname(*)        ! arguments
      integer                  ,intent(out)   :: err               ! arguments
      integer                  ,intent(out)   :: msg(*)            ! arguments
      type(floatio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)        ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)          :: filename9         ! local
      character(len=40)                       :: secname9          ! local
      character(len=80)                       :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call floatio_open_write   (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine floatio_frou_open_write


!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!


      subroutine floatio_frou_close (fpoint)
      use floatio_frou_module
      implicit none
      type(floatio_frou_struct),intent(inout) :: fpoint            ! argument
      type(floatio_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call floatio_close (obj)
      fpoint%obj => obj
      return
      end subroutine floatio_frou_close


!!----------------------------- read line ---------------------------------!!
!!----------------------------- read line ---------------------------------!!
!!----------------------------- read line ---------------------------------!!


      subroutine floatio_frou_read_line (fpoint,err,msg,vline,ncolumns)
      use floatio_frou_module
      implicit none
      type(floatio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                  ,intent(out)   :: err              ! arguments
      integer                  ,intent(out)   :: msg(*)           ! arguments
      integer                  ,intent(in)    :: ncolumns         ! arguments
      real                     ,intent(out)   :: vline(ncolumns)  ! arguments
      type(floatio_struct)     ,pointer       :: obj              ! local
      character(len=80)                       :: msg9             ! local

      obj => fpoint%obj
      call floatio_read_line   (obj,err,msg9,vline)
      call string_cc2hh        (msg9, msg)
      return
      end subroutine floatio_frou_read_line


!!----------------------------- write line ---------------------------------!!
!!----------------------------- write line ---------------------------------!!
!!----------------------------- write line ---------------------------------!!


      subroutine floatio_frou_write_line (fpoint,err,msg,vline,ncolumns)
      use floatio_frou_module
      implicit none
      type(floatio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                  ,intent(out)   :: err              ! arguments
      integer                  ,intent(out)   :: msg(*)           ! arguments
      integer                  ,intent(in)    :: ncolumns         ! arguments
      real                     ,intent(in)    :: vline(ncolumns)  ! arguments
      type(floatio_struct)     ,pointer       :: obj              ! local
      character(len=80)                       :: msg9             ! local

      obj => fpoint%obj
      call floatio_write_line (obj,err,msg9,vline)
      call string_cc2hh       (msg9, msg)
      return
      end subroutine floatio_frou_write_line


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
