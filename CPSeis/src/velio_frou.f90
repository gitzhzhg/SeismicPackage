!<CPS_v1 type="AUXILIARY_FILE"/>
!!---------------------------- velio_frou.f90 -----------------------------!!
!!---------------------------- velio_frou.f90 -----------------------------!!
!!---------------------------- velio_frou.f90 -----------------------------!!

      ! other files are:  velio_wrapper.cc  velio_wrapper.hh  velio.f90


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


      module velio_frou_module
      use velio_module
      use pjar_module
      use pjar_frou_module
      use string_module
      use named_constants_module

      implicit none

      character(len=100),public,save :: VELIO_FROU_IDENT = &
'$Id: velio_frou.f90,v 1.1 2002/04/10 17:15:18 Stoeckley prod sps $'

      type :: velio_frou_struct
        type(velio_struct),pointer :: obj
      end type

      end module velio_frou_module


!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!


      subroutine velio_frou_verify (pjar,secname,err,msg)
      use velio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      integer               ,intent(out)   :: err               ! arguments
      integer               ,intent(out)   :: msg(*)            ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local
      character(len=80)                    :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc (secname, secname9)
      call velio_verify (pjar9,secname9,err,msg9)
      call string_cc2hh (msg9, msg)
      return
      end subroutine velio_frou_verify


!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!


      subroutine velio_frou_augment (pjar,secname)
      use velio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local

      pjar9 => pjar%obj
      call string_hh2cc  (secname, secname9)
      call velio_augment (pjar9,secname9)
      return
      end subroutine velio_frou_augment


!!--------------------------- open read -------------------------------------!!
!!--------------------------- open read -------------------------------------!!
!!--------------------------- open read -------------------------------------!!


      subroutine velio_frou_open_read (fpoint,filename,pjar,secname,err,msg)
      use velio_frou_module
      implicit none
      type(velio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct) ,intent(inout) :: pjar              ! arguments
      integer                ,intent(in)    :: secname(*)        ! arguments
      integer                ,intent(out)   :: err               ! arguments
      integer                ,intent(out)   :: msg(*)            ! arguments
      type(velio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)      ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)        :: filename9         ! local
      character(len=40)                     :: secname9          ! local
      character(len=80)                     :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call velio_open_read    (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine velio_frou_open_read


!!--------------------------- open foreign ----------------------------------!!
!!--------------------------- open foreign ----------------------------------!!
!!--------------------------- open foreign ----------------------------------!!


      subroutine velio_frou_open_foreign (fpoint,filename,pjar,secname,err,msg)
      use velio_frou_module
      implicit none
      type(velio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct) ,intent(inout) :: pjar              ! arguments
      integer                ,intent(in)    :: secname(*)        ! arguments
      integer                ,intent(out)   :: err               ! arguments
      integer                ,intent(out)   :: msg(*)            ! arguments
      type(velio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)      ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)        :: filename9         ! local
      character(len=40)                     :: secname9          ! local
      character(len=80)                     :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call velio_open_foreign (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine velio_frou_open_foreign


!!--------------------------- open write ----------------------------------!!
!!--------------------------- open write ----------------------------------!!
!!--------------------------- open write ----------------------------------!!


      subroutine velio_frou_open_write (fpoint,filename,pjar,secname,err,msg)
      use velio_frou_module
      implicit none
      type(velio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct) ,intent(inout) :: pjar              ! arguments
      integer                ,intent(in)    :: secname(*)        ! arguments
      integer                ,intent(out)   :: err               ! arguments
      integer                ,intent(out)   :: msg(*)            ! arguments
      type(velio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)      ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)        :: filename9         ! local
      character(len=40)                     :: secname9          ! local
      character(len=80)                     :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call velio_open_write   (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine velio_frou_open_write


!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!


      subroutine velio_frou_close (fpoint)
      use velio_frou_module
      implicit none
      type(velio_frou_struct),intent(inout) :: fpoint            ! argument
      type(velio_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call velio_close (obj)
      fpoint%obj => obj
      return
      end subroutine velio_frou_close


!!----------------------------- read velfun ---------------------------------!!
!!----------------------------- read velfun ---------------------------------!!
!!----------------------------- read velfun ---------------------------------!!


      subroutine velio_frou_read_velfun                                    &
                               (fpoint,xcoord,ycoord,npicks,maxpicks,      &
                                tpicks,vpicks,err,msg,                     &
                                velname,veltype,                           &
                                project,line,rdate,pdate,userid,comment)
      use velio_frou_module
      implicit none
      type(velio_frou_struct),intent(inout) :: fpoint           ! arguments
      real                   ,intent(out)   :: xcoord,ycoord    ! arguments
      integer                ,intent(out)   :: npicks           ! arguments
      integer                ,intent(in)    :: maxpicks         ! arguments
      real                   ,intent(out)   :: tpicks(maxpicks) ! arguments
      real                   ,intent(out)   :: vpicks(maxpicks) ! arguments
      integer                ,intent(out)   :: err              ! arguments
      integer                ,intent(out)   :: msg(*)           ! arguments
      integer                ,intent(out)   :: velname(*)       ! arguments
      integer                ,intent(out)   :: veltype(*)       ! arguments
      integer                ,intent(out)   :: project(*)       ! arguments
      integer                ,intent(out)   :: line(*)          ! arguments
      integer                ,intent(out)   :: rdate(*)         ! arguments
      integer                ,intent(out)   :: pdate(*)         ! arguments
      integer                ,intent(out)   :: userid(*)        ! arguments
      integer                ,intent(out)   :: comment(*)       ! arguments
      type(velio_struct)     ,pointer       :: obj              ! local
      character(len=80)                     :: msg9             ! local
      character(len=20)                     :: velname9         ! local
      character(len=20)                     :: veltype9         ! local
      character(len=20)                     :: project9         ! local
      character(len=20)                     :: line9            ! local
      character(len=20)                     :: rdate9           ! local
      character(len=20)                     :: pdate9           ! local
      character(len=20)                     :: userid9          ! local
      character(len=20)                     :: comment9         ! local

      obj => fpoint%obj
      call velio_read_velfun                                               &
                        (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg9,  &
                         velname9,veltype9,                                &
                         project9,line9,rdate9,pdate9,userid9,comment9)
      call string_cc2hh (msg9    , msg)
      call string_cc2hh (velname9, velname)
      call string_cc2hh (veltype9, veltype)
      call string_cc2hh (project9, project)
      call string_cc2hh (line9   , line)
      call string_cc2hh (rdate9  , rdate)
      call string_cc2hh (pdate9  , pdate)
      call string_cc2hh (userid9 , userid)
      call string_cc2hh (comment9, comment)
      return
      end subroutine velio_frou_read_velfun


!!----------------------------- write velfun ---------------------------------!!
!!----------------------------- write velfun ---------------------------------!!
!!----------------------------- write velfun ---------------------------------!!


      subroutine velio_frou_write_velfun                                   &
                               (fpoint,xcoord,ycoord,npicks,               &
                                tpicks,vpicks,err,msg,                     &
                                velname,veltype,                           &
                                project,line,rdate,pdate,userid,comment)
      use velio_frou_module
      implicit none
      type(velio_frou_struct),intent(inout) :: fpoint          ! arguments
      real                   ,intent(in)    :: xcoord,ycoord   ! arguments
      integer                ,intent(in)    :: npicks          ! arguments
      real                   ,intent(in)    :: tpicks(npicks)  ! arguments
      real                   ,intent(in)    :: vpicks(npicks)  ! arguments
      integer                ,intent(out)   :: err             ! arguments
      integer                ,intent(out)   :: msg(*)          ! arguments
      integer                ,intent(in)    :: velname(*)      ! arguments
      integer                ,intent(in)    :: veltype(*)      ! arguments
      integer                ,intent(in)    :: project(*)      ! arguments
      integer                ,intent(in)    :: line(*)         ! arguments
      integer                ,intent(in)    :: rdate(*)        ! arguments
      integer                ,intent(in)    :: pdate(*)        ! arguments
      integer                ,intent(in)    :: userid(*)       ! arguments
      integer                ,intent(in)    :: comment(*)      ! arguments
      type(velio_struct)     ,pointer       :: obj             ! local
      character(len=80)                     :: msg9            ! local
      character(len=20)                     :: velname9        ! local
      character(len=20)                     :: veltype9        ! local
      character(len=20)                     :: project9        ! local
      character(len=20)                     :: line9           ! local
      character(len=20)                     :: rdate9          ! local
      character(len=20)                     :: pdate9          ! local
      character(len=20)                     :: userid9         ! local
      character(len=20)                     :: comment9        ! local

      obj => fpoint%obj
      call string_hh2cc (velname, velname9)
      call string_hh2cc (veltype, veltype9)
      call string_hh2cc (project, project9)
      call string_hh2cc (line   , line9)
      call string_hh2cc (rdate  , rdate9)
      call string_hh2cc (pdate  , pdate9)
      call string_hh2cc (userid , userid9)
      call string_hh2cc (comment, comment9)
      call velio_write_velfun                                              &
                        (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg9,  &
                         velname9,veltype9,                                &
                         project9,line9,rdate9,pdate9,userid9,comment9)
      call string_cc2hh (msg9    , msg)
      return
      end subroutine velio_frou_write_velfun


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
