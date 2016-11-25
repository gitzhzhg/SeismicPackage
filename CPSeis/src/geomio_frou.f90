!<CPS_v1 type="AUXILIARY_FILE"/>
!!---------------------------- geomio_frou.f90 -----------------------------!!
!!---------------------------- geomio_frou.f90 -----------------------------!!
!!---------------------------- geomio_frou.f90 -----------------------------!!

      ! other files are:  geomio_wrapper.cc  geomio_wrapper.hh  geomio.f90


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


      module geomio_frou_module
      use geomio_module
      use pjar_module
      use pjar_frou_module
      use string_module
      use named_constants_module

      implicit none

      character(len=100),public,save :: GEOMIO_FROU_IDENT = &
'$Id: geomio_frou.f90,v 1.1 2002/04/10 14:55:48 Stoeckley prod sps $'

      type :: geomio_frou_struct
        type(geomio_struct),pointer :: obj
      end type

      end module geomio_frou_module


!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!
!!------------------------------- verify ------------------------------------!!


      subroutine geomio_frou_verify (pjar,secname,err,msg)
      use geomio_frou_module
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
      call geomio_verify (pjar9,secname9,err,msg9)
      call string_cc2hh  (msg9, msg)
      return
      end subroutine geomio_frou_verify


!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!
!!------------------------------- augment -----------------------------------!!


      subroutine geomio_frou_augment (pjar,secname)
      use geomio_frou_module
      implicit none
      type(pjar_frou_struct),intent(inout) :: pjar              ! arguments
      integer               ,intent(in)    :: secname(*)        ! arguments
      type(pjar_struct)     ,pointer       :: pjar9             ! local
      character(len=40)                    :: secname9          ! local

      pjar9 => pjar%obj
      call string_hh2cc   (secname, secname9)
      call geomio_augment (pjar9,secname9)
      return
      end subroutine geomio_frou_augment


!!--------------------------- open read -------------------------------------!!
!!--------------------------- open read -------------------------------------!!
!!--------------------------- open read -------------------------------------!!


      subroutine geomio_frou_open_read (fpoint,filename,pjar,secname,err,msg)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                 ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct)  ,intent(inout) :: pjar              ! arguments
      integer                 ,intent(in)    :: secname(*)        ! arguments
      integer                 ,intent(out)   :: err               ! arguments
      integer                 ,intent(out)   :: msg(*)            ! arguments
      type(geomio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)       ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)         :: filename9         ! local
      character(len=40)                      :: secname9          ! local
      character(len=80)                      :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call geomio_open_read   (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine geomio_frou_open_read


!!--------------------------- open foreign ----------------------------------!!
!!--------------------------- open foreign ----------------------------------!!
!!--------------------------- open foreign ----------------------------------!!


      subroutine geomio_frou_open_foreign (fpoint,filename,pjar,secname,err,msg)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                 ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct)  ,intent(inout) :: pjar              ! arguments
      integer                 ,intent(in)    :: secname(*)        ! arguments
      integer                 ,intent(out)   :: err               ! arguments
      integer                 ,intent(out)   :: msg(*)            ! arguments
      type(geomio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)       ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)         :: filename9         ! local
      character(len=40)                      :: secname9          ! local
      character(len=80)                      :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc        (filename, filename9)
      call string_hh2cc        (secname , secname9)
      call geomio_open_foreign (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh        (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine geomio_frou_open_foreign


!!--------------------------- open write ----------------------------------!!
!!--------------------------- open write ----------------------------------!!
!!--------------------------- open write ----------------------------------!!


      subroutine geomio_frou_open_write (fpoint,filename,pjar,secname,err,msg)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint            ! arguments
      integer                 ,intent(in)    :: filename(*)       ! arguments
      type(pjar_frou_struct)  ,intent(inout) :: pjar              ! arguments
      integer                 ,intent(in)    :: secname(*)        ! arguments
      integer                 ,intent(out)   :: err               ! arguments
      integer                 ,intent(out)   :: msg(*)            ! arguments
      type(geomio_struct)     ,pointer       :: obj               ! local
      type(pjar_struct)       ,pointer       :: pjar9             ! local
      character(len=FILENAME_LENGTH)         :: filename9         ! local
      character(len=40)                      :: secname9          ! local
      character(len=80)                      :: msg9              ! local

      pjar9 => pjar%obj
      call string_hh2cc       (filename, filename9)
      call string_hh2cc       (secname , secname9)
      call geomio_open_write  (obj,filename9,pjar9,secname9,err,msg9)
      call string_cc2hh       (msg9, msg)
      fpoint%obj => obj
      return
      end subroutine geomio_frou_open_write


!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!
!!------------------------------ close -----------------------------------!!


      subroutine geomio_frou_close (fpoint)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint            ! argument
      type(geomio_struct)     ,pointer       :: obj               ! local

      obj => fpoint%obj
      call geomio_close (obj)
      fpoint%obj => obj
      return
      end subroutine geomio_frou_close


!!----------------------------- read ld card --------------------------------!!
!!----------------------------- read ld card --------------------------------!!
!!----------------------------- read ld card --------------------------------!!


      subroutine geomio_frou_read_ld_card (fpoint,ild,err,msg,  &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: ild              ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      real       ,intent(out)   :: sp,dist,xloc,yloc,elev        ! arguments
      real       ,intent(out)   :: depth,tuh,tr,ts,xsd,ysd,elsd  ! arguments
      integer    ,intent(out)   :: line                          ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local

      obj => fpoint%obj
      call geomio_read_ld_card (obj,ild+1,err,msg9,  &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_read_ld_card


!!---------------------------- write ld card --------------------------------!!
!!---------------------------- write ld card --------------------------------!!
!!---------------------------- write ld card --------------------------------!!


      subroutine geomio_frou_write_ld_card (fpoint,ild,err,msg,  &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: ild              ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      real       ,intent(in)    :: sp,dist,xloc,yloc,elev        ! arguments
      real       ,intent(in)    :: depth,tuh,tr,ts,xsd,ysd,elsd  ! arguments
      integer    ,intent(in)    :: line                          ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local

      obj => fpoint%obj
      call geomio_write_ld_card (obj,ild+1,err,msg9,  &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_write_ld_card


!!--------------------------- read rp card --------------------------------!!
!!--------------------------- read rp card --------------------------------!!
!!--------------------------- read rp card --------------------------------!!


      subroutine geomio_frou_read_rp_card (fpoint,irp,err,msg,  &
                  ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: irp              ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer            ,intent(out)   :: ipat1,line1           ! arguments
      integer            ,intent(out)   :: nx,ixinc,ny,iyinc     ! arguments
      real               ,intent(out)   :: sp1,xsd1,ysd1,elsd1   ! arguments
      integer            ,intent(out)   :: flag(*)               ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: flag9            ! local

      obj => fpoint%obj
      call geomio_read_rp_card (obj,irp+1,err,msg9,  &
                  ipat1,flag9,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      call string_cc2hh (msg9 , msg)
      call string_cc2hh (flag9, flag)
      return
      end subroutine geomio_frou_read_rp_card


!!---------------------------- write rp card --------------------------------!!
!!---------------------------- write rp card --------------------------------!!
!!---------------------------- write rp card --------------------------------!!


      subroutine geomio_frou_write_rp_card (fpoint,irp,err,msg,  &
                  ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: irp              ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer            ,intent(in)    :: ipat1,line1           ! arguments
      integer            ,intent(in)    :: nx,ixinc,ny,iyinc     ! arguments
      real               ,intent(in)    :: sp1,xsd1,ysd1,elsd1   ! arguments
      integer            ,intent(in)    :: flag(*)               ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: flag9            ! local

      obj => fpoint%obj
      call string_hh2cc (flag, flag9)
      call geomio_write_rp_card (obj,irp+1,err,msg9,  &
                  ipat1,flag9,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      call string_cc2hh (msg9 , msg)
      return
      end subroutine geomio_frou_write_rp_card


!!--------------------------- read pp card --------------------------------!!
!!--------------------------- read pp card --------------------------------!!
!!--------------------------- read pp card --------------------------------!!


      subroutine geomio_frou_read_pp_card (fpoint,ipp,err,msg,  &
          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: ipp              ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      real               ,intent(out)   :: sp2,sp3,xsd2,ysd2     ! arguments
      integer            ,intent(out)   :: line2,line3,ipat2     ! arguments
      real               ,intent(out)   :: elev2,depth2,tuh2     ! arguments
      integer            ,intent(out)   :: hold,is,ir,ig         ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local

      obj => fpoint%obj
      call geomio_read_pp_card (obj,ipp+1,err,msg9,  &
          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_read_pp_card


!!--------------------------- write pp card --------------------------------!!
!!--------------------------- write pp card --------------------------------!!
!!--------------------------- write pp card --------------------------------!!


      subroutine geomio_frou_write_pp_card (fpoint,ipp,err,msg,  &
          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: ipp              ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      real               ,intent(in)    :: sp2,sp3,xsd2,ysd2     ! arguments
      integer            ,intent(in)    :: line2,line3,ipat2     ! arguments
      real               ,intent(in)    :: elev2,depth2,tuh2     ! arguments
      integer            ,intent(in)    :: hold,is,ir,ig         ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local

      obj => fpoint%obj
      call geomio_write_pp_card (obj,ipp+1,err,msg9,  &
          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_write_pp_card


!!--------------------------- read zt1 card --------------------------------!!
!!--------------------------- read zt1 card --------------------------------!!
!!--------------------------- read zt1 card --------------------------------!!


      subroutine geomio_frou_read_zt1_card (fpoint,izt1,err,msg,  &
                                            ccc1,sss1,sss1a,lll1)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt1             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(out)   :: ccc1(*)          ! arguments
      real                    ,intent(out)   :: sss1,sss1a       ! arguments
      integer                 ,intent(out)   :: lll1             ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call geomio_read_zt1_card (obj,izt1+1,err,msg9,  &
                                 ccc9,sss1,sss1a,lll1)
      call string_cc2hh (msg9, msg)
      call string_cc2hh (ccc9, ccc1)
      return
      end subroutine geomio_frou_read_zt1_card


!!--------------------------- write zt1 card --------------------------------!!
!!--------------------------- write zt1 card --------------------------------!!
!!--------------------------- write zt1 card --------------------------------!!


      subroutine geomio_frou_write_zt1_card (fpoint,izt1,err,msg,  &
                                             ccc1,sss1,sss1a,lll1)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt1             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(in)    :: ccc1(*)          ! arguments
      real                    ,intent(in)    :: sss1,sss1a       ! arguments
      integer                 ,intent(in)    :: lll1             ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call string_hh2cc (ccc1, ccc9)
      call geomio_write_zt1_card (obj,izt1+1,err,msg9,  &
                                  ccc9,sss1,sss1a,lll1)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_write_zt1_card


!!--------------------------- read zt2 card --------------------------------!!
!!--------------------------- read zt2 card --------------------------------!!
!!--------------------------- read zt2 card --------------------------------!!


      subroutine geomio_frou_read_zt2_card (fpoint,izt2,err,msg,  &
                                            ccc2,rrr2,rrr2a,lll2)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt2             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(out)   :: ccc2(*)          ! arguments
      real                    ,intent(out)   :: rrr2,rrr2a       ! arguments
      integer                 ,intent(out)   :: lll2             ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call geomio_read_zt2_card (obj,izt2+1,err,msg9,  &
                                 ccc9,rrr2,rrr2a,lll2)
      call string_cc2hh (msg9, msg)
      call string_cc2hh (ccc9, ccc2)
      return
      end subroutine geomio_frou_read_zt2_card


!!--------------------------- write zt2 card --------------------------------!!
!!--------------------------- write zt2 card --------------------------------!!
!!--------------------------- write zt2 card --------------------------------!!


      subroutine geomio_frou_write_zt2_card (fpoint,izt2,err,msg,  &
                                             ccc2,rrr2,rrr2a,lll2)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt2             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(in)    :: ccc2(*)          ! arguments
      real                    ,intent(in)    :: rrr2,rrr2a       ! arguments
      integer                 ,intent(in)    :: lll2             ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call string_hh2cc (ccc2, ccc9)
      call geomio_write_zt2_card (obj,izt2+1,err,msg9,  &
                                  ccc9,rrr2,rrr2a,lll2)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_write_zt2_card


!!--------------------------- read zt3 card --------------------------------!!
!!--------------------------- read zt3 card --------------------------------!!
!!--------------------------- read zt3 card --------------------------------!!


      subroutine geomio_frou_read_zt3_card (fpoint,izt3,err,msg,  &
                                            ccc3,iggg3,iggg3a,ittt3,ittt3a)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt3             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(out)   :: ccc3(*)          ! arguments
      integer                 ,intent(out)   :: iggg3,iggg3a     ! arguments
      integer                 ,intent(out)   :: ittt3,ittt3a     ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call geomio_read_zt3_card (obj,izt3+1,err,msg9,  &
                                        ccc9,iggg3,iggg3a,ittt3,ittt3a)
      call string_cc2hh (msg9, msg)
      call string_cc2hh (ccc9, ccc3)
      return
      end subroutine geomio_frou_read_zt3_card


!!--------------------------- write zt3 card --------------------------------!!
!!--------------------------- write zt3 card --------------------------------!!
!!--------------------------- write zt3 card --------------------------------!!


      subroutine geomio_frou_write_zt3_card (fpoint,izt3,err,msg,  &
                                             ccc3,iggg3,iggg3a,ittt3,ittt3a)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt3             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(in)    :: ccc3(*)          ! arguments
      integer                 ,intent(in)    :: iggg3,iggg3a     ! arguments
      integer                 ,intent(in)    :: ittt3,ittt3a     ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call string_hh2cc (ccc3, ccc9)
      call geomio_write_zt3_card (obj,izt3+1,err,msg9,  &
                                        ccc9,iggg3,iggg3a,ittt3,ittt3a)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_write_zt3_card


!!--------------------------- read zt4 card --------------------------------!!
!!--------------------------- read zt4 card --------------------------------!!
!!--------------------------- read zt4 card --------------------------------!!


      subroutine geomio_frou_read_zt4_card (fpoint,izt4,err,msg,  &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt4             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(out)   :: ccc4(*)          ! arguments
      real                    ,intent(out)   :: sss4,sss4a       ! arguments
      real                    ,intent(out)   :: rrr4,rrr4a       ! arguments
      integer                 ,intent(out)   :: lll4,lll4a       ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call geomio_read_zt4_card (obj,izt4+1,err,msg9,  &
                                        ccc9,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      call string_cc2hh (msg9, msg)
      call string_cc2hh (ccc9, ccc4)
      return
      end subroutine geomio_frou_read_zt4_card


!!--------------------------- write zt4 card --------------------------------!!
!!--------------------------- write zt4 card --------------------------------!!
!!--------------------------- write zt4 card --------------------------------!!


      subroutine geomio_frou_write_zt4_card (fpoint,izt4,err,msg,  &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      use geomio_frou_module
      implicit none
      type(geomio_frou_struct),intent(inout) :: fpoint           ! arguments
      integer                 ,intent(in)    :: izt4             ! arguments
      integer                 ,intent(out)   :: err              ! arguments
      integer                 ,intent(out)   :: msg(*)           ! arguments
      integer                 ,intent(in)    :: ccc4(*)          ! arguments
      real                    ,intent(in)    :: sss4,sss4a       ! arguments
      real                    ,intent(in)    :: rrr4,rrr4a       ! arguments
      integer                 ,intent(in)    :: lll4,lll4a       ! arguments
      type(geomio_struct)     ,pointer       :: obj              ! local
      character(len=80)                      :: msg9             ! local
      character(len=12)                      :: ccc9             ! local

      obj => fpoint%obj
      call string_hh2cc (ccc4, ccc9)
      call geomio_write_zt4_card (obj,izt4+1,err,msg9,  &
                                        ccc9,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      call string_cc2hh (msg9, msg)
      return
      end subroutine geomio_frou_write_zt4_card


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
