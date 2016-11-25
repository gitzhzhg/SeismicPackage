!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ muteio1.f90 -------------------------------!!
!!------------------------------ muteio1.f90 -------------------------------!!
!!------------------------------ muteio1.f90 -------------------------------!!


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
! Name       : MUTEIO1
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2001-12-28   by: Tom Stoeckley
! Maturity   : production   2002-02-04
! Purpose    : To read and write old-style CPS mute files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing old-style CPS mute files.
!
! This primitive does not open or close the file.  The calling program must
! use the DIO primitive to open and close the file.
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
!                                  b   b      i     o   o
!     call muteio1_read_header   (dio,pjar,secname,err,msg)
!     call muteio1_write_header  (dio,pjar,secname,err,msg)
!                                  b   b      i     o   o 
!
!                                  b    o      o      o     o    o   o
!     call muteio1_read_card     (dio,offset,xcoord,ycoord,time,err,msg)
!     call muteio1_write_card    (dio,offset,xcoord,ycoord,time,err,msg)
!                                  b    i      i      i     i    o   o
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(dio_struct)  dio     = reference to the DIO data structure.
! type(pjar_struct) pjar    = reference to the PJAR data structure.
! char(*)           secname = PJAR section containing the parameters.
! integer           err     = error flag (returned).
! char(*)           msg     = message for possible printing (returned).
!
! integer nho      = header word containing the offset.
! integer nhx      = header word containing the X coordinate.
! integer nhy      = header word containing the Y coordinate.
! real    xlast    = latest X coordinate updated.
! real    ylast    = latest Y coordinate updated.
! real    offset   = mute location offset.
! real    xcoord   = mute location X coordinate.
! real    ycoord   = mute location Y coordinate.
! real    time     = mute time.
!
! ERR will be set to DIO_OK or DIO_ERROR or DIO_EOF.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR primitive.
!  1. 2001-01-10  Stoeckley  Initial version.
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
 
 
      module muteio1_module
      use dio_module
      use pjar_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: MUTEIO1_IDENT = &
'$Id: muteio1.f90,v 1.2 2002/01/30 20:03:20 Stoeckley prod sps $'

      character(len=50),private,parameter :: PHRASE1 = &
             '### CPS 3D MUTE FILE'

      character(len=50),private,parameter :: PHRASE2 = &
             '### file saved:'

      character(len=50),private,parameter :: PHRASE3 = &
             '### offset, inline, crossline headers:'

      character(len=50),private,parameter :: PHRASE4 = &
             '### latest inline and crossline updated:'

      character(len=50),private,parameter :: PHRASE5 = &
             '###   OFFSET   XCOORD   YCOORD    TIME'

      contains


!!-------------------------- muteio1 read header -------------------------!!
!!-------------------------- muteio1 read header -------------------------!!
!!-------------------------- muteio1 read header -------------------------!!


      subroutine muteio1_read_header (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct)    ,intent(inout) :: dio                 ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar                ! arguments
      character(len=*)    ,intent(in)    :: secname             ! arguments
      integer             ,intent(out)   :: err                 ! arguments
      character(len=*)    ,intent(out)   :: msg                 ! arguments
      character(len=80)                  :: record              ! local
      integer                            :: istat,length        ! local
      integer                            :: nho,nhx,nhy         ! local
      real                               :: xlast,ylast         ! local

!----------read first line:

      call dio_read_card (dio,record)
      call dio_status    (dio,err,msg)
      if (err /= DIO_OK) return

      if (record /= PHRASE1) then
           err = DIO_ERROR
           msg = 'bad first line on old-style CPS mute file'
           return
      end if

!----------read second line:

      call dio_read_card (dio,record)
      call dio_status    (dio,err,msg)
      if (err /= DIO_OK) return

!----------read third line:

      call dio_read_card (dio,record)
      call dio_status    (dio,err,msg)
      if (err /= DIO_OK) return

      length = len_trim(PHRASE3)
      if (record(1:length) /= PHRASE3) then
           err = DIO_ERROR
           msg = 'bad third line on old-style CPS mute file'
           return
      end if

      read (record(length+1:),*,iostat=istat) nho,nhx,nhy
      if (istat /= 0) then
           err = DIO_ERROR
           msg = 'error decoding third line on old-style CPS mute file'
           return
      end if

      call pjar_choose_section (pjar,secname)
      call pjar_put            (pjar,'nho'     ,nho)
      call pjar_put            (pjar,'nhx'     ,nhx)
      call pjar_put            (pjar,'nhy'     ,nhy)

!----------read fourth line:

      call dio_read_card (dio,record)
      call dio_status    (dio,err,msg)
      if (err /= DIO_OK) return

      length = len_trim(PHRASE4)
      if (record(1:length) /= PHRASE4) then
           err = DIO_ERROR
           msg = 'bad fourth line on old-style CPS mute file'
           return
      end if

      read (record(length+1:),*,iostat=istat) xlast,ylast
      if (istat /= 0) then
           err = DIO_ERROR
           msg = 'error decoding fourth line on old-style CPS mute file'
           return
      end if

      call pjar_put (pjar,'xlast'   ,xlast)
      call pjar_put (pjar,'ylast'   ,ylast)

!----------finish up and return:

      call dio_status (dio,err,msg)
      if (err == DIO_OK) msg = 'old-style CPS mute file header read'
      return
      end subroutine muteio1_read_header


!!-------------------------- muteio1 write header -------------------------!!
!!-------------------------- muteio1 write header -------------------------!!
!!-------------------------- muteio1 write header -------------------------!!


      subroutine muteio1_write_header (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct)    ,intent(inout) :: dio                 ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar                ! arguments
      character(len=*)    ,intent(in)    :: secname             ! arguments
      integer             ,intent(out)   :: err                 ! arguments
      character(len=*)    ,intent(out)   :: msg                 ! arguments
      character(len=80)                  :: record              ! local
      integer                            :: istat,length        ! local
      integer                            :: nho,nhx,nhy         ! local
      real                               :: xlast,ylast         ! local

!----------write first line:

      record = PHRASE1

      call dio_write_card (dio,record)
      call dio_status     (dio,err,msg)
      if (err /= DIO_OK) return

!----------write second line:

      call string_time_date (record)
      record = trim(PHRASE2)//' '//record

      call dio_write_card (dio,record)
      call dio_status     (dio,err,msg)
      if (err /= DIO_OK) return

!----------write third line:

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar,'nho'     ,nho)
      call pjar_get            (pjar,'nhx'     ,nhx)
      call pjar_get            (pjar,'nhy'     ,nhy)

      record = PHRASE3
      length = len_trim(record)
      write (record(length+3:),*,iostat=istat) nho,nhx,nhy
      if (istat /= 0) then
           err = DIO_ERROR
           msg = 'error encoding header words for old-style CPS mute file'
           return
      end if

      call dio_write_card (dio,record)
      call dio_status     (dio,err,msg)
      if (err /= DIO_OK) return

!----------write fourth line:

      call pjar_get (pjar,'xlast'   ,xlast)
      call pjar_get (pjar,'ylast'   ,ylast)

      record = PHRASE4
      length = len_trim(record)
      write (record(length+3:),*,iostat=istat) xlast,ylast
      if (istat /= 0) then
           err = DIO_ERROR
           msg = 'error encoding last coordinates for old-style CPS mute file'
           return
      end if

      call dio_write_card (dio,record)
      call dio_status     (dio,err,msg)
      if (err /= DIO_OK) return

!----------write fifth line:

      record = PHRASE5

      call dio_write_card (dio,record)
      call dio_status     (dio,err,msg)
      if (err == DIO_OK) msg = 'old-style CPS mute file header written'
      return
      end subroutine muteio1_write_header


!!------------------------ muteio1 read card ------------------------------!!
!!------------------------ muteio1 read card ------------------------------!!
!!------------------------ muteio1 read card ------------------------------!!
 
 
      subroutine muteio1_read_card  (dio,offset,xcoord,ycoord,time,err,msg)
      implicit none
      type(dio_struct)    ,intent(inout) :: dio                   ! arguments
      real                ,intent(out)   :: offset,xcoord,ycoord  ! arguments
      real                ,intent(out)   :: time                  ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=80)                  :: record                ! local
      integer                            :: istat                 ! local
 
      offset = 0.0
      xcoord = 0.0
      ycoord = 0.0
      time   = 0.0

      do
           call dio_read_card (dio,record)
           call dio_status    (dio,err,msg)
           if (err /= DIO_OK) return
           if (record      == ' ') cycle
           if (record(1:1) == '#') cycle
           exit
      end do

      read (record,*,iostat=istat) offset,xcoord,ycoord,time
      if (istat /= 0) then
           err = DIO_ERROR
           msg = 'error decoding card image from old-style CPS mute file'
           return
      end if

      err = DIO_OK
      msg = 'ok' 
      return
      end subroutine muteio1_read_card
 
 
!!------------------------- muteio1 write card ---------------------------!!
!!------------------------- muteio1 write card ---------------------------!!
!!------------------------- muteio1 write card ---------------------------!!
 

      subroutine muteio1_write_card (dio,offset,xcoord,ycoord,time,err,msg)
      implicit none
      type(dio_struct)    ,intent(inout) :: dio                   ! arguments
      real                ,intent(in)    :: offset,xcoord,ycoord  ! arguments
      real                ,intent(in)    :: time                  ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      character(len=80)                  :: record                ! local
      integer                            :: istat                 ! local

      write (record,*,iostat=istat) offset,xcoord,ycoord,time
      if (istat /= 0) then
           err = DIO_ERROR
           msg = 'error encoding card image for old-style CPS mute file'
           return
      end if

      call dio_write_card (dio,record)
      call dio_status     (dio,err,msg)
      return
      end subroutine muteio1_write_card
 

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module muteio1_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
