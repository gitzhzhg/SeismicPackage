!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ muteio2.f90 -------------------------------!!
!!------------------------------ muteio2.f90 -------------------------------!!
!!------------------------------ muteio2.f90 -------------------------------!!


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
! Name       : MUTEIO2
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2001-12-28   by: Tom Stoeckley
! Maturity   : production   2002-02-04
! Purpose    : To read and write self-defining ascii and binary mute files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing a single mute data
! section in a self-defining non-trace file which might contain various
! sections of different file types.
!
! This primitive does not open or close the file.
! This primitive reads and writes self defining ascii and hybrid mute files.
! This primitive cannot read or write self defining binary mute files.
! This primitive cannot read or write old-style CPS mute files.
! The calling program must use the PJAR and FIO primitives with this primitive.
! See documentation in PJAR and FIO for details.
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
!                              i   b   b      i
!    call muteio2_create     (obj,fio,pjar,secname)
!    call muteio2_delete     (obj)
!                              b
!
!                              b    o      o      o     o    o   o
!    call muteio2_read_card  (obj,offset,xcoord,ycoord,time,err,msg)
!    call muteio2_write_card (obj,offset,xcoord,ycoord,time,err,msg)
!                              b    i      i      i     i    o   o
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(muteio2_struct)  obj = pointer the to MUTEIO2 data structure.
! type(fio_struct)      fio = reference to the FIO data structure to use.
! type(pjar_struct)    pjar = reference to the PJAR data structure to use.
! char(*)           secname = PJAR section containing the parameters.
! integer               err = error flag (returned).
! char(*)               msg = message for possible printing (returned).
!
! real    offset   = mute location offset.
! real    xcoord   = mute location X coordinate.
! real    ycoord   = mute location Y coordinate.
! real    time     = mute time.
!
! ERR will be set to FIO_OK or FIO_ERROR or FIO_EOF.
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
!                             new PJAR and FIO primitives.
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
 
 
      module muteio2_module
      use fio_module
      use pjar_module
      implicit none
      public

      character(len=100),public,save :: MUTEIO2_IDENT = &
'$Id: muteio2.f90,v 1.2 2002/01/30 20:05:21 Stoeckley prod sps $'

      type,public :: muteio2_struct

         private
         type(fio_struct) ,pointer :: fio
         integer                   :: col_OFFSET 
         integer                   :: col_XCOORD 
         integer                   :: col_YCOORD 
         integer                   :: col_TIME   

      end type muteio2_struct

      contains


!!-------------------------- muteio2 create -----------------------------!!
!!-------------------------- muteio2 create -----------------------------!!
!!-------------------------- muteio2 create -----------------------------!!


      subroutine muteio2_create (obj,fio,pjar,secname)
      implicit none
      type(muteio2_struct),pointer              :: obj           ! arguments
      type(fio_struct)    ,intent(inout),target :: fio           ! arguments
      type(pjar_struct)   ,intent(inout)        :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments

      allocate (obj)

      obj%fio => fio

      call pjar_choose_section (pjar, secname)

      obj%col_offset = pjar_find (pjar, 'fields', 'offset')
      obj%col_xcoord = pjar_find (pjar, 'fields', 'xcoord')
      obj%col_ycoord = pjar_find (pjar, 'fields', 'ycoord')
      obj%col_time   = pjar_find (pjar, 'fields', 'time'  )
      return
      end subroutine muteio2_create


!!--------------------------- muteio2 delete -----------------------------!!
!!--------------------------- muteio2 delete -----------------------------!!
!!--------------------------- muteio2 delete -----------------------------!!


      subroutine muteio2_delete (obj)
      implicit none
      type(muteio2_struct),pointer :: obj             ! arguments

      if (associated(obj)) deallocate(obj)
      return
      end subroutine muteio2_delete


!!------------------------ muteio2 read card ------------------------------!!
!!------------------------ muteio2 read card ------------------------------!!
!!------------------------ muteio2 read card ------------------------------!!
 
 
      subroutine muteio2_read_card (obj,offset,xcoord,ycoord,time,err,msg)
      implicit none
      type(muteio2_struct),intent(inout) :: obj                   ! arguments
      real                ,intent(out)   :: offset,xcoord,ycoord  ! arguments
      real                ,intent(out)   :: time                  ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
 
      call fio_before_read_line (obj%fio)

      call fio_read_line (obj%fio, obj%col_offset    ,offset)
      call fio_read_line (obj%fio, obj%col_xcoord    ,xcoord)
      call fio_read_line (obj%fio, obj%col_ycoord    ,ycoord)
      call fio_read_line (obj%fio, obj%col_time      ,time  )

      call fio_after_read_line (obj%fio,err,msg)
      return
      end subroutine muteio2_read_card
 
 
!!------------------------- muteio2 write card ---------------------------!!
!!------------------------- muteio2 write card ---------------------------!!
!!------------------------- muteio2 write card ---------------------------!!
 

      subroutine muteio2_write_card (obj,offset,xcoord,ycoord,time,err,msg)
      implicit none
      type(muteio2_struct),intent(inout) :: obj                   ! arguments
      real                ,intent(in)    :: offset,xcoord,ycoord  ! arguments
      real                ,intent(in)    :: time                  ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments

      call fio_before_write_line (obj%fio)

      call fio_write_line (obj%fio, obj%col_offset    , offset, ndec=0)
      call fio_write_line (obj%fio, obj%col_xcoord    , xcoord, ndec=1)
      call fio_write_line (obj%fio, obj%col_ycoord    , ycoord, ndec=1)
      call fio_write_line (obj%fio, obj%col_time      , time  , ndec=3)

      call fio_after_write_line (obj%fio,err,msg)
      return
      end subroutine muteio2_write_card
 

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module muteio2_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
