!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ statio2.f90 -------------------------------!!
!!------------------------------ statio2.f90 -------------------------------!!
!!------------------------------ statio2.f90 -------------------------------!!


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
! Name       : STATIO2
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : To read and write a static data section in a self-defining file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing a single static data
! section in a self-defining non-trace file which might contain various
! sections of different types.
!
! This primitive does not open or close the file.
! This primitive does not read or write header sections.
! This primitive cannot read or write old-style CPS static files.
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
!                                   b     b      i      o    o       o
!      call statio2_read_statics  (fio, pjar, secname, err, msg, pstatics)
!      call statio2_scan_statics  (fio, pjar, secname, err, msg)
!      call statio2_write_statics (fio, pjar, secname, err, msg,  statics)
!                                   b     b      i      o    o       i
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(fio_struct)         fio = reference to the FIO data structure to use.
! type(pjar_struct)       pjar = reference to the PJAR data structure to use.
! char(*)              secname = PJAR section containing the parameters.
! integer                  err = error flag (returned).
! char(*)                  msg = message for possible printing (returned).
! real          statics(nx*ny) = array of static values (1D).
! real         pstatics(nx*ny) = pointer to array of static values (1D).
!
! ERR will be set to FIO_OK or FIO_ERROR.
!
! PSTATICS is a pointer which must be nullified before first use.
! It will be deallocated and reallocated to contain the returned contents.
! It will always be allocated to a dimension of at least one.
! It should be conditionally deallocated when no longer needed.
!
!-------------------------------------------------------------------------------
!                       PJAR PARAMETERS ACCESSED
!
!     parameter       scan_statics        read_statics     write_statics
!                    (AH)       (B)        (AH)   (B)       (AH)   (B)
!     ---------     -----------------     ------------     -------------
!     encoding      input      input      input  input     input  input
!     fields(:)     input      input      input  input     input  input
!     ncolumns      input        -        input    -         -      -
!     x1            output(C)    -        input    -       input    - 
!     y1            output(C)    -        input    -       input    - 
!     xinc          output(C)    -        input    -       input    - 
!     yinc          output(C)    -        input    -       input    - 
!     nx            output(C)  input      input  input     input  input
!     ny            output(C)  input      input  input     input  input
!     statmin       output     output       -      -         -      -
!     statmax       output     output       -      -         -      -
!     numnils       output     output       -      -         -      -
!
! Note (AH): encoding is FIO_ASCII or FIO_HYBRID.
! Note  (B): encoding is FIO_BINARY.
! Note  (C): These are output only if ncolumns > 1.
!
! See elsewhere for parameter definitions.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  5. 2007-11-29  Stoeckley  Eliminate the memman primitive.
!004. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!  3. 2003-06-17  Stoeckley  Replace ARRAY_MODULE references with MEMMAN.
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  1. 2000-11-27  Stoeckley  Initial version moved from portions of code in
!                             the STATIO primitive.
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
 
 
      module statio2_module
      use pjar_module
      use fio_module
      use named_constants_module
      use increment_module
      implicit none
      public

      character(len=100),public,save :: STATIO2_IDENT = &
'$Id: statio2.f90,v 1.5 2007/11/30 13:55:19 Stoeckley beta sps $'

      contains


!!-------------------------------- scan statics ---------------------------!!
!!-------------------------------- scan statics ---------------------------!!
!!-------------------------------- scan statics ---------------------------!!


      subroutine statio2_scan_statics (fio,pjar,secname,err,msg)
      implicit none
      type(fio_struct)    ,intent(inout) :: fio                   ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar                  ! arguments
      character(len=*)    ,intent(in)    :: secname               ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real                               :: x1,y1,xinc,yinc       ! local
      real                               :: statmin,statmax       ! local
      integer                            :: numnils,nx,ny         ! local
      character(len=8)                   :: encoding              ! local
      integer                            :: npicks,ipick,ierr     ! local
      real                               :: xcoord,ycoord,static  ! local
      type(increment_struct)             :: incx,incy             ! local
      real                ,pointer       :: pstatics(:)           ! local
      integer                            :: col_XCOORD            ! local
      integer                            :: col_YCOORD            ! local
      integer                            :: col_STATIC            ! local

      nullify (pstatics) ! jpa

!----------get started:
 
      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', encoding)
      call pjar_get            (pjar, 'nx'      , nx)
      call pjar_get            (pjar, 'ny'      , ny)
 
      col_xcoord = pjar_find (pjar, 'fields', 'xcoord')
      col_ycoord = pjar_find (pjar, 'fields', 'ycoord')
      col_static = pjar_find (pjar, 'fields', 'static')

      if (encoding /= FIO_BINARY) then
           call increment_init (incx, 1.0, 12)
           call increment_init (incy, 1.0, 12)
      end if

      statmin = FNIL
      statmax = FNIL
      numnils = 0

!----------scan binary static values:
 
      if (encoding == FIO_BINARY) then
 
           call fio_before_read_binary (fio, npicks)

           if (npicks /= nx * ny) call fio_register_error &
                            (fio,'mismatching number of static values')

           allocate (pstatics(npicks), stat=ierr)

           if (ierr /= 0) call fio_register_error &
                            (fio,'pstatics allocation error')

           call fio_read_binary        (fio, col_STATIC, pstatics)
           call fio_after_read_binary  (fio, err, msg)

           do ipick = 1,npicks
                static = pstatics(ipick)
                if (static == FNIL) then
                     numnils = numnils + 1
                else if (statmin == FNIL) then
                     statmin = static
                     statmax = static
                else
                     statmin = min(static,statmin)
                     statmax = max(static,statmax)
                end if
           end do
           if (associated(pstatics)) deallocate (pstatics)

!----------scan ascii or hybrid static values:
 
      else
 
           do
                call fio_before_read_line (fio)

                call fio_read_line (fio, col_XCOORD, xcoord)
                call fio_read_line (fio, col_YCOORD, ycoord)
                call fio_read_line (fio, col_STATIC, static)

                call fio_after_read_line (fio,err,msg)
                if (err == FIO_EOF) then
                     err = FIO_OK
                     exit
                else if (err /= FIO_OK) then
                     exit
                end if

                if (xcoord /= FNIL) call increment_update (incx, xcoord)
                if (ycoord /= FNIL) call increment_update (incy, ycoord)

                if (static == FNIL) then
                     numnils = numnils + 1
                else if (statmin == FNIL) then
                     statmin = static
                     statmax = static
                else
                     statmin = min(static,statmin)
                     statmax = max(static,statmax)
                end if
           end do

      end if

!----------finish up and return:

      if (encoding /= FIO_BINARY) then
           call increment_result (incx, xbmin=x1, xinc=xinc, nx=nx)
           call increment_result (incy, xbmin=y1, xinc=yinc, nx=ny)
           call pjar_put         (pjar, 'x1'   , x1)
           call pjar_put         (pjar, 'y1'   , y1)
           call pjar_put         (pjar, 'xinc' , xinc)
           call pjar_put         (pjar, 'yinc' , yinc)
           call pjar_put         (pjar, 'nx'   , nx)
           call pjar_put         (pjar, 'ny'   , ny)
      end if

      if (statmin == FNIL) then
           statmin = 0.0
           statmax = 0.0
      end if

      call pjar_put (pjar, 'statmin' , statmin)
      call pjar_put (pjar, 'statmax' , statmax)
      call pjar_put (pjar, 'numnils' , numnils)
      return
      end subroutine statio2_scan_statics


!!-------------------------------- read statics ---------------------------!!
!!-------------------------------- read statics ---------------------------!!
!!-------------------------------- read statics ---------------------------!!
 
 
      subroutine statio2_read_statics (fio,pjar,secname,err,msg,pstatics)
      implicit none
      type(fio_struct)    ,intent(inout) :: fio                   ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar                  ! arguments
      character(len=*)    ,intent(in)    :: secname               ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real                ,pointer       :: pstatics(:)           ! arguments
      character(len=8)                   :: encoding              ! local
      integer                            :: ncolumns,nx,ny,ierr   ! local
      real                               :: x1,y1,xinc,yinc       ! local
      integer                            :: npicks,indx,ix,iy     ! local
      real                               :: xcoord,ycoord,static  ! local
      integer                            :: col_XCOORD            ! local
      integer                            :: col_YCOORD            ! local
      integer                            :: col_STATIC            ! local
 
!----------get started:

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', encoding)
      call pjar_get            (pjar, 'ncolumns', ncolumns)
      call pjar_get            (pjar, 'nx'      , nx)
      call pjar_get            (pjar, 'ny'      , ny)
      call pjar_get            (pjar, 'x1'      , x1)
      call pjar_get            (pjar, 'y1'      , y1)
      call pjar_get            (pjar, 'xinc'    , xinc)
      call pjar_get            (pjar, 'yinc'    , yinc)
 
      col_xcoord = pjar_find (pjar, 'fields', 'xcoord')
      col_ycoord = pjar_find (pjar, 'fields', 'ycoord')
      col_static = pjar_find (pjar, 'fields', 'static')

      npicks = nx * ny

!----------read binary static values:
 
      if (encoding == FIO_BINARY) then
 
           call fio_before_read_binary (fio, npicks)

           if (npicks /= nx * ny) call fio_register_error &
                        (fio,'mismatching number of static values')

           if (associated(pstatics)) deallocate (pstatics)
           allocate (pstatics(npicks),stat=ierr)

           if (ierr /= 0) call fio_register_error &
                            (fio,'pstatics allocation error')

           call fio_read_binary        (fio, col_STATIC, pstatics)
           call fio_after_read_binary  (fio, err, msg)
 
!----------read ascii or hybrid static values:
 
      else
 
           if (associated(pstatics)) deallocate (pstatics)
           allocate (pstatics(npicks),stat=ierr)

           if (ierr /= 0) then
                call fio_register_error (fio,'pstatics allocation error')
                return
           end if

           indx = 0
           pstatics(1:npicks) = FNIL
           do
                call fio_before_read_line (fio)

                call fio_read_line (fio, col_XCOORD, xcoord)
                call fio_read_line (fio, col_YCOORD, ycoord)
                call fio_read_line (fio, col_STATIC, static)

                call fio_after_read_line (fio, err, msg)
                if (err == FIO_EOF) then
                     err = FIO_OK
                     exit
                else if (err /= FIO_OK) then
                     exit
                end if
 
                if (ncolumns > 1) then
                     if (xcoord == FNIL .or. ycoord == FNIL) cycle
                     ix = nint((xcoord - x1) / xinc) + 1
                     iy = nint((ycoord - y1) / yinc) + 1
                     if (ix >= 1 .and. ix <= nx .and.  &
                         iy >= 1 .and. iy <= ny) then
                              indx = ix + (iy-1) * nx
                              pstatics(indx) = static
                     end if
                else
                     indx = indx + 1
                     if (indx <= npicks) pstatics(indx) = static
                end if
           end do
 
      end if
      return
      end subroutine statio2_read_statics
 
 
!!------------------------------ write statics ---------------------------!!
!!------------------------------ write statics ---------------------------!!
!!------------------------------ write statics ---------------------------!!
 

      subroutine statio2_write_statics (fio,pjar,secname,err,msg,statics)
      implicit none
      type(fio_struct)    ,intent(inout) :: fio                   ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar                  ! arguments
      character(len=*)    ,intent(in)    :: secname               ! arguments
      integer             ,intent(out)   :: err                   ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      real                ,intent(in)    :: statics(:)            ! arguments
      character(len=8)                   :: encoding              ! local
      integer                            :: nx,ny                 ! local
      real                               :: x1,y1,xinc,yinc       ! local
      integer                            :: npicks,indx,ix,iy     ! local
      real                               :: xcoord,ycoord,static  ! local
      integer                            :: col_XCOORD            ! local
      integer                            :: col_YCOORD            ! local
      integer                            :: col_STATIC            ! local

!----------get started:

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', encoding)
      call pjar_get            (pjar, 'nx'      , nx)
      call pjar_get            (pjar, 'ny'      , ny)
      call pjar_get            (pjar, 'x1'      , x1)
      call pjar_get            (pjar, 'y1'      , y1)
      call pjar_get            (pjar, 'xinc'    , xinc)
      call pjar_get            (pjar, 'yinc'    , yinc)
 
      col_xcoord = pjar_find (pjar, 'fields', 'xcoord')
      col_ycoord = pjar_find (pjar, 'fields', 'ycoord')
      col_static = pjar_find (pjar, 'fields', 'static')

!----------write binary static values:
 
      if (encoding == FIO_BINARY) then
 
           npicks = nx * ny
           call fio_before_write_binary (fio, npicks)
           call fio_write_binary        (fio, col_STATIC, statics)
           call fio_after_write_binary  (fio, err, msg)
 
!----------write ascii or hybrid static values:
 
      else
 
           err = FIO_OK
           do iy = 1,ny
             do ix = 1,nx
               xcoord = x1 + (ix-1) * xinc
               ycoord = y1 + (iy-1) * yinc
               indx   = ix + (iy-1) * nx
               static = statics(indx)

               call fio_before_write_line (fio)
 
               call fio_write_line (fio, col_XCOORD, xcoord, ndec=3)
               call fio_write_line (fio, col_YCOORD, ycoord, ndec=3)
               call fio_write_line (fio, col_STATIC, static, ndec=3)
 
               call fio_after_write_line (fio,err,msg)
               if (err == FIO_ERROR) exit
             end do
             if (err == FIO_ERROR) exit
           end do
 
      end if
      return
      end subroutine statio2_write_statics
 

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module statio2_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
