!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- statio1.f90 --------------------------------!!
!!----------------------------- statio1.f90 --------------------------------!!
!!----------------------------- statio1.f90 --------------------------------!!


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
! Name       : STATIO1
! Category   : io
! Written    : 2000-10-12   by: Tom Stoeckley
! Revised    : 2001-12-28   by: Tom Stoeckley
! Maturity   : production   2002-02-04
! Purpose    : To read and write old-style CPS static files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is used for reading and writing old-style CPS static files.
!
! This primitive does not open or close the file.
! The calling program must use the PJAR and DIO primitives with this primitive.
! See documentation in PJAR and DIO for details.
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
!        call statio1_read_file   (dio, pjar, secname, err, msg, pstatics)
!        call statio1_read_header (dio, pjar, secname, err, msg)
!        call statio1_scan_file   (dio, pjar, secname, err, msg)
!        call statio1_write_file  (dio, pjar, secname, err, msg,  statics)
!                                   b     b      i      o    o       i
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(gio_struct)         dio = reference to the DIO data structure to use.
! type(pjar_struct)       pjar = reference to the PJAR data structure to use.
! char(*)              secname = PJAR section containing the parameters.
! integer                  err = error flag (returned).
! char(*)                  msg = message for possible printing (returned).
! real          statics(nx*ny) = array of static values (1D).
! real         pstatics(nx*ny) = pointer to array of static values (1D).
!
! The PJAR section 'history' will also be accessed.
!
! ERR will be set to DIO_OK or DIO_ERROR.
!
! PSTATICS is a pointer which must be nullified before first use.
! It will be deallocated and reallocated to contain the returned contents.
! It will always be allocated to a dimension of at least one. 
! It should be conditionally deallocated when no longer needed.
!
!-------------------------------------------------------------------------------
!                       PJAR PARAMETERS ACCESSED
!
!     parameter    read_file    read_header    scan_file     write_file
!     ---------    ---------    -----------    ---------     ----------
!     stattype      output        output         output        input  
!     nhx           output        output         output        input  
!     nhy           output        output         output        input  
!     nhx2          output        output         output        input  
!     nhy2          output        output         output        input  
!     x1            output        output         output        input  
!     y1            output        output         output        input  
!     xinc          output        output         output        input  
!     yinc          output        output         output        input  
!     nx            output        output         output        input  
!     ny            output        output         output        input  
!     statmin         -             -            output          -    
!     statmax         -             -            output          -    
!     numnils         -             -            output          -    
!     hist(nhist)   output        output         output        input
!
! See elsewhere for parameter definitions.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                
!
!  Old-style CPS static files have the following format:
!
!     first record:        (a8,4i4/4g14.6,2i7)  stattype,nhx,nhy,nhx2,nhy2,
!                                               x1,y1,xinc,yinc,nx,ny
!     each history card:        (a80)           card
!     special card after all
!       (or no) history cards:  (a80)           card (='+++END+++')
!     static values written:    (i8,5g14.6)     static (nx*ny values)
!     static values read:       (8x,5g14.0)     static (nx*ny values)
!
!     The field designated i8 contains the 1-dimensional index of the
!     first value of array 'static' on that line.  This field is skipped
!     while reading the file.  It is intended as help to the user if he
!     is viewing or editing the static file with a general-purpose text
!     editor such as evv, or better yet vi.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR primitive.
!  2. 2001-02-13  Stoeckley  Modified I/O of STATTYPE so that it is read and
!                             written correctly when len(stattype) < 8.
!  1. 2000-11-27  Stoeckley  Initial version moved from private routines in
!                             the STATIO primitive which came originally from
!                             the old CPS primitive STATRII.
!
! Revision history of the old STATRII primitive:
!
!     Date        Author     Description
!     ----        ------     -----------
! 11. 1999-09-09  O'Brien    Documentation fix.
! 10. 1999-08-27  O'Brien    Fixed many bugs flushed out in initial testing.
!  9. 1999-08-13  O'Brien    Full f90 conversion.
!                            Subroutine naming convention changed:
!                             From: statrii   ==>   To: statio_oldcps_rii
!                             From: statrcc   ==>   To: statio_oldcps_rcc
!                             From: statread  ==>   To: statio_oldcps_read
!                             From: statwii   ==>   To: statio_oldcps_wii
!                             From: statwcc   ==>   To: statio_oldcps_wcc
!                             From: statwrit  ==>   To: statio_oldcps_write
!  8. 1999-01-25  Goodger    Begin using the fortran90 compiler.
!  7. 1994-04-07  Troutt     Make sure stattype is left justified after being
!                            read from file.
!  6. 1992-01-13  Peterson   Change 2000 format from 5g14.6 to 5g14.0
!  5. 1991-06-20  Troutt     Made all error returns for reads and writes
!                            unigue for making trouble shooting easier.
!  4. 1989-05-08  Stoeckley  Minor change in documentation.
!  3. 1989-01-30  Stoeckley  Add documentation for use on the VAX.
!  2. 1988-12-05  Stoeckley  Add some print statements.
!  1. 1988-11-23  Stoeckley  Add second set of headers, and stattype code.
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
 
 
      module statio1_module
      use dio_module
      use pjar_module
      use statutil_module
      implicit none
      private
      public :: statio1_scan_file
      public :: statio1_read_file
      public :: statio1_read_header
      public :: statio1_write_file
 
      character(len=100),public,save :: STATIO1_IDENT = &
'$Id: statio1.f90,v 1.3 2002/01/30 20:15:53 Stoeckley prod sps $'

      character(len=*),private,parameter :: ENDFLAG = '+++END+++'

      contains


!!--------------------------- statio1 scan file ----------------------------!!
!!--------------------------- statio1 scan file ----------------------------!!
!!--------------------------- statio1 scan file ----------------------------!!


      subroutine statio1_scan_file (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct) ,intent(inout)  :: dio                      ! arguments
      type(pjar_struct),intent(inout)  :: pjar                     ! arguments
      character(len=*) ,intent(in)     :: secname                  ! arguments
      integer          ,intent(out)    :: err                      ! arguments
      character(len=*) ,intent(out)    :: msg                      ! arguments
      real             ,pointer        :: pstatics(:)              ! local
      real                             :: statmin,statmax          ! local
      integer                          :: nx,ny,numnils,nstatics   ! local

      nullify (pstatics)
      call statio1_read_file (dio,pjar,secname,err,msg,pstatics)
      if (.not.associated(pstatics)) return
      if (err /= DIO_OK) then
           deallocate (pstatics)
           return
      end if

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'nx', nx)
      call pjar_get            (pjar, 'ny', ny)

      nstatics = nx * ny
      call statutil_scan_statics (nstatics,pstatics,statmin,statmax,numnils)

      call pjar_put (pjar, 'statmin', statmin)
      call pjar_put (pjar, 'statmax', statmax)
      call pjar_put (pjar, 'numnils', numnils)
      deallocate (pstatics)
      return
      end subroutine statio1_scan_file


!!--------------------------- statio1 read header --------------------------!!
!!--------------------------- statio1 read header --------------------------!!
!!--------------------------- statio1 read header --------------------------!!


      subroutine statio1_read_header (dio,pjar,secname,err,msg)
      implicit none
      type(dio_struct) ,intent(inout)       :: dio               ! arguments
      type(pjar_struct),intent(inout)       :: pjar              ! arguments
      character(len=*) ,intent(in)          :: secname           ! arguments
      integer          ,intent(out)         :: err               ! arguments
      character(len=*) ,intent(out)         :: msg               ! arguments
      character(len=8)                      :: stattype          ! local
      integer                               :: nhx,nhy,nhx2,nhy2 ! local
      real                                  :: x1,y1,xinc,yinc   ! local
      integer                               :: nx,ny,nhist       ! local
      character(len=80),allocatable         :: phist(:)          ! local
 
!----------read the header:

      call statio1_oldcps_rii (dio,stattype,nhx,nhy,nhx2,nhy2,  &
                               x1,y1,xinc,yinc,nx,ny,nhist,err,msg)

      if (err /= DIO_OK) return

      call pjar_choose_section (pjar, secname)
      call pjar_put            (pjar, 'stattype' , stattype)
      call pjar_put            (pjar, 'nhx'      , nhx)
      call pjar_put            (pjar, 'nhy'      , nhy)
      call pjar_put            (pjar, 'nhx2'     , nhx2)
      call pjar_put            (pjar, 'nhy2'     , nhy2)
      call pjar_put            (pjar, 'x1'       , x1)
      call pjar_put            (pjar, 'y1'       , y1)
      call pjar_put            (pjar, 'xinc'     , xinc)
      call pjar_put            (pjar, 'yinc'     , yinc)
      call pjar_put            (pjar, 'nx'       , nx)
      call pjar_put            (pjar, 'ny'       , ny)

!----------read the history cards:

      allocate (phist(max(nhist,1)))

      call statio1_oldcps_rcc (dio,phist,nhist,err,msg)

      if (err /= DIO_OK) then
            deallocate (phist)
            return
      end if

      call pjar_choose_section (pjar, 'history')
      call pjar_put_cards      (pjar, phist, nhist)

      deallocate (phist)
      return
      end subroutine statio1_read_header


!!--------------------------- statio1 read file ----------------------------!!
!!--------------------------- statio1 read file ----------------------------!!
!!--------------------------- statio1 read file ----------------------------!!


      subroutine statio1_read_file (dio,pjar,secname,err,msg,pstatics)
      implicit none
      type(dio_struct) ,intent(inout)       :: dio               ! arguments
      type(pjar_struct),intent(inout)       :: pjar              ! arguments
      character(len=*) ,intent(in)          :: secname           ! arguments
      integer          ,intent(out)         :: err               ! arguments
      character(len=*) ,intent(out)         :: msg               ! arguments
      real             ,pointer             :: pstatics(:)       ! arguments
      integer                               :: nx,ny             ! local
 
!----------get started:

      if (associated(pstatics)) deallocate (pstatics)
      allocate (pstatics(1))

!----------read the header:

      call statio1_read_header (dio,pjar,secname,err,msg)
      if (err /= DIO_OK) return

!----------read the static values:

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'nx', nx)
      call pjar_get            (pjar, 'ny', ny)

      deallocate (pstatics)
      allocate (pstatics(max(nx*ny,1)))

      call statio1_oldcps_read (dio,nx,ny,pstatics,err,msg)
      return
      end subroutine statio1_read_file


!!-------------------------- statio1 write file -----------------------------!!
!!-------------------------- statio1 write file -----------------------------!!
!!-------------------------- statio1 write file -----------------------------!!


      subroutine statio1_write_file (dio,pjar,secname,err,msg,statics)
      implicit none
      type(dio_struct) ,intent(inout)       :: dio               ! arguments
      type(pjar_struct),intent(inout)       :: pjar              ! arguments
      character(len=*) ,intent(in)          :: secname           ! arguments
      integer          ,intent(out)         :: err               ! arguments
      character(len=*) ,intent(out)         :: msg               ! arguments
      real             ,intent(in)          :: statics(:)        ! arguments
      character(len=8)                      :: stattype          ! local
      integer                               :: nhx,nhy,nhx2,nhy2 ! local
      real                                  :: x1,y1,xinc,yinc   ! local
      integer                               :: nx,ny,nhist       ! local
      character(len=80),pointer             :: phist(:)          ! local

!----------write the header:

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'stattype' , stattype)
      call pjar_get            (pjar, 'nhx'      , nhx)
      call pjar_get            (pjar, 'nhy'      , nhy)
      call pjar_get            (pjar, 'nhx2'     , nhx2)
      call pjar_get            (pjar, 'nhy2'     , nhy2)
      call pjar_get            (pjar, 'x1'       , x1)
      call pjar_get            (pjar, 'y1'       , y1)
      call pjar_get            (pjar, 'xinc'     , xinc)
      call pjar_get            (pjar, 'yinc'     , yinc)
      call pjar_get            (pjar, 'nx'       , nx)
      call pjar_get            (pjar, 'ny'       , ny)

      call statio1_oldcps_wii (dio,stattype,nhx,nhy,nhx2,nhy2,  &
                               x1,y1,xinc,yinc,nx,ny,err,msg)

      if (err /= DIO_OK) return

!----------write the history cards:

      nullify (phist)
 
      call pjar_choose_section (pjar, 'history')
      call pjar_alloc_cards    (pjar, phist, nhist)

      call statio1_oldcps_wcc  (dio,phist,nhist,err,msg)

      deallocate (phist)

      if (err /= DIO_OK) return

!----------write the static values:

      call statio1_oldcps_write (dio,nx,ny,statics,err,msg)
      return
      end subroutine statio1_write_file


!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!
!!------------ private routines from the old statrii primitive ------------!!


!!------------------------ statio1 oldcps rii ---------------------------!!
!!------------------------ statio1 oldcps rii ---------------------------!!
!!------------------------ statio1 oldcps rii ---------------------------!!

! Read initial information.


      subroutine statio1_oldcps_rii &
                           (dio,stattype,nhx,nhy,nhx2,nhy2,x1,y1, &
                            xinc,yinc,nx,ny,nhist,err,msg) 
      implicit none
      type(dio_struct),intent(inout) :: dio                    ! arguments
      integer,         intent(out)   :: nhx,nhy,nhx2,nhy2      ! arguments
      integer,         intent(out)   :: nx,ny, nhist           ! arguments
      real,            intent(out)   :: x1,xinc, y1,yinc       ! arguments
      character(len=*),intent(out)   :: stattype               ! arguments
      integer,         intent(out)   :: err                    ! arguments
      character(len=*),intent(out)   :: msg                    ! arguments
      integer                        :: ier                    ! local
      character(len=100)             :: card                   ! local
      character(len=*),parameter     :: fmt3 = '(a8,4i4)'      ! local
      character(len=*),parameter     :: fmt4 = '(4g14.6,2i7)'  ! local
      character(len=8)               :: stattype2              ! local

!  Initialize a couple variables.

      nhist = 0

!  Read first header card.

      call dio_rewind    (dio)
      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg)
      if (err /= DIO_OK) return
      read (card, fmt3, iostat=ier) stattype2, nhx, nhy, nhx2, nhy2
      if (ier /= 0) then
        err = DIO_ERROR
        msg = 'error decoding first line of old-style CPS static file header'
        return
      endif

      stattype = stattype2
           ! so the A8 format will work correctly when len(stattype) < 8.

!  Read second header card.

      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg)
      if (err /= DIO_OK) return
      read (card, fmt4, iostat=ier) x1, y1, xinc, yinc, nx, ny
      if (ier /= 0) then
        err = DIO_ERROR
        msg = 'error decoding second line of old-style CPS static file header'
        return
      endif

!  Make sure STATTYPE is left justified.

      stattype = adjustl(stattype)

!  Count the history cards.

      do
        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg)
        if (err /= DIO_OK) return
        if (card == ENDFLAG) exit
        nhist = nhist + 1
      enddo

!  Reset the static file pointer to the card following the initial info.

      call dio_rewind    (dio)
      call dio_read_card (dio,card)
      call dio_read_card (dio,card)
      call dio_status    (dio,err,msg)
      return
      end subroutine statio1_oldcps_rii


!!------------------------ statio1 oldcps rcc ---------------------------!!
!!------------------------ statio1 oldcps rcc ---------------------------!!
!!------------------------ statio1 oldcps rcc ---------------------------!!

! Read history cards. 


      subroutine statio1_oldcps_rcc (dio,hist,nhist,err,msg)
      implicit none
      type(dio_struct), intent(inout) :: dio              ! arguments
      character(len=*), intent(out)   :: hist(:)          ! arguments
      integer,          intent(out)   :: nhist            ! arguments
      integer,          intent(out)   :: err              ! arguments
      character(len=*), intent(out)   :: msg              ! arguments
      character(len=80)               :: card             ! local

      nhist = 0
      do
        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg)
        if (err /= DIO_OK) return
        if (card == ENDFLAG) exit
        if (size(hist) <= nhist) then
             err = DIO_ERROR
             msg = 'history card array too small to contain all history cards'
             return
        end if
        nhist = nhist + 1
        hist(nhist) = card
      enddo
      call dio_backspace (dio)
      return
      end subroutine statio1_oldcps_rcc


!!------------------------ statio1 oldcps read ---------------------------!!
!!------------------------ statio1 oldcps read ---------------------------!!
!!------------------------ statio1 oldcps read ---------------------------!!

! Read static values.


      subroutine statio1_oldcps_read (dio,nx,ny,statics,err,msg)
      implicit none
      type(dio_struct),intent(inout)  :: dio                   ! arguments
      integer,         intent(in)     :: nx,ny                 ! arguments
      real,            intent(out)    :: statics(:)            ! arguments
      integer,         intent(out)    :: err                   ! arguments
      character(len=*),intent(out)    :: msg                   ! arguments
      integer                         :: i,i2,ier,nstatics     ! local
      character(len=80)               :: card                  ! local
      character(len=*),parameter      :: fmt2 = '(8x,5g14.0)'  ! local

!  Get started.

      nstatics = nx*ny
      if (size(statics) < nstatics) then
           err = DIO_ERROR
           msg = 'statics array too small to contain static values'
           return
      end if
 
!  Get to the first static data card.

      do
        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg)
        if (err /= DIO_OK) return
        if (card == ENDFLAG) exit
      enddo

!  Now read in the static field.

      do i = 1,nstatics,5
        call dio_read_card (dio,card)
        call dio_status    (dio,err,msg)
        if (err /= DIO_OK) return
        i2 = min(i+4,nstatics)
        read (card, fmt2, iostat=ier) statics(i:i2)
        if (ier /= 0) then
             err = DIO_ERROR
             write(msg,*) 'error decoding old style static file value ',i
             return
        endif
      enddo
      return
      end subroutine statio1_oldcps_read


!!------------------------ statio1 oldcps wii ---------------------------!!
!!------------------------ statio1 oldcps wii ---------------------------!!
!!------------------------ statio1 oldcps wii ---------------------------!!

! Write initial information.


      subroutine statio1_oldcps_wii &
                             (dio,stattype,nhx,nhy,nhx2,nhy2,x1,y1, &
                              xinc,yinc,nx,ny,err,msg) 
      implicit none
      type(dio_struct),intent(inout) :: dio                    ! arguments
      integer,         intent(in)    :: nhx,nhy,nhx2,nhy2      ! arguments
      integer,         intent(in)    :: nx,ny                  ! arguments
      real,            intent(in)    :: x1,xinc, y1,yinc       ! arguments
      character(len=*),intent(in)    :: stattype               ! arguments
      integer,         intent(out)   :: err                    ! arguments
      character(len=*),intent(out)   :: msg                    ! arguments
      character(len=80)              :: card                   ! local
      integer                        :: ier                    ! local
      character(len=*),parameter     :: fmt3 = '(a8,4i4)'      ! local
      character(len=*),parameter     :: fmt4 = '(4g14.6,2i7)'  ! local

!  Write first header card.

      write (card, fmt3, iostat=ier) stattype, nhx, nhy, nhx2, nhy2
      if (ier /= 0) then
        err = DIO_ERROR
        msg = 'error encoding first line of old-style CPS static file header'
        return
      endif
      call dio_write_card (dio,card)

!  Write second header card.

      write (card, fmt4, iostat=ier) x1, y1, xinc, yinc, nx, ny
      if (ier /= 0) then
        err = DIO_ERROR
        msg = 'error encoding second line of old-style CPS static file header'
        return
      endif
      call dio_write_card (dio,card)
      call dio_status     (dio,err,msg)
      return
      end subroutine statio1_oldcps_wii


!!------------------------ statio1 oldcps wcc ---------------------------!!
!!------------------------ statio1 oldcps wcc ---------------------------!!
!!------------------------ statio1 oldcps wcc ---------------------------!!

! Write history cards (omitting blank ones). 


      subroutine statio1_oldcps_wcc (dio,hist,nhist,err,msg)
      implicit none
      type(dio_struct),intent(inout) :: dio              ! arguments
      integer,         intent(in)    :: nhist            ! arguments
      character(len=*),intent(in)    :: hist(:)          ! arguments
      integer,         intent(out)   :: err              ! arguments
      character(len=*),intent(out)   :: msg              ! arguments
      integer                        :: i                ! local

      do i = 1,nhist
        if (hist(i) == ' ') cycle
        call dio_write_card (dio,hist(i))
        call dio_status     (dio,err,msg)
        if (err /= DIO_OK) return
      enddo
      return
      end subroutine statio1_oldcps_wcc


!!------------------------ statio1 oldcps write ---------------------------!!
!!------------------------ statio1 oldcps write ---------------------------!!
!!------------------------ statio1 oldcps write ---------------------------!!

! Write static values.


      subroutine statio1_oldcps_write (dio,nx,ny,static,err,msg)
      implicit none
      type(dio_struct),intent(inout)  :: dio                   ! arguments
      integer,         intent(in)     :: nx,ny                 ! arguments
      real,            intent(in)     :: static(:)             ! arguments
      integer,         intent(out)    :: err                   ! arguments
      character(len=*),intent(out)    :: msg                   ! arguments
      character(len=80)               :: card                  ! local
      integer                         :: i,i2,nstatics,ier     ! local
      character(len=*),parameter      :: fmt1 = '(i8,5g14.6)'  ! local

!  Write the file header ENDFLAG.

      call dio_write_card (dio,ENDFLAG)
      call dio_status     (dio,err,msg)
      if (err /= DIO_OK) return

!  Write the static values, 5 per card.
!  Note that all values of static have been packed into 1st dimension.

      nstatics = nx*ny
      do i = 1,nstatics,5
        i2 = min(i+4,nstatics)
        write(card, fmt1, iostat=ier) i, static(i:i2)
        if (ier /= 0) then
          err = DIO_ERROR
          write(msg,*) 'error encoding old style static file value ',i
          return
        endif
        call dio_write_card (dio,card)
        call dio_status     (dio,err,msg)
        if (err /= DIO_OK) return
      enddo
      return
      end subroutine statio1_oldcps_write


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module statio1_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
