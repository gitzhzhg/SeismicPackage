
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- rtcstatio.f90 ------------------------------!!
!!---------------------------- rtcstatio.f90 ------------------------------!!
!!---------------------------- rtcstatio.f90 ------------------------------!!

 
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
!                         C P S   P R I M I T I V E
!
! Name       : RTCSTATIO
! Category   : io
! Written    : 2000-05-01   by: Tom Stoeckley
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-20
! Purpose    : Static file I/O for RTC non-surface-consistent statics.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive reads and writes RTC statics files.  These files differ
! from other statics files in that they are not surface consistent and
! may contain dynamic shifts (more than one shift per trace).
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2000-10-20  Stoeckley  Add missing required documentation section.
!  1. 2000-09-27  Stoeckley  Initial version.
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
!                                    o      i         i       o
!      call rtcstatio_open_read    (obj, filename, lunprint, err)
!      call rtcstatio_open_write   (obj, filename, lunprint, err)
!
!                                    b
!      call rtcstatio_close        (obj)
!
!                                    b  i    o      o     o    o     o    o
!      call rtcstatio_read_record  (obj,hd,ccbest,statms,nwt,ccwin,stwin,err)
!      call rtcstatio_write_record (obj,hd,ccbest,statms,nwt,ccwin,stwin,err)
!                                    b  I    i      i     i    i     i    o
!
!
! type(rtcstatio_struct)      obj = pointer to RTCSTATIO structure.
! character(len=*)       filename = name of the file for input or output.
! integer                lunprint = unit number for printing (or 0).
! integer                     err = error flag = RTCSTATIO_OK if no error.
!                                              = RTCSTATIO_ERROR if error.
!                                              = RTCSTATIO_EOF if end of file.
! double precision          hd(:) = trace header word array.
! real                     ccbest = best correlation coefficient.
! real                     statms = static in milliseconds.
! integer                     nwt = number of windows on trace.
! real                 ccwin(nwt) = corr coef for each window on trace.
! real                 stwin(nwt) = static (ms) for each window on trace.
!
! NOTE: CCWIN and STWIN are used only if there are two or more windows.
!
! If an error occurs, the file is closed, but the data structure is not
! deallocated until rtcstatio_close is called.
!
! If an error occurs on a read, the output arguments (except for ERR) may
! not be set.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module rtcstatio_module
      use named_constants_module
      use sio_module
      implicit none
      private
      public :: rtcstatio_open_read    
      public :: rtcstatio_open_write
      public :: rtcstatio_close
      public :: rtcstatio_read_record  
      public :: rtcstatio_write_record

      character(len=100),public,save :: rtcstatio_IDENT = &
       '$Id: rtcstatio.f90,v 1.2 2000/10/20 13:58:20 sps prod sps $'

      type,public :: rtcstatio_struct
           private
           integer                        :: lun
           character(len=FILENAME_LENGTH) :: pathname
           integer                        :: lunprint
           integer                        :: errcount
      end type rtcstatio_struct

      integer,public,parameter :: RTCSTATIO_OK    = SIO_ZERO
      integer,public,parameter :: RTCSTATIO_ERROR = SIO_ERROR
      integer,public,parameter :: RTCSTATIO_EOF   = SIO_EOF

      contains


!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!

! Prints messages unless the error count is too large.
! If ERR indicates an error:
!   the error count is incremented.
!   if the file is open, it is closed.


      subroutine rtcstatio_msg (obj,err,msg)                   ! private
      implicit none
      type(rtcstatio_struct),intent(inout) :: obj              ! arguments
      integer               ,intent(in)    :: err              ! arguments
      character(len=*)      ,intent(in)    :: msg              ! arguments

      if (obj%lunprint >= 0) then
           if (err /= RTCSTATIO_OK) then
                write (obj%lunprint,*) 'RTCSTATIO: FATAL ERROR'
           end if
           write (obj%lunprint,*) 'RTCSTATIO: ',trim(msg),' RTC STATIC FILE'
           write (obj%lunprint,*) 'RTCSTATIO: PATHNAME = ',trim(obj%pathname)
      end if

      if (err /= RTCSTATIO_OK .and. obj%lun > 0) then
           call sio_close (obj%lun)
           if (obj%lunprint >= 0) then
                write (obj%lunprint,*) 'RTCSTATIO: FILE CLOSED'
           end if
      end if

      if (err /= RTCSTATIO_OK) then
           obj%errcount = obj%errcount + 1
           if (obj%errcount > 20) obj%lunprint = 0
      end if
      return
      end subroutine rtcstatio_msg


!!----------------------------- open read ----------------------------------!!
!!----------------------------- open read ----------------------------------!!
!!----------------------------- open read ----------------------------------!!


      subroutine rtcstatio_open_read (obj,pathname,lunprint,err)
      implicit none
      type(rtcstatio_struct),pointer :: obj                 ! arguments
      character(len=*),intent(in)    :: pathname            ! arguments
      integer         ,intent(in)    :: lunprint            ! arguments
      integer         ,intent(out)   :: err                 ! arguments

      allocate (obj)
      obj%pathname = pathname
      obj%lunprint = lunprint
      obj%errcount = 0

      call sio_open_read (obj%lun, obj%pathname)
      if (obj%lun == SIO_ZERO) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'PATH NOT SPECIFIED FOR INPUT')
      else if (obj%lun == SIO_ERROR) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ERROR OPENING INPUT')
      else
           err = RTCSTATIO_OK
           call rtcstatio_msg (obj,err,'SUCCESSFULLY OPENED INPUT')
      end if
      return
      end subroutine rtcstatio_open_read


!!----------------------------- open write ----------------------------------!!
!!----------------------------- open write ----------------------------------!!
!!----------------------------- open write ----------------------------------!!


      subroutine rtcstatio_open_write (obj,pathname,lunprint,err)
      implicit none
      type(rtcstatio_struct),pointer :: obj                 ! arguments
      character(len=*),intent(in)    :: pathname            ! arguments
      integer         ,intent(in)    :: lunprint            ! arguments
      integer         ,intent(out)   :: err                 ! arguments

      allocate (obj)
      obj%pathname = pathname
      obj%lunprint = lunprint
      obj%errcount = 0

      call sio_open_write (obj%lun, obj%pathname)
      if (obj%lun == SIO_ZERO) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'PATH NOT SPECIFIED FOR OUTPUT')
      else if (obj%lun == SIO_ERROR) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ERROR OPENING OUTPUT')
      else
           err = RTCSTATIO_OK
           call rtcstatio_msg (obj,err,'SUCCESSFULLY OPENED OUTPUT')
      end if
      return
      end subroutine rtcstatio_open_write


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine rtcstatio_close (obj)
      implicit none
      type(rtcstatio_struct),pointer :: obj                 ! arguments

      call sio_close (obj%lun)
      call rtcstatio_msg (obj,RTCSTATIO_OK,'CLOSED')

      deallocate (obj)
      return
      end subroutine rtcstatio_close


!!------------------------- read record ----------------------------------!!
!!------------------------- read record ----------------------------------!!
!!------------------------- read record ----------------------------------!!


      subroutine rtcstatio_read_record &
                                  (obj,hd,ccbest,statms,nwt,ccwin,stwin,err)
      implicit none
      type(rtcstatio_struct),intent(inout) :: obj                 ! arguments
      double precision      ,intent(out)   :: hd(:)               ! arguments
      real                  ,intent(out)   :: ccbest,statms       ! arguments
      integer               ,intent(out)   :: nwt                 ! arguments
      real                  ,intent(out)   :: ccwin(:),stwin(:)   ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      integer                              :: istat,indx,i,i1,i2  ! local
      character(len=100)                   :: card                ! local

      if (obj%lun <= 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ATTEMPT TO READ FROM CLOSED')
           return
      end if

      call sio_read_card (obj%lun, card)
      if (obj%lun <= 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ERROR 1 READING FROM')
           return
      end if

      read (card,5000,iostat=istat) hd(6),hd(7),hd(8),  &
              hd(9),hd(33),hd(34),hd(35),hd(36),        &
              hd(46),hd(47),ccbest,statms,nwt
5000  format (f5.0,2f5.0,f5.0,4f5.0,2f5.0,2f10.3,i5)
      if (istat > 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ERROR 2 READING FROM')
           return
      else if (istat < 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'EOF PREMATURELY REACHED ON')
           return
      end if

      if (nwt > 1) then
           do indx = 1,nwt,8
                call sio_read_card (obj%lun, card)
                if (obj%lun <= 0) then
                     err = RTCSTATIO_ERROR
                     call rtcstatio_msg (obj,err,'ERROR 3 READING FROM')
                     return
                end if
                i1 = indx
                i2 = min(indx+7,nwt)
                read (card,6000,iostat=istat) (ccwin(i),stwin(i),i=i1,i2)
6000            format (8f10.3)
                if (istat /= 0) then
                     err = RTCSTATIO_ERROR
                     call rtcstatio_msg (obj,err,'ERROR 4 READING FROM')
                     return
                end if
           end do
      end if
      err = RTCSTATIO_OK
      return
      end subroutine rtcstatio_read_record


!!------------------------- write record ----------------------------------!!
!!------------------------- write record ----------------------------------!!
!!------------------------- write record ----------------------------------!!


      subroutine rtcstatio_write_record &
                                  (obj,hd,ccbest,statms,nwt,ccwin,stwin,err)
      implicit none
      type(rtcstatio_struct),intent(inout) :: obj                 ! arguments
      double precision      ,intent(in)    :: hd(:)               ! arguments
      real                  ,intent(in)    :: ccbest,statms       ! arguments
      integer               ,intent(in)    :: nwt                 ! arguments
      real                  ,intent(in)    :: ccwin(:),stwin(:)   ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      integer                              :: istat,indx,i,i1,i2  ! local
      character(len=100)                   :: card                ! local

      if (obj%lun <= 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ATTEMPT TO WRITE TO CLOSED')
           return
      end if

      write (card,5000,iostat=istat) nint(hd(6)),hd(7),hd(8),  &
              nint(hd(9)),hd(33),hd(34),hd(35),hd(36),  &
              nint(hd(46)),nint(hd(47)),ccbest,statms,nwt
5000  format (i5,2f5.0,i5,4f5.0,2i5,2f10.3,i5)
      if (istat /= 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ERROR 1 WRITING TO')
           return
      end if

      call sio_write_card (obj%lun, card)
      if (obj%lun <= 0) then
           err = RTCSTATIO_ERROR
           call rtcstatio_msg (obj,err,'ERROR 2 WRITING TO')
           return
      end if

      if (nwt > 1) then
           do indx = 1,nwt,8
                i1 = indx
                i2 = min(indx+7,nwt)
                write (card,6000,iostat=istat) (ccwin(i),stwin(i),i=i1,i2)
6000            format (8f10.3)
                if (istat /= 0) then
                     err = RTCSTATIO_ERROR
                     call rtcstatio_msg (obj,err,'ERROR 3 WRITING TO')
                     return
                end if
                call sio_write_card (obj%lun, card)
                if (obj%lun <= 0) then
                     err = RTCSTATIO_ERROR
                     call rtcstatio_msg (obj,err,'ERROR 4 WRITING TO')
                     return
                end if
           end do
      end if
      err = RTCSTATIO_OK
      return
      end subroutine rtcstatio_write_record


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module rtcstatio_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

