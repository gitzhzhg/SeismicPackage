
!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- flexio.f90 ------------------------------!!
!!---------------------------- flexio.f90 ------------------------------!!
!!---------------------------- flexio.f90 ------------------------------!!

 
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
! Name       : FLEXIO
! Category   : io
! Written    : 2000-05-17   by: Tom Stoeckley
! Revised    : 2001-08-30   by: Tom Stoeckley
! Maturity   : production   2001-10-18
! Purpose    : Read and write a file containing flex-binning information.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to open, read, write, and close a binary file
! containing flex-binning information used by the FLEX process module.
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
!                               o      i         i         o      o
!    call flexio_open_new     (obj, pathname, lunprint,          err)
!    call flexio_open_old     (obj, pathname, lunprint, ntraces, err)
!    call flexio_open_append  (obj, pathname, lunprint, ntraces, err)
!
!                               b      i       i   
!    call flexio_close_new    (obj, ntraces,       )
!    call flexio_close_old    (obj,                )
!    call flexio_close_append (obj, ntraces, ninput)
!
!                               b     i      i   i    i    i    i    i     o
!    call flexio_write_new    (obj, itrace, hd9,hd10,hd11,hd12,hd14,hd15, err)
!    call flexio_read_old     (obj, itrace, hd9,hd10,hd11,hd12,hd14,hd15, err)
!                               b     i      o   o    o    o    o    o     o
!
! type(flexio_struct)      obj = pointer to the FLEXIO structure.
! character(len=*)    pathname = name of flexfile.
! integer             lunprint = unit number for printing (or 0).
! integer              ntraces = total number of traces written to file.
! integer               ninput = total number of new traces appended to file.
! integer                  err = FLEXIO_ERROR (if error) or FLEXIO_OK.
! integer               itrace = trace number (record number) on disk (>= 1).
! double precision hd9,hd10,hd11,hd12,hd14,hd15 = header words to read or write.
!
! Use flexio_write_new to append new trace to file.
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


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2001-10-18  Stoeckley  Fix to work for files greater than 2 gigabytes.
!  2. 2000-10-06  Stoeckley  Add required missing documentation section.
!  1. 2000-05-17  Stoeckley  Initial version.
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


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module flexio_module
      use named_constants_module
      use sizeof_module
      use swap_module
      use cio_module
      use string_module
      implicit none
      private
      public :: flexio_open_new 
      public :: flexio_open_old  
      public :: flexio_open_append
      public :: flexio_close_new  
      public :: flexio_close_old   
      public :: flexio_close_append 
      public :: flexio_write_new    
      public :: flexio_read_old     

      character(len=100),public,save :: flexio_IDENT = &
       '$Id: flexio.f90,v 1.3 2001/10/17 20:00:55 Stoeckley prod sps $'

      type,public :: flexio_struct
           private
           character(len=FILENAME_LENGTH) :: pathname
           integer                        :: lunprint
           integer                        :: lun
      end type flexio_struct

      integer,public ,parameter :: FLEXIO_OK    = CIO_OK
      integer,public ,parameter :: FLEXIO_ERROR = CIO_ERROR
      integer,private,parameter :: NWORDS       = 6
      integer,private,save      :: ISIZE
      integer,private,save      :: RECSIZE
      logical,private,save      :: ENDIAN


      contains


!!------------------------------- private ----------------------------------!!
!!------------------------------- private ----------------------------------!!
!!------------------------------- private ----------------------------------!!

      subroutine flexio_private_startup
      implicit none
      integer :: isize_test

      ISIZE   = sizeof(isize_test)
 !!!  ISIZE   = sizeof(1)               ! segfaults with Absoft compiler
                                        ! unless surrounded by print messages.
      RECSIZE = NWORDS * ISIZE
      ENDIAN  = (swap_endian() == 1)
      return
      end subroutine flexio_private_startup




      subroutine flexio_private_seek (obj, itrace, err)
      implicit none
      type(flexio_struct),intent(inout) :: obj             ! arguments
      integer            ,intent(in)    :: itrace          ! arguments
      integer            ,intent(out)   :: err             ! arguments
      integer                           :: istat           ! local

 !!!! istat = cio_fseek (obj%lun, itrace * RECSIZE, 0)     ! <2GB only.
      istat = cio_fseek (obj%lun, RECSIZE, itrace, 0, 0)
      if (istat /= CIO_OK) then
           err = FLEXIO_ERROR
           if (itrace > 0) then
             call flexio_msg (obj,err,'ERROR SEEKING TRACE LOCATION',itrace)
           else
             call flexio_msg (obj,err,'ERROR SEEKING BEGINNING OF FLEXFILE')
           end if
      else
           err = FLEXIO_OK
      end if
      return
      end subroutine flexio_private_seek




      subroutine flexio_private_read (obj, itrace, array, err)
      implicit none
      type(flexio_struct),intent(inout) :: obj             ! arguments
      integer            ,intent(in)    :: itrace          ! arguments
      integer            ,intent(out)   :: array(NWORDS)   ! arguments
      integer            ,intent(out)   :: err             ! arguments
      integer                           :: istat           ! local

      call flexio_private_seek (obj, itrace, err)
      if (err == FLEXIO_ERROR) return

      istat = cio_fread(array, ISIZE, NWORDS, obj%lun)
      if (istat == NWORDS) then
           if (ENDIAN) call swap_bytes (array(1:NWORDS))
           err = FLEXIO_OK
      else
           err = FLEXIO_ERROR
           if (itrace > 0) then
             call flexio_msg (obj,err,'ERROR READING TRACE',itrace)
           else
             call flexio_msg (obj,err,'ERROR READING BEGINNING OF FLEXFILE')
           end if
      end if
      return
      end subroutine flexio_private_read



!!! Note that ARRAY is intent(inout) in this call:

      subroutine flexio_private_write (obj, itrace, array, err)
      implicit none
      type(flexio_struct),intent(inout) :: obj             ! arguments
      integer            ,intent(in)    :: itrace          ! arguments
      integer            ,intent(inout) :: array(NWORDS)   ! arguments
      integer            ,intent(out)   :: err             ! arguments
      integer                           :: istat           ! local

      call flexio_private_seek (obj, itrace, err)
      if (err == FLEXIO_ERROR) return

      if (ENDIAN) call swap_bytes (array(1:NWORDS))
      istat = cio_fwrite(array, ISIZE, NWORDS, obj%lun)
      if (istat == NWORDS) then
           err = FLEXIO_OK
      else
           err = FLEXIO_ERROR
           if (itrace > 0) then
             call flexio_msg (obj,err,'ERROR WRITING TRACE',itrace)
           else
             call flexio_msg (obj,err,'ERROR WRITING BEGINNING OF FLEXFILE')
           end if
      end if
      return
      end subroutine flexio_private_write


!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!

! Prints messages.
! If ERR indicates an error and the file is open, it is closed.


      subroutine flexio_msg (obj,err,msg,itrace)            ! private
      implicit none
      type(flexio_struct),intent(inout) :: obj              ! arguments
      integer            ,intent(in)    :: err              ! arguments
      character(len=*)   ,intent(in)    :: msg              ! arguments
      integer,optional   ,intent(in)    :: itrace           ! arguments
      integer                           :: istat            ! local

      if (obj%lunprint > 0) then
           if (err == FLEXIO_ERROR) then
                write (obj%lunprint,*) 'FLEXIO: FLEXFILE FATAL ERROR'
           end if
           write (obj%lunprint,*) 'FLEXIO: ',trim(msg)
           write (obj%lunprint,*) 'FLEXIO: PATHNAME = ',trim(obj%pathname)
           if(present(itrace)) then
           if(itrace > 0) then
                write (obj%lunprint,*) 'FLEXIO: TRACE NUMBER = ',itrace
           end if
           end if
      end if

      if (err == FLEXIO_ERROR .and. obj%lun > 0) then
           istat = cio_fclose (obj%lun)
           obj%lun = 0
           if (obj%lunprint > 0) then
                write (obj%lunprint,*) 'FLEXIO: FLEXFILE CLOSED'
           end if
      end if
      return
      end subroutine flexio_msg


!!--------------------- old flexfile i/o ------------------------------!!
!!--------------------- old flexfile i/o ------------------------------!!
!!--------------------- old flexfile i/o ------------------------------!!


      subroutine flexio_open_old (obj, pathname, lunprint, ntraces, err)
      implicit none
      type(flexio_struct),pointer    :: obj                 ! arguments
      character(len=*),intent(in)    :: pathname            ! arguments
      integer         ,intent(in)    :: lunprint            ! arguments
      integer         ,intent(out)   :: ntraces             ! arguments
      integer         ,intent(out)   :: err                 ! arguments
      integer                        :: array(NWORDS)       ! arguments
      character(len=80)              :: msg                 ! local

      call flexio_private_startup

      allocate (obj)

      obj%pathname = pathname
      obj%lunprint = lunprint

      obj%lun = cio_fopen(obj%pathname, 'r+')
      if (obj%lun <= 0) then
           obj%lun = 0
           err = FLEXIO_ERROR
           call flexio_msg (obj,err,'ERROR OPENING OLD FLEXFILE')
           return
      end if

      call flexio_msg (obj,err,'SUCCESSFULLY OPENED OLD FLEXFILE')

      call flexio_private_read (obj, 0, array, err)
      if (err == FLEXIO_ERROR) return

      ntraces = array(1)

      call string_encode (msg, 'THERE ARE',ntraces,'TRACES ON OLD FLEXFILE')
      call flexio_msg    (obj,err,msg)
      return
      end subroutine flexio_open_old




      subroutine flexio_close_old (obj)
      implicit none
      type(flexio_struct),pointer :: obj                 ! arguments
      integer                     :: istat,err           ! local

      if (.not.associated(obj)) return

      if (obj%lun > 0) then
           istat = cio_fclose (obj%lun)
           if (istat == 0) then
                err = FLEXIO_OK
           else
                err = FLEXIO_ERROR
           end if
           call flexio_msg (obj,err,'OLD FLEXFILE CLOSED')
      end if

      deallocate (obj)
      return
      end subroutine flexio_close_old




      subroutine flexio_read_old (obj,itrace,               &
                                  hd9,hd10,hd11,hd12,hd14,hd15,err)
      implicit none
      type(flexio_struct),intent(inout) :: obj                      ! arguments
      integer         ,intent(in)  :: itrace                        ! arguments
      double precision,intent(out) :: hd9,hd10,hd11,hd12,hd14,hd15  ! arguments
      integer         ,intent(out) :: err                           ! arguments
      integer                      :: array(NWORDS)                 ! local

      call flexio_private_read (obj, itrace, array, err)
      if (err == FLEXIO_ERROR) return

      hd9  = array(1)
      hd10 = array(2)
      hd11 = array(3)
      hd12 = array(4)
      hd14 = array(5)
      hd15 = array(6)
      return
      end subroutine flexio_read_old


!!--------------------- new flexfile i/o -------------------------------!!
!!--------------------- new flexfile i/o -------------------------------!!
!!--------------------- new flexfile i/o -------------------------------!!


      subroutine flexio_open_new (obj, pathname, lunprint, err)
      implicit none
      type(flexio_struct),pointer    :: obj                 ! arguments
      character(len=*),intent(in)    :: pathname            ! arguments
      integer         ,intent(in)    :: lunprint            ! arguments
      integer         ,intent(out)   :: err                 ! arguments
      integer                        :: array(NWORDS)       ! arguments

      call flexio_private_startup

      allocate (obj)

      obj%pathname = pathname
      obj%lunprint = lunprint

      obj%lun = cio_fopen(obj%pathname, 'w+')
      if (obj%lun <= 0) then
           obj%lun = 0
           err = FLEXIO_ERROR
           call flexio_msg (obj,err,'ERROR OPENING NEW FLEXFILE')
           return
      end if

      call flexio_msg (obj,err,'SUCCESSFULLY OPENED NEW FLEXFILE')

      array(:) = 0
      call flexio_private_write (obj, 0, array, err)
      return
      end subroutine flexio_open_new



      subroutine flexio_close_new (obj,ntraces)
      implicit none
      type(flexio_struct),pointer    :: obj                 ! arguments
      integer            ,intent(in) :: ntraces             ! arguments
      character(len=80)              :: msg                 ! local
      integer                        :: istat,err           ! local
      integer                        :: array(NWORDS)       ! local

      if (.not.associated(obj)) return

      if (obj%lun > 0) then
           array(:) = ntraces
           call flexio_private_write (obj, 0, array, err)
           if (err == FLEXIO_OK) then
             call string_encode (msg,'',ntraces,'TRACES SAVED TO NEW FLEXFILE')
             call flexio_msg    (obj,err,msg)
           end if
           istat = cio_fclose (obj%lun)
           if (istat == 0) then
                continue          ! do not reset err.
           else
                err = FLEXIO_ERROR
           end if
           call flexio_msg (obj,err,'NEW FLEXFILE CLOSED')
      end if

      deallocate (obj)
      return
      end subroutine flexio_close_new




      subroutine flexio_write_new (obj,itrace,               &
                                   hd9,hd10,hd11,hd12,hd14,hd15,err)
      implicit none
      type(flexio_struct),intent(inout) :: obj                      ! arguments
      integer         ,intent(in)  :: itrace                        ! arguments
      double precision,intent(in)  :: hd9,hd10,hd11,hd12,hd14,hd15  ! arguments
      integer         ,intent(out) :: err                           ! arguments
      integer                      :: array(NWORDS)                 ! local

      array(1) = nint(hd9 )
      array(2) = nint(hd10)
      array(3) = nint(hd11)
      array(4) = nint(hd12)
      array(5) = nint(hd14)
      array(6) = nint(hd15)

      call flexio_private_write (obj, itrace, array, err)
      return
      end subroutine flexio_write_new


!!---------------------- append flexfile i/o ---------------------------!!
!!---------------------- append flexfile i/o ---------------------------!!
!!---------------------- append flexfile i/o ---------------------------!!


      subroutine flexio_open_append (obj, pathname, lunprint, ntraces, err)
      implicit none
      type(flexio_struct),pointer  :: obj                 ! arguments
      character(len=*),intent(in)  :: pathname            ! arguments
      integer         ,intent(in)  :: lunprint            ! arguments
      integer         ,intent(out) :: ntraces             ! arguments
      integer         ,intent(out) :: err                 ! arguments

      call flexio_open_old (obj,pathname,lunprint,ntraces,err)
      call flexio_msg      (obj,err,'WE WILL APPEND TO OLD FLEXFILE')
      return
      end subroutine flexio_open_append




      subroutine flexio_close_append (obj,ntraces,ninput)
      implicit none
      type(flexio_struct),pointer :: obj                 ! arguments
      integer         ,intent(in) :: ntraces,ninput      ! arguments
      character(len=80)           :: msg                 ! local

      if (.not.associated(obj)) return

      call string_encode    (msg,'',ninput,'TRACES APPENDED TO OLD FLEXFILE')
      call flexio_msg       (obj,FLEXIO_OK,msg)
      call flexio_close_new (obj,ntraces)
      return
      end subroutine flexio_close_append


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module flexio_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

