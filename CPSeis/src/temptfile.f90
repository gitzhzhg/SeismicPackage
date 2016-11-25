!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- temptfile.f90 ------------------------------!!
!!---------------------------- temptfile.f90 ------------------------------!!
!!---------------------------- temptfile.f90 ------------------------------!!

 
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
! Name       : TEMPTFILE
! Category   : io
! Written    : 2000-04-19   by: Tom Stoeckley
! Revised    : 2005-01-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Very simple temporary random-access seismic trace file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to open, read, write, and close a temporary
! sequential or random-access file which stores seismic traces.  It is designed
! for efficient use by a process module which needs to store traces or similar
! data temporarily.  It is useful for storing entire traces and headers, or
! for storing small trace windows which might be packed several trace values
! per word.  The traces can be optionally packed and unpacked by this primitive
! when writing and reading.
!
! This primitive uses the CIO, TEMPNAME, and PKUTIL primitives.  A file name
! does not have to be specified.  The name of a non-existent file is created
! from a user-supplied seed and optional directory.  The trace file is
! automatically deleted from the operating system when closed. 
!
! This primitive can actually read and write any data consisting of fixed
! length records.  Each record consists of a header word array and/or a trace
! array.  Normally, for seismic traces, the header word array is type double
! precision and the trace array is type real.  However, both arrays can
! instead be type integer, or the header array can be type integer and the
! trace array can be type real.  When the arrays are integers, they can
! optionally be packed several per word outside this primitive.
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
!    call temptfile_open
!      (obj, seed, nwih, ndpt, lunprint, err, directory, maxrecords, vartypes)
!        o    i     i     i       i       o       i          i          i
!                                                opt        opt        opt
!
!                            b
!    call temptfile_close  (obj)
!    call temptfile_rewind (obj)    ! for sequential reads and writes only.
!
!                                                       PAIR  HD  TR  VARTYPES
!                            b    i     i    i    o
!    call temptfile_write  (obj, irec, hdd, trr, err)     1   dbl real  'DR'
!    call temptfile_read   (obj, irec, hdd, trr, err)     1   dbl real  'DR'
!                            b    i     o    o    o
!
!                            b    i     i    i    o
!    call temptfile_write  (obj, irec, hdi, tri, err)     2   int int   'II'
!    call temptfile_read   (obj, irec, hdi, tri, err)     2   int int   'II'
!                            b    i     o    o    o
!
!                            b    i     i    i    o
!    call temptfile_write  (obj, irec, hdi, trr, err)     3   int real  'IR'
!    call temptfile_read   (obj, irec, hdi, trr, err)     3   int real  'IR'
!                            b    i     o    o    o
!
!                            b    i     i    i    o
!    call temptfile_write8 (obj, irec, hdd, trr, err)     4   dbl real  'DR8'
!    call temptfile_read8  (obj, irec, hdd, trr, err)     4   dbl real  'DR8'
!                            b    i     o    o    o
!
!                            b    i     i    i    i    o
!    call temptfile_write  (obj, irec, hdd, trr, aur, err)  5  dbl rl rl 'DRR'
!    call temptfile_read   (obj, irec, hdd, trr, aur, err)  5  dbl rl rl 'DRR'
!                            b    i     o    o    o    o
!
!
! type(temptfile_struct)   obj = pointer to the TEMPTFILE data structure.
! character(len=*)        seed = base name for creating non-existent filename.
! integer                 nwih = number of words of HDD/HDI to read or write.
! integer                 ndpt = number of words of TRR/TRI to read or write.
! integer             lunprint = unit number for printing (or 0 not to print).
! integer                  err = error flag.
! character(len=*)   directory = directory in which to put the file.
! integer           maxrecords = estimated max number records to read or write.
! character(len=*)    vartypes = which pair of read/write routines to be used.
! integer                 irec = record number (trace number) on disk (>= 1).
!                                 (set irec to zero for sequential access)
! double precision   hdd(nwih) = full-precision header word array.
! real               trr(ndpt) = full-precision trace array.
! integer            hdi(nwih) = integerized or packed header word array.
! integer            tri(ndpt) = integerized or packed trace array.
! real               aur(ndpt) = optional auxiliary array.
!
!
! ERRORS:
!
!   ERR = TEMPTFILE_OK    if no error occurred.
!   ERR = TEMPTFILE_ERROR if an error occurred.
!   ERR = TEMPTFILE_EOF   if an end-of-file was read.
!
!   If an error occurs, the file is closed, but the data structure is not
!   deallocated until TEMPTFILE_CLOSE is called.  If an error occurs on a read,
!   the output arguments (except for ERR) may not be set.
!
! TEMPTFILE_OPEN:
!
!   This routine allocates the data structure and opens the file.
!   Either NWIH or NDPT can be zero, but not both for obvious reasons.
!
!   If DIRECTORY is not specified or blank, the cpstemp directory will be used.
!   This is a special directory on the local node.  If the file is too large
!   to fit on the local disk, parts of the file are distributed to other
!   disks.  Because of NFS mounting, the local disk should be much faster.
!   The cpstemp directory does not have to pre-exist.
!
!   If DIRECTORY is specified and not blank, the filename will be prepended
!   by the specified directory, which can be an absolute or relative path.
!   For example:
!   Setting DIRECTORY to '/tmp' will place the file into the /tmp directory.
!   Setting DIRECTORY to '.' will place the file into the local directory.
!
!   If MAXRECORDS is specified, this number is used to calculate an extent
!   size for the file.  This is important for very large files because there
!   is a limited number of extents available.  Otherwise the default extent
!   size is used.
!
!   If VARTYPES is specified, this is used with the MAXRECORDS argument to
!   calculate the extent size.  The allowed values are shown above.  If
!   VARTYPES is not specified or invalid, 'DR' is used.  There is no other
!   use for the VARTYPES argument.
!
! TEMPTFILE_CLOSE:
!
!   This routine closes the file and deallocates the data structure.
!   If OBJ is not associated, nothing is done.
!
! TEMPTFILE_REWIND:
!
!   This routine needs to be used only when writing and reading sequentially.
!   It should be called when finished writing traces sequentially and before
!   reading them sequentially.
!
! COMMENTS REGARDING THE WRITE/READ ROUTINES:
!
!   Five pairs of read/write subroutines are available.  All reads and writes
!   to any one file should use only one of these five pairs.  They should not
!   be mixed.
!
!   HDD and TRR are type double precision and real, respectively.  This is
!   the normal case for storing seismic traces in their full precision.
!
!   HDI and TRI are both type integer.  This could be the case where the values
!   are not really seismic traces, or are packed (or integerized) traces and
!   headers.  Note that when HDI and TRI contain packed values, the values of
!   NWIH and NDPT correspond to the number of words of memory containing the
!   packed values, and not the number of values themselves.
!
!   For sequential rather than random reads and writes, IREC should be set
!   to zero.  It should be possible to intermix random and sequential reads
!   and writes, although this is probably not recommended because it may be
!   confusing.
!
! TEMPTFILE_WRITE8 and TEMPTFILE_READ8:
!
!   These two routines convert the header array to type real before storing
!   on disk, and back to double precision when read from disk.  The trace
!   array is packed into byte values for storing on disk, and unpacked to
!   its original real values (but with less precision of course) when read
!   from disk.
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
! 12. 2005-01-31  Stoeckley  Add printout to show number of disk reads and
!                             writes performed; add auxiliary array argument.
! 11. 2001-08-27  Stoeckley  Move some code to the new EXTSIZE primitive.
! 10. 2001-05-11  Stoeckley  Add optional MAXRECORDS and VARTYPES arguments.
!  9. 2001-03-15  Stoeckley  Add support for files greater than 2 gigabytes.
!  8. 2001-02-23  Stoeckley  Fix incorrect message when write error occurs.
!  7. 2001-02-13  Stoeckley  Implement use of the CPSTEMP directory.
!  6. 2000-09-27  Stoeckley  Add option for the header array to be type
!                             integer and the trace array to be type real;
!                             fix some incorrect documentation.
!  5. 2000-08-22  Stoeckley  Fix bug when reading and writing integer headers
!                             and traces; add optional argument DIRECTORY to
!                             the open call; add routines TEMPTFILE_WRITE8
!                             and TEMPTFILE_READ8; rewrite some code with
!                             private routines to reduce redundant code and
!                             simplify maintenance.
!  4. 2000-05-22  Stoeckley  Fix problem regarding detecting EOF on sequential
!                             file.
!  3. 2000-05-08  Stoeckley  Add option for sequential reads and writes.
!  2. 2000-04-28  Stoeckley  Changed to use scratch argument to CIO_FOPEN
!                             instead of CIO_UNLINK.
!  1. 2000-04-19  Stoeckley  Initial version.
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


      module temptfile_module
      use named_constants_module
      use tempname_module
      use cio_module
      use sizeof_module
      use extsize_module
      use pkutil_module
      implicit none
      private
      public :: temptfile_open
      public :: temptfile_close
      public :: temptfile_rewind
      public :: temptfile_read
      public :: temptfile_write
      public :: temptfile_read8
      public :: temptfile_write8

      character(len=100),public,save :: temptfile_IDENT = &
'$Id: temptfile.f90,v 1.12 2005/01/31 14:10:39 Stoeckley prod sps $'

      type,public :: temptfile_struct
           private
           integer                        :: lun
           character(len=FILENAME_LENGTH) :: pathname
           integer                        :: nwih,ndpt,ndpt8
           integer                        :: lunprint
           integer                        :: errcount
           integer                        :: irec,nrec
           integer                        :: recsize_dr,recsize_ii,recsize_ir
           integer                        :: recsize_dr8,recsize_drr
           character(len=8)               :: vartypes
           integer                        :: rkount
           integer                        :: wkount
      end type temptfile_struct

      integer,public,parameter  :: TEMPTFILE_OK    = CIO_OK
      integer,public,parameter  :: TEMPTFILE_ERROR = CIO_ERROR
      integer,public,parameter  :: TEMPTFILE_EOF   = CIO_EOF  
      integer,private           :: isize,fsize,dsize

      interface temptfile_read
           module procedure temptfile_read_ii
           module procedure temptfile_read_dr
           module procedure temptfile_read_ir
           module procedure temptfile_read_drr
      end interface

      interface temptfile_write
           module procedure temptfile_write_ii
           module procedure temptfile_write_dr
           module procedure temptfile_write_ir
           module procedure temptfile_write_drr
      end interface

      contains


!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!
!!------------------------------- msg ---------------------------------------!!

! Prints messages unless the error count is too large.
! If ERR indicates an error:
!   the error count is incremented.
!   if the file is open, it is closed.


      subroutine temptfile_msg (obj,err,msg)                     ! private
      implicit none
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: err                ! arguments
      character(len=*)      ,intent(in)    :: msg                ! arguments
      integer                              :: istat,recsize      ! local
      real                                 :: filesize           ! local
      character(len=50)                    :: phrase1,phrase2    ! local
      integer               ,parameter     :: GIGA = 1000000000  ! local

      if (obj%lunprint > 0) then
           select case (obj%vartypes)
                case ('DR' ) ; recsize = obj%recsize_dr
                               phrase1 = '(double precision)'
                               phrase2 = '(real)'
                case ('II' ) ; recsize = obj%recsize_ii
                               phrase1 = '(integer)'
                               phrase2 = '(integer)'
                case ('IR' ) ; recsize = obj%recsize_ir
                               phrase1 = '(integer)'
                               phrase2 = '(real)'
                case ('DR8') ; recsize = obj%recsize_dr8
                               phrase1 = '(double precision stored as reals)'
                               phrase2 = '(real stored as bytes)'
                case ('DRR') ; recsize = obj%recsize_drr
                               phrase1 = '(double precision)'
                               phrase2 = '(real) (real)'
                case default ; recsize = 0
                               phrase1 = ' '
                               phrase2 = ' '
           end select

           if (err == TEMPTFILE_ERROR) write (obj%lunprint,3005)

                            write (obj%lunprint,3000) trim(msg)
                            write (obj%lunprint,3001) trim(obj%pathname)
                            write (obj%lunprint,3002) obj%nwih,trim(phrase1)
                            write (obj%lunprint,3003) obj%ndpt,trim(phrase2)
           if(obj%irec > 0) write (obj%lunprint,3004) obj%irec
3000       format (' TEMPTFILE: ',A,' TEMPORARY TRACE FILE')
3001       format (' TEMPTFILE: PATHNAME          = ',A)
3002       format (' TEMPTFILE: NWIH              = ',I9,1x,A)
3003       format (' TEMPTFILE: NDPT              = ',I9,1x,A)
3004       format (' TEMPTFILE: RECORD NUMBER     = ',I9)
3005       format (' TEMPTFILE: FATAL ERROR')
3006       format (' TEMPTFILE: FILE CLOSED AND DELETED')

           if (recsize > 0) then
                filesize = real(recsize) * real(obj%nrec)
                                 write (obj%lunprint,4002) recsize
                if(obj%nrec > 0) write (obj%lunprint,4003) obj%nrec
                if(obj%nrec > 0) write (obj%lunprint,4005) filesize / GIGA
                if(obj%nrec > 0) write (obj%lunprint,4007) obj%wkount
                if(obj%nrec > 0) write (obj%lunprint,4008) obj%rkount
4002            format (' TEMPTFILE: record size       = ',I9  ,' bytes')
4003            format (' TEMPTFILE: # records on file = ',I9)
4005            format (' TEMPTFILE: file size         = ',F9.4,' gigabytes')
4007            format (' TEMPTFILE: # trace writes    = ',I9)
4008            format (' TEMPTFILE: # trace reads     = ',I9)
           end if
      end if

      if (err == TEMPTFILE_ERROR .and. obj%lun > 0) then
           istat = cio_fclose (obj%lun)
           obj%lun = 0
           if (obj%lunprint > 0) write (obj%lunprint,3006)
      end if

      if (err == TEMPTFILE_ERROR) then
           obj%errcount = obj%errcount + 1
           if (obj%errcount > 20) obj%lunprint = 0
      end if
      return
      end subroutine temptfile_msg


!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!
!!----------------------------- open --------------------------------------!!


      subroutine temptfile_open &
                (obj,seed,nwih,ndpt,lunprint,err,directory,maxrecords,vartypes)
      implicit none
      type(temptfile_struct)   ,pointer     :: obj              ! arguments
      character(len=*)         ,intent(in)  :: seed             ! arguments
      integer                  ,intent(in)  :: nwih,ndpt        ! arguments
      integer                  ,intent(in)  :: lunprint         ! arguments
      integer                  ,intent(out) :: err              ! arguments
      character(len=*),optional,intent(in)  :: directory        ! arguments
      integer         ,optional,intent(in)  :: maxrecords       ! arguments
      character(len=*),optional,intent(in)  :: vartypes         ! arguments
      integer                               :: isize_test       ! local
      real                                  :: fsize_test       ! local
      double precision                      :: dsize_test       ! local
      integer                               :: recsize          ! local

!----------get started.

      isize  = sizeof(isize_test)
      fsize  = sizeof(fsize_test)
      dsize  = sizeof(dsize_test)
 
      allocate (obj)

      obj%lun          = 0
      obj%pathname     = tempname(seed,directory)
      obj%nwih         = nwih   
      obj%ndpt         = ndpt   
      obj%ndpt8        = pkutil_npack8 (obj%ndpt)
      obj%lunprint     = lunprint
      obj%errcount     = 0
      obj%irec         = 0
      obj%nrec         = 0
      obj%recsize_dr   = obj%nwih * dsize + obj%ndpt * fsize
      obj%recsize_ii   = obj%nwih * isize + obj%ndpt * isize
      obj%recsize_ir   = obj%nwih * isize + obj%ndpt * fsize
      obj%recsize_dr8  = obj%nwih * fsize + obj%ndpt * isize
      obj%recsize_drr  = obj%nwih * dsize + obj%ndpt * fsize * 2
      obj%rkount       = 0
      obj%wkount       = 0

      if (present(vartypes)) then
           obj%vartypes = vartypes
      else
           obj%vartypes = ' '
      end if

      if (obj%pathname == ' ') then
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'CANNOT GET NAME FOR')
           return
      end if

      if (obj%vartypes /= 'DR'  .and. obj%vartypes /= 'II'  .and. &
          obj%vartypes /= 'IR'  .and. obj%vartypes /= 'DR8' .and. &
          obj%vartypes /= 'DRR' .and. obj%vartypes /= ' ') then
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'BAD VARTYPES FOR')
           return
      end if

!----------set the file extent size.

      if (present(maxrecords)) then
           select case (obj%vartypes)
                case ('DR' ) ; recsize = obj%recsize_dr
                case ('II' ) ; recsize = obj%recsize_ii
                case ('IR' ) ; recsize = obj%recsize_ir
                case ('DR8') ; recsize = obj%recsize_dr8
                case ('DRR') ; recsize = obj%recsize_drr
                case default ; recsize = obj%recsize_dr ! ; obj%vartypes = 'DR'
           end select
           call extsize (obj%lunprint,maxrecords,recsize,err)
           if (err == TEMPTFILE_ERROR) then
                call temptfile_msg (obj,err,'ERROR SETTING EXTENT SIZE FOR')
                return
           end if
      end if

!----------open the temporary file.

      obj%lun = cio_fopen(obj%pathname, 'w+', scratch=.true.)
      if (obj%lun <= 0) then
           obj%lun = 0
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'ERROR OPENING')
      else
           err = TEMPTFILE_OK
           call temptfile_msg (obj,err,'SUCCESSFULLY OPENED')
      end if
      return
      end subroutine temptfile_open


!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!
!!------------------------------- close -----------------------------------!!


      subroutine temptfile_close (obj)
      implicit none
      type(temptfile_struct),pointer :: obj                 ! arguments
      integer                        :: istat               ! local

      if (.not.associated(obj)) return

      if (obj%lun > 0) then
           istat = cio_fclose (obj%lun)
           call temptfile_msg (obj,TEMPTFILE_OK,'CLOSED AND DELETED')
      end if

      deallocate (obj)
      return
      end subroutine temptfile_close


!!------------------------------ rewind -----------------------------------!!
!!------------------------------ rewind -----------------------------------!!
!!------------------------------ rewind -----------------------------------!!


      subroutine temptfile_rewind (obj)
      implicit none
      type(temptfile_struct),intent(inout) :: obj            ! arguments

      if (obj%lun > 0) then
           call temptfile_msg (obj,TEMPTFILE_OK,'REWOUND')
           call cio_frewind (obj%lun)
           obj%irec = 0
      end if
      return
      end subroutine temptfile_rewind


!!------------------------ before read ---------------------------------!!
!!------------------------ before read ---------------------------------!!
!!------------------------ before read ---------------------------------!!


      subroutine temptfile_before_read (obj,irec,recsize,vt,err)
      implicit none
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      integer               ,intent(in)    :: irec,recsize        ! arguments
      character(len=*)      ,intent(in)    :: vt                  ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      integer                              :: istat               ! local

      if (obj%lun <= 0) then
          err = TEMPTFILE_ERROR
          call temptfile_msg (obj,err,'ATTEMPT TO READ FROM CLOSED AND DELETED')
          return
      end if

      if (obj%vartypes == ' ') then
  !        obj%vartypes = vt
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'ATTEMPT TO READ BEFORE WRITING')
           return
      else if (obj%vartypes /= vt) then
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'MISMATCHING VARIABLE TYPES ('// &
                               trim(obj%vartypes)//' instead of '//vt// &
                               ') WHEN READING')
           return
      end if

      if (irec > 0) then
           obj%irec = irec
  !!!!     istat = cio_fseek (obj%lun, (irec-1) * recsize, 0)   ! <2GB only.
           istat = cio_fseek (obj%lun, recsize, irec-1, 0, 0)
           if (istat /= CIO_OK) then
             err = TEMPTFILE_ERROR
             call temptfile_msg (obj,err,'ERROR SEEKING INPUT RECORD OF')
             return
           end if
      else
           obj%irec = obj%irec + 1
           if (obj%irec > obj%nrec) then
                err = TEMPTFILE_EOF
                call temptfile_msg (obj,err,'EOF READING RECORD FROM')
                return
           end if
      end if

      obj%rkount = obj%rkount + 1
      err = TEMPTFILE_OK
      return
      end subroutine temptfile_before_read


!!------------------------ before write ---------------------------------!!
!!------------------------ before write ---------------------------------!!
!!------------------------ before write ---------------------------------!!


      subroutine temptfile_before_write (obj,irec,recsize,vt,err)
      implicit none
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      integer               ,intent(in)    :: irec,recsize        ! arguments
      character(len=*)      ,intent(in)    :: vt                  ! arguments
      integer               ,intent(out)   :: err                 ! arguments
      integer                              :: istat               ! local

      if (obj%lun <= 0) then
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'ATTEMPT TO WRITE TO CLOSED AND DELETED')
           return
      end if

      if (obj%vartypes == ' ') then
           obj%vartypes = vt
      else if (obj%vartypes /= vt) then
           err = TEMPTFILE_ERROR
           call temptfile_msg (obj,err,'MISMATCHING VARIABLE TYPES ('// &
                               trim(obj%vartypes)//' instead of '//vt// &
                               ') WHEN WRITING')
           return
      end if

      if (irec > 0) then
           obj%irec = irec
 !!!!      istat = cio_fseek (obj%lun, (irec-1) * recsize, 0)   ! <2GB only.
           istat = cio_fseek (obj%lun, recsize, irec-1, 0, 0)
           if (istat /= CIO_OK) then
             err = TEMPTFILE_ERROR
             call temptfile_msg (obj,err,'ERROR SEEKING OUTPUT RECORD OF')
             return
           end if
      else
           obj%irec = obj%irec + 1
      end if

      obj%nrec = max(obj%nrec,obj%irec)
      obj%wkount = obj%wkount + 1
      err = TEMPTFILE_OK
      return
      end subroutine temptfile_before_write


!!------------------------- private read routines --------------------------!!
!!------------------------- private read routines --------------------------!!
!!------------------------- private read routines --------------------------!!


      subroutine temptfile_private_read_i (obj,array,narray,word,err)
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      integer               ,intent(out)   :: array(:)            ! arguments
      integer               ,intent(in)    :: narray              ! arguments
      character(len=*)      ,intent(in)    :: word                ! arguments
      integer               ,intent(inout) :: err                 ! arguments

      if (err /= TEMPTFILE_OK .or. narray == 0) return
      if (cio_fread(array, isize, narray, obj%lun) == narray) return
      err = TEMPTFILE_ERROR
      call temptfile_msg (obj,err,'ERROR READING '//word//' FROM')
      end subroutine temptfile_private_read_i



      subroutine temptfile_private_read_r (obj,array,narray,word,err)
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      real                  ,intent(out)   :: array(:)            ! arguments
      integer               ,intent(in)    :: narray              ! arguments
      character(len=*)      ,intent(in)    :: word                ! arguments
      integer               ,intent(inout) :: err                 ! arguments

      if (err /= TEMPTFILE_OK .or. narray == 0) return
      if (cio_fread(array, fsize, narray, obj%lun) == narray) return
      err = TEMPTFILE_ERROR
      call temptfile_msg (obj,err,'ERROR READING '//word//' FROM')
      end subroutine temptfile_private_read_r



      subroutine temptfile_private_read_d (obj,array,narray,word,err)
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      double precision      ,intent(out)   :: array(:)            ! arguments
      integer               ,intent(in)    :: narray              ! arguments
      character(len=*)      ,intent(in)    :: word                ! arguments
      integer               ,intent(inout) :: err                 ! arguments

      if (err /= TEMPTFILE_OK .or. narray == 0) return
      if (cio_fread(array, dsize, narray, obj%lun) == narray) return
      err = TEMPTFILE_ERROR
      call temptfile_msg (obj,err,'ERROR READING '//word//' FROM')
      end subroutine temptfile_private_read_d


!!------------------------- private write routines --------------------------!!
!!------------------------- private write routines --------------------------!!
!!------------------------- private write routines --------------------------!!


      subroutine temptfile_private_write_i (obj,array,narray,word,err)
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      integer               ,intent(in)    :: array(:)            ! arguments
      integer               ,intent(in)    :: narray              ! arguments
      character(len=*)      ,intent(in)    :: word                ! arguments
      integer               ,intent(inout) :: err                 ! arguments

      if (err /= TEMPTFILE_OK .or. narray == 0) return
      if (cio_fwrite(array, isize, narray, obj%lun) == narray) return
      err = TEMPTFILE_ERROR
      call temptfile_msg (obj,err,'ERROR WRITING '//word//' TO')
      end subroutine temptfile_private_write_i



      subroutine temptfile_private_write_r (obj,array,narray,word,err)
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      real                  ,intent(in)    :: array(:)            ! arguments
      integer               ,intent(in)    :: narray              ! arguments
      character(len=*)      ,intent(in)    :: word                ! arguments
      integer               ,intent(inout) :: err                 ! arguments

      if (err /= TEMPTFILE_OK .or. narray == 0) return
      if (cio_fwrite(array, fsize, narray, obj%lun) == narray) return
      err = TEMPTFILE_ERROR
      call temptfile_msg (obj,err,'ERROR WRITING '//word//' TO')
      end subroutine temptfile_private_write_r



      subroutine temptfile_private_write_d (obj,array,narray,word,err)
      type(temptfile_struct),intent(inout) :: obj                 ! arguments
      double precision      ,intent(in)    :: array(:)            ! arguments
      integer               ,intent(in)    :: narray              ! arguments
      character(len=*)      ,intent(in)    :: word                ! arguments
      integer               ,intent(inout) :: err                 ! arguments

      if (err /= TEMPTFILE_OK .or. narray == 0) return
      if (cio_fwrite(array, dsize, narray, obj%lun) == narray) return
      err = TEMPTFILE_ERROR
      call temptfile_msg (obj,err,'ERROR WRITING '//word//' TO')
      end subroutine temptfile_private_write_d


!!------------------------- public read routines -------------------------!!
!!------------------------- public read routines -------------------------!!
!!------------------------- public read routines -------------------------!!


      subroutine temptfile_read_dr (obj,irec,hdd,trr,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(out)   :: hdd(:)             ! arguments
      real                  ,intent(out)   :: trr(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_read    (obj,irec,obj%recsize_dr,'DR',err)
      call temptfile_private_read_d (obj,hdd,obj%nwih,'HEADERS'  ,err)
      call temptfile_private_read_r (obj,trr,obj%ndpt,'TRACE'    ,err)
      end subroutine temptfile_read_dr



      subroutine temptfile_read_ii (obj,irec,hdi,tri,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      integer               ,intent(out)   :: hdi(:)             ! arguments
      integer               ,intent(out)   :: tri(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_read    (obj,irec,obj%recsize_ii,'II',err)
      call temptfile_private_read_i (obj,hdi,obj%nwih,'HEADERS'  ,err)
      call temptfile_private_read_i (obj,tri,obj%ndpt,'TRACE'    ,err)
      end subroutine temptfile_read_ii



      subroutine temptfile_read_ir (obj,irec,hdi,trr,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      integer               ,intent(out)   :: hdi(:)             ! arguments
      real                  ,intent(out)   :: trr(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_read    (obj,irec,obj%recsize_ir,'IR',err)
      call temptfile_private_read_i (obj,hdi,obj%nwih,'HEADERS'  ,err)
      call temptfile_private_read_r (obj,trr,obj%ndpt,'TRACE'    ,err)
      end subroutine temptfile_read_ir



      subroutine temptfile_read8 (obj,irec,hdd,trr,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(out)   :: hdd(:)             ! arguments
      real                  ,intent(out)   :: trr(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments
      real                                 :: hdr(obj%nwih)      ! local
      integer                              :: tri(obj%ndpt8)     ! local

      call temptfile_before_read    (obj,irec,obj%recsize_dr8,'DR8',err)
      call temptfile_private_read_r (obj,hdr,obj%nwih ,'HEADERS'   ,err)
      call temptfile_private_read_i (obj,tri,obj%ndpt8,'TRACE'     ,err)
      if (err /= TEMPTFILE_OK) return
      hdd(1:obj%nwih) = hdr(:)
      call pkutil_unpack8 (tri,obj%ndpt8,  trr,obj%ndpt)
      end subroutine temptfile_read8



      subroutine temptfile_read_drr (obj,irec,hdd,trr,aur,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(out)   :: hdd(:)             ! arguments
      real                  ,intent(out)   :: trr(:)             ! arguments
      real                  ,intent(out)   :: aur(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_read    (obj,irec,obj%recsize_drr,'DRR',err)
      call temptfile_private_read_d (obj,hdd,obj%nwih,'HEADERS'    ,err)
      call temptfile_private_read_r (obj,trr,obj%ndpt,'TRACE'      ,err)
      call temptfile_private_read_r (obj,aur,obj%ndpt,'AUX'        ,err)
      end subroutine temptfile_read_drr


!!------------------------- public write routines -------------------------!!
!!------------------------- public write routines -------------------------!!
!!------------------------- public write routines -------------------------!!


      subroutine temptfile_write_dr (obj,irec,hdd,trr,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(in)    :: hdd(:)             ! arguments
      real                  ,intent(in)    :: trr(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_write    (obj,irec,obj%recsize_dr,'DR',err)
      call temptfile_private_write_d (obj,hdd,obj%nwih,'HEADERS'  ,err)
      call temptfile_private_write_r (obj,trr,obj%ndpt,'TRACE'    ,err)
      end subroutine temptfile_write_dr



      subroutine temptfile_write_ii (obj,irec,hdi,tri,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      integer               ,intent(in)    :: hdi(:)             ! arguments
      integer               ,intent(in)    :: tri(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_write    (obj,irec,obj%recsize_ii,'II',err)
      call temptfile_private_write_i (obj,hdi,obj%nwih,'HEADERS'  ,err)
      call temptfile_private_write_i (obj,tri,obj%ndpt,'TRACE'    ,err)
      end subroutine temptfile_write_ii



      subroutine temptfile_write_ir (obj,irec,hdi,trr,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      integer               ,intent(in)    :: hdi(:)             ! arguments
      real                  ,intent(in)    :: trr(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_write    (obj,irec,obj%recsize_ir,'IR',err)
      call temptfile_private_write_i (obj,hdi,obj%nwih,'HEADERS'  ,err)
      call temptfile_private_write_r (obj,trr,obj%ndpt,'TRACE'    ,err)
      end subroutine temptfile_write_ir



      subroutine temptfile_write8 (obj,irec,hdd,trr,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(in)    :: hdd(:)             ! arguments
      real                  ,intent(in)    :: trr(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments
      real                                 :: hdr(obj%nwih)      ! local
      integer                              :: tri(obj%ndpt8)     ! local

      hdr(:) = hdd(1:obj%nwih)
      call pkutil_pack8 (trr,obj%ndpt,  tri,obj%ndpt8)
      call temptfile_before_write    (obj,irec,obj%recsize_dr8,'DR8',err)
      call temptfile_private_write_r (obj,hdr,obj%nwih ,'HEADERS'   ,err)
      call temptfile_private_write_i (obj,tri,obj%ndpt8,'TRACE'     ,err)
      end subroutine temptfile_write8



      subroutine temptfile_write_drr (obj,irec,hdd,trr,aur,err)
      type(temptfile_struct),intent(inout) :: obj                ! arguments
      integer               ,intent(in)    :: irec               ! arguments
      double precision      ,intent(in)    :: hdd(:)             ! arguments
      real                  ,intent(in)    :: trr(:)             ! arguments
      real                  ,intent(in)    :: aur(:)             ! arguments
      integer               ,intent(out)   :: err                ! arguments

      call temptfile_before_write    (obj,irec,obj%recsize_drr,'DRR',err)
      call temptfile_private_write_d (obj,hdd,obj%nwih,'HEADERS'    ,err)
      call temptfile_private_write_r (obj,trr,obj%ndpt,'TRACE'      ,err)
      call temptfile_private_write_r (obj,aur,obj%ndpt,'AUX'        ,err)
      end subroutine temptfile_write_drr


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module temptfile_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

