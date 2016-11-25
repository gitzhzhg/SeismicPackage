
!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- dio.f90 --------------------------------!!
!!----------------------------- dio.f90 --------------------------------!!
!!----------------------------- dio.f90 --------------------------------!!


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
! Name       : DIO            (disk I/O)
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : To read and write sequential ascii or binary files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION               
!
! This primitive contains support for reading and writing sequential ascii or
! binary files.  
!
! This support simply consists of a thin wrapper around the CIO primitive,
! allowing CIO to open, close, rewind, and backspace files, and read and write
! individual ascii card images or binary records more easily than using the
! CIO primitive directly.
!
! When reading or writing ascii card images, the module using this primitive
! must decode and encode the card images.
!
! When reading or writing binary data, this primitive does byte swapping
! when necessary so that the file can be read or written on either a big
! endian or little endian machine.
!
! This primitive also simplifies error handling by allowing a set of calls
! to be made to do several reads and writes, and allowing a delay of a test
! for errors until the entire set of calls is finished.
!
!-------------------------------------------------------------------------------
!                        OTHER RELATED PRIMITIVES
!
! This primitive has all the capabilities of the OLDSTYLE and SIO primitives,
! which also are thin wrappers around the CIO primitive, but differ from the
! OLDSTYLE and SIO primitives in the following ways:
!
!  (1) It uses a data structure rather than just a logical unit number.
!  (2) It does not close the file when an error occurs or an EOF is encountered.
!  (3) It returns error flags and messages (unlike the SIO primitive).
!  (4) It accesses both ascii and binary data (unlike the OLDSTYLE primitive).
!  (5) It calls FINQUIRE before opening the file (unlike the SIO primitive).
!  (6) Binary strings and arrays are written with the character or element
!       count so that they can be properly read without knowing the count
!       ahead of time.
!  (7) It contains a number of combination convenience routines.
!
! To fetch files from remote nodes, or dispose files to remote nodes, you
! should use the RCPFILE primitive or fetch the file yourself using RCP or
! FTP.  Normally this is not required since all our nodes are connected
! using NFS mounting.
!
! To inquire about the status of an existing or non-existing file, including
! read/write permissions, you can use the FINQUIRE primitive (which is also
! used by DIO).
!
! To test the syntax of a file name, or to assemble or disassemble the parts
! of a file name, you should use the PATH or PATHCHECK primitive.
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
! Open, close, rewind, and backspace the file:
!
!                              o      i       o    o
!      call dio_open_read    (obj, filename, err, msg)
!      call dio_open_write   (obj, filename, err, msg)
!
!                              b 
!      call dio_close        (obj)
!
!      call dio_rewind       (obj)
!      call dio_backspace    (obj)     ! for ascii card images only.
!                              i
!
! Tell or seek current file position:
!
!                             b      o
!      call dio_tell        (obj, position)
!      call dio_seek        (obj, position)
!                             b      i
!
! Read and write an ascii card image:
!  (if nchar is present, writes card(1:nchar) followed by a newline)
!  (if nchar is absent, writes trim(card) followed by a newline)
!  (using nchar is useful when rewriting after tell and seek)
!
!                                  b    o 
!      call dio_read_card        (obj, card)           ! single card image.
!      call dio_write_card       (obj, card, nchar)    ! single card image.
!                                  b    i      i
!                                             opt
!
! Read and write a single character:
!
!                                  b     o 
!      call dio_read_character   (obj, string)   ! single character only.
!      call dio_write_character  (obj, string)   ! single character only.
!                                  b     i  
!
!                                  b
!      call dio_read_newline     (obj)           ! any single character.
!      call dio_write_newline    (obj)           ! single newline character.
!                                  b 
!
! Read and write a binary numerical scalar value:
!
!                                  b     o 
!      call dio_read_scalar      (obj, scalar)
!      call dio_write_scalar     (obj, scalar)
!                                  b     i  
!
! Read and write a binary character string:
!  (if nchar is present, writes and reads string(1:nchar))
!  (if nchar is absent, writes and reads len_trim(string) and trim(string))
!
!                                               opt
!                                  b     o       i
!      call dio_read_string      (obj, string, nchar)
!      call dio_write_string     (obj, string, nchar)
!                                  b     i       i
!                                               opt
!
! Read and write a binary numerical array:
!  (the first set of routines writes and reads array(1:nelements) only)
!  (the second set of routines writes and reads nelements also)
!
!                                  b     o        i   
!      call dio_read_array       (obj, array, nelements)
!      call dio_write_array      (obj, array, nelements)
!                                  b     i        i      
!                                                           opt
!                                  b     o        o          i
!      call dio_read_array_plus  (obj, array, nelements, nexpected)
!      call dio_write_array_plus (obj, array, nelements)
!                                  b     i        i      
!
! Read and write a binary character array:
!  (the first set of routines writes and reads strings(1:nelements) only)
!  (the second set of routines writes and reads nelements also)
!  (if nchar is present, writes and reads string(1:nchar))
!  (if nchar is absent, writes and reads len_trim(string) and trim(string))
!
!                                                             opt
!                                    b      o         i        i
!      call dio_read_strings       (obj, strings, nelements, nchar)
!      call dio_write_strings      (obj, strings, nelements, nchar)
!                                    b      i         i        i
!                                                             opt
!
!                                                               opt      opt
!                                    b      o         o          i        i
!      call dio_read_strings_plus  (obj, strings, nelements, nexpected, nchar)
!      call dio_write_strings_plus (obj, strings, nelements,            nchar)
!                                    b      i         i                   i
!                                                                        opt
!
! Get the current status (at any time):
!
!                        b    o    o    i
!      call dio_status (obj, err, msg, noun)
!      call dio_status (obj, err, msg)
!      call dio_status (obj, err)
!      call dio_status (obj, msg)
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS               
!
! type(dio_struct) obj   = pointer to the DIO data structure.
! char(*)    filename    = name of the file for input or output.
! integer    err         = error flag (returned).
! char(*)    msg         = message for possible printing (returned).
! char(*)    noun        = noun phrase to append to error message.
! integer    position(2) = current file position (works even for files > 2GB).
! char(*)    card        = a single card image             to read or write.
! char(*)    string      = a single character string       to read or write.
! (any type) scalar      = a single scalar numerical value to read or write.
! (any type) array(:)    = an array of numerical values    to read or write.
! char(*)    strings(:)  = an array of character strings   to read or write.
! integer    nchar       = number of characters in string  to read or write.
! integer    nelements   = number of elements in array     to read or write.
! integer    nexpected   = expected number of elements in array to read.
!
! The POSITION array and the public parameter DIO_BLOCKSIZE define the
! file position in bytes to be DIO_BLOCKSIZE * POSITION(1) + POSITION(2).
! POSITION(1) is also called "whichblock" (zero at the beginning of the file).
! POSITION(2) is also called "whichbyte"  (zero at the beginning of the file).
!
! CARD should not contain a null termination in a WRITE call.
! CARD will not contain a null termination in a READ call.
!
! SCALAR and ARRAY can be type integer, real, double precision, or logical.
!
!-------------------------------------------------------------------------------
!                           ERRORS AND ENDFILES
!
! ERR will always have one of the following values:
!  (1) DIO_OK      (zero)   if the operation was successful.
!  (2) DIO_ERROR (negative) if an open or read/write error occurred.
!  (3) DIO_EOF   (negative) if an end-of-file was encountered.
!
! DIO_OK and DIO_ERROR and DIO_EOF have the same values as the corresponding
! error flags in the CIO primitive.
!
! MSG will contain an error or EOF message, or simply 'ok' otherwise.
! If an EOF was encountered, MSG will start with 'EOF'.
!
! Only the first error (or EOF) which occurs will generate an error message.
! Subsequent returned values of ERR and MSG will then be set according to
! this first error.
!
! Note that several calls to read or write card images or binary records
! can be made back-to-back without testing for an error until after the last
! call of the set has been made.  This works because, after an error occurs
! with one of the calls, any subsequent calls will know to do nothing, and
! the original error message and error status can be obtained at any time.
!
! If an error or EOF occurs on a current or previous read:
!  (1) the subroutine output arguments CARD, STRING, SCALAR, or NELEMENTS
!       will be set to blank or zero or false.
!  (2) the subroutine output arguments STRINGS or ARRAY will be filled with
!       nils up to the NEXPECTED number of elements if NEXPECTED is present,
!       or up to the NELEMENTS number of elements if NELEMENTS is an input
!       argument.
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE OPERATIONS              
!
! Calls to open a file:
!  (1) The data structure is allocated.
!  (2) The file is opened.
!  (3) If the open fails: file is closed and the data structure is deallocated.
!  (4) If the open fails: no other routine can be called except dio_close.
!
! Calls to read or write a card image or a binary record:
!  (1) The operation is performed if no previous error or EOF had occurred.
!  (2) Nothing is done if a previous error or EOF had occurred.
!
! Calls to backspace or rewind the file:
!  (1) The operation is always performed.
!  (2) A previous error or EOF flag is cleared.
!
! Calls to close the file:
!  (1) The file is closed (only if it is currently open).
!  (2) The data structure is deallocated (only if it is currently allocated).
!  (3) If the open had failed, this routine will do nothing.
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
!005. 2006-04-25  B. Menger   Removed Unused Variables.
!  4. 2002-02-04  Stoeckley  Improve the flexibility in getting error and
!                             status information; change local variable name
!                             STAT to NREAD or NWRITE for clarity.
!  3. 2001-08-27  Stoeckley  Fix bug calling cio_ftell for files < 2 gigabytes;
!                             add code to fill output array with nils when
!                             error occurs in the _PLUS routines; improve
!                             error messages; disable preallocating file space
!                             and set extent size to 2GB when writing a file.
!  2. 2001-03-21  Stoeckley  Add support for files greater than 2 gigabytes.
!  1. 2000-11-27  Stoeckley  Initial version.
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
 
 
      module dio_module
      use cio_module
      use finquire_module
      use string_module
      use sizeof_module
      use swap_module
      implicit none
      public
      private :: dio_private_create
      private :: dio_private_whoops


      character(len=100),public,save :: DIO_IDENT = &
       '$Id: dio.f90,v 1.5 2006/04/25 12:00:54 Menger prod sps $'

      integer,public ,parameter :: DIO_ERROR     = CIO_ERROR
      integer,public ,parameter :: DIO_EOF       = CIO_EOF
      integer,public ,parameter :: DIO_OK        = CIO_OK
      integer,public ,parameter :: DIO_BLOCKSIZE = 2000000000
      integer,public ,parameter :: DIO_EXTSIZE   = 2000000000

      integer,private           :: ISIZE,FSIZE,DSIZE,LSIZE
      logical,private           :: ENDIAN

      type,public :: dio_struct
           private
           integer           :: lun       ! logical unit number.
           integer           :: err       ! error flag.
           character(len=80) :: msg       ! error message.
           logical           :: whoops    ! true if ERR = DIO_ERROR or DIO_EOF.
      end type dio_struct


!!--------------------------- interfaces ----------------------------------!!
!!--------------------------- interfaces ----------------------------------!!
!!--------------------------- interfaces ----------------------------------!!


      interface dio_read_scalar
           module procedure dio_read_scalar_ivar
           module procedure dio_read_scalar_fvar
           module procedure dio_read_scalar_dvar
           module procedure dio_read_scalar_lvar
      end interface

      interface dio_write_scalar
           module procedure dio_write_scalar_ivar
           module procedure dio_write_scalar_fvar
           module procedure dio_write_scalar_dvar
           module procedure dio_write_scalar_lvar
      end interface

      interface dio_read_array
           module procedure dio_read_array_ivars
           module procedure dio_read_array_fvars
           module procedure dio_read_array_dvars
           module procedure dio_read_array_lvars
      end interface

      interface dio_write_array
           module procedure dio_write_array_ivars
           module procedure dio_write_array_fvars
           module procedure dio_write_array_dvars
           module procedure dio_write_array_lvars
      end interface

      interface dio_read_array_plus
           module procedure dio_read_array_iplus
           module procedure dio_read_array_fplus
           module procedure dio_read_array_dplus
           module procedure dio_read_array_lplus
      end interface

      interface dio_write_array_plus
           module procedure dio_write_array_iplus
           module procedure dio_write_array_fplus
           module procedure dio_write_array_dplus
           module procedure dio_write_array_lplus
      end interface

      interface dio_status
           module procedure dio_status_err
           module procedure dio_status_msg
           module procedure dio_status_both
           module procedure dio_status_noun
      end interface

      contains


!!---------------------------- private create ------------------------------!!
!!---------------------------- private create ------------------------------!!
!!---------------------------- private create ------------------------------!!


      subroutine dio_private_create (obj,lun)
      implicit none
      type(dio_struct),pointer       :: obj               ! arguments
      integer         ,intent(in)    :: lun               ! arguments
      integer                        :: isize_test        ! local
      real                           :: fsize_test        ! local
      double precision               :: dsize_test        ! local
      logical                        :: lsize_test        ! local

      ISIZE    = sizeof(isize_test)
      FSIZE    = sizeof(fsize_test)
      DSIZE    = sizeof(dsize_test)
      LSIZE    = sizeof(lsize_test)
      ENDIAN   = (swap_endian() == 1)

      allocate (obj)

      obj%lun    = lun
      obj%err    = DIO_OK
      obj%msg    = 'ok'
      obj%whoops = .false.
      return
      end subroutine dio_private_create


!!----------------------------- private whoops ---------------------------!!
!!----------------------------- private whoops ---------------------------!!
!!----------------------------- private whoops ---------------------------!!
 
! optional arguments should be omitted only from right to left.

 
      subroutine dio_private_whoops (obj,msg,n1,w1,n2,w2,n3)
      implicit none
      type(dio_struct),intent(inout)        :: obj           ! arguments
      character(len=*),intent(in)           :: msg           ! arguments
      integer         ,intent(in),optional  :: n1,n2,n3      ! arguments
      character(len=*),intent(in),optional  :: w1,w2         ! arguments

      if (obj%whoops) return

      obj%err    = DIO_ERROR
      obj%msg    = msg
      obj%whoops = .true.

      if (present(n1)) obj%msg = trim(obj%msg) // ' (' //string_ii2ss(n1)
      if (present(w1)) obj%msg = trim(obj%msg) // ' '  //w1
      if (present(n2)) obj%msg = trim(obj%msg) // ' '  //string_ii2ss(n2)
      if (present(w2)) obj%msg = trim(obj%msg) // ' '  //w2
      if (present(n3)) obj%msg = trim(obj%msg) // ' '  //string_ii2ss(n3)
      if (present(n1)) obj%msg = trim(obj%msg) // ')'
      return
      end subroutine dio_private_whoops


!!------------------------- get error and message ------------------------!!
!!------------------------- get error and message ------------------------!!
!!------------------------- get error and message ------------------------!!
 
 
      subroutine dio_status_err (obj,err)
      implicit none
      type(dio_struct),intent(inout)        :: obj           ! arguments
      integer         ,intent(out)          :: err           ! arguments

      err = obj%err
      return
      end subroutine dio_status_err


 
      subroutine dio_status_msg (obj,msg)
      implicit none
      type(dio_struct),intent(inout)        :: obj           ! arguments
      character(len=*),intent(out)          :: msg           ! arguments

      msg = obj%msg
      return
      end subroutine dio_status_msg


 
      subroutine dio_status_both (obj,err,msg)
      implicit none
      type(dio_struct),intent(inout)        :: obj           ! arguments
      integer         ,intent(out)          :: err           ! arguments
      character(len=*),intent(out)          :: msg           ! arguments

      err = obj%err
      msg = obj%msg
      return
      end subroutine dio_status_both


 
      subroutine dio_status_noun (obj,err,msg,noun)
      implicit none
      type(dio_struct),intent(inout)        :: obj           ! arguments
      integer         ,intent(out)          :: err           ! arguments
      character(len=*),intent(out)          :: msg           ! arguments
      character(len=*),intent(in)           :: noun          ! arguments

      err = obj%err
      if (obj%whoops .and. noun /= ' ') then
           msg = trim(obj%msg)//' ('//trim(noun)//')'
      else
           msg = obj%msg
      end if
      return
      end subroutine dio_status_noun


!!--------------------------- dio open read ----------------------------!!
!!--------------------------- dio open read ----------------------------!!
!!--------------------------- dio open read ----------------------------!!
 
 
      subroutine dio_open_read (obj,filename,err,msg)
      implicit none
      type(dio_struct),pointer          :: obj               ! arguments
      character(len=*),intent(in)       :: filename          ! arguments
      integer         ,intent(out)      :: err               ! arguments
      character(len=*),intent(out)      :: msg               ! arguments
      integer                           :: lun               ! local

      nullify (obj)

      err = finquire_input (filename,msg)
      if (err == FINQUIRE_ERROR) then
           err = DIO_ERROR
           return
      end if

      lun = cio_fopen (filename, 'r')
      if (lun <= 0) then
           err = DIO_ERROR
           msg = 'error opening input file'
           return
      end if

      call dio_private_create (obj,lun)

      err = DIO_OK
      msg = 'input file successfully opened'
      return
      end subroutine dio_open_read
 

!!--------------------------- dio open write ----------------------------!!
!!--------------------------- dio open write ----------------------------!!
!!--------------------------- dio open write ----------------------------!!
 
 
      subroutine dio_open_write (obj,filename,err,msg)
      implicit none
      type(dio_struct),pointer     :: obj                    ! arguments
      character(len=*),intent(in)  :: filename               ! arguments
      integer         ,intent(out) :: err                    ! arguments
      character(len=*),intent(out) :: msg                    ! arguments
      integer                      :: lun,status             ! local

      nullify (obj)

      err = finquire_output (filename,msg)
      if (err == FINQUIRE_ERROR) then
           err = DIO_ERROR
           return
      end if

      err = cio_remove (filename)     ! get rid of the file because
                                      ! w+ leaves the previous contents there.

      call cio_set_file_space_commit (PREALLOCATE_FILE_SPACE_DISABLED)

      status = cio_set_file_ext_size (DIO_EXTSIZE)

      lun = cio_fopen (filename, 'w+')   ! w+ allows rewriting a record.
      if (lun <= 0) then
           err = DIO_ERROR
           msg = 'error opening output file'
           return
      end if

      call dio_private_create (obj,lun)

      err = DIO_OK
      msg = 'output file successfully opened'
      return
      end subroutine dio_open_write
 

!!------------------------ dio close ------------------------------!!
!!------------------------ dio close ------------------------------!!
!!------------------------ dio close ------------------------------!!


      subroutine dio_close (obj)
      implicit none
      type(dio_struct),pointer :: obj             ! arguments
      integer                  :: stat            ! local

      if (associated(obj)) then
           stat = cio_fclose (obj%lun)
           deallocate(obj)
      end if
      return
      end subroutine dio_close


!!------------------------ dio rewind ------------------------------!!
!!------------------------ dio rewind ------------------------------!!
!!------------------------ dio rewind ------------------------------!!


      subroutine dio_rewind (obj)
      implicit none
      type(dio_struct),intent(inout) :: obj               ! arguments

      call cio_frewind (obj%lun)
      obj%err    = DIO_OK
      obj%msg    = 'ok'
      obj%whoops = .false.
      return
      end subroutine dio_rewind


!!------------------------ dio backspace ------------------------------!!
!!------------------------ dio backspace ------------------------------!!
!!------------------------ dio backspace ------------------------------!!


      subroutine dio_backspace (obj)
      implicit none
      type(dio_struct),intent(inout) :: obj               ! arguments

      call cio_fbackspace (obj%lun)
      obj%err    = DIO_OK
      obj%msg    = 'ok'
      obj%whoops = .false.
      return
      end subroutine dio_backspace


!!---------------------------- tell or seek --------------------------------!!
!!---------------------------- tell or seek --------------------------------!!
!!---------------------------- tell or seek --------------------------------!!


      subroutine dio_tell (obj,position)
      implicit none
      type(dio_struct),intent(inout) :: obj               ! arguments
      integer         ,intent(out)   :: position(2)       ! arguments
      integer                        :: stat              ! local

      if (obj%whoops) then
           position = 0
      else
           stat = cio_ftell (obj%lun,DIO_BLOCKSIZE,position(1),position(2))
           if (stat /= DIO_OK) then
                call dio_private_whoops &
                         (obj,'error telling file position',DIO_BLOCKSIZE)
                position = 0
           end if
      end if
      return
      end subroutine dio_tell



      subroutine dio_seek (obj,position)
      implicit none
      type(dio_struct),intent(inout) :: obj               ! arguments
      integer         ,intent(in)    :: position(2)       ! arguments
      integer                        :: stat              ! local

      if (obj%whoops) then
           continue
      else
           stat = cio_fseek (obj%lun,DIO_BLOCKSIZE,position(1),position(2),0)
           if (stat /= DIO_OK) then
                call dio_private_whoops (obj,'error seeking file position',  &
                            DIO_BLOCKSIZE,' ',position(1),' ',position(2))
           end if
      end if
      return
      end subroutine dio_seek


!!-------------------- dio read and write card ---------------------------!!
!!-------------------- dio read and write card ---------------------------!!
!!-------------------- dio read and write card ---------------------------!!


      subroutine dio_read_card (obj,card)
      implicit none
      type(dio_struct),intent(inout)        :: obj             ! arguments
      character(len=*),intent(out)          :: card            ! arguments
      integer                               :: nread           ! local
      integer                               :: temp

      if (obj%whoops) then
           card = ' '
           return
      end if
      temp = len(card)
      nread = cio_fgetline (card,temp,obj%lun)
      if (nread == CIO_EOF) then
           if (.not.obj%whoops) then
                !! This insures that obj%err is not reset to DIO_EOF
                !! if it is already set to DIO_ERROR.
                call dio_private_whoops (obj,'EOF (end of file) encountered')
                obj%err = DIO_EOF    ! reset from DIO_ERROR.
           end if
           card = ' '
      else if (nread == CIO_ERROR) then
           call dio_private_whoops (obj,'ascii card image read error')
           card = ' '
      end if
      return
      end subroutine dio_read_card



      subroutine dio_write_card (obj,card,nchar)
      implicit none
      type(dio_struct),intent(inout)        :: obj            ! arguments
      character(len=*),intent(in)           :: card           ! arguments
      integer         ,intent(in),optional  :: nchar          ! arguments
      integer                               :: nwrite         ! local
      integer                               :: temp

      if (obj%whoops) return
      if (present(nchar)) then
           nwrite = cio_fputline (card,nchar,obj%lun)
      else
           temp = len_trim(card)
           nwrite = cio_fputline (card,temp,obj%lun)
      end if
      if (nwrite == CIO_ERROR) then
           call dio_private_whoops (obj,'ascii card image write error')
      end if
      return
      end subroutine dio_write_card


!!-------------------- dio read and write character -----------------------!!
!!-------------------- dio read and write character -----------------------!!
!!-------------------- dio read and write character -----------------------!!


      subroutine dio_read_character (obj,string)
      implicit none
      type(dio_struct),intent(inout)        :: obj            ! arguments
      character(len=*),intent(out)          :: string         ! arguments
      integer                               :: nread          ! local

      if (obj%whoops) then
           string = ' '
           return
      else
           nread = cio_fread(string, 1, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                               (obj,'character read error',nread,'not 1')
                string = ' '
           end if
      end if
      return
      end subroutine dio_read_character



      subroutine dio_write_character (obj,string)
      implicit none
      type(dio_struct),intent(inout)        :: obj            ! arguments
      character(len=*),intent(in)           :: string         ! arguments
      integer                               :: nwrite         ! local

      if (obj%whoops) then
           return
      else
           nwrite = cio_fwrite(string, 1, 1, obj%lun)
           if (nwrite /= 1) then
                call dio_private_whoops &
                               (obj,'character write error',nwrite,'not 1')
           end if
      end if
      return
      end subroutine dio_write_character



!!-------------------- dio read and write newline -----------------------!!
!!-------------------- dio read and write newline -----------------------!!
!!-------------------- dio read and write newline -----------------------!!


      subroutine dio_read_newline (obj)
      implicit none
      type(dio_struct),intent(inout)        :: obj            ! arguments
      integer                               :: nread          ! local
      character(len=1)                      :: newline        ! local

      if (obj%whoops) then
           return
      else
           nread = cio_fread(newline, 1, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                               (obj,'newline read error',nread,'not 1')
           end if
      end if
      return
      end subroutine dio_read_newline



      subroutine dio_write_newline (obj)
      implicit none
      type(dio_struct),intent(inout)        :: obj                 ! arguments
      integer                               :: nwrite              ! local
      character(len=1),parameter            :: newline = char(10)  ! local

      if (obj%whoops) then
           return
      else
           nwrite = cio_fwrite(newline, 1, 1, obj%lun)
           if (nwrite /= 1) then
                call dio_private_whoops &
                               (obj,'newline write error',nwrite,'not 1')
           end if
      end if
      return
      end subroutine dio_write_newline



!!------------------------ dio read and write string -----------------------!!
!!------------------------ dio read and write string -----------------------!!
!!------------------------ dio read and write string -----------------------!!


      subroutine dio_read_string (obj,string,nchar)
      implicit none
      type(dio_struct),intent(inout)       :: obj                 ! arguments
      character(len=*),intent(out)         :: string              ! arguments
      integer         ,intent(in),optional :: nchar               ! arguments
      integer                              :: nread,nchar2        ! local
      integer                              :: temp

      if (obj%whoops) then
           string  = ' '
           return
      else if (present(nchar)) then
           nchar2 = nchar
      else
           call dio_read_scalar_ivar (obj, nchar2)
      end if

      if (nchar2 == 0) then
           string  = ' '
      else if (nchar2 > len(string)) then
           temp = len(string)
           call dio_private_whoops &
                (obj,'character string size read error',nchar2,'>',temp)
           string = ' '
      else
           nread = cio_fread(string, nchar2, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                            (obj,'character string read error',nread,'not 1')
                string = ' '
           end if
      end if
      return
      end subroutine dio_read_string



      subroutine dio_write_string (obj,string,nchar)
      implicit none
      type(dio_struct),intent(inout)       :: obj                 ! arguments
      character(len=*),intent(in)          :: string              ! arguments
      integer         ,intent(in),optional :: nchar               ! arguments
      integer                              :: nwrite,nchar2,temp  ! local

      if (obj%whoops) then
           return
      else if (present(nchar)) then
           nchar2 = nchar
      else
           nchar2 = len_trim(string)
           call dio_write_scalar_ivar (obj, nchar2)
      end if

      if (nchar2 == 0) then
           return
      else if (nchar2 > len(string)) then
           temp = len(string)
           call dio_private_whoops &
               (obj,'character string size write error',nchar2,'>',temp)
      else
           nwrite = cio_fwrite(string, nchar2, 1, obj%lun)
           if (nwrite /= 1) then
                call dio_private_whoops &
                            (obj,'character string write error',nwrite,'not 1')
           end if
      end if
      return
      end subroutine dio_write_string


!!------------------------- dio read and write strings ---------------------!!
!!------------------------- dio read and write strings ---------------------!!
!!------------------------- dio read and write strings ---------------------!!


      subroutine dio_read_strings (obj,strings,nelements,nchar)
      implicit none
      type(dio_struct),intent(inout)       :: obj                 ! arguments
      character(len=*),intent(out)         :: strings(:)          ! arguments
      integer         ,intent(in)          :: nelements           ! arguments
      integer         ,intent(in),optional :: nchar               ! arguments
      integer                              :: i,temp              ! local

      if (size(strings) < nelements) then
           temp = size(strings)
           call dio_private_whoops &
            (obj,'string read array size too small',temp,'<',nelements)
           strings(:) = CNIL
      else
           do i = 1,nelements
                call dio_read_string (obj,strings(i),nchar)
           end do
      end if
      return
      end subroutine dio_read_strings



      subroutine dio_write_strings (obj,strings,nelements,nchar)
      implicit none
      type(dio_struct),intent(inout)       :: obj                 ! arguments
      character(len=*),intent(in)          :: strings(:)          ! arguments
      integer         ,intent(in)          :: nelements           ! arguments
      integer         ,intent(in),optional :: nchar               ! arguments
      integer                              :: i                   ! local

      do i = 1,nelements
           call dio_write_string (obj,strings(i),nchar)
      end do
      return
      end subroutine dio_write_strings


!!--------------------- dio read and write strings plus ---------------------!!
!!--------------------- dio read and write strings plus ---------------------!!
!!--------------------- dio read and write strings plus ---------------------!!


      subroutine dio_read_strings_plus (obj,strings,nelements,nexpected,nchar)
      implicit none
      type(dio_struct),intent(inout)       :: obj                 ! arguments
      character(len=*),intent(out)         :: strings(:)          ! arguments
      integer         ,intent(out)         :: nelements           ! arguments
      integer         ,intent(in),optional :: nexpected           ! arguments
      integer         ,intent(in),optional :: nchar               ! arguments

      call dio_read_scalar_ivar (obj,nelements)
      call dio_read_strings     (obj,strings,nelements,nchar)
      if (obj%whoops) then
           nelements = 0
      else if (present(nexpected)) then
           if (nelements /= nexpected) then
                call dio_private_whoops &
                 (obj,'unexpected string array size',nelements,'not',nexpected)
                nelements = 0
           end if
      end if
      if (present(nexpected) .and. obj%whoops) then
           strings(1:min(size(strings),nexpected)) = CNIL
      end if
      return
      end subroutine dio_read_strings_plus



      subroutine dio_write_strings_plus (obj,strings,nelements,nchar)
      implicit none
      type(dio_struct),intent(inout)       :: obj                 ! arguments
      character(len=*),intent(in)          :: strings(:)          ! arguments
      integer         ,intent(in)          :: nelements           ! arguments
      integer         ,intent(in),optional :: nchar               ! arguments

      call dio_write_scalar_ivar (obj,nelements)
      call dio_write_strings     (obj,strings,nelements,nchar)
      return
      end subroutine dio_write_strings_plus


!!-------------------------- dio read scalar ------------------------------!!
!!-------------------------- dio read scalar ------------------------------!!
!!-------------------------- dio read scalar ------------------------------!!


      subroutine dio_read_scalar_ivar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: scalar              ! arguments
      integer                        :: nread               ! local

      if (obj%whoops) then
           scalar = 0
      else
           nread = cio_fread(scalar, ISIZE, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                            (obj,'integer scalar read error',nread,'not 1')
                scalar = 0
           else if (ENDIAN) then
                call swap_bytes (scalar)
           end if
      end if
      return
      end subroutine dio_read_scalar_ivar



      subroutine dio_read_scalar_fvar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      real            ,intent(out)   :: scalar              ! arguments
      integer                        :: nread               ! local

      if (obj%whoops) then
           scalar = 0.0
      else
           nread = cio_fread(scalar, FSIZE, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                            (obj,'float scalar read error',nread,'not 1')
                scalar = 0.0
           else if (ENDIAN) then
                call swap_bytes (scalar)
           end if
      end if
      return
      end subroutine dio_read_scalar_fvar



      subroutine dio_read_scalar_dvar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      double precision,intent(out)   :: scalar              ! arguments
      integer                        :: nread               ! local

      if (obj%whoops) then
           scalar = 0.0
      else
           nread = cio_fread(scalar, DSIZE, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                            (obj,'double scalar read error',nread,'not 1')
                scalar = 0.0
           else if (ENDIAN) then
                call swap_bytes (scalar)
           end if
      end if
      return
      end subroutine dio_read_scalar_dvar



      subroutine dio_read_scalar_lvar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      logical         ,intent(out)   :: scalar              ! arguments
      integer                        :: nread               ! local

      if (obj%whoops) then
           scalar = .false.
      else
           nread = cio_fread(scalar, LSIZE, 1, obj%lun)
           if (nread /= 1) then
                call dio_private_whoops &
                            (obj,'logical scalar read error',nread,'not 1')
                scalar = .false.
           else if (ENDIAN) then
                call swap_unk (scalar, LSIZE)
           end if
      end if
      return
      end subroutine dio_read_scalar_lvar


!!-------------------------- dio read array ------------------------------!!
!!-------------------------- dio read array ------------------------------!!
!!-------------------------- dio read array ------------------------------!!


      subroutine dio_read_array_ivars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      integer                        :: nread,temp          ! local

      if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
             (obj,'integer array read size too small',temp,'<',nelements)
           array(:) = INIL
      else if (nelements == 0) then
           return
      else if (obj%whoops) then
           array(1:nelements) = INIL
           return
      else
           nread = cio_fread(array, ISIZE, nelements, obj%lun)
           if (nread /= nelements) then
                call dio_private_whoops &
                         (obj,'integer array read error',nread,'not',nelements)
                array(1:nelements) = INIL
           else if (ENDIAN) then
                call swap_bytes (array(1:nelements))
           end if
      end if
      return
      end subroutine dio_read_array_ivars



      subroutine dio_read_array_fvars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      real            ,intent(out)   :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      integer                        :: nread,temp          ! local

      if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
             (obj,'float array read size too small',temp,'<',nelements)
           array(:) = FNIL
      else if (nelements == 0) then
           return
      else if (obj%whoops) then
           array(1:nelements) = FNIL
           return
      else
           nread = cio_fread(array, FSIZE, nelements, obj%lun)
           if (nread /= nelements) then
                call dio_private_whoops &
                         (obj,'float array read error',nread,'not',nelements)
                array(1:nelements) = FNIL
           else if (ENDIAN) then
                call swap_bytes (array(1:nelements))
           end if
      end if
      return
      end subroutine dio_read_array_fvars



      subroutine dio_read_array_dvars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      double precision,intent(out)   :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      integer                        :: nread,temp          ! local

      if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
             (obj,'double array read size too small',temp,'<',nelements)
           array(:) = DNIL
      else if (nelements == 0) then
           return
      else if (obj%whoops) then
           array(1:nelements) = DNIL
           return
      else
           nread = cio_fread(array, DSIZE, nelements, obj%lun)
           if (nread /= nelements) then
                call dio_private_whoops &
                         (obj,'double array read error',nread,'not',nelements)
                array(1:nelements) = DNIL
           else if (ENDIAN) then
                call swap_bytes (array(1:nelements))
           end if
      end if
      return
      end subroutine dio_read_array_dvars



      subroutine dio_read_array_lvars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      logical         ,intent(out)   :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      integer                        :: nread,temp          ! local

      if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
             (obj,'logical array read size too small',temp,'<',nelements)
           array(:) = LNIL
      else if (nelements == 0) then
           return
      else if (obj%whoops) then
           array(1:nelements) = LNIL
           return
      else
           nread = cio_fread(array, LSIZE, nelements, obj%lun)
           if (nread /= nelements) then
                call dio_private_whoops &
                         (obj,'logical array read error',nread,'not',nelements)
                array(1:nelements) = LNIL
           else if (ENDIAN) then
                call swap_unk_cvec (array(1), LSIZE, nelements)
           end if
      end if
      return
      end subroutine dio_read_array_lvars


!!-------------------------- dio read array plus -------------------------!!
!!-------------------------- dio read array plus -------------------------!!
!!-------------------------- dio read array plus -------------------------!!


      subroutine dio_read_array_iplus (obj,array,nelements,nexpected)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: array(:)            ! arguments
      integer         ,intent(out)   :: nelements           ! arguments
      integer,optional,intent(in)    :: nexpected           ! arguments

      call dio_read_scalar (obj,nelements)
      call dio_read_array  (obj,array,nelements)
      if (obj%whoops) then
           nelements = 0
      else if (present(nexpected)) then
           if (nelements /= nexpected) then
                call dio_private_whoops &
                 (obj,'unexpected integer array size',nelements,'not',nexpected)
                nelements = 0
           end if
      end if
      if (present(nexpected) .and. obj%whoops) then
           array(1:min(size(array),nexpected)) = INIL
      end if
      return
      end subroutine dio_read_array_iplus



      subroutine dio_read_array_fplus (obj,array,nelements,nexpected)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      real            ,intent(out)   :: array(:)            ! arguments
      integer         ,intent(out)   :: nelements           ! arguments
      integer,optional,intent(in)    :: nexpected           ! arguments

      call dio_read_scalar (obj,nelements)
      call dio_read_array  (obj,array,nelements)
      if (obj%whoops) then
           nelements = 0
      else if (present(nexpected)) then
           if (nelements /= nexpected) then
                call dio_private_whoops &
                 (obj,'unexpected float array size',nelements,'not',nexpected)
                nelements = 0
           end if
      end if
      if (present(nexpected) .and. obj%whoops) then
           array(1:min(size(array),nexpected)) = FNIL
      end if
      return
      end subroutine dio_read_array_fplus



      subroutine dio_read_array_dplus (obj,array,nelements,nexpected)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      double precision,intent(out)   :: array(:)            ! arguments
      integer         ,intent(out)   :: nelements           ! arguments
      integer,optional,intent(in)    :: nexpected           ! arguments


      call dio_read_scalar (obj,nelements)
      call dio_read_array  (obj,array,nelements)
      if (obj%whoops) then
           nelements = 0
      else if (present(nexpected)) then
           if (nelements /= nexpected) then
                call dio_private_whoops &
                 (obj,'unexpected double array size',nelements,'not',nexpected)
                nelements = 0
           end if
      end if
      if (present(nexpected) .and. obj%whoops) then
           array(1:min(size(array),nexpected)) = DNIL
      end if
      return
      end subroutine dio_read_array_dplus



      subroutine dio_read_array_lplus (obj,array,nelements,nexpected)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      logical         ,intent(out)   :: array(:)            ! arguments
      integer         ,intent(out)   :: nelements           ! arguments
      integer,optional,intent(in)    :: nexpected           ! arguments


      call dio_read_scalar (obj,nelements)
      call dio_read_array  (obj,array,nelements)
      if (obj%whoops) then
           nelements = 0
      else if (present(nexpected)) then
           if (nelements /= nexpected) then
                call dio_private_whoops &
                 (obj,'unexpected logical array size',nelements,'not',nexpected)
                nelements = 0
           end if
      end if
      if (present(nexpected) .and. obj%whoops) then
           array(1:min(size(array),nexpected)) = LNIL
      end if
      return
      end subroutine dio_read_array_lplus


!!------------------------ dio write scalar ------------------------------!!
!!------------------------ dio write scalar ------------------------------!!
!!------------------------ dio write scalar ------------------------------!!


      subroutine dio_write_scalar_ivar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: scalar              ! arguments
      integer                        :: scalar2             ! local
      integer                        :: nwrite              ! local

      if (obj%whoops) then
           return
      else if (ENDIAN) then
           scalar2 = scalar
           call swap_bytes (scalar2)
           nwrite = cio_fwrite(scalar2, ISIZE, 1, obj%lun)
      else
           nwrite = cio_fwrite(scalar , ISIZE, 1, obj%lun)
      end if
      if (nwrite /= 1) then
           call dio_private_whoops &
                            (obj,'integer scalar write error',nwrite,'not 1')
      end if
      return
      end subroutine dio_write_scalar_ivar



      subroutine dio_write_scalar_fvar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      real            ,intent(in)    :: scalar              ! arguments
      real                           :: scalar2             ! local
      integer                        :: nwrite              ! local

      if (obj%whoops) then
           return
      else if (ENDIAN) then
           scalar2 = scalar
           call swap_bytes (scalar2)
           nwrite = cio_fwrite(scalar2, FSIZE, 1, obj%lun)
      else
           nwrite = cio_fwrite(scalar , FSIZE, 1, obj%lun)
      end if
      if (nwrite /= 1) then
           call dio_private_whoops &
                              (obj,'float scalar write error',nwrite,'not 1')
      end if
      return
      end subroutine dio_write_scalar_fvar



      subroutine dio_write_scalar_dvar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                   ! arguments
      double precision,intent(in)    :: scalar                ! arguments
      double precision               :: scalar2               ! local
      integer                        :: nwrite                ! local

      if (obj%whoops) then
           return
      else if (ENDIAN) then
           scalar2 = scalar
           call swap_bytes (scalar2)
           nwrite = cio_fwrite(scalar2, DSIZE, 1, obj%lun)
      else
           nwrite = cio_fwrite(scalar , DSIZE, 1, obj%lun)
      end if
      if (nwrite /= 1) then
           call dio_private_whoops &
                              (obj,'double scalar write error',nwrite,'not 1')
      end if
      return
      end subroutine dio_write_scalar_dvar



      subroutine dio_write_scalar_lvar (obj,scalar)
      implicit none
      type(dio_struct),intent(inout) :: obj                   ! arguments
      logical         ,intent(in)    :: scalar                ! arguments
      logical                        :: scalar2               ! local
      integer                        :: nwrite                ! local

      if (obj%whoops) then
           return
      else if (ENDIAN) then
           scalar2 = scalar
           call swap_unk (scalar2, LSIZE)
           nwrite = cio_fwrite(scalar2, LSIZE, 1, obj%lun)
      else
           nwrite = cio_fwrite(scalar , LSIZE, 1, obj%lun)
      end if
      if (nwrite /= 1) then
           call dio_private_whoops &
                              (obj,'logical scalar write error',nwrite,'not 1')
      end if
      return
      end subroutine dio_write_scalar_lvar


!!-------------------------- dio write array -----------------------------!!
!!-------------------------- dio write array -----------------------------!!
!!-------------------------- dio write array -----------------------------!!


      subroutine dio_write_array_ivars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      integer                        :: array2(nelements)   ! local
      integer                        :: nwrite,temp         ! local

      if (obj%whoops) then
           return
      else if (nelements == 0) then
           return
      else if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
            (obj,'integer array write size too small',temp,'<',nelements)
           return
      else if (ENDIAN) then
           array2(:) = array(1:nelements)
           call swap_bytes (array2)
           nwrite = cio_fwrite(array2, ISIZE, nelements, obj%lun)
      else
           nwrite = cio_fwrite(array , ISIZE, nelements, obj%lun)
      end if
      if (nwrite /= nelements) then
           call dio_private_whoops &
                        (obj,'integer array write error',nwrite,'not',nelements)
      end if
      return
      end subroutine dio_write_array_ivars



      subroutine dio_write_array_fvars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      real            ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      real                           :: array2(nelements)   ! local
      integer                        :: nwrite,temp         ! local

      if (obj%whoops) then
           return
      else if (nelements == 0) then
           return
      else if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
            (obj,'float array write size too small',temp,'<',nelements)
           return
      else if (ENDIAN) then
           array2(:) = array(1:nelements)
           call swap_bytes (array2)
           nwrite = cio_fwrite(array2, FSIZE, nelements, obj%lun)
      else
           nwrite = cio_fwrite(array , FSIZE, nelements, obj%lun)
      end if
      if (nwrite /= nelements) then
           call dio_private_whoops &
                        (obj,'float array write error',nwrite,'not',nelements)
      end if
      return
      end subroutine dio_write_array_fvars



      subroutine dio_write_array_dvars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      double precision,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      double precision               :: array2(nelements)   ! local
      integer                        :: nwrite,temp         ! local

      if (obj%whoops) then
           return
      else if (nelements == 0) then
           return
      else if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
            (obj,'double array write size too small',temp,'<',nelements)
           return
      else if (ENDIAN) then
           array2(:) = array(1:nelements)
           call swap_bytes (array2)
           nwrite = cio_fwrite(array2, DSIZE, nelements, obj%lun)
      else
           nwrite = cio_fwrite(array , DSIZE, nelements, obj%lun)
      end if
      if (nwrite /= nelements) then
           call dio_private_whoops &
                        (obj,'double array write error',nwrite,'not',nelements)
      end if
      return
      end subroutine dio_write_array_dvars



      subroutine dio_write_array_lvars (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      logical         ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      logical                        :: array2(nelements)   ! local
      integer                        :: nwrite,temp         ! local

      if (obj%whoops) then
           return
      else if (nelements == 0) then
           return
      else if (size(array) < nelements) then
           temp = size(array)
           call dio_private_whoops &
            (obj,'logical array write size too small',temp,'<',nelements)
           return
      else if (ENDIAN) then
           array2(:) = array(1:nelements)
           call swap_unk_cvec (array2(1), LSIZE, nelements)
           nwrite = cio_fwrite(array2, LSIZE, nelements, obj%lun)
      else
           nwrite = cio_fwrite(array , LSIZE, nelements, obj%lun)
      end if
      if (nwrite /= nelements) then
           call dio_private_whoops &
                        (obj,'logical array write error',nwrite,'not',nelements)
      end if
      return
      end subroutine dio_write_array_lvars


!!------------------------ dio write array plus -----------------------------!!
!!------------------------ dio write array plus -----------------------------!!
!!------------------------ dio write array plus -----------------------------!!


      subroutine dio_write_array_iplus (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments

      call dio_write_scalar (obj,nelements)
      call dio_write_array  (obj,array,nelements)
      return
      end subroutine dio_write_array_iplus



      subroutine dio_write_array_fplus (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      real            ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments

      call dio_write_scalar (obj,nelements)
      call dio_write_array  (obj,array,nelements)
      return
      end subroutine dio_write_array_fplus



      subroutine dio_write_array_dplus (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      double precision,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments

      call dio_write_scalar (obj,nelements)
      call dio_write_array  (obj,array,nelements)
      return
      end subroutine dio_write_array_dplus



      subroutine dio_write_array_lplus (obj,array,nelements)
      implicit none
      type(dio_struct),intent(inout) :: obj                 ! arguments
      logical         ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments

      call dio_write_scalar (obj,nelements)
      call dio_write_array  (obj,array,nelements)
      return
      end subroutine dio_write_array_lplus


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module dio_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
