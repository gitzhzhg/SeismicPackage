!<CPS_v1 type="PRIMITIVE"/>
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
!                       C P S   P R I M I T I V E
!
! Name       : CIO
! Category   : io
! Written    : 1999-09-14   by: Bill Menger
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : This provides FORTRAN 90 with common "C" I/O functions, such
!              as getline,putline,fseek,fopen,fclose,fread,fwrite,ftell...
! Portability: Depends upon the ASCII collating sequence.
! References : This file references functions defined in "cio_crou.c"
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!This code is designed to bring the best of the "C" I/O functionality to F90.
!The CIO module reserves space for up to 999 open files, using unit numbers
!beginning at 110 and going up.  Any files accessed by CIO should have been
!opened by CIO and should be closed by CIO.  Units 110 and above are not
!available for standard F90 reads or writes.
!Improper use is made more difficult by using unit numbers
!that are out of the range of F90 file units.  An operating system may further
!restrict how many simultaneously opened files are allowed.
!
!CIO files are used in sequential, random, or mixed use mode.  Record lengths
!are not fixed.  The files are byte addressable, but functionality is given
!for variable-length ASCII record I/O as well.  Records may be read as ASCII
!or BINARY type.
!
! MODES
!   files may be opened with any of the following modes:
!   r    => read only mode.  Opens file in read-only mode positions to BOF.
!                            *** If file is protected from read access or does
!                                not exist, fail. DOES NOT ALLOW WRITES.
!   w    => write mode.      Creates a new file with read/write access.
!                            *** If file exists, TRUNCATE file before writing.
!   a    => append mode.     Opens file in update mode positions to EOF.
!                            *** If file does not exist then revert to "w" mode
!                            *** (Does not keep file at EOF if user decides to
!                                 reposition file pointer later.)
!   r+   => read/write       Opens file in update mode positions to BOF
!                            *** If file does not exist, FAIL
!   w+   => read/write       Opens file in update mode positions to BOF
!                            *** If file does not exist, revert to "w" mode.
!   a+   => read/write       Opens file in update mode positions to EOF.
!                            *** If file does not exist, revert to "w" mode.
!   wn   => write mode.      same as "w" except fail if file exists.
!   an   => append mode.     same as "a" except fail if file exists.
!
!-------------------------------------------------------------------------------
!</descript_doc>
!<calling_doc>
!-------------------------------------------------------------------------------
!                     INPUT AND OUTPUT ARGUMENTS
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
!  STATUS VARIABLES:
!  CIO_OK    ==  0
!  CIO_EOF   == -1
!  CIO_ERROR == -7
!                         CALLING SEQUENCE
!
!  RESULT       FUNCTION_NAME       ARGUMENT LIST
!
!                                 i         i    i(opt)
!  unit     =   cio_fopen      ( filename, mode,scratch )
!                                i     i(opt)
!  status   =   cio_fclose     (unit, delete)
!  status   =   cio_fflush     (unit )
!  position =   cio_ftell      (unit )
!                                i      o
!  status   =   cio_finquire   (unit, filename)
!                                i      i           i
!  record_number = cio_ftell   (unit , start_byte, record_length)
!  record_number = cio_ftell   (unit,  start_vec , record_length)
!
!                                i      o
!  status   =   cio_ftell      (unit , position_vector)
!
!                                i      i           o             o
!  status   =   cio_ftell      (unit , blocksize,  whichblock,whichbyte )
!
!         call  cio_frewind    (unit )
!         call  cio_fbackspace (unit )
!                                 i          i
!  status   =   cio_chmod      ( filename, perms)
!                                i     i                 i
!  status   =   cio_fseek      (unit, offset         , origin)
!  status   =   cio_fseek      (unit, position_vector, origin)
!                                i     i          i             i
!  status   =   cio_fseek      (unit, start_byte,record_length,record_number)
!  status   =   cio_fseek      (unit, start_vec ,record_length,record_number)
!                                i     i          i             i        i
!  status   =   cio_fseek      (unit, blocksize ,whichblock, whichbyte, origin)
!
!                                o      i                     i
!  length   =   cio_fgetline   (string,max_nbr_chars_to_get, unit)
!               *** cio_fgetline strips the newline from the retrieved string
!                   before loading it into your character variable.
!                                i      i(opt)                i
!  nbr_put  =   cio_fputline   (string,                      unit)
!  nbr_put  =   cio_fputline   (string,max_nbr_chars_to_put, unit)
!               *** cio_fputline adds a newline character after writing your
!                   string to the file.
!               If your string is zero length, max_nbr_chars_to_put is zero,
!               or if you sent a blank string with no length specified, then
!               only a newline character is written, and nbr_put = 1.
!
!                                o       i     i                     i
!  nbr_read =   cio_fread      (object, size, num_objects_to_read,  unit)
!                                i       i     i                     i
!  nbr_written= cio_fwrite     (object, size, num_objects_to_write, unit)
!
!                            i
!  isfifo   =   cio_isfifo (unit) 
!               return value = CIO_ERROR on error, 1=yes is named pipe, 0=no
!                              i
!  status   =   cio_unlink (filename)
!  Purpose:  To allow open to create scratch files.
!
!  status   =   cio_remove (filename)
!  Purpose:  To remove a file from the "UNIX" system.
!
!                              i        i
!  status   =   cio_rename (oldname, newname)
!  Purpose:  To rename a file (and its extension files) from "oldname" to
!            "newname".  If either file is in the "cpsdata" path, both must be.
!
!  max_extent_size = cio_extsize()
!  Purpose: To show how large a file extent can be on this filesystem.
!           If > 2 gigabytes, will return 2**31-1
!
!                                    i
!  ext_size = cio_get_file_ext_size(unit)
!  Purpose: To return the extent size for this opened unit.
!           (integer returned)
!
!                                      i
!  status   = cio_set_file_ext_size(ext_size)
!  Purpose:   To temporarily set the extent size for the next file that opens.
!             integer :: ext_size OR
!             type(ll) :: ext_size OR
!             integer,dimension(2) :: ext_size
!             (if you call the last two, the "block size" (word 1 of your LL or
!              integer(2)) is set to the value returned by the function:
!             cio_get_file_ext_size(unit) above.  This is currently set to
!             256meg or 256million.  for a 3 gigabyte extent size, you would
!             set ext_size =(/12,0/)  For 3gigs plus 1 byte, you would set
!             ext_size = (/12,1/) (== 12*256meg+1).  If the ext_size returned
!             above isn't what you want, you can always do the math and make
!             your extent size what you need by adjusting block/byte to fit.
!
!  call  cio_finalize()
!  Purpose: flush all buffers, close all files, free up all buffer memory and
!           internal cio memory and reinitialize cio.
!
!                                 i
!  call cio_set_remote_access(iaccess)
!  Purpose: set access method for remote files.  Default is "UFS or NFS"
!           (iaccess == 0), the other option is SOCKETS, iaccess==1.
!           This should be called before any cio is performed in a job, and
!           NEVER CHANGED DURING A JOB!!!!!.
!           IF YOU USE SOCKETS, you MUST HAVE a PFIO_SERVER running on any
!           node that is present in ANY file name using "node:/dir/file.ext"
!           syntax.  If the "node:" is not present, the server will not be
!           called.  If the "node:" is present and you are using NFS, the
!           nodename will be stripped from the file before the file is opened.
!
!                  i       o
!  call cio_flsz (unit, file_size)
!  call cio_flsz (fname,file_size)
!    Purpose:  Return the size of a file in bytes (2 words returned)
!    character(len=*) :: fname
!    integer          :: unit
!    integer,dimension(2) :: file_size
!
!                                  i
!  call cio_set_file_space_commit(isw)
!    Purpose:  Pre-allocate file size for next file to open.
!              if isw = PREALLOCATE_FILE_SPACE_ENABLED  preallocate,
!                       PREALLOCATE_FILE_SPACE_DISABLED don't preallocate
!                       PREALLOCATE_FILE_SPACE_DEFAULT  preallacate cpstemp/data
!    note cpsdata file are automatically preallocated
!    integer :: isw
!
!                           i      i
!   status = cio_truncate(unit,file_size)
!     Purpose:  Truncate file to size of the file_size vector (see above).
!
!
!                                        i       i       i-opt     o-opt
!   status = cio_try_locking_file  (filename, seconds, lock_type, lock_status)
!     Purpose is to try to lock a file and lock if if possible returning CIO_OK,
!       If the file already locked return CIO_ERROR
!     optional lock_status is the prior status of lock of filename 
!       cio_lock_error    - error with the calling parameters 
!       cio_file_unlocked - file was unlocked 
!       cio_file_locked   - file was locked
!       cio_lock_expired  - lock on file expired to be able to be locked
!       cio_lock_inactive - lock file not present-locks are disabled
!       
!     optional lock_type specifies type of lock-options are
!       cio_normal_lock   -default, other unlocks can delete the lock if it 
!                          expires more than 15 minutes 
!       cio_extended_lock -lock has to expires more than 10 days for other
!                           unlocks can delete it.
!
!                                i
!   status=cio_get_lock_status(filename)
!     Purpose is to get the status of a lock on a given file, returns
!       cio_file_unlocked - file was unlocked 
!       cio_file_locked   - file was locked
!       cio_lock_inactive - lock file not present-locks are disabled
!
!                              i          l-opt
!   call cio_spin_until_locked(lock_name, unlock)
!     Purpose: acts a semaphore across process and cpus
!      waits until lock_name is locked by someone
!      if unlock is pesent and true lock is unlocked after it is found locked
!
!                               i       i       i-opt     o-opt
!   status = cio_lock_file  (filename, seconds, lock_type, lock_status)
!   status = cio_unlock_file(filename,                    lock_status)
!
!     Purpose: to lock a file for "seconds" seconds. and to unlock the file.
!     optional lock_status is the prior status of lock of filename 
!       cio_lock_error    - error  with the calling parameters
!       cio_file_unlocked - file was unlocked 
!                           This is the normal status for cio_lock_file
!       cio_file_locked   - file was locked
!                           This is the normal status fo cio_unlock_file
!       cio_lock_expired  - lock on file expired to be able to be locked
!       cio_lock_inactive - lock file not present-locks are disabled
!       
!     optional lock_type specifies type of lock-options are
!       cio_normal_lock   -default, other unlocks can delete the lock if it 
!                          expires more than 15 minutes 
!       cio_extended_lock -lock has to expires more than 10 days for other
!                           unlocks can delete it.
!
!                                  i   i
!   status = cio_set_wrt_err_rcvr(unit,mode)
!     Purpose: Set the write error recovery option (mode=1(default) = on)
!                                                   mode=0          =off)
!                                  i   i
!   status = cio_set_filrgnlock (unit,mode)
!     Purpose: Set the filrgnlock  option (mode=1          = on)
!                                          mode=0(default) =off)
!
!                           i     i(opt)
!   status = cio_set_bufsz(bufsz,unit)
!     Purpose: Set a file's buffer size (or unbuffer a file) OR
!              change the default buffer size (or unbuffer subsequent opens.)
!     If bufsz > 0, set buffer size to this value
!     If unit is missing, then bufsz is used as the new default buffer size.
!     (default size is 128K bytes if not called)
!     If unit is present and linked to an open file, then the file's buffer
!     size is changed accordingly, and the default is not changed.
!
!
!                            i
!   call cio_set_trace_mode(isw)
!     Purpose: To set trace mode for all i/o in CPS.
!     integer :: isw
!              0 = default == no trace
!              1 = enable trace mode (begins building linked list of all i/o
!                  information
!             -1 = trash all trace information and set to no trace
!              0  (subsequent to having set a 1) ==
!                 output all information from the tracing operation and set to
!                 no trace.
!
!                       i        i      i         i    i
!  call cio_trace_dump(filename,recl,startrec,endrec,offset)
!    Purpose: To format and output specific information from the cio_trace_dump.
!    character(len=*) filename (part or all of a file name. blank == all files.)
!    recl = integer "record length".  Use this to "block" your request of
!    information to begin at a "startrec" and end at an "endrec"... this allows
!    you to do "pseudo-blocking" similar to the above seek functions.  The
!    recl is meaningless except when combined with the startrec,endrec to
!    calculate a file offset that is larger than 2 gig.
!    startrec, endrec (beginning and ending record to check activity between.)
!    offset (starting byte to add to the calculated address in the file where
!    dump output will start.
!
!    Example:  To see all activity in the first 2048 bytes of a file:
!              offset = 0, recl=1, startrec=0,endrec=2047
!
!              To see activity in second extent of a 2 gig file,look at 1 meg.:
!              offset=2**31-1024, recl=1024, startrec=0,endrec=1024
!
!                    i  i    o
!  call cio_checksum(a, n, chksm)
!    Purpose:  To calculate the checksum of n elements of and place results
!    into chksm.  This is meant primarily for diagnostic usage.
!       integer, dimension(*) :: a
!       integer               :: n
!       integer               :: chksm
!
!                       i  i
!  crc = cio_calc_crc(buff,n)
!    Purpose: To calculate CRC of n bytes of "buff" and return in "crc"
!    character(len=*) :: buff ! Either use character OR integer (below)
!    integer          :: buff ! should be starting address for an array
!    integer          :: n
!    integer          :: crc
!                               
!  call cio_update_file_time(file_name)
!    Purpose:  To modify file time stamp to current time.
!    This will idle a second if file current time stamp is current time.
!       character(len=*) :: file_name
!
!                               i       i
!  call cio_write_message_file(file, message)
!    Purpose:  To write message to file.
!       character(len=*) :: file
!       character(len=*) :: message
!
!  call cio_set_debug_mode(idebug)
!    Purpose: set pfio debug mode (0-none:default,>0 debug on)
!
!  cio_set_file_lock_control(isw) returns(status)
!    Purpose:  set lock control parameter for next file to open.
!              if isw = FILE_LOCK_DISABLED then do not lock file,
!                     = FILE_LOCK_ENABLED lock as needed
!    integer :: isw
!    returns status integer: CIO_OK or CIO_ERROR
!
!                                i
!  cio_set_file_auto_delete(isw) returns(status)
!    Purpose:  set auto delete parameter for next file opened.
!              if isw =  AUTO_DELETE_DISABLED then do not auto delete file,
!                     =  AUTO_DELETE_ON_EXIT auto delete when program exits
!                     =  AUTO_DELETE_ON_CLOSE auto delete when file is closed
!
!    integer :: isw
!    returns status integer: CIO_OK or CIO_ERROR
!
!                          i        o         o
!  call cio_get_file_info(file_in, node_out, file_out)
!    Purpose:  to get the node and target file name of input file name
!              file_in can be a symbolic link and can have ~,.,/'s in it
!              invalid input results in blank node_out and file_out
!
!    character(len=*) file_in (input file name)
!    character(len=*) file_out (output target file name)
!    character(len=*) node_out (output node where file_out exists)
!
!                    i
!  call cio_system(command)
!    Purpose: to execute a system command contained in "command"
!    character(len=*) command (system command to execute)
!
!  VARIABLE         TYPE            MEANING
!  unit             integer         cio file "unit" number (>=100)
!  filename         character       cio file name
!  oldfile          character       cio file name
!  newfile          character       cio file name
!  delete           logical         true=delete file, false = keep
!  mode             character       mode = r, w, a, r+, w+, a+ wn an
!  perms            character(len=*) rwx,rwx,rwx or rwx------ ...
!        or
!  perms            integer         decimal equiv of octal 644, 755,...
!                                   (for 643 use 64*6 + 8*4 + 3)
!  status           integer         0 = all ok, -1 = error
!  scratch          logical         true = open for scratch (delete when done.)
!  position         integer         byte position of cio file pointer
!  position_vector  integer(2)      pv(1) = extent number pv(2) = offset in ext.
!        or         type(ll)
!  offset           integer         byte position from "origin" to position ptr
!  whichblock       integer         block number file is on.(for seek or tell)
!  blocksize        integer         size of "virtual" blocks.  These allow you
!                                   to break VERY large files into chunks for
!                                   seek and tell to work when positions are
!                                   greater than 2 gigabytes.
!  whichbyte        integer         position within "block" (seek or tell)
!  origin           integer         0=start, 1=current pos, 2=end of file
!  length           integer         how many chars were returned from getline
!  string           character       "line" to get or put from file.
!  start_byte       integer         starting byte for trace data for fseek_recd
!  record_length    integer         record length for each trace in bytes.
!  record_number    integer         record number of record you are on.
!  trace_number     integer         trace number of trace to seek for.
!  max_nbr_chars_to_[get|put]
!                   integer         nbr of chars not to exceed when performing
!                                   either a putline or a getline.
!  object           any type*       starting address of variable to read from
!                                   or write to when communicating with "unit"
!                                   using fread or fwrite.
!      *types include:
!      real,complex,logical,double precision,integer,character
!      in SCALAR, VECTOR, or 2-D ARRAY (chars only 1_D)
!
!  size             integer         size in BYTES of "object"
!  num_objects_to_[read|write]
!                   integer         how many "object"s to transfer to/from unit
!  iaccess          integer         1=socket i/o, 0 = nfs i/o.
!  idebug           integer         0-no debug, 1 normal debug >1 heavier debug
!  file_size        integer(2)      1 = num extents, 2 = num bytes in last ext.
!              or   type(ll)
!  seconds          integer         time in seconds.
!  bufsz            integer         buffer size in bytes.
!
!</calling_doc>
!-------------------------------------------------------------------------------
!<advice_doc>
!                       ADVICE FOR USERS (USAGE)
! Open a file with cio_fopen, retrieving the unit number from cio.
!   EX: lun = cio_fopen('myfile.dat','r')
!
! Subsequently use the file by referring to it by the unit number (lun above).
!   EX: length_of_line = cio_fgetline(mylinebuffer,len(mylinebuffer),unit)
!   (the second argument assures that you won't exceed the length of your
!    character string.)

! You may use cio_fgetline just like the FORTRAN READ statement, and
! cio_fputline just like the FORTRAN WRITE statement for formatted I/O, but you
! must have encoded the character string prior to the cio_fputline, and you
! must decode the string after the cio_fgetline. Note that the test of
! nwritten below is against "len_trim(string) + 1", because the return
! value includes the newline written at the end of the string.
!   EX: write formatted output of 3 columns x,y,z in f14.7 format.
!   ...
!   character(len=80) :: string
!   lun = cio_fopen('myfile.dat','w')
!   if(lun == 0 ) call error_abort('file not opened')
!   write(string,'(3(f14.7,1x)')x,y,z
!   nwritten = cio_fputline(string,len_trim(string),lun)
!   if(nwritten /= len_trim(string) + 1 ) call error_abort('Error on write.')
!   ...
!   if(cio_fclose(lun) /= 0 ) call error_abort('Problem closing file')
!   ...
!   status = cio_chmod('myfile.dat','rw,r,r') ! change permissions on file.
!   ...
!   lun = cio_fopen('myfile.dat','r')
!   if(lun == 0 ) call error_abort('file not opened')
!   nread = cio_fgetline(string,len(string),lun)
!   if(nread < 0 ) call error_abort('Error on read')
!   read(string,'(3(f14.7,1x)')x,y,z
!   ...
!   status = cio_unlink('myfile.dat') ! will cause delete upon close.
!   status = cio_remove('myfile.dat') ! will cause removal of file even
!                                       if not open.
!
! You may also use cio_fread, cio_fwrite calls to do unformatted, binary i/o.
! The advantage of using cio over FORTRAN read/write is that cio allows you to
! do completely random i/o without fixed record lengths.  A file may contain
! several sections that differ.  A variable length ASCII header may be attached
! to the front of a fixed-length binary data set, which may be followed by a
! different data set that is of a different fixed length.

! Most common data types are supported for cio_fread, cio_fwrite.  So far,
! an interface is completed for:
!  SCALAR: real, double_precision, complex, integer, logical, character
!  VECTOR: real, double_precision, complex, integer, logical, character
!  2-D:    real, double_precision, complex, integer, logical
!   Example:
!   for: real A(100,45), B(55), C
!        character(len=1) :: str(80)
!        character(len=80) :: line
!   nwrite = cio_fwrite(A,4,100*45,lun)
!   nwrite = cio_fwrite(B,4,55,lun)
!   nwrite = cio_fwrite(C,4,1,lun)
!   nwrite = cio_fwrite(str,1,80,lun)
!   nwrite = cio_fwrite(line,80,1,lun)
!    -or-
!   nwrite = cio_fwrite(A(1:1,1),4,100*45,lun)
!   nwrite = cio_fwrite(B(1:1),4,55,lun)
!    -or-
!   nwrite = cio_fwrite(A(1,1),4,100*45,lun)
!
!
!  Usage:  Always give the starting address of your data to read/write as the
!  first argument.  The second argument is the length in BYTES (8 bit chunks)
!  of each element in your data set.  The third argument is the number of
!  data objects you wish to transfer, and the fourth arg is the unit number.

!   EX:  nread = cio_fread(x,4,1,lun) (read ONE element of length 4 bytes into
!   the address of variable X from unit number LUN.)
!   double precision z(1024)
!   ...
!   EX:  nwrite = cio_fwrite(z,8,1024,lun)  (write 1024 elements of length 8
!   bytes from the z vector to disk).


!   ****** FOR FILES > 2 GIGABYTES in LENGTH *****

!  You cannot use regular seek and tell when the files are > 2 gigabytes
!  in length. You must call the cio_fseek and cio_ftell with additional
!  arguments in this case.
!  status   =   cio_fseek      (unit, blocksize ,whichblock, whichbyte, origin)
!  status   =   cio_fseek      (unit, position_vector,origin)
!  status   =   cio_fseek      (unit, start_vector,record_length,record_number)
!
!  status   =   cio_ftell      (unit , blocksize,  whichblock,whichbyte )
!  status   =   cio_ftell      (unit , position_vector)
!  record_number = cio_ftell   (unit , start_vector,record_length)
!
!  The position_vector tells you:
!  position_vector(1) = extent number (0 = base file, 1 = base.fil.001...)
!  position_vector(2) = offset_within extent (0 to 2147482624 bytes)
!
!  calling cio_ftell with the position_vector argument is equivalent to calling
!  it with blocksize = 2147482624 bytes.
!  The "blocksize" is input to both seek and tell, and does not imply that your
!  file is "blocked" in any way.  All that this does is allow seek /tell to
!  either compute or decompose the "long long" integer internally.  F90 does
!  not support integer*8 on many platforms, so we must use some mechanism to
!  get around its limitations.  This is done in "seek" as follows:
!  The user supplies a "blocksize" and origin.  The program calculates an 8-byte
!  integer from blocksize, whichblock, whichbyte where:
!     position = blocksize*whichblock + whichbyte
!  EXAMPLE:
!     You want to get to byte 1024 in a file where you decide to use a
!     "blocksize" of 512:
!     status = cio_fseek(unit,512,2,0,0)
!     You want the second block offset by zero bytes (the start of the block
!     is at whichbyte = 0.
!     In a large file, you might want to go to block 600000 using 65536 byte
!     blocks, and start on byte 124 within that block.
!     status = cio_fseek(unit,65536,600000,124,0).  Your absolute address
!     within the file is 65536*600000+124 = 39321600124 (around 37 gigabytes).

!     NOTE: byte 0 within a block is the FIRST BYTE in that block!!!
!           byte 1 is actually the second byte within the block!!!

!  TELL is similar:  by specifying a blocksize that is sufficiently large,
!  one may obtain 2 resultant position indicators (whichblock, whichbyte) that
!  tell one where the file position is.
!  EXAMPLE:
!    if the file is at byte 1024, then
!    status = cio_ftell(unit,512,myblock, mybyte)
!    results in myblock=2, mybyte=0
!    if the file is at byte 39321600124, then
!    status = cio_ftell(unit,65536,myblock,mybyte)
!    results in myblock = 600000, mybyte = 124.
!
!  The block size does not matter, except that it must be sufficiently large
!  so that your two results do not exceed the capacity of an integer*4 number.
!  Within "tell", the position is determined and decomposed as follows:
!  myblock = position/blocksize
!  mybyte  = modulo(position,blocksize)
!
!  Similarly, the position_vector is calculated as follows:
!
!  position_vector(1) = position/2147482624
!  position_vector(2) = modulo(position,2147482624)
!
!  OVERLOADING:
!  cio_fseek and cio_ftell are overloaded with FIVE separate functional calls
!  for each.  The particular subroutine/function you get depends upon the
!  number and type of arguments that are passed.  BE CAREFUL!!!
!
!-------------------------------------------------------------------------------
!</advice_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
!061. 2006-06-12  B. Menger    Removed Unused Variables.
! 60  2005-12-05  Brian Macy   Added cio_set_direct_io
! 59  2004-08-23  Bill Menger  Added cio_isfifo function
! 58  2004-01-21  Bill Done    Updated documentation about cio_fputline.
! 57  2003-10-08  Bill Menger  Added cio_set_file_ext_size (int(2)) or ll arg.
! 56  2003-08-07  Bill Menger  modified documentation for cio_extsize
! 55  2003-06-25  C C Burch    Added cio_fread_c/fwrite_c and made changes for 
!                              Intel compiler
! 54  2003-05-23  C C Burch    Added get_lock_status and spin_until_locked.
! 53  2002-08-12  R.S.Day      Added c callable  functions cio_lock_file_f,
!                              cio_unlock_file_f,cio_normal_lock_f,
!                              cio_extended_lock_f
! 52  2002-07-23  C C Burch    Remove lockfile name in lock routines
!                              Modified local calls to be pfio compatible
!                              Added cio_set_cpsdisk_control
! 51  2002-06-18  C C Burch    Speeded up cio_fgetline
!     2002-05-24  C C Burch    Added cio_try_locking_file
! 50. 2002-05-20  C C Burch    Enhanced cio_lock/unlock_file
! 49. 2002-02-04  C C Burch    Added cio_set_debug_mode
! 48. 2001-12-06  Bill Menger  Added calc_crc
! 47  2001-11-01  Ed Schmauch  Added cio_buffer_too_small to print
!                              error message.
! 46  2001-10-16  C C Burch    Changed status in read and write routines at EOF
! 45. 2001-10-05  C C Burch    Various changes for consistent C string handling,
!                               c support subroutine/function names, and
!                               reorder code into similar function groups
!                              Eliminated use of allocates and transfers
!                              Enhanced error checking.
! 44. 2001-09-14  R.S.Day      Revised transfer call in cio_fopen_private
!                              to suppress purify warnings.
! 43. 2001-08-29  Bill Menger  Modified memory usage to use txfr_module
! 42. 2001-08-27  C C Burch    Added cio_set_file_lock_control
!                              Added cio_get_file_info
!                              Added cio_set_file_auto_delete
!                              Added optional arguments: file_space_commit,
!                               file_lock and file_auto_delete to cio_fopen
! 41. 2001-04-26  Ed Schmauch  Added cio_checksum, cio_update_file_time,
!                              and cio_write_message_file.
! 40. 2001-04-16  Bill Menger  Remove print statements.
! 39. 2001-04-03  Bill Menger  Added buffer size change ability.
!                              Added trace dump capability.
! 38. 2001-03-01  Bill Menger  Modified documentation Region locks default =off.
! 37. 2001-02-27  Bill Menger  Added wrt_err_rcvr, filrgnlock.
! 36. 2001-01-03  Bill Menger  Added long-long capability(overloaded functions).
! 35. 2001-01-02  Bill Menger  Modified the write_character primitive to not
!                              accidentally write in unallocated memory on some
!                              occasions.
! 34. 2000-12-14  Bill Menger  Added "if (allocated(xxx)) ..." before deallocs.
! 33. 2000-12-13  Bill Menger  Found bug in read_character_s that returned 0
!                              instead of nbytes read.!!!
! 32. 2000-12-12  Bill Menger  Changed the name of cio_get/set_ext_siz to
!                              cio_get/set_ext_size.
!                              Added cio_lock_file, cio_unlock_file
! 31. 2000-12-07  Bill Menger  Added interface for new pfio functions.
! 30. 2000-11-29  Bill Menger  Fixed bug in cio_fputline that caused
!                              long strings to not be written correctly.
! 29. 2000-11-13  Bill Menger  Added cio_flsz function.
! 28. 2000-10-04  Bill Menger  Modified fputline to return 1 if sent a blank
!                              string and no length or a string of zero length.
!                              A newline (char(10)) is written.
!                              Modified all routines to ignore node and userid.
! 27. 2000-10-03  Bill Menger  Added rename function. Remove ident function.
! 26. 2000-08-25  Bill Menger  Modified cio_fputline to only write out the
!                              number of elements in a string if it is shorter
!                              than the max_number that are allowed to be
!                              written.
! 25. 2000-08-24  Bill Menger  Modified all calls to "c" to ensure length of
!                              mode <= 2.
! 24. 2000-08-21  Bill Menger  Changed all calls to "c" to be integer args
!                              instead of character strings.
! 23. 2000-09-18  Bill Menger  *Added optional argument for nobj in fputline.
!                               Modified all PATHs to include NODE if present.
!                              *Fixed transfer call on line 1222 because of bug
!                               in Absoft compiler.
!                              *Upgraded string functions to be better at
!                               handling arrays of strings.
!                              *Added ability to use the pfio_server by setting
!                               "remote access = 1".  Also now all node names
!                               are passed to the underlying cio_crou functions.
!                               (note: usernames are still stripped from all
!                                filenames)
! 21. 2000-06-06  Bill Menger  Removed cc2hh and hh2cc calls, used transfer ftn.
!                              plugged  holes in code where len strings == 0 or
!                              less.
! 20. 2000-05-12  Bill Menger  Documented the file modes.
! 19. 2000-05-11  Bill Menger  Added cio_extsize and cio_get_ident, and finaliz.
! 18. 2000-04-25  Bill Menger  Fixed unlink and remove and added ability to
!                              remove a file on close. Added inquire for name.
! 17. 2000-04-19  Bill Menger  Added position_vector to seek/tell
! 16. 2000-04-03  Bill Menger  Overloaded rewind, backspace, putline, getline,
!                              close to allow FORTRAN I/O if unit number is
!                              less than 100.
! 15. 2000-03-10  Bill Menger  Undid chg 14 and fixed underlying cause, which
!                              was wrong intent on interfaces to "c" in some
!                              cases.  Cleaned up unneeded pointers/targets.
! 14. 2000-03-08  Bill Menger  Modified code to allow optimization to occur on
!                              solaris. Also added error return if file=
!                              path-empty and dir=path-empty together.
! 13. 2000-02-24  Bill Menger  Added exptilde code to expand directory name.
!                              Added cio_path_expand subroutine to use exptilde.
!                              Added cio_pathclean subroutine to clean up.
! 12. 2000-02-07  Bill Menger  Added CIO_OK CIO_ERROR CIO_EOF
! 11. 2000-02-04  Bill Menger  Modified fopen to fail on mode=wn or an
!                              if file exists.
!                              Modified all calls with filename in arg list to
!                              graciously accept userid,nodename, etc and then
!                              politely ignore them!
!                              added a cio_chmod function.
!                              Modified getline to always fill string with blank
! 10. 2000-01-13  Bill Menger  Fixed remove and unlink for filename problem.
!  9. 2000-01-11  Bill Menger  Added remove function
!  8. 1999-12-27  Bill Menger  Added unlink function
!                              Added "trim" function around file name.
!  7. 1999-12-09  Bill Menger  Added ident string
!  6. 1999-12-03  Bill Menger  bug on status return from cio_fwrite_character_1d
!  5. 1999-10-27  Bill Menger  Added long-long seek and tell.
!  4. 1999-10-11  Bill Menger  Small modification.
!  3. 1999-09-22  Bill Menger  Added trace addressable seek function.
!  2. 1999-09-21  Bill Menger  Added interfaces, fixed bugs.
!  1. 1999-09-14  Bill Menger  Initial version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
! Portions of this code depend upon the ASCII collating sequence and/or ASCII
! variable definitions.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
! The supplemental file "cio_crou.c" must be included as part of this module.
!-------------------------------------------------------------------------------
!</compile_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
module cio_module

  use path_module
  use exptilde_module, only: exptilde
  use sizeof_module
  use ll_module
  use string_module
  use cnfg_module
  use unix_module

  implicit none

  private

  public :: cio_set_direct_io
  public :: cio_fbackspace           !tested CCB cio_test3
  public :: cio_calc_crc             !           cio_test21
  public :: cio_checksum             !tested CCB cio_test3
  public :: cio_get_lock_status      !tested CCB cio_lock_test2
  public :: cio_chmod                !tested CCB cio_test2
  public :: cio_dump_lock_file       !diagnostic only 
  public :: cio_extsize              !tested CCB cio_test2
  public :: cio_fclose               !tested CCB cio_test1
  public :: cio_fflush               !tested CCB cio_test2
  public :: cio_fgetline             !tested CCB cio_test3
  public :: cio_finquire             !tested CCB cio_test3
  public :: cio_finalize             !tested CCB cio_test1
  public :: cio_flsz                 !tested CCB cio_test2(mostly, not ll)
  public :: cio_fopen                !tested CCB cio_test1
  public :: cio_form_c_filename      !tested CCB cio_test0
  public :: cio_form_c_str           !tested CCB cio_test0
  public :: cio_fputline             !tested CCB cio_test3
  public :: cio_fread                !tested CCB cio_test1
  public :: cio_fread_c
  public :: cio_frewind              !tested CCB cio_test2
  public :: cio_fseek                !tested CCB cio_test2(not recd or ll)
  public :: cio_ftell                !tested CCB cio_test2(not recd or ll)
  public :: cio_fwrite               !tested CCB cio_test1
  public :: cio_fwrite_c
  public :: cio_get_file_ext_size    !tested CCB cio_test2
  public :: cio_get_file_info        !tested CCB cio_test2
  public :: cio_isfifo               ! wmm no tests yet
  public :: cio_lock_file            !tested CCB cio_test3
  public :: cio_print_c_str          !tested CCB cio_test0
  public :: cio_remove               !tested CCB cio_test1
  public :: cio_rename               !tested CCB cio_test2
  public :: cio_set_bufsz            !tested CCB cio_test3
  public :: cio_set_cpsdisk_control  !tested CCB cio_test1
  public :: cio_set_debug_mode       !tested CCB cio_test3
  public :: cio_set_file_auto_delete !tested CCB cio_test3
  public :: cio_set_file_ext_size    !tested CCB cio_test2
  public :: cio_set_file_lock_control!tested CCB cio_test3
  public :: cio_set_file_space_commit!tested CCB cio_test3
  public :: cio_set_filrgnlock       !currently do-nothing-action suppressed
  public :: cio_set_remote_access    !tested CCB cio_test3
  public :: cio_set_trace_mode       !tested CCB cio_test1
  public :: cio_set_wrt_err_rcvr     !currently do-nothing-action suppressed
  public :: cio_spin_until_locked    !tested CCB cio_lock_test2
  public :: cio_system               !tested CCB cio_test3
  public :: cio_trace_dump           !tested CCB cio_test0
  public :: cio_truncate             !tested CCB cio_test3
  public :: cio_try_locking_file     !tested CCB cio_lock_test
  public :: cio_unform_c_str         !tested CCB cio_test0
  public :: cio_unlink               !tested CCB cio_test3
  public :: cio_unlock_file          !tested CCB cio_test3
  public :: cio_update_file_time     !tested CCB cio_test3
  public :: cio_write_message_file   !tested CCB cio_test3

  integer,parameter,public :: cio_ok    =  0
  integer,parameter,public :: cio_eof   = -1
  integer,parameter,public :: cio_error = -7
  integer,parameter,public :: cio_min_unit = 110

  integer,parameter,public :: cio_lock_error    = -1
  integer,parameter,public :: cio_file_unlocked = 0
  integer,parameter,public :: cio_lock_expired  = 1
  integer,parameter,public :: cio_lock_inactive = 2
  integer,parameter,public :: cio_file_locked   = 3
  integer,parameter,public :: cio_normal_lock   = 10
  integer,parameter,public :: cio_extended_lock = 11

  integer,parameter,public :: CPSDISK_FILE_DISABLED =0!local cpsdisk files
  integer,parameter,public :: CPSDISK_FILE_ENABLED =1 !normal cpsdisk files

  integer,parameter,public :: FILE_LOCK_DISABLED   =0 !do not lock file,
  integer,parameter,public :: FILE_LOCK_ENABLED    =1 !lock file as needed

  integer,parameter,public :: AUTO_DELETE_DISABLED =0 !do not auto delete file,
  integer,parameter,public :: AUTO_DELETE_ON_EXIT  =1
  !                           auto delete when program exits
  integer,parameter,public :: AUTO_DELETE_ON_CLOSE =2
  !                           auto delete when file is closed

  integer,parameter,public :: PREALLOCATE_FILE_SPACE_DISABLED =-1
  !                           don't preallocate
  integer,parameter,public :: PREALLOCATE_FILE_SPACE_ENABLED  = 1
  !                           preallocate space
  integer,parameter,public :: PREALLOCATE_FILE_SPACE_DEFAULT  = 0
  !                           preallocate if file is cpstemp or cpsdata

  character(len=100),save,public :: cio_ident =                                &
   "$Id: cio.f90,v 1.61 2006/06/12 13:03:48 Menger prod sps $"

  character(len=1),parameter                  :: NULL=char(0)
  integer, pointer,save,dimension(:)          :: h
  integer,save                                :: h_alloc=0
  integer,save                                :: c_private_alloc=0
  character(len=1),dimension(:),pointer,save  :: c_private

!---------------------- cio_crou routine interfaces --------------

  interface
    function cio_set_direct_io_c(unit) result(status)
      integer , intent(in) :: unit
      integer              :: status
    end function cio_set_direct_io_c
  end interface

  interface
    subroutine cio_fbackspace_c(unit)
      integer , intent(in) :: unit
    end subroutine cio_fbackspace_c
  end interface

  interface cio_calc_crc
    function cio_calc_crc_c(buff,n) result (crc)
      character(len=*),intent(in) :: buff
      integer,intent(in)          :: n
      integer                     :: crc    
    end function cio_calc_crc_c

    function cio_calc_crci_c(buff,n) result (crc)
      integer,intent(in)          :: buff
      integer,intent(in)          :: n
      integer                     :: crc    
    end function cio_calc_crci_c
  end interface

  interface
    subroutine cio_checksum_c(a, n, chksm)
      integer, dimension(*), intent(in ) :: a
      integer,               intent(in ) :: n
      integer,               intent(out) :: chksm
    end subroutine cio_checksum_c
  end interface

  interface
    function cio_get_lock_status_c(lockfile, filename) result(status)
      character, intent(in) :: lockfile
      character, intent(in) :: filename
      integer               :: status
    end function cio_get_lock_status_c
  end interface
 
  interface
    function cio_chmod_c(filename, mode_t) result (status)
      character, intent(in)               :: filename
      integer,   intent(in)               :: mode_t
      integer                             :: status
    end function cio_chmod_c
  end interface

  interface
    function cio_extsize_c() result (extsize)
      integer :: extsize
    end function cio_extsize_c
  end interface

  interface
    subroutine cio_finalize_c()
    end subroutine cio_finalize_c
  end interface

  interface
    function cio_finquire_c(unit,filename) result(status)
      character,intent(out)              :: filename
      integer,intent(in)                 :: unit
      integer                            :: status
    end function cio_finquire_c
  end interface

  interface
    subroutine cio_flsz_cn_c(fname,ext,off)
      character, intent(in)           :: fname
      integer,   intent(out)          :: ext,off
    end subroutine cio_flsz_cn_c
  end interface

  interface
    subroutine cio_flsz_cu_c(unit,ext,off)
      integer,intent(in)           :: unit
      integer,intent(out)          :: ext,off
    end subroutine cio_flsz_cu_c
  end interface

  interface
    subroutine cio_dump_lock_file_c()
    end subroutine cio_dump_lock_file_c
  end interface

  interface
    function cio_fclose_c(unit,delete) result (status)
      integer,intent(in)                 :: unit
      integer,intent(in)                 :: delete
      integer                            :: status
    end function cio_fclose_c
  end interface

  interface
    function cio_fflush_c(unit) result(status)
      integer,intent(in)                 :: unit
      integer                            :: status
    end function cio_fflush_c
  end interface

  interface
    function cio_fgetline_c (string,max,unit) result (length)
      character(len=*),intent(out)       :: string
      integer,         intent(in)        :: unit
      integer,         intent(in)        :: max
      integer                            :: length
    end function cio_fgetline_c
  end interface

  interface
    function cio_fopen_c(filename,mode,scratch) result (unit)
      character, intent(in)              :: filename
      character, intent(in)              :: mode
      integer         ,intent(in)        :: scratch
      integer                            :: unit
    end function cio_fopen_c
  end interface

  interface
    function cio_fread_char_c (ptr, nbytes, unit) result (numread)
      character, intent(out)                  :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_char_c
  end interface

  interface
    function cio_fread_complex_c (ptr, nbytes, unit) result (numread)
      complex, intent(out)                    :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_complex_c
  end interface

  interface
    function cio_fread_double_c (ptr, nbytes, unit) result (numread)
      double precision, intent(out)           :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_double_c
  end interface

  interface
    function cio_fread_integer_c (ptr, nbytes, unit) result (numread)
      integer,intent(out)                     :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_integer_c
  end interface

  interface
    function cio_fread_integer1_c (ptr, nbytes, unit) result (numread)
      integer(kind=1),intent(out)             :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_integer1_c
  end interface

  interface
    function cio_fread_integer2_c (ptr, nbytes, unit) result (numread)
      integer(kind=2),intent(out)             :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_integer2_c
  end interface

  interface
    function cio_fread_logical_c (ptr, nbytes, unit) result (numread)
      logical, intent(out)                    :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_logical_c
  end interface

  interface
    function cio_fread_real_c (ptr, nbytes, unit) result (numread)
      real, intent(out)                       :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: numread
    end function cio_fread_real_c
  end interface

  interface
    subroutine cio_frewind_c(unit)
      integer,intent(in)                 :: unit
    end subroutine cio_frewind_c
  end interface

  interface
    function cio_fseek_block_and_byte_c(unit,blocksize,whichblock,     &
       whichbyte, origin ) result (status)
      integer, intent(in)                :: unit
      integer, intent(in)                :: blocksize
      integer, intent(in)                :: whichblock
      integer, intent(in)                :: whichbyte
      integer, intent(in)                :: origin
      integer                            :: status
    end function cio_fseek_block_and_byte_c
  end interface

  interface
    function cio_fseek_normal_c(unit,offset,origin) result( status)
      integer,intent(in)                 :: unit
      integer,intent(in)                 :: offset
      integer,intent(in)                 :: origin
      integer                            :: status
    end function cio_fseek_normal_c
  end interface

  interface
    function cio_fseek_recd_c(unit,start_byte,record_length, recd_number)      &
       result (status)
      integer,intent(in)                 :: unit
      integer,intent(in)                 :: start_byte
      integer,intent(in)                 :: record_length
      integer,intent(in)                 :: recd_number
      integer                            :: status
    end function cio_fseek_recd_c
  end interface

  interface
    function cio_ftell_block_and_byte_c(unit,blocksize,whichblock,whichbyte)   &
       result (status)
      integer, intent(in)                :: unit
      integer, intent(in)                :: blocksize
      integer, intent(out)               :: whichblock
      integer, intent(out)               :: whichbyte
      integer                            :: status
    end function cio_ftell_block_and_byte_c
  end interface

  interface
    function cio_ftell_normal_c(unit) result(what_byte)
      integer,intent(in)                 :: unit
      integer                            :: what_byte
    end function cio_ftell_normal_c
  end interface

  interface
    function cio_ftell_recd_c(unit,start_byte,record_length) result(what_recd)
      integer,intent(in)                 :: unit
      integer,intent(in)                 :: start_byte
      integer,intent(in)                 :: record_length
      integer                            :: what_recd
    end function cio_ftell_recd_c
  end interface

  interface
    function cio_fwrite_char_c (ptr, nbytes, unit) result (numread)
      character, intent(in)                   :: ptr
      integer,  intent(in)                    :: nbytes, unit
      integer                                 :: numread
    end function cio_fwrite_char_c
  end interface

  interface
    function cio_fwrite_complex_c (ptr, nbytes, unit) result (status)
      complex, intent(in)                     :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_complex_c
  end interface

  interface
    function cio_fwrite_double_c (ptr, nbytes, unit) result (status)
      double precision, intent(in)            :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_double_c
  end interface

  interface
    function cio_fwrite_integer_c (ptr, nbytes, unit) result (status)
      integer,intent(in)                      :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_integer_c
  end interface

  interface
    function cio_fwrite_integer1_c (ptr, nbytes, unit) result (status)
      integer(kind=1),intent(in)              :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_integer1_c
  end interface

  interface
    function cio_fwrite_integer2_c(ptr, nbytes, unit) result (status)
      integer(kind=2),intent(in)              :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_integer2_c
  end interface

  interface
    function cio_fwrite_logical_c (ptr, nbytes, unit) result (status)
      logical, intent(in)                     :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_logical_c
  end interface

  interface
    function cio_fwrite_real_c (ptr, nbytes, unit) result (status)
      real, intent(in )                       :: ptr
      integer, intent(in)                     :: nbytes, unit
      integer                                 :: status
    end function cio_fwrite_real_c
  end interface

  interface
    function cio_get_file_ext_size_c(unit) result(ext_size)
      integer,intent(in)                 :: unit
      integer                            :: ext_size
    end function cio_get_file_ext_size_c
  end interface

  interface
    subroutine cio_get_file_info_c(filename, f_out, n_out)
      character,intent(in)        :: filename
      character,intent(out)       :: f_out
      character,intent(out)       :: n_out
    end subroutine cio_get_file_info_c
  end interface

  interface
    function cio_isfifo_c(unit) result(isfifo)
      integer,intent(in)                   :: unit
      integer                              :: isfifo
    end function cio_isfifo_c
  end interface

  interface
    function cio_lock_file_c(lockfile, filename,seconds) result(status)
      character, intent(in)   :: lockfile
      character, intent(in)   :: filename
      integer,   intent(in)   :: seconds
      integer                 :: status
    end function cio_lock_file_c
  end interface

  interface
    function cio_remove_c(filename) result (status)
      character, intent(in)              :: filename
      integer                            :: status
    end function cio_remove_c
  end interface

  interface
    function cio_rename_c(oldfile,newfile) result (status)
      character, intent(in)                :: oldfile
      character, intent(in)                :: newfile
      integer                              :: status
    end function cio_rename_c
  end interface

  interface
    function cio_set_bufsz_all_c(bufsz) result (status)
      integer,intent(in)                :: bufsz
      integer                           :: status
    end function cio_set_bufsz_all_c
  end interface

  interface
    function cio_set_bufsz_file_c(bufsz,unit) result (status)
      integer,intent(in)                :: bufsz
      integer,intent(in)                :: unit
      integer                           :: status
    end function cio_set_bufsz_file_c
  end interface

  interface
    subroutine cio_set_cpsdisk_control_c(isw)
      integer,intent(in) :: isw
    end subroutine cio_set_cpsdisk_control_c
  end interface

  interface
    subroutine cio_set_debug_mode_c(idebug)
      integer,intent(in) :: idebug
    end subroutine cio_set_debug_mode_c
  end interface

  interface
    function cio_set_file_ext_size_i2_c(blk,byt) result(status)
      integer,intent(in)                 :: blk,byt
      integer                            :: status
    end function cio_set_file_ext_size_i2_c
  end interface

  interface
    function cio_set_filrgnlock_c(unit,mode) result (status)
      integer,intent(in)                :: unit
      integer,intent(in)                :: mode
      integer                           :: status
    end function cio_set_filrgnlock_c
  end interface

  interface
    subroutine cio_set_lock_type_c(type)
      integer, intent(in) :: type
    end subroutine cio_set_lock_type_c
  end interface
 
  interface
    subroutine cio_set_remote_access_c(iaccess)
      integer,intent(in) :: iaccess
    end subroutine cio_set_remote_access_c
  end interface

  interface
    subroutine cio_set_trace_mode_c(isw)
      integer,intent(in) :: isw
    end subroutine cio_set_trace_mode_c
  end interface

  interface
    function cio_set_wrt_err_rcvr_c(unit,mode) result (status)
      integer,intent(in)                :: unit
      integer,intent(in)                :: mode
      integer                           :: status
    end function cio_set_wrt_err_rcvr_c
  end interface

  interface
    subroutine cio_system_c(command)
      character, intent(in)      :: command
    end subroutine cio_system_c
  end interface

  interface
    subroutine cio_trace_dump_c(filename,recl,startrec,endrec,offset)
      character,intent(in)        :: filename
      integer,intent(in)          :: recl
      integer,intent(in)          :: startrec
      integer,intent(in)          :: endrec
      integer,intent(in)          :: offset
    end subroutine cio_trace_dump_c
  end interface

  interface
    function cio_truncate_c(fname, ext,off) result (status)
      character(len=*),intent(in) :: fname
      integer,intent(in)          :: ext,off
      integer                     :: status
    end function cio_truncate_c
  end interface

  interface
    function cio_try_lock_file_c(lockfile,filename,seconds,type) result(status)
      character, intent(in) :: lockfile
      character, intent(in) :: filename
      integer, intent(in)   :: seconds
      integer, intent(in)   :: type
      integer               :: status
    end function cio_try_lock_file_c
  end interface
 
  interface
    function cio_unlock_file_c(lockfile, filename) result(status)
      character, intent(in) :: lockfile
      character, intent(in) :: filename
      integer               :: status
    end function cio_unlock_file_c
  end interface

  interface
    subroutine cio_update_file_time_c(file_name)
      character, intent(in) :: file_name
    end subroutine cio_update_file_time_c
  end interface

  interface
    subroutine cio_write_message_file_c(file, mess)
      character, dimension(*), intent(in) :: file
      character, dimension(*), intent(in) :: mess
    end subroutine cio_write_message_file_c
  end interface

!--------------------- defining cio routines ----------------------

  interface cio_chmod
    module procedure cio_chmod_private_char
    module procedure cio_chmod_private_int
  end interface

  interface cio_fclose
    module procedure cio_fclose_private
  end interface

  interface cio_fgetline
    module procedure cio_fgetline_char
    module procedure cio_fgetline_char_1
  end interface

  interface cio_finquire
    module procedure cio_finquire_private
  end interface

  interface cio_flsz
    module procedure cio_flsz_cu
    module procedure cio_flsz_cn
    module procedure cio_flsz_unit
    module procedure cio_flsz_unit_ll
    module procedure cio_flsz_name
    module procedure cio_flsz_name_ll
  end interface

  interface cio_fopen
    module procedure cio_fopen_private
  end interface

  interface cio_fputline
    module procedure cio_fputline_char
    module procedure cio_fputline_char_1
  end interface

  interface cio_fread
    module procedure cio_fread_char
    module procedure cio_fread_char_1d
    module procedure cio_fread_complex_0
    module procedure cio_fread_complex_1
    module procedure cio_fread_complex_2
    module procedure cio_fread_double_0
    module procedure cio_fread_double_1
    module procedure cio_fread_double_2
    module procedure cio_fread_integer_0
    module procedure cio_fread_integer_1
    module procedure cio_fread_integer_2
    module procedure cio_fread_integer1_0
    module procedure cio_fread_integer1_1
    module procedure cio_fread_integer1_2
    module procedure cio_fread_integer2_0
    module procedure cio_fread_integer2_1
    module procedure cio_fread_integer2_2
    module procedure cio_fread_logical_0
    module procedure cio_fread_logical_1
    module procedure cio_fread_logical_2
    module procedure cio_fread_real_0
    module procedure cio_fread_real_1
    module procedure cio_fread_real_2
  end interface
  
  interface cio_fread_c
    module procedure cio_fread_char_ci
    module procedure cio_fread_integer_ci
    module procedure cio_fread_real_ci
    module procedure cio_fread_double_ci
    module procedure cio_fread_complex_ci
    module procedure cio_fread_logical_ci
  end interface

  interface cio_fseek
    module procedure cio_fseek_block_and_byte
    module procedure cio_fseek_normal
    module procedure cio_fseek_pv
    module procedure cio_fseek_pv_ll
    module procedure cio_fseek_pv_recd
    module procedure cio_fseek_pv_recd_ll
    module procedure cio_fseek_recd
  end interface

  interface  cio_ftell
    module procedure cio_ftell_block_and_byte
    module procedure cio_ftell_normal
    module procedure cio_ftell_pv
    module procedure cio_ftell_pv_ll
    module procedure cio_ftell_pv_recd
    module procedure cio_ftell_pv_recd_ll
    module procedure cio_ftell_recd
  end interface

  interface cio_fwrite
    module procedure cio_fwrite_char_1d
    module procedure cio_fwrite_char
    module procedure cio_fwrite_complex_0
    module procedure cio_fwrite_complex_1
    module procedure cio_fwrite_complex_2
    module procedure cio_fwrite_double_0
    module procedure cio_fwrite_double_1
    module procedure cio_fwrite_double_2
    module procedure cio_fwrite_integer_0
    module procedure cio_fwrite_integer_1
    module procedure cio_fwrite_integer_2
    module procedure cio_fwrite_integer1_0
    module procedure cio_fwrite_integer1_1
    module procedure cio_fwrite_integer1_2
    module procedure cio_fwrite_integer2_0
    module procedure cio_fwrite_integer2_1
    module procedure cio_fwrite_integer2_2
    module procedure cio_fwrite_logical_0
    module procedure cio_fwrite_logical_1
    module procedure cio_fwrite_logical_2
    module procedure cio_fwrite_real_0
    module procedure cio_fwrite_real_1
    module procedure cio_fwrite_real_2
  end interface

  interface cio_fwrite_c
    module procedure cio_fwrite_char_ci
    module procedure cio_fwrite_integer_ci
    module procedure cio_fwrite_real_ci
    module procedure cio_fwrite_double_ci
    module procedure cio_fwrite_complex_ci
    module procedure cio_fwrite_logical_ci
  end interface

  interface cio_remove
    module procedure cio_remove_private
  end interface

  interface cio_rename
    module procedure cio_rename_private
  end interface

  interface cio_set_file_ext_size
    module procedure cio_set_file_ext_size_i
    module procedure cio_set_file_ext_size_ll
    module procedure cio_set_file_ext_size_i2
  end interface

  interface cio_truncate
    module procedure cio_truncate_unit
    module procedure cio_truncate_unit_ll
    module procedure cio_truncate_fname
  end interface

  interface cio_unlink
    module procedure cio_unlink_private
  end interface

contains

! ----------------------- support routines ----------------

!----------------------- ALLOCATE ROUTINES -----------------
! expand h buffer as needed
  subroutine cio_alloc_h(n)
    integer,intent(in) :: n

    if(n > h_alloc ) then
      h_alloc=n
      if(associated(h) ) then
        deallocate(h)
        nullify(h)
      else
        nullify(h)
      endif
    endif

    if(.not. associated(h) ) then
      allocate(h(h_alloc))
    endif

    return
  end subroutine cio_alloc_h

! expand c_private buffer as needed
  subroutine cio_alloc_c_private(n)
    integer,intent(in) :: n

    if(n > c_private_alloc ) then
      c_private_alloc=n
      if(associated(c_private) ) then
        deallocate(c_private)
        nullify(c_private)
      else
        nullify(c_private)
      endif
    endif

    if(.not. associated(c_private) ) then
      allocate(c_private(c_private_alloc))
    endif

    return
  end subroutine cio_alloc_c_private

!--------------------- C_STRING ROUTINES ---------------

! convert fortran string f_name to c string c_name
  subroutine cio_form_c_str(f_name, c_name, length)
    character (len=*), intent(in)   :: f_name
    character,         intent(out)  :: c_name(:)
    integer, optional, intent(in)   :: length

    integer                         :: n, i

    n=min(size(c_name)-1,len(f_name))
    if(present(length)) n=min(n,length)

    do i=1,n
      c_name(i)=f_name(i:i)
    enddo
    c_name(i)=char(0)

    return
  end subroutine cio_form_c_str

! convert c string c_name into fortran string f_name and provide length
  subroutine cio_unform_c_str(f_name, c_name, length)
    character (len=*), intent(out)  :: f_name
    character,         intent(in)   :: c_name(:)
    integer, optional, intent(out)  :: length

    integer                         :: n, i

    n=min(size(c_name),len(f_name))
    do i=1,n
      if(c_name(i).eq.char(0)) exit
      f_name(i:i)=c_name(i)
    enddo
    if(i.lt.len(f_name)) f_name(i:)=' '

    if(present(length)) length=i-1
    return
  end subroutine cio_unform_c_str

! print c_string c preceeding with contents of info
  subroutine cio_print_c_str(info, a)
    character        :: a(:)
    character(len=*) :: info
    integer          :: n, i
    integer          :: lun=6

    n=size(a)
    do i=1,n
      if(a(i).eq.char(0)) exit
    enddo

    if(i.gt.1) then
      write(lun,*) info,a(1:i-1)
    else
      write(lun,*) info//"NULL"
    endif
    return
  end subroutine cio_print_c_str

!------------------ FILENAME SUPPORT ROUTINES ---------------

!--- purpose: to expand tilde to full path, clean up path, and later to
!--- support environment variables also.
  subroutine cio_path_expand(filename, pathname)
    character(len=*),intent(in)        :: filename
    character(len=*),intent(out)       :: pathname

    character(len=FILENAME_LENGTH)     :: node,userid
    character(len=FILENAME_LENGTH)     :: dir
    character(len=FILENAME_LENGTH)     :: file

    ! file_tmp is to avoid compiler warning on ifc.
    character(len=2*FILENAME_LENGTH+1) :: file_tmp


    !---
    call cio_pathclean(filename, pathname)
    call path_parse(pathname,userid,node,dir,file)

    if( DIR    /= PATH_EMPTY ) then
       file_tmp=trim(dir)//'/'//file
       file = file_tmp(1:FILENAME_LENGTH)
    endif

    call cio_pathclean(file, pathname)
    if(dir(1:1) == '~' ) call exptilde(pathname)

    call path_parse(pathname,userid,node,dir,file)

    if( DIR    /= PATH_EMPTY ) then
       file_tmp=trim(dir)//'/'//file
       file = file_tmp(1:FILENAME_LENGTH)
    endif

    call cio_pathclean(file, pathname)
    return
  end subroutine cio_path_expand

!--- Purpose: remove duplicate slashed and embedded spaces.
  subroutine cio_pathclean(filename, pathname)
    character(len=*), intent(in)        :: filename
    character(len=*), intent(out)       :: pathname

    integer                             :: i, i1, len_pathname, len_filename

    i1=0
    len_pathname=len(pathname)
    len_filename=len_trim(filename)
    do i = 1, len_filename
      if(filename(i:i)==' ') cycle
      if(filename(i:i)=='/' .and. i1>0) then
        if(pathname(i1:i1)=='/') cycle
      endif

      i1=i1+1
      if(i1>len_pathname) return
      pathname(i1:i1)=filename(i:i)
    end do
    if(i1<len_pathname) pathname(i1+1:len_pathname)=' '

    return
  end subroutine cio_pathclean

! convert filename into expanded filename_c as c string, retrns CIO_OK if no err
  function cio_form_c_filename(filename, filename_c) result (status)
    character(len=*),intent(in)        :: filename
    character,       intent(out)       :: filename_c(:)
    integer                            :: status
    integer                            :: temp

    character(len=FILENAME_LENGTH)     :: pathname, node, dir, userid, file

    ! file_tmp is to avoid compiler warning on ifc.
    character(len=2*FILENAME_LENGTH+1) :: file_tmp


    filename_c = ''
    if(len_trim(filename)>FILENAME_LENGTH) then
      status=CIO_ERROR
      return
    endif

    call cio_path_expand(filename, pathname)
    call path_parse(pathname,userid,node,dir,file)
    if( FILE == PATH_EMPTY .and. DIR == PATH_EMPTY ) then
      status=CIO_ERROR
      return
    endif

    if( DIR    /= PATH_EMPTY ) then
       file_tmp=trim(dir)//'/'//file
       file = file_tmp(1:FILENAME_LENGTH)
    endif

    call cio_pathclean(file, pathname)
    temp = len_trim(pathname)
    call cio_form_c_str(pathname, filename_c, temp)
    status=CIO_OK

    return
  end function cio_form_c_filename

  subroutine cio_buffer_too_small(routine)
    character(len=*), intent(in) :: routine

    integer, save                :: lun=6

    write(lun, *) 'Buffer too small in '//trim(routine)

  end subroutine cio_buffer_too_small

!--------------START OF CIO USER ROUTINES--------------------

! ------------------ FOPEN ---------------------------------

  function cio_fopen_private(filename, mode, scratch, file_space_commit,       &
     file_lock,  file_auto_delete) result (unit)

    character(len=*),intent(in)        :: filename
    character(len=*),intent(in)        :: mode
    logical,optional                   :: scratch
    integer,optional                   :: file_space_commit
    integer,optional                   :: file_lock
    integer,optional                   :: file_auto_delete

    integer                            :: unit

    integer                            :: iscratch
    character                          :: filename_c(FILENAME_LENGTH+1)
    character                          :: mode_c(4)
    integer                            :: temp

    if(cio_form_c_filename(filename, filename_c).ne.CIO_OK) then
      unit=CIO_ERROR
      return
    endif
    temp = len_trim(mode)
    call cio_form_c_str(mode, mode_c,temp)

    iscratch = 0
    if(present(scratch) ) then
      if(scratch) iscratch = 1
    endif

    if(present(file_space_commit)) then
      call cio_set_file_space_commit(file_space_commit)
    endif

    if(present(file_lock)) then
      if(cio_set_file_lock_control(file_lock).ne.CIO_OK) then
        call cio_set_file_space_commit_c(-2) !restore default
        return
      endif
    endif

    if(present(file_auto_delete)) then
      if(cio_set_file_auto_delete(file_auto_delete).ne.CIO_OK) then
        call cio_set_file_space_commit_c(-2)        !restore default
        call cio_set_file_lock_control_c(-2)  !restore default
        return
      endif
    endif

    unit = cio_fopen_c (filename_c(1), mode_c(1), iscratch)
    return
  end function cio_fopen_private

! ----------------------- fclose ------------------------

  function cio_fclose_private(unit,delete) result(status)
    integer,intent(in)                   :: unit
    logical         ,intent(in),optional :: delete
    integer                              :: status

    character(len=6)                     :: filestatus
    integer                              :: i_delete

    filestatus = 'keep'
    if(present(delete) ) then
      if(delete) filestatus = 'delete'
    endif

    if(unit < cio_min_unit) then ! use fortran i/o
      close(unit,iostat=status,status=filestatus)
    else
      i_delete = 0
      if(filestatus == 'delete' ) i_delete = 1
      status = cio_fclose_c(unit,i_delete)
    endif

    return
  end function cio_fclose_private

!--------------------------- FSEEK -----------------------

  function cio_fseek_block_and_byte(unit,blocksize,whichblock,         &
     whichbyte, origin) result(status)
    integer, intent(in)                :: unit
    integer, intent(in)                :: blocksize
    integer, intent(in)                :: whichblock
    integer, intent(in)                :: whichbyte
    integer, intent(in)                :: origin
    integer                            :: status

    status=cio_fseek_block_and_byte_c(unit,blocksize,whichblock,              &
     whichbyte, origin )
    return
  end function cio_fseek_block_and_byte

  function cio_fseek_normal(unit,offset,origin) result(status)
    integer,intent(in)                 :: unit
    integer,intent(in)                 :: offset
    integer,intent(in)                 :: origin
    integer                            :: status

    status=cio_fseek_normal_c(unit,offset,origin)
    return
  end function cio_fseek_normal

  function cio_fseek_pv(unit,pos_vec,origin) result (status)
    integer,intent(in)                    :: unit
    integer,intent( in),dimension(2)      :: pos_vec
    integer,intent(in)                    :: origin
    integer                               :: status
    integer                               :: extsize

    ! arguments:             unit blocksize     whichblock whichbyte  origin)

    extsize = cio_get_file_ext_size(unit)
    if(extsize <= 0 ) then
    status  = cio_error
      return
    endif

    status =                                                                   &
     cio_fseek_block_and_byte(unit,extsize,pos_vec(1),pos_vec(2),origin)

    return
  end function cio_fseek_pv

  function cio_fseek_pv_ll(unit,pos_vec,origin) result (status)
    integer,intent(in)                    :: unit
    type(ll),intent( in)                  :: pos_vec
    integer(kind=4),intent(in)                    :: origin
    integer                               :: status
    integer(kind=4)                               :: extsize
    integer(kind=4),dimension(:)          :: pos(2)

    ! arguments:             unit blocksize     whichblock whichbyte  origin)
    extsize = cio_get_file_ext_size(unit)
    status  = ll_set_maxintval(extsize)
    if(extsize <= 0 ) then
      status  = cio_error
      return
    endif

    pos = pos_vec
    status =                                                                   &
     cio_fseek_block_and_byte(unit,extsize,pos(1),pos(2),origin)

    return
  end function cio_fseek_pv_ll

  function cio_fseek_pv_recd(unit,strt_vec,record_length,record_number)        &
     result (status)
    integer,intent(in)                    :: unit
    integer,intent( in),dimension(2)      :: strt_vec
    integer,intent(in)                    :: record_length
    integer,intent(in)                    :: record_number
    integer                               :: status,status1

    !--- first, assume data records start at beginning of file, move to the
    !--- record number requested.
    status1 = cio_fseek(unit,0,record_length,record_number)
    !--- Second, move incrementally by the position of the start vector.
    status  = cio_fseek(unit,strt_vec,1)

    if(status1 /= cio_ok .OR. status /= cio_ok) then
      status = cio_error
    else
      status = cio_ok
    endif

    return
  end function cio_fseek_pv_recd

  function cio_fseek_pv_recd_ll(unit,strt_vec,record_length,record_number)     &
     result (status)
    integer,intent(in)                    :: unit
    type(ll),intent( in)                  :: strt_vec
    integer,intent(in)                    :: record_length
    integer,intent(in)                    :: record_number
    integer                               :: status,status1

    !--- first, assume data records start at beginning of file, move to the
    !--- record number requested.
    status1 = cio_fseek(unit,0,record_length,record_number)
    !--- Second, move incrementally by the position of the start vector.

    status  = cio_fseek(unit,strt_vec,1)

    if(status1 /= cio_ok .OR. status /= cio_ok) then
      status = cio_error
    else
      status = cio_ok
    endif

    return
  end function cio_fseek_pv_recd_ll

  function cio_fseek_recd(unit,start_byte,record_length, recd_number) &
   result(status)
    integer,intent(in)                 :: unit
    integer,intent(in)                 :: start_byte
    integer,intent(in)                 :: record_length
    integer,intent(in)                 :: recd_number
    integer                            :: status

    status=cio_fseek_recd_c(unit,start_byte,record_length, recd_number)
    return
  end function cio_fseek_recd


! ----------------------- ftell --------------------------
  function cio_ftell_block_and_byte(unit,blocksize,whichblock, whichbyte) &
   result(status)
    integer, intent(in)                :: unit
    integer, intent(in)                :: blocksize
    integer, intent(out)               :: whichblock
    integer, intent(out)               :: whichbyte
    integer                            :: status

    status=cio_ftell_block_and_byte_c(unit,blocksize,whichblock,whichbyte)
    return
  end function cio_ftell_block_and_byte

  function cio_ftell_normal(unit) result(status)
    integer,intent(in)    :: unit
    integer               :: status

    status=cio_ftell_normal_c(unit)
    return
  end function cio_ftell_normal

  function cio_ftell_pv(unit,pos_vec) result (status)
    integer,intent(in)                    :: unit
    integer,intent(out),dimension(2)      :: pos_vec
    integer                               :: status
    integer                               :: extsize

    ! arguments:             unit blocksize     whichblock whichbyte)
    extsize = cio_get_file_ext_size(unit)
    if(extsize <= 0 ) then
      status  = cio_error
      return
    endif

    status =                                                                   &
     cio_ftell_block_and_byte(unit,extsize,pos_vec(1),pos_vec(2))

    return
  end function cio_ftell_pv

  function cio_ftell_pv_ll(unit,pos_vec) result (status)
    integer,intent(in)                    :: unit
    type(ll),intent(out)                  :: pos_vec
    integer                               :: status
    integer                               :: extsize
    integer            ,dimension(2)      :: pos

    ! arguments:             unit blocksize     whichblock whichbyte)
    extsize = cio_get_file_ext_size(unit)
    if(extsize <= 0 ) then
      status  = cio_error
      return
    endif

    status  = ll_set_maxintval(extsize)
    status =                                                                   &
     cio_ftell_block_and_byte(unit,extsize,pos(1),pos(2))
    pos_vec = pos

    return
  end function cio_ftell_pv_ll

  function cio_ftell_pv_recd(unit,strt_vec,record_length)                      &
     result (record_number)
    integer,intent(in)                    :: unit
    integer,intent(in ),dimension(2)      :: strt_vec
    integer,intent(in)                    :: record_length
    integer                               :: status

    integer                               :: record_number,bytes_left
    integer,dimension(2)                  :: diff_vec
    integer                               :: extsize

    status     =  cio_ftell(unit,diff_vec) ! current position
    diff_vec   = diff_vec - strt_vec       ! how much data?
    extsize    = cio_get_file_ext_size(unit)
    if(extsize <= 0 ) then
      record_number = -1
      return
    endif

    record_number  = diff_vec(1) * (extsize/record_length)
    bytes_left     = diff_vec(2) +                                             &
     (diff_vec(1)*(modulo(extsize,record_length)))
    record_number  = 1 + record_number + bytes_left/record_length

    return
  end function cio_ftell_pv_recd


  function cio_ftell_pv_recd_ll(unit,strt_vec,record_length)                   &
     result (record_number)
    integer,intent(in)                    :: unit
    type(ll),intent(in )                  :: strt_vec
    integer,intent(in)                    :: record_length
    integer                               :: status,record_number,bytes_left
    integer,dimension(2)                  :: diff_vec
    integer                               :: extsize

    extsize    = cio_get_file_ext_size(unit)
    if(extsize <= 0 ) then
      record_number = -1
      return
    endif

    status     = ll_set_maxintval(extsize)
    status     = cio_ftell(unit,diff_vec)  ! current position
    diff_vec   = diff_vec - strt_vec       ! how much data?
    record_number  = diff_vec(1) * (extsize/record_length)
    bytes_left     = diff_vec(2) +                                             &
     (diff_vec(1)*(modulo(extsize,record_length)))
    record_number  = 1 + record_number + bytes_left/record_length

  end function cio_ftell_pv_recd_ll

  function cio_ftell_recd(unit,start_byte,record_length) result(status)
    integer,intent(in)                 :: unit
    integer,intent(in)                 :: start_byte
    integer,intent(in)                 :: record_length
    integer                            :: status

    status=cio_ftell_recd_c(unit,start_byte,record_length)
    return
  end function cio_ftell_recd


!--------------------- FREAD ROUTINES -----------------------

! fread of character string
  function cio_fread_char (c,isize,nobj,unit) result (status)
    character(len=*), intent(out) :: c
    integer, intent(in)           :: isize
    integer, intent(in)           :: nobj
    integer, intent(in)           :: unit
    integer                       :: status

    integer                       :: nbytes, i, len_c
    character                     :: c_data(len(c))    !workspace for c str

    len_c=len(c)
    nbytes=isize*nobj

    if(nbytes <= 0 ) then
      status = 0            !no bytes to read-return 0
      return
    endif

    if(nbytes>len_c) then
      status=CIO_ERROR      !c not big enough for data requested
      return
    endif

    status = cio_fread_char_c(c_data(1), nbytes, unit)
    if(status >=  0)  then
      status=status/isize
    else
      status=CIO_ERROR
      return
    endif

    do i = 1,nbytes
      c(i:i) = c_data(i)   !move read data to c
    end do
    if(len_c > nbytes) c(nbytes+1:len_c)=char(0)  !zero rest of c

    return
  end function cio_fread_char

! read into array of characters
  function cio_fread_char_1d (c,isize,nobj,unit) result (status)
    character(len=*), intent(inout) :: c(:)
    integer, intent(in)             :: isize
    integer, intent(in)             :: nobj
    integer, intent(in)             :: unit
    integer                         :: status

    integer                         :: nbytes, len_c, size_c, size_c_data
    integer                         :: i, j, k   
    character                       :: c_data(len(c(1))*size(c)) !workspace

    len_c=len(c(1))
    size_c=size(c)
    size_c_data=size_c*len_c
    nbytes = isize*nobj

    if(nbytes <= 0 ) then
      status = 0               !no data to read-return 0
      return
    endif

    if(size_c_data < nbytes) then
      status=CIO_ERROR         !c not big enough for data reqested
      return
    endif

    status = cio_fread_char_c(c_data(1), nbytes, unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
      return
    endif

    k=0                     !move read data to c--zero pad if needed
    do i=1, size_c
      do j=1,len_c
        k=k+1
        if(k <= nbytes) then
          c(i)(j:j)=c_data(k)
        else
          c(i)(j:j)=char(0)  !zero part of c where data not read
        endif
      enddo
    enddo

    return
  end function cio_fread_char_1d

! fread complex--note no checking if data area overflows
  function cio_fread_complex_0(tgt,isize,nobj,unit) result (status)
    complex,intent(out)    :: tgt
    integer, intent(in)    :: nobj,isize,unit
    integer                :: status

    integer                :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_complex_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_complex_0

! fread 1d complex
  function cio_fread_complex_1(tgt,isize,nobj,unit) result (status)
    complex,intent(out)    :: tgt(:)
    integer, intent(in)    :: nobj,isize,unit
    integer                :: status

    integer                :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_complex_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_complex_1

! fread 2d complex
  function cio_fread_complex_2(tgt, isize, nobj, unit) result (status)
    complex, intent(out)   :: tgt(:,:)
    integer, intent(in)    :: nobj, isize,unit
    integer                :: status

    integer                                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_complex_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_complex_2

! fread double precision--note no checking if data area overflows
  function cio_fread_double_0(tgt, isize, nobj, unit) result (status)
    double precision,intent(out)  :: tgt
    integer,         intent(in)   :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_double_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_double_0

! fread 1d double precision
  function cio_fread_double_1(tgt, isize, nobj, unit) result (status)
    double precision,intent(out)  :: tgt(:)
    integer,         intent(in)   :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_double_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_double_1

 ! fread 2d double precision
  function cio_fread_double_2(tgt, isize, nobj, unit) result (status)
    double precision, intent(out)  :: tgt(:,:)
    integer,          intent(in)   :: nobj, isize, unit
    integer                        :: status

    integer                        :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_double_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_double_2

! fread integer--note no checking if data area overflows
  function cio_fread_integer_0(tgt, isize, nobj, unit) result (status)
    integer, intent(out)  :: tgt
    integer, intent(in)   :: nobj, isize, unit
    integer               :: status

    integer               :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_integer_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer_0

! fread 1d integer
  function cio_fread_integer_1(tgt, isize, nobj, unit) result (status)
    integer, intent(out)  :: tgt(:)
    integer, intent(in)   :: nobj, isize, unit
    integer               :: status

    integer               :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_integer_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer_1

! fread 2d integer
  function cio_fread_integer_2(tgt, isize, nobj, unit) result (status)
    integer, intent(out)  :: tgt(:,:)
    integer, intent(in)   :: nobj, isize, unit
    integer               :: status

    integer               :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_integer_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer_2

! fread integer(kind=1)--note no checking if data area overflows
  function cio_fread_integer1_0(tgt, isize, nobj, unit) result (status)
    integer(kind=1), intent(out)  :: tgt
    integer,         intent(in)   :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_integer1_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer1_0

! fread 1d integer(kind=1)
  function cio_fread_integer1_1(tgt, isize, nobj, unit) result (status)
    integer(kind=1), intent(out)  :: tgt(:)
    integer,         intent(in)   :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_integer1_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer1_1

! fread 2d integer(kind=1)
  function cio_fread_integer1_2(tgt, isize, nobj, unit) result (status)
    integer(kind=1), intent(out)  :: tgt(:,:)
    integer,         intent(in)   :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_integer1_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer1_2

! fread integer(kind=2)--note no checking if data area overflows
  function cio_fread_integer2_0(tgt, isize, nobj, unit) result (status)
    integer(kind=2), intent(out) :: tgt
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_integer2_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer2_0

! fread 1d integer(kind=2)
  function cio_fread_integer2_1(tgt, isize, nobj, unit) result (status)
    integer(kind=2), intent(out) :: tgt(:)
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_integer2_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer2_1

! fread 2d integer(kind=2)
  function cio_fread_integer2_2(tgt, isize, nobj, unit) result (status)
    integer(kind=2), intent(out) :: tgt(:,:)
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_integer2_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_integer2_2

! fread  logical--note no checking if data area overflows
  function cio_fread_logical_0(tgt, isize, nobj, unit) result (status)
    logical, intent(out) :: tgt
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_logical_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_logical_0

! fread 1d logical
  function cio_fread_logical_1(tgt, isize, nobj, unit) result (status)
    logical, intent(out) :: tgt(:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_logical_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_logical_1

! fread 2d logical
  function cio_fread_logical_2(tgt, isize, nobj, unit) result (status)
    logical, intent(out) :: tgt(:,:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_logical_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_logical_2

! fread real--note no checking if data area overflows
  function cio_fread_real_0(tgt, isize, nobj, unit) result (status)
    real,    intent(out) :: tgt
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fread_real_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_real_0

! fread 1d real
  function cio_fread_real_1(tgt, isize, nobj, unit) result (status)
    real,    intent(out) :: tgt(:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>size(tgt)*sizeof(tgt(1)) ) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_real_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_real_1

! fread 2d real
  function cio_fread_real_2(tgt, isize, nobj, unit) result (status)
    real,    intent(out) :: tgt(:,:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fread')
      status=CIO_ERROR
      return
    endif

    status = cio_fread_real_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fread_real_2

!---------------------- FWRITE ROUTINES ---------------------

! fwrite character
  function cio_fwrite_char (c, isize, nobj, unit) result (status)
    character(len=*), intent(in)       :: c
    integer, intent(in)                :: isize
    integer, intent(in)                :: nobj
    integer, intent(in)                :: unit
    integer                            :: status

    integer                            :: i, nbytes, len_c, n
    character                          :: c_data(max(1,isize*nobj)) !workspace

    nbytes = isize*nobj
    len_c=len(c)

    if(nbytes <= 0) then
      status=0          !no data to write
      return
    endif

! - move data to workspace

    n=min(nbytes,len_c)
    do i = 1, n
      c_data(i) = c(i:i)
    end do
    if(nbytes.gt.len_c) c_data(len_c+1:nbytes)=char(0)

    status = cio_fwrite_char_c(c_data(1), nbytes, unit)
    if(status .ge. 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_char

! fwrite array of character
  function cio_fwrite_char_1d (c,isize,nobj,unit) result (status)
    character(len=*), intent(in) :: c(:)
    integer, intent(in)          :: isize
    integer, intent(in)          :: nobj
    integer, intent(in)          :: unit
    integer                      :: status

    integer                      :: i, j, k, nbytes, len_c, size_c, size_c_data
    character                    :: c_data(max(1,nobj*isize))

    len_c=len(c(1))
    size_c=size(c)
    size_c_data=size_c*len_c

    nbytes=isize*nobj
    if(nbytes <= 0) then
      status=0
      return
    endif

! - move data to workspace

    k = 0
    do i = 1, size(c)
      do j = 1, len(c(i))
        k=k+1
        if(k > nbytes) exit
        c_data(k) = c(i)(j:j)
      enddo
      if(k > nbytes) exit
    enddo
    if(size_c_data<nbytes) c_data(size_c_data+1:nbytes)=char(0)

    status = cio_fwrite_char_c(c_data(1),nbytes,unit)
    if(status .ge. 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_char_1d

! fwrite  complex--note no checking if data area overflows
  function cio_fwrite_complex_0(tgt, isize, nobj, unit) result (status)
    complex, intent(in)  :: tgt
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_complex_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_complex_0

! fwrite 1d complex
  function cio_fwrite_complex_1(tgt, isize, nobj, unit) result (status)
    complex, intent(in)  :: tgt(:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_complex_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_complex_1

! fwrite 2d complex
  function cio_fwrite_complex_2(tgt, isize, nobj, unit) result (status)
    complex, intent(in)  :: tgt(:,:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_complex_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_complex_2

! fwrite double precision--note no checking if data area overflows
  function cio_fwrite_double_0(tgt, isize, nobj, unit) result (status)
    double precision, intent(in)  :: tgt
    integer,          intent(in)  :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_double_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_double_0

! fwrite 1d double precision
  function cio_fwrite_double_1(tgt, isize, nobj, unit) result (status)
    double precision, intent(in)  :: tgt(:)
    integer,          intent(in)  :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_double_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_double_1

! fwrite 2d double precision
  function cio_fwrite_double_2(tgt, isize, nobj, unit) result (status)
    double precision, intent(in)  :: tgt(:,:)
    integer,          intent(in)  :: nobj, isize, unit
    integer                       :: status

    integer                       :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_double_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_double_2

! fwrite integer--note no checking if data area overflows
  function cio_fwrite_integer_0(tgt, isize, nobj, unit) result (status)
    integer, intent(in)  :: tgt
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_integer_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer_0

! fwrite 1d integer
  function cio_fwrite_integer_1(tgt, isize, nobj, unit) result (status)
    integer, intent(in)  :: tgt(:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_integer_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer_1

! fwrite 2d integer
  function cio_fwrite_integer_2(tgt, isize, nobj, unit) result (status)
    integer, intent(in)  :: tgt(:,:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_integer_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer_2

! fwrite integer(kind=1)--note no checking if data area overflows
  function cio_fwrite_integer1_0(tgt, isize, nobj, unit) result (status)
    integer(kind=1), intent(in)  :: tgt
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_integer1_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer1_0

! fwrite 1d integer(kind=1)
  function cio_fwrite_integer1_1(tgt, isize, nobj, unit) result (status)
    integer(kind=1), intent(in)  :: tgt(:)
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_integer1_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer1_1

! fwrite 2d integer(kind=1)
  function cio_fwrite_integer1_2(tgt, isize, nobj, unit) result (status)
    integer(kind=1), intent(in)  :: tgt(:,:)
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_integer1_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer1_2

! fwrite integer(kind=2)--note no checking if data area overflows
  function cio_fwrite_integer2_0(tgt, isize, nobj, unit) result (status)
    integer(kind=2), intent(in)  :: tgt
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_integer2_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer2_0

! fwrite 1d integer(kind=2)
  function cio_fwrite_integer2_1(tgt, isize, nobj, unit) result (status)
    integer(kind=2), intent(in)  :: tgt(:)
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_integer2_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer2_1

! fwrite 2d integer(kind=2)
  function cio_fwrite_integer2_2(tgt, isize, nobj, unit) result (status)
    integer(kind=2), intent(in)  :: tgt(:,:)
    integer,         intent(in)  :: nobj, isize, unit
    integer                      :: status

    integer                      :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_integer2_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_integer2_2

! fwrite logical--note no checking if data area overflows
  function cio_fwrite_logical_0(tgt, isize, nobj, unit) result (status)
    logical, intent(in)  :: tgt
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_logical_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_logical_0

! fwrite 1d logical
  function cio_fwrite_logical_1(tgt, isize, nobj, unit) result (status)
    logical, intent(in)  :: tgt(:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_logical_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_logical_1

! fwrite 2d logical
  function cio_fwrite_logical_2(tgt, isize, nobj, unit) result (status)
    logical, intent(in)  :: tgt(:,:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_logical_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_logical_2

! fwrite real--note no checking if data area overflows
  function cio_fwrite_real_0(tgt, isize, nobj, unit) result (status)
    real,    intent(in)  :: tgt
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    status = cio_fwrite_real_c(tgt,n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_real_0

! fwrite 1d real
  function cio_fwrite_real_1(tgt, isize, nobj, unit) result (status)
    real,    intent(in)  :: tgt(:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_real_c(tgt(1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_real_1

! fwrite 2d real
  function cio_fwrite_real_2(tgt, isize, nobj, unit) result (status)
    real,    intent(in)  :: tgt(:,:)
    integer, intent(in)  :: nobj, isize, unit
    integer              :: status

    integer              :: n

    n=isize*nobj
    if(n<=0) then
      status=0
      return
    endif

    if(n>sizeof(tgt(1,1))*size(tgt)) then
      call cio_buffer_too_small('cio_fwrite')
      status=CIO_ERROR
      return
    endif

    status = cio_fwrite_real_c(tgt(1,1),n,unit)
    if(status >= 0) then
      status=status/isize
    else
      status=CIO_ERROR
    endif

    return
  end function cio_fwrite_real_2

! ------------------------------ finquire----------------
  function cio_finquire_private(unit,filename) result(status)
    integer,intent(in)                 :: unit
    character(len=*),intent(out)       :: filename
    integer                            :: status

    character                          :: filename_c(FILENAME_LENGTH+1)

    status = cio_finquire_c(unit,filename_c(1))
    if(status /= cio_ok) return

    call cio_unform_c_str(filename, filename_c)
    return
  end function cio_finquire_private

! --------------------remove-----------------------------
  function cio_remove_private(filename) result (status)
    character(len=*),intent(in)        :: filename
    integer                            :: status

    character                          :: filename_c(FILENAME_LENGTH+1)

    if(cio_form_c_filename(filename, filename_c).ne.CIO_OK) then
      status=CIO_ERROR
    else
      status = cio_remove_c(filename_c(1))
    endif
    return
  end function cio_remove_private

!------------------------- UNLINK -------------------------
  function cio_unlink_private(filename) result (status)
    character(len=*),intent(in)        :: filename
    integer                            :: status

    character                          :: filename_c(FILENAME_LENGTH+1)

    if(cio_form_c_filename(filename, filename_c).ne.CIO_OK) then
      status=CIO_ERROR
      return
    endif
    status = cio_remove_c(filename_c(1))

    return
  end function cio_unlink_private

! -----------------------rename---------------------------------
  function cio_rename_private (oldfilename,newfilename) result (status)
    character(len=*),intent(in)              :: oldfilename
    character(len=*),intent(in)              :: newfilename
    integer                                  :: status

    character, dimension(FILENAME_LENGTH+1)  :: oldname_c,newname_c

    if(cio_form_c_filename(oldfilename, oldname_c).ne.CIO_OK .or. &
       cio_form_c_filename(newfilename, newname_c).ne.CIO_OK) then
      status=CIO_ERROR
      return
    endif

    status = cio_rename_c (oldname_c(1),newname_c(1))
    return
  end function cio_rename_private

!-------------------------- CHMOD -----------------------
  function cio_chmod_private_char(filename,perms) result (status)
    character(len=*),intent(in)              :: filename
    character(len=*),intent(in)              :: perms
    integer                                  :: status

    integer                            :: mode_t,mode_part
    integer                            :: i,j,k,kptr,ipower
    character(len=3),dimension(3)      :: internal_perms

    internal_perms(:) = ' '
    kptr = 1
    do i = 1,2
      k = index(perms(kptr:),',')
      if(k <= 0 ) then
        internal_perms(i) = perms(kptr : min(len(perms),kptr+2))
        kptr = kptr + 3
      else
        k = kptr + k - 1
        internal_perms(i) = perms(kptr:min(kptr+2,k-1))
        kptr = k + 1
      endif
    end do

    if(kptr <= len(perms) ) then
      internal_perms(i) = perms(kptr:)
    endif

    mode_t = 0
    do i = 3,1,-1
      ipower = 3-i
      mode_part = 0
      do j = 1,3
        select case(internal_perms(i)(j:j))
        case('r','R')
          mode_part = mode_part + 4
        case('w','W')
          mode_part = mode_part + 2
        case('x','X')
          mode_part = mode_part + 1
        end select
      end do
      mode_t = mode_t + mode_part*(8**(ipower))
    end do

    status = cio_chmod_private_int (filename,mode_t)
    return
  end function cio_chmod_private_char

  function cio_chmod_private_int(filename,perms) result (status)
    character(len=*),intent(in)              :: filename
    integer,         intent(in)              :: perms
    integer                                  :: status

    character                          :: filename_c(FILENAME_LENGTH+1)

    if(cio_form_c_filename(filename, filename_c).ne.CIO_OK) then
      status = CIO_ERROR
      return
    endif

    status = cio_chmod_c (filename_c(1),perms)
    return
  end function cio_chmod_private_int

!---------------------- FPUTLINE --------------------------

!- putline character string
  function cio_fputline_char(string,nobj,unit) result (nbr_put)
    character(len=*),intent(in)        :: string
    integer,intent(in),optional        :: nobj
    integer,intent(in)                 :: unit
    integer                            :: nbr_put

    integer                            :: nobj_private,n

    if(present(nobj)) then
      nobj_private = nobj
    else
      nobj_private = len_trim(string)
    endif

    n = min(len(string),nobj_private)

    if(n < 0 ) then
      nbr_put = 0
    elseif(n == 0 ) then
      if(unit < cio_min_unit ) then ! use fortran i/o
        write(unit,'(a)')''
        nbr_put = 1
      else
        nbr_put = cio_fwrite(char(10),1,1,unit)
      endif
    elseif(n > 0 ) then
      if(unit < cio_min_unit ) then ! use fortran i/o
        write(unit,'(a)')trim(string(:n))
        nbr_put = len_trim(string(:n)) + 1
      else
        nbr_put = cio_fwrite(string(1:n),1,n,unit)
        nbr_put = nbr_put + cio_fwrite(char(10),1,1,unit)
      endif
    endif

    return
  end function cio_fputline_char

!- putline array of character strings
  function cio_fputline_char_1(string,nobj,unit) result (nbr_put)
    character(len=*),intent(in)        :: string(:)
    integer,intent(in),optional        :: nobj
    integer,intent(in)                 :: unit

    integer                            :: nbr_put

    integer                            :: i,j,k
    character(len=len(string(1))*size(string)+1)   :: c_privatep
    integer                            :: nobj_private

    if(present(nobj)) then
      nobj_private = nobj
    else
      nobj_private = len(string(1))*size(string)
    endif

    if(nobj_private <= 0 .or. len(c_privatep) <= 1) then
      nbr_put = cio_fputline('',0,unit)
      return
    endif

    k = 0
    c_privatep = ' '
    do i = 1,size(string)
      do j = 1, len(string(i))
        k = k + 1
        c_privatep(k:k) = string(i)(j:j)
      end do
    end do

    if(k <= nobj_private ) nobj_private = k
    if(.not.present(nobj))nobj_private=len_trim(c_privatep)
    nbr_put = cio_fputline_char(c_privatep(1:nobj_private),nobj_private,unit)

    return
  end function cio_fputline_char_1

!---------------------- FGETLINE ----------------------

! getline character string
  function cio_fgetline_char(string,imax,unit) result (length)
    character(len=*),intent(out)       :: string
    integer,         intent(in)        :: imax
    integer,         intent(in)        :: unit
    integer                            :: length

    length = 0
    if(imax <= 0 .or. len(string) <= 0 ) return

    if(unit < cio_min_unit) then ! use fortran i/o
      string(:len(string)) = NULL
      read(unit,'(a)')string
      length = index(string,NULL)
      if(length > 0 ) string(length+1:)=' '
    else
      length = cio_fgetline_c(string,imax,unit)
    endif

!   print *,"getline:length=",length,",s=", trim(string)
    return
  end function cio_fgetline_char

! getline array of character strings
  function cio_fgetline_char_1(string,imax,unit) result (length)
    character(len=*),intent(inout)     :: string(:)
    integer,         intent(in)        :: imax
    integer,         intent(in)        :: unit
    integer                            :: length

    integer                            :: i,j,k
    character(len=len(string(1))*size(string)+1) :: c_privatep
    integer                            :: temp

    length = 0
    if(imax <= 0 .or. len(c_privatep) <= 1 ) return
    c_privatep = ' '
    temp = min(len(c_privatep),imax)
    length = cio_fgetline_char(c_privatep,temp,unit)

    k = 1
    do i = 1, size(string)
      do j = 1, len(string(i))
        string(i)(j:j) = c_privatep(k:k)
        if(k >= imax) return
        k = k + 1
      end do
    end do

    return
  end function cio_fgetline_char_1

!-------------------------CHECKSUM-----------------
  subroutine cio_checksum(a, n, chksm)
    integer, dimension(*), intent(in ) :: a
    integer,               intent(in ) :: n
    integer,               intent(out) :: chksm

    call cio_checksum_c(a, n, chksm)
    return
  end subroutine cio_checksum

!---------------------------UPDATE FILE TIME-------------
  subroutine cio_update_file_time(file_name)
    character(len=*), intent(in) :: file_name
    character                    :: filename_c(FILENAME_LENGTH+1)
    integer                      :: temp

    temp = len_trim(file_name)
    call cio_form_c_str(file_name,filename_c,temp)
    call cio_update_file_time_c(filename_c(1))
    return
  end subroutine cio_update_file_time

! -----------------------write message file ------------------
  subroutine cio_write_message_file(filename, mess)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mess

    character                    :: filename_c(FILENAME_LENGTH+1)
    character                    :: mess_c(len(mess)+1)
    integer                      :: temp
    temp = len_trim(filename)
    call cio_form_c_str(filename,filename_c,temp)
    temp = len_trim(mess)
    call cio_form_c_str(mess,mess_c,temp)
    call cio_write_message_file_c(filename_c, mess_c)
    return
  end subroutine cio_write_message_file

!-------------------------SET FILE SPACE COMMIT---------------
  subroutine cio_set_file_space_commit(isw)
    integer, intent(in) :: isw

    if(isw.eq.PREALLOCATE_FILE_SPACE_ENABLED .or.                              &
     isw.eq.PREALLOCATE_FILE_SPACE_DISABLED.or.                                &
     isw.eq.PREALLOCATE_FILE_SPACE_DEFAULT) then
      call cio_set_file_space_commit_c(isw)
    else
      call cio_set_file_space_commit_c(-2)
    endif

    return
  end subroutine cio_set_file_space_commit

!------------------SET CPSDISK CONTROL---------------
  subroutine cio_set_cpsdisk_control(isw)
    integer, intent(in) :: isw

    call cio_set_cpsdisk_control_c(isw) !CPSDISK_FILE_DISABLED is 0
    return
  end subroutine cio_set_cpsdisk_control

!----------------------SET_FILE_LOCK_CONTROL------------
  function cio_set_file_lock_control(isw) result(status)
    integer, intent(in) :: isw
    integer             :: status

    if(isw.eq.FILE_LOCK_ENABLED .or.isw.eq.FILE_LOCK_DISABLED) then
      status=CIO_OK
      call cio_set_file_lock_control_c(isw)
    else
      status=CIO_ERROR
    endif

    return
  end function cio_set_file_lock_control

!------------------------SET_FILE_AUTO_DELETE--------------
  function cio_set_file_auto_delete(isw) result(status)
    integer, intent(in) :: isw
    integer             :: status

    if(isw.eq.AUTO_DELETE_DISABLED .or.isw.eq.AUTO_DELETE_ON_EXIT .or.         &
     isw.eq.AUTO_DELETE_ON_CLOSE) then
      status=CIO_OK
      call cio_set_file_auto_delete_c(isw)
    else
      status=CIO_ERROR
    endif

    return
  end function cio_set_file_auto_delete

!---------------------- GET_FILE_INFO------------------
  subroutine cio_get_file_info(file_in, node_out, file_out)
    character(len=*), intent(in)  :: file_in
    character(len=*), intent(out) :: node_out, file_out

    character                     :: f_out(260), n_out(260)
    character                     :: filename_c(FILENAME_LENGTH+1)
    integer                       :: temp

    temp = len_trim(file_in)
    call cio_form_c_str(file_in,filename_c,temp)
    call cio_get_file_info_c(filename_c(1),f_out(1), n_out(1))

    call cio_unform_c_str(file_out,f_out)
    call cio_unform_c_str(node_out,n_out)

    return
  end subroutine cio_get_file_info

!---------------------- IS_FIFO ----------------------------
  function cio_isfifo (unit) result (isfifo)
    integer, intent(in) :: unit
    integer             :: isfifo

    isfifo = cio_isfifo_c(unit)

    return
  end function cio_isfifo

!-----------------------CIO_FINALIZE------------------------
  subroutine cio_finalize()
    call cio_finalize_c()

    if(associated(h) ) deallocate(h)
    h_alloc = 0

    if(associated(c_private) ) deallocate(c_private)
    c_private_alloc=0

    return
  end subroutine cio_finalize

!------------------------CIO_FFLUSH--------------------
  function cio_fflush(unit) result(status)
    integer, intent(in)  :: unit
    integer              :: status

    status=cio_fflush_c(unit)
    return
  end function cio_fflush

!-----------------------------EXTSIZE------------------
  function cio_extsize() result(ext_size)
    integer   :: ext_size

    ext_size=cio_extsize_c()
    return
  end function cio_extsize

!---------------------GET FILE EXT SIZ
  function cio_get_file_ext_size(unit) result(ext_size)
    integer,intent(in)        :: unit
    integer                   :: ext_size

    ext_size=cio_get_file_ext_size_c(unit)
    return
  end function cio_get_file_ext_size

!------------------SET DEBUG MODE---------------------
  subroutine cio_set_debug_mode(idebug)
    integer,intent(in) :: idebug

    call cio_set_debug_mode_c(idebug)
  end subroutine cio_set_debug_mode

!--------------------------SET FILE EXT SIZE (integer)
  function cio_set_file_ext_size_i(ext_size) result (status)
    integer,intent(in)                 :: ext_size
    integer                            :: status

    status=cio_set_file_ext_size_i2_c(0,ext_size)
    return
  end function cio_set_file_ext_size_i

!--------------------------SET FILE EXT SIZE (long long)
  function cio_set_file_ext_size_ll(ext_size) result (status)
    type(ll),intent(in)                :: ext_size
    integer                            :: status
    integer,dimension(2)               :: size
    size=ext_size
    status=cio_set_file_ext_size_i2_c(size(1),size(2))
    return
  end function cio_set_file_ext_size_ll

!--------------------------SET FILE EXT SIZE (integer(2))
  function cio_set_file_ext_size_i2(ext_size) result (status)
    integer,dimension(2)               :: ext_size
    integer                            :: status
    status=cio_set_file_ext_size_i2_c(ext_size(1),ext_size(2))
    return
  end function cio_set_file_ext_size_i2

!------------------SET REMOTE ACCESS---------------------
  subroutine cio_set_remote_access(iaccess)
    integer,intent(in) :: iaccess

    call cio_set_remote_access_c(iaccess)
  end subroutine cio_set_remote_access

!--------------------SET TRACE MODE----------------------
  subroutine cio_set_trace_mode(isw)
    integer,intent(in) :: isw
    call cio_set_trace_mode_c(isw)
    return
  end subroutine cio_set_trace_mode

!--------------------TRACE DUMP------------------------------
  subroutine cio_trace_dump(filename,recl,startrec,endrec,offset)
    character(len=*),intent(in) :: filename
    integer,intent(in)          :: recl
    integer,intent(in)          :: startrec
    integer,intent(in)          :: endrec
    integer,intent(in)          :: offset
    character                   :: filename_c(FILENAME_LENGTH+1)
    integer                     :: temp

    if(len_trim(filename)>FILENAME_LENGTH) return
    temp     = len_trim(filename)
    call cio_form_c_str(filename,filename_c,temp)
    call cio_trace_dump_c(filename_c(1),recl,startrec,endrec,offset)

    return
  end subroutine cio_trace_dump

  function cio_set_bufsz(bufsz,unit) result (status)
    integer,intent(in)           :: bufsz
    integer,intent(in),optional  :: unit
    integer                      :: status

    if(present(unit) ) then
      status = cio_set_bufsz_file_c(bufsz,unit)
    else
      status = cio_set_bufsz_all_c(bufsz)
    endif

    return
  end function cio_set_bufsz

!------------------------ DUMP LOCK FILE --------------------
  subroutine cio_dump_lock_file()
    call cio_dump_lock_file_c()
    return
  end subroutine cio_dump_lock_file

!------------------------ LOCK FILE --------------------
  function cio_lock_file(filename, seconds, lock_type, lock_status)  &
   result (status)
    
    character(len=*),intent(in)             :: filename
    integer,intent(in)                      :: seconds
    integer                                 :: status
    integer, intent(in ), optional          :: lock_type  
    integer, intent(out), optional          :: lock_status

    character                        :: filename_c(FILENAME_LENGTH+1)
    character                        :: lockfile_c(FILENAME_LENGTH+1)
    character(len=FILENAME_LENGTH)   :: cps_lock_file
    integer                          :: len_filename, len_lockfile
   
    call cnfg_get_value("cps_lock_file", cps_lock_file)
    len_lockfile=len_trim(cps_lock_file)
    call cio_form_c_str(cps_lock_file,lockfile_c, len_lockfile)
    
    len_filename=len_trim(filename)
    if(len_filename>FILENAME_LENGTH .or. len_filename==0 .or. &
       len_lockfile>FILENAME_LENGTH .or. len_lockfile==0 .or. &
       seconds.le.0) then
      if(present(lock_status)) lock_status=cio_lock_error
      status=CIO_ERROR
      return
    endif
    
    call cio_form_c_str(filename,filename_c,len_filename)

    if(present(lock_type)) then
      if(lock_type.eq.cio_normal_lock) then
        status=CIO_OK           !a no-op
      else if(lock_type.eq.cio_extended_lock) then
        call cio_set_lock_type_c(ichar('X'))
      else
        if(present(lock_status)) lock_status=cio_lock_error
        status=CIO_ERROR
        return
      endif
    endif

    status = cio_lock_file_c(lockfile_c(1), filename_c(1), seconds)
    if(present(lock_status)) then
      if(status.eq.0) then         !convert to cio lock status
        lock_status=cio_file_unlocked
      else if(status.eq.1) then
        lock_status=cio_lock_expired
      else if(status.eq.2) then
        lock_status=cio_lock_inactive
      else
        lock_status=cio_lock_error
      endif
    endif
    status=CIO_OK
    
    return
  end function cio_lock_file

!------------------------ TRY_LOCKING_FILE --------------------
  function cio_try_locking_file(filename, seconds, lock_type, lock_status) &
    result (status)
    character(len=*),intent(in)             :: filename
    integer,intent(in)                      :: seconds
    integer                                 :: status
    integer, intent(in ), optional          :: lock_type  
    integer, intent(out), optional          :: lock_status

    character                        :: filename_c(FILENAME_LENGTH+1)
    integer                          :: try_lock_type
    character                        :: lockfile_c(FILENAME_LENGTH+1)
    character(len=FILENAME_LENGTH)   :: cps_lock_file
    integer                          :: len_filename, len_lockfile
    
    call cnfg_get_value("cps_lock_file", cps_lock_file)
    len_lockfile=len_trim(cps_lock_file)
    len_filename=len_trim(filename)
    if(len_filename>FILENAME_LENGTH .or. len_filename==0 .or. &
       len_lockfile>FILENAME_LENGTH .or. len_lockfile==0 .or. &
       seconds.le.0) then
      if(present(lock_status)) lock_status=cio_lock_error
      status=CIO_ERROR
      return
    endif

    call cio_form_c_str(cps_lock_file,lockfile_c, len_lockfile)
    call cio_form_c_str(filename,filename_c,len_filename)

    try_lock_type=ichar('H')
    if(present(lock_type)) then
      if(lock_type.eq.cio_normal_lock) then
        status=CIO_OK           !a no-op
      else if(lock_type.eq.cio_extended_lock) then
        try_lock_type=ichar('X')
      else
        if(present(lock_status)) lock_status=cio_lock_error
        status=CIO_ERROR
        return
      endif
    endif

    status = cio_try_lock_file_c(lockfile_c(1),filename_c(1), seconds, &
      try_lock_type)

    if(present(lock_status)) then
      if(status.eq.0) then         !convert to cio lock status
        lock_status=cio_file_unlocked
      else if(status.eq.1) then
        lock_status=cio_file_locked
      else if(status.eq.2) then
        lock_status=cio_lock_inactive
      else if(status.eq.3) then
        lock_status=cio_lock_expired
      else
        lock_status=cio_lock_error
      endif
    endif

    if(status.eq.0 .or.status.eq.2 .or. status.eq.3) then
      status=CIO_OK
    else
      status=CIO_ERROR
    endif
    
    return
  end function cio_try_locking_file

!------------------------ GET_LOCK_STATUS --------------------
  function cio_get_lock_status(filename) result (status)
    character(len=*),intent(in)             :: filename
    integer                                 :: status

    character                        :: filename_c(FILENAME_LENGTH+1)
    character                        :: lockfile_c(FILENAME_LENGTH+1)
    character(len=FILENAME_LENGTH)   :: cps_lock_file
    integer                          :: len_filename, len_lockfile
    
    call cnfg_get_value("cps_lock_file", cps_lock_file)
    len_lockfile=len_trim(cps_lock_file)
    len_filename=len_trim(filename)
    if(len_filename>FILENAME_LENGTH .or. len_filename==0 .or. &
       len_lockfile>FILENAME_LENGTH .or. len_lockfile==0 ) then
      status=CIO_ERROR
      return
    endif

    call cio_form_c_str(cps_lock_file,lockfile_c, len_lockfile)
    call cio_form_c_str(filename,filename_c,len_filename)

    status = cio_get_lock_status_c(lockfile_c(1),filename_c(1))
    return
  end function cio_get_lock_status

!------------------------ SPIN_UNTIL_LOCKED --------------------
  subroutine cio_spin_until_locked(lock_name, unlock)
    character(len=*), intent(in) :: lock_name
    logical, optional            :: unlock

    integer                      :: lock_status, t_sleep
    logical                      :: unlock_sw

    if(present(unlock)) then
      unlock_sw=unlock
    else
      unlock_sw=.false.
    endif

    t_sleep=1
    lock_status=cio_get_lock_status(lock_name)
    do while(lock_status.eq.cio_file_unlocked)
      call unix_sleep(t_sleep)
      if(t_sleep.lt.10) t_sleep=t_sleep+1
      lock_status=cio_get_lock_status(lock_name)
    enddo 

    if(unlock_sw) lock_status=cio_unlock_file(lock_name)
    return
  end subroutine cio_spin_until_locked

!-------------------------- UNLOCK FILE -----------------
  function cio_unlock_file(filename, lock_status) result (status)
    character(len=*),intent(in)             :: filename
    integer                                 :: status
    integer, intent(out), optional          :: lock_status

    character                        :: filename_c(FILENAME_LENGTH+1)
    character                        :: lockfile_c(FILENAME_LENGTH+1)
    character(len=FILENAME_LENGTH)   :: cps_lock_file
    integer                          :: len_filename, len_lockfile
    
    call cnfg_get_value("cps_lock_file", cps_lock_file)
    len_lockfile=len_trim(cps_lock_file)
    call cio_form_c_str(cps_lock_file,lockfile_c, len_lockfile)
    
    len_filename=len_trim(filename)
    if(len_filename>FILENAME_LENGTH .or. len_filename==0 .or. &
       len_lockfile>FILENAME_LENGTH .or. len_lockfile==0 ) then
      if(present(lock_status)) lock_status=cio_lock_error
      status=CIO_ERROR
      return
    endif

    call cio_form_c_str(filename,filename_c,len_filename)
    status = cio_unlock_file_c(lockfile_c(1), filename_c(1))
    
    if(present(lock_status)) then
      if(status.eq.0) then         !convert to cio lock status
        lock_status=cio_file_locked
      else if(status.eq.1) then
        lock_status=cio_file_unlocked
      else if(status.eq.2) then
        lock_status=cio_lock_inactive
      else
        lock_status=cio_lock_error
      endif
    endif
    status=CIO_OK  

    return
  end function cio_unlock_file

!----------------------- FILE SIZE --------------------

  subroutine cio_flsz_cu(unit,ext,off)
    integer,intent(in)           :: unit
    integer,intent(out)          :: ext,off

    call cio_flsz_cu_c(unit, ext, off)
    return
  end subroutine cio_flsz_cu

  subroutine cio_flsz_cn(fname,ext,off)
    character(len=*),intent(in)           :: fname
    integer,intent(out)                   :: ext,off
    character                             :: filename_c(FILENAME_LENGTH+1)
    integer                               :: temp
    temp = len_trim(fname)
    call cio_form_c_str(fname,filename_c,temp)
    call cio_flsz_cn_c(filename_c(1), ext, off)
    return
  end subroutine cio_flsz_cn

  subroutine cio_flsz_unit(unit,file_size)
    integer,intent(in)                    :: unit
    integer,intent(out),dimension(:)      :: file_size(2)

    call cio_flsz_cu(unit,file_size(1),file_size(2))
    return
  end subroutine cio_flsz_unit

  subroutine cio_flsz_unit_ll(unit,file_size)
    integer,intent(in)                    :: unit
    type(ll),intent(out)                  :: file_size
    integer,dimension(:)                  :: pos(2)

    integer                               :: status,extsize

    extsize = cio_get_file_ext_size(unit)
    status  = ll_set_maxintval(extsize)
    call cio_flsz_cu(unit,pos(1),pos(2))
    file_size = pos

  end subroutine cio_flsz_unit_ll

  subroutine cio_flsz_name(filename,file_size)
    character(len=*),intent(in)               :: filename
    integer,intent(out),dimension(:)          :: file_size(2)

    call cio_flsz_cn(filename, file_size(1), file_size(2))

    return
  end subroutine cio_flsz_name

  subroutine cio_flsz_name_ll(fname,file_size)
    character(len=*),intent(in)               :: fname
    type(ll),intent(out)                      :: file_size
    integer,dimension(:)                      :: pos(2)

    call cio_flsz_name(fname,pos)
    file_size = pos

  end subroutine cio_flsz_name_ll

!--------------------- TRUNCATE FILE LENGTH -------------------
  function cio_truncate_fname(filename,file_size) result (status)
    character(len=*),intent(in)           :: filename
    integer,intent(in), dimension(:)      :: file_size(2)
    integer                               :: status

    character                             :: filename_c(FILENAME_LENGTH+1)
    integer                               :: temp
    temp = len_trim(filename)

    if(len_trim(filename)>FILENAME_LENGTH) then
      status=CIO_ERROR
      return
    endif

    call cio_form_c_str(filename,filename_c,temp)
    status = cio_truncate_c(filename_c(1),file_size(1),file_size(2))

  end function cio_truncate_fname ! (fname,file_size) result (status)

  function cio_truncate_unit(unit,file_size) result (status)
    integer,intent(in)                    :: unit
    integer,intent(in)                    :: file_size(2)
    integer                               :: status

    character(len=FILENAME_LENGTH)        :: filename

    status = cio_finquire_private(unit,filename)
    if(status /= cio_ok) return

    status = cio_truncate_fname(filename,file_size)
    return
  end function cio_truncate_unit ! (unit,file_size) result (status)

  function cio_truncate_unit_ll(unit,file_size) result (status)
    integer,intent(in)                    :: unit
    type(ll),intent(in)                   :: file_size
    integer, dimension(:)                 :: pos(2)
    integer                               :: status

    integer                               :: extsize
    character(len=FILENAME_LENGTH)        :: filename
    character                             :: filename_c(FILENAME_LENGTH+1)
    integer                               :: temp

    status = cio_finquire_private(unit,filename)
    if(status /= cio_ok) return

    extsize = cio_get_file_ext_size(unit)
    if(extsize<=0) then
      status=CIO_ERROR
      return
    endif

    status  = ll_set_maxintval(extsize)
    pos = file_size
    temp = len_trim(filename)
    call cio_form_c_str(filename,filename_c,temp)
    status = cio_truncate_c(filename_c(1),pos(1),pos(2))

    return
  end function cio_truncate_unit_ll ! (unit,file_size) result (status)

!-------------------- CIO_SET_DIRECT_IO ---------------------
  function cio_set_direct_io(unit) result(status)
    integer , intent(in) :: unit
    integer              :: status

    status = cio_set_direct_io_c(unit)

    return
  end function cio_set_direct_io

!---------------------- CIO_FBACKSPACE ----------------------
  subroutine cio_fbackspace(unit)
    integer , intent(in) :: unit

    if(unit < cio_min_unit) then
      backspace(unit)
    else
      call cio_fbackspace_c(unit)
    endif

    return
  end subroutine cio_fbackspace

!-------------------- CIO_REWIND -----------
  subroutine cio_frewind(unit)
    integer , intent(in) :: unit

    if(unit < cio_min_unit) then
      rewind(unit)
    else
      call cio_frewind_c(unit)
    endif

    return
  end subroutine cio_frewind

!----------------- SET FILE REGION LOCK ------------------
  function cio_set_filrgnlock(unit,mode) result (status)
    integer,intent(in)                :: unit
    integer,intent(in)                :: mode
    integer                           :: status

!    status=cio_set_filrgnlock_c(unit,mode)    !disable
    status=CIO_OK
    return
  end function cio_set_filrgnlock

!------------------- SET WRITE ERROR RECOVERY ------------------
  function cio_set_wrt_err_rcvr(unit,mode) result(status)
    integer,intent(in)                :: unit
    integer,intent(in)                :: mode
    integer                           :: status

   status = cio_set_wrt_err_rcvr_c(unit,mode)
   status=CIO_OK
   return
  end function cio_set_wrt_err_rcvr

! execute a system command
  subroutine cio_system(command)
    character (len=*), intent(in) :: command

    character                     :: command_c(len(command)+1)
    integer                       :: temp

    temp = len_trim(command)
    call cio_form_c_str(command,command_c,temp)
    call cio_system_c(command_c(1))
    return
  end subroutine cio_system

! ------------ routines to provide cio_fread_c --------------------
  function cio_fread_char_ci (ptr, nbytes, unit) result (numread)
    character, intent(out)                  :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fread_char_c(ptr, nbytes, unit)
    return    
  end function cio_fread_char_ci

  function cio_fread_integer_ci (ptr, nbytes, unit) result (numread)
    integer, intent(out)                    :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fread_integer_c(ptr, nbytes, unit)
    return    
  end function cio_fread_integer_ci

  function cio_fread_real_ci (ptr, nbytes, unit) result (numread)
    real, intent(out)                       :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fread_real_c(ptr, nbytes, unit)
    return    
  end function cio_fread_real_ci

  function cio_fread_double_ci (ptr, nbytes, unit) result (numread)
    double precision, intent(out)           :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fread_double_c(ptr, nbytes, unit)
    return    
  end function cio_fread_double_ci

  function cio_fread_complex_ci (ptr, nbytes, unit) result (numread)
    complex, intent(out)                    :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread
    numread=cio_fread_complex_c(ptr, nbytes, unit)
    return    
  end function cio_fread_complex_ci

  function cio_fread_logical_ci (ptr, nbytes, unit) result (numread)
    logical, intent(out)                    :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fread_logical_c(ptr, nbytes, unit)
    return    
  end function cio_fread_logical_ci

! ------------ routines to provide cio_fwrite_c --------------------
  function cio_fwrite_char_ci (ptr, nbytes, unit) result (numread)
    character, intent(in)                   :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fwrite_char_c(ptr, nbytes, unit)
    return    
  end function cio_fwrite_char_ci

  function cio_fwrite_integer_ci (ptr, nbytes, unit) result (numread)
    integer, intent(in)                     :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fwrite_integer_c(ptr, nbytes, unit)
    return    
  end function cio_fwrite_integer_ci

  function cio_fwrite_real_ci (ptr, nbytes, unit) result (numread)
    real, intent(in)                        :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fwrite_real_c(ptr, nbytes, unit)
    return    
  end function cio_fwrite_real_ci

  function cio_fwrite_double_ci (ptr, nbytes, unit) result (numread)
    double precision, intent(in)            :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fwrite_double_c(ptr, nbytes, unit)
    return    
  end function cio_fwrite_double_ci

  function cio_fwrite_complex_ci (ptr, nbytes, unit) result (numread)
    complex, intent(in)                     :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread
    numread=cio_fwrite_complex_c(ptr, nbytes, unit)
    return    
  end function cio_fwrite_complex_ci

  function cio_fwrite_logical_ci (ptr, nbytes, unit) result (numread)
    logical, intent(in)                     :: ptr
    integer, intent(in)                     :: nbytes, unit
    integer                                 :: numread

    numread=cio_fwrite_logical_c(ptr, nbytes, unit)
    return    
  end function cio_fwrite_logical_ci

end module cio_module

! the following functions give c-language routines access to the locking
! provided by the cio module
      integer function cio_lock_file_f(ifile,lsec,ltype,lstat ) result(i_err)
      use cio_module
      use string_module
      integer,intent(in)   :: ifile(*)
      integer,intent(in)   :: lsec
      integer,intent(in)   :: ltype
      integer,intent(inout):: lstat

      integer              :: lock_type
      character(len=120)   :: file
      file = ' '
      call string_hh2cc(ifile,file)

      lock_type = ltype
      if(lock_type /= cio_extended_lock) lock_type = cio_normal_lock 
      i_err = cio_lock_file(file,lsec,lock_type,lstat)
      return
      end function cio_lock_file_f

      integer function cio_unlock_file_f(ifile,lstat) result(i_err)
      use cio_module
      use string_module
      integer,intent(in)   :: ifile(*)
      integer,intent(inout):: lstat

      character(len=120)   :: file
      file = ' '
      call string_hh2cc(ifile,file)
      i_err = cio_unlock_file(file, lstat)
      return
      end function cio_unlock_file_f

      integer function cio_normal_lock_f() result(ltype)
      use cio_module
      ltype = cio_normal_lock
      return
      end function cio_normal_lock_f

      integer function cio_extended_lock_f() result(ltype)
      use cio_module
      ltype = cio_extended_lock
      return
      end function cio_extended_lock_f

