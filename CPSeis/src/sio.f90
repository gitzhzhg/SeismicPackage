
!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- sio.f90 --------------------------------!!
!!----------------------------- sio.f90 --------------------------------!!
!!----------------------------- sio.f90 --------------------------------!!


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
! Name       : SIO         (simple file I/O)
! Category   : io
! Written    : 2000-03-13   by: Tom Stoeckley
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-20
! Purpose    : To read and write simple ascii or binary files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION               
!
! This primitive contains support for reading and writing simple ascii or
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
!</descript_doc>

 
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2000-10-20  Stoeckley  Add missing required documentation section.
!  1. 2000-03-13  Stoeckley  Initial version.
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
! Open, close, rewind, and backspace the file:
!
!                              o      i
!      call sio_open_read    (lun, filename)
!      call sio_open_write   (lun, filename)
!
!                              b 
!      call sio_close        (lun)
!
!                              i
!      call sio_rewind       (lun)
!      call sio_backspace    (lun)   ! for ascii card images only.
!
!
! Read and write an ascii card image:
!
!                              b    o 
!      call sio_read_card    (lun, card)
!      call sio_write_card   (lun, card)
!                              b    i 
!
!
! Read and write a binary record:
!
!      call sio_read_binary  (lun, scalar,          )
!      call sio_read_binary  (lun, array , nelements)
!      call sio_read_binary  (lun, string, nchar,   )
!                              b     o       i 
!
!      call sio_write_binary (lun, scalar,          )
!      call sio_write_binary (lun, array , nelements)
!      call sio_write_binary (lun, string, nchar,   )
!                              b     i       i 
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS               
!
! integer    lun       = logical unit number.
! char(*)    filename  = name of the file for input or output.
! char(*)    card      = null-terminated card image     to read or write.
! (any type) scalar    = a single scalar value          to read or write.
! (any type) array(:)  = an array of values             to read or write.
! char(*)    string    = character string               to read or write.
! integer    nelements = number of elements   in array  to read or write.
! integer    nchar     = number of characters in string to read or write.
!
! CARD should not have a null termination in a WRITE call.
! CARD will not have a null termination in a READ call.
!
! The types for SCALAR and ARRAY can be integer, real, double precision,
! or logical.
!
! LUN will always have one of the following values:
!  (1) A value >= 1 for a valid unit number for an opened file.
!  (2) SIO_ERROR (negative) if an open or read/write error occurred.
!  (3) SIO_EOF   (negative) if an end-of-file was encountered.
!  (4) SIO_ZERO  (zero) when trying to open an unspecified file.
!  (5) SIO_ZERO  (zero) after a call to close the file.
!  (6) SIO_ZERO  (zero) if ARRAY is too small in a read or write call.
!  (7) SIO_ZERO  (zero) if STRING is too small in a read or write call.
!
! SIO_ERROR and SIO_EOF have the same values as the corresponding error flags
! in the CIO primitive.
!
! LUN is never set to SIO_ZERO by calls to read or write card images or scalar
! values, and is never set to SIO_EOF by calls to open or write.
!
! FILENAME is considered unspecified if it would be converted to the named
! constant STRING_EMPTY by a call to STRING_VALIDATE.  This means that the
! file name begins with the character char(0), or is blank, or is set to a
! case-insensitive equivalent of the named constant STRING_EMPTY ('NONE').
!
! If an error occurs on a read, the subroutine output arguments CARD, SCALAR,
! ARRAY, or STRING will not be set.
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE OPERATIONS              
!
! Calls to open a file:
!  (1) The file is opened.
!  (2) LUN is set to a positive integer if the open is successful.
!  (3) LUN is set to SIO_ZERO if the file name is unspecified.
!  (4) LUN is set to SIO_ERROR if an open error occurs.
!
! Calls to read or write a card image or a binary record:
!  (1) The operation is performed if LUN > 0.
!  (2) Nothing is done if LUN <= 0.
!  (3) Nothing is done if NCHAR or NELEMENTS is zero.
!  (4) The file is closed and LUN is reset to SIO_ERROR if an error occurs.
!  (5) The file is closed and LUN is reset to SIO_EOF if an EOF is read.
!  (6) The file is closed and LUN is reset to SIO_ZERO if ARRAY is too small.
!  (7) The file is closed and LUN is reset to SIO_ZERO if STRING is too small.
!
! Calls to rewind or backspace the file:
!  (1) The operation is performed if LUN > 0.
!  (2) Nothing is done if LUN <= 0.
!
! Calls to close the file:
!  (1) The file is closed if LUN > 0.
!  (2) Nothing is done if LUN <= 0.
!  (3) LUN is reset to SIO_ZERO.
!
! Note that several calls to read or write card images or binary records
! can be made back-to-back without testing for an error (zero or negative LUN)
! until after the last call of the set has been made.  This works because,
! after an error occurs with one of the calls, any subsequent calls will know
! to do nothing because LUN is no longer positive.
!
! Warning: If NELEMENTS or NCHAR might vary for a given call, that value must
! be written to the file prior to the call to write the ARRAY or STRING so
! that the actual length on the file is read from the file before the call
! to read that array or string from the file.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module sio_module
      use cio_module
      use string_module
      use sizeof_module
      use swap_module
      implicit none
      public
      private :: sio_private_open
      private :: sio_private_return
      private :: sio_private_update

      character(len=100),public,save :: SIO_IDENT = &
       '$Id: sio.f90,v 1.2 2000/10/20 13:56:12 sps prod sps $'

      integer,public,parameter  :: SIO_ERROR = CIO_ERROR
      integer,public,parameter  :: SIO_EOF   = CIO_EOF
      integer,public,parameter  :: SIO_ZERO  = 0

      integer,private           :: isize,fsize,dsize,lsize
      logical,private           :: endian


!!--------------------------- interfaces ----------------------------------!!
!!--------------------------- interfaces ----------------------------------!!
!!--------------------------- interfaces ----------------------------------!!


      interface sio_read_binary
           module procedure sio_read_binary_cvar
           module procedure sio_read_binary_ivar
           module procedure sio_read_binary_fvar
           module procedure sio_read_binary_dvar
           module procedure sio_read_binary_lvar
           module procedure sio_read_binary_ivars
           module procedure sio_read_binary_fvars
           module procedure sio_read_binary_dvars
           module procedure sio_read_binary_lvars
      end interface

      interface sio_write_binary
           module procedure sio_write_binary_cvar
           module procedure sio_write_binary_ivar
           module procedure sio_write_binary_fvar
           module procedure sio_write_binary_dvar
           module procedure sio_write_binary_lvar
           module procedure sio_write_binary_ivars
           module procedure sio_write_binary_fvars
           module procedure sio_write_binary_dvars
           module procedure sio_write_binary_lvars
      end interface

      contains


!!------------------------ sio private open ------------------------------!!
!!------------------------ sio private open ------------------------------!!
!!------------------------ sio private open ------------------------------!!


      subroutine sio_private_open (lun,filename,rw)
      implicit none
      integer         ,intent(out)   :: lun               ! arguments
      character(len=*),intent(in)    :: filename          ! arguments
      character(len=*),intent(in)    :: rw                ! arguments
      integer                        :: isize_test        ! local
      real                           :: fsize_test        ! local
      double precision               :: dsize_test        ! local
      logical                        :: lsize_test        ! local
      character(len=16)              :: buffer            ! local

      isize  = sizeof(isize_test)
      fsize  = sizeof(fsize_test)
      dsize  = sizeof(dsize_test)
      lsize  = sizeof(lsize_test)
      endian = (swap_endian() == 1)

      buffer = filename
      call string_validate (buffer)
      if (buffer == STRING_EMPTY) then
           lun = SIO_ZERO
           return
      end if

      lun = cio_fopen (filename, rw)
      if (lun <= 0) lun = SIO_ERROR
      return
      end subroutine sio_private_open


!!--------------------------- sio open read ----------------------------!!
!!--------------------------- sio open read ----------------------------!!
!!--------------------------- sio open read ----------------------------!!
 
 
      subroutine sio_open_read (lun,filename)
      implicit none
      integer         ,intent(out)   :: lun             ! arguments
      character(len=*),intent(in)    :: filename        ! arguments

      call sio_private_open (lun,filename,'r')
      return
      end subroutine sio_open_read
 

!!--------------------------- sio open write ----------------------------!!
!!--------------------------- sio open write ----------------------------!!
!!--------------------------- sio open write ----------------------------!!
 
 
      subroutine sio_open_write (lun,filename)
      implicit none
      integer         ,intent(out)   :: lun             ! arguments
      character(len=*),intent(in)    :: filename        ! arguments

      call sio_private_open (lun,filename,'w')
      return
      end subroutine sio_open_write
 

!!------------------------ sio close ------------------------------!!
!!------------------------ sio close ------------------------------!!
!!------------------------ sio close ------------------------------!!


      subroutine sio_close (lun)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      integer                        :: stat            ! local

      if (lun > 0) stat = cio_fclose (lun)
      lun = SIO_ZERO
      return
      end subroutine sio_close


!!------------------------ sio rewind ------------------------------!!
!!------------------------ sio rewind ------------------------------!!
!!------------------------ sio rewind ------------------------------!!


      subroutine sio_rewind (lun)
      implicit none
      integer         ,intent(in)    :: lun             ! arguments

      if (lun > 0) call cio_frewind (lun)
      return
      end subroutine sio_rewind


!!------------------------ sio backspace ------------------------------!!
!!------------------------ sio backspace ------------------------------!!
!!------------------------ sio backspace ------------------------------!!


      subroutine sio_backspace (lun)
      implicit none
      integer         ,intent(in)    :: lun             ! arguments

      if (lun > 0) call cio_fbackspace (lun)
      return
      end subroutine sio_backspace


!!----------------------- sio private return ---------------------------!!
!!----------------------- sio private return ---------------------------!!
!!----------------------- sio private return ---------------------------!!


      function sio_private_return (lun,nsize,nelements) result (skip)
      implicit none
      integer         ,intent(inout) :: lun               ! arguments
      integer         ,intent(in)    :: nsize,nelements   ! arguments
      logical                        :: skip              ! result
      integer                        :: stat              ! local

      if (lun <= 0 .or. nelements == 0) then
           skip = .true.
      else if (nsize >= nelements) then
           skip = .false.
      else
           stat = cio_fclose (lun)
           lun  = SIO_ZERO
           skip = .true.
      end if
      return
      end function sio_private_return


!!----------------------- sio private update ---------------------------!!
!!----------------------- sio private update ---------------------------!!
!!----------------------- sio private update ---------------------------!!

! this routine is called only if no previous errors had occurred (lun >= 1).
! status = CIO_ERROR or CIO_EOF or nelements (>= 1) or 1.


      subroutine sio_private_update (lun,status)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      integer         ,intent(in)    :: status          ! arguments
      integer                        :: stat            ! local

      if (status >= 1) return
      stat = cio_fclose (lun)
      if (status == CIO_ERROR) then
           lun  = SIO_ERROR
      else if (status == CIO_EOF) then
           lun  = SIO_EOF
      else
           lun  = SIO_ERROR
      end if
      return
      end subroutine sio_private_update


!!-------------------------- sio read card -------------------------------!!
!!-------------------------- sio read card -------------------------------!!
!!-------------------------- sio read card -------------------------------!!


      subroutine sio_read_card (lun,card)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      character(len=*),intent(out)   :: card            ! arguments
      integer                        :: status          ! local
      integer                        :: temp

      if (lun <= 0) return
      temp = len(card)
      status = cio_fgetline (card,temp,lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_card


!!-------------------------- sio write card -------------------------------!!
!!-------------------------- sio write card -------------------------------!!
!!-------------------------- sio write card -------------------------------!!


      subroutine sio_write_card (lun,card)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      character(len=*),intent(in)    :: card            ! arguments
      integer                        :: status          ! local
      integer                        :: temp

      if (lun <= 0) return
      temp = len_trim(card)
      status = cio_fputline (card,temp,lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_card


!!-------------------------- sio read binary ------------------------------!!
!!-------------------------- sio read binary ------------------------------!!
!!-------------------------- sio read binary ------------------------------!!


      subroutine sio_read_binary_cvar (lun, string, nchar)
      implicit none
      integer         ,intent(inout) :: lun                    ! arguments
      character(len=*),intent(out)   :: string                 ! arguments
      integer         ,intent(in)    :: nchar                  ! arguments
      integer                        :: status                 ! local
      integer                        :: buffer(nchar/isize+2)  ! local
      integer                        :: temp
      temp = len(string)
      if (sio_private_return(lun,temp,nchar)) return
      status = cio_fread (buffer, nchar+1, 1, lun)
      if (status == 1) call string_hh2cc (buffer, string)
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_cvar



      subroutine sio_read_binary_ivar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      integer         ,intent(out)   :: scalar          ! arguments
      integer                        :: status          ! local

      if (lun <= 0) return
      status = cio_fread(scalar, isize, 1, lun)
      if (status == 1 .and. endian) call swap_bytes (scalar)
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_ivar



      subroutine sio_read_binary_ivars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      integer         ,intent(out)   :: array(:)        ! arguments
      integer         ,intent(in)    :: nelements       ! arguments
      integer                        :: status          ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      status = cio_fread(array, isize, nelements, lun)
      if (status == nelements .and. endian) then
           call swap_bytes (array(1:nelements))
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_ivars



      subroutine sio_read_binary_fvar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      real            ,intent(out)   :: scalar          ! arguments
      integer                        :: status          ! local

      if (lun <= 0) return
      status = cio_fread(scalar, fsize, 1, lun)
      if (status == 1 .and. endian) call swap_bytes (scalar)
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_fvar



      subroutine sio_read_binary_fvars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      real            ,intent(out)   :: array(:)        ! arguments
      integer         ,intent(in)    :: nelements       ! arguments
      integer                        :: status          ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      status = cio_fread(array, fsize, nelements, lun)
      if (status == nelements .and. endian) then
           call swap_bytes (array(1:nelements))
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_fvars



      subroutine sio_read_binary_dvar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      double precision,intent(out)   :: scalar          ! arguments
      integer                        :: status          ! local

      if (lun <= 0) return
      status = cio_fread(scalar, dsize, 1, lun)
      if (status == 1 .and. endian) call swap_bytes (scalar)
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_dvar



      subroutine sio_read_binary_dvars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      double precision,intent(out)   :: array(:)        ! arguments
      integer         ,intent(in)    :: nelements       ! arguments
      integer                        :: status          ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      status = cio_fread(array, dsize, nelements, lun)
      if (status == nelements .and. endian) then
           call swap_bytes (array(1:nelements))
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_dvars


      subroutine sio_read_binary_lvar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      logical         ,intent(out)   :: scalar          ! arguments
      integer                        :: status          ! local

      if (lun <= 0) return
      status = cio_fread(scalar, lsize, 1, lun)
      if (status == 1 .and. endian) call swap_unk (scalar, lsize)
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_lvar



      subroutine sio_read_binary_lvars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      logical         ,intent(out)   :: array(:)        ! arguments
      integer         ,intent(in)    :: nelements       ! arguments
      integer                        :: status          ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      status = cio_fread(array, lsize, nelements, lun)
      if (status == nelements .and. endian) then
           call swap_unk_cvec (array(1), lsize, nelements)
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_read_binary_lvars


!!------------------------ sio write binary ------------------------------!!
!!------------------------ sio write binary ------------------------------!!
!!------------------------ sio write binary ------------------------------!!


      subroutine sio_write_binary_cvar (lun, string, nchar)
      implicit none
      integer         ,intent(inout) :: lun                    ! arguments
      character(len=*),intent(in)    :: string                 ! arguments
      integer         ,intent(in)    :: nchar                  ! arguments
      integer                        :: status                 ! local
      integer                        :: buffer(nchar/isize+2)  ! local
      integer                        :: temp

      temp = len(string)
      if (sio_private_return(lun,temp,nchar)) return
      call string_cc2hh (string, buffer)
      status = cio_fwrite(buffer, nchar+1, 1, lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_cvar



      subroutine sio_write_binary_ivar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      integer         ,intent(in)    :: scalar          ! arguments
      integer                        :: scalar2         ! local
      integer                        :: status          ! local

      if (lun <= 0) return
      scalar2 = scalar
      if (endian) call swap_bytes (scalar2)
      status = cio_fwrite(scalar2, isize, 1, lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_ivar



      subroutine sio_write_binary_ivars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun                 ! arguments
      integer         ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      integer                        :: array2(nelements)   ! local
      integer                        :: status              ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      if (endian) then
           array2(:) = array(1:nelements)
           call swap_bytes (array2)
           status = cio_fwrite(array2, isize, nelements, lun)
      else
           status = cio_fwrite(array, isize, nelements, lun)
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_ivars



      subroutine sio_write_binary_fvar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      real            ,intent(in)    :: scalar          ! arguments
      real                           :: scalar2         ! local
      integer                        :: status          ! local

      if (lun <= 0) return
      scalar2 = scalar
      if (endian) call swap_bytes (scalar2)
      status = cio_fwrite(scalar2, fsize, 1, lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_fvar



      subroutine sio_write_binary_fvars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun                 ! arguments
      real            ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      real                           :: array2(nelements)   ! local
      integer                        :: status              ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      if (endian) then
           array2(:) = array(1:nelements)
           call swap_bytes (array2)
           status = cio_fwrite(array2, fsize, nelements, lun)
      else
           status = cio_fwrite(array, fsize, nelements, lun)
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_fvars



      subroutine sio_write_binary_dvar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      double precision,intent(in)    :: scalar          ! arguments
      double precision               :: scalar2         ! local
      integer                        :: status          ! local

      if (lun <= 0) return
      scalar2 = scalar
      if (endian) call swap_bytes (scalar2)
      status = cio_fwrite(scalar2, dsize, 1, lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_dvar



      subroutine sio_write_binary_dvars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun                 ! arguments
      double precision,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      double precision               :: array2(nelements)   ! local
      integer                        :: status              ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      if (endian) then
           array2(:) = array(1:nelements)
           call swap_bytes (array2)
           status = cio_fwrite(array2, fsize, nelements, lun)
      else
           status = cio_fwrite(array, fsize, nelements, lun)
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_dvars


      subroutine sio_write_binary_lvar (lun, scalar)
      implicit none
      integer         ,intent(inout) :: lun             ! arguments
      logical         ,intent(in)    :: scalar          ! arguments
      logical                        :: scalar2         ! local
      integer                        :: status          ! local

      if (lun <= 0) return
      scalar2 = scalar
      if (endian) call swap_unk (scalar2, lsize)
      status = cio_fwrite(scalar2, lsize, 1, lun)
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_lvar



      subroutine sio_write_binary_lvars (lun, array, nelements)
      implicit none
      integer         ,intent(inout) :: lun                 ! arguments
      logical         ,intent(in)    :: array(:)            ! arguments
      integer         ,intent(in)    :: nelements           ! arguments
      logical                        :: array2(nelements)   ! local
      integer                        :: status              ! local
      integer                        :: temp

      temp = size(array)
      if (sio_private_return(lun,temp,nelements)) return
      if (endian) then
           array2(:) = array(1:nelements)
           call swap_unk_cvec (array2(1), lsize, nelements)
           status = cio_fwrite(array2, lsize, nelements, lun)
      else
           status = cio_fwrite(array, lsize, nelements, lun)
      end if
      call sio_private_update (lun,status)
      return
      end subroutine sio_write_binary_lvars


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module sio_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
