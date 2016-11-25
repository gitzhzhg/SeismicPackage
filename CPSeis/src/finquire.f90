
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ finquire.f90 -----------------------------!!
!!------------------------------ finquire.f90 -----------------------------!!
!!------------------------------ finquire.f90 -----------------------------!!

                   ! other files are:  finquire_crou.c
 

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
! Name       : FINQUIRE 
! Category   : io
! Written    : 1999-11-02   by: Tom Stoeckley
! Revised    : 2004-08-23   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Fortran-callable primitive to inquire about a disk file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Inquire about the status of a disk file to learn whether
! the file exists, is readable, is writeable, etc.
!
! This primitive calls the INQUIRE C-language primitive.
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
!                                                 opt    opt
!               o                          i       o      i
!             status = finquire_file   (filename, msg, quickly)
!             status = finquire_input  (filename, msg, quickly)
!             status = finquire_output (filename, msg, quickly)
!
!             long   = finquire_fetch_message()
!             brief  = finquire_fetch_brief_message()
!               o
!
! character(len=*)    filename = name of the file.
! integer               status = the status of the file (see below).
! character(len=*)         msg = message describing the status of the file.
! character(len=*)        long = message describing the status of the file.
! character(len=*)       brief = brief message describing status of the file.
! logical              quickly = performs quick inquire if present and true.
!
! LONG is the last MSG returned (or potentially returned) by the last call
! to one of the first three functions above.
!
! BRIEF is a short version of the last MSG returned (or potentially returned)
! by the last call to one of the first three functions above.  This can be
! useful if the message will be displayed in a gui beside the file name,
! where there is no space for a long message, as for example when an array of
! file names is displayed in a column.
!
!-------------------------------------------------------------------------------
! If QUICKLY is missing or false, a full inquiry is made (following links
! if necessary):
!
! status (first function)  description
! -----------------------  -----------
! FINQUIRE_BLANK           filename is not specified.
! FINQUIRE_NOT_FOUND       file does not exist but is writeable.
! FINQUIRE_NOT_CREATEABLE  file does not exist and is not writeable.
! FINQUIRE_FOUND           file exists and is readable and writeable.
! FINQUIRE_NOT_READABLE    file exists but is not readable (but is writeable).
! FINQUIRE_NOT_WRITEABLE   file exists but is not writeable (but is readable).
! FINQUIRE_NOT_READ_WRITE  file exists but is not readable or writeable.
!
! input status             description
! ------------             -----------
! FINQUIRE_FOUND           input file exists and can be read.
! FINQUIRE_ERROR           any other condition.
!
! output status            description
! -------------            -----------
! FINQUIRE_FOUND           output file exists and can be overwritten.
! FINQUIRE_NOT_FOUND       output file does not exist and can be created.
! FINQUIRE_ERROR           any other condition.
!
!-------------------------------------------------------------------------------
! If QUICKLY is present and true, links are not followed (to circumvent
! long delays due to automount errors):
!
! status (first function)  description
! -----------------------  -----------
! FINQUIRE_BLANK           filename is not specified.
! FINQUIRE_NOT_FOUND       file does not exist.
! FINQUIRE_FOUND           file exists.
!
! input status             description
! ------------             -----------
! FINQUIRE_FOUND           input file exists.
! FINQUIRE_ERROR           any other condition.
!
! output status            description
! -------------            -----------
! FINQUIRE_FOUND           output file exists.
! FINQUIRE_NOT_FOUND       output file does not exist.
! FINQUIRE_ERROR           any other condition.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2004-08-23  Stoeckley    Add optional argument QUICKLY.
!  4. 2003-06-10  Stoeckley    Add finquire_fetch_message and
!                               finquire_fetch_brief_message.
!  3. 2000-10-19  Stoeckley    Add optional MSG argument to each routine.
!  2. 1999-11-17  Stoeckley    Add ident string for RCS.
!  1. 1999-11-02  Stoeckley    Initial version.
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


      module finquire_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: FINQUIRE_IDENT = &
       '$Id: finquire.f90,v 1.5 2004/08/23 13:15:25 Stoeckley prod sps $'

      integer,parameter,public :: FINQUIRE_BLANK          =  1
      integer,parameter,public :: FINQUIRE_NOT_FOUND      =  2
      integer,parameter,public :: FINQUIRE_NOT_CREATEABLE =  3
      integer,parameter,public :: FINQUIRE_FOUND          =  4
      integer,parameter,public :: FINQUIRE_NOT_READABLE   =  5
      integer,parameter,public :: FINQUIRE_NOT_WRITEABLE  =  6
      integer,parameter,public :: FINQUIRE_NOT_READ_WRITE =  7
      integer,parameter,public :: FINQUIRE_ERROR          = 41

!!! Note: The above parameters must have values which match the
!!!       equivalent defined constants in the inquire.h header file.


      contains


!!-------------------- finquire fetch message -------------------------!!
!!-------------------- finquire fetch message -------------------------!!
!!-------------------- finquire fetch message -------------------------!!


      function finquire_fetch_message () result (long)
      implicit none
      character(len=80)            :: long           ! result
      integer                      :: buffer(50)     ! local

      call finquire_crou_fetch_msg (buffer)

      call string_hh2cc (buffer,long)
      return
      end function finquire_fetch_message


!!-------------------- finquire fetch brief message -------------------------!!
!!-------------------- finquire fetch brief message -------------------------!!
!!-------------------- finquire fetch brief message -------------------------!!


      function finquire_fetch_brief_message () result (brief)
      implicit none
      character(len=80)            :: brief          ! result
      integer                      :: buffer(50)     ! local

      call finquire_crou_fetch_brief_msg (buffer)

      call string_hh2cc (buffer,brief)
      return
      end function finquire_fetch_brief_message


!!---------------------------- finquire file -------------------------------!!
!!---------------------------- finquire file -------------------------------!!
!!---------------------------- finquire file -------------------------------!!


      function finquire_file (filename,msg,quickly) result (status)
      implicit none
      character(len=*),intent(in)           :: filename       ! arguments
      character(len=*),intent(out),optional :: msg            ! arguments
      logical         ,intent(in) ,optional :: quickly        ! arguments
      integer                               :: status         ! result
      integer                               :: buffer(50)     ! local
      integer                               :: buffer2(50)    ! local
      integer                :: finquire_crou_file            ! external
      integer                :: finquire_crou_file_quickly    ! external

      call string_cc2hh (filename,buffer)

      if (.not.present(quickly)) then
           status = finquire_crou_file         (buffer,buffer2)
      else if (quickly) then
           status = finquire_crou_file_quickly (buffer,buffer2)
      else
           status = finquire_crou_file         (buffer,buffer2)
      end if

      if (present(msg)) call string_hh2cc (buffer2,msg)
      return
      end function finquire_file


!!---------------------------- finquire input ------------------------------!!
!!---------------------------- finquire input ------------------------------!!
!!---------------------------- finquire input ------------------------------!!


      function finquire_input (filename,msg,quickly) result (status)
      implicit none
      character(len=*),intent(in)           :: filename       ! arguments
      character(len=*),intent(out),optional :: msg            ! arguments
      logical         ,intent(in) ,optional :: quickly        ! arguments
      integer                               :: status         ! result
      integer                               :: buffer(50)     ! local
      integer                               :: buffer2(50)    ! local
      integer                :: finquire_crou_input           ! external
      integer                :: finquire_crou_input_quickly   ! external

      call string_cc2hh (filename,buffer)

      if (.not.present(quickly)) then
           status = finquire_crou_input         (buffer,buffer2)
      else if (quickly) then
           status = finquire_crou_input_quickly (buffer,buffer2)
      else
           status = finquire_crou_input         (buffer,buffer2)
      end if

      if (present(msg)) call string_hh2cc (buffer2,msg)
      return
      end function finquire_input


!!---------------------------- finquire output -----------------------------!!
!!---------------------------- finquire output -----------------------------!!
!!---------------------------- finquire output -----------------------------!!


      function finquire_output (filename,msg,quickly) result (status)
      implicit none
      character(len=*),intent(in)           :: filename       ! arguments
      character(len=*),intent(out),optional :: msg            ! arguments
      logical         ,intent(in) ,optional :: quickly        ! arguments
      integer                               :: status         ! result
      integer                               :: buffer(50)     ! local
      integer                               :: buffer2(50)    ! local
      integer                :: finquire_crou_output          ! external
      integer                :: finquire_crou_output_quickly  ! external

      call string_cc2hh (filename,buffer)

      if (.not.present(quickly)) then
           status = finquire_crou_output         (buffer,buffer2)
      else if (quickly) then
           status = finquire_crou_output_quickly (buffer,buffer2)
      else
           status = finquire_crou_output         (buffer,buffer2)
      end if

      if (present(msg)) call string_hh2cc (buffer2,msg)
      return
      end function finquire_output


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module finquire_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

