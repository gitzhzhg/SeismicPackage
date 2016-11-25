!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- floatio.f90 -------------------------------!!
!!--------------------------- floatio.f90 -------------------------------!!
!!--------------------------- floatio.f90 -------------------------------!!


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
! Name       : FLOATIO
! Category   : io
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : To read/write self defining files with floating point data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading and writing generic self defining
! files which contain columns of floating point (or double precision) numbers.
!
! Files being written cannot contain any delimiters.
! Binary files being read cannot contain any delimiters.
! Ascii or hybrid files being read may contain delimiters, but they will
! be read as if there were no delimiters.
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
!                         OPEN AND CLOSE THE FILE            
!
! Open the file and read or write initial information:
!
!                                o     i      b      i     o   o
!    call floatio_open_read    (obj,filename,pjar,secname,err,msg)
!    call floatio_open_write   (obj,filename,pjar,secname,err,msg)
!    call floatio_open_foreign (obj,filename,pjar,secname,err,msg)
!                                o     i      b      i     o   o
!
! Verify or augment parameters:
!
!                           i      i     o   o
!    call floatio_verify  (pjar,secname,err,msg)
!    call floatio_augment (pjar,secname)
!                           b      i
!
! Close the file:
!
!                         b
!    call floatio_close (obj)
!
!-------------------------------------------------------------------------------
!                       READ AND WRITE THE DATA              
!
! Read or write data (one line at a time) when encoding is ascii or hybrid:
!
!                              b   o   o    o
!    call floatio_read_line  (obj,err,msg,vline)         ! for each line.
!    call floatio_write_line (obj,err,msg,vline)         ! for each line.
!                              b   o   o    i   
!
! Read or write data (one column at a time) when encoding is binary:
!
!                                       b 
!    call floatio_before_read_binary  (obj)
!    call floatio_before_write_binary (obj,nlines)
!                                       b    i
!
!                                b    i       o
!    call floatio_read_binary  (obj,column,vcolumn)  ! for each desired column.
!    call floatio_write_binary (obj,column,vcolumn)  ! for each column.
!                                b    i       i
!
!                                       b   o   o
!    call floatio_after_read_binary   (obj,err,msg)
!    call floatio_after_write_binary  (obj,err,msg)
!                                       b   o   o
!
! Read or write data (all data at once) for any encoding format:
!
!                              b   o   o    o
!    call floatio_read_data  (obj,err,msg,vdata)
!    call floatio_write_data (obj,err,msg,vdata,nlines)
!                              b   o   o    i     i
!
!-------------------------------------------------------------------------------
!                     EASY ASCII FILE READ AND WRITE
!
! These alternatives have the following simplifications and restrictions:
!  (1) They do not require the user to deal with the pickle jar.
!  (2) They read only ascii or hybrid self defining files and foreign files.
!  (3) They write only ascii self defining files.
!  (4) Any header information in a foreign file must start with a pound sign.
!  (5) Only the first section in a self defining file will be read.
!  (6) No history section is read or written.
!
! Open the file:
!                              o     i       o       o      o   o
!    call floatio_easy_read  (obj,filename,nlines,ncolumns,err,msg,
!                             firstline,nilstring)
!                                 i         i
!                                opt       opt
!
!                              o     i        i      o   o
!    call floatio_easy_write (obj,filename,ncolumns,err,msg,
!                             noheaders,nilstring)
!                                 i         i
!                                opt       opt
!
! Read or write a line:
!
!                              b   o   o    o
!    call floatio_read_line  (obj,err,msg,vline)         ! for each line.
!    call floatio_write_line (obj,err,msg,vline)         ! for each line.
!                              b   o   o    i   
!
! Close the file:
!
!                              b
!    call floatio_close      (obj)
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(floatio_struct)  obj = pointer to the FLOATIO data structure.
! type(pjar_struct)    pjar = pointer to the pickle jar data structure.
!
! char(*) filename  = name of the static file for input or output.
! char(*) secname   = name of the section on the file to read or write.
! integer err       = error flag (returned).
! char(*) msg       = message for possible printing (returned).
! integer ncolumns  = number of columns of numbers on the file.
! integer nlines    = number of lines of numbers on the file.
! integer firstline = first line of foreign file to read (default 1).
! char(*) nilstring = symbol for nil value (default "nil").
! logical noheaders = true to inhibit writing header info (default false).
!
! real    vline  (ncolumns)      = values from all columns in one line of data.
! real    vcolumn(nlines)        = values from all lines in one column of data.
! real    vdata(nlines,ncolumns) = all data values in the data.
! integer column                 = column number to read or write.
!
! FLOATIO_EASY_READ:
!   FIRSTLINE and NILSTRING are ignored unless the file is a foreign file.
!
! VLINE, VCOLUMN, and VDATA can also be double precision.
!
! NCOLUMNS and NLINES are pickle jar parameters.
!
! If VLINE(NCOLUMNS) or VDATA(NPICKS,NCOLUMNS) has its NCOLUMNS dimension
! smaller than required (while reading), the excess columns will not be
! placed into the array.
!
!-------------------------------------------------------------------------------
!                        RETURNED ERROR FLAGS
!
! The returned error will have one of these integer named constant values:
!
!          err              description
!          ---              -----------
!          FLOATIO_OK       the operation was successful.
!          FLOATIO_ERROR    an open or read/write error occurred.
!          FLOATIO_EOF      an end-of-file was detected.
!
!-------------------------------------------------------------------------------
!                          ENCODING FORMATS
!
! The encoding format of the file must be one of these character(len=8)
! named constant values:
!
!     encoding         description
!     --------         -----------
!     FLOATIO_ASCII    CPS self-defining ascii file.
!     FLOATIO_HYBRID   CPS self-defining hybrid (columnar) binary file.
!     FLOATIO_BINARY   CPS self-defining blocked binary file.
!
! The default value of ENCODING for output is FLOATIO_ASCII.
!
!-------------------------------------------------------------------------------
!                           PJAR PARAMETERS
!
! integer ncolumns = number of columns of numbers on the file.
! integer nlines   = number of lines of numbers on the file.
!
! char(*) encoding  = encoding format of the file.
! char(*) nilstring = string for nil values in the file (default 'nil').
! integer wrap      = number of card images per record (normally 1).
! integer firstline = line to start reading on (for foreign files only).
! char(*) template  = template for adjusting foreign input card image.
! logical noheaders = true forces elimination of output ascii header sections.
!
! char(*) fields    (ncolumns) = list of field names for each column.
! integer hdrs      (ncolumns) = list of header words for each column.
! char(*) fieldtypes(ncolumns) = list of field types for each column.
! char(*) units     (ncolumns) = list of units for each column.
! integer widths    (ncolumns) = list of widths to write for each column.
! integer maxchars  (ncolumns) = max number of chars in each column to decode.
! integer decimals  (ncolumns) = list of decimals to write for each column.
!
!   parameter                open_read         open_write       open_foreign
!   ---------                ---------         ----------       ------------
!   (history cards)          returned          optional         irrelevant
!
!   ncolumns                 returned          required         required
!   nlines                   returned          irrelevant       irrelevant
!
!   encoding                 returned          optional         required
!   nilstring                returned          optional         optional
!   wrap                     returned          optional         optional
!   firstline                not present       irrelevant       optional
!   template                 not present       irrelevant       optional
!   noheaders                not present       optional         irrelevant
!
!   fields    (ncolumns)     returned          optional         irrelevant
!   hdrs      (ncolumns)     returned          optional         irrelevant
!   fieldtypes(ncolumns)     returned          optional         irrelevant
!   units     (ncolumns)     returned          optional         irrelevant
!   widths    (ncolumns)     returned          optional         irrelevant
!   maxchars  (ncolumns)     returned          optional         optional
!   decimals  (ncolumns)     not present       optional         irrelevant
!
! NILSTRING and WRAP are always irrelevant for hybrid and binary encoding.
!
! FIRSTLINE, TEMPLATE, and MAXCHARS are needed only when reading a truly
! foreign file (ascii encoding) without Conoco self defining information.
!
! Any floating point columns in any ascii or hybrid (including truly foreign)
! files can be read, regardless of whether any columns are delimiters.
!
! Binary format files cannot be read if they contain any delimiters.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                
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
!009. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!  8. 2005-10-24  Stoeckley  Use double precision VLINE instead of real in
!                             floatio_easy_read.
!  7. 2004-08-23  Stoeckley  Add optional argument NOHEADERS.
!  6. 2004-06-08  Stoeckley  Add optional arguments NILSTRING and FIRSTLINE.
!  5. 2004-03-17  Stoeckley  Add ability to set the number of decimals.
!  4. 2003-12-09  Stoeckley  Add double precision option.
!  3. 2002-08-19  Stoeckley  Add floatio_easy_read and floatio_easy_write.
!  2. 2002-04-11  Stoeckley  Add calls to FIOUTIL_VERIFY1 and FIOUTIL_VERIFY3
!                             to support workstation programs; remove unneeded
!                             SKIP argument and associated code.
!  1. 2002-02-04  Stoeckley  Initial version.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module floatio_module
      use pjar_module
      use dio_module
      use fio_module
      use fioutil_module
      use string_module
      use named_constants_module
      implicit none
      public
 
      character(len=100),public,save :: FLOATIO_IDENT = &
'$Id: floatio.f90,v 1.9 2006/09/18 13:32:47 Glover prod sps $'

      integer,public,parameter          :: FLOATIO_OK      = FIO_OK
      integer,public,parameter          :: FLOATIO_ERROR   = FIO_ERROR
      integer,public,parameter          :: FLOATIO_EOF     = FIO_EOF  
 
      character(len=8),public,parameter  :: FLOATIO_ASCII   = FIO_ASCII
      character(len=8),public,parameter  :: FLOATIO_BINARY  = FIO_BINARY
      character(len=8),public,parameter  :: FLOATIO_HYBRID  = FIO_HYBRID

      character(len=20),private,parameter :: DEFAULT_FILETYPE = 'floatio'
      character(len=20),private,parameter :: DEFAULT_SECNAME  = 'floatio'

      type,public :: floatio_struct
           private
           type(dio_struct)       ,pointer :: dio
           type(fio_struct)       ,pointer :: fio
           character(len=8)                :: encoding
           integer                         :: ncolumns,nlines
           integer                ,pointer :: widths(:),decimals(:)
      end type floatio_struct

      interface floatio_read_line
        module procedure floatio_read_line_real 
        module procedure floatio_read_line_double
      end interface

      interface floatio_write_line
        module procedure floatio_write_line_real 
        module procedure floatio_write_line_double
      end interface

      interface floatio_read_binary
        module procedure floatio_read_binary_real 
        module procedure floatio_read_binary_double
      end interface

      interface floatio_write_binary
        module procedure floatio_write_binary_real 
        module procedure floatio_write_binary_double 
      end interface

      interface floatio_read_data
        module procedure floatio_read_data_real 
        module procedure floatio_read_data_double
      end interface

      interface floatio_write_data
        module procedure floatio_write_data_real 
        module procedure floatio_write_data_double
      end interface

      contains


!!--------------------------- floatio augment ---------------------------!!
!!--------------------------- floatio augment ---------------------------!!
!!--------------------------- floatio augment ---------------------------!!

! called before writing a file to disk.
! pickle jar contents are modified.


      subroutine floatio_augment (pjar,secname)

      type(pjar_struct),intent(inout) :: pjar                ! arguments
      character(len=*) ,intent(in)    :: secname             ! arguments
      integer                         :: ncolumns,i          ! local

      call fioutil_augment1 (pjar, secname, FLOATIO_ASCII)

      call pjar_get (pjar,'ncolumns' , ncolumns , 0 )
      call pjar_put (pjar,'vartypes' , (/('F',i=1,ncolumns)/) , ncolumns)

      call fioutil_augment3 (pjar)

      end subroutine floatio_augment


!!--------------------------- floatio verify ---------------------------!!
!!--------------------------- floatio verify ---------------------------!!
!!--------------------------- floatio verify ---------------------------!!

! called after reading file header sections from disk.
! called before using the the pickle jar to read a data section from disk.
! called before writing a file to disk.
! pickle jar contents are verified but not changed.


      subroutine floatio_verify (pjar,secname,err,msg)

      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      integer          ,intent(out)   :: err                ! arguments
      character(len=*) ,intent(out)   :: msg                ! arguments

      call fioutil_verify1 (pjar,secname,msg)
      if(msg /= ' ') then
          err = FLOATIO_ERROR
          return
      endif

      call fioutil_verify3 (pjar,msg)
      if(msg /= ' ') then
          err = FLOATIO_ERROR
          return
      endif

      call fioutil_verify (pjar, secname, msg)
      if (msg /= ' ') then
           err = FLOATIO_ERROR
      else
           err = FLOATIO_OK
      endif

      end subroutine floatio_verify


!!--------------------------- floatio close -----------------------------!!
!!--------------------------- floatio close -----------------------------!!
!!--------------------------- floatio close -----------------------------!!


      subroutine floatio_close (obj)

      type(floatio_struct),pointer :: obj             ! arguments

      if (associated(obj)) then
           if (associated(obj%widths  )) deallocate (obj%widths)
           if (associated(obj%decimals)) deallocate (obj%decimals)
           call fio_delete (obj%fio)
           call dio_close  (obj%dio)
           deallocate      (obj)
      endif

      end subroutine floatio_close


!!---------------------- floatio easy read ---------------------------------!!
!!---------------------- floatio easy read ---------------------------------!!
!!---------------------- floatio easy read ---------------------------------!!


      subroutine floatio_easy_read (obj,filename,nlines,ncolumns,err,msg, &
                                    firstline,nilstring)

      type(floatio_struct)     ,pointer     :: obj                 ! arguments
      character(len=*)         ,intent(in)  :: filename            ! arguments
      integer                  ,intent(out) :: nlines,ncolumns     ! arguments
      integer                  ,intent(out) :: err                 ! arguments
      character(len=*)         ,intent(out) :: msg                 ! arguments
      integer         ,optional,intent(in)  :: firstline           ! arguments
      character(len=*),optional,intent(in)  :: nilstring           ! arguments
      type(pjar_struct)        ,pointer     :: pjar                ! local
      character(len=40)                     :: secname             ! local
      integer,parameter                     :: MAXCOLUMNS = 50     ! local
      double precision                      :: vline (MAXCOLUMNS)  ! local
      integer                               :: indx                ! local

!----------get started:

      allocate (obj)
      nullify  (obj%fio)
      nullify  (obj%dio)
      nullify  (obj%widths)
      nullify  (obj%decimals)
      obj%encoding = 'unset'
      obj%nlines   = 0
      obj%ncolumns = 0
      nlines       = 0
      ncolumns     = 0
      nullify (pjar) ! jpa
      call pjar_create (pjar)

!----------open input file:

      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= FLOATIO_OK) return

!----------read header sections:

      call fio_create               (obj%fio,obj%dio)
      call fio_read_header_sections (obj%fio,pjar,err,msg)
      if (err /= FLOATIO_OK) then
           call pjar_delete (pjar)
           return
      endif

      call pjar_choose_section (pjar,1)
      call pjar_get_secname    (pjar,secname)

      if (secname == 'FOREIGN') then
                                call pjar_put (pjar, 'ncolumns' , MAXCOLUMNS)
                                call pjar_put (pjar, 'firstline', 1)
                                call pjar_put (pjar, 'nilstring', 'nil')
        if (present(firstline)) call pjar_put (pjar, 'firstline', firstline)
        if (present(nilstring)) call pjar_put (pjar, 'nilstring', nilstring)
                                call floatio_augment (pjar, secname)
      else
                                call pjar_get (pjar, 'nlines'  , obj%nlines)
                                call pjar_get (pjar, 'ncolumns', obj%ncolumns)
      endif

      call floatio_verify      (pjar, secname, err, msg)
      if (err /= FLOATIO_OK) then
           call pjar_delete (pjar)
           return
      endif

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)

      if (obj%encoding /= FIO_ASCII .and. obj%encoding /= FIO_HYBRID) then
           err = FLOATIO_ERROR
           msg = 'encoding must be ascii or hybrid for floatio_easy_read'
           call pjar_delete (pjar)
           return
      endif

!----------read through foreign file to get NLINES and NCOLUMNS:

      if (secname == 'FOREIGN') then
           call fio_read_data_section (obj%fio,pjar,secname,err,msg)
           if (err == FLOATIO_ERROR) then
                call pjar_delete (pjar)
                return
           endif
           obj%nlines   = 0
           obj%ncolumns = MAXCOLUMNS
           vline(:)     = DNIL
           do
                call floatio_read_line (obj,err,msg,vline(1:obj%ncolumns))
                if (err == FLOATIO_ERROR) then
                     call pjar_delete (pjar)
                     return
                endif
                if (err == FLOATIO_EOF) exit
                obj%nlines = obj%nlines + 1
                if (obj%nlines == 1) then
                     obj%ncolumns = 0
                     do indx = 1,MAXCOLUMNS
                          if (vline(indx) /= DNIL) obj%ncolumns = indx
                     enddo
                endif
           enddo
           call pjar_choose_section (pjar,secname)
           call pjar_put            (pjar, 'nlines'  , obj%nlines)
           call pjar_put            (pjar, 'ncolumns', obj%ncolumns)
      endif

!----------prepare to read data section:

      call fio_read_data_section (obj%fio,pjar,secname,err,msg)

      nlines   = obj%nlines  
      ncolumns = obj%ncolumns
      call pjar_delete (pjar)

      end subroutine floatio_easy_read


!!---------------------- floatio easy write ---------------------------------!!
!!---------------------- floatio easy write ---------------------------------!!
!!---------------------- floatio easy write ---------------------------------!!


      subroutine floatio_easy_write (obj,filename,ncolumns,err,msg, &
                                     noheaders,nilstring)

      type(floatio_struct)     ,pointer     :: obj                 ! arguments
      character(len=*)         ,intent(in)  :: filename            ! arguments
      integer                  ,intent(in)  :: ncolumns            ! arguments
      integer                  ,intent(out) :: err                 ! arguments
      character(len=*)         ,intent(out) :: msg                 ! arguments
      logical         ,optional,intent(in)  :: noheaders           ! arguments
      character(len=*),optional,intent(in)  :: nilstring           ! arguments
      type(pjar_struct)        ,pointer     :: pjar                ! local
      character(len=40)        ,parameter   :: secname = 'floatio' ! local

      nullify (pjar) ! jpa
      call pjar_create         (pjar)
      call pjar_choose_section (pjar,secname)

                              call pjar_put (pjar, 'encoding' , FIO_ASCII)
                              call pjar_put (pjar, 'ncolumns' , ncolumns)
      if (present(noheaders)) call pjar_put (pjar, 'noheaders', noheaders)
      if (present(nilstring)) call pjar_put (pjar, 'nilstring', nilstring)

      call floatio_open_write  (obj,filename,pjar,secname,err,msg)
      call pjar_delete         (pjar)

      end subroutine floatio_easy_write


!!---------------------- floatio open read ---------------------------------!!
!!---------------------- floatio open read ---------------------------------!!
!!---------------------- floatio open read ---------------------------------!!


      subroutine floatio_open_read (obj,filename,pjar,secname,err,msg)

      type(floatio_struct),pointer       :: obj               ! arguments
      character(len=*)    ,intent(in)    :: filename          ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar              ! arguments
      character(len=*)    ,intent(in)    :: secname           ! arguments
      integer             ,intent(out)   :: err               ! arguments
      character(len=*)    ,intent(out)   :: msg               ! arguments

!----------get started:

      allocate (obj)
      nullify  (obj%fio)
      nullify  (obj%dio)
      nullify  (obj%widths)
      nullify  (obj%decimals)
      obj%encoding = 'unset'
      obj%ncolumns = 0
      obj%nlines   = 0
      call pjar_clear (pjar)

!----------open input file:

      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= FLOATIO_OK) return

!----------read header sections:

      call fio_create               (obj%fio,obj%dio)
      call fio_read_header_sections (obj%fio,pjar,err,msg)

      call floatio_verify           (pjar, secname, err, msg)
      if (err /= FLOATIO_OK) return

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)
      call pjar_get            (pjar, 'ncolumns', obj%ncolumns)
      call pjar_get            (pjar, 'nlines'  , obj%nlines)

!----------prepare to read data section:

      call fio_read_data_section (obj%fio,pjar,secname,err,msg)

      end subroutine floatio_open_read


!!---------------------- floatio open foreign -------------------------------!!
!!---------------------- floatio open foreign -------------------------------!!
!!---------------------- floatio open foreign -------------------------------!!


      subroutine floatio_open_foreign (obj,filename,pjar,secname,err,msg)

      type(floatio_struct),pointer       :: obj               ! arguments
      character(len=*)    ,intent(in)    :: filename          ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar              ! arguments
      character(len=*)    ,intent(in)    :: secname           ! arguments
      integer             ,intent(out)   :: err               ! arguments
      character(len=*)    ,intent(out)   :: msg               ! arguments

!----------get started:

      allocate (obj)
      nullify  (obj%fio)
      nullify  (obj%dio)
      nullify  (obj%widths)
      nullify  (obj%decimals)
      obj%encoding = 'unset'
      obj%ncolumns = 0
      obj%nlines   = 0

      call floatio_verify  (pjar, secname, err, msg)
      if (err /= FLOATIO_OK) return

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)
      call pjar_get            (pjar, 'ncolumns', obj%ncolumns)
      call pjar_get            (pjar, 'nlines'  , obj%nlines)

!----------open input file:

      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= FLOATIO_OK) return

!----------prepare to read data section:

      call fio_create            (obj%fio,obj%dio)
      call fio_read_data_section (obj%fio,pjar,secname,err,msg)

      end subroutine floatio_open_foreign


!!---------------------- floatio open write ---------------------------------!!
!!---------------------- floatio open write ---------------------------------!!
!!---------------------- floatio open write ---------------------------------!!


      subroutine floatio_open_write (obj,filename,pjar,secname,err,msg)

      type(floatio_struct),pointer       :: obj               ! arguments
      character(len=*)    ,intent(in)    :: filename          ! arguments
      type(pjar_struct)   ,intent(inout) :: pjar              ! arguments
      character(len=*)    ,intent(in)    :: secname           ! arguments
      integer             ,intent(out)   :: err               ! arguments
      character(len=*)    ,intent(out)   :: msg               ! arguments
      integer                            :: ncol              ! local

!----------get started:

      allocate (obj)
      nullify  (obj%fio)
      nullify  (obj%dio)
      nullify  (obj%widths)
      nullify  (obj%decimals)
      obj%encoding = 'unset'
      obj%ncolumns = 0
      obj%nlines   = 0
      call floatio_augment (pjar, secname)
      call floatio_verify  (pjar, secname, err, msg)
      if (err /= FLOATIO_OK) return

      call pjar_choose_section (pjar,secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)
      call pjar_get            (pjar, 'ncolumns', obj%ncolumns)
      call pjar_get            (pjar, 'nlines'  , obj%nlines)

      allocate (obj%widths     (obj%ncolumns))
      allocate (obj%decimals   (obj%ncolumns))

      obj%widths  (:) = 0
      obj%decimals(:) = 50

      call pjar_get            (pjar, 'widths'  , obj%widths  , ncol, 0)
      call pjar_get            (pjar, 'decimals', obj%decimals, ncol, 50)

!----------open output file:

      call dio_open_write (obj%dio,filename,err,msg)
      if (err /= FLOATIO_OK) return

!----------write header sections:

      call fio_create                (obj%fio,obj%dio)
      call fio_write_header_sections (obj%fio,pjar,err,msg,DEFAULT_FILETYPE)
      if (err /= FLOATIO_OK) return

!----------prepare to write data section:

      call fio_write_data_section (obj%fio,pjar,secname,err,msg)

      end subroutine floatio_open_write


!!--------------------- floatio read line ----------------------------!!
!!--------------------- floatio read line ----------------------------!!
!!--------------------- floatio read line ----------------------------!!


      subroutine floatio_read_line_real  (obj,err,msg,vline)

      type(floatio_struct),intent(inout) :: obj             ! arguments
      integer             ,intent(out)   :: err             ! arguments
      character(len=*)    ,intent(out)   :: msg             ! arguments
      real                ,intent(out)   :: vline(:)        ! vline(ncolumns)
      integer                            :: column          ! local

      call fio_before_read_line (obj%fio)
      do column = 1,obj%ncolumns
           if (column > size(vline,1)) exit
           call fio_read_line (obj%fio, column, vline(column))
      enddo
      call fio_after_read_line (obj%fio,err,msg)

      end subroutine floatio_read_line_real



      subroutine floatio_read_line_double  (obj,err,msg,vline)

      type(floatio_struct),intent(inout) :: obj             ! arguments
      integer             ,intent(out)   :: err             ! arguments
      character(len=*)    ,intent(out)   :: msg             ! arguments
      double precision    ,intent(out)   :: vline(:)        ! vline(ncolumns)
      integer                            :: column          ! local

      call fio_before_read_line (obj%fio)
      do column = 1,obj%ncolumns
           if (column > size(vline,1)) exit
           call fio_read_line (obj%fio, column, vline(column))
      enddo
      call fio_after_read_line (obj%fio,err,msg)

      end subroutine floatio_read_line_double


!!--------------------- floatio write line ----------------------------!!
!!--------------------- floatio write line ----------------------------!!
!!--------------------- floatio write line ----------------------------!!


      subroutine floatio_write_line_real  (obj,err,msg,vline)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments
      real                ,intent(in)    :: vline(:)      ! vline(ncolumns)
      integer                            :: column        ! local

      call fio_before_write_line (obj%fio)
      do column = 1,obj%ncolumns
           if (obj%widths(column) == 0) then
                call fio_write_line (obj%fio, column, vline(column),        &
                                     ndec=obj%decimals(column))
           else
                call fio_write_line (obj%fio, column, vline(column),        &
                                     obj%widths(column), obj%decimals(column))
           endif
      enddo
      call fio_after_write_line (obj%fio,err,msg)

      end subroutine floatio_write_line_real



      subroutine floatio_write_line_double  (obj,err,msg,vline)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments
      double precision    ,intent(in)    :: vline(:)      ! vline(ncolumns)
      integer                            :: column        ! local

      call fio_before_write_line (obj%fio)
      do column = 1,obj%ncolumns
           if (obj%widths(column) == 0) then
                call fio_write_line (obj%fio, column, vline(column),        &
                                     ndec=obj%decimals(column))
           else
                call fio_write_line (obj%fio, column, vline(column),        &
                                     obj%widths(column), obj%decimals(column))
           endif
      enddo
      call fio_after_write_line (obj%fio,err,msg)

      end subroutine floatio_write_line_double


!!--------------------- floatio read binary --------------------------------!!
!!--------------------- floatio read binary --------------------------------!!
!!--------------------- floatio read binary --------------------------------!!


      subroutine floatio_before_read_binary (obj)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer                            :: nlines        ! local

      call fio_before_read_binary  (obj%fio, nlines)

      end subroutine floatio_before_read_binary



      subroutine floatio_read_binary_real (obj,column,vcolumn)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(in)    :: column        ! arguments
      real                ,intent(out)   :: vcolumn(:)    ! vcolumn(nlines)

      call fio_read_binary (obj%fio,column,vcolumn)

      end subroutine floatio_read_binary_real



      subroutine floatio_read_binary_double (obj,column,vcolumn)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(in)    :: column        ! arguments
      double precision    ,intent(out)   :: vcolumn(:)    ! vcolumn(nlines)

      call fio_read_binary (obj%fio,column,vcolumn)

      end subroutine floatio_read_binary_double



      subroutine floatio_after_read_binary (obj,err,msg)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments

      call fio_after_read_binary  (obj%fio,err,msg)

      end subroutine floatio_after_read_binary


!!--------------------- floatio write binary -------------------------------!!
!!--------------------- floatio write binary -------------------------------!!
!!--------------------- floatio write binary -------------------------------!!


      subroutine floatio_before_write_binary (obj,nlines)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(in)    :: nlines        ! arguments

      call fio_before_write_binary  (obj%fio, nlines)

      end subroutine floatio_before_write_binary



      subroutine floatio_write_binary_real (obj,column,vcolumn)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(in)    :: column        ! arguments
      real                ,intent(in)    :: vcolumn(:)    ! vcolumn(nlines)

      call fio_write_binary  (obj%fio,column,vcolumn)

      end subroutine floatio_write_binary_real



      subroutine floatio_write_binary_double (obj,column,vcolumn)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(in)    :: column        ! arguments
      double precision    ,intent(in)    :: vcolumn(:)    ! vcolumn(nlines)

      call fio_write_binary  (obj%fio,column,vcolumn)

      end subroutine floatio_write_binary_double



      subroutine floatio_after_write_binary (obj,err,msg)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments

      call fio_after_write_binary  (obj%fio,err,msg)

      end subroutine floatio_after_write_binary


!!--------------------- floatio read data --------------------------------!!
!!--------------------- floatio read data --------------------------------!!
!!--------------------- floatio read data --------------------------------!!


      subroutine floatio_read_data_real (obj,err,msg,vdata)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments
      real                ,intent(out)   :: vdata(:,:)    ! (nlines,ncolumns)
      integer                            :: line,column   ! local

      if (obj%encoding == FLOATIO_BINARY) then

           call floatio_before_read_binary (obj)
           do column = 1,obj%ncolumns
                if (column > size(vdata,2)) exit
                call floatio_read_binary (obj,column,vdata(:,column))
           enddo
           call floatio_after_read_binary (obj,err,msg)

      else

           do line = 1,obj%nlines
                call floatio_read_line (obj,err,msg,vdata(line,:))
                if (err /= FLOATIO_OK) exit
           enddo

      endif

      end subroutine floatio_read_data_real



      subroutine floatio_read_data_double (obj,err,msg,vdata)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments
      double precision    ,intent(out)   :: vdata(:,:)    ! (nlines,ncolumns)
      integer                            :: line,column   ! local

      if (obj%encoding == FLOATIO_BINARY) then

           call floatio_before_read_binary (obj)
           do column = 1,obj%ncolumns
                if (column > size(vdata,2)) exit
                call floatio_read_binary (obj,column,vdata(:,column))
           enddo
           call floatio_after_read_binary (obj,err,msg)

      else

           do line = 1,obj%nlines
                call floatio_read_line (obj,err,msg,vdata(line,:))
                if (err /= FLOATIO_OK) exit
           enddo

      endif

      end subroutine floatio_read_data_double


!!--------------------- floatio write data --------------------------------!!
!!--------------------- floatio write data --------------------------------!!
!!--------------------- floatio write data --------------------------------!!


      subroutine floatio_write_data_real (obj,err,msg,vdata,nlines)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments
      real                ,intent(in)    :: vdata(:,:)    ! (nlines,ncolumns)
      integer             ,intent(in)    :: nlines        ! arguments
      integer                            :: line,column   ! local

      if (obj%encoding == FLOATIO_BINARY) then

           call floatio_before_write_binary (obj,nlines)
           do column = 1,obj%ncolumns
                call floatio_write_binary (obj,column,vdata(:,column))
           enddo
           call floatio_after_write_binary (obj,err,msg)

      else

           do line = 1,nlines
                call floatio_write_line (obj,err,msg,vdata(line,:))
                if (err /= FLOATIO_OK) exit
           enddo

      endif

      end subroutine floatio_write_data_real


 
      subroutine floatio_write_data_double (obj,err,msg,vdata,nlines)

      type(floatio_struct),intent(inout) :: obj           ! arguments
      integer             ,intent(out)   :: err           ! arguments
      character(len=*)    ,intent(out)   :: msg           ! arguments
      double precision    ,intent(in)    :: vdata(:,:)    ! (nlines,ncolumns)
      integer             ,intent(in)    :: nlines        ! arguments
      integer                            :: line,column   ! local

      if (obj%encoding == FLOATIO_BINARY) then

           call floatio_before_write_binary (obj,nlines)
           do column = 1,obj%ncolumns
                call floatio_write_binary (obj,column,vdata(:,column))
           enddo
           call floatio_after_write_binary (obj,err,msg)

      else

           do line = 1,nlines
                call floatio_write_line (obj,err,msg,vdata(line,:))
                if (err /= FLOATIO_OK) exit
           enddo

      endif

      end subroutine floatio_write_data_double


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module floatio_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
