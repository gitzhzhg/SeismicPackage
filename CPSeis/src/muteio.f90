!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- muteio.f90 --------------------------------!!
!!------------------------------- muteio.f90 --------------------------------!!
!!------------------------------- muteio.f90 --------------------------------!!


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
! Name       : MUTEIO
! Category   : io
! Written    : 2000-11-14   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : To read and write mute files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading and writing the following
! mute file formats:
!
!  (1) old-style CPS ascii  mute files (using the MUTEIO1 primitive).
!  (2) self-defining ascii  mute files (using the MUTEIO2 primitive).
!  (2) self-defining hybrid mute files (using the MUTEIO2 primitive).
!
! This primitive is smart enough to decide automatically what format to read.
! CBYT will be upgraded to use the new mute file formats.
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
!                           CALLING SEQUENCE
!
! Open the mute file and read initial information:
!  (parameters are in the argument list)
!  (intended for batch programs)
!                                                             opt   opt
!                            o     i      o   o   o   o   o    o     o
!   call muteio_open_read  (obj,filename,err,msg,nho,nhx,nhy,xlast,ylast,
!                           phist,nhist)
!                             o     o  
!                            opt   opt 
!
!
! Open the mute file and read or write initial information:
!  (the same parameters are in the pickle jar)
!
!                               o     i      b      i     o   o
!    call muteio_open_read    (obj,filename,pjar,secname,err,msg)
!    call muteio_open_write   (obj,filename,pjar,secname,err,msg)
!    call muteio_open_foreign (obj,filename,pjar,secname,err,msg)
!
!
! Close the mute file:
!
!                       b
!   call muteio_close (obj)
!
!
! Verify or augment parameters:
!
!                          i      i     o   o
!    call muteio_verify  (pjar,secname,err,msg)
!    call muteio_augment (pjar,secname)
!                          b      i
!
!
! Read or write a single card image:
!
!                            b    o      o      o     o    o   o
!   call muteio_read_card  (obj,offset,xcoord,ycoord,time,err,msg)
!   call muteio_write_card (obj,offset,xcoord,ycoord,time,err,msg)
!                            b    i      i      i     i    o   o
!
!
! Scan an open mute file to get ranges and number of cards:
!  (parameters are in the pickle jar)
!
!                      b   b      i     o   o
!   call muteio_scan (obj,pjar,secname,err,msg)
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(muteio_struct) obj  = pointer to the MUTEIO data structure.
! type(pjar_struct)   pjar = reference to the pickle jar data structure.
!
! char(*) secname  = name of the section on the file to read or write.
!                      (the history section is also read or written)
!
! char(*)          filename = name of the mute file for input or output.
! integer          err      = error flag (returned).
! char(*)          msg      = message for possible printing (returned).
!
! integer          nho      = header word containing the offset.
! integer          nhx      = header word containing the X coordinate.
! integer          nhy      = header word containing the Y coordinate.
! real             xlast    = latest X coordinate updated.
! real             ylast    = latest Y coordinate updated.
! real             offset   = mute location offset.
! real             xcoord   = mute location X coordinate.
! real             ycoord   = mute location Y coordinate.
! real             time     = mute time.
!
! integer            ncards = number of card images containing mute times.
! real     offmin,xmin,ymin = center of first   offset/xcoord/ycoord bin.
! real     offmax,xmax,ymax = center of last    offset/xcoord/ycoord bin.
! real            xinc,yinc = increment between        xcoord/ycoord bins.
! integer             nx,ny = number of                xcoord/ycoord bins.
!
! char(*) hist (nhist) = array of history cards (up to 80 characters).
! char(*) phist(nhist) = pointer to array of history cards (up to 80 chars).
! integer nhist        = number of history cards.
! char(*) progname     = name of program or process accessing this file.
!
! char(*) encoding    = encoding format of mute file to write.
! char(*) fields(:)   = list of fields (columns names) to read.
! integer nfields     = number of fields to read.
! char(*) nilstring   = string for nil values in the file (default '-nil-').
! integer wrap        = number of card images per record (normally 1).
! integer firstline   = line to start reading on (for foreign files only).
! char(*) template    = template for adjusting foreign input card image.
! integer maxchars(:) = maximum number of characters in each field to decode.
! integer nmaxchars   = number of maxchars.
!
! PHIST is a pointer which must be nullified before first use.  It will be
! deallocated and reallocated to contain the returned contents.  It will always
! be allocated to a dimension of at least one.  It should be conditionally
! deallocated when no longer needed.
!
! NILSTRING and WRAP are used only for ascii encoding (including foreign files).
!
! FIRSTLINE, TEMPLATE, and MAXCHARS(NMAXCHARS) are used only to read foreign
! files.  See documentation in the FIO primitive for more information.
!
! See documentation in the MUTEIO2 primitive for permitted values for the
! FIELDS(NFIELDS) array.
!
!-------------------------------------------------------------------------------
!                           SUBROUTINE DETAILS
!
! MUTEIO_OPEN_READ:
!  (1) Allocates the MUTEIO data structure.
!  (2) Opens the mute file.
!  (3) Returns values found in the file header.
!  (4) Returns zero for values not in the file header.
!
! MUTEIO_OPEN_WRITE:
!  (1) Allocates the MUTEIO data structure.
!  (2) Opens the mute file.
!  (3) Writes the argument values into the file header.
!  (4) Writes defaults for values not in the argument list.
!  (4) The desired ENCODING should be specified by the user.
!
! MUTEIO_OPEN_FOREIGN:
!  (1) Allocates the MUTEIO data structure.
!  (2) Opens the mute file.
!  (3) See documentation in the FIO primitive for more information.
!
! MUTEIO_CLOSE:
!  (1) Closes the mute file (unless already closed).
!  (2) Deallocates the MUTEIO data structure (unless already deallocated).
!
!-------------------------------------------------------------------------------
!                        RETURNED ERROR FLAGS
!
! The returned error will have one of these integer named constant values:
!
!          err             description
!          ---             -----------
!          MUTEIO_OK       the operation was successful.
!          MUTEIO_ERROR    an open or read/write error occurred.
!          MUTEIO_EOF      an end-of-file was encountered.
!
! MUTEIO_EOF is returned only when an end-of-file or end-of-section is
! encountered while reading ascii records.
!
!-------------------------------------------------------------------------------
!                           ENCODING FORMATS
!
! The encoding format of the mute file to read or write must be
! one of these character(len=8) named constant values:
!
!     encoding        description
!     --------        -----------
!     MUTEIO_OLDCPS   old-style CPS mute file.
!     MUTEIO_ASCII    CPS self-defining ascii mute file.
!     MUTEIO_HYBRID   CPS self-defining hybrid binary mute file.
!
! The default value of ENCODING for output is currently MUTEIO_OLDCPS, but
! this may be changed to MUTEIO_ASCII in the future.
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
!004. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!  3. 2002-04-11  Stoeckley  Add MSG argument in call to FIOUTIL_VERIFY1.
!  2. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  1. 2001-01-10  Stoeckley  Initial version.
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
 
 
      module muteio_module
      use muteio1_module
      use muteio2_module
      use pjar_module
      use dio_module
      use fio_module
      use increment_module
      implicit none
      public

      character(len=100),public,save :: MUTEIO_IDENT = &
       '$Id: muteio.f90,v 1.4 2006/10/17 13:45:45 Glover prod sps $'

      interface muteio_open_read
         module procedure muteio_open_read_args
         module procedure muteio_open_read_pjar
      end interface

      integer,public ,parameter :: MUTEIO_OK         = FIO_OK
      integer,public ,parameter :: MUTEIO_ERROR      = FIO_ERROR
      integer,public ,parameter :: MUTEIO_EOF        = FIO_EOF
 
      character(len=8),public ,parameter  :: MUTEIO_ASCII   = FIO_ASCII
      character(len=8),public ,parameter  :: MUTEIO_HYBRID  = FIO_HYBRID
      character(len=8),public ,parameter  :: MUTEIO_OLDCPS  = 'oldcps'

      character(len=20),private,parameter :: DEFAULT_FILETYPE = 'mute'
      character(len=20),private,parameter :: DEFAULT_SECNAME  = 'mute'

      integer,private,parameter :: STEP = 20

      type,public :: muteio_struct
           private
           type(muteio2_struct),pointer :: muteio2
           type(dio_struct)    ,pointer :: dio
           type(fio_struct)    ,pointer :: fio
           character(len=8)             :: encoding
      end type muteio_struct


      contains


!!----------------------------- augment ----------------------------------!!
!!----------------------------- augment ----------------------------------!!
!!----------------------------- augment ----------------------------------!!

! called before writing a file to disk.
! pickle jar contents are modified.


      subroutine muteio_augment (pjar,secname)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      integer                         :: nho,nhx,nhy        ! local

      call fioutil_augment1 (pjar, secname, MUTEIO_OLDCPS)   ! temporary.
 !!!  call fioutil_augment1 (pjar, secname, MUTEIO_ASCII)    ! use later.

      call pjar_augment (pjar, 'nho'          ,        0     )
      call pjar_augment (pjar, 'nhx'          ,        0     )
      call pjar_augment (pjar, 'nhy'          ,        0     )
      call pjar_augment (pjar, 'xlast'        ,       0.0    )
      call pjar_augment (pjar, 'ylast'        ,       0.0    )

      call pjar_get (pjar, 'nho'  , nho)
      call pjar_get (pjar, 'nhx'  , nhx)
      call pjar_get (pjar, 'nhy'  , nhy)

                 !!!                         opt     opt     opt     opt
                 !!!                          |       |       |       |
                 !!!   (pjar, field, defval, hdr, fieldtype, unit, delimiter)

      call fioutil_augment2 (pjar, 'offset', 0.0,  nho)
      call fioutil_augment2 (pjar, 'xcoord', 0.0,  nhx)
      call fioutil_augment2 (pjar, 'ycoord', 0.0,  nhy)
      call fioutil_augment2 (pjar, 'time'  , 0.0,   0 )

      call fioutil_augment3 (pjar)
      return
      end subroutine muteio_augment


!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!

! called after reading file header sections from disk.
! called before using the the pickle jar to read a data section from disk.
! called before writing a file to disk.
! first: optional pickle jar contents are augmented.
! then: all pickle jar contents are verified.


      subroutine muteio_verify (pjar,secname,err,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar                ! arguments
      character(len=*) ,intent(in)    :: secname             ! arguments
      integer          ,intent(out)   :: err                 ! arguments
      character(len=*) ,intent(out)   :: msg                 ! arguments
      character(len=8)                :: encoding            ! local
      integer                         :: nho,nhx,nhy,nlines  ! local

!----------get started:

      call fioutil_verify1 (pjar,secname,msg)

      if(msg /= ' ') then
          err = MUTEIO_ERROR
          return
      end if

!----------augment optional parameters:

      call pjar_augment (pjar, 'nho'          ,        0     )
      call pjar_augment (pjar, 'nhx'          ,        0     )
      call pjar_augment (pjar, 'nhy'          ,        0     )
      call pjar_augment (pjar, 'xlast'        ,       0.0    )
      call pjar_augment (pjar, 'ylast'        ,       0.0    )
      call pjar_augment (pjar, 'nlines'       ,       -1     )

!----------get selected parameters to test:

      call pjar_get (pjar, 'nho'             , nho         )
      call pjar_get (pjar, 'nhx'             , nhx         )
      call pjar_get (pjar, 'nhy'             , nhy         )
      call pjar_get (pjar, 'nlines'          , nlines      )

!----------test selected parameters:

                    !!!                   missing         invalid
                    !!!                      |               |
      call fioutil_verify2 ('nho'   , (nho   ==INIL), (nho    <= 0))
      call fioutil_verify2 ('nhx'   , (nhx   ==INIL), (nhx    <= 0))
      call fioutil_verify2 ('nhy'   , (nhy   ==INIL), (nhy    <= 0))
      call fioutil_verify2 ('nlines', (nlines==INIL), (nlines < -1))

!----------finish up and return:

      call fioutil_verify3 (pjar,msg)

      if(msg /= ' ') then
          err = MUTEIO_ERROR
          return
      end if

      call pjar_get (pjar, 'encoding', encoding)

      if (encoding /= MUTEIO_OLDCPS) then
           call fioutil_verify (pjar,secname,msg)
      end if

      if(msg == ' ') then
          err = MUTEIO_OK
          msg = 'XYO headers = '//trim(string_ii2ss(nhx))//' '    &
                                //trim(string_ii2ss(nhy))//' '    &
                                //trim(string_ii2ss(nho))//'    ' &
                                //trim(string_ii2ss(nlines))//' lines'
      else
          err = MUTEIO_ERROR
      end if
      return
      end subroutine muteio_verify


!!--------------------------- muteio open read ----------------------------!!
!!--------------------------- muteio open read ----------------------------!!
!!--------------------------- muteio open read ----------------------------!!
 
 
      subroutine muteio_open_read_args (obj,filename,err,msg,     &
                                        nho,nhx,nhy,xlast,ylast,  &
                                        phist,nhist)
      implicit none
      type(muteio_struct),pointer            :: obj             ! arguments
      character(len=*) ,intent(in)           :: filename        ! arguments
      integer          ,intent(out)          :: err             ! arguments
      character(len=*) ,intent(out)          :: msg             ! arguments
      integer          ,intent(out)          :: nho,nhx,nhy     ! arguments
      real             ,intent(out),optional :: xlast,ylast     ! arguments
      character(len=*) ,pointer    ,optional :: phist(:)        ! arguments
      integer          ,intent(out),optional :: nhist           ! arguments
      type(pjar_struct),pointer              :: pjar            ! local
 
      nullify (pjar) ! jpa
      call pjar_create         (pjar)
      call muteio_open_read    (obj,filename,pjar,DEFAULT_SECNAME,err,msg)
      call pjar_choose_section (pjar,DEFAULT_SECNAME)

                          call pjar_get (pjar, 'nho'     , nho)
                          call pjar_get (pjar, 'nhx'     , nhx)
                          call pjar_get (pjar, 'nhy'     , nhy)
      if (present(xlast)) call pjar_get (pjar, 'xlast'   , xlast)
      if (present(ylast)) call pjar_get (pjar, 'ylast'   , ylast)

      call pjar_choose_section (pjar,'history')
      call pjar_alloc_cards    (pjar,phist,nhist)
      call pjar_delete         (pjar)
      return
      end subroutine muteio_open_read_args
 
 
!!--------------------------- muteio open read ----------------------------!!
!!--------------------------- muteio open read ----------------------------!!
!!--------------------------- muteio open read ----------------------------!!
 
 
      subroutine muteio_open_read_pjar (obj,filename,pjar,secname,err,msg)
      implicit none
      type(muteio_struct),pointer            :: obj             ! arguments
      character(len=*) ,intent(in)           :: filename        ! arguments
      type(pjar_struct),intent(inout)        :: pjar            ! arguments
      character(len=*) ,intent(in)           :: secname         ! arguments
      integer          ,intent(out)          :: err             ! arguments
      character(len=*) ,intent(out)          :: msg             ! arguments

!!!!!!!!!! initialize data structure:

      allocate (obj)
      nullify (obj%muteio2)
      nullify (obj%fio)
      nullify (obj%dio)
      obj%encoding = 'unset'
      call pjar_clear (pjar)

!!!!!!!!!! open input file:
 
      call dio_open_read (obj%dio, filename, err, msg)
      if (err /= MUTEIO_OK) return

!!!!!!!!!! try reading oldcps file:
 
      call muteio1_read_header (obj%dio,pjar,secname,err,msg)
      if (err == MUTEIO_OK) then
           obj%encoding = MUTEIO_OLDCPS
           call pjar_choose_section (pjar,secname)
           call pjar_put            (pjar,'encoding' ,obj%encoding)
           call muteio_verify       (pjar, secname, err, msg)
           return
      end if

!!!!!!!!!! read newcps file:

      call fio_create               (obj%fio,obj%dio)
      call fio_read_header_sections (obj%fio,pjar,err,msg)
      if (err /= MUTEIO_OK) return

      call muteio_verify            (pjar, secname, err, msg)
      if (err /= MUTEIO_OK) return

      call fio_read_data_section    (obj%fio,pjar,secname,err,msg)
      if (err /= MUTEIO_OK) return

      call pjar_choose_section      (pjar,secname)
      call pjar_get                 (pjar,'encoding' ,obj%encoding)
      call muteio2_create           (obj%muteio2,obj%fio,pjar,secname)
      return
      end subroutine muteio_open_read_pjar
 
 
!!--------------------------- muteio open write ---------------------------!!
!!--------------------------- muteio open write ---------------------------!!
!!--------------------------- muteio open write ---------------------------!!
 

      subroutine muteio_open_write (obj,filename,pjar,secname,err,msg)
      implicit none
      type(muteio_struct),pointer            :: obj             ! arguments
      character(len=*) ,intent(in)           :: filename        ! arguments
      type(pjar_struct),intent(inout)        :: pjar            ! arguments
      character(len=*) ,intent(in)           :: secname         ! arguments
      integer          ,intent(out)          :: err             ! arguments
      character(len=*) ,intent(out)          :: msg             ! arguments
 
!!!!!!!!!! initialize data structure:

      allocate (obj)
      nullify (obj%muteio2)
      nullify (obj%fio)
      nullify (obj%dio)
      obj%encoding = 'unset'

      call muteio_augment (pjar, secname)
      call muteio_verify  (pjar, secname, err, msg)
      if (err /= MUTEIO_OK) return

      call pjar_choose_section    (pjar,secname)
      call pjar_get               (pjar,'encoding' ,obj%encoding)

!!!!!!!!!! open output file:

      call dio_open_write (obj%dio,filename,err,msg)
      if (err /= MUTEIO_OK) return

!!!!!!!!!! write oldcps file:
 
      if (obj%encoding == MUTEIO_OLDCPS) then
           call muteio1_write_header (obj%dio,pjar,secname,err,msg)
           return
      end if
 
!!!!!!!!!! write newcps file:
 
      call fio_create                (obj%fio,obj%dio)
      call fio_write_header_sections (obj%fio,pjar,err,msg,DEFAULT_FILETYPE)
      if (err /= MUTEIO_OK) return

      call fio_write_data_section    (obj%fio,pjar,secname,err,msg)
      if (err /= MUTEIO_OK) return

      call muteio2_create            (obj%muteio2,obj%fio,pjar,secname)
      return
      end subroutine muteio_open_write
 
 
!!--------------------------- muteio open foreign -------------------------!!
!!--------------------------- muteio open foreign -------------------------!!
!!--------------------------- muteio open foreign -------------------------!!
 

      subroutine muteio_open_foreign (obj,filename,pjar,secname,err,msg)
      implicit none
      type(muteio_struct),pointer              :: obj             ! arguments
      character(len=*)   ,intent(in)           :: filename        ! arguments
      type(pjar_struct)  ,intent(inout)        :: pjar            ! arguments
      character(len=*)   ,intent(in)           :: secname         ! arguments
      integer            ,intent(out)          :: err             ! arguments
      character(len=*)   ,intent(out)          :: msg             ! arguments

!!!!!!!!!! initialize data structure:

      allocate (obj)
      nullify (obj%muteio2)
      nullify (obj%fio)
      nullify (obj%dio)
      obj%encoding = 'unset'

      call muteio_augment (pjar, secname)
      call muteio_verify  (pjar, secname, err, msg)
      if (err /= MUTEIO_OK) return

!!!!!!!!!! open file:
 
      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= MUTEIO_OK) return

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)

!!!!!!!!!! read oldcps file:
 
      if (obj%encoding == MUTEIO_OLDCPS) then
           call muteio1_read_header (obj%dio,pjar,secname,err,msg)
           if (err == MUTEIO_OK) then
                call muteio_verify  (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! read newcps file:

      call fio_create               (obj%fio,obj%dio)
      call fio_read_data_section    (obj%fio,pjar,secname,err,msg)
      if (err /= MUTEIO_OK) return

      call muteio2_create           (obj%muteio2,obj%fio,pjar,secname)
      return
      end subroutine muteio_open_foreign
 
 
!!--------------------------- muteio close --------------------------------!!
!!--------------------------- muteio close --------------------------------!!
!!--------------------------- muteio close --------------------------------!!
 
 
      subroutine muteio_close (obj)
      implicit none
      type(muteio_struct),pointer :: obj            ! arguments
 
      if (associated(obj)) then
           call muteio2_delete (obj%muteio2)
           call fio_delete     (obj%fio)
           call dio_close      (obj%dio)
           deallocate(obj)
      end if
      return
      end subroutine muteio_close
 
 
!!------------------------- muteio read card -------------------------------!!
!!------------------------- muteio read card -------------------------------!!
!!------------------------- muteio read card -------------------------------!!


      subroutine muteio_read_card  (obj,offset,xcoord,ycoord,time,err,msg)
      implicit none
      type(muteio_struct),intent(inout) :: obj                   ! arguments
      real               ,intent(out)   :: offset,xcoord,ycoord  ! arguments
      real               ,intent(out)   :: time                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments

      if (obj%encoding == MUTEIO_OLDCPS) then
           call muteio1_read_card (obj%dio,offset,xcoord,ycoord,time,err,msg)
      else
           call muteio2_read_card &
                              (obj%muteio2,offset,xcoord,ycoord,time,err,msg)
      end if
      return
      end subroutine muteio_read_card


!!------------------------- muteio write card -------------------------------!!
!!------------------------- muteio write card -------------------------------!!
!!------------------------- muteio write card -------------------------------!!


      subroutine muteio_write_card (obj,offset,xcoord,ycoord,time,err,msg)
      implicit none
      type(muteio_struct),intent(inout) :: obj                   ! arguments
      real               ,intent(in)    :: offset,xcoord,ycoord  ! arguments
      real               ,intent(in)    :: time                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments

      if (obj%encoding == MUTEIO_OLDCPS) then
           call muteio1_write_card (obj%dio,offset,xcoord,ycoord,time,err,msg)
      else
           call muteio2_write_card &
                               (obj%muteio2,offset,xcoord,ycoord,time,err,msg)
      end if
      return
      end subroutine muteio_write_card


!!--------------------------- muteio scan --------------------------------!!
!!--------------------------- muteio scan --------------------------------!!
!!--------------------------- muteio scan --------------------------------!!

      subroutine muteio_scan (obj,pjar,secname,err,msg)
      implicit none
      type(muteio_struct),intent(inout) :: obj                   ! arguments
      type(pjar_struct)  ,intent(inout) :: pjar                  ! arguments
      character(len=*)   ,intent(in)    :: secname               ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real                              :: offset,xcoord,ycoord  ! local
      real                              :: time                  ! local
      integer                           :: nx,ny,ncards          ! local
      real                              :: offmin,xmin,ymin      ! local
      real                              :: offmax,xmax,ymax      ! local
      real                              ::        xinc,yinc      ! local
      type(increment_struct)            :: incx,incy             ! local

      call increment_init (incx, 1.0, 12)
      call increment_init (incy, 1.0, 12)

      offmin = 0.0
      offmax = 0.0
      ncards = 0
      do
           call muteio_read_card (obj,offset,xcoord,ycoord,time,err,msg)
           if (err /= DIO_OK) exit
           ncards = ncards + 1
           if (ncards == 1) then
                offmin = offset
                offmax = offset
           else
                offmin = min(offmin,offset)
                offmax = max(offmax,offset)
           end if
           call increment_update (incx, xcoord)
           call increment_update (incy, ycoord)
      end do

      call increment_result (incx, xbmin=xmin, xbmax=xmax, xinc=xinc, nx=nx)
      call increment_result (incy, xbmin=ymin, xbmax=ymax, xinc=yinc, nx=ny)

      call pjar_choose_section (pjar,secname)
      call pjar_put            (pjar, 'ncards', ncards )
      call pjar_put            (pjar, 'offmin', offmin )
      call pjar_put            (pjar, 'offmax', offmax )
      call pjar_put            (pjar, 'xmin  ', xmin   )
      call pjar_put            (pjar, 'xmax  ', xmax   )
      call pjar_put            (pjar, 'xinc  ', xinc   )
      call pjar_put            (pjar, 'nx    ', nx     )
      call pjar_put            (pjar, 'ymin  ', ymin   )
      call pjar_put            (pjar, 'ymax  ', ymax   )
      call pjar_put            (pjar, 'yinc  ', yinc   )
      call pjar_put            (pjar, 'ny    ', ny     )
      return
      end subroutine muteio_scan


!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
 

      end module muteio_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
