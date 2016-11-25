!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- geomio.f90 --------------------------------!!
!!------------------------------- geomio.f90 --------------------------------!!
!!------------------------------- geomio.f90 --------------------------------!!


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
! Name       : GEOMIO
! Category   : io
! Written    : 2000-03-07   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : To read and write field geometry files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading and writing the following
! field geometry file formats:
!
!  (1) old-style CPS field geometry (JD) files   (using the GEOMIO1 primitive).
!  (2) self-defining ascii field geometry files  (using the GEOMIO2 primitive).
!  (3) self-defining hybrid field geometry files (using the GEOMIO2 primitive).
!
! This primitive is smart enough to decide automatically what format to read.
! CFG will be upgraded to use the new field geometry file formats.
!
! See the context-sensitive help in the CFG program for more details about
! the various parameters in a field geometry file.
!
! NOTE: This primitive does I/O only; it does not attempt to validate or
! modify any values read or written; that job is left to a data object
! which understands the significance of the values.
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
! Open the field geometry file and read or write initial information:
!  (parameters are in the argument list)
!  (intended for batch programs)
!
!                            o     i      o   o  o    o      o     o
!   call geomio_open_read  (obj,filename,err,msg,ve,datum,fixdist,grid,
!     chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,phist,nhist)
!        o      o   o   o   o    o    o    o     o     o  
!                                               opt   opt 
!
!                            o     i      o   o  i    i      i     i
!   call geomio_open_write (obj,filename,err,msg,ve,datum,fixdist,grid,
!     chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,hist,nhist,progname,encoding)
!        i      i   i   i   i    i    i    i    i     i      i        i
!                                              opt   opt    opt      opt
!
!
! Open the field geometry file and read or write initial information:
!  (the same parameters are in the pickle jar)
!
!                               o     i      b      i     o   o
!    call geomio_open_read    (obj,filename,pjar,secname,err,msg)
!    call geomio_open_write   (obj,filename,pjar,secname,err,msg)
!    call geomio_open_foreign (obj,filename,pjar,secname,err,msg)
!
!
! Close the field geometry file:
!
!                       b
!   call geomio_close (obj)
!
!
! Verify or augment parameters:
!
!                          i      i     o   o
!    call geomio_verify  (pjar,secname,err,msg)
!    call geomio_augment (pjar,secname)
!                          b      i
!
!                        ++++++++++++++++++++++++++
!
! type(geomio_struct)  obj = pointer to the GEOMIO data structure.
! type(pjar_struct)   pjar = reference to the pickle jar data structure.
!
! char(*) secname  = name of main header section on the file to read or write.
!                      (the history section is also read or written)
!                      (several sections listed in the main header section
!                       are also read or written)
!
! char(*) filename = name of the field geometry file for input or output.
! integer err      = error flag (returned).
! char(*) msg      = message for possible printing (returned).
!
!                        ++++++++++++++++++++++++++
!
! real              ve       = reference velocity.
! real              datum    = datum elevation.
! real              fixdist  = uniform inline distance parameter.
! type(grid_struct) grid     = grid transformation structure.
! char(*)           chaining = chaining flag.
! integer           nld      = number of LD cards.
! integer           nrp      = number of RP cards.
! integer           npp      = number of PP cards.
! integer           nzt1     = number of ZT1 cards.
! integer           nzt2     = number of ZT2 cards.
! integer           nzt3     = number of ZT3 cards.
! integer           nzt4     = number of ZT4 cards.
!
! char(*) hist (nhist) = array of history cards (up to 80 characters).
! char(*) phist(nhist) = pointer to array of history cards (up to 80 chars).
! integer nhist        = number of history cards.
! char(*) progname     = name of program or process accessing this file.
!
! char(*) encoding    = encoding format of field geometry file to write.
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
! See documentation in the GEOMIO2 primitive for permitted values for the
! FIELDS(NFIELDS) array.
!
! Note: If ENCODING is GEOMIO_OLDCPS, it should be set in the main SECNAME
! header section.  Otherwise, it can be set either in the main section or
! in the individual header sections corresponding to data sections.
!
!                        ++++++++++++++++++++++++++
!
! GEOMIO_OPEN_READ:
!  (1) Allocates the GEOMIO data structure.
!  (2) Opens the field geometry file.
!  (3) Returns values found in the file header.
!  (4) Returns nils for values not in the file header.
!
! GEOMIO_OPEN_WRITE:
!  (1) Allocates the GEOMIO data structure.
!  (2) Opens the field geometry file.
!  (3) Writes the argument values into the file header.
!  (4) Writes defaults for values not in the argument list.
!  (4) The desired ENCODING should be specified by the user.
!
! GEOMIO_OPEN_FOREIGN:
!  (1) Allocates the GEOMIO data structure.
!  (2) Opens the field geometry file.
!  (3) See documentation in the FIO primitive for more information.
!
! GEOMIO_CLOSE:
!  (1) Closes the field geometry file (unless already closed).
!  (2) Deallocates the GEOMIO data structure (unless already deallocated).
!
!-------------------------------------------------------------------------------
!                      READ AND WRITE INDIVIDUAL CARDS
!
!      (must call for each LD  card with ild  = 1,nld  consecutively)
!      (must call for each RP  card with irp  = 1,nrp  consecutively)
!      (must call for each PP  card with ipp  = 1,npp  consecutively)
!      (must call for each ZT1 card with izt1 = 1,nzt1 consecutively)
!      (must call for each ZT2 card with izt2 = 1,nzt2 consecutively)
!      (must call for each ZT3 card with izt3 = 1,nzt3 consecutively)
!      (must call for each ZT4 card with izt4 = 1,nzt4 consecutively)
!        (the sections must be read or written in the above order)
!
! Read or write one LD card:
!
!                                    b   i    o   o
!        call geomio_read_ld_card  (obj,ild,err,msg,
!                sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!                o   o    o    o    o     o    o  o  o   o   o   o    o
!
!                                    b   i   o   o
!        call geomio_write_ld_card (obj,ild,err,msg,
!                sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
!                i   i    i    i    i     i    i  i  i   i   i   i    i
!
!
! Read or write one RP card:
!
!                                    b   i   o   o
!        call geomio_read_rp_card  (obj,irp,err,msg,
!                ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!                  o    o    o    o   o    o   o    o    o    o     o
!
!                                    b   i   o   o
!        call geomio_write_rp_card (obj,irp,err,msg,
!                ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
!                  i    i    i    i   i    i   i    i    i    i     i
!
!
! Read or write one PP card:
!
!                                    b   i   o   o
!        call geomio_read_pp_card  (obj,ipp,err,msg,
!          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!           o    o    o    o     o    o    o    o     o     o     o    o  o  o
!
!                                    b   i   o   o
!        call geomio_write_pp_card (obj,ipp,err,msg,
!          sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
!           i    i    i    i     i    i    i    i     i     i     i    i  i  i
!
!
! Read or write one ZT1 card:
!
!                                     b   i    o   o
!        call geomio_read_zt1_card  (obj,izt1,err,msg,
!                                    ccc1,sss1,sss1a,lll1)
!                                     o    o     o    o
!
!                                     b   i    o   o
!        call geomio_write_zt1_card (obj,izt1,err,msg,
!                                    ccc1,sss1,sss1a,lll1)
!                                     i    i     i    i
!
!
! Read or write one ZT2 card:
!
!                                     b   i    o   o
!        call geomio_read_zt2_card  (obj,izt2,err,msg,
!                                    ccc2,rrr2,rrr2a,lll2)
!                                     o    o     o    o
!
!                                     b   i    o   o
!        call geomio_write_zt2_card (obj,izt2,err,msg,
!                                    ccc2,rrr2,rrr2a,lll2)
!                                     i    i     i    i
!
!
! Read or write one ZT3 card:
!
!                                     b   i    o   o
!        call geomio_read_zt3_card  (obj,izt3,err,msg,
!                                    ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                     o     o     o      o     o
!
!                                     b   i    o   o
!        call geomio_write_zt3_card (obj,izt3,err,msg,
!                                    ccc3,iggg3,iggg3a,ittt3,ittt3a)
!                                     i     i     i      i     i
!
!
! Read or write one ZT4 card:
!
!                                     b   i    o   o
!        call geomio_read_zt4_card  (obj,izt4,err,msg,
!                                    ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                     o    o     o    o    o     o     o
!
!                                     b   i    o   o
!        call geomio_write_zt4_card (obj,izt4,err,msg,
!                                    ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
!                                     i    i     i    i    i     i     i
!
!                        ++++++++++++++++++++++++++
!
! type(geomio_struct),pointer obj = pointer to the GEOMIO data structure.
!
! integer err    = error flag (returned).
! char(*) msg    = message for possible printing (returned).
!
! integer ild    = LD card number.
! integer irp    = RP card number.
! integer ipp    = PP card number.
! integer izt1   = ZT1 card number.
! integer izt2   = ZT2 card number.
! integer izt3   = ZT3 card number.
! integer izt4   = ZT4 card number.
!
! real    sp     = LD card: shotpoint.
! real    dist   = LD card: distance to next flag.
! real    xloc   = LD card: surveyed X location.
! real    yloc   = LD card: surveyed Y location.
! real    elev   = LD card: default elevation.
! real    depth  = LD card: default hole depth.
! real    tuh    = LD card: default uphole time.
! real    tr     = LD card: receiver static.
! real    ts     = LD card: source static.
! real    xsd    = LD card: inline skid.
! real    ysd    = LD card: crossline skid.
! real    elsd   = LD card: elevation skid.
! integer line   = LD card: line number.
!
! integer ipat1  = RP card: pattern number.
! char(*) flag   = RP card: flag.
! real    sp1    = RP card: first receiver shotpoint.
! integer line1  = RP card: first receiver line number.
! integer nx     = RP card: number of receiver flags along line.
! integer ixinc  = RP card: receiver flag increment along line.
! integer ny     = RP card: number of receiver flags across lines.
! integer iyinc  = RP card: receiver flag increment across lines.
! real    xsd1   = RP card: receiver pattern inline skid.
! real    ysd1   = RP card: receiver pattern crossline skid.
! real    elsd1  = RP card: receiver pattern elevation skid.
!
! real    sp2    = PP card: source shotpoint.
! integer line2  = PP card: source line number.
! real    sp3    = PP card: first receiver shotpoint.
! integer line3  = PP card: first receiver line number.
! integer ipat2  = PP card: pattern number.
! real    xsd2   = PP card: source inline skid.
! real    ysd2   = PP card: source crossline skid.
! integer hold   = PP card: how many groups to hold skid.
! real    elev2  = PP card: new source elevation.
! real    depth2 = PP card: new hole depth.
! real    tuh2   = PP card: new uphole time.
! integer is     = PP card: flag moveup to subsequent sources.
! integer ir     = PP card: flag moveup to subsequent first receivers.
! integer ig     = PP card: group number (shot profile number).
!
! char(*) ccc1   = ZT1 card: code.
! real    sss1   = ZT1 card: first source shotpoint affected.
! real    sss1a  = ZT1 card: last source shotpoint affected.
! integer lll1   = ZT1 card: source line number.
!
! char(*) ccc2   = ZT2 card: code.
! real    rrr2   = ZT2 card: first receiver shotpoint affected.
! real    rrr2a  = ZT2 card: last receiver shotpoint affected.
! integer lll2   = ZT2 card: receiver line number.
!
! char(*) ccc3   = ZT3 card: code.
! integer iggg3  = ZT3 card: first group number affected.
! integer iggg3a = ZT3 card: last group number affected.
! integer ittt3  = ZT3 card: first trace number affected.
! integer ittt3a = ZT3 card: last trace number affected.
!
! char(*) ccc4   = ZT4 card: code.
! real    sss4   = ZT4 card: first source shotpoint affected.
! real    sss4a  = ZT4 card: last source shotpoint affected.
! integer lll4   = ZT4 card: source line number.
! real    rrr4   = ZT4 card: first receiver shotpoint affected.
! real    rrr4a  = ZT4 card: last receiver shotpoint affected.
! integer lll4a  = ZT4 card: receiver line number.
!
!                        ++++++++++++++++++++++++++
!
! GEOMIO_READ_..._CARD:
!  (1) Reads the next card image from the file and returns the values found.
!
! GEOMIO_WRITE_..._CARD:
!  (1) Writes the values for this card image to the file.
!
!-------------------------------------------------------------------------------
!                        RETURNED ERROR FLAGS
!
! The returned error will have one of these integer named constant values:
!
!          err             description
!          ---             -----------
!          GEOMIO_OK       the operation was successful.
!          GEOMIO_ERROR    an open or read/write error occurred.
!          GEOMIO_EOF      an end-of-file was encountered.
!
! GEOMIO_EOF is returned only when an end-of-file or end-of-section is
! encountered while reading ascii records.
!
!-------------------------------------------------------------------------------
!                           ENCODING FORMATS
!
! The encoding format of the field geometry file to read or write must be
! one of these character(len=8) named constant values:
!
!     encoding        description
!     --------        -----------
!     GEOMIO_OLDCPS   old-style CPS field geometry (JD) file.
!     GEOMIO_ASCII    CPS self-defining ascii field geometry file.
!     GEOMIO_HYBRID   CPS self-defining hybrid binary field geometry file.
!
! The default value of ENCODING for output is GEOMIO_ASCII.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                
!
!  1. The chaining flag in the field geometry file must be one of these string
!     values (although this primitive does not enforce these values):
!
!         chaining      description
!         --------      ------------
!         'HORI'        horizontal chaining
!         'SLOP'        slope chaining
!         'NONE'        no chaining
!
!  2. The code on the zero-trace cards (ZT1 and ZT2 and ZT3 and ZT4) must have
!     one of these string values (although this primitive does not enforce
!     these values):
!
!          code         description
!         ------        ------------
!         'ZERO'        kill the trace
!         'REV'         reverse the polarity of the trace
!         'MISS'        the trace is missing
!
!  3. File size comparisons for a typical field geometry:
!
!             oldcps:    40822 bytes
!             ascii:     21409 bytes (small because mostly nils)
!             binary:    25810 bytes (discontinued)
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!010. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!  9. 2006-01-10  B. Menger  Removed Unused Variables.
!  8. 2003-12-09  Stoeckley  Change default encoding to self-defining ascii.
!  7. 2002-06-24  Stoeckley  Remove augmentation of VE, DATUM, FIXDIST, GRID.
!  6. 2002-04-11  Stoeckley  Add MSG argument in call to FIOUTIL_VERIFY1;
!                             remove one call to geomio_augment; modify to
!                             allow reading field geometry files with unknown
!                             number of cards.
!  5. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  4. 2000-11-27  Stoeckley  Added ability to read and write self-defining
!                             ascii and binary files.
!  3. 2000-09-27  Stoeckley  Change some intent(out) statements to intent(in);
!                             remove the GET and SET subroutines;
!                             remove unused references to the FIO primitive;
!                             remove unused variables;                       
!                             remove some commented-out unimplemented code.
!  2. 2000-08-28  Stoeckley  Add routines to write field geometry files.
!  1. 2000-03-07  Stoeckley  Initial version.
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
 
 
      module geomio_module
      use geomio1_module
      use geomio2_module
      use pjar_module
      use dio_module
      use fio_module
      use fioutil_module
      use string_module
      use named_constants_module
      use grid_module
      implicit none
      public
      private :: geomio_private_open

      character(len=100),public,save :: GEOMIO_IDENT = &
       '$Id: geomio.f90,v 1.10 2006/09/18 13:32:48 Glover prod sps $'

      interface geomio_open_read
         module procedure geomio_open_read_args
         module procedure geomio_open_read_pjar
      end interface

      interface geomio_open_write
         module procedure geomio_open_write_args
         module procedure geomio_open_write_pjar
      end interface

      integer,public ,parameter :: GEOMIO_OK         = FIO_OK
      integer,public ,parameter :: GEOMIO_ERROR      = FIO_ERROR
      integer,public ,parameter :: GEOMIO_EOF        = FIO_EOF
 
      character(len=8),public,parameter  :: GEOMIO_ASCII   = FIO_ASCII
      character(len=8),public,parameter  :: GEOMIO_HYBRID  = FIO_HYBRID
      character(len=8),public,parameter  :: GEOMIO_OLDCPS  = 'oldcps'

      character(len=20),private,parameter :: DEFAULT_FILETYPE = 'geometry'
      character(len=20),private,parameter :: DEFAULT_SECNAME  = 'geometry'

      character(len=20),private,parameter :: DEFAULT_LD_SECNAME  = 'ldcards'
      character(len=20),private,parameter :: DEFAULT_RP_SECNAME  = 'rpcards'
      character(len=20),private,parameter :: DEFAULT_PP_SECNAME  = 'ppcards'
      character(len=20),private,parameter :: DEFAULT_ZT1_SECNAME = 'zt1cards'
      character(len=20),private,parameter :: DEFAULT_ZT2_SECNAME = 'zt2cards'
      character(len=20),private,parameter :: DEFAULT_ZT3_SECNAME = 'zt3cards'
      character(len=20),private,parameter :: DEFAULT_ZT4_SECNAME = 'zt4cards'

      type,public :: geomio_struct
           private
           type(geomio2_struct)   ,pointer :: geomio2
           type(dio_struct)       ,pointer :: dio
           type(fio_struct)       ,pointer :: fio
           type(pjar_struct)      ,pointer :: pjar
           type(pjar_struct)      ,pointer :: mypjar
           character(len=8)                :: encoding
           character(len=12)               :: ld_secname
           character(len=12)               :: rp_secname
           character(len=12)               :: pp_secname
           character(len=12)               :: zt1_secname
           character(len=12)               :: zt2_secname
           character(len=12)               :: zt3_secname
           character(len=12)               :: zt4_secname
      end type geomio_struct


      contains


!!------------------------------ private open -------------------------------!!
!!------------------------------ private open -------------------------------!!
!!------------------------------ private open -------------------------------!!


      subroutine geomio_private_open (obj,pjar)
      implicit none
      type(geomio_struct),pointer            :: obj             ! arguments
      type(pjar_struct),intent(inout),target :: pjar            ! arguments

      allocate (obj)
      nullify (obj%geomio2)
      nullify (obj%dio)
      nullify (obj%fio)
      nullify (obj%mypjar)
      obj%pjar        => pjar
      obj%encoding    = 'unset'
      obj%ld_secname  = ' '
      obj%rp_secname  = ' '
      obj%pp_secname  = ' '
      obj%zt1_secname = ' '
      obj%zt2_secname = ' '
      obj%zt3_secname = ' '
      obj%zt4_secname = ' '
      return
      end subroutine geomio_private_open


!!------------------------------ augment ------------------------------------!!
!!------------------------------ augment ------------------------------------!!
!!------------------------------ augment ------------------------------------!!

! called before writing a file to disk.
! pickle jar contents are modified.


      subroutine geomio_augment (pjar,secname)
      implicit none
      type(pjar_struct)  ,intent(inout) :: pjar                  ! arguments
      character(len=*)   ,intent(in)    :: secname               ! arguments
 !!!  type(grid_struct)                 :: grid                  ! local
      character(len=8)                  :: encoding              ! local
      character(len=8)                  :: nilstring             ! local
      integer                           :: nld,nrp,npp           ! local
      integer                           :: nzt1,nzt2,nzt3,nzt4   ! local
      character(len=12)                 :: ld_secname            ! local
      character(len=12)                 :: rp_secname            ! local
      character(len=12)                 :: pp_secname            ! local
      character(len=12)                 :: zt1_secname           ! local
      character(len=12)                 :: zt2_secname           ! local
      character(len=12)                 :: zt3_secname           ! local
      character(len=12)                 :: zt4_secname           ! local

!----------augment the main section.

 !!!  call grid_initialize     (grid)
      call pjar_choose_section (pjar, secname)

 !!!  call pjar_augment (pjar, 've'      ,  0.0)
 !!!  call pjar_augment (pjar, 'datum'   ,  0.0)
 !!!  call pjar_augment (pjar, 'fixdist' ,  0.0)
      call pjar_augment (pjar, 'chaining',  ' ')
 !!!  call pjar_augment (pjar, 'grid'    , grid)
      call pjar_augment (pjar, 'nld'     ,   0 )
      call pjar_augment (pjar, 'nrp'     ,   0 )
      call pjar_augment (pjar, 'npp'     ,   0 )
      call pjar_augment (pjar, 'nzt1'    ,   0 )
      call pjar_augment (pjar, 'nzt2'    ,   0 )
      call pjar_augment (pjar, 'nzt3'    ,   0 )
      call pjar_augment (pjar, 'nzt4'    ,   0 )

      call pjar_get     (pjar,'encoding' ,encoding, 'unset')
      call pjar_get     (pjar, 'nld'     ,  nld)
      call pjar_get     (pjar, 'nrp'     ,  nrp)
      call pjar_get     (pjar, 'npp'     ,  npp)
      call pjar_get     (pjar, 'nzt1'    ,  nzt1)
      call pjar_get     (pjar, 'nzt2'    ,  nzt2)
      call pjar_get     (pjar, 'nzt3'    ,  nzt3)
      call pjar_get     (pjar, 'nzt4'    ,  nzt4)

      if (encoding == GEOMIO_OLDCPS) return

!----------now encoding may not even be in the main section.

      call pjar_augment (pjar, 'ld_secname' ,  DEFAULT_LD_SECNAME )
      call pjar_augment (pjar, 'rp_secname' ,  DEFAULT_RP_SECNAME )
      call pjar_augment (pjar, 'pp_secname' ,  DEFAULT_PP_SECNAME )
      call pjar_augment (pjar, 'zt1_secname',  DEFAULT_ZT1_SECNAME)
      call pjar_augment (pjar, 'zt2_secname',  DEFAULT_ZT2_SECNAME)
      call pjar_augment (pjar, 'zt3_secname',  DEFAULT_ZT3_SECNAME)
      call pjar_augment (pjar, 'zt4_secname',  DEFAULT_ZT4_SECNAME)

      call pjar_get     (pjar, 'ld_secname' , ld_secname )
      call pjar_get     (pjar, 'rp_secname' , rp_secname )
      call pjar_get     (pjar, 'pp_secname' , pp_secname )
      call pjar_get     (pjar, 'zt1_secname', zt1_secname)
      call pjar_get     (pjar, 'zt2_secname', zt2_secname)
      call pjar_get     (pjar, 'zt3_secname', zt3_secname)
      call pjar_get     (pjar, 'zt4_secname', zt4_secname)

!----------get default values for subsections.

      if (encoding == 'unset') encoding = GEOMIO_ASCII 
      nilstring = 'n'

      call pjar_remove_keyword (pjar, 'encoding')

!----------augment the separate subsections.

      if (nld > 0) then
           call fioutil_augment1 (pjar, ld_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'sp     ', FNIL)
           call fioutil_augment2 (pjar, 'dist   ', FNIL)
           call fioutil_augment2 (pjar, 'xloc   ', FNIL)
           call fioutil_augment2 (pjar, 'yloc   ', FNIL)
           call fioutil_augment2 (pjar, 'elev   ', FNIL)
           call fioutil_augment2 (pjar, 'depth  ', FNIL)
           call fioutil_augment2 (pjar, 'tuh    ', FNIL)
           call fioutil_augment2 (pjar, 'tr     ', FNIL)
           call fioutil_augment2 (pjar, 'ts     ', FNIL)
           call fioutil_augment2 (pjar, 'xsd    ', FNIL)
           call fioutil_augment2 (pjar, 'ysd    ', FNIL)
           call fioutil_augment2 (pjar, 'elsd   ', FNIL)
           call fioutil_augment2 (pjar, 'line   ',  0  )
           call fioutil_augment3 (pjar)
      end if

      if (nrp > 0) then
           call fioutil_augment1 (pjar, rp_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'ipat1  ',     0  )
           call fioutil_augment2 (pjar, 'flag   ',    'X' )
           call fioutil_augment2 (pjar, 'sp1    ',    0.0 )
           call fioutil_augment2 (pjar, 'line1  ',     0  )
           call fioutil_augment2 (pjar, 'nx     ',     1  )
           call fioutil_augment2 (pjar, 'ixinc  ',     1  )
           call fioutil_augment2 (pjar, 'ny     ',     1  )
           call fioutil_augment2 (pjar, 'iyinc  ',     1  )
           call fioutil_augment2 (pjar, 'xsd1   ',    0.0 )
           call fioutil_augment2 (pjar, 'ysd1   ',    0.0 )
           call fioutil_augment2 (pjar, 'elsd1  ',    0.0 )
           call fioutil_augment3 (pjar)
      end if

      if (npp > 0) then
           call fioutil_augment1 (pjar, pp_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'sp2    ',   FNIL )
           call fioutil_augment2 (pjar, 'line2  ',   INIL )
           call fioutil_augment2 (pjar, 'sp3    ',   FNIL )
           call fioutil_augment2 (pjar, 'line3  ',   INIL )
           call fioutil_augment2 (pjar, 'ipat2  ',   INIL )
           call fioutil_augment2 (pjar, 'xsd2   ',   FNIL )
           call fioutil_augment2 (pjar, 'ysd2   ',   FNIL )
           call fioutil_augment2 (pjar, 'hold   ',   INIL )
           call fioutil_augment2 (pjar, 'elev2  ',   FNIL )
           call fioutil_augment2 (pjar, 'depth2 ',   FNIL )
           call fioutil_augment2 (pjar, 'tuh2   ',   FNIL )
           call fioutil_augment2 (pjar, 'is     ',   INIL )
           call fioutil_augment2 (pjar, 'ir     ',   INIL )
           call fioutil_augment2 (pjar, 'ig     ',   INIL )
           call fioutil_augment3 (pjar)
      end if

      if (nzt1 > 0) then
           call fioutil_augment1 (pjar, zt1_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'ccc1   ',   'ZERO')
           call fioutil_augment2 (pjar, 'sss1   ',     0.0 )
           call fioutil_augment2 (pjar, 'sss1a  ',     0.0 )
           call fioutil_augment2 (pjar, 'lll1   ',      0  )
           call fioutil_augment3 (pjar)
      end if

      if (nzt2 > 0) then
           call fioutil_augment1 (pjar, zt2_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'ccc2   ',   'ZERO')
           call fioutil_augment2 (pjar, 'rrr2   ',     0.0 )
           call fioutil_augment2 (pjar, 'rrr2a  ',     0.0 )
           call fioutil_augment2 (pjar, 'lll2   ',      0  )
           call fioutil_augment3 (pjar)
      end if

      if (nzt3 > 0) then
           call fioutil_augment1 (pjar, zt3_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'ccc3   ',   'ZERO')
           call fioutil_augment2 (pjar, 'iggg3  ',      0  )
           call fioutil_augment2 (pjar, 'iggg3a ',      0  )
           call fioutil_augment2 (pjar, 'ittt3  ',      0  )
           call fioutil_augment2 (pjar, 'ittt3a ',      0  )
           call fioutil_augment3 (pjar)
      end if

      if (nzt4 > 0) then
           call fioutil_augment1 (pjar, zt4_secname, encoding)
           call pjar_augment     (pjar, 'nilstring', nilstring)
           call fioutil_augment2 (pjar, 'ccc4   ',   'ZERO')
           call fioutil_augment2 (pjar, 'sss4   ',     0.0 )
           call fioutil_augment2 (pjar, 'sss4a  ',     0.0 )
           call fioutil_augment2 (pjar, 'lll4   ',      0  )
           call fioutil_augment2 (pjar, 'rrr4   ',     0.0 )
           call fioutil_augment2 (pjar, 'rrr4a  ',     0.0 )
           call fioutil_augment2 (pjar, 'lll4a  ',      0  )
           call fioutil_augment3 (pjar)
      end if

      call pjar_choose_section (pjar,secname)
      return
      end subroutine geomio_augment


!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!

! called after reading file header sections from disk.
! called before using the the pickle jar to read a data section from disk.
! called before writing a file to disk.
! first: optional pickle jar contents are augmented.
! then: all pickle jar contents are verified.


      subroutine geomio_verify (pjar,secname,err,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar                  ! arguments
      character(len=*) ,intent(in)    :: secname               ! arguments
      integer          ,intent(out)   :: err                   ! arguments
      character(len=*) ,intent(out)   :: msg                   ! arguments
      character(len=8)                :: encoding              ! local
      integer                         :: nld,nrp,npp           ! local
      integer                         :: nzt1,nzt2,nzt3,nzt4   ! local
      character(len=12)               :: ld_secname            ! local
      character(len=12)               :: rp_secname            ! local
      character(len=12)               :: pp_secname            ! local
      character(len=12)               :: zt1_secname           ! local
      character(len=12)               :: zt2_secname           ! local
      character(len=12)               :: zt3_secname           ! local
      character(len=12)               :: zt4_secname           ! local
      logical                         :: whoops                ! local

!----------get started verifying main section:

      call fioutil_verify1 (pjar,secname,msg)

      if(msg /= ' ') then
          err = GEOMIO_ERROR
          return
      end if

!----------augment optional parameters:

      call pjar_augment (pjar, 'nld'          ,        0     )
      call pjar_augment (pjar, 'nrp'          ,        0     )
      call pjar_augment (pjar, 'npp'          ,        0     )
      call pjar_augment (pjar, 'nzt1'         ,        0     )
      call pjar_augment (pjar, 'nzt2'         ,        0     )
      call pjar_augment (pjar, 'nzt3'         ,        0     )
      call pjar_augment (pjar, 'nzt4'         ,        0     )

      call pjar_augment (pjar, 'ld_secname' ,  DEFAULT_LD_SECNAME )
      call pjar_augment (pjar, 'rp_secname' ,  DEFAULT_RP_SECNAME )
      call pjar_augment (pjar, 'pp_secname' ,  DEFAULT_PP_SECNAME )
      call pjar_augment (pjar, 'zt1_secname',  DEFAULT_ZT1_SECNAME)
      call pjar_augment (pjar, 'zt2_secname',  DEFAULT_ZT2_SECNAME)
      call pjar_augment (pjar, 'zt3_secname',  DEFAULT_ZT3_SECNAME)
      call pjar_augment (pjar, 'zt4_secname',  DEFAULT_ZT4_SECNAME)

!----------get selected parameters to test:

      call pjar_get (pjar, 'nld'             , nld         )
      call pjar_get (pjar, 'nrp'             , nrp         )
      call pjar_get (pjar, 'npp'             , npp         )
      call pjar_get (pjar, 'nzt1'            , nzt1        )
      call pjar_get (pjar, 'nzt2'            , nzt2        )
      call pjar_get (pjar, 'nzt3'            , nzt3        )
      call pjar_get (pjar, 'nzt4'            , nzt4        )

      call pjar_get (pjar, 'ld_secname' , ld_secname )
      call pjar_get (pjar, 'rp_secname' , rp_secname )
      call pjar_get (pjar, 'pp_secname' , pp_secname )
      call pjar_get (pjar, 'zt1_secname', zt1_secname)
      call pjar_get (pjar, 'zt2_secname', zt2_secname)
      call pjar_get (pjar, 'zt3_secname', zt3_secname)
      call pjar_get (pjar, 'zt4_secname', zt4_secname)

!----------test selected parameters:

                    !!!                   missing        invalid
                    !!!                      |              |
      call fioutil_verify2 ('nld'   , (nld  == INIL), (nld  < -1))
      call fioutil_verify2 ('nrp'   , (nrp  == INIL), (nrp  < -1))
      call fioutil_verify2 ('npp'   , (npp  == INIL), (npp  < -1))
      call fioutil_verify2 ('nzt1'  , (nzt1 == INIL), (nzt1 < -1))
      call fioutil_verify2 ('nzt2'  , (nzt2 == INIL), (nzt2 < -1))
      call fioutil_verify2 ('nzt3'  , (nzt3 == INIL), (nzt3 < -1))
      call fioutil_verify2 ('nzt4'  , (nzt4 == INIL), (nzt4 < -1))

!----------finish up verifying main section:

      call fioutil_verify3 (pjar,msg)

      if(msg /= ' ') then
          err = GEOMIO_ERROR
          return
      end if

      call pjar_get (pjar, 'encoding', encoding)

!----------verify subsections:

      if (encoding /= GEOMIO_OLDCPS) then

           whoops = .false.

           if (nld > 0) then
                call fioutil_verify (pjar,ld_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (nrp > 0) then
                call fioutil_verify (pjar,rp_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (npp > 0) then
                call fioutil_verify (pjar,pp_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (nzt1 > 0) then
                call fioutil_verify (pjar,zt1_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (nzt2 > 0) then
                call fioutil_verify (pjar,zt2_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (nzt3 > 0) then
                call fioutil_verify (pjar,zt3_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (nzt4 > 0) then
                call fioutil_verify (pjar,zt4_secname,msg)
                if (msg /= ' ') go to 999
                call pjar_get (pjar, 'encoding', encoding)
                if (encoding == FIO_BINARY) whoops = .true.
           end if

           if (whoops .and. msg == ' ') then
   msg = 'binary encoding not allowed - must be ascii or hybrid or oldcps'
           end if

      end if

!----------finish up and return:

999   if(msg == ' ') then
          err = GEOMIO_OK
          msg =   'LD ' //trim(string_ii2ss(nld ))//    &
                '  RP ' //trim(string_ii2ss(nrp ))//    &
                '  PP ' //trim(string_ii2ss(npp ))//    &
                '  ZT1 '//trim(string_ii2ss(nzt1))//    &
                '  ZT2 '//trim(string_ii2ss(nzt2))//    &
                '  ZT3 '//trim(string_ii2ss(nzt3))//    &
                '  ZT4 '//trim(string_ii2ss(nzt4))
      else
          err = GEOMIO_ERROR
      end if

      call pjar_choose_section (pjar,secname)
      return
      end subroutine geomio_verify


!!--------------------------- geomio open read ----------------------------!!
!!--------------------------- geomio open read ----------------------------!!
!!--------------------------- geomio open read ----------------------------!!
 
 
      subroutine geomio_open_read_args                                     &
                               (obj,filename,err,msg,                      &
                                ve,datum,fixdist,grid,                     &
                                chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,  &
                                phist,nhist)
      implicit none
      type(geomio_struct),pointer            :: obj                 ! arguments
      character(len=*) ,intent(in)           :: filename            ! arguments
      integer          ,intent(out)          :: err                 ! arguments
      character(len=*) ,intent(out)          :: msg                 ! arguments
      real             ,intent(out)          :: ve,datum,fixdist    ! arguments
      type(grid_struct),intent(out)          :: grid                ! arguments
      character(len=*) ,intent(out)          :: chaining            ! arguments
      integer          ,intent(out)          :: nld,nrp,npp         ! arguments
      integer          ,intent(out)          :: nzt1,nzt2,nzt3,nzt4 ! arguments
      character(len=*) ,pointer    ,optional :: phist(:)            ! arguments
      integer          ,intent(out),optional :: nhist               ! arguments
      type(pjar_struct),pointer              :: pjar                ! local

      nullify (pjar) ! jpa
      
      call pjar_create (pjar)

      call geomio_open_read    (obj,filename,pjar,DEFAULT_SECNAME,err,msg)
      obj%mypjar => pjar

      call pjar_choose_section (pjar,DEFAULT_SECNAME)
      call pjar_get            (pjar, 've'       , ve)
      call pjar_get            (pjar, 'datum'    , datum)
      call pjar_get            (pjar, 'fixdist'  , fixdist)
      call pjar_get            (pjar, 'grid'     , grid)
      call pjar_get            (pjar, 'chaining' , chaining)
      call pjar_get            (pjar, 'nld'      , nld)
      call pjar_get            (pjar, 'nrp'      , nrp)
      call pjar_get            (pjar, 'npp'      , npp)
      call pjar_get            (pjar, 'nzt1'     , nzt1)
      call pjar_get            (pjar, 'nzt2'     , nzt2)
      call pjar_get            (pjar, 'nzt3'     , nzt3)
      call pjar_get            (pjar, 'nzt4'     , nzt4)

      call pjar_choose_section (pjar,'history')
      call pjar_alloc_cards    (pjar,phist,nhist)
      return
      end subroutine geomio_open_read_args
 
 
!!--------------------------- geomio open write ---------------------------!!
!!--------------------------- geomio open write ---------------------------!!
!!--------------------------- geomio open write ---------------------------!!
 

      subroutine geomio_open_write_args                                    &
                               (obj,filename,err,msg,                      &
                                ve,datum,fixdist,grid,                     &
                                chaining,nld,nrp,npp,nzt1,nzt2,nzt3,nzt4,  &
                                hist,nhist,progname,encoding)
      implicit none
      type(geomio_struct),pointer            :: obj                 ! arguments
      character(len=*) ,intent(in)           :: filename            ! arguments
      integer          ,intent(out)          :: err                 ! arguments
      character(len=*) ,intent(out)          :: msg                 ! arguments
      real             ,intent(in)           :: ve,datum,fixdist    ! arguments
      type(grid_struct),intent(in)           :: grid                ! arguments
      character(len=*) ,intent(in)           :: chaining            ! arguments
      integer          ,intent(in)           :: nld,nrp,npp         ! arguments
      integer          ,intent(in)           :: nzt1,nzt2,nzt3,nzt4 ! arguments
      character(len=*) ,intent(in) ,optional :: hist(:)             ! arguments
      integer          ,intent(in) ,optional :: nhist               ! arguments
      character(len=*) ,intent(in) ,optional :: progname            ! arguments
      character(len=*) ,intent(in) ,optional :: encoding            ! arguments
      type(pjar_struct),pointer              :: pjar                ! local

      nullify (pjar) ! jpa

      call pjar_create (pjar)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)
                             call pjar_put (pjar, 've'       , ve)
                             call pjar_put (pjar, 'datum'    , datum)
                             call pjar_put (pjar, 'fixdist'  , fixdist)
                             call pjar_put (pjar, 'grid'     , grid)
                             call pjar_put (pjar, 'chaining' , chaining)
                             call pjar_put (pjar, 'nld'      , nld)
                             call pjar_put (pjar, 'nrp'      , nrp)
                             call pjar_put (pjar, 'npp'      , npp)
                             call pjar_put (pjar, 'nzt1'     , nzt1)
                             call pjar_put (pjar, 'nzt2'     , nzt2)
                             call pjar_put (pjar, 'nzt3'     , nzt3)
                             call pjar_put (pjar, 'nzt4'     , nzt4)
      if (present(encoding)) call pjar_put (pjar,'encoding'  ,encoding)

      call pjar_choose_section (pjar,'history')
      call pjar_put_cards      (pjar,hist,nhist,progname)

      call geomio_open_write   (obj,filename,pjar,DEFAULT_SECNAME,err,msg)
      obj%mypjar => pjar
      return
      end subroutine geomio_open_write_args
 
 
!!--------------------------- geomio open read ----------------------------!!
!!--------------------------- geomio open read ----------------------------!!
!!--------------------------- geomio open read ----------------------------!!
 
 
      subroutine geomio_open_read_pjar (obj,filename,pjar,secname,err,msg)
      implicit none
      type(geomio_struct),pointer              :: obj             ! arguments
      character(len=*)   ,intent(in)           :: filename        ! arguments
      type(pjar_struct)  ,intent(inout),target :: pjar            ! arguments
      character(len=*)   ,intent(in)           :: secname         ! arguments
      integer            ,intent(out)          :: err             ! arguments
      character(len=*)   ,intent(out)          :: msg             ! arguments
 
!!!!!!!!!! initialize data structure:

      call geomio_private_open (obj,pjar)
      call pjar_clear          (pjar)

!!!!!!!!!! open input file:
 
      call dio_open_read (obj%dio, filename, err, msg)
      if (err /= GEOMIO_OK) return

!!!!!!!!!! try reading oldcps file:
 
      call geomio1_read_header (obj%dio,pjar,secname,err,msg)
      if (err == GEOMIO_OK) then
           obj%encoding = GEOMIO_OLDCPS
           call pjar_choose_section (pjar,secname)
           call pjar_put            (pjar,'encoding' ,obj%encoding)
           call geomio_verify       (pjar,secname,err,msg)
           return
      end if

!!!!!!!!!! read newcps file:

      obj%encoding = FIO_ASCII   ! only needs to be different from oldcps.

      call fio_create               (obj%fio,obj%dio)
      call fio_read_header_sections (obj%fio,pjar,err,msg)
      if (err /= GEOMIO_OK) return

      call geomio_verify            (pjar, secname, err, msg)
      if (err /= GEOMIO_OK) return

      call geomio2_create           (obj%geomio2,obj%fio,pjar,secname)
      call pjar_choose_section      (pjar,secname)

      call pjar_get     (pjar, 'ld_secname' , obj%ld_secname )
      call pjar_get     (pjar, 'rp_secname' , obj%rp_secname )
      call pjar_get     (pjar, 'pp_secname' , obj%pp_secname )
      call pjar_get     (pjar, 'zt1_secname', obj%zt1_secname)
      call pjar_get     (pjar, 'zt2_secname', obj%zt2_secname)
      call pjar_get     (pjar, 'zt3_secname', obj%zt3_secname)
      call pjar_get     (pjar, 'zt4_secname', obj%zt4_secname)
      return
      end subroutine geomio_open_read_pjar
 
 
!!--------------------------- geomio open write ---------------------------!!
!!--------------------------- geomio open write ---------------------------!!
!!--------------------------- geomio open write ---------------------------!!
 

      subroutine geomio_open_write_pjar (obj,filename,pjar,secname,err,msg)
      implicit none
      type(geomio_struct),pointer              :: obj             ! arguments
      character(len=*)   ,intent(in)           :: filename        ! arguments
      type(pjar_struct)  ,intent(inout),target :: pjar            ! arguments
      character(len=*)   ,intent(in)           :: secname         ! arguments
      integer            ,intent(out)          :: err             ! arguments
      character(len=*)   ,intent(out)          :: msg             ! arguments
 
!!!!!!!!!! initialize data structure:

      call geomio_private_open    (obj,pjar)

      call geomio_augment (pjar, secname)
      call geomio_verify  (pjar, secname, err, msg)
      if (err /= GEOMIO_OK) return

      call pjar_choose_section    (pjar,secname)
      call pjar_get               (pjar,'encoding' ,obj%encoding)

!!!!!!!!!! open output file:
 
      call dio_open_write (obj%dio,filename,err,msg)
      if (err /= GEOMIO_OK) return

!!!!!!!!!! write oldcps file:
 
      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_header (obj%dio,pjar,secname,err,msg)
           return
      end if
 
!!!!!!!!!! write newcps file:
 
      call fio_create                (obj%fio,obj%dio)
      call fio_write_header_sections (obj%fio,pjar,err,msg,DEFAULT_FILETYPE)
      if (err /= GEOMIO_OK) return

      call geomio2_create            (obj%geomio2,obj%fio,pjar,secname)
      call pjar_choose_section       (pjar,secname)

      call pjar_get     (pjar, 'ld_secname' , obj%ld_secname )
      call pjar_get     (pjar, 'rp_secname' , obj%rp_secname )
      call pjar_get     (pjar, 'pp_secname' , obj%pp_secname )
      call pjar_get     (pjar, 'zt1_secname', obj%zt1_secname)
      call pjar_get     (pjar, 'zt2_secname', obj%zt2_secname)
      call pjar_get     (pjar, 'zt3_secname', obj%zt3_secname)
      call pjar_get     (pjar, 'zt4_secname', obj%zt4_secname)
      return
      end subroutine geomio_open_write_pjar
 
 
!!--------------------------- geomio open foreign -------------------------!!
!!--------------------------- geomio open foreign -------------------------!!
!!--------------------------- geomio open foreign -------------------------!!
 

      subroutine geomio_open_foreign (obj,filename,pjar,secname,err,msg)
      implicit none
      type(geomio_struct),pointer              :: obj             ! arguments
      character(len=*)   ,intent(in)           :: filename        ! arguments
      type(pjar_struct)  ,intent(inout),target :: pjar            ! arguments
      character(len=*)   ,intent(in)           :: secname         ! arguments
      integer            ,intent(out)          :: err             ! arguments
      character(len=*)   ,intent(out)          :: msg             ! arguments

!!!!!!!!!! initialize data structure:

      call geomio_private_open (obj,pjar)

  !!  call geomio_augment (pjar, secname)    ! removed 4/5/2002
      call geomio_verify  (pjar, secname, err, msg)
      if (err /= GEOMIO_OK) return

!!!!!!!!!! open file:
 
      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= GEOMIO_OK) return

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)

!!!!!!!!!! read oldcps file:

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_header (obj%dio,pjar,secname,err,msg)
           if (err == GEOMIO_OK) then
                call geomio_verify  (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! read newcps file:
 
      call fio_create               (obj%fio,obj%dio)
      call geomio2_create           (obj%geomio2,obj%fio,pjar,secname)
      call pjar_choose_section      (pjar,secname)

      call pjar_get     (pjar, 'ld_secname' , obj%ld_secname )
      call pjar_get     (pjar, 'rp_secname' , obj%rp_secname )
      call pjar_get     (pjar, 'pp_secname' , obj%pp_secname )
      call pjar_get     (pjar, 'zt1_secname', obj%zt1_secname)
      call pjar_get     (pjar, 'zt2_secname', obj%zt2_secname)
      call pjar_get     (pjar, 'zt3_secname', obj%zt3_secname)
      call pjar_get     (pjar, 'zt4_secname', obj%zt4_secname)
      return
      end subroutine geomio_open_foreign
 
 
!!--------------------------- geomio close --------------------------------!!
!!--------------------------- geomio close --------------------------------!!
!!--------------------------- geomio close --------------------------------!!
 
 
      subroutine geomio_close (obj)
      implicit none
      type(geomio_struct),pointer :: obj            ! arguments
 
      if (associated(obj)) then
                                       call geomio2_delete (obj%geomio2)
                                       call fio_delete     (obj%fio)
                                       call dio_close      (obj%dio)
           if (associated(obj%mypjar)) call pjar_delete    (obj%mypjar)
           deallocate(obj)
      end if
      return
      end subroutine geomio_close
 
 
!!--------------------- geomio read ld card -------------------------------!!
!!--------------------- geomio read ld card -------------------------------!!
!!--------------------- geomio read ld card -------------------------------!!


      subroutine geomio_read_ld_card  (obj,ild,err,msg,   &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: ild                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real   ,intent(out)  :: sp,dist,xloc,yloc,elev             ! arguments
      real   ,intent(out)  :: depth,tuh,tr,ts,xsd,ysd,elsd       ! arguments
      integer,intent(out)  :: line                               ! arguments








      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_ld_card  (obj%dio,ild,        err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      else
           if (ild == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%ld_secname,err,msg)
           call geomio2_read_ld_card  (obj%geomio2,err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      end if
      return
      end subroutine geomio_read_ld_card


!!--------------------- geomio write ld card -------------------------------!!
!!--------------------- geomio write ld card -------------------------------!!
!!--------------------- geomio write ld card -------------------------------!!


      subroutine geomio_write_ld_card  (obj,ild,err,msg,   &
                  sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: ild                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real   ,intent(in)   :: sp,dist,xloc,yloc,elev             ! arguments
      real   ,intent(in)   :: depth,tuh,tr,ts,xsd,ysd,elsd       ! arguments
      integer,intent(in)   :: line                               ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_ld_card  (obj%dio,ild,        err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      else
           if (ild == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%ld_secname,err,msg)
           call geomio2_write_ld_card  (obj%geomio2,err,msg,   &
                 sp,dist,xloc,yloc,elev,depth,tuh,tr,ts,xsd,ysd,elsd,line)
      end if
      return
      end subroutine geomio_write_ld_card


!!--------------------- geomio read rp card -------------------------------!!
!!--------------------- geomio read rp card -------------------------------!!
!!--------------------- geomio read rp card -------------------------------!!


      subroutine geomio_read_rp_card  (obj,irp,err,msg,   &
                  ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: irp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      integer            ,intent(out)   :: ipat1,line1           ! arguments
      integer            ,intent(out)   :: nx,ixinc,ny,iyinc     ! arguments
      real               ,intent(out)   :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)   ,intent(out)   :: flag                  ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_rp_card  (obj%dio,irp,        err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      else
           if (irp == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%rp_secname,err,msg)
           call geomio2_read_rp_card  (obj%geomio2,err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      end if
      return
      end subroutine geomio_read_rp_card


!!--------------------- geomio write rp card -------------------------------!!
!!--------------------- geomio write rp card -------------------------------!!
!!--------------------- geomio write rp card -------------------------------!!


      subroutine geomio_write_rp_card  (obj,irp,err,msg,   &
                  ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: irp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      integer            ,intent(in)    :: ipat1,line1           ! arguments
      integer            ,intent(in)    :: nx,ixinc,ny,iyinc     ! arguments
      real               ,intent(in)    :: sp1,xsd1,ysd1,elsd1   ! arguments
      character(len=*)   ,intent(in)    :: flag                  ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_rp_card  (obj%dio,irp,        err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      else
           if (irp == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%rp_secname,err,msg)
           call geomio2_write_rp_card  (obj%geomio2,err,msg,   &
                 ipat1,flag,sp1,line1,nx,ixinc,ny,iyinc,xsd1,ysd1,elsd1)
      end if
      return
      end subroutine geomio_write_rp_card


!!--------------------- geomio read pp card -------------------------------!!
!!--------------------- geomio read pp card -------------------------------!!
!!--------------------- geomio read pp card -------------------------------!!


      subroutine geomio_read_pp_card  (obj,ipp,err,msg,   &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: ipp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real               ,intent(out)   :: sp2,sp3,xsd2,ysd2     ! arguments
      integer            ,intent(out)   :: line2,line3,ipat2     ! arguments
      real               ,intent(out)   :: elev2,depth2,tuh2     ! arguments
      integer            ,intent(out)   :: hold,is,ir,ig         ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_pp_card  (obj%dio,ipp,        err,msg,   &
            sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      else
           if (ipp == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%pp_secname,err,msg)
           call geomio2_read_pp_card  (obj%geomio2,err,msg,   &
            sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      end if
      return
      end subroutine geomio_read_pp_card


!!--------------------- geomio write pp card -------------------------------!!
!!--------------------- geomio write pp card -------------------------------!!
!!--------------------- geomio write pp card -------------------------------!!


      subroutine geomio_write_pp_card  (obj,ipp,err,msg,   &
           sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: ipp                   ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      real               ,intent(in)    :: sp2,sp3,xsd2,ysd2     ! arguments
      integer            ,intent(in)    :: line2,line3,ipat2     ! arguments
      real               ,intent(in)    :: elev2,depth2,tuh2     ! arguments
      integer            ,intent(in)    :: hold,is,ir,ig         ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_pp_card  (obj%dio,ipp,        err,msg,   &
            sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      else
           if (ipp == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%pp_secname,err,msg)
           call geomio2_write_pp_card  (obj%geomio2,err,msg,   &
            sp2,line2,sp3,line3,ipat2,xsd2,ysd2,hold,elev2,depth2,tuh2,is,ir,ig)
      end if
      return
      end subroutine geomio_write_pp_card


!!--------------------- geomio read zt1 card -------------------------------!!
!!--------------------- geomio read zt1 card -------------------------------!!
!!--------------------- geomio read zt1 card -------------------------------!!


      subroutine geomio_read_zt1_card  (obj,izt1,err,msg,   &
                                        ccc1,sss1,sss1a,lll1)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt1                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc1                  ! arguments
      real               ,intent(out)   :: sss1,sss1a            ! arguments
      integer            ,intent(out)   :: lll1                  ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_zt1_card  (obj%dio,izt1,        err,msg,   &
                                        ccc1,sss1,sss1a,lll1)
      else
           if (izt1 == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%zt1_secname,err,msg)
           call geomio2_read_zt1_card  (obj%geomio2, err,msg,   &
                                        ccc1,sss1,sss1a,lll1)
      end if
      return
      end subroutine geomio_read_zt1_card


!!--------------------- geomio write zt1 card -------------------------------!!
!!--------------------- geomio write zt1 card -------------------------------!!
!!--------------------- geomio write zt1 card -------------------------------!!


      subroutine geomio_write_zt1_card  (obj,izt1,err,msg,   &
                                         ccc1,sss1,sss1a,lll1)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt1                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc1                  ! arguments
      real               ,intent(in)    :: sss1,sss1a            ! arguments
      integer            ,intent(in)    :: lll1                  ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_zt1_card  (obj%dio,izt1,       err,msg,   &
                                         ccc1,sss1,sss1a,lll1)
      else
           if (izt1 == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%zt1_secname,err,msg)
           call geomio2_write_zt1_card  (obj%geomio2,err,msg,   &
                                         ccc1,sss1,sss1a,lll1)
      end if
      return
      end subroutine geomio_write_zt1_card


!!--------------------- geomio read zt2 card -------------------------------!!
!!--------------------- geomio read zt2 card -------------------------------!!
!!--------------------- geomio read zt2 card -------------------------------!!


      subroutine geomio_read_zt2_card  (obj,izt2,err,msg,   &
                                        ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt2                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc2                  ! arguments
      real               ,intent(out)   :: rrr2,rrr2a            ! arguments
      integer            ,intent(out)   :: lll2                  ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_zt2_card  (obj%dio,izt2,       err,msg,   &
                                        ccc2,rrr2,rrr2a,lll2)
      else
           if (izt2 == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%zt2_secname,err,msg)
           call geomio2_read_zt2_card  (obj%geomio2,err,msg,   &
                                        ccc2,rrr2,rrr2a,lll2)
      end if
      return
      end subroutine geomio_read_zt2_card


!!--------------------- geomio write zt2 card -------------------------------!!
!!--------------------- geomio write zt2 card -------------------------------!!
!!--------------------- geomio write zt2 card -------------------------------!!


      subroutine geomio_write_zt2_card  (obj,izt2,err,msg,   &
                                         ccc2,rrr2,rrr2a,lll2)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt2                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc2                  ! arguments
      real               ,intent(in)    :: rrr2,rrr2a            ! arguments
      integer            ,intent(in)    :: lll2                  ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_zt2_card  (obj%dio,izt2,       err,msg,   &
                                         ccc2,rrr2,rrr2a,lll2)
      else
           if (izt2 == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%zt2_secname,err,msg)
           call geomio2_write_zt2_card  (obj%geomio2,err,msg,   &
                                         ccc2,rrr2,rrr2a,lll2)
      end if
      return
      end subroutine geomio_write_zt2_card


!!--------------------- geomio read zt3 card -------------------------------!!
!!--------------------- geomio read zt3 card -------------------------------!!
!!--------------------- geomio read zt3 card -------------------------------!!


      subroutine geomio_read_zt3_card  (obj,izt3,err,msg,   &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt3                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc3                  ! arguments
      integer            ,intent(out)   :: iggg3,iggg3a          ! arguments
      integer            ,intent(out)   :: ittt3,ittt3a          ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_zt3_card  (obj%dio,izt3,       err,msg,   &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      else
           if (izt3 == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%zt3_secname,err,msg)
           call geomio2_read_zt3_card  (obj%geomio2,err,msg,   &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      end if
      return
      end subroutine geomio_read_zt3_card


!!--------------------- geomio write zt3 card -------------------------------!!
!!--------------------- geomio write zt3 card -------------------------------!!
!!--------------------- geomio write zt3 card -------------------------------!!


      subroutine geomio_write_zt3_card (obj,izt3,err,msg,   &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt3                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc3                  ! arguments
      integer            ,intent(in)    :: iggg3,iggg3a          ! arguments
      integer            ,intent(in)    :: ittt3,ittt3a          ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_zt3_card (obj%dio,izt3,       err,msg,   &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      else
           if (izt3 == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%zt3_secname,err,msg)
           call geomio2_write_zt3_card (obj%geomio2,err,msg,   &
                                        ccc3,iggg3,iggg3a,ittt3,ittt3a)
      end if
      return
      end subroutine geomio_write_zt3_card


!!--------------------- geomio read zt4 card -------------------------------!!
!!--------------------- geomio read zt4 card -------------------------------!!
!!--------------------- geomio read zt4 card -------------------------------!!


      subroutine geomio_read_zt4_card  (obj,izt4,err,msg,   &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt4                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(out)   :: ccc4                  ! arguments
      real               ,intent(out)   :: sss4,sss4a            ! arguments
      real               ,intent(out)   :: rrr4,rrr4a            ! arguments
      integer            ,intent(out)   :: lll4,lll4a            ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_read_zt4_card  (obj%dio,izt4,       err,msg,   &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      else
           if (izt4 == 1) call fio_read_data_section &
                                     (obj%fio,obj%pjar,obj%zt4_secname,err,msg)
           call geomio2_read_zt4_card  (obj%geomio2,err,msg,   &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      end if
      return
      end subroutine geomio_read_zt4_card


!!--------------------- geomio write zt4 card -------------------------------!!
!!--------------------- geomio write zt4 card -------------------------------!!
!!--------------------- geomio write zt4 card -------------------------------!!


      subroutine geomio_write_zt4_card (obj,izt4,err,msg,   &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      implicit none
      type(geomio_struct),intent(inout) :: obj                   ! arguments
      integer            ,intent(in)    :: izt4                  ! arguments
      integer            ,intent(out)   :: err                   ! arguments
      character(len=*)   ,intent(out)   :: msg                   ! arguments
      character(len=*)   ,intent(in)    :: ccc4                  ! arguments
      real               ,intent(in)    :: sss4,sss4a            ! arguments
      real               ,intent(in)    :: rrr4,rrr4a            ! arguments
      integer            ,intent(in)    :: lll4,lll4a            ! arguments

      if (obj%encoding == GEOMIO_OLDCPS) then
           call geomio1_write_zt4_card (obj%dio,izt4,       err,msg,   &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      else
           if (izt4 == 1) call fio_write_data_section &
                                     (obj%fio,obj%pjar,obj%zt4_secname,err,msg)
           call geomio2_write_zt4_card (obj%geomio2,err,msg,   &
                                        ccc4,sss4,sss4a,lll4,rrr4,rrr4a,lll4a)
      end if
      return
      end subroutine geomio_write_zt4_card


!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
!!------------------------- end of module ---------------------------------!!
 

      end module geomio_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
