!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- statio.f90 --------------------------------!!
!!------------------------------- statio.f90 --------------------------------!!
!!------------------------------- statio.f90 --------------------------------!!


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
! Name       : STATIO
! Category   : io
! Written    : 2000-03-01   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : To read and write static files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading and writing the following
! static file formats:
!
!   (1) old-style CPS ascii  static files (using the STATIO1 primitive).
!   (2) self-defining ascii  static files (using the STATIO2 primitive).
!   (3) self-defining hybrid static files (using the STATIO2 primitive).
!   (4) self-defining binary static files (using the STATIO2 primitive).
!
! This primitive is smart enough to decide automatically what format to read.
! This primitive does not try to support the old binary static file format
! or the old columnar format used in the experimental program MSEPITA.  CBYT,
! CFG, ISEP, and MSEPITA will be upgraded to use the new static file formats.
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
! Read or write a file:
!  (parameters are in the argument list)
!  (intended primarily for batch programs)
!
!                               i        o      o   o   o    o
!    call statio_read_file  (filename,stattype,nhx,nhy,nhx2,nhy2,
!                            x1,y1,xinc,yinc,nx,ny,pstatics,
!                            o  o   o    o   o  o     o    
!
!                               opt   opt    opt      opt 
!                       o   o    o     o      i        i 
!                      err,msg,phist,nhist,progname,lunprint,
!                      encoding,fields,nfields,nilstring,wrap)
!                         o       o       o       o       o  
!                        opt     opt     opt     opt     opt
!
!
!                               i        i      i   i   i    i
!    call statio_write_file (filename,stattype,nhx,nhy,nhx2,nhy2,
!                            x1,y1,xinc,yinc,nx,ny,statics,
!                            i  i   i    i   i  i     i   
!
!                               opt   opt    opt      opt
!                       o   o    i     i      i        i 
!                      err,msg,hist,nhist,progname,lunprint,
!                      encoding,fields,nfields,nilstring,wrap)
!                         i       i       i       i       i
!                        opt     opt     opt     opt     opt
!
!
! Read or write a file:
!  (the same parameters are in the pickle jar)
!  (intended primarily for interactive programs)
!
!                                 i      b      i       o      o   o
!    call statio_read_header  (filename,pjar,secname,         err,msg)
!    call statio_read_file    (filename,pjar,secname,pstatics,err,msg)
!    call statio_read_foreign (filename,pjar,secname,pstatics err,msg)
!    call statio_scan_foreign (filename,pjar,secname,         err,msg)
!    call statio_write_file   (filename,pjar,secname, statics,err,msg)
!                                 i      b      i       i      o   o 
!
!
! Verify or augment or print parameters:
!
!                          i      i     o   o
!    call statio_verify  (pjar,secname,err,msg)
!    call statio_augment (pjar,secname)
!                          b      i 
!
!                            i      i      i     o   o
!    call statio_print   (filename,pjar,secname,err,msg,
!                         io,progname,lunprint,statics)
!                         i     i        i       i
!                        opt   opt      opt     opt
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(pjar_struct)  pjar = reference to the pickle jar data structure.
!
! char(*) secname  = name of the section on the file to read or write.
!                      (the history section is also read or written)
!
! char(*) filename = name of the static file for input or output.
! integer err      = error flag (returned).
! char(*) msg      = message for possible printing (returned).
! char(*) io       = the string 'input' or 'output'.
!
! char(*) stattype       = type of static file (up to 8 characters).
! integer nhx            = header word containing the X ground position.
! integer nhy            = header word containing the Y ground position.
! integer nhx2           = second X header word for source=receiver file.
! integer nhy2           = second Y header word for source=receiver file.
! real    x1             = smallext X ground position in the file.
! real    y1             = smallext Y ground position in the file.
! real    xinc           = increment between X ground positions (> zero).
! real    yinc           = increment between Y ground positions (> zero).
! integer nx             = number of ground positions in X direction.
! integer ny             = number of ground positions in Y direction
! real    statics(nx*ny) = array of static values (1 or 2 dimensions).
! real   pstatics(nx*ny) = pointer to array of static values (1 or 2 dims).
!
! real    statmin =    minimum    static value  determined while scanning.
! real    statmax =    maximum    static value  determined while scanning.
! integer numnils = number of nil static values determined while scanning.
!
! char(*) hist (nhist) = array of history cards (up to 80 characters).
! char(*) phist(nhist) = pointer to array of history cards (up to 80 chars).
! integer nhist        = number of history cards.
! char(*) progname     = name of program or process accessing this file.
! integer lunprint     = logical unit number for printing (or <= 0).
!
! char(*) encoding    = encoding format of static file.
! char(*) fields(:)   = list of static value fields to read or write.
! integer nfields     = number of static value fields to read or write.
! char(*) nilstring   = string for nil values in the file (default '-nil-').
! integer wrap        = number of card images per record (normally 1).
! integer firstline   = line to start reading on (for foreign files only).
! char(*) template    = template for adjusting foreign input card image.
! integer maxchars(:) = maximum number of characters in each field to decode.
! integer nmaxchars   = number of maxchars.
!
! The STATICS and PSTATICS arrays can be either one dimension (nx*ny) or two
! dimensions (nx,ny).
!
! PSTATICS and PHIST are pointers which must be nullified before first use.
! They will be deallocated and reallocated to contain the returned contents.
! They will always be allocated to a dimension of at least one.  They should
! be conditionally deallocated when no longer needed.
!
! NILSTRING and WRAP are used only for ascii encoding (including foreign files).
!
! FIRSTLINE, TEMPLATE, and MAXCHARS(NMAXCHARS) are used only to read foreign
! files.  See documentation in the FIO primitive for more information.
!
! See documentation in the STATIO2 primitive for permitted values for the
! FIELDS(NFIELDS) array.
!
!-------------------------------------------------------------------------------
!                        RETURNED ERROR FLAGS
!
! The returned error will have one of these integer named constant values:
!
!          err             description
!          ---             -----------
!          STATIO_OK       the operation was successful.
!          STATIO_ERROR    an open or read/write error occurred.
!
!-------------------------------------------------------------------------------
!                     STATIC FILE ENCODING FORMATS
!
! The encoding format of the static file to read or write must be one of these
! character(len=8) named constant values:
!
!     encoding        description
!     --------        -----------
!     STATIO_OLDCPS   old-style CPS static file.
!     STATIO_ASCII    CPS self-defining ascii static file.
!     STATIO_HYBRID   CPS self-defining columnar binary static file.
!     STATIO_BINARY   CPS self-defining blocked binary static file.
!
! The default value of ENCODING for output is STATIO_ASCII.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                
!
!  1. The usual values for NHX and NHY are as follows (although other values
!     are also OK):
!                                      source NHX NHY   receiver NHX NHY
!                                      --------------   ----------------
!          group number:                       9   0
!          sequential ground position:        46   0             47   0
!          grid coordinates:                  33  34             35  36
!          shotpoint and line number:         29  26             28  27
!
!     NHX and NHY cannot both be zero.  If only one header word is to be used,
!     set NHY=0, NY=1, Y1=anything, and YINC=nonzero.
!
!  2. The second set of header words (NHX2 and NHY2) is used only for
!     source=receiver static files (i.e. static files to be used for both
!     sources and receivers).  Otherwise they should both be set to zero.
!     If they are non-zero, NHX should be matched with NHX2, and NHY should
!     be matched with NHY2, by replacing the source header word with the
!     corresponding receiver header word or vice versa.
!
!  3. The one-dimensional index for STATIC(IX,IY) is STATIC(IX+(NX-1)*IY).
!
!  4. The usual values for STATTYPE are as follows (although other values
!     are also OK):
!
!       stattype      description           summed into trace header word
!       --------      ------------          -----------------------------
!       'REFR'        refraction statics    41 (cumulative refraction static)
!       'DATUM'       datum statics         42 (cumulative datum static)
!       'RESID'       residual statics      43 (cumulative residual static)
!       anything else                       none
!
!     When a static shift is applied to a seismic trace in the processing
!     system, the process which shifts the trace should increment the
!     appropriate header word by the amount of the static applied, based on
!     the STATTYPE in the static file from which the static was obtained.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!008. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!  7. 2003-12-09  Stoeckley  Change default encoding to self-defining ascii.
!  6. 2002-04-11  Stoeckley  Add MSG argument in call to FIOUTIL_VERIFY1.
!  5. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  4. 2000-11-27  Stoeckley  Extensively overhauled, with the motivation to
!                             simplify use and maintenance in an interactive
!                             environment where foreign files with missing
!                             information might be read, and to allow saving
!                             history records on self-defining static files;
!                             moved most of the code to new primitives
!                             STATIO1 and STATIO2.
!  3. 2000-06-27  Stoeckley  Improved printout of type of static file.
!  2. 2000-04-07  Stoeckley  Improved the optional printouts.
!  1. 2000-03-01  Stoeckley  Initial version.
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
 
 
      module statio_module
      use fioutil_module
      use statio1_module
      use statio2_module
      use pjar_module
      use dio_module
      use fio_module
      use string_module
      use named_constants_module
      use statutil_module
      implicit none
      private
      public :: statio_read_header
      public :: statio_read_file
      public :: statio_read_foreign
      public :: statio_scan_foreign
      public :: statio_write_file
      public :: statio_verify
      public :: statio_augment
      public :: statio_print
 
      character(len=100),public,save :: STATIO_IDENT = &
'$Id: statio.f90,v 1.8 2006/09/18 13:32:51 Glover prod sps $'

      integer,public,parameter          :: STATIO_OK      = FIO_OK
      integer,public,parameter          :: STATIO_ERROR   = FIO_ERROR
 
      character(len=8),public,parameter :: STATIO_ASCII   = FIO_ASCII
      character(len=8),public,parameter :: STATIO_BINARY  = FIO_BINARY
      character(len=8),public,parameter :: STATIO_HYBRID  = FIO_HYBRID
      character(len=8),public,parameter :: STATIO_OLDCPS  = 'oldcps'

      character(len=20),private,parameter :: DEFAULT_FILETYPE = 'static'
      character(len=20),private,parameter :: DEFAULT_SECNAME  = 'static'

      interface statio_read_file
           module procedure statio_read_file_1d
           module procedure statio_read_file_2d
           module procedure statio_read_file_pjar
      end interface

      interface statio_write_file
           module procedure statio_write_file_1d
           module procedure statio_write_file_2d
           module procedure statio_write_file_pjar
      end interface


      contains


!!----------------------------- augment ----------------------------------!!
!!----------------------------- augment ----------------------------------!!
!!----------------------------- augment ----------------------------------!!

! called before writing a file to disk.
! pickle jar contents are modified.


      subroutine statio_augment (pjar,secname)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      integer                         :: nhx,nhy            ! local
      real                            :: x1,y1              ! local

      call fioutil_augment1 (pjar, secname, STATIO_ASCII)

      call pjar_get (pjar, 'nhx'      , nhx     , 0)
      call pjar_get (pjar, 'nhy'      , nhy     , 0)
      call pjar_get (pjar, 'x1'       , x1      , 0.0)
      call pjar_get (pjar, 'y1'       , y1      , 0.0)

                      !!!   (pjar, field, defval, hdr,fieldtype,unit,delimiter)

      call fioutil_augment2 (pjar, 'xcoord',  x1 , nhx)
      call fioutil_augment2 (pjar, 'ycoord',  y1 , nhy)
      call fioutil_augment2 (pjar, 'static', FNIL,  0 )

      call fioutil_augment3 (pjar)
      return
      end subroutine statio_augment


!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!

! called after reading file header sections from disk.
! called before using the the pickle jar to read a data section from disk.
! called before writing a file to disk.
! first: optional pickle jar contents are augmented.
! then: all pickle jar contents are verified.


      subroutine statio_verify (pjar,secname,err,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      integer          ,intent(out)   :: err                ! arguments
      character(len=*) ,intent(out)   :: msg                ! arguments
      character(len=8)                :: encoding           ! local
      integer                         :: nhx,nhy,nhx2,nhy2  ! local
      real                            :: x1,y1,xinc,yinc    ! local
      integer                         :: nx,ny              ! local

!----------get started:

      call fioutil_verify1 (pjar,secname,msg)

      if(msg /= ' ') then
          err = STATIO_ERROR
          return
      end if

!----------augment optional parameters:

      call pjar_augment (pjar, 'stattype', 'MISC')

!----------get selected parameters to test:

      call pjar_get (pjar, 'nhx'     , nhx       )
      call pjar_get (pjar, 'nhy'     , nhy       )
      call pjar_get (pjar, 'nhx2'    , nhx2      )
      call pjar_get (pjar, 'nhy2'    , nhy2      )
      call pjar_get (pjar, 'x1'      , x1        )
      call pjar_get (pjar, 'y1'      , y1        )
      call pjar_get (pjar, 'xinc'    , xinc      )
      call pjar_get (pjar, 'yinc'    , yinc      )
      call pjar_get (pjar, 'nx'      , nx        )
      call pjar_get (pjar, 'ny'      , ny        )

!----------test selected parameters:

                    !!!                 missing         invalid
                    !!!                    |               |
      call fioutil_verify2 ('nhx'  , (nhx  == INIL), (nhx  <=   0))
      call fioutil_verify2 ('nhy'  , (nhy  == INIL), (nhy  <    0))
      call fioutil_verify2 ('nhx2' , (nhx2 == INIL), (nhx2 <    0))
      call fioutil_verify2 ('nhy2' , (nhy2 == INIL), (nhy2 <    0))
      call fioutil_verify2 ('x1'   , (x1   == FNIL))
      call fioutil_verify2 ('y1'   , (y1   == FNIL))
      call fioutil_verify2 ('xinc' , (xinc == FNIL), (xinc <= 0.0))
      call fioutil_verify2 ('yinc' , (yinc == FNIL), (yinc <= 0.0))
      call fioutil_verify2 ('nx'   , (nx   == INIL), (nx   <=   0))
      call fioutil_verify2 ('ny'   , (ny   == INIL), (ny   <=   0))

!----------finish up and return:

      call fioutil_verify3 (pjar,msg)

      if(msg /= ' ') then
          err = STATIO_ERROR
          return
      end if

      call pjar_get (pjar, 'encoding', encoding)

      if (encoding /= STATIO_OLDCPS) then
           call fioutil_verify (pjar,secname,msg)
      end if

      if(msg == ' ') then
          err = STATIO_OK
          msg = 'XY headers = '//trim(string_ii2ss(nhx))//' '      &
                               //trim(string_ii2ss(nhy))//'      ' &
                 //'XY bins = '//trim(string_ii2ss(nx)) //' '      &
                               //trim(string_ii2ss(ny))
      else
          err = STATIO_ERROR
      end if
      return
      end subroutine statio_verify


!!---------------------------- statio print --------------------------------!!
!!---------------------------- statio print --------------------------------!!
!!---------------------------- statio print --------------------------------!!


      subroutine statio_print (filename,pjar,secname,err,msg, &
                               io,progname,lunprint,statics)
      implicit none
      character(len=*) ,intent(in)          :: filename          ! arguments
      type(pjar_struct),intent(inout)       :: pjar              ! arguments
      character(len=*) ,intent(in)          :: secname           ! arguments
      integer          ,intent(in)          :: err               ! arguments
      character(len=*) ,intent(in)          :: msg               ! arguments
      character(len=*) ,intent(in),optional :: io                ! arguments
      character(len=*) ,intent(in),optional :: progname          ! arguments
      integer          ,intent(in),optional :: lunprint          ! arguments
      real             ,intent(in),optional :: statics(:)        ! arguments
      character(len=8)                      :: stattype          ! local
      integer                               :: nhx,nhy,nhx2,nhy2 ! local
      real                                  :: x1,y1,xinc,yinc   ! local
      integer                               :: nx,ny             ! local
      character(len=8)                      :: encoding          ! local
      character(len=80)  ,pointer           :: hist(:)           ! local
      integer                               :: nhist,indx        ! local
      character(len=80)                     :: which             ! local
      character(len=80)                     :: phrase            ! local
      real                                  :: xend,yend         ! local
      integer                               :: nstatics          ! local
      real                                  :: statmin,statmax   ! local
      integer                               :: numnils           ! local

!----------return if no printing desired.

      if (.not.present(lunprint)) return
      if (lunprint <= 0)          return

!----------get started.

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'encoding', encoding  )
      call pjar_get            (pjar, 'stattype', stattype  )
      call pjar_get            (pjar, 'nhx'     , nhx       )
      call pjar_get            (pjar, 'nhy'     , nhy       )
      call pjar_get            (pjar, 'nhx2'    , nhx2      )
      call pjar_get            (pjar, 'nhy2'    , nhy2      )
      call pjar_get            (pjar, 'x1'      , x1        )
      call pjar_get            (pjar, 'y1'      , y1        )
      call pjar_get            (pjar, 'xinc'    , xinc      )
      call pjar_get            (pjar, 'yinc'    , yinc      )
      call pjar_get            (pjar, 'nx'      , nx        )
      call pjar_get            (pjar, 'ny'      , ny        )

!----------print initial information.

      write(lunprint,*) ' '
      write(lunprint,*) 'STATIO: FILENAME = ',trim(filename)

      select case (encoding)
        case (STATIO_OLDCPS ) ; phrase = 'old-style CPS static file'
        case (STATIO_ASCII  ) ; phrase = 'CPS self-defining ascii static file'
        case (STATIO_BINARY ) ; phrase = 'CPS self-defining binary static file'
        case (STATIO_HYBRID ) ; phrase = 'CPS self-defining hybrid static file'
        case default          ; phrase = 'file of unknown format attempted'
      end select

      if (present(io)) then
           phrase = trim(phrase)//' '//io
      else
           phrase = trim(phrase)//' accessed'
      end if

      if (present(progname)) then
           write(lunprint,*) 'STATIO: ',trim(phrase),' by ',trim(progname)
      else
           write(lunprint,*) 'STATIO: ',trim(phrase)
      end if

!----------return if an error occurred.

      if (err /= STATIO_OK) then
                            write(lunprint,*) 'STATIO: ',trim(msg)
           if (present(io)) write(lunprint,*) 'STATIO: file not ',trim(io)
                            write(lunprint,*) ' '
           return
      end if

!----------print file header.

      if (stattype /= ' ' .or. nhx  /= 0 .or. nhy  /= 0   &
                          .or. nhx2 /= 0 .or. nhy2 /= 0) then
           if      (nhx2 /= 0) then ; which =                      '(S=R FILE)'
           else if (nhx ==  9) then ; which =                    '(GROUP FILE)'
           else if (nhx == 46) then ; which =        '(SEQUENTIAL SOURCE FILE)'
           else if (nhx == 33) then ; which =              '(GRID SOURCE FILE)'
           else if (nhx == 29) then ; which =   '(SHOTPOINT/LINE# SOURCE FILE)'
           else if (nhx == 47) then ; which =      '(SEQUENTIAL RECEIVER FILE)'
           else if (nhx == 35) then ; which =            '(GRID RECEIVER FILE)'
           else if (nhx == 28) then ; which = '(SHOTPOINT/LINE# RECEIVER FILE)'
           else                     ; which =                               ' '
           end if

           write(lunprint,*)  &
                        'STATIO: STATTYPE = ',trim(stattype),'  ',trim(which)
           write(lunprint,1001) nhx,nhy, nhx2,nhy2
      end if

      xend = x1 + (nx-1)*xinc
      yend = y1 + (ny-1)*yinc
      write(lunprint,1002) x1,y1
      write(lunprint,1003) xinc,yinc
      write(lunprint,1004) xend,yend
      write(lunprint,1005) nx,ny
1001  format (' STATIO: X,Y HEADER WORDS =              ',i8,i14,3x,2i4)
1002  format (' STATIO: STARTING X,Y GROUND POSITIONS = ',2g14.6)
1003  format (' STATIO: X,Y GROUND POSITION INCREMENTS =',2g14.6)
1004  format (' STATIO: ENDING X,Y GROUND POSITIONS =   ',2g14.6)
1005  format (' STATIO: NUMBER OF X,Y GROUND POSITIONS =',i8,i14)

!----------print history cards.

      nullify (hist)
      call pjar_choose_section (pjar, 'history')
      call pjar_alloc_cards    (pjar, hist, nhist)

      if (nhist == 0) then
           write(lunprint,*) 'STATIO: NO HISTORY CARDS'
      else
           write(lunprint,*) 'STATIO: ',nhist,' HISTORY CARDS:'
           do indx = 1,nhist
                write (lunprint,2000) trim(hist(indx))
           enddo
      end if
      if (associated(hist)) deallocate (hist)

2000  format (' STATIO:  ',a)

!----------print summary of static values.

      nstatics = nx * ny
      write(lunprint,*) 'STATIO: total number of static values = ',nstatics

      if (present(statics)) then
        call statutil_scan_statics (nstatics,statics,statmin,statmax,numnils)
        write(lunprint,*) 'STATIO: number of nil static values   = ',numnils
        write(lunprint,*) 'STATIO: minimum static value          = ',statmin
        write(lunprint,*) 'STATIO: maximum static value          = ',statmax
      end if

!----------finish up and return.

      write(lunprint,*) 'STATIO: ',trim(msg)
      write(lunprint,*) ' '
      return
      end subroutine statio_print


!!------------------- copy between 1d and 2d arrays -----------------------!!
!!------------------- copy between 1d and 2d arrays -----------------------!!
!!------------------- copy between 1d and 2d arrays -----------------------!!


      subroutine statio_private_copy_1d_to_2d (err,nx,ny,tempspace,pstatics)
      implicit none
      integer         ,intent(in) :: err,nx,ny              ! arguments
      real            ,intent(in) :: tempspace(:)           ! argumsnts
      real            ,pointer    :: pstatics(:,:)          ! arguments
      integer                     :: ix,iy,indx             ! local
 
      if (err == STATIO_OK) then
           if (associated(pstatics)) deallocate (pstatics)
           allocate (pstatics(max(nx,1),max(ny,1)))
           indx = 0
           do iy = 1,ny
           do ix = 1,nx
                indx = indx + 1
                pstatics(ix,iy) = tempspace(indx)
           end do
           end do
      else
           if (associated(pstatics)) deallocate (pstatics)
           allocate (pstatics(1,1))
      end if
      return
      end subroutine statio_private_copy_1d_to_2d



      subroutine statio_private_copy_2d_to_1d (nx,ny,statics,tempspace)
      implicit none
      integer         ,intent(in)  :: nx,ny                  ! arguments
      real            ,intent(in)  :: statics(:,:)           ! arguments
      real            ,intent(out) :: tempspace(:)           ! argumsnts
      integer                      :: ix,iy,indx             ! local
 
      indx = 0
      do iy = 1,ny
      do ix = 1,nx
           indx = indx + 1
           tempspace(indx) = statics(ix,iy)
      end do
      end do
      return
      end subroutine statio_private_copy_2d_to_1d


!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!
!!-------------------------- public routines ------------------------------!!


!!-------------------- statio scan foreign ---------------------------------!!
!!-------------------- statio scan foreign ---------------------------------!!
!!-------------------- statio scan foreign ---------------------------------!!


      subroutine statio_scan_foreign (filename,pjar,secname,err,msg)
      implicit none
      character(len=*)    ,intent(in)           :: filename      ! arguments
      type(pjar_struct)   ,intent(inout)        :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments
      integer             ,intent(out)          :: err           ! arguments
      character(len=*)    ,intent(out)          :: msg           ! arguments
      type(dio_struct)    ,pointer              :: dio           ! local
      type(fio_struct)    ,pointer              :: fio           ! local
      character(len=8)                          :: encoding      ! local
 
!----------get started:

      nullify (dio)
      nullify (fio)

!----------open file:

      call dio_open_read (dio,filename,err,msg)
      if (err /= STATIO_OK) return

      call pjar_get (pjar, 'encoding', encoding)

!----------scan oldcps file:

      if (encoding == STATIO_OLDCPS) then
           call statio1_scan_file (dio,pjar,secname,err,msg)
           go to 999
      end if

!----------scan newcps file:

      call fio_create              (fio,dio)
      call fio_read_data_section   (fio,pjar,secname,err,msg)
      if (err /= STATIO_OK) go to 999

      call statio2_scan_statics    (fio,pjar,secname,err,msg)

!----------finish up and return:

999   if (err == STATIO_OK) msg = 'static file successfully scanned'

      call fio_delete (fio)
      call dio_close  (dio)
      return
      end subroutine statio_scan_foreign


!!---------------------- statio read foreign -------------------------------!!
!!---------------------- statio read foreign -------------------------------!!
!!---------------------- statio read foreign -------------------------------!!


      subroutine statio_read_foreign (filename,pjar,secname,pstatics,err,msg)
      implicit none
      character(len=*)    ,intent(in)           :: filename      ! arguments
      type(pjar_struct)   ,intent(inout)        :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments
      real                ,pointer              :: pstatics(:)   ! arguments
      integer             ,intent(out)          :: err           ! arguments
      character(len=*)    ,intent(out)          :: msg           ! arguments
      type(dio_struct)    ,pointer              :: dio           ! local
      type(fio_struct)    ,pointer              :: fio           ! local
      character(len=8)                          :: encoding      ! local

!----------get started:

      nullify (dio)
      nullify (fio)

      call statio_verify (pjar, secname, err, msg)
      if (err /= STATIO_OK) return

!----------open file:

      call dio_open_read (dio,filename,err,msg)
      if (err /= STATIO_OK) return

      call pjar_get (pjar, 'encoding', encoding)

!----------read oldcps file:

      if (encoding == STATIO_OLDCPS) then
           call statio1_read_file (dio,pjar,secname,err,msg,pstatics)
           if (err == STATIO_OK) then
                call statio_verify  (pjar, secname, err, msg)
           end if
           go to 999
      end if

!----------read newcps file:

      call fio_create            (fio,dio)
      call fio_read_data_section (fio,pjar,secname,err,msg)
      if (err /= STATIO_OK) go to 999

      call statio2_read_statics  (fio,pjar,secname,err,msg,pstatics)

!----------finish up and return:

999   if (err == STATIO_OK) msg = 'static file successfully read'

      call fio_delete (fio)
      call dio_close  (dio)
      return
      end subroutine statio_read_foreign


!!---------------------- statio read header --------------------------------!!
!!---------------------- statio read header --------------------------------!!
!!---------------------- statio read header --------------------------------!!


      subroutine statio_read_header (filename,pjar,secname,err,msg)
      implicit none
      character(len=*)    ,intent(in)           :: filename      ! arguments
      type(pjar_struct)   ,intent(inout)        :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments
      integer             ,intent(out)          :: err           ! arguments
      character(len=*)    ,intent(out)          :: msg           ! arguments
      type(dio_struct)    ,pointer              :: dio           ! local
      type(fio_struct)    ,pointer              :: fio           ! local

!----------get started:

      nullify (dio)
      nullify (fio)
      call pjar_clear (pjar)

!----------open input file:

      call dio_open_read (dio,filename,err,msg)
      if (err /= STATIO_OK) return

!----------read oldcps file:

      call statio1_read_header (dio,pjar,secname,err,msg)
      if (err == STATIO_OK) then
           call pjar_choose_section (pjar, secname)
           call pjar_put            (pjar, 'encoding', STATIO_OLDCPS)
           call statio_verify       (pjar, secname, err, msg)
           go to 999
      end if

!----------read newcps file:

      call fio_create               (fio,dio)
      call fio_read_header_sections (fio,pjar,err,msg)
      if (err /= STATIO_OK) go to 999

      call statio_verify (pjar, secname, err, msg)

!----------finish up and return:

999   call fio_delete (fio)
      call dio_close  (dio)
      return
      end subroutine statio_read_header


!!---------------------- statio read file pjar -----------------------------!!
!!---------------------- statio read file pjar -----------------------------!!
!!---------------------- statio read file pjar -----------------------------!!


      subroutine statio_read_file_pjar (filename,pjar,secname,pstatics,err,msg)
      implicit none
      character(len=*)    ,intent(in)           :: filename      ! arguments
      type(pjar_struct)   ,intent(inout)        :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments
      real                ,pointer              :: pstatics(:)   ! arguments
      integer             ,intent(out)          :: err           ! arguments
      character(len=*)    ,intent(out)          :: msg           ! arguments
      type(dio_struct)    ,pointer              :: dio           ! local
      type(fio_struct)    ,pointer              :: fio           ! local

!----------get started:

      nullify (dio)
      nullify (fio)
      call pjar_clear (pjar)

!----------open input file:

      call dio_open_read (dio,filename,err,msg)
      if (err /= STATIO_OK) return

!----------read oldcps file:

      call statio1_read_file (dio,pjar,secname,err,msg,pstatics)
      if (err == STATIO_OK) then
           call pjar_choose_section (pjar, secname)
           call pjar_put            (pjar, 'encoding', STATIO_OLDCPS)
           call statio_verify       (pjar, secname, err, msg)
           go to 999
      end if

!----------read newcps file:

      call fio_create               (fio,dio)
      call fio_read_header_sections (fio,pjar,err,msg)
      if (err /= STATIO_OK) go to 999

      call statio_verify (pjar, secname, err, msg)
      if (err /= STATIO_OK) go to 999

      call fio_read_data_section    (fio,pjar,secname,err,msg)
      if (err /= STATIO_OK) go to 999

      call statio2_read_statics     (fio,pjar,secname,err,msg,pstatics)

!----------finish up and return:

999   if (err == STATIO_OK) msg = 'static file successfully read'

      call fio_delete (fio)
      call dio_close  (dio)
      return
      end subroutine statio_read_file_pjar


!!---------------------- statio write file pjar -----------------------------!!
!!---------------------- statio write file pjar -----------------------------!!
!!---------------------- statio write file pjar -----------------------------!!


      subroutine statio_write_file_pjar (filename,pjar,secname,statics,err,msg)
      implicit none
      character(len=*)    ,intent(in)           :: filename      ! arguments
      type(pjar_struct)   ,intent(inout)        :: pjar          ! arguments
      character(len=*)    ,intent(in)           :: secname       ! arguments
      real                ,intent(in)           :: statics(:)    ! arguments
      integer             ,intent(out)          :: err           ! arguments
      character(len=*)    ,intent(out)          :: msg           ! arguments
      type(dio_struct)    ,pointer              :: dio           ! local
      type(fio_struct)    ,pointer              :: fio           ! local
      character(len=8)                          :: encoding      ! local

!----------get started:

      nullify (dio)
      nullify (fio)

      call statio_augment (pjar, secname)
      call statio_verify  (pjar, secname, err, msg)
      if (err /= STATIO_OK) return

      call pjar_get (pjar, 'encoding', encoding)

!----------open output file:

      call dio_open_write (dio,filename,err,msg)
      if (err /= STATIO_OK) return

!----------write oldcps file:

      if (encoding == STATIO_OLDCPS) then
           call statio1_write_file (dio,pjar,secname,err,msg,statics)
           go to 999
      end if

!----------write newcps file:

      call fio_create                (fio,dio)
      call fio_write_header_sections (fio,pjar,err,msg,DEFAULT_FILETYPE)
      if (err /= STATIO_OK) go to 999

      call fio_write_data_section    (fio,pjar,secname,err,msg)
      if (err /= STATIO_OK) go to 999

      call statio2_write_statics     (fio,pjar,secname,err,msg,statics)

!----------finish up and return:

999   if (err == STATIO_OK) msg = 'static file successfully written'

      call fio_delete (fio)
      call dio_close  (dio)
      return
      end subroutine statio_write_file_pjar


!!--------------------------- statio read file 2d -------------------------!!
!!--------------------------- statio read file 2d -------------------------!!
!!--------------------------- statio read file 2d -------------------------!!


      subroutine statio_read_file_2d (filename,stattype,nhx,nhy,nhx2,nhy2,   &
                                      x1,y1,xinc,yinc,nx,ny,pstatics,        &
                                      err,msg,phist,nhist,progname,lunprint, &
                                      encoding,fields,nfields,nilstring,wrap)
      implicit none
      character(len=*),intent(in)           :: filename          ! arguments
      character(len=*),intent(out)          :: stattype          ! arguments
      integer         ,intent(out)          :: nhx,nhy,nhx2,nhy2 ! arguments
      real            ,intent(out)          :: x1,y1,xinc,yinc   ! arguments
      integer         ,intent(out)          :: nx,ny             ! arguments
      real            ,pointer              :: pstatics(:,:)     ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      character(len=*),pointer    ,optional :: phist(:)          ! arguments
      integer         ,intent(out),optional :: nhist             ! arguments
      character(len=*),intent(in) ,optional :: progname          ! arguments
      integer         ,intent(in) ,optional :: lunprint          ! arguments
      character(len=*),intent(out),optional :: encoding          ! arguments
      character(len=*),intent(out),optional :: fields(:)         ! arguments
      integer         ,intent(out),optional :: nfields           ! arguments
      character(len=*),intent(out),optional :: nilstring         ! arguments
      integer         ,intent(out),optional :: wrap              ! arguments
      real            ,pointer              :: tempspace(:)      ! local
 
      allocate (tempspace(1))

      call statio_read_file_1d (filename,stattype,nhx,nhy,nhx2,nhy2,   &
                                x1,y1,xinc,yinc,nx,ny,tempspace,       &
                                err,msg,phist,nhist,progname,lunprint, &
                                encoding,fields,nfields,nilstring,wrap)

      call statio_private_copy_1d_to_2d (err,nx,ny,tempspace,pstatics)

      deallocate (tempspace)
      return
      end subroutine statio_read_file_2d


!!--------------------------- statio read file 1d -------------------------!!
!!--------------------------- statio read file 1d -------------------------!!
!!--------------------------- statio read file 1d -------------------------!!


      subroutine statio_read_file_1d (filename,stattype,nhx,nhy,nhx2,nhy2,   &
                                      x1,y1,xinc,yinc,nx,ny,pstatics,        &
                                      err,msg,phist,nhist,progname,lunprint, &
                                      encoding,fields,nfields,nilstring,wrap)
      implicit none
      character(len=*),intent(in)           :: filename          ! arguments
      character(len=*),intent(out)          :: stattype          ! arguments
      integer         ,intent(out)          :: nhx,nhy,nhx2,nhy2 ! arguments
      real            ,intent(out)          :: x1,y1,xinc,yinc   ! arguments
      integer         ,intent(out)          :: nx,ny             ! arguments
      real            ,pointer              :: pstatics(:)       ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      character(len=*),pointer    ,optional :: phist(:)          ! arguments
      integer         ,intent(out),optional :: nhist             ! arguments
      character(len=*),intent(in) ,optional :: progname          ! arguments
      integer         ,intent(in) ,optional :: lunprint          ! arguments
      character(len=*),intent(out),optional :: encoding          ! arguments
      character(len=*),intent(out),optional :: fields(:)         ! arguments
      integer         ,intent(out),optional :: nfields           ! arguments
      character(len=*),intent(out),optional :: nilstring         ! arguments
      integer         ,intent(out),optional :: wrap              ! arguments
      type(pjar_struct),pointer             :: pjar              ! local
      logical                               :: present_fields    ! local
 
      nullify (pjar) ! jpa
      call pjar_create      (pjar)
      call statio_read_file (filename,pjar,DEFAULT_SECNAME,pstatics,err,msg)

      call statio_print (filename,pjar,DEFAULT_SECNAME,err,msg, &
                         'input',progname,lunprint,pstatics)

      call pjar_choose_section (pjar,DEFAULT_SECNAME)

      call pjar_get (pjar, 'stattype', stattype)
      call pjar_get (pjar, 'nhx'     , nhx     )
      call pjar_get (pjar, 'nhy'     , nhy     )
      call pjar_get (pjar, 'nhx2'    , nhx2    )
      call pjar_get (pjar, 'nhy2'    , nhy2    )
      call pjar_get (pjar, 'x1'      , x1      )
      call pjar_get (pjar, 'y1'      , y1      )
      call pjar_get (pjar, 'xinc'    , xinc    )
      call pjar_get (pjar, 'yinc'    , yinc    )
      call pjar_get (pjar, 'nx'      , nx      )
      call pjar_get (pjar, 'ny'      , ny      )

      present_fields = (present(fields))
                 ! needed to get around new absoft compiler bug.

      if (present_fields    ) call pjar_get (pjar, 'fields'   , fields,nfields)
 !!!  if (present(fields   )) call pjar_get (pjar, 'fields'   , fields,nfields)
      if (present(encoding )) call pjar_get (pjar, 'encoding' , encoding      )
      if (present(nilstring)) call pjar_get (pjar, 'nilstring', nilstring     )
      if (present(wrap     )) call pjar_get (pjar, 'wrap'     , wrap          )

      call pjar_choose_section (pjar,'history')
      call pjar_alloc_cards    (pjar,phist,nhist)
      call pjar_delete         (pjar)
      return
      end subroutine statio_read_file_1d


!!-------------------------- statio write file 2d --------------------------!!
!!-------------------------- statio write file 2d --------------------------!!
!!-------------------------- statio write file 2d --------------------------!!


      subroutine statio_write_file_2d (filename,stattype,nhx,nhy,nhx2,nhy2,  &
                                       x1,y1,xinc,yinc,nx,ny,statics,        &
                                       err,msg,hist,nhist,progname,lunprint, &
                                       encoding,fields,nfields,nilstring,wrap)
      implicit none
      character(len=*),intent(in)           :: filename          ! arguments
      character(len=*),intent(in)           :: stattype          ! arguments
      integer         ,intent(in)           :: nhx,nhy,nhx2,nhy2 ! arguments
      real            ,intent(in)           :: x1,y1,xinc,yinc   ! arguments
      integer         ,intent(in)           :: nx,ny             ! arguments
      real            ,intent(in)           :: statics(:,:)      ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      character(len=*),intent(in) ,optional :: hist(:)           ! arguments
      integer         ,intent(in) ,optional :: nhist             ! arguments
      character(len=*),intent(in) ,optional :: progname          ! arguments
      integer         ,intent(in) ,optional :: lunprint          ! arguments
      character(len=*),intent(in) ,optional :: encoding          ! arguments
      character(len=*),intent(in) ,optional :: fields(:)         ! arguments
      integer         ,intent(in) ,optional :: nfields           ! arguments
      character(len=*),intent(in) ,optional :: nilstring         ! arguments
      integer         ,intent(in) ,optional :: wrap              ! arguments
      real                                  :: tempspace(nx*ny)  ! local
 
      call statio_private_copy_2d_to_1d (nx,ny,statics,tempspace)

      call statio_write_file_1d (filename,stattype,nhx,nhy,nhx2,nhy2,  &
                                 x1,y1,xinc,yinc,nx,ny,tempspace,      &
                                 err,msg,hist,nhist,progname,lunprint, &
                                 encoding,fields,nfields,nilstring,wrap)
      return
      end subroutine statio_write_file_2d


!!-------------------------- statio write file 1d --------------------------!!
!!-------------------------- statio write file 1d --------------------------!!
!!-------------------------- statio write file 1d --------------------------!!


      subroutine statio_write_file_1d (filename,stattype,nhx,nhy,nhx2,nhy2,  &
                                       x1,y1,xinc,yinc,nx,ny,statics,        &
                                       err,msg,hist,nhist,progname,lunprint, &
                                       encoding,fields,nfields,nilstring,wrap)
      implicit none
      character(len=*),intent(in)           :: filename          ! arguments
      character(len=*),intent(in)           :: stattype          ! arguments
      integer         ,intent(in)           :: nhx,nhy,nhx2,nhy2 ! arguments
      real            ,intent(in)           :: x1,y1,xinc,yinc   ! arguments
      integer         ,intent(in)           :: nx,ny             ! arguments
      real            ,intent(in)           :: statics(:)        ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      character(len=*),intent(in) ,optional :: hist(:)           ! arguments
      integer         ,intent(in) ,optional :: nhist             ! arguments
      character(len=*),intent(in) ,optional :: progname          ! arguments
      integer         ,intent(in) ,optional :: lunprint          ! arguments
      character(len=*),intent(in) ,optional :: encoding          ! arguments
      character(len=*),intent(in) ,optional :: fields(:)         ! arguments
      integer         ,intent(in) ,optional :: nfields           ! arguments
      character(len=*),intent(in) ,optional :: nilstring         ! arguments
      integer         ,intent(in) ,optional :: wrap              ! arguments
      type(pjar_struct),pointer             :: pjar              ! local
 
      nullify (pjar) ! jpa
      call pjar_create         (pjar)
      call pjar_choose_section (pjar,DEFAULT_SECNAME)

      call pjar_put (pjar, 'stattype', stattype)
      call pjar_put (pjar, 'nhx'     , nhx     )
      call pjar_put (pjar, 'nhy'     , nhy     )
      call pjar_put (pjar, 'nhx2'    , nhx2    )
      call pjar_put (pjar, 'nhy2'    , nhy2    )
      call pjar_put (pjar, 'x1'      , x1      )
      call pjar_put (pjar, 'y1'      , y1      )
      call pjar_put (pjar, 'xinc'    , xinc    )
      call pjar_put (pjar, 'yinc'    , yinc    )
      call pjar_put (pjar, 'nx'      , nx      )
      call pjar_put (pjar, 'ny'      , ny      )

      if (present(encoding )) call pjar_put (pjar, 'encoding' , encoding      )
      if (present(fields   )) call pjar_put (pjar, 'fields'   , fields,nfields)
      if (present(nilstring)) call pjar_put (pjar, 'nilstring', nilstring     )
      if (present(wrap     )) call pjar_put (pjar, 'wrap'     , wrap          )

      call pjar_choose_section (pjar,'history')
      call pjar_put_cards      (pjar,hist,nhist,progname)

      call statio_write_file (filename,pjar,DEFAULT_SECNAME,statics,err,msg)

      call statio_print (filename,pjar,DEFAULT_SECNAME,err,msg, &
                         'output',progname,lunprint,statics)
      call pjar_delete  (pjar)
      return
      end subroutine statio_write_file_1d


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module statio_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
