
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- velio.f90 --------------------------------!!
!!------------------------------- velio.f90 --------------------------------!!
!!------------------------------- velio.f90 --------------------------------!!


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
! Name       : VELIO
! Category   : io
! Written    : 1999-09-26   by: Tom Stoeckley
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : To read and write velocity function files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION               
!
! This primitive is to be used for reading and writing the following
! velocity file formats:
!
!  (1) old-style CPS ascii  velocity files     (using the VELIO1 primitive).
!  (2) self-defining ascii  velocity files     (using the VELIO2 primitive).
!  (3) self-defining hybrid velocity files     (using the VELIO2 primitive).
!  (4) self-defining binary velocity files     (using the VELIO2 primitive).
!  (5) modspec files containing velocities     (using the VELIO3 primitive).
!  (6) TRCIO trace files containing velocities (using the VELIO4 primitive).
!
! This primitive is smart enough to decide automatically what format to read.
! This primitive does not try to support the old CPS binary velocity workfile
! format or the old columnar format used in VA.  VA and GEOPRESS have been
! upgraded to use the new velocity file formats.
!
!-------------------------------------------------------------------------------
!               SPECIAL STEPS TAKEN WITH MODSPEC FILES
!
! Modspec files can be read by this primitive (by calling VELIO3) but cannot
! be written.  Velocities in modspec files are always interval velocities,
! but may be in either the time or depth domain.
!
! All velocity functions obtained from a modspec file will be returned with
! a fixed number of uniformly sampled picks beginning at zero time (or depth)
! and ending at the deepest time or depth found in the modspec file.
!
! The VELIO3 primitive can be used directly to gain additional functionality
! for modspec files, such as reading the velocity functions randomly,
! converting the velocity functions between time and depth, or resampling
! the velocity functions at different sample rates.
!
!-------------------------------------------------------------------------------
!                  SPECIAL STEPS TAKEN WITH TRCIO FILES
!
! TRCIO files can be read by this primitive (by calling VELIO4) but cannot
! be written.  Velocity types in TRCIO files are not specified, so the velocity
! type is returned as a blank string.
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
! Open the velocity file and read or write initial information:
!  (parameters are in the argument list)
!  (intended for batch programs)
!                                                     opt opt   opt    opt
!                            o     i      o    o   o   o   o     o      o
!    call velio_open_read  (obj,filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,
!            phist,nhist,progname,dunits,attname,attunits,tdunits,
!              o     o      i       o       o       o        o   
!             opt   opt    opt     opt     opt     opt      opt 
!
!              opt     opt     opt     opt     opt
!               o       o       o       o       o  
!            encoding,fields,nfields,nilstring,wrap,
!            default_xcoord,default_ycoord,default_veltype)
!                 o              o              o
!                opt            opt            opt
!
!                                                               opt    opt
!                            o     i      i    o   o   i   i     i      i
!    call velio_open_write (obj,filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,
!             hist,nhist,progname,dunits,attname,attunits,tdunits,
!              i     i      i       i       i       i        i   
!             opt   opt    opt     opt     opt     opt      opt 
!
!              opt     opt     opt     opt     opt    opt    opt     opt
!               i       i       i       i       i      i      i       i
!            encoding,fields,nfields,nilstring,wrap,fillout,widths,nwidths,
!            default_xcoord,default_ycoord,default_veltype)
!                 i              i              i
!                opt            opt            opt
!
!
! Open the velocity file and read or write initial information:
!  (the same parameters are in the pickle jar)
!
!                              o     i      b      i     o   o
!    call velio_open_read    (obj,filename,pjar,secname,err,msg)
!    call velio_open_write   (obj,filename,pjar,secname,err,msg)
!    call velio_open_foreign (obj,filename,pjar,secname,err,msg)
!                              o     i      b      i     o   o
!
!
! Close the velocity file:
!                            
!                       b
!    call velio_close (obj)
!
!
! Verify or augment parameters:
!
!                         i      i     o   o
!    call velio_verify  (pjar,secname,err,msg)
!    call velio_augment (pjar,secname)
!                         b      i
!
!
! Read or write a single velocity function:
!                                                       opt    opt
!                              b    o      o      o      o      o     o   o 
!    call velio_read_velfun  (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,
!            velname,veltype,project,line,rdate,pdate,userid,comment)
!               o       o       o     o     o     o     o       o   
!              opt     opt     opt   opt   opt   opt   opt     opt  
!
!                              b    i      i      i      i      i     o   o 
!    call velio_write_velfun (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,
!            velname,veltype,project,line,rdate,pdate,userid,comment)
!               i       i       i     i     i     i     i       i   
!              opt     opt     opt   opt   opt   opt   opt     opt  
!
!
! Scan the contents of a velocity file:
!  (parameters are in the argument list)
!                                                 opt opt  opt     opt
!                              i      o    o   o   o   o    o       o
!    call velio_scan_alloc (filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,
!            maxpicks,xcoords,ycoords,xbins,ybins,nxbins,nybins,
!               o        o       o      o     o     o      o 
!              opt      opt     opt    opt   opt   opt    opt
!
!            phist,nhist,progname,dunits,attname,attunits,tdunits,
!              o     o      i       o       o       o        o   
!             opt   opt    opt     opt     opt     opt      opt 
!
!              opt     opt     opt     opt     opt
!               o       o       o       o       o
!            encoding,fields,nfields,nilstring,wrap,
!            default_xcoord,default_ycoord,default_veltype)
!                 o              o              o
!                opt            opt            opt
!
!
! Scan the contents of a velocity file:
!  (the same parameters are in the pickle jar)
!
!                                i      b      i     o   o
!    call velio_scan_alloc   (filename,pjar,secname,err,msg,
!                             xcoords,ycoords,xbins,ybins,nxbins,nybins)
!                                o       o      o     o     o      o 
!                               opt     opt    opt   opt   opt    opt
!
!                                i      b      i     o   o
!    call velio_scan_foreign (filename,pjar,secname,err,msg,
!                             xcoords,ycoords,xbins,ybins,nxbins,nybins)
!                                o       o      o     o     o      o 
!                               opt     opt    opt   opt   opt    opt
!
!-------------------------------------------------------------------------------
!                         SUBROUTINE ARGUMENTS
!
! type(velio_struct)    obj = pointer to the VELIO data structure.
! type(pjar_struct)    pjar = reference to the pickle jar data structure.
!
! char(*) secname  = name of the section on the file to read or write.
!                      (the history section is also read or written)
!
! char(*) filename = name of the velocity file for input or output.
! integer err      = error flag (returned).
! char(*) msg      = message for possible printing (returned).
!
! integer nfun     = number of velocity functions in the file.
! integer nhx      = CPS trace header word containing X coordinate.
! integer nhy      = CPS trace header word containing Y coordinate.
! real    nmosign  = the   sign   used for the moveout (normally 1.0).
! real    nmoexp   = the exponent used for the moveout (normally 2.0).
! char(*) dunits   = depth units of velocity functions (feet or meters).
! char(*) attname  = any attribute name given to the ordinate (e.g. velocity).
! char(*) attunits = attribute units of the ordinate (e.g. feet/sec).
! char(*) tdunits  = time/depth units of the abscissa (e.g. sec, feet, meters).
!
! char(*) hist (nhist) = array of history cards (up to 80 characters).
! char(*) phist(nhist) = pointer to array of history cards (up to 80 chars).
! integer nhist        = number of history cards.
! char(*) progname     = name of program or process accessing this file.
!
! char(*) encoding    = encoding format of velocity file.
! char(*) fields(:)   = list of velocity function fields to read or write.
! integer nfields     = number of velocity function fields to read or write.
! char(*) nilstring   = string for nil values in the file (default 'nil').
! integer wrap        = number of card images per record (normally 1).
! logical fillout     = whether to always output all columns.
! integer widths(:)   = list of widths to write for each field.
! integer nwidths     = number of widths specified.
! integer firstline   = line to start reading on (for foreign files only).
! char(*) template    = template for adjusting foreign input card image.
! integer maxchars(:) = maximum number of characters in each field to decode.
! integer nmaxchars   = number of maxchars specified.
!
! real    default_xcoord  = default X coord of vel function (if not in column).
! real    default_ycoord  = default Y coord of vel function (if not in column).
! char(*) default_veltype = default  type   of vel function (if not in column).
!
! real    xcoord    = X coordinate of the velocity function.
! real    ycoord    = Y coordinate of the velocity function.
! integer npicks    = number of time/vel picks in the velocity function.
! real    tpicks(:) = array that holds the abscissae (  TIME   picks).
! real    vpicks(:) = array that holds the ordinates (VELOCITY picks).
! char(*) velname   = name of velocity function  (8 characters).
! char(*) veltype   = type of velocity function  (4 characters).
! char(*) project   = project name              (10 characters).
! char(*) line      = line name                 (10 characters).
! char(*) rdate     = recording date             (5 characters).
! char(*) pdate     = processing date            (5 characters).
! char(*) userid    = user ID                    (3 characters).
! char(*) comment   = comment                   (15 characters).
!
! integer maxpicks  = maximum number of picks in any velocity function.
! real,p xcoords(:) = pointer to X coordinates of all NFUN velocity functions.
! real,p ycoords(:) = pointer to Y coordinates of all NFUN velocity functions.
! real,p   xbins(:) = pointer to X coordinates of rectangular array.
! real,p   ybins(:) = pointer to Y coordinates of rectangular array.
! integer nxbins    = number of X coordinates in the XBINS array.
! integer nybins    = number of Y coordinates in the YBINS array.
!
! PHIST, XCOORDS, YCOORDS, XBINS, and YBINS are pointers which must be
! nullified before first use.  They will be deallocated and reallocated to
! contain the returned contents.  They will always be allocated to a dimension
! of at least one.  They should be conditionally deallocated when no longer
! needed.
!
! NILSTRING and WRAP are used only for ascii encoding (including foreign files).
!
! FIRSTLINE, TEMPLATE, and MAXCHARS(NMAXCHARS) are used only to read foreign
! files.  See documentation in the FIO primitive for more information.
!
! See documentation in the VELIO2 primitive for permitted values for the
! FIELDS(NFIELDS) array.
!
!-------------------------------------------------------------------------------
!                           SUBROUTINE DETAILS
!
! VELIO_OPEN_READ:
!  (1) Allocates the VELIO data structure.
!  (2) Opens the velocity file.
!  (3) Returns values found in the file header.
!  (4) Returns defaults for values not in the file.
!  (5) Leaves the file positioned to read the first velocity function.
!  (6) The ENCODING is not specified by the user; it is automatically
!        determined by this routine.
!
! VELIO_OPEN_WRITE:
!  (1) Allocates the VELIO data structure.
!  (2) Opens the velocity file,
!  (3) Writes the argument values into the file header.
!  (4) Writes defaults for values not in the argument list.
!  (5) Leaves the file positioned to write the first velocity function.
!  (6) The desired ENCODING should be specified by the user.
!
! VELIO_OPEN_FOREIGN:
!  (1) Allocates the VELIO data structure.
!  (2) Opens the velocity file,
!  (3) Uses the argument values to read the file.
!  (4) Uses defaults for values not in the argument list.
!  (5) Leaves the file positioned to read the first velocity function.
!  (6) The ENCODING is of course assumed to be ascii.
!
! VELIO_CLOSE:
!  (1) Closes the velocity file (unless already closed).
!  (2) Deallocates the VELIO data structure (unless already deallocated).
!
! VELIO_READ_VELFUN:
!  (1) Returns information for the next velocity function on the file.
!  (2) Returns defaults for information not in the file.
!  (3) Leaves the file positioned to read the next velocity function.
!  (4) Returns an error if the TPICKS or VPICKS array size is too small.
!  (5) The TPICKS and VPICKS arguments can be omitted if needing only NPICKS.
!
! VELIO_WRITE_VELFUN:
!  (1) Writes all the information for a single velocity function to the file.
!  (2) Writes defaults for information not in the argument list.
!  (3) Leaves the file positioned to write the next velocity function.
!
! VELIO_SCAN_ALLOC:
! VELIO_SCAN_FOREIGN:
!  (1) Opens, reads, and closes the velocity file.
!  (2) Returns requested information about the file.
!  (3) If the XCOORDS and YCOORDS define a rectangular grid of coordinates,
!       the XBINS and YBINS arrays will be filled out with NXBINS coordinates
!       in the X direction and NYBINS coordinates in the Y direction, such
!       that NXBINS * NYBINS = NFUN.  Otherwise, NXBINS and NYBINS will be
!       set to zero.
!  (4) XBINS,YBINS,NXBINS,NYBINS should be either all present or all absent.
!
!-------------------------------------------------------------------------------
!                        RETURNED ERROR FLAGS
!
! The returned error will have one of these integer named constant values:
!
!            err          description
!          --------       -----------
!          VELIO_OK       the operation was successful.
!          VELIO_ERROR    an open or read/write error occurred.
!          VELIO_EOF      an end-of-file was encountered.
!
! VELIO_EOF is returned only when an end-of-file or end-of-section is
! encountered while reading ascii records.
!
!-------------------------------------------------------------------------------
!                     VELOCITY FILE ENCODING FORMATS
!
! The encoding format of the velocity file to read or write must be one of
! these character(len=8) named constant values:
!
!     encoding       description
!     --------       -----------
!     VELIO_OLDCPS   old-style CPS velocity file.
!     VELIO_ASCII    CPS self-defining ascii velocity file.
!     VELIO_HYBRID   CPS self-defining hybrid (columnar) binary velocity file.
!     VELIO_BINARY   CPS self-defining blocked binary velocity file.
!     VELIO_MODSPEC  modspec file (for input only).
!     VELIO_TRCIO    TRCIO trace file (for input only).
!
! The default value of ENCODING for output is VELIO_ASCII.
!
! The DUNITS, ATTNAME, ATTUNITS, and TDUNITS arguments are not saved on
! old-style CPS velocity files.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!  1. The usual values for NHX and NHY are (7,8) or (17,18) or (37,38)
!     or the reverse of these.  Other header word values may be OK in some
!     circumstances.
!
!  2. The only valid pairs of values for NMOSIGN and NMOEXP are these:
!          NMOSIGN =  1.0  and  NMOEXP = 2.0  for normal moveout.
!          NMOSIGN = -1.0  and  NMOEXP = 4.0  for 4th-order residual moveout.
!
!  3. The PROJECT, LINE, RDATE, PDATE, USERID, and COMMENT arguments are left
!     over from the old CONSEIS processing system which was retired in 1989.
!     They are generally of little use anymore, but can be used for storing
!     miscellaneous information about individual velocity functions, such as
!     an alternative set of coordinates, or an elevation or water depth, or a
!     comment.
!
!  4. The VELNAME argument is likewise left over from 1989, but is still used
!     for a label for plotting on a seismic section.  VELNAME is often generated
!     automatically from the coordinates using the interactive VA (velocity
!     analysis) program.
!
!  5. The DUNITS argument currently has little use.
!
!  6. The ATTNAME, ATTUNITS, and TDUNITS arguments are used for the interactive
!     GEOPRESS (geopressure) program but currently have no other use.
!
!-------------------------------------------------------------------------------
!                       VELOCITY FUNCTION TYPES
!
! The type of velocity function should be one of these string values:
!
!       veltype     ordinate vpicks(npicks)     abscissa tpicks(npicks)
!       -------     -----------------------     -----------------------
!       'VTNM'         stacking velocity             2-way time
!       'VTRM'              RMS velocity             2-way time
!       'VTAV'          average velocity             2-way time
!       'VTIN'         interval velocity             2-way time
!       'VTDP'               depth                   2-way time
!       'VZRM'              RMS velocity                depth
!       'VZAV'          average velocity                depth
!       'VZIN'         interval velocity                depth
!       'VLRM'              RMS velocity           layer thickness
!       'VLAV'          average velocity           layer thickness
!       'VLIN'         interval velocity           layer thickness
!
!-------------------------------------------------------------------------------
!                    TIMING AND DISK SPACE COMPARISONS   
!
! Here are some comparisons among the different encoding styles for
! velocity files.  These comparisons are made for files containing 5000
! velocity functions, each of which contains 50 time/velocity picks.
!
!       encoding          disk space        write time       read time
!       ---------        -------------      ----------       ---------
!       VELIO_OLDCPS     3.1 megabytes        15 sec           5 sec
!       VELIO_ASCII      5.8 megabytes        34 sec          31 sec
!       VELIO_BINARY     2.4 megabytes         3 sec           2 sec
!
! These velocity files were read and written by poepsn03.
! The files went through NFS to a disk on poepin06.
! These experiments were performed 11-07-99.
!
! Here are the same tests using our X-windows velocity analysis program VA
! run on poepsn03 through NFS to a disk on poepin06:
!
!       encoding          disk space        write time       read time
!       ---------        -------------      ----------       ---------
!       VELIO_OLDCPS     3.1 megabytes        22 sec           8 sec (2)
!  old CPS generic (1)  14.5 megabytes        13 sec          16 sec (2)
!  old CPS binary        2.5 megabytes         3 sec           5 sec (2)
!
! Here are the same tests using our X-windows velocity analysis program VA
! run locally on poepin06 (using local disk):
!
!       encoding          disk space        write time       read time
!       ---------        -------------      ----------       ---------
!       VELIO_OLDCPS     3.1 megabytes        27 sec          10 sec (2)
!  old CPS generic (1)  14.5 megabytes        10 sec          14 sec (2)
!  old CPS binary        2.5 megabytes       0.3 sec           5 sec (2)
!
! The following points are relevant:
!  (1) The old CPS generic format is not really generic, since the columns
!        used are fixed, making faster reads and writes.  This format also
!        uses all columns for all picks, wasting disk space.
!  (2) The time taken by VA to allocate data structures and execute velocity
!        function type conversion code while reading the file is also included
!        in the above time estimates.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!014. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
! 13. 2005-11-29  Stoeckley  Add ability to read TRCIO trace files.
! 12. 2005-01-31  Stoeckley  Add ability to read modspec files.
! 11. 2004-03-15  Stoeckley  Make VPICKS and TPICKS optional when reading
!                             a velocity function; change scanning function
!                             to not use the VPICKS and TPICKS arguments.
! 10. 2003-09-23  Stoeckley  Change default encoding to self-defining ascii.
!  9. 2002-04-11  Stoeckley  Add MSG argument in call to FIOUTIL_VERIFY1;
!                             improve handling of some missing self defining
!                             file parameters.
!  8. 2002-02-04  Stoeckley  Modify (with much simplification) to use the
!                             new PJAR and FIO primitives.
!  7. 2000-11-27  Stoeckley  Extensively overhauled, with the motivation to
!                             simplify use and maintenance in an interactive
!                             environment where foreign files with missing
!                             information might be read, and to allow saving
!                             history records on velocity files; moved most of
!                             the code to new primitives VELIO1 and VELIO2.
!  6. 2000-04-09  Stoeckley  Fix bug introduced in previous revision regarding
!                             reading old-style velocity files which do not
!                             have the NMOSIGN and NMOEXP parameters.
!  5. 2000-03-10  Stoeckley  Improve handling of open errors when trying to
!                             open an old-style velocity file to read; change
!                             to call the new OLDSTYLE module for old-style
!                             velocity file I/O.
!  4. 2000-01-28  Stoeckley  Add reference to named constant FILENAME_LENGTH.
!  3. 1999-11-17  Stoeckley  Simplify code and improve efficiency; move common
!                             non-trace-file code to the new primitive FIO;
!                             add ability to read and write binary files;
!                             incorporate IO_CPSVF into this primitive as
!                             private subroutines; add ident string for RCS;
!                             add VELIO_SCAN_ALLOC.
!  2. 1999-10-20  Stoeckley  Add ability also to call CPSIO to read and write
!                             new-style ascii (but not yet binary) files.
!  1. 1999-09-21  Stoeckley  Initial version (calls IO_CPSVF only).
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
 
 
      module velio_module
      use velio1_module
      use velio2_module
      use velio3_module
      use velio4_module
      use pjar_module
      use dio_module
      use fio_module
      use string_module
      use gridcheck_module
      use named_constants_module
      implicit none
      public
      private :: velio_private_scan
      private :: velio_private_alloc

      character(len=100),public,save :: VELIO_IDENT = &
'$Id: velio.f90,v 1.14 2006/10/17 13:45:49 Glover prod sps $'

      interface velio_open_read
         module procedure velio_open_read_args
         module procedure velio_open_read_pjar
      end interface

      interface velio_open_write
         module procedure velio_open_write_args
         module procedure velio_open_write_pjar
      end interface

      interface velio_scan_alloc
         module procedure velio_scan_alloc_args
         module procedure velio_scan_alloc_pjar
      end interface

      integer,public,parameter  :: VELIO_OK          = FIO_OK  
      integer,public,parameter  :: VELIO_ERROR       = FIO_ERROR
      integer,public,parameter  :: VELIO_EOF         = FIO_EOF

      character(len=8),public,parameter  :: VELIO_ASCII   = FIO_ASCII
      character(len=8),public,parameter  :: VELIO_BINARY  = FIO_BINARY
      character(len=8),public,parameter  :: VELIO_HYBRID  = FIO_HYBRID
      character(len=8),public,parameter  :: VELIO_OLDCPS  = 'oldcps'
      character(len=8),public,parameter  :: VELIO_MODSPEC = 'modspec'
      character(len=8),public,parameter  :: VELIO_TRCIO   = 'trcio'

      character(len=20),private,parameter :: DEFAULT_FILETYPE = 'velocity'
      character(len=20),private,parameter :: DEFAULT_SECNAME  = 'velocity'

      type,public :: velio_struct
           private
           type(velio2_struct)    ,pointer :: velio2
           type(velio3_struct)    ,pointer :: velio3
           type(velio4_struct)    ,pointer :: velio4
           type(dio_struct)       ,pointer :: dio
           type(fio_struct)       ,pointer :: fio
           character(len=8)                :: encoding
      end type velio_struct

      contains


!!----------------------------- augment ----------------------------------!!
!!----------------------------- augment ----------------------------------!!
!!----------------------------- augment ----------------------------------!!
 
! called before writing a file to disk.
! pickle jar contents are modified.

 
      subroutine velio_augment (pjar,secname)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      integer                         :: nhx,nhy            ! local
      real                            :: default_xcoord     ! local
      real                            :: default_ycoord     ! local
      character(len=8)                :: default_veltype    ! local
      character(len=44)               :: attname            ! local
      character(len=44)               :: attunits           ! local
      character(len=44)               :: tdunits            ! local

      call fioutil_augment1 (pjar, secname, VELIO_ASCII)

      call pjar_augment (pjar, 'nfun'            ,       -1     )
      call pjar_augment (pjar, 'nhx'             ,        0     )
      call pjar_augment (pjar, 'nhy'             ,        0     )
      call pjar_augment (pjar, 'nmosign'         ,       1.0    )
      call pjar_augment (pjar, 'nmoexp'          ,       2.0    )
      call pjar_augment (pjar, 'dunits'          ,      'unset' )
      call pjar_augment (pjar, 'attname'         ,      'unset' )
      call pjar_augment (pjar, 'attunits'        ,      'unset' )
      call pjar_augment (pjar, 'tdunits'         ,      'unset' )
      call pjar_augment (pjar, 'default_xcoord'  ,       0.0    )
      call pjar_augment (pjar, 'default_ycoord'  ,       0.0    )
      call pjar_augment (pjar, 'default_veltype' ,      'VTNM'  )

      call pjar_get     (pjar, 'nhx'             , nhx            )
      call pjar_get     (pjar, 'nhy'             , nhy            )
      call pjar_get     (pjar, 'default_xcoord'  , default_xcoord )
      call pjar_get     (pjar, 'default_ycoord'  , default_ycoord )
      call pjar_get     (pjar, 'default_veltype' , default_veltype)
      call pjar_get     (pjar, 'attname'         , attname        )
      call pjar_get     (pjar, 'attunits'        , attunits       )
      call pjar_get     (pjar, 'tdunits'         , tdunits        )

                 !!!                         opt     opt     opt     opt
                 !!!                          |       |       |       | 
                 !!!   (pjar, field, defval, hdr, fieldtype, unit, delimiter)

 call fioutil_augment2 (pjar,'abscissa', 0.0,                    unit=tdunits )
 call fioutil_augment2 (pjar,'ordinate', 0.0, fieldtype=attname, unit=attunits)
 call fioutil_augment2 (pjar,'xcoord'  , default_xcoord, nhx, delimiter=.true.)
 call fioutil_augment2 (pjar,'ycoord'  , default_ycoord, nhy, delimiter=.true.)
 call fioutil_augment2 (pjar,'veltype' , default_veltype    , delimiter=.true.)
 call fioutil_augment2 (pjar,'velname' ,   'none'           , delimiter=.true.)
 call fioutil_augment2 (pjar,'project' ,   'none'           , delimiter=.true.)
 call fioutil_augment2 (pjar,'line'    ,   'none'           , delimiter=.true.)
 call fioutil_augment2 (pjar,'rdate'   ,   'none'           , delimiter=.true.)
 call fioutil_augment2 (pjar,'pdate'   ,   'none'           , delimiter=.true.)
 call fioutil_augment2 (pjar,'userid'  ,   'non'            , delimiter=.true.)
 call fioutil_augment2 (pjar,'comment' ,   'none'           , delimiter=.true.)

      call fioutil_augment3 (pjar)
      return
      end subroutine velio_augment
 

!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!
!!--------------------------------- verify ------------------------------!!

! called after reading file header sections from disk.
! called before using the the pickle jar to read a data section from disk.
! called before writing a file to disk.
! first: optional pickle jar contents are augmented.
! then: all pickle jar contents are verified.


      subroutine velio_verify (pjar,secname,err,msg)
      implicit none
      type(pjar_struct),intent(inout) :: pjar               ! arguments
      character(len=*) ,intent(in)    :: secname            ! arguments
      integer          ,intent(out)   :: err                ! arguments
      character(len=*) ,intent(out)   :: msg                ! arguments
      character(len=8)                :: encoding           ! local
      integer                         :: nfun,nhx,nhy       ! local
      integer                         :: npackets           ! local
      integer                         :: ncolumns,column    ! local
      real                            :: nmosign,nmoexp     ! local
      character(len=4)                :: default_veltype    ! local

!----------get started:

      call fioutil_verify1 (pjar,secname,msg)

      if(msg /= ' ') then
          err = VELIO_ERROR
          return
      end if

!----------augment optional parameters:

      call pjar_augment (pjar, 'nfun'            ,       -1     )
      call pjar_augment (pjar, 'npackets'        ,       -1     )
      call pjar_augment (pjar, 'nhx'             ,        0     )
      call pjar_augment (pjar, 'nhy'             ,        0     )
      call pjar_augment (pjar, 'nmosign'         ,       1.0    )
      call pjar_augment (pjar, 'nmoexp'          ,       2.0    )
      call pjar_augment (pjar, 'dunits'          ,      'unset' )
      call pjar_augment (pjar, 'attname'         ,      'unset' )
      call pjar_augment (pjar, 'attunits'        ,      'unset' )
      call pjar_augment (pjar, 'tdunits'         ,      'unset' )
      call pjar_augment (pjar, 'default_xcoord'  ,       0.0    )
      call pjar_augment (pjar, 'default_ycoord'  ,       0.0    )
      call pjar_augment (pjar, 'default_veltype' ,      'VTNM'  )

!----------get selected parameters to test:

      call pjar_get (pjar, 'encoding'        , encoding)
      call pjar_get (pjar, 'nfun'            , nfun        )
      call pjar_get (pjar, 'npackets'        , npackets    )
      call pjar_get (pjar, 'nhx'             , nhx         )
      call pjar_get (pjar, 'nhy'             , nhy         )
      call pjar_get (pjar, 'nmosign'         , nmosign     )
      call pjar_get (pjar, 'nmoexp'          , nmoexp      )
      call pjar_get (pjar, 'default_veltype' , default_veltype )
      call pjar_get (pjar, 'ncolumns'        , ncolumns )

      if (nfun < 0 .or. nfun == INIL) then
           nfun = npackets
           call pjar_put (pjar, 'nfun', nfun)
      end if

!----------test selected parameters:

                    !!!                        missing         invalid
                    !!!                           |               |
      call fioutil_verify2 ('nfun'       ,(nfun       ==INIL), (nfun < -1))
      call fioutil_verify2 ('nhx'        ,(nhx        ==INIL), (nhx <= 0))
      call fioutil_verify2 ('nhy'        ,(nhy        ==INIL), (nhy <= 0))
      call fioutil_verify2 ('nmosign'    ,(nmosign    ==FNIL), (nmosign == 0.0))
      call fioutil_verify2 ('nmoexp'     ,(nmoexp     ==FNIL), (nmoexp  == 0.0))

      if (encoding /= VELIO_OLDCPS .and. encoding /= VELIO_MODSPEC &
          .and. encoding /= VELIO_TRCIO) then
           column = pjar_find (pjar, "fields", "veltype")
           if (column == 0 .or. column > ncolumns) then
                call fioutil_verify2 ('default_veltype',                &
                                      (default_veltype==CNIL),          &
                                      (default_veltype /= 'VTNM' .and.  &
                                       default_veltype /= 'VTRM' .and.  &
                                       default_veltype /= 'VTAV' .and.  &
                                       default_veltype /= 'VTIN' .and.  &
                                       default_veltype /= 'VTDP' .and.  &
                                       default_veltype /= 'VZRM' .and.  &
                                       default_veltype /= 'VZAV' .and.  &
                                       default_veltype /= 'VZIN' .and.  &
                                       default_veltype /= 'VLRM' .and.  &
                                       default_veltype /= 'VLAV' .and.  &
                                       default_veltype /= 'VLIN'))
           end if
      end if

!----------finish up and return:

      call fioutil_verify3 (pjar,msg)

      if(msg /= ' ') then
          err = VELIO_ERROR
          return
      end if

      if (encoding /= VELIO_OLDCPS .and. encoding /= VELIO_MODSPEC &
          .and. encoding /= VELIO_TRCIO) then
           call fioutil_verify (pjar,secname,msg)
      end if

      if(msg == ' ') then
          err = VELIO_OK
          msg = 'XY headers = '//trim(string_ii2ss(nhx))//' '     &
                               //trim(string_ii2ss(nhy))//'     ' &
                               //trim(string_ii2ss(nfun))//' functions'
      else
          err = VELIO_ERROR
      end if
      return
      end subroutine velio_verify


!!------------------- velio open read (using arguments) ---------------------!!
!!------------------- velio open read (using arguments) ---------------------!!
!!------------------- velio open read (using arguments) ---------------------!!
 
 
      subroutine velio_open_read_args                                  &
                   (obj,filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,  &
                    phist,nhist,progname,                              &
                    dunits,attname,attunits,tdunits,                   &
                    encoding,fields,nfields,nilstring,wrap,            &
                    default_xcoord,default_ycoord,default_veltype)
      implicit none
      type(velio_struct),pointer            :: obj               ! arguments
      character(len=*),intent(in)           :: filename          ! arguments
      integer         ,intent(out)          :: nfun              ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      integer         ,intent(out),optional :: nhx,nhy           ! arguments
      real            ,intent(out),optional :: nmosign,nmoexp    ! arguments
      character(len=*),pointer    ,optional :: phist(:)          ! arguments
      integer         ,intent(out),optional :: nhist             ! arguments
      character(len=*),intent(in) ,optional :: progname          ! arguments
      character(len=*),intent(out),optional :: dunits            ! arguments
      character(len=*),intent(out),optional :: attname           ! arguments
      character(len=*),intent(out),optional :: attunits          ! arguments
      character(len=*),intent(out),optional :: tdunits           ! arguments
      character(len=*),intent(out),optional :: encoding          ! arguments
      character(len=*),intent(out),optional :: fields(:)         ! arguments
      integer         ,intent(out),optional :: nfields           ! arguments
      character(len=*),intent(out),optional :: nilstring         ! arguments
      integer         ,intent(out),optional :: wrap              ! arguments
      real            ,intent(out),optional :: default_xcoord    ! arguments
      real            ,intent(out),optional :: default_ycoord    ! arguments
      character(len=*),intent(out),optional :: default_veltype   ! arguments
      type(pjar_struct),pointer             :: pjar              ! local
      logical                               :: present_fields    ! local
 
      nullify (pjar) ! jpa
      call pjar_create         (pjar)
      call velio_open_read     (obj,filename,pjar,DEFAULT_SECNAME,err,msg)
      call pjar_choose_section (pjar,DEFAULT_SECNAME)

      call pjar_get (pjar, 'nfun', nfun)
      if (nfun == INIL) call pjar_get (pjar, 'npackets', nfun)
      if (nfun == INIL) nfun = -1

      present_fields = (present(fields))
                 ! needed to get around new absoft compiler bug.

      if (present(nhx      )) call pjar_get (pjar, 'nhx'      , nhx           )
      if (present(nhy      )) call pjar_get (pjar, 'nhy'      , nhy           )
      if (present(nmosign  )) call pjar_get (pjar, 'nmosign'  , nmosign       )
      if (present(nmoexp   )) call pjar_get (pjar, 'nmoexp'   , nmoexp        )
      if (present(dunits   )) call pjar_get (pjar, 'dunits'   , dunits        )
      if (present(attname  )) call pjar_get (pjar, 'attname'  , attname       )
      if (present(attunits )) call pjar_get (pjar, 'attunits' , attunits      )
      if (present(tdunits  )) call pjar_get (pjar, 'tdunits'  , tdunits       )
      if (present(encoding )) call pjar_get (pjar, 'encoding' , encoding      )
      if (present_fields    ) call pjar_get (pjar, 'fields'   , fields,nfields)
  !!! if (present(fields   )) call pjar_get (pjar, 'fields'   , fields,nfields)
      if (present(nilstring)) call pjar_get (pjar, 'nilstring', nilstring     )
      if (present(wrap     )) call pjar_get (pjar, 'wrap'     , wrap          )

      if (present(default_xcoord)) &
                         call pjar_get (pjar,'default_xcoord' ,default_xcoord)
      if (present(default_ycoord)) &
                         call pjar_get (pjar,'default_ycoord' ,default_ycoord)
      if (present(default_veltype)) &
                         call pjar_get (pjar,'default_veltype',default_veltype)

      call pjar_choose_section (pjar,'history')
      call pjar_alloc_cards    (pjar,phist,nhist)
      call pjar_delete         (pjar)
      return
      end subroutine velio_open_read_args
 

!!------------------- velio open write (using arguments) ------------------!!
!!------------------- velio open write (using arguments) ------------------!!
!!------------------- velio open write (using arguments) ------------------!!
 
 
      subroutine velio_open_write_args                                 &
                   (obj,filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,  &
                    hist,nhist,progname,                               &
                    dunits,attname,attunits,tdunits,                   &
                    encoding,fields,nfields,nilstring,wrap,            &
                    fillout,widths,nwidths,                            &
                    default_xcoord,default_ycoord,default_veltype)
      implicit none
      type(velio_struct),pointer           :: obj               ! arguments
      character(len=*),intent(in)          :: filename          ! arguments
      integer         ,intent(in)          :: nfun              ! arguments
      integer         ,intent(out)         :: err               ! arguments
      character(len=*),intent(out)         :: msg               ! arguments
      integer         ,intent(in)          :: nhx,nhy           ! arguments
      real            ,intent(in),optional :: nmosign,nmoexp    ! arguments
      character(len=*),intent(in),optional :: hist(:)           ! arguments
      integer         ,intent(in),optional :: nhist             ! arguments
      character(len=*),intent(in),optional :: progname          ! arguments
      character(len=*),intent(in),optional :: dunits            ! arguments
      character(len=*),intent(in),optional :: attname           ! arguments
      character(len=*),intent(in),optional :: attunits          ! arguments
      character(len=*),intent(in),optional :: tdunits           ! arguments
      character(len=*),intent(in),optional :: encoding          ! arguments
      character(len=*),intent(in),optional :: fields(:)         ! arguments
      integer         ,intent(in),optional :: nfields           ! arguments
      character(len=*),intent(in),optional :: nilstring         ! arguments
      integer         ,intent(in),optional :: wrap              ! arguments
      logical         ,intent(in),optional :: fillout           ! arguments
      integer         ,intent(in),optional :: widths(:)         ! arguments
      integer         ,intent(in),optional :: nwidths           ! arguments
      real            ,intent(in),optional :: default_xcoord    ! arguments
      real            ,intent(in),optional :: default_ycoord    ! arguments
      character(len=*),intent(in),optional :: default_veltype   ! arguments
      type(pjar_struct),pointer            :: pjar              ! local
 
      nullify (pjar) ! jpa
      call pjar_create (pjar)
      call pjar_choose_section (pjar,DEFAULT_SECNAME)

      call pjar_put (pjar, 'nfun'    , nfun        )
      call pjar_put (pjar, 'nhx'     , nhx         )
      call pjar_put (pjar, 'nhy'     , nhy         )

      if (present(nmosign    )) call pjar_put (pjar, 'nmosign'    , nmosign   )
      if (present(nmoexp     )) call pjar_put (pjar, 'nmoexp'     , nmoexp    )
      if (present(dunits     )) call pjar_put (pjar, 'dunits'     , dunits    )
      if (present(attname    )) call pjar_put (pjar, 'attname'    , attname   )
      if (present(attunits   )) call pjar_put (pjar, 'attunits'   , attunits  )
      if (present(tdunits    )) call pjar_put (pjar, 'tdunits'    , tdunits   )
      if (present(encoding   )) call pjar_put (pjar, 'encoding'   , encoding  )
      if (present(fields     )) call pjar_put (pjar, 'fields'  ,fields,nfields)
      if (present(nilstring  )) call pjar_put (pjar, 'nilstring'  , nilstring )
      if (present(wrap       )) call pjar_put (pjar, 'wrap'       , wrap      )
      if (present(fillout    )) call pjar_put (pjar, 'fillout'    ,fillout    )
      if (present(widths     )) call pjar_put (pjar, 'widths'  ,widths,nwidths)

      if (present(default_xcoord)) &
                         call pjar_put (pjar,'default_xcoord' ,default_xcoord)
      if (present(default_ycoord)) &
                         call pjar_put (pjar,'default_ycoord' ,default_ycoord)
      if (present(default_veltype)) &
                         call pjar_put (pjar,'default_veltype',default_veltype)

      call pjar_choose_section (pjar,'history')
      call pjar_put_cards      (pjar,hist,nhist,progname)

      call velio_open_write    (obj,filename,pjar,DEFAULT_SECNAME,err,msg)
      call pjar_delete         (pjar)
      return
      end subroutine velio_open_write_args
 

!!-------------------- velio open read (using pickle jar) ------------------!!
!!-------------------- velio open read (using pickle jar) ------------------!!
!!-------------------- velio open read (using pickle jar) ------------------!!
 
 
      subroutine velio_open_read_pjar (obj,filename,pjar,secname,err,msg)
      implicit none
      type(velio_struct),pointer              :: obj             ! arguments
      character(len=*)  ,intent(in)           :: filename        ! arguments
      type(pjar_struct) ,intent(inout)        :: pjar            ! arguments
      character(len=*)  ,intent(in)           :: secname         ! arguments
      integer           ,intent(out)          :: err             ! arguments
      character(len=*)  ,intent(out)          :: msg             ! arguments
 
!!!!!!!!!! initialize data structure:

      allocate (obj)
      nullify (obj%velio2)
      nullify (obj%velio3)
      nullify (obj%velio4)
      nullify (obj%fio)
      nullify (obj%dio)
      obj%encoding = 'unset'
      call pjar_clear (pjar)

!!!!!!!!!! try reading modspec file:

      if (velio3_is_modspec(filename)) then
           call velio3_open_read (obj%velio3,filename,pjar,secname,err,msg)
           if (err == VELIO_OK) then
                obj%encoding = VELIO_MODSPEC
                call pjar_choose_section (pjar, secname)
                call pjar_put            (pjar, 'encoding' ,obj%encoding)
                call velio_verify        (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! try reading trcio file:

      if (velio4_is_trcio(filename)) then
           call velio4_open_read (obj%velio4,filename,pjar,secname,err,msg)
           if (err == VELIO_OK) then
                obj%encoding = VELIO_TRCIO
                call pjar_choose_section (pjar, secname)
                call pjar_put            (pjar, 'encoding' ,obj%encoding)
                call velio_verify        (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! open input file:

      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= VELIO_OK) return

!!!!!!!!!! try reading oldcps file:

      call velio1_read_header  (obj%dio,pjar,secname,err,msg)
      if (err == VELIO_OK) then
           obj%encoding = VELIO_OLDCPS
           call pjar_choose_section (pjar, secname)
           call pjar_put            (pjar, 'encoding' ,obj%encoding)
           call velio_verify        (pjar, secname, err, msg)
           return
      end if

!!!!!!!!!! read newcps file:

      call fio_create               (obj%fio,obj%dio)
      call fio_read_header_sections (obj%fio,pjar,err,msg)

      call velio_verify             (pjar, secname, err, msg)
      if (err /= VELIO_OK) return

      call fio_read_data_section    (obj%fio,pjar,secname,err,msg)
      if (err /= VELIO_OK) return

      call pjar_choose_section      (pjar, secname)
      call pjar_get                 (pjar,'encoding' ,obj%encoding)
      call velio2_create            (obj%velio2,obj%fio,pjar,secname)
      return
      end subroutine velio_open_read_pjar
 

!!------------------- velio open foreign (using pickle jar) -----------------!!
!!------------------- velio open foreign (using pickle jar) -----------------!!
!!------------------- velio open foreign (using pickle jar) -----------------!!
 
 
      subroutine velio_open_foreign (obj,filename,pjar,secname,err,msg)
      implicit none
      type(velio_struct),pointer              :: obj             ! arguments
      character(len=*)  ,intent(in)           :: filename        ! arguments
      type(pjar_struct) ,intent(inout)        :: pjar            ! arguments
      character(len=*)  ,intent(in)           :: secname         ! arguments
      integer           ,intent(out)          :: err             ! arguments
      character(len=*)  ,intent(out)          :: msg             ! arguments
 
!!!!!!!!!! initialize data structure:

      allocate (obj)
      nullify (obj%velio2)
      nullify (obj%velio3)
      nullify (obj%velio4)
      nullify (obj%fio)
      nullify (obj%dio)
      obj%encoding = 'unset'

      call velio_augment (pjar, secname)
      call velio_verify  (pjar, secname, err, msg)
      if (err /= VELIO_OK) return

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)

!!!!!!!!!! read modspec file:

      if (obj%encoding == VELIO_MODSPEC) then
           call velio3_open_read (obj%velio3,filename,pjar,secname,err,msg)
           if (err == VELIO_OK) then
                call velio_verify (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! read trcio file:

      if (obj%encoding == VELIO_TRCIO) then
           call velio4_open_read (obj%velio4,filename,pjar,secname,err,msg)
           if (err == VELIO_OK) then
                call velio_verify (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! open file:

      call dio_open_read (obj%dio,filename,err,msg)
      if (err /= VELIO_OK) return

!!!!!!!!!! read oldcps file:

      if (obj%encoding == VELIO_OLDCPS) then
           call velio1_read_header (obj%dio,pjar,secname,err,msg)
           if (err == VELIO_OK) then
                call velio_verify  (pjar, secname, err, msg)
           end if
           return
      end if

!!!!!!!!!! read newcps file:

      call fio_create               (obj%fio,obj%dio)
      call fio_read_data_section    (obj%fio,pjar,secname,err,msg)
      if (err /= VELIO_OK) return

      call velio2_create            (obj%velio2,obj%fio,pjar,secname)
      return
      end subroutine velio_open_foreign
 

!!-------------------- velio open write (using pickle jar) ------------------!!
!!-------------------- velio open write (using pickle jar) ------------------!!
!!-------------------- velio open write (using pickle jar) ------------------!!


      subroutine velio_open_write_pjar (obj,filename,pjar,secname,err,msg)
      implicit none
      type(velio_struct),pointer              :: obj             ! arguments
      character(len=*)  ,intent(in)           :: filename        ! arguments
      type(pjar_struct) ,intent(inout)        :: pjar            ! arguments
      character(len=*)  ,intent(in)           :: secname         ! arguments
      integer           ,intent(out)          :: err             ! arguments
      character(len=*)  ,intent(out)          :: msg             ! arguments
 
!!!!!!!!!! initialize data structure:

      allocate (obj)
      nullify (obj%velio2)
      nullify (obj%velio3)
      nullify (obj%velio4)
      nullify (obj%fio)
      nullify (obj%dio)
      obj%encoding = 'unset'

      call velio_augment (pjar, secname)
      call velio_verify  (pjar, secname, err, msg)
      if (err /= VELIO_OK) return

      call pjar_choose_section (pjar, secname)
      call pjar_get            (pjar, 'encoding', obj%encoding)

!!!!!!!!!! open output file:

      call dio_open_write (obj%dio,filename,err,msg)
      if (err /= VELIO_OK) return

!!!!!!!!!! write oldcps file:

      if (obj%encoding == VELIO_OLDCPS) then
           call velio1_write_header (obj%dio,pjar,secname,err,msg)
           return
      end if

!!!!!!!!!! write newcps file:

      call fio_create                (obj%fio,obj%dio)
      call fio_write_header_sections (obj%fio,pjar,err,msg,DEFAULT_FILETYPE)
      if (err /= VELIO_OK) return

      call fio_write_data_section    (obj%fio,pjar,secname,err,msg)
      if (err /= VELIO_OK) return

      call velio2_create             (obj%velio2,obj%fio,pjar,secname)
      return
      end subroutine velio_open_write_pjar


!!--------------------------- velio close --------------------------------!!
!!--------------------------- velio close --------------------------------!!
!!--------------------------- velio close --------------------------------!!


      subroutine velio_close (obj)
      implicit none
      type(velio_struct),pointer :: obj             ! arguments

      if (associated(obj)) then
           call velio2_delete  (obj%velio2)
           call velio3_close   (obj%velio3)
           call velio4_close   (obj%velio4)
           call fio_delete     (obj%fio)
           call dio_close      (obj%dio)
           deallocate(obj)
      end if
      return
      end subroutine velio_close


!!--------------------------- velio read velfun -------------------------!!
!!--------------------------- velio read velfun -------------------------!!
!!--------------------------- velio read velfun -------------------------!!


      subroutine velio_read_velfun                                    &
                    (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                     velname,veltype,                                 &
                     project,line,rdate,pdate,userid,comment)
      implicit none
      type(velio_struct),pointer            :: obj                 ! arguments
      real            ,intent(out)          :: xcoord,ycoord       ! arguments
      integer         ,intent(out)          :: npicks              ! arguments
      real            ,intent(out),optional :: tpicks(:),vpicks(:) ! arguments
      integer         ,intent(out)          :: err                 ! arguments
      character(len=*),intent(out)          :: msg                 ! arguments
      character(len=*),intent(out),optional :: velname,veltype     ! arguments
      character(len=*),intent(out),optional :: project,line        ! arguments
      character(len=*),intent(out),optional :: rdate,pdate         ! arguments
      character(len=*),intent(out),optional :: userid              ! arguments
      character(len=*),intent(out),optional :: comment             ! arguments

      if (obj%encoding == VELIO_OLDCPS) then
           call velio1_read_velfun                                         &
                  (obj%dio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,     &
                   velname,veltype,                                        &
                   project,line,rdate,pdate,userid,comment)
      else if (obj%encoding == VELIO_MODSPEC) then
           call velio3_read_velfun                                         &
                  (obj%velio3,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                   velname,veltype,                                        &
                   project,line,rdate,pdate,userid,comment)
      else if (obj%encoding == VELIO_TRCIO) then
           call velio4_read_velfun                                         &
                  (obj%velio4,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                   velname,veltype,                                        &
                   project,line,rdate,pdate,userid,comment)
      else
           call velio2_read_velfun                                         &
                  (obj%velio2,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                   velname,veltype,                                        &
                   project,line,rdate,pdate,userid,comment)

      end if
      return
      end subroutine velio_read_velfun


!!--------------------------- velio write velfun -------------------------!!
!!--------------------------- velio write velfun -------------------------!!
!!--------------------------- velio write velfun -------------------------!!


      subroutine velio_write_velfun                                   &
                    (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                     velname,veltype,                                 &
                     project,line,rdate,pdate,userid,comment)
      implicit none
      type(velio_struct),pointer            :: obj                 ! arguments
      real            ,intent(in)           :: xcoord,ycoord       ! arguments
      integer         ,intent(in)           :: npicks              ! arguments
      real            ,intent(in)           :: tpicks(:),vpicks(:) ! arguments
      integer         ,intent(out)          :: err                 ! arguments
      character(len=*),intent(out)          :: msg                 ! arguments
      character(len=*),intent(in) ,optional :: velname,veltype     ! arguments
      character(len=*),intent(in) ,optional :: project,line        ! arguments
      character(len=*),intent(in) ,optional :: rdate,pdate         ! arguments
      character(len=*),intent(in) ,optional :: userid              ! arguments
      character(len=*),intent(in) ,optional :: comment             ! arguments

      if (obj%encoding == VELIO_OLDCPS) then
           call velio1_write_velfun                                        &
                  (obj%dio,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,     &
                   velname,veltype,                                        &
                   project,line,rdate,pdate,userid,comment)
      else
           call velio2_write_velfun                                        &
                  (obj%velio2,xcoord,ycoord,npicks,tpicks,vpicks,err,msg,  &
                   velname,veltype,                                        &
                   project,line,rdate,pdate,userid,comment)

      end if
      return
      end subroutine velio_write_velfun


!!-------------------------- velio private alloc ---------------------------!!
!!-------------------------- velio private alloc ---------------------------!!
!!-------------------------- velio private alloc ---------------------------!!


      subroutine velio_private_alloc (nalloc,array,narray)
      implicit none
      integer         ,intent(in)           :: nalloc          ! arguments
      real            ,pointer    ,optional :: array(:)        ! arguments
      integer         ,intent(out),optional :: narray          ! arguments

      if (present(array)) then
           if (associated(array)) deallocate(array)
           allocate (array(max(nalloc,1)))
           array(:) = 0.0
      end if

      if (present(narray)) narray = nalloc
      return
      end subroutine velio_private_alloc


!!--------------------- velio scan alloc (using arguments) ---------------!!
!!--------------------- velio scan alloc (using arguments) ---------------!!
!!--------------------- velio scan alloc (using arguments) ---------------!!


      subroutine velio_scan_alloc_args                                   &
                   (filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,        &
                    maxpicks,xcoords,ycoords,xbins,ybins,nxbins,nybins,  &
                    phist,nhist,progname,                                &
                    dunits,attname,attunits,tdunits,                     &
                    encoding,fields,nfields,nilstring,wrap,              &
                    default_xcoord,default_ycoord,default_veltype)
      implicit none
      character(len=*),intent(in)           :: filename          ! arguments
      integer         ,intent(out)          :: nfun              ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      integer         ,intent(out),optional :: nhx,nhy           ! arguments
      real            ,intent(out),optional :: nmosign,nmoexp    ! arguments
      integer         ,intent(out),optional :: maxpicks          ! arguments
      real            ,pointer    ,optional :: xcoords(:)        ! arguments
      real            ,pointer    ,optional :: ycoords(:)        ! arguments
      real            ,pointer    ,optional :: xbins(:)          ! arguments
      real            ,pointer    ,optional :: ybins(:)          ! arguments
      integer         ,intent(out),optional :: nxbins,nybins     ! arguments
      character(len=*),pointer    ,optional :: phist(:)          ! arguments
      integer         ,intent(out),optional :: nhist             ! arguments
      character(len=*),intent(in) ,optional :: progname          ! arguments
      character(len=*),intent(out),optional :: dunits            ! arguments
      character(len=*),intent(out),optional :: attname           ! arguments
      character(len=*),intent(out),optional :: attunits          ! arguments
      character(len=*),intent(out),optional :: tdunits           ! arguments
      character(len=*),intent(out),optional :: encoding          ! arguments
      character(len=*),intent(out),optional :: fields(:)         ! arguments
      integer         ,intent(out),optional :: nfields           ! arguments
      character(len=*),intent(out),optional :: nilstring         ! arguments
      integer         ,intent(out),optional :: wrap              ! arguments
      real            ,intent(out),optional :: default_xcoord    ! arguments
      real            ,intent(out),optional :: default_ycoord    ! arguments
      character(len=*),intent(out),optional :: default_veltype   ! arguments
      type(velio_struct),pointer            :: obj               ! local
 
!!!!!!!!!! get started:

      nullify (obj) ! jpa
      call velio_private_alloc (0,xcoords,nfun  )
      call velio_private_alloc (0,ycoords,nfun  )
      call velio_private_alloc (0,xbins  ,nxbins)
      call velio_private_alloc (0,ybins  ,nybins)

!!!!!!!!!! open the file:

      call velio_open_read                                             &
                   (obj,filename,nfun,err,msg,nhx,nhy,nmosign,nmoexp,  &
                    phist,nhist,progname,                              &
                    dunits,attname,attunits,tdunits,                   &
                    encoding,fields,nfields,nilstring,wrap,            &
                    default_xcoord,default_ycoord,default_veltype)

      if (err /= VELIO_OK) return

!!!!!!!!!! scan the file to get optional arguments:

      call velio_private_scan (obj,nfun,err,msg,                        &
                    maxpicks,xcoords,ycoords,xbins,ybins,nxbins,nybins)

      call velio_close (obj)
      return
      end subroutine velio_scan_alloc_args


!!--------------------- velio scan alloc (using pickle jar) ---------------!!
!!--------------------- velio scan alloc (using pickle jar) ---------------!!
!!--------------------- velio scan alloc (using pickle jar) ---------------!!


      subroutine velio_scan_alloc_pjar                                   &
                            (filename,pjar,secname,err,msg,              &
                             xcoords,ycoords,xbins,ybins,nxbins,nybins)
      implicit none
      character(len=*)  ,intent(in)           :: filename          ! arguments
      type(pjar_struct) ,intent(inout)        :: pjar              ! arguments
      character(len=*)  ,intent(in)           :: secname           ! arguments
      integer           ,intent(out)          :: err               ! arguments
      character(len=*)  ,intent(out)          :: msg               ! arguments
      real              ,pointer    ,optional :: xcoords(:)        ! arguments
      real              ,pointer    ,optional :: ycoords(:)        ! arguments
      real              ,pointer    ,optional :: xbins(:)          ! arguments
      real              ,pointer    ,optional :: ybins(:)          ! arguments
      integer           ,intent(out),optional :: nxbins,nybins     ! arguments
      type(velio_struct),pointer              :: obj               ! local
      integer                                 :: nfun,maxpicks     ! local
 
!!!!!!!!!! get started:

      nullify (obj) ! jpa
      call pjar_put (pjar, 'maxpicks', 0)

      call velio_private_alloc (0,xcoords,nfun  )
      call velio_private_alloc (0,ycoords,nfun  )
      call velio_private_alloc (0,xbins  ,nxbins)
      call velio_private_alloc (0,ybins  ,nybins)

!!!!!!!!!! open the file:

      call velio_open_read (obj,filename,pjar,secname,err,msg)
      if (err /= VELIO_OK) return

!!!!!!!!!! scan the file to get optional arguments:

      call pjar_get (pjar, 'nfun', nfun)

      call velio_private_scan (obj,nfun,err,msg,                        &
                    maxpicks,xcoords,ycoords,xbins,ybins,nxbins,nybins)

      call pjar_put (pjar, 'maxpicks', maxpicks)

      call velio_close (obj)
      return
      end subroutine velio_scan_alloc_pjar


!!-------------------------- velio scan foreign ----------------------------!!
!!-------------------------- velio scan foreign ----------------------------!!
!!-------------------------- velio scan foreign ----------------------------!!


      subroutine velio_scan_foreign                              &
                           (filename,pjar,secname,err,msg,       &
                            xcoords,ycoords,xbins,ybins,nxbins,nybins)
      implicit none
      character(len=*)  ,intent(in)           :: filename          ! arguments
      type(pjar_struct) ,intent(inout)        :: pjar              ! arguments
      character(len=*)  ,intent(in)           :: secname           ! arguments
      integer           ,intent(out)          :: err               ! arguments
      character(len=*)  ,intent(out)          :: msg               ! arguments
      real              ,pointer    ,optional :: xcoords(:)        ! arguments
      real              ,pointer    ,optional :: ycoords(:)        ! arguments
      real              ,pointer    ,optional :: xbins(:)          ! arguments
      real              ,pointer    ,optional :: ybins(:)          ! arguments
      integer           ,intent(out),optional :: nxbins,nybins     ! arguments
      type(velio_struct),pointer              :: obj               ! local
      integer                                 :: nfun,maxpicks     ! local
      real                                    :: xcoord,ycoord     ! local
      integer                                 :: npicks            ! local
      real                                    :: tpicks(1000)      ! local
      real                                    :: vpicks(1000)      ! local
 
!!!!!!!!!! get started:

      nullify (obj) ! jpa
      call pjar_choose_section (pjar, secname)
      call pjar_put            (pjar, 'nfun'    , 0)
      call pjar_put            (pjar, 'maxpicks', 0)

      call velio_private_alloc (0,xcoords,nfun  )
      call velio_private_alloc (0,ycoords,nfun  )
      call velio_private_alloc (0,xbins  ,nxbins)
      call velio_private_alloc (0,ybins  ,nybins)

!!!!!!!!!! open the file:

      call velio_open_foreign (obj,filename,pjar,secname,err,msg)
      if (err /= VELIO_OK) return

!!!!!!!!!! get number of velocity functions:

      nfun = 0
      do
           call velio_read_velfun   &
                  (obj,xcoord,ycoord,npicks,tpicks,vpicks,err,msg)
           if (err == VELIO_ERROR) then
                call velio_close (obj)
                return
           end if
           if (err /= VELIO_OK) exit
           nfun = nfun + 1
      end do
      call velio_close (obj)
      call velio_open_foreign (obj,filename,pjar,secname,err,msg)
      if (err /= VELIO_OK) return

!!!!!!!!!! scan the file to get optional arguments:

      call velio_private_scan (obj,nfun,err,msg,                        &
                    maxpicks,xcoords,ycoords,xbins,ybins,nxbins,nybins)

      call pjar_choose_section (pjar, secname)
      call pjar_put            (pjar, 'nfun'    , nfun)
      call pjar_put            (pjar, 'maxpicks', maxpicks)

      call velio_close (obj)
      return
      end subroutine velio_scan_foreign


!!-------------------------- velio private scan --------------------------!!
!!-------------------------- velio private scan --------------------------!!
!!-------------------------- velio private scan --------------------------!!

! xcoords,ycoords,xbins,ybins are already allocated to length one.


      subroutine velio_private_scan                                      &
                   (obj,nfun,err,msg,                                    &
                    maxpicks,xcoords,ycoords,xbins,ybins,nxbins,nybins)
      implicit none
      type(velio_struct),pointer            :: obj               ! arguments
      integer         ,intent(in)           :: nfun              ! arguments
      integer         ,intent(out)          :: err               ! arguments
      character(len=*),intent(out)          :: msg               ! arguments
      integer         ,intent(out),optional :: maxpicks          ! arguments
      real            ,pointer    ,optional :: xcoords(:)        ! arguments
      real            ,pointer    ,optional :: ycoords(:)        ! arguments
      real            ,pointer    ,optional :: xbins(:)          ! arguments
      real            ,pointer    ,optional :: ybins(:)          ! arguments
      integer         ,intent(out),optional :: nxbins,nybins     ! arguments
      integer                               :: indx,npicks       ! local
      real                                  :: xcoord,ycoord     ! local
      real            ,pointer              :: xcoords2(:)       ! local
      real            ,pointer              :: ycoords2(:)       ! local
 
!!!!!!!!!! return if no optional arguments are present:

      if (.not.present(maxpicks) .and.                              &
          .not.present(xcoords)  .and. .not.present(ycoords) .and.  &
          .not.present(xbins  )  .and. .not.present(ybins  ) .and.  &
          .not.present(nxbins )  .and. .not.present(nybins )) then
           err = VELIO_OK
           msg = 'velocity file successfully scanned'
           return
      end if

!!!!!!!!!! initialize optional arguments:

      if (present(maxpicks)) maxpicks = 0
      if (present(nxbins  )) nxbins   = 0
      if (present(nybins  )) nybins   = 0

      if (present(xcoords)) then
           call velio_private_alloc (nfun,xcoords)
           xcoords2 => xcoords
      else
           allocate (xcoords2(nfun))
      end if

      if (present(ycoords)) then
           call velio_private_alloc (nfun,ycoords)
           ycoords2 => ycoords
      else
           allocate (ycoords2(nfun))
      end if

!!!!!!!!!! read velocity functions:

      do indx = 1,nfun
           call velio_read_velfun (obj,xcoord,ycoord,npicks,err=err,msg=msg)
           if (err /= VELIO_OK) then
                if (.not.present(xcoords)) deallocate (xcoords2)
                if (.not.present(ycoords)) deallocate (ycoords2)
                return
           end if
           if (present(maxpicks)) maxpicks = max(maxpicks,npicks)
           xcoords2(indx) = xcoord
           ycoords2(indx) = ycoord
      end do

!!!!!!!!!! return if grid information is not requested:

      if (nfun == 0 .or. .not.present(xbins ) .or. .not.present(ybins )  &
                    .or. .not.present(nxbins) .or. .not.present(nybins)) then
           if (.not.present(xcoords)) deallocate (xcoords2)
           if (.not.present(ycoords)) deallocate (ycoords2)
           err = VELIO_OK
           msg = 'velocity file successfully scanned'
           return
      end if

!!!!!!!!!! get grid information:

      call gridcheck (nfun,xcoords2,ycoords2,nxbins,nybins,msg)
      if (msg(1:1) /= ' ') then
           nxbins = 0
           nybins = 0
           err = VELIO_OK
           msg = 'velocity file successfully scanned - no grid'
           return
      end if
      call velio_private_alloc (nxbins,xbins)
      call velio_private_alloc (nybins,ybins)
      xbins(:) = xcoords2(1:nxbins)
      ybins(:) = ycoords2(1:nfun:nxbins)
      if (.not.present(xcoords)) deallocate (xcoords2)
      if (.not.present(ycoords)) deallocate (ycoords2)
      err = VELIO_OK
      msg = 'velocity file successfully scanned'
      return
      end subroutine velio_private_scan


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
 
 
      end module velio_module
 
 
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
 
