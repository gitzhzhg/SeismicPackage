
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- fio.f90 --------------------------------!!
!!------------------------------- fio.f90 --------------------------------!!
!!------------------------------- fio.f90 --------------------------------!!


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
! Name       : FIO    (self-defining non-trace file I/O)
! Category   : io
! Written    : 1999-11-17   by: Tom Stoeckley
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : To help read and write self-defining non-trace files.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

 
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION               
!
! This primitive is designed to be used for reading and writing self-defining
! sequential non-trace files such as velocity files, static files, mute files,
! field geometry files, pick files, horizon files, and well log files.  This
! primitive encapsulates code which would otherwise be redundantly included
! in the code for the various types of non-trace files, and also provides a
! simplified and efficient interface to CIO (through DIO) for those features
! which are needed.
!
! This primitive can be used in two distinct ways:
!
!  (1) This primitive can be used by other primitives which read specific
!      types of files (such as velocity files and static files).
!
!  (2) This primitive can be used directly in a generic manner to read or
!      write any CPS self-defining non-trace ascii file by a program which
!      might not care what the file type is.  See examples below.
!
!-------------------------------------------------------------------------------
!                       FORMATS OF SELF DEFINING FILES       
!
! This primitive can be used to read and write files with the following
! formats:
!
!  (1) CPS self-defining ascii columnar files.
!  (2) CPS self-defining binary files.
!  (3) CPS self-defining hybrid files (binary files arranged like ascii files).
!  (3) CPS self-defining files with mixed ascii, binary, and hybrid data
!       sections.
!  (4) Ascii columnar files with missing information (maybe foreign origin).
!  (5) CPS self-defining ascii, binary or hybrid files with some missing
!       information.
!
! When trying to read a file, all other formats (not supported by FIO) should
! be tried before trying to read the file with FIO.  If the file is a foreign
! file with missing information, or even if it is not the right kind of a file
! at all but any kind of ascii file, an attempt to read this file with FIO
! may suggest that it might be a foreign file.
!
!-------------------------------------------------------------------------------
!                   CHARACTERISTICS OF SELF DEFINING FILES       
!
! The self-defining files have the following characteristics:
!
!  (1) Each file begins with a tag identifying the file type.
!  (2) Each file contains one or more header sections followed by one or
!       more data sections.
!  (3) Each data section is associated with a header section.
!  (4) A header section does not have to be associated with a data section.
!  (5) One or more of the header sections may contain history cards.
!  (6) Each section is identified by delimiting tags.
!  (7) Everything except the contents of the data sections are on card images
!       beginning with the pound sign (#) so that these lines will look like
!       comments when the files are read as foreign files by FIO or by
!       anything else.
!  (8) The first part of each file (down to the beginning of data) is always
!       ascii so that it can be viewed in a text editor, even if some of the
!       data sections are binary.
!  (9) Any file can contain a mixture of ascii, binary, and hybrid data
!       sections.
! (10) Each column in an ascii file (and each variable in a binary or hybrid
!       file) is accessed efficiently by a column index, and for binary files
!       by a byte address.
! (11) A header section can contain keywords and parameters, or unstructured
!       data cards.
! (12) A header section must contain keywords and parameters, including
!       certain specific parameters, if it has an associated data section.
!       These parameters completely define the information necessary to
!       read the associated ascii or binary or hybrid data section.
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
!                           PRIMARY ROUTINES
!
! Create and delete the FIO object:
!
!                             i   b 
!           call fio_create (obj,dio)
!           call fio_delete (obj)
!                             b
!
! Read or write all header sections:
!                                                                opt
!                                              o   b    o   o     o       
!           call fio_read_header_sections    (obj,pjar,err,msg,filetype)
!           call fio_write_header_sections   (obj,pjar,err,msg,filetype)
!                                              o   b    o   o     i       
!                                                                opt
!
! Choose a data section to read or write:
!
!                                             b   b      o     o   o
!           call fio_read_next_data_section (obj,pjar,secname,err,msg)
!
!           call fio_read_data_section      (obj,pjar,secname,err,msg)
!           call fio_write_data_section     (obj,pjar,secname,err,msg)
!                                             b   b      i     o   o
!
! Register an error not known to this primitive at any time:
!
!           call fio_register_error (obj,msg)
!                                     b   i
!
! Find out the current error status at any time:
!
!           call fio_status (obj,err,msg,goodmsg)
!                             i   o   o     i
!                                          opt
!
!                      ++++++++++++++++++++++++++++
!
! type(fio_struct)    obj = pointer to the FIO data structure.
! type(dio_struct)    dio = reference to the DIO data structure.
! type(pjar_struct)  pjar = reference to the PJAR data structure.
! char(*)        filetype = identifier for this type of file.
! char(*)         secname = name of the data section on the file.
! integer             err = error flag (returned).
! char(*)             msg = message for possible printing (returned).
! char(*)         goodmsg = good message to return if there have been no errors.
!
! FILETYPE should be an adjective such as 'velocity' or 'static' or 'mute'.
!
! NAME can be a word such as 'velocity' or 'static' or 'mute' or 'history'.
! NAME must exist in the PJAR object.
!
! ERR will be returned as FIO_OK if the operation is successful.
! ERR will be returned as FIO_ERROR if an error has occurred.
! ERR will be returned as FIO_EOF if an end-of-file was read.
!
! Call routines in PJAR to access the contents of any header section.
!
!                      ++++++++++++++++++++++++++++
!
! FIO_CREATE:
!  (1) the file must first be opened by calling DIO_OPEN.
!  (2) PJAR must first be created by calling PJAR_CREATE.
!  (3) these allocate the FIO data structure and rewind the file.
!
! FIO_DELETE:
!  (1) deallocates the FIO data structure unless already deallocated.
!  (2) the file must then be closed by calling DIO_CLOSE.
!  (3) PJAR must then be deleted by calling PJAR_DELETE.
!
! FIO_READ_HEADER_SECTIONS:
!  (1) rewinds the file and clears PJAR.
!  (2) then reads all header sections from the file into PJAR.
!  (3) adds header section FOREIGN if there are no header sections.
!
! FIO_WRITE_HEADER_SECTIONS:
!  (1) rewinds the file.
!  (2) then writes all header sections from PJAR into the file.
!
! FIO_READ_NEXT_DATA_SECTION:
!  (1) moves to the next data section on the file without rewinding.
!  (2) the associated (existing) header section is then found and made current.
!
! FIO_READ_DATA_SECTION:
!  (1) the associated (existing) header section is first found and made current.
!  (2) positions the file to read the requested data section.
!  (3) if the byte address of the beginning of the data section is in the
!       associated header section, data sections can be read in any order
!       and multiple times.
!  (4) when reading a data section whose byte address is NOT in the associated
!       header section, moves to the requested data section without rewinding,
!       which means that the data sections should be read in the correct order.
!
! FIO_WRITE_DATA_SECTION:
!  (1) the associated (existing) header section is first found and made current.
!  (2) positions the file to write the requested data section.
!  (3) data sections can be written in any order.
!
!-------------------------------------------------------------------------------
!            READ ASCII OR HYBRID DATA FROM THE CURRENT DATA SECTION
!            WRITE ASCII OR HYBRID DATA TO THE CURRENT DATA SECTION
!                 (encoding must be FIO_ASCII or FIO_HYBRID)
!
! Read or write a single ascii card image (or binary equivalent)
! in the current data section:
!
!                                        b 
!          call fio_before_read_line   (obj)
!          call fio_before_write_line  (obj)
!                                                         opt
!                                        b    o    o       o
!          call fio_after_read_line    (obj, err, msg, end_packet)
!          call fio_after_write_line   (obj, err, msg)
!
!                                        i     i       o
!          call fio_read_line          (obj, column, value)
!          call fio_write_line         (obj, column, value, nchar, ndec)
!                                        b     i       i      i     i
!                                                            opt   opt
!
!                      ++++++++++++++++++++++++++++
!
! type(fio_struct)  obj = pointer to the FIO data structure.
! integer        column = column number of field to read or write.
! (any type)      value = the value in the specified column in the file.
! (any type)   array(:) = an array of values in the specified column.
! integer         nchar = maximum number of characters to encode.
! integer          ndec = maximum number of decimals to encode.
! integer           err = error flag (returned).
! char(*)           msg = message for possible printing (returned).
! logical    end_packet = true if finished reading the data packet.
!
! VALUE can be character(len=*) or integer or real or double precision
! or logical.
!
! The NCHAR argument is for integer, real, and double precision variables only.
! The NDEC argument is for real and double precision variables only.
! No maximum restrictions are imposed if NCHAR and NDEC are not specified.
! NCHAR and NDEC are ignored for hybrid data.
!
! ERR will be returned as FIO_OK if the operation is successful.
! ERR will be returned as FIO_ERROR if an error has occurred.
! ERR will be returned as FIO_EOF if an end-of-file or end-of-section was read.
!
!                      ++++++++++++++++++++++++++++
!
! FIO_BEFORE_READ_LINE:
! FIO_BEFORE_WRITE_LINE:
! FIO_AFTER_READ_LINE:
! FIO_AFTER_WRITE_LINE:
!  (1) must bracket multiple calls to the read and write routines.
!  (2) one pair of these routines brackets a single ascii card image
!       (or binary equivalent).
!
! FIO_READ_LINE:
!  (1) the default is returned if the column number is out of range.
!  (2) the default is the last value read from the column.
!  (3) the default starts out as the 'defaults' value in PJAR.
!  (4) nil is returned if there was an error.
!  (5) can be called for different columns in any order.
!  (6) not all columns need to be specified.
!
! FIO_WRITE_LINE:
!  (1) nothing is done if the column number is out of range.
!  (2) the default is written for column numbers not specified.
!  (3) the default is the last value written to the column.
!  (4) the default starts out as the 'defaults' value in PJAR.
!  (5) can be called for different columns in any order.
!  (6) not all columns need to be specified.
!
!-------------------------------------------------------------------------------
!               READ BINARY DATA FROM THE CURRENT DATA SECTION
!               WRITE BINARY DATA TO THE CURRENT DATA SECTION
!                      (encoding must be FIO_BINARY)
!
! Read or write a single packet of binary data in the current data section:
!
!                                       b     o
!        call fio_before_read_binary  (obj, npicks)
!        call fio_before_write_binary (obj, npicks)
!                                       b     i
!
!                                       b    o    o 
!        call fio_after_read_binary   (obj, err, msg)
!        call fio_after_write_binary  (obj, err, msg)
!
!                                       b     i       o
!        call fio_read_binary         (obj, column, scalar)
!        call fio_write_binary        (obj, column, scalar)
!                                       b     i       i
!        
!                                       b     i       o  
!        call fio_read_binary         (obj, column, array)
!        call fio_write_binary        (obj, column, array)
!                                       b     i       i  
!
!                      ++++++++++++++++++++++++++++
!
! type(fio_struct)       obj = pointer to the FIO data structure.
! integer             npicks = number of picks in the data packet.
! integer             column = "column" number of field to read or write.
! (any type)          scalar = a single scalar value (a delimiter).
! (any type)   array(npicks) = an array of values (not a delimiter).
! integer                err = error flag (returned).
! char(*)                msg = message for possible printing (returned).
!
! SCALAR and ARRAY can be character(len=*) or integer or real or double
! precision or logical.
!
! ERR will be returned as FIO_OK if the operation is successful.
! ERR will be returned as FIO_ERROR if an error has occurred.
! ERR will be returned as FIO_EOF if an end-of-file or end-of-section was read.
!
!                      ++++++++++++++++++++++++++++
!
! FIO_BEFORE_READ_BINARY:
! FIO_BEFORE_WRITE_BINARY:
! FIO_AFTER_READ_BINARY:
! FIO_AFTER_WRITE_BINARY:
!  (1) must bracket multiple calls to the read and write routines.
!  (2) one pair of these routines brackets a single packet of binary data.
!
! FIO_READ_BINARY:
!  (1) the default is returned if the column number is out of range.
!  (2) the default is the last value read from the column.
!  (3) the default starts out as the 'defaults' value in PJAR.
!  (4) nil is returned if there was an error.
!  (5) can be called for different columns in any order.
!  (6) not all columns need to be specified.
!
! FIO_WRITE_BINARY:
!  (1) nothing is done if the column number is out of range.
!  (2) the default is written for column numbers not specified.
!  (3) the default is the last value written to the column.
!  (4) the default starts out as the 'defaults' value in PJAR.
!  (5) can be called for different columns in any order.
!  (6) not all columns need to be specified.
!
!-------------------------------------------------------------------------------
!</calling_doc>

 
!<advice_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL KEYWORDS AND DEFAULTS
!
! Here is a list of all keywords used by this primitive.  Many of these
! keywords must be present in any header section which has an associated data
! section.  None of these special keywords are required if this header section
! does not have an associated data section.
!
!   variable name       description
!   -------------    -----------------------------------------------------
!    encoding        encoding format of this file.
!    fields(:)       list of fields (names of columns) to read or write.
!    defaults(:)     list of default values for each field.
!    hdrs(:)         list of header word numbers corresponding to fields.
!    units(:)        units corresponding to fields.
!    vartypes(:)     variable types corresponding to fields.
!    delimiters(:)   whether each field is a data packet delimiter.
!    widths(:)       widths (in characters) of each field to output.
!    converters(:)   how to convert fields when reading.
!    skip(:)         true to skip reading the column (return default instead).
!    nilstring       string for nil values in the file (normally 'nil').
!    wrap            number of card images per record (normally 1).
!    ncolumns        number of columns in the data section.
!    nlines          number of lines (card images) in the data section.
!    npackets        number of data packets in the data section.
!    maxpicks        maximum number of picks in a data packet.
!    firstline       line to start reading on (for foreign files only).
!    template        template for adjusting foreign input card image.
!    maxchars(:)     maximum number of characters in each field to decode.
!    fillout         true forces all columns to always be written.
!    noheaders       true forces elimination of output ascii header sections.
!
!                                      default
!      keyword       variable type      value     ASCII    HYBRID   BINARY
!   -------------   ----------------   -------    -----    ------   ------
!   'encoding'      character scalar     ' '      RW yes   RW yes   RW yes
!   'fields'        character array  (no default) RW yes   RW yes   RW yes
!   'defaults'      character array      ' '      RW yes   RW yes   RW yes
!   'hdrs'          integer   array       0          yes      yes      yes
!   'units'         character array      ' '         yes      yes      yes
!   'vartypes'      character array      ' '      RW yes   RW yes   RW yes
!   'delimiters'    logical   array    .false.    RW yes   RW yes   RW yes
!   'widths'        integer   array       0        W no       no       no 
!   'converters'    character array      ' '      R  no    R  no    R  no 
!   'skip'          logical   array    .false.    R  no    R  no       no 
!   'nilstring'     character scalar    'nil'     RW yes      no       no 
!   'wrap'          integer   scalar      1       RW yes      no       no 
!   'ncolumns'      integer   scalar   #fields    RW yes   RW yes   RW yes
!   'nlines'        integer   scalar     -1        C yes    C yes    C yes
!   'npackets'      integer   scalar     -1        C yes    C yes    C yes
!   'maxpicks'      integer   scalar      0        C yes    C yes    C yes
!   'firstline'     integer   scalar    INIL      R  no       no       no 
!   'template'      character scalar     ' '      R  no       no       no 
!   'maxchars'      integer   array       0       R  no       no       no 
!   'fillout'       logical   scalar   .false.     W no     W no       no 
!   'noheaders'     logical   scalar   .false.     W no       no       no 
!
!     R   means the parameter is needed by FIO while reading a file.
!     W   means the parameter is needed by FIO while writing a file.
!     U   means the parameter is to be user-specified while writing a file.
!     C   means the parameter is  calculated by FIO   while writing a file.
!     yes means the parameter resides on the file.
!     no  means the parameter does not reside on the file.
!
! When reading a file:
!  (1) The parameters marked "yes" above will normally be defined in the file
!       and can be obtained if necessary by calling PJAR_GET.
!  (2) Any required missing parameters can be specified by calling PJAR_PUT
!       unless the defaults are acceptable.
!  (3) Any parameters on the file can be modified by calling PJAR_PUT.
!  (4) The parameters marked "no" above may be specified by calling PJAR_PUT.
!  (5) FIRSTLINE must be specified by calling PJAR_PUT to read ascii files
!       without section delimiters (normally foreign files).
!  (6) TEMPLATE and/or MAXCHARS may have to be specified by calling PJAR_PUT
!       to read ascii files with non-standard columns (normally foreign files).
!
! When writing a file:
!  (1) The parameters marked 'yes' above will be written to the file.
!  (2) The parameters marked both "W" and "yes" above may be specified by
!       calling PJAR_PUT unless the default values are acceptable.
!
! Any additional parameters can be written to the file by calling PJAR_PUT and
! retrieved from the file by calling PJAR_GET.
!
! ENCODING must be set to FIO_ASCII for ascii data sections and foreign files.
! ENCODING must be set to FIO_BINARY for binary data sections.
! ENCODING must be set to FIO_HYBRID for hybrid style data sections.
!
! ENCODING should be missing, or should NOT be set to FIO_ASCII or FIO_BINARY
! or FIO_HYBRID, if this header section does not have an associated data setion.
!
! VARTYPES must be set to 'I' or 'F' or 'D' or 'C' or 'L'.
!
! If FILLOUT is false, the number of columns written may be reduced on
! lines which are not the first line of a data packet, but only if the
! encoding is ascii or hybrid, and wrap is 1, and there are delimiting
! values, and the last column is a delimiting value.  In all other cases,
! the FILLOUT parameter is irrelevant and all columns will be written
! on every card image (or wrapped card image).
!
!-------------------------------------------------------------------------------
!                    HOW TO DEAL WITH FOREIGN FILES
!
! A file of foreign origin differs from a CPS self defining ascii file
! only by having missing information in the file header, and possibly some
! missing columns.  If a foreign file is opened and found to contain missing
! information, it can still be read as long as at least some of the missing
! information is supplied before beginning to read the data in the file.
! Usually, a person has to look at the first part of the file to figure out
! what the missing information should be.
!
! This FIO primitive contains the ability to read a foreign file by providing
! the missing information it requires.  You will note that the missing
! information is input to this primitive before reading the file, whereas
! the same information would be read and returned from a self defining file.
! The derived class which reads a specific type of foreign file may also need
! to provide additional missing information for its own use.
!
! For batch programs, the missing information to read a foreign file would
! have to be set up ahead of time, and the batch program configured (perhaps
! using keywords and values in data cards) to receive the missing information
! to read the file.  If this effort is not available, the batch program should
! terminate with an error message.  A better alternative is to convert the
! foreign file to a self defining file before running the batch program.
!
! A foreign file can be converted to a self defining file simply by editing
! the file to put information at the front of the file in the correct self
! defining format.  Or the foreign file might be converted using an interactive
! program.
!
! For interactive programs, the missing information to read a foreign file
! can be supplied interactively by supplying the user with a view of the
! first part of the foreign file in a window, and providing GUI elements
! for the user to enter the correct missing information.  The user then tells
! the program to reread the file after all necessary information is supplied.
! Any desired information (missing or not) can be supplied or modified in
! this way.
!
! The only critical information required for reading a foreign file may be the
! association between the columns and the types of values they contain.  This
! information is supplied as a list of field names in the proper order.
! Alternatively, the data can be read simply by specifying the desired
! column numbers.
!
! Although other missing parameters may not be required for reading the file
! successfully, they might be required by the calling program to use the
! data in a desired manner.
!
!-------------------------------------------------------------------------------
!                    RECOMMENDATIONS FOR FOREIGN FILES
!                    OR FILES WITH MISSING INFORMATION
!
! If the file being read might be a readable foreign file with with no self
! defining information, the returned FILETYPE will probably be blank, and
! the PJAR object will probably be empty.  But if the file being read is
! in fact a Conoco self defining file but has missing information, the PJAR
! object will not contain the missing information.  In either case, the
! following steps can be taken to read the file:
!
! Batch programs:
!
!  (1) If a batch program is being used to read the file, an attempt to read
!      it should be considered an error unless reasonable defaults can be used
!      in place of the missing information.
!
! Interactive programs:
!
!  (1) If an interactive program is being used to read the file, an error
!      indication should NOT necessarily be considered a reason to reject
!      the file.
!
!  (2) The interactive program should display the first part of the file
!      in a window to the user.
!
!  (3) The returned MSG argument and the view of the first part of the file
!      should provide the user with enough information to decide whether
!      to abandon attempts to read the file, or to attempt to provide
!      missing information.
!
!  (4) The interactive program should provide GUI elements for the user to
!      reset the missing information.
!
!  (5) When the user chooses, the interactive program should reread the file
!      using the information which has been supplied by the user through the
!      GUI.  This time, the returned ERR argument will be set to FIO_OK if
!      the file can be read, or to FIO_ERROR if the file still needs more
!      missing information or if there was another error.
!
!-------------------------------------------------------------------------------
!                 PARAMETERS TO USE FOR READING FOREIGN FILES
!
! The following parameters (which are mentioned above) may have to be
! supplied to read foreign files successfully:
!
! FIELDS(NFIELDS):
!
!  (1) list of fields (names of columns) to read or write.
!  (2) must always be specified (no defaults).
!  (3) cannot be blank.
!  (4) also needed for all self defining files (will be on the file).
!
! NILSTRING:
!
!  (1) string representing nil values in an ascii file.
!  (2) default is 'nil'.
!  (3) also needed for all self defining ascii files (will be on the file).
!
! WRAP:
!
!  (1) number of card images per record in an ascii file.
!  (2) default is 1.
!  (3) useful for files with wrapped lines, where more than one card image
!       contributes to a set of fields to be decoded.
!  (4) wrapping the lines keeps individual lines from being too long for
!       easier reading in a text editor.
!  (5) LAS files are sometimes wrapped files.
!  (6) also needed for all self defining ascii files (will be on the file).
!
! FIRSTLINE:
!
!  (1) line to start reading on.
!  (2) default is INIL (meaning that a pre data tag should be searched for).
!  (3) useful for files which contain header information without # at the
!       beginning of the line, or files without a pre data tag.
!  (4) LAS files are an example of this kind of file.
!
! TEMPLATE:
!
!  (1) template for adjusting foreign input card image.
!  (2) default is ' ' (a blank character string).
!  (3) if WRAP > 1, this parameter is used on only the first card image.
!  (4) a hyphen character (-) at a specified location means that the
!       corresponding character location (character number in card image)
!       should be ignored, and will be set to a space character.
!  (5) a non-blank character (other than a hyphen) at a specified location
!       means that a new field, which might not be separated from the previous
!       field, starts at the corresponding character location of the card image.
!  (6) can be used when two adjacent fields are not separated by a space, and
!       the beginning of the second of the two juxtaposed fields always begins
!       at the same character location in all card images.
!  (7) can be used when there is a region of undesirable information, possibly
!       broken into variable numbers of blank-delimited fields, which always
!       resides at the same character location in all card images.
!  (8) commas are always ignored, even when WRAP > 1 or the template is blank.
!  (9) example for desired fields aaaa, bbbbb, ccccc, and undesirable field xx:
!       original card image:     ' xxx x aaaabbbbb, ccccc     '
!       template:                ' -----     x                '
!       modified card image:     '       aaaa bbbbb  ccccc    '
!
! MAXCHARS(NMAXCHARS):
!
!  (1) maximum number of characters in each field to decode.
!  (2) default is all zeroes.
!  (3) zero means to use all of the characters in the field with no maximum.
!  (4) positive or negative means to use no more than this many characters.
!  (5) positive means the rest of the field becomes the next field.
!  (6) negative means the rest of the field is skipped.
!  (7) can be used when two adjacent fields are not separated by a space, and
!       the beginning of the second of the two juxtaposed fields always begins
!       after the same number of characters in the combined field.
!  (8) works for all values of WRAP.
!  (9) the fields do not have to begin at the same character location on
!       different card images.
! (10) example for desired fields aaaa, bbbbb, ccccc, ddd, ee, and fff:
!       original or modified card image:  ' aaaabbbbb  cccccwww ddd  eefffgg '
!       maxchars:                            4    0     -5       0   2 -3
!       retained fields:                    aaaa bbbbb  ccccc   ddd  ee fff
!
! CONVERTERS(NCONVERTERS):
!
!  (1) how to convert input columnar values.
!  (2) default is all ones.
!  (3) ' '     means use the number as is.
!  (4) '1000'  means multiply the number by 1000.
!  (5) '0.001' means multiply the number by 0.001 (not done on integers).
!  (6) 'recip' means take the reciprocal (not done on integers).
!  (7) anything else uses the number as is.
!  (8) ignored for non-numeric fields.
!  (9) not used when writing data sections.
! (10) no conversion is done on nil or zero values.
!
! SKIP(NSKIP):
!
!  (1) whether the column should be skipped while reading the file.
!  (2) default is all false.
!  (3) if true, an attempt to read the column will return the default, or
!       nil if the variable type is incompatible.
!
!-------------------------------------------------------------------------------
!                   TO READ OR WRITE A FILE (EXAMPLE)
!
! The following sub-sections illustrate examples of code to read or write
! a file with the following characteristics:
!
! filename to read or write:            'norway'
! section on the file to read or write: 'bergen'
! keywords to get or put:               'hello' and 'goodby'
! fields to read or write:              'sardine' and 'haddock' and 'herring'
!
!-------------------------------------------------------------------------------
!                    OPEN A SELF DEFINING INPUT FILE
!
! Calling FIO_FIND can be omitted if column numbers are already known.
!
!     call pjar_create              (pjar)
!     call dio_open_read            (dio, 'norway', err, msg)
!     call fio_create               (fio, dio)
!     call fio_read_header_sections (fio, pjar, err, msg)
!     call pjar_choose_section      (pjar, 'bergen')
!
!     call pjar_get (pjar, 'encoding', encoding)
!     call pjar_get (pjar, 'hello'   , hello)
!     call pjar_get (pjar, 'goodby'  , goodby)
!     call pjar_get (pjar, 'nlines'  , nlines)                   ! if needed.
!     call pjar_get (pjar, 'npackets', npackets)                 ! if needed.
!     call pjar_get (pjar, +++ anything else you want ++++)      ! if needed.
!     call pjar_put (pjar, +++ any missing information +++)      ! if needed.
!
!     call pjar_choose_section (pjar, 'history')             ! optional.
!     call pjar_alloc_cards    (pjar, pcards, ncards)        ! optional.
!     call pjar_choose_section (pjar, 'bergen')
!
!     column_sardine = pjar_find (pjar, 'fields', 'sardine')
!     column_haddock = pjar_find (pjar, 'fields', 'haddock')
!     column_herring = pjar_find (pjar, 'fields', 'herring')
!                                                         
!     call fio_read_data_section  (fio, pjar, 'bergen', err, msg)
!
!-------------------------------------------------------------------------------
!                     OPEN A FOREIGN ASCII INPUT FILE
!
! The HELLO and GOODBY parameters are not available.
! Fields must be listed in the correct column order.
! Calling FIO_FIND can be omitted if column numbers are already known.
!
!     call pjar_create         (pjar)
!     call pjar_choose_section (pjar, 'anyname')
!
!     call pjar_put (pjar, 'fields'    , (/'haddock','sardine','herring'/),3)
!     call pjar_put (pjar, 'encoding'  , FIO_ASCII)
!     call pjar_put (pjar, 'nilstring' , nilstring)              ! if needed.
!     call pjar_put (pjar, 'wrap'      , wrap)                   ! if needed.
!     call pjar_put (pjar, 'firstline' , firstline)              ! if needed.
!     call pjar_put (pjar, 'template'  , template)               ! if needed.
!     call pjar_put (pjar, 'maxchars'  , maxchars  , nmaxchars)  ! if needed.
!     call pjar_put (pjar, 'converters', converters, nconverters)! if needed.
!     call pjar_put (pjar, 'skip'      , skip      , nskip)      ! if needed.
!     call pjar_put (pjar, +++ any missing information +++)      ! if needed.
!
!     column_sardine = pjar_find (pjar, 'fields', 'sardine')
!     column_haddock = pjar_find (pjar, 'fields', 'haddock')
!     column_herring = pjar_find (pjar, 'fields', 'herring')
!                                                         
!     call dio_open_read           (dio, 'norway', err, msg)
!     call fio_create              (fio, dio)
!     call fio_read_data_section   (fio, pjar, 'anyname', err, msg)
!
!-------------------------------------------------------------------------------
!                    OPEN A SELF DEFINING OUTPUT FILE
!
! Calling FIO_FIND can be omitted if column numbers are already known.
!
!     call pjar_create         (pjar)
!     call pjar_choose_section (pjar, 'bergen')
!
!     call pjar_put (pjar, 'encoding', encoding)
!     call pjar_put (pjar, 'hello'   , hello)
!     call pjar_put (pjar, 'goodby'  , goodby)
!     call pjar_put (pjar, +++ anything else you want ++++)       ! if needed.
!
!     call pjar_choose_section (pjar,'history',err,msg)         ! optional.
!     call pjar_put_cards      (pjar,cards,ncards,progname)     ! optional.
!     call pjar_choose_section (pjar, 'bergen')
!
!     column_sardine = pjar_find (pjar, 'fields', 'sardine')
!     column_haddock = pjar_find (pjar, 'fields', 'haddock')
!     column_herring = pjar_find (pjar, 'fields', 'herring')
!                                                         
!     call dio_open_write            (dio, 'norway', err, msg)
!     call fio_create                (fio, dio)
!     call fio_write_header_sections (fio, pjar, err, msg)
!     call fio_write_data_section    (fio, pjar, 'bergen', err, msg)
!
!-------------------------------------------------------------------------------
!                    READ OR WRITE AN ASCII CARD IMAGE
!                         (or binary equivalent)
!
! ENCODING must be FIO_ASCII or FIO_HYBRID.
! The calls to FIO_READ and FIO_WRITE can be in any order.
!
!     call fio_before_read_line  (fio)                         
!     call fio_read_line         (fio, column_sardine, sardine)
!     call fio_read_line         (fio, column_haddock, haddock)
!     call fio_read_line         (fio, column_herring, herring)
!     call fio_after_read_line   (fio, err, msg)                    
!
!
!     call fio_before_write_line (fio)
!     call fio_write_line        (fio, column_sardine, sardine)
!     call fio_write_line        (fio, column_haddock, haddock)
!     call fio_write_line        (fio, column_herring, herring)
!     call fio_after_write_line  (fio, err, msg)                    
!
!-------------------------------------------------------------------------------
!         READ OR WRITE AN ENTIRE ASCII DATA SECTION TO OR FROM ARRAYS
!                         (or binary equivalent)
!
! ENCODING must be FIO_ASCII or FIO_HYBRID.
! The calls to FIO_READ and FIO_WRITE can be in any order.
!
!     indx = 0
!     do
!          !!!!! reallocate the arrays here if necessary.
!          !!!!! arrays can be pre-allocated if NLINES is known.
!          indx = indx + 1
!          call fio_before_read_line  (fio)                    
!          call fio_read_line         (fio, column_sardine, sardine(indx))
!          call fio_read_line         (fio, column_haddock, haddock(indx))
!          call fio_read_line         (fio, column_herring, herring(indx))
!          call fio_after_read_line   (fio, err, msg)                    
!          if (err /= FIO_OK) exit
!     end do
!     nlines = indx - 1
!
!
!     do indx = 1,nlines
!          call fio_before_write_line (fio)                         
!          call fio_write_line        (fio, column_sardine, sardine(indx))
!          call fio_write_line        (fio, column_haddock, haddock(indx))
!          call fio_write_line        (fio, column_herring, herring(indx))
!          call fio_after_write_line  (fio, err, msg)                    
!          if (err /= FIO_OK) exit
!     end do
!
!-------------------------------------------------------------------------------
!                    READ OR WRITE AN ASCII DATA PACKET
!                         (or binary equivalent)
!
! ENCODING must be FIO_ASCII or FIO_HYBRID.
! SARDINE and HERRING are data packet delimiters (DELIMITERS flags are true).
! HADDOCK(NPICKS) is not a data packet delimiter (the DELIMITERS flag is false).
! NPICKS must be >= 1.
! The calls to FIO_READ and FIO_WRITE can be in any order.
!
!     indx = 0
!     do
!          !!!!! reallocate the arrays here if necessary.
!          !!!!! arrays can be pre-allocated if MAXPICKS is known.
!          indx = indx + 1
!          call fio_before_read_line (fio)
!          call fio_read_line        (fio, column_sardine, sardine)
!          call fio_read_line        (fio, column_haddock, haddock(indx))
!          call fio_read_line        (fio, column_herring, herring)
!          call fio_after_read_line  (fio, err, msg, end_packet)
!          if (end_packet) exit                          o
!     end do
!     npicks = indx - 1
!
!
!     do indx = 1,npicks
!          call fio_before_write_line (fio)                         
!          call fio_write_line        (fio, column_sardine, sardine)
!          call fio_write_line        (fio, column_haddock, haddock(indx))
!          call fio_write_line        (fio, column_herring, herring)
!          call fio_after_write_line  (fio, err, msg)                    
!          if (err /= FIO_OK) exit
!     end do
!
!-------------------------------------------------------------------------------
!                   READ OR WRITE A BINARY DATA PACKET
!
! ENCODING must be FIO_BINARY.
! SARDINE and HERRING are scalars and are flagged as data packet delimiters.
! HADDOCK(NPICKS) is an array and is NOT flagged as a data packet delimiter.
! NPICKS must be >= 0.
! The calls to FIO_READ_BINARY and FIO_WRITE_BINARY can be in any order.
!
! If NPICKS is equal to the total number of "card images" in the data section,
! there is only one data packet consisting of the entire data section.
!
! If NPICKS is always 1, each data packet is the equivalent of a single
! card image, and all fields must be scalars (and flagged as delimiters).
!
!                                       o
!   call fio_before_read_binary (fio, npicks)
!   call fio_read_binary        (fio, column_sardine, sardine)  ! scalar
!   call fio_read_binary        (fio, column_haddock, haddock)  ! array(npicks)
!   call fio_read_binary        (fio, column_herring, herring)  ! scalar
!   call fio_after_read_binary  (fio, err, msg)
!
!                                        i
!   call fio_before_write_binary (fio, npicks)
!   call fio_write_binary        (fio, column_sardine, sardine)  ! scalar
!   call fio_write_binary        (fio, column_haddock, haddock)  ! array(npicks)
!   call fio_write_binary        (fio, column_herring, herring)  ! scalar
!   call fio_after_write_binary  (fio, err, msg)
!
!-------------------------------------------------------------------------------
!                           CLOSE THE FILE
!
!     call fio_delete (fio)
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                 
!
!     Date        Author     Description
!     ----        ------     -----------
!012. 2006-04-25  B. Menger   Removed Unused Variables.
! 11. 2004-09-01  Stoeckley  Fix bug whereby the NDEC argument was ignored by
!                             fio_write_line_dvar; change the NOHEADERS
!                             parameter to write a single header line; write
!                             a pound sign at the beginning of all blank lines.
! 10. 2003-12-09  Stoeckley  Add 0.001 to list of possible converters.
!  9. 2002-06-24  Stoeckley  Make several efficiency improvements.
!  8. 2002-05-20  Stoeckley  Replace any white space with an actual space
!                             when reading ascii card images.
!  7. 2002-05-02  Stoeckley  Increase and standardize length of several
!                             character variables.
!  6. 2002-04-11  Stoeckley  Add ability to omit writing out header sections;
!                             change to add a foreign header section when
!                             reading a file without header sections; add
!                             SKIP array parameter; these additions are needed
!                             to support workstation applications.
!  5. 2002-02-04  Stoeckley  Modify to further simplify use with interactive
!                             programs and to make the self-defining features
!                             more complete and more easily extendable.
!  4. 2000-11-27  Stoeckley  Do considerable simplifying and overhauling to
!                             better support using interactive programs to
!                             read foreign files with missing information;
!                             add ability to read and write history cards;
!                             add calls to the FINQUIRE primitive; add ability
!                             to read and write wrapped files and to read
!                             foreign files starting at a specified line number
!                             and files with adjacent columns of numbers not
!                             separated by spaces; add ability to read and
!                             write multiple header sections and multiple
!                             ascii and/or binary data sections.
!  3. 2000-03-17  Stoeckley  Change error constants to be the same as in CIO;
!                             add test for unspecified file name.
!  2. 2000-01-28  Stoeckley  Add byte swapping to binary read/write routines.
!  1. 1999-11-17  Stoeckley  Initial version.
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


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! This primitive was originally built on top of CPSIO, but also made direct
! calls to CIO.  Not all features in CPSIO were used in this primitive.  For
! example, reading of named fields in columnar ascii files is done differently
! (for efficiency) by converting field names to column numbers rather than
! using field names directly for each card image.  Other features were needed
! but not supported in CPSIO.  For example, getting card images rather than
! keywords and parameters from header records, supporting multiple binary
! data sections, and supporting the grid_struct data type.  In fact, it was
! determined that eliminating CPSIO entirely as a middle man between FIO and
! CIO was possible without significantly increasing the amount of code in FIO.
! Therefore, FIO no longer uses CPSIO, although it continues to conform to the
! self-defining file format standards enforced by CPSIO.
!
! This primitive does not pretend to have all of the functionality and
! generality of CPSIO.  It contains only those features useful for reading
! and writing self-defining ascii and binary sequential non-trace files.
! This primitive is customized to make it as easy as possible to access
! such files with a minimum of effort, with as many details as possible
! taken care of by this primitive.  Simplicity is bliss.
!
! This primitive uses the DIO primitive, which accesses CIO, instead of the
! CIO primitive directly.  The reasons are twofold: (1) DIO hides several messy
! details, such as dealing with the SIZEOF primitive and doing byte swapping.
! (2) We need DIO to read and write old-style (non self-defining) files, so
! by using DIO also in FIO allows the file to be opened once (by DIO) and
! then read or written either way.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
 
 
      module fio_module
      use string_module
      use dio_module
      use pjar_module
      use grid_module
      use fioutil_module
      implicit none
      public

      character(len=100),public,save :: FIO_IDENT = &
'$Id: fio.f90,v 1.12 2006/04/25 12:00:54 Menger prod sps $'


!!-------------------------- private routines -------------------------------!!
!!-------------------------- private routines -------------------------------!!
!!-------------------------- private routines -------------------------------!!


      private :: fio_private_initialize
      private :: fio_private_prepare

      private :: fio_rewrite_scalar
      private :: fio_rewrite_array

      private :: fio_private_convert_ivar
      private :: fio_private_convert_fvar
      private :: fio_private_convert_dvar

      private :: fio_private_fixup_card
      private :: fio_private_fixup_tokens
      private :: fio_private_write_wrapped_line

      private :: fio_private_finish_tasks
      private :: fio_private_finish_read_data
      private :: fio_private_finish_write_data

      private :: fio_private_header_tag
      private :: fio_private_data_tag
      private :: fio_private_end_tag
      private :: fio_is_header_tag
      private :: fio_is_data_tag 
      private :: fio_is_end_tag   
      private :: fio_name_from_tag 
      private :: fio_write_header_tag
      private :: fio_write_data_tag 
      private :: fio_write_end_tag
      private :: fio_read_filetype 
      private :: fio_write_filetype 


!!------------------------------- interfaces --------------------------------!!
!!------------------------------- interfaces --------------------------------!!
!!------------------------------- interfaces --------------------------------!!


      interface fio_read_line
           module procedure fio_read_line_cvar
           module procedure fio_read_line_ivar
           module procedure fio_read_line_fvar
           module procedure fio_read_line_dvar
           module procedure fio_read_line_lvar
      end interface

      interface fio_write_line
           module procedure fio_write_line_cvar
           module procedure fio_write_line_ivar
           module procedure fio_write_line_fvar
           module procedure fio_write_line_dvar
           module procedure fio_write_line_lvar
      end interface

      interface fio_read_binary
           module procedure fio_read_binary_cvar
           module procedure fio_read_binary_ivar
           module procedure fio_read_binary_fvar
           module procedure fio_read_binary_dvar
           module procedure fio_read_binary_lvar
           module procedure fio_read_binary_cvars
           module procedure fio_read_binary_ivars
           module procedure fio_read_binary_fvars
           module procedure fio_read_binary_dvars
           module procedure fio_read_binary_lvars
      end interface

      interface fio_write_binary
           module procedure fio_write_binary_cvar
           module procedure fio_write_binary_ivar
           module procedure fio_write_binary_fvar
           module procedure fio_write_binary_dvar
           module procedure fio_write_binary_lvar
           module procedure fio_write_binary_cvars
           module procedure fio_write_binary_ivars
           module procedure fio_write_binary_fvars
           module procedure fio_write_binary_dvars
           module procedure fio_write_binary_lvars
      end interface

 
!!------------------------- static parameters ------------------------------!!
!!------------------------- static parameters ------------------------------!!
!!------------------------- static parameters ------------------------------!!

 
      integer,public,parameter :: FIO_OK      = DIO_OK 
      integer,public,parameter :: FIO_ERROR   = DIO_ERROR
      integer,public,parameter :: FIO_EOF     = DIO_EOF 

      character(len=*),public,parameter :: FIO_ASCII  = FIOUTIL_ASCII
      character(len=*),public,parameter :: FIO_BINARY = FIOUTIL_BINARY
      character(len=*),public,parameter :: FIO_HYBRID = FIOUTIL_HYBRID

      integer,private,parameter :: MAXCOLUMNS   = FIOUTIL_MAXCOLUMNS
      integer,private,parameter :: BINARY_EOS   = -765     ! must be negative.
      integer,private,parameter :: BAD_NCOLUMNS = -876     ! must be negative.
      integer,private,parameter :: FIXLENGTH    = 70
      integer,private,parameter :: LRECORD      = 200
      integer,private,parameter :: LCOMBINED    = 600
      integer,private,parameter :: LCHAR        = 40
      integer,private,parameter :: LMSG         = 80

      character(len=1),private,parameter :: COMMENT = '#'
      character(len=6),private,parameter :: HDR_TAG = '#<HDR_'
      character(len=6),private,parameter :: DTA_TAG = '#<DTA_'
      character(len=3),private,parameter :: END_TAG = '#</'

      character(len=*),private,parameter :: DEF_ENCODING    =  ' '
      character(len=*),private,parameter :: DEF_FIELDS      =  ' '    
      character(len=*),private,parameter :: DEF_DEFAULTS    =  ' '    
      integer         ,private,parameter :: DEF_HDRS        =  0   
      character(len=*),private,parameter :: DEF_UNITS       =  ' '    
      character(len=*),private,parameter :: DEF_VARTYPES    =  ' '    
      logical         ,private,parameter :: DEF_DELIMITERS  =  .false.
      integer         ,private,parameter :: DEF_WIDTHS      =  0   
      character(len=*),private,parameter :: DEF_CONVERTERS  =  ' '
      logical         ,private,parameter :: DEF_SKIP        =  .false.
      character(len=*),private,parameter :: DEF_NILSTRING   =  'nil'   
      integer         ,private,parameter :: DEF_NLINES      =  -1   
      integer         ,private,parameter :: DEF_NPACKETS    =  -1   
      integer         ,private,parameter :: DEF_MAXPICKS    =  0   
      integer         ,private,parameter :: DEF_FIRSTLINE   =  INIL  
      character(len=*),private,parameter :: DEF_TEMPLATE    =  ' '    
      integer         ,private,parameter :: DEF_MAXCHARS    =  0   
      logical         ,private,parameter :: DEF_FILLOUT     =  .false.   
      logical         ,private,parameter :: DEF_NOHEADERS   =  .false.   


!!-------------------------- data structure ---------------------------------!!
!!-------------------------- data structure ---------------------------------!!
!!-------------------------- data structure ---------------------------------!!


      type,public :: fio_struct

         private

! initialized in fio_create:

         type(dio_struct) ,pointer :: dio          ! reference.

         logical             :: whoops
         character(len=LMSG) :: errmsg
         logical             :: noheaders    ! whether to eliminate output
                                             !  header sections.

! initialized in fio_private_initialize:

         logical           :: reading_data_section   ! true if in progress.
         logical           :: writing_data_section   ! true if in progress.

         character(len=LCHAR) :: secname     ! name of section.
         character(len=LCHAR) :: nilstring   ! string representing nil.
         integer              :: wrap        ! #cards per record.
         integer              :: ncolumns    ! number of columns.
         integer              :: ntokens     ! number of tokens in card image.

         integer :: kount_lines    ! line number counter.
         integer :: kount_picks    ! line number counter in a data packet.
         integer :: kount_packets  ! data packet number counter.
         integer :: npicks         ! number of picks in a data packet.
         integer :: maxpicks       ! maximum number of picks in a data packet.
         integer :: ncolumns_fewer ! reduced number of columns to write.
         logical :: delimiting     ! whether delimiting data packets.
         logical :: end_packet     ! whether finished with a data packet.

         integer                :: firstline
         character(len=LRECORD) :: template
         integer                :: maxchars(MAXCOLUMNS)
         integer                :: nmaxchars

         logical  :: is_ascii      ! encoding is FIO_ASCII.
         logical  :: is_binary     ! encoding is FIO_BINARY.
         logical  :: is_hybrid     ! encoding is FIO_HYBRID.
         logical  :: use_template  ! whether using template (for efficiency).
         logical  :: use_maxchars  ! whether using maxchars (for efficiency).
         logical  :: skipping      ! whether skipping       (for efficiency).
         logical  :: wrapping      ! whether wrapping       (for efficiency).
         logical  :: converting    ! whether converting     (for efficiency).

    character(len=LCHAR) :: fields      (MAXCOLUMNS) ! names of fields.
    character(len=1)     :: vartypes    (MAXCOLUMNS) ! variable types.
    logical              :: delimiters  (MAXCOLUMNS) ! data packet delimiters.
    integer              :: widths      (MAXCOLUMNS) ! widths of fields.
    character(len=LCHAR) :: converters  (MAXCOLUMNS) ! data value converters.
    logical              :: skip        (MAXCOLUMNS) ! whether to skip field.
    character(len=LCHAR) :: cdefaults   (MAXCOLUMNS) ! default starting value.
    integer              :: idefaults   (MAXCOLUMNS) ! default starting value.
    real                 :: fdefaults   (MAXCOLUMNS) ! default starting value.
    double precision     :: ddefaults   (MAXCOLUMNS) ! default starting value.
    logical              :: ldefaults   (MAXCOLUMNS) ! default starting value.
    integer              :: addresses (2*MAXCOLUMNS) ! start of binary fields.
    logical              :: written     (MAXCOLUMNS) ! whether column written.

         integer           :: tell_address (2)   ! for rewriting parameter.
         integer           :: tell_nlines  (2)   ! for rewriting parameter.
         integer           :: tell_npackets(2)   ! for rewriting parameter.
         integer           :: tell_maxpicks(2)   ! for rewriting parameter.

         integer           :: address      (2)   ! start of data section.
         integer           :: this_address (2)   ! start of this packet.
         integer           :: next_address (2)   ! start of next packet.

      end type fio_struct


      contains

 
!!------------------------ create and delete -------------------------------!!
!!------------------------ create and delete -------------------------------!!
!!------------------------ create and delete -------------------------------!!

 
      subroutine fio_create (obj,dio)
      implicit none
      type(fio_struct) ,pointer              :: obj           ! arguments
      type(dio_struct) ,intent(inout),target :: dio           ! arguments
 
      allocate (obj)

      obj%dio => dio

      obj%whoops    = .false.
      obj%errmsg    = 'unspecified error message'
      obj%noheaders = DEF_NOHEADERS

      call fio_private_initialize (obj)
      return
      end subroutine fio_create
 
 
                             !!!!!!!!!!!!!


      subroutine fio_delete (obj)
      implicit none
      type(fio_struct),pointer :: obj             ! arguments

      if (associated(obj)) then
           call fio_private_finish_tasks (obj)
           deallocate(obj)
      end if
      return
      end subroutine fio_delete
 
 
!!------------------------- private initialize ------------------------------!!
!!------------------------- private initialize ------------------------------!!
!!------------------------- private initialize ------------------------------!!

! This information is needed only while in the process of reading or
! writing a data section.


      subroutine fio_private_initialize (obj)
      implicit none
      type(fio_struct),intent(inout)        :: obj           ! arguments
 
      obj%reading_data_section = .false.
      obj%writing_data_section = .false.

      obj%secname         = ' '
      obj%nilstring       = DEF_NILSTRING
      obj%wrap            = 1
      obj%ncolumns        = 0
      obj%ntokens         = 0

      obj%kount_lines     = 0
      obj%kount_picks     = 0
      obj%kount_packets   = 0
      obj%npicks          = 0
      obj%maxpicks        = 0
      obj%ncolumns_fewer  = 0
      obj%delimiting      = .false.
      obj%end_packet      = .false.

      obj%firstline       = DEF_FIRSTLINE
      obj%template        = DEF_TEMPLATE
      obj%maxchars(:)     = DEF_MAXCHARS
      obj%nmaxchars       = 0

      obj%is_ascii        = .false.
      obj%is_binary       = .false.
      obj%is_hybrid       = .false.
      obj%use_template    = .false.
      obj%use_maxchars    = .false.
      obj%skipping        = .false.
      obj%wrapping        = .false.
      obj%converting      = .false.

      obj%fields     (:)  = DEF_FIELDS
      obj%vartypes   (:)  = DEF_VARTYPES
      obj%delimiters (:)  = DEF_DELIMITERS
      obj%widths     (:)  = DEF_WIDTHS
      obj%converters (:)  = DEF_CONVERTERS
      obj%skip       (:)  = DEF_SKIP
      obj%cdefaults  (:)  = CNIL
      obj%idefaults  (:)  = INIL
      obj%fdefaults  (:)  = FNIL
      obj%ddefaults  (:)  = DNIL
      obj%ldefaults  (:)  = LNIL
      obj%addresses  (:)  = 0
      obj%written    (:)  = .false.

      obj%tell_address (:) = 0
      obj%tell_nlines  (:) = 0
      obj%tell_npackets(:) = 0
      obj%tell_maxpicks(:) = 0

      obj%address      (:) = 0
      obj%this_address (:) = 0
      obj%next_address (:) = 0
      return
      end subroutine fio_private_initialize
 

!!---------------------------- private prepare ----------------------------!!
!!---------------------------- private prepare ----------------------------!!
!!---------------------------- private prepare ----------------------------!!

! First finds the desired existing section.
! Then gathers the information necessary to read or write the data section.
! Registers an error if any required information is invalid or missing.
! fio_private_initialize must be called before calling this routine.


      subroutine fio_private_prepare (obj,pjar,secname)
      implicit none
      type(fio_struct) ,intent(inout) :: obj                     ! argument
      type(pjar_struct),intent(inout) :: pjar                    ! argument
      character(len=*) ,intent(in)    :: secname                 ! argument
      character(len=LMSG)             :: msg                     ! local
      character(len=8)                :: encoding                ! local
      integer                         :: column,nfields,ndummy   ! local
      logical                         :: fillout                 ! local

!----------initial verifications.

      call fioutil_verify (pjar,secname,msg)

      obj%secname = string_2_lower(secname)

      if (msg /= ' ') call fio_register_error (obj,msg,'parameter errors')

      if (obj%whoops) return

!----------get required information.

  call pjar_get (pjar,'encoding'   ,    encoding              ,DEF_ENCODING)
  call pjar_get (pjar,'nilstring'  ,obj%nilstring             ,DEF_NILSTRING)
  call pjar_get (pjar,'wrap'       ,obj%wrap                  ,1)
  call pjar_get (pjar,'ncolumns'   ,obj%ncolumns              ,0)
  call pjar_get (pjar,'fields'     ,obj%fields    ,nfields    ,DEF_FIELDS)
  call pjar_get (pjar,'defaults'   ,obj%cdefaults ,ndummy     ,DEF_DEFAULTS)
  call pjar_get (pjar,'vartypes'   ,obj%vartypes  ,ndummy     ,DEF_VARTYPES)
  call pjar_get (pjar,'delimiters' ,obj%delimiters,ndummy     ,DEF_DELIMITERS)
  call pjar_get (pjar,'widths'     ,obj%widths    ,ndummy     ,DEF_WIDTHS)
  call pjar_get (pjar,'converters' ,obj%converters,ndummy     ,DEF_CONVERTERS)
  call pjar_get (pjar,'skip'       ,obj%skip      ,ndummy     ,DEF_SKIP)
  call pjar_get (pjar,'firstline'  ,obj%firstline             ,DEF_FIRSTLINE)
  call pjar_get (pjar,'template'   ,obj%template              ,DEF_TEMPLATE)
  call pjar_get (pjar,'maxchars'   ,obj%maxchars,obj%nmaxchars,DEF_MAXCHARS)
  call pjar_get (pjar,'maxpicks'   ,obj%maxpicks              ,DEF_MAXPICKS)
  call pjar_get (pjar,'fillout'    ,fillout                   ,DEF_FILLOUT)

  call pjar_get (pjar,'tell_address' ,obj%tell_address ,ndummy,0)
  call pjar_get (pjar,'tell_nlines'  ,obj%tell_nlines  ,ndummy,0)
  call pjar_get (pjar,'tell_npackets',obj%tell_npackets,ndummy,0)
  call pjar_get (pjar,'tell_maxpicks',obj%tell_maxpicks,ndummy,0)
  call pjar_get (pjar,'address'      ,obj%address      ,ndummy,0)

      if (obj%ncolumns <= 0 .or. obj%ncolumns > nfields) obj%ncolumns = nfields
      if (obj%wrap <            1) obj%wrap = 1
      if (obj%wrap > obj%ncolumns) obj%wrap = obj%ncolumns

!----------check for errors.

      call pjar_status        (pjar,msg)
      call fio_register_error (obj,msg,'preparing parameters')
      if (obj%whoops) return

!----------set logical variables for efficiency.

      obj%is_ascii     = encoding == 'ascii'
      obj%is_binary    = encoding == 'binary'
      obj%is_hybrid    = encoding == 'hybrid'
      obj%use_template = obj%template /= ' '
      obj%use_maxchars = obj%nmaxchars > 0 .and. any(obj%maxchars /= 0)
      obj%skipping     = any(obj%skip)
      obj%wrapping     = obj%wrap > 1
      obj%converting   = any(obj%converters /= DEF_CONVERTERS)

!----------get data packet delimiting information.

      obj%delimiting = .false.
 !!   do column = 1,obj%ncolumns    !! removed 2/13/02
      do column = 1,nfields         !! added   2/13/02
           if (obj%delimiters(column)) obj%delimiting = .true.
      end do

!----------get reduced number of columns.

      if (obj%is_binary) then
           obj%ncolumns_fewer = obj%ncolumns
      else if (obj%is_ascii .and. obj%wrapping) then
           obj%ncolumns_fewer = obj%ncolumns
      else if (fillout .or. .not.obj%delimiting) then
           obj%ncolumns_fewer = obj%ncolumns
      else
           obj%ncolumns_fewer = 0
           do column = 1,obj%ncolumns
                if (.not.obj%delimiters(column)) obj%ncolumns_fewer = column
           end do
      end if

!----------initialize starting values from defaults.

      do column = 1,MAXCOLUMNS
           obj%idefaults(column) = string_ss2ii(obj%cdefaults(column))
           obj%fdefaults(column) = string_ss2ff(obj%cdefaults(column))
           obj%ddefaults(column) = string_ss2dd(obj%cdefaults(column))
           obj%ldefaults(column) = string_ss2ll(obj%cdefaults(column))
      end do

!----------adjust widths for right and left justification.

      do column = 1,MAXCOLUMNS
           if (obj%widths(column) /= 0) then
                if (obj%vartypes(column) == 'C' .or. &
                    obj%vartypes(column) == 'L') then
                     obj%widths(column) = - abs(obj%widths(column))
                else
                     obj%widths(column) =   abs(obj%widths(column))
                end if
           end if
      end do
      return
      end subroutine fio_private_prepare


!!------------------------- read data section -------------------------------!!
!!------------------------- read data section -------------------------------!!
!!------------------------- read data section -------------------------------!!

! Finds the requested (existing) header section and makes it active.
! Gathers the information necessary to read the data section.
! Registers an error if there is no match.
! Registers an error if any required information is invalid or missing.
! Inactivates the header section if there is no match or there is an error.
! Positions the file at the beginning of the data section.


      subroutine fio_read_data_section (obj,pjar,secname,err,msg)
      implicit none
      type(fio_struct) ,intent(inout) :: obj              ! arguments
      type(pjar_struct),intent(inout) :: pjar             ! arguments
      character(len=*) ,intent(in)    :: secname          ! arguments
      integer          ,intent(out)   :: err              ! arguments
      character(len=*) ,intent(out)   :: msg              ! arguments
      character(len=LRECORD)          :: card             ! local
      integer                         :: indx             ! local

!----------get started.

      call fio_private_finish_tasks (obj)
      call fio_private_initialize   (obj)
      call pjar_choose_no_section   (pjar)

      if (obj%whoops) then
           call fio_status (obj,err,msg,'reading data section')
           return
      end if

!----------prepare to read data section.

      call fio_private_prepare (obj,pjar,secname)

      if (obj%whoops) then
           call fio_private_initialize (obj)
           call pjar_choose_no_section (pjar)
           call fio_status (obj,err,msg,'reading data section')
           return
      end if

!----------find data section.

      if (obj%firstline /= INIL .and. obj%firstline >= 1) then

           call dio_rewind (obj%dio)
           do indx = 2,obj%firstline
                call dio_read_card      (obj%dio,card)
                call dio_status         (obj%dio,msg)
                call fio_register_error (obj,msg,'searching for foreign data')
                if (obj%whoops) exit
           end do

      else if (obj%address(1) > 0 .or. obj%address(2) > 0) then

           call dio_seek           (obj%dio,obj%address)
           call dio_status         (obj%dio,msg)
           call fio_register_error (obj,msg,'seeking data')

      else         ! needed only if address of data section not specified.
                   ! searching from current location without rewinding.

           do
                call dio_read_card      (obj%dio,card)
                call dio_status         (obj%dio,msg)
                call fio_register_error (obj,msg,'searching for data')
                if (obj%whoops) then
                  exit
                else if (fio_is_data_tag(card)) then
                  if (fio_name_from_tag(card) == obj%secname) exit
                end if
           end do

      end if

!----------finish up and return.

      if (obj%whoops) then
           call fio_private_initialize (obj)
           call pjar_choose_no_section (pjar)
      else
           obj%reading_data_section = .true.
      end if

      call fio_status (obj,err,msg,'reading data section')
      return
      end subroutine fio_read_data_section


!!---------------------------- write data section ---------------------------!!
!!---------------------------- write data section ---------------------------!!
!!---------------------------- write data section ---------------------------!!

! Finds the requested (existing) header section and makes it active.
! Gathers the information necessary to write the data section.
! Registers an error if there is no match.
! Registers an error if any required information is invalid or missing.
! Inactivates the header section if there is no match or there is an error.
! Positions the file at the beginning of the data section.


      subroutine fio_write_data_section (obj,pjar,secname,err,msg)
      implicit none
      type(fio_struct) ,intent(inout) :: obj              ! arguments
      type(pjar_struct),intent(inout) :: pjar             ! arguments
      character(len=*) ,intent(in)    :: secname          ! arguments
      integer          ,intent(out)   :: err              ! arguments
      character(len=*) ,intent(out)   :: msg              ! arguments

!----------get started.

      call fio_private_finish_tasks (obj)
      call fio_private_initialize   (obj)
      call pjar_choose_no_section   (pjar)

      if (obj%whoops) then
           call fio_status (obj,err,msg,'writing data section')
           return
      end if

!----------prepare to write data section.

      call fio_private_prepare (obj,pjar,secname)

      if (obj%whoops) then
           call fio_private_initialize (obj)
           call pjar_choose_no_section (pjar)
           call fio_status (obj,err,msg,'writing data section')
           return
      end if

      call fio_write_data_tag (obj, obj%secname)
      call dio_tell           (obj%dio, obj%address)
      call fio_rewrite_array  (obj,'address',obj%tell_address,obj%address,2)
      call pjar_put           (pjar,'address',obj%address,2)
      call dio_status         (obj%dio,msg)
      call fio_register_error (obj,msg)

!----------finish up and return.

      if (obj%whoops) then
           call fio_private_initialize (obj)
           call pjar_choose_no_section (pjar)
      else
           obj%writing_data_section = .true.
      end if

      call fio_status (obj,err,msg,'writing data section')
      return
      end subroutine fio_write_data_section


!!------------------------- read next data section ------------------------!!
!!------------------------- read next data section ------------------------!!
!!------------------------- read next data section ------------------------!!

! Finds the next data section on the file.
! Positions the file at the beginning of the data section.
! Then finds the corresponding (existing) header section and makes it active.
! Gathers the information necessary to read the data section.
! Registers an error if there is no match.
! Registers an error if any required information is invalid or missing.
! Inactivates the header section if there is no match or there is an error.


      subroutine fio_read_next_data_section (obj,pjar,secname,err,msg)
      implicit none
      type(fio_struct) ,intent(inout) :: obj              ! arguments
      type(pjar_struct),intent(inout) :: pjar             ! arguments
      character(len=*) ,intent(out)   :: secname          ! arguments
      integer          ,intent(out)   :: err              ! arguments
      character(len=*) ,intent(out)   :: msg              ! arguments
      character(len=LRECORD)          :: card             ! local


!----------get started.

      secname = ' '
      call fio_private_finish_tasks (obj)
      call fio_private_initialize   (obj)
      call pjar_choose_no_section   (pjar)

      if (obj%whoops) then
           call fio_status (obj,err,msg,'searching for next data')
           return
      end if

!----------find next data section.

      do
           call dio_read_card      (obj%dio,card)
           call dio_status         (obj%dio,msg)
           call fio_register_error (obj,msg,'searching for next data')
           if (obj%whoops) then
                exit
           else if (fio_is_data_tag(card)) then
                secname = fio_name_from_tag(card)
                exit
           end if
      end do

!----------prepare to read data section.

      call fio_private_prepare (obj,pjar,secname)

!----------finish up and return.

      if (obj%whoops) then
           call fio_private_initialize (obj)
           call pjar_choose_no_section (pjar)
           secname = ' '
           obj%secname = ' '
      else
           obj%reading_data_section = .true.
      end if

      call fio_status (obj,err,msg,'reading next data section')
      return
      end subroutine fio_read_next_data_section


!!------------------------ private finish tasks ----------------------------!!
!!------------------------ private finish tasks ----------------------------!!
!!------------------------ private finish tasks ----------------------------!!


      subroutine fio_private_finish_read_data (obj)
      implicit none
      type(fio_struct),intent(inout)       :: obj                ! arguments

      if (obj%whoops) then
           if(obj%errmsg(1:3) == 'EOS') obj%whoops = .false.
           if(obj%errmsg(1:3) == 'EOF') obj%whoops = .false.
      end if
      return
      end subroutine fio_private_finish_read_data


                          !!!!!!!!!!!!!!!!!!!


      subroutine fio_private_finish_write_data (obj)
      implicit none
      type(fio_struct),intent(inout) :: obj              ! arguments

      if (obj%whoops) return

   call fio_rewrite_scalar(obj,'nlines'  ,obj%tell_nlines  ,obj%kount_lines)
   call fio_rewrite_scalar(obj,'npackets',obj%tell_npackets,obj%kount_packets)
   call fio_rewrite_scalar(obj,'maxpicks',obj%tell_maxpicks,obj%maxpicks)

      if (obj%is_ascii) then
           if (.not.obj%noheaders) call fio_private_write_wrapped_line &
                   (obj,obj%fields,obj%ncolumns,COMMENT,obj%kount_lines,0)
      else
           call dio_write_scalar  (obj%dio, BINARY_EOS)
           call dio_write_newline (obj%dio)
      end if

      call fio_write_end_tag  (obj,obj%secname)
      return
      end subroutine fio_private_finish_write_data


                          !!!!!!!!!!!!!!!!!!!


      subroutine fio_private_finish_tasks (obj)
      implicit none
      type(fio_struct),intent(inout) :: obj             ! arguments

      if (obj%writing_data_section) then
           call fio_private_finish_write_data (obj)
           obj%writing_data_section = .false.
      end if

      if (obj%reading_data_section) then
           call fio_private_finish_read_data (obj)
           obj%reading_data_section = .false.
      end if
      return
      end subroutine fio_private_finish_tasks


!!------------------- private rewrite parameter ----------------------------!!
!!------------------- private rewrite parameter ----------------------------!!
!!------------------- private rewrite parameter ----------------------------!!


      subroutine fio_rewrite_scalar (obj,keyword,address,scalar)
      implicit none
      type(fio_struct) ,intent(inout) :: obj                    ! arguments
      character(len=*) ,intent(in)    :: keyword                ! arguments
      integer          ,intent(in)    :: address(2)             ! arguments
      integer          ,intent(in)    :: scalar                 ! arguments
      integer                         :: here(2)                ! local
      integer                         :: length ! local
      character(len=FIXLENGTH)        :: card                   ! local
      character(len=LMSG)             :: msg                    ! local

      if (obj%noheaders) return
      if (obj%whoops) return
      if (address(1) == 0 .and. address(2) == 0) then
           call fio_register_error (obj,'zero address rewriting '//keyword)
           return
      end if

      call dio_tell           (obj%dio,here)
      call dio_seek           (obj%dio,address)
      call string_to_upper    (keyword,card)

      length = max(len_trim(card),10)
      card = COMMENT//card(1:length)//' = '//string_ii2ss(scalar)

      call dio_write_card     (obj%dio,card,FIXLENGTH)
      call dio_seek           (obj%dio,here)
      call dio_status         (obj%dio,msg)
      call fio_register_error (obj,msg,'error rewriting '//keyword)
      return
      end subroutine fio_rewrite_scalar



      subroutine fio_rewrite_array (obj,keyword,address,array,nelements)
      implicit none
      type(fio_struct) ,intent(inout) :: obj                    ! arguments
      character(len=*) ,intent(in)    :: keyword                ! arguments
      integer          ,intent(in)    :: address(2)             ! arguments
      integer          ,intent(in)    :: array(:)               ! arguments
      integer          ,intent(in)    :: nelements              ! arguments
      integer                         :: here(2)                ! local
      integer                         :: length       ,indx ! local
      character(len=FIXLENGTH)        :: card                   ! local
      character(len=LMSG)             :: msg                    ! local

      if (obj%noheaders) return
      if (obj%whoops) return
      if (address(1) == 0 .and. address(2) == 0) then
           call fio_register_error (obj,'zero address rewriting '//keyword)
           return
      end if

      call dio_tell           (obj%dio,here)
      call dio_seek           (obj%dio,address)
      call string_to_upper    (keyword,card)

      length = max(len_trim(card),10)
      card = COMMENT//card(1:length)//' = ('
      do indx = 1,nelements
           if (indx == 1) then
                card = trim(card)//string_ii2ss(array(indx))
           else
                card = trim(card)//', '//string_ii2ss(array(indx))
           end if
      end do
      card = trim(card)//')'

      call dio_write_card     (obj%dio,card,FIXLENGTH)
      call dio_seek           (obj%dio,here)
      call dio_status         (obj%dio,msg)
      call fio_register_error (obj,msg,'error rewriting '//keyword)
      return
      end subroutine fio_rewrite_array


!!------------------------- read header sections ---------------------------!!
!!------------------------- read header sections ---------------------------!!
!!------------------------- read header sections ---------------------------!!
 
 
      subroutine fio_read_header_sections (obj,pjar,err,msg,filetype)
      implicit none
      type(fio_struct) ,intent(inout)        :: obj            ! arguments
      type(pjar_struct),intent(inout)        :: pjar           ! arguments
      integer          ,intent(out)          :: err            ! arguments
      character(len=*) ,intent(out)          :: msg            ! arguments
      character(len=*) ,intent(out),optional :: filetype       ! arguments
      character(len=LRECORD)                 :: card           ! local
      logical                                :: adding         ! local

!----------get started:

      call fio_read_filetype (obj,filetype)
      call pjar_clear        (pjar)

!----------loop through header sections.

      adding = .false.
      do
           call dio_read_card      (obj%dio,card)
           call dio_status         (obj%dio,err,msg)
           call fio_register_error (obj,msg,'reading header sections')
           if (err == DIO_EOF) then
                call dio_backspace (obj%dio)
                exit
           else if (obj%whoops) then
                exit
           else if (card == ' ') then
                cycle
           else if (card(1:1) /= COMMENT) then
                call dio_backspace (obj%dio)
                exit
           else if (fio_is_data_tag(card)) then
                call dio_backspace (obj%dio)
                exit
           end if

           if (fio_is_header_tag(card)) then
                obj%secname = fio_name_from_tag (card)
                call pjar_choose_section (pjar,obj%secname)
                adding = .true.
           else if (fio_is_end_tag(card)) then
                adding = .false.
           else if (adding) then
                call pjar_add_card (pjar,card(2:))
           end if
      end do

!----------finish up and return:

      if (pjar_num_sections(pjar) == 0) then
            call pjar_choose_section (pjar, 'foreign')
            call pjar_put            (pjar, "encoding" , FIO_ASCII)
            call pjar_put            (pjar, "nilstring", 'nil')
            call pjar_put            (pjar, "wrap"     , 1)
            call pjar_put            (pjar, "ncolumns" , 0)
            call pjar_put            (pjar, "firstline", 1)
      end if

      call pjar_choose_no_section  (pjar)
      call dio_status              (obj%dio,msg)
      call fio_register_error      (obj,msg,'header sections')
      call pjar_status             (pjar,msg)
      call fio_register_error      (obj,msg,'header sections')
      call fio_status              (obj,err,msg,'header sections read')
      obj%secname = ' '
      return
      end subroutine fio_read_header_sections


!!------------------------- write header sections ---------------------------!!
!!------------------------- write header sections ---------------------------!!
!!------------------------- write header sections ---------------------------!!


      subroutine fio_write_header_sections (obj,pjar,err,msg,filetype)
      implicit none
      type(fio_struct) ,intent(inout)       :: obj                 ! arguments
      type(pjar_struct),intent(inout)       :: pjar                ! arguments
      integer          ,intent(out)         :: err                 ! arguments
      character(len=*) ,intent(out)         :: msg                 ! arguments
      character(len=*) ,intent(in),optional :: filetype            ! arguments
      integer                               :: tell_address (2)    ! local
      integer                               :: tell_nlines  (2)    ! local
      integer                               :: tell_npackets(2)    ! local
      integer                               :: tell_maxpicks(2)    ! local
      integer                               :: indx,ncards,length  ! local
      integer                               :: nsections,isection  ! local
      character(len=LMSG)                   :: card                ! local
      character(len=LCHAR)                  :: keyword             ! local
      character(len=LCHAR)                  :: encoding            ! local
      logical                               :: noheaders           ! local

!----------get started:

      call fioutil_augment_all     (pjar)
      call pjar_status             (pjar,msg)
      call fio_register_error      (obj,msg,'header sections')

      if (obj%whoops) then
           call pjar_choose_no_section (pjar)
           call fio_status             (obj,err,msg)
           obj%secname = ' '
           return
      end if

      nsections = pjar_num_sections (pjar)

!----------find out whether to eliminate header sections:
!          the noheaders flag can reside in any ascii header section.
!          headers are always written if there is any binary or hybrid section.

      obj%noheaders = .false.

      do isection = 1,nsections
           call pjar_choose_section  (pjar,isection)
           call pjar_get             (pjar,'encoding',encoding)
           if (encoding == FIO_ASCII) then
               call pjar_get (pjar,'noheaders',noheaders)
               if (noheaders) obj%noheaders = .true.
           else if (encoding == FIO_BINARY .or. encoding == FIO_HYBRID) then
               obj%noheaders = .false.
               exit
           end if
      end do

      if (obj%noheaders) then
           call pjar_status            (pjar,msg)
           call fio_register_error     (obj,msg,'header sections')
           call pjar_choose_no_section (pjar)
           call fio_status             (obj,err,msg)
           obj%secname = ' '
           return
      end if

!----------write filetype:

      call fio_write_filetype      (obj,filetype)

      if (obj%whoops) then
           call pjar_choose_no_section (pjar)
           call fio_status             (obj,err,msg)
           obj%secname = ' '
      end if

!----------loop through header sections.

      do isection = 1,nsections
           call pjar_choose_section  (pjar,isection)
           call pjar_get             (pjar,'encoding',encoding)
           call pjar_get_secname     (pjar,obj%secname)
           call pjar_status          (pjar,msg)
           call fio_register_error   (obj,msg,'header section')
           call fio_write_header_tag (obj,obj%secname)
           if (obj%whoops) exit

           tell_address (:) = 0
           tell_nlines  (:) = 0
           tell_npackets(:) = 0
           tell_maxpicks(:) = 0
           ncards           = pjar_num_cards (pjar)

!----------write the data cards to the file.

           do indx = 1,ncards
                call pjar_get_card (pjar,indx,card)

                if (encoding == FIO_ASCII  .or. &
                    encoding == FIO_BINARY .or. &
                    encoding == FIO_HYBRID) then
                  length  = index(card,'=')
                  keyword = string_2_lower(card(1:length-1))

                  select case (keyword)
                     case ('address' ) ; call dio_tell (obj%dio,tell_address)
                     case ('nlines'  ) ; call dio_tell (obj%dio,tell_nlines)
                     case ('npackets') ; call dio_tell (obj%dio,tell_npackets)
                     case ('maxpicks') ; call dio_tell (obj%dio,tell_maxpicks)
                     case ('tell_address'     ) ; cycle
                     case ('tell_nlines'      ) ; cycle
                     case ('tell_npackets'    ) ; cycle
                     case ('tell_maxpicks'    ) ; cycle
                  end select
                end if

                card   = COMMENT//card
                length = max(len_trim(card),FIXLENGTH)
                call dio_write_card     (obj%dio,card,length)
                call dio_status         (obj%dio,msg)
                call fio_register_error (obj,msg,'header sections')
                if (obj%whoops) exit
           end do

!----------save byte addresses of some parameters on the file.

           if (encoding == FIO_ASCII  .or. &
               encoding == FIO_BINARY .or. &
               encoding == FIO_HYBRID) then
                call pjar_put (pjar,'tell_address' ,tell_address ,2)
                call pjar_put (pjar,'tell_nlines'  ,tell_nlines  ,2)
                call pjar_put (pjar,'tell_npackets',tell_npackets,2)
                call pjar_put (pjar,'tell_maxpicks',tell_maxpicks,2)
           end if

!----------finish looping through header sections.

           call fio_write_end_tag  (obj,obj%secname)
           call pjar_status        (pjar,msg)
           call fio_register_error (obj,msg,'header section')
           if (obj%whoops) exit
      end do

!----------finish up and return:

      call pjar_choose_no_section (pjar)
      call fio_status             (obj,err,msg,'header sections written')
      obj%secname = ' '
      return
      end subroutine fio_write_header_sections


!!----------------------------- status ------------------------------------!!
!!----------------------------- status ------------------------------------!!
!!----------------------------- status ------------------------------------!!


      subroutine fio_status (obj,err,msg,goodmsg)
      implicit none
      type(fio_struct),intent(in)           :: obj              ! arguments
      integer         ,intent(out)          :: err              ! arguments
      character(len=*),intent(out)          :: msg              ! arguments
      character(len=*),intent(in) ,optional :: goodmsg          ! arguments

      if (obj%whoops) then
           msg = obj%errmsg
           if      (msg(1:3) == 'EOF') then ; err = FIO_EOF
           else if (msg(1:3) == 'EOS') then ; err = FIO_EOF
           else                             ; err = FIO_ERROR
           end if
      else if (present(goodmsg)) then
           err = FIO_OK
           msg = goodmsg
      else
           err = FIO_OK
           msg = 'all is well'
      end if
      return
      end subroutine fio_status


!!----------------------- fio register error --------------------------!!
!!----------------------- fio register error --------------------------!!
!!----------------------- fio register error --------------------------!!

! optional argument PHRASE is deliberately not documented.


      subroutine fio_register_error (obj,msg,phrase)
      implicit none
      type(fio_struct),intent(inout)       :: obj                ! arguments
      character(len=*),intent(in)          :: msg                ! arguments
      character(len=*),intent(in),optional :: phrase             ! arguments
      character(len=LCHAR)                 :: suffix             ! local


      if (msg == ' ' ) return       ! this is not an error at all.
      if (msg == 'ok') return       ! this is not an error at all.
      if (msg == 'OK') return       ! this is not an error at all.
      if (obj%whoops ) return       ! the first error message is retained.
      obj%whoops = .true.
      obj%errmsg = msg

      if (present(phrase)) then
        if (phrase /= ' ') then
          if (len_trim(obj%errmsg) + len_trim(phrase) + 3 &
                 <= len(obj%errmsg)) then
                      obj%errmsg = trim(obj%errmsg)//' ('//trim(phrase)//')'
          end if
        end if
      end if

      if (obj%secname == ' ') return

      suffix = ' (section '//obj%secname

      if (len_trim(obj%errmsg) + len_trim(suffix) >= len(obj%errmsg)) return

      obj%errmsg = trim(obj%errmsg)//suffix

      if (obj%kount_lines == 0) then
           suffix = ')'
      else if (obj%is_ascii) then
           suffix = ' line '//trim(string_ii2ss(obj%kount_lines))//')'
      else if (obj%is_hybrid) then
           suffix = ' line '//trim(string_ii2ss(obj%kount_lines))//')'
      else if (obj%is_binary) then
           suffix = ' packet '//trim(string_ii2ss(obj%kount_packets))//')'
      else
           suffix = ')'
      end if

      if (len_trim(obj%errmsg) + len_trim(suffix) >= len(obj%errmsg)) then
           suffix = ')'
      end if

      obj%errmsg = trim(obj%errmsg)//suffix
      return
      end subroutine fio_register_error


!!----------------------- fio private convert ---------------------------!!
!!----------------------- fio private convert ---------------------------!!
!!----------------------- fio private convert ---------------------------!!


      subroutine fio_private_convert_ivar (obj,column,value)
      implicit none
      type(fio_struct),intent(in)          :: obj            ! arguments
      integer         ,intent(in)          :: column         ! arguments
      integer         ,intent(inout)       :: value          ! arguments

      if (value == INIL .or. value == 0) return

      select case (obj%converters(column))
           case ('1000' ) ; value = 1000 * value
      end select
      return
      end subroutine fio_private_convert_ivar



      subroutine fio_private_convert_fvar (obj,column,value)
      implicit none
      type(fio_struct),intent(in)          :: obj            ! arguments
      integer         ,intent(in)          :: column         ! arguments
      real            ,intent(inout)       :: value          ! arguments

      if (value == FNIL .or. value == 0.0) return

      select case (obj%converters(column))
           case ('recip') ; value = 1.0 / value
           case ('1000' ) ; value = 1000.0 * value
           case ('0.001') ; value = 0.001 * value
      end select
      return
      end subroutine fio_private_convert_fvar



      subroutine fio_private_convert_dvar (obj,column,value)
      implicit none
      type(fio_struct),intent(in)          :: obj            ! arguments
      integer         ,intent(in)          :: column         ! arguments
      double precision,intent(inout)       :: value          ! arguments

      if (value == DNIL .or. value == 0.0) return

      select case (obj%converters(column))
           case ('recip') ; value = 1.0 / value
           case ('1000' ) ; value = 1000.0 * value
           case ('0.001') ; value = 0.001 * value
      end select
      return
      end subroutine fio_private_convert_dvar


!!-------------------------- fio read line --------------------------------!!
!!-------------------------- fio read line --------------------------------!!
!!-------------------------- fio read line --------------------------------!!


      subroutine fio_read_line_cvar (obj,column,value)
      implicit none
      type(fio_struct),intent(inout)        :: obj            ! arguments
      integer         ,intent(in)           :: column         ! arguments
      character(len=*),intent(out)          :: value          ! arguments

      if (obj%end_packet) return
      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           value = CNIL
      else
           value = obj%cdefaults(column)
           call string_replace_character (value,'|',' ')
      end if
      return
      end subroutine fio_read_line_cvar



      subroutine fio_read_line_ivar (obj,column,value)
      implicit none
      type(fio_struct),intent(inout)        :: obj            ! arguments
      integer         ,intent(in)           :: column         ! arguments
      integer         ,intent(out)          :: value          ! arguments
      integer                               :: error          ! local

      if (obj%end_packet) return
      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           value = INIL
      else if (obj%is_hybrid .or. column > obj%ntokens) then
           value = obj%idefaults(column)
      else if (obj%skip(column)) then
           value = obj%idefaults(column)
      else if (obj%cdefaults(column) == ' ') then
           value = INIL
      else
           read (obj%cdefaults(column),*,iostat = error) value
           if (error /= 0) then
                value = INIL
                call fio_register_error (obj, &
                  'error decoding '//trim(obj%cdefaults(column))//' to integer')
           end if
           obj%idefaults(column) = value
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      call fio_private_convert_ivar (obj,column,value)
      return
      end subroutine fio_read_line_ivar



      subroutine fio_read_line_fvar (obj,column,value)
      implicit none
      type(fio_struct),intent(inout)        :: obj            ! arguments
      integer         ,intent(in)           :: column         ! arguments
      real            ,intent(out)          :: value          ! arguments
      integer                               :: error          ! local

      if (obj%end_packet) return
      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           value = FNIL
      else if (obj%is_hybrid .or. column > obj%ntokens) then
           value = obj%fdefaults(column)
      else if (obj%skip(column)) then
           value = obj%fdefaults(column)
      else if (obj%cdefaults(column) == ' ') then
           value = FNIL
      else
           read (obj%cdefaults(column),*,iostat = error) value
           if (error /= 0) then
                value = FNIL
                call fio_register_error (obj, &
                  'error decoding '//trim(obj%cdefaults(column))//' to real')
           end if
           obj%fdefaults(column) = value
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      call fio_private_convert_fvar (obj,column,value)
      return
      end subroutine fio_read_line_fvar



      subroutine fio_read_line_dvar (obj,column,value)
      implicit none
      type(fio_struct),intent(inout)        :: obj            ! arguments
      integer         ,intent(in)           :: column         ! arguments
      double precision,intent(out)          :: value          ! arguments
      integer                               :: error          ! local

      if (obj%end_packet) return
      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           value = DNIL
      else if (obj%is_hybrid .or. column > obj%ntokens) then
           value = obj%ddefaults(column)
      else if (obj%skip(column)) then
           value = obj%ddefaults(column)
      else if (obj%cdefaults(column) == ' ') then
           value = DNIL
      else
           read (obj%cdefaults(column),*,iostat = error) value
           if (error /= 0) then
                value = DNIL
                call fio_register_error (obj, &
                  'error decoding '//trim(obj%cdefaults(column))//' to double')
           end if
           obj%ddefaults(column) = value
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      call fio_private_convert_dvar (obj,column,value)
      return
      end subroutine fio_read_line_dvar



      subroutine fio_read_line_lvar (obj,column,value)
      implicit none
      type(fio_struct),intent(inout)        :: obj            ! arguments
      integer         ,intent(in)           :: column         ! arguments
      logical         ,intent(out)          :: value          ! arguments
      character(len=LMSG)                   :: errmsg         ! local
      integer                               :: stat           ! local

      if (obj%end_packet) return
      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           value = LNIL
      else if (obj%is_hybrid .or. column > obj%ntokens) then
           value = obj%ldefaults(column)
      else if (obj%skip(column)) then
           value = obj%ldefaults(column)
      else
           value = string_ss2ll(obj%cdefaults(column),stat,errmsg)
           if (stat < 0) call fio_register_error (obj,errmsg)
           obj%ldefaults(column) = value
      end if
      return
      end subroutine fio_read_line_lvar


!!-------------------------- fio write line -------------------------------!!
!!-------------------------- fio write line -------------------------------!!
!!-------------------------- fio write line -------------------------------!!


      subroutine fio_write_line_cvar (obj, column, value)
      implicit none
      type(fio_struct),intent(inout)       :: obj             ! arguments
      integer         ,intent(in)          :: column          ! arguments
      character(len=*),intent(in)          :: value           ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (value == obj%cdefaults(column))    return
      if (obj%delimiters(column))            obj%end_packet = .true.
                                             obj%cdefaults(column) = value

      call string_replace_character (obj%cdefaults(column),' ','|')
      return
      end subroutine fio_write_line_cvar



      subroutine fio_write_line_ivar (obj, column, value, nchar)
      implicit none
      type(fio_struct),intent(inout)       :: obj             ! arguments
      integer         ,intent(in)          :: column          ! arguments
      integer         ,intent(in)          :: value           ! arguments
      integer         ,intent(in),optional :: nchar           ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (value == obj%idefaults(column))    return
      if (obj%delimiters(column))            obj%end_packet = .true.
                                             obj%idefaults(column) = value
      if (obj%is_ascii) then
           obj%cdefaults(column) = string_ii2ss(value,nchar)
      end if
      return
      end subroutine fio_write_line_ivar



      subroutine fio_write_line_fvar (obj, column, value, nchar, ndec)
      implicit none
      type(fio_struct),intent(inout)       :: obj             ! arguments
      integer         ,intent(in)          :: column          ! arguments
      real            ,intent(in)          :: value           ! arguments
      integer         ,intent(in),optional :: nchar,ndec      ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (value == obj%fdefaults(column))    return
      if (obj%delimiters(column))            obj%end_packet = .true.
                                             obj%fdefaults(column) = value
      if (obj%is_ascii) then
           obj%cdefaults(column) = string_ff2ss(value,nchar,ndec)
      end if
      return
      end subroutine fio_write_line_fvar



      subroutine fio_write_line_dvar (obj, column, value, nchar, ndec)
      implicit none
      type(fio_struct),intent(inout)       :: obj             ! arguments
      integer         ,intent(in)          :: column          ! arguments
      double precision,intent(in)          :: value           ! arguments
      integer         ,intent(in),optional :: nchar,ndec      ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (value == obj%ddefaults(column))    return
      if (obj%delimiters(column))            obj%end_packet = .true.
                                             obj%ddefaults(column) = value
      if (obj%is_ascii) then
           obj%cdefaults(column) = string_dd2ss(value,nchar,ndec)
      end if
      return
      end subroutine fio_write_line_dvar



      subroutine fio_write_line_lvar (obj, column, value)
      implicit none
      type(fio_struct),intent(inout)       :: obj             ! arguments
      integer         ,intent(in)          :: column          ! arguments
      logical         ,intent(in)          :: value           ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (value .eqv. obj%ldefaults(column)) return
      if (obj%delimiters(column))            obj%end_packet = .true.
                                             obj%ldefaults(column) = value
      if (obj%is_ascii) then
           obj%cdefaults(column) = string_ll2ss(value)
      end if
      return
      end subroutine fio_write_line_lvar


!!-------------------- before and after read binary ------------------------!!
!!-------------------- before and after read binary ------------------------!!
!!-------------------- before and after read binary ------------------------!!


      subroutine fio_before_read_binary (obj,npicks)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: npicks              ! arguments

      if (.not.obj%is_binary) then
           call fio_register_error (obj,'calling fio_before_read_binary', &
                                    'when encoding is not binary')
      end if

      call dio_read_scalar (obj%dio,npicks)
      call dio_read_array  (obj%dio,obj%next_address ,2)
      call dio_read_array  (obj%dio,obj%addresses    ,2*obj%ncolumns)

      if (npicks == BINARY_EOS) then
           call fio_register_error (obj,'EOS (end of section) encountered')
           npicks = 0
      else if (npicks < 0 .or. npicks > obj%maxpicks) then
           call fio_register_error (obj,'bad NPICKS = '// &
                         trim(string_ii2ss(npicks))//' read from file')
           npicks = 0
      end if

      obj%kount_packets = obj%kount_packets + 1
      obj%npicks        = npicks
      return
      end subroutine fio_before_read_binary


                            !!!!!!!!!!!!!!!!!!!!!


      subroutine fio_after_read_binary (obj,err,msg)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: err                 ! arguments
      character(len=*),intent(out)   :: msg                 ! arguments

 !!!  call dio_read_newline      (obj%dio)                     ! not needed.
      call dio_seek              (obj%dio,obj%next_address)
      call dio_status            (obj%dio,msg)
      call fio_register_error    (obj,msg,'reading binary data packet')
      call fio_status            (obj,err,msg,'binary data successfully read')
      return
      end subroutine fio_after_read_binary


!!----------------- before and after write binary ---------------------!!
!!----------------- before and after write binary ---------------------!!
!!----------------- before and after write binary ---------------------!!


      subroutine fio_before_write_binary (obj,npicks)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: npicks              ! arguments

      if (.not.obj%is_binary) then
           call fio_register_error (obj,'calling fio_before_write_binary', &
                                    'when encoding is not binary')
      end if

      obj%kount_lines             = obj%kount_lines + npicks
      obj%kount_packets           = obj%kount_packets + 1
      obj%npicks                  = npicks
      obj%maxpicks                = max(obj%maxpicks,npicks)
      obj%written(1:obj%ncolumns) = .false.

      call dio_tell         (obj%dio,obj%this_address)
      call dio_write_scalar (obj%dio,npicks)
      call dio_write_array  (obj%dio,obj%next_address ,2)               ! zero.
      call dio_write_array  (obj%dio,obj%addresses    ,2*obj%ncolumns)  ! zero.
      return
      end subroutine fio_before_write_binary


                            !!!!!!!!!!!!!!!!!!!!!


      subroutine fio_after_write_binary (obj,err,msg)
      implicit none
      type(fio_struct)    ,intent(inout) :: obj                 ! arguments
      integer             ,intent(out)   :: err                 ! arguments
      character(len=*)    ,intent(out)   :: msg                 ! arguments
      character(len=LCHAR),allocatable   :: carray(:)           ! local
      integer             ,allocatable   :: iarray(:)           ! local
      real                ,allocatable   :: farray(:)           ! local
      double precision    ,allocatable   :: darray(:)           ! local
      logical             ,allocatable   :: larray(:)           ! local
      integer                            :: column              ! local

      call dio_write_newline     (obj%dio)
      call dio_tell              (obj%dio,obj%next_address)
      call dio_seek              (obj%dio,obj%this_address)
      call dio_write_scalar      (obj%dio,obj%npicks)
      call dio_write_array       (obj%dio,obj%next_address ,2)
      call dio_write_array       (obj%dio,obj%addresses    ,2*obj%ncolumns)
      call dio_seek              (obj%dio,obj%next_address)
      call dio_status            (obj%dio,msg)
      call fio_register_error    (obj,msg,'writing binary data packet')

      do column = 1,obj%ncolumns
        if (.not.obj%written(column)) then
          if (obj%delimiters(column)) then
            select case (obj%vartypes(column))
             case('C'); call fio_write_binary (obj,column,obj%cdefaults(column))
             case('I'); call fio_write_binary (obj,column,obj%idefaults(column))
             case('F'); call fio_write_binary (obj,column,obj%fdefaults(column))
             case('D'); call fio_write_binary (obj,column,obj%ddefaults(column))
             case('L'); call fio_write_binary (obj,column,obj%ldefaults(column))
             case default; call fio_register_error &
                              (obj,'bad variable type '//obj%vartypes(column))
            end select
          else
            select case (obj%vartypes(column))
             case('C'); allocate (carray(obj%npicks))
                        carray(:) = obj%cdefaults(column)
                        call fio_write_binary (obj,column,carray)
                        deallocate (carray)
             case('I'); allocate (iarray(obj%npicks))
                        iarray(:) = obj%idefaults(column)
                        call fio_write_binary (obj,column,iarray)
                        deallocate (iarray)
             case('F'); allocate (farray(obj%npicks))
                        farray(:) = obj%fdefaults(column)
                        call fio_write_binary (obj,column,farray)
                        deallocate (farray)
             case('D'); allocate (darray(obj%npicks))
                        darray(:) = obj%ddefaults(column)
                        call fio_write_binary (obj,column,darray)
                        deallocate (darray)
             case('L'); allocate (larray(obj%npicks))
                        larray(:) = obj%ldefaults(column)
                        call fio_write_binary (obj,column,larray)
                        deallocate (larray)
             case default; call fio_register_error &
                             (obj,'bad variable type '//obj%vartypes(column))
            end select
          end if
        end if
      end do

      if (any(obj%written(1:obj%ncolumns) .eqv. .false.)) then
           call fio_register_error (obj,'some columns not written')
      end if

      call fio_status (obj,err,msg,'binary data successfully written')
      return
      end subroutine fio_after_write_binary


!!----------------------- private fixup card ----------------------------!!
!!----------------------- private fixup card ----------------------------!!
!!----------------------- private fixup card ----------------------------!!

! called only if obj%use_template is true.
! this routine never sets an error message.


      subroutine fio_private_fixup_card (obj,card)
      implicit none
      type(fio_struct),intent(in)    :: obj                     ! arguments
      character(len=*),intent(inout) :: card                    ! arguments
      integer                        :: indx,length1,length2    ! local

      length1 = len_trim(card)
      length2 = len_trim(obj%template)
      do indx = 1,length1
          if (obj%template(indx:indx) == '-') card(indx:indx) = ' '
      end do
      do indx = length2,2,-1
          if (obj%template(indx:indx) /= ' ') then
              if (indx <= length1) then
                   card = card(1:indx-1)//' '//card(indx:)
              end if
          end if
      end do
      return
      end subroutine fio_private_fixup_card


!!----------------------- private fixup tokens ----------------------------!!
!!----------------------- private fixup tokens ----------------------------!!
!!----------------------- private fixup tokens ----------------------------!!

! called only if obj%use_maxchars is true.
! this routine never sets an error message.


      subroutine fio_private_fixup_tokens (obj,newtokens,ntokens)
      implicit none
      type(fio_struct),intent(in)    :: obj                     ! arguments
      character(len=*),intent(inout) :: newtokens(:)            ! arguments
      integer         ,intent(inout) :: ntokens                 ! arguments
      integer                        :: column,nchar            ! local

      do column = 1,obj%nmaxchars
           nchar = obj%maxchars(column)
           if (nchar == 0) then
                cycle
           else if (nchar < 0) then
                newtokens(column)             = newtokens(column)(1:-nchar)
           else if (ntokens < MAXCOLUMNS) then
                newtokens(column+1:ntokens+1) = newtokens(column:ntokens)
                newtokens(column)             = newtokens(column)  (1:nchar)
                newtokens(column+1)           = newtokens(column+1)(nchar+1:)
                ntokens = ntokens + 1
           end if
      end do
      do column = 1,ntokens
           if (newtokens(column) == obj%nilstring) newtokens(column) = ' '
      end do
      return
      end subroutine fio_private_fixup_tokens


!!---------------------- before and after read line -----------------------!!
!!---------------------- before and after read line -----------------------!!
!!---------------------- before and after read line -----------------------!!


      subroutine fio_before_read_line (obj)
      implicit none
      type(fio_struct),intent(inout) :: obj                       ! arguments
      integer                        :: iwrap,column,err          ! local
      character(len=LRECORD)         :: card                      ! local
      character(len=LMSG)            :: msg                       ! local
      character(len=LCOMBINED)       :: combined                  ! local
      character(len=LCHAR)           :: newtokens(MAXCOLUMNS)     ! local
      integer                        :: ncolumns_write            ! local
      logical                        :: delimiting                ! local
      integer                        :: length                    ! local
      character(len=LCHAR)           :: cvalue                    ! local
      integer                        :: ivalue                    ! local
      real                           :: fvalue                    ! local
      double precision               :: dvalue                    ! local
      logical                        :: lvalue                    ! local
      character(len=1),parameter     :: tab = char(9)             ! local

!----------get started.

      if (obj%is_binary) then
           call fio_register_error (obj,'calling fio_before_read_line', &
                                    'when encoding is not ascii or hybrid')
      end if

      if (obj%end_packet) then
           obj%end_packet    = .false.
           obj%kount_packets = obj%kount_packets + 1
           !!!! the tokens previously obtained will now be used.
           return
      end if

      if (obj%whoops) then
           obj%cdefaults(:) = CNIL
           obj%idefaults(:) = INIL
           obj%fdefaults(:) = FNIL
           obj%ddefaults(:) = DNIL
           obj%ldefaults(:) = LNIL
           return
      end if

      obj%kount_lines = obj%kount_lines + 1

      delimiting = (obj%kount_lines > 1 .and. obj%delimiting)

!----------read binary records.

      if (obj%is_hybrid) then
           call dio_read_scalar (obj%dio,ncolumns_write)
           if (ncolumns_write == BINARY_EOS) then
                continue
           else if (ncolumns_write < 0 .or. ncolumns_write > obj%ncolumns) then
                call fio_register_error (obj,'bad NCOLUMNS = '// &
                        trim(string_ii2ss(ncolumns_write))//' read from file')
                ncolumns_write = BAD_NCOLUMNS
           end if

           do column = 1,ncolumns_write
             select case (obj%vartypes(column))
               case('C'); call dio_read_string (obj%dio,cvalue)
                          if (cvalue /= obj%cdefaults(column)) then
                               if (delimiting .and. obj%delimiters(column)) &
                                            obj%end_packet = .true.
                               obj%cdefaults(column) = cvalue
                          end if
               case('I'); call dio_read_scalar (obj%dio,ivalue)
                          if (ivalue /= obj%idefaults(column)) then
                               if (delimiting .and. obj%delimiters(column)) &
                                            obj%end_packet = .true.
                               obj%idefaults(column) = ivalue
                          end if
               case('F'); call dio_read_scalar (obj%dio,fvalue)
                          if (fvalue /= obj%fdefaults(column)) then
                               if (delimiting .and. obj%delimiters(column)) &
                                            obj%end_packet = .true.
                               obj%fdefaults(column) = fvalue
                          end if
               case('D'); call dio_read_scalar (obj%dio,dvalue)
                          if (dvalue /= obj%ddefaults(column)) then
                               if (delimiting .and. obj%delimiters(column)) &
                                            obj%end_packet = .true.
                               obj%ddefaults(column) = dvalue
                          end if
               case('L'); call dio_read_scalar (obj%dio,lvalue)
                          if (lvalue .neqv. obj%ldefaults(column)) then
                               if (delimiting .and. obj%delimiters(column)) &
                                            obj%end_packet = .true.
                               obj%ldefaults(column) = lvalue
                          end if
               case default; call fio_register_error &
                               (obj,'bad variable type '//obj%vartypes(column))
             end select
           end do

  !!!!     call dio_read_newline   (obj%dio)     ! doubles the file i/o time!
           call dio_status         (obj%dio,msg)
           call fio_register_error (obj,msg,'reading hybrid line')

           if (ncolumns_write == BINARY_EOS) then
                call fio_register_error (obj,'EOS (end of section) encountered')
           end if

           if (obj%whoops) then
                obj%cdefaults(:) = CNIL
                obj%idefaults(:) = INIL
                obj%fdefaults(:) = FNIL
                obj%ddefaults(:) = DNIL
                obj%ldefaults(:) = LNIL
                if (delimiting) obj%end_packet = .true.
           end if
           return
      end if

!----------read through ascii cards and get combined record.

      if (obj%wrapping) then
           iwrap    = 0
           combined = ' '
      end if

      do
           call dio_read_card (obj%dio,card)
           call dio_status    (obj%dio,err)
           if (err /= DIO_OK) then
                call dio_status         (obj%dio,msg)
                call fio_register_error (obj,msg)
           end if

           if (obj%whoops) exit            ! error or end of file.
           if (card == ' ') cycle

           if (card(1:1) == COMMENT) then
                if (fio_is_end_tag(card)) then
                     if (obj%firstline >= 1) cycle
                     call fio_register_error &
                                (obj,'EOS (end of section) encountered')
                     exit
                else if (fio_is_data_tag(card)) then
                     if (obj%firstline >= 1) cycle
                     call dio_backspace (obj%dio)
                     call fio_register_error &
                                (obj,'EOS (end of section) encountered')
                     exit
                else
                     cycle
                end if
           end if

           length = len_trim(card)
           call string_replace_character2 (card, tab, ' ',length)   ! tab
           call string_replace_character2 (card, ',', ' ',length)   ! comma

           if (obj%wrapping) then
                iwrap = iwrap + 1
                if (iwrap == 1) then
                     if (obj%use_template) then
                          call fio_private_fixup_card (obj,card)
                     end if
                     combined = card
                else
                     combined = trim(combined)//' '//card
                end if
                if (iwrap >= obj%wrap) exit
           else
                if (obj%use_template) then
                     call fio_private_fixup_card (obj,card)
                     length = len_trim(card)
                                ! because might have changed and needed below.
                end if
                exit
           end if
      end do

!----------get new tokens.

      if (obj%whoops) then
           obj%ntokens = 0
      else if (obj%wrapping) then
           length = len_trim(combined)
           call string_get_tokens2 &
                        (combined,newtokens,obj%ntokens,obj%nilstring,length)
      else
           call string_get_tokens2 &
                        (card,newtokens,obj%ntokens,obj%nilstring,length)
      end if

      if (obj%ntokens == 0) then
           call fio_register_error (obj,'error getting tokens')
           obj%cdefaults(:) = CNIL
      else if (obj%use_maxchars) then
           call fio_private_fixup_tokens (obj,newtokens,obj%ntokens)
      end if

      obj%ntokens = min(obj%ntokens,obj%ncolumns)

!----------replace tokens with defaults for skipped columns.

      if (obj%skipping) then
           do column = 1,obj%ntokens
                if (obj%skip(column)) newtokens(column) = obj%cdefaults(column)
           end do
      end if

!----------check for end of data packet.

      if (delimiting) then
           do column = 1,obj%ntokens
                if (obj%delimiters(column)) then
                     if (newtokens(column) /= obj%cdefaults(column)) then
                          obj%end_packet = .true.
                          exit
                     end if
                end if
           end do
           if (obj%whoops) obj%end_packet = .true.
      end if

!--------update the tokens and return.

      obj%cdefaults(1:obj%ntokens) = newtokens(1:obj%ntokens)
      return
      end subroutine fio_before_read_line


                            !!!!!!!!!!!!!!!!!!!!!


      subroutine fio_after_read_line (obj,err,msg,end_packet)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: err                 ! arguments
      character(len=*),intent(out)   :: msg                 ! arguments
      logical,optional,intent(out)   :: end_packet          ! arguments

      call dio_status (obj%dio,err)

      if (err /= DIO_OK) then
           call dio_status (obj%dio,msg)
           if (obj%is_hybrid) then
                call fio_register_error (obj,msg,'reading hybrid line')
           else
                call fio_register_error (obj,msg,'reading ascii line')
           end if
      end if

      if (obj%is_hybrid) then
           call fio_status (obj,err,msg,'hybrid data read')
      else
           call fio_status (obj,err,msg,'ascii data read')
      end if

      if (.not.present(end_packet)) then
           obj%delimiting = .false.
           !!!! this insures that obj%delimiters(:) will not be used
           !!!! and obj%end_packet will never be set to true.
           return
      end if

      select case (err)
        case (FIO_OK)
                         end_packet = obj%end_packet
                         if (obj%end_packet) then
                              msg = 'data packet successfully read'
                         end if
        case (FIO_ERROR)
                         end_packet = .true.
                         obj%end_packet = .false.
        case (FIO_EOF)
                         end_packet = .true.
                         if (obj%end_packet) then
                              err = FIO_OK
                              msg = 'last data packet successfully read'
                         end if
      end select
      return
      end subroutine fio_after_read_line


!!---------------------- before and after write line -----------------------!!
!!---------------------- before and after write line -----------------------!!
!!---------------------- before and after write line -----------------------!!


      subroutine fio_before_write_line (obj)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments

      if (obj%is_binary) then
           call fio_register_error (obj,'calling fio_before_write_line', &
                                    'when encoding is not ascii or hybrid')
      end if

      obj%kount_lines    = obj%kount_lines + 1
      obj%kount_picks    = obj%kount_picks + 1
      obj%end_packet     = .false.
      return
      end subroutine fio_before_write_line


                            !!!!!!!!!!!!!!!!!!!!!


      subroutine fio_after_write_line (obj,err,msg)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(out)   :: err                 ! arguments
      character(len=*),intent(out)   :: msg                 ! arguments
      integer                        :: ncolumns_write      ! local
      integer                        :: number1,number2     ! local
      integer                        :: column              ! local

      if (obj%kount_lines == 1) then
           obj%kount_packets = 1
           ncolumns_write    = obj%ncolumns
           number1           = obj%kount_lines
           number2           = obj%kount_packets
      else if (obj%end_packet) then
           obj%maxpicks      = max(obj%maxpicks,obj%kount_picks-1)
           obj%kount_packets = obj%kount_packets + 1
           obj%kount_picks   = 1
           number1           = obj%kount_lines
           number2           = obj%kount_packets
           ncolumns_write    = obj%ncolumns
      else if (mod(obj%kount_picks,50) == 1) then
           ncolumns_write    = obj%ncolumns_fewer
           number1           = obj%kount_lines
           number2           = 0
      else
           ncolumns_write    = obj%ncolumns_fewer
           number1           = 0
           number2           = 0
      end if

      obj%maxpicks = max(obj%maxpicks,obj%kount_picks)

      if (obj%is_hybrid) then
           call dio_write_scalar (obj%dio,ncolumns_write)
           do column = 1,ncolumns_write
             select case (obj%vartypes(column))
               case('C'); call dio_write_string (obj%dio,obj%cdefaults(column))
               case('I'); call dio_write_scalar (obj%dio,obj%idefaults(column))
               case('F'); call dio_write_scalar (obj%dio,obj%fdefaults(column))
               case('D'); call dio_write_scalar (obj%dio,obj%ddefaults(column))
               case('L'); call dio_write_scalar (obj%dio,obj%ldefaults(column))
               case default; call fio_register_error &
                               (obj,'bad variable type '//obj%vartypes(column))
             end select
           end do
 !!!!      call dio_write_newline (obj%dio)   ! doubles the file i/o time!
      else
           if (obj%kount_lines == 1 .and. obj%noheaders) then
                call fio_private_write_wrapped_line &
                      (obj,obj%fields,obj%ncolumns,COMMENT,0,0)
           else if (number1 >= 1 .and. .not.obj%noheaders) then
                if (number1 > 1) call dio_write_card (obj%dio,COMMENT)
                call fio_private_write_wrapped_line &
                      (obj,obj%fields,obj%ncolumns,COMMENT,number1,number2)
           end if
           call fio_private_write_wrapped_line &
                             (obj,obj%cdefaults,ncolumns_write,' ',0,0)
      end if

      if (obj%is_hybrid) then
           call dio_status         (obj%dio,msg)
           call fio_register_error (obj,msg,'writing hybrid line')
           call fio_status         (obj,err,msg,'hybrid data written')
      else
           call dio_status         (obj%dio,msg)
           call fio_register_error (obj,msg,'writing ascii line')
           call fio_status         (obj,err,msg,'ascii data written')
      end if
      return
      end subroutine fio_after_write_line


!!-------------------- private write wrapped line --------------------------!!
!!-------------------- private write wrapped line --------------------------!!
!!-------------------- private write wrapped line --------------------------!!

! called only if obj%is_ascii is true.
! prefix should be COMMENT for comment card or ' ' for data card.
! this routine does not check DIO status.


      subroutine fio_private_write_wrapped_line &
                                  (obj,fields,nfields,prefix,number1,number2)
      implicit none
      type(fio_struct),intent(inout) :: obj                     ! arguments
      character(len=*),intent(in)    :: fields(:)               ! arguments
      integer         ,intent(in)    :: nfields                 ! arguments
      character(len=*),intent(in)    :: prefix                  ! arguments
      integer         ,intent(in)    :: number1,number2         ! arguments
      character(len=LRECORD)         :: card                    ! local
      integer                        :: wrapfields              ! local
      integer                        :: iwrap,istart,nwrite     ! local
      character(len=LMSG)            :: msg                     ! local

      if (.not.obj%wrapping) then
           card = prefix
           call string_put_tokens &
                    (card(3:),fields,nfields,obj%nilstring,obj%widths,msg)
           if (prefix /= COMMENT .and. msg /= ' ') then
                call fio_register_error (obj,msg,'writing line')
           end if
           if (number1 > 0) card = trim(card)//'  '//string_ii2ss(number1)
           if (number2 > 0) card = trim(card)//'  '//string_ii2ss(number2)
           call dio_write_card (obj%dio,card)
      else
           wrapfields = nfields / obj%wrap
           if (wrapfields * obj%wrap < nfields) wrapfields = wrapfields + 1
           do iwrap = 1,obj%wrap
                istart = 1 + (iwrap - 1) * wrapfields
                nwrite = min(wrapfields, nfields - istart + 1)
                if (istart > nfields .or. nwrite <= 0) exit
                card = prefix
                call string_put_tokens (card(3:),fields(istart:),nwrite, &
                                        obj%nilstring,obj%widths(istart:),msg)
                if (prefix /= COMMENT .and. msg /= ' ') then
                     call fio_register_error (obj,msg,'writing line')
                end if
                if (iwrap == obj%wrap) then
                     if (number1 > 0) &
                            card = trim(card)//'  '//string_ii2ss(number1)
                     if (number2 > 0) &
                            card = trim(card)//'  '//string_ii2ss(number2)
                end if
                call dio_write_card (obj%dio,card)
           end do
      end if
      return
      end subroutine fio_private_write_wrapped_line


!!-------------------------- fio read binary ---------------------------!!
!!-------------------------- fio read binary ---------------------------!!
!!-------------------------- fio read binary ---------------------------!!


      subroutine fio_read_binary_cvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      character(len=*),intent(out)   :: scalar              ! arguments

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           scalar = CNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           scalar = obj%cdefaults(column)

      else if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           scalar = CNIL

      else
           call dio_seek        (obj%dio,obj%addresses(2*column-1:))
           call dio_read_string (obj%dio,scalar)
      end if
      return
      end subroutine fio_read_binary_cvar



      subroutine fio_read_binary_cvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      character(len=*),intent(out)   :: array(:)            ! arguments
      integer                        :: nelements,nfill     ! local

      nfill = min(size(array),obj%npicks)

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           array(1:nfill) = CNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           array(1:nfill) = obj%cdefaults(column)

      else if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           array(1:nfill) = CNIL

      else
           call dio_seek              (obj%dio,obj%addresses(2*column-1:))
           call dio_read_strings_plus (obj%dio,array,nelements,obj%npicks)
      end if
      return
      end subroutine fio_read_binary_cvars



      subroutine fio_read_binary_ivar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      integer         ,intent(out)   :: scalar              ! arguments

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           scalar = INIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           scalar = obj%idefaults(column)

      else if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           scalar = INIL

      else
           call dio_seek        (obj%dio,obj%addresses(2*column-1:))
           call dio_read_scalar (obj%dio,scalar)
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      call fio_private_convert_ivar (obj,column,scalar)
      return
      end subroutine fio_read_binary_ivar



      subroutine fio_read_binary_ivars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                       ! arguments
      integer         ,intent(in)    :: column                    ! arguments
      integer         ,intent(out)   :: array(:)                  ! arguments
      integer                        :: nelements,nfill,ifill     ! local

      nfill = min(size(array),obj%npicks)

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           array(1:nfill) = INIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           array(1:nfill) = obj%idefaults(column)

      else if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           array(1:nfill) = INIL

      else
           call dio_seek            (obj%dio,obj%addresses(2*column-1:))
           call dio_read_array_plus (obj%dio,array,nelements,obj%npicks)
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      do ifill = 1,nfill
           call fio_private_convert_ivar (obj,column,array(ifill))
      end do
      return
      end subroutine fio_read_binary_ivars



      subroutine fio_read_binary_fvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      real            ,intent(out)   :: scalar              ! arguments

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           scalar = FNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           scalar = obj%fdefaults(column)

      else if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           scalar = FNIL

      else
           call dio_seek        (obj%dio,obj%addresses(2*column-1:))
           call dio_read_scalar (obj%dio,scalar)
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      call fio_private_convert_fvar (obj,column,scalar)
      return
      end subroutine fio_read_binary_fvar



      subroutine fio_read_binary_fvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                       ! arguments
      integer         ,intent(in)    :: column                    ! arguments
      real            ,intent(out)   :: array(:)                  ! arguments
      integer                        :: nelements,nfill,ifill     ! local

      nfill = min(size(array),obj%npicks)

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           array(1:nfill) = FNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           array(1:nfill) = obj%fdefaults(column)

      else if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           array(1:nfill) = FNIL

      else
           call dio_seek            (obj%dio,obj%addresses(2*column-1:))
           call dio_read_array_plus (obj%dio,array,nelements,obj%npicks)
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      do ifill = 1,nfill
           call fio_private_convert_fvar (obj,column,array(ifill))
      end do
      return
      end subroutine fio_read_binary_fvars



      subroutine fio_read_binary_dvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      double precision,intent(out)   :: scalar              ! arguments

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           scalar = DNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           scalar = obj%ddefaults(column)

      else if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           scalar = DNIL

      else
           call dio_seek        (obj%dio,obj%addresses(2*column-1:))
           call dio_read_scalar (obj%dio,scalar)
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      call fio_private_convert_dvar (obj,column,scalar)
      return
      end subroutine fio_read_binary_dvar



      subroutine fio_read_binary_dvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                       ! arguments
      integer         ,intent(in)    :: column                    ! arguments
      double precision,intent(out)   :: array(:)                  ! arguments
      integer                        :: nelements,nfill,ifill     ! local

      nfill = min(size(array),obj%npicks)

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           array(1:nfill) = DNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           array(1:nfill) = obj%ddefaults(column)

      else if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           array(1:nfill) = DNIL

      else
           call dio_seek            (obj%dio,obj%addresses(2*column-1:))
           call dio_read_array_plus (obj%dio,array,nelements,obj%npicks)
      end if

      if (.not.obj%converting) return
      if (obj%converters(column) == DEF_CONVERTERS) return

      do ifill = 1,nfill
           call fio_private_convert_dvar (obj,column,array(ifill))
      end do
      return
      end subroutine fio_read_binary_dvars



      subroutine fio_read_binary_lvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      logical         ,intent(out)   :: scalar              ! arguments

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           scalar = LNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           scalar = obj%ldefaults(column)

      else if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           scalar = LNIL

      else
           call dio_seek        (obj%dio,obj%addresses(2*column-1:))
           call dio_read_scalar (obj%dio,scalar)
      end if
      return
      end subroutine fio_read_binary_lvar



      subroutine fio_read_binary_lvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      logical         ,intent(out)   :: array(:)            ! arguments
      integer                        :: nelements,nfill     ! local

      nfill = min(size(array),obj%npicks)

      if (column < 1 .or. column > MAXCOLUMNS .or. obj%whoops) then
           array(1:nfill) = LNIL
           return

      else if (column > obj%ncolumns .or. obj%skip(column)) then
           array(1:nfill) = obj%ldefaults(column)

      else if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           array(1:nfill) = LNIL

      else
           call dio_seek            (obj%dio,obj%addresses(2*column-1:))
           call dio_read_array_plus (obj%dio,array,nelements,obj%npicks)
      end if
      return
      end subroutine fio_read_binary_lvars


!!------------------------ fio write binary ------------------------------!!
!!------------------------ fio write binary ------------------------------!!
!!------------------------ fio write binary ------------------------------!!


      subroutine fio_write_binary_cvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      character(len=*),intent(in)    :: scalar              ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           return
      end if

      call dio_tell         (obj%dio,obj%addresses(2*column-1:))
      call dio_write_string (obj%dio,scalar)

      obj%cdefaults(column) = scalar
      obj%written  (column) = .true.
      return
      end subroutine fio_write_binary_cvar



      subroutine fio_write_binary_cvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      character(len=*),intent(in)    :: array(:)            ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           return
      end if

      call dio_tell               (obj%dio,obj%addresses(2*column-1:))
      call dio_write_strings_plus (obj%dio,array,obj%npicks)

      if (obj%npicks > 0) obj%cdefaults(column) = array(obj%npicks)
      obj%written(column) = .true.
      return
      end subroutine fio_write_binary_cvars



      subroutine fio_write_binary_ivar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      integer         ,intent(in)    :: scalar              ! arguments


      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           return
      end if

      call dio_tell         (obj%dio,obj%addresses(2*column-1:))
      call dio_write_scalar (obj%dio,scalar)

      obj%idefaults(column) = scalar
      obj%written  (column) = .true.
      return
      end subroutine fio_write_binary_ivar



      subroutine fio_write_binary_ivars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      integer         ,intent(in)    :: array(:)            ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           return
      end if

      call dio_tell             (obj%dio,obj%addresses(2*column-1:))
      call dio_write_array_plus (obj%dio,array,obj%npicks)

      if (obj%npicks > 0) obj%idefaults(column) = array(obj%npicks)
      obj%written(column) = .true.
      return
      end subroutine fio_write_binary_ivars



      subroutine fio_write_binary_fvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      real            ,intent(in)    :: scalar              ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           return
      end if

      call dio_tell         (obj%dio,obj%addresses(2*column-1:))
      call dio_write_scalar (obj%dio,scalar)

      obj%fdefaults(column) = scalar
      obj%written  (column) = .true.
      return
      end subroutine fio_write_binary_fvar



      subroutine fio_write_binary_fvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      real            ,intent(in)    :: array(:)            ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           return
      end if

      call dio_tell             (obj%dio,obj%addresses(2*column-1:))
      call dio_write_array_plus (obj%dio,array,obj%npicks)

      if (obj%npicks > 0) obj%fdefaults(column) = array(obj%npicks)
      obj%written(column) = .true.
      return
      end subroutine fio_write_binary_fvars



      subroutine fio_write_binary_dvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      double precision,intent(in)    :: scalar              ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           return
      end if

      call dio_tell         (obj%dio,obj%addresses(2*column-1:))
      call dio_write_scalar (obj%dio,scalar)

      obj%ddefaults(column) = scalar
      obj%written  (column) = .true.
      return
      end subroutine fio_write_binary_dvar



      subroutine fio_write_binary_dvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      double precision,intent(in)    :: array(:)            ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           return
      end if

      call dio_tell             (obj%dio,obj%addresses(2*column-1:))
      call dio_write_array_plus (obj%dio,array,obj%npicks)

      if (obj%npicks > 0) obj%ddefaults(column) = array(obj%npicks)
      obj%written(column) = .true.
      return
      end subroutine fio_write_binary_dvars



      subroutine fio_write_binary_lvar (obj,column,scalar)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      logical         ,intent(in)    :: scalar              ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (.not.obj%delimiters(column)) then
           call fio_register_error (obj,'binary scalar field '//           &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should be a delimiter')
           return
      end if

      call dio_tell         (obj%dio,obj%addresses(2*column-1:))
      call dio_write_scalar (obj%dio,scalar)

      obj%ldefaults(column) = scalar
      obj%written  (column) = .true.
      return
      end subroutine fio_write_binary_lvar



      subroutine fio_write_binary_lvars (obj,column,array)
      implicit none
      type(fio_struct),intent(inout) :: obj                 ! arguments
      integer         ,intent(in)    :: column              ! arguments
      logical         ,intent(in)    :: array(:)            ! arguments

      if (column < 1 .or. column > obj%ncolumns .or. obj%whoops) return

      if (obj%delimiters(column)) then
           call fio_register_error (obj,'binary array field '//            &
                                trim(string_2_upper(obj%fields(column)))// &
                                ' should NOT be a delimiter')
           return
      end if

      call dio_tell             (obj%dio,obj%addresses(2*column-1:))
      call dio_write_array_plus (obj%dio,array,obj%npicks)

      if (obj%npicks > 0) obj%ldefaults(column) = array(obj%npicks)
      obj%written(column) = .true.
      return
      end subroutine fio_write_binary_lvars


                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!
                 !!-------- private support for tags --------!!


!!------------------------- fio private get tags --------------------------!!
!!------------------------- fio private get tags --------------------------!!
!!------------------------- fio private get tags --------------------------!!


      function fio_private_header_tag (secname) result (tag)
      character(len=*),intent(in) :: secname                    ! arguments
      character(len=LCHAR)        :: tag                        ! result
      tag = HDR_TAG//trim(string_2_upper(secname))//'>'
      end function fio_private_header_tag


      function fio_private_data_tag (secname) result (tag)
      character(len=*),intent(in) :: secname                    ! arguments
      character(len=LCHAR)        :: tag                        ! result
      tag = DTA_TAG//trim(string_2_upper(secname))//'>'
      end function fio_private_data_tag


      function fio_private_end_tag (secname) result (tag)
      character(len=*),intent(in) :: secname                    ! arguments
      character(len=LCHAR)        :: tag                        ! result
      tag = END_TAG//trim(string_2_upper(secname))//'>'
      end function fio_private_end_tag


!------------------------- is header or data or end tag ---------------------!!
!------------------------- is header or data or end tag ---------------------!!
!------------------------- is header or data or end tag ---------------------!!


      function fio_is_header_tag (card) result (answer)
      character(len=*),intent(in) :: card                       ! arguments
      logical                     :: answer                     ! result
      answer = (card(1:6) == HDR_TAG)
      end function fio_is_header_tag


      function fio_is_data_tag (card) result (answer)
      character(len=*),intent(in) :: card                       ! arguments
      logical                     :: answer                     ! result
      answer = (card(1:6) == DTA_TAG)
      end function fio_is_data_tag


      function fio_is_end_tag (card) result (answer)
      character(len=*),intent(in) :: card                       ! arguments
      logical                     :: answer                     ! result
      answer = (card(1:3) == END_TAG)
      end function fio_is_end_tag


!!------------------------------ name from tag ------------------------------!!
!!------------------------------ name from tag ------------------------------!!
!!------------------------------ name from tag ------------------------------!!


      function fio_name_from_tag (card) result (secname)
      character(len=*),intent(in) :: card                       ! arguments
      character(len=LCHAR)        :: secname                    ! result
      integer                     :: indx                       ! local
      if (fio_is_header_tag(card)) then
           indx    = index(card,'>')
           secname = string_2_lower(card(7:indx-1))
      else if (fio_is_data_tag(card)) then
           indx    = index(card,'>')
           secname = string_2_lower(card(7:indx-1))
      else if (fio_is_end_tag(card)) then
           indx    = index(card,'>')
           secname = string_2_lower(card(4:indx-1))
      else
           secname = ' '
      end if
      end function fio_name_from_tag


!!----------------------- read or write filetype ----------------------------!!
!!----------------------- read or write filetype ----------------------------!!
!!----------------------- read or write filetype ----------------------------!!


      subroutine fio_read_filetype (obj,filetype)
      implicit none
      type(fio_struct),intent(inout)        :: obj               ! arguments
      character(len=*),intent(out),optional :: filetype          ! arguments
      character(len=LRECORD)                :: card              ! local
      character(len=LMSG)                   :: msg               ! local
      integer                               :: indx1,indx2       ! local

                       call dio_rewind         (obj%dio)
                       call dio_read_card      (obj%dio,card)
      if (card == ' ') call dio_read_card      (obj%dio,card)
                       call dio_rewind         (obj%dio)
                       call dio_status         (obj%dio,msg)
                       call fio_register_error (obj,msg,'filetype')

      if (present(filetype)) then
           indx1 = index(card,'=')
           indx2 = index(card,'/')
           if (indx1 > 0 .and. indx2 > indx1) then
                filetype = string_2_lower(card(indx1+1:indx2-1))
           else
                filetype = ' '
           end if
      end if
      return
      end subroutine fio_read_filetype



      subroutine fio_write_filetype (obj,filetype)
      implicit none
      type(fio_struct),intent(inout)       :: obj                ! arguments
      character(len=*),intent(in),optional :: filetype           ! arguments
      character(len=LRECORD)               :: card               ! local
      character(len=LMSG)                  :: msg                ! local

      if (present(filetype)) then
           card = string_2_upper(filetype)
      else
           card = 'GENERIC'
      end if
      card = COMMENT//'<CPS_v1 TYPE='//trim(card)//'/>'

      call dio_rewind         (obj%dio)
      call dio_write_card     (obj%dio,card)
      call dio_write_card     (obj%dio,COMMENT)
      call dio_status         (obj%dio,msg)
      call fio_register_error (obj,msg,'filetype')
      return
      end subroutine fio_write_filetype


!!------------------------- read and write tags ----------------------------!!
!!------------------------- read and write tags ----------------------------!!
!!------------------------- read and write tags ----------------------------!!


      subroutine fio_write_header_tag (obj,secname)
      implicit none
      type(fio_struct),intent(inout) :: obj               ! arguments
      character(len=*),intent(in)    :: secname           ! arguments
      character(len=LCHAR)           :: tag               ! local
      character(len=LMSG)            :: msg               ! local

      if (obj%noheaders) return
      if (obj%whoops) return
      tag = fio_private_header_tag (secname)
      call dio_write_card          (obj%dio,COMMENT)
      call dio_write_card          (obj%dio,tag)
      call dio_status              (obj%dio,msg)
      call fio_register_error      (obj,msg,'header tag')
      return
      end subroutine fio_write_header_tag



      subroutine fio_write_data_tag (obj,secname)
      implicit none
      type(fio_struct),intent(inout) :: obj               ! arguments
      character(len=*),intent(in)    :: secname           ! arguments
      character(len=LCHAR)           :: tag               ! local
      character(len=LMSG)            :: msg               ! local

      if (obj%noheaders) return
      if (obj%whoops) return
      tag = fio_private_data_tag (secname)
      call dio_write_card        (obj%dio,COMMENT)
      call dio_write_card        (obj%dio,tag)
      call dio_status            (obj%dio,msg)
      call fio_register_error    (obj,msg,'data tag')
      return
      end subroutine fio_write_data_tag



      subroutine fio_write_end_tag (obj,secname)
      implicit none
      type(fio_struct),intent(inout) :: obj               ! arguments
      character(len=*),intent(in)    :: secname           ! arguments
      character(len=LCHAR)           :: tag               ! local
      character(len=LMSG)            :: msg               ! local

      if (obj%noheaders) return
      if (obj%whoops) return
      tag = fio_private_end_tag (secname)
      call dio_write_card       (obj%dio,tag)
      call dio_write_card       (obj%dio,COMMENT)
      call dio_status           (obj%dio,msg)
      call fio_register_error   (obj,msg,'end tag')
      return
      end subroutine fio_write_end_tag


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module fio_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

