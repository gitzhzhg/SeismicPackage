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
!-------------------------------------------------------------------------------
!                       C P S   P R I M I T I V E             
!
! Name       : cpsio (Conoco Processing System Input Output)
! Category   : io
! Written    : 1999-07-16   by: Bill Menger
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Perform functions on self-defining files.
! Portability: No known limitations
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!CPS (the Conoco Processing System) allows files to be self-defining. This means
!that when a file is opened with the cpsio package that the file will be scanned
!for a valid header and descriptive information. If a file is opened with the
!cpsio package and does not contain valid headers, then a set of rules will be
!followed that are well-defined, so that the programmer should be able to
!anticipate the behavior of the system. By including file headers within files,
!users will be able to document (for the system as well as for future users of
!the file) what is in the file and how to read/interpret the file.
!
!Rules:
!1. Every CPS file starts with an ASCII-encoded record in line 1 OR line 2 that
!contains the following:
!  (pre-tag)(prolog [optional keyword=value entries] (self-ending tag token).
!   An example:
!   #<CPS_v1 type=FILETYPE [with optional info here] />.
!   The first character(s) that are in the pre-tag are optional, and can be
!   anything. Typically this will be the pound sign (#) or the exclamation
!   point (!). The prolog line is typically found on line 1 of the file
!   (before the first line-feed character in a binary file.)
!   If the file is "C" source code OR a UNIX shell script, then line 1 could
!   have:
!   #!/shell/binary/location/exactly/with/the/full/path
!    or 
!   /*
!    or possibly some other value (perhaps an XML prolog.)
!   (This means that the parser will ignore the first line of a file if it does
!   not see the CPS prolog, and will look for a PRETAG to be declared when it
!   does read the prolog. It will scan line 2 for "<CPS_v1" and will set PRETAG
!   to the set of characters found before the "<" in "<CPS_v1". If PRETAG is
!   declared, it will use the declared value. This behavior allows us to read
!   any file, regardless of what character(s) are used as comment characters.)
!2. There are only six types of tags defined (to date). These are:
!   * Prolog:  <CPS_v1 type=FILETYPE [optional attribute=value ] /> 
!   * Header:  <HDR_name ["optional keyword=value, keyword=value..."] > 
!   * Data:    <DTA_name> 
!   * Other:   <name> 
!   * Comment: <-- (your comment is between these tags.
!       The tag is "<--" to start an inline comment. , and "/> " to end it.
!   * Implied: The "CPS_v1 tag automatically opens an implied tag called 
!     HDR_GLOBAL, into which is placed any values found on the prolog line
!     (such as the "type=FILETYPE" above.) If no HDR or DTA tags are present,
!     then all data is assumed to be described by any directives found in the
!     GLOBAL header section. 
!3. The tags are formatted as shown above. The comment tag occurs anywhere and
!   is ignored by the parser (from the opening <-- to the closing />.)
!4. The tagged sections are always ended with the associated #</name> or if 
!   unnamed, with #</>.
!5. Tags may not be nested. (For example,
!   #<doc>
!   (some preliminary info here)
!   #<overview> (ignored since we are inside the "doc" section.)
!   # To include a comment, you must use the inline comment tag. All data
!   # within and including the inline comment tag will not be passed out of
!   # the I/O API, but thrown in the trash bin. <-- or whatever! />
!     (blank lines within section are passed as is.)
!   #</overview>
!   # I am still in a previously defined section (doc) so this line is passed
!   # after stripping off the #.
!   (detailed doc here)
!   #</doc>
!   # This line is not inside a section, and is treated as a line comment. 
!   # It is not passed out of the I/O API. It is NOT parsed 
!   # by the I/O api for keyword=value pairs because it follows another section.
!   # If you want to define GLOBAL keywords, to it at the top of the file.
!   Lines that are within HDR sections that need to be parsed for keywords
!   must look like:
!   PRETAG (whitespace) KEYWORD(whitespace)=(whitespace)VALUE
!     (optionally more kw=value...)
!   where: 
!     PRETAG is usually # or !, but can be set as defined below.
!     KEYWORD is a contiguous character string
!     (no whitespace or punctuation in it, only - or _ or A-Za-z 0-9 allowed.)
!     VALUE is a contiguous character string OR a " delimited non-contiguous
!     character string OR a set of strings.
!     VALUE is optionally enclosed within () and delimited by whitespace or
!     comma 
!6. All tags except for comment tags ALWAYS begin at the start of a record.
!   This includes ending tags. (e.g. </doc>)
!7. A file may only have ONE unnamed Header and associated unnamed Data section,
!   but is not required to have an unnamed section. (The un-named header
!   section is referred to by the name GLOBAL within the cpsio system.)
!   * The unnamed HDR section may be used to define global information about
!     the file, and is implied to exist if not explicitly present. 
!   * This means that a file can have the following constructs to declare
!     global keywords for the file: (all are ok) 
!     #<CPS_v1 type=MYTYPE />
!     #<HDR_GLOBAL>
!     #MYGLOBAL = myvalue
!     #</GLOBAL>
!     or -------------------
!     #<CPS_v1 type=MYTYPE, MYGLOBAL = myvalue />
!     or ------------------
!     #<CPS_v1 type=MYTYPE/>
!     #<HDR_>
!     # MYGLOBAL = myvalue
!     #</>
!8. If a file has an unnamed section, the tags look like this:
!     * #<HDR_> To define the header  (OR #<HDR> )
!     * #<DTA_> To define the start of data  (OR #<DTA> )
!     * #</> To determine the end of either section (header or data). 
!9. If a file has keywords defined in an unnamed section, they are assumed to
!   apply only for the unnamed section of the file that occurs before any
!   named data sections. Keywords defined within a named header section have
!   a scope only within that section.
!10. Data sections (sections enclosed within <DTA_name> and </name> tags )
!    are not required to be present. A corresponding HDR section may be used to
!    define such a section if it were present, but the presence of a <HDR_name>
!    section does not require that the corresponding DTA section must exist.
!11. If more than one DTA section appears that corresponds to a given HDR
!    section, only the first DTA section will be read by the system.
!      #<HDR_my_data_type_definition_name>
!      #mykeyword=myvalue
!      #</my_data_type_definition_name>
!      ...
!      #<DTA_my_data_type_definition_name>
!      my data for my first data section
!      #</my_first_data_section_name>
!      ...
!      #<DTA_my_data_type_definition_name>
!      (this data section will be ignored.)
!      my data for my second data section
!      #</my_data_type_definition_name>
!
!CPSIO BEHAVIOR 
!The CPS API will read ANY file, with the following actions:
!If the CPS prolog is missing or incorrect, a non-fatal status code will be
!returned. Action by the caller will determine how this error is handled.
!(The front - end might pull up a file viewer utility to guide the user in
!creating a header, the back end may simply log an error in the job log.) 
!If line one does not contain a prolog, then line two is parsed for the
!string "<CPS_v1". If found, then PRETAG is set to the string of
!character(s) preceding "<CPS_v1". If pretag is declared (after "v1") 
!then the value of pretag is replaced accordingly. (Example:
!     // C++ file in CPS format
!     //<CPS_v1 type=MYTYPE, PRETAG="//"/>
!     // rest of file
!     or ---------
!     #!/bin/sh
!     #<CPS_v1 type=scriptfile pretag=! />
!     ! first line
!     ! second line...
!If a data section is found with no header and no globally defined
!(unnamed) header field information, default parsing rules will be followed. 
!
!Default Parsing rules (when no information is available in a header):
!1. Assume defaults for all values
!2. Position the file at first "data" record. (non-blank line not
!   starting with "pre-tag".)
!3. Pass all data out to reader as normal fortran I/O would do, skipping any
!   blank lines or lines beginning with the "pretag" string, and stripping
!   out all inline comments.
!4. If proper tag-endtag sequences (including nesting) are not found in the
!   header, then assume that the records being read still belong inside
!   the header that was previously opened.
!5. If, while in a tagged data section, another begin-tag is found,
!   assume end of that data section and return CPSIO_EOS.
!6. If, while in a tagged data section an EOF is found, return non-fatal
!   error, assume an end-tag was found, close the open data section.
!7. If records occur that are outside of any tags and do not begin with
!   PRETAG, read them up to the first tag found in a data section.
!8. Upon reading a record beginning with PRETAG (usually the # character),
!   determine the RECORD type and take appropriate action as follows:
!   TAG begin
!     Determine Tag Type 
!     * HDR: Open new HDR object, assign name, push name onto tag stack. 
!     * DTA Match name with existing HDR object. 
!     * If no match, error code is returned.
!     If match, load attributes and position file at first data record.
!   TAG end
!     Find matching tag name in stack
!     Wrapup writing that tag's HDR info to memory if HDR end tag
!     Send return code indicating end of data for DTA end tag
!   KEYWORD = VALUE
!     Parse the line, assign values to keywords as they are found. 
!     Place in memory based on position within named tag section. 
!   CONTINUATION
!     Add characters to previous record until final end of record is found 
!     (with no continuation), then treat as a complete record.
!   COMMENT
!     Ignore
! 
!DEFINED KEYWORDS and DEFAULTS
!
! Keyword   |Default Value|Meaning
!-----------+-------------+-----------------------------------------------------
! PRETAG    |             |The character read at position 1 on the prolog
!           |             | (usually # or !)|This tells the parser when to
!           |             | look for tagged entries and/or keyword definitions.
!           |             | It should be set different from the prolog only
!           |             | when necessary because of the nature of the file
!           |             | content. (For example, C code may need
!           |             | another character, but the prolog can be embedded
!           |             | in a comment )
!-----------+-------------+-----------------------------------------------------
! TYPE      |             |What type of CPS file this is. (i.e. F90Source,
!           |             | CSource, Mute...)
!-----------+-------------+-----------------------------------------------------
! NULL      |-nil-        |The string that indicates a null value is present.
!-----------+-------------+-----------------------------------------------------
! DELIM     |comma, white |If data has more than one field, use this character
!           |             | (or any in the set) as the delimiter. (*)
!-----------+-------------+-----------------------------------------------------
! SKIP_BNK  |  TRUE       |In data section, do I skip or process blank lines?
!-----------+-------------+-----------------------------------------------------
! WRAP      |1            |Number of lines per record. (*)
!-----------+-------------+-----------------------------------------------------
! CONTINUE  |&            |Continuation Character for use when a value spans
!           |             | a record boundary. (*)
!-----------+-------------+-----------------------------------------------------
! ENCODING  |ASCII        |Could be BINARY or ASCII.(*)
!-----------+-------------+-----------------------------------------------------
! NUM_FLD   |undefined    |How many fields in each record. (Automagically set.)
!-----------+-------------+-----------------------------------------------------
! FLD_FRMT  |undefined    |Optional (not needed by cpsio).
!           |             | How to read a data column.( %14.7f, 14.7f, z16...)
!-----------+-------------+-----------------------------------------------------
! FLD_NAME  |undefined    |Name associated with each data field (e.g. x,y,z)
!-----------+-------------+-----------------------------------------------------
! FLD_UNIT  |undefined    |Units for each field. (e.g. (meters,feet,sec.)
!-----------+-------------+-----------------------------------------------------
! FLD_THDR  |undefined    |Trace header associated with this data column.
!-----------+-------------+-----------------------------------------------------
! FLD_RANK  |Not avail.   |Dimensionality of a variable. (1=vector, 
!-----------+-------------+-----------------------------------------------------
!           |             | 2=2-d array...) leftmost index is sequential,
!           |             | rightmost has largest stride (FORTRAN default)
!-----------+-------------+-----------------------------------------------------
! FLD_DIM(i)|Not Avail.   |Size of dimensions (This is a vector value of
!           |             | length min(1,FLD_RANK). For a 1 x 5 vector, 
!           |             | fld_rank=1, fld_dim=(1,5). For a 2 x 3 array,
!           |             | fld_rank=2, fld_dim=(2,3), For a scalar, 
!           |             | fld_rank=0, fld_dim=(1). (i=1,NUM_FLD)
!-----------+-------------+-----------------------------------------------------
! Simple example
! #<CPS_v1 TYPE=Mutefile />
! #<history>
! # This file was created on Aug 23, 1999 to describe the mute file syntax in a
! # trivial example. Actual
! # mute files may not look like this at all.
! #</history>
! #latest_inline_updated = 4.000000
! #latest_crossline_updated = 0.000000
! #fld_name = (offset, inline, crossline, time)
! #fld_thdr = (6 46 0 0)
! #SKIP_BNK=.true.                      <-- this is the default \>
! #</mute>
! #<DTA mute>
! 2670.000000 1.000000 0.000000 0.589474
! 4314.000000 1.000000 0.000000 0.968421
!
! 5964.000000 1.000000 0.000000 1.202339
! 9259.000000 1.000000 0.000000 1.631579 
! 2348.000000 2.000000 0.000000 0.581459
! 9917.000000 2.000000 0.000000 1.803883 
! 2357.000000 3.000000 0.000000 0.497685
!
! 9920.000000 3.000000 0.000000 1.800541 
! 2363.000000 4.000000 0.000000 0.481687
! 9921.000000 4.000000 0.000000 1.666623 
! 2383.000000 5.000000 0.000000 0.545425
! 5345.000000 5.000000 0.000000 1.072165
! 5673.000000 5.000000 0.000000 1.345361
! 6984.000000 5.000000 0.000000 1.221650
!
! 8951.000000 5.000000 0.000000 1.503370
! 9924.000000 5.000000 0.000000 1.649088 
! 2329.000000 8.000000 0.000000 0.868377
! 4966.000000 8.000000 0.000000 1.289441
! 6941.000000 8.000000 0.000000 1.308897
! 9882.000000 8.000000 0.000000 1.789996 
! 4353.000000 10.000000 0.000000 0.835052 
! 2402.000000 11.000000 0.000000 0.643261
! 4031.000000 11.000000 0.000000 1.010309
! 5336.000000 11.000000 0.000000 1.055534
! 8590.000000 11.000000 0.000000 1.512644
! 9893.000000 11.000000 0.000000 1.767354 
! #</mute>
! 
! How to use the above example:
! Code fragment:
!    
!    integer :: myunit, nrec, inline, crossline
!    real :: offset, time, value
!    character(len=MYLEN) :: filename, mode, mycharstring
! 
!    mode = 'r' 
!    ! currently mode must be "r" (read) ... soon will also 
!    ! allow 'u(pdate) w(rite) a(ppend).
!    filename = 'mutefile.dat' ! name of the above mute file.
! 
!    if( cpsio_open (filename,mode, myunit) /= CPSIO_OK) abort 
!    ! Open file, read and parse the header(s).
!    if (cpsio_position_file(myunit,1,'mute') /= CPSIO_OK ) abort
!    ! Put the file at start of mute data section.
!    nrec = cpsio_number_records(myunit,'mute','inline','1.000000') 
!    ! find out how many records in the 'mute' section
!    ! have the 'inline' field = '1.000000'
!    do i = 1, nrec
!      status = cpsio_get_value(myunit, 'inline', inline)
!      if(status /= CPSIO_OK) exit
!      if(inline == 1 ) then
!        status = cpsio_get_value(myunit,'offset',offset)
!        status = cpsio_get_value(myunit,'time',time)
!      else
!        cycle
!      end if
!      print*,' For inline= ',inline, 'Offset: ',offset,' Time = ',time
!    end do
!    ...
!    OR
!    status = cpsio_read_data(myunit, mycharstring)
!    read(mycharstring',(MY FORMAT)')offset,inline,crossline,time
!    ADDIITONALLY:
!    status = cpsio_get_keyword(myunit,'latest_inline_updated',value,'mute')
!    print*, 'latest inline updated = ',value
!    ...
!    status = cpsio_close(myunit)
!    end...
!</descript_doc>
!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS      
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!    opt = optional argument
!
! RETURN CODES: (status variable)
!
!          CPSIO_OK       =  all is well
!          CPSIO_EOF      =  end of physical file
!          CPSIO_EOS      =  end of data section
!          CPSIO_COMMENT  =  a "pretagged" line was read and skipped in a data
!                            section
!          CPSIO_BLANK    =  a blank line was read and skipped (in data sec.)
!                            (if skip_bnk = false, a blank line will return 
!                             with CPSIO_OK instead.)
!          CPSIO_MISSING  =  Keyword or section is missing.
!          CPSIO_ERROR    =  Other error occurred.
! 
!                          CALLING SEQUENCE
!
!                           i       i    o   i(opt)
!     status = cpsio_open(filename,mode,lun,scratch)
!        Purpose:         Open a file, read and load the header and keywords,
!                         associate the file with a lun, position file at start 
!                               of data.
!              character(*)     filename
!              character(*)     mode (r/w/u/a)(+/n) read/write/update/append
!              integer          lun  (logical unit number)
!              integer          status (0 = ok, <>0 = error.)
!              logical          scratch (true = delete when done.)
!                           i   i(opt) i(opt)
!     status = cpsio_close(lun,remove,keep_open)
!              Purpose:         Close a file previously opened by cpsio.
!              integer          lun (logical unit number)
!              logical          remove (true = delete file on close)
!              logical          keep_open (true leaves the file open, but
!                               removes the cpsio overhead (cardsets in memory)
!                               and the file should only be accessed via the
!                               cio primitives subsequently.
!              integer          status (0 = ok, <> 0 = error.)
!
!
!     number_sections = cpsio_number_sections(lun)
!      Purpose:         Tell you how many sections are defined in the header.
!                       Each section may describe different data sets within the
!                       file, or may contain textual information.
!      integer          lun (logical unit number)
!      integer          number_sections
!
!                                      i       o 
!     status = cpsio_get_sectionlist(lun,sectionlist) 
!       Purpose:         Return array of character strings, each containing the
!                        name of defined file header sections stored in memory.
!              integer          lun (logical unit number)
!              character(*)     sectionlist(:) names of defined sections.
!              integer          status (0 = ok, <> 0 = error)
!
!                                              i      i(opt)
!     number_keywords = cpsio_number_keywords(lun,section_name)
!      Purpose:         Return the number of keywords in a file section.
!      integer          number_keywords
!      integer          lun logical unit number
!      character(*)     section_name (if missing, looks for the global section.)
! 
!                                     i     o          i(opt)   
!    status =  cpsio_get_keywordlist(lun,keywordlist, section_name)
!    Purpose:         Return the keyword information for a section (or for the
!                     'GLOBAL' section if section_name is missing.)
!    integer          status (CPSIO_OK if no errors, CPSIO_ERROR if errors)
!    character        keywordlist(:)
!    integer          lun logical unit number
!
!                                         i     i        i(opt)
!    nelements = cpsio_get_keyword_nelem(lun,keyword,section_name)
!      Purpose:         Return the number of elements in a keyword.
!      integer          lun (logical unit number)
!      character        keyword
!      character        section_name (optional)
!      nelements        How many elements. (0 = not found or none) 
!
!                                i     i      o       i(opt)
!    status = cpsio_get_keyword(lun,keyword,value,section_name)
!    ************* for scalar keywords*******
!                                i     i      o     o(opt)    i(opt)
!    status = cpsio_get_keyword(lun,keyword,value,nelements,section_name)
!    ************* for array keywords********
!*RESTRICTIONS:  Only returns REAL or CHARACTER keywords so far.
!    purpose:       return value of a keyword in an optional header section.
!                   if not present, status=-1 value = NULL
!    integer        status 0 = found keyword. -1 = not found.
!    integer        lun (logical unit number)
!    character      keyword
!    integer        nelements (how many values are returned) (you must have
!                              allocated space for these ahead of time.)
!    ANY TYPE       value(:) or value (if scalar)
!    character      section_name
!
!                                    i     i       i(opt)
!     status  = cpsio_position_file(lun,position,section_name)
!     Purpose:         position the file at a particular record in an optional
!                      section = "section_name" (this is a data section).
!     integer          lun logical unit number
!     character(*)     section_name
!
!                              i      i           o
!    status = cpsio_get_value(lun,field_name,field_value)
!    status = cpsio_get_value(lun,field_number,field_value)
!    Purpose:  Return a data value for a data field either by field name or
!              field number.
!    integer        lun 
!    character      field_name
!    integer        field_number
!    any type       field_value (logical,integer,real,double,character)
!
!                                     i    o        i(opt)
!     status   = cpsio_find_position(lun,position,section_name)
!***  CURRENT RESTRICTION DOES NOT USE section_name ***
!     Purpose:         find the position of the file pointer.(record number)
!                      (position is ALWAYS ABSOLUTE WITHIN THE FILE.
!     integer          lun logical unit number
!     integer          position    Where am i globally within the file. 
!     character        section_name NOT USED.
!
!                               i   o      i(opt)       i(opt)    i(opt)
!  status = cpsio_read_data   (lun,record,section_name,key_field,key_value,
!                                o(opt)
!                              num_cards)
!    purpose:       return a data record from optional section_name matching
!                   optional key_field with value key_value.
!    integer        status ! 0 = ok, EOF = end of file 
!    integer        lun (logical unit number)
!    character(*)   record
!    character(*)   section_name (optional)  
!    character(*)   key_field    (optional)  If used, then the next record in
!                   the file (or section) where this field = key_value is
!                   returned.
!    integer        num_cards (optional)   Number of physical records read.
!
!                        i   o
!  status = cpsio_read (lun,record)
!    Purpose: Just to read a record. (a line)  from a file.
!    integer        status ! 0 = ok, EOF = end of file 
!    integer        lun (logical unit number)
!    character(len=*) or character(len=*),dimension(:) record 
!                         i   i
!  status = cpsio_write (lun,record)
!    purpose:       write a record to a cpsio file.
!
!    integer        status ! 0 = ok, CPSIO_ERROR = error.
!    integer        lun (logical unit number)
!    character(len=*) or character(len=*),dimension(:) record 
!
!                           i
!  subroutine cpsio_rewind(lun) ! to rewind the file.
!                              i
!  subroutine cpsio_backspace(lun) ! to backspace one record.
!
!                          i 
!  is_cpsio  = cpsio_test(lun)
!     purpose:  To return TRUE if lun is opened as a cpsio file, FALSE otherwise
!
!                                         i    i(opt)       i(opt)   i(opt)
!  number_records = cpsio_number_records(lun,section_name,key_field,key_value,
!                                          o(opt)
!                                        num_cards)
!  Purpose:      Return the number of logical records in a file that optionally 
!                are in a section and that optionally have a key-field with
!                value = key-value. (Note:  This may require scanning the
!                entire file, which could be lengthy.
!  integer       number_records ! the answer!
!  integer       lun logical unit number
!  character(*)  section_name ! optional section name
!  character(*)  key_field    ! optional key_field name
!  any type      key_value    ! value of the key_field for comparison.
!  integer       num_cards    ! optional number of physical records read.
!
!                                       i    i(opt)       i(opt)   i(opt)
!  number_cards = cpsio_number_cards(lun,section_name,key_field,key_value)
!  Purpose:      Return the number of physical records in file that optionally 
!                are in a section and that optionally have a key-field with
!                value = key-value. (Note:  This may require scanning the
!                entire file, which could be lengthy.
!  integer       number_cards ! the answer!
!  integer       lun logical unit number
!  character(*)  section_name ! optional section name
!  character(*)  key_field    ! optional key_field name
!  any type      key_value    ! value of the key_field for comparison.
!
!**************** NOT  IN SERVICE (ALL ROUTINES BELOW)***********************
!***                                  i    o        i(opt)
!***  status   = cpsio_find_position(lun,position,section_name)
!***  Purpose:         find the position of the file pointer.(record number)
!***                   (position is within the optional section or absolute
!***                   if the section_name is missing)
!***  integer          lun logical unit number
!***  integer          position    Where am i (globally or in a section)?
!***  character        section_name In the section given, where am i?
!*
!****************************************************************************
!-------------------------------------------------------------------------------
!</calling_doc>
!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!     Date        Author       Description
!     ----        ------       -----------
!030. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 29. 2006-01-10  B. Menger    Removed Unused Variables.
! 28. 2004-05-03  R Selzler    Correct one compiler error, Absoft 8.0 with
!                              latest quick fix: Dummy arguments with the
!                              INTENT(OUT) attribute must be defined before use.
! 27. 2004-01-21  R. Selzler   Corrected two substring references.
! 26. 2003-10-08  Bill Menger  Modified to load sectionlists for w+. 
! 25. 2002-09-26  Ed Schmauch  Nullified fileptr%sectionlist%section in
!                              cpsio_create.
! 24. 2002-02-04  Ed Schmauch  Nullified newly created pointers in
!                              cpsio_init_section.
! 23. 2001-11-01  Ed Schmauch  Fixed bug in cpsio_assemble_recd to allow
!                              continued record without being in quotes.
!                              Add function cpsio_number_cards.
! 22. 2001-10-16  Bill Menger  Added test before deallocate of 
!                              thisfile%sectionlist%section
! 21. 2001-10-10  Bill Menger  Fixed memory leak on delete function.
! 20. 2001-07-27  Ed Schmauch  Made file array dynamic.
! 19. 2001-02-13  Bill Menger  Removed 40-byte memory leak on create.
! 18. 2000-12-14  Bill Menger  Added nullify statements to pointers.
! 17. 2000-11-17  Bill Menger  Modified cpsio_open to always rewind file if
!                              it is not cpsio unless mode "a", then seek to
!                              end.
! 16. 2000-10-17  Bill Menger  Added cpsio_test function to see if unit is a
!                              cpsio - type file.  Also added optional arg to
!                              cpsio_close (keep_open).
! 15. 2000-09-26  Bill Menger  Changed continuation character from \ to &.
! 14. 2000-09-21  Bill Menger  Fixed ident string, changed access for private
!                              functions to private.
! 13. 2000-09-14  Bill Menger  Fixed bug in cpsio_read. Status was returned
!                              incorrectly.
! 12. 2000-08-17  Bill Menger  Added ability to NOT skip blank data records.
! 11. 2000-04-25  Bill Menger  Added delete feature to cpsio_close.
! 10. 2000-04-19  Bill Menger  Took out errmsg static strings, fixed 
!                              cpsio_write_vec to correctly use the len fntn.
!  9. 2000-02-07  Bill Menger  Added use of CIO_OK, CIO_ERROR, CIO_EOF
!  8. 2000-02-04  Bill Menger  Added mode wn and an
!  7. 2000-01-11  Bill Menger  Removed illegal pointer usage on line 2506.
!  6. 1999-12-27  Bill Menger  commented out the "cpsio_open: message about
!                              file header not cpsio format.
!  5. 1999-10-18  Bill Menger  Added ability to correctly unmangle a bad header
!                              into a global section where parameters are read.
!  4. 1999-10-07  Bill Menger  Added pass-through for read.
!  3. 1999-10-06  Bill Menger  Added pass-through for backspace, rewind,write
!  2. 1999-09-22  Bill Menger  Reworked to use cio module instead of F90 I/O.
!  1. 1999-07-16  Bill Menger  Initial version.
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

module cpsio_module

!Purpose: To perform basic file i/o for the CPS system

use cio_module
use cardset_module
use cardsetlist_module

implicit none

!public
private

!PROCEDURES      

public :: cpsio_open, cpsio_close
public :: cpsio_number_sections, cpsio_get_sectionlist
public :: cpsio_number_keywords, cpsio_get_keyword,cpsio_get_keywordlist
public :: cpsio_position_file, cpsio_find_position,cpsio_get_keyword_nelem
public :: cpsio_get_value, cpsio_read_data
public :: cpsio_number_records, cpsio_number_cards
public :: cpsio_backspace, cpsio_rewind,cpsio_write,cpsio_read
public :: cpsio_test

!DATA STRUCTURES,VARIABLES

public :: CPSIO_OK, CPSIO_ERROR, CPSIO_EOF, CPSIO_EOS, CPSIO_COMMENT
public :: CPSIO_BLANK, CPSIO_MISSING

!-----------------------Ident
character(len=100),save,public :: cpsio_ident=&
'$Id: cpsio.f90,v 1.30 2006/09/18 13:32:44 Glover prod sps $'

!--------------Parameters

logical,parameter                  :: debug=.false.
integer,parameter                  :: stdout = 6
character(len=2),parameter         :: tag_self_terminate="/>",&
                                      tag_terminate="</" 
character(len=1),parameter         :: tag_begin ="<", tag_end = ">"
character(len=7),parameter         :: prolog="<CPS_v1"
character(len=1),parameter         :: NULL = ''         ! the null string
character(len=1),parameter         :: CONTIN=achar(38)
character(len=1),parameter         :: QUOTE=achar(34)
character(len=3),parameter         :: COMMENT_START="<--"
character(len=2),parameter         :: COMMENT_END=tag_self_terminate
character(len=3),parameter         :: WHITESPACE=achar(32)//achar(9)//achar(0)
character(len=4),parameter         :: DELIM=','//WHITESPACE
!---- The kwlst is checked for each section to load up values needed.
character(len=8),parameter          :: kwlst(8)=(/'WRAP    ','NULL    ',&
                                                  'NUM_FLD ','FLD_FRMT', &
                                                  'FLD_NAME','FLD_UNIT', &
                                                  'FLD_THDR','SKIP_BNK'/)
!========= RETURN VALUES
integer, parameter                 :: CPSIO_OK       =  0
integer, parameter                 :: CPSIO_EOF      = -1
integer, parameter                 :: CPSIO_EOS      = -2
integer, parameter                 :: CPSIO_COMMENT  = -3
integer, parameter                 :: CPSIO_BLANK    = -4
integer, parameter                 :: CPSIO_MISSING  = -5
integer, parameter                 :: CPSIO_ERROR    = -7
integer, parameter                 :: CPSIO_FIELD_SIZE      =  255
integer, parameter                 :: CPSIO_FIELD_NAME_SIZE =  255
integer, parameter                 :: CPSIO_CARD_SIZE       =  255
integer, parameter                 :: CPSIO_RECORD_SIZE     = CARDSET_LENGTH
integer, parameter                 :: CPSIO_FILE_NAME_SIZE =   160
integer, parameter                 :: CPSIO_MAX_NUM_FIELDS = 99
integer, parameter                 :: CPSIO_MAX_NUM_SECTIONS = 99
integer, parameter                 :: CPSIO_ALLOC_EXTRA_CHUNK = 10

!--------------Derived data Types

type :: cpsio_section
  character(len=CPSIO_FIELD_SIZE)  :: section_name ! section name. 
  character(len=CPSIO_FIELD_SIZE)  :: section_type ! section type.
  integer                          :: wrap      ! how many cards per record.
  integer                          :: num_fld    ! how many fields defined below
  character(len=CPSIO_FIELD_SIZE)  :: null      ! set of chars that = null val. 
  logical                          :: skip_blanks
  character(len=CPSIO_FIELD_SIZE),pointer :: fld_frmt(:)! format of each field
  character(len=CPSIO_FIELD_SIZE),pointer :: fld_name(:)! name of each field
  character(len=CPSIO_FIELD_SIZE),pointer :: fld_unit(:)! units for each field
  integer                        ,pointer :: fld_thdr(:)!Trace hdr wrd for fld.
  character(len=CPSIO_FIELD_SIZE),pointer :: field(:) ! Last read fields.
  logical                        ,pointer :: stale(:) ! .true. = need to read.
end type cpsio_section

type :: cpsio_sectionlist
  integer                          :: num_sections ! how many sections?
  type(cardsetlist_struct),pointer :: cardsetlist ! pointer to card sets.
  type(cpsio_section),pointer      :: section(:)
end type cpsio_sectionlist

type :: cpsio_file
  character(len=CPSIO_FILE_NAME_SIZE) :: name        ! file name
  integer                          :: lun         ! unit number
  character(len=2)                 :: mode        ! r/w/a/r+/w+/wn
  character(len=CPSIO_FIELD_SIZE)  :: pretag      ! The tag at start of records
  integer                          :: len_pretag  ! Length of the pretag
  logical                          :: pretag_defined ! is pretag defined yet?
  type(cpsio_sectionlist), pointer :: sectionlist ! ptr to sections
  integer                          :: starting_data_record 
                                                      ! first record with data.
  integer                          :: next_abs_record ! next absolute record.
  logical                          :: initial_read_of_data_section
  character(len=CPSIO_FIELD_SIZE)  :: current_data_section ! where am i?
  character(len=CPSIO_FIELD_SIZE),&
                           pointer :: field_ptr(:)
  logical, pointer                 :: stale_ptr(:)
end type cpsio_file

type :: cpsio_struct
  type(cpsio_file),pointer         :: p           ! pointer to file structure
end type cpsio_struct 

!--------------Global variables for the module

type(cpsio_struct), save, dimension(:), pointer :: file
integer, save                                   :: file_max_lun = 0

!---------------------------INTERFACES-------------------


interface cpsio_read
  module procedure cpsio_read_str
  module procedure cpsio_read_vec
end interface

interface cpsio_write
  module procedure cpsio_write_str
  module procedure cpsio_write_vec
end interface

interface cpsio_get_keyword
  module procedure cpsio_get_keyword_real_scalar
  module procedure cpsio_get_keyword_real_array
  module procedure cpsio_get_keyword_int_scalar
  module procedure cpsio_get_keyword_int_array
  module procedure cpsio_get_keyword_char_scalar
  module procedure cpsio_get_keyword_char_array
  module procedure cpsio_get_keyword_log_scalar
  module procedure cpsio_get_keyword_log_array
  module procedure cpsio_get_keyword_dble_scalar
  module procedure cpsio_get_keyword_dble_array
end interface

interface cpsio_get_value
  module procedure cpsio_get_value_by_nmbr_real
  module procedure cpsio_get_value_by_name_real
  module procedure cpsio_get_value_by_nmbr_int
  module procedure cpsio_get_value_by_name_int
  module procedure cpsio_get_value_by_nmbr_dble
  module procedure cpsio_get_value_by_name_dble
  module procedure cpsio_get_value_by_nmbr_char
  module procedure cpsio_get_value_by_name_char
  module procedure cpsio_get_value_by_nmbr_log
  module procedure cpsio_get_value_by_name_log
end interface

!--------------------------PROCEDURES--------------------

contains

integer function cpsio_open (filename,mode,lun,scratch ) result(status)
!--------------Argument list
character(len=*),    intent(in)           :: filename ! the user's file
character(len=*),    intent(in)           :: mode     ! [r|w|a|r+|w+] read/write
integer, intent(out)                      :: lun      ! fortran unit number.
logical,optional,intent(in)               :: scratch  ! is this a temp file?
!--------------local variables
logical                                   :: local_scratch

                                             ! position of tag in string

                                             ! position in record
integer                                   :: ierr         ! Error code

character(len=CPSIO_RECORD_SIZE)          :: record,scrap,tag_name, &
                                             tag_type

type(cpsio_struct), dimension(:), pointer :: file_temp
integer                                   :: alloc_err
!--------------Cardset variables
type(cardset_struct),pointer              :: cardsetptr 
                                             ! reuseable pointer to a cardset
!--------------Logical variables
logical                                   :: in_data          ,& 
                                             in_tagged_area,tagged,pretagged , &
                                             self_ended_tag,in_global_header

character(len= CPSIO_FIELD_SIZE)          :: section_type
character(len= CPSIO_FIELD_SIZE)          :: section_name


!--------------------------Initialize variables for this call
status         = CPSIO_OK
!--------------------------Logical state variables
if(present(scratch) ) then
  local_scratch = scratch
else
  local_scratch = .false.
endif

in_global_header = .false.
in_data        = .false.
in_tagged_area = .false.

!--------------------------Determine mode for opening the file.
select case (mode)
case('r', 'w' ,'a' ,'r+' ,'w+' ,'a+' , 'an', 'wn' )
case default
  call cpsio_errmsg('cpsio_open: incorrect mode for opening ' &
               //trim(filename)//'. MODE= '//mode//'.')
  go to 9999
end select
  
!--------------------------Do the open and associate with unit lun.
lun = cio_fopen(filename,mode,local_scratch)
!----------------------------If error then die.
if(lun <= 0) goto 9999

if (lun < cio_min_unit) then
  call cpsio_errmsg('cpsio_open:  bad lun from cio_fopen')
  goto 9999
endif
!----------------------------Make sure file array is big enough.
if (lun > file_max_lun) then
  allocate(file_temp(cio_min_unit:lun + CPSIO_ALLOC_EXTRA_CHUNK), &
           STAT = alloc_err)

  if (alloc_err /= 0) then
    call cpsio_errmsg('cpsio_open:  Out of memory')
    goto 9999
  endif

  if (file_max_lun > 0) then
     file_temp(cio_min_unit:file_max_lun) = file(cio_min_unit:file_max_lun)
     deallocate(file)
  endif

  file => file_temp
  nullify(file_temp)

  file_max_lun = lun + CPSIO_ALLOC_EXTRA_CHUNK
endif
!----------------------------Associate with file(:) element.
file(lun)%p => cpsio_create(filename,mode,lun)

!--- If open for write or write+, return because we have no data to read first.
if (mode(1:1) == 'w') then
    if(len(mode) == 1) then
      !delete the cpsio structure for this file, since it won't be used.
      !status = cpsio_delete(file(lun)%p) 
      return
    else if(mode(2:2) /= '+' ) then
      !delete the cpsio structure for this file, since it won't be used.
      !status = cpsio_delete(file(lun)%p) 
      return
    endif
endif

!--- Now, position file to beginning of data even if it was opened for append.
!--- We can then read the header, and later position to end of file.
call cio_frewind(lun)


!-------------Read the header and stash into cardsets, position file at 
!-------------Beginning of data.

!-------------Read the file for a prolog first.
ierr = cpsio_get_logical_recd(lun,record,pretagged)
if(ierr == CPSIO_ERROR .or. ierr == CPSIO_EOF ) then
  !call cpsio_errmsg('cpsio_open: file header not cpsio format.')
  !delete the cpsio structure for this file, since it won't be used.
  !status = cpsio_delete(file(lun)%p) 
  status = CPSIO_OK
  if(mode(1:1) == 'a') then
    status = cio_fseek(lun,0,2) ! position at end of file.
  else
    call cio_frewind(lun)
  endif
  return
endif

!------------Now the prolog has been read in, if it exists.
if (pretagged) then
  !check for tag telling me what to do
  call cpsio_extract_tag(record,tag_type,tag_name,tagged,self_ended_tag,scrap)
  if(tagged) then
    ! For default actions, start the file in the default state, with
    ! the tag name = 'GLOBAL' of  type HEADER.
    section_name = tag_name
    section_type = tag_type
    in_tagged_area = .true.
    ! must handle the tag (create new section, end section,...)
    ! Create the Global cardset for this file.
    cardsetptr => & 
      cardsetlist_create_new_cardset(file(lun)%p%sectionlist%cardsetlist, &
      trim(section_name))
    call cpsio_init_section(lun,section_name,section_type)
    ! if prolog, put scrap into named GLOBAL cardset( unless blank).
    if(.not. cpsio_is_blank(scrap)) call cardset_put_card(cardsetptr,scrap)
    ! if self_ended_tag, unset the in_tagged-area state.
    if(self_ended_tag) in_tagged_area = .false.
    in_global_header = .true.
  else
    ! no prolog on the file
    section_name = 'GLOBAL'
    section_type = 'HEADER'
    in_tagged_area=.true.
    in_global_header = .true.
    ! Create the Global cardset for this file.
    cardsetptr => & 
      cardsetlist_create_new_cardset(file(lun)%p%sectionlist%cardsetlist, &
      trim(section_name))
    call cpsio_init_section(lun,section_name,section_type)
    call cardset_put_card(cardsetptr,record)
  endif
else ! no pretag, this is a data line already!!!
    in_data=.true.
endif
!-------------Now we have established the PROLOG presense, have read it in, or
!-------------have read in the first header card, or
!-------------have read in the first data card.

!-------------Read in the rest of the header, stopping on first data line.
!-------------Stash the header into individual cardsets, by name...


do            !--- until we reach the "data" section of the file.
  if(in_data)exit
  ierr=cpsio_get_logical_recd(lun,record,pretagged)
  if (ierr == CPSIO_ERROR .or. ierr == CPSIO_EOF ) exit !Error on read
  !We now have a logical record ready.
  !Do something with the logical record.

  PRETAGGD: if (pretagged) then
    !check for tag telling me what to do
    call cpsio_extract_tag( &
      record,tag_type,tag_name,tagged,self_ended_tag,scrap)
    if(tag_name == 'GLOBAL' .and. tag_type == 'HEADER' .and. in_global_header) &
    then
      in_tagged_area = .true. 
    endif
    TAGGD: if(tagged) then ! -- must handle the tag --
      if(in_tagged_area) then
        if(trim(tag_type)=='END' .and. trim(tag_name)==trim(section_name)) then
          in_tagged_area=.false.
          in_global_header = .false. ! on first end tag, kill the global sect.
          section_name=NULL
          section_type=NULL
        elseif(trim(tag_type) == 'HEADER' .and. in_global_header) then
          ! don't pass the header.
          if(.not. cpsio_is_blank(scrap) ) &
            call cardset_add_card(cardsetptr,trim(scrap))
        else
          ! pass the entire record (less pretag) to the cardset.
          call cardset_add_card(cardsetptr,trim(record))
        endif
      else ! not in tagged area
        !test for global tag.
        if (trim(tag_type) /= 'END' ) then
          ! tag must be NAMED , DATA, or HEADER type.
          in_tagged_area=.true.
          section_name = tag_name
          section_type=tag_type
          ! see if name already existing, if not, 
          ! create a new cardset.
          cardsetptr => & 
            cardsetlist_find_cardset(file(lun)%p%sectionlist%cardsetlist, &
            trim(section_name))
          if(.not. associated(cardsetptr) ) then
            cardsetptr => & 
              cardsetlist_create_new_cardset( &
              file(lun)%p%sectionlist%cardsetlist,trim(section_name))
            call cpsio_init_section(lun,section_name,section_type)
            if(trim(section_type) == 'NAMED') then 
              !call cardset_put_card(cardsetptr,record)
              ! 8/13/99 don't add the entire card for named sections.
              if(.not. cpsio_is_blank(scrap) ) & 
                call cardset_put_card(cardsetptr,scrap)
            else
              if(.not. cpsio_is_blank(scrap) ) & 
                call cardset_put_card(cardsetptr,scrap)
            endif
          else
            ! continuing in a section, adding cards.
            if(.not. cpsio_is_blank(scrap)) &
              call cardset_add_card(cardsetptr,trim(scrap))
          endif

          if(self_ended_tag) in_tagged_area = .false. ! but keep section name
                                                      ! and section type.
          if(trim(tag_type) == 'DATA') in_data = .true.
        else    
          call cpsio_errmsg('Cpsio Warning: End tag found out of place.')
        endif
      endif
    else ! TAGGD line is not tagged.
      if(in_tagged_area) then
        ! Add card to current cardset.
        call cardset_add_card(cardsetptr,trim(record))
      else
        if(in_global_header) then
          call cardset_add_card(cardsetptr,trim(record))
        else
          !This must be a comment card.
        endif
      endif
    endif TAGGD
  else ! PRETAGGD Not pretagged ...
  ! Skip blank lines.
    if(cpsio_is_blank(record)) cycle
    ! out of pretagged section and not blank, treat as data line.
    ! no header info is allowed to follow.
    in_tagged_area = .false.    ! no tag found.
    section_type = 'DATA'        ! in some data section. 
    in_data=.true.
  endif  PRETAGGD

end do
! Now determine how many sections you have that are HEADER
! Allocate memory for a section pointer for each, associate the pointers and
! load the structures from the corresponding cardsets.
! ==== Here we will load all header sections with default keywords and see if
! ==== any have been overwritten.

call cpsio_load_sectiondefs(lun)

! set pointer to this record in file.
file(lun)%p%starting_data_record=file(lun)%p%next_abs_record
call cio_fbackspace(lun)

if(mode(1:1) == 'a') status = cio_fseek(lun,0,2) ! position to end of file.
return

! error handling
9999 continue
     !call cpsio_errmsg( &
     !'Error from cpsio_open: error opening '//trim(filename)//'.')
     !status    =cpsio_close(lun)
status    =CPSIO_ERROR
return
end function cpsio_open

function cpsio_test(lun) result (is_cpsio)
  integer, intent(in)          :: lun
  logical                      :: is_cpsio
  !----------
  if (associated (file(lun)%p) ) then
    is_cpsio = .true.
  else
    is_cpsio = .false.
  endif
end function cpsio_test


integer function cpsio_close(lun,remove,keep_open)
integer,intent(in)             :: lun  ! logical unit number
logical,intent(in),optional    :: remove
logical,intent(in),optional    :: keep_open
integer                        :: local_status  

!---- local vars
logical                        :: local_remove,local_keep_open
integer                        :: cpsio_lun  

if(present(remove)) then
  local_remove = remove
else
  local_remove = .false.
endif

if(present(keep_open)) then
  local_keep_open = keep_open
else
  local_keep_open = .false.
endif

local_status = -1 
cpsio_close  = -1

if(.not. associated(file(lun)%p) ) then
  !call cpsio_errmsg('Error from cpsio_close: file pointer not associated.')
else
  cpsio_lun = file(lun)%p%lun
  if(cpsio_lun /= lun) then
    !call cpsio_errmsg('Error from cpsio_close: unit number not allocated.')
  else
    if(cpsio_delete(file(lun)%p) /= 0) &
    call cpsio_errmsg('Error from cpsio_close: failure in cpsio_delete.')
  endif
endif
if(.not. local_keep_open) then
  local_status = cio_fclose(lun,local_remove)
else
  local_status = 0
  cpsio_close  = 0
endif

if(local_status /= 0 ) then 
  call cpsio_errmsg('Error from cpsio_close: iostat on close is bad.')
else
  cpsio_close=0
endif

end function cpsio_close


integer function cpsio_number_sections (lun) result (number_sections)
  ! return -1 on error, else return the number of sections found in the file.
  integer, intent(in)         :: lun ! logical unit number
  number_sections =  & 
  cardsetlist_num_cardsets(file(lun)%p%sectionlist%cardsetlist)
  return
end function cpsio_number_sections

integer function cpsio_get_sectionlist(lun,sectionlist) result(status)
  integer, intent(in)              :: lun             ! logical unit number
  character(len=* )   ,intent(out) :: sectionlist(:)  ! a list of section names.
  ! Local variables ===============
  integer                          :: i,number_sections
  type(cardset_struct),pointer     :: cardset
  
  status = 0
  number_sections = cpsio_number_sections(lun)
  do i = 1, number_sections
    cardset => cardsetlist_get_cardset(file(lun)%p%sectionlist%cardsetlist,i)
    sectionlist(i) = ' '
    call cardset_get_name(cardset,sectionlist(i))
  end do
  
  if (number_sections < 0) status=-1
  
  return
end function cpsio_get_sectionlist

integer function cpsio_number_keywords(lun,section_name) result(number_keywords)
   ! return number of keywords for a data section
   integer,intent(in)         :: lun              ! fortran logical unit number
   character(len=*),intent(in),optional :: section_name
                                                  ! section name where located
   ! ---------------------local variables.

   character(len=CARDSET_LENGTH)        :: local_section_name   
   type(cardset_struct),pointer         :: cardset
                                                  ! cardset where section kept.

                                                  ! status of allocate.
   type(cardsetlist_struct),pointer     :: cardsetlist      !

   number_keywords = CPSIO_ERROR
   if(present(section_name)) then
     local_section_name = trim(section_name)
   else
     local_section_name = 'GLOBAL'
   endif

   if(.not.associated(file(lun)%p)) then
     return
   endif

   if(.not.associated(file(lun)%p%sectionlist%cardsetlist)) then
     return
   endif

   cardsetlist =>file(lun)%p%sectionlist%cardsetlist
   cardset => cardsetlist_find_cardset(cardsetlist,local_section_name)
   if(.not.associated(cardset) ) then
     return
   endif

   number_keywords = cardset_num_keywords(cardset)

   return
end function cpsio_number_keywords

integer function cpsio_get_keywordlist( &
    lun,keywordlist,section_name) result(status)
  integer,intent(in)        :: lun   ! fortran logical unit number
  character(len=*),dimension(:),intent(out) :: keywordlist
  character(len=*),intent(in), optional     :: section_name  
                                     ! section name where located
   !  -------------- local variables -----------------
   integer                       :: number_keywords,i
   character(len=CARDSET_LENGTH) :: local_section_name  
   type(cardset_struct),pointer  :: cardset

   type(cardsetlist_struct),pointer :: cardsetlist      !

   if(present(section_name)) then
     local_section_name = section_name
   else
     local_section_name = 'global'
   endif
   call string_to_upper(local_section_name)

   ! -- set up for error conditions that may occur.
   status=CPSIO_ERROR
   number_keywords = cpsio_number_keywords(lun,local_section_name)
   if(number_keywords <= 0 ) return

   cardsetlist =>file(lun)%p%sectionlist%cardsetlist
   cardset => cardsetlist_find_cardset(file(lun)%p%sectionlist%cardsetlist, &
              local_section_name)
   if(.not.associated(cardset)) return

   status=0
   do i = 1, number_keywords
     keywordlist(i) = cardset_get_keyword(cardset,i)
   end do
   return
end function cpsio_get_keywordlist

integer function cpsio_get_keyword_char_scalar &
  (lun,keyword,value,section_name) result(status)
! purpose:       return value of a keyword in an optional header section.
!                if not present, status=-1 value = NULL
! integer        status 0 = found keyword. CPSIO_MISSING = not found.
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  character(len=*),intent(inout)             :: value
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------
  integer                                    :: keyword_status,nelements
  character(len=CARDSET_LENGTH)              :: local_section_name,errmsg
  type(cardset_struct),pointer               :: cardset
!
  if(present(section_name)) then
    local_section_name = section_name
  else
    local_section_name = 'global'
  endif
  call string_to_upper(local_section_name)
  
  ! -- set up for error conditions that may occur.
  status=CPSIO_MISSING
  cardset =>  & 
    cardsetlist_find_cardset(file(lun)%p%sectionlist%cardsetlist, &
    local_section_name)
  if(.not. associated(cardset)) return
  if(.not. cardset_keyword_present(cardset,keyword) ) return
  nelements = cardset_num_elements(cardset,keyword)
  keyword_status = cardset_nature(cardset,keyword)
  select case (keyword_status)
    case(CARDSET_MISSING) ; return
    case(CARDSET_ARRAY )
      call cpsio_errmsg('cpsio_get_keyword: &
      &keyword is array, you asked for scalar')
      call cardset_get_element (cardset,keyword,1,value,errmsg)
    case(CARDSET_SCALAR)
      call cardset_get_scalar(cardset,keyword,value,errmsg)
    case default ;          return
  end select
  if(errmsg == '') then 
    status=0
  else
    call cpsio_errmsg(errmsg)
  endif
  return
end function cpsio_get_keyword_char_scalar

integer function cpsio_get_keyword_nelem (lun,keyword,section_name) &
result(nelements)
  integer,intent(in)                         :: lun
  character(len=*),intent(in)                :: keyword
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------

  character(len=CARDSET_LENGTH)              :: local_section_name  
  type(cardset_struct),pointer               :: cardset

  if(present(section_name)) then
    local_section_name = section_name
  else
    local_section_name = 'global'
  endif
  call string_to_upper(local_section_name)
  nelements = 0
  cardset =>  & 
    cardsetlist_find_cardset(file(lun)%p%sectionlist%cardsetlist, &
    local_section_name)
  if(.not. associated(cardset)) return
  if(.not. cardset_keyword_present(cardset,keyword) ) return
  nelements = cardset_num_elements(cardset,keyword)
  return
end function cpsio_get_keyword_nelem

integer function cpsio_get_keyword_char_array &
  (lun,keyword,value,nelements,section_name) &
                 result(status)
! purpose:       return value of a keyword in an optional header section.
!                if not present, status=-1 value = NULL
! integer        status 0 = found keyword. CPSIO_MISSING = not found.
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  integer, intent(out)                       :: nelements
  character(len=*),intent(inout)             :: value(:)
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------
  integer                                    :: keyword_status
  character(len=CARDSET_LENGTH)              :: local_section_name,errmsg
  type(cardset_struct),pointer               :: cardset
!
  if(present(section_name)) then
    local_section_name = section_name
  else
    local_section_name = 'global'
  endif
  call string_to_upper(local_section_name)
  
  ! -- set up for error conditions that may occur.
  status=CPSIO_MISSING
  cardset =>  & 
    cardsetlist_find_cardset(file(lun)%p%sectionlist%cardsetlist, &
    local_section_name)
  if(.not. associated(cardset)) return
  if(.not. cardset_keyword_present(cardset,keyword) ) return
  nelements = cardset_num_elements(cardset,keyword)
  if(nelements > size(value)) then
    status = CPSIO_ERROR
    return
  endif
  keyword_status = cardset_nature(cardset,keyword)
  select case (keyword_status)
    case(CARDSET_MISSING) ; return
    case(CARDSET_ARRAY )
      call cardset_get_array (cardset,keyword,value,nelements,errmsg)
    case(CARDSET_SCALAR)
      call cpsio_errmsg('cpsio_get_keyword: &
      &keyword is scalar, you asked for array')
      call cardset_get_scalar(cardset,keyword,value(1),errmsg)
      nelements=1
    case default ;          return
  end select
  if(errmsg == '') then 
    status=0
  else
    call cpsio_errmsg(errmsg)
  endif
  return
end function cpsio_get_keyword_char_array

integer function cpsio_get_keyword_real_scalar &
  (lun,keyword,value,section_name) &
  result(status)
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  real     , intent(inout)                   :: value
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------
  character(len=CPSIO_FIELD_SIZE)            :: cvalue
!
  if(present(section_name)) then
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue,section_name)
  else
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue)
  endif
  if(status /= CPSIO_OK) return
  call string_cc2ff(cvalue,value)
end function cpsio_get_keyword_real_scalar

integer function cpsio_get_keyword_real_array &
(lun,keyword,value,nelements,section_name) &
result(status)
  ! purpose:       return value of a keyword in an optional header section.
  !                if not present, status=-1 value = NULL
  ! integer        status 0 = found keyword. CPSIO_MISSING = not found.
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  integer, intent(out)                       :: nelements
  real,intent(inout)                         :: value(:)
  character(len=*),intent(in),optional       :: section_name
  !------ Local variables.-----------------
  integer                                    :: i
  character(len=CPSIO_FIELD_SIZE)            :: cvalue(1:size(value)) 
  if(present(section_name) ) then
    status = & 
    cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements,section_name)
  else
    status = cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements)
  endif
  if(status /= CPSIO_OK) return
  do i = 1,nelements
    call string_cc2ff(cvalue(i),value(i))
  end do
end function cpsio_get_keyword_real_array

integer function cpsio_get_keyword_int_scalar &
  (lun,keyword,value,section_name) &
  result(status)
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  integer  , intent(inout)                   :: value
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------
  character(len=CPSIO_FIELD_SIZE)            :: cvalue
!
  if(present(section_name)) then
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue,section_name)
  else
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue)
  endif
  if(status /= CPSIO_OK) return
  call string_cc2ii(cvalue,value)
end function cpsio_get_keyword_int_scalar

integer function cpsio_get_keyword_int_array &
(lun,keyword,value,nelements,section_name) &
result(status)
  ! purpose:       return value of a keyword in an optional header section.
  !                if not present, status=-1 value = NULL
  ! integer        status 0 = found keyword. CPSIO_MISSING = not found.
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  integer, intent(out)                       :: nelements
  integer,intent(inout)                      :: value(:)
  character(len=*),intent(in),optional       :: section_name
  !------ Local variables.-----------------
  integer                                    :: i
  character(len=CPSIO_FIELD_SIZE)            :: cvalue(1:size(value)) 
  if(present(section_name) ) then
    status = & 
    cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements,section_name)
  else
    status = cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements)
  endif
  if(status /= CPSIO_OK) return
  do i = 1,nelements
    call string_cc2ii(cvalue(i),value(i))
  end do
end function cpsio_get_keyword_int_array

integer function cpsio_get_keyword_dble_scalar &
  (lun,keyword,value,section_name) &
  result(status)
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  double precision     , intent(inout)       :: value
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------
  character(len=CPSIO_FIELD_SIZE)            :: cvalue
!
  if(present(section_name)) then
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue,section_name)
  else
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue)
  endif
  if(status /= CPSIO_OK) return
  call string_cc2dd(cvalue,value)
end function cpsio_get_keyword_dble_scalar

integer function cpsio_get_keyword_dble_array &
(lun,keyword,value,nelements,section_name) &
result(status)
  ! purpose:       return value of a keyword in an optional header section.
  !                if not present, status=-1 value = NULL
  ! integer        status 0 = found keyword. CPSIO_MISSING = not found.
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  integer, intent(out)                       :: nelements
  double precision,intent(inout)             :: value(:)
  character(len=*),intent(in),optional       :: section_name
  !------ Local variables.-----------------
  integer                                    :: i
  character(len=CPSIO_FIELD_SIZE)            :: cvalue(1:size(value)) 
  if(present(section_name) ) then
    status = & 
    cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements,section_name)
  else
    status = cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements)
  endif
  if(status /= CPSIO_OK) return
  do i = 1,nelements
    call string_cc2dd(cvalue(i),value(i))
  end do
end function cpsio_get_keyword_dble_array

integer function cpsio_get_keyword_log_scalar &
  (lun,keyword,value,section_name) &
  result(status)
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  logical     , intent(inout)                :: value
  character(len=*),intent(in),optional       :: section_name
!------ Local variables.-----------------
  character(len=CPSIO_FIELD_SIZE)            :: cvalue
!
  if(present(section_name)) then
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue,section_name)
  else
    status = cpsio_get_keyword_char_scalar(lun,keyword,cvalue)
  endif
  if(status /= CPSIO_OK) return
  call string_cc2ll(cvalue,value)
end function cpsio_get_keyword_log_scalar

integer function cpsio_get_keyword_log_array &
(lun,keyword,value,nelements,section_name) &
result(status)
  ! purpose:       return value of a keyword in an optional header section.
  !                if not present, status=-1 value = NULL
  ! integer        status 0 = found keyword. CPSIO_MISSING = not found.
  integer, intent(in)                        :: lun
  character(len=*),intent(in)                :: keyword
  integer, intent(out)                       :: nelements
  logical,intent(inout)                      :: value(:)
  character(len=*),intent(in),optional       :: section_name
  !------ Local variables.-----------------
  integer                                    :: i
  character(len=CPSIO_FIELD_SIZE)            :: cvalue(1:size(value)) 
  if(present(section_name) ) then
    status = & 
    cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements,section_name)
  else
    status = cpsio_get_keyword_char_array(lun,keyword,cvalue,nelements)
  endif
  if(status /= CPSIO_OK) return
  do i = 1,nelements
    call string_cc2ll(cvalue(i),value(i))
  end do
end function cpsio_get_keyword_log_array

integer function cpsio_position_file(lun,position,section_name) result (status)
  ! status = -1 error ,  0 = ok
      ! position file to read specific record
   integer,intent(in) :: lun                     ! fortran logical unit number
   character(len=*),intent(in),optional :: section_name !sect.name where located
   integer,intent(in) :: position
      ! if position = 0, position before first record
      ! if position < 0 or > number of records, position after end of records
      ! if position = n, position to read record n
   !---------- local variables.----------
   character(len=CARDSET_LENGTH)        :: local_section_name
   character(len=255)                   :: card,scrap
   integer                              ::      i 
   character(len= 80)                   :: tag_name, tag_type
   logical                              :: tagged,self_ended_tag
   integer                              :: local_position
   logical                              :: pretagged

   type(cpsio_file),pointer             :: fptr
   ! ---------- rewind the file and read up to the starting data record.
   status = CPSIO_OK
   fptr => file(lun)%p
   local_position = position
   call cio_frewind(lun)
   fptr%next_abs_record = 1
   do while ( fptr%next_abs_record < fptr%starting_data_record)
     status = cpsio_get_logical_recd(lun,card,pretagged)
     if (status == CPSIO_ERROR .or. status == CPSIO_EOF ) return
   enddo
   fptr%next_abs_record = fptr%next_abs_record - 1
   call cio_fbackspace(lun)
   !------- Now at start of data sections

   if(present(section_name)) then
     local_section_name =trim(section_name)
   else
     local_section_name = 'global'
   endif
   call string_to_upper(local_section_name)
   !-------See if a matching header description exists -------

   !-------If no header match, then move to EOF         ------
   !-------If header match, then initially move to Beginning of SECTION -1
   !-------If no header specified, then move to Beginning of Data - 1 
   !       (we are there) 
   !----- Find the section -----
   fptr%current_data_section = local_section_name
   if(present(section_name) .and. local_section_name /= 'GLOBAL') then
     do 
       status = cpsio_get_logical_recd(lun,card,pretagged)
       if (status == CPSIO_ERROR .or. status == CPSIO_EOF ) return
       !--- look for tag that matches top of section.
       if(pretagged) then 
         call cpsio_extract_tag( &
           card,tag_type,tag_name,tagged,self_ended_tag,scrap)
         if(tagged .and. .not. self_ended_tag) then
           if(tag_type == 'DATA') then
             call string_to_upper(tag_name)
             if(trim(tag_name) == trim(local_section_name) ) then
               exit
             endif 
             ! then found start of this section.
           elseif(tag_type == 'NAMED') then
             !-- look for a match in the header tag list.
             if(cpsio_found_matching_header(lun,local_section_name) .and. &
                trim(tag_name) == trim(local_section_name) ) exit 
             ! then found start of this section.
           endif
         endif
       endif
     end do
   else
     do
       status = cpsio_get_logical_recd(lun,card,pretagged)
       if (status == CPSIO_ERROR .or. status == CPSIO_EOF ) then
         !----- We get here if someone asks for the global section or no
         !----- section and has no tag telling us where to start reading.
         call cio_frewind(lun)
         fptr%next_abs_record = 1
         do while ( fptr%next_abs_record < fptr%starting_data_record)
           status = cpsio_get_logical_recd(lun,card,pretagged)
           if (status == CPSIO_ERROR .or. status == CPSIO_EOF )return
         enddo
         fptr%next_abs_record = fptr%next_abs_record - 1
         call cio_fbackspace(lun)
         !------- Now at start of data sections
         exit
       endif
       !--- look for tag that matches top of section. (global tag or null tag)
       !--- here is where i am going to find a <DTA_global> or <DTA_> tag.
       if(pretagged) then 
         call cpsio_extract_tag( &
           card,tag_type,tag_name,tagged,self_ended_tag,scrap)
         if(tagged .and. .not. self_ended_tag) then
           if(tag_type == 'DATA') then
             call string_to_upper(tag_name)
             if(trim(tag_name) == trim(local_section_name) ) then
               exit
             endif 
             ! then found start of this section.
           elseif(tag_type == 'NAMED') then
             !-- look for a match in the header tag list.
             if(cpsio_found_matching_header(lun,local_section_name) .and. &
                trim(tag_name) == trim(local_section_name) ) exit 
             ! then found start of this section.
           endif
         endif
       endif
     end do

   endif

!  ----- now position self inside the data section just before 
!  ----- record = "position"

   do i = 1, position-1
     status = cpsio_get_logical_recd(lun,card,pretagged)
     if (status == CPSIO_ERROR ) return
     if (status == CPSIO_EOF ) exit
     if(present(section_name) .and. local_section_name /= 'GLOBAL' &
        .and. pretagged) then
       call cpsio_extract_tag( &
         card,tag_type,tag_name,tagged,self_ended_tag,scrap)
       if(tagged) then
         status = CPSIO_EOS
         local_section_name = 'global'
         call string_to_upper(local_section_name)
         fptr%current_data_section = local_section_name
         exit ! can't have a tag inside a data section,
              ! must be at end of section.
       endif
     else
         local_section_name = 'global'
         call string_to_upper(local_section_name)
         fptr%current_data_section = local_section_name
     end if
   end do
   return
end function cpsio_position_file

integer function cpsio_find_position(lun,position,section_name) result (status)
  !status = -1 error ,  0 = ok
  ! position file to read specific record
  integer,intent(in)                   :: lun    ! fortran logical unit number
  integer,intent(in)                   :: position ! abs or rel (rel if sect.)
  character(len=*),intent(in),optional :: section_name ! section name (opt)
   if(present(section_name)) then
     status = file(lun)%p%next_abs_record
   else
     status = file(lun)%p%next_abs_record
   endif
   return
end function cpsio_find_position

integer function cpsio_get_value_by_nmbr_real(lun,field_number,field_value ) &
  result (status)
  integer,intent(in)    :: lun !logical unit number
  integer,intent(in)    :: field_number
  real,intent(out)      :: field_value
  ! LOCAL
  type(cpsio_section),pointer  :: section
  type(cpsio_file), pointer    :: fptr
  nullify (section) ! jpa
  status = CPSIO_MISSING
  if(field_number == 0 ) return
  fptr =>file(lun)%p
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  status = cpsio_load_field(lun,section,field_number)
  if(status /= 0) return
  if(trim(section%field(field_number)) == trim(section%null) ) then 
    field_value = FNIL
  else
    read(section%field(field_number),*,iostat=status)field_value
    if(status /= 0) then
      !write(stdout,*)'CPSIO_GET_VALUE: error reading field ',field_number, & 
      !' on unit ',lun, ' iostat= ',status,' Field_value = ',         &
      !trim(section%field(field_number))
      status = CPSIO_ERROR
    endif
  endif
  ! === if error, we may need to update the section pointer, hence the next line
  if(status /= CPSIO_OK) fptr%initial_read_of_data_section=.true.
  return
end function cpsio_get_value_by_nmbr_real

integer function cpsio_get_value_by_nmbr_int(lun,field_number,field_value ) &
result (status)
integer,intent(in)    :: lun !logical unit number
integer,intent(in)    :: field_number
integer,intent(out)   :: field_value
  ! LOCAL
  type(cpsio_section),pointer  :: section
  type(cpsio_file), pointer    :: fptr
  nullify (section) ! jpa
  status = CPSIO_MISSING
  if(field_number == 0 ) return
  fptr =>file(lun)%p
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  status = cpsio_load_field(lun,section,field_number)
  if(status /= 0) return
  if(trim(section%field(field_number)) == trim(section%null) ) then 
    field_value = INIL
  else
    read(section%field(field_number),*,iostat=status)field_value
    if(status /= 0) then
      !write(stdout,*)'CPSIO_GET_VALUE: error reading field ',field_number, & 
      !' on unit ',lun, ' iostat= ',status,' Field_value = ',         &
      !trim(section%field(field_number))
      status = CPSIO_ERROR
    endif
  endif
  ! === if error, we may need to update the section pointer, hence the next line
  if(status /= CPSIO_OK) fptr%initial_read_of_data_section=.true.
  return
end function cpsio_get_value_by_nmbr_int

integer function cpsio_get_value_by_nmbr_dble(lun,field_number,field_value ) &
result (status)
integer,intent(in)           :: lun !logical unit number
integer,intent(in)           :: field_number
double precision,intent(out) :: field_value
  ! LOCAL
  type(cpsio_section),pointer  :: section
  type(cpsio_file), pointer    :: fptr
  nullify (section) ! jpa
  status = CPSIO_MISSING
  if(field_number == 0 ) return
  fptr =>file(lun)%p
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  status = cpsio_load_field(lun,section,field_number)
  if(status /= 0) return
  if(trim(section%field(field_number)) == trim(section%null) ) then 
    field_value = DNIL
  else
    read(section%field(field_number),*,iostat=status)field_value
    if(status /= 0) then
      !write(stdout,*)'CPSIO_GET_VALUE: error reading field ',field_number, & 
      !' on unit ',lun, ' iostat= ',status,' Field_value = ',         &
      !trim(section%field(field_number))
      status = CPSIO_ERROR
    endif
  endif
  ! === if error, we may need to update the section pointer, hence the next line
  if(status /= CPSIO_OK) fptr%initial_read_of_data_section=.true.
  return
end function cpsio_get_value_by_nmbr_dble

integer function cpsio_get_value_by_nmbr_char(lun,field_number,field_value ) &
result (status)
integer,intent(in)           :: lun !logical unit number
integer,intent(in)           :: field_number
character(len=*),intent(out) :: field_value
  ! LOCAL
  type(cpsio_section),pointer  :: section
  type(cpsio_file), pointer    :: fptr
  nullify (section) ! jpa
  status = CPSIO_MISSING
  if(field_number == 0 ) return
  fptr =>file(lun)%p
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  status = cpsio_load_field(lun,section,field_number)
  if(status /= 0) return
  if(trim(section%field(field_number)) == trim(section%null) ) then 
    field_value = CNIL
  else
    field_value = section%field(field_number)
    status = 0
    !read(section%field(field_number),*,iostat=status)field_value
    if(status /= 0) then
      !write(stdout,*)'CPSIO_GET_VALUE: error reading field ',field_number, & 
      !' on unit ',lun, ' iostat= ',status,' Field_value = ',         &
      !trim(section%field(field_number))
      status = CPSIO_ERROR
    endif
  endif
  ! === if error, we may need to update the section pointer, hence the next line
  if(status /= CPSIO_OK) fptr%initial_read_of_data_section=.true.
  return
end function cpsio_get_value_by_nmbr_char

integer function cpsio_get_value_by_nmbr_log(lun,field_number,field_value ) &
result (status)
integer,intent(in)    :: lun !logical unit number
integer,intent(in)    :: field_number
logical,intent(out)   :: field_value
  ! LOCAL
  type(cpsio_section),pointer  :: section
  type(cpsio_file), pointer    :: fptr
  nullify (section) ! jpa
  status = CPSIO_MISSING
  if(field_number == 0 ) return
  fptr =>file(lun)%p
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  status = cpsio_load_field(lun,section,field_number)
  if(status /= 0) return
  if(trim(section%field(field_number)) == trim(section%null) ) then 
    field_value = LNIL
  else
    read(section%field(field_number),*,iostat=status)field_value
    if(status /= 0) then
      !write(stdout,*)'CPSIO_GET_VALUE: error reading field ',field_number, & 
      !' on unit ',lun, ' iostat= ',status,' Field_value = ',         &
      !trim(section%field(field_number))
      status = CPSIO_ERROR
    endif
  endif
  ! === if error, we may need to update the section pointer, hence the next line
  if(status /= CPSIO_OK) fptr%initial_read_of_data_section=.true.
  return
end function cpsio_get_value_by_nmbr_log

integer function cpsio_get_value_by_name_real(lun, field_name, field_value ) &
result (status)
  integer,intent(in)          :: lun !logical unit number
  character(len=*),intent(in) :: field_name
  real,intent(out)            :: field_value
  ! ------------------ local variables --------------------
  type(cpsio_file), pointer                    :: fptr
  type(cpsio_section),pointer,save             :: section

  ! === set file pointer to this unit.
  fptr =>file(lun)%p
  ! === update the "section" pointer if necessary (may do nothing.  This
  ! === uses the logical fptr%initial_read_of_data_section and the value
  ! === fptr%current_data_section to determine if an update is needed.
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  ! === Test for error condition.
  status = cpsio_get_value_by_nmbr_real(lun, &
           cpsio_find_field_by_name(section,field_name),field_value)
  return
end function cpsio_get_value_by_name_real

integer function cpsio_get_value_by_name_int(lun, field_name, field_value )  &
result (status)
integer,intent(in)          :: lun !logical unit number
character(len=*),intent(in) :: field_name
integer,intent(out)         :: field_value
  ! ------------------ local variables --------------------
  type(cpsio_file), pointer                    :: fptr
  type(cpsio_section),pointer,save             :: section

  ! === set file pointer to this unit.
  fptr =>file(lun)%p
  ! === update the "section" pointer if necessary (may do nothing.  This
  ! === uses the logical fptr%initial_read_of_data_section and the value
  ! === fptr%current_data_section to determine if an update is needed.
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  ! === Test for error condition.
  status = cpsio_get_value_by_nmbr_int(lun, &
           cpsio_find_field_by_name(section,field_name),field_value)
  return
end function cpsio_get_value_by_name_int

integer function cpsio_get_value_by_name_dble(lun, field_name, field_value ) &
result (status)
integer,intent(in)                 :: lun !logical unit number
character(len=*),intent(in)        :: field_name
double precision,intent(out)       :: field_value
  ! ------------------ local variables --------------------
  type(cpsio_file), pointer                    :: fptr
  type(cpsio_section),pointer,save             :: section

  ! === set file pointer to this unit.
  fptr =>file(lun)%p
  ! === update the "section" pointer if necessary (may do nothing.  This
  ! === uses the logical fptr%initial_read_of_data_section and the value
  ! === fptr%current_data_section to determine if an update is needed.
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  ! === Test for error condition.
  status = cpsio_get_value_by_nmbr_dble(lun, &
           cpsio_find_field_by_name(section,field_name),field_value)
  return
end function cpsio_get_value_by_name_dble

integer function cpsio_get_value_by_name_char(lun, field_name, field_value ) &
result (status)
integer,intent(in)          :: lun !logical unit number
character(len=*),intent(in) :: field_name
character(len=*),intent(out):: field_value
  ! ------------------ local variables --------------------
  type(cpsio_file), pointer                    :: fptr
  type(cpsio_section),pointer,save             :: section

  ! === set file pointer to this unit.
  fptr =>file(lun)%p
  ! === update the "section" pointer if necessary (may do nothing.  This
  ! === uses the logical fptr%initial_read_of_data_section and the value
  ! === fptr%current_data_section to determine if an update is needed.
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  ! === Test for error condition.
  status = cpsio_get_value_by_nmbr_char(lun, &
           cpsio_find_field_by_name(section,field_name),field_value)
  return
end function cpsio_get_value_by_name_char

integer function cpsio_get_value_by_name_log(lun, field_name, field_value )  &
result (status)
integer,intent(in)          :: lun !logical unit number
character(len=*),intent(in) :: field_name
logical,intent(out)         :: field_value
  ! ------------------ local variables --------------------
  type(cpsio_file), pointer                    :: fptr
  type(cpsio_section),pointer,save             :: section

  ! === set file pointer to this unit.
  fptr =>file(lun)%p
  ! === update the "section" pointer if necessary (may do nothing.  This
  ! === uses the logical fptr%initial_read_of_data_section and the value
  ! === fptr%current_data_section to determine if an update is needed.
  status = cpsio_initial_section_read(lun,section)
  if(status /= 0) return
  ! === Test for error condition.
  status = cpsio_get_value_by_nmbr_log(lun, &
           cpsio_find_field_by_name(section,field_name),field_value)
  return
end function cpsio_get_value_by_name_log

integer function cpsio_number_cards(lun,section_name,key_field,key_value) &
  result (num_cards)
  integer         ,intent(in)          :: lun          ! fortran lun
  character(len=*),intent(in),optional :: section_name ! section where located
  character(len=*),intent(in),optional :: key_field    ! name to use as key
  character(len=*),intent(in),optional :: key_value    ! value 2 match key with

  integer                              :: num_logical_recds_junk

  num_logical_recds_junk = cpsio_number_records(lun,section_name,key_field, &
                             key_value,num_cards)

  return
end function cpsio_number_cards

integer function cpsio_number_records(lun,section_name,key_field, &
  key_value,total_num_cards) result (num)
  integer,intent(in) :: lun                               ! fortran lun
  character(len=*),intent(in),optional :: section_name ! section where located
  character(len=*),intent(in),optional :: key_field    ! name to use as key
  character(len=*),intent(in),optional :: key_value    ! value 2 match key with
  integer,intent(out),optional         :: total_num_cards
  ! LOCAL
  character(len=CPSIO_RECORD_SIZE)     :: record
  type(cpsio_file),pointer             :: fptr




  integer                              :: status
  integer                              :: num_cards_in_recd

  num = 0
  status = CPSIO_OK
  fptr => file(lun)%p

  if(fptr%mode(1:1) == 'w') then
    if (present(total_num_cards)) then
      total_num_cards = 0
    endif
    return
  endif

  if(present(section_name) ) then
    status = cpsio_position_file(lun,1,section_name)
  else
    status = cpsio_position_file(lun,1)
  endif

  if (present(total_num_cards)) then
    total_num_cards = 0
  endif

  do
    if(present(section_name) .and. present(key_field) .and. present(key_value))&
    then
      status = cpsio_read_data(lun,record,section_name,key_field,key_value, &
        num_cards = num_cards_in_recd)
    elseif(present(section_name) .and. .not. present(key_field) ) then
      status = cpsio_read_data(lun,record,section_name, &
        num_cards = num_cards_in_recd)
    elseif(present(key_field) .and. .not. present(section_name) ) then
      status = cpsio_read_data(lun,record,key_field=key_field,&
               key_value=key_value, num_cards = num_cards_in_recd)
    else
      status = cpsio_read_data(lun,record, num_cards = num_cards_in_recd)
    endif
    if(status /= CPSIO_OK) exit
    num = num + 1

    if (present(total_num_cards)) then
      total_num_cards = total_num_cards + num_cards_in_recd
    endif

  end do

  if(present(section_name) ) then
    status = cpsio_position_file(lun,1,section_name)
  else
    status = cpsio_position_file(lun,1)
  endif

  return
end function cpsio_number_records

integer function cpsio_read_data(lun,record,section_name,key_field, &
  key_value,num_cards) result(status)
  integer,intent(in) :: lun                           ! fortran lun
  character(len=*),intent(out) :: record              ! the record as read in.
  character(len=*),intent(in),optional :: section_name! section where located
  character(len=*),intent(in),optional :: key_field   ! name to use as key
  character(len=*),intent(in),optional :: key_value   ! value to match key with
  integer,intent(out),optional         :: num_cards
  ! LOCAL
  type(cpsio_file),pointer             :: fptr
  type(cpsio_section),pointer,save     :: section
  character(len=CPSIO_FIELD_SIZE)      :: field_value
  integer                              :: field_number
  character(len=CPSIO_FIELD_SIZE)      :: local_section
  integer                              :: temp

  ! In case we hit error return.
  !
  if (present(num_cards)) then
    num_cards = 0
  endif

  status=CPSIO_ERROR
  ! === associate file pointer
  fptr => file(lun)%p
  ! === Find the section we are looking for, associate section pointer
  if(present(section_name)) then
    local_section = section_name
    call string_to_upper(local_section)
    if(associated(section) ) then
      if(local_section /= section%section_name) then
        section => cpsio_find_section(fptr%sectionlist,local_section)
        if (.not. associated(section) )  return
        fptr%current_data_section = local_section
        status=cpsio_position_file(lun,1,local_section)
        if (status /= CPSIO_OK) return
      endif
    else
      section => cpsio_find_section(fptr%sectionlist,local_section)
        if (.not. associated(section) )  return
        fptr%current_data_section = local_section
        status=cpsio_position_file(lun,1,local_section)
        if (status /= CPSIO_OK) return
    endif
  else  
    local_section = 'GLOBAL'
    ! === the default section is "global"
    section => cpsio_find_section(fptr%sectionlist,local_section)
    if (.not. associated(section) )  then ! we are probably not in a file
      !                                     that has cpsio header in it.
      !                                     so just read it.
      temp = len(record)
      status = cio_fgetline(record,temp,lun)
      if (present(num_cards)) then
        num_cards = 1
      endif
      return
    endif
  endif
  ! ==== I now am assured of a valid section pointer
  ! ==== check key field and key value
  !if(present(section_name) .and. present(key_field) .and. present(key_value) )&
  if( present(key_field) .and. present(key_value) ) then
    ! === get field number
    field_number = cpsio_find_field_by_name(section,key_field)
    if (field_number == 0 ) then ! === no field was found by that name in this
                                 ! === section.
      status = CPSIO_ERROR
      return
    endif
    status = CPSIO_OK
    do
      ! === if field_value is equal to the section's null token, 
      ! === then it is replaced with the null value.
      status = cpsio_get_value(lun,key_field,field_value)
      if (status /= CPSIO_OK) return
      if (field_value == key_value ) then
        ! === back up to re-read the record that contained the key field = key
        ! === value.
        call cio_fbackspace(lun)
        ! === re-read that record into the "record" field.
        ! === The key_value will NOT be replaced by the null value if it
        ! === equals the null token
        ! === in the returned record, BUT it will match because of the above
        ! ===  call to get_value.
        status = cpsio_get_next_data (lun,record,section%skip_blanks,num_cards)
        return
      endif
    end do
  else
    status = cpsio_get_next_data (lun,record,section%skip_blanks,num_cards)
  endif
  return
end function cpsio_read_data

function cpsio_write_str(lun,datacard) result (status)
  integer , intent(in)                      :: lun
  character(len=*),intent(in)               :: datacard
  integer                                   :: status
  !--- Local variables
  integer                                   :: nchars
  nchars =len(datacard)

  status = cio_fputline(datacard,nchars,lun)
  if(status <= 0 ) then 
    status = cpsio_error
  else
    status = cpsio_ok
  endif
  if(status /= 0 ) status = cpsio_error

end function cpsio_write_str

function cpsio_write_vec(lun,datacard) result (status)
  integer , intent(in)                      :: lun
  character(len=*),intent(in),dimension(:)  :: datacard
  integer                                   :: status
  !--- Local variables
  integer                                   :: nchars
  nchars=size(datacard)*len(datacard(1))

  status = cio_fputline(datacard,nchars,lun)
  if(status <= 0 ) then 
    status = cpsio_error
  else
    status = cpsio_ok
  endif

end function cpsio_write_vec

function cpsio_read_str(lun,datacard) result (status)
  integer , intent(in)                      :: lun
  character(len=*),intent(out)              :: datacard
  integer                                   :: status
  !--- Local variables
  integer                                   :: nchars
  nchars=len(datacard)

  status = cio_fgetline(datacard,nchars,lun)
  select case (status)
    case (cio_eof)   ; status = cpsio_eof
    case (cio_error) ; status = cpsio_error 
    case default
      if(status >= 0 ) then
        status  = cpsio_ok
      else
        status  = cpsio_error
      endif
  end select
end function cpsio_read_str

function cpsio_read_vec(lun,datacard) result (status)
  integer , intent(in)                      :: lun
  character(len=*),intent(out),dimension(:) :: datacard
  integer                                   :: status
  !--- Local variables
  integer                                   :: nchars
  nchars=size(datacard)*len(datacard(1))

  status = cio_fgetline(datacard,nchars,lun)
  select case (status)
    case (cio_eof)   ; status = cpsio_eof
    case (cio_error) ; status = cpsio_error 
    case default
      if(status >= 0 ) then
        status  = cpsio_ok
      else
        status  = cpsio_error
      endif
  end select

end function cpsio_read_vec

subroutine cpsio_rewind(lun)
  integer,intent(in) :: lun
  call cio_frewind(lun)
end subroutine cpsio_rewind

subroutine cpsio_backspace(lun)
  integer,intent(in) :: lun
  call cio_fbackspace(lun)
end subroutine cpsio_backspace

!
!====== EVERYTHING BELOW HERE SHOULD BE PRIVATE ===========

!
!-----------------------------LOGICAL PRIMITIVE FUNCTIONS--------------

logical function cpsio_found_matching_header(lun,section_name) result (found)
  ! Purpose: to match up a section_name with a previously stored header section.
  ! This allows us to find data sections to match up with header sections.
  integer , intent(in)                   :: lun
  character(len=*), intent(in)           :: section_name
  !--------LOCAL variables
  integer                                :: i,number_sections,ierr
  character(len=CARDSET_LENGTH), pointer :: sectionlist(:)

  number_sections = cpsio_number_sections(lun)
  allocate(sectionlist(number_sections))
  ierr = cpsio_get_sectionlist(lun,sectionlist)
  found = .false.
  do i = 1, number_sections
    if(sectionlist(i) == section_name ) then
      found = .true.
      exit
    endif
  end do
  return
end function cpsio_found_matching_header

logical function cpsio_is_blank(string)
character(len=*) , intent(in)        :: string
if (verify(string,WHITESPACE) == 0 .or. len_trim(string) == 0) then
  cpsio_is_blank=.true.
else
  cpsio_is_blank=.false.
endif
end function cpsio_is_blank

logical function cpsio_end_w_cont(string)
!------------------------------------------i
!Purpose: Determine if string terminates with continuation char.
character(len=*)    ,         intent(in) :: string
!----------local variables
integer                                  :: lastchr
lastchr=len_trim(string)
if(lastchr == 0) then
    ! 2 Dec 2003, Randy Selzler.  Handle the case of all blanks gracefully.
  cpsio_end_w_cont=.false.
else if(string(lastchr:lastchr) == CONTIN ) then
  cpsio_end_w_cont=.true.
else
  cpsio_end_w_cont=.false.
endif
return
end function cpsio_end_w_cont

logical function cpsio_is_pretag_present(lun,string)
!-----------------------------------------i----i
!Purpose: Determine if pretag is present at beginning of string)
character(len= *)   ,intent(in)     :: string
integer             ,intent(in)     :: lun

if(.not. file(lun)%p%pretag_defined) then
  cpsio_is_pretag_present=.false.
  return
elseif(file(lun)%p%pretag_defined .and. file(lun)%p%len_pretag == 0 ) then
  cpsio_is_pretag_present=.true.
  return
endif

if(index(string,file(lun)%p%pretag(:file(lun)%p%len_pretag) ) == 1 ) then
  cpsio_is_pretag_present=.true.
else
  cpsio_is_pretag_present=.false.
endif
return
end function cpsio_is_pretag_present

logical function cpsio_in_quoted_str(string,initial_state)
!-------------------------        -i        i
!Purpose: Determine if I am in a quoted string at end of "string"
character(len= *)   ,intent(in)  :: string
logical,intent(in),optional      :: initial_state
!---------local variables
integer                          :: i,nbr_quotechars

if(present(initial_state) ) then
  if(initial_state) then 
    nbr_quotechars = 1
  else 
    nbr_quotechars = 0
  endif
else
  nbr_quotechars = 0
endif

do i = 1,len(string)
  if(string(i:i) == QUOTE)nbr_quotechars=nbr_quotechars+1 
end do
if(mod(nbr_quotechars,2) == 1) then
  cpsio_in_quoted_str = .true.
else
  cpsio_in_quoted_str = .false.
endif
return
end function cpsio_in_quoted_str

!------------------------------Simple Functions-----------------------

subroutine cpsio_strip_pretag(lun,string)
!------------------------------i---io
!Purpose: To remove the pretag from a line.
!         (assumed to contain the pretag.)
!Warning: This removes the first "len(pretag)" characters whether 
!         the tag is present or not.
character(len= *)   ,intent(inout) :: string
integer             ,intent( in)   :: lun
if(cpsio_is_pretag_present(lun,string)) then
  string=string(file(lun)%p%len_pretag+1:)
endif
return
end subroutine cpsio_strip_pretag


recursive subroutine cpsio_strip_inline_commt(line,continued)
!-          --------------------------------io      i
!Purpose:  To remove any comments within the string "line"
!          as long as they are not "quoted".
character(len=* )   ,intent(inout)      :: line
logical, intent(in), optional           :: continued
! local variables
logical                                 :: in_string
integer                                 :: j,k
character(len=len(line))                :: newline      ! automatic character.


if(present(continued)) then
  in_string=continued
else
  in_string=.false.
endif

j=index(line,COMMENT_START)
if(j == 0) return
if(cpsio_in_quoted_str(line(1:j),in_string)) then
  k = index(line(j+1:),QUOTE)
  ! we are embedded within a quoted string, ignore the comment.
  if(k > 0) then ! more line left to parse for comments.
    newline=line(j+k+1:)
    call cpsio_strip_inline_commt(newline)
    line=line(:j+k)//trim(newline)
  endif
  return
endif

!-- search from back of string to front...

k=index(line,COMMENT_END,.true.)

if(k == 0) k=len(line)

line = line(1:j-1)//line(k+2:len(line))

if(present(continued)) then
  in_string=continued
else
  in_string=.false.
endif
call cpsio_strip_inline_commt(line,in_string)
return
end subroutine cpsio_strip_inline_commt

subroutine cpsio_extract_tag(record,tag_type,tag_name,tagged,self_ended,&
remainder)
!-------------------------i      o        o       o    o          o
!Purpose:  To read a record and return a tag if present, with info about it.
character(len= *)   ,intent(inout)        :: record
character(len= *)   ,intent(out)          :: tag_type,tag_name
logical,intent(out)                       :: tagged,self_ended
character(len= *)   ,intent(out),optional :: remainder
!---------------------Local variables
integer                                   :: i,j,k ! scratch integers.
character(len=3)                          :: ttype
self_ended=.false.
if( index(record,tag_begin) /= 1) then
  tagged=.false.
  tag_type=NULL
  tag_name=NULL
  if(present(remainder))remainder=record
  return
endif
!---------Tag found, now deal with it.------------- 
tagged=.true.
if( index(record,tag_terminate) == 1) then
  ! We are terminating a tagged section perhaps.
  tag_type='END'
  tag_name=trim(record(len(tag_terminate)+1:index(record,tag_end)-1))
  call string_to_upper(tag_name)
  if(len_trim(tag_name) == 0 .or.   &
     trim(tag_name) == 'HDR' .or.   &
     trim(tag_name) == 'DTA'      ) tag_name = 'GLOBAL'
  if(present(remainder)) remainder=NULL
  return
endif
!---------Tag is not an ending tag, but a start of section tag.
k = scan(record," ,>")
i=index(record,tag_self_terminate)
if(i>0) self_ended=.true.
j=index(record,tag_end)
tag_name=trim(record(len(tag_begin)+1:scan(record," ,>")-1))
call string_to_upper(tag_name)
tag_type=tag_name(1:3)
ttype=tag_type
select case(ttype)
case('HDR')
  tag_type='HEADER'
  tag_name=tag_name(5:)
case('CPS')
  ! --- prolog is just a special header... tag_type='PROLOG'
  tag_type='HEADER'
  tag_name='GLOBAL'
case('DTA')
  tag_type='DATA'
  tag_name=tag_name(5:)
case default
  tag_type='NAMED'
end select
if(tag_name == NULL) tag_name = 'GLOBAL'
if(present(remainder)) then
  if(i==0 .and. j == 0 ) then
    remainder=NULL
  else
    if(i == 0 ) i = j
    if(j == 0 ) j = i
    i=min(i,j)-1
    if(i > 0) then
      remainder=record(k+1:i)
    else
      remainder=NULL
    endif
  endif
endif
return
end subroutine cpsio_extract_tag

!-----------------------------COMPLEX FUNCTIONS/SUBROUTINES-------------

function cpsio_create(filename,mode,lun) result (fileptr)
!-----------------------i       i    i
!Purpose:  To create cardsetlist pointer and associate filename,
!          logical unit number, and mode with the file pointer.
  character(len=*),     intent(in) :: filename ! the user's file
  character(len=*),     intent(in) :: mode     ! [r|w|a|u] read/write
  integer         ,     intent(in) :: lun      ! fortran unit number.
  type(cpsio_file),     pointer    :: fileptr  ! pointer to cpsio file object

  integer                          :: alloc_status ! error code if no mem avail.
allocate(fileptr,stat=alloc_status)
allocate(fileptr%sectionlist,stat=alloc_status)
nullify(fileptr%sectionlist%section)
fileptr%sectionlist%num_sections=0
call cardsetlist_create(fileptr%sectionlist%cardsetlist)
fileptr%name=filename
fileptr%lun =lun
fileptr%mode=mode
fileptr%pretag=NULL
fileptr%len_pretag=0
fileptr%pretag_defined=.false.
fileptr%starting_data_record=1
fileptr%next_abs_record=1
fileptr%current_data_section='GLOBAL'
nullify(fileptr%field_ptr)
nullify(fileptr%stale_ptr)
fileptr%initial_read_of_data_section=.true.

end function cpsio_create

integer function cpsio_delete(thisfile)
type(cpsio_file  )   ,pointer       :: thisfile
integer                             :: ierr,err(6)
integer                             :: i
type(cpsio_section),pointer         :: section
cpsio_delete = CPSIO_OK
ierr    = 0
err(:)  = 0
if(associated(thisfile)) then
  thisfile%lun=0
  thisfile%name=''
  thisfile%mode=''
  if(associated(thisfile%sectionlist)) then 
    do i = 1, thisfile%sectionlist%num_sections
      section => thisfile%sectionlist%section(i)
      if(associated(section)) then
        if(associated(section%fld_frmt)) deallocate(section%fld_frmt)
        if(associated(section%fld_name)) deallocate(section%fld_name)
        if(associated(section%fld_unit)) deallocate(section%fld_unit)
        if(associated(section%fld_thdr)) deallocate(section%fld_thdr)
        if(associated(section%field)) deallocate(section%field)
        if(associated(section%stale)) deallocate(section%stale)
      endif
      nullify(section)
    end do
    if(associated(thisfile%sectionlist%section))  &
      deallocate(thisfile%sectionlist%section, stat = err(1) )
    nullify(thisfile%sectionlist%section)
    if(associated(thisfile%sectionlist%cardsetlist) ) then
      call cardsetlist_delete(thisfile%sectionlist%cardsetlist)
      if(associated(thisfile%sectionlist%cardsetlist)) &
        deallocate(thisfile%sectionlist%cardsetlist, stat= err(2) )
      nullify(thisfile%sectionlist%cardsetlist)
    endif
    deallocate(thisfile%sectionlist, stat = err(3))
    nullify (thisfile%sectionlist)
  endif
  if(associated(thisfile%field_ptr)) then
    deallocate(thisfile%field_ptr, stat = err(4))
    nullify (thisfile%field_ptr)
  endif
  if(associated(thisfile%stale_ptr)) then
    deallocate(thisfile%stale_ptr, stat = err(5))
    nullify (thisfile%stale_ptr)
  endif
  deallocate(thisfile, stat=err(6))
  nullify(thisfile)
else
  err = -999
endif
ierr = sum(err)
  if(ierr /= 0) then
    cpsio_delete=CPSIO_ERROR
    return
  endif
return
end function cpsio_delete


integer function cpsio_get_phys_recd(lun,record,pretagged)
!----------------      ---------------i   o       io         o
!Purpose: To read a line and determine if pretag prefixes the line, keep
!         running tally of the next_record to read from logical unit lun.
!         Strip off the pretag before returning.
integer , intent(in)                :: lun         ! logical unit number
character(len=* )   ,intent(out)    :: record      ! string with assembled rcd.
logical,intent(out)                 :: pretagged   ! if prefixed then true.     
!Local variables
character(len=len(record))          :: temprcd     ! temporary character.
integer                             :: ierr        ! error code for read.
integer                             :: i,j,k       ! scratch integer.
integer                             :: temp
  

  temp = len(record)
  ierr = cio_fgetline(record,temp,lun)
  !----Die if EOF or ERROR
  if(ierr == CIO_EOF) then
    !call cpsio_errmsg(&
    !'Warning:CPSIO_GetPhysicalRecord: End Of File Detected.    ')
  elseif(ierr == CIO_ERROR ) then
    !call cpsio_errmsg(&
    !'Err:CPSIO_GetPhysicalRecord: Read Error Detected.                   ')
  else
     ierr = CIO_OK
  endif
  if(.not. file(lun)%p%pretag_defined) then
    ! no pretag defined.
    ! find it.
    ! define it.
    i=index(record,prolog)
    if(i>1) then
      file(lun)%p%pretag=record(1:i-1) 
      file(lun)%p%len_pretag=i-1
      file(lun)%p%pretag_defined=.true.
    elseif (i == 1) then
      file(lun)%p%pretag=NULL
      file(lun)%p%len_pretag=0
      file(lun)%p%pretag_defined=.true.
    else
      temp = len(record)
      ierr = cio_fgetline(record,temp,lun)
      !----Die if EOF or ERROR
      if(ierr == CIO_EOF) then
        !call cpsio_errmsg(&
        !'Warning:CPSIO_GetPhysicalRecord: End Of File Detected.    ')
      elseif( ierr == CIO_ERROR ) then
        !call cpsio_errmsg(&
        !'Err:CPSIO_GetPhysicalRecord: Read Error Detected.                   ')
      else
        ierr = CIO_OK
      endif
      i=index(record,prolog)
      if(i>1) then
        file(lun)%p%pretag=record(1:i-1) 
        file(lun)%p%len_pretag=i-1
        file(lun)%p%pretag_defined=.true.
      elseif (i == 1) then
        file(lun)%p%pretag=NULL
        file(lun)%p%len_pretag=0
        file(lun)%p%pretag_defined=.true.
      else
        !call cpsio_errmsg(&
        !'Err:CPSIO_GetPhysicalRecord: '//&
        !'Not a valid CPS file. Using Defaults.  ')
        file(lun)%p%pretag='#'
        file(lun)%p%len_pretag=1
        file(lun)%p%pretag_defined=.true.
        !code
        ! Set other defaults here.
        !/code
        call cio_frewind(lun)
        file(lun)%p%next_abs_record=1
        temp = len(record)
        ierr = cio_fgetline(record,temp,lun)
        !----Die if EOF or ERROR
        if(ierr == CIO_EOF) then
          !call cpsio_errmsg(&
          !'Warning:CPSIO_GetPhysicalRecord: End Of File Detected.    ')
        elseif( ierr == CIO_ERROR ) then
          !call cpsio_errmsg(&
          !'Err:CPSIO_GetPhysicalRecord: Read Error Detected. ')
        else
          ierr = CIO_OK 
        endif
      endif
    endif
    temprcd = record
    call string_to_upper(temprcd)
    i = index(temprcd,'PRETAG')
    if(i > 0 ) then
      j = index(temprcd(i:),'"')+i
      k = index(temprcd(j+1:),'"')+j-1
      if(j <= k) then
         file(lun)%p%pretag=record(j:k)
         file(lun)%p%len_pretag=k-j+1
         file(lun)%p%pretag_defined=.true.
      endif
      pretagged=cpsio_is_pretag_present(lun,record)
      if(pretagged) call cpsio_strip_pretag(lun,record)
      cpsio_get_phys_recd = ierr
      pretagged=.true.
      return
    endif
  endif
  ! Prepare record for output.  It will be input from file less pretag.
  pretagged=cpsio_is_pretag_present(lun,record)
  if(pretagged) call cpsio_strip_pretag(lun,record)
cpsio_get_phys_recd = ierr
return
end function cpsio_get_phys_recd

subroutine cpsio_assemble_recd(lun,record,continued,in_quoted_str)
!-------------------------  i   io       io 
!Purpose: To assemble partial records (that may be continued using the 
!         continuation character), stripping out any inline comments.
!-----
integer , intent(in)                :: lun         ! logical unit number
character(len= *)   ,intent(inout)  :: record      ! string with assembled rcd.
logical,intent(inout)               :: continued
logical,intent(inout)               :: in_quoted_str
!----------------------Local variables-------------------
character(len= 255)  ,save          :: line

if(.not.continued) line=NULL

  ! Clean out any in line comments.
  call cpsio_strip_inline_commt(record,continued)

  !--------Check for continuation.
  if(continued) then 
    record=trim(adjustl(record))
    if(index(record,CONTIN) == 1) then
      record=record(2:) ! strip off the prefixed contin char.
    else
      call cpsio_errmsg(&
      'Err:CPSIO_GetLogicalRecord: Continue line 1st nonwhite char not & .')
      go to 9999
    endif
  endif

  in_quoted_str = cpsio_in_quoted_str(record,in_quoted_str)

  if(cpsio_end_w_cont(record)) then
    record=record(1:len_trim(record)-1) ! remove Trailing contin char.
    line=trim(line)//record
    continued=.true.
  else
    if(in_quoted_str) then
      call cpsio_errmsg(&
        'Err:CPSIO_GetLogicalRecord: Line ends inside string but no cont chr.')
      go to 9999 
    else
      line=trim(line)//record
      continued=.false.
    endif
  endif

if(.not.continued) then
  record=trim(line)
  line=NULL
endif
return
9999 continue ! Error was generated.
     continued=.false.
     record=trim(line)//record
     line=NULL
return
end subroutine cpsio_assemble_recd

integer function cpsio_get_logical_recd(lun,record,pretagged,num_cards) &
  result(status)
  integer,intent(in)                    :: lun
  character(len=*),intent(out)          :: record
  logical,intent(out)                   :: pretagged
  integer,intent(out),optional          :: num_cards
  !======= local variables =============
  logical                               :: continued
  integer                               :: num_read
  integer                               :: ierr
  type(cpsio_file),pointer              :: fptr
  logical                               :: in_quoted_str

  fptr => file(lun)%p
  if(.not.associated(fptr)) then
    !call cpsio_errmsg('cpsio_get_logical_recd: file pointer not assoc.')
    status = CPSIO_ERROR
    if (present(num_cards)) then
       num_cards = 0
    endif
    return
  endif
  num_read  = 0
  continued = .false.
  in_quoted_str = .false.
  do ! assemble the record... In case the line is continued...
    ierr   = cpsio_get_phys_recd(lun,record,pretagged)
    status = CPSIO_OK
    if (ierr > 0 ) status = CPSIO_ERROR
    if (ierr == CIO_EOF .or. ierr == CIO_ERROR) status = CPSIO_EOF
    if (status /= CPSIO_OK ) exit
    ! ====== check for partial record 
    call cpsio_assemble_recd(lun,record,continued,in_quoted_str)
    num_read = num_read + 1
    if(.not. continued) exit
  end do
  fptr%next_abs_record = fptr%next_abs_record + num_read
  if (present(num_cards)) then
     num_cards = num_read
  endif
  return
end function cpsio_get_logical_recd

  subroutine cpsio_init_section(lun,section_name,section_type)
  integer,intent(in)                             :: lun ! unit.
  character(len=*),intent(in)                    :: section_name
  character(len=*),intent(in)                    :: section_type
  ! LOCAL -------
  type(cpsio_section),pointer                    :: section
  type(cpsio_section),pointer,dimension(:)       :: sec_ptr,sec_tmp

  nullify(sec_ptr)
  nullify(sec_tmp)
  nullify(section)
  !------ See how many sections are defined. --------
  !------ Get space to hold all current section definitions + 1
  allocate(sec_tmp(file(lun)%p%sectionlist%num_sections + 1) )
  if(file(lun)%p%sectionlist%num_sections > 0 )  then
    !------ point to current section definitions.       
    sec_ptr => file(lun)%p%sectionlist%section
    !------ Copy current definitions to temporary space.
    sec_tmp(1:file(lun)%p%sectionlist%num_sections) = & 
    sec_ptr(1:file(lun)%p%sectionlist%num_sections)
    !------ We are done with sec_ptr, remove it.
    nullify (sec_ptr)
    !------ We have copied sectionlist to sec_tmp, we can deallocate it.
    deallocate(file(lun)%p%sectionlist%section)
  endif
  !-- increment number of sections
  file(lun)%p%sectionlist%num_sections = & 
  file(lun)%p%sectionlist%num_sections + 1
  !------ put new info into this section's area.
  section => sec_tmp(file(lun)%p%sectionlist%num_sections)
  section%section_name = section_name
  section%section_type = section_type

  ! Newly created pointers are undefined, must nullify them.
  ! ehs 10dec01
  !
  nullify(section%fld_frmt)
  nullify(section%fld_name)
  nullify(section%fld_unit)
  nullify(section%fld_thdr)
  nullify(section%field   )
  nullify(section%stale   )

  !------ Replace global pointer to point to sec_tmp
  file(lun)%p%sectionlist%section => sec_tmp
  !------ We are finished with sec_tmp
  nullify(section)
  nullify(sec_tmp)

end subroutine cpsio_init_section

subroutine cpsio_load_sectiondefs(lun)
  ! Now determine how many sections you have that are HEADER
  ! Allocate memory for a section pointer for each, associate the pointers and
  ! load the structures from the corresponding cardsets.
  ! ==== Here we will load all header sections with default keywords and see if
  ! ==== any have been overwritten.
  integer,intent(in)                :: lun
  ! ==== Local variables 
  type(cpsio_section),pointer       :: section
  integer                           :: nelements,i,j,itmp
  character(len=CPSIO_CARD_SIZE)    :: errmsg
  type(cardset_struct),pointer     :: cardset


  do i = 1, file(lun)%p%sectionlist%num_sections
    section => file(lun)%p%sectionlist%section(i)
    section%num_fld = 0
    if(section%section_type == 'HEADER') then
      cardset =>  & 
              cardsetlist_find_cardset(file(lun)%p%sectionlist%cardsetlist, &
              section%section_name)
      if(associated(cardset)) then
        do j = 1,8
          nelements = cardset_num_elements(cardset,kwlst(j))
          select case ( kwlst(j) )
            case ('SKIP_BNK')
              if(nelements == 1) then
                call cardset_get_scalar(cardset,kwlst(j),section%skip_blanks,&
                errmsg)
              else
                section%skip_blanks=.true.
              endif
            case ('WRAP')
              if(nelements == 1) then 
                call cardset_get_scalar(cardset,kwlst(j),section%wrap,errmsg)
              else
                section%wrap = 1
              endif
            case ('NULL')
              if(nelements == 1) then 
                call cardset_get_scalar(cardset,kwlst(j),section%null,errmsg)
              else
                section%null = '-nil-' 
              endif
            case ('NUM_FLD')
              if(nelements == 1) then 
                call cardset_get_scalar( &
                     cardset,kwlst(j),itmp,errmsg)
                section%num_fld = max(section%num_fld,itmp)
              endif
            case ('FLD_FRMT')
              if(nelements >= 1) then 
                allocate(section%fld_frmt(nelements))
                call cardset_get_array( &
                     cardset,kwlst(j),section%fld_frmt,nelements,errmsg)
                section%num_fld = max(nelements,section%num_fld)
              endif
            case ('FLD_NAME')
              if(nelements >= 1) then 
                allocate(section%fld_name(nelements))
                call cardset_get_array( &
                     cardset,kwlst(j),section%fld_name,nelements,errmsg)
                section%num_fld = max(nelements,section%num_fld)
              endif
            case ('FLD_UNIT')
              if(nelements >= 1) then 
                allocate(section%fld_unit(nelements))
                call cardset_get_array( &
                     cardset,kwlst(j),section%fld_unit,nelements,errmsg)
                section%num_fld = max(nelements,section%num_fld)
              endif
            case ('FLD_THDR')
              if(nelements >= 1) then 
                allocate(section%fld_thdr(nelements))
                call cardset_get_array( &
                     cardset,kwlst(j),section%fld_thdr,nelements,errmsg)
                section%num_fld = max(nelements,section%num_fld)
              endif
          end select
        end do
        ! ===== create buffer space for fields as they are read in.
        allocate(section%field(section%num_fld))
        ! ===== initialize all fields to NULL
        section%field(:section%num_fld) = NULL
        ! ===== create buffer space for stale "read" flag for each field.
        allocate(section%stale(section%num_fld))
      endif
    endif
  end do
  return
end subroutine cpsio_load_sectiondefs

function cpsio_find_section(sectionlist,section_name) result (section)
  type(cpsio_sectionlist), intent(in)          :: sectionlist
  character(len=*),intent(in)                  :: section_name
  type(cpsio_section), pointer                 :: section
  !=========== LOCAL VARS
  integer                                      :: i

  do i = 1, sectionlist%num_sections
    if(sectionlist%section(i)%section_name == section_name ) then
      section => sectionlist%section(i)
      exit
    endif
  end do
  return
end function cpsio_find_section

integer function cpsio_freshen_stale_field(lun,section) result(status)
  integer                   ,intent(in)        :: lun
  type(cpsio_section),pointer                  :: section
  ! LOCAL
  character(len=255)                           :: data_card
  type(cpsio_file), pointer                    :: fptr
  integer                                      :: i   ,iend,ld 

  ! === point to the file pointer.
  fptr =>file(lun)%p
  status = CPSIO_ERROR
  status = cpsio_get_next_data (lun,data_card,section%skip_blanks)
  if(status /= CPSIO_OK) then
    fptr%initial_read_of_data_section=.true.
    return
  endif
  iend = 0
  do i = 1, section%num_fld
    data_card = trim(adjustl(data_card(iend+1:)))
    do while (scan(data_card,DELIM) == 1)
      data_card = data_card(2:)
      ld        = len_trim(data_card)
      if(ld <= 0 ) exit
    end do
    ld        = len_trim(data_card)
    if(ld <= 0 ) exit
    if(data_card(1:1) == QUOTE ) then
      iend      = index(data_card(2:),QUOTE)
      data_card = data_card(2:)
    else
      iend      = scan(data_card(:ld),DELIM)
    endif
    if(iend == 0 ) iend = ld+1
    section%field(i) = data_card(:iend-1)
  end do
  ! === If I am here, I read a new set of fields in the current section.
  ! === reset the stale logical to false, since I haven't touched the values.
  section%stale(:) = .false.
end function cpsio_freshen_stale_field

integer function cpsio_get_next_data (lun,card,skip_blanks,num_cards) &
  result (status)
  integer  , intent(in)                        :: lun
  character(len=*),intent(out)                 :: card
  logical,intent(in)                           :: skip_blanks
  integer,intent(out),optional                 :: num_cards
  ! local
  logical                                      :: pretagged
  character(len=7)                             :: tag_type
  character(len=CPSIO_FIELD_NAME_SIZE)         :: tag_name
  character(len=255)                           :: scrap
  logical                                      :: tagged,self_ended_tag
  integer                                      :: num_cards_in_recd

  status = CPSIO_ERROR

  if (present(num_cards)) then
    num_cards = 0
  endif

  do while (status /= CPSIO_OK )
    ! === actually read the file, fill all fields.
    status = cpsio_get_logical_recd(lun,card,pretagged,num_cards_in_recd)
    if(status == CPSIO_EOF .or. status == CPSIO_ERROR ) then
      ! === read errors or EOF cause me to reset the initial-read logical and
      ! === to exit.
      exit
    endif

    if (present(num_cards)) then
      num_cards = num_cards + num_cards_in_recd
    endif

    if(cpsio_is_blank(card) ) then
      status = CPSIO_BLANK
      if(.not.skip_blanks)status = CPSIO_OK
    elseif(pretagged) then
      call cpsio_extract_tag( & 
      card,tag_type,tag_name,tagged,self_ended_tag,scrap)
      if(tagged) then
        ! I must be at the end of this data section.
        status = CPSIO_EOS
        exit
      endif
      status = CPSIO_COMMENT
      cycle
    endif
  end do
end function cpsio_get_next_data

integer function cpsio_initial_section_read(lun ,section) result(status)
  integer,intent(in)                           :: lun
  type(cpsio_section),pointer                  :: section
  ! LOCAL
  type(cpsio_file)  ,pointer                   :: fptr
  type(cpsio_section),pointer,save             :: saved_section_ptr

  ! === set file pointer to this unit.
  fptr =>file(lun)%p
  status = CPSIO_OK
  if(.not. fptr%initial_read_of_data_section)  then 
    section =>saved_section_ptr
    return
  endif
  fptr%initial_read_of_data_section=.false.
  ! === initial_read is set whenever a section has been previously exited.
  ! === get the section pointer for this file.
  section => cpsio_find_section(fptr%sectionlist,fptr%current_data_section)

  ! === Test for various error conditions
  if(.not. associated(section) ) then
    !call cpsio_errmsg('cpsio_get_value: Error associating section.')
    status=CPSIO_ERROR
    return
  endif

  if(section%section_type /= 'HEADER' ) then
    !call cpsio_errmsg('cpsio_get_value: This is not a HEADER section type.')
    status=CPSIO_ERROR
    return
  endif

  if(section%num_fld <= 0 ) then
    !call cpsio_errmsg('cpsio_get_value: No fields defined in section.')
    status=CPSIO_ERROR
    return
  endif

  ! === initially set the values to stale so that I will read them below.
  section%stale(:) = .true.
  saved_section_ptr => section

  status = CPSIO_OK
  return
end function cpsio_initial_section_read

integer function cpsio_load_field(lun,section,field_number) result (status)
  integer  , intent(in)                        :: lun
  type(cpsio_section),pointer                  :: section
  integer,intent(in)                           :: field_number
  ! ------------------ local variables --------------------
  type(cpsio_file), pointer                    :: fptr
  status = 0
  fptr =>file(lun)%p
  ! === Test for error condition.
  if(field_number > section%num_fld ) then
    !write(stdout,*) &
    !  'CPSIO_LOAD_FIELD: You asked for field# ',field_number,', but &
    !&there are only ',section%num_fld,' fields defined for section ', &
    !trim(section%section_name)
    status = CPSIO_ERROR
    fptr%initial_read_of_data_section=.true.
    return
  endif
  ! === if I've already read this field, read in a new record.
  if(section%stale(field_number) ) then
    status = cpsio_freshen_stale_field(lun,section)
    ! === set stale logical true, because I have read this field.
    section%stale(field_number)= .true.
    if(status /= 0 ) return
  endif
  return
end function cpsio_load_field

integer function cpsio_find_field_by_name(section,field_name) &
  result(field_number)
  type(cpsio_section),pointer       :: section
  character(len=*),intent(in)       :: field_name
  ! === local
  integer                           :: i
  field_number = 0
  if(.not. associated(section) ) then
    call cpsio_errmsg(' INTERNAL ERROR IN CPSIO_FIND_FIELD: Call programmer.')
    return
  endif
  do i = 1, section%num_fld
    if (field_name == section%fld_name(i) ) then
      field_number = i
      return
    endif
  end do
  !  write(stdout,*)'cpsio_find_field_by_name: You asked for field ' &
  !            //trim(field_name)// &
  !  ', but it is not defined for section '//trim(section%section_name)
  return
end function cpsio_find_field_by_name

subroutine cpsio_errmsg(message)
  ! purpose: To output an error message.
  character(len=*), intent(in)         :: message
  ! local
  integer                              :: iostat
  write(stdout,'(a)',iostat=iostat)trim(message)
  return
end subroutine cpsio_errmsg

!--------- end of private functions -------------------------
end module cpsio_module
