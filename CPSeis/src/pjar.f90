
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ pjar.f90 --------------------------------!!
!!------------------------------ pjar.f90 --------------------------------!!
!!------------------------------ pjar.f90 --------------------------------!!


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
! Name       : PJAR             (Pickle Jar)
! Category   : character
! Written    : 2001-12-28   by: Tom Stoeckley
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Parameter container using a CARDSETLIST object.
! Portability: No known limitations, but see comments below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This primitive is a simple container for storing sets of parameters for
! a variety of uses.  This primitive can be used in any situation where
! generic parameters need to be handled by modules which do not know
! anything about the parameters.  An example would be the parameters
! associated with self defining files.
!
! This primitive is similar to the PC (parameter cache) primitive but is
! much simpler and also more general.  This primitive has the following
! characteristics which differ from the parameter cache:
!  (1) this primitive is a creatable and deletable object in the usual
!       object-oriented sense rather than an internal stack of hidden data
!       structures.
!  (2) this primitive can have any number of generic sections (CARDSET
!       objects) rather than a fixed number of special-purpose sections.
!  (3) data cards are created in unpacked format.
!  (4) some subroutine arguments are optional to simplify their use when
!       called from routines which also contain optional arguments.
!  (5) the GET routines always return something, even if keyword is not found.
!
! This primitive is built upon the CARDSET primitive, which contains a
! list of CARDSET objects.  Unlike the CARDSETLIST primitive, this primitive
! keeps a pointer to an active CARDSET instead of returning a CARDSET pointer
! to the calling program.  All access to the active CARDSET is provided with
! pass-through subroutines, some of which modify the behavior of the CARDSET
! subroutines.  For simplicity, not all subroutines available in the CARDSET
! primitive are provided.
!
! Occasionally (where documented below), this primitive will terminate the
! program with an error message, but only when an error is detected which
! is considered to be a programming logic error in the calling program.
! This is similar to the ASSERT macro in C.
!
!-------------------------------------------------------------------------------
!</descript_doc>



!<calling_doc>
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS
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
!              CREATE AND DELETE AND CLEAR AND PRINT AND STATUS
!
!                                    o
!                 call pjar_create (obj)
!
!                                    b
!                 call pjar_delete (obj)
!                 call pjar_clear  (obj)
!
!                                    i     b   
!                 call pjar_copy   (obj1, obj2)
!
!                                          opt       opt     opt    opt
!                                    b      i         i       i      i
!                 call pjar_print  (obj, lunprint, progname,header,footer)
!                 call pjar_status (obj, errmsg)
!                                    b     o   
!                                         opt 
!
! type(pjar_struct)   obj = (pointer to) the pickle jar object.
! integer        lunprint = logical unit number for printing (or zero).
! char(*)        progname = program name to be used as a prefix to each line.
! char(*)          header = message to be placed on first line.
! char(*)          footer = message to be placed on last line.
! char(*)          errmsg = first error message which occurred (or blank).
!
! PJAR_CREATE:
!  (1) creates a new empty pickle jar.
!  (2) there will initially be no active section in OBJ.
!         
! PJAR_DELETE:
!  (1) deletes the pickle jar (unless already deallocated).
!
! PJAR_CLEAR:
!  (1) clears the contents of the pickle jar (including any error message).
!  (2) there will be no active section in OBJ after this operation.
!
! PJAR_COPY:
!  (1) copies the contents of OBJ1 to OBJ2.
!  (2) the previous contents of OBJ2 are deleted first.
!  (3) there will be no active section in OBJ2.
!
! PJAR_PRINT:
!  (1) prints the contents of the pickle jar (including any error message).
!  (2) does nothing if LUNPRINT is missing or <= zero.
!  (3) precedes each line with progname if progname is present.
!  (4) prints HEADER at beginning if present.
!  (5) prints FOOTER at end if present.
!  (6) prints blank line before and after the printing.
!
! PJAR_STATUS:
!  (1) returns a message describing the first error which might have occurred.
!  (2) returns a blank message if no error has occurred in the pickle jar.
!
! The only routines which might register an error are these:
!         PJAR_GET    PJAR_GET_CARDS    PJAR_GET_CARD
!
!-------------------------------------------------------------------------------
!                        CHOOSE AN ACTIVE SECTION
!
!                                            i
!            nsections = pjar_num_sections (obj)
!            indx      = pjar_find_section (obj, secname)
!                                            i      i
!
!                                          b     i 
!            call pjar_choose_section    (obj, indx)
!            call pjar_choose_section    (obj, secname)
!                                          b     i 
!
!                                          b
!            call pjar_choose_no_section (obj)
!
!                                                opt
!                                          i      o
!            call pjar_get_secname       (obj, secname)
!            call pjar_get_secnames      (obj, secnames, nsections)
!                                          i      o          o
!                                                opt        opt
!
! type(pjar_struct)         obj = the pickle jar object.
! integer             nsections = number of sections.
! integer                  indx = index of the desired section.
! character(len=*)      secname = name of the desired section.
! character(len=*)  secnames(:) = names of all of the sections.
!
! The secname is converted to upper case when input.
! The secname matching is done in a case-insensitive manner.
!
! PJAR_NUM_SECTIONS:
!  (1) If NSECTIONS is zero, there is no active section.
!
! PJAR_FIND_SECTION:
!  (1) Returns the index of the requested section.
!  (2) The requested section will become the active section.
!  (3) If INDX is zero, there is no active section.
!  (4) If SECNAME is blank, INDX will be zero and no section will be active.
!
! PJAR_CHOOSE_SECTION (INDX):
!  (1) The requested section will become the active section.
!  (2) If INDX is out of range, program will TERMINATE with an error message.
!
! PJAR_CHOOSE_SECTION (SECNAME):
!  (1) The requested section will become the active section.
!  (2) If the section is not found, a new empty section will be created.
!  (3) If SECNAME is blank, no section is created and no section will be active.
!
! PJAR_CHOOSE_NO_SECTION:
!  (1) There will be no active section.
!
! PJAR_GET_SECNAME:
!  (1) Returns the name of the active section.
!  (2) Returns a blank name if there is no active section.
!  (3) Argument is optional for convenience.
!
! PJAR_GET_SECNAMES:
!  (1) Returns the names of all of the sections.
!  (2) Arguments are optional for convenience.
!
! The active section remains active until the next time PJAR_CHOOSE_SECTION
! or PJAR_CLEAR is called.  Any attempt to access the active section when
! none are active will cause the program to TERMINATE with an error message.
!
!-------------------------------------------------------------------------------
!                     ACCESS PARAMETERS AS DATA CARDS
!                     (to and from the active section)
!
!                                    b
!          ncards = pjar_num_cards (obj)
!
!                                        opt    opt
!                                  b      o      o
!          call pjar_alloc_cards (obj, pcards, ncards)
!          call pjar_get_cards   (obj,  cards, ncards)
!
!          call pjar_clear_cards (obj)
!          call pjar_put_cards   (obj,  cards, ncards, progname, lastcard)
!          call pjar_add_cards   (obj,  cards, ncards, progname, lastcard)
!                                  b      i      i        i         o
!                                        opt    opt      opt       opt
!
!                                  b    i     o
!          call pjar_get_card    (obj, indx, card)
!          call pjar_add_card    (obj,       card)
!                                  b          i
!
!          call pjar_new_card    (obj, progname, lastcard)
!                                  b      i         o
!                                        opt       opt
!
! type(pjar_struct)      obj = the pickle jar object.
! integer             ncards = number of data cards.
! integer               indx = index of desired data card (1 thru ncards).
! char(*)               card = a single data card.
! char(*)           cards(:) = an array of data cards.
! char(*),pointer  pcards(:) = pointer to an array of data cards.
! char(*)           progname = name of program or process accessing this file.
! char(*)           lastcard = last history card (created by the subroutine).
!
! PCARDS is a pointer which must be nullified before first use.
! It will be deallocated and reallocated to contain the returned contents.
! It will always be allocated to a dimension of at least one.
! It should be conditionally deallocated when no longer needed.
!
! PROGNAME (and maybe LASTCARD) are recommended when writing history cards.
!
! Nothing is done by PJAR_ALLOC_CARDS or PJAR_GET_CARDS if either optional
! argument is missing.
!
! All previous cards are replaced by PJAR_PUT_CARDS if both optional arguments
! CARDS and NCARDS are present, or deleted otherwise.  Then a card is added
! if PROGNAME is present and not blank.
!
! An error message is generated if an error occurs in PJAR_GET_CARDS or
! PJAR_GET_CARD.
!
!-------------------------------------------------------------------------------
!                        GET KEYWORD INFORMATION
!                        (in the active section)
!
!                 o                              b       i
!               nkeys   = pjar_num_keywords    (obj)
!               keyword = pjar_get_keyword     (obj,    indx)
!               present = pjar_keyword_present (obj, keyword)
!                    call pjar_remove_keyword  (obj, keyword)
!
! type(pjar_struct)   obj = the pickle jar object.
! integer           nkeys = number of keywords.
! integer            indx = index of desired keyword (1 thru nkeys).
! char(*)         keyword = keyword of the desired parameter.
! logical         present = whether the keyword is present.
!
! The keyword is converted to upper case when input.
! The keyword matching is done in upper case.
! KEYWORD is returned as blank if INDX is out of range.
!
!-------------------------------------------------------------------------------
!                      ACCESS PARAMETERS BY KEYWORD
!                    (to and from the active section)
!
!                o                           b      i
!            nelements = pjar_num_elements (obj, keyword)
!            nature    = pjar_nature       (obj, keyword)
!
!                                                  opt
!                            b      i       o       i
!        call pjar_get     (obj, keyword, scalar, defval)
!
!        call pjar_put     (obj, keyword, scalar, nchar, ndec)
!        call pjar_augment (obj, keyword, scalar, nchar, ndec)
!                            b      i       i       i     i
!                                                  opt   opt
!
!                                                  opt       opt
!                            b      i       o       o         i
!        call pjar_get     (obj, keyword, array, nelements, defval)  
!
!        call pjar_put     (obj, keyword, array, nelements, nchar, ndec)
!        call pjar_augment (obj, keyword, array, nelements, nchar, ndec)
!                            b      i       i       i         i     i
!                                                  opt       opt   opt
!
!                                                             opt
!                                b      i      i       o       i
!        call pjar_get_element (obj, keyword, indx, element, defval)
!
!        call pjar_put_element (obj, keyword, indx, element, nchar, ndec)
!        call pjar_add_element (obj, keyword,       element, nchar, ndec)
!                                b      i      i       i       i     i
!                                                             opt   opt
!                                                              
!                                   b     i     i
!        call pjar_insert_element (obj,keyword,indx)
!        call pjar_remove_element (obj,keyword,indx)
!        call pjar_clear_buffer   (obj,keyword)
!
!
!      indx = pjar_find     (obj, keyword, element)
!      indx = pjar_find_add (obj, keyword, element, nchar, ndec)
!       o                     b      i        i       i     i
!                                                    opt   opt
!
! type(pjar_struct)     obj = the pickle jar object.
! char(*)           keyword = keyword of the desired parameter.
! integer         nelements = number of array elements.
! integer            nature = nature of the parameter.
! (any type)         scalar = single scalar parameter value.
! (any type)       array(:) = array of parameter values.
! (any type)        element = a single array element.
! (any type)         defval = default value to replace missing or nil values.
! integer             nchar = maximum number of characters to encode.
! integer              ndec = maximum number of decimals to encode.
! integer              indx = index of the matching array element.
!
! The type of ARRAY and SCALAR and ELEMENT and DEFVAL can be real, integer,
! double precision, logical, or character(len=*).  The type of SCALAR can also
! be type(grid_struct).
!
! The NCHAR argument is for integer, real, and double precision variables only.
! The NDEC argument is for real and double precision variables only.
! The NCHAR and NDEC arguments are both used for type(grid_struct).
! No maximum restrictions are imposed if NCHAR and NDEC are not specified.
!
! PJAR_NUM_ELEMENTS:
!  (1) returns number of elements (0 or more) if the parameter is an ARRAY.
!  (2) returns              1                 if the parameter is a SCALAR.
!  (3) returns              0                 if KEYWORD is not found.
!
! PJAR_NATURE:
!  (1) returns named constant PJAR_ARRAY   if the parameter is an ARRAY.
!  (2) returns named constant PJAR_SCALAR  if the parameter is a SCALAR.
!  (3) returns named constant PJAR_MISSING if KEYWORD is not found.
!
! PJAR_GET (for scalars and arrays):
!  (1) gets the scalar or array parameter.
!  (2) returns SCALAR = nil if KEYWORD is not found or an error occurs.
!  (3) returns ARRAY(:) = nil and NELEMENTS = 0 if KEYWORD not found or error.
!  (4) does nothing if NELEMENTS is not present in the argument list.
!  (5) an error message will be generated if an error occurs.
!  (6) if DEFVAL is present, any returned nil values are replaced by DEFVAL.
!
! PJAR_PUT (for scalars and arrays):
!  (1) replaces the scalar parameter or array parameter or array element.
!  (2) adds the scalar or array parameter if KEYWORD is not found.
!  (3) sets the nature of the parameter to be a SCALAR or an ARRAY.
!  (4) the nature of the previous contents is irrelevant.
!  (5) does nothing if NELEMENTS is not present in the argument list.
!
! PJAR_AUGMENT (for scalars and arrays):
!  (1) same as PG_PUT except nothing is done if KEYWORD already exists.
!  (2) useful for setting default values before or after real values are put.
!
! PJAR_GET_ELEMENT:
!  (1) gets the specified array element.
!  (2) returns ELEMENT = nil if KEYWORD is not found or an error occurs.
!  (3) returns ELEMENT = nil if INDX is too small or too large.
!  (4) an error message will be generated if an error occurs.
!  (5) if DEFVAL is present, any returned nil values are replaced by DEFVAL.
!
! PJAR_PUT_ELEMENT:
!  (1) replaces or adds one element to previous contents regardless of nature.
!  (2) adds the array parameter if KEYWORD is not found.
!  (3) sets the nature of the parameter to be an ARRAY.
!  (4) does nothing if INDX is too small.
!  (5) increases the length of the array if necessary, filling any
!       intermediate array elements with nil.
!
! PJAR_ADD_ELEMENT:
!  (1) adds one element to previous contents regardless of nature.
!  (2) adds the array parameter if KEYWORD is not found.
!  (3) sets the nature of the parameter to be an ARRAY.
!
! PJAR_INSERT_ELEMENT:
!  (1) inserts the specified element at the specified INDX.
!  (2) inserts element from buffer.
!  (2) adds the array parameter if KEYWORD is not found.
!  (3) sets the nature of the parameter to be an ARRAY.
!  (4) does nothing if INDX is too small.
!  (5) increases the length of the array if necessary, filling any
!       intermediate array elements with nil.
!
! PJAR_REMOVE_ELEMENT:
!  (1) removes the element at the specified INDX.
!  (2) puts removed element into a buffer for possible insertion later.
!  (2) adds the array parameter if KEYWORD is not found.
!  (3) sets the nature of the parameter to be an ARRAY.
!  (4) does nothing if INDX is too small.
!  (5) increases the length of the array if necessary, filling any
!       intermediate array elements with nil.
!
! PJAR_FIND:
!  (1) returns the index of the matching array element.
!  (2) returns zero if KEYWORD is not found or there is no match.
!  (3) returns zero if the parameter is a scalar or an error occurs.
!  (4) no error message is ever generated.
!
! PJAR_FIND_ADD:
!  (1) finds the matching element regardless of the nature of the contents.
!  (2) adds the element to the array if there is no match.
!  (3) sets the nature of the parameter to be an ARRAY.
!  (4) returns the index of the matching (or added) array element.
!  (5) adds the array parameter if KEYWORD is not found.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!004. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!  3. 2003-06-17  Stoeckley  Add workaround in pjar_get_carray to keep
!                             the sgi compiler from aborting.
!  2. 2002-04-11  Stoeckley  Add PJAR_FIND_SECTION; replace PJAR_FIND by
!                             new PJAR_FIND_ADD; add new PJAR_FIND;
!                             remove PJAR_FIND_PUT; change behavior of
!                             PJAR_INSERT_ELEMENT and PJAR_REMOVE_ELEMENT.
!  1. 2002-02-04  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations after providing workarounds for buggy compilers.
!
! Absoft compiler:
!
!  See pjar_get_secnames for details relating to compiler problems with
!  testing a pair of optional arguments for their presence.
!
! SGI compiler:
!
!  The "where" construct with a character array causes the compiler to abort
!  with the following message:
!    Signal: Bus error in Wopt Initialization phase.
!    Error: Signal Bus error in phase Wopt Initialization -- processing aborted
!    f90 ERROR:  /usr/lib32/cmplrs/be died due to signal 4
!  See pjar_get_carray for details.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!


      module pjar_module

      use cardsetlist_module
      use cardset_module
      use grid_module
      use string_module
      implicit none
      public
      private :: pjar_private_whoops 
      private :: pjar_private_abort 

      character(len=100),public,save :: PJAR_IDENT = &
'$Id: pjar.f90,v 1.4 2006/09/18 13:32:51 Glover prod sps $'


      type,public :: pjar_struct
        private
        type(cardsetlist_struct),pointer :: cardsetlist
        type(cardset_struct)    ,pointer :: cardset      ! active section.
        character(len=80)                :: errmsg 
      end type pjar_struct


   integer,parameter :: PJAR_DATACARD_LENGTH = CARDSET_DATACARD_LENGTH
   integer,parameter :: PJAR_LENGTH          = CARDSET_LENGTH   ! char length.
   integer,parameter :: PJAR_MISSING         = CARDSET_MISSING  ! nature.
   integer,parameter :: PJAR_SCALAR          = CARDSET_SCALAR   ! nature.
   integer,parameter :: PJAR_ARRAY           = CARDSET_ARRAY    ! nature.


      interface pjar_choose_section
        module procedure pjar_choose_section_by_indx
        module procedure pjar_choose_section_by_name
      end interface

      interface pjar_get
        module procedure pjar_get_gscalar
        module procedure pjar_get_iscalar
        module procedure pjar_get_fscalar
        module procedure pjar_get_dscalar
        module procedure pjar_get_cscalar
        module procedure pjar_get_lscalar
        module procedure pjar_get_iarray
        module procedure pjar_get_farray
        module procedure pjar_get_darray
        module procedure pjar_get_carray
        module procedure pjar_get_larray
      end interface

      interface pjar_put
        module procedure pjar_put_gscalar
        module procedure pjar_put_iscalar
        module procedure pjar_put_fscalar
        module procedure pjar_put_dscalar
        module procedure pjar_put_cscalar
        module procedure pjar_put_lscalar
        module procedure pjar_put_iarray
        module procedure pjar_put_farray
        module procedure pjar_put_darray
        module procedure pjar_put_carray
        module procedure pjar_put_larray
      end interface

      interface pjar_augment
        module procedure pjar_augment_gscalar
        module procedure pjar_augment_iscalar
        module procedure pjar_augment_fscalar
        module procedure pjar_augment_dscalar
        module procedure pjar_augment_cscalar
        module procedure pjar_augment_lscalar
        module procedure pjar_augment_iarray
        module procedure pjar_augment_farray
        module procedure pjar_augment_darray
        module procedure pjar_augment_carray
        module procedure pjar_augment_larray
      end interface

      interface pjar_get_element
        module procedure pjar_get_ielement
        module procedure pjar_get_felement
        module procedure pjar_get_delement
        module procedure pjar_get_celement
        module procedure pjar_get_lelement
      end interface

      interface pjar_put_element
        module procedure pjar_put_ielement
        module procedure pjar_put_felement
        module procedure pjar_put_delement
        module procedure pjar_put_celement
        module procedure pjar_put_lelement
      end interface

      interface pjar_add_element
        module procedure pjar_add_ielement
        module procedure pjar_add_felement
        module procedure pjar_add_delement
        module procedure pjar_add_celement
        module procedure pjar_add_lelement
      end interface

      interface pjar_find
        module procedure pjar_find_ielement
        module procedure pjar_find_felement
        module procedure pjar_find_delement
        module procedure pjar_find_celement
        module procedure pjar_find_lelement
      end interface

      interface pjar_find_add
        module procedure pjar_find_add_ielement
        module procedure pjar_find_add_felement
        module procedure pjar_find_add_delement
        module procedure pjar_find_add_celement
        module procedure pjar_find_add_lelement
      end interface

      contains


!!-------------------- create delete clear copy status ---------------------!!
!!-------------------- create delete clear copy status ---------------------!!
!!-------------------- create delete clear copy status ---------------------!!


      subroutine pjar_create (obj)
      implicit none
      type(pjar_struct),pointer :: obj                    ! argument

      allocate                (obj)
      nullify (obj%cardset)
      nullify (obj%cardsetlist) ! jpa
      call cardsetlist_create (obj%cardsetlist)
      obj%errmsg = ' '
      return
      end subroutine pjar_create



      subroutine pjar_delete (obj)
      implicit none
      type(pjar_struct),pointer :: obj                    ! argument

      if (associated(obj)) then
           call cardsetlist_delete (obj%cardsetlist)
           deallocate              (obj)
      end if
      return
      end subroutine pjar_delete



      subroutine pjar_clear (obj)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument

      call cardsetlist_clear (obj%cardsetlist)
      nullify                (obj%cardset)
      obj%errmsg = ' '
      return
      end subroutine pjar_clear



      subroutine pjar_copy (obj1,obj2)
      implicit none
      type(pjar_struct),intent(in)    :: obj1               ! argument
      type(pjar_struct),intent(inout) :: obj2               ! argument

      call cardsetlist_copy (obj1%cardsetlist, obj2%cardsetlist)
      nullify               (obj2%cardset)
      obj2%errmsg = obj1%errmsg
      return
      end subroutine pjar_copy



      subroutine pjar_status (obj,errmsg)
      implicit none
      type(pjar_struct),intent(inout)        :: obj         ! argument
      character(len=*) ,intent(out),optional :: errmsg      ! argument

      if (present(errmsg)) errmsg = obj%errmsg
      return
      end subroutine pjar_status


!!------------------------------- print -------------------------------------!!
!!------------------------------- print -------------------------------------!!
!!------------------------------- print -------------------------------------!!


      subroutine pjar_print (obj,lunprint,progname,header,footer)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                 ! argument
      integer          ,intent(in),optional :: lunprint            ! argument
      character(len=*) ,intent(in),optional :: progname            ! argument
      character(len=*) ,intent(in),optional :: header              ! argument
      character(len=*) ,intent(in),optional :: footer              ! argument
      integer                               :: nsections           ! local
      integer                               :: ncards,i,j          ! local
      character(len=80)                     :: card,secname,keep   ! local
      character(len=40)                     :: prefix              ! local

      if (.not.present(lunprint)) return
      if (lunprint <= 0) return

      if (present(progname)) then
           call string_to_upper(progname,prefix)
      else
           prefix = 'PJAR'
      end if
      if (prefix == ' ') prefix = 'PJAR'
      prefix = trim(prefix)//':'

      nsections = pjar_num_sections(obj)
      call pjar_get_secname (obj,keep)

      write (lunprint,1000)
      if (present(header)) write (lunprint,7000) trim(prefix),trim(header)
      write (lunprint,2000) trim(prefix),nsections
      if (obj%errmsg == ' ') then
           write (lunprint,3000) trim(prefix),'no errors'
      else
           write (lunprint,3000) trim(prefix),obj%errmsg
      end if

      do i = 1,nsections
           call pjar_choose_section (obj,i)
           call pjar_get_secname    (obj,secname)
           if (secname == keep) secname = trim(secname)//' (active section)'
           write (lunprint,5000) trim(prefix),i,trim(secname)
           ncards = pjar_num_cards(obj)
           do j = 1,ncards
                call pjar_get_card (obj,j,card)
                write (lunprint,6000) trim(prefix),j,trim(card)
           end do
      end do
      if (present(footer)) write (lunprint,7000) trim(prefix),trim(footer)
      write (lunprint,1000)
      call pjar_choose_section (obj,keep)
1000  format (1x)
2000  format (1x,a,' pickle jar contents = ',i3,' sections')
3000  format (1x,a,' error message = ',a)
5000  format (1x,a,' section index = ',i3,'    section name = ',a)
6000  format (1x,a,1x,i4,1x,a)
7000  format (1x,a,1x,a)
      return
      end subroutine pjar_print


!!------------------------ private whoops ---------------------------------!!
!!------------------------ private whoops ---------------------------------!!
!!------------------------ private whoops ---------------------------------!!


      subroutine pjar_private_whoops (obj,errmsg)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: errmsg             ! argument

      if (errmsg == ' ' .or. errmsg(1:8) == 'keyword ') return
      if (obj%errmsg == ' ') obj%errmsg = errmsg
      return
      end subroutine pjar_private_whoops


!!------------------------ private abort ---------------------------------!!
!!------------------------ private abort ---------------------------------!!
!!------------------------ private abort ---------------------------------!!

! This routine aborts with a message if there is no active cardset in use.
! It should be called from all routines which access the active cardset.


      subroutine pjar_private_abort (obj,routine,keyword)
      implicit none
      type(pjar_struct),intent(in)          :: obj               ! argument
      character(len=*) ,intent(in)          :: routine           ! argument
      character(len=*) ,intent(in),optional :: keyword           ! argument
      integer,parameter                     :: lunstop = 6       ! local

      if (.not.associated(obj%cardset)) then
           write(lunstop,*) '--> Illegally calling pjar_',trim(routine)
           if (present(keyword)) &
           write(lunstop,*) '--> using keyword = ',trim(keyword)
           write(lunstop,*) '--> when there is no active CARDSET &
                                                 &in the PJAR object.'
           write(lunstop,*) '--> This is a programming error &
                                                 &in the calling program.'
           stop
      end if
      return
      end subroutine pjar_private_abort


!!------------------------ choose an active section ------------------------!!
!!------------------------ choose an active section ------------------------!!
!!------------------------ choose an active section ------------------------!!


      function pjar_num_sections (obj) result (nsections)
      implicit none
      type(pjar_struct),intent(in)    :: obj                ! argument
      integer                         :: nsections          ! result

      nsections = cardsetlist_num_cardsets (obj%cardsetlist)
      return
      end function pjar_num_sections



      function pjar_find_section (obj,secname) result (indx)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument
      character(len=*) ,intent(in)    :: secname            ! argument
      integer                         :: indx               ! result
      integer                         :: nsections          ! local
      character(len=CARDSET_LENGTH)   :: name2              ! local
      character(len=CARDSET_LENGTH)   :: name3              ! local

      nsections = cardsetlist_num_cardsets (obj%cardsetlist)
      name2 = secname
      call string_to_upper (name2)
      do indx = 1,nsections
          obj%cardset => cardsetlist_get_cardset (obj%cardsetlist,indx)
          call cardset_get_name (obj%cardset, name3)
          if (name3 == name2) return
      end do
      nullify(obj%cardset)
      indx = 0
      return
      end function pjar_find_section



      subroutine pjar_choose_no_section (obj)
      implicit none
      type(pjar_struct),intent(inout) :: obj              ! argument

      nullify (obj%cardset)
      return
      end subroutine pjar_choose_no_section



      subroutine pjar_choose_section_by_indx (obj,indx)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument
      integer          ,intent(in)           :: indx             ! argument
      integer,parameter                      :: lunstop = 6      ! local

      obj%cardset => cardsetlist_get_cardset (obj%cardsetlist,indx)
      if (.not.associated(obj%cardset)) then
           write(lunstop,*) '--> Illegally calling pjar_choose_section &
                                                 &when INDX is out of range.'
           write(lunstop,*) '--> This is a programming error &
                                                 &in the calling program.'
           stop
      end if
      return
      end subroutine pjar_choose_section_by_indx



      subroutine pjar_choose_section_by_name (obj,secname)
      implicit none
      type(pjar_struct),intent(inout)       :: obj              ! argument
      character(len=*) ,intent(in)          :: secname          ! argument

      if (secname == ' ') then
        nullify (obj%cardset)
      else
        obj%cardset => cardsetlist_find_or_add_cardset &
                                                (obj%cardsetlist,secname)
      end if
      return
      end subroutine pjar_choose_section_by_name



      subroutine pjar_get_secname (obj,secname)
      implicit none
      type(pjar_struct),intent(inout)        :: obj             ! argument
      character(len=*) ,intent(out),optional :: secname         ! argument

      if (present(secname)) then
           if (associated(obj%cardset)) then
                call cardset_get_name (obj%cardset,secname)
           else
                secname = ' '
           end if
      end if
      return
      end subroutine pjar_get_secname


! Note: The BOTH_PRESENT variable was introduced below to get around a
! bug in the new Absoft compiler which makes this complaint:
! Dummy arguments with the INTENT(OUT) attribute must be defined before use.


      subroutine pjar_get_secnames (obj,secnames,nsections)
      implicit none
      type(pjar_struct),intent(in)           :: obj            ! argument
      character(len=*) ,intent(out),optional :: secnames(:)    ! argument
      integer          ,intent(out),optional :: nsections      ! argument
      integer                                :: ndummy         ! local
      logical                                :: both_present   ! local

      both_present = (present(secnames) .and. present(nsections))
 !!!  if (present(secnames) .and. present(nsections)) then
      if (both_present) then
           call cardsetlist_get_cardset_names &
                                          (obj%cardsetlist,secnames,nsections)
      else if (present(secnames)) then
           call cardsetlist_get_cardset_names &
                                          (obj%cardsetlist,secnames,ndummy)
      else if (present(nsections)) then
           nsections = cardsetlist_num_cardsets (obj%cardsetlist)
      end if
      return
      end subroutine pjar_get_secnames


!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!


      function pjar_num_elements (obj,keyword) result (nelements)
      implicit none
      type(pjar_struct),intent(inout) :: obj               ! argument
      character(len=*) ,intent(in)    :: keyword           ! argument
      integer                         :: nelements         ! result

      call        pjar_private_abort   (obj,'num_elements',keyword)
      nelements = cardset_num_elements (obj%cardset,keyword)
      return
      end function pjar_num_elements


!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!


      function pjar_nature (obj,keyword) result (nature)
      implicit none
      type(pjar_struct),intent(inout) :: obj            ! argument
      character(len=*) ,intent(in)    :: keyword        ! argument
      integer                         :: nature         ! result

      call     pjar_private_abort (obj,'nature',keyword)
      nature = cardset_nature     (obj%cardset,keyword)
      return
      end function pjar_nature



!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!


      subroutine pjar_get_gscalar (obj,keyword,scalar,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      type(grid_struct),intent(out)          :: scalar            ! argument
      type(grid_struct),intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local
      type(grid_struct)                      :: gnil              ! local

      call grid_initialize     (scalar)
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_scalar  (obj%cardset,keyword,scalar,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval)) then
           call grid_initialize (gnil)
           if (scalar == gnil) scalar = defval
      end if
      return
      end subroutine pjar_get_gscalar



      subroutine pjar_get_iscalar (obj,keyword,scalar,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      integer          ,intent(out)          :: scalar            ! argument
      integer          ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      scalar = INIL
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_scalar  (obj%cardset,keyword,scalar,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval) .and. scalar == INIL) scalar = defval
      return
      end subroutine pjar_get_iscalar



      subroutine pjar_get_fscalar (obj,keyword,scalar,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      real             ,intent(out)          :: scalar            ! argument
      real             ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      scalar = FNIL
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_scalar  (obj%cardset,keyword,scalar,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval) .and. scalar == FNIL) scalar = defval
      return
      end subroutine pjar_get_fscalar



      subroutine pjar_get_dscalar (obj,keyword,scalar,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      double precision ,intent(out)          :: scalar            ! argument
      double precision ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      scalar = DNIL
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_scalar  (obj%cardset,keyword,scalar,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval) .and. scalar == DNIL) scalar = defval
      return
      end subroutine pjar_get_dscalar



      subroutine pjar_get_cscalar (obj,keyword,scalar,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(out)          :: scalar            ! argument
      character(len=*) ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      scalar = CNIL
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_scalar  (obj%cardset,keyword,scalar,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval) .and. scalar == CNIL) scalar = defval
      return
      end subroutine pjar_get_cscalar



      subroutine pjar_get_lscalar (obj,keyword,scalar,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      logical          ,intent(out)          :: scalar            ! argument
      logical          ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      scalar = LNIL
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_scalar  (obj%cardset,keyword,scalar,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval) .and. (scalar .eqv. LNIL)) scalar = defval
      return
      end subroutine pjar_get_lscalar


!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!


      subroutine pjar_get_iarray (obj,keyword,array,nelements,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj                ! argument
      character(len=*) ,intent(in)           :: keyword            ! argument
      integer          ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out),optional :: nelements          ! argument
      integer          ,intent(in) ,optional :: defval             ! argument
      character(len=80)                      :: errmsg             ! local

      if (.not.present(nelements)) return
      array(:)  = INIL
      nelements = 0
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_array   (obj%cardset,keyword,array,nelements,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval)) then
           where (array(:) == INIL) array(:) = defval
      end if
      return
      end subroutine pjar_get_iarray



      subroutine pjar_get_farray (obj,keyword,array,nelements,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj                ! argument
      character(len=*) ,intent(in)           :: keyword            ! argument
      real             ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out),optional :: nelements          ! argument
      real             ,intent(in) ,optional :: defval             ! argument
      character(len=80)                      :: errmsg             ! local

      if (.not.present(nelements)) return
      array(:)  = FNIL
      nelements = 0
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_array   (obj%cardset,keyword,array,nelements,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval)) then
           where (array(:) == FNIL) array(:) = defval
      end if
      return
      end subroutine pjar_get_farray



      subroutine pjar_get_darray (obj,keyword,array,nelements,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj                ! argument
      character(len=*) ,intent(in)           :: keyword            ! argument
      double precision ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out),optional :: nelements          ! argument
      double precision ,intent(in) ,optional :: defval             ! argument
      character(len=80)                      :: errmsg             ! local

      if (.not.present(nelements)) return
      array(:)  = DNIL
      nelements = 0
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_array   (obj%cardset,keyword,array,nelements,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval)) then
           where (array(:) == DNIL) array(:) = defval
      end if
      return
      end subroutine pjar_get_darray



      subroutine pjar_get_larray (obj,keyword,array,nelements,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj                ! argument
      character(len=*) ,intent(in)           :: keyword            ! argument
      logical          ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out),optional :: nelements          ! argument
      logical          ,intent(in) ,optional :: defval             ! argument
      character(len=80)                      :: errmsg             ! local

      if (.not.present(nelements)) return
      array(:)  = LNIL
      nelements = 0
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_array   (obj%cardset,keyword,array,nelements,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval)) then
           where (array(:) .eqv. LNIL) array(:) = defval
      end if
      return
      end subroutine pjar_get_larray



      subroutine pjar_get_carray (obj,keyword,array,nelements,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj                ! argument
      character(len=*) ,intent(in)           :: keyword            ! argument
      character(len=*) ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out),optional :: nelements          ! argument
      character(len=*) ,intent(in) ,optional :: defval             ! argument
      character(len=80)                      :: errmsg             ! local
      integer                                :: indx               ! local

      if (.not.present(nelements)) return
      array(:)  = CNIL
      nelements = 0
      call pjar_private_abort  (obj,'get',keyword)
      call cardset_get_array   (obj%cardset,keyword,array,nelements,errmsg)
      call pjar_private_whoops (obj,errmsg)
      if (present(defval)) then
 !!!       where (array(:) == CNIL) array(:) = defval             !! sgi abort.
           do indx = 1,nelements                                  !! sgi ok.
                if (array(indx) == CNIL) array(indx) = defval     !! sgi ok.
           end do                                                 !! sgi ok.
      end if
      return
      end subroutine pjar_get_carray


!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!


      subroutine pjar_put_gscalar (obj,keyword,scalar,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      type(grid_struct),intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar,ndec    ! argument

      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar,ndec)
      return
      end subroutine pjar_put_gscalar



      subroutine pjar_put_iscalar (obj,keyword,scalar,nchar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      integer          ,intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar         ! argument

      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar)
      return
      end subroutine pjar_put_iscalar



      subroutine pjar_put_fscalar (obj,keyword,scalar,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      real             ,intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar,ndec    ! argument

      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar,ndec)
      return
      end subroutine pjar_put_fscalar



      subroutine pjar_put_dscalar (obj,keyword,scalar,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      double precision ,intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar,ndec    ! argument

      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar,ndec)
      return
      end subroutine pjar_put_dscalar



      subroutine pjar_put_cscalar (obj,keyword,scalar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      character(len=*) ,intent(in)          :: scalar        ! argument

      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_scalar (obj%cardset,keyword,scalar)
      return
      end subroutine pjar_put_cscalar



      subroutine pjar_put_lscalar (obj,keyword,scalar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      logical          ,intent(in)          :: scalar        ! argument

      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_scalar (obj%cardset,keyword,scalar)
      return
      end subroutine pjar_put_lscalar


!!------------------------- augment scalars --------------------------------!!
!!------------------------- augment scalars --------------------------------!!
!!------------------------- augment scalars --------------------------------!!


      subroutine pjar_augment_gscalar (obj,keyword,scalar,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      type(grid_struct),intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar,ndec    ! argument

      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar,ndec)
      return
      end subroutine pjar_augment_gscalar



      subroutine pjar_augment_iscalar (obj,keyword,scalar,nchar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      integer          ,intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar         ! argument

      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar)
      return
      end subroutine pjar_augment_iscalar



      subroutine pjar_augment_fscalar (obj,keyword,scalar,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      real             ,intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar,ndec    ! argument

      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar,ndec)
      return
      end subroutine pjar_augment_fscalar



      subroutine pjar_augment_dscalar (obj,keyword,scalar,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      double precision ,intent(in)          :: scalar        ! argument
      integer          ,intent(in),optional :: nchar,ndec    ! argument

      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_scalar (obj%cardset,keyword,scalar,nchar,ndec)
      return
      end subroutine pjar_augment_dscalar


!!!! idea:

  !   subroutine pjar_augment_cscalar (obj,keyword,defval,scalar)
  !   implicit none
  !   type(pjar_struct),intent(inout)        :: obj           ! argument
  !   character(len=*) ,intent(in)           :: keyword       ! argument
  !   character(len=*) ,intent(in)           :: defval        ! argument
  !   character(len=*) ,intent(out),optional :: scalar        ! argument
  !   character(len=CARDSET_LENGTH)          :: scalar2       ! argument

  !   scalar2 = CNIL
  !   call pjar_private_abort  (obj,'augment',keyword)
  !   call cardset_get_scalar  (obj%cardset,keyword,scalar2,errmsg)
  !   call pjar_private_whoops (obj,errmsg)
  !   if (scalar2 == CNIL) then
  !        scalar2 = defval
  !        call cardset_put_scalar  (obj%cardset,keyword,scalar2)
  !   end if
  !   if (present(scalar)) scalar = scalar2
  !   return
  !   end subroutine pjar_augment_cscalar

!!!! the above differs from the below as follows:
!!!!  the above replaces an existing value with DEFVAL if it is nil.
!!!!  the below never changes an existing value even if it is nil.
!!!!  in the above, to keep a nil value, DEFVAL must be nil.
!!!!  the above optionally returns the value in SCALAR.
!!!! in the below, SCALAR is really the same as DEFVAL above.



      subroutine pjar_augment_cscalar (obj,keyword,scalar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      character(len=*) ,intent(in)          :: scalar        ! argument

      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_scalar (obj%cardset,keyword,scalar)
      return
      end subroutine pjar_augment_cscalar



      subroutine pjar_augment_lscalar (obj,keyword,scalar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj           ! argument
      character(len=*) ,intent(in)          :: keyword       ! argument
      logical          ,intent(in)          :: scalar        ! argument

      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_scalar (obj%cardset,keyword,scalar)
      return
      end subroutine pjar_augment_lscalar


!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!


      subroutine pjar_put_iarray (obj,keyword,array,nelements,nchar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      integer          ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument
      integer          ,intent(in),optional :: nchar              ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_array  (obj%cardset,keyword,array,nelements,nchar)
      return
      end subroutine pjar_put_iarray



      subroutine pjar_put_farray (obj,keyword,array,nelements,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      real             ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument
      integer          ,intent(in),optional :: nchar,ndec         ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_array  (obj%cardset,keyword,array,nelements,nchar,ndec)
      return
      end subroutine pjar_put_farray



      subroutine pjar_put_darray (obj,keyword,array,nelements,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      double precision ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument
      integer          ,intent(in),optional :: nchar,ndec         ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_array  (obj%cardset,keyword,array,nelements,nchar,ndec)
      return
      end subroutine pjar_put_darray



      subroutine pjar_put_carray (obj,keyword,array,nelements)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      character(len=*) ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_array  (obj%cardset,keyword,array,nelements)
      return
      end subroutine pjar_put_carray



      subroutine pjar_put_larray (obj,keyword,array,nelements)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      logical          ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'put',keyword)
      call cardset_put_array  (obj%cardset,keyword,array,nelements)
      return
      end subroutine pjar_put_larray


!!------------------------- augment arrays ---------------------------------!!
!!------------------------- augment arrays ---------------------------------!!
!!------------------------- augment arrays ---------------------------------!!


      subroutine pjar_augment_iarray (obj,keyword,array,nelements,nchar)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      integer          ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument
      integer          ,intent(in),optional :: nchar              ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_array  (obj%cardset,keyword,array,nelements,nchar)
      return
      end subroutine pjar_augment_iarray



      subroutine pjar_augment_farray (obj,keyword,array,nelements,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      real             ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument
      integer          ,intent(in),optional :: nchar,ndec         ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_array  (obj%cardset,keyword,array,nelements,nchar,ndec)
      return
      end subroutine pjar_augment_farray



      subroutine pjar_augment_darray (obj,keyword,array,nelements,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      double precision ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument
      integer          ,intent(in),optional :: nchar,ndec         ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_array  (obj%cardset,keyword,array,nelements,nchar,ndec)
      return
      end subroutine pjar_augment_darray



      subroutine pjar_augment_carray (obj,keyword,array,nelements)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      character(len=*) ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_array  (obj%cardset,keyword,array,nelements)
      return
      end subroutine pjar_augment_carray



      subroutine pjar_augment_larray (obj,keyword,array,nelements)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      logical          ,intent(in)          :: array(:)           ! argument
      integer          ,intent(in),optional :: nelements          ! argument

      if (.not.present(nelements)) return
      call pjar_private_abort (obj,'augment',keyword)
      if (cardset_keyword_present(obj%cardset,keyword)) return
      call cardset_put_array  (obj%cardset,keyword,array,nelements)
      return
      end subroutine pjar_augment_larray


!!----------------------------- find element -------------------------------!!
!!----------------------------- find element -------------------------------!!
!!----------------------------- find element -------------------------------!!


      function pjar_find_ielement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      integer          ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort   (obj,'find',keyword)
      indx = cardset_find_element (obj%cardset,keyword,element)
      return
      end function pjar_find_ielement



      function pjar_find_felement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      real             ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort   (obj,'find',keyword)
      indx = cardset_find_element (obj%cardset,keyword,element)
      return
      end function pjar_find_felement



      function pjar_find_delement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      double precision ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort   (obj,'find',keyword)
      indx = cardset_find_element (obj%cardset,keyword,element)
      return
      end function pjar_find_delement



      function pjar_find_celement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      character(len=*) ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort   (obj,'find',keyword)
      indx = cardset_find_element (obj%cardset,keyword,element)
      return
      end function pjar_find_celement



      function pjar_find_lelement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      logical          ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort   (obj,'find',keyword)
      indx = cardset_find_element (obj%cardset,keyword,element)
      return
      end function pjar_find_lelement


!!-------------------------- find add element -------------------------------!!
!!-------------------------- find add element -------------------------------!!
!!-------------------------- find add element -------------------------------!!


      function pjar_find_add_ielement &
                              (obj, keyword, element, nchar) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      integer          ,intent(in)          :: element            ! argument
      integer          ,intent(in),optional :: nchar              ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort          (obj,'find_add',keyword)
      indx = cardset_find_or_add_element (obj%cardset,keyword,element,nchar)
      return
      end function pjar_find_add_ielement



      function pjar_find_add_felement &
                              (obj, keyword, element,nchar,ndec) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      real             ,intent(in)          :: element            ! argument
      integer          ,intent(in),optional :: nchar,ndec         ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort          (obj,'find_add',keyword)
      indx = cardset_find_or_add_element (obj%cardset,keyword,element, &
                                                        nchar,ndec)
      return
      end function pjar_find_add_felement



      function pjar_find_add_delement &
                              (obj, keyword, element,nchar,ndec) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      double precision ,intent(in)          :: element            ! argument
      integer          ,intent(in),optional :: nchar,ndec         ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort          (obj,'find_add',keyword)
      indx = cardset_find_or_add_element (obj%cardset,keyword,element, &
                                                        nchar,ndec)
      return
      end function pjar_find_add_delement



      function pjar_find_add_celement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      character(len=*) ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort          (obj,'find_add',keyword)
      indx = cardset_find_or_add_element (obj%cardset,keyword,element)
      return
      end function pjar_find_add_celement



      function pjar_find_add_lelement (obj, keyword, element) result (indx)
      implicit none
      type(pjar_struct),intent(inout)       :: obj                ! argument
      character(len=*) ,intent(in)          :: keyword            ! argument
      logical          ,intent(in)          :: element            ! argument
      integer                               :: indx               ! result

      call   pjar_private_abort          (obj,'find_add',keyword)
      indx = cardset_find_or_add_element (obj%cardset,keyword,element)
      return
      end function pjar_find_add_lelement


!!------------------------------ put element ----------------------------!!
!!------------------------------ put element ----------------------------!!
!!------------------------------ put element ----------------------------!!


      subroutine pjar_put_ielement (obj,keyword,indx,element,nchar)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument
      integer          ,intent(in)    :: element             ! argument
      integer,optional ,intent(in)    :: nchar               ! argument

      call pjar_private_abort             (obj,'put_element',keyword)
      call cardset_replace_or_add_element (obj%cardset,keyword, &
                                                   indx,element,nchar)
      return
      end subroutine pjar_put_ielement



      subroutine pjar_put_felement (obj,keyword,indx,element,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument
      real             ,intent(in)    :: element             ! argument
      integer,optional ,intent(in)    :: nchar,ndec          ! argument

      call pjar_private_abort             (obj,'put_element',keyword)
      call cardset_replace_or_add_element (obj%cardset,keyword, &
                                                   indx,element,nchar,ndec)
      return
      end subroutine pjar_put_felement



      subroutine pjar_put_delement (obj,keyword,indx,element,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument
      double precision ,intent(in)    :: element             ! argument
      integer,optional ,intent(in)    :: nchar,ndec          ! argument

      call pjar_private_abort             (obj,'put_element',keyword)
      call cardset_replace_or_add_element (obj%cardset,keyword, &
                                                   indx,element,nchar,ndec)
      return
      end subroutine pjar_put_delement



      subroutine pjar_put_celement (obj,keyword,indx,element)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument
      character(len=*) ,intent(in)    :: element             ! argument

      call pjar_private_abort             (obj,'put_element',keyword)
      call cardset_replace_or_add_element (obj%cardset,keyword,indx,element)
      return
      end subroutine pjar_put_celement



      subroutine pjar_put_lelement (obj,keyword,indx,element)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument
      logical          ,intent(in)    :: element             ! argument

      call pjar_private_abort             (obj,'put_element',keyword)
      call cardset_replace_or_add_element (obj%cardset,keyword,indx,element)
      return
      end subroutine pjar_put_lelement


!!------------------------------ add element ----------------------------!!
!!------------------------------ add element ----------------------------!!
!!------------------------------ add element ----------------------------!!


      subroutine pjar_add_ielement (obj,keyword,element,nchar)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: element             ! argument
      integer,optional ,intent(in)    :: nchar               ! argument

      call pjar_private_abort  (obj,'add_element',keyword)
      call cardset_add_element (obj%cardset,keyword,element,nchar)
      return
      end subroutine pjar_add_ielement



      subroutine pjar_add_felement (obj,keyword,element,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      real             ,intent(in)    :: element             ! argument
      integer,optional ,intent(in)    :: nchar,ndec          ! argument

      call pjar_private_abort  (obj,'add_element',keyword)
      call cardset_add_element (obj%cardset,keyword,element,nchar,ndec)
      return
      end subroutine pjar_add_felement



      subroutine pjar_add_delement (obj,keyword,element,nchar,ndec)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      double precision ,intent(in)    :: element             ! argument
      integer,optional ,intent(in)    :: nchar,ndec          ! argument

      call pjar_private_abort  (obj,'add_element',keyword)
      call cardset_add_element (obj%cardset,keyword,element,nchar,ndec)
      return
      end subroutine pjar_add_delement



      subroutine pjar_add_celement (obj,keyword,element)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      character(len=*) ,intent(in)    :: element             ! argument

      call pjar_private_abort  (obj,'add_element',keyword)
      call cardset_add_element (obj%cardset,keyword,element)
      return
      end subroutine pjar_add_celement



      subroutine pjar_add_lelement (obj,keyword,element)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      logical          ,intent(in)    :: element             ! argument

      call pjar_private_abort  (obj,'add_element',keyword)
      call cardset_add_element (obj%cardset,keyword,element)
      return
      end subroutine pjar_add_lelement


!!------------------------- access the buffer -------------------------------!!
!!------------------------- access the buffer -------------------------------!!
!!------------------------- access the buffer -------------------------------!!


      subroutine pjar_insert_element (obj,keyword,indx)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument

      call pjar_private_abort     (obj,'insert_element',keyword)
      if (cardset_num_elements(obj%cardset,keyword) < indx-1) &
          call cardset_replace_or_add_element (obj%cardset,keyword,indx-1,CNIL)
      call cardset_insert_element (obj%cardset,keyword,indx)
      return
      end subroutine pjar_insert_element



      subroutine pjar_remove_element (obj,keyword,indx)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument
      integer          ,intent(in)    :: indx                ! argument

      call pjar_private_abort     (obj,'remove_element',keyword)
      if (cardset_num_elements(obj%cardset,keyword) < indx) &
          call cardset_replace_or_add_element (obj%cardset,keyword,indx,CNIL)
      call cardset_remove_element (obj%cardset,keyword,indx)
      return
      end subroutine pjar_remove_element



      subroutine pjar_clear_buffer (obj,keyword)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: keyword             ! argument

      call pjar_private_abort   (obj,'clear_buffer',keyword)
      call cardset_clear_buffer (obj%cardset,keyword)
      return
      end subroutine pjar_clear_buffer


!!---------------------- get element --------------------------------------!!
!!---------------------- get element --------------------------------------!!
!!---------------------- get element --------------------------------------!!


      subroutine pjar_get_ielement (obj,keyword,indx,element,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      integer          ,intent(in)           :: indx              ! argument
      integer          ,intent(out)          :: element           ! argument
      integer          ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      element = INIL
      call pjar_private_abort  (obj,'get',keyword)
      if (indx >= 1 .and. indx <= cardset_num_elements(obj%cardset,keyword) &
          .and. cardset_nature(obj%cardset,keyword) == CARDSET_ARRAY) then
        call cardset_get_element (obj%cardset,keyword,indx,element,errmsg)
        call pjar_private_whoops (obj,errmsg)
      end if
      if (present(defval) .and. element == INIL) element = defval
      return
      end subroutine pjar_get_ielement



      subroutine pjar_get_felement (obj,keyword,indx,element,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      integer          ,intent(in)           :: indx              ! argument
      real             ,intent(out)          :: element           ! argument
      real             ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      element = FNIL
      call pjar_private_abort  (obj,'get',keyword)
      if (indx >= 1 .and. indx <= cardset_num_elements(obj%cardset,keyword) &
          .and. cardset_nature(obj%cardset,keyword) == CARDSET_ARRAY) then
        call cardset_get_element (obj%cardset,keyword,indx,element,errmsg)
        call pjar_private_whoops (obj,errmsg)
      end if
      if (present(defval) .and. element == FNIL) element = defval
      return
      end subroutine pjar_get_felement



      subroutine pjar_get_delement (obj,keyword,indx,element,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      integer          ,intent(in)           :: indx              ! argument
      double precision ,intent(out)          :: element           ! argument
      double precision ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      element = DNIL
      call pjar_private_abort  (obj,'get',keyword)
      if (indx >= 1 .and. indx <= cardset_num_elements(obj%cardset,keyword) &
          .and. cardset_nature(obj%cardset,keyword) == CARDSET_ARRAY) then
        call cardset_get_element (obj%cardset,keyword,indx,element,errmsg)
        call pjar_private_whoops (obj,errmsg)
      end if
      if (present(defval) .and. element == DNIL) element = defval
      return
      end subroutine pjar_get_delement



      subroutine pjar_get_celement (obj,keyword,indx,element,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      integer          ,intent(in)           :: indx              ! argument
      character(len=*) ,intent(out)          :: element           ! argument
      character(len=*) ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      element = CNIL
      call pjar_private_abort  (obj,'get',keyword)
      if (indx >= 1 .and. indx <= cardset_num_elements(obj%cardset,keyword) &
          .and. cardset_nature(obj%cardset,keyword) == CARDSET_ARRAY) then
        call cardset_get_element (obj%cardset,keyword,indx,element,errmsg)
        call pjar_private_whoops (obj,errmsg)
      end if
      if (present(defval) .and. element == CNIL) element = defval
      return
      end subroutine pjar_get_celement



      subroutine pjar_get_lelement (obj,keyword,indx,element,defval)
      implicit none
      type(pjar_struct),intent(inout)        :: obj               ! argument
      character(len=*) ,intent(in)           :: keyword           ! argument
      integer          ,intent(in)           :: indx              ! argument
      logical          ,intent(out)          :: element           ! argument
      logical          ,intent(in),optional  :: defval            ! argument
      character(len=80)                      :: errmsg            ! local

      element = LNIL
      call pjar_private_abort  (obj,'get',keyword)
      if (indx >= 1 .and. indx <= cardset_num_elements(obj%cardset,keyword) &
          .and. cardset_nature(obj%cardset,keyword) == CARDSET_ARRAY) then
        call cardset_get_element (obj%cardset,keyword,indx,element,errmsg)
        call pjar_private_whoops (obj,errmsg)
      end if
      if (present(defval) .and. (element .eqv. LNIL)) element = defval
      return
      end subroutine pjar_get_lelement


!!---------------------- get and put data cards ----------------------------!!
!!---------------------- get and put data cards ----------------------------!!
!!---------------------- get and put data cards ----------------------------!!


      function pjar_num_cards (obj) result (ncards)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument
      integer                         :: ncards             ! result

      call     pjar_private_abort (obj,'num_cards')
      ncards = cardset_num_cards  (obj%cardset)
      return
      end function pjar_num_cards



      subroutine pjar_alloc_cards (obj,pcards,ncards)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument
      character(len=*) ,pointer    ,optional :: pcards(:)        ! argument
      integer          ,intent(out),optional :: ncards           ! argument

      if (.not.present(pcards)) return
      if (.not.present(ncards)) return
      call pjar_private_abort  (obj,'alloc_cards')
      call cardset_alloc_cards (obj%cardset,pcards,ncards)
      return
      end subroutine pjar_alloc_cards

   

      subroutine pjar_get_cards (obj,cards,ncards)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument
      character(len=*)             ,optional :: cards(:)         ! argument
      integer          ,intent(out),optional :: ncards           ! argument
      character(len=80)                      :: errmsg           ! local

      if (.not.present(cards )) return
      if (.not.present(ncards)) return
      call pjar_private_abort  (obj,'get_cards')
      call cardset_get_cards   (obj%cardset,cards,ncards,errmsg)
      call pjar_private_whoops (obj,errmsg)
      return
      end subroutine pjar_get_cards


 
      subroutine pjar_clear_cards (obj)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument

      call pjar_private_abort (obj,'clear_cards')
      call cardset_clear      (obj%cardset)
      return
      end subroutine pjar_clear_cards


 
      subroutine pjar_put_cards (obj,cards,ncards,progname,lastcard)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument
      character(len=*) ,intent(in) ,optional :: cards(:)         ! argument
      integer          ,intent(in) ,optional :: ncards           ! argument
      character(len=*) ,intent(in) ,optional :: progname         ! argument
      character(len=*) ,intent(out),optional :: lastcard         ! argument

      call pjar_private_abort (obj,'put_cards')
      if (present(cards) .and. present(ncards)) then
           call cardset_put_cards (obj%cardset,cards,ncards)
      else
           call cardset_clear (obj%cardset)
      end if
      call pjar_new_card (obj,progname,lastcard)
      return
      end subroutine pjar_put_cards



      subroutine pjar_add_cards (obj,cards,ncards,progname,lastcard)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument
      character(len=*) ,intent(in) ,optional :: cards(:)         ! argument
      integer          ,intent(in) ,optional :: ncards           ! argument
      character(len=*) ,intent(in) ,optional :: progname         ! argument
      character(len=*) ,intent(out),optional :: lastcard         ! argument

      call pjar_private_abort (obj,'add_cards')
      if (present(cards) .and. present(ncards)) then
           call cardset_add_cards (obj%cardset,cards,ncards)
      end if
      call pjar_new_card (obj,progname,lastcard)
      return
      end subroutine pjar_add_cards



      subroutine pjar_get_card (obj,indx,card)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      integer          ,intent(in)    :: indx                ! argument
      character(len=*) ,intent(out)   :: card                ! argument
      character(len=80)               :: errmsg              ! local

      call pjar_private_abort  (obj,'get_card')
      call cardset_get_card    (obj%cardset,indx,card,errmsg)
      call pjar_private_whoops (obj,errmsg)
      return
      end subroutine pjar_get_card



      subroutine pjar_add_card (obj,card)
      implicit none
      type(pjar_struct),intent(inout) :: obj                 ! argument
      character(len=*) ,intent(in)    :: card                ! argument

      call pjar_private_abort (obj,'add_card')
      call cardset_add_card   (obj%cardset,card)
      return
      end subroutine pjar_add_card



      subroutine pjar_new_card (obj,progname,lastcard)
      implicit none
      type(pjar_struct),intent(inout)        :: obj              ! argument
      character(len=*) ,intent(in) ,optional :: progname         ! argument
      character(len=*) ,intent(out),optional :: lastcard         ! argument
      character(len=80)                      :: lastcard2        ! local

      call pjar_private_abort (obj,'new_card')
      if (.not.present(progname)) then
           lastcard2 = ' '
      else if (progname == ' ') then
           lastcard2 = ' '
      else
           call string_time_date (lastcard2)
           lastcard2 = 'file written by '//trim(progname)//' '//lastcard2
           call cardset_add_card (obj%cardset, lastcard2)
      end if
      if (present(lastcard)) lastcard = lastcard2
      return
      end subroutine pjar_new_card


!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!


      function pjar_keyword_present (obj,keyword) result (present)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical                         :: present            ! result

      call      pjar_private_abort      (obj,'keyword_present',keyword)
      present = cardset_keyword_present (obj%cardset,keyword)
      return
      end function pjar_keyword_present



      function pjar_num_keywords (obj) result (nkeys)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument
      integer                         :: nkeys              ! result

      call    pjar_private_abort   (obj,'num_keywords')
      nkeys = cardset_num_keywords (obj%cardset)
      return
      end function pjar_num_keywords



      function pjar_get_keyword (obj,indx) result (keyword)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument
      integer          ,intent(in)    :: indx               ! argument
      character(len=40)               :: keyword            ! result

      call      pjar_private_abort  (obj,'get_keyword')
      keyword = cardset_get_keyword (obj%cardset,indx)
      return
      end function pjar_get_keyword



      subroutine pjar_remove_keyword (obj,keyword)
      implicit none
      type(pjar_struct),intent(inout) :: obj                ! argument
      character(len=*) ,intent(in)    :: keyword            ! argument

      call pjar_private_abort     (obj,'remove_keyword',keyword)
      call cardset_remove_keyword (obj%cardset,keyword)
      return
      end subroutine pjar_remove_keyword


!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!


      end module pjar_module


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

