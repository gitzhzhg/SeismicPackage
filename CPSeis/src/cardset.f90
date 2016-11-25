
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
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ cardset.f90 --------------------------------!!
!!------------------------------ cardset.f90 --------------------------------!!
!!------------------------------ cardset.f90 --------------------------------!!





!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E   
!
! Name       : CARDSET 
! Category   : character
! Written    : 1999-06-22   by: Tom Stoeckley
! Revised    : 2008-12-11   by: B. Menger
! Maturity   : beta
! Purpose    : Module for storing and retrieving a set of data cards.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>



!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION       
!
! This module is a repository for a set of data cards.  This module is
! used by the "parameter cache" in the Conoco Processing System, and by
! file I/O software used in the Conoco Processing System and related
! interactive workstation programs.
!
! This module is ignorant of the nature of the data cards it contains.
! These data cards can be inserted into, or removed from, this module
! all at once or one at a time.  They can contain any miscellaneous
! information, or they can be keyword-encoded.  Keyword-encoded data cards
! can be in either a packed or an unpacked format.  Information (scalar or
! array values of any variable type) can be encoded by keyword into the
! data cards, or decoded by keyword from the data cards.
!
! Each parameter stored in keyword-encoded data cards can be a scalar data
! value (a single value) or an array of data values (zero or more values).
! The data values can be integer, real, double precision, logical, or character
! variables.  Scalar data values can also be type (grid_struct).  Each
! parameter is retained as a string or a list of strings, and therefore can
! be provided or retrieved either as strings or as variables of any compatible
! type.
!
! Each parameter is identified by a unique keyword which is case-insensitive.
! Any calling program can create any parameter simply by calling an appropriate
! subroutine with a unique keyword.
!
! The subroutines in this module are grouped into several categories which
! are documented in separate sections:
!
!      - routines for creating, clearing, and deleting the cardset.
!      - routines for accessing the name of the cardset.
!      - routines for accessing the packing option for data cards.
!      - routines for accessing data cards.
!      - routines for getting keyword information.
!      - routines for accessing parameter values.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS   
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                  CREATE AND CLEAR AND DELETE THE CARDSET    
!
!                       call cardset_create (obj)
!                       call cardset_clear  (obj)
!                       call cardset_delete (obj)
!                       call cardset_copy   (obj1, obj2)
!                       call cardset_append (obj1, obj2)
!                                             i     b
!
! type(cardset_struct)      obj = the cardset structure.
!
! CARDSET_CREATE:
!   (1) allocates obj (PASSED AS A POINTER).
!   (2) initializes the contants to contain no parameters or data cards.
!   (3) initializes the packing option to CARDSET_UNPACKED.
!   (4) sets the name of the cardset to blank.
!
! CARDSET_CLEAR:
!   (1) deletes all parameters and data cards.
!   (2) resets the packing option to CARDSET_UNPACKED.
!   (3) does not change the name of the cardset.
!
! CARDSET_DELETE:
!   (1) deletes all parameters and data cards.
!   (2) deallocates obj (PASSED AS A POINTER).
!
! CARDSET_COPY:
!   (1) copies the contents of OBJ1 to OBJ2.
!   (2) the previous contents of OBJ2 are deleted first.
!   (3) all data structure variables are copied.
!
! CARDSET_APPEND:
!   (1) adds the contents of OBJ1 to the contents of OBJ2.
!   (2) the contents are copied as data cards, having the effect that
!        existing parameters are replaced and new parameters are added.
!   (3) other data structure variables are not copied.
!
! The nature of each parameter can be one of two possibilities:
!   (1) the parameter can be a SCALAR (a single scalar parameter).
!   (2) the parameter can be an ARRAY (zero or more array elements).
!
! Note that an ARRAY with one element is different from a SCALAR.
!
!-------------------------------------------------------------------------------
!                  TO ACCESS THE NAME OF THE CARDSET 
!
!                                               i
!                  call cardset_set_name (obj, name)
!                  call cardset_get_name (obj, name)
!                                               o
!
!                                                   i
!             matches = cardset_name_matches (obj, name)
!
! type(cardset_struct)   obj = the cardset structure.
! character(len=*)      name = name of the cardset.
!
! The name can be blank.
! The name is converted to upper case when input.
! The name matching is done in a case-insensitive manner.
!
!-------------------------------------------------------------------------------
!               TO ACCESS THE PACKING OPTION FOR DATA CARDS    
!
!                                                       i
!               call cardset_set_packing_option (obj,packing)
!               call cardset_get_packing_option (obj,packing)
!                                                       o
!
! type(cardset_struct)    obj = the cardset structure.
! integer             packing = packing option to use when getting data cards.
!
!
!   packing option (named constants)  format of data cards output
!   --------------------------------  ---------------------------
!   CARDSET_UNPACKED                  Cards are output in unpacked format.
!   CARDSET_PACKED                    Cards are output in  packed  format.
!
! The packing option is initialized to CARDSET_UNPACKED by CARDSET_CREATE.
! The packing option is    reset    to CARDSET_UNPACKED by CARDSET_CLEAR.
!
! The packing option is irrelevant when data cards are input; data cards will
! be decoded correctly (if required) regardless of how they are packed.
! The packing option is relevant only when data cards are encoded from a
! list of keywords and values.  Data cards will be encoded from a list of
! keywords and parameters only if information is input by keyword and later
! requested as data cards.
!
! If data cards are input and later output, and parameters are never
! accessed by keyword, the packing option is irrelevant and the data cards
! will be output as they were input.
!
! The packing option is irrelevant when accessing parameters by keyword.
! If parameters are input and later output by keyword, and data cards are
! never accessed, the packing option is irrelevant and the parameters
! will be output as they were input.
!
! The data cards are keyword-encoded if they are input that way, or if a
! subroutine is called to add a parameter by keyword.  Keyword-encoded and
! verbatum (i.e. not keyword-encoded) data cards should not be mixed.  An
! attempt to get a value by keyword from verbatum data cards will have
! unpredictable consequences.  This cardset object will not know whether the
! data cards are keyword-encoded if they are input; it is up to the user
! to know that; this cardset object will assume that they are verbatum unless
! an attempt is made to get or put a value by keyword.
!
! Note that the packing option is specified with named constants rather than
! simply as a true/false logical value.  This allows the possible future
! addition of more packing options with minimum changes to existing code
! in the event that other formats become desirable.
!
!-------------------------------------------------------------------------------
!                         TO ACCESS DATA CARDS              
!
!           o
!         ncards = cardset_num_cards (obj)
!
!                                          o       o       o
!         call cardset_get_cards   (obj, cards,  ncards, errmsg)
!         call cardset_alloc_cards (obj, pcards, ncards)
!
!                                          i       i   
!         call cardset_put_cards   (obj, cards,  ncards)
!         call cardset_add_cards   (obj, cards,  ncards)
!
!                                          i      o        o
!         call cardset_get_card    (obj, icard,  card,   errmsg)
!         call cardset_put_card    (obj, card)
!         call cardset_add_card    (obj, card)
!                                         i
!
!
! type(cardset_struct)            obj = the cardset structure.
! integer                      ncards = number of data cards.
! integer                       icard = index of desired data card (1-ncards).
! character(len=*)               card = a single data card.
! character(len=*)           cards(:) = an array of data cards.
! character(len=*),pointer  pcards(:) = pointer to an array of data cards.
! character(len=*)             errmsg = error message (blank if no error).
!
! The recommended length of data card character variables is
! CARDSET_DATACARD_LENGTH characters.
!
! Upon input, data cards can have any length, but data cards with information
! exceeding CARDSET_LENGTH characters will be truncated.
!
! Upon output, the information on a single data card will not exceed
! CARDSET_DATACARD_LENGTH characters minus at least CARDSET_DATACARD_PADDING
! characters, unless data cards are input and output without ever being
! converted to keyword/value parameters and back, in which case the output
! data cards will be the same length as input.
!
!      | CARDSET_LENGTH is a named constant with the same value |
!      | as STRINGLIST_LENGTH (currently 160 as of 2000-01-28)  |
!      | in the STRINGLIST primitive.                           |
!
!     | CARDSET_DATACARD_LENGTH and CARDSET_DATACARD_PADDING are |
!     | named constants with the values 80 and 8 respectively.   |
!
! CARDSET_NUM_CARDS:
!   (1) returns the number of data cards (0 or more) in the cardset object.
!
! CARDSET_ALLOC_CARDS:
!   (1) gets all of the data cards in the cardset object.
!   (2) PCARDS is deallocated and reallocated to contain all data cards.
!          (PCARDS must be nullified or allocated before first use.)
!          (PCARDS should be deallocated after last use.)
!   (3) PCARDS is always reallocated to at least one array element, even if
!        there are no data cards and NCARDS is set or reset to zero.
!
! CARDSET_GET_CARDS:
!   (1) gets all of the data cards in the cardset object.
!   (2) an error occurs if CARDS is dimensioned too small for all data cards.
!   (3) does not reset CARDS or NCARDS if an error occurs.
!
! CARDSET_GET_CARD:
!   (1) gets the requested data card in the cardset object.
!   (2) an error occurs if ICARD is out of range.
!   (3) does not reset CARD if an error occurs.
!
! CARDSET_PUT_CARDS:
!   (1) replaces the previous contents with an array with 0 or more data cards.
!
! CARDSET_ADD_CARDS:
!   (1) appends the data cards to the previous contents.
!
! CARDSET_PUT_CARD:
!   (1) replaces the previous contents with a single data card.
!
! CARDSET_ADD_CARD:
!   (1) appends one data card to the previous contents.
!
!-------------------------------------------------------------------------------
!                     TO GET KEYWORD INFORMATION              
!
! The data cards have to be keyword-encoded for this to work.
!
!        nkeys     = cardset_num_keywords    (obj)
!        keyword   = cardset_get_keyword     (obj, ikey)
!        present   = cardset_keyword_present (obj, keyword)
!          o                                        i
!
!                                                    i        i
!               call cardset_reset_keyword   (obj, keyword, newword)
!               call cardset_remove_keyword  (obj, keyword)
!
! type(cardset_struct)              obj = the cardset structure.
! integer                         nkeys = number of keywords.
! integer                          ikey = index of desired keyword.
! character(len=*)              keyword = keyword of the desired parameter.
! logical                       present = whether the keyword is present.
! character(len=*)              newword = new keyword to replace old keyword.
!
! The keyword is converted to upper case when input.
! The keyword matching is done in upper case.
! KEYWORD is returned as blank if IKEY is out of range.
! NEWWORD is not used if KEYWORD is not present.
!
!-------------------------------------------------------------------------------
!                    TO ACCESS PARAMETERS BY KEYWORD         
!
! The data cards have to be keyword-encoded for this to work.
!
!                o                                     i
!            nelements = cardset_num_elements (obj, keyword)
!            nature    = cardset_nature       (obj, keyword)
!            vartype   = cardset_vartype      (obj, keyword)
!
!                                        i        o          o         o
!   call cardset_alloc_array     (obj, keyword, parray,   nelements, errmsg)
!   call cardset_get_array       (obj, keyword, array,    nelements, errmsg)
!
!                                        i        i      o         o
!   call cardset_get_scalar      (obj, keyword,       scalar,    errmsg)
!   call cardset_get_element     (obj, keyword, indx, element,   errmsg)
!
!                                                                  opt   opt
!                                        i       i        i         i     i
!   call cardset_put_array       (obj, keyword, array, nelements, nchar, ndec)
!
!                                                                  opt  opt
!                                              i      i     i       i    i
!   call cardset_put_scalar             (obj,keyword,     scalar, nchar,ndec)
!   call cardset_add_element            (obj,keyword,     element,nchar,ndec)
!   call cardset_insert_element         (obj,keyword,indx,element,nchar,ndec)
!   call cardset_insert_element         (obj,keyword,indx                   )
!   call cardset_remove_element         (obj,keyword,indx                   )
!   call cardset_replace_element        (obj,keyword,indx,element,nchar,ndec)
!   call cardset_replace_or_add_element (obj,keyword,indx,element,nchar,ndec)
!   call cardset_clear_buffer           (obj,keyword)
!
!          o                                      i       i         i
!      matches = cardset_array_matches   (obj, keyword, array,  nelements)
!      matches = cardset_scalar_matches  (obj, keyword, scalar)
!      matches = cardset_element_matches (obj, keyword, element, indx)
!
!         indx = cardset_find_element        (obj,keyword,element)
!         indx = cardset_find_or_add_element (obj,keyword,element,nchar,ndec)
!           o                                        i       i      i    i
!                                                                  opt  opt
!
!
! type(cardset_struct)        obj = the cardset structure.
! character(len=*)        keyword = keyword of the desired parameter.
! integer               nelements = number of array elements.
! integer                  nature = nature of the parameter.
! integer                 vartype = variable type of the parameter.
! integer                    indx = index of desired array element.
! (any type)               scalar = single scalar parameter value.
! (any type)              element = individual array element.
! (any type)             array(:) = array of parameter values.
! (any type),pointer    parray(:) = pointer to array of parameter values.
! character(len=*)         errmsg = non-blank if an error occurs.
! integer,      optional    nchar = maximum number of characters to encode.
! integer,      optional     ndec = maximum number of decimals to encode.
! logical                 matches = true if argument matches the parameter.
!
! The type of ARRAY and SCALAR and ELEMENT can be real, integer, double
! precision, logical, or character(len=*).  The type of SCALAR can also
! be type(grid_struct).  Character variables exceeding length CARDSET_LENGTH
! will be truncated.
!
!      | CARDSET_LENGTH is a named constant with the same value |
!      | as STRINGLIST_LENGTH (currently 160 as of 2000-01-28)  |
!      | in the STRINGLIST primitive.                           |
!
! Since the parameter values are stored internally as character strings,
! the type does not have to match from one subroutine call to another.
! Logical values are stored internally as the strings 'YES' or 'NO'.
! Nil values are stored internally as a blank string.
! Nil values are defined in the NAMED_CONSTANTS module.
!
! The keyword is converted to upper case when input.
! The keyword matching is done in upper case.
!
! The NCHAR argument is for integer, real, and double precision variables only.
! The NDEC argument is for real and double precision variables only.
! The NCHAR and NDEC arguments are both used for type(grid_struct).
! No maximum restrictions are imposed if NCHAR and NDEC are not specified.
!
! If an error occurs because the keyword is not found, the error message
! will begin with the characters 'keyword '.
!
! CARDSET_NUM_ELEMENTS:
!   (1) returns number of elements (0 or more) if the parameter is an ARRAY.
!   (2) returns              1                 if the parameter is a SCALAR.
!   (3) returns              0                 if KEYWORD is not found.
!
! CARDSET_NATURE:
!   (1) returns named constant CARDSET_ARRAY   if the parameter is an ARRAY.
!   (2) returns named constant CARDSET_SCALAR  if the parameter is a SCALAR.
!   (3) returns named constant CARDSET_MISSING if KEYWORD is not found.
!
! CARDSET_ALLOC_ARRAY:
!   (1) gets all of the array elements (0 or more) in the parameter object.
!   (2) PARRAY is deallocated and reallocated to contain all elements.
!          (PARRAY must be nullified or allocated before first use.)
!          (PARRAY should be deallocated after last use.)
!   (3) PARRAY is always reallocated to at least one array element, even if
!        there are no array elements and NELEMENTS is set or reset to zero.
!   (4) an error occurs if the parameter is a scalar.
!   (5) an error occurs if any element cannot be decoded into the desired type.
!   (6) an error occurs if KEYWORD is not found.
!   (7) does not reset PARRAY or NELEMENTS if an error occurs.
!
! CARDSET_GET_ARRAY:
!   (1) gets all of the array elements (0 or more) in the parameter object.
!   (2) an error occurs if ARRAY is dimensioned too small for all elements.
!   (3) an error occurs if the parameter is a scalar.
!   (4) an error occurs if any element cannot be decoded into the desired type.
!   (5) an error occurs if KEYWORD is not found.
!   (6) does not reset ARRAY or NELEMENTS if an error occurs.
!
! CARDSET_GET_SCALAR:
!   (1) gets the requested scalar value in the parameter object.
!   (2) an error occurs if the parameter is an array.
!   (3) an error occurs if the scalar cannot be decoded into the desired type.
!   (4) an error occurs if KEYWORD is not found.
!   (5) does not reset SCALAR if an error occurs.
!
! CARDSET_GET_ELEMENT:
!   (1) gets the requested array element in the parameter object.
!   (2) an error occurs if IELEMENT is out of range.
!   (3) an error occurs if the parameter is a scalar.
!   (4) an error occurs if the element cannot be decoded into the desired type.
!   (5) an error occurs if KEYWORD is not found.
!   (6) does not reset ELEMENT if an error occurs.
!
! CARDSET_PUT_ARRAY:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) replaces the previous contents with an array with 0 or more elements.
!   (3) sets the nature of the parameter to be an ARRAY.
!   (4) sets the variable type of the parameter.
!
! CARDSET_PUT_SCALAR:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) replaces the previous contents with a scalar.
!   (3) sets the nature of the parameter to be a SCALAR.
!   (4) sets the variable type of the parameter.
!
! CARDSET_ADD_ELEMENT:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) appends one element to the previous contents regardless of its nature.
!   (3) sets the nature of the parameter to be an ARRAY.
!   (4) sets the variable type of the parameter.
!
! CARDSET_INSERT_ELEMENT:
!   (1) inserts the specified element at the specified INDEX.
!   (2) inserts element from buffer if argument ELEMENT is missing.
!   (3) does nothing if KEYWORD is not found.
!   (4) does nothing if INDEX is out of range.
!   (5) does nothing if the nature of the parameter is not an ARRAY.
!   (6) sets the variable type of the parameter.
!
! CARDSET_REMOVE_ELEMENT:
!   (1) removes the element at the specified INDEX.
!   (2) puts removed element into a buffer for possible insertion later.
!   (3) does nothing if KEYWORD is not found.
!   (4) does nothing if INDEX is out of range.
!   (5) does nothing if the nature of the parameter is not an ARRAY.
!
! CARDSET_REPLACE_ELEMENT:
!   (1) replaces a previous element with the specified element.
!   (2) does nothing if KEYWORD is not found.
!   (3) does nothing if INDEX is out of range.
!   (4) does nothing if the nature of the parameter is not an ARRAY.
!   (5) sets the variable type of the parameter.
!
! CARDSET_REPLACE_OR_ADD_ELEMENT:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) replaces or adds one element to previous contents regardless of nature.
!   (3) does nothing if INDEX is too small.
!   (4) sets the nature of the parameter to be an ARRAY.
!   (5) increases the length of the array if necessary, filling any
!        intermediate array elements with nil.
!   (6) sets the variable type of the parameter.
!
! CARDSET_ARRAY_MATCHES:
!   (1) matches all of the array elements (0 or more) with the parameter object.
!   (2) returns false if KEYWORD is not found.
!   (3) returns false if the parameter is a scalar.
!   (4) returns false if any element cannot be decoded into the desired type.
!   (5) returns false if the number of elements does not match.
!   (6) returns false if any element value does not match.
!   (7) returns true  if all element values match.
!
! CARDSET_SCALAR_MATCHES:
!   (1) matches the requested scalar value with the parameter object.
!   (2) returns false if KEYWORD is not found.
!   (3) returns false if the parameter is an array.
!   (4) returns false if the scalar cannot be decoded into the desired type.
!   (5) returns false if the scalar value does not match.
!   (6) returns true  if the scalar value matches.
!
! CARDSET_ELEMENT_MATCHES:
!   (1) matches the requested array element value with the parameter object.
!   (2) returns false if KEYWORD is not found.
!   (3) returns false if the parameter is a scalar.
!   (4) returns false if the element cannot be decoded into the desired type.
!   (5) returns false if the index is out of range.
!   (6) returns false if the element value does not match.
!   (7) returns true  if the element value matches.
!
! CARDSET_FIND_ELEMENT:
!   (1) returns the index of the matching array element.
!   (2) returns zero if KEYWORD is not found or there is no match.
!   (3) returns zero if the parameter is a scalar or an error occurs.
!   (4) no error message is ever generated.
!
! CARDSET_FIND_OR_ADD_ELEMENT:
!   (1) first creates a new parameter if KEYWORD is not found.
!   (2) finds the matching element regardless of the nature of the contents.
!   (3) adds the element to the array if there is no match.
!   (4) sets the nature of the parameter to be an ARRAY.
!   (5) returns the index of the matching (or added) array element.
!   (6) sets the variable type of the parameter (if adding).
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                    KEYWORD-ENCODED DATA CARD FORMAT         
!
! Data cards are card images.  If they are keyword-encoded, they will contain
! parameter values identified by keywords.  Data cards can be transferred to
! and from this module using subroutines documented above.
!
! Formats for individual parameters and keywords on the data cards, and
! examples of data cards, are documented in the DATACARDS primitive and
! will not be repeated here.
!
!-------------------------------------------------------------------------------
!                    ENCODING AND DECODING OF DATA CARDS    
!
!  1. Information is stored internally either as data cards, or as a list of
!     keywords and parameters, but not necessarily both ways at the same time.
!     The parameters are stored as character strings.
!
!  2. Whenever an attempt is made to access (get or put) parameters by keyword,
!     or to ask for the number of keywords, and the list of keywords and
!     parameters does not yet exist, the list of keywords and parameters is
!     created by decoding the data cards.  The data cards will be retained
!     unless and until the list of keywords and parameters is modified, in
!     which case the data cards will be deleted since they become out of date.
!
!  3. Whenever an attempt is made to access (get or put) data cards, or to
!     ask for the number of data cards, and the data cards do not yet exist,
!     the data cards are created by encoding the list of keywords and
!     parameters using the current value of the packing option.  The list
!     of keywords and parameters will be retained unless and until the data
!     cards are modified, in which case the list of keywords and parameters
!     will be deleted since it becomes out of date.
!
!  4. Whenever the packing/unpacking data card output option is changed, and
!     the list of keywords and parameters does not exist, the list of keywords
!     and parameters is created by decoding the data cards.  The data cards
!     are then deleted since they become out of date.
!
!  5. If the data cards are decoded and later encoded, they may not appear
!     identical even if the information they contain has not changed.
!
!  6. If an error occurs when the information is decoded from data cards,
!     some information will probably be lost if and when the data cards are
!     subsequently deleted, and the data cards will not then be able to be
!     restored as they were.  Therefore it is important not to try to
!     access parameters by keyword if the data cards have not previously
!     been encoded by keyword.  It is likewise important not to try to
!     change the packing/unpacking data card output option if the data cards
!     have not previously been encoded by keyword.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                   
!
!     Date        Author     Description
!     ----        ------     -----------
! 17. 2008-12-11  B. Menger  Added more NULLIFY statements.
! 16. 2007-09-18  Stoeckley  Add ability to get variable types.
!015. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
! 14. 2004-05-03  Hanson     Add file_to_card and card_to_file.
! 13. 2002-04-11  Stoeckley  Modification to keep stringlist from being cleared
!                             if keyword not present (to support workstation
!                             program interactive I/O).
! 12. 2002-02-04  Stoeckley  Add CARDSET_APPEND, CARDSET_ADD_CARDS,
!                             CARDSET_INSERT_FROM_BUFFER, CARDSET_CLEAR_BUFFER,
!                             CARDSET_REPLACE_OR_ADD_ELEMENT,
!                             CARDSET_ELEMENT_MATCHES, CARDSET_FIND_ELEMENT,
!                             and CARDSET_FIND_OR_ADD_ELEMENT, all to help
!                             support workstation program I/O; make a few
!                             slight improvements in variable names for
!                             consistency.
! 11. 2001-05-17  Stoeckley  Add routine CARDSET_NAME_MATCHES.
! 10. 2000-10-19  Stoeckley  Remove data card encoding and decoding routines
!                             to new DATACARDS primitive.
!  9. 2000-09-15  Stoeckley  Change CARDSET_MAYBE_ENCODE to be more efficient;
!                             change print* statements to write(lunstop,*).
!  8. 2000-03-10  Stoeckley  Fix subtle bug when decoding data cards (had left
!                             out an ADJUSTL).
!  7. 2000-01-28  Stoeckley  Increase length of character variables,
!                             but retain data card length at 80; add code
!                             to split long variables between data cards.
!  6. 2000-01-24  Stoeckley  Change CARDSET_ALLOC routines to always allocate
!                             at least one array element.
!  5. 2000-01-18  Stoeckley  Fixed bug regarding parameter values "()" in
!                             CARDSET_MAYBE_DECODE.
!  4. 1999-12-29  Stoeckley  Add CARDSET_REMOVE_KEYWORD and fix a bug in
!                             CARDSET_RESET_KEYWORD.
!  3. 1999-11-17  Stoeckley  Add ident string for RCS.
!  2. 1999-09-10  Stoeckley  Minor documentation changes.
!  1. 1999-06-22  Stoeckley  Initial version.
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


module cardset_module

  use string_module
  use stringlist_module
  use parlist_module
  use grid_module
  use datacards_module
  use cio_module
  use finquire_module
  implicit none

  public
  private :: cardset_maybe_encode
  private :: cardset_maybe_decode

      character(len=100),public,save :: CARDSET_IDENT = &
'$Id: cardset.f90,v 1.16 2007/09/19 14:02:23 Stoeckley beta sps $'


!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!
!!------------------------- data structure -------------------------------!!


  integer,parameter,public  :: CARDSET_DATACARD_LENGTH  = DATACARDS_CARD_LENGTH
  integer,parameter,public  :: CARDSET_DATACARD_PADDING = DATACARDS_CARD_PADDING
  integer,parameter,public  :: CARDSET_LENGTH   = PARLIST_LENGTH
  integer,parameter,public  :: CARDSET_MISSING  = PARLIST_MISSING ! nature.
  integer,parameter,public  :: CARDSET_SCALAR   = PARLIST_SCALAR  ! nature.
  integer,parameter,public  :: CARDSET_ARRAY    = PARLIST_ARRAY   ! nature.
  integer,parameter,public  :: CARDSET_INTEGER  = PARLIST_INTEGER ! vartype.
  integer,parameter,public  :: CARDSET_FLOAT    = PARLIST_FLOAT   ! vartype.
  integer,parameter,public  :: CARDSET_DOUBLE   = PARLIST_DOUBLE  ! vartype.
  integer,parameter,public  :: CARDSET_STRING   = PARLIST_STRING  ! vartype.
  integer,parameter,public  :: CARDSET_LOGICAL  = PARLIST_LOGICAL ! vartype.
  integer,parameter,public  :: CARDSET_GRID     = PARLIST_GRID    ! vartype.
  integer,parameter,public  :: CARDSET_UNPACKED = 1
  integer,parameter,public  :: CARDSET_PACKED   = 2
  integer,parameter,private :: lunstop          = 6


  type,public :: cardset_struct              

      private
      type(stringlist_struct),pointer :: cards
      type(parlist_struct)   ,pointer :: params
      integer                         :: packing

  end type cardset_struct


!!-------------------------- interfaces ----------------------------------!!
!!-------------------------- interfaces ----------------------------------!!
!!-------------------------- interfaces ----------------------------------!!


  interface cardset_alloc_array
    module procedure cardset_alloc_iarray
    module procedure cardset_alloc_farray
    module procedure cardset_alloc_darray
    module procedure cardset_alloc_carray
    module procedure cardset_alloc_larray
  end interface

  interface cardset_get_array
    module procedure cardset_get_iarray
    module procedure cardset_get_farray
    module procedure cardset_get_darray
    module procedure cardset_get_carray
    module procedure cardset_get_larray
  end interface

  interface cardset_get_scalar
    module procedure cardset_get_iscalar
    module procedure cardset_get_fscalar
    module procedure cardset_get_dscalar
    module procedure cardset_get_cscalar
    module procedure cardset_get_lscalar
    module procedure cardset_get_gscalar
  end interface

  interface cardset_get_element
    module procedure cardset_get_ielement
    module procedure cardset_get_felement
    module procedure cardset_get_delement
    module procedure cardset_get_celement
    module procedure cardset_get_lelement
  end interface

  interface cardset_put_array
    module procedure cardset_put_iarray
    module procedure cardset_put_farray
    module procedure cardset_put_darray
    module procedure cardset_put_carray
    module procedure cardset_put_larray
  end interface

  interface cardset_put_scalar
    module procedure cardset_put_iscalar
    module procedure cardset_put_fscalar
    module procedure cardset_put_dscalar
    module procedure cardset_put_cscalar
    module procedure cardset_put_lscalar
    module procedure cardset_put_gscalar
  end interface

  interface cardset_add_element
    module procedure cardset_add_ielement
    module procedure cardset_add_felement
    module procedure cardset_add_delement
    module procedure cardset_add_celement
    module procedure cardset_add_lelement
  end interface

  interface cardset_insert_element
    module procedure cardset_insert_ielement
    module procedure cardset_insert_felement
    module procedure cardset_insert_delement
    module procedure cardset_insert_celement
    module procedure cardset_insert_lelement
    module procedure cardset_insert_from_buffer
  end interface

  interface cardset_replace_element
    module procedure cardset_replace_ielement
    module procedure cardset_replace_felement
    module procedure cardset_replace_delement
    module procedure cardset_replace_celement
    module procedure cardset_replace_lelement
  end interface

  interface cardset_replace_or_add_element
    module procedure cardset_replace_or_add_ielement
    module procedure cardset_replace_or_add_felement
    module procedure cardset_replace_or_add_delement
    module procedure cardset_replace_or_add_celement
    module procedure cardset_replace_or_add_lelement
  end interface

  interface cardset_scalar_matches
    module procedure cardset_iscalar_matches
    module procedure cardset_fscalar_matches
    module procedure cardset_dscalar_matches
    module procedure cardset_cscalar_matches
    module procedure cardset_lscalar_matches
    module procedure cardset_gscalar_matches
  end interface

  interface cardset_array_matches
    module procedure cardset_iarray_matches
    module procedure cardset_farray_matches
    module procedure cardset_darray_matches
    module procedure cardset_carray_matches
    module procedure cardset_larray_matches
  end interface

  interface cardset_element_matches
    module procedure cardset_ielement_matches
    module procedure cardset_felement_matches
    module procedure cardset_delement_matches
    module procedure cardset_celement_matches
    module procedure cardset_lelement_matches
  end interface

  interface cardset_find_element
    module procedure cardset_find_ielement
    module procedure cardset_find_felement
    module procedure cardset_find_delement
    module procedure cardset_find_celement
    module procedure cardset_find_lelement
  end interface

  interface cardset_find_or_add_element
    module procedure cardset_find_or_add_ielement
    module procedure cardset_find_or_add_felement
    module procedure cardset_find_or_add_delement
    module procedure cardset_find_or_add_celement
    module procedure cardset_find_or_add_lelement
  end interface


!!----------------------------- end of data ------------------------------!!
!!----------------------------- end of data ------------------------------!!
!!----------------------------- end of data ------------------------------!!


      contains


!!--------------------- create clear delete copy ---------------------------!!
!!--------------------- create clear delete copy ---------------------------!!
!!--------------------- create clear delete copy ---------------------------!!


      subroutine cardset_create (obj)
      implicit none
      type(cardset_struct),pointer :: obj       ! argument

      nullify(obj)
      allocate (obj)
      nullify (obj%cards) ! jpa
      nullify (obj%params) ! jpa

      call stringlist_create (obj%cards)
      call parlist_create    (obj%params)
      obj%packing = CARDSET_UNPACKED
      return
      end subroutine cardset_create



      subroutine cardset_delete (obj)
      implicit none
      type(cardset_struct),pointer :: obj       ! argument

      call stringlist_delete (obj%cards)
      call parlist_delete    (obj%params)
      deallocate (obj)
      return
      end subroutine cardset_delete



      subroutine cardset_clear (obj)
      implicit none
      type(cardset_struct),intent(inout) :: obj       ! argument

      call stringlist_clear (obj%cards)
      call parlist_clear    (obj%params)
      obj%packing = CARDSET_UNPACKED
      return
      end subroutine cardset_clear



      subroutine cardset_copy (obj1, obj2)
      implicit none
      type(cardset_struct),intent(in)    :: obj1      ! argument
      type(cardset_struct),intent(inout) :: obj2      ! argument

      call stringlist_copy (obj1%cards , obj2%cards)
      call parlist_copy    (obj1%params, obj2%params)
      obj2%packing = obj1%packing
      return
      end subroutine cardset_copy



      subroutine cardset_append (obj1, obj2)
      implicit none
      type(cardset_struct),intent(inout)             :: obj1       ! argument
      type(cardset_struct),intent(inout)             :: obj2       ! argument
      character(len=CARDSET_DATACARD_LENGTH),pointer :: pcards(:)  ! local
      integer                                        :: ncards     ! local

      nullify (pcards)
      call cardset_alloc_cards (obj1,pcards,ncards)
      call cardset_add_cards   (obj2,pcards,ncards)
      if (associated(pcards)) deallocate (pcards)
      return
      end subroutine cardset_append


!!------------- to access the packing option for data cards ----------------!!
!!------------- to access the packing option for data cards ----------------!!
!!------------- to access the packing option for data cards ----------------!!


      subroutine cardset_set_packing_option (obj,packing)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      integer             ,intent(in)    :: packing             ! argument

      if (packing /= CARDSET_UNPACKED .and. packing /= CARDSET_PACKED) return
      if (packing == obj%packing) return
      call cardset_maybe_decode (obj)
      call stringlist_clear (obj%cards)
      obj%packing = packing
      return
      end subroutine cardset_set_packing_option



      subroutine cardset_get_packing_option (obj,packing)
      implicit none
      type(cardset_struct),intent(in)  :: obj                 ! argument
      integer             ,intent(out) :: packing             ! argument

      packing = obj%packing
      return
      end subroutine cardset_get_packing_option


!!----------------- to access the name of the cardset ------------------!!
!!----------------- to access the name of the cardset ------------------!!
!!----------------- to access the name of the cardset ------------------!!


      subroutine cardset_set_name (obj, name)
      implicit none
      type(cardset_struct),intent(inout) :: obj              ! argument
      character(len=*)    ,intent(in)    :: name             ! argument

      call stringlist_set_name (obj%cards, name)
      return
      end subroutine cardset_set_name



      subroutine cardset_get_name (obj, name)
      implicit none
      type(cardset_struct),intent(in)  :: obj              ! argument
      character(len=*)    ,intent(out) :: name             ! argument

      call stringlist_get_name (obj%cards, name)
      return
      end subroutine cardset_get_name



      function cardset_name_matches (obj, name) result (matches)
      implicit none
      type(cardset_struct),intent(in) :: obj            ! argument
      character(len=*)    ,intent(in) :: name           ! argument
      logical                         :: matches        ! result

      matches = stringlist_name_matches (obj%cards, name)
      return
      end function cardset_name_matches


!!----------------------- to access data cards -------------------------!!
!!----------------------- to access data cards -------------------------!!
!!----------------------- to access data cards -------------------------!!


      function cardset_num_cards (obj) result (ncards)
      implicit none
      type(cardset_struct),intent(inout) :: obj                ! argument
      integer                            :: ncards             ! result

      call cardset_maybe_encode       (obj)
      ncards = stringlist_num_strings (obj%cards)
      return
      end function cardset_num_cards


                        !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_alloc_cards (obj, pcards, ncards)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,pointer       :: pcards(:)         ! argument
      integer                  ,intent(out)   :: ncards            ! argument

      call cardset_maybe_encode     (obj)
      call stringlist_alloc_strings (obj%cards,pcards,ncards)
      return
      end subroutine cardset_alloc_cards



      subroutine cardset_get_cards (obj, cards, ncards, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(out)   :: cards(:)          ! argument
      integer                  ,intent(out)   :: ncards            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_encode   (obj)
      call stringlist_get_strings (obj%cards, cards, ncards, errmsg)
      return
      end subroutine cardset_get_cards



      subroutine cardset_get_card (obj, icard, card, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      integer                  ,intent(in)    :: icard             ! argument
      character(len=*)         ,intent(out)   :: card              ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_encode  (obj)
      call stringlist_get_string (obj%cards, icard, card, errmsg)
      return
      end subroutine cardset_get_card



      subroutine cardset_put_cards (obj,cards,ncards)
      implicit none
      type(cardset_struct),intent(inout)        :: obj            ! argument
      character(len=*)    ,intent(in)           :: cards(:)       ! argument
      integer             ,intent(in)           :: ncards         ! argument

      call cardset_maybe_encode   (obj)
      call stringlist_put_strings (obj%cards,cards,ncards)
      call parlist_clear          (obj%params)
      return
      end subroutine cardset_put_cards



      subroutine cardset_add_cards (obj,cards,ncards)
      implicit none
      type(cardset_struct),intent(inout)        :: obj            ! argument
      character(len=*)    ,intent(in)           :: cards(:)       ! argument
      integer             ,intent(in)           :: ncards         ! argument

      call cardset_maybe_encode   (obj)
      call stringlist_add_strings (obj%cards, cards, ncards)
      call parlist_clear          (obj%params)
      return
      end subroutine cardset_add_cards



      subroutine cardset_put_card (obj,card)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: card                ! argument

      call cardset_maybe_encode  (obj)
      call stringlist_put_string (obj%cards, card)
      call parlist_clear         (obj%params)
      return
      end subroutine cardset_put_card



      subroutine cardset_add_card (obj,card)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: card                ! argument

      call cardset_maybe_encode  (obj)
      call stringlist_add_string (obj%cards, card)
      call parlist_clear         (obj%params)
      return
      end subroutine cardset_add_card


!!--------------------- to get keyword information ------------------------!!
!!--------------------- to get keyword information ------------------------!!
!!--------------------- to get keyword information ------------------------!!


      function cardset_num_keywords (obj) result (nkeys)
      implicit none
      type(cardset_struct),intent(inout) :: obj               ! argument
      integer                            :: nkeys             ! result

      call cardset_maybe_decode    (obj)
      nkeys = parlist_num_keywords (obj%params)
      return
      end function cardset_num_keywords


      function cardset_get_keyword (obj, ikey) result (keyword)
      implicit none
      type(cardset_struct),intent(inout) :: obj             ! argument
      integer             ,intent(in)    :: ikey            ! argument
      character(len=CARDSET_LENGTH)      :: keyword         ! result

      call cardset_maybe_decode     (obj)
      keyword = parlist_get_keyword (obj%params, ikey)
      return
      end function cardset_get_keyword


      function cardset_keyword_present (obj, keyword) result (present)
      implicit none
      type(cardset_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      logical                            :: present         ! result

      call cardset_maybe_decode         (obj)
      present = parlist_keyword_present (obj%params, keyword)
      return
      end function cardset_keyword_present


      subroutine cardset_reset_keyword (obj, keyword, newword)
      implicit none
      type(cardset_struct),intent(inout) :: obj          ! argument
      character(len=*)    ,intent(in)    :: keyword      ! argument
      character(len=*)    ,intent(in)    :: newword      ! argument
 
      call cardset_maybe_decode  (obj)
      call parlist_reset_keyword (obj%params, keyword, newword)
      call stringlist_clear      (obj%cards)
      return
      end subroutine cardset_reset_keyword


      subroutine cardset_remove_keyword (obj, keyword)
      implicit none
      type(cardset_struct),intent(inout) :: obj          ! argument
      character(len=*)    ,intent(in)    :: keyword      ! argument
 
      call cardset_maybe_decode   (obj)
      if (.not.parlist_keyword_present(obj%params, keyword)) return
                !!! keeps stringlist from being cleared if keyword not present.
      call parlist_remove_keyword (obj%params, keyword)
      call stringlist_clear       (obj%cards)
      return
      end subroutine cardset_remove_keyword


!!----------------------- to access parameter values --------------------!!
!!----------------------- to access parameter values --------------------!!
!!----------------------- to access parameter values --------------------!!


      function cardset_num_elements (obj, keyword) result (nelements)
      implicit none
      type(cardset_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer                            :: nelements       ! result

      call cardset_maybe_decode        (obj)
      nelements = parlist_num_elements (obj%params, keyword)
      return
      end function cardset_num_elements



      function cardset_nature (obj, keyword) result (nature)
      implicit none
      type(cardset_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer                            :: nature          ! result

      call cardset_maybe_decode   (obj)
      nature = parlist_nature (obj%params, keyword)
      return
      end function cardset_nature



      function cardset_vartype (obj, keyword) result (vartype)
      implicit none
      type(cardset_struct),intent(inout) :: obj             ! argument
      character(len=*)    ,intent(in)    :: keyword         ! argument
      integer                            :: vartype         ! result

      call cardset_maybe_decode   (obj)
      vartype = parlist_vartype (obj%params, keyword)
      return
      end function cardset_vartype


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_alloc_iarray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      integer                  ,pointer       :: parray(:)     ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_alloc_iarray (obj%params, keyword,parray,nelements,errmsg)
      return
      end subroutine cardset_alloc_iarray


      subroutine cardset_alloc_farray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      real                     ,pointer       :: parray(:)     ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_alloc_farray (obj%params, keyword,parray,nelements,errmsg)
      return
      end subroutine cardset_alloc_farray


      subroutine cardset_alloc_darray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      double precision         ,pointer       :: parray(:)     ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_alloc_darray (obj%params, keyword,parray,nelements,errmsg)
      return
      end subroutine cardset_alloc_darray


      subroutine cardset_alloc_carray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      character(len=*)         ,pointer       :: parray(:)     ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_alloc_carray (obj%params, keyword,parray,nelements,errmsg)
      return
      end subroutine cardset_alloc_carray


      subroutine cardset_alloc_larray (obj, keyword, parray, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      logical                  ,pointer       :: parray(:)     ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_alloc_larray (obj%params, keyword,parray,nelements,errmsg)
      return
      end subroutine cardset_alloc_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_get_iarray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      integer                  ,intent(out)   :: array(:)      ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_iarray   (obj%params, keyword,array,nelements,errmsg)
      return
      end subroutine cardset_get_iarray


      subroutine cardset_get_farray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      real                     ,intent(out)   :: array(:)      ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_farray   (obj%params, keyword,array,nelements,errmsg)
      return
      end subroutine cardset_get_farray


      subroutine cardset_get_darray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      double precision         ,intent(out)   :: array(:)      ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_darray   (obj%params, keyword,array,nelements,errmsg)
      return
      end subroutine cardset_get_darray


      subroutine cardset_get_carray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      character(len=*)         ,intent(out)   :: array(:)      ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_carray   (obj%params, keyword,array,nelements,errmsg)
      return
      end subroutine cardset_get_carray


      subroutine cardset_get_larray (obj, keyword, array, nelements, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj           ! argument
      character(len=*)         ,intent(in)    :: keyword       ! argument
      logical                  ,intent(out)   :: array(:)      ! argument
      integer                  ,intent(out)   :: nelements     ! argument
      character(len=*)         ,intent(out)   :: errmsg        ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_larray   (obj%params, keyword,array,nelements,errmsg)
      return
      end subroutine cardset_get_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_get_iscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      integer                  ,intent(out)   :: scalar            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_iscalar  (obj%params, keyword, scalar, errmsg)
      return
      end subroutine cardset_get_iscalar


      subroutine cardset_get_fscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      real                     ,intent(out)   :: scalar            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_fscalar  (obj%params, keyword, scalar, errmsg)
      return
      end subroutine cardset_get_fscalar


      subroutine cardset_get_dscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      double precision         ,intent(out)   :: scalar            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_dscalar  (obj%params, keyword, scalar, errmsg)
      return
      end subroutine cardset_get_dscalar


      subroutine cardset_get_cscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      character(len=*)         ,intent(out)   :: scalar            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_cscalar  (obj%params, keyword, scalar, errmsg)
      return
      end subroutine cardset_get_cscalar


      subroutine cardset_get_lscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      logical                  ,intent(out)   :: scalar            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_lscalar  (obj%params, keyword, scalar, errmsg)
      return
      end subroutine cardset_get_lscalar


      subroutine cardset_get_gscalar (obj, keyword, scalar, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      type(grid_struct)        ,intent(out)   :: scalar            ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_gscalar  (obj%params, keyword, scalar, errmsg)
      return
      end subroutine cardset_get_gscalar


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_get_ielement (obj, keyword, indx, element, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      integer                  ,intent(in)    :: indx              ! argument
      integer                  ,intent(out)   :: element           ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_ielement (obj%params, keyword,indx,element,errmsg)
      return
      end subroutine cardset_get_ielement


      subroutine cardset_get_felement (obj, keyword, indx, element, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      integer                  ,intent(in)    :: indx              ! argument
      real                     ,intent(out)   :: element           ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_felement (obj%params, keyword,indx,element,errmsg)
      return
      end subroutine cardset_get_felement


      subroutine cardset_get_delement (obj, keyword, indx, element, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      integer                  ,intent(in)    :: indx              ! argument
      double precision         ,intent(out)   :: element           ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_delement (obj%params, keyword,indx,element,errmsg)
      return
      end subroutine cardset_get_delement


      subroutine cardset_get_celement (obj, keyword, indx, element, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      integer                  ,intent(in)    :: indx              ! argument
      character(len=*)         ,intent(out)   :: element           ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_celement (obj%params, keyword,indx,element,errmsg)
      return
      end subroutine cardset_get_celement


      subroutine cardset_get_lelement (obj, keyword, indx, element, errmsg)
      implicit none
      type(cardset_struct)     ,intent(inout) :: obj               ! argument
      character(len=*)         ,intent(in)    :: keyword           ! argument
      integer                  ,intent(in)    :: indx              ! argument
      logical                  ,intent(out)   :: element           ! argument
      character(len=*)         ,intent(out)   :: errmsg            ! argument

      call cardset_maybe_decode (obj)
      call parlist_get_lelement (obj%params, keyword,indx,element,errmsg)
      return
      end subroutine cardset_get_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_put_iarray (obj, keyword, array, nelements, nchar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: array(:)            ! argument
      integer             ,intent(in)    :: nelements           ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_iarray   (obj%params, keyword, array, nelements, nchar)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_iarray


      subroutine cardset_put_farray (obj,keyword,array,nelements,nchar,ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      real                ,intent(in)    :: array(:)            ! argument
      integer             ,intent(in)    :: nelements           ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_farray   (obj%params,keyword,array,nelements,nchar,ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_farray


      subroutine cardset_put_darray (obj,keyword,array,nelements,nchar,ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      double precision    ,intent(in)    :: array(:)            ! argument
      integer             ,intent(in)    :: nelements           ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_darray   (obj%params,keyword,array,nelements,nchar,ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_darray


      subroutine cardset_put_carray (obj, keyword, array, nelements)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      character(len=*)    ,intent(in)    :: array(:)            ! argument
      integer             ,intent(in)    :: nelements           ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_carray   (obj%params, keyword, array, nelements)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_carray


      subroutine cardset_put_larray (obj, keyword, array, nelements)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      logical             ,intent(in)    :: array(:)            ! argument
      integer             ,intent(in)    :: nelements           ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_larray   (obj%params, keyword, array, nelements)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_larray


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_put_iscalar (obj, keyword, scalar, nchar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: scalar              ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_iscalar  (obj%params, keyword, scalar, nchar)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_iscalar


      subroutine cardset_put_fscalar (obj, keyword, scalar, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      real                ,intent(in)    :: scalar              ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_fscalar  (obj%params, keyword, scalar, nchar, ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_fscalar


      subroutine cardset_put_dscalar (obj, keyword, scalar, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      double precision    ,intent(in)    :: scalar              ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_dscalar  (obj%params, keyword, scalar, nchar, ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_dscalar


      subroutine cardset_put_cscalar (obj, keyword, scalar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      character(len=*)    ,intent(in)    :: scalar              ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_cscalar  (obj%params, keyword, scalar)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_cscalar


      subroutine cardset_put_lscalar (obj, keyword, scalar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      logical             ,intent(in)    :: scalar              ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_lscalar  (obj%params, keyword, scalar)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_lscalar


      subroutine cardset_put_gscalar (obj, keyword, scalar, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      type(grid_struct)   ,intent(in)    :: scalar              ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_put_gscalar  (obj%params, keyword, scalar, nchar, ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_put_gscalar


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_add_ielement (obj, keyword, element, nchar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument

      call cardset_maybe_decode (obj)
      call parlist_add_ielement (obj%params, keyword, element, nchar)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_add_ielement


      subroutine cardset_add_felement (obj, keyword, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      real                ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_add_felement (obj%params, keyword, element, nchar, ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_add_felement


      subroutine cardset_add_delement (obj, keyword, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      double precision    ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode (obj)
      call parlist_add_delement (obj%params, keyword, element, nchar, ndec)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_add_delement


      subroutine cardset_add_celement (obj, keyword, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      character(len=*)    ,intent(in)    :: element             ! argument

      call cardset_maybe_decode (obj)
      call parlist_add_celement (obj%params, keyword, element)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_add_celement


      subroutine cardset_add_lelement (obj, keyword, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      logical             ,intent(in)    :: element             ! argument

      call cardset_maybe_decode (obj)
      call parlist_add_lelement (obj%params, keyword, element)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_add_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_insert_ielement (obj, keyword, indx, element, nchar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      integer             ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument

      call cardset_maybe_decode    (obj)
      call parlist_insert_ielement (obj%params, keyword, indx, element, nchar)
      call stringlist_clear        (obj%cards)
      return
      end subroutine cardset_insert_ielement


      subroutine cardset_insert_felement  &
                             (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      real                ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode    (obj)
      call parlist_insert_felement (obj%params, keyword,  &
                                                 indx, element, nchar, ndec)
      call stringlist_clear        (obj%cards)
      return
      end subroutine cardset_insert_felement


      subroutine cardset_insert_delement  &
                             (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      double precision    ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode    (obj)
      call parlist_insert_delement (obj%params, keyword,  &
                                                 indx, element, nchar, ndec)
      call stringlist_clear        (obj%cards)
      return
      end subroutine cardset_insert_delement


      subroutine cardset_insert_celement (obj, keyword, indx, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      character(len=*)    ,intent(in)    :: element             ! argument

      call cardset_maybe_decode    (obj)
      call parlist_insert_celement (obj%params, keyword, indx, element)
      call stringlist_clear        (obj%cards)
      return
      end subroutine cardset_insert_celement


      subroutine cardset_insert_lelement (obj, keyword, indx, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      logical             ,intent(in)    :: element             ! argument

      call cardset_maybe_decode    (obj)
      call parlist_insert_lelement (obj%params, keyword, indx, element)
      call stringlist_clear        (obj%cards)
      return
      end subroutine cardset_insert_lelement



      subroutine cardset_insert_from_buffer (obj, keyword, indx)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument

      call cardset_maybe_decode       (obj)
      call parlist_insert_from_buffer (obj%params, keyword, indx)
      call stringlist_clear           (obj%cards)
      return
      end subroutine cardset_insert_from_buffer


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_remove_element (obj, keyword, indx)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument

      call cardset_maybe_decode   (obj)
      call parlist_remove_element (obj%params, keyword, indx)
      call stringlist_clear       (obj%cards)
      return
      end subroutine cardset_remove_element



      subroutine cardset_clear_buffer (obj, keyword)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument

      call cardset_maybe_decode (obj)
      call parlist_clear_buffer (obj%params, keyword)
      call stringlist_clear     (obj%cards)
      return
      end subroutine cardset_clear_buffer


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_replace_ielement (obj, keyword, indx, element, nchar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      integer             ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument

      call cardset_maybe_decode     (obj)
      call parlist_replace_ielement (obj%params, keyword, indx, element, nchar)
      call stringlist_clear         (obj%cards)
      return
      end subroutine cardset_replace_ielement


      subroutine cardset_replace_felement  &
                             (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      real                ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode     (obj)
      call parlist_replace_felement (obj%params, keyword,  &
                                                 indx, element, nchar, ndec)
      call stringlist_clear         (obj%cards)
      return
      end subroutine cardset_replace_felement


      subroutine cardset_replace_delement  &
                             (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      double precision    ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode     (obj)
      call parlist_replace_delement (obj%params, keyword,  &
                                                 indx, element, nchar, ndec)
      call stringlist_clear         (obj%cards)
      return
      end subroutine cardset_replace_delement


      subroutine cardset_replace_celement (obj, keyword, indx, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      character(len=*)    ,intent(in)    :: element             ! argument

      call cardset_maybe_decode     (obj)
      call parlist_replace_celement (obj%params, keyword, indx, element)
      call stringlist_clear         (obj%cards)
      return
      end subroutine cardset_replace_celement


      subroutine cardset_replace_lelement (obj, keyword, indx, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      logical             ,intent(in)    :: element             ! argument

      call cardset_maybe_decode     (obj)
      call parlist_replace_lelement (obj%params, keyword, indx, element)
      call stringlist_clear         (obj%cards)
      return
      end subroutine cardset_replace_lelement


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine cardset_replace_or_add_ielement &
                                    (obj, keyword, indx, element, nchar)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      integer             ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument

      call cardset_maybe_decode            (obj)
      call parlist_replace_or_add_ielement (obj%params, keyword, &
                                                 indx, element, nchar)
      call stringlist_clear                (obj%cards)
      return
      end subroutine cardset_replace_or_add_ielement


      subroutine cardset_replace_or_add_felement  &
                             (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      real                ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode            (obj)
      call parlist_replace_or_add_felement (obj%params, keyword,  &
                                                 indx, element, nchar, ndec)
      call stringlist_clear                (obj%cards)
      return
      end subroutine cardset_replace_or_add_felement


      subroutine cardset_replace_or_add_delement  &
                             (obj, keyword, indx, element, nchar, ndec)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      double precision    ,intent(in)    :: element             ! argument
      integer,optional    ,intent(in)    :: nchar               ! argument
      integer,optional    ,intent(in)    :: ndec                ! argument

      call cardset_maybe_decode            (obj)
      call parlist_replace_or_add_delement (obj%params, keyword,  &
                                                 indx, element, nchar, ndec)
      call stringlist_clear                (obj%cards)
      return
      end subroutine cardset_replace_or_add_delement


      subroutine cardset_replace_or_add_celement (obj, keyword, indx, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      character(len=*)    ,intent(in)    :: element             ! argument

      call cardset_maybe_decode            (obj)
      call parlist_replace_or_add_celement (obj%params, keyword, indx, element)
      call stringlist_clear                (obj%cards)
      return
      end subroutine cardset_replace_or_add_celement


      subroutine cardset_replace_or_add_lelement (obj, keyword, indx, element)
      implicit none
      type(cardset_struct),intent(inout) :: obj                 ! argument
      character(len=*)    ,intent(in)    :: keyword             ! argument
      integer             ,intent(in)    :: indx                ! argument
      logical             ,intent(in)    :: element             ! argument

      call cardset_maybe_decode            (obj)
      call parlist_replace_or_add_lelement (obj%params, keyword, indx, element)
      call stringlist_clear                (obj%cards)
      return
      end subroutine cardset_replace_or_add_lelement


!!------------------ to test for matching scalar -------------------------!!
!!------------------ to test for matching scalar -------------------------!!
!!------------------ to test for matching scalar -------------------------!!


      function cardset_iscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      integer               ,intent(in)    :: scalar          ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_scalar_matches (obj%params, keyword, scalar)
      return
      end function cardset_iscalar_matches


      function cardset_fscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      real                  ,intent(in)    :: scalar          ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_scalar_matches (obj%params, keyword, scalar)
      return
      end function cardset_fscalar_matches


      function cardset_dscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      double precision      ,intent(in)    :: scalar          ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_scalar_matches (obj%params, keyword, scalar)
      return
      end function cardset_dscalar_matches


      function cardset_cscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      character(len=*)      ,intent(in)    :: scalar          ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_scalar_matches (obj%params, keyword, scalar)
      return
      end function cardset_cscalar_matches


      function cardset_lscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      logical               ,intent(in)    :: scalar          ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_scalar_matches (obj%params, keyword, scalar)
      return
      end function cardset_lscalar_matches


      function cardset_gscalar_matches (obj, keyword, scalar) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      type(grid_struct)     ,intent(in)    :: scalar          ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_scalar_matches (obj%params, keyword, scalar)
      return
      end function cardset_gscalar_matches


!!------------------ to test for matching array --------------------------!!
!!------------------ to test for matching array --------------------------!!
!!------------------ to test for matching array --------------------------!!


      function cardset_iarray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      integer               ,intent(in)    :: array(:)        ! argument
      integer               ,intent(in)    :: nelements       ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_array_matches (obj%params, keyword, array, nelements)
      return
      end function cardset_iarray_matches



      function cardset_farray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      real                  ,intent(in)    :: array(:)        ! argument
      integer               ,intent(in)    :: nelements       ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_array_matches (obj%params, keyword, array, nelements)
      return
      end function cardset_farray_matches



      function cardset_darray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      double precision      ,intent(in)    :: array(:)        ! argument
      integer               ,intent(in)    :: nelements       ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_array_matches (obj%params, keyword, array, nelements)
      return
      end function cardset_darray_matches



      function cardset_carray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      character(len=*)      ,intent(in)    :: array(:)        ! argument
      integer               ,intent(in)    :: nelements       ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_array_matches (obj%params, keyword, array, nelements)
      return
      end function cardset_carray_matches



      function cardset_larray_matches  &
                       (obj, keyword, array, nelements) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      logical               ,intent(in)    :: array(:)        ! argument
      integer               ,intent(in)    :: nelements       ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_array_matches (obj%params, keyword, array, nelements)
      return
      end function cardset_larray_matches


!!------------------ to test for matching array element ---------------------!!
!!------------------ to test for matching array element ---------------------!!
!!------------------ to test for matching array element ---------------------!!


      function cardset_ielement_matches  &
                            (obj, keyword, element, indx) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      integer               ,intent(in)    :: element         ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_element_matches (obj%params, keyword, element, indx)
      return
      end function cardset_ielement_matches



      function cardset_felement_matches  &
                            (obj, keyword, element, indx) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      real                  ,intent(in)    :: element         ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_element_matches (obj%params, keyword, element, indx)
      return
      end function cardset_felement_matches



      function cardset_delement_matches  &
                            (obj, keyword, element, indx) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_element_matches (obj%params, keyword, element, indx)
      return
      end function cardset_delement_matches



      function cardset_celement_matches  &
                            (obj, keyword, element, indx) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      character(len=*)      ,intent(in)    :: element         ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_element_matches (obj%params, keyword, element, indx)
      return
      end function cardset_celement_matches



      function cardset_lelement_matches  &
                            (obj, keyword, element, indx) result (matches)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      logical               ,intent(in)    :: element         ! argument
      integer               ,intent(in)    :: indx            ! argument
      logical                              :: matches         ! result

      call cardset_maybe_decode (obj)
      matches = parlist_element_matches (obj%params, keyword, element, indx)
      return
      end function cardset_lelement_matches


!!-------------------------------- find element ----------------------------!!
!!-------------------------------- find element ----------------------------!!
!!-------------------------------- find element ----------------------------!!


      function cardset_find_ielement (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      integer               ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_element (obj%params, keyword, element)
      return
      end function cardset_find_ielement



      function cardset_find_felement (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      real                  ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_element (obj%params, keyword, element)
      return
      end function cardset_find_felement



      function cardset_find_delement (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_element (obj%params, keyword, element)
      return
      end function cardset_find_delement



      function cardset_find_celement (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      character(len=*)      ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_element (obj%params, keyword, element)
      return
      end function cardset_find_celement



      function cardset_find_lelement (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      logical               ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_element (obj%params, keyword, element)
      return
      end function cardset_find_lelement


!!-------------------------- find or add element ----------------------------!!
!!-------------------------- find or add element ----------------------------!!
!!-------------------------- find or add element ----------------------------!!


      function cardset_find_or_add_ielement &
                                 (obj, keyword, element, nchar) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      integer               ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_or_add_element (obj%params, keyword, element, nchar)
      return
      end function cardset_find_or_add_ielement



      function cardset_find_or_add_felement &
                            (obj, keyword, element, nchar, ndec) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      real                  ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_or_add_element &
                                   (obj%params, keyword, element, nchar, ndec)
      return
      end function cardset_find_or_add_felement



      function cardset_find_or_add_delement &
                            (obj, keyword, element, nchar, ndec) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      double precision      ,intent(in)    :: element         ! argument
      integer,optional      ,intent(in)    :: nchar           ! argument
      integer,optional      ,intent(in)    :: ndec            ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_or_add_element &
                                   (obj%params, keyword, element, nchar, ndec)
      return
      end function cardset_find_or_add_delement



      function cardset_find_or_add_celement &
                                    (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      character(len=*)      ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_or_add_element (obj%params, keyword, element)
      return
      end function cardset_find_or_add_celement



      function cardset_find_or_add_lelement &
                                    (obj, keyword, element) result (indx)
      implicit none
      type(cardset_struct)  ,intent(inout) :: obj             ! argument
      character(len=*)      ,intent(in)    :: keyword         ! argument
      logical               ,intent(in)    :: element         ! argument
      integer                              :: indx            ! result

      call cardset_maybe_decode (obj)
      indx = parlist_find_or_add_element (obj%params, keyword, element)
      return
      end function cardset_find_or_add_lelement


!!------------------------ maybe encode -------------------------------!!
!!------------------------ maybe encode -------------------------------!!
!!------------------------ maybe encode -------------------------------!!

! This subroutine encodes the keywords and parameters into data cards.
! This subroutine does nothing if the data cards are already encoded.
! This subroutine does nothing if there are no keywords and parameters.


      subroutine cardset_maybe_encode (obj)
      implicit none
      type(cardset_struct),intent(inout)    :: obj                  ! argument
      integer                               :: ncards,nkeys         ! local

      ncards = stringlist_num_strings (obj%cards)
      nkeys  = parlist_num_keywords   (obj%params)
      if (ncards > 0 .or. nkeys == 0) return

      call datacards_encode (obj%params, obj%cards, obj%packing)
      return
      end subroutine cardset_maybe_encode


!!------------------------ maybe decode -------------------------------!!
!!------------------------ maybe decode -------------------------------!!
!!------------------------ maybe decode -------------------------------!!

! This subroutine decodes the data cards into keywords and parameters.
! This subroutine does nothing if the data cards are already decoded.
! This subroutine does nothing if there are no data cards.


      subroutine cardset_maybe_decode (obj)
      implicit none
      type(cardset_struct),intent(inout) :: obj                ! argument
      integer                            :: ncards,nkeys       ! local

      ncards = stringlist_num_strings (obj%cards)
      nkeys  = parlist_num_keywords   (obj%params)
      if (nkeys > 0 .or. ncards == 0) return

      call datacards_decode (obj%cards, obj%params)
      return
      end subroutine cardset_maybe_decode

!!------------------------ cardset_card_to_file -----------------------!!
!!------------------------ cardset_card_to_file -----------------------!!
!!------------------------ cardset_card_to_file -----------------------!!

! This subroutine write the contents of the data cards to a file.
  subroutine cardset_card_to_file ( file_name, card_set, i_err )
    !
    ! write file card images to file
    !
    character(len=*),    intent (in   ) :: file_name       ! file name
    type ( cardset_struct ),    pointer :: card_set        ! cardset structure
    integer,             intent (inout) :: i_err           ! err 0=o.k. -1=error
    !
    integer                             :: file_unit       ! err 0=o.k. -1=error
    !
    integer                             :: j_err           ! err 0=o.k. -1=error
    !
    integer                             :: n_card          ! card number
    integer                             :: j_card          ! card index
    integer                             :: m_card
    !
    character(len=CARDSET_DATACARD_LENGTH):: crd_xxx       ! card image 
    character(len=80)                   :: crd_80          ! card image
    !
    i_err = 0
    !
    j_err = 0
    m_card= 0
    !
    !print'(" cardset_card_to_file file_name=", a)', trim(file_name)
    !
    ! there is a problem if this structure does not exist
    !
    if ( .not. associated ( card_set ) ) go to 998
    !
    ! open the file
    !
    call cardset_file_open ( file_name, 'w+', file_unit, i_err )
    !
    if ( i_err .ne. 0 ) i_err = -1
    !
    if ( i_err .lt. 0 ) go to 997
    !
    ! write each card to the output file
    !
    do_cards : do j_card = 1 , cardset_num_cards ( card_set )
      !
      call cardset_get_card ( card_set, j_card, crd_xxx, crd_80 )
      !
      n_card = cio_fputline ( string = crd_xxx, unit = file_unit )
      !
      xxif_n_card_0 : &
      if ( n_card .lt. 0 ) then
        !
        print '("cardset_card_to_file: cio_fputline error, j_card=", i8 )', &
        j_card
        !
        i_err = -1
        !
      else xxif_n_card_0 
        !
        m_card = m_card + 1
        !
      end if xxif_n_card_0 
      !
    end do do_cards 
    !
1999 continue
    !
    if ( i_err .lt. 0 ) &
    print'( " error in cardset_card_to_file during write " )'
    !
    j_err = 0
    !
    call cardset_file_close ( file_unit, j_err )
    !
    if ( j_err .ne. 0 ) &
    print'( " error in cardset_card_to_file during close " )'
    !
    return
    !
997 continue
    !
    print'( " error in cardset_card_to_file during cio_fopen ")'
    !
    go to 999
    !
998 continue
    !
    print'( " error in cardset_card_to_file struct does not exist " )'
    !
    go to 999
    !
999 continue
    !
    print'( /,  " error in cardset_card_to_file file_name=", a )', &
    trim(file_name)
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine cardset_card_to_file 
  !
!!------------------------ cardset_file_to_card -----------------------!!
!!------------------------ cardset_file_to_card -----------------------!!
!!------------------------ cardset_file_to_card -----------------------!!

! This subroutine reads the contents of a file into the data cards.
  subroutine cardset_file_to_card ( file_name, card_set, i_err )
    !
    character(len=*),    intent (in   ) :: file_name       ! file name
    type ( cardset_struct ),    pointer :: card_set        ! cardset structure
    integer,             intent (inout) :: i_err           ! err 0=o.k. -1=error
    !
    integer                             :: file_unit       ! err 0=o.k. -1=error
    !
    integer                             :: j_err           ! err 0=o.k. -1=error
    !
    integer                             :: n_card          ! card number
    integer                             :: j_card          ! card index
    !
    character(len=CARDSET_DATACARD_LENGTH):: crd_xxx          ! card image
    !
    i_err = 0
    !
    j_card = 0
    !
    ! create and clear the cardset structure
    !
    if (     associated ( card_set ) ) &
    call cardset_delete ( card_set )
    !
    call cardset_create ( card_set )
    !
    ! clear the cardset structure
    !
    call cardset_clear  ( card_set )
    !
    ! set the card set name
    !
    call cardset_set_name ( card_set, 'cardset_cardset' )
    !
    ! open the file
    !
    call cardset_file_open ( file_name, 'r', file_unit, i_err )
    !
    if ( i_err .lt. 0 ) go to 998
    !
    ! add the input cards to the cardset structure
    !
1   continue
    !
    crd_xxx = ' '
    !
    n_card = cio_fgetline ( &
           string=crd_xxx, imax=CARDSET_DATACARD_LENGTH, unit=file_unit )
    !
    if ( n_card .lt. 0 ) goto 2
    !
    j_card = j_card + 1
    !
    ! add this card image to the card
    !
    call cardset_add_card ( card_set, crd_xxx )
    !
    go to 1
    !
2   continue
    !
    xxif_err : &
    if ( n_card .lt. 0 .and. n_card .ne. CIO_EOF ) then
      !
      print'("cardset_file_to_card(",i2,"): error on read j_card=",i8, i6)', &
      j_card, n_card
      !
      i_err = -1
      !
    end if xxif_err 
    !
    if ( i_err .ne. 0 ) go to 999
    !
1999 continue
    !
    ! close the input unit
    !
    call cardset_file_close ( file_unit, j_err )
    !
    if ( j_err .ne. 0 ) &
    print'( " error in cardset_file_to_card during close " )' 
    !
    return
    !
998 continue
    !
    print'( &
    & /,  " error in cardset_file_to_card during cardset_file_open file=", a &
    & )', trim(file_name)
    !
    go to 999
    !
999 continue
    !
    print'( /,  " error in cardset_file_to_card file_name=", a )', &
    trim ( file_name )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine cardset_file_to_card 
  !
!!------------------------ cardset_file_open -----------------------!!
!!------------------------ cardset_file_open -----------------------!!
!!------------------------ cardset_file_open -----------------------!!

! This subroutine opens a file for reading or writing.
  subroutine cardset_file_open ( file_name, file_stat, file_unit, i_err )
    !
    character(len=*),    intent (in   ) :: file_name       ! file name
    character(len=*),    intent (in   ) :: file_stat       ! file name
    integer,             intent (inout) :: file_unit       ! file unit
    integer,             intent (inout) :: i_err           ! err 0=o.k. -1=error
    !
    !print'(" cardset_file_open stat=",a8," name=",a)', &
    !, trim(file_stat), trim(file_name)
    !
    i_err = 0
    !
    file_unit = -1
    !
    if ( string_upper_compare ( file_stat, 'w' ) ) &
    i_err = cio_remove ( file_name )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    i_err = 0
    !
    ! open the file
    !
    file_unit = cio_fopen ( file_name, file_stat )
    !
    !print'(" aa1 cardset_file_open u=",i8)', file_unit
    !
    if ( file_unit .lt. 0 ) go to 997
    !
    ! rewind the input unit
    !
    call  cio_frewind ( file_unit )
    !
    return
    !
997 continue
    !
    print'( /,  " error in cardset_file_open during cio_fopen err=", i8 )', &
    file_unit
    !
    print'( " error in cardset_file_open during cio_fopen " )'
    !
    go to 999
    !
998 continue
    !
    print'( /,  " error in cardset_file_open during cio_remove " )'
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /,  " error in cardset_file_open file_name=", a &
    & )', &
    trim ( file_name )
    !
    print'(" cardset_file_open stat=",a8," name=",a)', &
    trim(file_stat), trim(file_name)
    !
    i_err = -1
    !
    return
    !
  end subroutine cardset_file_open 
  !
!!------------------------ cardset_file_close -----------------------!!
!!------------------------ cardset_file_close -----------------------!!
!!------------------------ cardset_file_close -----------------------!!

! This subroutine closes a file.
  subroutine cardset_file_close ( file_unit, i_err )
    !
    integer,             intent (inout) :: file_unit       ! file unit
    integer,             intent (inout) :: i_err           ! err 0=o.k. -1=error
    !
    character(len=filename_length)      :: file_name       ! file name
    integer                             :: perms           ! permission value
    integer                             :: status          ! status vlaue
    integer                             :: file_exist      ! file exist flag
    !
    i_err = 0
    !
    ! close the file
    !
    ! get the file name associated with this file
    !
    status = cio_finquire ( file_unit, file_name )
    !
    ! close the file
    !
    i_err = cio_fclose ( file_unit )
    !
    if ( i_err .ne. 0 ) &
    print'( " error in cardset_file_close during close " )'
    !
    ! set the protection of the file
    !
    perms = 64 * 7 + 8 * 5 + 5
    !
    !perms            character(len=*) rwx,rwx,rwx or rwx------ ...
    !        or
    !  perms            integer         decimal equiv of octal 644, 755,...
    !                                   (for 643 use 64*6 + 8*4 + 3)
    ! check for the existance of this file
    !
    file_exist = finquire_file ( file_name )
    !
    ! if it exists set its protection
    !
    if ( file_exist .ne. finquire_not_found ) &
    status = cio_chmod  ( file_name, perms )
    !
    return
    !
  end subroutine cardset_file_close 
  !
!!------------------------ cardset_card_print -----------------------!!
!!------------------------ cardset_card_print -----------------------!!
!!------------------------ cardset_card_print -----------------------!!

! This subroutine prints the contents of the data crads.
  subroutine cardset_card_print ( &
                                  print_title, print_unit, print_flag, &
                                  file_name, card_set &
                                )
    !
    character(len=*),    intent (in   ) :: print_title     ! print title
    integer,             intent (in   ) :: print_unit      ! print unit
    logical,             intent (in   ) :: print_flag      ! print flag
    character(len=*),    intent (in   ) :: file_name       ! file name
    type ( cardset_struct ),    pointer :: card_set        ! cardset structure
    !
    integer                             :: n_card          ! card num
    integer                             :: j_card          ! card index
    !
    character(len=80)                   :: crd_80          ! card image 80
    character(len=CARDSET_DATACARD_LENGTH):: crd_xxx         ! card image 
    character(len=CARDSET_DATACARD_LENGTH):: c_nam           ! card name
    !
    n_card = cardset_num_cards ( card_set )
    !
    call cardset_get_name ( card_set, c_nam )
    !
    if ( print_flag ) &
    print'( &
    & /, " cardset_card_print ", a, &
    & /, " file_name=", a, &
    & /, " card_name=", a &
    & /, " number of cards=", i8, &
    & /, " index card " &
    & )', &
    trim ( print_title ), trim ( file_name ), n_card
    !
    do_write_cards : do j_card = 1 , n_card
      !
      crd_xxx = ' '
      !
      call cardset_get_card ( card_set, j_card,  crd_xxx, crd_80 )
      !
      xxif_print_flag : if ( print_flag ) then
        !
        print'( 1x, i8, 1x, a )', j_card, trim ( crd_xxx )
        !
      else xxif_print_flag 
        !
        print'( a )', trim ( crd_xxx )
        !
      end if xxif_print_flag 
      !
    end do do_write_cards 
    !
    return
    !
  end subroutine cardset_card_print

!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!


end module cardset_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

