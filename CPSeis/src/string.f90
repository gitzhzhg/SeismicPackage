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
!!----------------------------- string.f90 -------------------------------!!
!!----------------------------- string.f90 -------------------------------!!
!!----------------------------- string.f90 -------------------------------!!

                    ! other files are:  string_crou.c




!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E         
!
! Name       : STRING
! Category   : character
! Written    : 1996-12-06   by: Tom Stoeckley
! Revised    : 2008-12-11   by: B. Menger
! Maturity   : beta
! Purpose    : A collection of routines for manipulating character strings.
! Portability: No known limitations, but see comment in string_put_tokens.
!
!-------------------------------------------------------------------------------
!</brief_doc>



!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION             
!
! This module is a collection of routines for manipulating character strings.
! It includes routines for modifying strings, concatenating strings, encoding
! and decoding strings, converting between hollerith and character variables,
! and other useful operations.  There is a related primitive called STR
! for use in the C programming language.
!
!-------------------------------------------------------------------------------
!              USEFUL FORTRAN-90 STRING MANIPULATION ROUTINES
!
! The following Fortran-90 string manipulation routines are available.
! Their functionality is not duplicated in this module.
!
!   c = achar    (i)              ! character in i-th position in ascii table.
!   i = iachar   (c)              ! position of character in ascii table.
!   i = len      (string)         ! length including trailing spaces.
!   i = len_trim (string)         ! length excluding trailing spaces.
!   s = trim     (string)         ! string with trailing spaces removed.
!   s = adjustl  (string)         ! adjust left by removing leading spaces.
!   s = adjustr  (string)         ! adjust right by removing trailing spaces.
!
!   s = repeat   (string,ncopies)         ! repeated concatenation.
!   i = index    (string,substring,back)  ! reverse search if back is true.
!   i = scan     (string,set,back)        ! scan for first of any set of chars.
!   i = verify   (string,set,back)        ! scan for first char not in set.
!
! To concatenate two strings without including trailing blanks:
!
!   s = trim(string1)//trim(string2)
!
! Note: The Fortran-77 functions CHAR and ICHAR can be used instead of
!       ACHAR and IACHAR, but the two pairs of functions may not be identical.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS         
!
! For each subroutine or function documented here, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!------------------------------------------------------------------------------
!                      TO GET TIME AND DATE STRINGS
!
!                                        returns time and/or date as follows:
!                                 o      ------------------------------------
!        call string_time_date (string)       Day Mon DD HH:MM:SS YYYY
!        call string_time      (string)       HH.MM.SS
!        call string_date      (string)       YYYY-MM-DD
!
! character(len=*) STRING = character string to receive the time and/or date.
!
!-------------------------------------------------------------------------------
!                    TO DO A CASE-INSENSITIVE COMPARE
!
!             o                              i        i
!           equal = string_upper_compare (string1, string2)
!
! character(len=*) STRING1 = one of the two strings to compare with each other.
! character(len=*) STRING2 = the other string to compare.
! logical            EQUAL = true if the strings match in a case-insensitive
!                             manner (the arguments are not altered).
!
!-------------------------------------------------------------------------------
!            TO CONVERT A CHARACTER STRING TO ALL UPPER CASE   
!            TO CONVERT A CHARACTER STRING TO ALL LOWER CASE   
!            TO REMOVE UNPRINTABLE CHARACTERS FROM A CHARACTER STRING
!            TO REPLACE ALL CHAR(0) CHARACTERS WITH BLANKS     
!            TO REMOVE BLANKS FROM A CHARACTER STRING          
!
!               o                         i
!             string2 = string_2_upper (string1)
!             string2 = string_2_lower (string1)
!
!           call string_to_upper          (string)
!           call string_to_lower          (string)
!           call string_strip_unprintable (string)
!           call string_replace_zeroes    (string)
!           call string_squeeze_blanks    (string, nsize)
!           call string_strip_blanks      (string, nsize)
!           call string_compress_blanks   (string, nsize)
!                                            b       o
!                                                   opt
!
!           call string_to_upper        (string1, string2)
!           call string_to_lower        (string1, string2)
!           call string_squeeze_blanks  (string1, string2, nsize)
!           call string_strip_blanks    (string1, string2, nsize)
!           call string_compress_blanks (string1, string2, nsize)
!                                          i        o        o
!                                                           opt
!
! character(len=*)  STRING = character string to convert (input and output).
! character(len=*) STRING1 = character string to convert (input).
! character(len=*) STRING2 = converted character string  (output).
! integer,optional   NSIZE = trimmed length of output string (disregarding
!                              trailing blanks).
!
! STRING_REPLACE_ZEROES resets the first char(0) character with a blank,
! and then resets all following characters to blank.
!
! STRING_SQUEEZE_BLANKS removes all initial and embedded blanks.
! STRING_STRIP_BLANKS does the same except within sections of the string
!  contained within quotes (").
! STRING_COMPRESS_BLANKS removes all initial blanks and compresses
!  consecutive embedded blanks to a single blank.
!
!-------------------------------------------------------------------------------
!               TO VALIDATE THE CONTENTS OF A CHARACTER STRING
!
!                                           b
!                   call string_validate (string)
!
! character(len=*) STRING = string to be validated.
!
! Calls the following routines in the order specified:
!        STRING_REPLACE_ZEROES
!        STRING_STRIP_BLANKS
!        STRING_STRIP_UNPRINTABLE
!
! Then resets STRING to the named constant STRING_EMPTY if STRING is empty.
!
! STRING is considered to be empty if it begins with the character char(0),
! or is blank, or is set to a case-insensitive equivalent of the named
! constant STRING_EMPTY.
!
!                            +++++++++++++++++
!
! The named constant STRING_EMPTY is set to 'NONE'.  Users should use
! this constant instead of the string 'NONE' in case this constant is
! changed in the future.
!
!-------------------------------------------------------------------------------
!              TO CONVERT NUMBERS TO AND FROM CHARACTER STRINGS 
!
! To convert (encode) a number to a character string:
!
!                                   opt  opt
!                         i    o     i    i
!     call string_ii2cc (ivar,cvar,nchar)        ! integer variables
!     call string_ff2cc (fvar,cvar,nchar,ndec)   ! real variables
!     call string_dd2cc (dvar,cvar,nchar,ndec)   ! double precision variables
!     call string_ll2cc (lvar,cvar)              ! logical variables
!
!                                   opt  opt
!    o                    i          i    i
!   cvar = string_ii2ss (ivar,     nchar)        ! integer variables
!   cvar = string_ff2ss (fvar,     nchar,ndec)   ! real variables
!   cvar = string_dd2ss (dvar,     nchar,ndec)   ! double precision variables
!   cvar = string_ll2ss (lvar)                   ! logical variables
!
! To convert (decode) a character string to a number:
!
!                                   opt   opt
!                         i    o     o     o
!     call string_cc2ii (cvar,ivar,istat,errmsg)  ! integer variables
!     call string_cc2ff (cvar,fvar,istat,errmsg)  ! real variables
!     call string_cc2dd (cvar,dvar,istat,errmsg)  ! double precision variables
!     call string_cc2ll (cvar,lvar,istat,errmsg)  ! logical variables
!
!                                   opt   opt
!    o                    i          o     o
!   ivar = string_ss2ii (cvar,     istat,errmsg)  ! integer variables
!   fvar = string_ss2ff (cvar,     istat,errmsg)  ! real variables
!   dvar = string_ss2dd (cvar,     istat,errmsg)  ! double precision variables
!   lvar = string_ss2ll (cvar,     istat,errmsg)  ! logical variables
!
!
! integer          IVAR = number to convert to or from a text string.
! real             FVAR = number to convert to or from a text string.
! double precision DVAR = number to convert to or from a text string.
! logical          LVAR = number to convert to or from a text string.
! character*(*)    CVAR = text string containing encoded number.
! integer         NCHAR = maximum number of characters to encode  (optional).
! integer          NDEC = maximum number of decimals to encode    (optional).
! integer         ISTAT = status of the decoded number            (optional).
! character*(*)  ERRMSG = error message (blank unless ISTAT = -1) (optional).
!
!
!  1. If NCHAR is zero or negative or omitted, it is assumed to match the
!     length of the character variable CVAR.  The encoded value will be
!     made to fit into CVAR if possible, by reducing its precision for
!     real and double precision numbers.  If an encoded value cannot fit,
!     CVAR will contain an asterisk.
!
!  2. If NDEC is large or omitted, no limit is imposed on the number
!     of decimals to convert (within the permitted range of NCHAR).
!
!  3. The routines documented above support a special "nil" value for
!     each type of number.  These nil values are defined in the
!     NAMED_CONSTANTS primitive.  A nil value corresponds to a blank
!     character variable.
!
!  4. Upon return from the second set of routines:
!     (a) ISTAT=1 means the conversion was successful.
!     (b) ISTAT=0 means the number was set to nil because CVAR was a blank.
!     (c) ISTAT=-1 means the number was set to nil because the conversion
!            was not successful.
!     (d) ERRMSG (if present) will contain an error message if ISTAT=-1.
!
!  5. Logical values correspond to the strings 'YES' and 'NO'.
!
!-------------------------------------------------------------------------------
!               TO ENCODE INFORMATION INTO A CHARACTER STRING
!
! These subroutines are provided to facilitate encoding information into
! a character string for subsequent printing, or for any other useful
! purpose.  The information is encoded with the STRING_II2SS (etc.) functions
! in the order of the argument list.  Any of the optional arguments can be
! missing.
!                                                 opt   opt   opt
!                               o      i     i     i     i     i
!         call string_encode (buffer, msg1, var1, msg2, var2, msg3)
!
! character(len=*)    buffer = encoded string.
! character(len=*)    msg1   = beginning text.
! (several types)     var1   = first variable.
! character(len=*)    msg2   = subsequent text (optional).
! (several types)     var2   = second variable (optional).
! character(len=*)    msg3   = subsequent text (optional).
!
! The types of the arguments VAR1 and VAR2 can be integer, real, double
! precision, character(len=*), or logical.  VAR1 and VAR2 must have the
! same type.
!
!-------------------------------------------------------------------------------
!             TO PROTECT A TOKEN CONTAINING SPECIAL CHARACTERS
!
!                                             b       i
!                call string_protect_token (token, special)
!                call string_recover_token (token)
!
! character(len=*) token   = string possibly containing special characters.
! character(len=*) special = list of special characters.
!
! STRING_PROTECT_TOKEN:
! The TOKEN is checked to find out whether it contains any of the specified
! special characters.  It is then modified if necessary according to details
! specified below.  The special characters consist of all characters specified
! in SPECIAL.  The space character is also always a special character.
!
! STRING_RECOVER_TOKEN:
! Any modifications performed by STRING_PROTECT_TOKEN are undone.
!
! Details:
! (1) Trailing blanks in the token are ignored.
! (2) If the token is entirely blank, it is changed to two double quotes ("").
! (3) Otherwise if the token contains one or more double quotes ("),
!       it is enclosed by single quotes (').
! (4) Otherwise if the token contains one or more single quotes ('),
!       it is enclosed by double quotes (").
! (5) Otherwise if the token contains any special characters or spaces,
!       it is enclosed by double quotes (").
! (6) If the token is now enclosed by double quotes and also contains any
!       double quotes, each contained double quote is replaced by two double
!       quotes.
! (7) If the token is now enclosed by single quotes and also contains any
!       single quotes, each contained single quote is replaced by two single
!       quotes.
!
! The primary purpose of this pair of subroutines is to prepare a character
! string (called a "token") so that it can be placed into another larger
! character string (called a "data card") which contains additional tokens and
! special control characters.  This preparation is necessary to insure that
! any characters in the token are not mistaken as control characters or
! multiple tokens, and also to insure that the original token can be
! subsequently retrieved from the data card.
!
!-------------------------------------------------------------------------------
!                  TO PUT LIST OF TOKENS ONTO A RECORD
!                  TO GET LIST OF TOKENS FROM A RECORD
!
!                                                                 opt    opt
!                              o       i        i        i         i      o
!    call string_put_tokens (record, tokens, ntokens, nilstring, widths, msg)
!    call string_get_tokens (record, tokens, ntokens, nilstring, length)
!                              i       o        o        i         i
!                                                                 opt
!
! character(len=*) record     = record (data card) containing tokens.
! character(len=*) tokens(:)  = list of tokens.
! integer          ntokens    = number of tokens in list.
! character(len=*) nilstring  = special non-blank string for nil values.
! integer          widths(:)  = widths of each token (in characters).
! character(len=*) msg        = error message (blank if no error).
! integer          length     = length of string to check
!                                 (uses len_trim(record) if omitted).
!
! STRING_PUT_TOKENS:
! All tokens in the TOKENS array are put onto the record, separated by a
! blank (space character).  Any blanks in a token (except trailing blanks)
! must be replaced by some other character before calling this routine.
! If the token is entirely blank, it is replaced by the specified NILSTRING.
!
! STRING_GET_TOKENS:
! All tokens on the record are placed into the TOKENS array.  If the array
! is too small to contain them all, the excess tokens are not returned.
! If the token is equal to the specified NILSTRING, it is set to blank.
!
! The tokens in the record are blank-delimited character strings.
!
! You will note that these tokens are not protected and recovered as the
! routines STRING_PROTECT_TOKEN and STRING_RECOVER_TOKEN do it.  The reason
! is to maintain as much efficiency as possible for use in reading and
! writing generic ascii files containing columns of values (several tokens
! on each card image in the file).
!
! If WIDTHS is present in the call to STRING_PUT_TOKENS, the width of each
! token will be the maximum of the actual trimmed width and the value specified
! by the WIDTHS argument for that token.  A width of zero effectively turns
! off this operation for the corresponding token.
!
! If a WIDTHS element is positive, the associated field is right justified.
! If a WIDTHS element is negative, the associated field is left justified.
!
! If MSG is present, it will be set to an error message if an element of
! WIDTHS is too small to contain the token.  If there are no errors, or if
! WIDTHS is not present, MSG will be set to a blank string.
!
!-------------------------------------------------------------------------------
!                   TO REPLACE A CHARACTER IN A STRING
!                                                                   opt
!                                          b       i        i        i
!         call string_replace_character (string, oldchar, newchar, length)
!
! character(len=*) string   = string containing characters to replace.
! character(len=1) oldchar  = character to replace.
! character(len=1) newchar  = replacing character.
! integer          length   = length of string to check
!                               (uses len_trim(string) if omitted).
!
! All occurrences of the old character in the string are replaced by the
! new character.  Trailing blanks are not changed.
!
!-------------------------------------------------------------------------------
!             TO CONVERT BETWEEN CHARACTER AND HOLLERITH VARIABLES
!
! These subroutines are intended to facilitate passing character variables
! between C and Fortran.  They are to be called from Fortran.
!
!                                          i      o
!                call string_cc2hh       (cvar,  hvar)
!                call string_cc2hh_alloc (cvar, phvar)
!                call string_hh2cc       (hvar,  cvar)
!                                          i      o
!
!                                          i      o      i
!                call string_cc2hh_exact (cvar,  hvar, nchar)
!                call string_hh2cc_exact (hvar,  cvar, nchar)
!                                          i      o      i
!
!                                    i       o       i        i
!         call string_cc2hh_array (carray, harray, nwords, nelements)
!         call string_hh2cc_array (harray, carray, nwords, nelements)
!                                    i       o       i        i
!
! integer           hvar(*) = null-terminated hollerith array.
! character(len=*)  cvar    = equivalent character variable.
! integer,pointer  phvar(:) = pointer to null-terminated hollerith array.
!
! integer           harray(nwords,nelements) = null-terminated hollerith array.
! character(len=*)  carray(nelements)        = equivalent character array.
!
! integer           nchar     = exact number of characters to copy.
! integer           nwords    = number of hollerith integers per element.
! integer           nelements = number of elements in array.
!
! HARRAY can be either one or two dimensions, but must represent a contiguous
! array in memory containing NWORDS * NELEMENTS integers, or the equivalent
! amount of contiguous memory in C.
!
! STRING_CC2HH:
!  (1) Used to convert a Fortran character variable to a null-terminated
!        hollerith array before passing it to a C or C++ function.
!  (2) Copies character variable CVAR to hollerith buffer HVAR(*).
!  (3) HVAR(*) will be terminated with null after the last non-blank character.
!  (4) HVAR(*) must be large enough to store the characters from CVAR plus null.
!  (5) If CVAR is blank, the first character of HVAR(*) will be null.
!  (6) CVAR should not contain any nulls.
!
! STRING_CC2HH_ALLOC:
!  (1) Sames as STRING_CC2HH except that the hollerith array is deallocated
!        and reallocated to contain all characters plus the null termination.
!          (PHVAR must be nullified before first use.)
!          (PHVAR should be conditionally deallocated after last use.)
!
! STRING_HH2CC:
!  (1) Used to convert a null-terminated hollerith array to a blank-filled
!        Fortran character variable after receiving it from a C or C++ function.
!  (2) Copies hollerith buffer HVAR(*) to character variable CVAR.
!  (3) CVAR will be blank filled (no null).
!  (4) HVAR(*) should be terminated with null.
!  (5) Only chars preceding the first NULL in HVAR(*) will be copied to CVAR.
!  (6) No more than len(cvar) characters will be copied to CVAR.
!
! STRING_CC2HH_EXACT:
!  (1) Same as STRING_CC2HH except that HVAR(*) will be blank filled and
!        terminated with null after exactly NCHAR characters.
!
! STRING_HH2CC_EXACT:
!  (1) Same as STRING_HH2CC except that HVAR(*) need not be terminated with
!        null, and no more than NCHAR characters will be copied to CVAR.
!
! STRING_CC2HH_ARRAY:
!  (1) Calls STRING_CC2HH to convert each element in array CARRAY to the
!        corresponding representation in HARRAY.
!  (2) Take care that the arguments NWORDS and NELEMENTS are specified in the
!        correct order.
!
! STRING_HH2CC_ARRAY:
!  (1) Calls STRING_HH2CC to convert each element in array HARRAY to the
!        corresponding representation in CARRAY.
!  (2) Take care that the arguments NWORDS and NELEMENTS are specified in the
!        correct order.
!
! The hollerith array has type integer to guarantee that it will
! be passed by address rather than descripter (as with VMS), and to
! eliminate the non-portable problem of passing a character length
! between C and Fortran.
! 
!-------------------------------------------------------------------------------
!             TO GET INFORMATION ABOUT CHARACTER/INTEGER WORD SIZES
! 
!     chars_per_int = string_chars_per_integer    ()
!     num_integers  = string_num_integers         (num_chars)
!     num_integers  = string_num_integers         (cvar)
!     num_integers  = string_num_trimmed_integers (cvar)
!         o                                         i
! 
! integer chars_per_int = number of characters fitting into one integer word.
! integer     num_chars = number of characters to fit into a hollerith array. 
! integer  num_integers = number of integers required to store the specified
!                           number of characters plus a null terminator.
! character(len=*) cvar = character variable.
! 
! Here a hollerith array is defined as an array of integers which can contain
! all the characters in a character variable plus a null terminator.
! 
! STRING_CHARS_PER_INT:
!   Usually returns 4 on most machines or 8 on Cray machines.
!
! STRING_NUM_INTEGERS with the NUM_CHARS argument:
!   Returns the number of integers (dimension of an integer array) required
!   to store the specified number of characters plus a null terminator.
!
! STRING_NUM_INTEGERS with the CVAR argument:
!   Returns the number of integers (dimension of an integer array) required
!   to store all of the characters which could be stored in CVAR, plus a
!   null terminator.
!
! STRING_NUM_TRIMMED_INTEGERS with the CVAR argument:
!   Returns the number of integers (dimension of an integer array) required
!   to store all of the characters which are actually stored in CVAR (excluding
!   trailing blanks), plus a null terminator.
!
!-------------------------------------------------------------------------------
!                        TO TEST A SINGLE CHARACTER
!
!                                                                   equivalent
!                                       condition to test against   C function
!                                       -------------------------   ---------
!  answer = string_is_alphanum (c)      letter or digit              isalnum
!  answer = string_is_alpha    (c)      letter (a-z or A-Z)          isalpha
!  answer = string_is_control  (c)      control character            iscntrl
!  answer = string_is_digit    (c)      digit (0-9)                  isdigit
!  answer = string_is_graph    (c)      printable char except space  isgraph
!  answer = string_is_lower    (c)      lower case letter            islower
!  answer = string_is_print    (c)      printable char incl space    isprint
!  answer = string_is_punct    (c)      printable char except space  ispunct
!                                         or letter or digit                
!  answer = string_is_space    (c)      space character              isspace
!  answer = string_is_upper    (c)      upper case letter            isupper
!  answer = string_is_hex      (c)      hexadecimal digit            isxdigit
!  answer = string_is_one      (c,incl) one of the chars in INCL
!    o                          i  i
!
! logical   ANSWER = true if the argument C satisfies the condition.
! char(len=1)    C = single character to test.
! char(len=*) INCL = set of characters to test against.
!
! A control character is a non-printable character (including char(0) but
! not space).
!
! A space character is space, formfeed, newline, carriage return, tab, or
! vertical tab.
!
! Trailing blanks (space characters) in INCL are not included in the test.
! To include a blank in INCL, the blank must be followed by at least one
! non-blank character.
!
!-------------------------------------------------------------------------------
!                     TO SEARCH FOR A SUBSTRING
!
!                                                   opt
!            o                     i       i         i
!           indx = string_index (string,substring,reverse)
!
! character(len=*)     string = string to search through.
! character(len=*)  substring = substring to search for.
! logical             reverse = true to search in reverse dir (default false).
! integer                indx = index of first matching character.
!
! This function is an alternative for the intrinsic INDEX function because
! the Portland Group compiler sometimes returns the wrong value for INDX when
! REVERSE is true (as if the .true. argument were absent).
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY               
!
!     Date        Author     Description
!     ----        ------     -----------
! 34. 2008-12-11  B. Menger   Nullified pointer
!033. 2006-04-25  B. Menger   Removed Unused Variables.
! 32. 2004-05-03  R Selzler    Correct one compiler error, Absoft 8.0 with
!                              latest quick fix: Dummy arguments with the
!                              INTENT(OUT) attribute must be defined before use.
! 31. 2004-03-15  Stoeckley  Allow use of optional argument nchar=0 to match
!                             the documentation.
! 30. 2004-01-20  Stoeckley  Fix bug in which string_cc2ll occasionally reads
!                             one character off the beginning or end of the
!                             CVAR character variable when CVAR contains all
!                             blanks except possibly the last character.
! 29. 2003-10-18  Stoeckley  Add STRING_INDEX as an alternative for the
!                             intrinsic INDEX function because the Portland
!                             Group compiler INDEX function sometimes
!                             misbehaves.
! 28. 2002-06-24  Stoeckley  Change back to use only actual spaces as
!                             delimiters when getting tokens from a string;
!                             remove the use of | to replace white spaces
!                             in tokens processed by STRING_PUT_TOKENS and
!                             STRING_GET_TOKENS; add optional argument LENGTH
!                             to STRING_REPLACE_CHARACTER and STRING_GET_TOKENS;
!                             add STRING_CC2HH_EXACT and STRING_HH2CC_EXACT.
! 27. 2002-05-20  Stoeckley  Use any white space (not just actual spaces) as
!                             delimiters when getting tokens from a string.
! 26. 2002-04-11  Stoeckley  Add optional argument MSG to STRING_PUT_TOKENS.
! 25. 2001-12-26  Stoeckley  Add subroutines STRING_CC2HH_ARRAY and
!                             STRING_HH2CC_ARRAY, and add optional WIDTHS
!                             argument to STRING_PUT_TOKENS.
! 24. 2001-06-04  Stoeckley  Add functions STRING_2_UPPER and STRING_2_LOWER
!                             as alternatives to subroutines.
! 23. 2000-09-15  Stoeckley  Speed up STRING_UPPER_COMPARE; add documentation
!                             regarding timings for several alternate routines
!                             for STRING_UPPER_COMPARE and STRING_TO_UPPER;
!                             replace STRING_TO_UPPER with STRING_UPPER_COMPARE
!                             in STRING_VALIDATE.
! 22. 2000-08-21  Stoeckley  Add alternative routines for STRING_TO_UPPER and
!                             STRING_TO_LOWER which call C routines for slight
!                             speedup with absoft and slight slowdown with
!                             solaris.  Also add alternative routines for
!                             STRING_CC2HH and STRING_HH2CC which use the
!                             TRANSFER function, but these are not well tested
!                             and are not any faster than the tried-and-true
!                             versions (and cause an internal compiler error
!                             with pgf90).  These alternative routines are
!                             purposely not documented to keep them from being
!                             used, but are available for testing.
! 21. 2000-06-27  Stoeckley  Add new overloaded routines STRING_TO_UPPER and
!                             STRING_TO_LOWER which take two arguments.
! 20. 2000-05-17  Stoeckley  Add STRING_UPPER_COMPARE.
! 19. 2000-05-08  Stoeckley  Fix optional argument bug in
!                             STRING_COMPRESS_BLANKS.
! 18. 2000-05-03  Stoeckley  Add STRING_COMPRESS_BLANKS.
! 17. 2000-04-19  Stoeckley  Change dates to use dashes instead of slashes
!                             (doc change only - actual change is in str.c).
! 16. 2000-02-08  Stoeckley  Fix bug in STRING_ENCODE subroutines when
!                             the built string is too long, and improve the
!                             flexibility of STRING_ENCODE.
! 15. 2000-02-04  Stoeckley  Add the TRIM function to components of strings
!                             encoded by STRING_ENCODE.
! 14. 2000-01-24  Stoeckley  Add left adjustment to strings encoded by
!                             STRING_ENCODE.
! 13. 2000-01-17  Stoeckley  Add routine STRING_STRIP_UNPRINTABLE; replace
!                             STRING_VALIDATE_CONTENTS with more functional
!                             STRING_VALIDATE.
! 12. 2000-01-06  Stoeckley  Add named constant STRING_EMPTY.
! 11. 1999-12-29  Stoeckley  Enforce limit of NCHAR to the length of the
!                             character variable into which the number is
!                             encoded; add functions to test a single character
!                             for certain conditions (interface to C functions).
! 10. 1999-11-17  Stoeckley  Add ident string for RCS.
!  9. 1999-10-20  Stoeckley  Add routines string_put_tokens and
!                              string_get_tokens and string_replace_character.
!  8. 1999-09-22  Stoeckley  Add routines string_cc2hh_alloc,
!                              string_num_integers, string_chars_per_integer.
!  7. 1999-09-10  Stoeckley  Add reference to other files.
!  6. 1999-08-31  Stoeckley  Replaced calls to string_..._internal with calls
!                             to the str.c primitive, and added time/date
!                             subroutines.
!  5. 1999-08-11  Stoeckley  Add functions (as an alternative to subroutines)
!                             to convert between numbers and strings.
!                             Also fixed bug in string_protect_token.
!                             Also fixed bug where ISTAT was not optional.
!                             Also generalized string_cc2ll.
!  4. 1999-06-24  Stoeckley  Converted from old system.
!                             Converted to free-form format.
!                             Changed name from string_util to string.
!                             Added routines from convert_ii2cc.
!                             Added routines (simplified) from convert_cc2hh.
!                             Added routines (private) from char_into_buffer.
!                             Added string_encode routines.
!                             Added string_to_lower.
!                             Added string_strip_blanks provided by Donna.
!  3. 1999-01-06  Goodger    Begin using the fortran90 compiler.          
!  2. 1996-12-13  Goodger    Added CPS DOC cards for ease in printing a
!                             CPS manual.
!  1. 1996-12-06  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
! This module contains the function STRING_INDEX, which is an alternative for
! the intrinsic INDEX function, because the Portland Group compiler sometimes
! returns the wrong value when doing a reverse search (as if it were doing
! a forward search).
!
!-------------------------------------------------------------------------------
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module string_module
      use named_constants_module
      use sizeof_module
      implicit none

      public
      private :: string_enclose_token
      private :: string_exclose_token
      private :: string_private_squeeze

      character(len=100),public,save :: STRING_IDENT = &
       '$Id: string.f90,v 1.33 2006/04/25 13:24:07 Menger prod sps $'

      character(len=4),public ,parameter :: STRING_EMPTY = 'NONE'
      integer         ,private,parameter :: NBUF         = 200


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


      interface string_get_tokens
          module procedure string_get_tokens1
          module procedure string_get_tokens2
      end interface

      interface string_replace_character
          module procedure string_replace_character1
          module procedure string_replace_character2
      end interface

      interface string_to_upper
          module procedure string_to_upper1
          module procedure string_to_upper2
      end interface

      interface string_to_upper_alternative
          module procedure string_to_upper1_alternative
          module procedure string_to_upper2_alternative
      end interface

      interface string_to_upper_another
          module procedure string_to_upper1_another
          module procedure string_to_upper2_another
      end interface

      interface string_to_lower
          module procedure string_to_lower1
          module procedure string_to_lower2
      end interface

      interface string_to_lower_alternative
          module procedure string_to_lower1_alternative
          module procedure string_to_lower2_alternative
      end interface

      interface string_to_lower_another
          module procedure string_to_lower1_another
          module procedure string_to_lower2_another
      end interface

      interface string_squeeze_blanks
          module procedure string_squeeze_blanks1
          module procedure string_squeeze_blanks2
      end interface

      interface string_strip_blanks
          module procedure string_strip_blanks1
          module procedure string_strip_blanks2
      end interface

      interface string_compress_blanks
          module procedure string_compress_blanks1
          module procedure string_compress_blanks2
      end interface

      interface string_num_integers
          module procedure string_num_integers1
          module procedure string_num_integers2
      end interface

      interface string_encode
          module procedure string_encode_cici
          module procedure string_encode_cfcf
          module procedure string_encode_cdcd
          module procedure string_encode_cccc
          module procedure string_encode_clcl
      end interface


!!----------------------- start of subroutines ----------------------------!!
!!----------------------- start of subroutines ----------------------------!!
!!----------------------- start of subroutines ----------------------------!!


      contains


!!---------------------- get time and date strings ------------------------!!
!!---------------------- get time and date strings ------------------------!!
!!---------------------- get time and date strings ------------------------!!


      subroutine string_time_date (string)
      implicit none
      character(len=*),intent(out) :: string                ! argument
      integer                      :: buffer(20)            ! local

      call string_crou_time_date (buffer)
      call string_hh2cc          (buffer, string)
      return
      end subroutine string_time_date



      subroutine string_time (string)
      implicit none
      character(len=*),intent(out) :: string                ! argument
      integer                      :: buffer(20)            ! local

      call string_crou_time (buffer)
      call string_hh2cc     (buffer, string)
      return
      end subroutine string_time



      subroutine string_date (string)
      implicit none
      character(len=*),intent(out) :: string                ! argument
      integer                      :: buffer(20)            ! local

      call string_crou_date (buffer)
      call string_hh2cc     (buffer, string)
      return
      end subroutine string_date


!!------------------------ string upper compare ----------------------------!!
!!------------------------ string upper compare ----------------------------!!
!!------------------------ string upper compare ----------------------------!!


!!!! The following routine has been fine-tuned for efficiency.
!!!! The following routine has been fine-tuned for efficiency.
!!!! The following routine has been fine-tuned for efficiency.
!!!! Do not change it without rechecking its timing.
!!!! Do not change it without rechecking its timing.
!!!! Do not change it without rechecking its timing.


      function string_upper_compare (string1, string2) result (equal)
      implicit none
      character(len=*),intent(in) :: string1,string2       ! argument
      logical                     :: equal                 ! result
      integer                     :: len1,len2,i,k1,k2     ! local 
      integer                     :: i1,i2,i3,i4,idiff     ! local
      integer                     :: length                ! local

      equal = .false.
      len1 = len(string1)
      len2 = len(string2)
      length = min(len1,len2)
      i1 = ichar('a')
      i2 = ichar('z')
      i3 = ichar('A')
      i4 = ichar('Z')
      idiff = i3 - i1
      do i=1,length
           k1 = ichar(string1(i:i))
           k2 = ichar(string2(i:i))
           if (k1 /= k2) then
                if (k1 >= i1 .and. k1 <= i2) then
                  if (k2 /= k1 + idiff) return
                else if (k1 >= i3 .and. k1 <= i4) then
                  if (k2 /= k1 - idiff) return
                else
                  return
                end if
           end if
      end do
      if (length < len2) then
           if (string2(length+1:) /= ' ') return
      else if (length < len1) then
           if (string1(length+1:) /= ' ') return
      end if
      equal = .true.
      return
      end function string_upper_compare



!!! These various routines have the specified times:
!!!
!!! 100,000 calls to this routine        abf90 (pospx3 linux)
!!! -----------------------------    ----------------------------- 
!!! string_upper_compare              0.02  0.08  0.10  0.33  sec
!!! string_upper_compare_old          1.03  1.02  1.07  1.09  sec
!!! string_upper_compare_alt          1.35  1.35  1.41  1.41  sec
!!! string_upper_compare_alt2         0.13  0.18  0.10  0.17  sec
!!! string_upper_compare_alt4         0.05  0.11  0.12  0.33  sec
!!! string_upper_compare_alt5         0.04  0.22  0.25  1.27  sec
!!!                                    A     B     C     D
!!!
!!! 100,000 calls to this routine         f90 (pospx1 sol)
!!! -----------------------------    ----------------------------- 
!!! string_upper_compare              0.13  0.63  0.75  3.26  sec
!!! string_upper_compare_old          7.70  6.90  7.93  7.52  sec
!!! string_upper_compare_alt         14.43 14.61 15.21 14.50  sec
!!! string_upper_compare_alt2         0.64  1.32  0.50  1.20  sec
!!! string_upper_compare_alt4         0.32  0.88  0.91  3.53  sec
!!! string_upper_compare_alt5         0.20  1.26  1.42  7.16  sec
!!!                                    A     B     C     D
!!!
!!!                                (len=90)              (len=110)
!!! A = early char different    1234asdf5678ghjk  vs  1b34aSDf5678ghjk
!!! B = late char different     1234asdf5678ghjk  vs  1234aSDf5678gh6k
!!! C = chars tacked on         1234asdf5678ghjk  vs  1234aSDf5678ghjkzz
!!! D = same except for case    1234asdf5678ghjk  vs  1234aSDf5678ghjk


!!! The following routines are alternatives to the above one, for testing
!!! only.  They are deliberately not documented to keep them from casual use.



      function string_upper_compare_old (string1, string2) result (equal)
      implicit none
      character(len=*),intent(in) :: string1,string2       ! argument
      logical                     :: equal                 ! result
      character(len=200)          :: buffer1,buffer2       ! local

      call string_to_upper (string1,buffer1)
      call string_to_upper (string2,buffer2)
      equal = (buffer1 == buffer2)
      return
      end function string_upper_compare_old



      function string_upper_compare_alt &
                                    (string1, string2) result (equal)
      implicit none
      character(len=*),intent(in) :: string1,string2       ! argument
      logical                     :: equal                 ! result
      character(len=200)          :: buffer1,buffer2       ! local

      call string_to_upper_alternative (string1,buffer1)
      call string_to_upper_alternative (string2,buffer2)
      equal = (buffer1 == buffer2)
      return
      end function string_upper_compare_alt



      function string_upper_compare_alt2 (string1, string2) result (equal)
      implicit none
      character(len=*),intent(in) :: string1,string2       ! argument
      logical                     :: equal                 ! result
      integer                     :: len1,len2,i,k1,k2     ! local 
      integer                     :: i1,i2,i3,i4,idiff     ! local

      equal = .false.
      len1 = len_trim(string1)
      len2 = len_trim(string2)
      if (len1 /= len2) return
   !!                            len_trim is slower than len unless
   !!                            the strings match up to the minimum
   !!                            of their trimmed lengths.
      i1 = ichar('a')
      i2 = ichar('z')
      i3 = ichar('A')
      i4 = ichar('Z')
      idiff = i3 - i1
      do i=1,len1
           k1 = ichar(string1(i:i))
           k2 = ichar(string2(i:i))
   !       if (k1 == k2) cycle
   !       if (k1 >= i1 .and. k1 <= i2 .and. k2 == k1 + idiff) cycle
   !       if (k1 >= i3 .and. k1 <= i4 .and. k2 == k1 - idiff) cycle
   !
   !!!! The code commented out above and below is slower than the code used.
      !
      !    if (k1 == k2) then
      !         continue
      !    else if (k1 >= i1 .and. k1 <= i2 .and. k2 == k1 + idiff) then
      !         continue
      !    else if (k1 >= i3 .and. k1 <= i4 .and. k2 == k1 - idiff) then
      !         continue
      !    else
      !         return
      !    end if
 
           if (k1 == k2) then
             continue
           else if (k1 >= i1 .and. k1 <= i2) then
             if (k2 /= k1 + idiff) return
           else if (k1 >= i3 .and. k1 <= i4) then
             if (k2 /= k1 - idiff) return
           else
             return
           end if
      end do
      equal = .true.
      return
      end function string_upper_compare_alt2



      function string_upper_compare_alt4 (string1, string2) result (equal)
      implicit none
      character(len=*),intent(in) :: string1,string2       ! argument
      logical                     :: equal                 ! result

      equal = string_nocase_equal (string1,string2)
      return
      end function string_upper_compare_alt4



      function string_upper_compare_alt5 (string1, string2) result (equal)
      implicit none
      character(len=*),intent(in) :: string1,string2       ! argument
      logical                     :: equal                 ! result
      integer                     :: len1,len2,i,k1,k2     ! local 
      integer                     :: length                ! local
      integer                     :: string_crou_toupper   ! external

      equal = .false.
      len1 = len(string1)
      len2 = len(string2)
      length = min(len1,len2)
      do i=1,length
           k1 = string_crou_toupper(ichar(string1(i:i)))
           k2 = string_crou_toupper(ichar(string2(i:i)))
           if (k1 /= k2) return
      end do
      if (length < len2) then
           if (string2(length+1:) /= ' ') return
      else if (length < len1) then
           if (string1(length+1:) /= ' ') return
      end if
      equal = .true.
      return
      end function string_upper_compare_alt5


!!------------- routines from CCB for testing nocase compares --------------!!
!!------------- routines from CCB for testing nocase compares --------------!!
!!------------- routines from CCB for testing nocase compares --------------!!


! The first of these two functions (string_nocase_equal) is used in the above
! test routine string_upper_compare_alt4.  The second of these two functions
! (string_nocase_compare) is not used, but is a viable alternative if the
! user desires to get an integer return (-1, 0, or 1) instead of a logical.


!**************************************************************
! function to check if two character strings disregarding case
!   are equal.
! Return true if so and false if not
!
! This attempts to use the ASCII order to be efficient,
!   but uses slighly complicated logic
!
! Written August 2000 by Charles C Burch
!***************************************************************

logical function string_nocase_equal (str1, str2)
  implicit none

  character (len=*), intent(in)    :: str1, str2

  integer      :: len_str1, len_str2, i, ichar1, ichar2, n
  integer      :: ichar_a, ichar_z, i_adj

  len_str1=len(str1)
  len_str2=len(str2)
  string_nocase_equal=.false.        !assume we will get a false

  if(len_str2.lt.len_str1) then
! - len(str2) < len(str1) -- see if str1 past len(str2) are blank
    if(str1(len_str2+1:len_str1).ne." ") return
    n=len_str2

  else if(len_str1.lt.len_str2) then
! - len(str1) < len(str2) -- see if str2 past len(str1) are blank
    if(str2(len_str1+1:len_str2).ne." ") return
    n=len_str1

  else
    n=len_str1
  endif

! Now we see if chars in common string lengths are equal

  ichar_a=ichar('a')
  ichar_z=ichar('z')
  i_adj=ichar_a-ichar('A')   !relies on way ASCII characters are defined

  do i=1,n
    ichar1=ichar(str1(i:i))
    ichar2=ichar(str2(i:i))

    if(ichar1.ne.ichar2) then

! --- we are here only if chars are different
! --- if they are same case and equal, we will not be here

      if(ichar1.lt.ichar_a) then

! ----- we know char1 is not lower case
       if(ichar2.lt.ichar_a)             &
        return   !char1 and char2 both not lower case and ne

! ----- char2 might be lower case--make upper case if needed
       if(ichar2.le.ichar_z) ichar2=ichar2-i_adj !make upper if needed

! ----- both char1 and char 2 now not lower case
       if(ichar1.ne.ichar2) return

! ----- at this point char1 and char2 are equal disregarding case

      else

! ----- we know char1 is (>=ichar_a) so not in upper case of ASCII table

       if(ichar2.ge.ichar_a)             &
        return   !char1 and char2 both not upper case and ne

! ----- char2 not lower case -- char1 might be-make char1 upper if needed
       if(ichar1.le.ichar_z) ichar1=ichar1-i_adj

! ----- both char1 and char2 now not lower case
       if(ichar1.ne.ichar2) return

! ----- at this point char1 and char2 are equal disregarding case

      endif
    endif

  enddo      !check next character

  string_nocase_equal=.true.  !all characters(in nocase) are equal atthis point
  return
end function string_nocase_equal




!**************************************************************
! function to compare if two character strings disregarding case
!   are equal.
!
! Return true -1 if str1<str2, 0 if str1=str2 and 1 if str1>-str2
!
! Strings of nonequal length are assumed to be padded with blanks
!
! This attempts to use the ASCII order to be efficient,
!   but uses slighly complicated logic
!
! Written August 2000 by Charles C Burch
!***************************************************************

integer function string_nocase_compare (str1, str2)
  implicit none

  character (len=*), intent(in)    :: str1, str2

  integer      :: len_str1, len_str2, i, ichar1, ichar2, n
  integer      :: ichar_a, ichar_z, i_blank, i_adj

  len_str1=len(str1)
  len_str2=len(str2)
  n=max(len_str1,len_str2)

  i_blank=ichar(' ')
  ichar_a=ichar('a')
  ichar_z=ichar('z')
  i_adj=ichar_a-ichar('A')   !relies on way ASCII characters are defined

  do i=1,n
    if(i.le.len_str1) then
      ichar1=ichar(str1(i:i))
    else
      ichar1=i_blank
    endif

    if(i.le.len_str2) then
      ichar2=ichar(str2(i:i))
    else
      ichar2=i_blank
    endif

    if(ichar1.ne.ichar2) then

! --- we are here only if chars are different
! --- if they are same case and equal, we will not be here

      if(ichar1.lt.ichar_a) then

! ----- we know char1 is not lower case
       if(ichar2.lt.ichar_a) then
! ------- char1 and char2 both not lower case and ne
         string_nocase_compare=1
         if(ichar1.lt.ichar2) string_nocase_compare=-1
         return
       endif

! ----- char2 might be lower case--make upper case if needed
       if(ichar2.le.ichar_z) ichar2=ichar2-i_adj !make upper if needed

! ----- both char1 and char 2 now not lower case
       if(ichar1.ne.ichar2) then
         string_nocase_compare=1
         if(ichar1.lt.ichar2) string_nocase_compare=-1
         return
       endif

! ----- at this point char1 and char2 are equal disregarding case

      else

! ----- we know char1 is (>=ichar_a) so not in upper case of ASCII table

       if(ichar2.ge.ichar_a) then
! ------- char1 and char2 both not upper case and ne
         string_nocase_compare=1
         if(ichar1.lt.ichar2) string_nocase_compare=-1
         return
       endif

! ----- char2 not lower case -- char1 might be-make char1 upper if needed
       if(ichar1.le.ichar_z) ichar1=ichar1-i_adj

! ----- both char1 and char2 now not lower case
       if(ichar1.ne.ichar2) then
         string_nocase_compare=1
         if(ichar1.lt.ichar2) string_nocase_compare=-1
         return
       endif

! ----- at this point char1 and char2 are equal disregarding case

      endif
    endif

  enddo      !check next characters

  string_nocase_compare=0
  return
end function string_nocase_compare


!!------------------------ string replace zeroes --------------------------!!
!!------------------------ string replace zeroes --------------------------!!
!!------------------------ string replace zeroes --------------------------!!


      subroutine string_replace_zeroes (string)
      implicit none
      character(len=*),intent(inout) :: string                ! argument
      integer                        :: i                     ! local

      i = scan(string, char(0))
      if (i > 0) string(i:) = ' '
      return
      end subroutine string_replace_zeroes


!!------------------------- string validate -------------------------------!!
!!------------------------- string validate -------------------------------!!
!!------------------------- string validate -------------------------------!!


      subroutine string_validate (string)
      implicit none
      character(len=*),intent(inout) :: string                ! argument


      call string_replace_zeroes    (string)
      call string_strip_blanks      (string)
      call string_strip_unprintable (string)

      if (string == ' ') then
           string = STRING_EMPTY
      else if (string_upper_compare(string,STRING_EMPTY)) then
           string = STRING_EMPTY
      end if
      return
      end subroutine string_validate


!!------------------------- string 2 upper and lower ---------------------!!
!!------------------------- string 2 upper and lower ---------------------!!
!!------------------------- string 2 upper and lower ---------------------!!


      function string_2_upper (string1) result (string2)
      implicit none
      character(len=*),intent(in) :: string1                    ! argument
      character(len=222)          :: string2                    ! result

      call string_to_upper (string1,string2)
      return
      end function string_2_upper



      function string_2_lower (string1) result (string2)
      implicit none
      character(len=*),intent(in) :: string1                    ! argument
      character(len=222)          :: string2                    ! result

      call string_to_lower (string1,string2)
      return
      end function string_2_lower


!!------------------------- string to upper -------------------------------!!
!!------------------------- string to upper -------------------------------!!
!!------------------------- string to upper -------------------------------!!


      subroutine string_to_upper1 (string)
      implicit none
      character(len=*),intent(inout) :: string                     ! argument
      integer                        :: length,i,i1,i2,i3,idiff,k  ! local

! integer,save :: kount = 0
! kount = kount + 1
! print *, 'string_to_upper ',kount,' ',trim(string)

      i1     = ichar('a')
      i2     = ichar('z')
      i3     = ichar('A')
      idiff  = i3 - i1
      length = len_trim(string)     ! faster than len(string)
      do i=1,length
           k = ichar(string(i:i))
           if (k >= i1 .and. k <= i2) string(i:i) = char(k+idiff)
      end do
      return
      end subroutine string_to_upper1



      subroutine string_to_upper2 (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                    ! argument
      character(len=*),intent(out)   :: string2                    ! argument

      string2 = string1
      call string_to_upper1 (string2)
      return
      end subroutine string_to_upper2



!!! These various routines have the specified times when converting
!!! string (len=100) 1234aSDf5678ghjk to upper case:
!!!
!!! 100,000 calls to this routine   f90 (pospx1 sol)   abf90 (pospx3 linux)
!!! -----------------------------   ----------------   -------------------- 
!!! string_to_upper1                    2.66 sec             0.36 sec
!!! string_to_upper1_alternative        7.09 sec             0.70 sec
!!! string_to_upper1_another            5.91 sec             0.52 sec
!!!
!!! 100,000 calls to this routine   f90 (pospx1 sol)   abf90 (pospx3 linux)
!!! -----------------------------   ----------------   -------------------- 
!!! string_to_upper2                    2.90 sec             0.41 sec
!!! string_to_upper2_alternative        6.95 sec             0.59 sec
!!! string_to_upper2_another            5.93 sec             0.46 sec
!!! string_to_upper2_again              5.73 sec             0.47 sec


!!! The following routines are alternatives to the above one, for testing
!!! only.  They are deliberately not documented to keep them from casual use.



      subroutine string_to_upper1_alternative (string)
      implicit none
      character(len=*),intent(inout) :: string                  ! argument
      integer                        :: holler(100)             ! local

      call string_cc2hh         (string,holler)
      call string_crou_to_upper (holler)
      call string_hh2cc         (holler,string)
      return
      end subroutine string_to_upper1_alternative



      subroutine string_to_upper2_alternative (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                 ! argument
      character(len=*),intent(out)   :: string2                 ! argument
      integer                        :: holler(100)             ! local

      call string_cc2hh         (string1,holler)
      call string_crou_to_upper (holler)
      call string_hh2cc         (holler,string2)
      return
      end subroutine string_to_upper2_alternative



      subroutine string_to_upper1_another (string)
      implicit none
      character(len=*),intent(inout) :: string                     ! argument
      integer                        :: length,i                   ! local
      integer                        :: string_crou_toupper        ! external

      length = len_trim(string)     ! faster than len(string)
      do i=1,length
           string(i:i) = char(string_crou_toupper(ichar(string(i:i))))
      end do
      return
      end subroutine string_to_upper1_another



      subroutine string_to_upper2_another (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                    ! argument
      character(len=*),intent(out)   :: string2                    ! argument
      integer                        :: length,i                   ! local
      integer                        :: string_crou_toupper        ! external

      string2 = ' '
      length = len_trim(string1)     ! faster than len(string1)
      do i=1,length
           string2(i:i) = char(string_crou_toupper(ichar(string1(i:i))))
      end do
      return
      end subroutine string_to_upper2_another



      subroutine string_to_upper2_again (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                    ! argument
      character(len=*),intent(out)   :: string2                    ! argument
      integer                        :: length,i,i1,i2,i3,idiff,k  ! local

      string2 = ' '
      i1     = ichar('a')
      i2     = ichar('z')
      i3     = ichar('A')
      idiff  = i3 - i1
      length = len_trim(string1)     ! faster than len(string1)
      do i=1,length
           k = ichar(string1(i:i))
           if (k >= i1 .and. k <= i2) then
                string2(i:i) = char(k+idiff)
           else
                string2(i:i) = char(k)
           end if
      end do
      return
      end subroutine string_to_upper2_again


!!------------------------- string to lower -------------------------------!!
!!------------------------- string to lower -------------------------------!!
!!------------------------- string to lower -------------------------------!!


      subroutine string_to_lower1 (string)
      implicit none
      character(len=*),intent(inout) :: string                     ! argument
      integer                        :: length,i,i1,i3,i4,idiff,k  ! local

      i1     = ichar('a')
      i3     = ichar('A')
      i4     = ichar('Z')
      idiff  = i3 - i1
      length = len_trim(string)     ! faster than len(string)
      do i=1,length
           k = ichar(string(i:i))
           if (k >= i3 .and. k <= i4) string(i:i) = char(k-idiff)
      end do
      return
      end subroutine string_to_lower1



      subroutine string_to_lower2 (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                    ! argument
      character(len=*),intent(out)   :: string2                    ! argument
      integer                        :: length,i,i1,i3,i4,idiff,k  ! local

  !   string2 = string1                     ! this code is slower.
  !   call string_to_lower1 (string2)       ! this code is slower.

      string2 = ' '
      i1     = ichar('a')
      i3     = ichar('A')
      i4     = ichar('Z')
      idiff  = i3 - i1
      length = min(len_trim(string1),len(string2))  ! faster than len(string1)
      do i=1,length
           k = ichar(string1(i:i))
           if (k >= i3 .and. k <= i4) then
                string2(i:i) = char(k-idiff)
           else
                string2(i:i) = char(k)
           end if
      end do
      return
      end subroutine string_to_lower2



!!! The following alternative code appears to be slightly faster with the
!!! absoft compiler, but twice as slow with the solaris compiler.
!!! The alternative code is for testing only, and is deliberately not
!!! documented to keep it from casual use.



      subroutine string_to_lower1_alternative (string)
      implicit none
      character(len=*),intent(inout) :: string                  ! argument
      integer                        :: holler(100)             ! local

      call string_cc2hh         (string,holler)
      call string_crou_to_lower (holler)
      call string_hh2cc         (holler,string)
      return
      end subroutine string_to_lower1_alternative



      subroutine string_to_lower2_alternative (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                 ! argument
      character(len=*),intent(out)   :: string2                 ! argument
      integer                        :: holler(100)             ! local

      call string_cc2hh         (string1,holler)
      call string_crou_to_lower (holler)
      call string_hh2cc         (holler,string2)
      return
      end subroutine string_to_lower2_alternative



      subroutine string_to_lower1_another (string)
      implicit none
      character(len=*),intent(inout) :: string                     ! argument
      integer                        :: length,i                   ! local
      integer                        :: string_crou_tolower        ! external

      length = len_trim(string)     ! faster than len(string)
      do i=1,length
           string(i:i) = char(string_crou_tolower(ichar(string(i:i))))
      end do
      return
      end subroutine string_to_lower1_another



      subroutine string_to_lower2_another (string1,string2)
      implicit none
      character(len=*),intent(in)    :: string1                    ! argument
      character(len=*),intent(out)   :: string2                    ! argument
      integer                        :: length,i                   ! local
      integer                        :: string_crou_tolower        ! external

      string2 = ' '
      length = len_trim(string1)     ! faster than len(string1)
      do i=1,length
           string2(i:i) = char(string_crou_tolower(ichar(string1(i:i))))
      end do
      return
      end subroutine string_to_lower2_another


!!--------------------- string strip unprintable --------------------------!!
!!--------------------- string strip unprintable --------------------------!!
!!--------------------- string strip unprintable --------------------------!!


      subroutine string_strip_unprintable (string)
      implicit none
      character(len=*),intent(inout) :: string                  ! argument
      integer                        :: length,i,j              ! local

      length = len_trim(string)
      j = 0
      do i=1,length
           if (string_is_print(string(i:i))) then
               j = j + 1
               string(j:j) = string(i:i)
           end if
      end do
      string(j+1:) = ' '
      return
      end subroutine string_strip_unprintable


!!--------------------- string private squeeze ----------------------------!!
!!--------------------- string private squeeze ----------------------------!!
!!--------------------- string private squeeze ----------------------------!!

! RETAIN should be set to true to retain blanks contained within quotes.
! STRING1 and STRING2 can have the same address.


      subroutine string_private_squeeze (retain, string1, string2, nsize)
      implicit none
      logical         ,intent(in)           :: retain             ! argument
      character(len=*),intent(in)           :: string1            ! argument
      character(len=*),intent(out)          :: string2            ! argument
      integer         ,intent(out),optional :: nsize              ! argument
      character(len=1)                      :: single             ! local
      character(len=1),parameter            :: quote = '"'        ! local
      character(len=1),parameter            :: zero  = char(0)    ! local
      integer                               :: length1,length2    ! local
      integer                               :: i,j                ! local
      logical                               :: quoting            ! local

      length1 = len(string1)
      length2 = len(string2)
      if (length1 == 0 .or. length2 == 0) return
      j = 0
      quoting = .false.     ! true when inside quote marks.
      do i=1,length1
           single = string1(i:i)
           if (single == zero) exit
           if (retain .and. single == quote) quoting = .not.quoting
           if (single /= ' ' .or. quoting) then
                j = j + 1
                string2(j:j) = single
                if (j == length2) exit
           end if
      end do
      string2(j+1:) = ' '
      if (present(nsize)) nsize = j
      return
      end subroutine string_private_squeeze


!!--------------------- string squeeze blanks -----------------------------!!
!!--------------------- string squeeze blanks -----------------------------!!
!!--------------------- string squeeze blanks -----------------------------!!


      subroutine string_squeeze_blanks2 (string1, string2, nsize)
      implicit none
      character(len=*),intent(in)           :: string1            ! argument
      character(len=*),intent(out)          :: string2            ! argument
      integer         ,intent(out),optional :: nsize              ! argument

      call string_private_squeeze (.false., string1, string2, nsize)
      return
      end subroutine string_squeeze_blanks2



      subroutine string_squeeze_blanks1 (string, nsize)
      implicit none
      character(len=*),intent(inout)        :: string             ! argument
      integer         ,intent(out),optional :: nsize              ! argument

      call string_private_squeeze (.false., string, string, nsize)
      return
      end subroutine string_squeeze_blanks1


!!--------------------- string strip blanks -----------------------------!!
!!--------------------- string strip blanks -----------------------------!!
!!--------------------- string strip blanks -----------------------------!!


      subroutine string_strip_blanks2 (string1, string2, nsize)
      implicit none
      character(len=*),intent(in)           :: string1            ! argument
      character(len=*),intent(out)          :: string2            ! argument
      integer         ,intent(out),optional :: nsize              ! argument

      call string_private_squeeze (.true., string1, string2, nsize)
      return
      end subroutine string_strip_blanks2



      subroutine string_strip_blanks1 (string, nsize)
      implicit none
      character(len=*),intent(inout)        :: string             ! argument
      integer         ,intent(out),optional :: nsize              ! argument

      call string_private_squeeze (.true., string, string, nsize)
      return
      end subroutine string_strip_blanks1


!!--------------------- string compress blanks -----------------------------!!
!!--------------------- string compress blanks -----------------------------!!
!!--------------------- string compress blanks -----------------------------!!

! This works even if string1 and string2 are the same address.


      subroutine string_compress_blanks2 (string1, string2, nsize)
      implicit none
      character(len=*),intent(in)           :: string1            ! argument
      character(len=*),intent(out)          :: string2            ! argument
      integer         ,intent(out),optional :: nsize              ! argument
      integer                               :: i,j,length         ! argument

      length = len_trim(string1)
      string2(1:1) = string1(1:1)
      j = 1
      if (string2(1:1) == ' ') j = 0
      do i = 2,length
           if (string1(i:i) == ' ' .and. string1(i-1:i-1) == ' ') cycle
           j = j + 1
           string2(j:j) = string1(i:i)
      end do
      string2(j+1:) = ' '
      if(present(nsize)) nsize = len_trim(string2)
      return
      end subroutine string_compress_blanks2



      subroutine string_compress_blanks1 (string, nsize)
      implicit none
      character(len=*),intent(inout)        :: string             ! argument
      integer         ,intent(out),optional :: nsize              ! argument

      call string_compress_blanks2 (string, string, nsize)
      return
      end subroutine string_compress_blanks1


!!------------------- convert number to character string ------------------!!
!!------------------- convert number to character string ------------------!!
!!------------------- convert number to character string ------------------!!


      subroutine string_ii2cc (ivar,cvar,nchar)
      implicit none
      integer         ,intent(in)          :: ivar            ! argument
      character(len=*),intent(out)         :: cvar            ! argument
      integer         ,intent(in),optional :: nchar           ! argument
      integer                              :: hvar(20)        ! local
      integer                              :: nchar2          ! local

      IF (ivar == 0) then
           cvar='0'                        ! quick interception of zero.
      else IF (ivar == INIL) then
           cvar=' '
      else
           nchar2 = 50
           if (present(nchar)) nchar2 = nchar
           if (nchar2 <= 0) nchar2 = 50
           nchar2 = min(nchar2,len(cvar))
           call string_crou_ii2hh_simple (ivar, hvar, nchar2)
           call string_hh2cc             (hvar, cvar)
      end if
      return
      end subroutine string_ii2cc



      subroutine string_ff2cc (fvar,cvar,nchar,ndec)
      implicit none
      real            ,intent(in)          :: fvar            ! argument
      character(len=*),intent(out)         :: cvar            ! argument
      integer         ,intent(in),optional :: nchar           ! argument
      integer         ,intent(in),optional :: ndec            ! argument
      integer                              :: hvar(20)        ! local
      integer                              :: nchar2,ndec2    ! local

      IF (fvar == 0) then
           cvar='0'                        ! quick interception of zero.
      else IF (fvar == FNIL) then
           cvar=' '
      else
           nchar2 = 50
           ndec2  = 50
           if (present(nchar)) nchar2 = nchar
           if (present(ndec )) ndec2  = ndec
           if (nchar2 <= 0) nchar2 = 50
           nchar2 = min(nchar2,len(cvar))
           call string_crou_ff2hh_simple (fvar, hvar, nchar2, ndec2)
           call string_hh2cc             (hvar, cvar)
      end if
      return
      end subroutine string_ff2cc



      subroutine string_dd2cc (dvar,cvar,nchar,ndec)
      implicit none
      double precision,intent(in)          :: dvar            ! argument
      character(len=*),intent(out)         :: cvar            ! argument
      integer         ,intent(in),optional :: nchar           ! argument
      integer         ,intent(in),optional :: ndec            ! argument
      integer                              :: hvar(20)        ! local
      integer                              :: nchar2,ndec2    ! local

      IF (dvar == 0.0) then
           cvar='0'                        ! quick interception of zero.
      else IF (dvar == DNIL) then
           cvar=' '
      else
           nchar2 = 50
           ndec2  = 50
           if (present(nchar)) nchar2 = nchar
           if (present(ndec )) ndec2  = ndec
           if (nchar2 <= 0) nchar2 = 50
           nchar2 = min(nchar2,len(cvar))
           call string_crou_dd2hh_simple (dvar, hvar, nchar2, ndec2)
           call string_hh2cc             (hvar, cvar)
      end if
      return
      end subroutine string_dd2cc



      subroutine string_ll2cc (lvar,cvar)
      implicit none
      logical         ,intent(in)          :: lvar            ! argument
      character(len=*),intent(out)         :: cvar            ! argument

      IF (lvar) then
           cvar='YES'
      else
           cvar='NO'
      end if
      return
      end subroutine string_ll2cc


                           !!!!!!!!!!!!!!!!!!!!!!!


      function string_ii2ss (ivar,nchar) result (cvar)
      implicit none
      integer         ,intent(in)          :: ivar            ! argument
      integer         ,intent(in),optional :: nchar           ! argument
      character(len=80)                    :: cvar            ! result

      call string_ii2cc (ivar,cvar,nchar)
      return
      end function string_ii2ss



      function string_ff2ss (fvar,nchar,ndec) result (cvar)
      implicit none
      real            ,intent(in)          :: fvar            ! argument
      integer         ,intent(in),optional :: nchar           ! argument
      integer         ,intent(in),optional :: ndec            ! argument
      character(len=80)                    :: cvar            ! result

      call string_ff2cc (fvar,cvar,nchar,ndec)
      return
      end function string_ff2ss



      function string_dd2ss (dvar,nchar,ndec) result (cvar)
      implicit none
      double precision,intent(in)          :: dvar            ! argument
      integer         ,intent(in),optional :: nchar           ! argument
      integer         ,intent(in),optional :: ndec            ! argument
      character(len=80)                    :: cvar            ! result

      call string_dd2cc (dvar,cvar,nchar,ndec)
      return
      end function string_dd2ss



      function string_ll2ss (lvar) result (cvar)
      implicit none
      logical         ,intent(in)          :: lvar            ! argument
      character(len=80)                    :: cvar            ! result

      call string_ll2cc (lvar,cvar)
      return
      end function string_ll2ss


!!---------------- convert character string to number ---------------------!!
!!---------------- convert character string to number ---------------------!!
!!---------------- convert character string to number ---------------------!!


      subroutine string_cc2ii (cvar,ivar,istat,errmsg)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      integer         ,intent(out)          :: ivar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      integer                               :: error          ! local

      if (present(errmsg)) errmsg = ' '
      IF (cvar == '0') then
           ivar  = 0                          ! quick interception of zero.
           if (present(istat)) istat = 1
      else if (cvar == ' ') then
           ivar  = INIL
           if (present(istat)) istat = 0
      else
           read (cvar,*,iostat = error) ivar
           if (error == 0) then
                if (present(istat)) istat = 1
           else
                ivar  = INIL
                if (present(istat)) istat = -1
                if (present(errmsg)) then
                     errmsg = 'error decoding '//trim(cvar)//' to integer'
                end if
           end if
      end if
      return
      end subroutine string_cc2ii



      subroutine string_cc2ff (cvar,fvar,istat,errmsg)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      real            ,intent(out)          :: fvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      integer                               :: error          ! local

      if (present(errmsg)) errmsg = ' '
      IF (cvar == '0') then
           fvar = 0.0                        ! quick interception of zero.
           if (present(istat)) istat= 1
      else IF (cvar == ' ') then
           fvar  = FNIL
           if (present(istat)) istat = 0
      else
           read (cvar,*,iostat = error) fvar
           if (error == 0) then
                if (present(istat)) istat = 1
           else
                fvar  = FNIL
                if (present(istat)) istat = -1
                if (present(errmsg)) then
                     errmsg = 'error decoding '//trim(cvar)//' to real'
                end if
           end if
      end if
      return
      end subroutine string_cc2ff



      subroutine string_cc2dd (cvar,dvar,istat,errmsg)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      double precision,intent(out)          :: dvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      integer                               :: error          ! local

      if (present(errmsg)) errmsg = ' '
      IF (cvar == '0') then
           dvar  = 0.0                       ! quick interception of zero.
           if (present(istat)) istat = 1
      else IF (cvar == ' ') then
           dvar  = DNIL
           if (present(istat)) istat = 0
      else
           read (cvar,*,iostat = error) dvar
           if (error == 0) then
                if (present(istat)) istat = 1
           else
                dvar  = DNIL
                if (present(istat)) istat = -1
                if (present(errmsg)) then
                     errmsg = 'error decoding '//trim(cvar)//' to double'
                end if
           end if
      end if
      return
      end subroutine string_cc2dd



      subroutine string_cc2ll (cvar,lvar,istat,errmsg)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      logical         ,intent(out)          :: lvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      integer                               :: ifirst         ! local
      character(len=1)                      :: charr1         ! local
      character(len=2)                      :: charr2         ! local

      if (present(errmsg)) errmsg = ' '
      ifirst = verify(cvar,' ')        ! scan for first non-blank charactor.
      if (ifirst == 0) then
           lvar = LNIL
           if (present(istat)) istat = 0
           return
      end if
      charr1 = cvar(ifirst:ifirst)
      if (ifirst < len(cvar)) then
           charr2 = cvar(ifirst:ifirst+1)
      else
           charr2 = cvar(ifirst:ifirst)//' '
      end if
      if      (charr1 ==  'Y' .or. charr1 ==  'y' .or.  &
               charr1 ==  'T' .or. charr1 ==  't' .or.  &
               charr2 == '.T' .or. charr2 == '.t') then
           lvar = .TRUE.
           if (present(istat)) istat = 1
      else if (charr1 ==  'N' .or. charr1 ==  'n' .or.  &
               charr1 ==  'F' .or. charr1 ==  'f' .or.  &
               charr2 == '.F' .or. charr2 == '.f') then
           lvar = .FALSE.
           if (present(istat)) istat = 1
      else
           lvar = LNIL
           if (present(istat)) istat = -1
           if (present(errmsg)) then
                errmsg = 'error decoding '//trim(cvar)//' to logical'
           end if
      end if
      return
      end subroutine string_cc2ll


                           !!!!!!!!!!!!!!!!!!!!!!!

      function string_ss2ii (cvar,istat,errmsg) result (ivar)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      integer                               :: ivar           ! result

      call string_cc2ii (cvar,ivar,istat,errmsg)
      return
      end function string_ss2ii



      function string_ss2ff (cvar,istat,errmsg) result (fvar)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      real                                  :: fvar           ! result

      call string_cc2ff (cvar,fvar,istat,errmsg)
      return
      end function string_ss2ff



      function string_ss2dd (cvar,istat,errmsg) result (dvar)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      double precision                      :: dvar           ! result

      call string_cc2dd (cvar,dvar,istat,errmsg)
      return
      end function string_ss2dd



      function string_ss2ll (cvar,istat,errmsg) result (lvar)
      implicit none
      character(len=*),intent(in)           :: cvar           ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: errmsg         ! argument
      logical                               :: lvar           ! result

      call string_cc2ll (cvar,lvar,istat,errmsg)
      return
      end function string_ss2ll


!!-------------------------- string cc2hh --------------------------------!!
!!-------------------------- string cc2hh --------------------------------!!
!!-------------------------- string cc2hh --------------------------------!!

! Copy character variable CVAR to hollerith buffer HVAR(*).
! HVAR(*) will be terminated with null after the last non-blank character.
! HVAR(*) must be large enough to store the characters from CVAR plus null.
! If CVAR is blank, the first character of HVAR(*) will be null.
! CVAR should not contain any nulls.

! HVAR is passed as HVAR(*) rather than HVAR(:) because the original
! hollerith array might have been passed from C without any length
! information.


      subroutine string_cc2hh (cvar, hvar)
      implicit none
      character(len=*),intent(in)  :: cvar                   ! argument
      integer         ,intent(out) :: hvar(*)                ! argument
      integer                      :: length,i,j             ! local

      length = len_trim(cvar)
      do i = 1,length
           j = ichar(cvar(i:i))
           call string_crou_char_into_buffer (j,i,hvar)
      end do
      call string_crou_char_into_buffer (0,length+1,hvar)
      return
      end subroutine string_cc2hh


!!! The following version appeared to work in a test, but has not
!!! been checked out fully.  It is not any faster than the above
!!! version, which is well-tested and extensively used.

!!! The following version also causes an internal compiler error with pgf90
!!! (both linux and intelsol), and is therefore commented out.


  !   subroutine string_cc2hh_notused (cvar, hvar)
  !   implicit none
  !   character(len=*),intent(in)  :: cvar                   ! argument
  !   integer         ,intent(out) :: hvar(*)                ! argument
  !   integer                      :: length,nwords          ! local

  !   length = len_trim(cvar)
  !   nwords = 1 + length / sizeof(hvar(1))
  !   hvar(1:nwords) = transfer (cvar(1:length)//char(0), hvar(1), nwords)
  !   return
  !   end subroutine string_cc2hh_notused


!!-------------------------- string cc2hh alloc --------------------------!!
!!-------------------------- string cc2hh alloc --------------------------!!
!!-------------------------- string cc2hh alloc --------------------------!!


      subroutine string_cc2hh_alloc (cvar, hvar)
      implicit none
      character(len=*),intent(in)  :: cvar                   ! argument
      integer         ,pointer     :: hvar(:)                ! argument
      integer                      :: nwords                 ! local

      if (associated(hvar)) deallocate (hvar)
      nullify(hvar)
      nwords = string_num_trimmed_integers(cvar)
      allocate (hvar(nwords))
      call string_cc2hh (cvar, hvar)
      return
      end subroutine string_cc2hh_alloc


!!-------------------------- string hh2cc --------------------------------!!
!!-------------------------- string hh2cc --------------------------------!!
!!-------------------------- string hh2cc --------------------------------!!

! copy hollerith buffer HVAR(*) to character variable CVAR.
! CVAR will be blank filled (no null).
! HVAR(*) should be terminated with null.
! only characters preceding the first NULL in HVAR(*) will be copied to CVAR.
! no more than len(cvar) characters will be copied to CVAR.

! HVAR is passed as HVAR(*) rather than HVAR(:) because the original
! hollerith array might have been passed from C without any length
! information.


      subroutine string_hh2cc (hvar, cvar)
      implicit none
      integer         ,intent(in)  :: hvar(*)             ! argument
      character(len=*),intent(out) :: cvar                ! argument
      integer                      :: length,i,j          ! local

      length = len(cvar)
      cvar   = ' '
      do i = 1,length
           call string_crou_char_from_buffer (hvar,i,j)
           if (j == 0) return
           cvar(i:i) = char(j)
      end do
      return
      end subroutine string_hh2cc


!!! The following version appeared to work in a test, but has not
!!! been checked out fully.  It is not any faster than the above
!!! version, which is well-tested and extensively used.  It is
!!! deliberately not documented to keep it from casual use.


      subroutine string_hh2cc_notused (hvar, cvar)
      implicit none
      integer         ,intent(in)  :: hvar(*)             ! argument
      character(len=*),intent(out) :: cvar                ! argument
      integer                      :: length,nwords       ! local

      length = len(cvar)
      nwords = 1 + length / sizeof(hvar(1))
      cvar(1:length) = transfer (hvar(1:nwords), cvar(1:length))
      return
      end subroutine string_hh2cc_notused


!!------------------------- string cc2hh array -----------------------------!!
!!------------------------- string cc2hh array -----------------------------!!
!!------------------------- string cc2hh array -----------------------------!!


      subroutine string_cc2hh_array (carray,harray,  nwords,nelements)
      implicit none
      integer         ,intent(in)  :: nwords,nelements          ! argument
      character(len=*),intent(in)  :: carray(nelements)         ! argument
      integer         ,intent(out) :: harray(nwords,nelements)  ! argument
      integer                      :: indx                      ! local

      do indx = 1,nelements
           call string_cc2hh (carray(indx), harray(1:,indx))
      end do
      return
      end subroutine string_cc2hh_array


!!------------------------- string hh2cc array -----------------------------!!
!!------------------------- string hh2cc array -----------------------------!!
!!------------------------- string hh2cc array -----------------------------!!


      subroutine string_hh2cc_array (harray,carray,  nwords,nelements)
      implicit none
      integer         ,intent(in)  :: nwords,nelements          ! argument
      integer         ,intent(in)  :: harray(nwords,nelements)  ! argument
      character(len=*),intent(out) :: carray(nelements)         ! argument
      integer                      :: indx                      ! local

      do indx = 1,nelements
           carray(indx) = ' '
           call string_hh2cc (harray(1:,indx), carray(indx))
      end do
      return
      end subroutine string_hh2cc_array


!!-------------------------- string cc2hh exact -------------------------!!
!!-------------------------- string cc2hh exact -------------------------!!
!!-------------------------- string cc2hh exact -------------------------!!

! Copy character variable CVAR to hollerith buffer HVAR(*).
! HVAR(*) will be terminated with null after exactly NCHAR characters.
! HVAR(*) must be large enough to store the characters from CVAR plus null.
! If CVAR is blank, all NCHAR characters of HVAR(*) will be blank.
! CVAR should not contain any nulls.

! HVAR is passed as HVAR(*) rather than HVAR(:) because the original
! hollerith array might have been passed from C without any length
! information.


      subroutine string_cc2hh_exact (cvar,hvar,nchar)
      implicit none
      character(len=*),intent(in)  :: cvar                   ! argument
      integer         ,intent(in)  :: nchar                  ! argument
      integer         ,intent(out) :: hvar(*)                ! argument
      integer                      :: length,i,j             ! local

      length = len_trim(cvar)
      do i = 1,nchar
           if (i <= length) then
                j = ichar(cvar(i:i))
           else
                j = ichar(' ')
           end if
           call string_crou_char_into_buffer (j,i,hvar)
      end do
      call string_crou_char_into_buffer (0,nchar+1,hvar)
      return
      end subroutine string_cc2hh_exact


!!-------------------------- string hh2cc exact -------------------------!!
!!-------------------------- string hh2cc exact -------------------------!!
!!-------------------------- string hh2cc exact -------------------------!!

! copy hollerith buffer HVAR(*) to character variable CVAR.
! CVAR will be blank filled (no null).
! HVAR(*) need not be terminated with null.
! only characters preceding the first NULL in HVAR(*) will be copied to CVAR.
! no more than len(cvar) characters will be copied to CVAR.
! no more than   NCHAR   characters will be copied to CVAR.

! HVAR is passed as HVAR(*) rather than HVAR(:) because the original
! hollerith array might have been passed from C without any length
! information.


      subroutine string_hh2cc_exact (hvar,cvar,nchar)
      implicit none
      integer         ,intent(in)  :: hvar(*)                ! argument
      integer         ,intent(in)  :: nchar                  ! argument
      character(len=*),intent(out) :: cvar                   ! argument
      integer                      :: length,i,j             ! local

      length = min(len(cvar),nchar)
      cvar   = ' '
      do i = 1,length
           call string_crou_char_from_buffer (hvar,i,j)
           if (j == 0) return
           cvar(i:i) = char(j)
      end do
      return
      end subroutine string_hh2cc_exact


!!--------------------- string chars per integer --------------------------!!
!!--------------------- string chars per integer --------------------------!!
!!--------------------- string chars per integer --------------------------!!


      function string_chars_per_integer () result (nchars)
      implicit none
      integer :: nchars                            ! result
      integer :: string_crou_chars_per_integer     ! external

      nchars = string_crou_chars_per_integer()
      end function string_chars_per_integer


!!--------------------------- string num integers --------------------------!!
!!--------------------------- string num integers --------------------------!!
!!--------------------------- string num integers --------------------------!!


      function string_num_integers1 (num_chars) result (num_integers)
      implicit none
      integer,intent(in) :: num_chars              ! arguments
      integer            :: num_integers           ! result

      num_integers = 1 + num_chars / string_chars_per_integer()
      end function string_num_integers1



      function string_num_integers2 (cvar) result (num_integers)
      implicit none
      character(len=*),intent(in) :: cvar              ! arguments
      integer                     :: num_integers      ! result
      integer                     :: temp
      temp = len(cvar)
      num_integers = string_num_integers (temp)
      end function string_num_integers2



      function string_num_trimmed_integers (cvar) result (num_integers)
      implicit none
      character(len=*),intent(in) :: cvar              ! arguments
      integer                     :: num_integers      ! result
      integer                     :: temp
      temp = len_trim(cvar)

      num_integers = string_num_integers (temp)
      end function string_num_trimmed_integers


!!------------------------- string encode ---------------------------------!!
!!------------------------- string encode ---------------------------------!!
!!------------------------- string encode ---------------------------------!!


      subroutine string_encode_cici (buffer, msg1,var1,msg2,var2,msg3)
      implicit none
      character(len=*),intent(out)         :: buffer     ! arguments.
      character(len=*),intent(in)          :: msg1       ! arguments.
      integer         ,intent(in)          :: var1       ! arguments.
      character(len=*),intent(in),optional :: msg2,msg3  ! arguments.
      integer         ,intent(in),optional :: var2       ! arguments.

                         buffer = trim(msg1)  //' '//string_ii2ss(var1)
      if (present(msg2)) buffer = trim(buffer)//' '//msg2
      if (present(var2)) buffer = trim(buffer)//' '//string_ii2ss(var2)
      if (present(msg3)) buffer = trim(buffer)//' '//msg3
      return
      end subroutine string_encode_cici


      subroutine string_encode_cfcf (buffer, msg1,var1,msg2,var2,msg3)
      implicit none
      character(len=*),intent(out)         :: buffer     ! arguments.
      character(len=*),intent(in)          :: msg1       ! arguments.
      real            ,intent(in)          :: var1       ! arguments.
      character(len=*),intent(in),optional :: msg2,msg3  ! arguments.
      real            ,intent(in),optional :: var2       ! arguments.

                         buffer = trim(msg1)  //' '//string_ff2ss(var1)
      if (present(msg2)) buffer = trim(buffer)//' '//msg2
      if (present(var2)) buffer = trim(buffer)//' '//string_ff2ss(var2)
      if (present(msg3)) buffer = trim(buffer)//' '//msg3
      return
      end subroutine string_encode_cfcf


      subroutine string_encode_cdcd (buffer, msg1,var1,msg2,var2,msg3)
      implicit none
      character(len=*),intent(out)         :: buffer     ! arguments.
      character(len=*),intent(in)          :: msg1       ! arguments.
      double precision,intent(in)          :: var1       ! arguments.
      character(len=*),intent(in),optional :: msg2,msg3  ! arguments.
      double precision,intent(in),optional :: var2       ! arguments.

                         buffer = trim(msg1)  //' '//string_dd2ss(var1)
      if (present(msg2)) buffer = trim(buffer)//' '//msg2
      if (present(var2)) buffer = trim(buffer)//' '//string_dd2ss(var2)
      if (present(msg3)) buffer = trim(buffer)//' '//msg3
      return
      end subroutine string_encode_cdcd


      subroutine string_encode_cccc (buffer, msg1,var1,msg2,var2,msg3)
      implicit none
      character(len=*),intent(out)         :: buffer     ! arguments.
      character(len=*),intent(in)          :: msg1       ! arguments.
      character(len=*),intent(in)          :: var1       ! arguments.
      character(len=*),intent(in),optional :: msg2,msg3  ! arguments.
      character(len=*),intent(in),optional :: var2       ! arguments.

                         buffer = trim(msg1)  //' '//var1
      if (present(msg2)) buffer = trim(buffer)//' '//msg2
      if (present(var2)) buffer = trim(buffer)//' '//var2
      if (present(msg3)) buffer = trim(buffer)//' '//msg3
      return
      end subroutine string_encode_cccc


      subroutine string_encode_clcl (buffer, msg1,var1,msg2,var2,msg3)
      implicit none
      character(len=*),intent(out)         :: buffer        ! arguments.
      character(len=*),intent(in)          :: msg1          ! arguments.
      logical         ,intent(in)          :: var1          ! arguments.
      character(len=*),intent(in),optional :: msg2,msg3     ! arguments.
      logical         ,intent(in),optional :: var2          ! arguments.

                         buffer = trim(msg1)  //' '//string_ll2ss(var1)
      if (present(msg2)) buffer = trim(buffer)//' '//msg2
      if (present(var2)) buffer = trim(buffer)//' '//string_ll2ss(var2)
      if (present(msg3)) buffer = trim(buffer)//' '//msg3
      return
      end subroutine string_encode_clcl


!!-------------------------- string enclose token --------------------------!!
!!-------------------------- string enclose token --------------------------!!
!!-------------------------- string enclose token --------------------------!!

     ! private.
     ! first doubles each occurrence of the specified character in the token.
     ! then encloses the token with the specified character.

      subroutine string_enclose_token (token, charr)
      implicit none
      character(len=*),intent(inout) :: token                   ! argument
      character(len=1),intent(in)    :: charr                   ! argument
      integer                        :: length,i                ! local

      length = len_trim (token)
      i = 1
      do
           if (i > length) exit
           if (token(i:i) == charr) then
                token = token(1:i)//charr//token(i+1:)
                i = i + 2
                length = length + 1
           else
                i = i + 1
           end if
      end do
      token = charr//token(1:length)//charr
      return
      end subroutine string_enclose_token


!!-------------------------- string exclose token --------------------------!!
!!-------------------------- string exclose token --------------------------!!
!!-------------------------- string exclose token --------------------------!!

     ! private.
     ! un-does the action performed by string_enclose_token.

      subroutine string_exclose_token (token, charr)
      implicit none
      character(len=*),intent(inout) :: token                   ! argument
      character(len=1),intent(in)    :: charr                   ! argument
      character(len=2)               :: pair                    ! local
      integer                        :: length,i                ! local

      length = len_trim (token)
      if (length < 2)                    return
      if (token(1:1)           /= charr) return
      if (token(length:length) /= charr) return
      token = token(2:length-1)
      length = length - 2
      pair = charr//charr
      i = 1
      do
           if (i >= length) exit
           if (token(i:i+1) == pair) then
                token = token(1:i)//token(i+2:)
                i = i + 1
                length = length - 1
           else
                i = i + 1
           end if
      end do
      return
      end subroutine string_exclose_token


!!-------------------------- string protect token --------------------------!!
!!-------------------------- string protect token --------------------------!!
!!-------------------------- string protect token --------------------------!!


      subroutine string_protect_token (token, special)
      implicit none
      character(len=*),intent(inout)  :: token                   ! argument
      character(len=*),intent(in)     :: special                 ! argument
      integer                         :: length                  ! local
      logical                         :: special_present         ! local
      logical                         :: single_present          ! local
      logical                         :: double_present          ! local
      character(len=1),parameter      :: single = "'"            ! local
      character(len=1),parameter      :: double = '"'            ! local

      length = len_trim (token)
      if (length == 0) then
           token = double//double
           return
      end if
      special_present = (scan(trim(token), special) > 0)
      single_present  = (scan(trim(token), single ) > 0)
      double_present  = (scan(trim(token), double ) > 0)
      if (double_present) then
           call string_enclose_token (token, single)
      else if (single_present .or. special_present) then
           call string_enclose_token (token, double)
      end if
      return
      end subroutine string_protect_token


!!-------------------------- string recover token --------------------------!!
!!-------------------------- string recover token --------------------------!!
!!-------------------------- string recover token --------------------------!!


      subroutine string_recover_token (token)
      implicit none
      character(len=*),intent(inout)  :: token                   ! argument
      character(len=1),parameter      :: single = "'"            ! local
      character(len=1),parameter      :: double = '"'            ! local

      if (token(1:1) == single) then
           call string_exclose_token (token, single)
      else if (token(1:1) == double) then
           call string_exclose_token (token, double)
      end if
      return
      end subroutine string_recover_token


!!----------------------- string put tokens -------------------------------!!
!!----------------------- string put tokens -------------------------------!!
!!----------------------- string put tokens -------------------------------!!


      subroutine string_put_tokens &
                       (record, tokens, ntokens, nilstring, widths, msg)
      implicit none
      character(len=*),intent(out)          :: record             ! argument
      character(len=*),intent(in)           :: tokens(:)          ! argument
      integer         ,intent(in)           :: ntokens            ! argument
      character(len=*),intent(in)           :: nilstring          ! argument
      integer         ,intent(in) ,optional :: widths(:)          ! argument
      character(len=*),intent(out),optional :: msg                ! argument
      integer                               :: istart,length,i    ! local
      character(len=80)                     :: buffer             ! local

      if (present(msg)) msg = ' '
      record = ' '
      istart = 1
      do i = 1,ntokens
           buffer = tokens(i)
           if (buffer == ' ') buffer = nilstring
           length = len_trim(buffer)
           if (present(widths)) then
           if (widths(i) /= 0) then
                if (length > abs(widths(i))) then
                     if (present(msg)) then
                          msg = 'specified WIDTH('//trim(string_ii2ss(i)) &
                                   //') too small for '//buffer
                     end if
                else
                     length = abs(widths(i))
                end if
           !    length = max(length,abs(widths(i)))
   !!!!!!       if (widths(i) > 0) buffer = adjustr(buffer(1:length)) !PGBAD.
                if (widths(i) > 0) then                               !PGOK.
                     buffer = adjustr(buffer(1:length))               !PGOK.
                end if                                                !PGOK.
           end if
           end if
           record(istart:istart+length-1) = buffer(1:length)
           istart = istart + length + 1
      end do
      return
      end subroutine string_put_tokens

! The Portland Group compiler has a bug which generates a run time error
! on the line of code indicated by !PGBAD in the above routine:
! A workaround consists of the lines indicated by !PGOK.


!!----------------------- string get tokens -------------------------------!!
!!----------------------- string get tokens -------------------------------!!
!!----------------------- string get tokens -------------------------------!!


      subroutine string_get_tokens1 (record, tokens, ntokens, nilstring)
      implicit none
      character(len=*),intent(in)  :: record                      ! argument
      character(len=*),intent(out) :: tokens(:)                   ! argument
      integer         ,intent(out) :: ntokens                     ! argument
      character(len=*),intent(in)  :: nilstring                   ! argument
      integer                      :: length                      ! local

      length = len_trim(record)
      call string_get_tokens2 (record, tokens, ntokens, nilstring, length)
      return
      end subroutine string_get_tokens1



      subroutine string_get_tokens2 &
                              (record, tokens, ntokens, nilstring, length)
      implicit none
      character(len=*),intent(in)  :: record                      ! argument
      character(len=*),intent(out) :: tokens(:)                   ! argument
      integer         ,intent(out) :: ntokens                     ! argument
      character(len=*),intent(in)  :: nilstring                   ! argument
      integer         ,intent(in)  :: length                      ! argument
      integer                      :: i,istart,isize              ! local

      ntokens = 0
      isize = size(tokens)
      if (isize == 0) return
      istart = 0
      do i = 1,length
           if (record(i:i) == ' ') then
                if (istart > 0) then
                  ntokens = ntokens + 1
                  tokens(ntokens) = record(istart:i-1)
                  if (tokens(ntokens) == nilstring) tokens(ntokens) = ' '
                  if (isize == ntokens) return
                  istart = 0
                end if
           else if (istart == 0) then
                istart = i
           end if
      end do
      if (istart > 0) then
           ntokens = ntokens + 1
           tokens(ntokens) = record(istart:length)
           if (tokens(ntokens) == nilstring) tokens(ntokens) = ' '
      end if
      return
      end subroutine string_get_tokens2


!!----------------------- string replace character -------------------------!!
!!----------------------- string replace character -------------------------!!
!!----------------------- string replace character -------------------------!!


      subroutine string_replace_character1 (string, oldchar, newchar)
      implicit none
      character(len=*),intent(inout)  :: string                  ! argument
      character(len=1),intent(in)     :: oldchar                 ! argument
      character(len=1),intent(in)     :: newchar                 ! argument
      integer                         :: length                  ! local

      length = len_trim(string)
      call string_replace_character2 (string, oldchar, newchar, length)
      return
      end subroutine string_replace_character1



      subroutine string_replace_character2 (string, oldchar, newchar, length)
      implicit none
      character(len=*),intent(inout)  :: string                  ! argument
      character(len=1),intent(in)     :: oldchar                 ! argument
      character(len=1),intent(in)     :: newchar                 ! argument
      integer         ,intent(in)     :: length                  ! argument
      integer                         :: i                       ! local

      do i = 1,length
           if (string(i:i) == oldchar) string(i:i) = newchar
      end do
      return
      end subroutine string_replace_character2


!!--------------------- to test a single character -----------------------!!
!!--------------------- to test a single character -----------------------!!
!!--------------------- to test a single character -----------------------!!


      logical function string_is_alphanum (c)
      character(len=1),intent(in)     :: c                         ! argument
      integer                         :: string_crou_is_alphanum   ! external
      string_is_alphanum = (string_crou_is_alphanum (ichar(c)) /= 0)
      end function string_is_alphanum


      logical function string_is_alpha (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_alpha   ! external
      string_is_alpha = (string_crou_is_alpha (ichar(c)) /= 0)
      end function string_is_alpha


      logical function string_is_control (c)
      character(len=1),intent(in)     :: c                        ! argument
      integer                         :: string_crou_is_control   ! external
      string_is_control = (string_crou_is_control (ichar(c)) /= 0)
      end function string_is_control


      logical function string_is_digit (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_digit   ! external
      string_is_digit = (string_crou_is_digit (ichar(c)) /= 0)
      end function string_is_digit


      logical function string_is_graph (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_graph   ! external
      string_is_graph = (string_crou_is_graph (ichar(c)) /= 0)
      end function string_is_graph


      logical function string_is_lower (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_lower   ! external
      string_is_lower = (string_crou_is_lower (ichar(c)) /= 0)
      end function string_is_lower


      logical function string_is_print (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_print   ! external
      string_is_print = (string_crou_is_print (ichar(c)) /= 0)
      end function string_is_print


      logical function string_is_punct (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_punct   ! external
      string_is_punct = (string_crou_is_punct (ichar(c)) /= 0)
      end function string_is_punct


      logical function string_is_space (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_space   ! external
      string_is_space = (string_crou_is_space (ichar(c)) /= 0)
      end function string_is_space


      logical function string_is_upper (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_upper   ! external
      string_is_upper = (string_crou_is_upper (ichar(c)) /= 0)
      end function string_is_upper


      logical function string_is_hex (c)
      character(len=1),intent(in)     :: c                      ! argument
      integer                         :: string_crou_is_hex     ! external
      string_is_hex = (string_crou_is_hex (ichar(c)) /= 0)
      end function string_is_hex


      logical function string_is_one (c,incl)
      character(len=1),intent(in)     :: c                       ! argument
      character(len=*),intent(in)     :: incl                    ! argument
      integer                         :: holler1(1),holler2(10)  ! local
      integer                         :: string_crou_is_one      ! external
      call string_cc2hh (c   ,holler1)
      call string_cc2hh (incl,holler2)
      string_is_one = (string_crou_is_one (holler1,holler2) /= 0)
      end function string_is_one


!!---------------------------- index -------------------------------------!!
!!---------------------------- index -------------------------------------!!
!!---------------------------- index -------------------------------------!!

!!!   indx =     index    (string,substring,.true.)
!!!   indx = string_index (string,substring,.true.)

!!! This function is an alternative for the first line above because the
!!! Portland Group compiler sometimes returns the wrong value for indx when
!!! the .true. argument is present (as if the .true. argument were absent).

      function string_index (string,substring,reverse) result (indx)
      character(len=*),intent(in) :: string,substring       ! arguments
      logical,optional,intent(in) :: reverse                ! arguments
      integer                     :: indx                   ! result
      integer                     :: nnn,iii,len1,len2,kkk  ! local
      logical                     :: forward                ! local

!!!   indx = index(string,substring,reverse)

      if (present(reverse)) then
           forward = .not.reverse
      else
           forward = .true.
      end if

      len1 = len(string)
      len2 = len(substring)
      nnn = len1 - len2 + 1
      kkk = len2 - 1
      indx = 0
      do iii = 1,nnn
           if (string(iii:iii+kkk) == substring) then
                indx = iii
                if (forward) return
           end if
      end do
      end function string_index


!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!


      end module string_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

