
!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- datacards.f90 --------------------------------!!
!!--------------------------- datacards.f90 --------------------------------!!
!!--------------------------- datacards.f90 --------------------------------!!


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
! Name       : DATACARDS 
! Category   : character
! Written    : 2000-10-09   by: Tom Stoeckley
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Encode and decode data cards.
! Portability: No known limitations, but see comments below.
!
!-------------------------------------------------------------------------------
!</brief_doc>



!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION       
!
! This primitive encodes parameters into data cards, and decodes data cards
! into parameters.  The parameters are stored in a PARLIST object, and the
! data cards are stored in a STRINGLIST object.  A PARLIST object and a
! STRINGLIST object are passed to this primitive to do the encoding or
! decoding.  This primitive does not own any data.
!
! The data cards in the STRINGLIST object passed to this primitive must of
! course be keyword-encoded.  These keyword-encoded data cards can be in
! either a packed or an unpacked format.
!
! Each parameter stored in keyword-encoded data cards can be a scalar data
! value (a single value) or an array of data values (zero or more values).
! The data values, which are stored as character variables in the PARLIST
! object, are treated by this primitive as character variables although in
! principle they could be other variable types.
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
!                           CALLING SEQUENCE
!
!                                      i    b      i
!              call datacards_encode (ppp, sss, packing)
!              call datacards_decode (sss, ppp)
!                                      i    b
!
! type(parlist_struct)     ppp = the  PARLIST   object containing parameters.
! type(stringlist_struct)  sss = the STRINGLIST object containing data cards.
! integer              packing = packing option for the data cards.
!
! DATACARDS_ENCODE first clears CARDS  before loading it with data cards.
! DATACARDS_DECODE first clears PARAMS before loading it with parameters.
!
! PACKING = DATACARDS_UNPACKED outputs the data cards in unpacked format.
! PACKING = DATACARDS_PACKED   outputs the data cards in  packed  format.
!
! The packing option is irrelevant when data cards are input.
!
! Note that the packing option is specified with named constants rather than
! simply as a true/false logical value.  This allows the possible future
! addition of more packing options with minimum changes to existing code
! in the event that other formats become desirable.
!
! If the data cards are decoded and later encoded, they may not appear
! identical even if the information they contain has not changed.
!
! If an error occurs when information is decoded from faulty data cards,
! some information will probably be lost or misinterpreted.
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
!
! FORMATS FOR INDIVIDUAL PARAMETERS ON THE DATA CARDS:
!
!   (1) Scalar parameters will be encoded as follows:
!
!                    keyword = value
!
!   (2) Array parameters will be encoded as follows:
!
!                    keyword = (value1, value2, value3)
!
!   (3) Array parameters with zero length will be encoded as follows:
!
!                    keyword = ()
!
!
! GENERAL DATA CARD FORMAT:
!
! In unpacked format, each data card will begin with a keyword, with only one
! parameter (scalar or array) on a card.  If more than one card is required
! for the parameter, additional cards will be used.
!
! In packed format, parameters will be arranged in the data cards by
! placing a comma after each individual parameter (following the scalar
! value or the closing parenthesis for arrays), except after the last
! parameter in the set of data cards.  As many keywords and values can be
! placed on a single data card as is practical, as long as the card does not
! become too large (preferably DATACARDS_CARD_LENGTH minus some padding).
! A break between any card and the next card can occur before or after any
! keyword or value or control character, or in the middle of a value when
! a single value is too long to fit on a data card (see below).
!
! Individual keywords and values will not be broken between data cards, except
! when a value is so long that it will not fit on a single card, in which
! case a special continuation character will be the last non-blank character
! on the card containing the first part of the value, and the rest of the
! value will be preceded by the same continuation character on the next card.
! This continuation character is the and-sign (&).  Any spaces preceding the
! first and-sign or following the second and-sign will be interpreted as
! part of the value.  If a value includes an and-sign as part of its
! characters, that and-sign must not be the last non-blank character on
! a line because it would be interpreted as a continuation character.
! Long values cannot be split onto more than two data cards.
!
! In addition to the internal formats of individual keywords and parameter
! values (see below), data cards can contain only the following characters:
!
!   (1) The blank space character (treated as insignificant except when
!         necessary to separate keywords or values from each other).
!   (2) The following special characters (as illustrated above): = ( ) , &
!
!
! FORMAT OF AN INDIVIDUAL KEYWORD:
!
! Keywords on the data cards must contain at least one character (preferably
! between 2 and 12 characters).  Since keywords are to be spelled like their
! corresponding Fortran variable names in process modules, only the following
! characters are allowed:
!
!   (1) The digits 0 through 9 (but not as the first character).
!   (2) The letters A through Z and a through z.
!   (3) The underscore (_) (but not normally as the first or last character).
!
! Note that this module treats keywords as case-insensitive.  This
! means that keywords specified to this module can be either upper
! or lower or mixed case, and matches will be made regardless of case.
! However, keywords output by this module will always be in upper
! case.
!
!
! FORMAT OF AN INDIVIDUAL PARAMETER VALUE (SCALAR VALUE OR ARRAY ELEMENT):
!
! Parameter values on the data cards can normally contain any characters
! EXCEPT the following:
!
!   (1) White space characters such as spaces and tabs.
!   (2) Non-printable characters such as the bell character.
!   (3) The following special characters: = ( ) , & ' "
!   (4) Illegal non-numeric characters when the parameter is a numeric one.
!
! If the parameter value must contain a space or any other special character
! not normally allowed, it can be enclosed in single or double quotes on the
! data card.  If the parameter value must contain a double (single) quote
! and is enclosed by double (single) quotes, the contained double (single)
! quote must be shown twice.  For example, the following string:
!                    abc& ":123
! can be displayed on a data card as:
!                   "abc& "":123"      or     'abc& ":123'
!
! The type of a parameter is not directly indicated in the data cards.
! Although an actual parameter can be integer, real, double precision,
! character, or logical, it is represented as a character string.  Logical
! values are represented as YES or NO (upper or lower or mixed case).
! A nil numeric value, or a blank character value, must be specified with
! zero or more spaces bracketed by single or double quotes.  A GUI should
! represent a nil value as a blank field.
!
! The GRID_STRUCT parameter is represented on the data cards in one of two
! ways:
!  (1) Two scalar values for the X and Y origins and one array with four
!        values for the forward rotation matrix.  These values are
!        referenced with two scalar keywords and one array keyword, with
!        real or double precision variable types.
!  (2) An array with six values, representing the same quantities as (1)
!        in the order listed in (1).  These values are referenced with a
!        single keyword and variable type GRID_STRUCT.
!
!-------------------------------------------------------------------------------
!                           DATA CARD EXAMPLES              
!
! Equal signs, commas, and parentheses are optionally preceded or followed
! by blanks.
!
!
! Output packed format:
!
!   KEYWORD1 = value1, KEYWORD2 = value2, KEYWORD3 = (value3a, value3b,
!   value3c, value3d), KEYWORD4 = (), KEYWORD5 = value5
!
!
! Output unpacked format:
!
!   KEYWORD1 = value1
!   KEYWORD2 = value2
!   KEYWORD3 = (value3a, value3b, value3c, value3d)
!   KEYWORD4 = ()
!   KEYWORD5 = value5
!
!
! Examples of variations (allowed upon input but not used upon output):
!
!   KEYWORD1 = value1  KEYWORD2 = value2  KEYWORD3 = (value3a  value3b
!   value3c  value3d)  KEYWORD4 = ()  KEYWORD5 = value5
!
!   KEYWORD1 = value1,
!   KEYWORD2 = value2,
!   KEYWORD3 = (value3a, value3b, value3c, value3d),
!   KEYWORD4 = (),
!   KEYWORD5 = value5,
!
!   KEYWORD1 = value1   KEYWORD2 = value2,
!   KEYWORD3 = (value3a  value3b  value3c  value3d),   KEYWORD4 = ()
!   KEYWORD5 = value5
!
! Note that when data cards are input into the datacards module, they can be
! in either packed or unpacked or mixed format, and they can optionally
! include commas.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                   
!
!     Date        Author     Description
!     ----        ------     -----------
!008. 2006-06-12  B. Menger   Removed Unused Variables.
!  7. 2002-02-04  Day and    Change code to circumvent a bug in the Portland
!                  Stoeckley  Group compiler (associated with ADJUSTL).
!  6. 2001-11-06  Stoeckley  Change code on several lines (concatenating
!                             character variables) to make the intel compiler
!                             happy.
!  5. 2001-06-29  Stoeckley  Fix bug when decoding two or more consecutive
!                             continuation cards; change so as never to encode
!                             two or more consecutive continuation cards.
!  4. 2001-06-11  Stoeckley  Fix bug introduced into datacards_encode.
!                            PRODUCTION.
!  3. 2001-05-17  Stoeckley  Modify to make the datacards easier to read and
!                             to increase efficiency.
!  2. 2001-05-10  Stoeckley  Modify to never begin a datacard with a comma.
!  1. 2000-10-19  Stoeckley  Initial version, made from code taken out of
!                             the CARDSET primitive; fixed bug when decoding
!                             data cards when & character begins one card and
!                             ends the next card; improve format of packed
!                             data cards for readability.
!
!-------------------------------------------------------------------------------
!</history_doc>



!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations, but see comments regarding a Portland Group compiler
! bug in subroutine DATACARDS_PRIV_GET_CARD.
!
! As you will see, the ADJUSTL intrinsic function aborts when passed a string
! which was passed as a formal parameter when the -O2 compiler option is used,
! but works OK when the -g compiler option is used instead.
!
! The fix is either to compile with -g, or to change the code so that ADJUSTL
! receives a local variable rather than a formal parameter.  In this case,
! we chose to change the code.
!
! This Portland Group compiler bug was discovered and fixed by Richard Day.
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


module datacards_module

  use string_module
  use stringlist_module
  use parlist_module
  implicit none

  private
  public :: datacards_encode
  public :: datacards_decode

      character(len=100),public,save :: DATACARDS_IDENT = &
'$Id: datacards.f90,v 1.8 2006/06/12 13:03:49 Menger prod sps $'

  integer,parameter,public  :: DATACARDS_CARD_LENGTH  = 80
  integer,parameter,public  :: DATACARDS_CARD_PADDING = 8
  integer,parameter,public  :: DATACARDS_UNPACKED     = 1
  integer,parameter,public  :: DATACARDS_PACKED       = 2
  integer,parameter,private :: lunstop                = 6
  integer,parameter,private ::   &
                MAXLEN = DATACARDS_CARD_LENGTH - DATACARDS_CARD_PADDING

  contains


!!------------------------------ encode -----------------------------------!!
!!------------------------------ encode -----------------------------------!!
!!------------------------------ encode -----------------------------------!!

! Calls datacards_priv_add_keyword.
! Calls datacards_priv_add_value.

! Calls string_protect_token.

! Calls stringlist_clear.
! Calls stringlist_add_empty_string.

! Calls parlist_num_keywords.
! Calls parlist_get_keyword.
! Calls parlist_num_elements.
! Calls parlist_nature.
! Calls parlist_get_scalar.
! Calls parlist_get_element.


      subroutine datacards_encode (ppp, sss, packing)
      implicit none
      type(parlist_struct)   ,intent(in)    :: ppp                  ! argument
      type(stringlist_struct),intent(inout) :: sss                  ! argument
      integer                ,intent(in)    :: packing              ! argument
      logical                               :: packed               ! local
      integer                               ::        nkeys,ikey ! local
      integer                               :: nelements            ! local
      integer                               :: ielement,nature      ! local
      character(len=PARLIST_LENGTH)         :: keyword              ! local
      character(len=PARLIST_LENGTH)         :: value,string         ! local
      character(len=PARLIST_LENGTH)         :: errmsg               ! local
      character(len=12),parameter           :: SPECIAL = '() ,=&'   ! local
      integer                               :: available            ! local
      integer                               :: lstring,ltokens      ! local
      integer                               :: lkeyword,lvalue      ! local
      integer                               :: longest_keyword      ! local
      integer                               :: longest_value        ! local
      integer                               :: length,tab           ! local

!----------get started:

      call stringlist_clear (sss)
      nkeys  = parlist_num_keywords(ppp)
      if (nkeys == 0) return
      packed = (packing == DATACARDS_PACKED)

      if (packed) then
           longest_keyword = 7
           longest_value   = 7
           tab = longest_keyword + 4 + longest_value
      else
           longest_keyword = 10
           longest_value   = 7
           tab = longest_keyword + 4 + longest_value
      end if

!----------start loop to encode scalars and arrays:

      do ikey = 1,nkeys
           keyword   = parlist_get_keyword  (ppp, ikey)
           nelements = parlist_num_elements (ppp, ikey)
           nature    = parlist_nature       (ppp, ikey)

           call datacards_priv_get_last_string (sss, string)

           if (.not.packed .or. nelements > 1) then
                if (string /= ' ') then
                     call stringlist_add_empty_string (sss)
                     string = ' '
                end if
           end if

!----------encode a scalar parameter:
!----------encode an array parameter with no elements:
!----------encode an array parameter with one element:

           if (nature == PARLIST_SCALAR) then
                call parlist_get_scalar   (ppp, ikey, value, errmsg)
                call string_protect_token (value, SPECIAL)
           else if (nelements == 0) then
                value = '()'
           else if (nelements == 1) then
                call parlist_get_element (ppp, ikey, 1, value, errmsg)
                call string_protect_token (value, SPECIAL)
  !!            value = '('//trim(value)//')'
  !! The above is replaced by the below to make the intel compiler happy.
  !! Likewise in several other places in this file.
                value = '('//trim(value(1:PARLIST_LENGTH-2))//')'
           else
                value = ' '           ! this means nelements >= 2.
           end if

           if (value /= ' ') then
  !!            if (ikey < nkeys .and. packed) value = trim(value)//','
                if (ikey < nkeys .and. packed) value = &
                                   trim(value(1:PARLIST_LENGTH-1))//','

                lkeyword = len_trim(keyword)
                lvalue   = len_trim(value)
                lstring  = len_trim(string)
                lstring  = tab * ((lstring + tab - 1) / tab)
                length   = max(longest_keyword,lkeyword)

                if (packed) then
                     available = tab - 4 - lvalue
                     if (length > available .and. lkeyword <= available) then
                          length = available
                     end if
                end if

                keyword  = keyword(1:length)//' ='
                ltokens  = length + 3 + lvalue

                if (lstring > 0 .and. ltokens > MAXLEN - lstring) then
                     call stringlist_add_empty_string (sss)
                end if
                call datacards_priv_add_keyword (sss, keyword,tab)
                call datacards_priv_add_value   (sss, value)
                cycle
           end if

!----------encode an array parameter with several elements:

           lkeyword = len_trim(keyword)
           length   = max(longest_keyword,lkeyword)
           keyword  = keyword(1:length)//' ='

           call datacards_priv_add_keyword (sss, keyword, tab)

           do ielement = 1,nelements
               call parlist_get_element (ppp, ikey, ielement, value, errmsg)
               call string_protect_token (value, SPECIAL)
  !!           if (ielement == 1) value = '('//value
               if (ielement == 1) value = '('//value(1:PARLIST_LENGTH-1)
               if (ielement < nelements) then
  !!                value = trim(value)//','
                    value = trim(value(1:PARLIST_LENGTH-1))//','
               else if (ikey < nkeys .and. packed) then
  !!                value = trim(value)//'),'
                    value = trim(value(1:PARLIST_LENGTH-2))//'),'
               else
  !!                value = trim(value)//')'
                    value = trim(value(1:PARLIST_LENGTH-1))//')'
               end if
               call datacards_priv_add_value (sss, value)
           end do

!----------finish loop to encode scalars and arrays:

      end do
      return
      end subroutine datacards_encode


!!------------------------------ decode -----------------------------------!!
!!------------------------------ decode -----------------------------------!!
!!------------------------------ decode -----------------------------------!!

! Calls datacards_priv_get_token.

! Calls string_recover_token.

! Calls stringlist_num_strings.

! Calls parlist_clear.
! Calls parlist_put_scalar.
! Calls parlist_put_array.
! Calls parlist_add_element.


      subroutine datacards_decode (sss, ppp)
      implicit none
      type(stringlist_struct),intent(in)    :: sss                  ! argument
      type(parlist_struct)   ,intent(inout) :: ppp                  ! argument
      integer                               :: ncards               ! local

      character(len=PARLIST_LENGTH)         :: keyword              ! local
      character(len=PARLIST_LENGTH)         :: token                ! local
      character(len=1)                      :: empty(1)             ! local
      integer                               :: icard                ! local
      integer                               :: icolumn              ! local
      integer                               :: state                ! local
      integer,parameter                     :: before_keyword = 1   ! local
      integer,parameter                     :: before_equal   = 2   ! local
      integer,parameter                     :: after_equal    = 3   ! local
      integer,parameter                     :: in_array       = 4   ! local

      call parlist_clear (ppp)
      ncards = stringlist_num_strings (sss)
      if (ncards == 0) return

      icard   = 1   ! index of current data card being read.
      icolumn = 1   ! index of next character on data card to be looked at.
      state   = before_keyword
      keyword = 'junkjunk'
      do
        call datacards_priv_get_token (sss, icard, icolumn, token)
!!!           tokens expected are ( or ) or () or = or a word.
!!!           commas are ignored above.
        if (token == ' ') then
             if (state == before_equal .or. state == after_equal)  &
                       call parlist_put_scalar (ppp, keyword, ' ')
             exit
        end if
        if (state == before_keyword) then
             call string_recover_token (token)
             keyword = token
             state = before_equal
        else if (token == '=') then
             if (state == before_equal) state = after_equal
        else if (state == before_equal) then
             call parlist_put_scalar   (ppp, keyword, ' ')
             call string_recover_token (token)
             keyword = token
        else if (token == '()') then
             call parlist_put_array (ppp, keyword, empty, 0)
             state = before_keyword
        else if (token == '(') then
             state = in_array
        else if (token == ')') then
             state = before_keyword
        else if (state == after_equal) then
             call string_recover_token (token)
             call parlist_put_scalar   (ppp, keyword, token)
             state = before_keyword
        else
             call string_recover_token (token)
             call parlist_add_element  (ppp, keyword, token)
        end if
      end do
      return
      end subroutine datacards_decode


!!------------------------ private get last string ------------------------!!
!!------------------------ private get last string ------------------------!!
!!------------------------ private get last string ------------------------!!


      subroutine datacards_priv_get_last_string (sss, string)
      implicit none
      type(stringlist_struct),intent(inout) :: sss                  ! argument
      character(len=*)       ,intent(out)   :: string               ! argument
      integer                               :: ncards               ! local
      character(len=8)                      :: errmsg               ! local

      ncards = stringlist_num_strings (sss)
      if (ncards == 0) call stringlist_add_empty_string (sss)
      call stringlist_get_last_string (sss, string, errmsg)
      if (errmsg /= ' ') then
           write(lunstop,*) 'Error in datacards_priv_get_last_string.'
           write(lunstop,*) trim(errmsg)
           write(lunstop,*) 'This is a programming error.'
           stop
      end if
      return
      end subroutine datacards_priv_get_last_string


!!------------------------ private add keyword -----------------------------!!
!!------------------------ private add keyword -----------------------------!!
!!------------------------ private add keyword -----------------------------!!

! TOKEN is a keyword;  ' ='  must already be appended.

! Calls stringlist_add_empty_string.
! Calls stringlist_get_last_string.
! Calls stringlist_replace_last_string.


      subroutine datacards_priv_add_keyword (sss, token, tab)
      implicit none
      type(stringlist_struct),intent(inout) :: sss                  ! argument
      character(len=*)       ,intent(in)    :: token                ! argument
      integer                ,intent(in)    :: tab                  ! argument
      character(len=PARLIST_LENGTH)         :: string               ! local
      integer                               :: lstring,ltoken       ! local
      character(len=1),parameter            :: andsign = '&'        ! local

      call datacards_priv_get_last_string (sss, string)

      lstring  = len_trim(string)
      ltoken   = len_trim(token)

      if (lstring > 0) lstring = tab * ((lstring + tab) / tab)

      if (ltoken <= MAXLEN - lstring) then
           string(lstring+1:) = token
      else if (ltoken <= MAXLEN) then
           call stringlist_add_empty_string    (sss)
           string = token
      else
           call stringlist_add_empty_string    (sss)
           string = token(1:MAXLEN)//andsign
           call stringlist_replace_last_string (sss, string)
           call stringlist_add_empty_string    (sss)
           string = ' '//andsign//token(MAXLEN+1:)
      end if

      call stringlist_replace_last_string (sss, string)
      return
      end subroutine datacards_priv_add_keyword


!!------------------------ private add value -----------------------------!!
!!------------------------ private add value -----------------------------!!
!!------------------------ private add value -----------------------------!!

! If TOKEN is a scalar,  ','  may already be appended.
! If TOKEN is an array element,  '('  may already be prepended.
! If TOKEN is an array element,  ',' or ')' or '),' may already be appended.

! Calls stringlist_add_empty_string.
! Calls stringlist_get_last_string.
! Calls stringlist_replace_last_string.


      subroutine datacards_priv_add_value (sss, token)
      implicit none
      type(stringlist_struct),intent(inout) :: sss                  ! argument
      character(len=*)       ,intent(in)    :: token                ! argument
      character(len=PARLIST_LENGTH)         :: string               ! local
      integer                               :: lstring,ltoken       ! local
      character(len=1),parameter            :: andsign = '&'        ! local

      call datacards_priv_get_last_string (sss, string)

      lstring  = len_trim(string)
      ltoken   = len_trim(token)

      if (ltoken <= MAXLEN - lstring) then
           string(lstring+2:) = token
      else if (ltoken <= MAXLEN) then
           call stringlist_add_empty_string    (sss)
           string = ' '//token
      else
           call stringlist_add_empty_string    (sss)
           string = ' '//token(1:MAXLEN)//andsign
           call stringlist_replace_last_string (sss, string)
           call stringlist_add_empty_string    (sss)
           string = ' '//andsign//token(MAXLEN+1:)
      end if

      call stringlist_replace_last_string (sss, string)
      return
      end subroutine datacards_priv_add_value


!!----------------------- private get card ---------------------------------!!
!!----------------------- private get card ---------------------------------!!
!!----------------------- private get card ---------------------------------!!

! Calls stringlist_get_string.


      subroutine datacards_priv_get_card (sss, icard, ncards, string)
      implicit none
      type(stringlist_struct),intent(in)    :: sss               ! argument
      integer                ,intent(inout) :: icard             ! argument
      integer                ,intent(in)    :: ncards            ! argument
      character(len=*)       ,intent(out)   :: string            ! argument
      character(len=PARLIST_LENGTH)         :: string2           ! local
      integer                               :: lstring,indx      ! local
      character(len=8)                      :: errmsg            ! local
      character(len=1),parameter            :: andsign = '&'     ! local

      call stringlist_get_string (sss, icard, string, errmsg)
      if (errmsg /= ' ') then
           write(lunstop,*) 'Error 1 in datacards_priv_get_card.'
           write(lunstop,*) trim(errmsg)
           write(lunstop,*) 'This is a programming error.'
           stop
      end if

  !   string = adjustl(string)      ! pgf90 bug when using -O2
  !   string = adjustl(string)      ! pgf90 works OK when using -g instead

      string2 = string              ! pgf90 works OK here using -O2 or -g
      string2 = adjustl(string2)    ! pgf90 works OK here using -O2 or -g
      string  = string2             ! pgf90 works OK here using -O2 or -g

! skip over previously-used continuation cards:

      do
           if (string(1:1) == andsign) then
                icard = icard + 1
                if (icard > ncards) return
                call stringlist_get_string (sss, icard, string, errmsg)
                if (errmsg /= ' ') then
                     write(lunstop,*) 'Error 2 in datacards_priv_get_card.'
                     write(lunstop,*) trim(errmsg)
                     write(lunstop,*) 'This is a programming error.'
                     stop
                end if
                string = adjustl(string)
           else
                exit
           end if
      end do

! now we have a new string to start with:

      indx = icard
      do
           lstring = len_trim(string)
           if (lstring == 0) return

           if (string(lstring:lstring) /= andsign) then
                return
           else if (indx == ncards) then
                string(lstring:lstring) = ' '
                return
           end if

! now the string ends in andsign (and it is not the last string):

           indx = indx + 1
           call stringlist_get_string (sss, indx, string2, errmsg)
           if (errmsg /= ' ') then
                write(lunstop,*) 'Error 3 in datacards_priv_get_card.'
                write(lunstop,*) trim(errmsg)
                write(lunstop,*) 'This is a programming error.'
                stop
           end if
           string2 = adjustl(string2)
           if (string2(1:1) == andsign) then
                string(lstring:) = string2(2:)
           else
                string(lstring:) = string2(1:)
           end if
      end do
      return
      end subroutine datacards_priv_get_card


!!----------------------- private get token --------------------------------!!
!!----------------------- private get token --------------------------------!!
!!----------------------- private get token --------------------------------!!

! Calls datacards_priv_get_card.


      subroutine datacards_priv_get_token (sss, icard, icolumn, token)
      implicit none
      type(stringlist_struct),intent(in)    :: sss                  ! argument
      integer                ,intent(inout) :: icard,icolumn        ! argument
      character(len=*)       ,intent(out)   :: token                ! argument
      character(len=1000)                   :: string                  ! local
      integer                               :: ncards,lstring,jcolumn  ! local
      character(len=1),parameter            :: single = "'"            ! local
      character(len=1),parameter            :: double = '"'            ! local

      ncards = stringlist_num_strings (sss)

      do
        if (icard > ncards) then
             token = ' '
             return
        end if
        call datacards_priv_get_card (sss, icard, ncards, string)
        if (icard > ncards) then
             token = ' '
             return
        end if
        lstring = len_trim(string)

        do
             if (icolumn > lstring) exit
             if (string(icolumn:icolumn) /= ' ' .and.  &
                 string(icolumn:icolumn) /= ',') exit
             icolumn = icolumn + 1
        end do

        if (icolumn <= lstring) exit
        icard = icard + 1
        icolumn = 1
      end do

!!! now icolumn points to the first character of the new token.

      if (string(icolumn:icolumn+1) == '()') then
           token = string(icolumn:icolumn+1)
           icolumn = icolumn + 2
           return
      else if (string(icolumn:icolumn) == '(') then
           token = string(icolumn:icolumn)
           icolumn = icolumn + 1
           return
      else if (string(icolumn:icolumn) == ')') then
           token = string(icolumn:icolumn)
           icolumn = icolumn + 1
           return
      else if (string(icolumn:icolumn) == '=') then
           token = string(icolumn:icolumn)        
           icolumn = icolumn + 1                 
           return                               
      end if

      jcolumn = icolumn
      if (string(icolumn:icolumn) == single) then

        do
             jcolumn = jcolumn + 1
             if (jcolumn > lstring) exit
             if (string(jcolumn:jcolumn) == single) then
                  jcolumn = jcolumn + 1
                  if (jcolumn-1 == lstring) exit
                  if (string(jcolumn:jcolumn) /= single) exit
             end if
        end do

      else if (string(icolumn:icolumn) == double) then

        do
             jcolumn = jcolumn + 1
             if (jcolumn > lstring) exit
             if (string(jcolumn:jcolumn) == double) then
                  jcolumn = jcolumn + 1
                  if (jcolumn-1 == lstring) exit
                  if (string(jcolumn:jcolumn) /= double) exit
             end if
        end do

      else

        do
             if (jcolumn > lstring) exit
             if (string(jcolumn:jcolumn) == ' ' .or.  &
                 string(jcolumn:jcolumn) == '=' .or.  &
                 string(jcolumn:jcolumn) == ',' .or.  &
                 string(jcolumn:jcolumn) == ')') exit
             jcolumn = jcolumn + 1
        end do

      end if

!!! now jcolumn points to the first character AFTER the new token.

      token = string(icolumn:jcolumn-1)
      icolumn = jcolumn
      return
      end subroutine datacards_priv_get_token


!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!
!!---------------------------- end of module -----------------------------!!


end module datacards_module


!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!
!!-------------------------------- end -----------------------------------!!

