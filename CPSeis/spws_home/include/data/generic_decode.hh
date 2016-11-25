/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

//---------------------- static_decode.hh ---------------------//
//---------------------- static_decode.hh ---------------------//
//---------------------- static_decode.hh ---------------------//

//            header file for the GenericDecode class
//                  not derived from any class
//                      subdirectory oprim


       // decodes a card which contains several numbers.


#ifndef _GENERIC_DECODE_HH_
#define _GENERIC_DECODE_HH_


class GenericDecode
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  enum { COLUMN_ORIENTED, ORDER_ORIENTED, BAD_CODE };

  enum { REASON_UNSET, REASON_NONMATCH, REASON_VALID, REASON_INVALID };

  char   *_how;          // how to decode the card image.
  char   *_required;     // list of required decode letters.
  char   *_optional;     // list of optional decode letters.
  char   *_allowed;      // list of allowed (required+optional) decode letters.
  char   *_bad;          // bad code information ("" if OK).
  int     _orientation;  // one of the above enums.
  int     _num_fields;   // number of fields on the card image.
  int     _num_cards;    // number of card images to decode together.
  int     _num_received; // number of card images received for decoding.
  int    *_index1;       // array of first index of each field to decode.
  int    *_nchar;        // array of number of characters in each field.
  char   *_letter;       // array of character associated with each field.
  char   *_card;         // card image currently being decoded.
  int     _length;       // length of _card.
  int     _decode_error; // whether a decode error occurred while decodeCard
                         //   was called, or while any of the following
                         //   functions were subsequently called:
                         //       getLongValue    getFloatValue
                         //       getDoubleValue  getStringValue
  int     _reason;       // reason why a nil was returned by the last call
                         //   to any of the following functions:
                         //       getLongValue    getFloatValue
                         //       getDoubleValue  getStringValue


//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:  // constructor and destructor.
         // how      = the default howDecode string (such as "XXX YYY MMM").
         // required = required letters             (such as "XY").
         // optional = optional letters             (such as "SM").
         // a dash (-) is always an optional letter meaning to skip a column.
         // a slash (/) means to continue to the next card image.
         // trailing dashes on the only (or last) card image can be omitted.
         // the number of card images read and decoded together is one more
         //   than the number of slashes.
         // dashes and slashes are allowed only for order-oriented decoding.
         // the blank char is optional anywhere for order-oriented decoding.
         // all characters (including the blank char) are position-specific
         //   for column-oriented decoding.

           GenericDecode(const char *how,
                         const char *required,
                         const char *optional);
  virtual ~GenericDecode();

public:  // specify a decode string.

  void        howDecode     (const char *how);
  void        reset         (const char *how,
                             const char *required,
                             const char *optional);
  const char *howDecode     ()  const  { return _how; }
  int         badDecode     ()  const  { return (_orientation == BAD_CODE); }

  const char *requiredCodes ()  const  { return _required; }
  const char *optionalCodes ()  const  { return _optional; }
  const char *allowedCodes  ()  const  { return _allowed; }
  const char *badCodeInfo   ()  const  { return _bad; }
  int         numCards      ()  const  { return _num_cards; }
  int         numReceived   ()  const  { return _num_received; }
  int         needAnotherCard() const  { return (_num_received < _num_cards); }
  int         decodeError   ()  const  { return _decode_error; }
  int         reason        ()  const  { return _reason; }

public:  // returns TRUE if specified letter is present in the decode string.

  int         hasCode     (char letter)  const;

public:  // supply a card image to decode.
         // returns error TRUE if a decode error occurs.
         // asserts if badDecode() returns TRUE.
         // sets _decode_error to the returned value (TRUE or FALSE).
         // sets _reason to REASON_UNSET.

  int         decodeCard  (const char *card);

public:  // call these to get individual values after calling decodeCard.
         // these assert if badDecode()       returns TRUE.
         // these assert if needAnotherCard() returns TRUE.
         // one of these results can be expected:
         //  (1) If the letter does NOT match a letter in the decode string,
         //        _reason is set to REASON_NONMATCH and nil is returned.
         //  (2) If the letter matches a letter in the decode string,
         //        and the corresponding field is successfully decoded,
         //        _reason is set to REASON_VALID and the answer is returned.
         //  (3) If the letter matches a letter in the decode string,
         //        but the corresponding field does not exist or contains
         //        bad information, _reason is set to REASON_INVALID and
         //        nil is returned.  Also, _decode_error is reset to TRUE.
         // a nil is defined as INIL or FNIL or DNIL or NULL.

  long        getLongValue    (char letter);
  float       getFloatValue   (char letter);
  double      getDoubleValue  (char letter);
  const char *getStringValue  (char letter);

private:

  int         privateDecode     ();
  const char *getField          (char letter);
  void        constructorHelper (const char *how);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
