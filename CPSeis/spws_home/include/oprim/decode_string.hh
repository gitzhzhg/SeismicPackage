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

//------------------------ decode_string.hh ----------------------------//
//------------------------ decode_string.hh ----------------------------//
//------------------------ decode_string.hh ----------------------------//

//            header file for the DecodeString class
//                 not derived from any class
//                     subdirectory oprim


//    This class decodes values from a string (which might
//    be thought of as a "card image").  The values are
//    decoded in a column-specific manner.  The primary
//    purpose is file I/O.
//    See the implementation file for documentation.


#ifndef _DECODE_STRING_HH_
#define _DECODE_STRING_HH_

#include <stdio.h>


class DecodeString
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  const char *_card;       // pointer to card currently being decoded.
  char       *_cvar;       // decoded character string.
  int         _len;        // length of card currently being decoded.
  int         _index;      // current location on card being decoded.
  int         _allblanks;  // whether sub-string is all blanks.
  int         _numerrors;  // how many decode errors occurred.

//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:    // constructor and destructor.

  DecodeString();
  virtual ~DecodeString();

  void        registerString     (const char* card);
  void        decodeSkip         (int nchar);
  const char *decodeCvar         (int nchar);
  long        decodeIvar         (int nchar);
  float       decodeFvar         (int nchar);
  double      decodeDvar         (int nchar);
  int         allBlanks          ()  const    { return _allblanks; }
  int         numErrors          ()  const    { return _numerrors; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
