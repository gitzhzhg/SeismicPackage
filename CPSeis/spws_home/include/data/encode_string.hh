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

//------------------------ encode_string.hh ----------------------------//
//------------------------ encode_string.hh ----------------------------//
//------------------------ encode_string.hh ----------------------------//

//            header file for the EncodeString class
//                 not derived from any class
//                     subdirectory oprim


//    This class encodes values into a string (which might
//    be thought of as a "card image").  The values are
//    encoded in a column-specific manner.  The primary
//    purpose is file I/O.
//    See the implementation file for documentation.


#ifndef _ENCODE_STRING_HH_
#define _ENCODE_STRING_HH_

#include <stdio.h>


class EncodeString
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  char *_card;      // string currently being encoded.
  int   _index;     // current location on string being encoded.

//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:    // constructor and destructor.

  EncodeString();
  virtual ~EncodeString();

  void        clearEncodedString();
  void        encodeBlanks                  (long nchar);
  void        encodeCvar  (const char *cvar, long nchar);
  void        encodeIvar  (long        ivar, long nchar);
  void        encodeFvar  (float       fvar, long nchar, long ndec);
  void        encodeDvar  (double      dvar, long nchar, long ndec);
  const char *getEncodedString();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
