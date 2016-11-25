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


//---------------------- encode_string.cc ------------------------//
//---------------------- encode_string.cc ------------------------//
//---------------------- encode_string.cc ------------------------//

//          implementation file for the EncodeString class
//                    not derived from any class     
//                        subdirectory oprim


#include "oprim/encode_string.hh"
#include "cprim.h"
#include "str.h"
#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>


#define NBUF  200


//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//


EncodeString::EncodeString()
          :
            _card       (NULL),
            _index      (0)
{
  _card = new char [NBUF+1];
}


EncodeString::~EncodeString()
{
  delete [] _card;
}



//--------------------- clear encoded string ----------------------//
//--------------------- clear encoded string ----------------------//
//--------------------- clear encoded string ----------------------//

    // public.
    // must be called before beginning to encode values into the string.
    // presets _card to blanks with \0 in first character.
    // initializes _index to zero.
    // _index points to next location in which to encode a value.

void EncodeString::clearEncodedString()
{
  memset(_card, ' ', NBUF);
  _card[0] = '\0';
  _index = 0;
}



//-------------------- get encoded string -------------------------//
//-------------------- get encoded string -------------------------//
//-------------------- get encoded string -------------------------//

    // public.
    // returns pointer to the fully-encoded internal string.
    // this should be called after the string has been encoded
    //    with all desired values.  actually, this function
    //    can be called at any time since the address of the
    //    string never changes.
    // since the string gets re-used, the contents of this
    //    string must be used or copied before the next time
    //    clearEncodedString is called.

const char *EncodeString::getEncodedString()
{
  return _card;
}



//----------------- shorthand macros ------------------------//
//----------------- shorthand macros ------------------------//
//----------------- shorthand macros ------------------------//

#define BEFORE                          \
  _card[_index] = ' ';                  \
  _index += (int)nchar;                 \
  assert(nchar > 0 && _index <= NBUF);



#define AFTER                \
  _card[_index] = '\0';



//-------------------------- encode -------------------------------//
//-------------------------- encode -------------------------------//
//-------------------------- encode -------------------------------//

    // public.
    // encode a value into _card.
    // _index become incremented.
    // asserts if _index becomes too large.

void EncodeString::encodeBlanks(long nchar)
{
  BEFORE
  AFTER
}


void EncodeString::encodeCvar(const char *cvar, long nchar)
{
  BEFORE
  if(cvar)
      {
      int len = strlen(cvar);
      if(len > 0)
          {
          if(len > nchar) len = (int)nchar;
          memcpy(&_card[_index - len], cvar, len);
          }
      }
  AFTER
}


void EncodeString::encodeIvar(long ivar, long nchar)
{
  static char cvar[NBUF+1];
  int ivar2  = (int)ivar;
  int nchar2 = (int)nchar;
/*
  convert_ii2ss(&ivar2, cvar, &nchar2);
*/
  str_ii2ss(ivar2, cvar, nchar2);
  encodeCvar(cvar, nchar);
/*
  BEFORE
  convert_ii2ss(&ivar, &_card[_index - nchar], &nchar);
  AFTER
*/
}


void EncodeString::encodeFvar(float fvar, long nchar, long ndec)
{
  static char cvar[NBUF+1];
  int nchar2 = (int)nchar;
  int ndec2  = (int)ndec;
/*
  convert_ff2ss(&fvar, cvar, &nchar2, &ndec2);
*/
  str_ff2ss(fvar, cvar, nchar2, ndec2);
  encodeCvar(cvar, nchar);
/*
  BEFORE
  convert_ff2ss(&fvar, &_card[_index - nchar], &nchar, &ndec);
  AFTER
*/
}


void EncodeString::encodeDvar(double dvar, long nchar, long ndec)
{
  static char cvar[NBUF+1];
  int nchar2 = (int)nchar;
  int ndec2  = (int)ndec;
/*
  convert_dd2ss(&dvar, cvar, &nchar2, &ndec2);
*/
  str_dd2ss(dvar, cvar, nchar2, ndec2);
  encodeCvar(cvar, nchar);
/*
  BEFORE
  convert_dd2ss(&dvar, &_card[_index - nchar], &nchar, &ndec);
  AFTER
*/
}



//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//


