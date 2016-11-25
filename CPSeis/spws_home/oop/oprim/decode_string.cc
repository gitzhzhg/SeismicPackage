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


//---------------------- decode_string.cc ------------------------//
//---------------------- decode_string.cc ------------------------//
//---------------------- decode_string.cc ------------------------//

//          implementation file for the DecodeString class
//                    not derived from any class     
//                        subdirectory oprim


#include "oprim/decode_string.hh"
#include "cprim.h"
#include "named_constants.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>


#define NBUF    200


    //  _allblanks is set by decodeCvar.
    //  _allblanks is used by decodeIvar and decodeFvar and decodeDvar.

    //  _numerrors is set to 0 by registerString.
    //  _numerrors is incremented by decodeIvar and decodeFvar and
    //                    decodeDvar if a decode error occurrs.

    //  _allblanks is returned by allBlanks().
    //  _numerrors is returned by numErrors().

    //  allBlanks() must be called right after calling decodeCvar or
    //     decodeIvar or decodeFvar or decodeCvar if you want to know
    //     whether the substring being returned, or the substring just
    //     decoded, was all blanks.

    //  numErrors() can be called any time after calling registerString,
    //     plus one or more calls to decodeIvar or decodeFvar or decodeCvar,
    //     if you want to know how many errors occurred while decoding
    //     any numbers since calling registerString.

    //  if a substring is all blanks, or if a decode error occurrs,
    //     the following functions return the indicated values:
    //
/*
    //     decodeIvar:  INIL if all blanks,  IERR if decode error.
    //     decodeFvar:  FNIL if all blanks,  FERR if decode error.
    //     decodeDvar:  DNIL if all blanks,  DERR if decode error.
*/
    //     decodeIvar:  INIL if all blanks or decode error.
    //     decodeFvar:  FNIL if all blanks or decode error.
    //     decodeDvar:  DNIL if all blanks or decode error.



//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//


DecodeString::DecodeString()
          :
            _card            (NULL),
            _cvar            (NULL),
            _len             (0),
            _index           (0),
            _allblanks       (TRUE),
            _numerrors       (0)
{
  _cvar = new char [NBUF+1];
}


DecodeString::~DecodeString()
{
  delete [] _cvar;
}



//-------------------- register string ----------------------------//
//-------------------- register string ----------------------------//
//-------------------- register string ----------------------------//

    // public.
    // saves pointer to nul-terminated string in _card.
    // saves length of _card in _len.
    // initializes _index to zero.
    // _index points to the next character in _card to decode.

void DecodeString::registerString(const char *card)
{
  _card = card;
  assert(_card);
  _index = 0;
  _len = strlen(_card);
  _numerrors = 0;
}



//------------------------- decode --------------------------------//
//------------------------- decode --------------------------------//
//------------------------- decode --------------------------------//

    // public.
    // decode one datafield on _card which has been registered.
    // _index is incremented by nchar before returning.
    // asserts if _index or nchar become too large.

void DecodeString::decodeSkip(int nchar)
{
  assert(_index >= 0 && _index <  NBUF);
  assert( nchar >  0 &&  nchar <= NBUF);
  _index += nchar;
}


/*****
    OLD CODE
const char *DecodeString::decodeCvar(int nchar)
{
  assert(_index >= 0 && _index <  NBUF);
  assert( nchar >  0 &&  nchar <= NBUF);
  if(_len > _index)
      {
      strncpy(_cvar, &_card[_index], nchar);
      _cvar[nchar] = '\0';
      }
  else
      {
      _cvar[0] = '\0';
      }
  _index += nchar;
  return _cvar;
}


long DecodeString::decodeIvar(int nchar)
{
  const char *cvar = decodeCvar(nchar);
  int ivar;
  int istat;
  convert_ss2ii((char*)cvar, &ivar, &istat);    // old
  return ivar;
}


float DecodeString::decodeFvar(int nchar)
{
  const char *cvar = decodeCvar(nchar);
  float fvar;
  int istat;
  convert_ss2ff((char*)cvar, &fvar, &istat);    // old
  return fvar;
}


double DecodeString::decodeDvar(int nchar)
{
  const char *cvar = decodeCvar(nchar);
  double dvar;
  int istat;
  convert_ss2dd((char*)cvar, &dvar, &istat);    // old
  return dvar;
}
*****/



const char *DecodeString::decodeCvar(int nchar)
{
  assert(_index >= 0 && _index <  NBUF);
  assert( nchar >  0 &&  nchar <= NBUF);
  int lastplus = _index + nchar;
  if(lastplus > _len) lastplus = _len;
  int n = 0;
  _allblanks = TRUE;
  for(int i = _index; i < lastplus; i++)
         {
         _cvar[n++] = _card[i];
         if(_card[i] != ' ') _allblanks = FALSE;
         }
  _cvar[n] = '\0';
  _index += nchar;
  return _cvar;
}



long DecodeString::decodeIvar(int nchar)
{
  const char *cvar = decodeCvar(nchar);
  if(_allblanks) return INIL;
  char *endp = NULL;
  long ivar = strtol(cvar, &endp, 10);
  if(endp && endp[0] != '\0' && endp[0] != ' ')
      {
      _numerrors++;
/*
      return IERR;
*/
      return INIL;
      }
  return ivar;
}


float DecodeString::decodeFvar(int nchar)
{
  const char *cvar = decodeCvar(nchar);
  if(_allblanks) return FNIL;
  char *endp = NULL;
  float fvar = (float)strtod(cvar, &endp);
  if(endp && endp[0] != '\0' && endp[0] != ' ')
      {
      _numerrors++;
/*
      return FERR;
*/
      return FNIL;
      }
  return fvar;
}


double DecodeString::decodeDvar(int nchar)
{
  const char *cvar = decodeCvar(nchar);
  if(_allblanks) return DNIL;
  char *endp = NULL;
  double dvar = strtod(cvar, &endp);
  if(endp && endp[0] != '\0' && endp[0] != ' ')
      {
      _numerrors++;
/*
      return DERR;
*/
      return DNIL;
      }
  return dvar;
}



//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//


