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

//--------------------- static_decode.cc ---------------------//
//--------------------- static_decode.cc ---------------------//
//--------------------- static_decode.cc ---------------------//

//         implementation file for the StaticDecode class
//                  not derived from any class
//                      subdirectory stat



#include "stat/static_decode.hh"
#include "str.h"
#include "named_constants.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


#define MAX_ORDER 15
#define WIDTH     80


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StaticDecode::StaticDecode()
           :
          _how          (NULL)
{
  _how    = str_newstr("");
  _decode = str_newstr("");
  howDecode("");              // sets all member variables.
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StaticDecode::~StaticDecode()
{
  free(_how);
  free(_decode);
}



//------------------------ how decode ------------------------------//
//------------------------ how decode ------------------------------//
//------------------------ how decode ------------------------------//

   // if first character is #, replaces it with the second character.
   // if code is blank, uses "X Y M".

void StaticDecode::howDecode(const char *how)
{
/////////////////////////// grab new code:
  free(_how);
  _how = str_newstr(how);
  str_remove_trailing_blanks(_how, _how);
  str_to_upper              (_how, _how);
/////////////////////////// replace first character if it is #:
  if(_how[0] == '#')
      {
      if(strlen(_how) >= 2) _how[0] = _how[1];
      else                  _how[0] = '\0';
      }
/////////////////////////// choose default code if it is blank:
  if(_how[0] == '\0')
      {
      free(_how);
      _how = str_newstr(" X Y M");
      }
/////////////////////////// preset variables:
  free(_decode);
  _decode = str_newstr("");
  _xwhich = -1;
  _ywhich = -1;
  _swhich = -1;
  _mwhich = -1;
  _xfirst = -1;
  _yfirst = -1;
  _sfirst = -1;
  _mfirst = -1;
  _xlast = -1;
  _ylast = -1;
  _slast = -1;
  _mlast = -1;
/////////////////////////// see whether order orientation works:
  int error = tryOrderOrientation();
  if(!error)
      {
      _orientation = ORDER_ORIENTED;
      return;
      }
/////////////////////////// see whether column orientation works:
  error = tryColumnOrientation();
  if(!error)
      {
      _orientation = COLUMN_ORIENTED;
      return;
      }
/////////////////////////// deal with invalid code:
  _orientation = BAD_CODE;
}



//---------------------- try order orientation ------------------//
//---------------------- try order orientation ------------------//
//---------------------- try order orientation ------------------//


int StaticDecode::tryOrderOrientation()
{
  free(_decode);
  _decode = str_newstr("");
  _xwhich = -1;
  _ywhich = -1;
  _swhich = -1;
  _mwhich = -1;
  int len = strlen(_how);
  int error = 0;
  int kount = 0;
  int i;
  for(i = 0; i < len; i++)
      {
      if     (_how[i] == 'X') { if(_xwhich >= 0) { error = 1; }
                                else             { _xwhich = kount; kount++; } }
      else if(_how[i] == 'Y') { if(_ywhich >= 0) { error = 1; }
                                else             { _ywhich = kount; kount++; } }
      else if(_how[i] == 'S') { if(_swhich >= 0) { error = 1; }
                                else             { _swhich = kount; kount++; } }
      else if(_how[i] == 'M') { if(_mwhich >= 0) { error = 1; }
                                else             { _mwhich = kount; kount++; } }
      else if(_how[i] == 'B') { kount++; }
      else if(_how[i] != ' ') { error = 1; }
      }
  if(_xwhich == -1)                  error = 1;
  if(_swhich == -1 && _mwhich == -1) error = 1;
  if(_swhich != -1 && _mwhich != -1) error = 1;
  if(_xwhich >= MAX_ORDER)           error = 1;
  if(_ywhich >= MAX_ORDER)           error = 1;
  if(_swhich >= MAX_ORDER)           error = 1;
  if(_mwhich >= MAX_ORDER)           error = 1;
  if(error)
      {
      free(_decode);
      _decode = str_newstr("");
      _xwhich = -1;
      _ywhich = -1;
      _swhich = -1;
      _mwhich = -1;
      return 1;
      }
  char *buffer = new char [2 * kount + 1];
  strcpy(buffer, "");
  for(i = 0; i < kount; i++)
      {
      strcat(buffer, "%s");
      }
  free(_decode);
  _decode = str_newstr(buffer);
  delete [] buffer;
  return 0;
}



//---------------------- try column orientation ------------------//
//---------------------- try column orientation ------------------//
//---------------------- try column orientation ------------------//


static void helper(char *how, int i, int *start, int *stop, int *error)
{
  if(*start == -1)
      {
      *start = i;
      *stop = i;
      }
  else if(how[i-1] == how[i])
      {
      *stop = i;
      }
  else
      {
      *error = 1;
      }
}



int StaticDecode::tryColumnOrientation()
{
  _xfirst = -1;
  _yfirst = -1;
  _sfirst = -1;
  _mfirst = -1;
  _xlast = -1;
  _ylast = -1;
  _slast = -1;
  _mlast = -1;
  int error = 0;
  int len = strlen(_how);
  for(int i = 0; i < len; i++)
      {
      switch(_how[i])
          {
          case 'X': helper(_how, i, &_xfirst, &_xlast, &error); break;
          case 'Y': helper(_how, i, &_yfirst, &_ylast, &error); break;
          case 'S': helper(_how, i, &_sfirst, &_slast, &error); break;
          case 'M': helper(_how, i, &_mfirst, &_mlast, &error); break;
          case ' ':                                             break;
          default : error = 1;                                  break;
          }
      }
  if(_xfirst == -1)                  error = 1;
  if(_sfirst == -1 && _mfirst == -1) error = 1;
  if(_sfirst != -1 && _mfirst != -1) error = 1;
  if(_xlast - _xfirst + 1 > WIDTH)   error = 1;
  if(_ylast - _yfirst + 1 > WIDTH)   error = 1;
  if(_slast - _sfirst + 1 > WIDTH)   error = 1;
  if(_mlast - _mfirst + 1 > WIDTH)   error = 1;
  if(error)
      {
      _xfirst = -1;
      _yfirst = -1;
      _sfirst = -1;
      _mfirst = -1;
      _xlast = -1;
      _ylast = -1;
      _slast = -1;
      _mlast = -1;
      return 1;
      }
  return 0;
}



//----------------------- use column orientation -------------------//
//----------------------- use column orientation -------------------//
//----------------------- use column orientation -------------------//


static int get_value(const char *card, int len,
                       int first, int last, float *value)
{
  char buffer[WIDTH + 1];
  if(first <     0)             return 1;
  if(first >   len - 1)         return 1;
  if(last  >   len - 1)         last = len - 1;
  if(last  > first + WIDTH - 1) return 1;
  if(last  < first)             return 1;
  assert(last - first + 1 <= WIDTH);
  memcpy(buffer, &card[first], last - first + 1);
  buffer[last - first + 1] = '\0';
  *value = atof(buffer);
  return 0;
}



int StaticDecode::useColumnOrientation (const char *card,
                                float *xbin, float *ybin, float *value)
{
  int len = strlen(card);
  int error1 = get_value(card, len, _xfirst, _xlast, xbin);
  int error2;
  int error3;
  if(_yfirst >= 0)
      {
      error2 = get_value(card, len, _yfirst, _ylast, ybin);
      }
  else
      {
      *ybin = FNIL;
      error2 = 0;
      }
  if(_sfirst >= 0)
      {
      error3 = get_value(card, len, _sfirst, _slast, value);
      if(!error3 && *value != FNIL) *value *= 1000.0;
      }
  else
      {
      error3 = get_value(card, len, _mfirst, _mlast, value);
      }
  if(error1 || error2 || error3) return 1;
  return 0;
}



//----------------------- use order orientation -------------------//
//----------------------- use order orientation -------------------//
//----------------------- use order orientation -------------------//


int StaticDecode::useOrderOrientation (const char *card,
                                float *xbin, float *ybin, float *value)
{
/*********************************
        // ORIGINAL CODE
  int e = sscanf(card, "%g %g %g", xbin, ybin, value);
  if(e != 3) return 1;
  return 0;
*********************************/
  char a[MAX_ORDER][20];
  int e = sscanf(card, _decode, a[ 0], a[ 1], a[ 2], a[ 3], a[ 4],
                                a[ 5], a[ 6], a[ 7], a[ 8], a[ 9],
                                a[10], a[11], a[12], a[13], a[14]);
                     ////////  the above is a[0] thru a[MAX_ORDER-1].
  if(e < _xwhich + 1) return 1;
  if(e < _ywhich + 1) return 1;
  if(e < _swhich + 1) return 1;
  if(e < _mwhich + 1) return 1;
  *xbin = atof(a[_xwhich]);
  if(_ywhich >= 0)
      {
      *ybin = atof(a[_ywhich]);
      }
  else
      {
      *ybin = FNIL;
      }
  if(_swhich >= 0)
      {
      *value = atof(a[_swhich]);
      if(*value != FNIL) *value *= 1000.0;
      }
  else
      {
      *value = atof(a[_mwhich]);
      }
  return 0;
}



//--------------------------- decode value ----------------------------//
//--------------------------- decode value ----------------------------//
//--------------------------- decode value ----------------------------//

      // card must not be a comment card (must not begin with #).
      // always returns value in milliseconds (or nil).

int StaticDecode::decodeValue (const char *card,
                               float *xbin, float *ybin, float *value)
{
  if(_orientation == COLUMN_ORIENTED)
  return useColumnOrientation(card, xbin, ybin, value);
  return useOrderOrientation (card, xbin, ybin, value);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

