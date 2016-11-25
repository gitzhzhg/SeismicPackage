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
#include "ipc/str_tok.hh"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define SIZE_INCR 10

StrTok::StrTok (const char *str, const char *delim) :
  _count   (0),
  _size    (0),
  _tokens  (0)
{
  assert (str && delim); // sanity check

  // split the incoming string up into all of its tokens
  //   while permitting empty tokens
  const char *cp, *cc, *cl;
  int slen = strlen (str);
  int dlen = strlen (delim);

  cp = &(str[0]);
  cl = &(str[slen]);

  int len;
  for (cc = cp; cc < cl; ) {
    cc = strstr (cp, delim); // determine the current character pointer
    if (cc == 0) {
      // no delimiter was found: remaining string is in this token
      len = cl - cp; // find the length of the remaining string
      cc = cl; // set to be at the end of the string
    }
    else /* if (c != 0) */ {
      // compute length of token found (may be zero)
      len = cc - cp; //
      cc += dlen; // point to the character just past the delimiter
    }
    if (_size == _count) {
      char **tmp = (char **)malloc (_size+SIZE_INCR);
      int k2;
      for (k2 = 0; k2 < _count; k2++) {
	tmp[k2] = _tokens[k2];
      }
      if (_tokens) free (_tokens);
      _tokens = tmp;
      _size += SIZE_INCR;
    }
    if (len) {
      // malloc & copy characters from previous character pointer
      _tokens[_count] = (char *)malloc (len+1);
      strncpy (_tokens[_count], cp, len);
      _tokens[_count][len] = '\0'; // null terminate token
    }
    else {
      // store a null pointer
      _tokens[_count] = 0;
    }
    _count++;
    cp = cc; // update previous character pointer
  }
}

StrTok::~StrTok ()
{
  int k2;
  for (k2 = 0; k2 < _count; k2++) {
    if (_tokens[k2]) free (_tokens[k2]);
  }
  free (_tokens);
}

int StrTok::count ()
{
  return _count;
}

const char *StrTok::get (int which)
{
  const char *retval;

  if (which > -1 && which < _count) {
    retval = (const char *)_tokens[which];
  }
  else {
    retval = 0;
  }
  return retval;
}
