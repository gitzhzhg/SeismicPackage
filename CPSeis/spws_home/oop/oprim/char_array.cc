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

//---------------------- char_array.cc -----------------------//
//---------------------- char_array.cc -----------------------//
//---------------------- char_array.cc -----------------------//

//        implementation file for the CharArray class
//              derived from the ArrayBase class
//                     subdirectory oprim


#include "oprim/char_array.hh"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


CharArray::CharArray(long istep, long nstep)
           :    ArrayBase(sizeof(char), istep, nstep)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

CharArray::~CharArray()
{
  removeAllElements();
}



//------------- access one element --------------------------//
//------------- access one element --------------------------//
//------------- access one element --------------------------//


char CharArray::fetchElement(long index)  const
{
  char value;
  ArrayBase::fetchElement(index, &value);
  return value;
}


long CharArray::appendElement(char value)
{
  return ArrayBase::appendElement(&value);
}


long CharArray::insertElement(long index, char value)
{
  return ArrayBase::insertElement(index, &value);
}



//------------- access several elements ---------------------//
//------------- access several elements ---------------------//
//------------- access several elements ---------------------//


void CharArray::fetchElements(long index, long nget, char *values) const
{
  ArrayBase::fetchElements(index, nget, values);
}


long CharArray::appendElements(long nins, char *values)
{
  return ArrayBase::appendElements(nins, values);
}


long CharArray::insertElements(long index, long nins, char *values)
{
  return ArrayBase::insertElements(index, nins, values);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

