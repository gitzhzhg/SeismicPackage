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

//---------------------- string_array.cc -----------------------//
//---------------------- string_array.cc -----------------------//
//---------------------- string_array.cc -----------------------//

//        implementation file for the StringArray class
//              derived from the ArrayBase class
//                     subdirectory oprim


#include "oprim/string_array.hh"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StringArray::StringArray(int nchar, long istep, long nstep)
    :    ArrayBase(sizeof(char) * nchar, istep, nstep),
                    _nchar   (nchar)
{
  assert(_nchar >= 1);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StringArray::~StringArray()
{
  removeAllElements();
}



//------------- access one element --------------------------//
//------------- access one element --------------------------//
//------------- access one element --------------------------//


void StringArray::fetchElement(long index, char *string)  const
{
  ArrayBase::fetchElement(index, string);
}


long StringArray::appendElement(char *string)
{
  return ArrayBase::appendElement(string);
}


long StringArray::insertElement(long index, char *string)
{
  return ArrayBase::insertElement(index, string);
}



//------------- access several elements ---------------------//
//------------- access several elements ---------------------//
//------------- access several elements ---------------------//


void StringArray::fetchElements(long index, long nget, char *strings) const
{
  assert(strings);
  ArrayBase::fetchElements(index, nget, strings);
}


long StringArray::appendElements(long nins, char *strings)
{
  assert(strings);
  return ArrayBase::appendElements(nins, strings);
}


long StringArray::insertElements(long index, long nins, char *strings)
{
  assert(strings);
  return ArrayBase::insertElements(index, nins, strings);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

