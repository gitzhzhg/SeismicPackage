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

//---------------------- double_array.cc -----------------------//
//---------------------- double_array.cc -----------------------//
//---------------------- double_array.cc -----------------------//

//        implementation file for the DoubleArray class
//              derived from the ArrayBase class
//                     subdirectory oprim


#include "oprim/double_array.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


DoubleArray::DoubleArray(long istep, long nstep)
           :    ArrayBase(sizeof(double), istep, nstep)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

DoubleArray::~DoubleArray()
{
  removeAllElements();
}



//------------- access one element --------------------------//
//------------- access one element --------------------------//
//------------- access one element --------------------------//


double DoubleArray::fetchElement(long index)  const
{
  double value;
  ArrayBase::fetchElement(index, &value);
  return value;
}


long DoubleArray::appendElement(double value)
{
  return ArrayBase::appendElement(&value);
}


long DoubleArray::insertElement(long index, double value)
{
  return ArrayBase::insertElement(index, &value);
}



//------------- access several elements ---------------------//
//------------- access several elements ---------------------//
//------------- access several elements ---------------------//


void DoubleArray::fetchElements(long index, long nget, double *values) const
{
  ArrayBase::fetchElements(index, nget, values);
}


long DoubleArray::appendElements(long nins, double *values)
{
  return ArrayBase::appendElements(nins, values);
}


long DoubleArray::insertElements(long index, long nins, double *values)
{
  return ArrayBase::insertElements(index, nins, values);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

