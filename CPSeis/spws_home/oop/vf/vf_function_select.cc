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

//----------------------- vf_function_select.cc -----------------------//
//----------------------- vf_function_select.cc -----------------------//
//----------------------- vf_function_select.cc -----------------------//

//         implementation file for the VfFunctionSelect class
//                derived from the SimpleSelect class
//                         subdirectory vf



#include "vf/vf_function_select.hh"
#include "vf/vf_function_array.hh"
#include "vf/vf_function.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>




//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfFunctionSelect::VfFunctionSelect(VfFunctionArray *array)
            :   SimpleSelect(),
                _array    (array),
                _several  (FALSE)
{
  assert(_array);
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfFunctionSelect::~VfFunctionSelect()
{
}



//-------------------- get and set select value ----------------------//
//-------------------- get and set select value ----------------------//
//-------------------- get and set select value ----------------------//

      // private.
      // overriding virtual functions.

char VfFunctionSelect::getSelectValue (long ifun)  const
{
  return (char)_array->velfun(ifun)->getSelectFlag();
}


void VfFunctionSelect::setSelectValue (long ifun, char select)
{
  _array->velfun(ifun)->setSelectFlag((int)select);
       ///// VfFunction asserts if select value is illegal.
}



//-------------------------- get select flags -----------------------------//
//-------------------------- get select flags -----------------------------//
//-------------------------- get select flags -----------------------------//

    // public.

long VfFunctionSelect::numSelectedVelocityFunctions ()  const
{
  return numSelected();
}


int VfFunctionSelect::velocityFunctionIsSelected (long ifun)  const
{
  long nfun = _array->numElements();
  assert(ifun >= 0 && ifun < nfun);
  return isSelected(ifun);
}


int VfFunctionSelect::getSelectFlag (long ifun)  const
{
  long nfun = _array->numElements();
  assert(ifun >= 0 && ifun < nfun);
  return (int)getSelectValue(ifun);
}



//-------------------------- set select flags -----------------------------//
//-------------------------- set select flags -----------------------------//
//-------------------------- set select flags -----------------------------//

    // public.

void VfFunctionSelect::clearSelectFlags()
{
  long nfun = _array->numElements();
  clearSelections(nfun);
}



void VfFunctionSelect::incrementSelectFlag(long ifun)
{
  long nfun = _array->numElements();
  assert(ifun >= 0 && ifun < nfun);
  incrementSelectValue(ifun);
  if(!_several) updateSelections(nfun);
}



void VfFunctionSelect::setSelectFlag (long ifun, int select)
{
  long nfun = _array->numElements();
  assert(ifun >= 0 && ifun < nfun);
  setSelectValue(ifun, select);
  if(!_several) updateSelections(nfun);
}



void VfFunctionSelect::beforeSettingSeveralSelectFlags()
{
  assert(!_several);
  _several = TRUE;
}



void VfFunctionSelect::afterSettingSeveralSelectFlags()
{
  assert(_several);
  long nfun = _array->numElements();
  updateSelections(nfun);
  _several = FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

