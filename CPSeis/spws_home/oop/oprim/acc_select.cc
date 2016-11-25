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

//---------------------- acc_select.cc -----------------------//
//---------------------- acc_select.cc -----------------------//
//---------------------- acc_select.cc -----------------------//

//           implementation file for the AccSelect class
//                derived from the AccBase class
//           also derived from the SimpleSelect class
//                     subdirectory oprim


#include "oprim/acc_select.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


 // This class (and the base class) calls the following functions
 // in SmartArray:
 //
 //    void valuesWillChange  (int ident, long index, long nrem, long nins)
 //    void valuesHaveChanged (int ident, long index, long nrem, long nins)



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccSelect::AccSelect(SmartArray *array, int ident,
                     GetSelectValue *get_select_value,
                     SetSelectValue *set_select_value)
           : AccBase(array, ident),
             SimpleSelect(),
                _get_select_value   (get_select_value),
                _set_select_value   (set_select_value)
{
  assert(_get_select_value);
  assert(_set_select_value);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccSelect::~AccSelect()
{
}



//------------------- clear selections -----------------------//
//------------------- clear selections -----------------------//
//------------------- clear selections -----------------------//

                     // public

void AccSelect::clearSelections()
{
  long n = _array->numElements();
  SimpleSelect::clearSelections(n);
}



//-------------------- get select value ---------------------//
//-------------------- get select value ---------------------//
//-------------------- get select value ---------------------//

          // public virtual function overriding SimpleSelect.

char AccSelect::getSelectValue(long index)  const
{
  return _get_select_value(_array, index);
}



//-------------------- set select value ---------------------//
//-------------------- set select value ---------------------//
//-------------------- set select value ---------------------//

          // public virtual function overriding SimpleSelect.

void AccSelect::setSelectValue(long index, char select)
{
  _set_select_value(_array, index, select);
}



//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//

          // private virtual function overriding AccBase.

void AccSelect::getRange(long n, long* /*index1*/, long *index2)
{
  *index2 = n-1;
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

          // private virtual function overriding AccBase.

void AccSelect::fixRange(long n, long /*index1*/, long /*index2*/)
{
  SimpleSelect::updateSelections(n);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

