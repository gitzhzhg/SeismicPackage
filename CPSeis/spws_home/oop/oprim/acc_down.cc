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

//---------------------- acc_down.cc -----------------------//
//---------------------- acc_down.cc -----------------------//
//---------------------- acc_down.cc -----------------------//

//           implementation file for the AccDown class
//                derived from the AccBase class
//                     subdirectory oprim


#include "oprim/acc_down.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


   // This class resets values in array elements in a class
   // derived from SmartArray.  The values are reset by
   // downward repetition, based on other values with the
   // same ident but different indices.
   // This class uses values which are NOT flagged as "dependent",
   // to reset all values which ARE flagged as "dependent".

   // This class (and the base class) calls the following functions
   // in SmartArray:
   //
   //    long numElements       ()
   //     int valueIsDependent  (int ident, long index)
   //    void setDependencyTrue (int ident, long index)
   //    void setDependencyFalse(int ident, long index)
   //    void valuesWillChange  (int ident, long index, long nrem, long nins)
   //    void valuesHaveChanged (int ident, long index, long nrem, long nins)


static const double DZERO = 0.0;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccDown::AccDown(SmartArray *array, int ident,
                 GetValue *get_value, SetValue *set_value,
                 double tolerance)
           : AccBase(array, ident),
                 _get_value         (get_value),
                 _set_value         (set_value),
                 _tolerance         (tolerance)
{
  assert(_get_value);
  assert(_set_value);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccDown::~AccDown()
{
}



//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//

                 // private virtual function.

void AccDown::getRange(long n, long *index1, long *index2)
{
  long i1 = *index1;
  long i2 = *index2;
  while(i1 > 0   && _array->valueIsDependent(_ident, i1-1)) {i1--;}
  while(i2 < n-1 && _array->valueIsDependent(_ident, i2+1)) {i2++;}
  if(i1 > 0) i1 = *index1;
  *index1 = i1;
  *index2 = i2;
}



//-------------- adjust dependency flags ---------------------//
//-------------- adjust dependency flags ---------------------//
//-------------- adjust dependency flags ---------------------//

              // public virtual function.

void AccDown::adjustDependencyFlags()
{
  long n = _array->numElements();
  if(n == 0) return;
  int all_zero = TRUE;
  double prev = _get_value(_array, 0);
  _array->setDependencyFalse(_ident, 0);
  if(prev != DZERO) all_zero = FALSE;
  for(long index = 1; index < n; index++)
      {
      double value = _get_value(_array, index);
      if(AbsoluteValue(value - prev) < _tolerance)
                    _array->setDependencyTrue (_ident, index);
      else          _array->setDependencyFalse(_ident, index);
      prev = value;
      if(prev != DZERO) all_zero = FALSE;
      }
  if(all_zero) _array->setDependencyTrue(_ident, 0);
}



//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//
//-------------------- fix range ---------------------------------//

                 // private virtual function.

void AccDown::fixRange(long n, long index1, long index2)
{
  if(n == 0) return;
  long index = index1;
  double value;
  if(index1 == 0)
      {
      while(index < index2 && _array->valueIsDependent(_ident, index))
                                                         { index++; }
      if(!_array->valueIsDependent(_ident, index))
          {
          value = _get_value(_array, index);
          }
      else if(index == n-1)
          {
          value = DZERO;
          }
      else
          {
          index++;
          assert(!_array->valueIsDependent(_ident, index));
          value = _get_value(_array, index);
          }
      }
  else
      {
      index--;
      value = _get_value(_array, index);
      }
  for(index = index1; index <= index2; index++)
      {
      if(_array->valueIsDependent(_ident, index))
          {
          _set_value(_array, index, value);
          }
      else
          {
          value = _get_value(_array, index);
          }
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

