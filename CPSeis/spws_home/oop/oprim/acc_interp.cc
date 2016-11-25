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

//---------------------- acc_interp.cc -----------------------//
//---------------------- acc_interp.cc -----------------------//
//---------------------- acc_interp.cc -----------------------//

//          implementation file for the AccInterp class
//                 derived from the AccBase class
//                     subdirectory oprim

      ///////// note: DERR removed 10/31/2001


#include "oprim/acc_interp.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


   // This class resets values in array elements in a class
   // derived from SmartArray.  The values are reset to
   // interpolated or extrapolated values, based on other
   // values with the same ident but different indices.
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


AccInterp::AccInterp(SmartArray *array, int ident,
                     GetValue *get_value, SetValue *set_value, 
                     int sloping, double tolerance)
           : AccBase(array, ident),
                 _get_value            (get_value),
                 _set_value            (set_value),
                 _sloping              (sloping),
                 _tolerance            (tolerance)
{
  assert(_get_value);
  assert(_set_value);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccInterp::~AccInterp()
{
}



//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//
//----------------------- get range ---------------------------//

                  // private virtual function

void AccInterp::getRange(long n, long *index1, long *index2)
{
  long i1 = *index1;
  long i2 = *index2;
  while(i1 > 0   && _array->valueIsDependent(_ident, i1-1)) {i1--;}
  while(i2 < n-1 && _array->valueIsDependent(_ident, i2+1)) {i2++;}
  if(_sloping)
      {
      if(i1 > 1)
         {
         long j = i1-1;
         while(j > 0   && _array->valueIsDependent(_ident, j-1)) {j--;}
         if(j == 0) i1 = j;
         }
      if(i2 < n-2)
         {
         long j = i2+1;
         while(j < n-1 && _array->valueIsDependent(_ident, j+1)) {j++;}
         if(j == n-1) i2 = j;
         }
      }
  *index1 = i1;
  *index2 = i2;
}




//-------------- adjust dependency flags ---------------------//
//-------------- adjust dependency flags ---------------------//
//-------------- adjust dependency flags ---------------------//

              // public virtual function.

void AccInterp::adjustDependencyFlags()
{
  long n = _array->numElements();
  if(n == 0) return;
  int all_zero = TRUE;
  int all_same = TRUE;
  double prev  = _get_value(_array, 0);
  double next  = _get_value(_array, n - 1);
  if(prev != DZERO) all_zero = FALSE;
  if(next != DZERO) all_zero = FALSE;
  if(prev != next)  all_same = FALSE;
  _array->setDependencyFalse(_ident, 0    );
  _array->setDependencyFalse(_ident, n - 1);
  if(n > 2)
      {
      double value = _get_value(_array, 1);
      for(long index = 1; index < n - 1; index++)
          {
          next = _get_value(_array, index + 1);
          double value2 = (prev + next) / 2.0;
          if(AbsoluteValue(value - value2) < _tolerance)
                        _array->setDependencyTrue (_ident, index);
          else          _array->setDependencyFalse(_ident, index);
          prev = value;
          value = next;
          if(next != DZERO) all_zero = FALSE;
          if(prev != next)  all_same = FALSE;
          }
      }
  if(all_zero) _array->setDependencyTrue(_ident, 0    );
  if(all_same) _array->setDependencyTrue(_ident, n - 1);
}



//---------------- find non-interpolated range ---------------//
//---------------- find non-interpolated range ---------------//
//---------------- find non-interpolated range ---------------//

    // private.
    // If there are no values, returns first == -1 and last == -1.
    // If all values are to be interpolated, sets them to zero
    //   and returns first == -1 and last == -1.
    // If there is only one non-interpolated value, sets all other
    //   values to that value and returns first == -1 and last == -1.

void AccInterp::findNonTerpRange(long *first, long *last)
{
  *first = -1;
  *last  = -1;
  long n = _array->numElements();
  if(n == 0) return;
  *first = findFirstNonTerpValue();
  if(*first == -1)
      {
      replaceByValue(0, n - 1, DZERO);
      return;
      }
  *last  = findLastNonTerpValue();
  if(*last == *first)
      {
      double value = _get_value(_array, *first);
      replaceByValue(0, n - 1, value);
      *first = -1;
      *last  = -1;
      return;
      }
  assert(*last > *first);
}



//----------- find first or last non-interpolated value ------------//
//----------- find first or last non-interpolated value ------------//
//----------- find first or last non-interpolated value ------------//

    // private.
    // if no values, or all are to be interpolated, returns -1.
             
             
long AccInterp::findFirstNonTerpValue()  const
{
  long n = _array->numElements();
  for(long index = 0; index < n; index++)
      {
      if(!_array->valueIsDependent(_ident, index)) return index;
      }
  return -1;
}



long AccInterp::findLastNonTerpValue()  const
{
  long n = _array->numElements();
  for(long index = n - 1; index >= 0; index--)
      {
      if(!_array->valueIsDependent(_ident, index)) return index;
      }
  return -1;
}



//--------------------- replace by value -------------------------//
//--------------------- replace by value -------------------------//
//--------------------- replace by value -------------------------//

    // private.
    // replace range by specified value.

void AccInterp::replaceByValue(long first, long last, double value)
{
  for(long index = first; index <= last; index++)
      {
      if(_array->valueIsDependent(_ident, index))
                      _set_value(_array, index, value);
      }
}



//------------------- do grade -------------------------------//
//------------------- do grade -------------------------------//
//------------------- do grade -------------------------------//

    // private.
    // replace range by graded values.

void AccInterp::doGrade(long first, long last,
                    double value1, double value2, long index1, long index2)
{
  assert(index2 != index1);
/*
  if(value1 == DERR || value2 == DERR)
      {
      replaceByValue(first, last, DERR);
      return;
      }
*/
  if(value1 == DNIL || value2 == DNIL)
      {
      replaceByValue(first, last, DNIL);
      return;
      }
  double slope = (value2 - value1) / (index2 - index1);
  for(long index = first; index <= last; index++)
      {
      if(_array->valueIsDependent(_ident, index))
          {
          double value = value1 + (index - index1) * slope;
          _set_value(_array, index, value);
          }
      }
}




//------------------- fix range terp -------------------------//
//------------------- fix range terp -------------------------//
//------------------- fix range terp -------------------------//

                 // private virtual function.

void AccInterp::fixRange(long n, long /*index1*/, long /*index2*/)
{
  if(n == 0) return;
  long first, last;
  findNonTerpRange(&first, &last);
  if(first == -1) return;

  doInterpolations(first, last);
  if(_sloping)
      {
      double value1 = _get_value(_array, first);
      double value2 = _get_value(_array, first + 1);
      doGrade(0, first - 1, value1, value2, first, first + 1);
      value1 = _get_value(_array, last);
      value2 = _get_value(_array, last - 1);
      doGrade(last + 1, n - 1, value1, value2, last, last - 1);
      }
  else
      {
      double value1 = _get_value(_array, first);
      double value2 = _get_value(_array, last);
      replaceByValue(0,    first - 1, value1);
      replaceByValue(last + 1, n - 1, value2);
      }
}




//-------------------- do interpolations --------------------//
//-------------------- do interpolations --------------------//
//-------------------- do interpolations --------------------//

    // private.
    // replace values by interpolation.

void AccInterp::doInterpolations(long first, long last)
{
  long   index1, index2;
  double value1, value2;
  for(long index = first + 1; index <= last; index++)
      {
      int flag1 = _array->valueIsDependent(_ident, index - 1);
      int flag2 = _array->valueIsDependent(_ident, index    );
      if(!flag1 && flag2)
          {
          index1 = index - 1;
          value1 = _get_value(_array, index1);
          }
      else if(flag1 && !flag2)
          {
          index2 = index;
          value2 = _get_value(_array, index2);
          doGrade(index1 + 1, index2 - 1,
                               value2, value1, index2, index1);
          }
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

