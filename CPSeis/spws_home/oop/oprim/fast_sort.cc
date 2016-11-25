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

//---------------------- fast_sort.cc -----------------------//
//---------------------- fast_sort.cc -----------------------//
//---------------------- fast_sort.cc -----------------------//

//         implementation file for the FastSort class
//                  not derived from any class     
//                     subdirectory oprim


#include "oprim/fast_sort.hh"
#include "oprim/smart_array.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


   // This class sorts the array elements in a class derived
   // from SmartArray.

   // This class calls the following functions in SmartArray:
   //
   //    void switchElements (long index1, long index2)



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FastSort::FastSort(SmartArray *array, GetSortKey *get_sort_key,
                                          float tolerance)
           :
             _array               (array),
             _get_sort_key        (get_sort_key),
             _tolerance           (tolerance),
             _desired_direction   (0),
             _indices             (NULL)
{
  assert(_array);
  assert(_get_sort_key);
  assert(_tolerance >= 0.0);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FastSort::~FastSort()
{
}



//------------------- static sort fun -----------------------//
//------------------- static sort fun -----------------------//
//------------------- static sort fun -----------------------//

  // staticSortFun is a private static function used for sorting.
  // staticSortFun returns TRUE if the values are switched.
  // staticSortFun returns FALSE if the values are not switched.

int FastSort::staticSortFun(void *data, long lo, long up)
{
  FastSort                *THIS = (FastSort*)data;
  SmartArray             *array = THIS->_array;
  GetSortKey      *get_sort_key = THIS->_get_sort_key;
  float               tolerance = THIS->_tolerance;
  int         desired_direction = THIS->_desired_direction;
  long                 *indices = THIS->_indices;
  float value1 = desired_direction * get_sort_key(array, lo-1);
  float value2 = desired_direction * get_sort_key(array, up-1);
  int doit;
  if     (value2 > value1 + tolerance)    doit = FALSE;
  else if(value2 < value1 - tolerance)    doit = TRUE;
  else if(indices[up-1] >= indices[lo-1]) doit = FALSE;
  else                                    doit = TRUE;
  if(doit == FALSE) return FALSE;
  array->switchElements(lo-1, up-1);
  long temp     = indices[lo-1];
  indices[lo-1] = indices[up-1];
  indices[up-1] = temp;
  return TRUE;
}



//------------------------- sort -----------------------------//
//------------------------- sort -----------------------------//
//------------------------- sort -----------------------------//

     // public.
     // if desired_direction >  0, sorts to ascending order.
     // if desired_direction <  0, sorts to descending order.
     // if desired_direction == 0, sorts to whatever order
     //   is implied by the first and last value.

void FastSort::sort(int desired_direction)
{
  long n = _array->numElements();
  if(n <= 1) return;
  if(desired_direction == 0)
      {
      if(n == 2) return;
      float value1 = _get_sort_key(_array, 0);
      float value2 = _get_sort_key(_array, n - 1);
      if(value2 >= value1) desired_direction =  1;
      else                 desired_direction = -1;
      }
  _desired_direction = desired_direction;
  _indices           = new long[n];
  for(long i = 0; i < n; i++)
      {
      _indices[i] = i;
      }
  generic_sort(n, staticSortFun, this);
  delete [] _indices;
  _indices           = NULL;
  _desired_direction = 0;
}



//---------------- switch direction --------------------------//
//---------------- switch direction --------------------------//
//---------------- switch direction --------------------------//

   // public.
   // if desired_direction >  0, switches direction to ascending order.
   //      (does nothing if order is already ascending)
   // if desired_direction <  0, switches direction to descending order.
   //      (does nothing if order is already descending)
   // if desired_direction == 0, switches direction to the order
   //       opposite to the current order.
   // the current order is implied by the first and last value.
   // no sorting is done.

void FastSort::switchDirection(int desired_direction)
{
  long n = _array->numElements();
  if(n <= 1) return;
  float value1 = _get_sort_key(_array, 0);
  float value2 = _get_sort_key(_array, n - 1);
  if(desired_direction > 0 && value2 >= value1) return;
  if(desired_direction < 0 && value2 <= value1) return;
  long half = n / 2;
  for(long i = 0; i < half; i++)
      {
      _array->switchElements(i, n - i - 1);
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

