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

//--------------------- array_bucket.cc ---------------------------//
//--------------------- array_bucket.cc ---------------------------//
//--------------------- array_bucket.cc ---------------------------//

//         implementation file for the ArrayBucket class
//                  not derived from any class
//                      subdirectory oprim


#include "oprim/array_bucket.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



//----------------------- constructor -------------------------//
//----------------------- constructor -------------------------//
//----------------------- constructor -------------------------//


ArrayBucket::ArrayBucket(int step)
     :
         _step    (step),
         _n       (0),
         _nalloc  (0),
         _array   (NULL)
{
  assert(_step >= 1);
}



//--------------------- destructor -----------------------------//
//--------------------- destructor -----------------------------//
//--------------------- destructor -----------------------------//

       // the user should delete all elements first.

ArrayBucket::~ArrayBucket()
{
  if(_array) free(_array);
}




//--------------------- maybe adjust size -------------------------//
//--------------------- maybe adjust size -------------------------//
//--------------------- maybe adjust size -------------------------//

     // private.
     // adjusts the allocated length of the array if necessary.
     // increases the allocated length if minsize > nalloc.
     // decreases the allocated length if minsize < nalloc - 2 * _step.


void ArrayBucket::maybeAdjustSize(int minsize)
{
  assert(minsize + _step <= INT_MAX);
  if(minsize > _nalloc || minsize < _nalloc - 2 * _step)
      {
      void **array;
      int nalloc = minsize + _step;
      if(_array) array = (void**)realloc(_array, nalloc * sizeof(void*));
      else       array = (void**) malloc(        nalloc * sizeof(void*));
      if(array)
          {
          _array  = array;
          _nalloc = nalloc;
          }
      }
}



//-------------------- add element ------------------------------//
//-------------------- add element ------------------------------//
//-------------------- add element ------------------------------//

      // allocates or reallocates the array if necessary.
      // reduces the size of the array if too large.
      // sets pointer to next array element to NULL.
      // increments n if successful.
      // returns next array index to use.
      // returns -1 if allocation/reallocation fails.


int ArrayBucket::addElement(void *element)
{
  maybeAdjustSize(_n + 1);
  if(_n >= _nalloc) return -1;
  if(_array == NULL) return -1;
  _array[_n] = element;
  _n++;
  return (_n - 1);
}



//---------------------- fetch element -----------------------//
//---------------------- fetch element -----------------------//
//---------------------- fetch element -----------------------//

    // returns pointer to array element with requested index.
    // asserts if index is out of range.
    // asserts if the pointer is NULL.

void *ArrayBucket::fetchElement(int i)   const
{
  assert(i >= 0 && i < _n);
  assert(_array[i]);
  return _array[i];
}



//------------------- validate element -----------------------//
//------------------- validate element -----------------------//
//------------------- validate element -----------------------//

    // returns TRUE  if the pointer is valid.
    // returns FALSE if the pointer is NULL.
    // asserts if index is out of range.

int ArrayBucket::validateElement(int i)   const
{
  assert(i >= 0 && i < _n);
  if(_array[i] == NULL) return FALSE;
  return TRUE;
}



//----------------- validate index -----------------------//
//----------------- validate index -----------------------//
//----------------- validate index -----------------------//

    // asserts if the index is out of range.

void ArrayBucket::validateIndex(int i)   const
{
  assert(i >= 0 && i < _n);
}



//----------------- validate range -----------------------//
//----------------- validate range -----------------------//
//----------------- validate range -----------------------//

    // asserts if either index is out of range.
    // asserts if the second index is less than the first index.

void ArrayBucket::validateRange(int i1, int i2)   const
{
  assert(i1 >=  0 && i1 < _n);
  assert(i2 >= i1 && i2 < _n);
}



//------------------- remove elements --------------------------//
//------------------- remove elements --------------------------//
//------------------- remove elements --------------------------//

   // elements are removed by setting the pointer to NULL.
   // the user should delete the elements first.
   // decrements _n only if the removed elements are the last ones
   //   in the array.
   // asserts if either index is out of range.
   // asserts if the second index is less than the first index.

void ArrayBucket::removeElements(int i1, int i2)
{
  assert(i1 >=  0 && i1 < _n);
  assert(i2 >= i1 && i2 < _n);
  for(int i = i1; i <= i2; i++)
      {
      _array[i] = NULL;
      }
  while(_n > 0 && _array[_n - 1] == NULL)
      {
      _n--;
      }
  maybeAdjustSize(_n);
}



//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//

