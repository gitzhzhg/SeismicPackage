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

//---------------------- logic_array.cc -----------------------//
//---------------------- logic_array.cc -----------------------//
//---------------------- logic_array.cc -----------------------//

//         implementation file for the LogicArray class
//                   not derived from any class
//                       subdirectory oprim


#include "oprim/logic_array.hh"
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



//-------------------- constructor -------------------------//
//-------------------- constructor -------------------------//
//-------------------- constructor -------------------------//

                      // public.

LogicArray::LogicArray(long n)
              :
                _size       (sizeof(int) * CHAR_BIT),
                _array      (NULL),
                _nalloc     (0),
                _n          (n),
                _ntrue      (0)
{
  assert(_size >= 1);
  assert(_n    >= 1);
  _nalloc = 1 + (_n - 1) / _size;
}



//---------------------- destructor ------------------------//
//---------------------- destructor ------------------------//
//---------------------- destructor ------------------------//

                         // public.

LogicArray::~LogicArray()
{
  if(_array) delete [] _array;
}



//------------------- reset num elements ------------------------//
//------------------- reset num elements ------------------------//
//------------------- reset num elements ------------------------//

                          // public.

void LogicArray::resetNumElements(long n)
{
  setAllElementsFalse();
  _n = n;
  assert(_n >= 1);
  _nalloc = 1 + (_n - 1) / _size;
}



//------------------ set all elements false ----------------------//
//------------------ set all elements false ----------------------//
//------------------ set all elements false ----------------------//

                           // public.

void LogicArray::setAllElementsFalse()
{
  if(_array)
      {
      delete [] _array;
      _array = NULL;
      _ntrue = 0;
      }
}



//--------------------- get word index and mask -------------------------//
//--------------------- get word index and mask -------------------------//
//--------------------- get word index and mask -------------------------//

                           // private.

long LogicArray::getWordIndex(long index)  const
{
  long i = index / _size;
  assert(index  >= 0 && index  < _n);
  assert(i      >= 0 && i      < _nalloc);
  return i;
}


int LogicArray::getMask(long index, long i)  const
{
  int ibit = (int)(index - i * _size);
  assert(ibit >= 0 && ibit < _size);
  int mask = (1 << ibit);
  return mask;
}



//-------------------- get element ---------------------------//
//-------------------- get element ---------------------------//
//-------------------- get element ---------------------------//

                          // public.

int LogicArray::getElement(long index)  const
{
  if(_array == NULL) return 0;       // FALSE

  long i = getWordIndex(index);
  int mask = getMask(index, i);

  int value = (_array[i] & mask);
  if(value) return 1;                // TRUE
  return 0;                          // FALSE
}



//-------------------- set element ---------------------------//
//-------------------- set element ---------------------------//
//-------------------- set element ---------------------------//

                          // public.

void LogicArray::setElement(long index, int value)
{
  if(_array == NULL)
      {
      if(value == 0) return;
      _array = new int [_nalloc];
      assert(_array);
      memset(_array, 0, sizeof(int) * (unsigned int)_nalloc);
      }

  long i = getWordIndex(index);
  int mask = getMask(index, i);

  int value2 = (_array[i] & mask);
  if(value)
      {
      if(value2) return;
      _array[i] |= mask;
      _ntrue++;
      }
  else
      {
      if(!value2) return;
      _array[i] &= ~mask;
      _ntrue--;
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

