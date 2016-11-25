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

//-------------------------- void_array.cc ----------------------------//
//-------------------------- void_array.cc ----------------------------//
//-------------------------- void_array.cc ----------------------------//

//            implementation file for the VoidArray class
//                    not derived from any class
//                         subdirectory oprim


#include "oprim/void_array.hh"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VoidArray::VoidArray()
               :
                 _n               (0),
                 _array           (NULL),
                 _active          (-1),
                 _reference       (-1)
{
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//


VoidArray::~VoidArray()
{
  if(_array) delete [] _array;
}




//------------------------- element -------------------------------------//
//------------------------- element -------------------------------------//
//------------------------- element -------------------------------------//

      // public.

void *VoidArray::element(long index)  const
{
  assert(index >= 0 && index < _n);
  assert(_array);
  return _array[index];
}


void *VoidArray::activeElement()  const
{
  if(_active == -1) return NULL;
  return element(_active);
}


void *VoidArray::referenceElement()  const
{
  if(_reference == -1) return NULL;
  return element(_reference);
}


void *VoidArray::lastElement()  const
{
  if(_n == 0) return NULL;
  return element(_n - 1);
}



//------------------- set active or reference index -----------------------//
//------------------- set active or reference index -----------------------//
//------------------- set active or reference index -----------------------//

      // public.

void VoidArray::setActiveIndex(long index)
{
  assert(index >= 0 && index < _n);
  _active = index;
}


void VoidArray::setReferenceIndex(long index)
{
  assert(index >= 0 && index < _n);
  _reference = index;
}



//------------------------ find element --------------------------------//
//------------------------ find element --------------------------------//
//------------------------ find element --------------------------------//

      // public.

long VoidArray::findElement(void *element)
{
  for(long index = 0; index < _n; index++)
      {
      if(_array[index] == element) return index;
      }
  return -1;
}



//------------------------ remove active element -----------------------//
//------------------------ remove active element -----------------------//
//------------------------ remove active element -----------------------//

      // public.

void VoidArray::removeActiveElement()
{
  if(_active >= 0) removeElement(_active);
}



//------------------------ replace active element -----------------------//
//------------------------ replace active element -----------------------//
//------------------------ replace active element -----------------------//

      // public.

long VoidArray::replaceActiveElement(void *element)
{
  if(_active >= 0) replaceElement(_active, element);
  return _active;
}



//------------------- insert element ----------------------------//
//------------------- insert element ----------------------------//
//------------------- insert element ----------------------------//

      // public.

void VoidArray::insertElement(long index, void *element)
{
  assert(index >= 0 && index <= _n);
  long n = _n + 1;
  long i;

  void **array = new void* [n];

  for(i = 0; i < index; i++)
      {
      array[i] = _array[i];
      }
  for(i = index; i < _n; i++)
      {
      array[i+1] = _array[i];
      }

  array[index] = element;
  if(_array) delete [] _array;
  _array = array;
  _n     = n;
  if(_active    == -1 || _active    >= index) _active++;
  if(_reference == -1 || _reference >= index) _reference++;
}



//------------------- append element ----------------------------//
//------------------- append element ----------------------------//
//------------------- append element ----------------------------//

      // public.

long VoidArray::appendElement(void *element)
{
  long n = _n + 1;
  void **array = new void* [n];

  for(long index = 0; index < _n; index++)
      {
      array[index] = _array[index];
      }

  array[_n] = element;
  if(_array) delete [] _array;
  _array = array;
  _n     = n;
  if(_active    == -1) _active    = 0;
  if(_reference == -1) _reference = 0;
  return (_n - 1);
}



//------------------------ find and remove element -----------------------//
//------------------------ find and remove element -----------------------//
//------------------------ find and remove element -----------------------//

      // public.

void VoidArray::findAndRemoveElement(void *element)
{
  long index = findElement(element);
  assert(index >= 0);
  removeElement(index);
}



//------------------------ remove element -----------------------//
//------------------------ remove element -----------------------//
//------------------------ remove element -----------------------//

      // public.

void VoidArray::removeElement(long index)
{
  assert(index >= 0 && index < _n);
  assert(_array);
  long n = _n - 1;
  long i;
  void **array = new void* [n];

  for(i = 0; i < index; i++)
      {
      array[i] = _array[i];
      }
  for(i = index+1; i < _n; i++)
      {
      array[i-1] = _array[i];
      }

  delete [] _array;
  _array = array;
  _n     = n;
  if(_active    > index || _active    == _n) _active--;
  if(_reference > index || _reference == _n) _reference--;
}



//------------------------ replace element -----------------------//
//------------------------ replace element -----------------------//
//------------------------ replace element -----------------------//

      // public.

void VoidArray::replaceElement(long index, void *element)
{
  assert(index >= 0 && index < _n);
  assert(_array);
  _array[index] = element;
}



//------------------------ clear elements -----------------------//
//------------------------ clear elements -----------------------//
//------------------------ clear elements -----------------------//

      // public.

void VoidArray::clearElements()
{
  if(_array) delete [] _array;
  _array     = NULL;
  _n         = 0;
  _active    = -1;
  _reference = -1;
}



//--------------------- create null elements --------------------//
//--------------------- create null elements --------------------//
//--------------------- create null elements --------------------//

      // public.

void VoidArray::createNullElements(long n)
{
  assert(n >= 0);
  clearElements();
  if(n == 0) return;
  _array     = new void* [n];
  _n         = n;
  _active    = 0;
  _reference = 0;
  for(long index = 0; index < _n; index++)
      {
      _array[index] = NULL;
      }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

