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

//---------------------- simple_short_array.cc ------------------------//
//---------------------- simple_short_array.cc ------------------------//
//---------------------- simple_short_array.cc ------------------------//

//          implementation file for the SimpleShortArray class
//                    not derived from any class
//                        subdirectory oprim

#include "oprim/simple_short_array.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//

SimpleShortArray::SimpleShortArray (const int maximum)
           :
        _maximum      (maximum),
        _array        (NULL),
        _nelements    (0),
        _buffer       (FNIL)
{
  assert(_maximum > 0 && _maximum != INIL);
  resetNumElements(NULL, 0);
}

//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

SimpleShortArray::~SimpleShortArray()
{
  if(_array) delete [] _array;
}

//---------------------- reset num elements --------------------------//
//---------------------- reset num elements --------------------------//
//---------------------- reset num elements --------------------------//

     // public.
     // this function is the big workhorse.
     // changes the number of elements.
     // computes all quantities.
     // sets the _array elements to the argument array (which can be NULL).
     // does not set or clear buffer.
     // _maximum must already be set to something > 0.
     // _nelements must already be set to something >= 0.
     // _array must already be either allocated to _nelements or set to NULL.
     // SHRT_MAX is 32767 (16 signed bits where SHRT_MIN is -32768).

void SimpleShortArray::resetNumElements(const float *values, int nelements)
{
  assert(nelements >= 0 && nelements <= _maximum && nelements != INIL);

////////// reset number of array elements if necessary:

  if(nelements != _nelements)
      {
      if(_array) { delete [] _array; _array = NULL; }
      _nelements = nelements;
      }

////////// compute various quantities:

  _numnils    = 0;          // set to number of nil values.
  _minval     = FNIL;       // set to minimum non-nil value.
  _maxval     = FNIL;       // set to maximum non-nil value.
  _npositive  = _nelements; // set to first index where value is nil or <= 0.
  _nascending = _nelements; // set to first index where value is nil or < 0 or <= previous value.
  float lav   = 0.0;        // set to largest non-nil absolute value.

  for(int index = 0; index < _nelements; index++)
      {
      float value = (values ? values[index] : FNIL);
      if(value == FNIL)
          {
          _numnils++;
          if(_npositive  == _nelements) _npositive  = index;
          if(_nascending == _nelements) _nascending = index;
          }
      else
          {
          if(_minval == FNIL || value < _minval) _minval = value;
          if(_maxval == FNIL || value > _maxval) _maxval = value;
          if(_npositive  == _nelements && value <= 0.0) _npositive  = index;
          if(_nascending == _nelements && value <  0.0) _nascending = index;
          if(_nascending == _nelements && index >= 1 && value <= values[index-1]) _nascending = index;
          lav = MaximumValue(AbsoluteValue(value), lav);
          }
      }

  if(lav == 0.0) _scalefactor = 1.0;
  else           _scalefactor = lav / SHRT_MAX;

////////// return if no elements or all elements are nils:

  if(_numnils == _nelements)         // includes _nelements == 0.
      {
      if(_array) { delete [] _array; _array = NULL; }
      return;
      }

////////// return if all elements are equal:

  if(_numnils == 0 && _minval == _maxval)
      {
      if(_array) { delete [] _array; _array = NULL; }
      return;
      }

////////// put values into short array:

  assert(values);
  if(!_array) _array = new short [_nelements];

  for(int index = 0; index < _nelements; index++)
      {
      float value = values[index];
      if(value == FNIL) _array[index] = SNIL;
      else              _array[index] = (short)NearestInteger(value / _scalefactor);
      }
}

//----------------------- delete all elements -----------------------//
//----------------------- delete all elements -----------------------//
//----------------------- delete all elements -----------------------//

     // public.
     // changes the number of elements to zero.
     // also clears buffer.

void SimpleShortArray::deleteAllElements()
{
  resetNumElements(NULL, 0);
  _buffer = FNIL;
}

//------------------- reset num elements and clear ----------------------//
//------------------- reset num elements and clear ----------------------//
//------------------- reset num elements and clear ----------------------//

     // public.
     // changes the number of elements.
     // sets all elements to nil.
     // also clears buffer.

void SimpleShortArray::resetNumElementsAndClear(int nelements)
{
  resetNumElements(NULL, nelements);
  _buffer = FNIL;
}

//---------------------------- copy all elements ----------------------//
//---------------------------- copy all elements ----------------------//
//---------------------------- copy all elements ----------------------//

     // public.
     // also copies buffer.

void SimpleShortArray::copyAllElements(const SimpleShortArray *object)
{
  assert(object);
  float *values = object->createArrayPointer();
  resetNumElements(values, object->_nelements);
  object->deleteArrayPointer(values);
  _buffer = object->_buffer;
}

//---------------- create and delete array pointer ---------------------//
//---------------- create and delete array pointer ---------------------//
//---------------- create and delete array pointer ---------------------//

   // public.
   // allocates one greater than _nelements so never returns NULL.
   // allocates one greater than _nelements so an additional element can be inserted.

float *SimpleShortArray::createArrayPointer()  const
{
  float *values = new float [_nelements + 1];
  getAllValues(values);
  return values;
}


void SimpleShortArray::deleteArrayPointer(float *values)  const
{
  if(values) delete [] values;
}

//--------------------------- get values -------------------------------//
//--------------------------- get values -------------------------------//
//--------------------------- get values -------------------------------//

   // public.

float SimpleShortArray::getValue(int index)  const
{
  assert(index >= 0 && index < _nelements);
  if(!_array) return _minval;   // this is FNIL if no elements or all nil.
  if(_array[index] == SNIL) return FNIL;
  return _array[index] * _scalefactor;
}


void SimpleShortArray::getAllValues(float *values)  const
{
  assert(values);
  for(int index = 0; index < _nelements; index++)
      {
      values[index] = getValue(index);
      }
}

//--------------------------- set values -------------------------------//
//--------------------------- set values -------------------------------//
//--------------------------- set values -------------------------------//

   // public.

void SimpleShortArray::setValue(int index, float value)
{
  assert(index >= 0 && index < _nelements);
  float *values = createArrayPointer();
  values[index] = value;
  setAllValues(values);
  deleteArrayPointer(values);
}


void SimpleShortArray::setAllValues(const float *values)
{
  resetNumElements(values, _nelements);
}

//---------------------- multiply by constant -------------------------//
//---------------------- multiply by constant -------------------------//
//---------------------- multiply by constant -------------------------//

void SimpleShortArray::multiplyByConstant(float constant)
{
  float *values = createArrayPointer();
  for(int index = 0; index < _nelements; index++)
      {
      if(values[index] != FNIL) values[index] *= constant;
      }
  setAllValues(values);
  deleteArrayPointer(values);
}

//--------------------------- add constant ----------------------------//
//--------------------------- add constant ----------------------------//
//--------------------------- add constant ----------------------------//

void SimpleShortArray::addConstant(float constant)
{
  float *values = createArrayPointer();
  for(int index = 0; index < _nelements; index++)
      {
      if(values[index] != FNIL) values[index] += constant;
      }
  setAllValues(values);
  deleteArrayPointer(values);
}

//------------------- insert (or append) one element ----------------------//
//------------------- insert (or append) one element ----------------------//
//------------------- insert (or append) one element ----------------------//

          // public.
          // the number of elements is incremented by one.

void SimpleShortArray::insertNilElement (int index)
{
  insertElement(index, FNIL);
}


void SimpleShortArray::insertElementFromBuffer (int index)
{
  insertElement(index, _buffer);
}


void SimpleShortArray::insertElement (int index, float value)
{
  assert(index >= 0 && index <= _nelements);
  float *values = createArrayPointer();
  for(int i = _nelements; i > index; i--)
      {
      values[i] = values[i - 1];
      }
  values[index] = value;
  resetNumElements(values, _nelements + 1);
  deleteArrayPointer(values);
}

//------------------------ remove one element ---------------------------//
//------------------------ remove one element ---------------------------//
//------------------------ remove one element ---------------------------//

          // public.
          // the number of elements is decremented by one.

void SimpleShortArray::removeElementToBuffer(int index)
{
  _buffer = getValue(index);
  removeElement(index);
}


void SimpleShortArray::removeElement(int index)
{
  assert(index >= 0 && index < _nelements);
  float *values = createArrayPointer();
  for(int i = index; i < _nelements - 1; i++)
      {
      values[i] = values[i + 1];
      }
  resetNumElements(values, _nelements - 1);
  deleteArrayPointer(values);
}

//-------------------- find bracketing values -----------------------//
//-------------------- find bracketing values -----------------------//
//-------------------- find bracketing values -----------------------//

     // public.
     // works only for ascending values.

void SimpleShortArray::findBracketingValues(float value, int *ia, int *ib)  const
{
  assert(ia && ib);

////////// check for cases when -1 should be returned:

  if(_nelements == 0 || value == FNIL || _numnils > 0 || _nascending < _nelements)
      {
      *ia = -1;
      *ib = -1;
      return;
      }
  assert(_minval != FNIL && _maxval != FNIL);

////////// check for value at or beyond array boundaries:

  if(value == _minval) { *ia =            0; *ib =            0; return; }
  if(value == _maxval) { *ia = _nelements-1; *ib = _nelements-1; return; }
  if(value <  _minval) { *ia =           -1; *ib =            0; return; }
  if(value >  _maxval) { *ia = _nelements-1; *ib =   _nelements; return; }

  assert(_maxval > _minval);
  assert(_nelements > 1);

////////// check for exact match:

  for(int index = 0; index < _nelements; index++)
      {
      if(getValue(index) == value)
          {
          *ia = index;
          *ib = index;
          return;
          }
      }

////////// find bracketing values:

  for(int index = 1; index < _nelements; index++)
      {
      if(getValue(index-1) < value && getValue(index) > value)
          {
          *ia = index-1;
          *ib = index;
          return;
          }
      }
  assert(FALSE);  // should not get to here.
}

//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
