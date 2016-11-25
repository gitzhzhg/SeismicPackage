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

//---------------------- simple_float_array.cc ------------------------//
//---------------------- simple_float_array.cc ------------------------//
//---------------------- simple_float_array.cc ------------------------//

//          implementation file for the SimpleFloatArray class
//                    not derived from any class
//                        subdirectory oprim


#include "oprim/simple_float_array.hh"
#include "cprim.h"
#include "named_constants.h"
#include "swapio.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>

static      char *IDENTIFIER = "SimpleFloatArray";
static const int   NBUF       = 20;


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


SimpleFloatArray::SimpleFloatArray (const long maximum, float tolerance)
           :
        _maximum      (maximum),
        _tolerance    (tolerance),
        _array        (NULL),
        _nalloc       (0),
        _nelements    (0),
        _buffer       (FNIL),
        _minval       (FNIL),
        _maxval       (FNIL),
        _numnils      (0),
        _nascending   (0),
        _ndescending  (0),
        _npositive    (0),
        _spacing      (0.0),
        _updated      (TRUE)
{
  assert(_maximum   >   0  && _maximum   != INIL);
//assert(_tolerance >  0.0 && _tolerance != FNIL);   // removed 9/24/99
  assert(_tolerance >= 0.0 && _tolerance != FNIL);   // added   9/24/99
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

SimpleFloatArray::~SimpleFloatArray()
{
  if(_array) delete [] _array;
}



//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//


static inline int private_match(float value1, float value2, float tolerance)
{
  assert(value1 != FNIL && value2 != FNIL);
  return (AbsoluteValue(value1 - value2) <= tolerance);
}



static float private_minimum(const float *array, long nelements)
{
  assert(array);
  float minval = FNIL;
  for(long index = 0; index < nelements; index++)
      {
      float value = array[index];
      if(value == FNIL) continue;
      if(minval == FNIL || value < minval) minval = value;
      }
  return minval;
}


static float private_maximum(const float *array, long nelements)
{
  assert(array);
  float maxval = FNIL;
  for(long index = 0; index < nelements; index++)
      {
      float value = array[index];
      if(value == FNIL) continue;
      if(maxval == FNIL || value > maxval) maxval = value;
      }
  return maxval;
}



static long private_num_nils(const float *array, long nelements)
{
  assert(array);
  long numnils = 0;
  for(long index = 0; index < nelements; index++)
      {
      if(array[index] == FNIL) numnils++;
      }
  return numnils;
}



static long private_ascending(const float *array, long nelements,
                              float minval, float maxval)
{
  if(!array)
      {
      if(minval == FNIL || minval < 0.0) return 0;
      if(nelements > 1 && minval == maxval) return 0;
      return nelements;
      }
  for(long index = 0; index < nelements; index++)
      {
      if(array[index] == FNIL || array[index] < 0.0)   return index;
      if(index >= 1 && array[index] <= array[index-1]) return index;
      }
  return nelements;
}



static long private_descending(const float *array, long nelements,
                               float minval, float maxval)
{
  if(!array)
      {
      if(minval == FNIL || minval < 0.0) return 0;
      if(nelements > 1 && minval == maxval) return 0;
      return nelements;
      }
  for(long index = 0; index < nelements; index++)
      {
      if(array[index] == FNIL || array[index] < 0.0)   return index;
      if(index >= 1 && array[index] >= array[index-1]) return index;
      }
  return nelements;
}



static long private_positive(const float *array, long nelements,
                             float minval)
{
  if(!array)
      {
      if(minval == FNIL || minval <= 0.0) return 0;
      return nelements;
      }
  for(long index = 0; index < nelements; index++)
      {
      if(array[index] == FNIL || array[index] <= 0.0) return index;
      }
  return nelements;
}



static float private_spacing(const float *array, long nelements,
                 float minval, float maxval, long numnils, float tolerance)
{
/*
  if(minval == maxval)
      {
      return 0.0;       // includes nelements <= 1 and numnils == nelements.
      }
/// the above four lines moved to below 11/04/99 by Tom Stoeckley.
*/
  if(numnils > 0 && numnils < nelements)
      {
      return FNIL;      // spacing cannot be equal.
      }
  if(minval == maxval)
      {
      return 0.0;       // includes nelements <= 1 and numnils == nelements.
      }
  assert(nelements > 1 && numnils == 0);
  assert(minval != FNIL && maxval != FNIL);
  float spacing = (maxval - minval) / (nelements - 1);
  if(array == NULL)
      {
      return spacing;   // spacing has to be equal.
      }
  for(long index = 0; index < nelements; index++)
      {
      if(array[index] == FNIL) return FNIL;    // added 22 june 2010 by stoeckley
      float value = minval + spacing * index;
      if(private_match(value, array[index], tolerance) == FALSE) return FNIL;
      }
  return spacing;
}



static void private_fill
               (float *array, long nelements, float minval, float spacing)
{
  assert(array);              // added 9 june 2010 by stoeckley
  assert(spacing != FNIL);
  assert(minval != FNIL || spacing == 0.0);
  for(long index = 0; index < nelements; index++)
      {
      array[index] = minval + spacing * index;
      }
}



static void private_copy
               (float *array, long nelements, const float *values)
{
  if(nelements == 0) return;
  assert(array && values);
  memcpy((void*)array, (void*)values, (unsigned int)nelements * sizeof(float));
}



//------------------- read from binary file --------------------------//
//------------------- read from binary file --------------------------//
//------------------- read from binary file --------------------------//

   // public.

int SimpleFloatArray::readFromBinaryFile(FILE *stream, char *msg)
{
  char buffer[NBUF];
  long nelements, numnils;
  float minval, maxval;
  int swapped = FALSE;
  int shrunken;
/*
  int num1 = fread( buffer   , NBUF         , 1, stream);
  int num2 = fread(&nelements, sizeof(long) , 1, stream);
  int num3 = fread(&numnils  , sizeof(long) , 1, stream);
  int num4 = fread(&minval   , sizeof(float), 1, stream);
  int num5 = fread(&maxval   , sizeof(float), 1, stream);
  int num6 = fread(&shrunken , sizeof(int)  , 1, stream);
  if(num1 != 1 || num2 != 1 || num3 != 1 ||
     num4 != 1 || num5 != 1 || num6 != 1)
*/
  int num1 = swapio_fread_char ( buffer   , NBUF, stream);
  int num2 = swapio_fread_long (&nelements,    1, stream);
  int num3 = swapio_fread_long (&numnils  ,    1, stream);
  int num4 = swapio_fread_float(&minval   ,    1, stream);
  int num5 = swapio_fread_float(&maxval   ,    1, stream);
  int num6 = swapio_fread_int  (&shrunken ,    1, stream);
  if(num1 != NBUF || num2 != 1 || num3 != 1 ||
     num4 !=    1 || num5 != 1 || num6 != 1)
      {
      strcpy(msg, "error while reading SimpleFloatArray values from file");
      return TRUE;
      }

  if (nelements < 0 || nelements > 999999)
      {
      swapio_toggle_read_swapping_action ();
      swapio_do_swap_long (&nelements, 1);
      swapio_do_swap_long (&numnils  , 1);
      swapio_do_swap_float(&minval   , 1);
      swapio_do_swap_float(&maxval   , 1);
      swapio_do_swap_int  (&shrunken , 1);
      swapped = TRUE;
      }

  if(strncmp(buffer, IDENTIFIER, NBUF) != 0)
      {
      strcpy(msg, "wrong SimpleFloatArray identifier in file");
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }
  if(nelements < 0 || nelements > _maximum  ||
     numnils   < 0 || numnils   > nelements ||
     minval    > maxval)
      {
      strcpy(msg, "bad SimpleFloatArray values in file");
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }
  if(shrunken)
      {
      if(_array) delete [] _array;
      _array       = NULL;
      _nalloc      = 0;
      _nelements   = nelements;
      _minval      = minval;
      _maxval      = maxval;
      _numnils     = numnils;
      _nascending  = private_ascending (_array, _nelements, _minval, _maxval);
      _ndescending = private_descending(_array, _nelements, _minval, _maxval);
      _npositive   = private_positive  (_array, _nelements, _minval);
      _spacing     = private_spacing   (_array, _nelements, _minval, _maxval,
                                                      _numnils, _tolerance);
      _updated     = TRUE;
      }
  else
      {
      if(nelements == 0)
          {
          strcpy(msg, "bad shrunken SimpleFloatArray value in file");
          if(swapped) swapio_toggle_read_swapping_action ();
          return TRUE;
          }
      float *values = new float [nelements];
/*
      int num = fread(values, sizeof(float), (int)nelements, stream); 
*/
      int num = swapio_fread_float(values, (int)nelements, stream); 
      if(num != nelements)
          {
          delete [] values;  ///// new 8/14/98
          strcpy
             (msg, "error while reading SimpleFloatArray elements from file");
          if(swapped) swapio_toggle_read_swapping_action ();
          return TRUE;
          }
      privateReset(values, nelements);
      delete [] values;  ///// new 8/14/98
      if(_numnils != numnils || _minval != minval || _maxval != maxval)
          {
          _buffer = FNIL;
          strcpy(msg, "incompatible SimpleFloatArray elements in file");
          if(swapped) swapio_toggle_read_swapping_action ();
          return TRUE;
          }
      }
  _buffer = FNIL;
  strcpy(msg, "OK");
  if(swapped) swapio_toggle_read_swapping_action ();
  return FALSE;
}



//--------------------- save to binary file --------------------------//
//--------------------- save to binary file --------------------------//
//--------------------- save to binary file --------------------------//

   // public.

int SimpleFloatArray::saveToBinaryFile(FILE *stream, char *msg)
{
  privateUpdate();
  int shrunken = (_array == NULL);
/*
  int num1 = fwrite( IDENTIFIER, NBUF         , 1, stream);
  int num2 = fwrite(&_nelements, sizeof(long) , 1, stream);
  int num3 = fwrite(&_numnils  , sizeof(long) , 1, stream);
  int num4 = fwrite(&_minval   , sizeof(float), 1, stream);
  int num5 = fwrite(&_maxval   , sizeof(float), 1, stream);
  int num6 = fwrite(&shrunken  , sizeof(int)  , 1, stream);
  if(num1 != 1 || num2 != 1 || num3 != 1 ||
     num4 != 1 || num5 != 1 || num6 != 1)
*/
  int num1 = swapio_fwrite_char ( IDENTIFIER, NBUF, stream);
  int num2 = swapio_fwrite_long (&_nelements,    1, stream);
  int num3 = swapio_fwrite_long (&_numnils  ,    1, stream);
  int num4 = swapio_fwrite_float(&_minval   ,    1, stream);
  int num5 = swapio_fwrite_float(&_maxval   ,    1, stream);
  int num6 = swapio_fwrite_int  (&shrunken  ,    1, stream);
  if(num1 != NBUF || num2 != 1 || num3 != 1 ||
     num4 != 1    || num5 != 1 || num6 != 1)
      {
      strcpy(msg, "error while writing SimpleFloatArray values to file");
      return TRUE;
      }
  if(!shrunken)
      {
/*
      int num = fwrite(_array, sizeof(float), (int)_nelements, stream); 
*/
      int num = swapio_fwrite_float(_array, (int)_nelements, stream); 
      if(num != _nelements)
          {
          strcpy(msg, "error while writing SimpleFloatArray elements to file");
          return TRUE;
          }
      }
  strcpy(msg, "OK");
  return FALSE;
}



//------------------------- private update -----------------------------//
//------------------------- private update -----------------------------//
//------------------------- private update -----------------------------//

    // does nothing if _updated is TRUE.
    // sets _updated to TRUE when finished.
    // needs these variables:
    //    _array, _nelements, _tolerance.
    // resets these variables:
    //    _minval, _maxval, _numnils,
    //    _nascending, _ndescending, _npositive, _spacing.
    // might reset _array to NULL and _nalloc to 0.

    // private.
    // update miscellaneous information.

    // whenever any array elements are inserted, removed, or changed:
    //   - _updated is automatically set to FALSE.
    //   - the array might be allocated or reallocated or deallocated.
    //   - this function might then be called after some major changes.

    // whenever certain information is requested:
    //   - this function will be called automatically if necessary.
    //   - a NULL array will be allocated if getArrayPointer is called.

    // this function does the following operations:
    //   - does nothing if _updated to TRUE.
    //   - determines _minval, _maxval, and _numnils.
    //   - determines _nascending, _ndescending, _npositive, and _spacing.
    //   - shrinks (deallocates) the array if it is equally spaced.
    //   - deallocates unused array space if not equally spaced.
    //   - sets _updated to TRUE.

void SimpleFloatArray::privateUpdate()
{
  if(_updated) return;
  if(_nelements == 0)
      {
      privateResetAndClear(0);
      return;
      }
  assert(_array);
  _minval      = private_minimum   (_array, _nelements);
  _maxval      = private_maximum   (_array, _nelements);
  _numnils     = private_num_nils  (_array, _nelements);
  _nascending  = private_ascending (_array, _nelements, _minval, _maxval);
  _ndescending = private_descending(_array, _nelements, _minval, _maxval);
  _npositive   = private_positive  (_array, _nelements, _minval);
  _spacing     = private_spacing   (_array, _nelements, _minval, _maxval,
                                                  _numnils, _tolerance);
  if(_spacing != FNIL)
      {
      delete [] _array;
      _array  = NULL;
      _nalloc = 0;
      }
  if(_nalloc > _nelements)
      {
      float *newarray = new float [_nelements];
      private_copy(newarray, _nelements, _array);
      delete [] _array;
      _array  = newarray;
      _nalloc = _nelements;
      }
  _updated = TRUE;
}



//---------------------- private reset and clear ------------------------//
//---------------------- private reset and clear ------------------------//
//---------------------- private reset and clear ------------------------//

     // private.
     // changes the number of elements.
     // sets all elements to nil.
     // does not clear buffer.
     // sets _updated to TRUE.

void SimpleFloatArray::privateResetAndClear(long nelements)
{
  assert(nelements <= _maximum);
  if(_array) delete [] _array;
  _array       = NULL;
  _nalloc      = 0;
  _nelements   = nelements;
  _minval      = FNIL;
  _maxval      = FNIL;
  _numnils     = nelements;
  _nascending  = 0;
  _ndescending = 0;
  _npositive   = 0;
  _spacing     = 0.0;
  _updated     = TRUE;
}



//--------------------------- private reset -----------------------------//
//--------------------------- private reset -----------------------------//
//--------------------------- private reset -----------------------------//

     // private.
     // changes the number of elements.
     // sets the elements to the argument array.
     // does not clear buffer.
     // sets _updated to TRUE.

void SimpleFloatArray::privateReset(const float *values, long nelements)
{
  if(nelements == 0)
      {
      privateResetAndClear(0);
      }
  else
      {
      assert(values);
      assert(nelements <= _maximum);
      if(nelements > _nalloc)
          {
          if(_array) delete [] _array;
          _array     = new float [nelements];
          _nalloc    = nelements;
          }
      _nelements = nelements;
      private_copy(_array, _nelements, values);
      _updated = FALSE;
      privateUpdate();
      }
}



//------------------------- private expand -----------------------------//
//------------------------- private expand -----------------------------//
//------------------------- private expand -----------------------------//

       // private.
       // does nothing if array is already expanded or _nelements == 0.
       // expands the array if it is NULL and _nelements > 0.
       // asserts if needs expanding and not updated.

void SimpleFloatArray::privateExpand()
{
  if(_nelements == 0 || _array) return;
  assert(_updated);
  _array  = new float [_nelements];
  _nalloc = _nelements;
  private_fill(_array, _nelements, _minval, _spacing);
}



//----------------------- delete all elements -----------------------//
//----------------------- delete all elements -----------------------//
//----------------------- delete all elements -----------------------//

        // public.
        // also clears buffer.

void SimpleFloatArray::deleteAllElements()
{
  privateResetAndClear(0);
  _buffer = FNIL;
}



//------------------- reset num elements and clear ----------------------//
//------------------- reset num elements and clear ----------------------//
//------------------- reset num elements and clear ----------------------//

     // public.
     // changes the number of elements.
     // sets all elements to nil.
     // also clears buffer.

void SimpleFloatArray::resetNumElementsAndClear(long nelements)
{
  privateResetAndClear(nelements);
  _buffer = FNIL;
}



//------------------- reset num elements and retain --------------------//
//------------------- reset num elements and retain --------------------//
//------------------- reset num elements and retain --------------------//

     // public.
     // changes the number of elements.
     // retains old values up to minimum of old and new number of elements.
     // sets any added values (above old number of elements) to nil.
     // does not clear buffer.

void SimpleFloatArray::resetNumElementsAndRetain(long nelements)
{
  if(nelements == _nelements) return;
  if(nelements == 0 || _nelements == 0)
      {
      privateResetAndClear(nelements);
      }
  else
      {
      assert(nelements <= _maximum);
      privateExpand();
      float *newarray = new float [nelements];
      for(long index = 0; index < nelements; index++)
          {
          if (index < _nelements) newarray[index] = _array[index];
          else                    newarray[index] = FNIL;
          }
      assert(_array);
      delete [] _array;
      _array     = newarray;
      _nalloc    = nelements;
      _nelements = nelements;
      _updated   = FALSE;
      privateUpdate();
      }
}



//------------------- reset num elements -----------------------------//
//------------------- reset num elements -----------------------------//
//------------------- reset num elements -----------------------------//

     // public.
     // changes the number of elements.
     // sets the elements to the argument array.
     // does not clear buffer.

void SimpleFloatArray::resetNumElements
                                (const float *values, long nelements)
{
  privateReset(values, nelements);
}



//---------------------------- copy all elements ----------------------//
//---------------------------- copy all elements ----------------------//
//---------------------------- copy all elements ----------------------//

   // public.

void SimpleFloatArray::copyAllElements(const SimpleFloatArray *object)
{
  assert(object);
  if(_array) delete [] _array;
  _array       = NULL;
  _nalloc      = 0;
  _nelements   = object->_nelements;
  _buffer      = object->_buffer;
  _minval      = object->_minval;
  _maxval      = object->_maxval;
  _numnils     = object->_numnils;
  _nascending  = object->_nascending;
  _ndescending = object->_ndescending;
  _npositive   = object->_npositive;
  _spacing     = object->_spacing;
  _updated     = object->_updated;
  assert(_nelements <= _maximum);
  if(object->_array)
      {
      assert(_nelements > 0);
      _array  = new float [_nelements];
      _nalloc = _nelements;
      private_copy(_array, _nelements, object->_array);
      }
  privateUpdate();
}


//---------------------- create and delete array pointer ------------------//
//---------------------- create and delete array pointer ------------------//
//---------------------- create and delete array pointer ------------------//

   // public.

float *SimpleFloatArray::createArrayPointer()  const
{
  float *values = new float [_nelements + 1];
  getAllValues(values);
  return values;
}

void SimpleFloatArray::deleteArrayPointer(float *values)  const
{
  if(values) delete [] values;
}


//--------------------------- get values -------------------------------//
//--------------------------- get values -------------------------------//
//--------------------------- get values -------------------------------//

   // public.

const float *SimpleFloatArray::getArrayPointer()
{
  if(_nelements == 0) return NULL;
  if(_array) return _array;
  assert(_updated);
  privateExpand();
  assert(_array || _nelements == 0);
  return _array;
}



float SimpleFloatArray::getValue(long index)  const
{
  assert(index >= 0 && index < _nelements);
  if(_array) return _array[index];
  assert(_updated);
  if(_spacing == 0.0) return _minval;
  assert(_spacing != FNIL && _minval != FNIL);
  return (_minval + _spacing * index);
}



void SimpleFloatArray::getAllValues(float *values)  const
{
  assert(values);
  if(_nelements == 0) return;
  if(_array)
      {
      private_copy(values, _nelements, _array);
      }
  else
      {
      assert(_updated);
      private_fill(values, _nelements, _minval, _spacing);
      }
}


float SimpleFloatArray::minimumValue()
{
  privateUpdate();
  return _minval;
}



float SimpleFloatArray::maximumValue()
{
  privateUpdate();
  return _maxval;
}



long  SimpleFloatArray::numNilValues()
{
  privateUpdate();
  return _numnils;
}



long  SimpleFloatArray::numLiveValues()
{
  privateUpdate();
  return (_nelements - _numnils);
}



long  SimpleFloatArray::validateAscending()
{
  privateUpdate();
  return _nascending;
}



long  SimpleFloatArray::validateDescending()
{
  privateUpdate();
  return _ndescending;
}



int  SimpleFloatArray::isAscending()
{
  privateUpdate();
  return (_nascending == _nelements);
}



int  SimpleFloatArray::isDescending()
{
  privateUpdate();
  return (_ndescending == _nelements);
}



long  SimpleFloatArray::validatePositive()
{
  privateUpdate();
  return _npositive;
}



int   SimpleFloatArray::isEquallySpaced()
{
  privateUpdate();
  return (_spacing != FNIL);
}



float SimpleFloatArray::getSpacing()
{
  privateUpdate();
  return _spacing;
}



//--------------------------- set values -------------------------------//
//--------------------------- set values -------------------------------//
//--------------------------- set values -------------------------------//

   // public.

void SimpleFloatArray::setValueToNil(long index)
{
  setValue(index, FNIL);
}



void SimpleFloatArray::setValue(long index, float value)
{
  assert(index >= 0 && index < _nelements);
  privateExpand();
  _array[index] = value;
  _updated = FALSE;
}



void SimpleFloatArray::setLastValue(float value)
{
  setValue(_nelements - 1, value);
}



void SimpleFloatArray::setAllValuesToNil()
{
  privateResetAndClear(_nelements);
}



void SimpleFloatArray::setAllValues(const float *values)
{
  privateReset(values, _nelements);
}



void SimpleFloatArray::multiplyByConstant(float constant)
{
  if(_nelements == 0) return;
  privateExpand();
  for(long index = 0; index < _nelements; index++)
      {
      if(_array[index] != FNIL) _array[index] *= constant;
      }
  _updated = FALSE;
  privateUpdate();
}



void SimpleFloatArray::addConstant(float constant)
{
  if(_nelements == 0) return;
  privateExpand();
  for(long index = 0; index < _nelements; index++)
      {
      if(_array[index] != FNIL) _array[index] += constant;
      }
  _updated = FALSE;
  privateUpdate();
}



//------------------- set or append one value ---------------------------//
//------------------- set or append one value ---------------------------//
//------------------- set or append one value ---------------------------//

          // public.

void SimpleFloatArray::setOrAppendNilValue(long index)
{
  if(index == _nelements) appendNilElement();
  else                    setValueToNil(index);
}



void SimpleFloatArray::setOrAppendValue(long index, float value)
{
  if(index == _nelements) appendElement(value);
  else                    setValue(index, value);
}



//---------------- insert or remove one element -------------------------//
//---------------- insert or remove one element -------------------------//
//---------------- insert or remove one element -------------------------//

          // public.
          // the number of elements is incremented or decremented.

void SimpleFloatArray::appendNilElement()
{
  insertNilElement(_nelements);
}


void SimpleFloatArray::appendElement (float value)
{
  insertElement(_nelements, value);
}



void SimpleFloatArray::insertNilElement (long index)
{
  insertElement(index, FNIL);
}


void SimpleFloatArray::insertElementFromBuffer (long index)
{
  insertElement(index, _buffer);
}


void SimpleFloatArray::insertElement (long index, float value)
{
  assert(index >= 0 && index <= _nelements && _nelements < _maximum);
  privateExpand();
  if(_nelements >= _nalloc)
      {
      _nalloc = _nelements + 10 + _nelements / 2;
      if(_nalloc > _maximum) _nalloc = _maximum;
      float *newarray = new float [_nalloc];
      private_copy(newarray, _nelements, _array);
      if(_array) delete [] _array;
      _array = newarray;
      }
  for(long i = _nelements; i > index; i--)
      {
      _array[i] = _array[i - 1];
      }
  _array[index] = value;
  _nelements++;
  _updated = FALSE;
}


void SimpleFloatArray::removeElement(long index)
{
  assert(index >= 0 && index < _nelements);
  privateExpand();
  _nelements--;
  for(long i = index; i < _nelements; i++)
      {
      _array[i] = _array[i + 1];
      }
  _updated = FALSE;
}


void SimpleFloatArray::removeElementToBuffer(long index)
{
  _buffer = getValue(index);
  removeElement(index);
}



//-------------------- find bracketing values -----------------------//
//-------------------- find bracketing values -----------------------//
//-------------------- find bracketing values -----------------------//

     // public.
     // works only for ascending values.

void SimpleFloatArray::findBracketingValues(float value, long *ia, long *ib)
{
long index;

  assert(ia && ib);
  privateUpdate();

////////// check for cases when -1 should be returned:

  if(_nelements == 0 ||
     value == FNIL   ||
     _numnils > 0    ||
     _nascending < _nelements)
      {
      *ia = -1;
      *ib = -1;
      return;
      }
  assert(_minval != FNIL && _maxval != FNIL);

////////// check for value at or beyond array boundaries:

  if(private_match(value, _minval, _tolerance))
      {
      *ia = 0;
      *ib = 0;
      return;
      }
  if(private_match(value, _maxval, _tolerance))
      {
      *ia = _nelements-1;
      *ib = _nelements-1;
      return;
      }
  if(value < _minval)
      {
      *ia = -1;
      *ib = 0;
      return;
      }
  if(value > _maxval)
      {
      *ia = _nelements-1;
      *ib = _nelements;
      return;
      }
  assert(_maxval > _minval);
  assert(_nelements > 1);

////////// deal with equally-spaced array:

  if(_spacing != FNIL)
      {
      assert(_spacing != 0.0 && _minval != FNIL);
      float findex = (value - _minval) / _spacing;
      index = NearestInteger(findex);
      assert(index >= 0 && index < _nelements);
      float value2 = _minval + index * _spacing;
      if(private_match(value2, value, _tolerance))
          {
          *ia = index;
          *ib = index;
          return;
          }
      *ia = (long)findex;
      *ib = *ia + 1;
      return;
      }

////////// check for exact match (within tolerance):

  assert(_array);
  for(index = 0; index < _nelements; index++)
      {
      if(private_match(_array[index], value, _tolerance))
          {
          *ia = index;
          *ib = index;
          return;
          }
      }

////////// find bracketing values:

  for(index = 1; index < _nelements; index++)
      {
      if(_array[index-1] < value && _array[index] > value)
          {
          *ia = index-1;
          *ib = index;
          return;
          }
      }
  assert(FALSE);
}



//------------------------ get interpolated value ----------------------//
//------------------------ get interpolated value ----------------------//
//------------------------ get interpolated value ----------------------//

     // public.

float SimpleFloatArray::getInterpolatedValue
                (float value, float value1, float value2, long ia, long ib)
{
  privateUpdate();

////////// check for cases when 0.0 should be returned:

  if(_nelements == 0  ||
     value  == FNIL   ||
     value1 == FNIL   ||
     value2 == FNIL   ||
     _numnils > 0)
      {
      return 0.0;
      }
  if(ia == -1 && ib == -1) return 0.0;

////////// return something meaningful:

  ia = ConstrainValue(ia, 0, _nelements-1);
  ib = ConstrainValue(ib, 0, _nelements-1);
  if(ia == ib) return getValue(ia);
  if(value1 == value2) return 0.5 * (getValue(ia) + getValue(ib));
  float spacing = (getValue(ib) - getValue(ia)) / (value2 - value1);
  return getValue(ia) + (value - value1) * spacing;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

