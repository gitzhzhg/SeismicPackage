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

//----------------------- limits_keeper.cc -----------------------------//
//----------------------- limits_keeper.cc -----------------------------//
//----------------------- limits_keeper.cc -----------------------------//

//           implementation file for the LimitsKeeper class
//                     not derived from any class
//                          subdirectory oprim


   // if the value being removed is a limiting (i.e. minimum or
   //   maximum) value, and if there are no other matching limiting
   //   values in the array, the limit will become invalid (the limits
   //   counter _nxmin or _nxmax will become zero).  if a new value
   //   is subsequently inserted which equals or exceeds this limiting
   //   value, the limit will again become valid.
   // if the limit is invalid when it is requested by minimumValue
   //   or maximumValue, the registered routine _get_value will be called
   //   for each value in the array, making the limit valid again.

   // this class may not work properly unless values which are removed
   //   are exactly the same as they were when they were inserted.


#include "oprim/limits_keeper.hh"
#include "cprim.h"
#include "named_constants.h"
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


LimitsKeeper::LimitsKeeper(GetValue *get_value, void *data)
           :
        _get_value    (get_value),
        _data         (data),
        _n            (0),
        _nlive        (0),
        _xmin         (FNIL),
        _xmax         (FNIL),
        _nxmin        (0),
        _nxmax        (0),
        _sum          (0.0)
{
  assert(get_value && data);
}



//--------------------------- destructor ------------------------------//
//--------------------------- destructor ------------------------------//
//--------------------------- destructor ------------------------------//

LimitsKeeper::~LimitsKeeper()
{
}



//-------------------------- clear values -----------------------------//
//-------------------------- clear values -----------------------------//
//-------------------------- clear values -----------------------------//

      // public.
      // when all values are to be removed at the same time, efficiency
      //   is improved by calling this routine rather than calling
      //   removeValue repeatedly.

void LimitsKeeper::clearValues()
{
  _n     = 0;
  _nlive = 0;
  _xmin  = FNIL;
  _xmax  = FNIL;
  _nxmin = 0;
  _nxmax = 0;
  _sum   = 0.0;
}



//-------------------------- get limits -------------------------------//
//-------------------------- get limits -------------------------------//
//-------------------------- get limits -------------------------------//

      // public.
      // when all values are to be inserted at the same time, it might
      //   be convenient to call this routine rather than calling
      //   insertValue repeatedly.  then, subsequent changes can be
      //   made by calling insertValue and removeValue as needed.
      // this routine is also called by minimumValue and maximumValue
      //   when necessary.

void LimitsKeeper::getLimits(long n)
{
  clearValues();
  for(long index = 0; index < n; index++)
      {
      float value = _get_value(_data, index);
      insertValue(value);
      }
}



//---------------------- minimum or maximum value ---------------------//
//---------------------- minimum or maximum value ---------------------//
//---------------------- minimum or maximum value ---------------------//

      // public.
      // call these routines to get the minimum and maximum values
      //   in the externally-owned array.
      // if the requested limiting value is invalid, these routines will
      //   call _get_value for each value in the externally-owned array.
      //   the array must contain a number of values equal to the number
      //   of times insertValue has been called minus the number of times
      //   removeValues was previously called.

float  LimitsKeeper::minimumValue()
{
  if(_nlive == 0) return 0.0;
  if(_nxmin <= 0) getLimits(_n);
  return _xmin;
}



float  LimitsKeeper::maximumValue()
{
  if(_nlive == 0) return 0.0;
  if(_nxmax <= 0) getLimits(_n);
  return _xmax;
}



//-------------------------- average value -------------------------//
//-------------------------- average value -------------------------//
//-------------------------- average value -------------------------//

      // public.

float  LimitsKeeper::averageValue()  const
{
  if(_nlive == 0) return FNIL;
  return _sum / _nlive;
}



//----------------------- insert value ---------------------------------//
//----------------------- insert value ---------------------------------//
//----------------------- insert value ---------------------------------//

      // public.
      // call this routine to notify this class anytime the specified
      //   value is being inserted into an externally-owned array.

void  LimitsKeeper::insertValue (float value)
{
  assert(_n >= 0);
  if(value != FNIL)
      {
      if     (_xmin == FNIL || value <  _xmin) { _xmin = value; _nxmin = 1; }
      else if                 (value == _xmin) { _nxmin++; }
      if     (_xmax == FNIL || value >  _xmax) { _xmax = value; _nxmax = 1; }
      else if                 (value == _xmax) { _nxmax++; }
      _sum += value;
      _nlive++;
      }
  _n++;
}



//----------------------- remove value ---------------------------------//
//----------------------- remove value ---------------------------------//
//----------------------- remove value ---------------------------------//

      // public.
      // call this routine to notify this class anytime the specified
      //   value is being removed from the externally-owned array.

void  LimitsKeeper::removeValue (float value)
{
  assert(_n > 0);
  if(value != FNIL)
      {
      if(value <= _xmin) _nxmin--;   // actually value < _xmin not expected.
      if(value >= _xmax) _nxmax--;   // actually value > _xmax not expected.
      _sum -= value;
      _nlive--;
      }
  _n--;
}



//--------------------------------- end -------------------------------//
//--------------------------------- end -------------------------------//
//--------------------------------- end -------------------------------//

