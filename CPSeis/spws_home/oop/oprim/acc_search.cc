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

//---------------------- acc_search.cc -----------------------//
//---------------------- acc_search.cc -----------------------//
//---------------------- acc_search.cc -----------------------//

//         implementation file for the AccSearch class
//                 not derived from any class   
//                     subdirectory oprim


#include "oprim/acc_search.hh"
#include "cprim.h"
#include "qsearch.h"
#include "named_constants.h"
#include "binary_search.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


   // This class searches for the index of a specified value
   // among the array elements in a class derived from SmartArray.
   // The value to be used is identified by an ident.
   // The search can return the index of the nearest value,
   // the index of the matching value, or the indices of
   // bracketing values.

   // The most efficient possible search mechanism is used.
   // The search is a calculated search if all values are
   // linear with the index.  Otherwise a binary search is
   // performed if the values are monotonic.  Otherwise a
   // sequential search is performed.

   // Later, this class might be enhanced to subdivide the range
   // of indices into several sub-ranges which separately might
   // be searched much faster than the whole range as a unit.
   // For example, the range may consist of separate ranges
   // which have values linear with index, such that a search
   // may involve a few calculations rather than a sequential
   // search through the values of all the array elements.

   // This class maintains information which is used to do the
   // fastest possible search for a value.  This information
   // must be updated as necessary (when elements are inserted
   // into the array, removed from the array, or sorted, or
   // when the value in an array element is changed).

   // The philosophy behind this class is that the values
   // to be searched are rarely changed, but the search is
   // performed many times.

   // The following functions must be called when a value is
   // changed or an array element is inserted/removed,
   // respectively:
   //
   //    void notifyValueChanged (int ident, long index)
   //    void notifyRemoveInsert (long index, long nrem, long nins)



static const float FZERO = 0.0;


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccSearch::AccSearch(SmartArray *array, GetValue *get_value,
                                              float tolerance)
           : AccBase(array, 0, FALSE),
             _get_value               (get_value),
             _tolerance               (tolerance),
             _n                       (0),
             _min_step                (FNIL),
             _max_step                (FNIL),
             _min_value               (FNIL),
             _max_value               (FNIL),
             _qsst                    (NULL),    // reset below
             _clever_list             (NULL),
             _clever_length           (0),
             _remember_matching_value (FZERO),
             _remember_matching_index (-1),
             _remember_nearest_value  (FZERO),
             _remember_nearest_index  (-1),
             _allow_calculated_search (FALSE),
             _last_index_found        (0)           // new 9/24/96
{
  assert(_get_value);
  assert(_tolerance >= FZERO);
  _qsst = qsearch_create();
  qsearch_register_function(_qsst, _get_value, _array);
  qsearch_set_tolerance    (_qsst, _tolerance);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccSearch::~AccSearch()
{
  qsearch_destroy(_qsst);
  if(_clever_list) delete [] _clever_list;
}



//--------------------- fix range ---------------------------//
//--------------------- fix range ---------------------------//
//--------------------- fix range ---------------------------//

       // private virtual function

void AccSearch::fixRange(long n, long /*index1*/, long /*index2*/)
{
  long index = modifiedIndex();
  long nrem  = modifiedNrem ();
  long nins  = modifiedNins ();
  int appended = (index == n - 1 && nrem == 0 && nins == 1);
  updateMinMaxSteps (appended);
  updateMinMaxValues(appended);
  cleverFindMatchingValueInitialize();
  _remember_matching_value = FZERO;
  _remember_matching_index = -1;
  _remember_nearest_value  = FZERO;
  _remember_nearest_index  = -1;
}



//---------------- test for equality --------------------------//
//---------------- test for equality --------------------------//
//---------------- test for equality --------------------------//

    // private.
    // returns  1 if value2 >  value1.
    // returns  0 if value2 == value1 within tolerance.
    // returns -1 if value2 <  value1.

int AccSearch::testForEquality(float value1, float value2)  const
{
  if(value1 < value2 - _tolerance) return -1;
  if(value1 > value2 + _tolerance) return  1;
  return 0;
}



//-------------------- update min max steps --------------------------//
//-------------------- update min max steps --------------------------//
//-------------------- update min max steps --------------------------//

// This private function sets the following:
//   _min_step   (positive or negative or zero)  (or nil if _n<=1)
//   _max_step   (positive or negative or zero)  (or nil if _n<=1)
//   _allow_calculated_search;

// This private function must be called at the following times:
//    (1) right after an array element is put into the array.
//    (2) right after an array element is removed from the array.
//    (3) right after the array is sorted.
//    (4) right after the relevant value of any array element is reset.

// This function is fast when an array element is appended, but must
// search through the relevant values of all array elements otherwise.


void AccSearch::updateMinMaxSteps(int appended)
{
  long n = _array->numElements();
  assert(n >= 0);
  assert(!appended || n == _n + 1);
  _n = n;
  if(_n <= 1)
      {
      _min_step = FNIL;  // no array elements or only one array element
      _max_step = FNIL;  // no array elements or only one array element
      _allow_calculated_search = FALSE;
      }
  else
      {
      if(appended && _n >= 3)
          {
          float step = _get_value(_array, _n - 1) -
                       _get_value(_array, _n - 2);
          if(_allow_calculated_search)
              {
              _allow_calculated_search = (step == _min_step &&
                                          step == _max_step);
              }
          _min_step = MinimumValue(step, _min_step);
          _max_step = MaximumValue(step, _max_step);
          }
      else
          {
          _min_step = _get_value(_array, 1) -
                      _get_value(_array, 0);
          _max_step = _min_step;
          for(long index = 2; index < _n; index++)
              {
              float step = _get_value(_array, index) -
                           _get_value(_array, index - 1);
              _min_step = MinimumValue(step, _min_step);
              _max_step = MaximumValue(step, _max_step);
              }
          _allow_calculated_search = (_min_step == _max_step);
          }
      if(testForEquality(_min_step,     FZERO) == 0) _min_step = FZERO;
      if(testForEquality(_max_step,     FZERO) == 0) _max_step = FZERO;
      if(testForEquality(_min_step, _max_step) == 0) _max_step = _min_step;
      }
}




//-------------------- update min max values --------------------------//
//-------------------- update min max values --------------------------//
//-------------------- update min max values --------------------------//

// This private function sets the following:
//   _min_value   (or nil if _n==0)
//   _max_value   (or nil if _n==0)

// This private function must be called at the following times:
//    (1) right after an array element is put into the array.
//    (2) right after an array element is removed from the array.
//    (3) right after the array is sorted.
//    (4) right after the relevant value of any array element is reset.

// This function is fast when an array element is appended, but must
// search through the relevant values of all array elements otherwise.


void AccSearch::updateMinMaxValues(int appended)
{
  long n = _array->numElements();
  assert(n >= 0);
// assert(!appended || n == _n + 1); // already done by updateMinMaxSteps
// _n = n;                           // already done by updateMinMaxSteps
  if(_n == 0)
      {
      _min_value = FNIL;  // no array elements
      _max_value = FNIL;  // no array elements
      }
  else
      {
      if(appended && _n >= 2)
          {
          float value = _get_value(_array, _n - 1);
          _min_value = MinimumValue(value, _min_value);
          _max_value = MaximumValue(value, _max_value);
          }
      else
          {
          _min_value = _get_value(_array, 0);
          _max_value = _min_value;
          for(long index = 1; index < _n; index++)
              {
              float value = _get_value(_array, index);
              _min_value = MinimumValue(value, _min_value);
              _max_value = MaximumValue(value, _max_value);
              }
          }
      }
}



//------------------- learn sorting information ---------------//
//------------------- learn sorting information ---------------//
//------------------- learn sorting information ---------------//

             // these functions return TRUE or FALSE.

int AccSearch::isSorted()  const
{
  if(_n <= 1) return TRUE;
  if(_min_step >= FZERO && _max_step >= FZERO) return TRUE;
  if(_min_step <= FZERO && _max_step <= FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isStrictlySorted()  const
{
  if(_n <= 1) return TRUE;
  if(_min_step > FZERO && _max_step > FZERO) return TRUE;
  if(_min_step < FZERO && _max_step < FZERO) return TRUE;
  return FALSE;
}


int AccSearch::hasAdjacentEqualValues()  const
{
  if(_n <= 1) return FALSE;
  if(_min_step == FZERO || _max_step == FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isAscending()  const
{
  if(_n <= 1) return FALSE;
  if(_min_step >= FZERO && _max_step >= FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isDescending()  const
{
  if(_n <= 1) return FALSE;
  if(_min_step <= FZERO && _max_step <= FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isStrictlyAscending()  const
{
  if(_n <= 1) return FALSE;
  if(_min_step > FZERO && _max_step > FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isStrictlyDescending()  const
{
  if(_n <= 1) return FALSE;
  if(_min_step < FZERO && _max_step < FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isUniformlyConstant()  const
{
  if(_n <= 1) return FALSE;
  if(!_allow_calculated_search) return FALSE;
  if(_min_step == FZERO && _max_step == FZERO) return TRUE;
  return FALSE;
}


int AccSearch::isUniformlyChanging()  const
{
  if(_n <= 1) return FALSE;
  if(!_allow_calculated_search) return FALSE;
  if(_min_step == FZERO || _max_step == FZERO) return FALSE;
  if(_min_step == _max_step) return TRUE;
  return FALSE;
}


int AccSearch::isUniformlyAscending()  const
{
  if(_n <= 1) return FALSE;
  if(!_allow_calculated_search) return FALSE;
  if(_min_step <= FZERO || _max_step <= FZERO) return FALSE;
  if(_min_step == _max_step) return TRUE;
  return FALSE;
}


int AccSearch::isUniformlyDescending()  const
{
  if(_n <= 1) return FALSE;
  if(!_allow_calculated_search) return FALSE;
  if(_min_step >= FZERO || _max_step >= FZERO) return FALSE;
  if(_min_step == _max_step) return TRUE;
  return FALSE;
}



//--------------------- private functions ---------------------//
//--------------------- private functions ---------------------//
//--------------------- private functions ---------------------//

  // The following private functions are called directly from
  // findBracketingValues, and indirectly from findNearestValue
  // and findMatchingValue and findInsertionLocation:
  //                  doSequentialSearch
  //                  doBinarySearch
  //                  doCalculatedSearch

  // All of these functions return the following:
  //  (1) a flag with one of the following values
  //       (defined in binary_search.h):
  //         BIN_SEARCH_NO_VALUES
  //         BIN_SEARCH_EXTRAP_DOWN
  //         BIN_SEARCH_EXTRAP_UP
  //         BIN_SEARCH_EXACT_MATCH
  //         BIN_SEARCH_INTERPOLATE
  //  (2) indices with values bracketing the specified value.

  // WARNING: if there are more than one equal value, which 
  // value is found is undefined.



//--------------------- do sequential search2 --------------------//
//--------------------- do sequential search2 --------------------//
//--------------------- do sequential search2 --------------------//

   // the data can be in any unsorted order.
   // special version of doSequentialSearch to find nearest value.

   // if this is true   returns this index
   // ---------------   ------------------
   //     n == 0              -1
   //     n == 1               0
   //  direction == 0   nearest value
   //  direction >  0   nearest value >= want (otherwise nearest value)
   //  direction <  0   nearest value <= want (otherwise nearest value)


long AccSearch::doSequentialSearch2(float want, int direction)  const
{
  // if(_n == 0) return -1;  // unnecessary
  // if(_n == 1) return  0;  // unnecessary
  float best_distance = FZERO;
  float best_directed_distance = FZERO;
  long   best_index = -1, best_directed_index = -1;
  for(long index = 0; index < _n; index++)
      {
      float value = _get_value(_array, index);
      int    test = testForEquality(want, value);
      if(test == 0) return index;
      if(best_directed_index == -1)
          {
          float distance = AbsoluteValue(value - want);
          if(best_index == -1 || distance < best_distance)
              {
              best_index = index;
              best_distance = distance;
              }
          }
      if(direction > 0 && value > want)
          {
          float distance = value - want;
          if(best_directed_index == -1 || distance < best_directed_distance)
              {
              best_directed_index = index;
              best_directed_distance = distance;
              }
          }
      else if(direction < 0 && value < want)
          {
          float distance = want - value;
          if(best_directed_index == -1 || distance < best_directed_distance)
              {
              best_directed_index = index;
              best_directed_distance = distance;
              }
          }
      }
  if(best_directed_index != -1) return best_directed_index;
  return best_index;
}



//--------------------- do sequential search --------------------//
//--------------------- do sequential search --------------------//
//--------------------- do sequential search --------------------//

       // the data can be in any unsorted order.
       // the search starts at the last matching location found.
       // loop over index was changed to loop over index2 9/24/96.
       // _last_index_found was added 9/24/96.

int AccSearch::doSequentialSearch(float want, long *ia, long *ib) // not const
{
  if(_n == 0)
      {
      *ia = 0;
      *ib = 0;
      return BIN_SEARCH_NO_VALUES;
      }
  float smallest_distance = FZERO;
  float best_value;
  long  best_index;
  if(_last_index_found >= _n) _last_index_found = 0;       // new 9/24/96
  for(long index2 = 0; index2 < _n; index2++)          // changed 9/24/96
      {
      long index = index2 + _last_index_found;             // new 9/24/96
      if(index >= _n) index -= _n;                         // new 9/24/96
      float value = _get_value(_array, index);
      int    test = testForEquality(want, value);
    ///  *ia = index;
    ///  *ib = index;
      if(test == 0)
          {
          *ia = index;
          *ib = index;
          _last_index_found = index;                       // new 9/24/96
          return BIN_SEARCH_EXACT_MATCH;
          }
      if(_n == 1)
          {
          *ia = index;
          *ib = index;
          if(want > value) return BIN_SEARCH_EXTRAP_UP;
          return BIN_SEARCH_EXTRAP_DOWN;
          }
      float distance = AbsoluteValue(value - want);
      if(index2 == 0 || distance < smallest_distance)
          {
          best_index = index;
          best_value = value;
          smallest_distance = distance;
          }
      }
  if(best_index == 0)
      {
      *ia = best_index;
      *ib = best_index + 1;
      float value = _get_value(_array, *ib);
      if(want > best_value && want < value) return BIN_SEARCH_INTERPOLATE;
      if(want < best_value && want > value) return BIN_SEARCH_INTERPOLATE;
      *ib = *ia;
      return BIN_SEARCH_EXTRAP_DOWN;
      }
  if(best_index == _n - 1)
      {
      *ia = best_index;
      *ib = best_index - 1;
      float value = _get_value(_array, *ib);
      if(want > best_value && want < value) return BIN_SEARCH_INTERPOLATE;
      if(want < best_value && want > value) return BIN_SEARCH_INTERPOLATE;
      *ib = *ia;
      return BIN_SEARCH_EXTRAP_UP;
      }
  *ia = best_index - 1;
  *ib = best_index + 1;
  float v1 = _get_value(_array, *ia);
  float v2 = _get_value(_array, *ib);
  if     (want > v1 && want < best_value) *ib = best_index;
  else if(want < v1 && want < best_value) *ib = best_index;
  else if(want > v2 && want < best_value) *ia = best_index;
  else if(want < v2 && want < best_value) *ia = best_index;
  else if(             want > best_value) *ia = best_index;
  else                                    *ib = best_index;
  return BIN_SEARCH_INTERPOLATE;
}



//--------------------- do binary search --------------------//
//--------------------- do binary search --------------------//
//--------------------- do binary search --------------------//

  // the data must be sorted by the relevant value.
  // See qsearch_object.c in ~spws/util/cprim for more information.

int AccSearch::doBinarySearch(float want, long *ia, long *ib)  const
{
  if(isDescending()) qsearch_set_descending(_qsst);
  else               qsearch_set_ascending (_qsst);
  int flag = qsearch_perform(_qsst, want, _n);
  *ia = qsearch_ia(_qsst);
  *ib = qsearch_ib(_qsst);
  return flag;
}



//--------------------- do calculated search --------------------//
//--------------------- do calculated search --------------------//
//--------------------- do calculated search --------------------//

    // there must be at least 2 elements.
    // the relevant values must be uniformly ascending or descending.
    // _allow_calculated_seacrh must be TRUE.


int AccSearch::doCalculatedSearch(float want, long *ia, long *ib)  const
{
  assert(_n >= 2);
  assert(isUniformlyChanging());
  float findex = (want - _get_value(_array, 0)) / _min_step;
  long   index = NearestInteger(findex);
  if(index < 0)
      {
      *ia = 0;
      *ib = 0;
      return BIN_SEARCH_EXTRAP_DOWN;
      }
  if(index >= _n)
      {
      *ia = _n - 1;
      *ib = _n - 1;
      return BIN_SEARCH_EXTRAP_UP;
      }
  float  value = _get_value(_array, index);
  float value2 = _get_value(_array, 0) + index * _min_step;
  assert(testForEquality(value2, value) == 0);
  int     test = testForEquality(want, value);
  *ia          = index;
  *ib          = index;
  if(test == 0) return BIN_SEARCH_EXACT_MATCH;
  if(isDescending()) test = -test;
  if(test < 0)
      {
      if(index == 0) return BIN_SEARCH_EXTRAP_DOWN;
      (*ia)--;
      return BIN_SEARCH_INTERPOLATE;
      }
  if(test > 0)
      {
      if(index == _n - 1) return BIN_SEARCH_EXTRAP_UP;
      (*ib)++;
      return BIN_SEARCH_INTERPOLATE;
      }
  assert(FALSE);
  return BIN_SEARCH_EXACT_MATCH;   // redundant
}



//------------------ find bracketing values ------------------//
//------------------ find bracketing values ------------------//
//------------------ find bracketing values ------------------//

  // public.
  // returns flag with one of the following values:
  //   BIN_SEARCH_NO_VALUES
  //   BIN_SEARCH_EXTRAP_DOWN
  //   BIN_SEARCH_EXTRAP_UP
  //   BIN_SEARCH_EXACT_MATCH
  //   BIN_SEARCH_INTERPOLATE
  // returns indices with values bracketing the specified value.
  // uses the most efficient search method.


int AccSearch::findBracketingValues(float want, long *ia, long *ib) //not const
{
  if(isUniformlyChanging()) return doCalculatedSearch(want, ia, ib);
  if(isSorted           ()) return doBinarySearch    (want, ia, ib);
                            return doSequentialSearch(want, ia, ib);
}



//----------------- find nearest value ---------------------//
//----------------- find nearest value ---------------------//
//----------------- find nearest value ---------------------//

  // public.
  // returns index with value nearest the specified value.
  // if there are no array elements, returns -1.
  // if direction > 0, guarantees that nearest value >= want.
  // if direction < 0, guarantees that nearest value <= want.


long AccSearch::findNearestValue(float want, int direction)  // not const
{
  if(_n == 0) return -1;
  if(_n == 1) return 0;
  if(_remember_nearest_index >= 0)
      {
      if(testForEquality(want, _remember_nearest_value) == 0)
                                   return _remember_nearest_index;
      }
  _remember_nearest_value = want;
  long ia, ib;
  if(!isUniformlyChanging() && !isSorted())
      {
      ia = doSequentialSearch2(want, direction);
      _remember_nearest_index = ia;
      return ia;
      }
  int flag = findBracketingValues(want, &ia, &ib);
  if(ia == ib)
      {
      _remember_nearest_index = ia;
      return ia;
      }
  float value1 = _get_value(_array, ia);
  float value2 = _get_value(_array, ib);
  if(direction > 0)
      {
      if(value1 >= want)
          {
          _remember_nearest_index = ia;
          return ia;
          }
      _remember_nearest_index = ib;
      return ib;
      }
  if(direction < 0)
      {
      if(value1 <= want)
          {
          _remember_nearest_index = ia;
          return ia;
          }
      _remember_nearest_index = ib;
      return ib;
      }
  if(AbsoluteValue(value1 - want) <
     AbsoluteValue(value2 - want))
      {
      _remember_nearest_index = ia;
      return ia;
      }
  _remember_nearest_index = ib;
  return ib;
}



//----------------- find matching value ---------------------//
//----------------- find matching value ---------------------//
//----------------- find matching value ---------------------//

  // public.
  // returns index with value matching the specified value.
  // if there are no matching values, returns -1.


long AccSearch::findMatchingValue(float want)  // not const
{
  long index = cleverFindMatchingValueHelper(want);
  if(index >= -1) return index;
  if(_n == 0) return -1;
  if(_remember_matching_index >= 0)
      {
      if(testForEquality(want, _remember_matching_value) == 0)
                                   return _remember_matching_index;
      }
  _remember_matching_value = want;
  long ia, ib;
  int flag = findBracketingValues(want, &ia, &ib);
  if(flag == BIN_SEARCH_EXACT_MATCH)
      {
      _remember_matching_index = ia;
      return ia;
      }
  _remember_matching_index = -1;
  return -1;
}



//------------------- find insertion location ------------------//
//------------------- find insertion location ------------------//
//------------------- find insertion location ------------------//

  // public.
  // returns index where value should be inserted.
  // never returns -1.

long AccSearch::findInsertionLocation(float want)  // not const
{
  long ia, ib, index;
  int flag = findBracketingValues(want, &ia, &ib);
  switch(flag)
      {
      case BIN_SEARCH_NO_VALUES  :  index = ia;      break;
      case BIN_SEARCH_EXTRAP_DOWN:  index = ia;      break;
      case BIN_SEARCH_EXTRAP_UP  :  index = ia + 1;  break;
      case BIN_SEARCH_EXACT_MATCH:  index = ia;      break;
      case BIN_SEARCH_INTERPOLATE:  index = ib;      break;
      default:                      assert(FALSE);
      }
  return index;
}



//--------------------- normalized value ---------------------------//
//--------------------- normalized value ---------------------------//
//--------------------- normalized value ---------------------------//

     // private.
     // returns normalized value corresponding to the input argument.
     // this normalized value is to be used as an index into the
     //   clever list.
     // if the normalized value is out of range, returns -1.

long AccSearch::normalizedValue(float value)
{
  assert(_min_step != FZERO);
  assert(_min_step != FNIL);
  long norm_value = NearestInteger(  (value - _min_value) /
                                     AbsoluteValue(_min_step)  );
  if(norm_value < 0 || norm_value >= _clever_length) return -1;
  return norm_value;
}



//---------------- clever find matching value initialize ----------------//
//---------------- clever find matching value initialize ----------------//
//---------------- clever find matching value initialize ----------------//

     // private.
     // fast way to find matching value.
     // not used if the clever list would be more than BIG_FACTOR times
     //   as long as the array, or as long or longer than TOO_BIG.
     // not used if the number of elements in the array is smaller than
     //   LOWER_LIMIT.

#define TOO_BIG     5000
#define BIG_FACTOR    10
#define LOWER_LIMIT    4

void AccSearch::cleverFindMatchingValueInitialize()
{
  if(_clever_list) delete [] _clever_list;
  _clever_list = NULL;
  _clever_length = 0;
  if(_n < LOWER_LIMIT) return;
  if(_min_step != 1.0) return;  // added 10/5/95
  if(_min_step == FZERO) return;
  if(_min_step == FNIL) return;
  long clever_length = NearestInteger(  (_max_value - _min_value) /
                                         AbsoluteValue(_min_step)  ) + 1;
  if(clever_length >= TOO_BIG) return;
  if(clever_length > BIG_FACTOR * _n) return;
  _clever_list = new long [clever_length];
  if(!_clever_list) return;
  _clever_length = clever_length;
  for(long norm_value = 0; norm_value < _clever_length; norm_value++)
      {
      _clever_list[norm_value] = -1;
      }
  for(long index = 0; index < _n; index++)
      {
      float     value = _get_value(_array, index);
      long norm_value = normalizedValue(value);
      assert(norm_value != -1);
      if(_clever_list[norm_value] == -1)
          _clever_list[norm_value] = index;
      }
}



//---------------- clever find matching value helper ----------------//
//---------------- clever find matching value helper ----------------//
//---------------- clever find matching value helper ----------------//

     // private.
     // fast way to find matching value.
     // returns index of matching value.
     // returns -1 if match not found.
     // returns -2 if this clever method is not being used.

long AccSearch::cleverFindMatchingValueHelper(float want)
{
  if(!_clever_list) return -2;
  assert(_clever_length > 0);
  long norm_value = normalizedValue(want);
  if(norm_value == -1) return -1;
  long index = _clever_list[norm_value];
  if(index == -1) return -1;
  float value = _get_value(_array, index);
  if(testForEquality(value, want) == 0) return index;
  return -1;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

