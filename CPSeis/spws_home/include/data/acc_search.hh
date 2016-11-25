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

//------------------------ acc_search.hh ---------------------//
//------------------------ acc_search.hh ---------------------//
//------------------------ acc_search.hh ---------------------//

//             header file for the AccSearch class
//               derived from the AccBase class
//                   subdirectory oprim

     // This class does a fast search for a value in a class
     //   derived from SmartArray.
     // The value to be searched is obtained by calling _get_value.
     // See the implementation file for documentation.


#ifndef _ACC_SEARCH_HH_
#define _ACC_SEARCH_HH_


#include "oprim/acc_base.hh"
#include "qsearch.h"
#include "named_constants.h"

class AccSearch : public AccBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  typedef float GetValue (void *data, long index);

  GetValue * const _get_value;

  const float _tolerance;  // tolerance for testing values for equality.
  long        _n;          // number of values to search through.
  float       _min_step;   // minimum step in adjacent values (or nil).
  float       _max_step;   // maximum step in adjacent values (or nil).
  float       _min_value;  // minimum value in array (or nil).
  float       _max_value;  // maximum value in array (or nil).
  QsearchStruct *_qsst;    // pointer to quick search object.
  long  *_clever_list;     // list of indices (index is wanted value).
  long   _clever_length;   // length of _clever_list.
  float  _remember_matching_value;
  long   _remember_matching_index;
  float  _remember_nearest_value;
  long   _remember_nearest_index;
  int    _allow_calculated_search;
  long   _last_index_found;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  AccSearch (SmartArray *array, GetValue *get_value,
                                           float tolerance = 0.001);
  virtual ~AccSearch();

public:    // get or find values

  float getMinimumStep        ()     const  { return _min_step; }
  float getMaximumStep        ()     const  { return _max_step; }

  float getMinimumValue       ()     const  { return _min_value; }
  float getMaximumValue       ()     const  { return _max_value; }

  int   findBracketingValues  (float want, long *ia, long *ib)  ;
  long  findNearestValue      (float want, int direction = 0)   ;
  long  findMatchingValue     (float want)                      ;
  long  findInsertionLocation (float want)                      ;

public:  // learn sorting information based on _n, _min_step. _max_step.

  int isSorted               () const; // n>=0 possibly some/all equal values
  int isStrictlySorted       () const; // n>=0 no equal values
  int hasAdjacentEqualValues () const; // n>=2 according to current order
  int isAscending            () const; // n>=2 possibly some equal values
  int isDescending           () const; // n>=2 possibly some equal values
  int isStrictlyAscending    () const; // n>=2 no equal values
  int isStrictlyDescending   () const; // n>=2 no equal values
  int isUniformlyConstant    () const; // n>=2 all equal values
  int isUniformlyChanging    () const; // n>=2 equal non-zero steps
  int isUniformlyAscending   () const; // n>=2 equal non-zero steps
  int isUniformlyDescending  () const; // n>=2 equal non-zero steps

private:

  virtual void  fixRange (long n, long  index1, long  index2);

private:

  void  updateMinMaxSteps    (int appended = FALSE);
  void  updateMinMaxValues   (int appended = FALSE);
  int   testForEquality      (float value1, float value2)     const;
  int   doSequentialSearch   (float want, long *ia, long *ib);
  long  doSequentialSearch2  (float want,      int direction)  const;
  int   doBinarySearch       (float want, long *ia, long *ib)  const;
  int   doCalculatedSearch   (float want, long *ia, long *ib)  const;

  long  normalizedValue                   (float value);
  void  cleverFindMatchingValueInitialize ();
  long  cleverFindMatchingValueHelper     (float want);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
