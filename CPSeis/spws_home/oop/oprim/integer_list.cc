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

//--------------------- integer_list.cc ---------------------//
//--------------------- integer_list.cc ---------------------//
//--------------------- integer_list.cc ---------------------//

//        implementation file for the IntegerList class
//                  not derived from any class
//                      subdirectory geom



#include "oprim/integer_list.hh"
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//--------------------- set allocation steps --------------------//
//--------------------- set allocation steps --------------------//
//--------------------- set allocation steps --------------------//

      // private static variables.
      // public static function.

      // after calling this function with non-default arguments,
      // this function should be called again with no arguments
      // to restore the steps to their default values, so that
      // there will be no surprises in unrelated subsequent
      // calls to IntegerList objects.

long IntegerList::_first_step =  1;
long IntegerList::_min_step   =  1;
long IntegerList::_max_step   = 50;


void IntegerList::setAllocationSteps (long first_step,
                                      long min_step,
                                      long max_step)
{
  _first_step = first_step;
  _min_step   = min_step;
  _max_step   = max_step;
  assert(_first_step >= 1);
  assert(_min_step   >= 1);
  assert(_max_step   >= _min_step);
}



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


IntegerList::IntegerList()
           :
          _n              (0),
          _nalloc         (0),
          _array          (NULL)
{
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

IntegerList::~IntegerList()
{
  if(_array) free(_array);
}



//------------------- clear list --------------------------------//
//------------------- clear list --------------------------------//
//------------------- clear list --------------------------------//


void IntegerList::clearList()
{
  if(_array) free(_array);
  _array  = NULL;
  _n      = 0;
  _nalloc = 0;
}



//----------------------- get element -----------------------------//
//----------------------- get element -----------------------------//
//----------------------- get element -----------------------------//

long IntegerList::getElement(long index)   const
{
  assert(index >= 0 && index < _n && _array);
  return _array[index];
}



//----------------------- add element ----------------------------//
//----------------------- add element ----------------------------//
//----------------------- add element ----------------------------//


void IntegerList::addElement(long element)
{
  if(_n == _nalloc)
      {
      long step;
      if(_n == 0)
          {
          step = _first_step;
          }
      else
          {
          step = _n;
          if     (step < _min_step) step = _min_step;
          else if(step > _max_step) step = _max_step;
          }
      assert(step > 0);
      long nalloc2 = _n + step;
      long *array2;
      if(_array) array2 = (long*)realloc(_array, (int)nalloc2 * sizeof(long));
      else       array2 = (long*) malloc(        (int)nalloc2 * sizeof(long));
      if(!array2) return;
      _array  = array2;
      _nalloc = nalloc2;
      }
  assert(_array && _n < _nalloc);
  _array[_n++] = element;
}



//-------------------- trim allocation ---------------------------//
//-------------------- trim allocation ---------------------------//
//-------------------- trim allocation ---------------------------//

      // optional.
      // call this to free excess allocated space.

void IntegerList::trimAllocation()
{
  if(_nalloc == _n) return;
  assert(_nalloc > _n && _n > 0 && _array);
  long *array2 = (long*)realloc(_array, (int)_n * sizeof(long));
  if(!array2) return;
  _array  = array2;
  _nalloc = _n;
}



//--------------------- static routines -----------------------//
//--------------------- static routines -----------------------//
//--------------------- static routines -----------------------//

        // public.


long IntegerList::numElements(IntegerList *list)
{
  if(!list) return 0;
  return list->_n;
//return list->numElements();
}


long *IntegerList::getListPointer(IntegerList *list)
{
  if(!list) return NULL;
  return list->_array;
//return list->getListPointer();
}


long IntegerList::getElement(IntegerList *list, long index)
{
  assert(list);
  return list->getElement(index);
}


IntegerList *IntegerList::addElement(IntegerList *list, long element)
{
  if(!list) list = new IntegerList();
  list->addElement(element);
  return list;
}


IntegerList *IntegerList::clearList(IntegerList *list)
{
  if(list) delete list;
  return NULL;
}


void IntegerList::trimAllocation(IntegerList *list)
{
  if(list) list->trimAllocation();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

