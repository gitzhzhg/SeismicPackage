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

//---------------------- acc_base.cc -----------------------//
//---------------------- acc_base.cc -----------------------//
//---------------------- acc_base.cc -----------------------//

//         implementation file for the AccBase class
//                 not derived from any class
//                     subdirectory oprim


#include "oprim/acc_base.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

   // This class is a base class with the following virtual functions
   // which should be overridden:
   //              getRange
   //              fixRange

   // This class uses or resets values in array elements in a class
   // derived from SmartArray.  The values are accessed based on any
   // desired information accessable by the derived class.


   // This class calls the following functions in SmartArray:
   //
   //    long numElements       ()
   //     int valueIsDependent  (int ident, long index)
   //    void setDependencyTrue (int ident, long index)
   //    void setDependencyFalse(int ident, long index)
   //    void valuesWillChange  (int ident, long index, long nrem, long nins)
   //    void valuesHaveChanged (int ident, long index, long nrem, long nins)


   // The following functions must be called when a value is
   // changed or an array element is inserted/removed:
   //
   //    void preChange   (long index, long nrem, long nins)
   //    void postChange  ()



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


AccBase::AccBase(SmartArray *array, int ident, int send)
           :
             _array           (array),
             _ident           (ident),
             _send            (send),
             _between         (FALSE),
             _freeze          (FALSE),
             _need            (FALSE),
             _accum           (FALSE),
             _index_keep      (0),
             _nrem_keep       (0),
             _nins_keep       (0),
             _n_new           (0)
{
  assert(_array);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

AccBase::~AccBase()
{
}



//------------------------ reg range --------------------------//
//------------------------ reg range --------------------------//
//------------------------ reg range --------------------------//

    //  public.
    //  These functions register a range which will be changed.
    //  They are needed for the case where more than one set of
    //    ranges must be combined to produce a larger inclusive
    //    range.  The range can be specified either by three numbers,
    //    or by another AccBase which knows an appropriate range.


void AccBase::regRange(AccBase *other_acc_base)
{
  long index = other_acc_base->modifiedIndex();
  long nrem  = other_acc_base->modifiedNrem ();
  long nins  = other_acc_base->modifiedNins ();
  regRange(index, nrem, nins);
}


void AccBase::regRange(long index, long nrem, long nins)
{
  assert(!_between);
  if(_freeze) return;

  long n = _array->numElements();

  assert(index >= 0 && index <= n);
  assert(nrem  >= 0 && nrem  <= n - index);
  assert(nins  >= 0);

  if(!_accum)
      {
      _index_keep = index;
      _nrem_keep  = nrem;
      _nins_keep  = nins;
      _n_new      = n;
      _accum      = TRUE;
      }
  else
      {
      assert(n           == _n_new);
      assert(nins - nrem == _nins_keep - _nrem_keep);
      long maxr = MaximumValue(index + nrem, _index_keep + _nrem_keep);
      long maxi = MaximumValue(index + nins, _index_keep + _nins_keep);
      _index_keep = MinimumValue(index, _index_keep);
      _nrem_keep  = maxr - _index_keep;
      _nins_keep  = maxi - _index_keep;
      }

  assert(_index_keep >= 0 && _index_keep <= n);
  assert(_nrem_keep  >= 0 && _nrem_keep  <= n - _index_keep);
  assert(_nins_keep  >= 0);
}



//------------------- pre and do and post change --------------------//
//------------------- pre and do and post change --------------------//
//------------------- pre and do and post change --------------------//

    //  public.

    //  This function must be called before values are changed, or
    //  before one or more array elements have been inserted or removed:
    //            preChange
    //  The preChange function can optionally be preceded by one or
    //  more calls to regRange.

    //  These two functions must be called after values are changed, or
    //  after one or more array elements have been inserted or removed:
    //            post1Change
    //            post2Change

    //  Alternatively, this function combines the work of post1Change
    //  and post2Change:
    //            postChange

    //  The preChange function determines the full range which will
    //  change, keeps necessary information, and optionally calls a
    //  virtual function in SmartArray to inform it of the full range
    //  to be changed. This full range may exceed the range implied
    //  by the input parameters.  No changes should be made in SmartArray
    //  by this function.

    //  The post1Change function optionally makes any necessary
    //  modifications to SmartArray, or calculations for its own use.

    //  The post2Change function optionally calls a virtual function
    //  in SmartArray to inform it of the range which has changed.
    //  No changes should be made in SmartArray by this function.

    //  Both virtual functions in SmartArray will receive the same
    //  argument values.

    //  If updates have been frozen, these functions simply record
    //  the need to do the update.  Otherwise, the updates are done
    //  immediately by the second function.


void AccBase::preChange(AccBase *other_acc_base)
{
  long index = other_acc_base->modifiedIndex();
  long nrem  = other_acc_base->modifiedNrem ();
  long nins  = other_acc_base->modifiedNins ();
  preChange(index, nrem, nins);
}



void AccBase::preChange(long index, long nrem, long nins)
{
  regRange(index, nrem, nins);

  _need    = TRUE;
  _between = TRUE;
  if(_freeze) return;

  preChangeHelper();

  if(_send) _array->valuesWillChange
                           (_ident, _index_keep, _nrem_keep, _nins_keep);
}

 

void AccBase::post1Change()
{
  assert(_between);
  assert(_need);
  if(_freeze) return;
  assert(_accum);

  post1ChangeHelper();
}



void AccBase::post2Change()
{
  assert(_between);
  assert(_need);
  _between = FALSE;
  if(_freeze) return;
  assert(_accum);
  _accum   = FALSE;
  _need    = FALSE;

  if(_send) _array->valuesHaveChanged
                           (_ident, _index_keep, _nrem_keep, _nins_keep);
}



void AccBase::postChange()
{
  post1Change();
  post2Change();
}



//------------------- pre and post1 change helper --------------------//
//------------------- pre and post1 change helper --------------------//
//------------------- pre and post1 change helper --------------------//

    //  private.

void AccBase::preChangeHelper()
{
  long n = _array->numElements();

  long index1 = _index_keep;
  long index2 = _index_keep + _nrem_keep - 1;

  getRange(n, &index1, &index2);                // virtual

  assert(index1 >= 0 && index1 <= _index_keep);
  assert(index2 >= _index_keep + _nrem_keep - 1 && index2 < n);

  long diff = _nins_keep - _nrem_keep;

  _index_keep = index1;
  _nrem_keep  = index2 - index1 + 1;
  _nins_keep  = _nrem_keep + diff;
  _n_new      = n + diff;
}

 

void AccBase::post1ChangeHelper()
{
  long n = _array->numElements();

  assert(n == _n_new);

  long index1 = _index_keep;
  long index2 = _index_keep + _nins_keep - 1;

  fixRange(n, index1, index2);                     // virtual
}



//----------------- freeze updates --------------------//
//----------------- freeze updates --------------------//
//----------------- freeze updates --------------------//

      //  public.

void AccBase::freezeUpdates()
{
  assert(!_between);
  assert(!_accum);
  _freeze     = TRUE;
  _index_keep = 0;
  _nrem_keep  = 0;
  _nins_keep  = 0;
}



//----------------- resume updates --------------------//
//----------------- resume updates --------------------//
//----------------- resume updates --------------------//

      //  public.
      //  This function should be called whenever any values
      //  might have to be updated.  If any values have
      //  been changed since they were previously frozen,
      //  updates will now be performed.


void AccBase::resumeUpdates()
{
  assert(!_between);
  assert(!_accum);
  _freeze = FALSE;
  if(!_need) return;
  long n = _array->numElements();
  _index_keep = 0;
  _nrem_keep = n;
  _nins_keep = n;
  fixRange(n, 0, n-1);           // virtual
  _need = FALSE;
}



//----------------- perform updates --------------------//
//----------------- perform updates --------------------//
//----------------- perform updates --------------------//

   //  public.
   //  This function should be called whenever the values
   //  must be updated, even though previous calls to the
   //  following functions may not have been diligently made:
   //     preChange   (long index, long nrem, long nins)
   //     postChange  ()


void AccBase::performUpdates()
{
  _need = TRUE;
  resumeUpdates();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

