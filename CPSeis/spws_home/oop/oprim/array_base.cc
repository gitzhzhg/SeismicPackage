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

//---------------------- array_base.cc -----------------------//
//---------------------- array_base.cc -----------------------//
//---------------------- array_base.cc -----------------------//

//         implementation file for the ArrayBase class
//                 not derived from any class
//                     subdirectory oprim


   // This is a base class for all arrays.  It manages the
   // insertion and deletion of array elements, and all required
   // memory allocation/reallocation.  It contains virtual
   // functions, some of which must be overridden in derived
   // classes.

   // This base class does not know anything about the array
   // elements it manages.  The derived class can use anything
   // it wants for array elements, such as pointers to objects,
   // floating point numbers, or actual objects of any size.

   // In principle, this base class can be used without derivation.
   // However, to enforce type-matching in public function prototypes,
   // the potentially-public functions which contain the <element> or
   // <elements> argument are protected in order to force the derived
   // class to provide new functions (which can have the same names)
   // with the appropriate type conversions.  These new functions
   // should then simply pass through to the base class functions.
   // See details below.

   // The <element> variable is always a (void*) pointer to a block
   // of memory containing the contents of one array element, with
   // size specified by an argument to the constructor.  The <elements>
   // variable is a similar (void*) pointer to a block of code
   // containing the contents of one or more array elements.  A block
   // of array elements is always stored contiguously.  When the
   // <element> or <elements> name appears in an argument list,
   // it always points to a copy of the array elements (or data which
   // will be inserted into the array, or removed from the array),
   // never a pointer into the array itself.


#include "oprim/array_base.hh"
#include "cprim.h"
#include "named_constants.h"
#include "memory.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


///////#define CHECK_ALLOC


enum { REMOVING_TO_BUFFER, DELETING_FROM_ARRAY,
       DELETING_FROM_BUFFER, INSERTED_FROM_BUFFER,
       INSERTED_FROM_LIST, CREATED_IN_ARRAY };



//-------------------- constructor -------------------------//
//-------------------- constructor -------------------------//
//-------------------- constructor -------------------------//

                      // protected

ArrayBase::ArrayBase(int size, long istep, long nstep)
              :
                _size       (size),
                _istep      (istep),
                _nstep      (nstep),
                _buffer     (malloc(_size)),
                _array      (NULL),
                _nalloc     (0),
                _n          (0),
                _active     (-1),
                _empty      (TRUE)
{
  assert(_size  >= 1);
  assert(_istep >= 1);
  assert(_nstep >= 1);
  assert(_buffer);
  memset(_buffer, 0, _size);
}



//---------------------- destructor ------------------------//
//---------------------- destructor ------------------------//
//---------------------- destructor ------------------------//

                         // public

        // WARNING:
        // The function removeAllElements must be called
        // from the destructor of all derived classes,
        // since it calls virtual functions.  It will
        // first be called from the most derived class.
        // All subsequent calls to removeAllElements (by
        // the destructors of less derived classes) will
        // then do nothing.

ArrayBase::~ArrayBase()
{
  removeAllElements();
  free(_buffer);
}



//-------------------- set active index ----------------------//
//-------------------- set active index ----------------------//
//-------------------- set active index ----------------------//

                          // public

void ArrayBase::setActiveIndex(long index)
{
  if(_n == 0) index = -1;
  else        index = ConstrainValue(index, 0, _n - 1);
//////  if(_active == index) return;
//////  we want to send a message even if the active index does not change.
  beforeNewActiveIndex();
  _active = index;
  afterNewActiveIndex();
}



/*********************

      not yet implemented

//------------------ set element value or values ---------------//
//------------------ set element value or values ---------------//
//------------------ set element value or values ---------------//

                 // public

void ArrayBase::setElementValue(int ident, long index, double value)
{
  beforeValuesChanged(ident, index, 1);
  if(value == DNIL) setDependentValue(ident, index, value);
  else              setValue         (ident, index, value);
  afterValuesChanged(ident, index, 1);
}


void ArrayBase::setElementValues
                       (int ident, long index, long nchng, double *values)
{
  assert(values);
  beforeValuesChanged(ident, index, nchng);
  for(long i = index; i < index + nchng; i++)
      {
      double value = values[i - index];
      if(value == DNIL) setDependentValue(ident, i, value);
      else              setValue         (ident, i, value);
      }
  afterValuesChanged(ident, index, nchng);
}
*********************/



//------------------------ clear buffer ---------------------//
//------------------------ clear buffer ---------------------//
//------------------------ clear buffer ---------------------//

                           // public

// The buffer is a temporary storage place for an array element
// which has been removed from the array and may subsequently
// be re-inserted into the array.  The buffer cannot be accessed
// publically, except by this function which clears it.

// An element cannot simultaneously exist both in the array and in
// the buffer.  The buffer is affected by calling these functions:
//
// clearBuffer:
//      (a) If the buffer is flagged as empty, nothing happens.
//      (b) Otherwise, elementDeletingFromBuffer() is called, and
//            then the buffer is zero-filled and flagged as empty.
//
// insertElementFromBuffer:
//      (a) If the buffer is flagged as empty, a zero-filled
//            element is inserted into the array, and then
//            elementCreatedInArray(index) is called.
//      (b) Otherwise, the contents of the buffer is inserted into the
//            array, elementInsertedFromBuffer(index) is called, and
//            the buffer is zero-filled and flagged as empty.
//
// removeElementToBuffer:
//      (a) First, clearBuffer is called (see above).
//      (b) Then, elementRemovingToBuffer(index) is called, the
//            element is removed from the array and placed into
//            the buffer, and the buffer is flagged as not empty.


void ArrayBase::clearBuffer()
{
  if(_empty) return;
  elementDeletingFromBuffer();
  memset(_buffer, 0, _size);
  _empty = TRUE;
}



//----------------- remove all elements ---------------------//
//----------------- remove all elements ---------------------//
//----------------- remove all elements ---------------------//

             // public
             // the buffer is also cleared.

void ArrayBase::removeAllElements()
{
  privateMove(0, _n, 0, NULL, DELETING_FROM_ARRAY);
  assert(_n == 0);
  if(_array) free(_array);
  _array  = NULL;
  _nalloc = 0;
  _active = -1;
  clearBuffer();
}



//--------------------- get or set value -----------------------//
//--------------------- get or set value -----------------------//
//--------------------- get or set value -----------------------//

                  // public virtual functions

// These must be overridden by a derived class if they are to be used.
// Otherwise they will assert.

// These functions get or return a value in an array element
// of specified index.  Which value is accessed depends on the
// ident argument, which might be an enum in the derived class,
// or an enum in the array element class.

// These functions can be used by the derived class for any purpose.

// If the value to be accessed is a float or integer value,
// it should be cast to or from type double as necessary.

// getValue should return the value whether or not it is flagged
//   as dependent (see below).

// setValue should set the value, and then set the dependency
//   flag to FALSE (see below).


double ArrayBase::getValue(int /*ident*/, long index)  const
{
  long n = numElements();            // put into overriding function.
  assert(index >= 0 && index < n);   // put into overriding function.
  assert(FALSE);                    // remove in overriding function.
  return 0.0;                      // replace in overriding function.
}


void ArrayBase::setValue(int /*ident*/, long index, double /*value*/)
{
  long n = numElements();            // put into overriding function.
  assert(index >= 0 && index < n);   // put into overriding function.
  assert(FALSE);                    // remove in overriding function.
  //// here set the new value.
  //// here set the dependency flag to FALSE.
}




//------------------- set dependent value --------------------------//
//------------------- set dependent value --------------------------//
//------------------- set dependent value --------------------------//

                     // public virtual function

// This must be overridden by a derived class if it is to be used.
// Otherwise it will assert.

     // This function must set the value of a dependent value (such
     //   as a value calculated or interpolated from other values).
     // This function must also set the dependency flag to TRUE
     //   for this value.
     // It should assert if the index is outside the valid range.


void ArrayBase::setDependentValue(int /*ident*/, long index, double /*value*/)
{
  long n = numElements();                 // put into overriding function.
  assert(index >= 0 && index < n);        // put into overriding function.
  assert(FALSE);                         // remove in overriding function.
  //// here set the new value.
  //// here set the dependency flag to TRUE.
}



//---------------------- value is dependent ---------------------//
//---------------------- value is dependent ---------------------//
//---------------------- value is dependent ---------------------//

                     // public virtual function

// This must be overridden by a derived class if it is to be used.
// Otherwise it will assert.

     // This is a protected virtual function which must be
     // overridden in a derived class if that class wants the
     // ability to flag some values in its array elements as
     // dependent or not dependent.  The values to be flagged,
     // and their associated flags, should reside within the
     // individual array element objects.

     // This function gets the flag corresponding to a value
     // in an array element of specified index.  Which flag (and
     // corresponding value) is referred to depends on the ident
     // argument, which might be an enum in the derived class,
     // or an enum in the array element class.

     // This function must be overridden if it is to be called.
     // Otherwise, it will assert.

     // The dependency flag should be set or un-set by the virtual
     // functions setValue and setDependentValue.


int ArrayBase::valueIsDependent(int /*ident*/, long index) const
{
  long n = numElements();            // put into overriding function.
  assert(index >= 0 && index < n);   // put into overriding function.
  assert(FALSE);                    // remove in overriding function.
  return FALSE;                    // replace in overriding function.
}



//---------------------- switch elements ---------------------//
//---------------------- switch elements ---------------------//
//---------------------- switch elements ---------------------//

    // This function interchanges two array elements.  It also
    // changes the active index if one of the two elements is
    // active, so that the active element remains active even
    // though it changes its location in the array.

    // This is a public virtual function which needs to be
    // overridden only if the derived class has to do additional
    // things besides the simple switch, and deal with the active
    // index.  If overridden, this function must be called from
    // the overriding function.

    // This function should be called one or more times in order
    // to rearrange the array elements (e.g. to sort them, or to
    // reverse their order in the array).

    // Direct or indirect calls to this function should be flanked
    // by calls to the virtual functions beforeNewActiveIndex and
    // afterNewActiveIndex, because of the likelyhood that the
    // active index will get changed.

    // This function asserts if there are no array elements, or if
    // the specified indices are out of range.


void ArrayBase::switchElements(long index1, long index2)
{
  assert(index1 >= 0 && index1 < _n);
  assert(index2 >= 0 && index2 < _n);
  if(index2 == index1) return;
  char *vvv2 = (char*)_array + _size * index2;
  char *vvv1 = (char*)_array + _size * index1;
  void *temp = malloc(_size);
  memcpy(temp, vvv1, _size);
  memcpy(vvv1, vvv2, _size);
  memcpy(vvv2, temp, _size);
  free(temp);
  if     (_active == index1) _active = index2;
  else if(_active == index2) _active = index1;
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

   // These are public virtual functions which need to be overridden only
   // if the derived class needs to know before and after values are
   // changed or array elements are removed and/or inserted.  These
   // functions do not need to be overridden even if they are called.


void ArrayBase::valuesWillChange
          (int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
}

void ArrayBase::valuesHaveChanged
          (int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/)
{
}



//---------------- allocate space --------------------------//
//---------------- allocate space --------------------------//
//---------------- allocate space --------------------------//

          // public.
          // primarily needed for private use.
          // made public for additional flexibility/efficiency.
          // lengthens the array if necessary.
          // might change _nalloc.
          // does not change _n.
          // returns 0 if no reallocation is needed.
          // returns 0 if reallocation is successful.
          // returns 1 if reallocation failed.

int ArrayBase::allocateSpace(long nadd)
{
  if(nadd <= 0) return 0;
  if(_nalloc >= _n + nadd) return 0;
  long nalloc = _n + nadd + _nstep;
  if(_nalloc == 0) nalloc = MaximumValue(nalloc, _istep);
  int error;
#ifdef CHECK_ALLOC
  cout << "alloc: nadd="   << nadd    <<
               "  n="      << _n      <<
               "  nalloc=" << _nalloc << " " << nalloc <<
               "  istep="  << _istep  <<
               "  nstep="  << _nstep  << endl;
#endif
  _array = memory_alloc_generic(_array, nalloc, _size, &error);
  if(error) return 1;
  _nalloc = nalloc;
  return 0;
}



//----------------------- free space ----------------------------//
//----------------------- free space ----------------------------//
//----------------------- free space ----------------------------//

          // public.
          // primarily needed for private use.
          // made public for additional flexibility/efficiency.
          // shortens the array if it is a good idea.
          // might change _nalloc.
          // does not change _n.

void ArrayBase::freeSpace()
{
  long nalloc = MaximumValue(_n + _nstep, _istep);
  if(nalloc >= _nalloc) return;
  int error;
#ifdef CHECK_ALLOC
  cout << "free: n="      << _n      <<
              "  nalloc=" << _nalloc << " " << nalloc <<
              "  istep="  << _istep  <<
              "  nstep="  << _nstep  << endl;
#endif
  _array = memory_alloc_generic(_array, nalloc, _size, &error);
  if(!error) _nalloc = nalloc;
}



//---------- notes regarding moving array elements -------------//
//---------- notes regarding moving array elements -------------//
//---------- notes regarding moving array elements -------------//

    // The following protected virtual functions:
    //
    //   [-]   void elementDeletingFromArray  (long index)
    //         void elementDeletingFromBuffer ()
    //   [-]   void elementRemovingToBuffer   (long index)
    //   [+]   void elementInsertedFromBuffer (long index)  [B]
    //   [+]   void elementInsertedFromList   (long index)  [L]
    //   [+]   void elementCreatedInArray     (long index)  [N]
    //
    // are called BY THIS BASE CLASS whenever any individual element
    // is moving into or out of the array or the buffer.  The index
    // is the location in the array.
    //
    //   [-] means the array will become shorter.
    //   [+] means the array has become longer.
    //
    //   [L] means the array element was copied from the <element> or
    //        <elements> arguments in one of the functions listed below.
    //   [B] means the array element was copied from the buffer.
    //   [N] means the array element is null (zero-filled).

    // Calls to the above virtual functions are initiated by calls to
    // the following functions:
    //
    // void clearBuffer             ();
    // void removeAllElements       ();
    // long appendNullElement       ();
    // long insertNullElement       (long index);
    // long insertElementFromBuffer (long index);
    // long removeElement           (long index);
    // long removeElementToBuffer   (long index);
    // long appendNullElements      (            long nins);
    // long insertNullElements      (long index, long nins);
    // long removeElements          (long index, long nrem);
    // long appendElement           (                       void *element);
    // long insertElement           (long index,            void *element);
    // long appendElements          (            long nins, void *elements);
    // long insertElements          (long index, long nins, void *elements);

    // Within the overriding virtual functions listed at the top of
    // this section, the contents of the specified array element (or
    // the buffer) can be changed.  The array element (or the buffer,
    // which might contain a removed array element) is an area in memory
    // with the size specified to the constructor of this base class.
    // The starting address of the array element can be obtained by
    // calling the function arrayElementPointer(index), and the address
    // of the buffer can be obtained by calling the function
    // bufferElementPointer().  



//--------- special concerns if the array elements are pointers --------//
//--------- special concerns if the array elements are pointers --------//
//--------- special concerns if the array elements are pointers --------//

    // If the element is a pointer to an object (or a structure or
    // allocated memory), you should read this section.

    // The derived ArrayBase class is said to "own" its array element
    // objects if it creates and deletes them, and (preferentially)
    // hides them from the outside world.  If the derived ArrayBase
    // class simply keeps a list of objects provided to it from the
    // outside world, then it does not "own" them, and has no business
    // trying to create or delete them, even (e.g.) when they are being
    // removed from the array.

    // If this class "owns" its objects, then special create/delete
    // actions must be performed by the following three overriding
    // virtual functions:
    //
    // elementCreatedInArray:
    //     The object must be CREATED by this function (unless you want
    //     to leave the object pointer NULL in the array).  The address
    //     of the created object must be copied into the location
    //     obtained by calling arrayElementPointer(index).
    //
    // elementDeletingFromArray:
    //     The object must be DELETED by this function (unless its
    //     pointer is NULL).  The address of the object to be deleted
    //     can be obtained by calling arrayElementPointer(index).
    //     
    // elementDeletingFromBuffer:
    //     The object must be DELETED by this function (unless its
    //     pointer is NULL).  The address of the object to be deleted
    //     can be obtained by calling bufferElementPointer().

    // If this class "owns" its objects, then public functions which
    // call the following functions should not be provided:
    //    void fetchElement   (long index, void *element)     const;
    //    long appendElement  (            void *element);
    //    long insertElement  (long index, void *element);
    //    void fetchElements  (long index, long nget, void *elements) const;
    //    long appendElements (            long nins, void *elements);
    //    long insertElements (long index, long nins, void *elements);



//------------ notify individual removals or insertions ------------//
//------------ notify individual removals or insertions ------------//
//------------ notify individual removals or insertions ------------//

           // private

void ArrayBase::notifyIndividualRemovals (long index, long nrem, int what)
{
  long i;
  switch(what)
      {
      case INSERTED_FROM_BUFFER: return;
      case INSERTED_FROM_LIST  : return;
      case CREATED_IN_ARRAY    : return;
      default                  : assert(FALSE);
      case DELETING_FROM_BUFFER: assert(FALSE);
      case REMOVING_TO_BUFFER  :
             {
             assert(nrem == 1);
             clearBuffer();
             elementRemovingToBuffer(index);
             void *element = arrayElementPointer(index);
             memcpy(_buffer, element, _size);
             _empty = FALSE;
             }
             break;
      case DELETING_FROM_ARRAY :
             for(i = index; i < index + nrem; i++)
                 {
                 elementDeletingFromArray(i);
                 }
             break;
      }
}



void ArrayBase::notifyIndividualInsertions (long index, long nins, int what)
{
  long i;
  switch(what)
      {
      case REMOVING_TO_BUFFER  : return;
      case DELETING_FROM_ARRAY : return;
      case DELETING_FROM_BUFFER: assert(FALSE);
      default                  : assert(FALSE);
      case INSERTED_FROM_BUFFER:
             assert(nins == 1);
             if(_empty)
                 {
                 elementCreatedInArray(index);
                 }
             else
                 {
                 void *element = arrayElementPointer(index);
                 memcpy(element, _buffer, _size);
                 memset(_buffer, 0, _size);
                 _empty = TRUE;
                 elementInsertedFromBuffer(index);
                 }
             break;
      case INSERTED_FROM_LIST  :
             for(i = index; i < index + nins; i++)
                 {
                 elementInsertedFromList(i);
                 }
             break;
      case CREATED_IN_ARRAY    :
             for(i = index; i < index + nins; i++)
                 {
                 elementCreatedInArray(i);
                 }
             break;
      }
}



//------------ public insert or remove functions -----------------//
//------------ public insert or remove functions -----------------//
//------------ public insert or remove functions -----------------//

         // public
         // remove or insert an array element.
         // these functions lack the <element> or <elements> argument.
         // if return long: return index where action occurred.
         // if return long: return -1 if not successful.

   // For insertNullElement, insertElementFromBuffer, and
   // insertNullElements, the index can be <= _n.  Otherwise the
   // index must be < _n.  The functions appendNullElement and
   // appendNullElements are identical to insertNullElement
   // and insertNullElements with index = _n.

   // These functions can be used directly with derived classes.


#define NREM_ZERO   0
#define NREM_ONE    1
#define NINS_ZERO   0
#define NINS_ONE    1


long ArrayBase::appendNullElement()
{
  return privateMove(_n, NREM_ZERO, NINS_ONE, NULL, CREATED_IN_ARRAY);
}


long ArrayBase::insertNullElement(long index)
{
  return privateMove(index, NREM_ZERO, NINS_ONE, NULL, CREATED_IN_ARRAY);
}


long ArrayBase::insertElementFromBuffer(long index)
{
  return privateMove(index, NREM_ZERO, NINS_ONE, NULL, INSERTED_FROM_BUFFER);
}


long ArrayBase::removeElement(long index)
{
  return privateMove(index, NREM_ONE, NINS_ZERO, NULL, DELETING_FROM_ARRAY);
}


long ArrayBase::removeElementToBuffer(long index)
{
  return privateMove(index, NREM_ONE, NINS_ZERO, NULL, REMOVING_TO_BUFFER);
}


long ArrayBase::appendNullElements(long nins)
{
  return privateMove(_n, NREM_ZERO, nins, NULL, CREATED_IN_ARRAY);
}


long ArrayBase::insertNullElements(long index, long nins)
{
  return privateMove(index, NREM_ZERO, nins, NULL, CREATED_IN_ARRAY);
}


long ArrayBase::removeElements(long index, long nrem)
{
  return privateMove(index, nrem, NINS_ZERO, NULL, DELETING_FROM_ARRAY);
}



//------------ protected insert or remove functions -----------------//
//------------ protected insert or remove functions -----------------//
//------------ protected insert or remove functions -----------------//

         // protected
         // fetch remove or insert an array element.
         // these functions include the <element> or <elements> argument.
         // if return long: return index where action occurred.
         // if return long: return -1 if not successful.

   // For insertElement and insertElements, the index can be <= _n.
   // Otherwise the index must be < _n.  The functions appendElement
   // and appendElements are identical to insertElement and
   // insertElements with index = _n.

   // In principle, these functions could be used directly with derived
   // classes.  However, they are made protected (rather than public)
   // to force derived classes to provide new functions (probably with
   // the same names), which should do the appropriate type conversions
   // and call these base class functions.

   // For the functions fetchElement and fetchElements, the <element>
   // or <elements> argument receives a copy (or copies) of the
   // requested elements in the array, starting at the specified index.
   // For the other functions, the specified element(s) will be inserted
   // into the array, starting at the specified index.


void ArrayBase::fetchElement(long index, void *element)  const
{
  assert(element);
  memory_fetch_generic(_array, _n, _size, index, 1, element);
}


long ArrayBase::appendElement(void *element)
{
  assert(element);
  return privateMove(_n, NREM_ZERO, NINS_ONE, element, INSERTED_FROM_LIST);
}


long ArrayBase::insertElement(long index, void *element)
{
  assert(element);
  return privateMove(index, NREM_ZERO, NINS_ONE, element, INSERTED_FROM_LIST);
}


void ArrayBase::fetchElements(long index, long nget, void *elements) const
{
  assert(elements);
  memory_fetch_generic(_array, _n, _size, index, nget, elements);
}


long ArrayBase::appendElements(long nins, void *elements)
{
  assert(elements);
  return privateMove(_n, NREM_ZERO, nins, elements, INSERTED_FROM_LIST);
}


long ArrayBase::insertElements(long index, long nins, void *elements)
{
  assert(elements);
  return privateMove(index, NREM_ZERO, nins, elements, INSERTED_FROM_LIST);
}



//------------------------ private move ------------------------------//
//------------------------ private move ------------------------------//
//------------------------ private move ------------------------------//

          // private

long ArrayBase::privateMove
           (long index, long nrem, long nins, void *elements, int what)
{
  if(nrem == 0 && nins == 0) return index;
  int error = allocateSpace(nins - nrem);
  if(error) return -1;

//----------
  long new_n = _n + nins - nrem;
  int changing_active_index = (_n == 0 || new_n == 0 || _active >= index);
  if(nrem == nins && _active >= index + nrem) changing_active_index = FALSE;
//----------

  if(changing_active_index) beforeNewActiveIndex();
                            beforeRemoveInsert  (index, nrem, nins);

  notifyIndividualRemovals   (index, nrem,       what);
  _n = memory_rem_ins_generic(_array, _n, _size,
                                         index, nrem, nins, elements);
  if(nrem > nins) freeSpace();
  notifyIndividualInsertions (index,       nins, what);

//----------
  assert(_n == new_n);
  if     (_n == 0)                 _active = -1;
  else if(_active == -1)           _active = 0;
  else if(_active >= index + nrem) _active += nins - nrem;
  if     (_n > 0)    _active = ConstrainValue(_active, 0, _n - 1);
//----------

                            afterRemoveInsert  (index, nrem, nins);
  if(changing_active_index) afterNewActiveIndex();
  return index;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

