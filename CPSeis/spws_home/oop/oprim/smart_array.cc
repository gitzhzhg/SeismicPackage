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

//---------------------- smart_array.cc -----------------------//
//---------------------- smart_array.cc -----------------------//
//---------------------- smart_array.cc -----------------------//

//          implementation file for the SmartArray class
//                   not derived from any class
//                       subdirectory oprim


   // This is a base class for all arrays of objects of any class.
   // This class manages the insertion and deletion of array elements,
   // and all required memory allocation/reallocation.  It contains
   // virtual functions, some of which must be overridden in derived
   // classes.

   // Two special features of this class:
   //   (1) one element is always flagged as "active".
   //   (2) one element is always flagged as "reference".
   //   (3) a buffer exists for temporarily storing an element
   //           removed from the array.


#include "oprim/smart_array.hh"
#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include <assert.h>

#include <signal.h>
#include <unistd.h>



enum { REMOVING_TO_BUFFER, DELETING_FROM_ARRAY,
       INSERTED_FROM_BUFFER, CREATED_IN_ARRAY };



//-------------------- constructor -------------------------//
//-------------------- constructor -------------------------//
//-------------------- constructor -------------------------//

                      // protected

SmartArray::SmartArray(long step)
              :
         _step                 (step),
         _buffer               (NULL),
         _array                (NULL),
         _nalloc               (0),
         _n                    (0),
         _active               (-1),
         _reference            (-1)
{
  assert(_step >= 1);
}



//---------------------- destructor ------------------------//
//---------------------- destructor ------------------------//
//---------------------- destructor ------------------------//

                         // public

SmartArray::~SmartArray()
{
  removeAllElements();
}



//-------------------- set active or reference index ----------------------//
//-------------------- set active or reference index ----------------------//
//-------------------- set active or reference index ----------------------//

                          // public

void SmartArray::setActiveIndex(long index)
{
  if     (_n == 0)     index = -1;
  else if(index < 0)   index = 0;
  else if(index >= _n) index = _n - 1;
//////  we want to send a message even if the active index does not change.
  beforeNewActiveIndex();
  _active = index;
  afterNewActiveIndex();
}


void SmartArray::setReferenceIndex(long index)
{
  if     (_n == 0)     index = -1;
  else if(index < 0)   index = 0;
  else if(index >= _n) index = _n - 1;
//////  we want to send a message even if the reference index does not change.
  beforeNewReferenceIndex();
  _reference = index;
  afterNewReferenceIndex();
}



static void handle_crash ()
{
  char buf[256];

  /* generate traceback to stdout and copy to /tmp/traceback.txt */
  sprintf (buf, "/usr/bin/pstack %d | /usr/bin/tee /tmp/traceback.txt\n",
    (int)getpid());
  /* undefine LD_PRELOAD to avoid 64-bit problems */
  system(buf);

  printf("A copy of the traceback is stored in /tmp/traceback.txt\n");
  exit(1);
}



static int test = 0;

void *SmartArray::arrayElement (long index) const
{
  if (test != 0) {
     printf (" From SmartArray::arrayElement, index = %d, _n = %d\n",
      index, _n);
  }

  if (index < 0 || index >= _n) {
    printf (" From SmartArray::arrayElement, index = %d, _n = %d\n",
      index, _n);
    handle_crash ();
  }


  assert (index >=0 && index < _n);
  assert (_array[index]);
  return _array[index];
}


//---------------------- active or reference element --------------------//
//---------------------- active or reference element --------------------//
//---------------------- active or reference element --------------------//

                          // public.

void *SmartArray::activeElement()  const
{
  if(_active == -1) return NULL;
  return _array[_active];
}


void *SmartArray::referenceElement()  const
{
  if(_reference == -1) return NULL;
  return _array[_reference];
}



//------------------------ clear buffer ---------------------//
//------------------------ clear buffer ---------------------//
//------------------------ clear buffer ---------------------//

                           // public

// The buffer is a temporary storage place for an array element
// which has been removed from the array and may subsequently
// be re-inserted into the array.

// An element cannot simultaneously exist both in the array and in
// the buffer.  The buffer is affected by calling these functions:
//                    clearBuffer
//                    insertElementFromBuffer
//                    removeElementToBuffer
//                    copyElementToBuffer

void SmartArray::clearBuffer()
{
  if(_buffer == NULL) return;
  doDeleteObject(_buffer);
  _buffer = NULL;
}



//----------------- remove all elements ---------------------//
//----------------- remove all elements ---------------------//
//----------------- remove all elements ---------------------//

             // public
             // the buffer is also cleared.

void SmartArray::removeAllElements()
{
  clearBuffer();
  if(_n == 0) return;
  assert(_array);
  beforeNewActiveIndex   ();
  beforeNewReferenceIndex();
  beforeRemoveInsert     (0, _n, 0);
  for(long index = 0; index < _n; index++)
      {
      objectWillBeRemoved(index);
      doDeleteObject(_array[index]);
      }
  free(_array);
  _array     = NULL;
  _n         = 0;
  _nalloc    = 0;
  _active    = -1;
  _reference = -1;
  afterRemoveInsert     (0, _n, 0);
  afterNewReferenceIndex();
  afterNewActiveIndex   ();
}



//---------------------- switch elements ---------------------//
//---------------------- switch elements ---------------------//
//---------------------- switch elements ---------------------//

    // This function interchanges two array elements.  It also
    // changes the active index and/or reference index if one of the
    // two elements is active and/or reference, so that the active
    // and/or reference element remains active and/or reference even
    // though it changes its location in the array.

    // This is a public virtual function which needs to be
    // overridden only if the derived class has to do additional
    // things besides the simple switch, and deal with the active
    // and reference index.  If overridden, this function must be
    // called from the overriding function.

    // This function should be called one or more times in order
    // to rearrange the array elements (e.g. to sort them, or to
    // reverse their order in the array).

    // Direct or indirect calls to this function should be flanked
    // by calls to the virtual functions beforeNewActiveIndex and
    // afterNewActiveIndex, and beforeNewReferenceIndex and
    // afterNewReferenceIndex, because of the likelihood that the
    // active and reference index will get changed.

    // This function asserts if there are no array elements, or if
    // the specified indices are out of range.


void SmartArray::switchElements(long index1, long index2)
{
  assert(index1 >= 0 && index1 < _n);
  assert(index2 >= 0 && index2 < _n);
  if(index2 == index1) return;
  void     *temp = _array[index1];
  _array[index1] = _array[index2];
  _array[index2] = temp;
  if     (_active == index1) _active = index2;
  else if(_active == index2) _active = index1;
  if     (_reference == index1) _reference = index2;
  else if(_reference == index2) _reference = index1;
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

   // These are public virtual functions which need to be overridden only
   // if the derived class needs to know before and after values are
   // changed or array elements are removed and/or inserted.  These
   // functions do not need to be overridden even if they are called.


void SmartArray::valuesWillChange
          (int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/) {}

void SmartArray::valuesHaveChanged
          (int /*ident*/, long /*index*/, long /*nrem*/, long /*nins*/) {}



//--------------- object has been inserted (or will be removed) ----------//
//--------------- object has been inserted (or will be removed) ----------//
//--------------- object has been inserted (or will be removed) ----------//

   // These are protected virtual functions which need to be overridden
   // only if the derived class needs to know when elements are inserted
   // or removed from the array, or if the function copyElementToBuffer
   // is intended to be called.

void SmartArray::objectHasBeenInserted (long /*index*/)   {}
void SmartArray::objectWillBeRemoved   (long /*index*/)   {}
void SmartArray::doCopyToBuffer        (void* /*object*/, void* /*buffer*/) {}



//------------------- before and after notifications --------------//
//------------------- before and after notifications --------------//
//------------------- before and after notifications --------------//

    // These are protected virtual functions.

void SmartArray::beforeNewActiveIndex () {}
void SmartArray::afterNewActiveIndex  () {}

void SmartArray::beforeNewReferenceIndex () {}
void SmartArray::afterNewReferenceIndex  () {}

void SmartArray::beforeRemoveInsert
                     (long /*index*/, long /*nrem*/, long /*nins*/) {}
void SmartArray::afterRemoveInsert
                     (long /*index*/, long /*nrem*/, long /*nins*/) {}



//--------------------- get and set dependency --------------------//
//--------------------- get and set dependency --------------------//
//--------------------- get and set dependency --------------------//

    // These are public virtual functions.

int SmartArray::valueIsDependent(int /*ident*/, long /*index*/)  const
{
return 0;
}

void SmartArray::setDependencyTrue  (int /*ident*/, long /*index*/) {}
void SmartArray::setDependencyFalse (int /*ident*/, long /*index*/) {}



//------------------------ reset num elements -------------------//
//------------------------ reset num elements -------------------//
//------------------------ reset num elements -------------------//

         // public.
         // first deletes all existing elements.
         // next allocates space.
         // finally creates empty elements.
         // returns error = 1 if an error occurred.
         // returns error = 0 if successful.

int SmartArray::resetNumElements(long n)
{
  removeAllElements();
  return appendNumElements(n);
}



//------------------------ append num elements -------------------//
//------------------------ append num elements -------------------//
//------------------------ append num elements -------------------//

         // public.
         // first allocates more space.
         // then creates empty elements and appends them.
         // returns error = 1 if an error occurred.
         // returns error = 0 if successful.

int SmartArray::appendNumElements(long nappend)
{
  if(nappend <= 0) return 0;
  int error = allocateSpace(nappend);
  if(error) return 1;
  assert(_array);
  long nkeep = _n;
  beforeRemoveInsert(nkeep, 0, nappend);
  for(long index = nkeep; index < nkeep + nappend; index++)
      {
      _array[index] = doCreateObject();
      assert(_array[index]);
      _n++;
      if(_active == -1 || _reference == -1)  // will be both or neither.
          {
          beforeNewActiveIndex();
          beforeNewReferenceIndex();
          _active    = 0;
          _reference = 0;
          afterNewReferenceIndex();
          afterNewActiveIndex();
          }
      objectHasBeenInserted(index);
      }
  assert(_n == nkeep + nappend);
  afterRemoveInsert(nkeep, 0, nappend);
  return 0;
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
          // returns error = 0 if no reallocation is needed.
          // returns error = 0 if reallocation is successful.
          // returns error = 1 if reallocation failed.

int SmartArray::allocateSpace(long nadd)
{
//if(nadd <= 0) return 0;               // redundant.
  if(_nalloc >= _n + nadd) return 0;
  long nalloc = _n + nadd +_step;
  void **array2;
  unsigned int nbytes = (unsigned int)nalloc * sizeof(void**);
  if(_array) array2 = (void**)realloc(_array, nbytes);
  else       array2 = (void**) malloc(        nbytes);
  if(array2 == NULL) return 1;
  _array = array2;
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

void SmartArray::freeSpace()
{
  if(_array == NULL) return;
  long nalloc = _n + _step;
  if(nalloc >= _nalloc) return;
  unsigned int nbytes = (unsigned int)nalloc * sizeof(void**);
  void **array2 = (void**)realloc(_array, nbytes);
  if(array2 == NULL) return;
  _array = array2;
  _nalloc = nalloc;
}



//------------ public insert or remove functions -----------------//
//------------ public insert or remove functions -----------------//
//------------ public insert or remove functions -----------------//

         // public.
         // remove or insert an array element.
         // if return long: return index where action occurred.
         // if return long: return -1 if not successful.

  // For insertNullElement and insertElementFromBuffer, the index can
  //   be <= _n.
  // Otherwise the index must be < _n.
  // The function appendNullElement is identical to insertNullElement
  //   with index = _n.


long SmartArray::appendNullElement()
{
  return privateInsert(_n, CREATED_IN_ARRAY);
}



long SmartArray::insertNullElement(long index)
{
  return privateInsert(index, CREATED_IN_ARRAY);
}



long SmartArray::insertElementFromBuffer(long index)
{
  return privateInsert(index, INSERTED_FROM_BUFFER);
}



long SmartArray::removeElement(long index)
{
  return privateRemove(index, DELETING_FROM_ARRAY);
}



long SmartArray::removeElementToBuffer(long index)
{
  return privateRemove(index, REMOVING_TO_BUFFER);
}



void SmartArray::copyElementToBuffer(long index)
{
  assert(index >= 0 && index < _n);
  if(!_buffer) _buffer = doCreateObject();
  assert(_buffer);
  doCopyToBuffer(_array[index], _buffer);
}



//------------------------ private remove ----------------------------//
//------------------------ private remove ----------------------------//
//------------------------ private remove ----------------------------//

          // private.

long SmartArray::privateRemove(long index, int what)
{
  assert(index >= 0 && index < _n);
  int changing_active_index    = (_active    > index || _active    == _n - 1);
  int changing_reference_index = (_reference > index || _reference == _n - 1);

  if(changing_active_index)    beforeNewActiveIndex   ();
  if(changing_reference_index) beforeNewReferenceIndex();
                               beforeRemoveInsert     (index, 1, 0);

  objectWillBeRemoved(index);
  if(what == REMOVING_TO_BUFFER)
          {
          clearBuffer();
          _buffer = _array[index];
          }
  else           // if(what == DELETING_FROM_ARRAY)
          {
          doDeleteObject(_array[index]);
          }

  unsigned int ncopy = (unsigned int)(_n - index - 1);
  if(ncopy > 0)
      memmove(&_array[index], &_array[index+1], sizeof(void*) * ncopy);
  if(changing_active_index)    _active--;
  if(changing_reference_index) _reference--;
  _n--;
  freeSpace();

                               afterRemoveInsert     (index, 1, 0);
  if(changing_reference_index) afterNewReferenceIndex();
  if(changing_active_index)    afterNewActiveIndex   ();
  return index;
}



//------------------------ private insert ----------------------------//
//------------------------ private insert ----------------------------//
//------------------------ private insert ----------------------------//

          // private.

long SmartArray::privateInsert(long index, int what)
{
  assert(index >= 0 && index <= _n);
  int error = allocateSpace(1);
  if(error) return -1;
  int changing_active_index    = (_n == 0 || _active    >= index);
  int changing_reference_index = (_n == 0 || _reference >= index);

  if(changing_active_index)    beforeNewActiveIndex   ();
  if(changing_reference_index) beforeNewReferenceIndex();
                               beforeRemoveInsert     (index, 0, 1);

  unsigned int ncopy = (unsigned int)(_n - index);
  if(ncopy > 0)
      memmove(&_array[index+1], &_array[index], sizeof(void*) * ncopy);
  if(changing_active_index)    _active++;
  if(changing_reference_index) _reference++;
  _n++;

  if(_buffer == NULL || what == CREATED_IN_ARRAY)
          {
          _array[index] = doCreateObject();
          assert(_array[index]);
          }
  else           // if(what == INSERTED_FROM_BUFFER)
          {
          _array[index] = _buffer;
          _buffer = NULL;
          }
  objectHasBeenInserted(index);

                               afterRemoveInsert     (index, 0, 1);
  if(changing_reference_index) afterNewReferenceIndex();
  if(changing_active_index)    afterNewActiveIndex   ();
  return index;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

