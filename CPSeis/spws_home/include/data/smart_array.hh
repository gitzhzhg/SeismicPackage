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

//------------------------ smart_array.hh ---------------------//
//------------------------ smart_array.hh ---------------------//
//------------------------ smart_array.hh ---------------------//

//              header file for the SmartArray class
//                   not derived from any class
//                       subdirectory oprim

     // This is a base class for all arrays of objects of any class.
     // Each object in the array is referred to as an array element.

     // This class "owns" its array elements.  That is, it creates
     // or deletes the objects when their pointers are inserted
     // into the array, or removed from the array, or when the array
     // is deleted.  It is the responsibility of a derived class to
     // accomplish the creations and deletions by overriding the
     // doCreateElement and doDeleteElement virtual functions.

     // See the implementation file for more documentation.

     // All functions which change array elements or their values
     // will call virtual functions to notify before and after
     // changes are made.  Such virtual functions, if overridden, can
     // then take additional action based on these notifications.


#ifndef _SMART_ARRAY_HH_
#define _SMART_ARRAY_HH_

#include <assert.h>


class SmartArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  const long  _step;   // allocation step (# elements).

  void **_array;     // pointer to area containing pointers to array elements.
  void  *_buffer;    // pointer to array element in buffer (might be NULL).
  long   _nalloc;    // space (# elements) allocated for the array.
  long   _n;         // actual number of array elements.
  long   _active;    // index of  active   array element (or -1 if _n == 0).
  long   _reference; // index of reference array element (or -1 if _n == 0).


//--------------- miscellaneous functions -------------------//
//--------------- miscellaneous functions -------------------//
//--------------- miscellaneous functions -------------------//

protected:   // constructor and destructor.

           SmartArray (long step);
  virtual ~SmartArray ();

public:      // miscellaneous.
             // getActiveIndex and getReferenceIndex return  -1  if _n == 0.
             // activeElement  and referenceElement  return NULL if _n == 0.

  long  numElements            ()  const     { return _n; }
  long  getActiveIndex         ()  const     { return _active; }
  long  getReferenceIndex      ()  const     { return _reference; }
  void  setActiveIndex         (long index);
  void  setReferenceIndex      (long index);

  void *arrayElement (long index) const;

  void *unsafeArrayElement  (long index)  const  { return _array[index]; }
  void *activeElement       ()            const;
  void *referenceElement    ()            const;
  void *bufferElement       ()            const  { return _buffer; }

private:

  long  privateRemove       (long index, int what);
  long  privateInsert       (long index, int what);


//----------- insert or remove array elements ---------------------//
//----------- insert or remove array elements ---------------------//
//----------- insert or remove array elements ---------------------//

public:  // remove or insert an array element.
         // if return long: return index where action occurred.
         // if return long: return -1 if not successful.
         // if return int: return error TRUE or FALSE.
         // these functions call the 5 protected virtual functions below.
  
  void  clearBuffer             ();            // deletes if not empty.
  void  removeAllElements       ();            // deletes.
  long  appendNullElement       ();            // creates.
  long  insertNullElement       (long index);  // creates.
  long  insertElementFromBuffer (long index);  // creates if buffer empty.
  long  removeElement           (long index);  // deletes.
  long  removeElementToBuffer   (long index);  // first clears buffer if full.
  void  copyElementToBuffer     (long index);

    // copyElementToBuffer:
    //   first creates null element in buffer if empty.
    //   does not remove element from array.
    //   leaves buffer element unchanged if doCopyToBuffer is not overridden.

    // if you wish to retain the buffer contents after inserting element
    // from buffer, follow insertElementFromBuffer by copyElementToBuffer.

  int   resetNumElements  (long n);
  int   appendNumElements (long nappend);
  int   allocateSpace     (long nadd);
  void  freeSpace         ();


//----------------- public virtual functions ---------------------//
//----------------- public virtual functions ---------------------//
//----------------- public virtual functions ---------------------//

   // Although these functions could be called by anybody, they are
   // intended to be called from within this class, from within
   // derived classes, or from helper classes contained within derived
   // classes.  They are public rather than protected simply so that
   // helper classes can call them without having to be friends.
   // The function switchElements probably does not need overriding.
   // The other functions do nothing (or return 0) if not overridden.

public:

  virtual int  valueIsDependent  (int ident, long index)  const;
  virtual void setDependencyTrue (int ident, long index);
  virtual void setDependencyFalse(int ident, long index);
  virtual void switchElements    (long index1, long index2);
  virtual void valuesWillChange  (int ident, long index, long nrem, long nins);
  virtual void valuesHaveChanged (int ident, long index, long nrem, long nins);


//----------------- protected virtual functions --------------------//
//----------------- protected virtual functions --------------------//
//----------------- protected virtual functions --------------------//

protected:   // called from the insert/remove public functions.
             // the first two functions must be overridden.
             // the other three functions do nothing if not overridden.

  virtual void *doCreateObject        ()             = 0;
  virtual void  doDeleteObject        (void *object) = 0;
  virtual void  objectHasBeenInserted (long index);
  virtual void  objectWillBeRemoved   (long index);
  virtual void  doCopyToBuffer        (void *object, void *buffer);


//----------------- protected virtual functions --------------------//
//----------------- protected virtual functions --------------------//
//----------------- protected virtual functions --------------------//

   // These functions are called BY THIS BASE CLASS to notify the
   // derived class whenever things change within the array.  These
   // are simply convenience routines so that any special actions to
   // be taken by the derived class can be localized in one place,
   // regardless of how the indicated change was initialized.

   // Derived classes may also wish to call some of these functions
   // if changes are made which are not known by this base class.

protected:   // these do nothing if not overridden.

  virtual void beforeNewActiveIndex    ();
  virtual void  afterNewActiveIndex    ();
  virtual void beforeNewReferenceIndex ();
  virtual void  afterNewReferenceIndex ();
  virtual void beforeRemoveInsert      (long index, long nrem, long nins);
  virtual void  afterRemoveInsert      (long index, long nrem, long nins);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
