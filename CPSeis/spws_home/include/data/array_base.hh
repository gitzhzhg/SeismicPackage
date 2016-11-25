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

//------------------------ array_base.hh ---------------------//
//------------------------ array_base.hh ---------------------//
//------------------------ array_base.hh ---------------------//

//            header file for the ArrayBase class
//               not derived from any class
//                   subdirectory oprim

   // This is a base class for all arrays.
   // See the implementation file for more documentation.


#ifndef _ARRAY_BASE_HH_
#define _ARRAY_BASE_HH_

#include <assert.h>


class ArrayBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  const int   _size;   // size of a single array element.
  const long  _istep;  // initial allocation step     (# elements).
  const long  _nstep;  // subsequent allocation steps (# elements).
  void *const _buffer; // pointer to area temporarily storing array element.
  void       *_array;  // pointer to area containing allocated array.
  long        _nalloc; // space (# elements) allocated for the array.
  long        _n;      // actual number of array elements.
  long        _active; // index of active array element (or -1 if _n == 0).
  int         _empty;  // whether the buffer is empty (TRUE or FALSE).


//--------------- miscellaneous functions -------------------//
//--------------- miscellaneous functions -------------------//
//--------------- miscellaneous functions -------------------//

   // These functions are all potentially public.  Some of them
   // are protected in order to force derived classes to replace
   // the functions containing the <element> or <elements>
   // arguments with those with the appropriate types for the
   // derived class (if the derived class wishes to provide public
   // access to the functionality of these functions).

   // All of these functions which change array elements or their
   // values will call virtual functions to notify before and after
   // changes are made.  Such virtual functions, if overridden, can
   // then take additional action based on these notifications.

protected:

  ArrayBase (int size, long istep, long nstep);

public:

  virtual ~ArrayBase();

  long  numElements            ()  const     { return _n; }
  long  getActiveIndex         ()  const     { return _active; }
  void  setActiveIndex         (long index);

/*
  void  setElementValue  (int ident, long index, double value);
  void  setElementValues (int ident, long index, long nchng, double *values);
*/

protected:  // get a pointer to an array element or the buffer.
            // no data is copied.

  void *arrayElementPointer  (long index)  const
                  { assert(index >= 0 && index < _n);
                    return (char*)_array + index * _size; }

  void *bufferElementPointer  ()  const  { return _buffer; }


//----------- insert or remove array elements ---------------------//
//----------- insert or remove array elements ---------------------//
//----------- insert or remove array elements ---------------------//

public:  // remove or insert array elements (without list of elements).
         // these functions lack the <element> or <elements> argument.
         // inserted null elements are zero-filled.
         // removeAllElements must be called from all derived destructors.
         // if return long: return index where action occurred.
         // if return long: return -1 if not successful.
  
  void  clearBuffer       ();
  void  removeAllElements ();

  long  appendNullElement       ();
  long  insertNullElement       (long index);
  long  insertElementFromBuffer (long index);
  long  removeElement           (long index);
  long  removeElementToBuffer   (long index);

  long  appendNullElements      (            long nins);
  long  insertNullElements      (long index, long nins);
  long  removeElements          (long index, long nrem);
  
  int   allocateSpace  (long nadd);
  void  freeSpace      ();

protected:  // fetch or insert or remove elements (with list of elements).
            // these functions include the <element> or <elements> argument.
            // if return long: return index where action occurred.
            // if return long: return -1 if not successful.

  void  fetchElement    (long index, void *element)      const;
  long  appendElement   (            void *element);
  long  insertElement   (long index, void *element);

  void  fetchElements   (long index, long nget, void *elements)   const;
  long  appendElements  (            long nins, void *elements);
  long  insertElements  (long index, long nins, void *elements);


//-------------------- private functions --------------------------//
//-------------------- private functions --------------------------//
//-------------------- private functions --------------------------//

private:

  void  notifyIndividualRemovals   (long index, long nrem, int what);
  void  notifyIndividualInsertions (long index, long nins, int what);
  long  privateMove                (long index, long nrem,
                                long nins, void *elements, int what);


//----------------- public virtual functions ---------------------//
//----------------- public virtual functions ---------------------//
//----------------- public virtual functions ---------------------//

   // Although these functions could be called by anybody, they are
   // intended to be called from within this class, from within
   // derived classes, or from helper classes contained within derived
   // classes.  They are public rather than protected simply so that
   // helper classes can call them without having to be friends.

public:

  virtual double getValue          (int ident, long index)    const;
  virtual void   setValue          (int ident, long index, double value);
  virtual void   setDependentValue (int ident, long index, double value);
  virtual int    valueIsDependent  (int ident, long index)    const;

  virtual void   switchElements    (long index1, long index2);

  virtual void valuesWillChange  (int ident, long index, long nrem, long nins);
  virtual void valuesHaveChanged (int ident, long index, long nrem, long nins);


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

protected:

  virtual void beforeNewActiveIndex () {}
  virtual void afterNewActiveIndex  () {}

/*
  virtual void beforeValuesChanged
                     (int ident, long index, long nchng) {}
  virtual void afterValuesChanged
                     (int ident, long index, long nchng) {}
*/

  virtual void beforeRemoveInsert
                     (long /*index*/, long /*nrem*/, long /*nins*/) {}
  virtual void afterRemoveInsert
                     (long /*index*/, long /*nrem*/, long /*nins*/) {}

  virtual void elementDeletingFromArray  (long /*index*/) {}
  virtual void elementDeletingFromBuffer ()               {}
  virtual void elementRemovingToBuffer   (long /*index*/) {}
  virtual void elementInsertedFromBuffer (long /*index*/) {}
  virtual void elementInsertedFromList   (long /*index*/) {}
  virtual void elementCreatedInArray     (long /*index*/) {}


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
