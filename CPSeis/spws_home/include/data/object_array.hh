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


//------------------------ object_array.hh ---------------------//
//------------------------ object_array.hh ---------------------//
//------------------------ object_array.hh ---------------------//

//             header file for the ObjectArray class
//               derived from the ArrayBase class
//                     subdirectory oprim

     // The array elements in this class are pointers to objects
     // of any type.  They are referred to as type void* in this
     // class.
 
     // This class "owns" its array elements.  That is, it creates
     // or deletes the objects when their pointers are inserted
     // into the array, or removed from the array, or when the array
     // is deleted.  It is the responsibility of a derived class to
     // accomplish the creations and deletions by overriding the
     // doCreateObject and doDeleteObject virtual functions.


#ifndef _OBJECT_ARRAY_HH_
#define _OBJECT_ARRAY_HH_


#include "oprim/array_base.hh"


class ObjectArray  :  public ArrayBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:     // no data

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

protected:   // constructor

  ObjectArray (long istep, long nstep);

public:   // destructor

  virtual ~ObjectArray();

private:   // virtual functions overriding ArrayBase.
           // not to be overridden in derived classes.
           // these call the following four protected virtual functions.

  virtual void elementDeletingFromArray  (long index);
  virtual void elementDeletingFromBuffer ();
  virtual void elementRemovingToBuffer   (long index);
  virtual void elementInsertedFromBuffer (long index);
  virtual void elementInsertedFromList   (long index);
  virtual void elementCreatedInArray     (long index);

protected:   // new virtual functions to override.
             // called from the above six private virtual functions.

  virtual void *doCreateObject        ()              = 0;
  virtual void  doDeleteObject        (void *obj)     = 0;
  virtual void  objectHasBeenInserted (long /*index*/) {}
  virtual void  objectWillBeRemoved   (long /*index*/) {}

protected:  // may be replaced by more explicit return type if desired.
            // protected to keep the outside world from gaining direct
            //   access to an "owned" object.  required pass-thru functions
            //   (in addition to those already provided) can be provided
            //   by derived classes if necessary.

  void *arrayElement (long index)  const
     { void *obj = *(void**)(arrayElementPointer(index));
       assert(obj);
       return obj; }

  void *bufferElement ()  const
     { void *obj = *(void**)(bufferElementPointer());
       return obj; }

//----------- functions in ArrayBase shown here for reference ---------//
//----------- functions in ArrayBase shown here for reference ---------//
//----------- functions in ArrayBase shown here for reference ---------//

                // functions not to use are not shown here 

/****************

public:  // functions in ArrayBase to use as is.

  long  numElements            ()  const;
  long  getActiveIndex         ()  const;
  void  setActiveIndex   (long index);
  void  setElementValue  (int ident, long index, double value);
  void  setElementValues (int ident, long index, long nchng, double *values);

  void  clearBuffer             ();
  void  removeAllElements       ();
  long  appendNullElement       ();
  long  insertNullElement       (long index);
  long  insertElementFromBuffer (long index);
  long  removeElement           (long index);
  long  removeElementToBuffer   (long index);
  long  appendNullElements      (            long nins);
  long  insertNullElements      (long index, long nins);
  long  removeElements          (long index, long nrem);

public:  // virtual functions to override if necessary. 

  virtual double getValue          (int ident, long index)    const;
  virtual void   setValue          (int ident, long index, double value);
  virtual void   setDependentValue (int ident, long index, double value);
  virtual int    valueIsDependent  (int ident, long index)    const;

  virtual void   switchElements  (long index1, long index2);
  virtual void valuesWillChange  (int ident, long index, long nrem, long nins);
  virtual void valuesHaveChanged (int ident, long index, long nrem, long nins);

protected: // virtual functions to override if necessary.

  virtual void beforeNewActiveIndex         ();
  virtual void afterNewActiveIndex          ();
  virtual void beforeValuesChanged  (int ident, long index, long nchng);
  virtual void afterValuesChanged   (int ident, long index, long nchng);
  virtual void beforeRemoveInsert   (long index, long nrem, long nins);
  virtual void afterRemoveInsert    (long index, long nrem, long nins);

*****************/

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
