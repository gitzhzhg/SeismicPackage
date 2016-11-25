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

//--------------------------- void_array.hh --------------------------//
//--------------------------- void_array.hh --------------------------//
//--------------------------- void_array.hh --------------------------//

//                header file for the VoidArray class
//                    not derived from any class
//                         subdirectory oprim

   // This is a very simple class which maintains an array of
   // void pointers.  The length of the array is changed to match
   // the current number of elements in the array each time a
   // pointer is inserted or removed.  This class does not own
   // the pointees and knows nothing about them.  This class can
   // be derived from, or can be used as is.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VOID_ARRAY_HH_
#define _VOID_ARRAY_HH_


class VoidArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  long   _n;         // number of array elements.
  void **_array;     // array of void pointers.
  long   _active;    // index of  active   array element (or -1 if no elements).
  long   _reference; // index of reference array element (or -1 if no elements).


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:   // constructor and destructor.

           VoidArray ();
  virtual ~VoidArray ();

public:    // get values.
           // activeElement    returns NULL if _n is 0 (_active    is -1).
           // referenceElement returns NULL if _n is 0 (_reference is -1).
           // lastElement      returns NULL if _n is 0.
           // findElement      returns index, or -1 if element is not found.

  long     numElements       ()            const  { return _n; }
  long     getActiveIndex    ()            const  { return _active; }
  long     getReferenceIndex ()            const  { return _reference; }
  long     getLastIndex      ()            const  { return _n - 1; }
  void    *element           (long index)  const;
  void    *activeElement     ()            const;
  void    *referenceElement  ()            const;
  void    *lastElement       ()            const;
  long     findElement       (void *element);

public:    // set values.
           // replaceActiveElement returns the index of the active element.
           // appendElement        returns the index of the new element.

  void setActiveIndex       (long index);
  void setReferenceIndex    (long index);
  void removeActiveElement  ();
  long replaceActiveElement             (void *element);
  void insertElement        (long index, void *element);
  long appendElement                    (void *element);
  void findAndRemoveElement             (void *element);
  void removeElement        (long index);
  void replaceElement       (long index, void *element);
  void clearElements        ();
  void createNullElements   (long n);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
