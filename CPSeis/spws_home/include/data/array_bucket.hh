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

//------------------- array_bucket.hh --------------------------//
//------------------- array_bucket.hh --------------------------//
//------------------- array_bucket.hh --------------------------//

//             header file for the ArrayBucket class
//                  not derived from any class
//                      subdirectory oprim

      // This base class maintains a list of pointers to
      // unspecified objects.  Derived classes can overload
      // some of the functions to refer to the actual class
      // of the objects.

#ifndef _ARRAY_BUCKET_HH_
#define _ARRAY_BUCKET_HH_


class ArrayBucket
{

//------------------------- data ----------------------------------//
//------------------------- data ----------------------------------//
//------------------------- data ----------------------------------//

private:

  int    _step;      // desired allocation step.
  int    _n;         // current number of array elements.
  int    _nalloc;    // current length of allocated array.
  void **_array;     // array of pointers to unspecified objects.

//-------------------------- functions -----------------------------//
//-------------------------- functions -----------------------------//
//-------------------------- functions -----------------------------//

public:

  ArrayBucket     (int step);

  virtual ~ArrayBucket();

  int   numElements     ()                const  { return _n; }
  int   addElement      (void *element);
  void *fetchElement    (int i)           const;
  int   validateElement (int i)           const;
  void  validateIndex   (int i)           const;
  void  validateRange   (int i1, int i2)  const;
  void  removeElements  (int i1, int i2);

private:

  void  maybeAdjustSize (int minsize);

//-------------------- end of functions ---------------------------//
//-------------------- end of functions ---------------------------//
//-------------------- end of functions ---------------------------//

} ;

#endif

//--------------------------- end ----------------------------------//
//--------------------------- end ----------------------------------//
//--------------------------- end ----------------------------------//

