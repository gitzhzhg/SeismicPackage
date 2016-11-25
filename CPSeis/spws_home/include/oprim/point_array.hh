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


//------------------------ point_array.hh ---------------------//
//------------------------ point_array.hh ---------------------//
//------------------------ point_array.hh ---------------------//

//             header file for the PointArray class
//               derived from the ArrayBase class
//                     subdirectory oprim

     // The array elements in this class are pointers to void.
 
     // This class does not know anything about the pointees (objects
     // whose pointers it maintains in the array).

     // This class may or may not "own" the pointees, depending on
     // which functions are used to insert elements into the array,
     // and on the activities of some of the overriding virtual
     // functions.  See documentation in array_base.cc for details.
     // An array "owns" its elements if it creates and deletes the
     // objects whose pointers it keeps in the array.  It does not
     // "own" its elements if it simply receives the pointers from
     // someone else who creates and deletes them.  It is the
     // responsibility of a derived class to "own" its pointees
     // if it wants to.


#ifndef _POINT_ARRAY_HH_
#define _POINT_ARRAY_HH_


#include "oprim/array_base.hh"


class PointArray  :  public ArrayBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:     // no data

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  PointArray (long istep, long nstep);
  virtual ~PointArray();

public:

  void* /*obj*/ fetchElement    (long index)                   const;
  long          appendElement   (             void* obj);
  long          insertElement   (long index,  void* obj);

  void  fetchElements   (long index, long nget, void **objs)   const;
  long  appendElements  (            long nins, void **objs);
  long  insertElements  (long index, long nins, void **objs);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
