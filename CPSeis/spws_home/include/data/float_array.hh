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


//------------------------ float_array.hh ---------------------//
//------------------------ float_array.hh ---------------------//
//------------------------ float_array.hh ---------------------//

//             header file for the FloatArray class
//               derived from the ArrayBase class
//                     subdirectory oprim

     // The array elements in this class are floats.
 

#ifndef _FLOAT_ARRAY_HH_
#define _FLOAT_ARRAY_HH_


#include "oprim/array_base.hh"


class FloatArray  :  public ArrayBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:     // no data

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  FloatArray (long istep, long nstep);
  virtual ~FloatArray();

public:

  float /*value*/ fetchElement    (long index)                   const;
  long            appendElement   (             float value);
  long            insertElement   (long index,  float value);

  void  fetchElements   (long index, long nget, float *values)   const;
  long  appendElements  (            long nins, float *values);
  long  insertElements  (long index, long nins, float *values);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
