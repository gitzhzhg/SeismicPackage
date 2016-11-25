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


//------------------------ char_array.hh ---------------------//
//------------------------ char_array.hh ---------------------//
//------------------------ char_array.hh ---------------------//

//              header file for the CharArray class
//               derived from the ArrayBase class
//                     subdirectory oprim

     // The array elements in this class are chars.
 

#ifndef _CHAR_ARRAY_HH_
#define _CHAR_ARRAY_HH_


#include "oprim/array_base.hh"


class CharArray  :  public ArrayBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:     // no data

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  CharArray (long istep, long nstep);
  virtual ~CharArray();

public:

  char /*value*/ fetchElement    (long index)                   const;
  long           appendElement   (             char value);
  long           insertElement   (long index,  char value);

  void  fetchElements   (long index, long nget, char *values)   const;
  long  appendElements  (            long nins, char *values);
  long  insertElements  (long index, long nins, char *values);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
