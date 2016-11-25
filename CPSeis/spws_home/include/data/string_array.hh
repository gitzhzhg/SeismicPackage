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


//------------------------ string_array.hh ---------------------//
//------------------------ string_array.hh ---------------------//
//------------------------ string_array.hh ---------------------//

//              header file for the StringArray class
//               derived from the ArrayBase class
//                     subdirectory oprim

     // The array elements in this class are strings containing
     // _nchar characters each.  The strings are treated as if
     // each string consists of exactly _nchar characters.  Nul
     // characters have no special meaning.  The strings are
     // held back-to-back in memory, without any additional space
     // for nul-terminations.
 
     // Strings passed through argument lists (via pointers) are
     // also treated as if each string consists exactly of _nchar
     // characters, with no special meaning for nul characters.

     // The default value for a new array element is a string full
     // of binary zeroes.

#ifndef _STRING_ARRAY_HH_
#define _STRING_ARRAY_HH_


#include "oprim/array_base.hh"


class StringArray  :  public ArrayBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  const int _nchar;    // number of characters in each string.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  StringArray (int nchar, long istep, long nstep); 
  virtual ~StringArray();

  int numChars()  const  { return _nchar; }

public:

  void  fetchElement    (long index,  char *string)              const;
  long  appendElement   (             char *string);
  long  insertElement   (long index,  char *string);

  void  fetchElements   (long index, long nget, char *strings)   const;
  long  appendElements  (            long nins, char *strings);
  long  insertElements  (long index, long nins, char *strings);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
