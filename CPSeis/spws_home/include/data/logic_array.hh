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

//------------------------ logic_array.hh ---------------------//
//------------------------ logic_array.hh ---------------------//
//------------------------ logic_array.hh ---------------------//

//           header file for the LogicArray class
//               not derived from any class
//                   subdirectory oprim

   // This class maintains an array of logical bits.
   // The array is allocated in the constructor.
   // The array is deleted in the destructor.
   // The number of elements (logical bits) in the array
   //   remains constant throughout the life of the object,
   //   except when resetNumElements is called.


#ifndef _LOGIC_ARRAY_HH_
#define _LOGIC_ARRAY_HH_


class LogicArray
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  const int   _size;   // size of an integer in bits.
  int        *_array;  // pointer to area containing allocated array.
  long        _nalloc; // space (# integers) allocated for the array.
  long        _n;      // total number of logic elements.
  long        _ntrue;  // number of logic elements which are true.

//---------------------- functions --------------------------//
//---------------------- functions --------------------------//
//---------------------- functions --------------------------//

public:

  LogicArray (long n);

  virtual ~LogicArray();

  long numElements        ()  const   { return _n; }
  long numTrueElements    ()  const   { return _ntrue; }
  long numFalseElements   ()  const   { return (_n - _ntrue); }

  void setElement (long index, int value); // value is true or false.
  int  getElement (long index)  const;     // returns 1(true) or 0(false).

  void setElementTrue  (long index)  { setElement(index, 1); }
  void setElementFalse (long index)  { setElement(index, 0); }

  int  elementIsTrue  (long index)  const  { return  getElement(index); }
  int  elementIsFalse (long index)  const  { return !getElement(index); }

  void setAllElementsFalse    ();   // all logic elements will be FALSE.
  void resetNumElements (long n);   // all logic elements will be FALSE.

private:

  long getWordIndex (long index)          const;
  int  getMask      (long index, long i)  const;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
