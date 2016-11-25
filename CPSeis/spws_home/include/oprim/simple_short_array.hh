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

//----------------------- simple_short_array.hh ----------------------//
//----------------------- simple_short_array.hh ----------------------//
//----------------------- simple_short_array.hh ----------------------//

//             header file for the SimpleShortArray class
//                    not derived from any class
//                        subdirectory oprim

    // This class maintains an array of short (16-bit) values and
    // a scale factor.  The public interface treats the array as
    // if it contained floats instead of shorts.  Elements of the
    // array can have nil values.  A nil value is given by the
    // constants SNIL (short) and FNIL (float) in named_constants.h.

//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//

#ifndef _SIMPLE_SHORT_ARRAY_HH_
#define _SIMPLE_SHORT_ARRAY_HH_

#include <limits.h>

class SimpleShortArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  const int   _maximum;    // maximum allowed number of array elements.
  short      *_array;      // array of values (NULL if no elements or all elements are nil).
  int         _nelements;  // number of array elements.
  float       _buffer;     // temporary storage for a single value.
  float       _scalefactor;// scale factor for multiplying short to get float.
  float       _minval;     // minimum value in array.
  float       _maxval;     // maximum value in array.
  int         _numnils;    // number of nil values in array.
  int         _nascending; // number of consecutive ascending non-nil elements.
  int         _npositive;  // number of consecutive positive  non-nil elements.

   // _minval and _maxval are FNIL if there are no elements.
   // _minval and _maxval are FNIL if all elements are nil.
   // otherwise, nil values are not used when getting _minval and _maxval.
   // _nascending == _nelements if all elements >= 0 and ascending (like time or depth).
   // _npositive == _nelements if all elements > 0 (like velocities).

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public: // constructor and destructor.
        // constructor asserts if maximum <= 0 or nil.

           SimpleShortArray (const int maximum = INT_MAX);
  virtual ~SimpleShortArray ();

public: // reallocate the number of elements to smaller/larger length.
        // deleteAllElements frees array, sets pointer to NULL, clears buffer.
        // resetNumElementsAndClear sets all elements to nil and clears buffer.
        // resetNumElements sets all elements to array values, retains buffer.
        // copyAllElements copies everything including buffer.
        // these assert if nelements < 0 or nil or > _maximum.
        // copyAllElements asserts if object is NULL.
        // values can be NULL.

  void deleteAllElements         ();
  void resetNumElementsAndClear  (int nelements);
  void resetNumElements          (const float *values, int nelements);
  void copyAllElements           (const SimpleShortArray *object);

public: // create and delete array pointer.
        // createArrayPointer allocates a new array containing the values.
        // allocates one greater than _nelements so never returns NULL.
        // allocates one greater than _nelements so an additional element can be inserted.
        // it is the user's responsibility to call deleteArrayPointer when finished.
        // this is a convenient alternative to getAllValues.
        // values can be NULL.

  float *createArrayPointer ()               const;
  void   deleteArrayPointer (float *values)  const;

public: // get values.
        // getValue asserts if index is out of range.
        // getAllValues asserts if values is NULL.

  float  getValue           (int index)  const;
  void   getAllValues   (float *values)  const;
  int    numElements                 ()  const  { return _nelements; }
  float  minimumValue                ()  const  { return _minval; }
  float  maximumValue                ()  const  { return _maxval; }
  int    numNilValues                ()  const  { return _numnils; }
  int    validateAscending           ()  const  { return _nascending; }
  int    validatePositive            ()  const  { return _npositive; }

public: // set values.
        // setValue asserts if index is out of range.

  void   setValue                (int index, float value);
  void   setAllValues            (const float *values);
  void   multiplyByConstant      (float constant);
  void   addConstant             (float constant);

public: // insert or append or remove one element.
        // these functions assert if index is out of range.

  void   appendNilElement        ()            { insertNilElement        (_nelements); }
  void   appendElementFromBuffer ()            { insertElementFromBuffer (_nelements); }
  void   appendElement           (float value) { insertElement           (_nelements, value); }
  void   insertNilElement        (int index);
  void   insertElementFromBuffer (int index);
  void   insertElement           (int index, float value);
  void   removeElement           (int index);
  void   removeElementToBuffer   (int index);

public: // find bracketing indices ia and ib, where normally ib = ia+1.
        // returns ia = last  index with value <= specified value.
        // returns ib = first index with value >= specified value.
        // returns ia = ib = -1 for any of these reasons:
        //   there are no array elements.
        //   the specified value is nil.
        //   ANY array element values are nil.
        //   the array element values are not in strictly ascending order.
        // special cases:
        // if specified value < first array element value,
        //                   returns ia = -1 and ib = 0.
        // if specified value >  last array element value,
        //                   returns ia = _nelements-1 and ib = _nelements.
        // if specified value matches an array element,
        //                   returns ia = ib = index of matching element.
        // works only for ascending values.
        // asserts if ia or ib is NULL.

  void  findBracketingValues (float value, int *ia, int *ib)  const;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
