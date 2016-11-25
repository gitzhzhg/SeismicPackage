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

//----------------------- simple_float_array.hh ----------------------//
//----------------------- simple_float_array.hh ----------------------//
//----------------------- simple_float_array.hh ----------------------//

//             header file for the SimpleFloatArray class
//                    not derived from any class
//                        subdirectory oprim

    // This class maintains an array of floating point values.
    // Elements of the array can have nil values.  A nil value is
    // given by the constant FNIL in named_constants.h.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _SIMPLE_FLOAT_ARRAY_HH_
#define _SIMPLE_FLOAT_ARRAY_HH_

#include <limits.h>
#include <stdio.h>


class SimpleFloatArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  const long  _maximum;    // maximum allowed number of array elements.
  const float _tolerance;  // tolerance for determining equality of spacing.

  float      *_array;      // array of floating point values
                           //   (NULL if no elements or shrunken).
  long        _nalloc;     // size of allocated array (0 if array is NULL).
  long        _nelements;  // number of array elements.
  float       _buffer;     // temporary storage for a single value.

  float       _minval;     // minimum value in array.
  float       _maxval;     // maximum value in array.
  long        _numnils;    // number of nil values in array.
  long        _nascending; // number of consecutive ascending non-nil elements.
  long       _ndescending; // number of consecutive descending non-nil elements.
  long        _npositive;  // number of consecutive positive  non-nil elements.
  float       _spacing;    // spacing between equally-spaced elements.

  int         _updated;    // whether the following variables are updated:
                           //   _minval, _maxval, _numnils,
                           //   _nascending, _ndescending, _npositive, _spacing.
 
   // _minval and _maxval are FNIL if there are no elements.
   // _minval and _maxval are FNIL if all elements are nil.
   // otherwise, nil values are not used when getting _minval and _maxval.

   // _spacing is FNIL if the array is not equally spaced, or if a
   //    subset of the values are nil.
   // _spacing is 0.0 if there are zero or one element, or all values are
   //    equal, or all values are nil.
   // the array cannot be shrunken when _spacing is FNIL.


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public: // constructor and destructor.

           SimpleFloatArray (const long maximum = LONG_MAX,
                             float tolerance = 1.0e-5);
  virtual ~SimpleFloatArray ();

public: // copy to or from binary file.
        // these return error = TRUE or FALSE.
        // these set message.

  int  readFromBinaryFile (FILE *stream, char *msg);
  int  saveToBinaryFile   (FILE *stream, char *msg);

private:

  void privateUpdate            ();
  void privateResetAndClear     (long nelements);
  void privateReset             (const float *values, long nelements);
  void privateExpand            ();

public: // reallocate the number of elements to smaller/larger length.
        // deleteAllElements frees array, sets pointer to NULL, clears buffer.
        // resetNumElementsAndClear sets all elements to nil and clears buffer.
        // resetNumElementsAndRetain retains previous elements up to minimum
        //   of old/new lengths, sets all elements beyond old length to nil,
        //   and retains buffer.
        // resetNumElements sets all elements to array values, retains buffer.
        // copyAllElements copies everything including buffer.

  void deleteAllElements         ();
  void resetNumElementsAndClear  (long nelements);
  void resetNumElementsAndRetain (long nelements);
  void resetNumElements          (const float *values, long nelements);
  void copyAllElements           (const SimpleFloatArray *object);

public: // create and delete array pointer.
        // createArrayPointer allocates a new array containing the values.
        // allocates one greater than _nelements so never returns NULL.
        // it is the user's responsibility to call deleteArrayPointer when finished.
        // this is a convenient alternative to getAllValues.
        // note: createArrayPointer and getArrayPointer must not be confused.

  float *createArrayPointer ()               const;
  void   deleteArrayPointer (float *values)  const;

public:  // get array pointer.
         // getArrayPointer returns NULL if there are no array elements.
         // getArrayPointer reallocates the array if it is shrunken.
         // warning:
         //   when using the returned pointer, it is the user's
         //   responsibility not to use an invalid index, not to recast
         //   the pointer to allow resetting the values, and not to free
         //   the array.
         // warning:
         //   the returned pointer may become invalid whenever any
         //   non-const function is subsequently called.

  const float *getArrayPointer();

public: // get values.
        // these functions assert if index is out of range.

  long         numElements                 ()  const  { return _nelements; }
  float        getValue          (long index)  const;
  void         getAllValues   (float *values)  const;
  float        minimumValue                ();
  float        maximumValue                ();
  long         numNilValues                ();
  long         numLiveValues               ();
  long         validateAscending           ();
  long         validateDescending          ();
  long         validatePositive            ();
  int          isAscending                 ();
  int          isDescending                ();
  int          isEquallySpaced             ();
  float        getSpacing                  ();

public: // set values.
        // these functions assert if index is out of range.

  void   setValueToNil           (long index);
  void   setValue                (long index, float value);
  void   setLastValue            (float value);
  void   setAllValuesToNil       ();
  void   setAllValues            (const float *values);
  void   multiplyByConstant      (float constant);
  void   addConstant             (float constant);

public: // set or append one value.
        // these functions assert if index is out of range.
        // setOrAppendNilValue calls setValueToNil    if index <  _nelements.
        // setOrAppendNilValue calls appendNilElement if index == _nelements.
        // setOrAppendValue    calls setValue         if index <  _nelements.
        // setOrAppendValue    calls appendElement    if index == _nelements.

  void   setOrAppendNilValue     (long index);
  void   setOrAppendValue        (long index, float value);

public: // insert or remove one element.
        // these functions assert if index is out of range.

  void   appendNilElement        ();
  void   appendElement           (float value);
  void   insertNilElement        (long index);
  void   insertElementFromBuffer (long index);
  void   insertElement           (long index, float value);
  void   removeElement           (long index);
  void   removeElementToBuffer   (long index);

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
        // values are assumed to match if their difference is <= _tolerance.

  void  findBracketingValues (float value, long *ia, long *ib);

public: // get interpolated/extrapolated value.
        // this array is considered to be an array of ordinates.
        // returns an interpolated/extrapolated ordinate value corresponding
        //   to a specified abscissa value.
        // value  = an abscissa whose corresponding ordinate is wanted.
        // value1 = an abscissa corresponding to the ordinate at index ia.
        // value2 = an abscissa corresponding to the ordinate at index ib.
        // returns 0.0 for any of these reasons:
        //   there are no array elements.
        //   ANY one or more of value, value1, or value2 is nil.
        //   ANY array element values are nil.
        //   both ia and ib are -1.
        // otherwise ia and ib are constrained to be valid indices.
        // then returns the following value:
        //   if ia == ib, value at index ia.
        //   if value1 == value2, average of values at indices ia and ib.
        //   else interpolated value using value, value1, value2, ia, and ib.

  float getInterpolatedValue
             (float value, float value1, float value2, long ia, long ib);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
