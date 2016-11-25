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

//--------------------- several_float_arrays.hh ------------------------//
//--------------------- several_float_arrays.hh ------------------------//
//--------------------- several_float_arrays.hh ------------------------//

//             header file for the SeveralFloatArrays class
//                     not derived from any class
//                         subdirectory oprim


       // This class contains several linked arrays of floating
       // point numbers.  This class can read these arrays from
       // a file, or save them to a file.  Virtual functions
       // are available for customizing this class.


#ifndef _SEVERAL_FLOAT_ARRAYS_HH_
#define _SEVERAL_FLOAT_ARRAYS_HH_

#include "named_constants.h"
#include <stdio.h>


class SeveralFloatArrays
{

//-------------------------- data ------------------------------------//
//-------------------------- data ------------------------------------//
//-------------------------- data ------------------------------------//

public:

  enum { COLUMNS,  // read/save in column-oriented binary format.
         SEPARATE  // read/save each array separately, one after the other.
       };

private:

  const int    _ncolumns;   // number of linked arrays (>= 1).
  const char  *_filetype;   // type of file (e.g. "horizon" or "velocity");
  int          _rerror;     // read error.
  int          _werror;     // write error.
  long         _nseg;       // number of segments.
  long        *_first;      // first index of each segment.

  class ActiveIndexKeeper  *_active;
  class SimpleFloatArray  **_arrays;


//---------------------------- functions -----------------------------//
//---------------------------- functions -----------------------------//
//---------------------------- functions -----------------------------//

public:

           SeveralFloatArrays (const int ncolumns,
                               const char *filetype,
                               const float tolerance = 1.0e-5);
  virtual ~SeveralFloatArrays ();

private:

  class SimpleFloatArray *array(int icol)  const;

public:   // get values.

  int         numColumns     ()       const  { return _ncolumns; }
  long        numElements    ()       const;
  long        getActiveIndex ()       const;
  int         readError      ()       const  { return _rerror; }
  int         writeError     ()       const  { return _werror; }

  float       getValue          (int icol, long index)  const;
  float       minimumValue      (int icol)              const;
  float       maximumValue      (int icol)              const;
  long        numNilValues      (int icol)              const;
  long        numLiveValues     (int icol)              const;
  int         isAscending       (int icol)              const;
  int         isDescending      (int icol)              const;
  int         isEquallySpaced   (int icol)              const;

public:   // set values.

  void        setNumElements     (long nelements);
  void        setActiveIndex     (long active);
  void        setValue           (int icol, long index, float value);
  void        setLastValue       (int icol,             float value);
  void        multiplyByConstant (int icol, float constant);
  void        addConstant        (int icol, float constant);

public: // set or append one value.
        // setOrAppendValue simply calls setValue    if index <  numElements().
        // setOrAppendValue first calls appendNilRow if index == numElements().

  void  setOrAppendValue    (int icol, long index, float value);

public: // insert or remove one row (one element in each array).
        // these functions reallocate the arrays.
        // all arrays always have the same number of elements.
        // when a value is inserted, all other columns receive nils.
        // these functions assert if icol or index is out of range.

  void  appendNilRow        ();
  void  appendRow           (int icol,             float value);
  void  insertNilRow                  (long index);
  void  insertRowFromBuffers          (long index);
  void  insertRow           (int icol, long index, float value);
  void  removeRow                     (long index);
  void  removeRowToBuffers            (long index);

public:   // get segment-specific values.
          // numSegments is zero if numElements is zero.
          // numSegments is one unless addSegment was called.
          // 0 <= sindex < numElementsInSegment.
          // 0 <=  index < numElements.
          // getSegmentNumber returns iseg+1 for the specified index.

  long   numSegments            ()                                   const;
  long   startingIndexOfSegment (long iseg)                          const;
  long   numElementsInSegment   (long iseg)                          const;
  long   getSegmentNumber       (long index)                         const;
  float  getValueInSegment      (long iseg, long icol, long sindex)  const;

public: // set segment-specific values.
        // addSegment documentation:
        //   specifies the starting index of a new segment.
        //   asserts if the index <= start of the previous segment.
        //   asserts if the index >= current number of elements.
        //   numSegments is reset to zero (or one) by the following functions:
        //       setNumElements
        //       import file
        //       readBinary file
        //   can be called multiple times after calling the above functions,
        //     or after calling supplyOneNilRow.
        //   addSegment will do nothing if index is zero.

  void   deleteSegments ();
  void   addSegment     (long index);

public:   // read and write disk file.
          // validateFile does not change anything in this class.
          // validateFile could have been static except for the desirability
          //  to call a virtual function.
          // validate/import/export... access a text file.
          // read/save... access a binary file.
          // validate... puts parameters from file into FloatioWrapper.
          // import... reads file using parameters in FloatioWrapper.
          // export... writes file using parameters in FloatioWrapper.
          // these set info or msg.
          // these return error TRUE or FALSE.

  int  validateFile   (class FloatioWrapper *floatio,
                       const char *filename, char *info);
  int  importFile     (class FloatioWrapper *floatio,
                       const char *filename, char *msg);
  int  exportFile     (class FloatioWrapper *floatio,
                       const char *filename, char *msg);

  int  readBinaryFile (const char *filename, char *msg, int method = COLUMNS);
  int  saveBinaryFile (const char *filename, char *msg, int method = COLUMNS);

protected:  

  virtual float getExportValue (int icol, long index);

            // virtual functions to override.
            // these are called from the above functions.
            // these set info or msg.
            // these return error TRUE or FALSE (FALSE if not overridden).
            // if info or msg is not reset, a generic message is substituted.
            // virtualValidateHeader should not change anything in the
            //  derived class.

  virtual int  virtualValidateHeader (class FloatioWrapper* /*floatio*/,
                                      char* /*info*/) { return FALSE; }
  virtual int  virtualImportHeader   (class FloatioWrapper* /*floatio*/,
                                      char* /*msg*/ ) { return FALSE; }
  virtual int  virtualExportHeader   (class FloatioWrapper* /*floatio*/,
                                      char* /*msg*/ ) { return FALSE; }

  virtual int  virtualReadBinaryHeader
                           (FILE* /*stream*/, char* /*msg*/) { return FALSE; }
  virtual int  virtualSaveBinaryHeader
                           (FILE* /*stream*/, char* /*msg*/) { return FALSE; }


//------------------------- end of functions --------------------------//
//------------------------- end of functions --------------------------//
//------------------------- end of functions --------------------------//

} ;

#endif

//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
