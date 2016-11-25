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


//------------------------ table_view_data.hh ---------------------//
//------------------------ table_view_data.hh ---------------------//
//------------------------ table_view_data.hh ---------------------//

//              header file for the TableViewData class
//                    not derived from any class
//                         subdirectory sl

   // This class manages an array of floating point data which
   // is displayed in SLTableViewBox.  The actual array is outside
   // of this class.  The main purpose is to display trace headers
   // from SeisPlot, although the class can be used for other
   // purposes.

   // The true column number corresponds to the column number in
   // the array in memory.  The current column number corresponds
   // to the true column number after a new array is registered,
   // after a sort, or whenever the column number does not lie
   // within the range [istart,istop].  Otherwise, after aligning
   // selected displayable columns, the current column number
   // may differ from the true column number within the range
   // [istart,istop].  All referenced column numbers (in argument
   // lists or as returned values) refer to current columns numbers
   // unless the function name specifically indicates otherwise.

   // The _array pointer points to _nstride * _ncolumns numbers.
   // Each column in _array contains _nrow numbers.
   // The first column in _array starts at _array[0].
   // The second column in _array starts at _array[_nstride].
   // Etc.
   // Strides thru _array are from one column to the next.
   // Strides thru _array should be >= _nrows.

   // Column numbers are 1 thru _ncolumns.
   // Displayable column numbers are _istart thru _istop.
   // Visible     column numbers are _first  thru _last.

   // When a new array is read in, the selected columns are not
   // changed if the number of columns does not change.  Otherwise,
   // the selected columns are cleared.
 
 
#ifndef _TABLE_VIEW_DATA_HH_
#define _TABLE_VIEW_DATA_HH_

#include <X11/Intrinsic.h>

class TableViewData
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:    // data that remains fixed for a given array

  float  *_array;     // pointer to array of floats (outside this class).
  long    _nrows;     // number of rows    in _array.
  long    _ncolumns;  // number of columns in _array.
  long    _nstride;   // stride thru _array (>= _nrows).
  long    _istart;    // beginning displayable _array column number.
  long    _istop;     // ending    displayable _array column number.

private:    // data that can be changed for a given array

  long    _first;     // first visible _array column number.
  long    _last;      // last  visible _array column number.
  long    _numvis;    // number of visible columns.
  long    _active;    // active column number.
  long    _nseltot;   // number of selected columns in total _array.
  long    _nseldisp;  // number of selected columns in displayable _array.

private:     // internally allocated arrays

  float  *_nilvector; // vector with _nrows nil values.
  Boolean*_selected;  // vector with _ncolumns true/false values.
  long   *_truecol;   // vector with _ncolumns true column numbers.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:          // constructors and destructor

  TableViewData();
  virtual ~TableViewData();

public:        // methods to set values

  void    newArray (const float *array, long nrows, long ncolumns,
                 long nstride = 0, long istart = 1, long istop = 0);
  void    emptyArray();
  Boolean setFirstVisibleColumn    (long first);
  Boolean setNumberOfVisibleColumns(long numvis);
  Boolean setActiveColumn          (long active);

public:     // methods to select/unselect columns

  Boolean setSelectedColumn        (long column, Boolean selected = TRUE);
  Boolean toggleSelectedColumn     (long column);
  void    clearAllSelections ();
  void    clearDisplayableSelections ();

public:    // methods to sort columns

  void sortAllColumns             ();
  void alignDisplayableSelections ();

public:       // methods to get information

  const float *vector(long column)  const;
  float *array                  ()  const  { return _array   ; }
  long   numberOfRows           ()  const  { return _nrows   ; }
  long   numberOfColumns        ()  const  { return _ncolumns; }
  long   stride                 ()  const  { return _nstride ; }
  long   firstDisplayableColumn ()  const  { return _istart  ; }
  long   lastDisplayableColumn  ()  const  { return _istop   ; }
  long   firstVisibleColumn     ()  const  { return _first   ; }
  long   lastVisibleColumn      ()  const  { return _last    ; }
  long   numberOfVisibleColumns ()  const  { return _numvis  ; }
  long   activeColumn           ()  const  { return _active  ; }

  long    numberOfAllSelectedColumns         () const { return _nseltot ; }
  long    numberOfDisplayableSelectedColumns () const { return _nseldisp; }
  Boolean columnIsSelected        (long column) const;

  long   trueColumnFromCurrentColumn (long column)  const;
  long   currentColumnFromTrueColumn (long truecol) const;

private:

  void       switchColumns             (long lo, long up);
  static int sortHelper    (void *data, long lo, long up);
  static int alignHelper   (void *data, long lo, long up);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
