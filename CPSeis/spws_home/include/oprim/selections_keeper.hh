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

//----------------------- selections_keeper.hh ---------------------------//
//----------------------- selections_keeper.hh ---------------------------//
//----------------------- selections_keeper.hh ---------------------------//

//             header file for the SelectionsKeeper class
//                    not derived from any class
//                        subdirectory oprim

  // This class maintains a list of selection flags for one or more
  // linked arrays.  For this class to work properly, the appropriate
  // function must be called each time a similar function is called
  // on the arrays to insert, remove, or replace any array elements,
  // or to reset the number of array elements.

  // This class does not allocate its array of select flags until there
  // is actually an attempt to set a flag.  This class also deallocates
  // its array if the flags are cleared.  Therefore this class does not
  // waste memory if selections are not performed.


//------------------------ start of coding --------------------------------//
//------------------------ start of coding --------------------------------//
//------------------------ start of coding --------------------------------//


#ifndef _SELECTIONS_KEEPER_HH_
#define _SELECTIONS_KEEPER_HH_

#include <stdio.h>

class SelectionsKeeper
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

public:

  enum { NO_MAYBE   = ' ',   // unspecified selection (depends on
                             //   whether flanked by YES_TOP and YES_BOTTOM).
         YES_MAYBE  = '*',   // unspecified selection (depends on
                             //   whether flanked by YES_TOP and YES_BOTTOM).
         YES_ALWAYS = 'Y',   // yes definitely selected.
         NO_ALWAYS  = 'n',   // no definitely not selected.
         YES_TOP    = 'T',   // top of selected range.
         YES_BOTTOM = 'B'    // bottom of selected range.
     };

private:

  long    _nelements;   // length of the array (number of elements).
  char   *_array;       // array of selection flags.
  long    _nselect;     // number of selections.


//------------------------------- functions -------------------------------//
//------------------------------- functions -------------------------------//
//------------------------------- functions -------------------------------//

public:

           SelectionsKeeper ();
  virtual ~SelectionsKeeper ();

public:  // get values.

  long        numElements        ()            const  { return _nelements; }
  long        numSelected        ()            const  { return _nselect; }
  int         getSelectFlag      (long index)  const;
  const char *getSelectString    (long index)  const;
  int         isSelected         (long index)  const;

public:  // set values.

  void  beforeSettingSeveralSelectFlags ();
  void  setOneOfSeveralSelectFlags      (long index, int select_flag);
  void  afterSettingSeveralSelectFlags  ();

  void  setSelectFlag            (long index, int select_flag);
  void  incrementSelectFlag      (long index);
  void  toggleAllSelections      ();
  void  clearSelectFlags         ();
  void  setAllSelections         ();

public:  // change number of elements.

  void  setOrAppendValue         (long index);
  void  appendElement            ();
  void  insertElement            (long index);
  void  removeElement            (long index);

  void  deleteAllElements        ();
  void  resetNumElements         (long nelements);
  void  resetNumElementsAndClear (long nelements);
  void  copyAllElements          (const SelectionsKeeper *another);

public:  // copy to or from a binary file.
         // these return error TRUE or FALSE.

  int   binaryRead               (FILE *stream);
  int   binaryWrite              (FILE *stream)  const;

private:

  void  allocateArrayIfNecessary();
  void  updateSelections();


//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

} ;

#endif

//--------------------------------- end -----------------------------------//
//--------------------------------- end -----------------------------------//
//--------------------------------- end -----------------------------------//
