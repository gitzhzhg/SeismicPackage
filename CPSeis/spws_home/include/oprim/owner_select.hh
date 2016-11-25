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

//--------------------------- owner_select.hh -------------------------------//
//--------------------------- owner_select.hh -------------------------------//
//--------------------------- owner_select.hh -------------------------------//

//             header file for the OwnerSelect class
//              derived from the SimpleSelect class 
//                     subdirectory oprim

     // this class differs from the SimpleSelect class in that it
     // owns the array of selections. it uses the SimpleSelect class to
     // do the work.

     // incrementSelectValue and setSelectValue must be followed
     // by updateSelections in order that numSelected will return
     // the correct value.

     // getSelectArrayForUpdate must be followed by updateSelections
     // after all desired changes are made to the array.

     // before calling any function, updateArrayLengthIfNecessary must
     // be called first, passing the number of elements.  if the number
     // changes from the last call, a new array will be allocated to
     // hold the current number of selections, and all selections will
     // be cleared.

     // if elements are to be inserted or removed, with adjustment of
     // the select values accordingly, removeOrInsertElements should
     // be called, passing the new length of the array and the index where
     // the insertion or deletion occurred.

     // this class was put together somewhat hurredly, but it does the
     // job it was needed to do at this time.  it is not as nice and clean
     // as it could be.


#ifndef _OWNER_SELECT_HH_
#define _OWNER_SELECT_HH_

#include "oprim/simple_select.hh"


class OwnerSelect  :  public SimpleSelect
{

//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//

private:

  long   _num;    // length of the _sel array.
  char  *_sel;    // array of selection flags.


//------------------------------- functions --------------------------------//
//------------------------------- functions --------------------------------//
//------------------------------- functions --------------------------------//

public:   // constructor and destructor.

           OwnerSelect ();
  virtual ~OwnerSelect ();

  void  updateArrayLengthIfNecessary(long num);
  void  removeOrInsertElements      (long num, long index);
  void  copyAllElements             (OwnerSelect *another);

  long        numElements            ()  const  { return _num; }
  const char *getSelectArray         ()  const  { return _sel; }
        char *getSelectArrayForUpdate()  const  { return _sel; }

/*******
  ///// base class functions to call:
  void   clearSelections      (long num);
  void   setAllSelections     (long num);
  void   toggleAllSelections  (long num);
  void   updateSelections     (long num);
  long   numSelected          ()            const;
  void   incrementSelectValue (long index);
  int    isSelected           (long index)  const;
*******/

/*******
  ///// virtual functions (not overridden):
  virtual char getSelectValue (long index)  const;
  virtual void setSelectValue (long index, char select_flag);
*******/


//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

} ;

#endif

//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
