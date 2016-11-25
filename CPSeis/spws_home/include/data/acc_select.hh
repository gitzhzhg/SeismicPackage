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

//------------------------ acc_select.hh ---------------------//
//------------------------ acc_select.hh ---------------------//
//------------------------ acc_select.hh ---------------------//

//             header file for the AccSelect class
//               derived from the AccBase class
//          also derived from the SimpleSelect class
//                    subdirectory oprim

     // This class resets values in array elements in
     //   a class derived from SmartArray.
     // One value (identified with an ident) can be reset
     //   by this class.
     // The method is to reset selection criteria based on
     //   other selection criteria.
     // See the implementation file for documentation.


#ifndef _ACC_SELECT_HH_
#define _ACC_SELECT_HH_

#include "oprim/acc_base.hh"
#include "oprim/simple_select.hh"


class AccSelect : public AccBase, public SimpleSelect
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  typedef char GetSelectValue (void *data, long index);
  typedef void SetSelectValue (void *data, long index, char select);

  GetSelectValue * const _get_select_value;
  SetSelectValue * const _set_select_value;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor.

  AccSelect(SmartArray *array, int ident,
                     GetSelectValue *get_select_value,
                     SetSelectValue *set_select_value);
  virtual ~AccSelect();

private:   // hide public functions in SimpleSelect.

  void registerSelectArray () {}
  void updateSelections    () {}

public:   // replace public function in SimpleSelect.

  void clearSelections();

public:   // overriding SimpleSelect.

  virtual char   getSelectValue   (long index)  const;
  virtual void   setSelectValue   (long index, char select);

private:   // overriding AccBase

  virtual void getRange (long n, long *index1, long *index2);
  virtual void fixRange (long n, long  index1, long  index2);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
