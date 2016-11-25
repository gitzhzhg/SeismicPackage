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

//-------------------------- vf_function_select.hh ----------------------//
//-------------------------- vf_function_select.hh ----------------------//
//-------------------------- vf_function_select.hh ----------------------//

//            header file for the VfFunctionSelect class
//               derived from the SimpleSelect class
//                          subdirectory vf

   // this class manages the list of selected velocity functions.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VF_FUNCTION_SELECT_HH_
#define _VF_FUNCTION_SELECT_HH_

#include "oprim/simple_select.hh"


class VfFunctionSelect  :  public SimpleSelect
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class VfFunctionArray *_array;

  int _several;  // whether in process of setting several flags.

//--------------------------- functions --------------------------------//
//--------------------------- functions --------------------------------//
//--------------------------- functions --------------------------------//

public:   // constructor and destructor.

           VfFunctionSelect (VfFunctionArray *array);
  virtual ~VfFunctionSelect ();

private:   // virtual functions overriding SimpleSelect.

  virtual char getSelectValue (long ifun)                 const;
  virtual void setSelectValue (long ifun, char select);

public:   // get select flags.
          // use these instead of similar functions in SimpleSelect.

  long numSelectedVelocityFunctions  ()           const;
  int  velocityFunctionIsSelected    (long ifun)  const;
  int  getSelectFlag                 (long ifun)  const;
  int  severalSelectionsInProgress   ()           const  { return _several; }

public:   // set select flags.
          // use these instead of similar functions in SimpleSelect.
          // do not call updateSelections in SimpleSelect.

  void clearSelectFlags                ();
  void incrementSelectFlag             (long ifun);
  void setSelectFlag                   (long ifun, int select);

public:   // before and after setting select flags:

    // these should be called before and after SEVERAL (more than one)
    //   consecutive calls to incrementSelectFlag or setSelectFlag.
    // these MUST be called before and after any of these activities:
    //   (1) inserting or removing one or more velocity functions.
    //   (2) sorting velocity functions.
    //   (3) setting the select flag directly in velocity functions
    //         instead of calling clearSelectFlags or incrementSelectFlag
    //         or setSelectFlag in this class.
    // these should not be called at any other time.

  void beforeSettingSeveralSelectFlags ();
  void afterSettingSeveralSelectFlags  ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
