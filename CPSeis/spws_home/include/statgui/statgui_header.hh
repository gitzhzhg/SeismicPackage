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

//---------------------- statgui_header.hh ---------------------------//
//---------------------- statgui_header.hh ---------------------------//
//---------------------- statgui_header.hh ---------------------------//

//             header file for the StatguiHeader class
//                 derived from the SLSmartForm class
//                        subdirectory statgui

       // This class displays static header data from a pickle jar
       // or from the active and/or _proposed dataset.


  // For each variable, the following table shows where that variable
  // resides, and whether that variable is editable.
  //
  // "act" means the variable resides in the active dataset.
  // "pro" means the variable resides in the proposed dataset.
  // "val" means the variable resides in the pickle jar.
  //
  // No variables are editable if editable is NOT_EDITABLE.
  // Otherwise, "E" shows which variables are editable.
  //
  //   variable         ACTIVE   VALIDATE   RESAMPLE
  //   --------         ------   --------   --------
  //   stattype         E act     E val        act
  //   nhx and nhy      E act     E val        act
  //   nhx2 and nhy2    E act     E val        act
  //   x1 and y1        E act     E val      E pro
  //   xinc and yinc    E act     E val      E pro
  //   xend and yend    E act     E val      E pro
  //   nx and ny        E act     E val      E pro
  //   history cards      act       val        act
  //   number of nils     act       val        act
  //   min/max values     act       val        act


#ifndef _STATGUI_HEADER_HH_
#define _STATGUI_HEADER_HH_

#include "sl/sl_smart_form.hh"


class StatguiHeader : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

public:

  enum { NOT_EDITABLE,           // not editable and use small print.
         EDITABLE_SMALL_PRINT,   // editable and use small print.
         EDITABLE_LARGE_PRINT    // editable and use larger print.
       };

  enum { ACTIVE,     // displaying active dataset (obtained from _manager).
         VALIDATE,   // displaying validated input/output file (using _statio).
         RESAMPLE    // displaying proposed resampled data (in _proposed)
                     //   and unchangeable data (in the active dataset).
       };

private:

  class StaticDataset   *_proposed;  // not used with ACTIVE or VALIDATE.
  class StaticManager   *_manager;   // not used with VALIDATE.
  int                    _display;   // one of the above enums.
  class StatioWrapper   *_statio;    // pointer to external object.
  class SLFileChoice    *_choice;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:    // set editable to one of the above enums.
           // set display to one of the above enums.
           // _proposed must be NULL when display is ACTIVE or VALIDATE.
           // _manager  must be NULL when display is VALIDATE.

  StatguiHeader (SLDelay *slparent,
                 class StaticDataset *proposed, class StaticManager *manager,
                 const char *title, int editable, int display,
                 class SLFileChoice *choice = NULL,
                 class StatioWrapper *statio = NULL);

  virtual ~StatguiHeader();

  class StaticDataset   *proposed ()  const  { return _proposed; }
  class StaticManager   *manager  ()  const  { return _manager;  }
  int                    display  ()  const  { return _display;  }
  class SLFileChoice    *choice   ()  const  { return _choice;   }
  class StatioWrapper   *statio   ()  const  { return _statio;   }

public:  // returns the appropriate dataset (_proposed or the active dataset).
         // which is an enum hidden in the implementation file.

  StaticDataset *dataset(int which)  const;

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
