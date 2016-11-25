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

//---------------------- statgui_merge.hh ---------------------------//
//---------------------- statgui_merge.hh ---------------------------//
//---------------------- statgui_merge.hh ---------------------------//

//             header file for the StatguiMerge class
//               derived from the SLSmartForm class
//                        subdirectory statgui



#ifndef _STATGUI_MERGE_HH_
#define _STATGUI_MERGE_HH_

#include "sl/sl_smart_form.hh"


class StatguiMerge : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class StaticManager *_manager;

  long    _action;      // one of the ACTION enums in StaticManager.
  long    _size;        // one of the SIZE   enums in StaticManager.
  long    _where;       // one of the WHERE  enums in StaticManager.
  long    _nils;        // one of the NILS   enums in StaticManager.
  long    _interp;      // one of the INTERP enums in StaticManager.
  long    _extrap;      // one of the EXTRAP enums in StaticManager.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  StatguiMerge(SLDelay *slparent, class StaticManager *manager);

  virtual ~StatguiMerge();

  void takeAction();

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
