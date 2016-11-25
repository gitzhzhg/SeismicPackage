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

//---------------------- statgui_status.hh ---------------------------//
//---------------------- statgui_status.hh ---------------------------//
//---------------------- statgui_status.hh ---------------------------//

//              header file for the StatguiStatus class
//                derived from the SLSmartForm class
//                derived from the StaticInform class
//                       subdirectory statgui

#ifndef _STATGUI_STATUS_HH_
#define _STATGUI_STATUS_HH_

#include "sl/sl_smart_form.hh"
#include "stat/static_inform.hh"


class StatguiStatus : public SLSmartForm, public StaticInform
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:    // also public manager() in StaticInform.

  int            _clear_flag;
  class SLpText *_msg;
  class SLpText *_status2;
  class SLpText *_status3;
  class SLpText *_status4;
  class SLpText *_status5;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  StatguiStatus (  SLDelay             *slparent,
                   class StaticManager *manager,
                   class ContainerList *clist,
                   int                  wide = TRUE);

  StatguiStatus (  Widget               wparent,
                   class StaticManager *manager,
                   class ContainerList *clist,
                   int                  wide = TRUE);

  virtual ~StatguiStatus();

private:    // overriding StaticInform.

  virtual void  showMessage          (const char *msg);
  virtual void  returningToEventLoop ();

private:

  void  constructorHelper (class ContainerList *clist, int wide);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
