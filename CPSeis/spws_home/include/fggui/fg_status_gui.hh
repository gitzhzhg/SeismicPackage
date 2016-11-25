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

//---------------------- fg_status_gui.hh ---------------------------//
//---------------------- fg_status_gui.hh ---------------------------//
//---------------------- fg_status_gui.hh ---------------------------//

//              header file for the FgStatusGui class
//               derived from the SLSmartForm class
//                derived from the FgInform class
//                       subdirectory fggui

#ifndef _FG_STATUS_GUI_HH_
#define _FG_STATUS_GUI_HH_

#include "sl/sl_smart_form.hh"
#include "geom/fg_inform.hh"


class FgStatusGui : public SLSmartForm, public FgInform
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:    // also protected _fg in FgInform.

  class SLpText *_stat1;
  class SLpText *_stat2;
  class SLpText *_stat3;
  class SLpText *_stat4;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  FgStatusGui(  SLDelay             *slparent,
                char                *name,
                class FieldGeometry *fg,
                class ContainerList *clist       = NULL,
                HelpCtx              hctx        = NULL,
                Boolean              doframe     = FALSE,
                Boolean              make_if_can = TRUE,
                Boolean              manage_now  = TRUE  );

  FgStatusGui(  Widget               wparent,
                char                *name,
                class FieldGeometry *fg,
                class ContainerList *clist      = NULL,
                HelpCtx              hctx       = NULL,
                Boolean              doframe    = FALSE,
                Boolean              make_now   = TRUE,
                Boolean              manage_now = TRUE  );

  virtual ~FgStatusGui();

protected:    // overriding FgInform.

  virtual void  finishedChanges (FieldGeometry *fg);

private:

  void constructorHelper(ContainerList *clist);
  void showStatus1();
  void showStatus2();
  void showStatus3();
  void showStatus4();

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
