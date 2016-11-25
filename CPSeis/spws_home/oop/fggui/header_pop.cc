
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
//---------------------- header_pop.cc ------------------------------//
//---------------------- header_pop.cc ------------------------------//
//---------------------- header_pop.cc ------------------------------//

//           implementation file for the HeaderPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to display CPS trace headers
      // obtained from field geometry data.


#include "fggui/header_pop.hh"
#include "fggui/header_table_gui.hh"
#include "fggui/header_top_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


HeaderPop::HeaderPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE)
{
  assert(fg && dcp);
  setTitle("CPS Trace Headers");
  SLSmartForm *work  = workArea();

  HeaderTopGui   *top   = new HeaderTopGui (work, "header_top" , fg);
  FgMessageGui   *msg   = new FgMessageGui (work, "header_msg" , fg);
  FgStatusGui    *stat  = new FgStatusGui  (work, "header_stat", fg, clist);
  HeaderTableGui *table = new HeaderTableGui
                               (work, "header_table", fg, top);

  work->attach(top  , work, work, work, NULL);
  work->attach(msg  , work, work , top, NULL);
  work->attach(stat , work, work , msg, NULL);
  work->attach(table, work, work, stat, work);

  SLpPush *push_control = addBottomPush("Data Control...");
                          addBottomRemove();
                          addBottomKeyhelp();
                          addBottomHelp("HEADER_POP");

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


HeaderPop::~HeaderPop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
