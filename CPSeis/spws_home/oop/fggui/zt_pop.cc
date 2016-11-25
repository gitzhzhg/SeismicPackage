
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
//---------------------- zt_pop.cc ------------------------------//
//---------------------- zt_pop.cc ------------------------------//
//---------------------- zt_pop.cc ------------------------------//

//             implementation file for the ZtPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to edit ZT (zero-trace)
      // cards within field geometry data.


#include "fggui/zt_pop.hh"
#include "fggui/zt_table_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


ZtPop::ZtPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                int            which,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE)
{
  assert(fg);
  char htitle[50];
  switch(which)
      {
      case 1: setTitle("Zero Sources (ZT1)");
              strcpy(htitle, "ZT1_CARDS");
              break;
      case 2: setTitle("Zero Receivers (ZT2)");
              strcpy(htitle, "ZT2_CARDS");
              break;
      case 3: setTitle("Zero Traces In Groups (ZT3)");
              strcpy(htitle, "ZT3_CARDS");
              break;
      case 4: setTitle("Zero Selected Ranges of Sources and Receivers (ZT4)");
              strcpy(htitle, "ZT4_CARDS");
              break;
      default: assert(FALSE);
      }
  SLSmartForm  *work  = workArea();

  FgMessageGui *msg   = new FgMessageGui (work, "zt_msg"  , fg);
  FgStatusGui  *stat  = new FgStatusGui  (work, "zt_stat" , fg, clist);
  ZtTableGui   *table = new ZtTableGui   (work, "zt_table", fg, which);

  work->attach(msg  , work, work, work, NULL);
  work->attach(stat , work, work, msg , NULL);
  work->attach(table, work, work, stat, work);

  SLpPush *push_control = addBottomPush("Data Control...");
  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp(htitle);

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


ZtPop::~ZtPop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
