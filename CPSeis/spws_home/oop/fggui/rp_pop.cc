
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
//---------------------- rp_pop.cc ------------------------------//
//---------------------- rp_pop.cc ------------------------------//
//---------------------- rp_pop.cc ------------------------------//

//           implementation file for the RpPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to edit RP (receiver pattern)
      // cards within field geometry data.


#include "fggui/rp_pop.hh"
#include "fggui/rp_table_gui.hh"
#include "fggui/rp_top_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//


static void sort_trap(void *data, long /*ident*/)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->sortReceiverPatterns();
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


RpPop::RpPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                        _fg       (fg),
                        _table    (NULL)
{
  assert(fg && dcp);
  setTitle("Receiver Pattern Table");
  SLSmartForm *work  = workArea();

  RpTopGui     *top   = new RpTopGui     (work, "rp_top"  , fg);
  FgMessageGui *msg   = new FgMessageGui (work, "rp_msg"  , fg);
  FgStatusGui  *stat  = new FgStatusGui  (work, "rp_stat" , fg, clist);
               _table = new RpTableGui   (work, "rp_table", fg);

  work->attach( top  , work, work, work, NULL);
  work->attach( msg  , work, work, top , NULL);
  work->attach( stat , work, work, msg , NULL);
  work->attach(_table, work, work, stat, work);

  SLpPush *push_control = addBottomPush("Data Control...");
  addBottomPush("Sort Receiver Patterns", 0, sort_trap  , fg);
  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp("RP_POP");

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


RpPop::~RpPop()
{
}



//-------------------- post manage notify ---------------------//
//-------------------- post manage notify ---------------------//
//-------------------- post manage notify ---------------------//

       // protected virtual function overriding SLDialog.

void RpPop::postManageNotify()
{
  _table->postNewActiveRpCard(_fg);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
