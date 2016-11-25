
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
//---------------------- linelist_pop.cc ------------------------------//
//---------------------- linelist_pop.cc ------------------------------//
//---------------------- linelist_pop.cc ------------------------------//

//           implementation file for the LinelistPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to edit seismic lines
      // within field geometry data.


#include "fggui/linelist_pop.hh"
#include "fggui/linelist_table_gui.hh"
#include "fggui/linelist_top_gui.hh"
#include "fggui/line_create_pop.hh"
#include "fggui/ld_edit_pop.hh"
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
  fg->sortByLineNumber();
}


static void clear_trap(void *data, long /*ident*/)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->clearLineSelections();
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


LinelistPop::LinelistPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                LdEditPop     *lde,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                   _lde   (lde)
{
  assert(fg && dcp && lde);
  setTitle("List of Seismic Lines in Survey");
  SLSmartForm *work = workArea();

  LinelistTopGui   *top   = new LinelistTopGui   (work, "linelist_top"  , fg);
  FgMessageGui     *msg   = new FgMessageGui     (work, "linelist_msg"  , fg);
  FgStatusGui      *stat  = new FgStatusGui (work, "linelist_stat", fg, clist);
  LinelistTableGui *table = new LinelistTableGui (work, "linelist_table", fg);
  LineCreatePop    *cre   = new LineCreatePop    (work, "line_create",hctx,fg);

  work->attach(top  , work, work, work, NULL);
  work->attach(msg  , work, work, top , NULL);
  work->attach(stat , work, work, msg , NULL);
  work->attach(table, work, work, stat, work);

  SLpPush *push_control = addBottomPush("Data Control...");
                          addBottomPush("Sort Lines"      , 0,  sort_trap, fg);
                          addBottomPush("Clear Selections", 0, clear_trap, fg);
  SLpPush *push_create  = addBottomPush("Create Lines...");
  SLpPush *push_edit    = addBottomPush("Edit LD Cards...");
                          addBottomRemove();
                          addBottomKeyhelp();
                          addBottomHelp("LINELIST_POP");

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
  push_create ->manageShellWhenPressed((SLShellContainer*)cre);
  push_edit   ->manageShellWhenPressed((SLShellContainer*)lde);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


LinelistPop::~LinelistPop()
{
}




//-------------------- post unmanage notify ---------------------//
//-------------------- post unmanage notify ---------------------//
//-------------------- post unmanage notify ---------------------//

       // protected virtual function overriding SLDialog.

void LinelistPop::postUnmanageNotify()
{
  _lde->unmanage();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
