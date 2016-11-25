
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
//---------------------- groups_pop.cc ------------------------------//
//---------------------- groups_pop.cc ------------------------------//
//---------------------- groups_pop.cc ------------------------------//

//           implementation file for the GroupsPop class
//                 derived from the SLDialog class
//                       subdirectory fggui


#include "fggui/groups_pop.hh"
#include "fggui/groups_table_gui.hh"
#include "fggui/groups_top_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



/*
//------------------------- traps -----------------------------//
//------------------------- traps -----------------------------//
//------------------------- traps -----------------------------//


static void clear_trap(void *data, long / *ident* /)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  fg->clearGroupSelections();   // not implemented yet
}
*/



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


GroupsPop::GroupsPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                        _fg       (fg),
                        _table    (NULL)
{
  assert(fg && dcp);
  setTitle("List of Source Gathers (Groups)");
  SLSmartForm  *work  = workArea();
  GroupsTopGui *top   = new GroupsTopGui   (work, "groups_top"  , fg);
  FgMessageGui *msg   = new FgMessageGui   (work, "groups_msg"  , fg);
  FgStatusGui  *stat  = new FgStatusGui    (work, "groups_stat" , fg, clist);
               _table = new GroupsTableGui (work, "groups_table", fg);

  work->attach( top  , work, work, work, NULL);
  work->attach( msg  , work, work ,top , NULL);
  work->attach( stat , work, work ,msg , NULL);
  work->attach(_table, work, work, stat, work);

  SLpPush *push_control = addBottomPush("Data Control...");
/*
  SLpPush *push_clear   = addBottomPush("Clear Selections");
*/
                          addBottomRemove();
                          addBottomKeyhelp();
                          addBottomHelp("GROUPS_POP");

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
/*
  push_clear  ->setAtrap(clear_trap, fg);
*/
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


GroupsPop::~GroupsPop()
{
}



//-------------------- post manage notify ---------------------//
//-------------------- post manage notify ---------------------//
//-------------------- post manage notify ---------------------//

       // protected virtual function overriding SLDialog.

void GroupsPop::postManageNotify()
{
  _table->postNewActiveGroup(_fg);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
