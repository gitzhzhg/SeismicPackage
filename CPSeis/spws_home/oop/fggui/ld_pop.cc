
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
//---------------------- ld_pop.cc ------------------------------//
//---------------------- ld_pop.cc ------------------------------//
//---------------------- ld_pop.cc ------------------------------//

//           implementation file for the LdPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to edit LD (line description)
      // cards within field geometry data.


#include "fggui/ld_pop.hh"
#include "fggui/ld_table_gui.hh"
#include "fggui/ld_top_gui.hh"
#include "fggui/ld_edit_pop.hh"
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


LdPop::LdPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                LdEditPop     *lde,
                                int            table_option,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                        _fg       (fg),
                        _lde      (lde),
                        _table    (NULL)
{
  assert(fg && dcp && lde);
  char htitle[50];
  switch(table_option)
      {
      case 1: setTitle("Line Description Table");
                                strcpy(htitle, "LD_CARDS");
                                break;
      case 2: setTitle("Additional LD Table");
                                strcpy(htitle, "ADDITIONAL_LD_CARDS");
                                break;
      case 3: setTitle("LD Quality Control Table");
                                strcpy(htitle, "LD_CARDS_QC");
                                break;
      default: assert(FALSE);
      }
  SLSmartForm  *work = workArea();

  LdTopGui     *top   = new LdTopGui     (work, "ld_top"  , fg);
  FgMessageGui *msg   = new FgMessageGui (work, "ld_msg"  , fg);
  FgStatusGui  *stat  = new FgStatusGui  (work, "ld_stat" , fg, clist);
               _table = new LdTableGui   (work, "ld_table", fg,
                                                top, table_option);

  work->attach( top  , work, work , work, NULL);
  work->attach( msg  , work, work ,  top, NULL);
  work->attach( stat , work, work ,  msg, NULL);
  work->attach(_table, work, work , stat, work);

  SLSmartForm *bottom = bottomArea();

  SLpPush *push_control = addBottomPush("Data Control...");
  SLpPush *push_edit    = addBottomPush("Edit LD Cards...");
                          addBottomRemove();
                          addBottomKeyhelp();
                          addBottomHelp(htitle);

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
  push_edit   ->manageShellWhenPressed((SLShellContainer*)lde);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


LdPop::~LdPop()
{
}



//-------------------- post manage notify ---------------------//
//-------------------- post manage notify ---------------------//
//-------------------- post manage notify ---------------------//

       // protected virtual function overriding SLDialog.

void LdPop::postManageNotify()
{
  long ixl = _fg->getActiveLineIndex();
  _table->postNewActiveFlag(_fg, ixl);
}



//-------------------- post unmanage notify ---------------------//
//-------------------- post unmanage notify ---------------------//
//-------------------- post unmanage notify ---------------------//

       // protected virtual function overriding SLDialog.

void LdPop::postUnmanageNotify()
{
  _lde->unmanage();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
