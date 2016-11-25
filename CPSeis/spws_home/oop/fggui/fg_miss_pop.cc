
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
//---------------------- fg_miss_pop.cc ------------------------------//
//---------------------- fg_miss_pop.cc ------------------------------//
//---------------------- fg_miss_pop.cc ------------------------------//

//           implementation file for the FgMissPop class
//                 derived from the SLDialog class
//                 derived from the FgInform class
//                       subdirectory fggui

      // This dialog box is used to display missing information
      // within field geometry data.


#include "fggui/fg_miss_pop.hh"
#include "fggui/fg_miss_table_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//------------------------- traps -----------------------------//
//------------------------- traps -----------------------------//
//------------------------- traps -----------------------------//


static void list_trap(void *data, long /*ident*/)
{
  FgMissPop *pop = (FgMissPop*)data;
  FgMissTableGui *table = pop->getFgMissTableGui();
  FieldGeometry *fg = pop->getFieldGeometry();
  fg->preSlowOperations();
  fg->showMessage("getting list of missing information...");
  table->updateMissingInfo();
  fg->showMessage("list of missing information has been completed");
  fg->postSlowOperations();
  pop->setUpdateFlag(TRUE);
}


static char *list_label_fun(void *data)
{
  static char *buffer1 = "Update Missing Info";
  static char *buffer2 = "List Missing Info";
  FgMissPop *pop = (FgMissPop*)data;
  FgMissTableGui *table = pop->getFgMissTableGui();
  long n = table->numMessages();
  if(n > 2) return buffer1;
  return buffer2;
}


static long list_sense_fun(void *data)
{
  FgMissPop *pop = (FgMissPop*)data;
  return !pop->getUpdateFlag();
}


static void delete_trap(void *data, long /*ident*/)
{
  FgMissPop *pop = (FgMissPop*)data;
  FgMissTableGui *table = pop->getFgMissTableGui();
  table->deleteMissingInfo();
  pop->setUpdateFlag(FALSE);
}


static long delete_sense_fun(void *data)
{
  FgMissPop *pop = (FgMissPop*)data;
  FgMissTableGui *table = pop->getFgMissTableGui();
  return (table->numMessages() > 2);
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


FgMissPop::FgMissPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
              FgInform(fg),
                        _table    (NULL),
                        _updated  (FALSE)
{
  assert(fg && dcp);
  setTitle("List of Missing Information");
  SLSmartForm *work = workArea();
  FgMessageGui *msg   = new FgMessageGui   (work, "miss_msg"  , fg);
  FgStatusGui  *stat  = new FgStatusGui    (work, "miss_stat" , fg, clist);
               _table = new FgMissTableGui (work, "miss_table", fg, this);

  work->attach( msg  , work, work ,work, NULL);
  work->attach( stat , work, work ,msg , NULL);
  work->attach(_table, work, work, stat, work);

  SLpPush *push_control = addBottomPush("Data Control...");
  SLpPush *push_list    = addBottomPush("List Missing Info");
  SLpPush *push_delete  = addBottomPush("Delete List");
                          addBottomRemove();
                          addBottomKeyhelp();
                          addBottomHelp("FG_MISS_POP");

  push_control->manageShellWhenPressed((SLShellContainer*)dcp);
  push_list   ->setAtrap      (list_trap       , this);
  push_list   ->setupLabelFun (list_label_fun  , this);
  push_list   ->setupSenseFun (list_sense_fun  , this);
  push_delete ->setAtrap      (delete_trap     , this);
  push_delete ->setupSenseFun (delete_sense_fun, this);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


FgMissPop::~FgMissPop()
{
}




//-------------------- overriding FgInform --------------------//
//-------------------- overriding FgInform --------------------//
//-------------------- overriding FgInform --------------------//

       // protected virtual functions overriding FgInform.


void FgMissPop::postRemoveInsertFlags      (FieldGeometry*,
                                 long ,long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertLines      (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertRpCards    (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertPpCards    (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertZt1Cards   (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertZt2Cards   (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertZt3Cards   (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postRemoveInsertZt4Cards   (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }



void FgMissPop::postResumeDependentUpdates (FieldGeometry*)
           { _updated = FALSE; }

void FgMissPop::dependentValuesOutOfDate   (FieldGeometry*)
           { _updated = FALSE; }

void FgMissPop::postReverseLineDirection   (FieldGeometry*, long)
           { _updated = FALSE; }

void FgMissPop::postSortReceiverPatterns   (FieldGeometry*)
           { _updated = FALSE; }



void FgMissPop::postFlagValuesChanged      (FieldGeometry*,
                  long, int ident, long, long, long)
           { if(ident == FG_SHOT) _updated = FALSE; }

void FgMissPop::postLineNumbersChanged     (FieldGeometry*,
                                       long, long, long)
           { _updated = FALSE; }

void FgMissPop::postPpValuesChanged        (FieldGeometry*,
                      int ident, long, long, long)
           { if(ident == PP_SSHOT || ident == PP_SLINE ||
                ident == PP_RSHOT || ident == PP_RLINE ||
                ident == PP_NGROUPS) _updated = FALSE; }

void FgMissPop::postRpValuesChanged        (FieldGeometry*,
                      int ident, long, long, long)
           { if(ident == RP_PAT || ident == RP_SHOT ||
                ident == RP_LINE) _updated = FALSE; }

void FgMissPop::postZt1ValuesChanged       (FieldGeometry*,
                      int ident, long, long, long)
           { if(ident == ZT1_SHOT1 || ident == ZT1_SHOT2 ||
                ident == ZT1_LINE) _updated = FALSE; }

void FgMissPop::postZt2ValuesChanged       (FieldGeometry*,
                      int ident, long, long, long)
           { if(ident == ZT2_SHOT1 || ident == ZT2_SHOT2 ||
                ident == ZT2_LINE) _updated = FALSE; }

void FgMissPop::postZt3ValuesChanged       (FieldGeometry*,
                      int ident, long, long, long)
           { if(ident == ZT3_GROUP1 || ident == ZT3_GROUP2)
                                   _updated = FALSE; }

void FgMissPop::postZt4ValuesChanged       (FieldGeometry*,
                      int ident, long, long, long)
           { if(ident == ZT4_SSHOT1 || ident == ZT4_SSHOT2 ||
                ident == ZT4_RSHOT1 || ident == ZT4_RSHOT2 ||
                ident == ZT4_SLINE  || ident == ZT4_RLINE)
                                   _updated = FALSE; }



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
