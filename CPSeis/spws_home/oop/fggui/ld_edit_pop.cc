
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
//---------------------- ld_edit_pop.cc ------------------------------//
//---------------------- ld_edit_pop.cc ------------------------------//
//---------------------- ld_edit_pop.cc ------------------------------//

//           implementation file for the LdEditPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to build/edit LD cards.


#include "fggui/ld_edit_pop.hh"
#include "fggui/ld_choice_gui.hh"
#include "fggui/ld_operation_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <assert.h>


//---------------------- get choice or operation ------------------//
//---------------------- get choice or operation ------------------//
//---------------------- get choice or operation ------------------//

long LdEditPop::getChoice()  const
{
  return _cc->getChoice();
}


long LdEditPop::getOperation()  const
{
  return _oo->getOperation();
}



//--------------------- make shotpoint independent -----------------//
//--------------------- make shotpoint independent -----------------//
//--------------------- make shotpoint independent -----------------//

    // make the shotpoint independent (i.e. turn off the
    // dependency flag) to make sure the shotpoint does not
    // change when cards are inserted or removed.
    // (does nothing if ixf is out of range)

static void make_shotpoint_independent(FieldGeometry *fg,
                                          long ixl, long ixf)
{
  long nflags = fg->numFlagsOnLine(ixl);
  if(ixf < 0 || ixf >= nflags) return;
  float sp = fg->getShotpoint(ixl, ixf);
  fg->setShotpoint(ixl, ixf, sp);
}



//------------------ do specific operations ---------------------//
//------------------ do specific operations ---------------------//
//------------------ do specific operations ---------------------//

     // these operations are to be done on the specified line.

static void do_delete_shotpoints(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  float   delete_sp1 = oo->getDeleteSp1();
  float   delete_sp2 = oo->getDeleteSp2();
  if(delete_sp1 == FNIL) return;
  if(delete_sp2 == FNIL) return;
  if(!fg->allowDeletingData()) return;
  long ixf1 = fg->findMatchingShotpointOnLine(ixl, delete_sp1);
  long ixf2 = fg->findMatchingShotpointOnLine(ixl, delete_sp2);
  if(ixf1 == -1) return;
  if(ixf2 == -1) return;
  make_shotpoint_independent(fg, ixl, ixf1-1);
  make_shotpoint_independent(fg, ixl, ixf2+1);
  for(long ixf = ixf2; ixf >= ixf1; ixf--)
      {
      fg->deleteFlagFromLine(ixl, ixf);
      }
}


static void do_clear_selected_flags(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  fg->clearFlagSelections(ixl);
}


static void do_insert_between(LdEditPop *pop, long ixl)
{
  FieldGeometry   *fg = pop->getFieldGeometry();
  LdOperationGui  *oo = pop->getLdOperationGui();
  long insert_between = oo->getInsertBetween();
  if(insert_between <= 0) return;
  long nflags = fg->numFlagsOnLine(ixl);
  if(nflags <= 1) return;
  for(long ixf = nflags-1; ixf >= 1; ixf--)
      {
      for(long i = 0; i < insert_between; i++)
          {
          fg->insertNewFlagOnLine(ixl, ixf);
          }
      }
}


static void do_insert_before(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  long insert_before = oo->getInsertBefore();
  float    sp_before = oo->getSpBefore();
  if(insert_before <= 0) return;
  if(sp_before == FNIL) return;
  long ixf = fg->findMatchingShotpointOnLine(ixl, sp_before);
  if(ixf == -1) return;
  make_shotpoint_independent(fg, ixl, ixf-1);
  make_shotpoint_independent(fg, ixl, ixf);
  for(long i = 0; i < insert_before; i++)
      {
      fg->insertNewFlagOnLine(ixl, ixf);
      }
}


static void do_insert_after(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  long  insert_after = oo->getInsertAfter();
  float     sp_after = oo->getSpAfter();
  if(insert_after <= 0) return;
  if(sp_after == FNIL) return;
  long ixf = fg->findMatchingShotpointOnLine(ixl, sp_after);
  if(ixf == -1) return;
  make_shotpoint_independent(fg, ixl, ixf);
  make_shotpoint_independent(fg, ixl, ixf+1);
  for(long i = 0; i < insert_after; i++)
      {
      fg->insertNewFlagOnLine(ixl, ixf+1);
      }
}


static void do_fill_missing(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  float  sp_interval = oo->getSpInterval();
  long        nflags = fg->numFlagsOnLine(ixl);
  if(sp_interval == 0.0) return;
  if(sp_interval == FNIL) return;
  if(nflags <= 1) return;
  for(long ixf = nflags-1; ixf >= 1; ixf--)
      {
      float sp1 = fg->getShotpoint(ixl, ixf-1);
      float sp2 = fg->getShotpoint(ixl, ixf);
      float delta  = (sp2 - sp1) / sp_interval;
      delta = AbsoluteValue(delta);
      long ninsert = NearestInteger(delta) - 1;
      for(long i = 0; i < ninsert; i++)
          {
          fg->insertNewFlagOnLine(ixl, ixf);
          }
      }
}


static void do_add_cards_beginning(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  long       ncards1 = oo->getNcards1();
  for(long i = 0; i < ncards1; i++)
      {
      fg->insertNewFlagOnLine(ixl, 0);
      }
}


static void do_add_cards_end(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  long       ncards2 = oo->getNcards2();
  for(long i = 0; i < ncards2; i++)
      {
      fg->appendNewFlagToLine(ixl);
      }
}


static void do_reverse_direction(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  if(!fg->allowReverseLineDirections()) return;
  fg->reverseLineDirection(ixl);
}


static void do_add_shots_beginning(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  float sp1          = oo->getSp1();
  float sp1_interval = oo->getSp1Interval();
  long  nflags       = fg->numFlagsOnLine(ixl);
  if(sp1 == FNIL) return;
  if(sp1_interval == FNIL) return;
  if(sp1_interval == 0.0) return;
  if(nflags == 0) return;
  float  sp    = fg->getShotpoint(ixl, 0);
  float delta  = (sp - sp1) / sp1_interval;
  long ninsert = NearestInteger(delta);
  make_shotpoint_independent(fg, ixl, 0);
  for(long i = 0; i < ninsert; i++)
      {
      fg->insertNewFlagOnLine(ixl, 0);
      }
  fg->setShotpoint(ixl, 0, sp1);
}


static void do_add_shots_end(LdEditPop *pop, long ixl)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  LdOperationGui *oo = pop->getLdOperationGui();
  float sp2          = oo->getSp2();
  float sp2_interval = oo->getSp2Interval();
  long  nflags       = fg->numFlagsOnLine(ixl);
  if(sp2 == FNIL) return;
  if(sp2_interval == FNIL) return;
  if(sp2_interval == 0.0) return;
  if(nflags == 0) return;
  float  sp    = fg->getShotpoint(ixl, nflags-1);
  float delta  = (sp2 - sp) / sp2_interval;
  long ninsert = NearestInteger(delta);
  make_shotpoint_independent(fg, ixl, nflags-1);
  for(long i = 0; i < ninsert; i++)
      {
      fg->appendNewFlagToLine(ixl);
      }
  nflags = fg->numFlagsOnLine(ixl);
  fg->setShotpoint(ixl, nflags-1, sp2);
}



//-------------------- append choice -----------------------//
//-------------------- append choice -----------------------//
//-------------------- append choice -----------------------//

      // appends the choice of lines to operate on
      //   to the message in the argument.

static void append_choice(LdEditPop *pop, char *msg)
{
  switch(pop->getChoice())
    {
    case LdChoiceGui::ACTIVE_LINE   : strcat(msg, " (on active line)");
                                      break;
    case LdChoiceGui::SELECTED_LINES: strcat(msg, " (on selected lines)");
                                      break;
    case LdChoiceGui::ALL_LINES     : strcat(msg, " (on all lines)");
                                      break;
    default: assert(FALSE);
    }
}



//---------------- impending operation ----------------------//
//---------------- impending operation ----------------------//
//---------------- impending operation ----------------------//

        // returns message describing impending operation.
        // used as an update function.

static char *impending_operation(void *data)
{
  LdEditPop    *pop = (LdEditPop*)data;
  static char msg[100];
  switch(pop->getOperation())
    {
    case LdOperationGui::DELETE_SHOTPOINTS    :
            strcpy(msg, "DELETE SHOTPOINTS");
            break;
    case LdOperationGui::CLEAR_SELECTED_FLAGS :
            strcpy(msg, "CLEAR SELECTED FLAGS");
            break;
    case LdOperationGui::INSERT_BETWEEN       :
            strcpy(msg, "INSERT CARDS BETWEEN PRESENT CARDS");
            break;
    case LdOperationGui::INSERT_BEFORE        :
            strcpy(msg, "INSERT CARDS BEFORE SPECIFIED SHOTPOINT");
            break;
    case LdOperationGui::INSERT_AFTER         :
            strcpy(msg, "INSERT CARDS AFTER SPECIFIED SHOTPOINT");
            break;
    case LdOperationGui::FILL_MISSING         :
            strcpy(msg, "FILL IN MISSING CARDS");
            break;
    case LdOperationGui::ADD_CARDS_BEGINNING  :
            strcpy(msg, "ADD CARDS AT BEGINNING");
            break;
    case LdOperationGui::ADD_CARDS_END        :
            strcpy(msg, "ADD CARDS AT END");
            break;
    case LdOperationGui::REVERSE_DIRECTION    :
            strcpy(msg, "REVERSE DIRECTION OF LINE");
            break;
    case LdOperationGui::ADD_SHOTS_BEGINNING  :
            strcpy(msg, "ADD SHOTPOINTS AT BEGINNING");
            break;
    case LdOperationGui::ADD_SHOTS_END        :
            strcpy(msg, "ADD SHOTPOINTS AT END");
            break;
    default: assert(FALSE);
    }
  append_choice(pop, msg);
  return msg;
}


//---------------- current operation ----------------------//
//---------------- current operation ----------------------//
//---------------- current operation ----------------------//

        // sets message to current operation.

static void current_operation(LdEditPop *pop, char *msg)
{
  switch(pop->getOperation())
    {
    case LdOperationGui::DELETE_SHOTPOINTS    :
            strcpy(msg, "deleting shotpoints");
            break;
    case LdOperationGui::CLEAR_SELECTED_FLAGS :
            strcpy(msg, "clearing selected flags");
            break;
    case LdOperationGui::INSERT_BETWEEN       :
            strcpy(msg, "inserting cards between present cards");
            break;
    case LdOperationGui::INSERT_BEFORE        :
            strcpy(msg, "inserting cards before specified shotpoint");
            break;
    case LdOperationGui::INSERT_AFTER         :
            strcpy(msg, "inserting cards after specified shotpoint");
            break;
    case LdOperationGui::FILL_MISSING         :
            strcpy(msg, "filling in missing cards");
            break;
    case LdOperationGui::ADD_CARDS_BEGINNING  :
            strcpy(msg, "adding cards at beginning");
            break;
    case LdOperationGui::ADD_CARDS_END        :
            strcpy(msg, "adding cards at end");
            break;
    case LdOperationGui::REVERSE_DIRECTION    :
            strcpy(msg, "reversing direction of line");
            break;
    case LdOperationGui::ADD_SHOTS_BEGINNING  :
            strcpy(msg, "adding shotpoints at beginning");
            break;
    case LdOperationGui::ADD_SHOTS_END        :
            strcpy(msg, "adding shotpoints at end");
            break;
    default: assert(FALSE);
    }
  append_choice(pop, msg);
}



//---------------- before doing the work --------------------//
//---------------- before doing the work --------------------//
//---------------- before doing the work --------------------//

#define FREEZE  fg->freezeDependentUpdates()
#define RESUME  fg->resumeDependentUpdates()

static void before_doing_the_work(LdEditPop *pop)
{
  FieldGeometry *fg = pop->getFieldGeometry();
  switch(pop->getOperation())
    {
    case LdOperationGui::DELETE_SHOTPOINTS    : FREEZE; break;
    case LdOperationGui::CLEAR_SELECTED_FLAGS :         break;
    case LdOperationGui::INSERT_BETWEEN       : FREEZE; break;
    case LdOperationGui::INSERT_BEFORE        : FREEZE; break;
    case LdOperationGui::INSERT_AFTER         : FREEZE; break;
    case LdOperationGui::FILL_MISSING         : FREEZE; break;
    case LdOperationGui::ADD_CARDS_BEGINNING  : FREEZE; break;
    case LdOperationGui::ADD_CARDS_END        : FREEZE; break;
    case LdOperationGui::REVERSE_DIRECTION    : RESUME; break;
    case LdOperationGui::ADD_SHOTS_BEGINNING  : FREEZE; break;
    case LdOperationGui::ADD_SHOTS_END        : FREEZE; break;
    default: assert(FALSE);
    }
  char msg[100];
  current_operation(pop, msg);
  strcat(msg, " in progress...");
  fg->showMessage(msg);
}



//---------------- after doing the work --------------------//
//---------------- after doing the work --------------------//
//---------------- after doing the work --------------------//


static void after_doing_the_work(LdEditPop *pop)
{
  FieldGeometry *fg = pop->getFieldGeometry();
  switch(pop->getOperation())
    {
    case LdOperationGui::DELETE_SHOTPOINTS    : RESUME; break;
    case LdOperationGui::CLEAR_SELECTED_FLAGS :         break;
    case LdOperationGui::INSERT_BETWEEN       : RESUME; break;
    case LdOperationGui::INSERT_BEFORE        : RESUME; break;
    case LdOperationGui::INSERT_AFTER         : RESUME; break;
    case LdOperationGui::FILL_MISSING         : RESUME; break;
    case LdOperationGui::ADD_CARDS_BEGINNING  : RESUME; break;
    case LdOperationGui::ADD_CARDS_END        : RESUME; break;
    case LdOperationGui::REVERSE_DIRECTION    :         break;
    case LdOperationGui::ADD_SHOTS_BEGINNING  : RESUME; break;
    case LdOperationGui::ADD_SHOTS_END        : RESUME; break;
    default: assert(FALSE);
    }
  char msg[100];
  current_operation(pop, msg);
  strcat(msg, " completed");
  fg->showMessage(msg);
}



//------------------- do the work ---------------------------//
//------------------- do the work ---------------------------//
//------------------- do the work ---------------------------//

static void do_the_work(LdEditPop *pop, long ixl)
{
  switch(pop->getOperation())
    {
    case LdOperationGui::DELETE_SHOTPOINTS    :
                      do_delete_shotpoints     (pop, ixl); break;
    case LdOperationGui::CLEAR_SELECTED_FLAGS :
                      do_clear_selected_flags  (pop, ixl); break;
    case LdOperationGui::INSERT_BETWEEN       :
                      do_insert_between        (pop, ixl); break;
    case LdOperationGui::INSERT_BEFORE        :
                      do_insert_before         (pop, ixl); break;
    case LdOperationGui::INSERT_AFTER         :
                      do_insert_after          (pop, ixl); break;
    case LdOperationGui::FILL_MISSING         :
                      do_fill_missing          (pop, ixl); break;
    case LdOperationGui::ADD_CARDS_BEGINNING  :
                      do_add_cards_beginning   (pop, ixl); break;
    case LdOperationGui::ADD_CARDS_END        :
                      do_add_cards_end         (pop, ixl); break;
    case LdOperationGui::REVERSE_DIRECTION    :
                      do_reverse_direction     (pop, ixl); break;
    case LdOperationGui::ADD_SHOTS_BEGINNING  :
                      do_add_shots_beginning   (pop, ixl); break;
    case LdOperationGui::ADD_SHOTS_END        :
                      do_add_shots_end         (pop, ixl); break;
    default: assert(FALSE);
    }
}



//----------------- prepare to do the work --------------------//
//----------------- prepare to do the work --------------------//
//----------------- prepare to do the work --------------------//

static void prepare_to_do_the_work(LdEditPop *pop)
{
  FieldGeometry *fg = pop->getFieldGeometry();
  long choice       = pop->getChoice();
  long operation    = pop->getOperation();
  if(!fg->allowModifyingLdCards()) return;
  fg->preMultipleOperations();
  before_doing_the_work(pop);
  switch(pop->getChoice())
      {
      case LdChoiceGui::ACTIVE_LINE:
               {
               long ixl = fg->getActiveLineIndex();
               if(ixl != -1) do_the_work(pop, ixl);
               }
               break;
      case LdChoiceGui::SELECTED_LINES:
               {
               long nsel = fg->numSelectedLines();
               if(nsel > 0)
                   {
                   long nlines = fg->numLines();
                   for(long ixl = 0; ixl < nlines; ixl++)
                       {
                       if(fg->lineIsSelected(ixl))
                                  do_the_work(pop, ixl);
                       }
                   }
               }
               break;
      case LdChoiceGui::ALL_LINES:
               {
               long nlines = fg->numLines();
               for(long ixl = 0; ixl < nlines; ixl++)
                   {
                   do_the_work(pop, ixl);
                   }
               }
               break;
      default: assert(FALSE);
      }
  after_doing_the_work(pop);
  fg->postMultipleOperations();
}



//--------------- yes and no clear traps ------------------------//
//--------------- yes and no clear traps ------------------------//
//--------------- yes and no clear traps ------------------------//

static void yes_clear_trap2(void *data)   // apply
{
  LdEditPop *pop = (LdEditPop*)data;
  prepare_to_do_the_work(pop);
}


static void no_clear_trap2(void* /*data*/)   // apply
{
}


static void yes_clear_trap1(void *data)     // ok
{
  LdEditPop *pop = (LdEditPop*)data;
  prepare_to_do_the_work(pop);
  pop->unmanage();
}


static void no_clear_trap1(void *data)     // ok
{
  LdEditPop *pop = (LdEditPop*)data;
  pop->unmanage();
}



//------------------------ delete trap ------------------------//
//------------------------ delete trap ------------------------//
//------------------------ delete trap ------------------------//

     // called from ok_trap when operation is DELETE_SHOTPOINTS.
     // ident == 1 means  ok   button.
     // ident == 2 means apply button.

static void delete_trap(LdEditPop *pop, long ident)
{
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowDeletingData())
      {
      fg->showMessage("cannot delete LD cards when data is locked");
      fg->ringBell();
      if(ident == 1) pop->unmanage();
      return;
      }
  char msg[500];
  strcpy(msg, "You have elected\nto delete LD cards\n");
  switch(pop->getChoice())
      {
      case LdChoiceGui::ACTIVE_LINE   : strcat(msg, "for the active line.");
                                        break;
      case LdChoiceGui::SELECTED_LINES: strcat(msg, "for selected lines.");
                                        break;
      case LdChoiceGui::ALL_LINES     : strcat(msg, "for all lines.");
                                        break;
      default: assert(FALSE);
      }
  strcat(msg, "\n----\n");
  if(fg->dataNeedsSaving())
      {
      strcat(msg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(msg, "Are you sure you want\nto delete these LD cards?");
  if(ident == 1)
    new SLQuestPop(pop, "Question", msg, FALSE,
                            yes_clear_trap1, no_clear_trap1, pop);
  else
    new SLQuestPop(pop, "Question", msg, FALSE,
                            yes_clear_trap2, no_clear_trap2, pop);
}



//----------------------- cancel trap ------------------------//
//----------------------- cancel trap ------------------------//
//----------------------- cancel trap ------------------------//

        // called by CANCEL pushbutton.

static void cancel_trap(void *data, long /*ident*/)
{
  LdEditPop *pop = (LdEditPop*)data;
  pop->unmanage();
}



//----------------------- ok trap ----------------------------//
//----------------------- ok trap ----------------------------//
//----------------------- ok trap ----------------------------//

        // called by OK and APPLY pushbuttons.
        // ident == 1 means  ok   button.
        // ident == 2 means apply button.

static void ok_trap(void *data, long ident)
{
  LdEditPop *pop = (LdEditPop*)data;
  if(pop->getOperation() == LdOperationGui::DELETE_SHOTPOINTS)
      {
      delete_trap(pop, ident);
      return;
      }
  prepare_to_do_the_work(pop);
  if(ident == 1) pop->unmanage();
}



//------------------ ok sense upfun ---------------------------//
//------------------ ok sense upfun ---------------------------//
//------------------ ok sense upfun ---------------------------//

        // used by OK and APPLY pushbuttons.

static long ok_sense_upfun(void *data)
{
  LdEditPop    *pop = (LdEditPop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowModifyingLdCards()) return FALSE;
  if(pop->getOperation() == LdOperationGui::DELETE_SHOTPOINTS)
      {
      if(!fg->allowDeletingData()) return FALSE;
      if(fg->totNumFlags() == 0) return FALSE;
      }
  switch(pop->getChoice())
      {
      case LdChoiceGui::ACTIVE_LINE   : return (fg->getActiveLineIndex() >= 0);
      case LdChoiceGui::SELECTED_LINES: return (fg->numSelectedLines  () >  0);
      case LdChoiceGui::ALL_LINES     : return (fg->numLines          () >  0);
      default: assert(FALSE);
      }
  return TRUE;
}



//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made.

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//

static char *defres[] =
               {
               ".foreground:     green4",
    ///////    ".fontList:       8x13bold",
               NULL
               };


LdEditPop::LdEditPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLDialog(slparent, name, hctx, FALSE),
                   _fg              (fg),
                   _cc              (NULL),
                   _oo              (NULL)
{
  assert(_fg);

  setTitle("Edit LD Cards");
//  setModal(SLShellContainer::FullAppModal); // has to be made first.

  Widget any = get_any_widget(this);
  setDefaultResources(XtDisplay(any), "ld_edit_pop_text", defres);

  SLSmartForm *work = workArea();
  SLSmartForm *top  = new SLSmartForm   (work, "top", hctx, FALSE);
              _cc   = new LdChoiceGui   (top , "cc" , hctx, _fg);
              _oo   = new LdOperationGui(work, "oo" , hctx, _fg);
  SLpText     *text = new SLpText       (work, "ld_edit_pop_text");

  top ->showEvenSpacing();
  text->showLabelAppearance();
  text->setupCvarFun(impending_operation, this);

    //                LEFT   RIGHT  TOP   BOTTOM
  work->attach(top ,  work,  work,  work,  NULL);
  work->attach(_oo ,  work,  work,  top ,  NULL,  0, 0, 10);
  work->attach(text,  work,  work,  _oo ,  work);

  SLpPush *ok    = addBottomOK     (1, ok_trap    , this);
  SLpPush *apply = addBottomApply  (2, ok_trap    , this);
                   addBottomCancel (0, cancel_trap, this);
                   addBottomHelp   ("EDIT_LD_CARDS");

  ok   ->setupSenseFun(ok_sense_upfun, this);
  apply->setupSenseFun(ok_sense_upfun, this);
 
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


LdEditPop::~LdEditPop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
