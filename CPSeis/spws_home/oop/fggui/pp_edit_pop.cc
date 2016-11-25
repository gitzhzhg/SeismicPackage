
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
//---------------------- pp_edit_pop.cc ------------------------------//
//---------------------- pp_edit_pop.cc ------------------------------//
//---------------------- pp_edit_pop.cc ------------------------------//

//           implementation file for the PpEditPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to build/edit PP cards.


#include "fggui/pp_edit_pop.hh"
#include "fggui/pp_operation_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>


//---------------------- get operation ----------------------------//
//---------------------- get operation ----------------------------//
//---------------------- get operation ----------------------------//

long PpEditPop::getOperation()  const
{
  return _oo->getOperation();
}



//------------------ do specific operations ---------------------//
//------------------ do specific operations ---------------------//
//------------------ do specific operations ---------------------//


static void do_delete_pp_cards(PpEditPop *pop)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  PpOperationGui *oo = pop->getPpOperationGui();
  long       ndelete = oo->numCardsToDelete();
  if(ndelete == 0) return;
  long ixpp1 = oo->getDeleteCard1() - 1;
  long ixpp2 = oo->getDeleteCard2() - 1;
  for(long ixpp = ixpp2; ixpp >= ixpp1; ixpp--)
      {
      fg->deletePpCard(ixpp);
      }
}



static void do_add_pp_cards(PpEditPop *pop)
{
  FieldGeometry  *fg = pop->getFieldGeometry();
  PpOperationGui *oo = pop->getPpOperationGui();
  long          nadd = oo->numCardsToAdd();
  if(nadd == 0) return;
  long  num_last_set = oo->getNumLastSet();
  long  num_new_sets = oo->getNumNewSets();
  long  npp          = fg->numPpCards();
  fg->allocateSpaceForPpCards(nadd);
  for(long kount = 0; kount < nadd; kount++)
      {
      long ixpp_check = fg->appendNewPpCard();
      if(ixpp_check == -1) return;
      }
  long npp_new = fg->numPpCards();
  assert(npp_new = npp + num_new_sets * num_last_set);
  for(long k = 0; k < num_last_set; k++)
    {
    long ixpp2 = npp - num_last_set + k; // second card establishing pattern
    long ixpp1 = ixpp2 - num_last_set;   // first card establishing pattern
    assert(ixpp2 > ixpp1);
    for(long l = 0; l < num_new_sets; l++)
        {
        long ixpp = ixpp2 + (l+1) * num_last_set; // new card to set.
        assert(ixpp >= npp && ixpp < npp_new);
        for(int ident = FIRST_PP_VARIABLE;
                ident <= LAST_PP_VARIABLE;
                ident++)
            {
            if(ident == PP_THRU_GR || ident == PP_THRU_TR ||
               ident == PP_NTRACES) continue;
            int d1 = fg->ppValueIsDependent(ixpp1, ident);
            int d2 = fg->ppValueIsDependent(ixpp2, ident);
            if(d1 || d2) continue;
            double v1 = fg->getPpValue(ixpp1, ident);
            double v2 = fg->getPpValue(ixpp2, ident);
            double v3 = v1 + (ixpp - ixpp1) * (v2 - v1) / (ixpp2 - ixpp1);
            fg->setPpValue(ixpp, ident, v3);
            }
        }
    }
}



//---------------- impending operation ----------------------//
//---------------- impending operation ----------------------//
//---------------- impending operation ----------------------//

        // returns message describing impending operation.
        // used as an update function.

static char *impending_operation(void *data)
{
  PpEditPop     *pop = (PpEditPop*)data;
  PpOperationGui *oo = pop->getPpOperationGui();
  static char msg[100];
  switch(pop->getOperation())
    {
    case PpOperationGui::DELETE_PP_CARDS:
            {
            long ndelete = oo->numCardsToDelete();
            sprintf(msg, "DELETE %d PP CARDS", ndelete);
            }
            break;
    case PpOperationGui::ADD_PP_CARDS:
            {
            long nadd = oo->numCardsToAdd();
            sprintf(msg, "ADD %d PP CARDS", nadd);
            }
            break;
    default: assert(FALSE);
    }
  return msg;
}


//---------------- current operation ----------------------//
//---------------- current operation ----------------------//
//---------------- current operation ----------------------//

        // sets message to current operation.

static void current_operation(PpEditPop *pop, char *msg)
{
  PpOperationGui *oo = pop->getPpOperationGui();
  switch(pop->getOperation())
    {
    case PpOperationGui::DELETE_PP_CARDS:
            {
            long ndelete = oo->numCardsToDelete();
            sprintf(msg, "deleting %d PP cards", ndelete);
            }
            break;
    case PpOperationGui::ADD_PP_CARDS:
            {
            long nadd = oo->numCardsToAdd();
            sprintf(msg, "adding %d PP cards", nadd);
            }
            break;
    }
}



//---------------- before doing the work --------------------//
//---------------- before doing the work --------------------//
//---------------- before doing the work --------------------//

#define FREEZE  fg->freezeDependentUpdates()
#define RESUME  fg->resumeDependentUpdates()

static void before_doing_the_work(PpEditPop *pop)
{
  FieldGeometry *fg = pop->getFieldGeometry();
  switch(pop->getOperation())
    {
    case PpOperationGui::DELETE_PP_CARDS: FREEZE; break;
    case PpOperationGui::ADD_PP_CARDS   : FREEZE; break;
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


static void after_doing_the_work(PpEditPop *pop)
{
  FieldGeometry *fg = pop->getFieldGeometry();
  switch(pop->getOperation())
    {
    case PpOperationGui::DELETE_PP_CARDS: RESUME; break;
    case PpOperationGui::ADD_PP_CARDS   : RESUME; break;
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

static void do_the_work(PpEditPop *pop)
{
  switch(pop->getOperation())
    {
    case PpOperationGui::DELETE_PP_CARDS:
                      do_delete_pp_cards(pop); break;
    case PpOperationGui::ADD_PP_CARDS   :
                      do_add_pp_cards   (pop); break;
    default: assert(FALSE);
    }
}



//----------------- prepare to do the work --------------------//
//----------------- prepare to do the work --------------------//
//----------------- prepare to do the work --------------------//

static void prepare_to_do_the_work(PpEditPop *pop)
{
  FieldGeometry *fg = pop->getFieldGeometry();
  long operation    = pop->getOperation();
  if(!fg->allowModifyingPpCards()) return;
  fg->preMultipleOperations();
  before_doing_the_work(pop);
  do_the_work(pop);
  after_doing_the_work(pop);
  fg->postMultipleOperations();
}



//--------------- yes and no clear traps ------------------------//
//--------------- yes and no clear traps ------------------------//
//--------------- yes and no clear traps ------------------------//

static void yes_clear_trap2(void *data)   // apply
{
  PpEditPop *pop = (PpEditPop*)data;
  prepare_to_do_the_work(pop);
}


static void no_clear_trap2(void* /*data*/)   // apply
{
}


static void yes_clear_trap1(void *data)     // ok
{
  PpEditPop *pop = (PpEditPop*)data;
  prepare_to_do_the_work(pop);
  pop->unmanage();
}


static void no_clear_trap1(void *data)     // ok
{
  PpEditPop *pop = (PpEditPop*)data;
  pop->unmanage();
}



//------------------------ delete trap ------------------------//
//------------------------ delete trap ------------------------//
//------------------------ delete trap ------------------------//

     // called from ok_trap when operation is DELETE_PP_CARDS.
     // ident == 1 means  ok   button.
     // ident == 2 means apply button.

static void delete_trap(PpEditPop *pop, long ident)
{
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowDeletingData())
      {
      fg->showMessage("cannot delete PP cards when data is locked");
      fg->ringBell();
      if(ident == 1) pop->unmanage();
      return;
      }
  char msg[500];
  strcpy(msg, "You have elected\nto delete PP cards\n----\n");
  if(fg->dataNeedsSaving())
      {
      strcat(msg, "You have data which\nhas NOT been saved.\n----\n");
      }
  strcat(msg, "Are you sure you want\nto delete these PP cards?");
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
  PpEditPop *pop = (PpEditPop*)data;
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
  PpEditPop *pop = (PpEditPop*)data;
  if(pop->getOperation() == PpOperationGui::DELETE_PP_CARDS)
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
  PpEditPop    *pop = (PpEditPop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowModifyingPpCards()) return FALSE;
  if(pop->getOperation() == PpOperationGui::DELETE_PP_CARDS)
      {
      if(!fg->allowDeletingData()) return FALSE;
      if(fg->numPpCards() == 0) return FALSE;
      }
  else
      {
      PpOperationGui *oo = pop->getPpOperationGui();
      if(fg->numPpCards() < 2 * oo->getNumLastSet()) return FALSE;
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
     //////    ".fontList:       8x13bold",
               NULL
               };


PpEditPop::PpEditPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLDialog(slparent, name, hctx, FALSE),
                   _fg              (fg),
                   _oo              (NULL)
{
  assert(_fg);

  setTitle("Edit PP Cards");
//  setModal(SLShellContainer::FullAppModal);  // has to be made first.

  Widget any = get_any_widget(this);
  setDefaultResources(XtDisplay(any), "pp_edit_pop_text", defres);

  SLSmartForm *work = workArea();
              _oo   = new PpOperationGui(work, "oo" , hctx, _fg);
  SLpText     *text = new SLpText       (work, "pp_edit_pop_text");

  text->showLabelAppearance();
  text->setupCvarFun(impending_operation, this);

    //                LEFT   RIGHT  TOP   BOTTOM
  work->attach(_oo ,  work,  work,  work,  NULL);
  work->attach(text,  work,  work,  _oo ,  work);

  SLpPush *ok    = addBottomOK     (1, ok_trap    , this);
  SLpPush *apply = addBottomApply  (2, ok_trap    , this);
                   addBottomCancel (0, cancel_trap, this);
                   addBottomHelp   ("EDIT_PP_CARDS");

  ok   ->setupSenseFun(ok_sense_upfun, this);
  apply->setupSenseFun(ok_sense_upfun, this);
 
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


PpEditPop::~PpEditPop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
