
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
//---------------------- pp_operation_gui.cc -----------------------//
//---------------------- pp_operation_gui.cc -----------------------//
//---------------------- pp_operation_gui.cc -----------------------//

//          implementation file for the PpOperationGui class
//                 derived from the SLSmartForm class
//                       subdirectory fggui

             // This class is used to choose an operation
             // to be performed on PP cards.


#include "fggui/pp_operation_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/radio_list.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_radio.hh"
#include "sl/sl_sep.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <assert.h>


//---------------------- get values ---------------------------//
//---------------------- get values ---------------------------//
//---------------------- get values ---------------------------//

long PpOperationGui::numCardsToDelete()  const
{
  if(_delete_card1 == INIL) return 0;
  if(_delete_card2 == INIL) return 0;
  long npp = _fg->numPpCards();
  if(_delete_card1 <= 0 || _delete_card1 > npp) return 0;
  if(_delete_card2 <= 0 || _delete_card2 > npp) return 0;
  long num_cards_to_delete = _delete_card2 - _delete_card1 + 1;
  if(num_cards_to_delete < 0) return 0;
  return num_cards_to_delete;
}


long PpOperationGui::numCardsToAdd()  const
{
  if(_num_last_set == INIL) return 0;
  if(_num_new_sets == INIL) return 0;
  long npp = _fg->numPpCards();
  if(_num_last_set <= 0 || _num_last_set > npp/2) return 0;
  if(_num_new_sets <= 0                         ) return 0;
  long num_cards_to_add = _num_last_set * _num_new_sets;
  if(num_cards_to_add < 0) return 0;
  return num_cards_to_add;
}



//---------------------- set values ---------------------------//
//---------------------- set values ---------------------------//
//---------------------- set values ---------------------------//

void PpOperationGui::setDeleteCard1(long delete_card1)
{
  if(delete_card1 > 0 || delete_card1 == INIL)
                        _delete_card1 = delete_card1;
}

void PpOperationGui::setDeleteCard2(long delete_card2)
{
  if(delete_card2 > 0 || delete_card2 == INIL)
                        _delete_card2 = delete_card2;
}

void PpOperationGui::setNumLastSet(long num_last_set)
{
  if(num_last_set > 0 || num_last_set == INIL)
                        _num_last_set = num_last_set;
}

void PpOperationGui::setNumNewSets(long num_new_sets)
{
  if(num_new_sets > 0 || num_new_sets == INIL)
                        _num_new_sets = num_new_sets;
}



//------------------------ traps --------------------------------//
//------------------------ traps --------------------------------//
//------------------------ traps --------------------------------//

static void delete_card1_trap(void *data, long /*ident*/,
                                     long /*oldvar*/, long newvar)
{
  PpOperationGui *THIS = (PpOperationGui*)data;
  THIS->setDeleteCard1(newvar);
}


static void delete_card2_trap(void *data, long /*ident*/,
                                     long /*oldvar*/, long newvar)
{
  PpOperationGui *THIS = (PpOperationGui*)data;
  THIS->setDeleteCard2(newvar);
}


static void num_last_set_trap(void *data, long /*ident*/,
                                     long /*oldvar*/, long newvar)
{
  PpOperationGui *THIS = (PpOperationGui*)data;
  THIS->setNumLastSet(newvar);
}


static void num_new_sets_trap(void *data, long /*ident*/,
                                     long /*oldvar*/, long newvar)
{
  PpOperationGui *THIS = (PpOperationGui*)data;
  THIS->setNumNewSets(newvar);
}



//------------------ sense update functions -----------------------//
//------------------ sense update functions -----------------------//
//------------------ sense update functions -----------------------//


static long add_sense_upfun(void *data)
{
  PpOperationGui *THIS = (PpOperationGui*)data;
  FieldGeometry  *fg   = THIS->getFieldGeometry();
  if(fg->numPpCards() < 2) return FALSE;
  return TRUE;
}


static long delete_sense_upfun(void *data)
{
  PpOperationGui *THIS = (PpOperationGui*)data;
  FieldGeometry  *fg   = THIS->getFieldGeometry();
  int sense = fg->allowModifyingPpCards();
  THIS->setSensitivity(sense);   // sensitivity of entire object.
  if(!fg->allowDeletingData()) return FALSE;
  if(fg->numPpCards() == 0) return FALSE;
  return TRUE;
}



//----------------------- labels ----------------------------//
//----------------------- labels ----------------------------//
//----------------------- labels ----------------------------//

#define D1_LABEL    "card number of first card to delete:"
#define D2_LABEL    "card number of last card to delete:"
#define N1_LABEL    "number of cards falling into the last set:"
#define N2_LABEL    "number of sets of new cards to build:"
#define N0_LABEL    \
"This option automatically adds new PP cards after\n\
the cards you already have.  The new cards will\n\
continue a pattern automatically established by the\n\
last few existing cards.  The pattern will be a\n\
repetition of sets of cards, with increments based\n\
on the increments between the last two sets."



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
               ".fontList:       8x13",
               ".alignment:      ALIGNMENT_BEGINNING",
               NULL
               };



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


PpOperationGui::PpOperationGui(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLSmartForm(slparent, name, hctx, TRUE),
                   _fg                  (fg),
                   _operation           (ADD_PP_CARDS),
                   _delete_card1        (INIL),
                   _delete_card2        (INIL),
                   _num_last_set        (INIL),
                   _num_new_sets        (INIL)
{
  assert(_fg);

  Widget any = get_any_widget(this);
  setDefaultResources(XtDisplay(any), "pp_operation_gui_n0", defres);

  RadioList *r = new RadioList();
  SLSep  *sep1 = new SLSep(this, "sep");

  SLpRadio *r1  = r->addRadio(this, "r1" , DELETE_PP_CARDS);
  SLpRadio *r2  = r->addRadio(this, "r2" , ADD_PP_CARDS   );

  r->setLabel(DELETE_PP_CARDS, "delete PP cards");
  r->setLabel(ADD_PP_CARDS   , "add PP cards");

  r->setupIvarPoint(&_operation);  // do not need trap or update function.

  r->setupSenseFun(DELETE_PP_CARDS, delete_sense_upfun, this);
  r->setupSenseFun(ADD_PP_CARDS   , add_sense_upfun   , this);

  SL2Text  *d1 = new SL2Text (this, "d1", 0, D1_LABEL, SLpText::_LONG, 10);
  SL2Text  *d2 = new SL2Text (this, "d2", 0, D2_LABEL, SLpText::_LONG, 10);
  SLpLabel *n0 = new SLpLabel(this, "pp_operation_gui_n0", 0, N0_LABEL);
  SL2Text  *n1 = new SL2Text (this, "n1", 0, N1_LABEL, SLpText::_LONG, 10);
  SL2Text  *n2 = new SL2Text (this, "n2", 0, N2_LABEL, SLpText::_LONG, 10);

  d1->setItrap(delete_card1_trap, this);
  d2->setItrap(delete_card2_trap, this);
  n1->setItrap(num_last_set_trap, this);
  n2->setItrap(num_new_sets_trap, this);

  d1->setupIvarPoint(&_delete_card1);
  d2->setupIvarPoint(&_delete_card2);
  n1->setupIvarPoint(&_num_last_set);
  n2->setupIvarPoint(&_num_new_sets);

  d1->setupSenseFun(delete_sense_upfun, this);
  d2->setupSenseFun(delete_sense_upfun, this);
  n0->setupSenseFun(   add_sense_upfun, this);
  n1->setupSenseFun(   add_sense_upfun, this);
  n2->setupSenseFun(   add_sense_upfun, this);

    //  <r1> delete PP cards
    //           card number of first card to delete: [d1]
    //           card number of last card to delete: [d2]
    //  --------------------------------------------------------------
    //  <r2> add PP cards
    //         [n0]
    //           number of cards falling into the last set: [n1]
    //           number of sets of new cards to build:      [n2]

    //          LEFT  RIGHT   TOP   BOTTOM
  attach(r1  ,  this,  NULL,  this,  NULL,   0, 0, 5);
  attach(d1  ,  this,  NULL,  r1  ,  NULL,  50, 0, 5);
  attach(d2  ,  this,  NULL,  d1  ,  NULL,  50, 0, 5);

  attach(sep1,  this,  this,  d2  ,  NULL,   0, 0, 5);   // separator.

  attach(r2  ,  this,  NULL,  sep1,  NULL,   0, 0, 5);
  attach(n0  ,  this,  this,  r2  ,  NULL,  20, 0, 5);
  attach(n1  ,  this,  this,  n0  ,  NULL,  50, 0, 5);   // attached on right.
  attach(n2  ,  this,  NULL,  n1  ,  this,  50, 0, 5, 5);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


PpOperationGui::~PpOperationGui()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
