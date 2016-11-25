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
//------------------- tred_pick_gui.cc ----------------------//
//------------------- tred_pick_gui.cc ----------------------//
//------------------- tred_pick_gui.cc ----------------------//

//         implementation file for the TredPickGui class
//              derived from the SLSmartForm class
//                      subdirectory tred

#include "sl/sl_smart_form.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "pick/custom_resources.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "pick/tred_pick_gui.hh"
#include "pick/tred_table_pop.hh"
#include "cprim.h"
#include <assert.h>

//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//

static void radio_trap (void *data, long /*id*/, long /*old*/, long newvar)
{
  TredTablePop *pop = (TredTablePop *)data;
  pop->setType ((TredTablePop::TredType) newvar);
}

static void del_hwd_trap (void *data, long id, long /*old*/, long newvar)
{
  TredTablePop *pop = (TredTablePop *)data;
  if (newvar == 1) newvar = 0;
  pop->setDeleteHeaderWord (newvar, id);
}

static void kill_hwd_trap (void *data, long id, long /*old*/, long newvar)
{
  TredTablePop *pop = (TredTablePop *)data;
  if (newvar == 1) newvar = 0;
  pop->setKillHeaderWord (newvar, id);
}

static void rev_hwd_trap (void *data, long id, long /*old*/, long newvar)
{
  TredTablePop *pop = (TredTablePop *)data;
  if (newvar == 1) newvar = 0;
  pop->setReverseHeaderWord (newvar, id);
}

static void flag_hwd_trap (void *data, long id, long /*old*/, long newvar)
{
  TredTablePop *pop = (TredTablePop *)data;
  if (newvar == 1) newvar = 0;
  pop->setFlagHeaderWord (newvar, id);
}

static long pick_sense_update (void *data)
{
  TredTablePop *pop = (TredTablePop *)data;
  return (long)pop->pickingInProgress ();
}

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

TredPickGui::TredPickGui (SLDelay *slparent, TredTablePop *pop, char *name,
     HelpCtx hctx)
        : SLSmartForm (slparent, name, hctx, False),
        _min_hwd       (MIN_HWD),
        _max_hwd       (MAX_HWD)
{
  assert (slparent && pop);

  CustomResources::startup (slparent);

//Set up the text widgets

  SLpLabel *hwd_num  = new SLpLabel (this, "HWD #");

  
  long k2;
  for (k2 = 0; k2 < NUM_HWDS; k2++) {

    _del_hwd_text [k2] = new SLpText  (this, "_del_hwd_text",  k2, 
                                       SLpText::_LONG, 9);
    _kill_hwd_text[k2] = new SLpText  (this, "_kill_hwd_text", k2, 
                                       SLpText::_LONG, 9);
    _rev_hwd_text [k2] = new SLpText  (this, "_rev_hwd_text",  k2, 
                                       SLpText::_LONG, 9);
    _flag_hwd_text[k2] = new SLpText  (this, "_flag_hwd_text", k2, 
                                       SLpText::_LONG, 9);

    _del_hwd_text [k2]->setItrap (del_hwd_trap,  pop);
    _kill_hwd_text[k2]->setItrap (kill_hwd_trap, pop);
    _rev_hwd_text [k2]->setItrap (rev_hwd_trap,  pop);
    _flag_hwd_text[k2]->setItrap (flag_hwd_trap, pop);

    _del_hwd [k2] = pop->getHeaderWord (TredTablePop::DELTYPE,  k2);
    _kill_hwd[k2] = pop->getHeaderWord (TredTablePop::KILLTYPE, k2);
    _rev_hwd [k2] = pop->getHeaderWord (TredTablePop::REVTYPE,  k2);
    _flag_hwd[k2] = pop->getHeaderWord (TredTablePop::FLAGTYPE, k2);

    _del_hwd_text [k2]->setupIvarPoint (&_del_hwd [k2]);
    _kill_hwd_text[k2]->setupIvarPoint (&_kill_hwd[k2]);
    _rev_hwd_text [k2]->setupIvarPoint (&_rev_hwd [k2]);
    _flag_hwd_text[k2]->setupIvarPoint (&_flag_hwd[k2]);

    _del_hwd_text [k2]->setupIminPoint (&_min_hwd);
    _del_hwd_text [k2]->setupImaxPoint (&_max_hwd);
    _kill_hwd_text[k2]->setupIminPoint (&_min_hwd);
    _kill_hwd_text[k2]->setupImaxPoint (&_max_hwd);
    _rev_hwd_text [k2]->setupIminPoint (&_min_hwd);
    _rev_hwd_text [k2]->setupImaxPoint (&_max_hwd);
    _flag_hwd_text[k2]->setupIminPoint (&_min_hwd);
    _flag_hwd_text[k2]->setupImaxPoint (&_max_hwd);

    _del_hwd_text [k2]->setupSenseFun (pick_sense_update, pop);
    _kill_hwd_text[k2]->setupSenseFun (pick_sense_update, pop);
    _rev_hwd_text [k2]->setupSenseFun (pick_sense_update, pop);
    _flag_hwd_text[k2]->setupSenseFun (pick_sense_update, pop);
  }


//Set up the radio box
  _radios = new RadioList ();

  _drad =new SLpRadio (this, "rad1", TredTablePop::DELTYPE, "Delete ",_radios);
  _krad =new SLpRadio (this, "rad2", TredTablePop::KILLTYPE," Kill  ",_radios);
  _rrad =new SLpRadio (this, "rad3", TredTablePop::REVTYPE, "Reverse",_radios);
  _frad =new SLpRadio (this, "rad4", TredTablePop::FLAGTYPE," Flag  ",_radios);

  _radios->setupSenseFun (TredTablePop::DELTYPE,  pick_sense_update, pop);
  _radios->setupSenseFun (TredTablePop::KILLTYPE, pick_sense_update, pop);
  _radios->setupSenseFun (TredTablePop::REVTYPE,  pick_sense_update, pop);
  _radios->setupSenseFun (TredTablePop::FLAGTYPE, pick_sense_update, pop);

  _irad = (long)pop->getType ();
  _radios->setupIvarPoint (&_irad);

  _radios->setItrap (radio_trap, pop);

  attach   (hwd_num,     this,          0,   this,       0,     0, 0, 0, 0);
  SLDelay *sld[4];
  for (k2 = 0; k2 < 4; k2++) {sld[k2] = this;}
  for (k2 = 0; k2 < NUM_HWDS; k2++) {

    attach (_del_hwd_text [k2],  hwd_num,           0,   sld[0],     0,     0,
                           0, 0, 0);
    attach (_kill_hwd_text[k2], _del_hwd_text [k2], 0,   sld[1],     0,     0,
                           0, 0, 0);
    attach (_rev_hwd_text [k2], _kill_hwd_text[k2], 0,   sld[2],     0,     0,
                           0, 0, 0);
    attach (_flag_hwd_text[k2], _rev_hwd_text [k2], 0,   sld[3],     0,     0,
                          0, 0, 0);

    sld[0] = _del_hwd_text[k2];
    sld[1] = _kill_hwd_text[k2];
    sld[2] = _rev_hwd_text[k2];
    sld[3] = _flag_hwd_text[k2];
  }
  attach   (_drad,        hwd_num,      0,   sld[0],     this,  0, 0, 0, 0);
  attach   (_krad,        sld[0],       0,   sld[1],     this,  0, 0, 0, 0);
  attach   (_rrad,        sld[1],       0,   sld[2],     this,  0, 0, 0, 0);
  attach   (_frad,        sld[2],       0,   sld[3],     this,  0, 0, 0, 0);
}

TredPickGui::~TredPickGui (void)
{
  delete _radios;
}

//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//


Widget TredPickGui::make(Widget p)
{
  if (!made ())
    {
      Widget w = SLSmartForm::make(p);
      CustomResources::setForeground (CustomResources::getPixelNominalRed   (),
        _drad);
      CustomResources::setForeground (CustomResources::getPixelNominalBlue  (),
        _krad);
      CustomResources::setForeground (CustomResources::getPixelNominalGreen (),
        _rrad);
      CustomResources::setForeground (CustomResources::getPixelNominalOrange(),
        _frad);
    }
  makeChildren();
  return topWidget();
}


void TredPickGui::updateNumHeaders(long num_headers)
{
  _max_hwd = num_headers;

  for (int k2 = 0; k2 < NUM_HWDS; k2++) {
    _del_hwd_text [k2]->setupImaxPoint (&_max_hwd);
    _kill_hwd_text[k2]->setupImaxPoint (&_max_hwd);
    _rev_hwd_text [k2]->setupImaxPoint (&_max_hwd);
    _flag_hwd_text[k2]->setupImaxPoint (&_max_hwd);
  }   

}


//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
