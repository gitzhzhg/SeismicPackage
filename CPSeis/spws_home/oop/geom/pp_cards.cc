
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
//---------------------- pp_cards.cc -----------------------//
//---------------------- pp_cards.cc -----------------------//
//---------------------- pp_cards.cc -----------------------//

//            implementation file for the PpCards class
//                 derived from the SmartArray class
//                        subdirectory geom


#include "geom/pp_cards.hh"
#include "geom/pp_card.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "geom/acc_ntraces.hh"
#include "oprim/acc_down.hh"
#include "oprim/acc_interp.hh"
#include "oprim/acc_sum.hh"
#include "oprim/acc_search.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>


#define STEP      2000


//--------------------- static functions ------------------------//
//--------------------- static functions ------------------------//
//--------------------- static functions ------------------------//

#define PPCARD  ((PpCards*)data)->unsafePpCard(index)


static float get_thru_gr2(void *data, long index)
{
  return (float)PPCARD->getThruGroupNumber();
}


static float get_thru_tr2(void *data, long index)
{
  return (float)PPCARD->getThruTraceNumber();
}


static long get_thru_gr(void *data, long index)
{
  return PPCARD->getThruGroupNumber();
}


static long get_thru_tr(void *data, long index)
{
  return PPCARD->getThruTraceNumber();
}


static long update_thru_gr(void *data, long index, long prev_thru_gr)
{
  return PPCARD->updateThruGroupNumber(prev_thru_gr);
}


static long update_thru_tr(void *data, long index, long prev_thru_tr)
{
  return PPCARD->updateThruTraceNumber(prev_thru_tr);
}



#define AAF(get_value, set_value, getValue, setValue)        \
static double get_value(void *data, long index)              \
{                                                            \
  return (double)PPCARD->getValue();                         \
}                                                            \
                                                             \
static void set_value(void *data, long index, double value)  \
{                                                            \
  PPCARD->setValue((float)value);                            \
}


#define AAI(get_value, set_value, getValue, setValue)        \
static double get_value(void *data, long index)              \
{                                                            \
  return (double)PPCARD->getValue();                         \
}                                                            \
                                                             \
static void set_value(void *data, long index, double value)  \
{                                                            \
  PPCARD->setValue(NearestInteger(value));                   \
}


AAF(get_sshot  , set_sshot  , getSourceShotpoint   , set2SourceShotpoint   )
AAI(get_sline  , set_sline  , getSourceLine        , set2SourceLine        )
AAF(get_rshot  , set_rshot  , getReceiverShotpoint , set2ReceiverShotpoint )
AAI(get_rline  , set_rline  , getReceiverLine      , set2ReceiverLine      )
AAI(get_pattern, set_pattern, getPatternNumber     , set2PatternNumber     )
AAI(get_smove  , set_smove  , getSourceMove        , set2SourceMove        )
AAI(get_rmove  , set_rmove  , getReceiverMove      , set2ReceiverMove      )



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


PpCards::PpCards(FgInformer *informer, FgConnect *connect)
           :  SmartArray(STEP),
                _frozen                 (FALSE),
                _need                   (FALSE),
                _active_trace           (0),
                _active_group           (0),
                _informer               (informer),
                _connect                (connect)
{
  assert(_informer);
  assert(_connect);
  _acc_file       = new AccBase    (this, PP_FILE);
  _acc_sshot      = new AccDown    (this, PP_SSHOT , get_sshot, set_sshot);
  _acc_sline      = new AccDown    (this, PP_SLINE , get_sline, set_sline);
  _acc_rshot      = new AccDown    (this, PP_RSHOT , get_rshot, set_rshot);
  _acc_rline      = new AccDown    (this, PP_RLINE , get_rline, set_rline);
  _acc_pattern    = new AccDown    (this, PP_PAT   , get_pattern, set_pattern);
  _acc_xskid      = new AccBase    (this, PP_XSKID);
  _acc_yskid      = new AccBase    (this, PP_YSKID);
  _acc_hold       = new AccBase    (this, PP_HOLD);
  _acc_elev       = new AccBase    (this, PP_ELEV);
  _acc_hd         = new AccBase    (this, PP_HD);
  _acc_tuh        = new AccBase    (this, PP_TUH);
  _acc_smove      = new AccDown    (this, PP_SMOVE , get_smove, set_smove);
  _acc_rmove      = new AccDown    (this, PP_RMOVE , get_rmove, set_rmove);
  _acc_ngroups    = new AccBase    (this, PP_NGROUPS);
  _acc_ntraces    = new AccNtraces (this, _connect);

  _sum_thru_gr    = new AccSum (this, PP_THRU_GR, get_thru_gr, update_thru_gr);
  _sum_thru_tr    = new AccSum (this, PP_THRU_TR, get_thru_tr, update_thru_tr);

  _search_thru_gr = new AccSearch  (this, get_thru_gr2);
  _search_thru_tr = new AccSearch  (this, get_thru_tr2);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

PpCards::~PpCards()
{
  deleteAllPpCards();
  delete _acc_file;
  delete _acc_sshot;
  delete _acc_sline;
  delete _acc_rshot;
  delete _acc_rline;
  delete _acc_pattern;
  delete _acc_xskid;
  delete _acc_yskid;
  delete _acc_hold;
  delete _acc_elev;
  delete _acc_hd;
  delete _acc_tuh;
  delete _acc_smove;
  delete _acc_rmove;
  delete _acc_ngroups;
  delete _acc_ntraces;
  delete _sum_thru_gr;
  delete _sum_thru_tr;
  delete _search_thru_gr;
  delete _search_thru_tr;
}


//------------ before and after new active index ------------------//
//------------ before and after new active index ------------------//
//------------ before and after new active index ------------------//

void PpCards::beforeNewActiveIndex()
{
  _informer->preNewActivePpCard();
}


void PpCards::afterNewActiveIndex()
{
  _informer->postNewActivePpCard();
}



//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//
//------------------- before remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray before removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void PpCards::beforeRemoveInsert(long index, long nrem, long nins)
{
  if(_need) return;
  _informer      ->preRemoveInsertPpCards(index, nrem, nins);
  _acc_file      ->preChange(index, nrem, nins);
  _acc_sshot     ->preChange(index, nrem, nins);
  _acc_sline     ->preChange(index, nrem, nins);
  _acc_rshot     ->preChange(index, nrem, nins);
  _acc_rline     ->preChange(index, nrem, nins);
  _acc_pattern   ->preChange(index, nrem, nins);
  _acc_xskid     ->preChange(index, nrem, nins);
  _acc_yskid     ->preChange(index, nrem, nins);
  _acc_hold      ->preChange(index, nrem, nins);
  _acc_elev      ->preChange(index, nrem, nins);
  _acc_hd        ->preChange(index, nrem, nins);
  _acc_tuh       ->preChange(index, nrem, nins);
  _acc_smove     ->preChange(index, nrem, nins);
  _acc_rmove     ->preChange(index, nrem, nins);
  _acc_ngroups   ->preChange(index, nrem, nins);
  _acc_ntraces   ->regRange (_acc_pattern);
  _acc_ntraces   ->preChange(_acc_ngroups);  // needs pattern and ngroups 
  _sum_thru_gr   ->preChange(_acc_ngroups);  // needs ngroups
  _sum_thru_tr   ->preChange(_acc_ntraces);  // needs ntraces
  _search_thru_gr->preChange(_sum_thru_gr);  // needs thru_gr
  _search_thru_tr->preChange(_sum_thru_tr);  // needs thru_tr
  setActiveTraceNumber(0);
  setActiveGroupNumber(0);
  if(!_frozen)
      {
      _informer->preNewActiveTrace();
      _informer->preNewActiveGroup();
      }
}



//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//
//------------------- after remove insert --------------------------//

        //  private virtual function overriding SmartArray.
        //  Called from SmartArray after removing and/or inserting
        //    one or more array elements.
        //  The function in SmartArray is empty.


void PpCards::afterRemoveInsert(long index, long nrem, long nins)
{
  if(_need) return;
  _acc_file      ->post1Change();
  _acc_sshot     ->post1Change();
  _acc_sline     ->post1Change();
  _acc_rshot     ->post1Change();
  _acc_rline     ->post1Change();
  _acc_pattern   ->post1Change();
  _acc_xskid     ->post1Change();
  _acc_yskid     ->post1Change();
  _acc_hold      ->post1Change();
  _acc_elev      ->post1Change();
  _acc_hd        ->post1Change();
  _acc_tuh       ->post1Change();
  _acc_smove     ->post1Change();
  _acc_rmove     ->post1Change();
  _acc_ngroups   ->post1Change();
  _acc_ntraces   ->post1Change();
  _sum_thru_gr   ->post1Change();
  _sum_thru_tr   ->post1Change();
  _search_thru_gr->post1Change();
  _search_thru_tr->post1Change();

  _acc_file      ->post2Change();
  _acc_sshot     ->post2Change();
  _acc_sline     ->post2Change();
  _acc_rshot     ->post2Change();
  _acc_rline     ->post2Change();
  _acc_pattern   ->post2Change();
  _acc_xskid     ->post2Change();
  _acc_yskid     ->post2Change();
  _acc_hold      ->post2Change();
  _acc_elev      ->post2Change();
  _acc_hd        ->post2Change();
  _acc_tuh       ->post2Change();
  _acc_smove     ->post2Change();
  _acc_rmove     ->post2Change();
  _acc_ngroups   ->post2Change();
  _acc_ntraces   ->post2Change();
  _sum_thru_gr   ->post2Change();
  _sum_thru_tr   ->post2Change();
  _search_thru_gr->post2Change();
  _search_thru_tr->post2Change();
  _informer->postRemoveInsertPpCards(index, nrem, nins);
  if(!_frozen)
      {
      _active_trace = ConstrainValue(_active_trace, 1, numTraces());
      _active_group = ConstrainValue(_active_group, 1, numGroups());
      _informer->postNewActiveTrace();
      _informer->postNewActiveGroup();
      }
  if(_frozen) _need = TRUE;
}



//-------------------- before value changed ---------------------//
//-------------------- before value changed ---------------------//
//-------------------- before value changed ---------------------//

      //  private convenience function.
      //  Called by a public function before that function changes
      //    a value.

void PpCards::beforeValueChanged(int ident, long index)
{
  if(_need) return;
  switch(ident)
    {
    case PP_FILE   : _acc_file   ->preChange(index, 1, 1); break;
    case PP_SSHOT  : _acc_sshot  ->preChange(index, 1, 1); break;
    case PP_SLINE  : _acc_sline  ->preChange(index, 1, 1); break;
    case PP_RSHOT  : _acc_rshot  ->preChange(index, 1, 1); break;
    case PP_RLINE  : _acc_rline  ->preChange(index, 1, 1); break;
    case PP_PAT    : _acc_pattern->preChange(index, 1, 1);
                     _acc_ntraces->preChange(_acc_pattern);
                     _sum_thru_tr->preChange(_acc_ntraces);
                  _search_thru_tr->preChange(_sum_thru_tr);
                     break;
    case PP_XSKID  : _acc_xskid  ->preChange(index, 1, 1); break;
    case PP_YSKID  : _acc_yskid  ->preChange(index, 1, 1); break;
    case PP_HOLD   : _acc_hold   ->preChange(index, 1, 1); break;
    case PP_ELEV   : _acc_elev   ->preChange(index, 1, 1); break;
    case PP_HD     : _acc_hd     ->preChange(index, 1, 1); break;
    case PP_TUH    : _acc_tuh    ->preChange(index, 1, 1); break;
    case PP_SMOVE  : _acc_smove  ->preChange(index, 1, 1); break;
    case PP_RMOVE  : _acc_rmove  ->preChange(index, 1, 1); break;
    case PP_NGROUPS: _acc_ngroups->preChange(index, 1, 1);
                     _acc_ntraces->preChange(_acc_ngroups);
                     _sum_thru_gr->preChange(_acc_ngroups);
                     _sum_thru_tr->preChange(_acc_ntraces);
                  _search_thru_gr->preChange(_sum_thru_gr);
                  _search_thru_tr->preChange(_sum_thru_tr);
                      setActiveTraceNumber(_active_trace);
                      setActiveGroupNumber(_active_group);
                      break;
    case PP_NTRACES:  assert(FALSE);
    case PP_THRU_GR:  assert(FALSE);
    case PP_THRU_TR:  assert(FALSE);
    default:          assert(FALSE);
    }
}



//-------------------- after value changed ---------------------//
//-------------------- after value changed ---------------------//
//-------------------- after value changed ---------------------//

      //  private convenience function.
      //  Called by a public function after that function changes
      //    a value.

void PpCards::afterValueChanged(int ident, long /*index*/)
{
  if(_need) return;
  switch(ident)
    {
    case PP_FILE   : _acc_file   ->post1Change(); break;
    case PP_SSHOT  : _acc_sshot  ->post1Change(); break;
    case PP_SLINE  : _acc_sline  ->post1Change(); break;
    case PP_RSHOT  : _acc_rshot  ->post1Change(); break;
    case PP_RLINE  : _acc_rline  ->post1Change(); break;
    case PP_PAT    : _acc_pattern->post1Change();
                     _acc_ntraces->post1Change(); // needs pattern & ngroups
                     _sum_thru_tr->post1Change(); // needs ntraces
                  _search_thru_tr->post1Change(); // needs thru_tr
                     break;
    case PP_XSKID  : _acc_xskid  ->post1Change(); break;
    case PP_YSKID  : _acc_yskid  ->post1Change(); break;
    case PP_HOLD   : _acc_hold   ->post1Change(); break;
    case PP_ELEV   : _acc_elev   ->post1Change(); break;
    case PP_HD     : _acc_hd     ->post1Change(); break;
    case PP_TUH    : _acc_tuh    ->post1Change(); break;
    case PP_SMOVE  : _acc_smove  ->post1Change(); break;
    case PP_RMOVE  : _acc_rmove  ->post1Change(); break;
    case PP_NGROUPS: _acc_ngroups->post1Change();
                     _acc_ntraces->post1Change(); // needs pattern & ngroups
                     _sum_thru_gr->post1Change(); // needs ngroups
                     _sum_thru_tr->post1Change(); // needs ntraces
                  _search_thru_gr->post1Change(); // needs thru_gr
                  _search_thru_tr->post1Change(); // needs thru_tr
                     break;
    case PP_NTRACES:  assert(FALSE);
    case PP_THRU_GR:  assert(FALSE);
    case PP_THRU_TR:  assert(FALSE);
    default:          assert(FALSE);
    }

  switch(ident)
    {
    case PP_FILE   : _acc_file   ->post2Change(); break;
    case PP_SSHOT  : _acc_sshot  ->post2Change(); break;
    case PP_SLINE  : _acc_sline  ->post2Change(); break;
    case PP_RSHOT  : _acc_rshot  ->post2Change(); break;
    case PP_RLINE  : _acc_rline  ->post2Change(); break;
    case PP_PAT    : _acc_pattern->post2Change();
                     _acc_ntraces->post2Change(); // needs pattern & ngroups
                     _sum_thru_tr->post2Change(); // needs ntraces
                  _search_thru_tr->post2Change(); // needs thru_tr
                     break;
    case PP_XSKID  : _acc_xskid  ->post2Change(); break;
    case PP_YSKID  : _acc_yskid  ->post2Change(); break;
    case PP_HOLD   : _acc_hold   ->post2Change(); break;
    case PP_ELEV   : _acc_elev   ->post2Change(); break;
    case PP_HD     : _acc_hd     ->post2Change(); break;
    case PP_TUH    : _acc_tuh    ->post2Change(); break;
    case PP_SMOVE  : _acc_smove  ->post2Change(); break;
    case PP_RMOVE  : _acc_rmove  ->post2Change(); break;
    case PP_NGROUPS: _acc_ngroups->post2Change();
                     _acc_ntraces->post2Change(); // needs pattern & ngroups
                     _sum_thru_gr->post2Change(); // needs ngroups
                     _sum_thru_tr->post2Change(); // needs ntraces
                  _search_thru_gr->post2Change(); // needs thru_gr
                  _search_thru_tr->post2Change(); // needs thru_tr
                     break;
    case PP_NTRACES:  assert(FALSE);
    case PP_THRU_GR:  assert(FALSE);
    case PP_THRU_TR:  assert(FALSE);
    default:          assert(FALSE);
    }
}



//----------------- receiver patterns have changed ------------//
//----------------- receiver patterns have changed ------------//
//----------------- receiver patterns have changed ------------//

   // public
   // called from FgConnect when receiver patterns have changed.

void PpCards::receiverPatternsHaveChanged()
{
  long n = numPpCards();
  if(n == 0) return;
  if(!_frozen)
      {
      _informer->preNewActiveTrace();
      _informer->showMessage("updating #traces on PP cards...");
      }
     _acc_ntraces->preChange(0, n, n);
     _sum_thru_tr->preChange(_acc_ntraces);
  _search_thru_tr->preChange(_sum_thru_tr);
     _acc_ntraces->post1Change();
     _sum_thru_tr->post1Change();
  _search_thru_tr->post1Change();
     _acc_ntraces->post2Change();
     _sum_thru_tr->post2Change();
  _search_thru_tr->post2Change();
  if(!_frozen)
      {
      _informer->showMessage("finished updating #traces on PP cards");
      _active_trace = ConstrainValue(_active_trace, 1, numTraces());
      _informer->postNewActiveTrace();
      }
}



//------------------- overriding virtual functions -----------------//
//------------------- overriding virtual functions -----------------//
//------------------- overriding virtual functions -----------------//

    // public virtual functions overriding SmartArray.
    // called by AccDown.
    // these are pass-thru to individual PP cards.
    // the functions in the individual PP cards are not virtual.

    // Can also be called by other people, but other functions are
    // provided for this, and should be used instead:
    //     ppValueIsDependent  (long ixpp, int ident)
    //     getPpValue          (long ixpp, int ident)
    //     setPpValue          (long ixpp, int ident, double value)
    //     setDependentPpValue (long ixpp, int ident, double value)
    // (note different order of arguments)


int PpCards::valueIsDependent(int ident, long ixpp)  const
{
  return unsafePpCard(ixpp)->ppValueIsDependent(ident);
}


void PpCards::setDependencyTrue(int ident, long ixpp)
{
  unsafePpCard(ixpp)->setPpDependencyTrue(ident);
}


void PpCards::setDependencyFalse(int ident, long ixpp)
{
  unsafePpCard(ixpp)->setPpDependencyFalse(ident);
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by AccDown and AccSum.

void PpCards::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  _informer->prePpValuesChanged (ident, index, nrem, nins);
}

void PpCards::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  _informer->postPpValuesChanged (ident, index, nrem, nins);
}



//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//
//------------ public access to these PP cards --------------//

        // ixpp = index of desired PP card.



//----------------------- get values ------------------------------//
//----------------------- get values ------------------------------//
//----------------------- get values ------------------------------//


long PpCards::numGroups()  const
{
  long n = numPpCards();
  if(n == 0) return 0;
  return getThruGroupNumber(n - 1);
}



long PpCards::numTraces()  const
{
  long n = numPpCards();
  if(n == 0) return 0;
  return getThruTraceNumber(n - 1);
}


// asserts if trace number is out of range or there are no traces:

long PpCards::groupNumber(long trace)  const
{
  long ixpp = findPpCardWithDesiredTrace(trace);
  assert(ixpp >= 0);
  long thru_group = getThruGroupNumber(ixpp);
  long thru_trace = getThruTraceNumber(ixpp);
  long num_groups = getNumGroupsOnCard(ixpp);
  long num_traces = getNumTracesOnCard(ixpp);
  assert(num_groups > 0);
  assert(num_traces > 0);
  long num_channels = num_traces / num_groups;
  assert(num_traces = num_groups * num_channels);
  long first_group = thru_group - num_groups + 1;
  long first_trace = thru_trace - num_traces + 1;
  long group = first_group + (trace - first_trace) / num_channels;
  return group;
}



long PpCards::channelNumber(long trace)  const
{
  long ixpp = findPpCardWithDesiredTrace(trace);
  assert(ixpp >= 0);
  long thru_group = getThruGroupNumber(ixpp);
  long thru_trace = getThruTraceNumber(ixpp);
  long num_groups = getNumGroupsOnCard(ixpp);
  long num_traces = getNumTracesOnCard(ixpp);
  assert(num_groups > 0);
  assert(num_traces > 0);
  long num_channels = num_traces / num_groups;
  assert(num_traces = num_groups * num_channels);
  long first_group = thru_group - num_groups + 1;
  long first_trace = thru_trace - num_traces + 1;
  long group = first_group + (trace - first_trace) / num_channels;
  long channel = (trace - first_trace + 1)
                      - (group - first_group) * num_channels;
  return channel;
}



//----------------------- set values ------------------------------//
//----------------------- set values ------------------------------//
//----------------------- set values ------------------------------//


void PpCards::setActiveTraceNumber(long trace)
{
  if(_frozen) return;
  long n = numTraces();
  if(n == 0) trace = 0;
  else       trace = ConstrainValue(trace, 1, n);
  if(trace == _active_trace) return;
  _informer->preNewActiveTrace();
  _active_trace = trace;
  _informer->postNewActiveTrace();
}


void PpCards::setActiveGroupNumber(long group)
{
  if(_frozen) return;
  long n = numGroups();
  if(n == 0) group = 0;
  else       group = ConstrainValue(group, 1, n);
  if(group == _active_group) return;
  _informer->preNewActiveGroup();
  _active_group = group;
  _informer->postNewActiveGroup();
}



void PpCards::freezeDependentUpdates()
{
  if(_frozen) return;
  _frozen = TRUE;
  _acc_file      ->freezeUpdates();
  _acc_sshot     ->freezeUpdates();
  _acc_sline     ->freezeUpdates();
  _acc_rshot     ->freezeUpdates();
  _acc_rline     ->freezeUpdates();
  _acc_pattern   ->freezeUpdates();
  _acc_xskid     ->freezeUpdates();
  _acc_yskid     ->freezeUpdates();
  _acc_hold      ->freezeUpdates();
  _acc_elev      ->freezeUpdates();
  _acc_hd        ->freezeUpdates();
  _acc_tuh       ->freezeUpdates();
  _acc_smove     ->freezeUpdates();
  _acc_rmove     ->freezeUpdates();
  _acc_ngroups   ->freezeUpdates();
  _acc_ntraces   ->freezeUpdates();
  _sum_thru_gr   ->freezeUpdates();
  _sum_thru_tr   ->freezeUpdates();
  _search_thru_gr->freezeUpdates();
  _search_thru_tr->freezeUpdates();
}


void PpCards::resumeDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
  _need   = FALSE;
  _acc_file      ->resumeUpdates();
  _acc_sshot     ->resumeUpdates();
  _acc_sline     ->resumeUpdates();
  _acc_rshot     ->resumeUpdates();
  _acc_rline     ->resumeUpdates();
  _acc_pattern   ->resumeUpdates();
  _acc_xskid     ->resumeUpdates();
  _acc_yskid     ->resumeUpdates();
  _acc_hold      ->resumeUpdates();
  _acc_elev      ->resumeUpdates();
  _acc_hd        ->resumeUpdates();
  _acc_tuh       ->resumeUpdates();
  _acc_smove     ->resumeUpdates();
  _acc_rmove     ->resumeUpdates();
  _acc_ngroups   ->resumeUpdates();
  _acc_ntraces   ->resumeUpdates();
  _sum_thru_gr   ->resumeUpdates();
  _sum_thru_tr   ->resumeUpdates();
  _search_thru_gr->resumeUpdates();
  _search_thru_tr->resumeUpdates();
  setActiveTraceNumber(_active_trace);
  setActiveGroupNumber(_active_group);
}


void PpCards::performDependentUpdates()
{
  if(!_frozen) return;
  _frozen = FALSE;
  _need   = FALSE;
  _acc_file      ->performUpdates();
  _acc_sshot     ->performUpdates();
  _acc_sline     ->performUpdates();
  _acc_rshot     ->performUpdates();
  _acc_rline     ->performUpdates();
  _acc_pattern   ->performUpdates();
  _acc_xskid     ->performUpdates();
  _acc_yskid     ->performUpdates();
  _acc_hold      ->performUpdates();
  _acc_elev      ->performUpdates();
  _acc_hd        ->performUpdates();
  _acc_tuh       ->performUpdates();
  _acc_smove     ->performUpdates();
  _acc_rmove     ->performUpdates();
  _acc_ngroups   ->performUpdates();
  _acc_ntraces   ->performUpdates();
  _sum_thru_gr   ->performUpdates();
  _sum_thru_tr   ->performUpdates();
  _search_thru_gr->performUpdates();
  _search_thru_tr->performUpdates();
}




//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *PpCards::doCreateObject()
{
  return new PpCard();
}


void PpCards::doDeleteObject(void *object)
{
  delete (PpCard*)object;
}




//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//
//----------------------- insert or remove cards --------------------//

   // the non-void functions return index (ixpp) where action occurred.
   // the non-void functions return -1 if failed.
 

long PpCards::appendNewPpCard()
{
  return appendNullElement();
}


long PpCards::insertNewPpCard(long ixpp)
{
  return insertNullElement(ixpp);
}


long PpCards::insertNewPpCardFromBuffer(long ixpp)
{
  return insertElementFromBuffer(ixpp);
}



long PpCards::deletePpCard(long ixpp)
{
  return removeElement(ixpp);
}


long PpCards::deletePpCardToBuffer(long ixpp)
{
  return removeElementToBuffer(ixpp);
}


void PpCards::deleteAllPpCards()
{
  removeAllElements();
}



//--------------------- search among PP cards ----------------------//
//--------------------- search among PP cards ----------------------//
//--------------------- search among PP cards ----------------------//

    // the first two return an index (ixpp), or -1 if not found.
    // the last one returns number of channels, or 0 if not found.

long PpCards::findPpCardWithDesiredGroup (long group) const
{
  long n = numPpCards();
  if(group <= 0 || n == 0) return -1;
  if(group >  ppCard(n-1)->getThruGroupNumber()) return -1;
  return _search_thru_gr->findNearestValue((float)group, 1);
}


long PpCards::findPpCardWithDesiredTrace (long trace) const
{
  long n = numPpCards();
  if(trace <= 0 || n == 0) return -1;
  if(trace >  ppCard(n-1)->getThruTraceNumber()) return -1;
  return _search_thru_tr->findNearestValue((float)trace, 1);
}


long PpCards::findNumChannelsInGroup (long group) const
{
  long ixpp = findPpCardWithDesiredGroup(group);
  if(ixpp == -1) return 0;
  return ppCard(ixpp)->getNumChannelsOnCard();
}


long PpCards::findTraceNumber (long group, long channel) const
{
  long ixpp = findPpCardWithDesiredGroup(group);
  if(ixpp == -1) return 0;
  long nchan = getNumChannelsOnCard(ixpp);
  if(channel <= 0 || channel > nchan) return 0;
  long first_trace = getFirstTraceNumber(ixpp);
  if(first_trace == INIL) return 0;
  long first_group = getFirstGroupNumber(ixpp);
  if(first_group == INIL) return 0;
  return (first_trace + (group - first_group) * nchan + channel - 1);
}



//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//
//----------- pass-thru functions to individual PP cards --------------//

     // ixpp  = index of desired PP card.



//---------------------- get card values ---------------------------//
//---------------------- get card values ---------------------------//
//---------------------- get card values ---------------------------//

int   PpCards::ppValueIsDependent   (long ixpp, int ident)  const
{
  return ppCard(ixpp)->ppValueIsDependent(ident);
}


/*
float PpCards::getPpValue           (long ixpp, int ident)  const
*/
double PpCards::getPpValue           (long ixpp, int ident)  const
{
  return ppCard(ixpp)->getPpValue(ident);
}



#define GETV(getSourceShotpoint, float)                \
float PpCards::getSourceShotpoint(long ixpp) const     \
{                                                      \
  return ppCard(ixpp)->getSourceShotpoint();           \
}


GETV(getFirstFileNumber  , long )
GETV(getThruFileNumber   , long )
GETV(getSourceShotpoint  , float)
GETV(getReceiverShotpoint, float)
GETV(getSourceLine       , long )
GETV(getReceiverLine     , long )
GETV(getPatternNumber    , long )
GETV(getSourceXskid      , float)
GETV(getSourceYskid      , float)
GETV(getSkidHold         , long )
GETV(getNewElevation     , float)
GETV(getNewHoleDepth     , float)
GETV(getNewUpholeTime    , float)
GETV(getSourceMove       , long )
GETV(getReceiverMove     , long )
GETV(getNumGroupsOnCard  , long )
GETV(getNumTracesOnCard  , long )
GETV(getNumChannelsOnCard, long )
GETV(getFirstGroupNumber , long )
GETV(getFirstTraceNumber , long )
GETV(getThruGroupNumber  , long )
GETV(getThruTraceNumber  , long )


void PpCards::getSourceSkids(long ixpp, long group,
                                  float *inline_skid,
                                  float *crossline_skid)  const
{
  ppCard(ixpp)->getSourceSkids(group, inline_skid, crossline_skid);
}


void PpCards::getGroupAndChannel(long ixpp, long trace,
                                  long *group, long *channel)  const
{
  ppCard(ixpp)->getGroupAndChannel(trace, group, channel);
}



//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//
//------------------------ set card values -------------------------//

/*
void PpCards::setPpValue           (long ixpp, int ident, float value)
*/
void PpCards::setPpValue           (long ixpp, int ident, double value)
{
  assert(ident != PP_THRU_GR);
  assert(ident != PP_THRU_TR);
  assert(ident != PP_NTRACES);
  beforeValueChanged(ident, ixpp);
  ppCard(ixpp)->setPpValue(ident, value);
  afterValueChanged(ident, ixpp);
}


/// the value argument is not used for some idents in setDependentPpValue.

/*
void PpCards::setDependentPpValue  (long ixpp, int ident, float value)
*/
void PpCards::setDependentPpValue  (long ixpp, int ident, double value)
{
  assert(ident != PP_THRU_GR);
  assert(ident != PP_THRU_TR);
  assert(ident != PP_NTRACES);
  beforeValueChanged(ident, ixpp);
  ppCard(ixpp)->setDependentPpValue(ident, value);
  afterValueChanged(ident, ixpp);
}



#define SETV(setSourceShotpoint, float, ident)              \
void PpCards::setSourceShotpoint(long ixpp, float value)    \
{                                                           \
  assert(ident != PP_NTRACES);               \
  beforeValueChanged(ident, ixpp);                          \
  ppCard(ixpp)->setSourceShotpoint(value);                  \
  afterValueChanged(ident, ixpp);                           \
}


SETV(setFirstFileNumber  , long , PP_FILE )
SETV(setSourceShotpoint  , float, PP_SSHOT)
SETV(setSourceLine       , long , PP_SLINE)
SETV(setReceiverShotpoint, float, PP_RSHOT)
SETV(setReceiverLine     , long , PP_RLINE)
SETV(setPatternNumber    , long , PP_PAT  )
SETV(setSourceXskid      , float, PP_XSKID)
SETV(setSourceYskid      , float, PP_YSKID)
SETV(setSkidHold         , long , PP_HOLD )
SETV(setNewElevation     , float, PP_ELEV )
SETV(setNewHoleDepth     , float, PP_HD   )
SETV(setNewUpholeTime    , float, PP_TUH  )
SETV(setSourceMove       , long , PP_SMOVE)
SETV(setReceiverMove     , long , PP_RMOVE)
SETV(setNumGroupsOnCard  , long , PP_NGROUPS)
SETV(setNumTracesOnCard  , long , PP_NTRACES)



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

