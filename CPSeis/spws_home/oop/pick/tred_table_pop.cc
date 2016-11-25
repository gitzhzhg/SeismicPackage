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
//------------------- tred_table_pop.cc ----------------------//
//------------------- tred_table_pop.cc ----------------------//
//------------------- tred_table_pop.cc ----------------------//

//         implementation file for the TredTablePop class
//                 derived from the StpPopupBase class
//                        subdirectory tred

#include "sl/sl_smart_form.hh"
#include "sp/seis_plot.hh"
#include "sl/slp_push.hh"
#include "pick/tred_table_pop.hh"
#include "pick/tred_table.hh"
#include "pick/tred_file_pair.hh"
#include "pick/tred_pick_gui.hh"
#include "cprim.h"

#define WANT_UPDATE_BUTTON  FALSE
#define WANT_KEYHELP_BUTTON FALSE

//static const char * const FILETYPE      = "TRED file";
//static const char * const EXTENSION     = "tred";
  static const char * const PICKING_MODE  = "Mode: Trace\nEdit Picking";
  static const char * const HELP_TOKEN    = "TRED_PICKING";
  static const char * const HELP_FALLBACK =
    "mouse*TRED_PICKING: BTN#1: Pick, BTN#2: Delete, BTN#3: Popup Menu";

//static const Boolean REQUIRED1 = FALSE;
//static const Boolean REQUIRED2 = FALSE;

//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//
//------------------- constructors and destructor -----------------//

TredTablePop::TredTablePop (SLDelay *slparent, char *name, HelpCtx hctx,
  SeisPlot *sp)
  : StpPopupBase (slparent, name, hctx,
      sp, WANT_UPDATE_BUTTON, WANT_KEYHELP_BUTTON, PICKING_MODE, HELP_TOKEN,
      HELP_FALLBACK),
    _ttable        (0),
    _pair          (0)

{
// initialize the header word arrays
  int k2;
  for (k2 = 0; k2 < HWDS; k2++) {

    _tred_del_hwds [k2] = 0;
    _tred_kill_hwds[k2] = 0;
    _tred_rev_hwds [k2] = 0;
    _tred_flag_hwds[k2] = 0;
  }
  setType (KILLTYPE);

  SLSmartForm *work = workArea();

  _pk_gui  = new TredPickGui  (work, this, "pick_gui", hctx);

// The following two routines should be placed in a function called
// TredTablePop::becomingActive in the future
  _ttable = new TredTable    (work, "TredTable", this);
  _pair   = new TredFilePair (work, _ttable, this);

  work->attach (_pair,   work, work, work,   0,    0, 0, 0,  0);
  work->attach (_pk_gui,  work, work, _pair,  0,    0, 0, 20, 0);
  work->attach (_ttable, work, work, _pk_gui, work, 0, 0, 20, 0);
}

TredTablePop::~TredTablePop (void)
{
  stopPicking();

  delete _ttable, _ttable = 0;
  delete _pair,   _pair = 0;
  delete _pk_gui, _pk_gui = 0;
}

//-------------------------- get file pair plus --------------------//
//-------------------------- get file pair plus --------------------//
//-------------------------- get file pair plus --------------------//

             // overriding virtual function

SLFilePairPlus *TredTablePop::getFilePairPlus (void)
{
  return _pair;
}

//----------------------- picking action completed  --------------------//
//----------------------- picking action completed  --------------------//
//----------------------- picking action completed  --------------------//

             // overriding virtual function

void TredTablePop::pickingActionCompleted (SeisPlot *sp, int button,
  PickBase::Modifier /*modifier*/, long /*direction*/, long first_trace,
  long last_trace, float /*first_time*/, float /*last_time*/)
{
  switch (_tred_type)
    {
      case DELTYPE :
        modifyCurrentPicks (sp, button==2, "DEL ",
          _tred_del_hwds, first_trace, last_trace);
        break;

      case KILLTYPE:
        modifyCurrentPicks (sp, button==2, "KILL",
          _tred_kill_hwds, first_trace, last_trace);
        break;

      case REVTYPE :
        modifyCurrentPicks (sp, button==2, "REV ",
          _tred_rev_hwds, first_trace, last_trace);
        break;

      case FLAGTYPE:
        modifyCurrentPicks (sp, button==2, "FLAG",
          _tred_flag_hwds, first_trace, last_trace);
        break;

      default      :
        break;
    }
}

void TredTablePop::modifyCurrentPicks (SeisPlot *sp, const Bool undo,
  const char *code, const long *hwds, const long first_trace,
  const long last_trace)
{
  float mins[HWDS], maxs[HWDS];
  int k2;
  long max_header = sp->numHeaders() + 1;

  if ((hwds[0]<1) &&
      (hwds[1]<1) &&
      (hwds[2]<1)   ) return;  // nothing to do

  for (k2 = 0; k2 < HWDS; k2++) {

    mins[k2] = 0, maxs[k2] = 0;
    if (hwds[k2] > 0)
      _ttable->findPickMinMax (sp, first_trace, last_trace, hwds[k2],
        &mins[k2], &maxs[k2]);
  }

  _ttable->updatePick (sp, undo, code,
    hwds[0]>1 && hwds[0]<max_header, hwds[0], mins[0], maxs[0],
    hwds[1]>1 && hwds[1]<max_header, hwds[1], mins[1], maxs[1],
    hwds[2]>1 && hwds[2]<max_header, hwds[2], mins[2], maxs[2]);
}

//-------------------------- create vectors -----------------------//
//-------------------------- create vectors -----------------------//
//-------------------------- create vectors -----------------------//

             // overriding virtual function

SeisVectLinkedList *TredTablePop::createVectors (SeisPlot *sp)
{
  return _ttable->createVectors (sp);
}

//-------------------------- delete vectors -----------------------//
//-------------------------- delete vectors -----------------------//
//-------------------------- delete vectors -----------------------//

             // overriding virtual function

void TredTablePop::deleteVectors (void)
{
  _ttable->deleteVectors ();
}

//-------------------------- update vectors -----------------------//
//-------------------------- update vectors -----------------------//
//-------------------------- update vectors -----------------------//

             // overriding virtual function

void TredTablePop::updateVectors (SeisPlot *sp, Why /*why*/)
{
  _ttable->post (sp);
}

void TredTablePop::setType (TredType type)
{
  _tred_type = type;
}

long TredTablePop::getHeaderWord (TredType type, long id)
{
  switch (type) {
    case DELTYPE:
      setDefaultHeaderWords (_tred_del_hwds );
      return getHeaderWordHelper (_tred_del_hwds,  id);
//    break;
    case KILLTYPE:
      setDefaultHeaderWords (_tred_kill_hwds);
      return getHeaderWordHelper (_tred_kill_hwds, id);
//    break;
    case REVTYPE:
      setDefaultHeaderWords (_tred_rev_hwds );
      return getHeaderWordHelper (_tred_rev_hwds,  id);
//    break;
    case FLAGTYPE:
      setDefaultHeaderWords (_tred_flag_hwds);
      return getHeaderWordHelper (_tred_flag_hwds, id);
//    break;
    default:
      setDefaultHeaderWords (_tred_kill_hwds);
      return getHeaderWordHelper (_tred_kill_hwds, id);
//    break;
  }
}

void TredTablePop::setDefaultHeaderWords (long *hwds)
{
  Boolean found = FALSE;
  for (int k2 = 0; k2 < HWDS && !found; k2++) {
    if (hwds[k2] > 0) found = TRUE;
  }
  if (!found) {
    hwds[0] =  9;
    hwds[1] = 10;
  }
}

void TredTablePop::setDeleteHeaderWord (const long hwd, const long id)
{
  _tred_del_hwds [id] = hwd;
}

void TredTablePop::setKillHeaderWord (const long hwd, const long id)
{
  _tred_kill_hwds[id] = hwd;
}

void TredTablePop::setReverseHeaderWord (const long hwd, const long id)
{
  _tred_rev_hwds [id] = hwd;
}

void TredTablePop::setFlagHeaderWord (const long hwd, const long id)
{
  _tred_flag_hwds[id] = hwd;
}

// I don't think the following routine is used
TredTable *TredTablePop::getTredTableObject()
{
  return _ttable;
}

// I don't think the following routine is used
TredFilePair *TredTablePop::getTredFilePairObject()
{
  return _pair;
}

void TredTablePop::updateFile (SeisPlot *sp, Why /*why*/)
{
  _ttable->post (sp);
  _pair->goUpdateFile ();
}

int TredTablePop::filePut ()
{
  SeisPlot *sp = getSeisPlot ();
  _pk_gui->updateNumHeaders(sp->numHeaders());
  _ttable->post (sp);
  return _pair->filePut ();
}

//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
