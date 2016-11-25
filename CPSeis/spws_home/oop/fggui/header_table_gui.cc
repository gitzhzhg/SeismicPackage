
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
//---------------------- header_table_gui.cc -----------------------//
//---------------------- header_table_gui.cc -----------------------//
//---------------------- header_table_gui.cc -----------------------//


//         implementation file for the HeaderTableGui class
//               derived from the SLMatrixViewBox class
//                also derived from the FgInform class
//                         subdirectory fggui

     // Displays trace headers from FieldGeometry.


#include "fggui/header_table_gui.hh"
#include "fggui/header_top_gui.hh"
#include "fggui/fg_active_choice.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_range_select.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_push.hh"
#include "wbox.h"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>


static const int NCHAR              = 10;
static const int NDEC               = 10;
static const int NROWMAX            = 35;
static const int NCOLMAX            =  7;
static const int WIDTH1             =  2;
static const int WIDTH2             =  2;
static const int WIDTH3             = 34;
static const int FIRST_HOLD_IDENT   = 101;
static const int FIRST_SCROLL_IDENT = 201;


enum { SHOW_ALL_TRACES = 1 , SHOW_ACTIVE_GROUP,
       SHOW_SOURCE_GATHER,
       SHOW_RECEIVER_GATHER, SHOW_MIDPOINT_GATHER };

enum { SCROLL_TRACE  = 1, SCROLL_GROUP,
       SCROLL_SLD       , SCROLL_RLD,
       SCROLL_RP        , SCROLL_PP ,    SCROLL_CMP,
       SCROLL_SPLUS     , SCROLL_RPLUS };

enum { MORE_ALL = 1, MORE_SOME };



//----------------------- get values ---------------------------//
//----------------------- get values ---------------------------//
//----------------------- get values ---------------------------//

     // public.
     // getWhichSource always returns a valid source number (1 or more)
     //   at the active flag, or 0 if there are no sources at that flag.


long HeaderTableGui::getWhichSource()  const
{
  long nsources = _fg->numSourcesAtActiveFlag();
  if(nsources == 0)            return 0;
  if(_which_source < 1)        return 1;
  if(_which_source > nsources) return nsources;
  return _which_source;
}



//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//

     // public.

void HeaderTableGui::setWhichSource(long value)
{
  long nsources = _fg->numSourcesAtActiveFlag();
  _which_source = value;
  finishedChanges(_fg);
  postNewActiveTrace(_fg);
  finishedChanges(_fg);
}



void HeaderTableGui::setShowChoice(long value)
{
  FgActiveChoice *top_gather = _top->getGatherArrows();
  switch(value)
      {
      case SHOW_ALL_TRACES     :
              top_gather->setActiveChoice(FgActiveChoice::TRACE); break;
      case SHOW_ACTIVE_GROUP   :
              top_gather->setActiveChoice(FgActiveChoice::GROUP); break;
      case SHOW_SOURCE_GATHER  :   // fall thru to next case.
      case SHOW_RECEIVER_GATHER:
              top_gather->setActiveChoice(FgActiveChoice::FLAG); break;
      case SHOW_MIDPOINT_GATHER:
              top_gather->setActiveChoice(FgActiveChoice::CMP); break;
      default                  : assert(FALSE);
      }
  if(value == _show_choice) return;
  _show_choice = value;
  finishedChanges(_fg);
  postNewActiveTrace(_fg);
  finishedChanges(_fg);
}



void HeaderTableGui::setScrollChoice(long value)
{
  switch(value)
      {
      case SCROLL_TRACE : break;
      case SCROLL_GROUP : break;
      case SCROLL_SLD   : break;
      case SCROLL_RLD   : break;
      case SCROLL_RP    : break;
      case SCROLL_PP    : break;
      case SCROLL_CMP   : break;
      case SCROLL_SPLUS : break;
      case SCROLL_RPLUS : break;
      default           : assert(FALSE);
      }
  _scroll_choice = value;
}



//------------------ top range select trap ----------------------//
//------------------ top range select trap ----------------------//
//------------------ top range select trap ----------------------//

     // called from HeaderTopGui.
     // called from SLRangeSelect when a selection (new
     //   first visible column) is made.

static void top_range_select_trap(void *data, long first)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  THIS->setFirstVisibleColumn(first);
}



//------------------ top show traps and update functions -------------//
//------------------ top show traps and update functions -------------//
//------------------ top show traps and update functions -------------//

     // called from HeaderTopGui guis.

static void top_show_trap(void *data, long /*ident*/,
                                long /*oldvar*/, long newvar)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  THIS->setShowChoice(newvar);
}

static long top_show_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  return THIS->getShowChoice();
}


static long top_t_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0) return FALSE;
  return TRUE;
}


static long top_g_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0) return FALSE;
  if(fg->getActiveGroupNumber() == 0) return FALSE;
  return TRUE;
}


static long top_s_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0) return FALSE;
  return !fg->sourceGathersOutOfDate();
}


static long top_r_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0) return FALSE;
  return !fg->receiverGathersOutOfDate();
}


static long top_m_sense_update(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  if(fg->numTraces() == 0) return FALSE;
  if(fg->receiverGathersOutOfDate()) return FALSE;
  if(fg->numCmpGathers() == 0) return FALSE;
  return TRUE;
}


static void top_sort_trap(void *data, long /*ident*/)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  THIS->sortHeaders();
}



//-------------------- sort headers ------------------------//
//-------------------- sort headers ------------------------//
//-------------------- sort headers ------------------------//

       // public so it can be called from top_sort_trap.

void HeaderTableGui::sortHeaders()
{
  for(int j = 0; j < CPS_MAX_HEADERS; j++)
      {
      for(int i = 1; i < CPS_MAX_HEADERS; i++)
          {
          int flip = FALSE;
          if(_ascending)
              {
              if     ( _toggled[i] && !_toggled[i-1]) flip = TRUE;
              else if(!_toggled[i] &&  _toggled[i-1]) flip = FALSE;
              else if( _order  [i] <   _order  [i-1]) flip = TRUE;
              }
          else
              {
              if(_order[i] < _order[i-1]) flip = TRUE;
              }
          if(flip)
              {
              int temp    = _order[i];
              _order[i]   = _order[i-1];
              _order[i-1] = temp;
              temp          = _toggled[i];
              _toggled[i]   = _toggled[i-1];
              _toggled[i-1] = temp;
              }
          }
      }
  _ascending = !_ascending;
  if(!_ascending) makeRowVisible(1);
}



//------------   top source traps and update functions ------------//
//------------   top source traps and update functions ------------//
//------------   top source traps and update functions ------------//

     // called from HeaderTopGui.

static void top_source_num1_trap(void *data, long newvar)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  THIS->setWhichSource(newvar);
}


static long top_source_num1_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  return THIS->getWhichSource();
}


static long top_source_num2_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  return fg->numSourcesAtActiveFlag();
}


static long top_source_sense_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  return (THIS->getShowChoice() == SHOW_SOURCE_GATHER);
}



/*
//------------ gather arrow traps and update functions ------------//
//------------ gather arrow traps and update functions ------------//
//------------ gather arrow traps and update functions ------------//

     // called from HeaderTopGui guis.

static void top_gather_num1_trap(void *data, long newvar)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  long show_choice = THIS->getShowChoice();
  switch(show_choice)
      {
      case SHOW_ALL_TRACES:
            {
            long ntraces = fg->numTraces();
            if(ntraces == 0) break;
            long trace = newvar;
            if(trace <= 0 || trace > ntraces) break;
            fg->setActiveTraceNumber(trace);
            THIS->makeColumnVisible(trace);  // shouldn't be needed
            }
            break;
      case SHOW_ACTIVE_GROUP:
            {
            long ngroups = fg->numGroups();
            if(ngroups == 0) break;
            long group = newvar;
            if(group <= 0 || group > ngroups) break;
            fg->setActiveGroupNumber(group);
            }
            break;
      case SHOW_SOURCE_GATHER  :    // fall thru to next case.
      case SHOW_RECEIVER_GATHER:
            {
            long ixl = fg->getActiveLineIndex();
            if(ixl == -1) break;
            long nflags = fg->numFlagsOnLine(ixl);
            if(nflags == 0) break;
            long ixf = newvar - 1;
            if(ixf < 0 || ixf >= nflags) break;
            fg->setActiveFlagIndexOnLine(ixl, ixf);
            }
            break;
      case SHOW_MIDPOINT_GATHER:
            {
            long ncmps = fg->numCmpGathers();
            if(ncmps == 0) break;
            long ixcmp = newvar - 1;
            if(ixcmp < 0 || ixcmp >= ncmps) break;
            fg->setActiveCmpIndex(ixcmp);
            }
            break;
      default:
            assert(FALSE);
      }
}


static long top_gather_num1_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  long show_choice = THIS->getShowChoice();
  switch(show_choice)
      {
      case SHOW_ALL_TRACES     : return fg->getActiveTraceNumber();
      case SHOW_ACTIVE_GROUP   : return fg->getActiveGroupNumber();
      case SHOW_SOURCE_GATHER  :    // fall thru to next case.
      case SHOW_RECEIVER_GATHER:
            {
            long ixl = fg->getActiveLineIndex();
            if(ixl == -1) return 0;
            long ixf = fg->getActiveFlagIndexOnLine(ixl);
            if(ixf == -1) return 0;
            return ixf + 1;
            }
      case SHOW_MIDPOINT_GATHER: return fg->getActiveCmpIndex() + 1;
      default                  : assert(FALSE);
      }
  assert(FALSE);
  return 0;
}


static long top_gather_num2_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  long show_choice = THIS->getShowChoice();
  switch(show_choice)
      {
      case SHOW_ALL_TRACES     : return fg->numTraces();
      case SHOW_ACTIVE_GROUP   : return fg->numGroups();
      case SHOW_SOURCE_GATHER  :    // fall thru to next case.
      case SHOW_RECEIVER_GATHER:
            {
            long ixl = fg->getActiveLineIndex();
            if(ixl == -1) return 0;
            return fg->numFlagsOnLine(ixl);
            }
      case SHOW_MIDPOINT_GATHER: return fg->numCmpGathers();
      default                  : assert(FALSE);
      }
  assert(FALSE);
  return 0;
}


static char *top_gather_label_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  FieldGeometry *fg = THIS->getFieldGeometry();
  long show_choice = THIS->getShowChoice();
  static char *buffer1 = "active trace";
  static char *buffer2 = "active group";
  static char *buffer3 = "active flag";
  static char *buffer4 = "active flag";
  static char *buffer5 = "active CMP";
  switch(show_choice)
      {
      case SHOW_ALL_TRACES     : return buffer1;
      case SHOW_ACTIVE_GROUP   : return buffer2;
      case SHOW_SOURCE_GATHER  : return buffer3;
      case SHOW_RECEIVER_GATHER: return buffer4;
      case SHOW_MIDPOINT_GATHER: return buffer5;
      default                  : assert(FALSE);
      }
  assert(FALSE);
  return buffer1;
}
*/



//------------------ scroll traps and update functions -------------//
//------------------ scroll traps and update functions -------------//
//------------------ scroll traps and update functions -------------//

     // called from HeaderTopGui guis.

static void top_scroll_focusin_trap(void *data, long /*ident*/)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  THIS->SLMatrixViewBox::showMessage
("choose here which table to scroll to when you press a SCROLL button");
}

static void top_scroll_trap(void *data, long /*ident*/,
                                long /*oldvar*/, long newvar)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  THIS->setScrollChoice(newvar);
}

static long top_scroll_update(void *data)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  return THIS->getScrollChoice();
}



//---------------------- constructor ------------------------//
//---------------------- constructor ------------------------//
//---------------------- constructor ------------------------//


HeaderTableGui::HeaderTableGui(SLDelay *slparent, char *name,
                               FieldGeometry *fg,
                               HeaderTopGui  *top)
           : SLMatrixViewBox(slparent, name, NULL,
                             NCHAR, NDEC, NROWMAX, NCOLMAX,
                             WIDTH1, WIDTH2, WIDTH3),
             FgInform(fg),
                 _top           (top),
                 _hold          (NULL),
                 _toggled       (NULL),
                 _order         (NULL),
                 _ascending     (TRUE),
                 _trace         (0),
                 _current_trace (0),
                 _which_source  (1),
                 _show_choice   (SHOW_ALL_TRACES),
                 _scroll_choice (SCROLL_RPLUS),
                 _more_choice   (MORE_ALL),
                 _round_choice  (FALSE)
{
  assert(_top);

////// add attributes to guis in HeaderTopGui:

  long numvis  = getNumVisibleColumns();
  long first   = getFirstVisibleColumn();
  long ntraces = numColumnsUpdate();
  if(ntraces == 0) ntraces = 1;

  SLRangeSelect  *top_range   = _top->getRangeSelect();
  SLpOption      *top_show    = _top->getShowOption();
  SLpOption      *top_scroll  = _top->getScrollOption();
  SLpOption      *top_more    = _top->getMoreOption();
  SLpToggle      *top_round   = _top->getRoundToggle();
  SLpPush        *top_sort    = _top->getSortPush();
  SL2Arrows      *top_source  = _top->getSourceArrows();
//SL2Arrows      *top_gather  = _top->getGatherArrows();
//FgActiveChoice *top_gather  = _top->getGatherArrows();

  assert(top_range);
  assert(top_show);
  assert(top_scroll);
  assert(top_more);
  assert(top_round);
  assert(top_sort);
  assert(top_source);
//assert(top_gather);

  setShowChoice(_show_choice);

  top_range->setValues          (first, 1, ntraces, numvis);
  top_range->setRangeSelectTrap (top_range_select_trap, this);

  top_show->addOption
              ("show all traces"                    , SHOW_ALL_TRACES);
  top_show->addOption
              ("show active SOURCE GATHER (GROUP)"  , SHOW_ACTIVE_GROUP);
  top_show->addOption
              ("show SOURCE GATHER at active flag"  , SHOW_SOURCE_GATHER);
  top_show->addOption
              ("show RECEIVER GATHER at active flag", SHOW_RECEIVER_GATHER);
  top_show->addOption
              ("show active MIDPOINT GATHER"        , SHOW_MIDPOINT_GATHER);

  top_show->setItrap            (top_show_trap           , this);
  top_show->setupIvarFun        (top_show_update         , this);
  top_show->setupOptionSenseFun (SHOW_ALL_TRACES     , top_t_sense_update, _fg);
  top_show->setupOptionSenseFun (SHOW_ACTIVE_GROUP   , top_g_sense_update, _fg);
  top_show->setupOptionSenseFun (SHOW_SOURCE_GATHER  , top_s_sense_update, _fg);
  top_show->setupOptionSenseFun (SHOW_RECEIVER_GATHER, top_r_sense_update, _fg);
  top_show->setupOptionSenseFun (SHOW_MIDPOINT_GATHER, top_m_sense_update, _fg);

  top_more->addOption("calculate all header words"         , MORE_ALL);
  top_more->addOption("calculate some header words"        , MORE_SOME);

  top_more->setupIvarPoint      (&_more_choice);
  top_round->setupIvarPoint     (&_round_choice);
  top_sort->setAtrap            (top_sort_trap          , this);

  top_source ->registerNum1Trap    (top_source_num1_trap   , this);
//top_source ->registerLabelUpdate (top_source_label_update, this);
  top_source ->registerNum1Update  (top_source_num1_update , this);
  top_source ->registerNum2Update  (top_source_num2_update , this);
  top_source ->registerSenseUpdate (top_source_sense_update, this);

/*
  top_gather ->registerNum1Trap    (top_gather_num1_trap   , this);
  top_gather ->registerLabelUpdate (top_gather_label_update, this);
  top_gather ->registerNum1Update  (top_gather_num1_update , this);
  top_gather ->registerNum2Update  (top_gather_num2_update , this);
//top_gather ->registerSenseUpdate (top_gather_sense_update, this);
*/

  top_scroll->addOption("choose active trace (no scroll)", SCROLL_TRACE);
  top_scroll->addOption("choose active group (no scroll)", SCROLL_GROUP);
  top_scroll->addOption("scroll to source LD card"       , SCROLL_SLD);
  top_scroll->addOption("scroll to receiver LD card"     , SCROLL_RLD);
  top_scroll->addOption("scroll to RP card"              , SCROLL_RP);
  top_scroll->addOption("scroll to PP card"              , SCROLL_PP);
  top_scroll->addOption("scroll to matching CMP"         , SCROLL_CMP);
  top_scroll->addOption("scroll to source LD,RP,PP,CMP"  , SCROLL_SPLUS);
  top_scroll->addOption("scroll to receiver LD,RP,PP,CMP", SCROLL_RPLUS);

  top_scroll->setFocusinTrap      (top_scroll_focusin_trap , this);
  top_scroll->setItrap            (top_scroll_trap         , this);
  top_scroll->setupIvarFun        (top_scroll_update       , this);

////// allocate and initialize arrays:

  _hold = new long [NCOLMAX];
  _toggled = new int [CPS_MAX_HEADERS];
  _order   = new int [CPS_MAX_HEADERS];
  int i;
  for(i = 0; i < NCOLMAX        ; i++)  { _hold   [i] =     0; }
  for(i = 0; i < CPS_MAX_HEADERS; i++)  { _toggled[i] = FALSE; }
  for(i = 0; i < CPS_MAX_HEADERS; i++)  { _order  [i] =     i; }
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

HeaderTableGui::~HeaderTableGui()
{
  delete [] _hold;
  delete [] _toggled;
  delete [] _order;
}



//-------- virtual functions overriding SLMatrixViewBox ----------//
//-------- virtual functions overriding SLMatrixViewBox ----------//
//-------- virtual functions overriding SLMatrixViewBox ----------//

       // protected.

void HeaderTableGui::newFirstVisibleColumn (long first)
{
  SLRangeSelect *range = _top->getRangeSelect();
  assert(range);
  range->setValue(first);
}


void HeaderTableGui::newNumVisibleColumns (long numvis)
{
  SLRangeSelect *range = _top->getRangeSelect();
  assert(range);
  range->setPage(numvis);
}



//------------------ overriding update functions -------------------//
//------------------ overriding update functions -------------------//
//------------------ overriding update functions -------------------//

       // protected.

long HeaderTableGui::numRowsUpdate ()
{
  return CPS_MAX_HEADERS;
}


long HeaderTableGui::numColumnsUpdate ()
{
  switch(_show_choice)
      {
      case SHOW_ALL_TRACES:
                return _fg->numTraces();
      case SHOW_ACTIVE_GROUP:
                {
                long group = _fg->getActiveGroupNumber();
                if(group == 0) return 0;
                return _fg->findNumChannelsInGroup(group);
                }
      case SHOW_SOURCE_GATHER:
                {
                long ixs2 = getWhichSource() - 1;
                if(ixs2 < 0) return 0;
                long group = _fg->sourceGroupNumberAtActiveFlag(ixs2);
                if(group == 0) return 0;
                return _fg->findNumChannelsInGroup(group);
                }
      case SHOW_RECEIVER_GATHER:
                return _fg->numReceiversAtActiveFlag();
      case SHOW_MIDPOINT_GATHER:
                {
                long ixcmp = _fg->getActiveCmpIndex();
                return _fg->foldOfStack(ixcmp);  // OK if ixcmp is -1.
                }
      default: assert(FALSE);
      }
  assert(FALSE);
  return 0;
}


void HeaderTableGui::readyToUpdate ()
{
  _current_trace = 0;
  _trace         = 0;
}


double HeaderTableGui::matrixUpdate (long irow, long icol)
{
  int  ihead         = getHeaderWordNumberFromRow     (irow);
  long current_trace = getCurrentTraceNumberFromColumn(icol);
  calculateHeaders(current_trace);
  if(ihead == 0) return 0.0;
  double value = _fg->getHeaderWordValue(ihead);
  if(!_round_choice || value == DNIL) return value;
  return (double)NearestInteger(value);
}


char *HeaderTableGui::promptUpdate (long icol)
{
  long current_trace = getCurrentTraceNumberFromColumn(icol);
  calculateHeaders(current_trace);
  static char prompt[25];
  int error = _fg->getHeaderErrorFlag();
  if(error) sprintf(prompt, "(%d)", current_trace);
  else      sprintf(prompt, "%d"  , current_trace);
  return prompt;
}



long  HeaderTableGui::integerUpdate (long irow)
{
  return getHeaderWordNumberFromRow(irow);
}



char *HeaderTableGui::messageUpdate (long irow)
{
  static char *blank = " ";
  int ihead = getHeaderWordNumberFromRow(irow);
  if(ihead == 0) return blank;
  return (char*)_fg->getHeaderWordDescription(ihead) + 3;
                           // +3 to skip off number.
}



//-------------- overriding switch update functions ---------------//
//-------------- overriding switch update functions ---------------//
//-------------- overriding switch update functions ---------------//

       // protected.

long HeaderTableGui::integerSwitchUpdate (long irow)
{
  if(_toggled[irow - 1]) return 6;
  return 2;
}



//------------------- overriding traps -------------------------//
//------------------- overriding traps -------------------------//
//------------------- overriding traps -------------------------//

       // protected.

void HeaderTableGui::matrixTrap (long irow, long /*icol*/, double /*dvar*/,
                                 long /*nread*/, char* endkey)
{
  int ihead = getHeaderWordNumberFromRow(irow);
  if(ihead == 0) return;
  if(strings_equal(endkey, "ARRIVED"))
      {
          char *msg = (char*)_fg->getHeaderWordDescription(ihead);
          SLMatrixViewBox::showMessage(msg);
      }
}



void HeaderTableGui::integerTrap(long irow, long /*ivar*/,
                                          long /*nread*/, char* endkey)
{
  if(strings_equal(endkey, "RETURN"))
                       _toggled[irow - 1] = !_toggled[irow - 1];
}
 


//-------------- get header word number from row ----------------//
//-------------- get header word number from row ----------------//
//-------------- get header word number from row ----------------//

   // private.
   // given   irow  == 1 thru CPS_MAX_HEADERS,
   // returns ihead == 1 thru CPS_MAX_HEADERS.

int HeaderTableGui::getHeaderWordNumberFromRow(long irow)  const
{
  return (_order[irow - 1] + 1);
}



//--------- get current trace number from column -------------//
//--------- get current trace number from column -------------//
//--------- get current trace number from column -------------//

       // private.
       // returns the trace number within the current collection of
       //   traces (as understood by SLRangeSelect and SLMatrixViewBox),

long HeaderTableGui::getCurrentTraceNumberFromColumn(long icol)  const
{
  long first = getFirstVisibleColumn();
  long hold_index = icol - first;
  assert(hold_index >= 0 && hold_index < NCOLMAX);
  long current_trace;
  if(_hold[hold_index] > 0) current_trace = _hold[hold_index];
  else                      current_trace = icol;
  return current_trace;
}



//----------------------- get original trace number --------------//
//----------------------- get original trace number --------------//
//----------------------- get original trace number --------------//

       // private.
       // given trace number in current gather,
       // returns original trace number.
       // returns 0 if original or current trace is out-of-range.

long HeaderTableGui::getOriginalTraceNumber(long current_trace)  const
{
  long trace = 0;
  if(current_trace <= 0) return trace;
  switch(_show_choice)
      {
      case SHOW_ALL_TRACES:
             {
             trace = current_trace;
             }
             break;
      case SHOW_ACTIVE_GROUP:
             {
             long group = _fg->getActiveGroupNumber();
             if(group == 0) break;
             long nchan = _fg->findNumChannelsInGroup(group);
             if(nchan == 0) break;
             long channel = current_trace;
             if(channel <= 0 || channel > nchan) break;
             trace = _fg->findTraceNumber(group, channel);
             }
             break;
      case SHOW_SOURCE_GATHER: 
             {
             long ixs2 = getWhichSource() - 1;
             if(ixs2 < 0) break;
             long group = _fg->sourceGroupNumberAtActiveFlag(ixs2);
             if(group == 0) break;
             long nchan = _fg->findNumChannelsInGroup(group);
             if(nchan == 0) break;
             long channel = current_trace;
             if(channel <= 0 || channel > nchan) break;
             trace = _fg->findTraceNumber(group, channel);
             }
             break;
      case SHOW_RECEIVER_GATHER: 
             {
             long nrec = _fg->numReceiversAtActiveFlag();
             if(nrec == 0) break;
             long ixr2 = current_trace - 1;
             if(ixr2 < 0 || ixr2 >= nrec) break;
             trace = _fg->receiverTraceNumberAtActiveFlag(ixr2);
             }
             break;
      case SHOW_MIDPOINT_GATHER: 
             {
             long ixcmp = _fg->getActiveCmpIndex();
             if(ixcmp == -1) break;
             long nfold = _fg->foldOfStack(ixcmp);
             if(nfold == 0) break;
             long ixfold = current_trace - 1;
             if(ixfold < 0 || ixfold >= nfold) break;
             long ixorig = _fg->originalTraceIndex(ixcmp, ixfold);
             if(ixorig == -1) break;
             trace = ixorig + 1;
             }
             break;
      default:
             {
             assert(FALSE);
             }
      }
  return trace;
}



//----------------------- get current trace number --------------//
//----------------------- get current trace number --------------//
//----------------------- get current trace number --------------//

       // private.
       // given original trace number,
       // returns trace number in current gather.
       // returns 0 if original or current trace is out-of-range.

long HeaderTableGui::getCurrentTraceNumber(long trace)  const
{
  long current_trace = 0;
  if(trace <= 0) return current_trace;
  switch(_show_choice)
      {
      case SHOW_ALL_TRACES:
             {
             current_trace = trace;
             }
             break;
      case SHOW_ACTIVE_GROUP:
             {
             long group = _fg->getActiveGroupNumber();
             if(group == 0) break;
             long nchan = _fg->findNumChannelsInGroup(group);
             if(nchan == 0) break;
             long trace1 = _fg->findTraceNumber(group, 1);
             long trace2 = _fg->findTraceNumber(group, nchan);
             if(trace < trace1) break;
             if(trace > trace2) break;
             current_trace = trace - trace1 + 1;
             }
             break;
      case SHOW_SOURCE_GATHER:
             {
             long ixs2 = getWhichSource() - 1;
             if(ixs2 < 0) break;
             long group = _fg->sourceGroupNumberAtActiveFlag(ixs2);
             if(group == 0) break;
             long nchan = _fg->findNumChannelsInGroup(group);
             if(nchan == 0) break;
             long trace1 = _fg->findTraceNumber(group, 1);
             long trace2 = _fg->findTraceNumber(group, nchan);
             if(trace < trace1) break;
             if(trace > trace2) break;
             current_trace = trace - trace1 + 1;
             }
             break;
      case SHOW_RECEIVER_GATHER:
             {
             long nrec = _fg->numReceiversAtActiveFlag();
             if(nrec == 0) break;
             for(long ixr2 = 0; ixr2 < nrec; ixr2++)
                 {
                 long itr = _fg->receiverTraceNumberAtActiveFlag(ixr2);
                 if(itr != trace) continue;
                 current_trace = ixr2 + 1;
                 break;
                 }
             }
             break;
      case SHOW_MIDPOINT_GATHER:
             {
             long ixcmp = _fg->getActiveCmpIndex();
             if(ixcmp == -1) break;
             long nfold = _fg->foldOfStack(ixcmp);  // OK if ixcmp is -1.
             if(nfold == 0) break;
             for(int ixfold = 0; ixfold < nfold; ixfold++)
                 {
                 long ixorig = _fg->originalTraceIndex(ixcmp, ixfold);
                 if(ixorig + 1 != trace) continue;
                 current_trace = ixfold + 1;
                 break;
                 }
             }
             break;
      default:
             {
             assert(FALSE);
             }
      }
  return current_trace;
}



//----------------- calculate headers ------------------------//
//----------------- calculate headers ------------------------//
//----------------- calculate headers ------------------------//

       // private.
       // given a trace number within the current collection of
       //   traces (as understood by SLRangeSelect and SLMatrixViewBox),
       //   determines the corresponding actual sequential trace
       //   number in the survey, and calculates the headers for
       //   that trace.
       // resets _current_trace to the argument.
       // resets _trace to the original sequential trace number.

void HeaderTableGui::calculateHeaders(long current_trace)
{
  if(current_trace == _current_trace) return;
  _current_trace = current_trace;
  _trace = getOriginalTraceNumber(_current_trace);
  int more = (_more_choice == MORE_ALL);
  _fg->calculateHeaderWords(_trace, more);
}



//-------------------- static scroll functions -------------------------//
//-------------------- static scroll functions -------------------------//
//-------------------- static scroll functions -------------------------//

        // private.
        // "scroll" pushbuttons above each column.

void HeaderTableGui::staticScrollTrap
                         (void *data, long ident, long /*index*/,
                          char * /*cvar*/, long /*nread*/, char* endkey)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;

  if(strings_equal(endkey, "ARRIVED"))
      {
      switch(THIS->_scroll_choice)
          {
          case SCROLL_TRACE : THIS->SLMatrixViewBox::showMessage
("press to select this trace as the active trace (no tables will be scrolled)");
            break;
          case SCROLL_GROUP : THIS->SLMatrixViewBox::showMessage
("press to select this group as the active group (no tables will be scrolled)");
            break;
          case SCROLL_SLD   : THIS->SLMatrixViewBox::showMessage
("press to scroll table of LD cards to the source location of this trace");
            break;
          case SCROLL_RLD   : THIS->SLMatrixViewBox::showMessage
("press to scroll table of LD cards to the receiver location of this trace");
            break;
          case SCROLL_RP    : THIS->SLMatrixViewBox::showMessage
("press to scroll table of RP cards to the receiver for this trace");
            break;
          case SCROLL_PP    : THIS->SLMatrixViewBox::showMessage
("press to scroll table of PP cards to the source for this trace");
            break;
          case SCROLL_CMP   : THIS->SLMatrixViewBox::showMessage
("press to scroll table of CMP gathers to the CMP of this trace");
            break;
          case SCROLL_SPLUS : THIS->SLMatrixViewBox::showMessage
("press to scroll LD(source), RP, PP, and CMP tables to match this trace");
            break;
          case SCROLL_RPLUS : THIS->SLMatrixViewBox::showMessage
("press to scroll LD(receiver), RP, PP, and CMP tables to match this trace");
            break;
          default: assert(FALSE);
          }
      }

  if(strings_equal(endkey, "RETURN"))
      {
      FieldGeometry *fg = THIS->getFieldGeometry();
      long scroll_index = ident - FIRST_SCROLL_IDENT;
      assert(scroll_index >= 0 && scroll_index < NCOLMAX);
      long first = THIS->getFirstVisibleColumn();
      long current_trace = first + scroll_index;
      THIS->calculateHeaders(current_trace);
      switch(THIS->_scroll_choice)
          {
          long ixl, ixf, ixrp, ixpp;
          long trace;
          double group;
          case SCROLL_TRACE:
            trace = fg->getHeaderTraceNumber();
            if(trace == 0) break;
            fg->setActiveTraceNumber(trace);
            break;
          case SCROLL_GROUP:
            group = fg->getHeaderWordValue(9);
            if(group == 0 || group == DNIL) break;
            fg->setActiveGroupNumber(NearestInteger(group));
            break;
          case SCROLL_SLD:
            ixl = fg->getHeaderSourceLineIndex();
            ixf = fg->getHeaderSourceFlagIndex();
            fg->setActiveIndices(ixl, ixf);
            break;
          case SCROLL_RLD:
            ixl = fg->getHeaderReceiverLineIndex();
            ixf = fg->getHeaderReceiverFlagIndex();
            fg->setActiveIndices(ixl, ixf);
            break;
          case SCROLL_RP :
            ixrp = fg->getHeaderRpCardIndex();
            if(ixrp == -1) break;
            fg->setActiveRpCardIndex(ixrp);
            break;
          case SCROLL_PP :
            ixpp = fg->getHeaderPpCardIndex();
            if(ixpp == -1) break;
            fg->setActivePpCardIndex(ixpp);
            break;
          case SCROLL_CMP:
            ixpp = fg->getHeaderCmpIndex();
            if(ixpp == -1) break;
            fg->setActiveCmpIndex(ixpp);
            break;
          case SCROLL_SPLUS:
            fg->setActiveSourceIndices(THIS->_trace);
            break;
          case SCROLL_RPLUS:
            fg->setActiveReceiverIndices(THIS->_trace);
            break;
          default: assert(FALSE);
          }
      THIS->finishedChanges(fg);
      THIS->postNewActiveTrace(fg);
      THIS->finishedChanges(fg);
      THIS->_current_trace = 0;
      }
}



static char *static_scroll_update
                      (void* /*data*/, long /*ident*/, long /*index*/)
{
  static char *scroll_label = "scroll";
  return scroll_label;
}



long HeaderTableGui::staticScrollSwitchUpdate
                      (void* data, long ident, long /*index*/)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  long active = THIS->_fg->getActiveTraceNumber();
  if(active == 0) return 2;
  long scroll_index = ident - FIRST_SCROLL_IDENT;
  assert(scroll_index >= 0 && scroll_index < NCOLMAX);
  long first = THIS->getFirstVisibleColumn();
  long current_trace = first + scroll_index;
  long trace = THIS->getOriginalTraceNumber(current_trace);
  if(trace == active) return 6;
  return 2;
}



static char *LABEL_TRACE   = "<--- press to select active trace";
static char *LABEL_GROUP   = "<--- press to select active group";
static char *LABEL_SLD     = "<--- scroll to source in LD table";
static char *LABEL_RLD     = "<--- scroll to receiver in LD table";
static char *LABEL_RP      = "<--- scroll to receiver in RP table";
static char *LABEL_PP      = "<--- scroll to source in PP table";
static char *LABEL_CMP     = "<--- scroll to CMP gather in CMP table";
static char *LABEL_SPLUS   = "<--- scroll to source in LD,RP,PP,CMP tables";
static char *LABEL_RPLUS   = "<--- scroll to receiver in LD,RP,PP,CMP tables";
static char *LABEL_UNKNOWN = " ";

char *HeaderTableGui::staticScrollMessageUpdate
                         (void *data, long /*ident*/, long /*index*/)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  switch(THIS->_scroll_choice)
          {
          case SCROLL_TRACE : return LABEL_TRACE;
          case SCROLL_GROUP : return LABEL_GROUP;
          case SCROLL_SLD   : return LABEL_SLD;
          case SCROLL_RLD   : return LABEL_RLD;
          case SCROLL_RP    : return LABEL_RP;
          case SCROLL_PP    : return LABEL_PP;
          case SCROLL_CMP   : return LABEL_CMP;
          case SCROLL_SPLUS : return LABEL_SPLUS;
          case SCROLL_RPLUS : return LABEL_RPLUS;
          default:            break;
          }
  return LABEL_UNKNOWN;
}



//-------------------- static hold functions -------------------------//
//-------------------- static hold functions -------------------------//
//-------------------- static hold functions -------------------------//

        // private.
        // "hold" toggle buttons above each column.

void HeaderTableGui::staticHoldTrap
                         (void *data, long ident, long /*index*/,
                          long ivar, long nread, char* endkey)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  long hold_index = ident - FIRST_HOLD_IDENT;
  assert(hold_index >= 0 && hold_index < NCOLMAX);
  if(strings_equal(endkey, "ARRIVED"))
      {
      if(THIS->_hold[hold_index] > 0) THIS->SLMatrixViewBox::showMessage
             ("press to release hold on this trace number"); 
      else                            THIS->SLMatrixViewBox::showMessage
             ("press to hold this trace number constant"); 
      }
  if(nread == 0) return;
  if(ivar > 0) ivar = THIS->getFirstVisibleColumn() + hold_index;
  THIS->_hold[hold_index] = ivar;
}



long HeaderTableGui::staticHoldUpdate
                         (void *data, long ident, long /*index*/)
{
  HeaderTableGui *THIS = (HeaderTableGui*)data;
  long hold_index = ident - FIRST_HOLD_IDENT;
  assert(hold_index >= 0 && hold_index < NCOLMAX);
  return THIS->_hold[hold_index];
}



static char *static_hold_message_update
                      (void* /*data*/, long /*ident*/, long /*index*/)
{
  static char *hold_label = "<--- press to hold trace constant";
  return hold_label;
}



//--------------- virtual functions overriding FgInform --------------//
//--------------- virtual functions overriding FgInform --------------//
//--------------- virtual functions overriding FgInform --------------//


void HeaderTableGui::postNewActiveTrace(FieldGeometry *fg)
{
  long trace = fg->getActiveTraceNumber();
  long current_trace = getCurrentTraceNumber(trace);
  if(current_trace > 0) makeColumnVisible(current_trace);
}



void HeaderTableGui::finishedChanges(FieldGeometry*)
{
  resetFirstVisibleColumn();
  long first = getFirstVisibleColumn();
  SLRangeSelect *range = _top->getRangeSelect();
  assert(range);
  range->setValue(first);
  long ntraces = numColumnsUpdate();
  if(ntraces >= 1) range->setMinMax(1, ntraces);
  else             range->setNoValues();
}



//----------------------- make helper ----------------------------//
//----------------------- make helper ----------------------------//
//----------------------- make helper ----------------------------//

       // protected.

void HeaderTableGui::makeHelper()
{
  static long zero  = 0;
  static long two   = 2;
  static long three = 3;

  for(int i = 0; i < NCOLMAX; i++)
      {
/*
      long ident  = FIRST_HOLD_IDENT   + i;
      long ident2 = FIRST_SCROLL_IDENT + i;
      long col    = (WIDTH1 + 3) + (NCHAR + 1) * i;
*/
      int ident  = FIRST_HOLD_IDENT   + i;
      int ident2 = FIRST_SCROLL_IDENT + i;
      int col    = (WIDTH1 + 3) + (NCHAR + 1) * i;

             //   ID     PROMPT  SWITCH  SWITCH  ROW  COL    NCHAR  NDEC
      dbox_creg (ident2,                 &two  ,  1,  col+1,   6,    0);
      dbox_ireg3(ident , "hold", &zero,  &three,  2,  col  ,   2,    0);

      dbox_set_ctrap( ident2, staticScrollTrap);
      dbox_set_cfun ( ident2, static_scroll_update);
      dbox_set_sfun ( ident2, staticScrollSwitchUpdate);
      dbox_set_itrap( ident , staticHoldTrap);
      dbox_set_ifun ( ident , staticHoldUpdate);
      }

  long col   = (WIDTH1 + 3) + (NCHAR + 1) * NCOLMAX + (WIDTH2 + 1) - 5;
  long nchar = WIDTH3 + 13;

         //   ID     PROMPT  SWITCH  SWITCH  ROW       COL       NCHAR   NDEC
  dbox_creg (777   ,                 &zero ,  1,  (int)col, (int)nchar,   0);
  dbox_creg (888   ,                 &zero ,  2,  (int)col, (int)nchar,   0);

  dbox_set_cfun (777, staticScrollMessageUpdate);
  dbox_set_cfun (888, static_hold_message_update);
  
  SLMatrixViewBox::makeHelper();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
