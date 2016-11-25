
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
//--------------------- seis_survey.cc ---------------------//
//--------------------- seis_survey.cc ---------------------//
//--------------------- seis_survey.cc ---------------------//

//          implementation file for the SeisSurvey class
//                derived from the SmartArray class
//                       subdirectory geom


//  The only constants (from fg_constants.hh) referred to within
//  this file are the chaining constants.

//  WARNING:  Dependent updates must NEVER be frozen in AccSearch,
//  since searching thru seismic lines is necessary even when
//  updates are frozen, in order to determine whether to add a
//  new line or append to an existing line when reading information
//  from a file.  For this reason, calls to freeze or resume
//  updates in AccSearch are commented out.


#include "geom/seis_survey.hh"
#include "geom/seis_line.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "geom/acc_matchable.hh"
#include "oprim/acc_select.hh"
#include "oprim/acc_search.hh"
#include "oprim/acc_index.hh"
#include "oprim/acc_sum.hh"
#include "oprim/fast_sort.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


#define  STEP           50
#define  DZERO          0.0
#define  IDENT_SELECT   6
#define  IDENT_JUNK     7


//--------------------- static functions ------------------------//
//--------------------- static functions ------------------------//
//--------------------- static functions ------------------------//

#define SLINE  ((SeisSurvey*)data)->unsafeSeisLine(index)


void set_index(void *data, long index)
{
  SLINE->setIndexOfThisLine(index);
}


static float get_line_number(void *data, long index)
{
  return (float)SLINE->getLineNumber();
}


static long get_cum_flag(void *data, long index)
{
  return SLINE->lastCumulativeGroundPosition();
}


static long update_cum_flag(void *data, long index, long prev_num_gps)
{
  return SLINE->updateLastCumulativeGP(prev_num_gps);
}


static char get_select_value(void *data, long index)
{
  return SLINE->getLineSelectValue();
}



static void set_select_value(void *data, long index, char select)
{
  SLINE->setLineSelectValue(select);
}



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SeisSurvey::SeisSurvey(FgInformer *informer, FgConnect *connect)
           :  SmartArray(STEP),
                 _chaining                (SLOPE_CHAINING),
                 _frozen                  (FALSE),
                 _tot_num_sources         (0),
                 _tot_num_receivers       (0),
                 _tot_num_flags           (0),
                 _dead_source_codes_set   (FALSE),
                 _dead_receiver_codes_set (FALSE),
                 _num_dead_sources        (0),
                 _num_dead_receivers      (0),
                 _num_reversed_sources    (0),
                 _num_reversed_receivers  (0),
                 _num_missing_sources     (0),
                 _num_missing_receivers   (0),
                 _num_live_sources        (0),
                 _num_live_receivers      (0),
                 _informer                (informer),
                 _connect                 (connect)
{
  assert(_informer && _connect);
  _search         = new AccSearch    (this, get_line_number);
  _acc_index      = new AccIndex     (this, set_index);
  _acc_cum_flag   = new AccSum       (this, IDENT_JUNK, get_cum_flag,
                                                     update_cum_flag);
  _acc_match_flag = new AccMatchable (this);
  _acc_select     = new AccSelect    (this, IDENT_SELECT, get_select_value,
                                                          set_select_value);
  _sort           = new FastSort     (this, get_line_number);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SeisSurvey::~SeisSurvey()
{
  deleteAllLines();
  delete _search;
  delete _acc_index;
  delete _acc_cum_flag;
  delete _acc_match_flag;
  delete _acc_select;
  delete _sort;
}



//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//
//--------------- values will change (or have changed) -------------//

    // virtual functions overriding SmartArray.
    // called by AccSelect and possibly other AccBase objects.

void SeisSurvey::valuesWillChange
                  (int ident, long index, long nrem, long nins)
{
  if(ident == IDENT_SELECT)
      _informer->preLineSelectionsChanged (index, nrem, nins);
}

void SeisSurvey::valuesHaveChanged
                  (int ident, long index, long nrem, long nins)
{
  if(ident == IDENT_SELECT)
      _informer->postLineSelectionsChanged (index, nrem, nins);
}



//---------------- before and after remove insert ----------------//
//---------------- before and after remove insert ----------------//
//---------------- before and after remove insert ----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray.
        //  The functions in SmartArray are empty.
        //  This functions makes sure the search information is updated,
        //    and then adjust the active index if necessary.


void SeisSurvey::beforeRemoveInsert(long index, long nrem, long nins)
{
  _informer      ->preRemoveInsertLines(index, nrem, nins);
  _search        ->preChange           (index, nrem, nins);
  _acc_index     ->preChange           (index, nrem, nins);
  _acc_cum_flag  ->preChange           (index, nrem, nins);
  _acc_match_flag->preChange           (index, nrem, nins);
  _acc_select    ->preChange           (index, nrem, nins);
}


void SeisSurvey::afterRemoveInsert(long index, long nrem, long nins)
{
  _search        ->post1Change();
  _acc_index     ->post1Change();
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  _search        ->post2Change();
  _acc_index     ->post2Change();
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  _informer      ->postRemoveInsertLines(index, nrem, nins);
}


//-------------- before and after new active index -----------------//
//-------------- before and after new active index -----------------//
//-------------- before and after new active index -----------------//

        //  private virtual functions overriding SmartArray.
        //  Called from SmartArray before and after changing
        //    the active index.
        //  The function in SmartArray is empty.


void SeisSurvey::beforeNewActiveIndex()
{
  _informer->preNewActiveLine();
}


void SeisSurvey::afterNewActiveIndex()
{
  _informer->postNewActiveLine();
}





//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//
//----------------- public access to this survey --------------------//

                     // ixl = index of desired line.



//--------------------------- get values -------------------------//
//--------------------------- get values -------------------------//
//--------------------------- get values -------------------------//


int SeisSurvey::allowSettingIncrDistance()  const
{
  return (_chaining != NO_CHAINING);
}


int SeisSurvey::allowSettingXloc()  const
{
  return (_chaining == NO_CHAINING);
}


int SeisSurvey::allowChangeChaining()  const
{
  return !_frozen;
}


int SeisSurvey::allowReverseLineDirections()  const
{
  return !_frozen;
}


int SeisSurvey::linesAreDuplicatedOrNotSorted()  const
{
  return !_search->isStrictlySorted();
}



double SeisSurvey::minimumXlocInSurvey()  const
{
  double xmin = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double x = minimumXlocOnLine(ixl);
      if(ixl == 0 || x < xmin) xmin = x;
      }
  return xmin;
}



double SeisSurvey::maximumXlocInSurvey()  const
{
  double xmax = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double x = maximumXlocOnLine(ixl);
      if(ixl == 0 || x > xmax) xmax = x;
      }
  return xmax;
}



double SeisSurvey::minimumYlocInSurvey()  const
{
  double ymin = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double y = minimumYlocOnLine(ixl);
      if(ixl == 0 || y < ymin) ymin = y;
      }
  return ymin;
}



double SeisSurvey::maximumYlocInSurvey()  const
{
  double ymax = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double y = maximumYlocOnLine(ixl);
      if(ixl == 0 || y > ymax) ymax = y;
      }
  return ymax;
}



long  SeisSurvey::numSelectedLines()  const
{
  return _acc_select->numSelected();
}



long SeisSurvey::getActiveLineNumber()  const
{
  long active = getActiveIndex();
  if(active >= 0) return seisLine(active)->getLineNumber();
  return INIL;
}


long SeisSurvey::getFirstLineNumber()  const
{
  long n = numLines();
  if(n >= 1) return seisLine(0)->getLineNumber();
  return INIL;
}


long SeisSurvey::getLastLineNumber()  const
{
  long n = numLines();
  if(n >= 1) return seisLine(n - 1)->getLineNumber();
  return INIL;
}


long SeisSurvey::getSmallestLineNumber()  const
{
  double value = _search->getMinimumValue();
  if(value == DNIL) return INIL;
  return NearestInteger(value);
}


long SeisSurvey::getLargestLineNumber()  const
{
  double value = _search->getMaximumValue();
  if(value == DNIL) return INIL;
  return NearestInteger(value);
}


long SeisSurvey::getMinLineIncrement()  const
{
  double step = _search->getMinimumStep();
  if(step == DNIL) return INIL;
  return NearestInteger(step);
}


long SeisSurvey::getMaxLineIncrement()  const
{
  double step = _search->getMaximumStep();
  if(step == DNIL) return INIL;
  return NearestInteger(step);
}



//--------------------------- set values -------------------------//
//--------------------------- set values -------------------------//
//--------------------------- set values -------------------------//


void SeisSurvey::setChaining(int chaining)  // all lines including buffer
{
  if(_chaining == chaining) return;
  assert(allowChangeChaining());
  _informer->preNewChaining();
  _chaining = chaining;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      seisLine(ixl)->setChaining(_chaining);
      }
  //SeisLine *buffer = (SeisLine*)fetchElementFromBuffer();
  SeisLine *buffer = seisLineInBuffer();
  if(buffer) buffer->setChaining(_chaining);
  _informer->postNewChaining();
}



//  if specified line number is not an existing line, then makes sure
//    that the new active line is in the direction of the line number
//    from the current active line, if possible.


void SeisSurvey::setActiveLineNumber(long line_number)          // search
{
  long n = numLines();
  if(n == 0) return;
  long active_line = getActiveLineNumber();
  int direction;
  if     (line_number > active_line) direction =  1;
  else if(line_number < active_line) direction = -1;
  else                               direction =  0;
//else                               return;
//   we want to send a message even if the active line does not change.
  long index = findNearestLineNumber(line_number, direction);
  setActiveLineIndex(index);
}



void SeisSurvey::sortByLineNumber()
{
  long n = numLines();
  _informer      ->preSortByLineNumber();
  _informer      ->preNewActiveLine();
  _search        ->preChange(0, n, n);
  _acc_index     ->preChange(0, n, n);
  _acc_cum_flag  ->preChange(0, n, n);
  _acc_match_flag->preChange(0, n, n);
  _acc_select    ->preChange(0, n, n);
  _sort->sort(1);                        // 1 = desired direction.
  _search        ->post1Change();
  _acc_index     ->post1Change();
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  _informer      ->postSortByLineNumber();
  _informer      ->postNewActiveLine();
  _search        ->post2Change();
  _acc_index     ->post2Change();
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
}



void SeisSurvey::freezeDependentUpdates()   // all lines including buffer
{
  if(_frozen) return;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      seisLine(ixl)->freezeDependentUpdatesOnLine();
      }
  SeisLine *buffer = seisLineInBuffer();
  if(buffer) buffer->freezeDependentUpdatesOnLine();
  _frozen = TRUE;
//_search        ->freezeUpdates();   // deliberately commented out.
  _acc_index     ->freezeUpdates();
  _acc_cum_flag  ->freezeUpdates();
  _acc_match_flag->freezeUpdates();
  _acc_select    ->freezeUpdates();
}
 
 
void SeisSurvey::resumeDependentUpdates()   // all lines including buffer
{
  if(!_frozen) return;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      seisLine(ixl)->resumeDependentUpdatesOnLine();
      }
  SeisLine *buffer = seisLineInBuffer();
  if(buffer) buffer->resumeDependentUpdatesOnLine();
  _frozen = FALSE;
//_search        ->resumeUpdates();   // deliberately commented out.
  _acc_index     ->resumeUpdates();
  _acc_cum_flag  ->resumeUpdates();
  _acc_match_flag->resumeUpdates();
  _acc_select    ->resumeUpdates();
}
 

//-------------- set dead source and receiver codes ---------------//
//-------------- set dead source and receiver codes ---------------//
//-------------- set dead source and receiver codes ---------------//

       // public.

void SeisSurvey::setDeadSourceCodes()
{
  if(_frozen)
      {
      clearDeadSourceCodes();
      return;
      }
  if(_dead_source_codes_set) return;
  _informer->showMessage("setting dead source codes...");
  _num_dead_sources       = 0;
  _num_reversed_sources   = 0;
  _num_missing_sources    = 0;
  _num_live_sources       = 0;
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      SeisLine *seis_line = seisLine(ixl);
      seis_line->setDeadSourceCodes();
      _num_dead_sources       += seis_line->numDeadSourcesOnLine();
      _num_reversed_sources   += seis_line->numReversedSourcesOnLine();
      _num_missing_sources    += seis_line->numMissingSourcesOnLine();
      _num_live_sources       += seis_line->numLiveSourcesOnLine();
      }
  _dead_source_codes_set = TRUE;
}



void SeisSurvey::setDeadReceiverCodes()
{
  if(_frozen)
      {
      clearDeadReceiverCodes();
      return;
      }
  if(_dead_receiver_codes_set) return;
  _informer->showMessage("setting dead receiver codes...");
  _num_dead_receivers     = 0;
  _num_reversed_receivers = 0;
  _num_missing_receivers  = 0;
  _num_live_receivers     = 0;
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      SeisLine *seis_line = seisLine(ixl);
      seis_line->setDeadReceiverCodes();
      _num_dead_receivers     += seis_line->numDeadReceiversOnLine();
      _num_reversed_receivers += seis_line->numReversedReceiversOnLine();
      _num_missing_receivers  += seis_line->numMissingReceiversOnLine();
      _num_live_receivers     += seis_line->numLiveReceiversOnLine();
      }
  _dead_receiver_codes_set = TRUE;
}



//--------------- clear dead source and receiver codes --------------//
//--------------- clear dead source and receiver codes --------------//
//--------------- clear dead source and receiver codes --------------//

       // public.

void SeisSurvey::clearDeadSourceCodes()
{
  if(!_dead_source_codes_set) return;
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      SeisLine *seis_line = seisLine(ixl);
      seis_line->clearDeadSourceCodes();
      }
  _dead_source_codes_set  = FALSE;
  _num_dead_sources       = 0;
  _num_reversed_sources   = 0;
  _num_missing_sources    = 0;
  _num_live_sources       = 0;
}



void SeisSurvey::clearDeadReceiverCodes()
{
  if(!_dead_receiver_codes_set) return;
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      SeisLine *seis_line = seisLine(ixl);
      seis_line->clearDeadReceiverCodes();
      }
  _dead_receiver_codes_set = FALSE;
  _num_dead_receivers      = 0;
  _num_reversed_receivers  = 0;
  _num_missing_receivers   = 0;
  _num_live_receivers      = 0;
}



//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//
//--------------- do create and delete object --------------------//

          // private virtual functions overriding SmartArray

void *SeisSurvey::doCreateObject()
{
  SeisLine *line = new SeisLine(_informer, _connect, _chaining);
  if(_frozen) line->freezeDependentUpdatesOnLine();
  return line;
}


void SeisSurvey::doDeleteObject(void *object)
{
  delete (SeisLine*)object;
}



void SeisSurvey::objectWillBeRemoved(long ixl)
{
  SeisLine *line = seisLine(ixl);
  _tot_num_flags     -= line->numFlagsOnLine();
  _tot_num_sources   -= line->numSourcesOnLine();
  _tot_num_receivers -= line->numReceiversOnLine();
  line->setIndexOfThisLine(-1);
  line->removeSourcesFromAllFlagsOnLine();
                           // since gathers will go out-of-date.
  line->removeReceiversFromAllFlagsOnLine();
                           // since gathers will go out-of-date.
}


void SeisSurvey::objectHasBeenInserted(long ixl)
{
  SeisLine *line = seisLine(ixl);
  _tot_num_flags     += line->numFlagsOnLine();
  _tot_num_sources   += line->numSourcesOnLine();
  _tot_num_receivers += line->numReceiversOnLine();
}



//---------------------- insert or remove lines -------------------//
//---------------------- insert or remove lines -------------------//
//---------------------- insert or remove lines -------------------//

    // the non-void functions return index (ixl) where action occurred.
    // the non-void functions return -1 if failed.


long SeisSurvey::appendNewLine()
{
  return appendNullElement();
}


long SeisSurvey::placeNewLine(long line_number)      // search
{
  long index = _search->findInsertionLocation((double)line_number);
  index = insertNewLine(index);
  if(index >= 0) setLineNumber(index, line_number);
  return index;
}


long SeisSurvey::insertNewLine(long ixl)
{
  return insertNullElement(ixl);
}



long SeisSurvey::insertNewLineFromBuffer(long ixl)
{
  return insertElementFromBuffer(ixl);
}



long SeisSurvey::deleteLine(long ixl)
{
  return removeElement(ixl);
}



long SeisSurvey::deleteLineToBuffer(long ixl)
{
  return removeElementToBuffer(ixl);
}


void SeisSurvey::deleteAllLines()        // all lines including buffer
{
  removeAllElements();
}



//------------------- find helper ------------------------//
//------------------- find helper ------------------------//
//------------------- find helper ------------------------//

                    // private

void SeisSurvey::findHelper(long ixl, double xloc, double yloc,
               long *nearest_index, double *nearest_distance2) const
{
  double distance2 = distanceSquaredToLine(ixl, xloc, yloc);
  if(distance2 == DNIL) return;
  if(*nearest_index == -1 || distance2 < *nearest_distance2)
      {
      *nearest_index = ixl;
      *nearest_distance2 = distance2;
      }
}



//------------- search among seismic lines ---------------------------//
//------------- search among seismic lines ---------------------------//
//------------- search among seismic lines ---------------------------//

        // these return an index (ixl), or -1 if not found.


long SeisSurvey::findNearestLineNumber(long line_number, int dir) const
{
  return _search->findNearestValue((double)line_number, dir);
}



long SeisSurvey::findMatchingLineNumber(long line_number) const
{
  return _search->findMatchingValue((double)line_number);
}



long SeisSurvey::findNearestLine(double xloc, double yloc) const
{
  long nearest_index = -1;
  double nearest_distance2;
  for(long ixl = 0; ixl < numLines(); ixl++)
      {
      findHelper(ixl, xloc, yloc, &nearest_index, &nearest_distance2);
      }
  return nearest_index;
}



long SeisSurvey::findNearestLineWithSource(double xloc, double yloc) const
{
  long nearest_index = -1;
  double nearest_distance2;
  for(long ixl = 0; ixl < numLines(); ixl++)
      {
      if(numSourcesOnLine(ixl) >= 1)
         {
         findHelper(ixl, xloc, yloc, &nearest_index, &nearest_distance2);
         }
      }
  return nearest_index;
}



long SeisSurvey::findNearestLineWithReceiver(double xloc, double yloc) const
{
  long nearest_index = -1;
  double nearest_distance2;
  for(long ixl = 0; ixl < numLines(); ixl++)
      {
      if(numReceiversOnLine(ixl) >= 1)
         {
         findHelper(ixl, xloc, yloc, &nearest_index, &nearest_distance2);
         }
      }
  return nearest_index;
}




//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//

                // ixl = index of desired line.
                // ixf = index of desired flag on desired line.



//--------------------- get seismic line values ---------------------//
//--------------------- get seismic line values ---------------------//
//--------------------- get seismic line values ---------------------//


int SeisSurvey::lineIsSelected(long ixl)  const
{
  return _acc_select->isSelected(ixl);
}


char SeisSurvey::getLineSelectValue(long ixl)  const
{
  return _acc_select->getSelectValue(ixl);
}



#define FUNCTION(long2, getLineNumber)               \
long2 SeisSurvey::getLineNumber(long ixl)  const     \
{                                                    \
  return seisLine(ixl)->getLineNumber();             \
}


FUNCTION(long  , getLineNumber)
FUNCTION(long  , numSourcesOnLine)
FUNCTION(long  , numReceiversOnLine)
FUNCTION(long  , numFlagsOnLine)
FUNCTION(long  , numSelectedFlagsOnLine)
FUNCTION(long  , firstCumulativeGroundPosition)
FUNCTION(long  , firstMatchableGroundPosition)
FUNCTION(long  , getActiveFlagIndexOnLine)
FUNCTION(float , getActiveShotpointOnLine)
FUNCTION(float , getFirstShotpointOnLine)
FUNCTION(float , getLastShotpointOnLine)
FUNCTION(float , getSmallestShotpointOnLine)
FUNCTION(float , getLargestShotpointOnLine)
FUNCTION(float , getMinShotpointIncrOnLine)
FUNCTION(float , getMaxShotpointIncrOnLine)
FUNCTION(int   , shotpointsAreDuplicatedOrNotSorted)
FUNCTION(double, minimumXlocOnLine)
FUNCTION(double, maximumXlocOnLine)
FUNCTION(double, minimumYlocOnLine)
FUNCTION(double, maximumYlocOnLine)


double SeisSurvey::distanceToLine(long ixl, double xloc, double yloc)
                                      const
{
  return seisLine(ixl)->distanceToLine(xloc, yloc);
}


double SeisSurvey::distanceSquaredToLine
                                 (long ixl, double xloc, double yloc)
                                      const
{
  return seisLine(ixl)->distanceSquaredToLine(xloc, yloc);
}



//--------------------- set seismic line values ---------------------//
//--------------------- set seismic line values ---------------------//
//--------------------- set seismic line values ---------------------//


void SeisSurvey::setLineNumber(long ixl, long line_number)
{
  _search->preChange(ixl, 1, 1);
  seisLine(ixl)->setLineNumber(line_number);
  _search->postChange();
}
 
 
void SeisSurvey::setActiveFlagIndexOnLine(long ixl, long active_flag_index)
{
  seisLine(ixl)->setActiveFlagIndexOnLine(active_flag_index);
}
 
 
void SeisSurvey::setActiveShotpointOnLine(long ixl, float active_shotpoint)
{
  seisLine(ixl)->setActiveShotpointOnLine(active_shotpoint);
}
 
 
void SeisSurvey::reverseLineDirection(long ixl)
{
  assert(allowReverseLineDirections());
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
//_acc_select    ->preChange(ixl, 1, 1);         // not needed.
  seisLine(ixl)->reverseLineDirection();
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
//_acc_select    ->post1Change();                 // not needed.
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
//_acc_select    ->post2Change();                 // not needed.
}
 
 
void SeisSurvey::setLineSelectValue(long ixl, char value)
{
  _acc_select->preChange(ixl, 1, 1);
  _acc_select->setSelectValue(ixl, value);
  _acc_select->postChange();
}


void SeisSurvey::incrementLineSelectValue(long ixl)
{
  _acc_select->preChange(ixl, 1, 1);
  _acc_select->incrementSelectValue(ixl);
  _acc_select->postChange();
}


void SeisSurvey::clearLineSelections()
{
  long n = numLines();
  _acc_select->preChange(0, n, n);
  _acc_select->clearSelections();
  _acc_select->postChange();
}



//------------------- update totals -------------------------//
//------------------- update totals -------------------------//
//------------------- update totals -------------------------//

        // private.

void SeisSurvey::updateTotalsWhenRemovingFlag(long ixl, long ixf)
{
  if(ixf == -1) return;
  _tot_num_flags--;
  _tot_num_sources   -= seisLine(ixl)->numSourcesAtFlag  (ixf);
  _tot_num_receivers -= seisLine(ixl)->numReceiversAtFlag(ixf);
}

void SeisSurvey::updateTotalsWhenInsertingFlag(long ixl, long ixf)
{
  if(ixf == -1) return;
  _tot_num_flags++;
  _tot_num_sources   += seisLine(ixl)->numSourcesAtFlag  (ixf);
  _tot_num_receivers += seisLine(ixl)->numReceiversAtFlag(ixf);
}



//-------------------- insert or remove flags --------------------//
//-------------------- insert or remove flags --------------------//
//-------------------- insert or remove flags --------------------//

    // the non-void functions return index (ixf) where action occurred.
    // the non-void functions return -1 if failed.


long SeisSurvey::appendNewFlagToLine(long ixl)
{
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  long ixf = seisLine(ixl)->appendNewFlagToLine();
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  updateTotalsWhenInsertingFlag(ixl, ixf);
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  return ixf;
}
 
 
long SeisSurvey::insertNewFlagOnLine(long ixl, long ixf)
{
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  ixf = seisLine(ixl)->insertNewFlagOnLine(ixf);
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  updateTotalsWhenInsertingFlag(ixl, ixf);
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  return ixf;
}
 
 
long SeisSurvey::placeNewFlagOnLine(long ixl, float shotpoint)
{
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  long ixf = seisLine(ixl)->placeNewFlagOnLine(shotpoint);
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  updateTotalsWhenInsertingFlag(ixl, ixf);
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  return ixf;
}
 
 
long SeisSurvey::insertNewFlagOnLineFromBuffer(long ixl, long ixf)
{
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  ixf = seisLine(ixl)->insertNewFlagOnLineFromBuffer(ixf);
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  updateTotalsWhenInsertingFlag(ixl, ixf);
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  return ixf;
}
 
 
long SeisSurvey::deleteFlagFromLine(long ixl, long ixf)
{
  updateTotalsWhenRemovingFlag(ixl, ixf);
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  ixf = seisLine(ixl)->deleteFlagFromLine(ixf);
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  return ixf;
}
 
 
long SeisSurvey::deleteFlagFromLineToBuffer(long ixl, long ixf)
{
  updateTotalsWhenRemovingFlag(ixl, ixf);
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  ixf = seisLine(ixl)->deleteFlagFromLineToBuffer(ixf);
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
  return ixf;
}
 
 
void SeisSurvey::deleteAllFlagsFromLine(long ixl)
{
  _tot_num_flags     -= seisLine(ixl)->numFlagsOnLine();
  _tot_num_sources   -= seisLine(ixl)->numSourcesOnLine();
  _tot_num_receivers -= seisLine(ixl)->numReceiversOnLine();
  _acc_cum_flag  ->preChange(ixl, 1, 1);
  _acc_match_flag->preChange(ixl, 1, 1);
  _acc_select    ->preChange(ixl, 1, 1);
  seisLine(ixl)->deleteAllFlagsFromLine();
  _acc_cum_flag  ->post1Change();
  _acc_match_flag->post1Change();
  _acc_select    ->post1Change();
  _acc_cum_flag  ->post2Change();
  _acc_match_flag->post2Change();
  _acc_select    ->post2Change();
}
 
 
void SeisSurvey::allocateSpaceForLine(long ixl, long nadd)
{
  seisLine(ixl)->allocateSpaceForLine(nadd);
}
 
 
void SeisSurvey::freeSpaceForLine(long ixl)
{
  seisLine(ixl)->freeSpaceForLine();
}
 
 



//------------------- search along seismic line -----------------------//
//------------------- search along seismic line -----------------------//
//------------------- search along seismic line -----------------------//

           // these return an index (ixf), or -1 if not found.


long SeisSurvey::findNearestShotpointOnLine
                               (long ixl, float shotpoint, int dir) const
{
  return seisLine(ixl)->findNearestShotpointOnLine(shotpoint, dir);
}
 
 
long SeisSurvey::findMatchingShotpointOnLine
                               (long ixl, float shotpoint)          const
{
  return seisLine(ixl)->findMatchingShotpointOnLine(shotpoint);
}
 
 
long SeisSurvey::findNearestFlagOnLine
                               (long ixl, double xloc, double yloc) const
{
  return seisLine(ixl)->findNearestFlagOnLine(xloc, yloc);
}
 
 
long SeisSurvey::findNearestSourceOnLine
                               (long ixl, double xloc, double yloc) const
{
  return seisLine(ixl)->findNearestSourceOnLine(xloc, yloc);
}
 
 
long SeisSurvey::findNearestReceiverOnLine
                               (long ixl, double xloc, double yloc) const
{
  return seisLine(ixl)->findNearestReceiverOnLine(xloc, yloc);
}
 
 
long SeisSurvey::closestNearbyFlag (long ixl, long ixf,
                                          double xloc, double yloc) const
{
  return seisLine(ixl)->closestNearbyFlag(ixf, xloc, yloc);
}
 
 

//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//
//----------------- pass-thru to SourcesReceivers ------------------//

   // ixl  = index of desired line.
   // ixf  = index of desired flag on desired line.
   // ixs2 = index of desired source at desired flag on desired line.
   // ixr2 = index of desired receiver at desired flag on desired line.

//----------------- get source-receiver values --------------------//
//----------------- get source-receiver values --------------------//
//----------------- get source-receiver values --------------------//


long SeisSurvey::numSourcesAtFlag(long ixl, long ixf)      const
{
  return seisLine(ixl)->numSourcesAtFlag(ixf);
}


long SeisSurvey::numReceiversAtFlag(long ixl, long ixf)      const
{
  return seisLine(ixl)->numReceiversAtFlag(ixf);
}

int  SeisSurvey::flagHasSource(long ixl, long ixf)      const
{
  return seisLine(ixl)->flagHasSource(ixf);
}


int  SeisSurvey::flagHasReceiver(long ixl, long ixf)      const
{
  return seisLine(ixl)->flagHasReceiver(ixf);
}


long SeisSurvey::sourceGroupNumber(long ixl, long ixf, long ixs2)  const
{
  return seisLine(ixl)->sourceGroupNumber(ixf, ixs2);
}


long SeisSurvey::receiverTraceNumber(long ixl, long ixf, long ixr2)  const
{
  return seisLine(ixl)->receiverTraceNumber(ixf, ixr2);
}



//----------------- set source-receiver values --------------------//
//----------------- set source-receiver values --------------------//
//----------------- set source-receiver values --------------------//


void SeisSurvey::addSourceToFlag   (long ixl, long ixf, long sgroup)
{
  long ns1 = seisLine(ixl)->numSourcesOnLine();
  seisLine(ixl)->addSourceToFlag(ixf, sgroup);
  long ns2 = seisLine(ixl)->numSourcesOnLine();
  _tot_num_sources += ns2 - ns1;
}


void SeisSurvey::addReceiverToFlag (long ixl, long ixf, long rtrace)
{
  long ns1 = seisLine(ixl)->numReceiversOnLine();
  seisLine(ixl)->addReceiverToFlag(ixf, rtrace);
  long ns2 = seisLine(ixl)->numReceiversOnLine();
  _tot_num_receivers += ns2 - ns1;
}



void SeisSurvey::removeSourcesFromFlag (long ixl, long ixf)
{
  long ns1 = seisLine(ixl)->numSourcesOnLine();
  seisLine(ixl)->removeSourcesFromFlag(ixf);
  long ns2 = seisLine(ixl)->numSourcesOnLine();
  _tot_num_sources += ns2 - ns1;
}


void SeisSurvey::removeSourcesFromAllFlagsOnLine (long ixl)
{
  long ns1 = seisLine(ixl)->numSourcesOnLine();
  seisLine(ixl)->removeSourcesFromAllFlagsOnLine();
  long ns2 = seisLine(ixl)->numSourcesOnLine();
  _tot_num_sources += ns2 - ns1;
}


void SeisSurvey::removeSourcesFromAllFlagsOnAllLines ()
{
  if(_tot_num_sources == 0) return;
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      seisLine(ixl)->removeSourcesFromAllFlagsOnLine();
      }
  _tot_num_sources = 0;
}



void SeisSurvey::removeReceiversFromFlag (long ixl, long ixf)
{
  long ns1 = seisLine(ixl)->numReceiversOnLine();
  seisLine(ixl)->removeReceiversFromFlag(ixf);
  long ns2 = seisLine(ixl)->numReceiversOnLine();
  _tot_num_receivers += ns2 - ns1;
}


void SeisSurvey::removeReceiversFromAllFlagsOnLine (long ixl)
{
  long ns1 = seisLine(ixl)->numReceiversOnLine();
  seisLine(ixl)->removeReceiversFromAllFlagsOnLine();
  long ns2 = seisLine(ixl)->numReceiversOnLine();
  _tot_num_receivers += ns2 - ns1;
}


void SeisSurvey::removeReceiversFromAllFlagsOnAllLines ()
{
  if(_tot_num_receivers == 0) return;
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      seisLine(ixl)->removeReceiversFromAllFlagsOnLine();
      }
  _tot_num_receivers = 0;
}


void SeisSurvey::trimSourceAllocationsOnAllFlagsOnAllLines ()
{
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      seisLine(ixl)->trimSourceAllocationsOnAllFlagsOnLine();
      }
}


void SeisSurvey::trimReceiverAllocationsOnAllFlagsOnAllLines ()
{
  long nlines = numLines();
  for(long ixl = 0; ixl < nlines; ixl++)
      {
      seisLine(ixl)->trimReceiverAllocationsOnAllFlagsOnLine();
      }
}



//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//

     // ixl  = index of desired line.
     // ixf  = index of desired flag on desired line.



//---------------------- get flag values --------------------------//
//---------------------- get flag values --------------------------//
//---------------------- get flag values --------------------------//

class FieldFlag *SeisSurvey::getFlagPointer(long ixl, long ixf)  const
{
  return seisLine(ixl)->getFlagPointer(ixf);
}


long SeisSurvey::getLineIndex(class FieldFlag *field_flag)  const
{
  return SeisLine::getLineIndex(field_flag);
}


long SeisSurvey::getFlagIndex(class FieldFlag *field_flag)  const
{
  return SeisLine::getFlagIndex(field_flag);
}


int  SeisSurvey::flagIsSelected(long ixl, long ixf)      const
{
  return seisLine(ixl)->flagIsSelected(ixf);
}
 
 
double SeisSurvey::distanceToFlag(long ixl, long ixf,
                             double xloc, double yloc)     const
{
  return seisLine(ixl)->distanceToFlag(ixf, xloc, yloc);
}
 
 
double SeisSurvey::distanceSquaredToFlag(long ixl, long ixf,
                              double xloc, double yloc)     const
{
  return seisLine(ixl)->distanceSquaredToFlag(ixf, xloc, yloc);
}
 
 
int  SeisSurvey::flagValueIsDependent(long ixl, long ixf, int ident) const
{
  return seisLine(ixl)->flagValueIsDependent(ixf, ident);
}
 
 
double SeisSurvey::getFlagValue(long ixl, long ixf, int ident) const
{
  return seisLine(ixl)->getFlagValue(ixf, ident);
}
 
 

#define  GETV(float, getShotpoint)                            \
float  SeisSurvey::getShotpoint(long ixl, long ixf)    const  \
{                                                             \
  return seisLine(ixl)->getShotpoint(ixf);                    \
}
 
GETV(float , getShotpoint)
GETV(double, getIncrDistance)
GETV(double, getXloc)
GETV(double, getYloc)
GETV(float , getElevation)
GETV(float , getHoleDepth)
GETV(float , getUpholeTime)
GETV(float , getReceiverStatic)
GETV(float , getSourceStatic )
GETV(float , getReceiverXskid)
GETV(float , getReceiverYskid)
GETV(float , getReceiverEskid)
GETV(char  , getFlagSelectValue)
GETV(double, getCumDistance)
GETV(double, getAzimuth)
GETV(int   , getDeadSourceCode)
GETV(int   , getDeadReceiverCode)
GETV(int   , sourceMaybeDead)
GETV(int   , receiverMaybeDead)
GETV(long  , getCumulativeGroundPosition)
GETV(long  , getMatchableGroundPosition)


void SeisSurvey::findCumulativeGroundPosition(long cumulative_gp,
                                           long *ixl, long *ixf)  const
{
  long n = numLines();
  for(long ixl2 = 0; ixl2 < n; ixl2++)
    {
    long ixf2 = seisLine(ixl2)->findCumulativeGroundPosition(cumulative_gp);
    if(ixf2 != -1)
        {
        *ixl = ixl2;
        *ixf = ixf2;
        return;
        }
    }
  *ixl = -1;
  *ixf = -1;
}


long SeisSurvey::findCumulativeGroundPosition
                                    (long ixl, long cumulative_gp)  const
{
  return seisLine(ixl)->findCumulativeGroundPosition(cumulative_gp);
}


long SeisSurvey::findMatchableGroundPosition
                                    (long ixl, long matchable_gp)  const
{
  return seisLine(ixl)->findMatchableGroundPosition(matchable_gp);
}


float SeisSurvey::defaultSourceDatumStatic
                (long ixl, long ixf, float ref, float ve)  const
{
  return seisLine(ixl)->defaultSourceDatumStatic(ixf, ref, ve);
}


float SeisSurvey::defaultReceiverDatumStatic
                (long ixl, long ixf, float ref, float ve)  const
{
  return seisLine(ixl)->defaultReceiverDatumStatic(ixf, ref, ve);
}



int SeisSurvey::receiverIsSkidded(long ixl, long ixf)  const
{
  return seisLine(ixl)->receiverIsSkidded(ixf);
}


void SeisSurvey::getSkiddedCoords(long ixl, long ixf,
                           float inline_skid, float crossline_skid,
                           double *x, double *y)  const
{
  seisLine(ixl)->getSkiddedCoords
                       (ixf, inline_skid, crossline_skid, x, y);
}


void SeisSurvey::getSkiddedCoordsPlus(long ixl, long ixf,
                           float inline_skid, float crossline_skid,
                           double *x, double *y)  const
{
  seisLine(ixl)->getSkiddedCoordsPlus
                       (ixf, inline_skid, crossline_skid, x, y);
}


void SeisSurvey::getSkiddedReceiverCoords(long ixl, long ixf,
                           double *x, double *y)  const
{
  seisLine(ixl)->getSkiddedReceiverCoords(ixf, x, y);
}



//---------------------- set flag values --------------------------//
//---------------------- set flag values --------------------------//
//---------------------- set flag values --------------------------//
 
 
void SeisSurvey::setFlagValue(long ixl, long ixf, int ident, double value)
{
  assert(ident != FG_DIST || allowSettingIncrDistance());
  assert(ident != FG_XLOC || allowSettingXloc        ());
  if(ident == FG_SHOT) _acc_match_flag->preChange(ixl, 1, 1);
  seisLine(ixl)->setFlagValue(ixf, ident, value);
  if(ident == FG_SHOT) _acc_match_flag->postChange();
}
 
 
void SeisSurvey::setDependentFlagValue
                             (long ixl, long ixf, int ident, double value)
{
  assert(ident != FG_DIST || allowSettingIncrDistance());
  assert(ident != FG_XLOC || allowSettingXloc        ());
  if(ident == FG_SHOT) _acc_match_flag->preChange(ixl, 1, 1);
  seisLine(ixl)->setDependentFlagValue(ixf, ident, value);
  if(ident == FG_SHOT) _acc_match_flag->postChange();
}
 
 

void SeisSurvey::setShotpoint(long ixl, long ixf, float value)
{                                         
  _acc_match_flag->preChange(ixl, 1, 1);
  seisLine(ixl)->setShotpoint(ixf, value);
  _acc_match_flag->postChange();
}
 
 
 
#define SETV(setShotpoint, float)                                \
void SeisSurvey::setShotpoint(long ixl, long ixf, float value)   \
{                                                                \
  seisLine(ixl)->setShotpoint(ixf, value);                       \
}
 
 
SETV(setYloc            , double)
SETV(setElevation       , float )
SETV(setHoleDepth       , float )
SETV(setUpholeTime      , float )
SETV(setReceiverStatic  , float )
SETV(setSourceStatic    , float )
SETV(setReceiverXskid   , float )
SETV(setReceiverYskid   , float )
SETV(setReceiverEskid   , float )
SETV(setFlagSelectValue , char  )


void SeisSurvey::incrementFlagSelectValue(long ixl, long ixf)
{                                          
  seisLine(ixl)->incrementFlagSelectValue(ixf);
}


void SeisSurvey::clearFlagSelections(long ixl)
{                                          
  seisLine(ixl)->clearFlagSelections();
}



void SeisSurvey::setIncrDistance(long ixl, long ixf, double value)
{
  assert(allowSettingIncrDistance());
  seisLine(ixl)->setIncrDistance(ixf, value);
}
 
 
void SeisSurvey::setXloc(long ixl, long ixf, double value)
{
  assert(allowSettingXloc());
  seisLine(ixl)->setXloc(ixf, value);
}
 
 


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

