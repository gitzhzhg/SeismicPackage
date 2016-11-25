
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
//---------------------- fg_trace_values.cc ------------------------//
//---------------------- fg_trace_values.cc ------------------------//
//---------------------- fg_trace_values.cc ------------------------//

//           implementation file for the FgTraceValues class
//                   not derived from any class
//                        subdirectory geom

//    This class calculates and returns values for seismic traces.

//    If some values cannot be calculated because of
//    incomplete or invalid information in FieldGeometry,
//    or because of a request for a trace number outside of
//    the valid range, those values will be set to nil.

//    Exceptions are the requested trace number, which is
//    always returned as received, even if invalid, and the
//    error flag, which is always TRUE or FALSE.

//    All indices (which start at 0) will have value -1 if they
//    cannot be calculated.


#include "geom/fg_trace_values.hh"
#include "geom/fg_constants.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_user_abort.hh"
#include "geom/seis_survey.hh"
#include "geom/rp_cards.hh"
#include "geom/pp_cards.hh"
#include "geom/zt_cards.hh"
#include "geom/fg_groups.hh"
#include "geom/fg_traces.hh"
#include "geom/midpoints.hh"

#include "geom/fgte_data.hh"	/* ehs */

#include "geom/seis_line.hh"
#include "geom/field_flag.hh"
#include "geom/rp_card.hh"
#include "geom/pp_card.hh"
#include "geom/fg_group.hh"

#include "oprim/integer_list.hh"

#include "cprim.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>


#define USE_ZT3_CARDS   TRUE
#define SKIP_ZT3_CARDS  FALSE

#define CREATING_MIDPOINTS  TRUE
#define SKIPPING_MIDPOINTS  FALSE


//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


FgTraceValues::FgTraceValues(FgInformer *informer, FgUserAbort *ua,
                             SeisSurvey *survey,
                             RpCards  *rp_cards,  PpCards  *pp_cards,
                             ZtCards  *zt_cards,  FgGroups *groups,
                             FgTraces *traces,    Midpoints *midpoints)
       :
             _informer                      (informer),
             _ua                            (ua),
             _survey                        (survey),
             _rp_cards                      (rp_cards),
             _pp_cards                      (pp_cards),
             _zt_cards                      (zt_cards),
             _groups                        (groups),
             _traces                        (traces),
             _midpoints                     (midpoints),
             _tred                          ((FgTeData *) NULL),   /* ehs */

             _sline                         (NULL),
             _sflag                         (NULL),
             _rline                         (NULL),
             _rflag                         (NULL),
             _rp_card                       (NULL),
             _pp_card                       (NULL),

             _unplaced_sources              (0),
             _unplaced_traces               (0),

             _dependent_values_out_of_date  (FALSE),
             _sources_out_of_date           (TRUE),
             _receivers_out_of_date         (TRUE),
             _dead_codes_out_of_date        (TRUE),
             _coords_out_of_date            (TRUE),
             _transform_out_of_date         (TRUE),

             _speedup_added_to_field_flags  (FALSE),
             _speedup_added_to_rp_cards     (FALSE),
             _speedup_added_to_traces       (FALSE),

             _ve                            (5000.0),
             _ref                           (0.0),
             _fixdist                       (0.0),
             _ndpt                          (1001)
{
  assert(_informer && _ua && _survey && _rp_cards && _pp_cards && _zt_cards);
  assert(_groups && _traces && _midpoints);
  clearAllTraceValues();
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


FgTraceValues::~FgTraceValues()
{
}



//------------------ dependent values notifications --------------//
//------------------ dependent values notifications --------------//
//------------------ dependent values notifications --------------//

    // public.
    // must be called whenever dependent values go out of date,
    //    or when dependent updates are resumed.

void FgTraceValues::dependentValuesOutOfDate()
{
  _dependent_values_out_of_date = TRUE;
  sourceGathersChanging();
}



void FgTraceValues::dependentUpdatesResumed()
{
  _dependent_values_out_of_date = FALSE;
}



//------------------ something changing --------------------------//
//------------------ something changing --------------------------//
//------------------ something changing --------------------------//

    // public.
    // must be called whenever anything changes in FieldGeometry
    //   which will make the locations of the sources, or the
    //   locations of the traces in the receiver gathers, or the
    //   CMP bins, out of date.


void FgTraceValues::sourceGathersChanging()
{
  if(_sources_out_of_date) return;
  receiverGathersChanging();
  assert(_receivers_out_of_date);
  assert(_coords_out_of_date);
  assert(_transform_out_of_date);
  assert(_dead_codes_out_of_date);
  _survey->removeSourcesFromAllFlagsOnAllLines();
  _groups->clearGroups();
  _sources_out_of_date = TRUE;
  _unplaced_sources    = 0;
  _informer->sourceGathersOutOfDate();
}



void FgTraceValues::receiverGathersChanging()
{
  if(_receivers_out_of_date) return;
  zt1CodesChanging();
  zt2CodesChanging();
  zt3CodesChanging();
  zt4CodesChanging();
  coordsChanging();
  transformChanging();
  assert(!_sources_out_of_date);
  assert(_coords_out_of_date);
  assert(_transform_out_of_date);
  assert(_dead_codes_out_of_date);
  _survey->removeReceiversFromAllFlagsOnAllLines();
  _traces->clearTraces();
  _receivers_out_of_date     = TRUE;
  _unplaced_traces           = 0;
  _speedup_added_to_rp_cards = FALSE;
  _speedup_added_to_traces   = FALSE;
  _informer->receiverGathersOutOfDate();
}



void FgTraceValues::zt1CodesChanging()
{
  _survey->clearDeadSourceCodes();
  if(_dead_codes_out_of_date) return;
  _traces->clearDeadTraceCodes();
  _midpoints->deadCodesChanging();
  _dead_codes_out_of_date  = TRUE;
}



void FgTraceValues::zt2CodesChanging()
{
  _survey->clearDeadReceiverCodes();
  if(_dead_codes_out_of_date) return;
  _traces->clearDeadTraceCodes();
  _midpoints->deadCodesChanging();
  _dead_codes_out_of_date  = TRUE;
}



void FgTraceValues::zt3CodesChanging()
{
  if(_dead_codes_out_of_date) return;
  _traces->clearDeadTraceCodes();
  _midpoints->deadCodesChanging();
  _dead_codes_out_of_date  = TRUE;
}



void FgTraceValues::zt4CodesChanging()
{
  _survey->clearDeadSourceCodes();
  _survey->clearDeadReceiverCodes();
  if(_dead_codes_out_of_date) return;
  _traces->clearDeadTraceCodes();
  _midpoints->deadCodesChanging();
  _dead_codes_out_of_date  = TRUE;
}



void FgTraceValues::tredCodesChanging()
{
  if(_dead_codes_out_of_date) return;
  _traces->clearDeadTraceCodes();
  _midpoints->deadCodesChanging();
  _dead_codes_out_of_date  = TRUE;
}



void FgTraceValues::coordsChanging()
{
  if(_coords_out_of_date) return;
  assert(!_sources_out_of_date && !_receivers_out_of_date);
  _traces->clearCoords();
  _midpoints->midpointsChanging();
  _speedup_added_to_field_flags = FALSE;
  _coords_out_of_date           = TRUE;
}



void FgTraceValues::transformChanging()
{
  if(_transform_out_of_date) return;
  assert(!_sources_out_of_date && !_receivers_out_of_date);
  _midpoints->transformChanging();
  _transform_out_of_date = TRUE;
}



//--------------------- update source gathers ----------------------//
//--------------------- update source gathers ----------------------//
//--------------------- update source gathers ----------------------//

     // public.

void FgTraceValues::updateSourceGathers()
{
  if(_dependent_values_out_of_date) return;
  if(_pp_cards->numTraces() == 0) return;
  if(!_sources_out_of_date) return;
  assert(_receivers_out_of_date);
  assert(_coords_out_of_date);
  assert(_transform_out_of_date);
  assert(_dead_codes_out_of_date);
  clearAllTraceValues();

  _informer->preUpdateSourceGathers();
  _informer->showMessage("creating source gathers...");
  createSourcesHelper();
  finalMessage("source", _unplaced_sources, "sources");
  _sources_out_of_date = FALSE;
  _informer->postUpdateSourceGathers();

  clearAllTraceValues();
}



//--------------------- update receiver gathers --------------------//
//--------------------- update receiver gathers --------------------//
//--------------------- update receiver gathers --------------------//

     // public.

void FgTraceValues::updateReceiverGathers()
{
  if(_dependent_values_out_of_date) return;
  if(_pp_cards->numTraces() == 0) return;
  if(!_receivers_out_of_date) return;
  updateSourceGathers();
  assert(!_sources_out_of_date);
  assert(_coords_out_of_date);
  assert(_transform_out_of_date);
  assert(_dead_codes_out_of_date);
  clearAllTraceValues();

  _informer->preUpdateReceiverGathers();
  IntegerList::setAllocationSteps(20);
  addSpeedupToRpCards();
  _informer->showMessage("creating receiver gathers...");
  createReceiversOrMidpointsHelper(SKIPPING_MIDPOINTS);
  finalMessage("receiver", _unplaced_traces, "traces");
  _receivers_out_of_date = FALSE;
  _informer->postUpdateReceiverGathers();

  IntegerList::setAllocationSteps();
  clearAllTraceValues();
  if(_ua->aborted())
      {
      receiverGathersChanging();
      }
}



//--------------------- update midpoint gathers --------------------//
//--------------------- update midpoint gathers --------------------//
//--------------------- update midpoint gathers --------------------//

     // public.

long FgTraceValues::updateMidpointGathers()
{
  long error = 0;
  if(_dependent_values_out_of_date) return error;
  if(_pp_cards->numTraces() == 0) return error;
  if(!_coords_out_of_date &&
     !_transform_out_of_date && !_dead_codes_out_of_date) return error;
  updateSourceGathers();
  assert(!_sources_out_of_date);
  int receivers_out_of_date_temp = _receivers_out_of_date;
  clearAllTraceValues();

  addSpeedupToRpCards();
  addSpeedupToFieldFlags();

  if(_coords_out_of_date || _receivers_out_of_date ||
     _dead_codes_out_of_date)
      {
      _survey->setDeadSourceCodes();
      _survey->setDeadReceiverCodes();
      if(_receivers_out_of_date)
          {
          _informer->preUpdateReceiverGathers();
          _informer->showMessage
               ("creating receiver gathers and gathering midpoint info...");
          }
      else
          {
          _informer->showMessage("gathering midpoint information...");
          }
      createReceiversOrMidpointsHelper(CREATING_MIDPOINTS);
                                           // maybe receivers too.
      if(_receivers_out_of_date)
          {
          _receivers_out_of_date = FALSE;
          _informer->postUpdateReceiverGathers();
          }
      if(_ua->aborted())
          {
          if(receivers_out_of_date_temp) receiverGathersChanging();
          return error;        // still zero.
          }
      updateDeadTraceCodes();
      if(_ua->aborted())
          {
          if(_coords_out_of_date)     _traces->clearCoords();
          if(_dead_codes_out_of_date) _traces->clearDeadTraceCodes();
          return error;        // still zero.
          }
      }

  if(_coords_out_of_date || _transform_out_of_date)
      {
      error = _midpoints->cmpSort();
      }
  else if(_dead_codes_out_of_date)
      {
      _midpoints->updateLiveFold();
      }
  if(error != 0 && receivers_out_of_date_temp)
      {
      finalMessage("receiver (but NOT CMP)", _unplaced_traces, "traces");
      if(_unplaced_traces == 0) _informer->ringBell();
      }
  else if(error != 0)
      {
      _informer->showMessage("CMP gathers NOT created.");
      _informer->ringBell();
      }
  else if(receivers_out_of_date_temp)
      {
      finalMessage("receiver and CMP", _unplaced_traces, "traces");
      }
  else if(_coords_out_of_date || _transform_out_of_date)
      {
      finalMessage("CMP", _unplaced_traces, "traces");
      }
  else   //  if only _dead_codes_out_of_date but nothing else.
      {
      _informer->showMessage("live fold successfully updated");
      }

  clearAllTraceValues();
  _transform_out_of_date  = (error != 0);  // to indicate no CMP gathers.
  _coords_out_of_date     = FALSE;
  _dead_codes_out_of_date = FALSE;
  return error;
}



//--------------------- update live fold ---------------------------//
//--------------------- update live fold ---------------------------//
//--------------------- update live fold ---------------------------//

       // public.

void FgTraceValues::updateLiveFold()
{
  updateMidpointGathers();
}



//--------------- working and final message -----------------//
//--------------- working and final message -----------------//
//--------------- working and final message -----------------//

        // private.

void FgTraceValues::workingMessage
            (char *word1, long i, long n, char *word2, long unplaced)
{
  if(i == 5000 * (i / 5000))
      {
      char msg[100];
      sprintf(msg, "%s %d of %d.  unplaced %s: %d",
                       word1, i, n, word2, unplaced);
      _informer->showMessage(msg);
      }
}



void FgTraceValues::finalMessage(char *word1, long unplaced, char *word2)
{
  char msg[100];
  if(unplaced > 0)
      {
      sprintf(msg, "%s gathers created with %d unplaced %s",
                                            word1, unplaced, word2);
      _informer->showMessage(msg);
      _informer->ringBell();
      }
  else
      {
      sprintf(msg, "%s gathers successfully created", word1);
      _informer->showMessage(msg);
      }
}



//------------------ create sources helper -------------------//
//------------------ create sources helper -------------------//
//------------------ create sources helper -------------------//

        // private.

void FgTraceValues::createSourcesHelper()
{
  long npp    = _pp_cards->numPpCards();
  long ngr    = _pp_cards->numGroups();
  _groups->createGroups(ngr);
  _group            = 0;
  _unplaced_sources = 0;
  for(_ixpp = 0; _ixpp < npp; _ixpp++)
      {
      _pp_card = _pp_cards->ppCard(_ixpp);
      clearPpCardInformation();
      int error = fetchPpCardInformationForSources();
      savePpCardInformationForSources(error);
      for(_ixg = 0; _ixg < _ngroups; _ixg++)
          {
          _group++;
          assert(_group == _first_group + _ixg);
          clearGroupInformation();
          int error = fetchGroupInformationForSources();
          saveGroupInformationForSources(error);
          if(error)
              {
              _unplaced_sources++;
              }
          else
              {
              _survey->addSourceToFlag(_ixl_source, _ixf_source, _group);
              }
          workingMessage
              ("adding source", _group, ngr, "sources", _unplaced_sources);
          }
      }
  assert(_group == ngr);
  _survey->trimSourceAllocationsOnAllFlagsOnAllLines();
}



//--------------- create receivers or midpoints helper --------------//
//--------------- create receivers or midpoints helper --------------//
//--------------- create receivers or midpoints helper --------------//

        // private.

void FgTraceValues::createReceiversOrMidpointsHelper(int creating_midpoints)
{
  long npp    = _pp_cards->numPpCards();
  long ngr    = _pp_cards->numGroups();
  long ntr    = _pp_cards->numTraces();
  if(_receivers_out_of_date)
      {
      _traces->createTraces(ntr);
      }
  if(creating_midpoints)
      {
      if(_coords_out_of_date)     _traces->createCoords();
      if(_dead_codes_out_of_date) _traces->createDeadTraceCodes();
      }
  _ua->startAbortOption();
  long keep        = _unplaced_traces;
  long trace       = 0;
  _group           = 0;
  _unplaced_traces = 0;
  for(_ixpp = 0; _ixpp < npp; _ixpp++)
      {
      _pp_card = _pp_cards->ppCard(_ixpp);
      clearPpCardInformation();
      int error = FALSE;
      int error1 = fetchPpCardInformationForSources();
      int error2 = fetchPpCardInformationForReceivers();
      if(_receivers_out_of_date)
          {
          savePpCardInformationForReceivers(error2);
          }
      error = (error || error1 || error2);
      for(_ixg = 0; _ixg < _ngroups; _ixg++)
          {
          _group++;
          assert(_group == _first_group + _ixg);
          clearGroupInformation();
          int error3 = fetchGroupInformationForSources();
          int error4 = fetchGroupInformationForReceivers();
          error = (error || error3 || error4);
          if(_receivers_out_of_date)
              {
              saveGroupInformationForReceivers(error4);
              }
          if(creating_midpoints)
              {
              int error5 = fetchGroupInformationForMidpoints();
              saveGroupInformationForMidpoints(error5);
              error = (error || error5);
              }
          FgGroup *grp = _groups->getGroupPointer(_group);
          grp->_unplaced = 0;
          for(_channel = 1; _channel <= _nchan; _channel++)
              {
              long ixorig = trace;
              trace++;
              _trace = _first_trace + _ixg * _nchan + _channel - 1;
              assert(_trace == trace);
              clearTraceInformation();
              int error6 = fetchTraceInformationForReceivers();
              if(error || error6)
                  {
                  _unplaced_traces++;
                  grp->_unplaced++;
                  }
              if(_receivers_out_of_date)
                  {
                  _traces->_grps  [ixorig] = grp;
                  _traces->_rflags[ixorig] = _rflag;
                  if(!error6)
                      {
                      _survey->addReceiverToFlag(_ixl_rec, _ixf_rec, trace);
                      }
                  }
              if(creating_midpoints && _coords_out_of_date)
                  {
                  int error7 = fetchTraceInformationForMidpoints();
                  if(error7)
                      {
                      _traces->_xmids  [ixorig] = INIL;
                      _traces->_ymids  [ixorig] = INIL;
                      _traces->_offsets[ixorig] = INIL;
                      }
                  else
                      {
                      double xloc   = getEffectiveCmpXloc();
                      double yloc   = getEffectiveCmpYloc();
                      _traces->_xmids  [ixorig] = NearestInteger(xloc);
                      _traces->_ymids  [ixorig] = NearestInteger(yloc);
                      _traces->_offsets[ixorig] = NearestInteger(_offset);
                      }
                  }
              if(creating_midpoints && _dead_codes_out_of_date)
                  {
                  obtainDeadTraceCode(SKIP_ZT3_CARDS);
                  _traces->_dead[ixorig] = _dead_trace_code;
                  }
              workingMessage
                 ("adding trace", trace, ntr, "traces", _unplaced_traces);
              if(trace == 20000 * (trace / 20000) && _ua->aborted())
                     {
                     if(creating_midpoints)
                         {
                         if(_coords_out_of_date)
                                           _traces->clearCoords();
                         if(_dead_codes_out_of_date)
                                           _traces->clearDeadTraceCodes();
                         }
                     if(_receivers_out_of_date)
                         {
                         _traces->clearTraces();
                         }
                     _ua->stopAbortOption();
                     return;
                     }
              }
          }
      }
  _ua->stopAbortOption();
  assert(trace  == ntr);
  assert(_group == ngr);
  if(_receivers_out_of_date)
      {
      _survey->trimReceiverAllocationsOnAllFlagsOnAllLines();
      _speedup_added_to_traces = TRUE;
      }
  else
      {
      assert(_unplaced_traces == keep);
      }
  if(creating_midpoints && _coords_out_of_date)
      {
      _traces->findOffsetRange();
      }
}



//------------------ add speedup to field flags ------------------//
//------------------ add speedup to field flags ------------------//
//------------------ add speedup to field flags ------------------//

       // private.

void FgTraceValues::addSpeedupToFieldFlags()
{
  if(_speedup_added_to_field_flags) return;
  _informer->showMessage("adding speedup information to field flags...");
  long nlines = _survey->numLines();
  for(long ixl_rec = 0; ixl_rec < nlines; ixl_rec++)
      {
      SeisLine *rline = _survey->seisLine(ixl_rec);
      long nflags = rline->numFlagsOnLine();
      for(long ixf_rec = 0; ixf_rec < nflags; ixf_rec++)
          {
          FieldFlag *rflag = rline->fieldFlag(ixf_rec);
          double rec_xloc, rec_yloc;
          long ixf_rec_closest = rline->getSkiddedReceiverCoords
                                            (ixf_rec, &rec_xloc, &rec_yloc);
          long rec_mgp         = rline->getMatchableGroundPosition
                                            (ixf_rec);
          long rec_mgp_closest = rline->getMatchableGroundPosition
                                            (ixf_rec_closest);
          rflag->_ixf_rec_closest = ixf_rec_closest;
          rflag->_rec_xloc        = rec_xloc;
          rflag->_rec_yloc        = rec_yloc;
          rflag->_rec_mgp         = rec_mgp;
          rflag->_rec_mgp_closest = rec_mgp_closest;
          rflag->_cmp_error = (rec_mgp == INIL || rec_mgp_closest == INIL);
          }
      }
  _speedup_added_to_field_flags = TRUE;
}



//------------------ add speedup to rp cards ----------------------//
//------------------ add speedup to rp cards ----------------------//
//------------------ add speedup to rp cards ----------------------//

       // private.

void FgTraceValues::addSpeedupToRpCards()
{
  if(_speedup_added_to_rp_cards) return;
  _informer->showMessage("adding speedup information to RP cards...");
  long nrp    = _rp_cards->numRpCards();
  for(long ixrp = 0; ixrp < nrp; ixrp++)
      {
      RpCard *rp_card = _rp_cards->rpCard(ixrp);
      long  line_number = rp_card->_line;
      float shotpoint = rp_card->_shot;
      long ixl2 = _survey->findMatchingLineNumber(line_number);
      long ixf2 = -1;
      if(ixl2 >= 0)
           ixf2 = _survey->findMatchingShotpointOnLine(ixl2, shotpoint);
      rp_card->_ixl = ixl2;
      rp_card->_ixf = ixf2;
      }
  _speedup_added_to_rp_cards = TRUE;
}



//------------------ get rp line and flag index ------------------//
//------------------ get rp line and flag index ------------------//
//------------------ get rp line and flag index ------------------//

    // private.
    // these use only the following member variables:
    //      _rp_cards  _receivers_out_of_date  _survey
    // called from getReceiverFlagFromPattern.
    // called from adjustForIrregularities.

long FgTraceValues::getRpLineIndex(RpCard *rp_card)  const
{
  if(_speedup_added_to_rp_cards)
      {
      return rp_card->_ixl;
      }
  return _survey->findMatchingLineNumber(rp_card->_line);
}



long FgTraceValues::getRpFlagIndex(RpCard *rp_card, long ixl)  const
{
  if(_speedup_added_to_rp_cards)
      {
      return rp_card->_ixf;
      }
  if(ixl == -1) return -1;
  return _survey->findMatchingShotpointOnLine(ixl, rp_card->_shot);
}



//-------------------- set values ----------------------------//
//-------------------- set values ----------------------------//
//-------------------- set values ----------------------------//

       // public.

void FgTraceValues::setVe(float ve)
{
  assert(ve > 0.0);
  _ve = ve;
}

void FgTraceValues::setRef(float ref)
{
  _ref = ref;
}

void FgTraceValues::setFixdist(float fixdist)
{
  _fixdist = fixdist;
}

void FgTraceValues::setNdpt(long ndpt)
{
  assert(ndpt > 0);
  _ndpt = ndpt;
}



//------------------- start from scratch ----------------------//
//------------------- start from scratch ----------------------//
//------------------- start from scratch ----------------------//

    // public.

    // to be called before making multiple calls to
    // calculateTraceValues, or after changes have occurred
    // in the FieldGeometry data object.  this insures that
    // the next call to calculateTraceValues will do all new
    // calculations, and not just assume that some of the
    // previously-calculated information might still be valid.

void FgTraceValues::startFromScratch()
{
  if(_all_clear) return;
  clearAllTraceValues();
}



//----------------- calculate trace values --------------------//
//----------------- calculate trace values --------------------//
//----------------- calculate trace values --------------------//

    // public.
    // sets a bunch of values associated with a single seismic trace.
    // must be called BEFORE any calls to getErrorFlag or
    // getRequestedTraceNumber or any functions which return trace values.

    // returns error = FALSE if all trace values are successfully built.
    // returns error = TRUE  if one or more trace values cannot be built.
    // (in this case, some or all values subsequently returned may
    // contain nils rather than the correct information)

    // does no calculations if the requested trace number matches the
    // previous call to this routine.

    // remembers requested trace number and error flag and
    // calculated trace values for people to fetch later.


int FgTraceValues::calculateTraceValues(long group, long channel, int more)
{
  long itrace = _pp_cards->findTraceNumber(group, channel);
  return calculateTraceValues(itrace, more);
}


int FgTraceValues::calculateTraceValues(long itrace, int more)
{
  long ntr = _pp_cards->numTraces();
  if(itrace < 1 || itrace > ntr || itrace == INIL || ntr == 0 ||
                               _dependent_values_out_of_date)
      {
      if(!_all_clear) clearAllTraceValues();
      _itrace = itrace;
      return _error;
      }

  if(itrace == _itrace) return _error;   // no change has occurred.

  clearAllTraceValues();
  _itrace        = itrace;
  _trace         = itrace;
  _all_clear     = FALSE;
  _error         = FALSE;

  fetchControlInformation();

  assert(_ixpp >= 0 && _pp_card && _ixg >= 0);
  assert(_group != INIL && _channel != INIL);

  int error1 = fetchPpCardInformationForSources();
  int error2 = fetchPpCardInformationForReceivers();
  int error3 = fetchGroupInformationForSources();
  int error4 = fetchGroupInformationForReceivers();
  if(more == -77)
      {
      int error6 = fetchTraceInformationForReceivers();
      _error = (error1 || error2 || error3 || error4 ||
                error6);
      return _error;
      }
  int error5 = fetchGroupInformationForMidpoints();
  int error6 = fetchTraceInformationForReceivers();
  int error7 = fetchTraceInformationForMidpoints();
//  obtainDeadTraceCode(USE_ZT3_CARDS);
  fetchAdditionalInformationForSources();      // inserted 6/24/97
  fetchAdditionalInformationForReceivers();    // inserted 6/24/97
  if(more)
      {
////  fetchAdditionalInformationForSources();      // removed 6/24/97
////  fetchAdditionalInformationForReceivers();    // removed 6/24/97
      fetchAdditionalInformationForMidpoints();
      }
  /*
   * Since obtainDeadTraceCode uses headers if TRED is active,
   * get everything else calculated 1st.
   */
  obtainDeadTraceCode(USE_ZT3_CARDS);
  _error = (error1 || error2 || error3 || error4 ||
            error5 || error6 || error7);
  return _error;
}



//-------------------- fetch control information ------------------//
//-------------------- fetch control information ------------------//
//-------------------- fetch control information ------------------//

   // private.
   // needs:   _trace.
   // fetches: _ixpp _pp_card _ixg _group _channel.
   // might also fetch additional variables.

void FgTraceValues::fetchControlInformation()
{
  if(_speedup_added_to_traces)
      {
      FgGroup *grp = _traces->_grps[_trace-1];
      assert(grp);
      _ixpp        = grp->_ixpp;
      _pp_card     = grp->_pp_card;
      _ixg         = grp->_ixg;
      _group       = grp->_group;
      assert(_pp_card);
      _first_trace = _pp_card->_first_trace;
      _nchan       = _pp_card->_nchan;
      assert(_ixg >= 0 && _nchan >= 1);
      }
  else
      {
      _ixpp = _pp_cards->findPpCardWithDesiredTrace(_trace);
      assert(_ixpp >= 0);
      _pp_card = _pp_cards->ppCard(_ixpp);
      assert(_pp_card);
      _first_group      = _pp_card->getFirstGroupNumber();
      _first_trace      = _pp_card->getFirstTraceNumber();
      _nchan            = _pp_card->getNumChannelsOnCard();
      assert(_first_group >= 1);
      assert(_first_trace >= 1);
      assert(_nchan       >= 1);
      _ixg = (_trace - _first_trace) / _nchan;
      assert(_ixg         >= 0);
      _group = _first_group + _ixg;
      }
  _channel = _trace - _first_trace + 1 - _ixg * _nchan;
}



//----------------------- obtain dead trace code --------------------//
//----------------------- obtain dead trace code --------------------//
//----------------------- obtain dead trace code --------------------//

     // private.
     // always set use_zt3_cards to TRUE unless you are in the
     //   process of updating the dead trace codes in _traces.

void FgTraceValues::obtainDeadTraceCode(int use_zt3_cards)
{
  //// If use_zt_codes is FALSE, the ZT3 cards are not used here,
  //// because it is more efficient (when creating CMP gathers)
  //// to learn the situation after the individual traces are
  //// created.

  if(use_zt3_cards && _traces->_dead && _trace != INIL)
      {
      _dead_trace_code = _traces->_dead[_trace-1];
      }
  else if(_ixl_source >= 0 && _ixf_source >= 0 &&
          _ixl_rec    >= 0 && _ixf_rec    >= 0)
      {
      int dead_source_code  = _sline->getDeadSourceCode  (_ixf_source);
      int dead_rec_code     = _rline->getDeadReceiverCode(_ixf_rec);
      int source_maybe_dead = _sline->sourceMaybeDead    (_ixf_source);
      int rec_maybe_dead    = _rline->receiverMaybeDead  (_ixf_rec);
      int use_zt4_cards = (source_maybe_dead || rec_maybe_dead);
      _dead_trace_code = _zt_cards->getDeadTraceCode
                                  (dead_source_code, dead_rec_code,
                                   use_zt3_cards,    use_zt4_cards,
                                        _group,     _channel,
                                  _source_line, _source_shot,
                                     _rec_line,    _rec_shot);

      /*
       * If dead code is 2 or 4 tred might change it.
       */
      if (use_zt3_cards && _tred && !(_dead_trace_code % 2))
          {
          int tredDead = _tred->getDeadTraceCode(_trace);
          _dead_trace_code = _zt_cards->combineDeadTraceCodes(
              tredDead, _dead_trace_code);
          }
      }
  else
      {
      _dead_trace_code = ZT_CODE_NONE;
      }
}



//------------------- adjust for irregularities -------------------//
//------------------- adjust for irregularities -------------------//
//------------------- adjust for irregularities -------------------//

     // private.
     // looks at RP cards with SKIP and DUP flags.
     // needs: _ixrp_first_irreg  _ixrp_last_irreg  _rp_cards.
     // calls member functions: getRpLineIndex  getRpFlagIndex.
     // returns (possibly) modified value for actual_ixf.
     // actual_ixf is actually member variable _ixf_rec.
     // first_ixf  is actually member variable _ixf_chan1.

long FgTraceValues::adjustForIrregularities(long xinc,
              long actual_line, long actual_ixf, long first_ixf)
{
  if(xinc != 0 && _ixrp_first_irreg != -1)
     {
     for(long ixrp = _ixrp_first_irreg; ixrp <= _ixrp_last_irreg; ixrp++)
         {
         RpCard *rp_card = _rp_cards->rpCard(ixrp);
         int rpflag = rp_card->_rpflag;
         if(rpflag != RP_FLAG_SKIP && rpflag != RP_FLAG_DUP) continue;
         long rp_line = rp_card->_line;
         if(rp_line != actual_line) continue;
         long ixl2 = getRpLineIndex(rp_card);
         long ixf2 = getRpFlagIndex(rp_card, ixl2);
         if(ixl2 == -1) return TRUE;
         if(ixf2 == -1) return TRUE;
         if(ixf2 < MinimumValue(first_ixf, actual_ixf)) continue;
         if(ixf2 > MaximumValue(first_ixf, actual_ixf)) continue;
         switch(rpflag)
            {
            case RP_FLAG_SKIP:
            if     (actual_ixf > ixf2) actual_ixf += AbsoluteValue(xinc);
            else if(actual_ixf < ixf2) actual_ixf -= AbsoluteValue(xinc);
            else                       actual_ixf += xinc;
            break;

            case RP_FLAG_DUP:
            if     (actual_ixf > ixf2) actual_ixf -= AbsoluteValue(xinc);
            else if(actual_ixf < ixf2) actual_ixf += AbsoluteValue(xinc);
            break;

            default: assert(FALSE);
            }
         }
     }
return actual_ixf;
}



//----------------- get total static -----------------------------//
//----------------- get total static -----------------------------//
//----------------- get total static -----------------------------//

     // public.
     // these use the _ref and _ve variables.

float FgTraceValues::getTotalSourceStatic()  const
{
  if(_source_elev   == FNIL) return FNIL;
  if(_source_hd     == FNIL) return FNIL;
  if(_source_static == FNIL) return FNIL;
  return _source_static
            - 1000.0 * (_source_elev - _source_hd - _ref) / _ve;
}


float FgTraceValues::getTotalReceiverStatic()  const
{
  if(_rec_elev   == FNIL) return FNIL;
  if(_hd_at_rec  == FNIL) return FNIL;
  if(_tuh_at_rec == FNIL) return FNIL;
  if(_rec_static == FNIL) return FNIL;
  return _rec_static - _tuh_at_rec
            - 1000.0 * (_rec_elev - _hd_at_rec - _ref) / _ve;
}


float FgTraceValues::getTotalTraceStatic()  const
{
  float sstatic = getTotalSourceStatic  ();
  float rstatic = getTotalReceiverStatic();
  if(sstatic == FNIL) return FNIL;
  if(rstatic == FNIL) return FNIL;
  return sstatic + rstatic;
}



//---------------- get values depending on fixdist --------------//
//---------------- get values depending on fixdist --------------//
//---------------- get values depending on fixdist --------------//

     // public.

double FgTraceValues::getEffectiveCmpXloc()  const
{
  if(_fixdist <= 0.0) return _cmp_xloc;
  if(_cmp_mgp == FNIL) return DNIL;
  return _fixdist * (_cmp_mgp - 1.0);
}


double FgTraceValues::getEffectiveCmpYloc()  const
{
  if(_fixdist <= 0.0) return _cmp_yloc;
  if(_cmp_mgp == FNIL) return DNIL;
  return 0.0;
}


float FgTraceValues::getCmpElevation()  const
{
  if(_fixdist <= 0.0) return _cmp_nearest_elev;
  return _cmp_center_elev;
}


float FgTraceValues::getCmpShotpoint()  const
{
  if(_fixdist <= 0.0) return _cmp_nearest_shot;
  return _cmp_center_shot;
}


long FgTraceValues::getSourceGroundPosition()  const
{
  if(_fixdist <= 0.0) return _source_cgp;
  return _source_mgp;
}


long FgTraceValues::getReceiverGroundPosition()  const
{
  if(_fixdist <= 0.0) return _rec_cgp;
  return _rec_mgp;
}



//------------------ get inline distances ----------------------//
//------------------ get inline distances ----------------------//
//------------------ get inline distances ----------------------//

     // public.
     // these use the _fixdist variable.
     // if _fixdist == 0:
     //   getSourceInlineFixdist     == getSourceInlineDistance.
     //   getReceiverInlineFixdist   == getReceiverInlineDistance.
     //   getCmpCenterInlineDistance != getCmpNearestInlineDistance.

double FgTraceValues::getSourceInlineFixdist()  const
{
  if(_fixdist == 0.0)     return _source_dist;
  if(_source_mgp == INIL) return DNIL;
  return AbsoluteValue(_fixdist) * (_source_mgp - 1);
}


double FgTraceValues::getReceiverInlineFixdist()  const
{
  if(_fixdist == 0.0)  return _rec_dist;
  if(_rec_mgp == INIL) return DNIL;
  return AbsoluteValue(_fixdist) * (_rec_mgp - 1);
}


double FgTraceValues::getCmpCenterInlineDistance()  const
{
  if(_fixdist == 0)    return 0.5 * (_source_dist + _rec_dist);
  if(_cmp_mgp == INIL) return DNIL;
  return AbsoluteValue(_fixdist) * (_cmp_mgp - 1);
}



//------------------- clear all trace values ---------------------//
//------------------- clear all trace values ---------------------//
//------------------- clear all trace values ---------------------//

  // private.
  // after calling this routine:
  //   reset  _itrace    to the requested trace number even if invalid.
  //   reset  _all_clear to FALSE if will be calculating trace values.
  //   initialize _error to FALSE if will be calculating trace values.

void FgTraceValues::clearAllTraceValues()
{
  _itrace        = 0;     // to reset (see above).
  _all_clear     = TRUE;  // to reset (see above).
  _error         = TRUE;  // to reset (see above).
  _ixpp          = -1;
  _pp_card       = NULL;
  _ixg           = -1;
  _group         = INIL;
  _channel       = INIL;
  _trace         = INIL;

  clearPpCardInformation();
  clearGroupInformation();
  clearTraceInformation();
  clearAdditionalInformation();
}



//-------------------- update dead trace codes -----------------------//
//-------------------- update dead trace codes -----------------------//
//-------------------- update dead trace codes -----------------------//

     // private.
     // _traces already have codes from ZT1,ZT2,ZT4.
     // this routine updates these codes to include ZT3.

void FgTraceValues::updateDeadTraceCodes()
{
  if(!_dead_codes_out_of_date) return;
  _informer->showMessage("updating dead trace codes...");
  long ngroups = _pp_cards->numGroups();
  long ntraces = _pp_cards->numTraces();
  long nzt3    = _zt_cards->numZt3Cards();
  if(ngroups == 0) return;
  if(ntraces == 0) return;
  for(long i = 0; i < nzt3; i++)
      {
      int  dead1    = _zt_cards->getZt3Code           (i);
      long group1   = _zt_cards->getZt3FromGroupNumber(i);
      long group2   = _zt_cards->getZt3ToGroupNumber  (i);
      long channel1 = _zt_cards->getZt3FromTraceNumber(i);
      long channel2 = _zt_cards->getZt3ToTraceNumber  (i);
      if(group2 < group1)
          { long temp = group1; group1 = group2; group2 = temp; }
      if(channel2 < channel1)
          { long temp = channel1; channel1 = channel2; channel2 = temp; }
      group1 = ConstrainValue(group1, 1, ngroups);
      group2 = ConstrainValue(group2, 1, ngroups);
      for(long group = group1; group <= group2; group++)
          {
          long nchan   = _pp_cards->findNumChannelsInGroup(group);
          if(nchan == 0) continue;
          long chan1   = ConstrainValue(channel1, 1, nchan);
          long chan2   = ConstrainValue(channel2, 1, nchan);
          long trace1  = _pp_cards->findTraceNumber(group, chan1);
          long trace2  = _pp_cards->findTraceNumber(group, chan2);
          trace1 = ConstrainValue(trace1, 1, ntraces);
          trace2 = ConstrainValue(trace2, 1, ntraces);
          for(long trace = trace1; trace <= trace2; trace++)
              {
              int dead2 = (int)_traces->_dead[trace-1];
              int dead3 = _zt_cards->combineDeadTraceCodes(dead1, dead2);
              _traces->_dead[trace-1] = (char)dead3;
              }
          }
      }

  if (_tred)
      _tred->getDeadTraceCodes(_ua);
  if(_ua->aborted())	/* In case we someday add more stuff at the */
      return;		/* bottom of this function. */
}





////////// more efficient way to get source line and flag indices:
////////// to use in functions below:
////////// (not yet tested):
/*********
  long ixl, ixf;
  if(!sourceGathersOutOfDate())
      {
      ixl  = _groups->getIxlSource(group);
      ixf  = _groups->getIxfSource(group);
      if(ixl == -1 || ixf == -1) return FNIL;
      }
  else
      {
      long  source_line   = _pp_cards->getSourceLine     (ixpp);
      float source_shot   = _pp_cards->getSourceShotpoint(ixpp);
      long  ixl_pp_source = _survey->findMatchingLineNumber(source_line);
      if(ixl_pp_source == -1) return FNIL;
      long  ixf_pp_source = _survey->findMatchingShotpointOnLine
                                            (ixl_pp_source, source_shot);
      if(ixf_pp_source == -1) return FNIL;
      long ixl = ixl_pp_source;
      long flag_step = ixg * _pp_cards->getSourceMove();
      long nflags    = _survey->numFlagsOnLine(ixl);
      long ixf       = ixf_pp_source + flag_step;
      if(ixf < 0 || ixf >= nflags) return FNIL;
      }
*********/



//-------------------- get values ----------------------------------//
//-------------------- get values ----------------------------------//
//-------------------- get values ----------------------------------//

     // public.
     // return -1 if not found.
     // not yet efficient.

long FgTraceValues::getSourceLineIndex(long group)
{
  if(!_sources_out_of_date) return _groups->getIxlSource(group);
  long channel = 1;
  calculateTraceValues(group, channel, FALSE);
  return _ixl_source;
}


long FgTraceValues::getSourceFlagIndex(long group)
{
  if(!_sources_out_of_date) return _groups->getIxfSource(group);
  long channel = 1;
  calculateTraceValues(group, channel, FALSE);
  return _ixf_source;
}


long FgTraceValues::getReceiverLineIndex(long group, long channel)
{
  if(!_receivers_out_of_date && channel == 1)
                       return _groups->getIxlChan1(group);
  calculateTraceValues(group, channel, FALSE);
  return _ixl_rec;
}


long FgTraceValues::getReceiverFlagIndex(long group, long channel)
{
  if(!_receivers_out_of_date && channel == 1)
                       return _groups->getIxfChan1(group);
  calculateTraceValues(group, channel, FALSE);
  return _ixf_rec;
}



//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//
//------------------- fetch pp card information ------------------//

      // private.
      // calculates values associated with a single PP card.
      // uses speedup information if possible.
      // needs:   _pp_card.
      // fetches: values initialized in first routine.
      // routines (saves optional) must be called in this order:
      //            clearPpCardInformation             ();
      //    error = fetchPpCardInformationForSources   ();
      //             savePpCardInformationForSources   (error);
      //    error = fetchPpCardInformationForReceivers ();
      //             savePpCardInformationForReceivers (error);


//---------------------- clear pp card information -----------------//
//---------------------- clear pp card information -----------------//
//---------------------- clear pp card information -----------------//

        // private.

void FgTraceValues::clearPpCardInformation()
{
  _ngroups          =    0;    // source
  _first_group      =    0;    // source
  _ixl_pp_source    =   -1;    // source
  _ixf_pp_source    =   -1;    // source

  _ntraces          =    0;    // receiver
  _first_trace      =    0;    // receiver
  _nchan            =    0;    // receiver
  _ixl_pp_rec       =   -1;    // receiver
  _ixf_pp_rec       =   -1;    // receiver
  _ixrp_first       =   -1;    // receiver
  _ixrp_last        =   -1;    // receiver
  _ixrp_first_irreg =   -1;    // receiver   ok if remains -1.
  _ixrp_last_irreg  =   -1;    // receiver   ok if remains -1.
  _ixrp_chan1       =   -1;    // receiver
  _rp_line_chan1    = INIL;    // receiver
  _rp_mgp_chan1     = INIL;    // receiver
}



//---------- fetch or save pp card information for sources ----------//
//---------- fetch or save pp card information for sources ----------//
//---------- fetch or save pp card information for sources ----------//

        // private.
        // needs:   _pp_card.
        // fetches: _ngroups _first_group _ixl_pp_source _ixf_pp_source.
        // returns: error (TRUE or FALSE).

int FgTraceValues::fetchPpCardInformationForSources()
{
  if(!_pp_card) return TRUE;
  _ngroups = _pp_card->_ngroups;

  if(!_sources_out_of_date)
      {
      _first_group   = _pp_card->_first_group;
      _ixl_pp_source = _pp_card->_ixl_pp_source;
      _ixf_pp_source = _pp_card->_ixf_pp_source;
      return _pp_card->_source_error;
      }

  _first_group      = _pp_card->getFirstGroupNumber();
  long  source_line = _pp_card->_sline;
  float source_shot = _pp_card->_sshot;
  _ixl_pp_source    = _survey->findMatchingLineNumber(source_line);
  if(_ixl_pp_source == -1) return TRUE;
  _ixf_pp_source = _survey->findMatchingShotpointOnLine
                                         (_ixl_pp_source, source_shot);
  if(_first_group == 0) return TRUE;
  return FALSE;
}



void FgTraceValues::savePpCardInformationForSources(int error)
{
  _pp_card->_first_group   = _first_group;
  _pp_card->_ixl_pp_source = _ixl_pp_source;
  _pp_card->_ixf_pp_source = _ixf_pp_source;
  _pp_card->_source_error  = error;
}



//------------- fetch or save pp card receiver information -----------//
//------------- fetch or save pp card receiver information -----------//
//------------- fetch or save pp card receiver information -----------//

   // private.
   // needs:   _pp_card.
   // fetches: _ntraces _first_trace _nchan _ixl_pp_rec _ixf_pp_rec.
   // fetches: _ixrp_first _ixrp_last.
   // fetches: _ixrp_first_irreg _ixrp_last_irreg.
   // fetches: _ixrp_chan1 _rp_line_chan1 _rp_mgp_chan1.
   // returns: error (TRUE or FALSE).

int FgTraceValues::fetchPpCardInformationForReceivers()
{
  if(!_pp_card) return TRUE;
  _ntraces = _pp_card->_ntraces;

  if(!_receivers_out_of_date)
      {
      _first_trace      = _pp_card->_first_trace;
      _nchan            = _pp_card->_nchan;
      _ixl_pp_rec       = _pp_card->_ixl_pp_rec;
      _ixf_pp_rec       = _pp_card->_ixf_pp_rec;
      _ixrp_first       = _pp_card->_ixrp_first;
      _ixrp_last        = _pp_card->_ixrp_last;
      _ixrp_first_irreg = _pp_card->_ixrp_first_irreg;
      _ixrp_last_irreg  = _pp_card->_ixrp_last_irreg;
      _ixrp_chan1       = _pp_card->_ixrp_chan1;
      _rp_line_chan1    = _pp_card->_rp_line_chan1;
      _rp_mgp_chan1     = _pp_card->_rp_mgp_chan1;
      return _pp_card->_rec_error;
      }

  _first_trace   = _pp_card->getFirstTraceNumber();
  _nchan         = _pp_card->getNumChannelsOnCard();
  long  rec_line = _pp_card->_rline;
  float rec_shot = _pp_card->_rshot;
  long   pattern = _pp_card->_pattern;

  _ixl_pp_rec = _survey->findMatchingLineNumber(rec_line);
  if(_ixl_pp_rec >= 0) _ixf_pp_rec =
          _survey->findMatchingShotpointOnLine(_ixl_pp_rec, rec_shot);

  _ixrp_first = _rp_cards->findReceiverPattern (pattern);
  if(_ixrp_first == -1) return TRUE;

  _ixrp_last = _rp_cards->getEndOfReceiverPattern (_ixrp_first);
  if(_ixrp_last == -1) return TRUE;

  _rp_cards->getRpIrregularRange(_ixrp_first, _ixrp_last,
                            &_ixrp_first_irreg, &_ixrp_last_irreg);
  _ixrp_chan1 = _rp_cards->getRpCardWithDesiredChannel
                                        (_ixrp_first, _ixrp_last, 1);
  if(_ixrp_chan1 == -1) return TRUE;

  RpCard *rp_card = _rp_cards->rpCard(_ixrp_chan1);
  _rp_line_chan1  = rp_card->_line;
  long ixl2 = getRpLineIndex(rp_card);
  long ixf2 = getRpFlagIndex(rp_card, ixl2);
  if(ixl2 == -1) return TRUE;
  if(ixf2 == -1) return TRUE;

  _rp_mgp_chan1 = _survey->getMatchableGroundPosition(ixl2, ixf2);
  if(_rp_mgp_chan1 == INIL) return TRUE;
  return FALSE;
}



void FgTraceValues::savePpCardInformationForReceivers(int error)
{
  _pp_card->_first_trace      = _first_trace;
  _pp_card->_nchan            = _nchan;
  _pp_card->_ixl_pp_rec       = _ixl_pp_rec;
  _pp_card->_ixf_pp_rec       = _ixf_pp_rec;
  _pp_card->_ixrp_first       = _ixrp_first;
  _pp_card->_ixrp_last        = _ixrp_last;
  _pp_card->_ixrp_first_irreg = _ixrp_first_irreg;
  _pp_card->_ixrp_last_irreg  = _ixrp_last_irreg;
  _pp_card->_ixrp_chan1       = _ixrp_chan1;
  _pp_card->_rp_line_chan1    = _rp_line_chan1;
  _pp_card->_rp_mgp_chan1     = _rp_mgp_chan1;
  _pp_card->_rec_error        = error;
}



//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//
//------------------- fetch group information ------------------//

      // private.
      // calculates values associated with a single group (source).
      // uses speedup information if possible.
      // needs:   _pp_card _ixg _group.
      // fetches: values initialized in first routine.
      // routines (saves optional) must be called in this order:
      //           clearGroupInformation               ();
      //   error = fetchGroupInformationForSources     ();
      //            saveGroupInformationForSources     (error);
      //   error = fetchGroupInformationForReceivers   ();
      //            saveGroupInformationForReceivers   (error);
      //   error = fetchGroupInformationForMidpoints   ();
      //            saveGroupInformationForMidpoints   (error);


//--------------------- clear group information ----------------//
//--------------------- clear group information ----------------//
//--------------------- clear group information ----------------//

      // private.

void FgTraceValues::clearGroupInformation()
{
  _ixl_source         =   -1;     // source
  _ixf_source         =   -1;     // source
  _sline              = NULL;     // source
  _sflag              = NULL;     // source
  _source_line        = INIL;     // source
  _source_shot        = FNIL;     // source

  _ixl_chan1          =   -1;     // receiver
  _ixf_chan1          =   -1;     // receiver
  _chan1_line         = INIL;     // receiver
  _chan1_mgp          = INIL;     // receiver

  _source_xloc        = DNIL;     // midpoint
  _source_yloc        = DNIL;     // midpoint
  _ixf_source_closest =   -1;     // midpoint
  _source_mgp_closest = INIL;     // midpoint
}


//------------- fetch or save group source information ------------//
//------------- fetch or save group source information ------------//
//------------- fetch or save group source information ------------//

   // needs:   _group _pp_card _ixg _ixl_pp_source _ixf_pp_source.
   // fetches: _ixl_source _ixf_source _sline _sflag.
   // fetches: _source_line _source_shot.
   // returns: error (TRUE or FALSE).

int FgTraceValues::fetchGroupInformationForSources()
{
  if(_group == INIL) return TRUE;

  if(!_sources_out_of_date)
      {
      FgGroup *grp = _groups->getGroupPointer(_group);
      _ixl_source = grp->_ixl_source;
      _ixf_source = grp->_ixf_source;
      _sline      = grp->_sline;
      _sflag      = grp->_sflag;
      if(_sline) _source_line = _sline->getLineNumber();
      if(_sflag) _source_shot = _sflag->_shotpoint;
      return grp->_source_error;
      }

  if(_ixl_pp_source == -1) return TRUE;

  _ixl_source  = _ixl_pp_source;
  _sline       = _survey->seisLine(_ixl_source);
  if(_sline) _source_line = _sline->getLineNumber();

  if(_pp_card       == NULL) return TRUE;
  if(_ixg           ==   -1) return TRUE;
  if(_ixf_pp_source ==   -1) return TRUE;

  long flag_step = _ixg * _pp_card->_smove;
  long nflags    = _sline->numFlagsOnLine();
  long ixf       = _ixf_pp_source + flag_step;
  if(ixf < 0 || ixf >= nflags) return TRUE;
  _ixf_source = ixf;
  _sflag = _sline->fieldFlag(_ixf_source);
  if(_sflag) _source_shot = _sflag->_shotpoint;
  return FALSE;
}



void FgTraceValues::saveGroupInformationForSources(int error)
{
  FgGroup *grp = _groups->getGroupPointer(_group);
  grp->_group        = _group;
  grp->_ixpp         = _ixpp;
  grp->_pp_card      = _pp_card;
  grp->_ixg          = _ixg;
  grp->_ixl_source   = _ixl_source;
  grp->_ixf_source   = _ixf_source;
  grp->_sline        = _sline;
  grp->_sflag        = _sflag;
  grp->_source_error = error;
}



//------------- fetch or save group receiver information -------------//
//------------- fetch or save group receiver information -------------//
//------------- fetch or save group receiver information -------------//

   // needs:   _group _pp_card _ixg _ixl_pp_rec _ixf_pp_rec.
   // fetches: _ixl_chan1 _ixf_chan1 _chan1_line _chan1_mgp.
   // returns: error (TRUE or FALSE).

int FgTraceValues::fetchGroupInformationForReceivers()
{
  if(_group == INIL) return TRUE;

  if(!_receivers_out_of_date)
      {
      FgGroup *grp = _groups->getGroupPointer(_group);
      _ixl_chan1  = grp->_ixl_chan1;
      _ixf_chan1  = grp->_ixf_chan1;
      _chan1_line = grp->_chan1_line;
      _chan1_mgp  = grp->_chan1_mgp;
      return grp->_rec_error;
      }

  _ixl_chan1 = _ixl_pp_rec;
  _ixf_chan1 = -1;
  if(_ixf_pp_rec == -1) return TRUE;
  if(_ixl_chan1  == -1) return TRUE;
  if(!_pp_card        ) return TRUE;

  _chan1_line = _survey->getLineNumber(_ixl_chan1);

  long flag_step = _ixg * _pp_card->_rmove;
  long ixf_chan1 = _ixf_pp_rec + flag_step;
  long nflags    = _survey->numFlagsOnLine(_ixl_chan1);
  if(ixf_chan1 < 0 || ixf_chan1 >= nflags) return TRUE;
  _ixf_chan1 = ixf_chan1;
  if(_ixf_chan1 == -1) return TRUE;

  _chan1_mgp = _survey->getMatchableGroundPosition(_ixl_chan1, _ixf_chan1);
  if(_chan1_mgp == INIL) return TRUE;
  return FALSE;
}



void FgTraceValues::saveGroupInformationForReceivers(int error)
{
  FgGroup *grp = _groups->getGroupPointer(_group);
  grp->_ixl_chan1  = _ixl_chan1;
  grp->_ixf_chan1  = _ixf_chan1;
  grp->_chan1_line = _chan1_line;
  grp->_chan1_mgp  = _chan1_mgp;
  grp->_rec_error  = error;
}



//------------- fetch or save group midpoint information -------------//
//------------- fetch or save group midpoint information -------------//
//------------- fetch or save group midpoint information -------------//

   // needs:   _group _pp_card _ixg _sline _ixf_source.
   // fetches: _source_xloc        _source_yloc.
   // fetches: _ixf_source_closest _source_mgp_closest.
   // returns: error (TRUE or FALSE).

int FgTraceValues::fetchGroupInformationForMidpoints()
{
  if(_group == INIL) return TRUE;

  if(!_coords_out_of_date)
      {
      FgGroup *grp = _groups->getGroupPointer(_group);
      _source_xloc        = grp->_source_xloc;
      _source_yloc        = grp->_source_yloc;
      _ixf_source_closest = grp->_ixf_source_closest;
      _source_mgp_closest = grp->_source_mgp_closest;
      return grp->_cmp_error;
      }

  if(!_pp_card        ) return TRUE;
  if(!_sline          ) return TRUE;
  if(_ixg        == -1) return TRUE;
  if(_ixf_source == -1) return TRUE;

  float inline_skid, crossline_skid;
  _pp_card->getSourceSkidsQuickly(_ixg, &inline_skid, &crossline_skid);

  _ixf_source_closest = _sline->getSkiddedCoords
                           (_ixf_source, inline_skid, crossline_skid,
                                         &_source_xloc, &_source_yloc);

  _source_mgp_closest = _sline->getMatchableGroundPosition(_ixf_source_closest);
  if(_source_mgp_closest == INIL) return TRUE;
  return FALSE;
}



void FgTraceValues::saveGroupInformationForMidpoints(int error)
{
  FgGroup *grp = _groups->getGroupPointer(_group);
  grp->_source_xloc        = _source_xloc;
  grp->_source_yloc        = _source_yloc;
  grp->_ixf_source_closest = _ixf_source_closest;
  grp->_source_mgp_closest = _source_mgp_closest;
  grp->_cmp_error          = error;
}



//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//
//------------------- fetch trace information ------------------//

      // private.
      // calculates values associated with a single trace.
      // uses speedup information if possible.
      // needs:   _pp_card _ixg _group _channel _trace.
      // updates: _error (already TRUE if any inputs are invalid).
      // fetches: values initialized in first routine.
      // routines must be called in this order:
      //            clearTraceInformation              ();
      //    error = fetchTraceInformationForReceivers  ();
      //    error = fetchTraceInformationForMidpoints  ();
      //            obtainDeadTraceCode(USE_ZT3_CARDS);


//--------------------- clear trace information ----------------//
//--------------------- clear trace information ----------------//
//--------------------- clear trace information ----------------//

      // private.

void FgTraceValues::clearTraceInformation()
{
  _ixrp             =   -1;      // receiver
  _rp_card          = NULL;      // receiver
  _ixl_rec          =   -1;      // receiver
  _ixf_rec          =   -1;      // receiver
  _rline            = NULL;      // receiver
  _rflag            = NULL;      // receiver
  _rec_line         = INIL;      // receiver
  _rec_shot         = FNIL;      // receiver

  _rec_xloc         = DNIL;      // midpoint
  _rec_yloc         = DNIL;      // midpoint
  _ixf_rec_closest  =   -1;      // midpoint
  _rec_mgp_closest  = INIL;      // midpoint
  _offset           = FNIL;      // midpoint
  _cmp_xloc         = DNIL;      // midpoint
  _cmp_yloc         = DNIL;      // midpoint
  _cmp_mgp          = FNIL;      // midpoint

  _dead_trace_code  = ZT_CODE_NONE;      // obtain dead trace code
}



//---------------- fetch trace information for receivers -------------//
//---------------- fetch trace information for receivers -------------//
//---------------- fetch trace information for receivers -------------//

   // needs: _ixrp_first _ixrp_last _group _channel _trace.
   // needs (only if not speedup): _rp_line_chan1 _rp_mgp_chan1.
   // needs (only if not speedup): _chan1_line _chan1_mgp _ixf_chan1.
   // fetches:  _ixrp _rp_card _ixl_rec _ixf_rec _rline _rflag.
   // fetches:  _rec_line _rec_shot.
   // returns: error (TRUE or FALSE).

int FgTraceValues::fetchTraceInformationForReceivers()
{
//---------get RP card with desired channel:

  if(_ixrp_first    ==   -1) return TRUE;
  if(_ixrp_last     ==   -1) return TRUE;
  if(_channel       == INIL) return TRUE;

  _ixrp = _rp_cards->getRpCardWithDesiredChannel
                              (_ixrp_first, _ixrp_last, _channel);
  if(_ixrp == -1) return TRUE;
  _rp_card = _rp_cards->rpCard(_ixrp);

//---------get receiver flag for desired channel (from trace):

////  if(_speedup_added_to_traces && _traces->numTraces() > 0)
  if(_speedup_added_to_traces)
      {
////      assert(_trace > 0 && _trace != INIL);
      _rflag = _traces->_rflags[_trace-1];
      if(!_rflag) return TRUE;
      _rec_shot = _rflag->_shotpoint;
      _ixf_rec  = _rflag->_ixf;
      _rline    = _rflag->_line;
      if(!_rline) return TRUE;
      _rec_line = _rline->getLineNumber();
      _ixl_rec  = _rline->getLineIndex();
      return FALSE;
      }

//---------get receiver flag for desired channel (slowly):

  long xinc, yinc, incrx, incry;
  _rp_card->getRpIncrements(_channel, &xinc, &yinc, &incrx, &incry);

//---------get ACTUAL LINE for correct channel on this RP card:

  if(_rp_line_chan1 == INIL) return TRUE;
  if(_ixl_pp_rec    ==   -1) return TRUE;
  if(_chan1_line    == INIL) return TRUE;

  long this_rp_line = _rp_card->_line;
  long actual_line = _chan1_line + this_rp_line - _rp_line_chan1 + incry * yinc;

  _ixl_rec     = _survey->findMatchingLineNumber(actual_line);
  if(_ixl_rec == -1) return TRUE;
  _rline       = _survey->seisLine(_ixl_rec);
  _rec_line    = _rline->getLineNumber();

//---------get ACTUAL FLAG for correct channel on this RP card:

  if(_rp_mgp_chan1  == INIL) return TRUE;
  if(_ixf_pp_rec    ==   -1) return TRUE;
  if(_chan1_mgp     == INIL) return TRUE;
  if(_ixf_chan1     ==   -1) return TRUE;

  long  ixl2        = getRpLineIndex(_rp_card);
  long  ixf2        = getRpFlagIndex(_rp_card, ixl2);
  if(ixl2 == -1) return TRUE;
  if(ixf2 == -1) return TRUE;

  long this_rp_mgp  = _survey->getMatchableGroundPosition(ixl2, ixf2);
  long actual_mgp  = _chan1_mgp  + this_rp_mgp  - _rp_mgp_chan1  + incrx * xinc;

  long ixf_rec = actual_mgp - _rline->firstMatchableGroundPosition();
  long n       = _rline->numFlagsOnLine();
  if(ixf_rec < 0 || ixf_rec >= n) return TRUE;

/*
//---------get actual quantities for correct channel on this RP card:

  long xinc, yinc, incrx, incry;
  _rp_card->getRpIncrements(_channel, &xinc, &yinc, &incrx, &incry);

  long actual_line = _chan1_line + this_rp_line - _rp_line_chan1 + incry * yinc;
  long actual_mgp  = _chan1_mgp  + this_rp_mgp  - _rp_mgp_chan1  + incrx * xinc;

  _ixl_rec     = _survey->findMatchingLineNumber(actual_line);
  if(_ixl_rec == -1) return TRUE;
  _rline       = _survey->seisLine(_ixl_rec);
  _rec_line    = _rline->getLineNumber();
  long ixf_rec = actual_mgp - _rline->firstMatchableGroundPosition();
  long n       = _rline->numFlagsOnLine();
  if(ixf_rec < 0 || ixf_rec >= n) return TRUE;
*/

//---------adjust for irregularities (SKIP and DUP cards):

  ixf_rec = adjustForIrregularities(xinc, actual_line, ixf_rec, _ixf_chan1);
  if(ixf_rec < 0 || ixf_rec >= n) return TRUE;
  _ixf_rec  = ixf_rec;
  _rflag    = _rline->fieldFlag(_ixf_rec);
  _rec_shot = _rflag->_shotpoint;
  return FALSE;
}



//---------------- fetch trace midpoint information ----------------//
//---------------- fetch trace midpoint information ----------------//
//---------------- fetch trace midpoint information ----------------//

   // needs:   _rp_card _rline _rflag _ixf_rec.
   // fetches: _rec_xloc _rec_yloc _ixf_rec_closest _rec_mgp_closest.
   // fetches: _offset _cmp_xloc _cmp_yloc _cmp_mgp.
   // returns: error (TRUE or FALSE).

int FgTraceValues::fetchTraceInformationForMidpoints()
{
  if(!_rp_card || !_rline || !_rflag || _ixf_rec == -1) return TRUE;

  float inline_skid    = _rp_card->_xskid;
  float crossline_skid = _rp_card->_yskid;
  if(_speedup_added_to_field_flags
                 && inline_skid == 0.0 && crossline_skid == 0.0)
      {
      _rec_xloc        = _rflag->_rec_xloc;
      _rec_yloc        = _rflag->_rec_yloc;
      _ixf_rec_closest = _rflag->_ixf_rec_closest;
      _rec_mgp_closest = _rflag->_rec_mgp_closest;
      if(_rflag->_cmp_error) return TRUE;
      }
  else
      {
      inline_skid    += _rflag->_xskid;
      crossline_skid += _rflag->_yskid;
      _ixf_rec_closest = _rline->getSkiddedCoords
                               (_ixf_rec, inline_skid, crossline_skid,
                                         &_rec_xloc, &_rec_yloc);
      _rec_mgp_closest = _rline->getMatchableGroundPosition(_ixf_rec_closest);
      if(_rec_mgp_closest == INIL) return TRUE;
      }
  if(_source_xloc == DNIL || _source_yloc == DNIL) return TRUE;
  if(_source_mgp_closest == INIL) return TRUE;

  double xdiff = _rec_xloc - _source_xloc;
  double ydiff = _rec_yloc - _source_yloc;
  _offset      = (float)sqrt(xdiff * xdiff + ydiff * ydiff);
  _cmp_xloc    = 0.5 * (_source_xloc        + _rec_xloc);
  _cmp_yloc    = 0.5 * (_source_yloc        + _rec_yloc);
  _cmp_mgp     = 0.5 * (_source_mgp_closest + _rec_mgp_closest);
  return FALSE;
}



//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//
//------------------- fetch additional information ------------------//

   // private.
   // calculates additional values not needed for gathers.
   // all other clear... and fetch... routine must be called first.
   // fetches: values initialized in first routine.
   // routines must be called in this order:
   //    void    clearAdditionalInformation              ();
   //    void    fetchAdditionalInformationForSources    ();
   //    void    fetchAdditionalInformationForReceivers  ();
   //    void    fetchAdditionalInformationForMidpoints  ();


//--------------------- clear additional information ----------------//
//--------------------- clear additional information ----------------//
//--------------------- clear additional information ----------------//

      // private.

void FgTraceValues::clearAdditionalInformation()
{
  _field_file         = INIL;     // source
  _source_static      = FNIL;     // source
  _source_dist        = DNIL;     // source
  _source_cgp         = INIL;     // source
  _source_mgp         = INIL;     // source
  _source_elev        = FNIL;     // source
  _source_hd          = FNIL;     // source
  _source_tuh         = FNIL;     // source

  _rec_mgp          = INIL;       // receiver
  _rec_elev         = FNIL;       // receiver
  _rec_static       = FNIL;       // receiver
  _hd_at_rec        = FNIL;       // receiver
  _tuh_at_rec       = FNIL;       // receiver
  _rec_dist         = DNIL;       // receiver
  _rec_cgp          = INIL;       // receiver
  _last_trace_flag  = INIL;       // receiver

  _cmp_line         = FNIL;       // midpoint
  _cmp_nearest_elev = FNIL;       // midpoint
  _cmp_nearest_shot = FNIL;       // midpoint
  _cmp_nearest_dist = DNIL;       // midpoint
  _cmp_center_elev  = FNIL;       // midpoint
  _cmp_center_shot  = FNIL;       // midpoint
}



//------------- fetch additional information for sources -----------//
//------------- fetch additional information for sources -----------//
//------------- fetch additional information for sources -----------//

   // private.
   // needs:   _pp_card _ixg _sline _sflag _ixf_source _ixf_source_closest.
   // fetches: _field_file _source_static.
   // fetches: _source_dist _source_cgp _source_mgp.
   // fetches: _source_elev _source_hd  _source_tuh.

void FgTraceValues::fetchAdditionalInformationForSources()
{
  if(!_pp_card      ) return;
  if(_ixg      == -1) return;

  _field_file = _pp_card->_file + _ixg;

  if(!_sline) return;
  if(!_sflag) return;
///// now _ixf_source and _ixf_source_closest are also valid.

  _source_static      = _sflag->_sstat;
  _source_dist        = _sflag->_cum;
  _source_cgp         = _sline->getCumulativeGroundPosition(_ixf_source);
  _source_mgp         = _sline->getMatchableGroundPosition(_ixf_source);

  if(_ixg == 0)
      {
      _source_elev = _pp_card->_elev;
      _source_hd   = _pp_card->_hd;
      _source_tuh  = _pp_card->_tuh;
      }
  if(_source_elev == FNIL)
     _source_elev = _sline->getElevation(_ixf_source_closest);
  if(_source_hd   == FNIL)
     _source_hd   = _sflag->_hd;
  if(_source_tuh  == FNIL)
     _source_tuh  = _sflag->_tuh;
}



//------------- fetch additional information for receivers -----------//
//------------- fetch additional information for receivers -----------//
//------------- fetch additional information for receivers -----------//

   // private.
   // needs:   _rline _rflag _rp_card _ixf_rec _trace _channel _nchan.
   // fetches: _rec_mgp _rec_elev _rec_static.
   // fetches: _hd_at_rec _tuh_at_rec _rec_dist _rec_cgp.
   // fetches: _last_trace_flag.

void FgTraceValues::fetchAdditionalInformationForReceivers()
{
  if(_rflag)
      {
      _rec_static = _rflag->_rstat;
      _hd_at_rec  = _rflag->_hd;
      _tuh_at_rec = _rflag->_tuh;
      _rec_dist   = _rflag->_cum;
      _rec_cgp    = _rline->getCumulativeGroundPosition(_ixf_rec);
      _rec_mgp    = _rline->getMatchableGroundPosition (_ixf_rec);
      _rec_elev   = _rflag->_elev + _rp_card->_eskid + _rflag->_eskid;
      }
  if(_trace != INIL && _channel != INIL && _nchan >= 1)
      {
      if     (_trace == _pp_cards->numTraces()) _last_trace_flag = 2;
      else if(_channel == _nchan)               _last_trace_flag = 1;
      else                                      _last_trace_flag = 0;
      }
}



//------------- fetch additional information for midpoints -----------//
//------------- fetch additional information for midpoints -----------//
//------------- fetch additional information for midpoints -----------//

   // private.
   // needs:   _source_line _rec_line _sline _rline.
   // needs:   _cmp_xloc _cmp_yloc _cmp_mgp.
   // fetches: _cmp_line
   // fetches: _cmp_nearest_elev _cmp_nearest_shot _cmp_nearest_dist.
   // fetches: _cmp_center_elev _cmp_center_shot.

void FgTraceValues::fetchAdditionalInformationForMidpoints()
{
  if(_source_line != INIL && _rec_line != INIL)
      _cmp_line = 0.5 * (_source_line + _rec_line);

  if(!_sline || !_rline) return;

///// find nearest flags to the true midpoint on source line:
/////    (based on true x and y locations)

  long  ixfa, ixfb;
  float wa, wb;

  if(_cmp_xloc != DNIL && _cmp_yloc != DNIL)
      { ///////////////////////////////// start true midpoint loop.
      _sline->findFlagsBracketingTrueMidpoint(_cmp_xloc, _cmp_yloc,
                                             &ixfa, &ixfb, &wa, &wb);
      FieldFlag *ffa = _sline->fieldFlag(ixfa);
      FieldFlag *ffb = _sline->fieldFlag(ixfb);

      _cmp_nearest_elev = wa * ffa->_elev      + wb * ffb->_elev;
      _cmp_nearest_shot = wa * ffa->_shotpoint + wb * ffb->_shotpoint;
      _cmp_nearest_dist = wa * ffa->_cum       + wb * ffb->_cum;
/*
      _cmp_nearest_elev = wa * ffa->getElevation() +
                          wb * ffb->getElevation();
      _cmp_nearest_shot = wa * ffa->getShotpoint() +
                          wb * ffb->getShotpoint();
      _cmp_nearest_dist = wa * ffa->getCumDistance() +
                          wb * ffb->getCumDistance();
*/

///// find nearest flags to the true midpoint on receiver line:
/////    (based on true x and y locations)

      if(_rline != _sline)
          {
          _rline->findFlagsBracketingTrueMidpoint(_cmp_xloc, _cmp_yloc,
                                             &ixfa, &ixfb, &wa, &wb);
          FieldFlag *ffa = _rline->fieldFlag(ixfa);
          FieldFlag *ffb = _rline->fieldFlag(ixfb);

          _cmp_nearest_elev = 0.5 * (_cmp_nearest_elev +
                                     wa * ffa->_elev +
                                     wb * ffb->_elev);
          _cmp_nearest_shot = 0.5 * (_cmp_nearest_shot +
                                     wa * ffa->_shotpoint +
                                     wb * ffb->_shotpoint);
          _cmp_nearest_dist = 0.5 * (_cmp_nearest_dist +
                                     wa * ffa->_cum +
                                     wb * ffb->_cum);
          }
      } ///////////////////////////////// end true midpoint loop.

///// find center flags to this midpoint on source line:
/////    (based on matchable ground positions)

  if(_cmp_mgp != FNIL)
      { ///////////////////////////////// start center midpoint loop.
      float past1, past2;

      _sline->findFlagsBracketingCenterMidpoint(_cmp_mgp,
                                        &ixfa, &ixfb, &wa, &wb, &past1);
      FieldFlag *ffa = _sline->fieldFlag(ixfa);
      FieldFlag *ffb = _sline->fieldFlag(ixfb);

      _cmp_center_elev = wa * ffa->_elev +
                         wb * ffb->_elev;
      _cmp_center_shot = wa * ffa->_shotpoint +
                         wb * ffb->_shotpoint;

///// find center flags to this midpoint on receiver line:
/////    (based on matchable ground positions)

      if(_rline != _sline)
          {
          _rline->findFlagsBracketingCenterMidpoint(_cmp_mgp,
                                        &ixfa, &ixfb, &wa, &wb, &past2);
          FieldFlag *ffa = _rline->fieldFlag(ixfa);
          FieldFlag *ffb = _rline->fieldFlag(ixfb);

          if(past1 == 0.0 && past2 == 0.0)
              {
              _cmp_center_elev = 0.5 * (_cmp_center_elev +
                                        wa * ffa->_elev +
                                        wb * ffb->_elev);
              _cmp_center_shot = 0.5 * (_cmp_center_shot +
                                        wa * ffa->_shotpoint +
                                        wb * ffb->_shotpoint);
              }
          else
              {
              float past3 = past1 + past2;
              _cmp_center_elev = ( past2 * _cmp_center_elev + past1 *
                                       (wa * ffa->_elev +
                                        wb * ffb->_elev) )
                                          / past3;
              _cmp_center_shot = ( past2 * _cmp_center_shot + past1 *
                                       (wa * ffa->_shotpoint +
                                        wb * ffb->_shotpoint) )
                                          / past3;
              }
          }
      } ///////////////////////////////// end center midpoint loop.
}

void FgTraceValues::setTred(FgTeData *tred)	/* ehs */
{
	_tred = tred;
}

int FgTraceValues::getDeadCode(int itrace)	/* ehs */
{
	if (_dead_codes_out_of_date)
		return -1;
	else
		return _traces->_dead[itrace];
}


//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
